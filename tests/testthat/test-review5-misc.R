# test-review5-misc.R -- the fifth pass on replay, the ellmer adapter, the
# corpus print and its limits: regressions the earlier fixes introduced, each
# checked against what the code did before any of them (0.5.0).
#
# Each block names the finding and says what the old behaviour was.

# A 0.5.0 recording of `obj` (a trace as a list): its pre-flight notes did not
# name the model the read asked for, and a read that asked for
# gr_options("model") had no `model` among its settings, since that was the
# default. Everything else is as it is recorded now.
r5_as_050 <- function(obj) {
  for (i in seq_along(obj$steps)) {
    if (identical(obj$steps[[i]]$label, "preflight")) {
      obj$steps[[i]]$detail$model <- NULL
      obj$steps[[i]]$detail$settings$model <- NULL
    }
  }
  obj
}

r5_save <- function(obj) {
  f <- withr::local_tempfile(fileext = ".json", .local_envir = parent.frame())
  writeLines(as.character(readgpt:::as_json.default(obj, pretty = TRUE)), f, useBytes = TRUE)
  f
}

# ---------------------------------------------------------------------------
# cross-3: a trace recorded by 0.5.0 from a read with a cheaper skim_model or
# summary_model did not replay. 0.5.0 read with gr_options("model") whatever
# the client, and wrote no model on the pre-flight note, so the replay client
# fell back to the model most calls asked for: the cheap one. A read now asks
# for its client's model, so the answer call asked for gpt-4o-mini, found its
# prompt recorded only under gpt-5.6-terra and stopped with gr_replay_miss.
# 0.5.0 replayed the same file.
# ---------------------------------------------------------------------------

test_that("a 0.5.0 recording with a skim or summary model replays", {
  local_registries()
  local_clean_cache()
  gr_options(max_calls = 1e4, max_cost_usd = 1e4)
  ch <- quiet(gr_segment(gr_ingest(sample_doc(3, 3)), list(method = "paragraph", max_tokens = 120)))
  q <- "How many participants?"
  specs <- list(list(reader = "skim", skim_model = "gpt-4o-mini"),
                list(reader = "hierarchical", summary_model = "gpt-4o-mini"),
                list(reader = "rerank", skim_model = "gpt-4o-mini"))
  for (spec in specs) {
    tr <- gr_trace()
    # How 0.5.0 recorded it: the read asked for gr_options("model"), and the
    # client (a mock, "mock-model") never came into it.
    live <- quiet(gr_read(ch, q, mock_echo("The cohort comprised 482 participants."),
                          c(spec, list(model = gr_options("model"))), trace = tr))
    f <- r5_save(r5_as_050(readgpt:::trace_as_list(tr)))
    rp <- gr_replay_client(f)
    expect_identical(rp$model, gr_options("model"), info = spec$reader)
    again <- quiet(gr_read(ch, q, rp, spec))
    expect_identical(rp$stats()$misses, 0L, info = spec$reader)
    expect_identical(again$answer, live$answer, info = spec$reader)

    # A replay that leaves the skim_model out still misses, as it did in
    # 0.5.0: the calls that setting made answer only a call that names it.
    rp <- gr_replay_client(f)
    expect_error(quiet(gr_read(ch, q, rp, list(reader = spec$reader))),
                 class = "gr_replay_miss", info = spec$reader)
  }
})

test_that("a 0.5.0 recording whose segmenter followed the client replays", {
  # In 0.5.0 the contextual segmenter asked for the client's model and the
  # read for gr_options("model"): two models where a recording now has one.
  # The model most calls asked for fits one of them, so 0.5.0 replayed this
  # only when the segmenter made most of the calls, and the fixed code before
  # this change missed on whichever half the guess did not fit.
  local_registries()
  local_clean_cache()
  gr_options(max_calls = 1e4, max_cost_usd = 1e4)
  cl <- mock_echo("The cohort comprised 482 participants.")
  doc <- quiet(gr_ingest(sample_doc(3, 3)))
  seg <- list(method = "contextual", context_source = "llm", max_tokens = 150)
  q <- "How many participants?"
  for (spec in list(list(reader = "stuff"), list(reader = "skim", skim_model = "gpt-4o-mini"))) {
    tr <- gr_trace()
    ch <- quiet(gr_segment(doc, seg, client = cl, trace = tr))
    live <- quiet(gr_read(ch, q, cl, c(spec, list(model = gr_options("model"))), trace = tr))
    asked <- vapply(Filter(function(s) !identical(s$kind, "local"), tr$steps),
                    function(s) as.character(s$params$model), character(1))
    expect_true(all(c("mock-model", gr_options("model")) %in% asked))
    f <- r5_save(r5_as_050(readgpt:::trace_as_list(tr)))
    rp <- gr_replay_client(f)
    tr2 <- gr_trace()
    ch2 <- quiet(gr_segment(doc, seg, client = rp, trace = tr2))
    again <- quiet(gr_read(ch2, q, rp, spec, trace = tr2))
    expect_identical(rp$stats()$misses, 0L, info = spec$reader)
    expect_identical(again$answer, live$answer, info = spec$reader)
  }
})

test_that("reads through two clients with different models replay through one", {
  # Recorded now: each pre-flight note names its own client's model, the notes
  # disagree, and the replay client took the model most calls asked for, so
  # the other read stopped with gr_replay_miss.
  local_registries()
  local_clean_cache()
  gr_options(max_calls = 1e4, max_cost_usd = 1e4)
  ch <- quiet(gr_segment(gr_ingest(sample_doc(3, 3)), list(method = "paragraph", max_tokens = 120)))
  q <- "How many participants?"
  ca <- mock_echo("The cohort comprised 482 participants.")
  ca$model <- "mock-a"
  cb <- mock_echo("Four hundred and eighty-two took part.")
  cb$model <- "mock-b"
  tr <- gr_trace()
  la <- quiet(gr_read(ch, q, ca, list(reader = "skim"), trace = tr))
  lb <- quiet(gr_read(ch, q, cb, list(reader = "map_reduce"), trace = tr))
  f <- r5_save(readgpt:::trace_as_list(tr))
  rp <- gr_replay_client(f)
  a <- quiet(gr_read(ch, q, rp, list(reader = "skim")))
  b <- quiet(gr_read(ch, q, rp, list(reader = "map_reduce")))
  expect_identical(rp$stats()$misses, 0L)
  expect_identical(a$answer, la$answer)
  expect_identical(b$answer, lb$answer)

  # The same prompt recorded under two other models is a guess between two
  # answers, so it is a miss rather than either of them.
  cc <- mock_echo("Answer C.")
  cc$model <- "mock-c"
  tr <- gr_trace()
  quiet(gr_read(ch, q, cc, list(reader = "map_reduce"), trace = tr))
  quiet(gr_read(ch, q, cc, list(reader = "map_reduce"), trace = tr))
  quiet(gr_read(ch, q, ca, list(reader = "stuff"), trace = tr))
  quiet(gr_read(ch, q, cb, list(reader = "stuff"), trace = tr))
  rp <- gr_replay_client(readgpt:::trace_as_list(tr))
  expect_identical(rp$model, "mock-c")
  expect_error(quiet(gr_read(ch, q, rp, list(reader = "stuff"))), class = "gr_replay_miss")
})

# ---------------------------------------------------------------------------
# client-1 / cache-5: the ellmer adapter wrote a structured reply back to text
# with jsonlite's `digits = NA`, which is 15 significant digits, not every
# digit. A 16-digit identifier, 1234567890123456, went into the extraction
# table as 1234567890123460 with status "ok" and nothing unverified. 0.5.0's
# four-decimal default kept both of these (and lost small decimals instead).
# ---------------------------------------------------------------------------

test_that("an ellmer structured reply keeps numbers that need 16 or 17 digits", {
  value <- list(id = 1234567890123456, amount = 123456789012345.6, pi = pi,
                p = 0.00003, small = 0.12345, n = 12L, groups = list(list(1L), list(2L)))
  stub <- list(chat_structured = function(user, type = NULL, echo = "none", convert = TRUE) value)
  txt <- readgpt:::ellmer_structured_text(stub, "q", NULL)
  back <- jsonlite::parse_json(txt)
  expect_identical(back$id, 1234567890123456)
  expect_identical(back$amount, 123456789012345.6)
  expect_identical(back$pi, pi)
  expect_identical(back$p, 0.00003)
  expect_identical(back$small, 0.12345)
  # An array of one-element arrays is still one (client-1 of the third pass).
  expect_match(txt, '"groups":[[1],[2]]', fixed = TRUE)
  # Written as the provider would: no exponent on an identifier, and no
  # seventeenth digit on a number fifteen write exactly.
  expect_match(txt, '"id":1234567890123456,', fixed = TRUE)
  expect_match(txt, '"small":0.12345,', fixed = TRUE)
})

test_that("an ellmer extraction keeps a 16-digit identifier exactly", {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("httr2")
  local_registries()
  gr_register_model("claude-test", context_window = 200000L, max_output = 64000L,
                    input_usd = 3, output_usd = 15)
  data <- paste0('{"registry_id":1234567890123456,"total_usd":12345678901234.56,',
                 '"registry_id__quote":"The trial registry number is 1234567890123456.",',
                 '"total_usd__quote":"Total spending was 12345678901234.56 dollars."}')
  httr2::local_mocked_responses(function(req) httr2::response(
    status_code = 200L, headers = list(`Content-Type` = "application/json"),
    body = charToRaw(sprintf(paste0(
      '{"id":"m","type":"message","role":"assistant","model":"claude-test",',
      '"content":[{"type":"tool_use","id":"tu","name":"_structured_tool_call",',
      '"input":{"data":%s}}],"stop_reason":"tool_use","stop_sequence":null,',
      '"usage":{"input_tokens":10,"output_tokens":7}}'), data))))
  doc <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("Methods. We randomised 120 adults.",
               "The trial registry number is 1234567890123456.",
               "Total spending was 12345678901234.56 dollars."), doc)
  cl <- gr_ellmer_client(ellmer::chat_anthropic(model = "claude-test", credentials = function() "k"))
  ex <- quiet(gr_extract(doc, gr_fields(
    registry_id = gr_field("the trial registry number", "number"),
    total_usd = gr_field("total spending in dollars", "number")), client = cl))
  expect_identical(ex$table$status, "ok")
  expect_identical(ex$table$registry_id, 1234567890123456)
  expect_identical(ex$table$total_usd, 12345678901234.56)
})

# ---------------------------------------------------------------------------
# client-2: ellmer raises a truncated structured reply through cli, which wraps
# the message to the console width. Below 69 columns a line break stood where
# the patterns had a space, so the reply came back with finish_reason NA: the
# plain failure the fix was meant to end, with no advice to raise the limit
# and the output tokens left unbilled. testthat runs at 80 columns, which is
# why nothing caught it.
# ---------------------------------------------------------------------------

test_that("a truncation is recognised in a message wrapped for a narrow console", {
  reason <- function(msg) readgpt:::ellmer_error_finish_reason(list(), simpleError(msg))
  expect_identical(reason("Response was truncated because it hit the\n`max_tokens` limit."),
                   "max_tokens")
  expect_identical(reason("Response was truncated because\nit hit the `max_tokens`\n  limit."),
                   "max_tokens")
  expect_identical(reason(paste0("Response was truncated because it exceeded the\n",
                                 "model's context window.")),
                   "context_window")
  expect_true(is.na(reason("Response was filtered by the provider's content\nmoderation policy.")))
})

test_that("ellmer's own truncation errors are recognised at any console width", {
  skip_if_not_installed("ellmer")
  check <- tryCatch(get("check_finish_reason", envir = asNamespace("ellmer")),
                    error = function(e) NULL)
  skip_if(!is.function(check), "this ellmer has no check_finish_reason()")
  # cli.condition_width is what cli wraps a condition to; testthat sets it to
  # Inf, which is why the suite never saw a wrapped message.
  for (w in c(30L, 40L, 55L, 68L, 80L)) {
    withr::local_options(width = w, cli.width = w, cli.condition_width = w)
    e1 <- tryCatch(check("max_tokens"), error = function(e) e)
    e2 <- tryCatch(check("context_window"), error = function(e) e)
    expect_identical(readgpt:::ellmer_error_finish_reason(NULL, e1), "max_tokens",
                     info = paste("width", w))
    expect_identical(readgpt:::ellmer_error_finish_reason(NULL, e2), "context_window",
                     info = paste("width", w))
    expect_match(conditionMessage(e1), "\n", fixed = TRUE)
    # The caller's console is left as it was.
    expect_identical(getOption("width"), w)
    expect_identical(getOption("cli.condition_width"), w)
  }
})

# ---------------------------------------------------------------------------
# corpus-trace-2 and corpus-trace-4: a corpus printed every embeddings request
# as a model call ("this run: 6 model call(s)" for 2 model calls and 4
# embeddings requests), while its own trace, printed next, counted them apart;
# and the max_total_calls warning said "the run has made 3 call(s)" of a run
# whose trace printed "1 model calls". The ceiling counts both kinds, as
# gr_options(max_calls =) does, and says so.
# ---------------------------------------------------------------------------

# An OpenAI-shaped server whose embeddings endpoint answers with vectors.
r5_endpoint <- function(env = parent.frame()) {
  respond <- function(url, out) {
    structure(list(url = url, status_code = 200L,
                   headers = structure(list(`content-type` = "application/json; charset=utf-8"),
                                       class = c("insensitive", "list")),
                   content = charToRaw(as.character(jsonlite::toJSON(out, auto_unbox = TRUE)))),
              class = "response")
  }
  testthat::local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      if (grepl("/embeddings$", url)) {
        texts <- unlist(body$input)
        return(respond(url, list(
          data = lapply(seq_along(texts), function(i) {
            list(embedding = as.list(c(nchar(texts[i]) %% 7 + 1, 1, i %% 3)))
          }),
          usage = list(prompt_tokens = sum(ceiling(nchar(texts) / 4))))))
      }
      all <- paste(vapply(body$messages, function(m) as.character(m$content), ""),
                   collapse = "\n")
      respond(url, list(
        id = "r5", object = "chat.completion", model = body$model,
        choices = list(list(index = 0, finish_reason = "stop",
                            message = list(role = "assistant",
                                           content = "Revenue was 45.2 million dollars."))),
        usage = list(prompt_tokens = ceiling(nchar(all) / 4), completion_tokens = 7)))
    },
    .package = "httr", .env = env)
}

r5_docs <- function(n) {
  vapply(seq_len(n), function(k) paste(sprintf(
    "Paragraph %d describes operations in region %d and the staff of site %d.",
    seq_len(30), seq_len(30), k), collapse = "\n\n"), character(1))
}

test_that("a corpus prints its model calls and its embeddings requests apart", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  r5_endpoint()
  cl <- gr_client(api_key = "sk-test", base_url = "https://r5.invalid/v1", api = "chat",
                  model = "gpt-4o", embedding_model = "text-embedding-3-small", max_retries = 0L)
  res <- quiet(gr_read_many(r5_docs(2), "What was revenue?", "needle", client = cl,
                            max_total_calls = 1000))
  s <- gr_trace_summary(res$trace)
  expect_gt(s$embed_calls, 0L)
  out <- capture.output(print(res))
  line <- grep("this run:", out, value = TRUE, fixed = TRUE)
  expect_length(line, 1L)
  expect_match(line, sprintf("this run: %d model call(s), %d embeddings request(s), ",
                             s$calls - s$embed_calls, s$embed_calls), fixed = TRUE)
  # The trace printed next says the same.
  expect_match(capture.output(print(res$trace))[1],
               sprintf("%d model calls, %d embeddings request(s)",
                       s$calls - s$embed_calls, s$embed_calls), fixed = TRUE)
})

test_that("the max_total_calls warning counts requests and says which", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  r5_endpoint()
  cl <- gr_client(api_key = "sk-test", base_url = "https://r5.invalid/v1", api = "chat",
                  model = "gpt-4o", embedding_model = "text-embedding-3-small", max_retries = 0L)
  msg <- NULL
  res <- withCallingHandlers(
    suppressMessages(gr_read_many(r5_docs(3), "What was revenue?", "needle", client = cl,
                                  max_total_calls = 1)),
    gr_corpus_call_cap = function(w) {
      msg <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning"))
  s <- gr_trace_summary(res$trace)
  expect_gt(s$embed_calls, 0L)
  expect_false(is.null(msg))
  expect_match(msg, sprintf("the run has made %d request(s) (%d model call(s), %d embeddings request(s))",
                            s$calls, s$calls - s$embed_calls, s$embed_calls), fixed = TRUE)
  expect_match(msg, "counts embeddings requests as well as model calls", fixed = TRUE)
  expect_identical(res$summary$status, c("ok", "skipped", "skipped"))
})

# ---------------------------------------------------------------------------
# corpus-trace-1 and parallel-segment-1: the help for gr_read_many() and
# gr_trace() said a recovered failure marks the answer partial. Only a
# fallback that ranked the chunks a reader sent does; the help now says so.
# These pin what it says for the segmenters.
# ---------------------------------------------------------------------------

test_that("a recovered segmenter failure is recorded, and partial is left alone", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  q <- "How many participants?"
  # Semantic cuts made on lexical vectors, read by a reader that does not
  # embed: not partial, and the fallback is in $warnings.
  cl <- gr_mock_client(function(m, p) "The cohort comprised 482 participants.",
                       embed_handler = function(texts) stop("HTTP 503: down"))
  a <- quiet(answer_document(sample_doc(3, 3), q, gr_recipe("sem",
    segment = list(method = "semantic", max_tokens = 120),
    read = list(reader = "map_reduce")), client = cl))
  expect_false(a$partial)
  expect_true(any(grepl("lexical", a$warnings, fixed = TRUE)))
  expect_true(all(vapply(a$trace$errors, function(e) isTRUE(e$recovered), logical(1))))
  # A contextual header that could not be written: only the trace says so.
  cl <- gr_mock_client(function(m, p) {
    if (grepl("You situate an excerpt", m[[1]]$content, fixed = TRUE)) stop("HTTP 503: down")
    "The cohort comprised 482 participants."
  })
  a <- quiet(answer_document(sample_doc(3, 3), q, gr_recipe("ctx",
    segment = list(method = "contextual", context_source = "llm", max_tokens = 120),
    read = list(reader = "map_reduce")), client = cl))
  expect_false(a$partial)
  expect_gt(length(a$trace$errors), 0L)
  expect_true(all(vapply(a$trace$errors, function(e) isTRUE(e$recovered), logical(1))))
  # A proposition batch kept as written: not partial, and $warnings says so.
  cl <- gr_mock_client(function(m, p) {
    if (grepl("Decompose text into standalone propositions", m[[1]]$content, fixed = TRUE)) {
      stop("HTTP 503: down")
    }
    "The cohort comprised 482 participants."
  })
  a <- quiet(answer_document(sample_doc(3, 3), q, gr_recipe("prop",
    segment = list(method = "proposition", max_tokens = 120),
    read = list(reader = "map_reduce")), client = cl))
  expect_false(a$partial)
  expect_gt(length(a$warnings), 0L)
  expect_gt(length(a$trace$errors), 0L)
  expect_true(all(vapply(a$trace$errors, function(e) isTRUE(e$recovered), logical(1))))
})
