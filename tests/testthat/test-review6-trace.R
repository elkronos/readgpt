# test-review6-trace.R -- the medium and low trace, replay, embedding and
# parallel findings the earlier passes left open, each checked against what the
# code did before any fix.
#
# Each block names the finding and says what the old behaviour was.

# An OpenAI-shaped embeddings endpoint. `respond(n, texts)` gives the status
# and the body of the n-th request.
r6_embed_endpoint <- function(respond, env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$n <- 0L
  testthat::local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      seen$n <- seen$n + 1L
      r <- respond(seen$n, unlist(body$input))
      structure(list(url = url, status_code = r$status,
                     headers = structure(list(`content-type` = "application/json; charset=utf-8"),
                                         class = c("insensitive", "list")),
                     content = charToRaw(as.character(jsonlite::toJSON(r$out, auto_unbox = TRUE,
                                                                       null = "null")))),
                class = "response")
    },
    .package = "httr", .env = env)
  seen
}

# One 3-dimensional vector per text, distinct for each, with the index the API
# gives it.
r6_vectors <- function(texts) {
  lapply(seq_along(texts), function(i) {
    list(index = i - 1L, embedding = as.list(c(i, 1, 10 - i)))
  })
}

r6_client <- function(max_retries = 0L) {
  gr_client(api_key = "sk-test", base_url = "https://r6.invalid/v1", api = "chat",
            model = "gpt-4o", embedding_model = "text-embedding-3-small",
            max_retries = max_retries, retry_pause_base = 0)
}

r6_unit <- function(v) v / sqrt(sum(v^2))

r6_doc <- function(n = 30L) {
  paste(sprintf(paste0("Section %d. Revenue in region %d rose by %d percent; total revenue ",
                       "was 45.2 million dollars."), seq_len(n), seq_len(n), seq_len(n)),
        collapse = "\n\n")
}

skip_if_no_future6 <- function() {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
}

# ---------------------------------------------------------------------------
# cache-trace-12: trace_absorb() set every folded step's recipe to the child's
# meta recipe, so folding a stage whose meta names none wrote NA over the
# recipe the step already had.
# ---------------------------------------------------------------------------

test_that("a step folded in twice keeps its recipe", {
  cl <- gr_mock_client(function(m, p) "fine")
  doc <- gr_trace(meta = list(recipe = "thorough", source = "c.txt"))
  gr_call(cl, "q", trace = doc)
  stage <- gr_trace(meta = list(stage = "synthesise"))
  readgpt:::trace_absorb(stage, doc)
  top <- gr_trace()
  readgpt:::trace_absorb(top, stage)
  df <- as.data.frame(top)
  expect_identical(df$recipe, "thorough")
  expect_identical(df$document, "c.txt")

  # A child that names a recipe still labels a step that has none.
  bare <- gr_trace()
  gr_call(cl, "q2", trace = bare)
  named <- gr_trace(meta = list(recipe = "fast"))
  readgpt:::trace_absorb(named, bare)
  expect_identical(as.data.frame(named)$recipe, "fast")
})

# ---------------------------------------------------------------------------
# cache-trace-05: a replayed call counted nothing against max_cost_usd, so a
# replay of a run the spending limit stopped never reached the limit and asked
# for the call after the stop (gr_replay_miss, blamed on "a different
# document"). The saved trace also left out budget_stop, stop_reason and
# spent_usd, so the file did not say the run was cut short.
# ---------------------------------------------------------------------------

r6_costcap_run <- function(env = parent.frame()) {
  local_registries(env)
  gr_register_model("r6-priced", context_window = 128000L, max_output = 4000L,
                    input_usd = 0.01, output_usd = 200)
  long <- paste(rep("Revenue rose in this section according to the excerpt.", 30),
                collapse = " ")
  cl <- gr_mock_client(function(m, p) long)
  doc <- paste(sprintf("Section %d. Revenue in region %d rose by %d percent.", 1:60, 1:60, 1:60),
               collapse = "\n\n")
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 60)))
  spec <- gr_read_spec("map_reduce", model = "r6-priced")
  gr_options(max_cost_usd = 0.5)
  tr <- gr_trace()
  live <- quiet(gr_read(ch, "What rose?", cl, spec, trace = tr))
  list(ch = ch, spec = spec, tr = tr, live = live, client = cl)
}

test_that("a saved trace says a limit cut the run short", {
  run <- r6_costcap_run()
  expect_true(run$tr$budget_stop)
  f <- withr::local_tempfile(fileext = ".json")
  gr_trace_save(run$tr, f)
  j <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_true(j$budget_stop)
  expect_identical(j$stop_reason, "cost")
  expect_equal(j$spent_usd, run$tr$spent_usd, tolerance = 1e-9)
})

test_that("a replay stops where the spending limit stopped the recorded run", {
  run <- r6_costcap_run()
  expect_true(run$live$partial)
  n_live <- run$tr$calls
  expect_lt(n_live, nrow(run$ch$chunks))
  f <- withr::local_tempfile(fileext = ".json")
  gr_trace_save(run$tr, f)

  rp <- gr_replay_client(f)
  tr <- gr_trace()
  again <- quiet(gr_read(run$ch, "What rose?", rp, run$spec, trace = tr))
  expect_identical(again$answer, run$live$answer)
  expect_true(again$partial)
  expect_identical(rp$stats()$misses, 0L)
  expect_identical(tr$calls, n_live)
  expect_true(tr$budget_stop)
  expect_identical(tr$stop_reason, "cost")
  # Nothing was paid for; what the recorded calls cost is counted apart.
  expect_identical(tr$spent_usd, 0)
  expect_equal(tr$replayed_usd, run$tr$spent_usd, tolerance = 1e-12)
})

test_that("a replay under a higher limit is told the recording was cut short", {
  run <- r6_costcap_run()
  f <- withr::local_tempfile(fileext = ".json")
  gr_trace_save(run$tr, f)
  gr_options(max_cost_usd = Inf)
  err <- tryCatch(quiet(gr_read(run$ch, "What rose?", gr_replay_client(f), run$spec)),
                  gr_replay_miss = function(e) e)
  expect_s3_class(err, "gr_replay_miss")
  expect_match(conditionMessage(err), "cut short by its spending limit", fixed = TRUE)
  expect_false(grepl("a different document", conditionMessage(err), fixed = TRUE))
})

test_that("a run the spending limit never stopped replays under any limit", {
  run <- r6_costcap_run()
  gr_options(max_cost_usd = Inf)
  tr <- gr_trace()
  full <- quiet(gr_read(run$ch, "What rose?", run$client, run$spec, trace = tr))
  expect_false(tr$budget_stop)
  expect_gt(tr$spent_usd, 0.5)
  # Replayed where the default limit, or any lower one, is in force: nothing
  # is spent, so nothing stops it.
  gr_options(max_cost_usd = 0.1)
  rp <- gr_replay_client(tr)
  rtr <- gr_trace()
  again <- quiet(gr_read(run$ch, "What rose?", rp, run$spec, trace = rtr))
  expect_identical(again$answer, full$answer)
  expect_false(again$partial)
  expect_false(rtr$budget_stop)
  expect_identical(rtr$replayed_usd, 0)
  expect_identical(rp$stats()$misses, 0L)
})

test_that("a replayed batch makes every call the recorded batch made", {
  local_registries()
  gr_register_model("r6-batch", context_window = 8000L, max_output = 1000L,
                    input_usd = 0, output_usd = 1e5)
  client <- gr_mock_client(function(m, p) "two words")
  fn <- function(i, trace) {
    if (!readgpt:::trace_can_call(trace)) return(NA_character_)
    gr_call(client, sprintf("item %d", i), model = "r6-batch", trace = trace)$text
  }
  gr_options(max_cost_usd = Inf)
  rec <- gr_trace()
  readgpt:::gr_lapply(1:4, fn, parallel = FALSE, trace = rec)
  # What a batch sent to workers leaves when it passes the limit: every call
  # made, and the stop recorded after them.
  rec$budget_stop <- TRUE
  rec$stop_reason <- "cost"
  expect_gt(rec$spent_usd, 0.5)
  gr_options(max_cost_usd = 0.5)
  client <- gr_replay_client(rec)
  tr <- gr_trace()
  got <- quiet(readgpt:::gr_lapply(1:4, fn, parallel = TRUE, workers = 2, trace = tr,
                                   item_usd = 0.01))
  expect_identical(unlist(got), rep("two words", 4L))
  expect_identical(client$stats()$hits, 4L)
  expect_true(tr$budget_stop)
  expect_identical(tr$stop_reason, "cost")
})

test_that("a recording whose steps do not carry their cost still replays to the stop", {
  run <- r6_costcap_run()
  obj <- readgpt:::trace_as_list(run$tr)
  obj$steps <- lapply(obj$steps, function(st) { st$budget_usd <- NULL; st })
  rp <- gr_replay_client(obj)
  again <- quiet(gr_read(run$ch, "What rose?", rp, run$spec))
  expect_identical(again$answer, run$live$answer)
  expect_identical(rp$stats()$misses, 0L)
})

test_that("a cache hit counts nothing against the limit, a replayed call its cost", {
  local_registries()
  tr <- gr_trace()
  readgpt:::trace_record(tr, "hit", list(list(role = "user", content = "q")),
                         readgpt:::gr_result(TRUE, "a", model = "gpt-4o", cached = TRUE,
                                   usage = list(input = 1000L, output = 10L)))
  expect_identical(tr$replayed_usd, 0)
  res <- readgpt:::gr_result(TRUE, "a", model = "gpt-4o", cached = TRUE,
                   usage = list(input = 1000L, output = 10L))
  res$replay_usd <- 0.25
  readgpt:::trace_record(tr, "replayed", list(list(role = "user", content = "q")), res)
  expect_identical(tr$spent_usd, 0)
  expect_identical(tr$replayed_usd, 0.25)
  expect_identical(tr$steps[[2]]$budget_usd, 0.25)
  gr_options(max_cost_usd = 0.25)
  expect_false(readgpt:::trace_can_call(tr))
  expect_identical(tr$stop_reason, "cost")
})

test_that("what a request counts is saved exactly in 15 digits, so a trace is written once", {
  local_registries()
  gr_register_model("r6-odd", context_window = 8000L, max_output = 1000L,
                    input_usd = 2.5, output_usd = 10)
  tr <- gr_trace()
  for (i in 1:40) {
    readgpt:::trace_record(tr, "call", list(list(role = "user", content = "q")),
                           readgpt:::gr_result(TRUE, "a", model = "r6-odd",
                                     usage = list(input = 1234L + i, output = 56L + i)))
  }
  expect_false(readgpt:::needs_more_digits(readgpt:::trace_as_list(tr)))
})

# ---------------------------------------------------------------------------
# cache-trace-08: the help and the gr_replay_no_embeddings warning promised
# that a replay which could not reproduce the embeddings still reproduced
# every recorded answer. A read that ranks or cuts by embeddings sends other
# prompts, and the miss said only "a different document, question, recipe or
# segmenter".
# ---------------------------------------------------------------------------

test_that("a miss caused by embeddings a replay cannot reproduce says so", {
  local_registries()
  local_clean_cache()
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  live <- quiet(answer_document(r6_doc(), "What was revenue?", "needle", client = cl))
  rp <- gr_replay_client(live$trace)
  msgs <- character(0)
  err <- withCallingHandlers(
    tryCatch(answer_document(r6_doc(), "What was revenue?", "needle", client = rp),
             gr_replay_miss = function(e) e),
    gr_replay_no_embeddings = function(w) {
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    },
    warning = function(w) invokeRestart("muffleWarning"),
    message = function(m) invokeRestart("muffleMessage"))
  expect_s3_class(err, "gr_replay_miss")
  expect_match(conditionMessage(err), "could not reproduce the recording's embeddings",
               fixed = TRUE)
  expect_true(length(msgs) >= 1L)
  expect_false(any(grepl("every recorded answer is reproduced", msgs, fixed = TRUE)))
  expect_true(all(grepl("prompts the recording does not hold", msgs, fixed = TRUE)))
})

test_that("the replay help no longer promises answers it cannot reproduce", {
  path <- testthat::test_path("..", "..", "R", "core-replay.R")
  if (!file.exists(path)) skip("package source not available")
  src <- paste(sub("^#'\\s*", "", readLines(path, warn = FALSE)), collapse = " ")
  expect_false(grepl("every recorded answer is still reproduced", src, fixed = TRUE))
  expect_true(grepl("does not give the recorded answer", src, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# tokenize-embed-04: embed_api() made one attempt per batch, ignoring the
# client's max_retries, and reported any status as "did not return
# embeddings". One 429 sent the whole matrix to lexical vectors.
# ---------------------------------------------------------------------------

test_that("an embeddings request is retried after a 429, as a model call is", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- r6_embed_endpoint(function(n, texts) {
    if (n == 1L) list(status = 429L, out = list(error = list(message = "slow down")))
    else list(status = 200L, out = list(data = r6_vectors(texts),
                                        usage = list(prompt_tokens = 6L)))
  })
  tr <- gr_trace()
  e <- quiet(gr_embed(r6_client(max_retries = 2L), c("a b", "c d"), trace = tr))
  expect_identical(attr(e, "embedding_source"), "api")
  expect_null(attr(e, "embedding_fallback"))
  expect_identical(seen$n, 2L)
  # One request in the ledger, retries and all.
  expect_identical(tr$calls, 1L)
  expect_length(tr$errors, 0L)
})

test_that("a failed embeddings request says what the server said", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- r6_embed_endpoint(function(n, texts) {
    list(status = 401L, out = list(error = list(message = "Incorrect API key provided")))
  })
  tr <- gr_trace()
  expect_warning(e <- gr_embed(r6_client(max_retries = 3L), c("a b", "c d"), trace = tr),
                 "HTTP 401: Incorrect API key provided", class = "gr_embed_fallback")
  # Not a transient failure, so not retried.
  expect_identical(seen$n, 1L)
  expect_match(tr$errors[[1]]$error, "HTTP 401: Incorrect API key provided", fixed = TRUE)
  expect_true(isTRUE(attr(e, "embedding_fallback")))
})

# ---------------------------------------------------------------------------
# tokenize-embed-09: embed_api() gave each text the vector at its position and
# ignored the `index` the API puts on each one, so a reply out of order gave
# every text another text's vector.
# ---------------------------------------------------------------------------

test_that("embeddings are matched to texts by their index", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  r6_embed_endpoint(function(n, texts) {
    list(status = 200L, out = list(data = rev(r6_vectors(texts)),
                                   usage = list(prompt_tokens = 9L)))
  })
  e <- gr_embed(r6_client(), c("one", "two", "three"))
  expect_identical(attr(e, "embedding_source"), "api")
  for (i in 1:3) expect_equal(e[i, ], r6_unit(c(i, 1, 10 - i)))
})

test_that("a reply whose indices do not match the texts is a failed request", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  r6_embed_endpoint(function(n, texts) {
    v <- r6_vectors(texts)
    v[[2]]$index <- 0L
    list(status = 200L, out = list(data = v, usage = list(prompt_tokens = 9L)))
  })
  expect_warning(e <- gr_embed(r6_client(), c("one", "two", "three")),
                 "index", class = "gr_embed_fallback")
  expect_identical(attr(e, "embedding_source"), "lexical")
})

# ---------------------------------------------------------------------------
# tokenize-embed-10: null or empty embeddings became an n x 0 matrix that
# gr_embed() returned as good "api" vectors, and a single null in a batch was
# zero-padded into a vector no query could reach.
# ---------------------------------------------------------------------------

test_that("null or empty embeddings are a failed request, not an empty matrix", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  r6_embed_endpoint(function(n, texts) {
    list(status = 200L, out = list(
      data = lapply(seq_along(texts), function(i) list(index = i - 1L, embedding = NULL)),
      usage = list(prompt_tokens = 9L)))
  })
  tr <- gr_trace()
  expect_warning(e <- gr_embed(r6_client(), c("one", "two", "three"), trace = tr),
                 "no usable embedding", class = "gr_embed_fallback")
  expect_identical(attr(e, "embedding_source"), "lexical")
  expect_true(isTRUE(attr(e, "embedding_fallback")))
  expect_identical(dim(e), c(3L, 512L))
  # The request was answered, and billed: its tokens stay in the ledger.
  expect_identical(tr$embed_tokens, 9L)
  expect_true(isTRUE(tr$errors[[1]]$recovered))
})

test_that("one null embedding in a batch fails the batch", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  r6_embed_endpoint(function(n, texts) {
    v <- r6_vectors(texts)
    v[[2]]$embedding <- list()
    list(status = 200L, out = list(data = v, usage = list(prompt_tokens = 9L)))
  })
  expect_warning(e <- gr_embed(r6_client(), c("one", "two", "three")),
                 "1 of 3 text", class = "gr_embed_fallback")
  expect_identical(attr(e, "embedding_source"), "lexical")
})

test_that("an embedder that returns no dimensions or holes has failed", {
  local_registries()
  cl <- gr_mock_client()
  expect_warning(e <- gr_embed(cl, c("a b", "c d"),
                               embedder = function(texts, params) matrix(numeric(0), 2, 0)),
                 "no dimensions", class = "gr_embed_fallback")
  expect_identical(attr(e, "embedding_source"), "lexical")
  expect_warning(e <- gr_embed(cl, c("a b", "c d"),
                               embedder = function(texts, params) matrix(c(1, NA, 0, 1), 2)),
                 "non-finite", class = "gr_embed_fallback")
  expect_identical(attr(e, "embedding_source"), "lexical")
})

# ---------------------------------------------------------------------------
# r2-export-serialization-fidelity-03: as_json() on a review stage, a corpus, a
# comparison or any list holding an answer failed with "cannot unclass an
# environment": every one of them holds a trace.
# ---------------------------------------------------------------------------

test_that("as_json() writes any object that holds a trace or an answer", {
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  cmp <- quiet(gr_compare(readgpt_example(), "What was revenue?", c("fast", "thorough"),
                          client = cl))
  j <- jsonlite::parse_json(as_json(cmp))
  expect_identical(j$trace$run_id, cmp$trace$run_id)
  # An answer inside is written as its own method writes it (but for the
  # clock, which moved on in between).
  own <- jsonlite::parse_json(as_json(cmp$answers$fast))
  got <- j$answers$fast
  own$trace$summary$elapsed_s <- got$trace$summary$elapsed_s <- NULL
  expect_identical(got, own)

  one <- jsonlite::parse_json(as_json(list(cmp$answers$fast)))
  expect_identical(one[[1]]$answer, cmp$answers$fast$answer)

  stage <- structure(list(table = data.frame(a = 1:2), trace = cmp$trace,
                          client = cl, fn = function(x) x, env = new.env()),
                     class = "gr_made_up_stage")
  js <- jsonlite::parse_json(as_json(stage))
  expect_identical(js$table[[2]]$a, 2L)
  expect_identical(length(js$trace$steps), length(cmp$trace$steps))
  expect_null(js$fn)
  expect_null(js$env)
  expect_null(js$client)
  # A client is never written, so neither is its key.
  keyed <- gr_cache_client(gr_client(api_key = "sk-r6-secret"), gr_cache(withr::local_tempdir()))
  expect_false(grepl("sk-r6-secret", as_json(list(client = keyed)), fixed = TRUE))
})

test_that("an object with nothing to replace is written as before", {
  x <- list(a = 1, b = "two", d = data.frame(x = c(0.1, 0.25)), t = as.POSIXlt("2024-01-02 03:04:05",
                                                                                tz = "UTC"))
  expect_identical(
    as.character(as_json(x)),
    as.character(jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE, null = "null",
                                  na = "null", force = TRUE, digits = NA)))
})

# ---------------------------------------------------------------------------
# r2-export-serialization-fidelity-06: auto_unbox wrote a field that lists
# things as a scalar when it held one: a one-recipe comparison's `recipes` was
# a string, a two-recipe one's an array.
# ---------------------------------------------------------------------------

test_that("a trace's fields that list things are arrays at every length", {
  cl <- gr_mock_client(function(m, p) "45.2 million")
  cmp <- quiet(gr_compare(readgpt_example(), "What was revenue?", "fast", client = cl,
                          allow_duplicates = TRUE))
  j <- jsonlite::parse_json(as_json(cmp$trace))
  expect_identical(j$meta$recipes, list("fast"))

  tr <- gr_trace()
  readgpt:::trace_note(tr, "retrieve.rank", list(k = 1L, top_scores = 0.5))
  readgpt:::trace_note(tr, "ingest", list(blocks = 3L, clean_steps = "unicode"))
  j <- jsonlite::parse_json(as_json(tr))
  expect_identical(j$steps[[1]]$detail$top_scores, list(0.5))
  expect_identical(j$steps[[1]]$detail$k, 1L)
  expect_identical(j$steps[[2]]$detail$clean_steps, list("unicode"))
})

# ---------------------------------------------------------------------------
# r2-export-serialization-fidelity-07: a trace parsed with jsonlite's default
# simplification has its steps as a data frame, and the replay walked its
# columns, found no calls, and said the run had made none. The JSON text of a
# trace or an answer was taken for a file name.
# ---------------------------------------------------------------------------

test_that("a trace parsed with jsonlite's defaults is refused, saying why", {
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  ans <- quiet(answer_document(readgpt_example(), "What was revenue?", "thorough", client = cl))
  f <- withr::local_tempfile(fileext = ".json")
  gr_trace_save(ans$trace, f)
  parsed <- jsonlite::fromJSON(f)
  expect_s3_class(parsed$steps, "data.frame")
  err <- tryCatch(gr_replay_client(parsed), error = function(e) e)
  expect_s3_class(err, "gr_replay_unreadable")
  expect_match(conditionMessage(err), "simplifyVector = FALSE", fixed = TRUE)
  expect_false(grepl("no recorded model calls", conditionMessage(err), fixed = TRUE))
  # Parsed as the message says, it replays.
  again <- quiet(answer_document(readgpt_example(), "What was revenue?", "thorough",
                                 client = gr_replay_client(jsonlite::fromJSON(f, simplifyVector = FALSE))))
  expect_identical(again$answer, ans$answer)
})

test_that("the JSON text of a trace or an answer replays", {
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  ans <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = cl))
  js <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = cl,
                              return = "json"))
  for (src in list(as_json(ans$trace), js, as.character(js))) {
    rp <- gr_replay_client(src)
    again <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = rp))
    expect_identical(again$answer, ans$answer)
    expect_identical(rp$stats()$misses, 0L)
  }
  expect_error(gr_replay_client(list(answer = "x")), class = "gr_replay_unreadable")
  expect_error(gr_replay_client("{ not json"), class = "gr_replay_unreadable")
})

# ---------------------------------------------------------------------------
# contracts-12: gr_embed()'s help said the client's embed function came before
# gr_options("embedder"); the code, gr_options() and the tests say the option
# wins.
# ---------------------------------------------------------------------------

test_that("gr_embed()'s help gives the order the code uses", {
  local_registries()
  gr_options(embedder = "lexical")
  cl <- gr_mock_client()
  e <- gr_embed(cl, c("a b", "c d"))
  expect_identical(attr(e, "embedding_source"), "lexical")
  expect_length(cl$embeds(), 0L)
  path <- testthat::test_path("..", "..", "R", "core-embed.R")
  if (!file.exists(path)) skip("package source not available")
  src <- paste(sub("^#'\\s*", "", readLines(path, warn = FALSE)), collapse = " ")
  expect_false(grepl("Defaults to the embed function supplied with", src, fixed = TRUE))
  expect_true(grepl("wins over the client's own embed function", src, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# state-concurrency-11: future_lapply(future.seed = TRUE) moved the caller's
# random number stream on, so a draw after a parallel read differed from the
# same draw after a sequential one.
# ---------------------------------------------------------------------------

test_that("a parallel batch leaves the caller's random numbers alone", {
  skip_if_no_future6()
  set.seed(42)
  quiet(readgpt:::gr_lapply(1:3, function(i, trace) i, parallel = TRUE, workers = 2,
                            trace = gr_trace()))
  after <- stats::runif(1)
  set.seed(42)
  expect_identical(after, stats::runif(1))
})

# ---------------------------------------------------------------------------
# tokenize-embed-11: an error in any item of a parallel batch was raised from
# inside future_lapply(), so every worker's trace was lost, calls already made
# included. Run sequentially, the trace kept every call before the error.
# ---------------------------------------------------------------------------

test_that("an item that fails in a parallel batch keeps every trace", {
  skip_if_no_future6()
  run <- function(parallel) {
    tr <- gr_trace()
    err <- tryCatch(quiet(readgpt:::gr_lapply(1:6, function(i, trace) {
      readgpt:::trace_record(trace, sprintf("item%d", i), list(),
                             readgpt:::gr_result(TRUE, "x", model = "mock-model",
                                                 usage = list(input = 10L, output = 2L)))
      if (i == 4L) stop(structure(class = c("r6_boom", "error", "condition"),
                                  list(message = "boom on item 4", call = NULL)))
      i
    }, parallel = parallel, workers = 2, trace = tr)), error = function(e) e)
    list(err = err, tr = tr)
  }
  seq <- run(FALSE)
  par <- run(TRUE)
  expect_s3_class(par$err, "r6_boom")
  expect_identical(conditionMessage(par$err), "boom on item 4")
  labels <- function(tr) vapply(tr$steps, function(s) s$label, character(1))
  # The calls made before the failure are in the trace, as they are when the
  # batch runs in this process; a worker stops at its failure too.
  expect_identical(labels(seq$tr), sprintf("item%d", 1:4))
  expect_true(all(sprintf("item%d", 1:4) %in% labels(par$tr)))
  expect_false(any(c("item5", "item6") %in% labels(par$tr)))
})
