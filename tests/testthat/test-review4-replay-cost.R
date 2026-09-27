# test-review4-replay-cost.R -- the fourth pass on replay and cost: handoffs
# from the fixers of the earlier passes, for changes that fell in core-replay.R,
# core-progress.R and corpus.R.
#
# Each block names the finding and says what the old behaviour was.

# Stands in for an OpenAI-shaped server: chat completions always answer, with a
# usage block of about a token per four characters; the embeddings endpoint
# answers with `embed_status` (a gateway without embeddings: 404).
r4_endpoint <- function(embed_status = 404L, env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$embed <- 0L
  seen$chat <- 0L
  respond <- function(url, status, out) {
    structure(list(url = url, status_code = status,
                   headers = structure(list(`content-type` = "application/json; charset=utf-8"),
                                       class = c("insensitive", "list")),
                   content = charToRaw(as.character(jsonlite::toJSON(out, auto_unbox = TRUE)))),
              class = "response")
  }
  testthat::local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      if (grepl("/embeddings$", url)) {
        seen$embed <- seen$embed + 1L
        return(respond(url, embed_status, list(error = list(message = "no such model"))))
      }
      seen$chat <- seen$chat + 1L
      all <- paste(vapply(body$messages, function(m) as.character(m$content), ""),
                   collapse = "\n")
      respond(url, 200L, list(
        id = "r4", object = "chat.completion", model = body$model,
        choices = list(list(index = 0, finish_reason = "stop",
                            message = list(role = "assistant",
                                           content = "Revenue was 45.2 million dollars."))),
        usage = list(prompt_tokens = ceiling(nchar(all) / 4), completion_tokens = 7)))
    },
    .package = "httr", .env = env)
  seen
}

r4_client <- function(embedding_model = "bad-embed") {
  gr_client(api_key = "sk-test", base_url = "https://r4.invalid/v1", api = "chat",
            model = "gpt-4o", embedding_model = embedding_model, max_retries = 0L)
}

r4_doc <- function(n = 40L) {
  paste(sprintf("Paragraph %d describes operations in region %d and the staff there.",
                seq_len(n), seq_len(n)), collapse = "\n\n")
}

# A trace holding one priced chat request and one embeddings request to an
# unregistered model that failed with nothing sent, as gr_embed() records a 404.
r4_trace_with_free_failure <- function() {
  tr <- gr_trace()
  readgpt:::trace_record(tr, "map.answer", list(list(role = "user", content = "q")),
                         gr_result(TRUE, text = "a", model = "gpt-4o",
                                   usage = list(input = 1000L, output = 10L)))
  readgpt:::trace_record(tr, "embed.request", list(list(role = "user", content = "one")),
                         gr_result(FALSE, error = "HTTP 404: no such model", status = 404L,
                                   model = "bad-embed"),
                         embedding = TRUE)
  tr
}

# ---------------------------------------------------------------------------
# verify H2 (cross-3): gr_replay_client() took its own model from the model
# most recorded calls asked for. A skim, hierarchical or rerank read with a
# cheaper skim_model or summary_model makes most of its calls under that model,
# so the replay client's model was the cheap one; the replayed read, naming no
# model, asked for it on the answer call too, found nothing recorded and
# stopped with gr_replay_miss. The pre-flight note records the model the read
# asked for, and the replay client now takes it from there.
# ---------------------------------------------------------------------------

test_that("a read with a cheaper skim or summary model replays from its file", {
  local_registries()
  local_clean_cache()
  ch <- quiet(gr_segment(gr_ingest(sample_doc(3, 3)), list(method = "paragraph", max_tokens = 120)))
  q <- "How many participants?"
  specs <- list(list(reader = "skim", skim_model = "gpt-4o-mini"),
                list(reader = "hierarchical", summary_model = "gpt-4o-mini"),
                list(reader = "rerank", skim_model = "gpt-4o-mini"))
  for (spec in specs) {
    live <- quiet(gr_read(ch, q, mock_echo(), spec))
    asked <- vapply(Filter(function(s) !identical(s$kind, "local"), live$trace$steps),
                    function(s) as.character(s$params$model), character(1))
    # The premise: most calls went to the cheap model, not the client's.
    expect_gt(sum(asked == "gpt-4o-mini"), sum(asked == "mock-model"))
    f <- withr::local_tempfile(fileext = ".json")
    gr_trace_save(live$trace, f)
    rp <- gr_replay_client(f)
    expect_identical(rp$model, "mock-model", info = spec$reader)
    again <- quiet(gr_read(ch, q, rp, spec))
    expect_identical(rp$stats()$misses, 0L, info = spec$reader)
    expect_identical(again$answer, live$answer, info = spec$reader)
    # The same from the trace itself, not only from a file.
    expect_identical(gr_replay_client(live$trace)$model, "mock-model", info = spec$reader)
  }
})

test_that("a recording without the note, or with conflicting notes, falls back as before", {
  local_registries()
  local_clean_cache()
  ch <- quiet(gr_segment(gr_ingest(sample_doc(3, 3)), list(method = "paragraph", max_tokens = 120)))
  live <- quiet(gr_read(ch, "How many participants?", mock_echo(),
                        list(reader = "skim", skim_model = "gpt-4o-mini")))
  obj <- readgpt:::trace_as_list(live$trace)
  pf <- which(vapply(obj$steps, function(s) identical(s$label, "preflight"), logical(1)))
  expect_length(pf, 1L)

  # An older trace: no `model` on the pre-flight note. The most frequent model.
  old <- obj
  old$steps[[pf]]$detail$model <- NULL
  expect_identical(gr_replay_client(old)$model, "gpt-4o-mini")

  # A read whose settings named its model asks for it again on replay, so its
  # note says nothing about the client's model.
  named <- obj
  named$steps[[pf]]$detail$settings <- list(model = "mock-model")
  expect_identical(gr_replay_client(named)$model, "gpt-4o-mini")

  # Reads through clients with different models: no one answer, so the guess.
  two <- obj
  other <- obj$steps[[pf]]
  other$detail$model <- "another-model"
  two$steps <- c(two$steps, list(other))
  expect_identical(gr_replay_client(two)$model, "gpt-4o-mini")

  # Both reads following the same client agree, and that model is taken.
  same <- obj
  same$steps <- c(same$steps, list(obj$steps[[pf]]))
  expect_identical(gr_replay_client(same)$model, "mock-model")
})

# ---------------------------------------------------------------------------
# trace H2: gr_trace_cost() priced a request that failed and sent no tokens by
# its model. An embeddings request to an unregistered embedding model that a
# gateway without embeddings answered 404 then made the whole run's cost NA,
# and a corpus row's cost_usd NA with it. Before the fixes such a request was
# never recorded and the run's cost was known. as.data.frame.gr_trace() already
# priced the row at 0; gr_trace_cost() now agrees with it.
# ---------------------------------------------------------------------------

test_that("a request that failed without sending tokens costs nothing in gr_trace_cost()", {
  local_registries()
  tr <- r4_trace_with_free_failure()
  cost <- gr_trace_cost(tr)
  expect_identical(cost$model, c("bad-embed", "gpt-4o"))
  expect_identical(cost$usd[cost$model == "bad-embed"], 0)
  expect_identical(cost$calls[cost$model == "bad-embed"], 1L)
  expect_identical(cost$paid_calls[cost$model == "bad-embed"], 1L)
  expect_equal(sum(cost$usd), gr_estimate_cost("gpt-4o", 1000L, 10L))
  # The two accounts agree.
  expect_equal(sum(cost$usd), sum(as.data.frame(tr)$usd))
  expect_match(readgpt:::format_trace_cost(tr), "^\\$0\\.0026 across")
  cc <- readgpt:::corpus_cost(tr)
  expect_false(is.na(cc$usd))
  expect_length(cc$unpriced_embed, 0L)
  expect_length(cc$unpriced, 0L)
})

test_that("an unpriced model is still unknown whenever it may have cost something", {
  local_registries()
  msg <- list(list(role = "user", content = "q"))
  # A failed request that DID send tokens.
  tr <- r4_trace_with_free_failure()
  readgpt:::trace_record(tr, "embed.request", msg,
                         gr_result(FALSE, error = "HTTP 500", status = 500L, model = "bad-embed",
                                   usage = list(input = 12L, output = 0L)),
                         embedding = TRUE)
  expect_true(is.na(gr_trace_cost(tr)$usd[1]))

  # A cached request of an unpriced model, beside a free failure of it: the
  # cached request is NA as it always was.
  tr <- r4_trace_with_free_failure()
  readgpt:::trace_record(tr, "embed.request", msg,
                         gr_result(TRUE, text = "", model = "bad-embed", cached = TRUE,
                                   usage = list(input = 5L, output = 0L)),
                         embedding = TRUE)
  expect_true(is.na(gr_trace_cost(tr)$usd[1]))
  expect_true(is.na(sum(as.data.frame(tr)$usd)))

  # A failed request whose count is unknown is not a request that sent nothing.
  tr <- r4_trace_with_free_failure()
  tr$steps[[2]]$tokens <- list(input = NA_integer_, output = NA_integer_)
  expect_true(is.na(gr_trace_cost(tr)$usd[1]))

  # A free failure of a PRICED model costs 0, as before.
  tr <- gr_trace()
  readgpt:::trace_record(tr, "map.answer", msg,
                         gr_result(FALSE, error = "HTTP 503", status = 503L, model = "gpt-4o"))
  expect_identical(gr_trace_cost(tr)$usd, 0)
})

test_that("a trace read back from a file prices a failed empty request at 0 too", {
  local_registries()
  tr <- r4_trace_with_free_failure()
  back <- jsonlite::fromJSON(as.character(as_json(tr)), simplifyVector = FALSE)
  rebuilt <- gr_trace()
  rebuilt$steps <- back$steps
  cost <- gr_trace_cost(rebuilt)
  expect_identical(cost$usd[cost$model == "bad-embed"], 0)
  expect_false(is.na(sum(cost$usd)))
})

test_that("a corpus on an endpoint without embeddings knows each document's cost", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- r4_endpoint(embed_status = 404L)
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.txt")
  b <- file.path(dir, "b.txt")
  writeLines(r4_doc(), a)
  writeLines(gsub("operations", "research", r4_doc()), b)
  warned <- character(0)
  out <- withCallingHandlers(
    suppressMessages(gr_read_many(c(a, b), "What was revenue?", recipe = "needle",
                                  client = r4_client("bad-embed"), max_total_usd = 100)),
    warning = function(w) {
      warned <<- c(warned, class(w))
      invokeRestart("muffleWarning")
    })
  expect_gt(seen$embed, 0L)
  expect_gt(seen$chat, 0L)
  # Every request to the embedding model failed, sending nothing.
  cost <- gr_trace_cost(out$trace)
  expect_identical(cost$usd[cost$model == "bad-embed"], 0)
  expect_gt(cost$usd[cost$model == "gpt-4o"], 0)
  # Each row's cost is known and adds up to the run's, and the ceiling is held
  # to the whole spend rather than to a floor under it.
  expect_false(anyNA(out$summary$cost_usd))
  expect_equal(sum(out$summary$cost_usd), sum(cost$usd))
  expect_false("gr_corpus_cost_floor" %in% warned)
  expect_false("gr_corpus_cost_unknown" %in% warned)
})

# ---------------------------------------------------------------------------
# trace H5: the progress line said "cost unknown" once any request went to a
# model with no price, including one that failed and sent nothing, so a 404 from
# a gateway without embeddings hid the spend of a run whose every paid request
# was priced.
# ---------------------------------------------------------------------------

test_that("the progress line keeps the spend through a failed empty request", {
  local_registries()
  local_mocked_bindings(progress_wanted = function() TRUE)
  tr <- r4_trace_with_free_failure()
  p <- readgpt:::progress_start(3L, "item", trace = tr)
  expect_false(is.null(p))
  expect_identical(readgpt:::progress_cost(p),
                   sprintf(", $%.4f spent", gr_estimate_cost("gpt-4o", 1000L, 10L)))

  # A request that sent tokens to an unpriced model still makes it unknown.
  readgpt:::trace_record(tr, "map.answer", list(list(role = "user", content = "q2")),
                         gr_result(TRUE, text = "a", model = "no-such-model",
                                   usage = list(input = 5L, output = 1L)))
  expect_identical(readgpt:::progress_cost(p), ", cost unknown")

  # And so does a failed request that sent tokens, from a fresh line.
  tr2 <- r4_trace_with_free_failure()
  readgpt:::trace_record(tr2, "embed.request", list(list(role = "user", content = "two")),
                         gr_result(FALSE, error = "HTTP 500", status = 500L, model = "bad-embed",
                                   usage = list(input = 3L, output = 0L)),
                         embedding = TRUE)
  p2 <- readgpt:::progress_start(3L, "item", trace = tr2)
  expect_identical(readgpt:::progress_cost(p2), ", cost unknown")
})

# ---------------------------------------------------------------------------
# trace H3 (cross-5): the gr_trace_cost() example said a run was priced by "the
# recipe's model, not by the mock that answered". Recipes carry no model now; a
# mock run is billed as the mock's own model, at no cost.
# ---------------------------------------------------------------------------

test_that("the gr_trace_cost() example describes what it shows", {
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  ans <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = cl))
  cost <- gr_trace_cost(ans$trace)
  expect_identical(cost$model, "mock-model")
  expect_identical(cost$usd, 0)
  path <- testthat::test_path("..", "..", "R", "corpus.R")
  if (!file.exists(path)) skip("package source not available")
  src <- readLines(path, warn = FALSE)
  expect_false(any(grepl("the recipe's model", src, fixed = TRUE)))
  expect_true(any(grepl("a mock's own model, at no cost", src, fixed = TRUE)))
})
