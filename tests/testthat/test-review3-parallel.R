# test-review3-parallel.R -- parallel batches, after the third review.
#
# Each block names the defect it guards against. Most start worker processes
# and need the future packages, so they are skipped without them.

skip_if_no_future <- function() {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
}

# Puts `vals` in the global environment for the length of a test, the way a
# script's top level would, and removes them afterwards. A function among
# them is given the global environment as its home, as one written there has.
local_top_level <- function(vals, env = parent.frame()) {
  for (nm in names(vals)) {
    v <- vals[[nm]]
    if (is.function(v)) environment(v) <- globalenv()
    assign(nm, v, envir = globalenv())
  }
  withr::defer(rm(list = names(vals), envir = globalenv()), envir = env)
}

# A numeric vector of about `mb` MiB. Not a sequence: R stores 1:n in a few
# bytes, and so does its serialised form.
megabytes <- function(mb) stats::runif(mb * 1024^2 / 8)

# A document of 20 short paragraphs, which a proposition segmentation at 200
# tokens a batch sends as several requests.
par_doc <- function() {
  quiet(gr_ingest(paste(sprintf("Paragraph %d states that site %d enrolled %d adults over the year.",
                                1:20, 1:20, 10 * (1:20)), collapse = "\n\n")))
}

prop_spec <- function() {
  list(method = "proposition", max_tokens = 400, proposition_batch_tokens = 200, parallel = TRUE)
}

# ---------------------------------------------------------------------------
# segment-1: only what a worker runs sends workspace globals, and only once.
# ---------------------------------------------------------------------------

test_that("a workspace object no batch uses is not sent to workers", {
  skip_if_no_future()
  local_registries()
  # A registered embedder over a large workspace matrix. Embedding happens in
  # this process; no batch calls it. Its matrix went to every worker, twice,
  # and past this limit the whole segmentation aborted with "Will not launch
  # future due to the size of the globals".
  local_top_level(list(review3_emb_big = megabytes(30)))
  emb <- function(texts, params) matrix(review3_emb_big[seq_len(4 * length(texts))], ncol = 4)
  environment(emb) <- globalenv()
  gr_register_embedder("review3_local", emb)
  withr::local_options(future.globals.maxSize = 20 * 1024^2)
  gr_options(workers = 2)
  cl <- gr_mock_client(function(m, p)
    sprintf('{"propositions": ["Process %d wrote this."]}', Sys.getpid()))
  ch <- NULL
  expect_no_warning(ch <- suppressMessages(gr_segment(par_doc(), prop_spec(), client = cl)))
  expect_identical(ch$method, "proposition")
  # The batches ran in workers, not here.
  pids <- as.integer(sub("Process ([0-9]+) wrote this.", "\\1",
                         unlist(strsplit(ch$chunks$text, "\n", fixed = TRUE))))
  expect_false(anyNA(pids))
  expect_false(any(pids == Sys.getpid()))
  expect_identical(length(cl$calls()), ch$trace$calls)

  # What a worker runs is the client's handler and the tokenizer in use.
  st <- readgpt:::gr_state
  old <- st$tokenizers
  withr::defer(st$tokenizers <- old)
  tok <- function(x) nchar(x)
  st$tokenizers <- list(review3_tok = tok)
  got <- readgpt:::worker_functions(cl, list(tokenizer = "review3_tok"))
  expect_identical(got$handler, cl$handler)
  expect_identical(got$tokenizer, tok)
  expect_null(readgpt:::worker_functions(cl, list(tokenizer = "heuristic"))$tokenizer)
  expect_null(readgpt:::worker_functions(NULL, list(tokenizer = "heuristic"))$handler)
})

test_that("what a handler needs from the workspace is sent once", {
  skip_if_no_future()
  local_registries()
  local_top_level(list(
    review3_table = megabytes(25),
    review3_reply = function() sprintf('{"propositions": ["Found %d in process %d."]}',
                                       length(review3_table), Sys.getpid())
  ))
  handler <- function(messages, params) review3_reply()
  environment(handler) <- globalenv()
  cl <- gr_mock_client(handler)
  # Room for the table once, not twice: it also travelled inside the function
  # each worker runs, whose frame still held everything gathered for the batch.
  withr::local_options(future.globals.maxSize = 40 * 1024^2)
  gr_options(workers = 2)
  ch <- NULL
  expect_no_warning(ch <- suppressMessages(gr_segment(par_doc(), prop_spec(), client = cl)))
  txt <- unlist(strsplit(ch$chunks$text, "\n", fixed = TRUE))
  expect_true(all(grepl(sprintf("^Found %d in process", length(review3_table)), txt)))
  pids <- as.integer(sub(".* in process ([0-9]+)\\.$", "\\1", txt))
  expect_false(any(pids == Sys.getpid()))

  # The function a worker runs holds only what an item needs.
  wrapped <- readgpt:::worker_item(function(item, trace) item, "", list(), list(), list(), NULL)
  expect_setequal(ls(environment(wrapped)), c("fn", "key", "opts", "regs", "parent_meta", "state"))
})

test_that("a batch whose globals pass future's size limit runs here instead of aborting", {
  skip_if_no_future()
  local_registries()
  # Every request the handler answers, wherever it runs, is written down.
  local_top_level(list(
    review3_huge = megabytes(25),
    review3_seen = withr::local_tempfile(),
    review3_answer = function() {
      cat(Sys.getpid(), "\n", file = review3_seen, append = TRUE)
      sprintf('{"propositions": ["Read %d in process %d."]}', length(review3_huge), Sys.getpid())
    }
  ))
  handler <- function(messages, params) review3_answer()
  environment(handler) <- globalenv()
  cache <- gr_cache(withr::local_tempdir())
  cl <- gr_cache_client(gr_mock_client(handler), cache)
  withr::local_options(future.globals.maxSize = 10 * 1024^2)
  gr_options(workers = 2)
  tr <- gr_trace()
  ch <- NULL
  # Before any fix a limit this small aborted the run ("Will not launch future
  # due to the size of the globals"); then the gathering of the handler's
  # globals refused it, with a warning that blamed the handler.
  w <- testthat::capture_warnings(
    ch <- suppressMessages(gr_segment(par_doc(), prop_spec(), client = cl, trace = tr)))
  expect_match(w, "could not send the batch to workers", all = FALSE)
  expect_false(any(grepl("Canceling all iterations", w, fixed = TRUE)))
  expect_identical(ch$method, "proposition")
  txt <- unlist(strsplit(ch$chunks$text, "\n", fixed = TRUE))
  expect_true(all(txt == sprintf("Read %d in process %d.", length(review3_huge), Sys.getpid())))
  # Each request was made once, here, and counted once: no worker had started
  # on the batch when it was refused.
  expect_gt(tr$calls, 1L)
  expect_identical(as.integer(readLines(review3_seen)), rep(Sys.getpid(), tr$calls))
  expect_identical(length(cl$calls()), tr$calls)
  expect_identical(gr_cache_stats(cache)$misses, tr$calls)
  expect_identical(gr_cache_stats(cache)$writes, tr$calls)
})

test_that("only future's size refusal is turned into a sequential run", {
  skip_if_no_future()
  local_registries()
  gr_options(max_calls = 100, max_cost_usd = NULL)
  cancel <- function() warning("Caught FutureError. Canceling all iterations ...", call. = FALSE)
  run <- function() readgpt:::gr_lapply(1:3, function(i, trace) i * 2, parallel = TRUE,
                                        workers = 2, trace = gr_trace())
  testthat::local_mocked_bindings(
    future_lapply = function(...) {
      cancel()
      stop(future::FutureError(paste0("Will not launch future due to the size of the globals ",
                                      "1.00 GiB exceeds 500.00 MiB. The total size of the 2 ",
                                      "globals exported is 1.00 GiB.")))
    }, .package = "future.apply")
  out <- NULL
  w <- testthat::capture_warnings(out <- suppressMessages(run()))
  expect_identical(unlist(out), c(2, 4, 6))
  expect_length(w, 1L)
  expect_match(w, "what it carries (1.00 GiB, against 500.00 MiB) is more than", fixed = TRUE)

  # Measured as the future is made, the refusal is a plain error.
  testthat::local_mocked_bindings(
    future_lapply = function(...) {
      stop(paste0("The total size of the 2 globals exported for future expression ('FUN()') is ",
                  "900.00 MiB. This exceeds the maximum allowed size 500.00 MiB per plan() ",
                  "argument 'maxSizeOfObjects'."))
    }, .package = "future.apply")
  w <- testthat::capture_warnings(out <- suppressMessages(run()))
  expect_identical(unlist(out), c(2, 4, 6))
  expect_match(w, "(900.00 MiB, against 500.00 MiB)", fixed = TRUE)

  # Any other failure is the caller's, with future.apply's warning as it was.
  testthat::local_mocked_bindings(
    future_lapply = function(...) {
      cancel()
      stop(future::FutureError("A worker died."))
    }, .package = "future.apply")
  expect_warning(expect_error(suppressMessages(run()), "A worker died", class = "FutureError"),
                 "Canceling all iterations")
  testthat::local_mocked_bindings(
    future_lapply = function(...) stop("A tokenizer failed."), .package = "future.apply")
  expect_error(suppressMessages(run()), "A tokenizer failed")
})

# ---------------------------------------------------------------------------
# segment-2: one worker does not count every call twice.
# ---------------------------------------------------------------------------

test_that("with one worker a mock's calls and a cache's counts are not doubled", {
  skip_if_no_future()
  local_registries()
  cache <- gr_cache(withr::local_tempdir())
  mk <- gr_mock_client(function(m, p) '{"propositions": ["Alpha is one.", "Beta is two."]}')
  cl <- gr_cache_client(mk, cache)
  gr_options(workers = 1)
  tr <- gr_trace()
  quiet(gr_segment(par_doc(), prop_spec(), client = cl, trace = tr))
  expect_gt(tr$calls, 1L)
  # 2x each: a one-worker plan runs its futures in this process, where the
  # client's log and counters were updated in place and then added again.
  expect_identical(length(mk$calls()), tr$calls)
  expect_identical(gr_cache_stats(cache)$misses, tr$calls)
  expect_identical(gr_cache_stats(cache)$writes, tr$calls)

  # An item that ran in this process is not added back, however it got here.
  gr_options(max_calls = 100, max_cost_usd = NULL)
  log <- new.env(parent = emptyenv())
  log$calls <- list()
  fake <- structure(list(.log = log), class = "gr_client")
  testthat::local_mocked_bindings(
    future_lapply = function(X, FUN, ...) lapply(X, FUN), .package = "future.apply")
  out <- suppressMessages(readgpt:::gr_lapply(1:3, function(i, trace) {
    log$calls <- c(log$calls, list(list(label = paste("item", i))))
    i
  }, parallel = TRUE, workers = 2, trace = gr_trace(), client = fake))
  expect_identical(unlist(out), 1:3)
  expect_identical(length(log$calls), 3L)
})
