# test-corpus.R -- one question over many documents, and what a run really cost.
#
# The loop itself is trivial. What is being tested is the four things that make
# it worth having: that one bad document costs you one row and not the run, that
# a budget is per document rather than shared, that a store makes a restart free,
# and that the cost column means what it says.

# A tiny corpus on disk, plus one file no extractor claims.
local_corpus <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  writeLines("Revenue was 45.2 million dollars in fiscal 2024.", file.path(d, "a.txt"))
  writeLines("Revenue was 51.8 million dollars in fiscal 2025.", file.path(d, "b.txt"))
  writeLines("# Notes\n\nRevenue was 12.0 million dollars.", file.path(d, "c.md"))
  writeLines("not a document", file.path(d, "ignore.xyz"))
  d
}

test_that("one row per document, in the order given", {
  local_registries()
  d <- local_corpus()
  out <- quiet(gr_read_many(file.path(d, c("b.txt", "a.txt")), "What was revenue?",
                            "fast", client = mock_echo("an answer")))
  expect_s3_class(out, "gr_corpus")
  expect_identical(out$summary$document, c("b.txt", "a.txt"))
  expect_identical(out$summary$status, c("ok", "ok"))
  expect_true(all(out$summary$answer == "an answer"))
  expect_length(out$answers, 2L)
  expect_output(print(out), "2 document\\(s\\)")
})

test_that("a directory is expanded by the extractor registry, not by a fixed list", {
  local_registries()
  d <- local_corpus()
  out <- quiet(gr_read_many(d, "Q?", "fast", client = mock_echo()))
  expect_identical(sort(out$summary$document), c("a.txt", "b.txt", "c.md"))

  # Register an extractor for .xyz and the same directory now yields four.
  gr_register_extractor("xyz", extensions = "xyz",
                        fn = function(path, spec) list(text = "registered content"),
                        description = "test")
  out2 <- quiet(gr_read_many(d, "Q?", "fast", client = mock_echo()))
  expect_identical(sort(out2$summary$document), c("a.txt", "b.txt", "c.md", "ignore.xyz"))
})

test_that("recursion into subdirectories is opt-in", {
  local_registries()
  d <- local_corpus()
  dir.create(file.path(d, "sub"))
  writeLines("Revenue was 9.9 million dollars.", file.path(d, "sub", "deep.txt"))

  flat <- quiet(gr_read_many(d, "Q?", "fast", client = mock_echo()))
  expect_false("deep.txt" %in% flat$summary$document)
  deep <- quiet(gr_read_many(d, "Q?", "fast", client = mock_echo(), recursive = TRUE))
  expect_true("deep.txt" %in% deep$summary$document)
})

test_that("an empty corpus is an error, not an empty result", {
  local_registries()
  expect_error(gr_read_many(character(0), "Q?", client = mock_echo()), class = "gr_no_sources")
  expect_error(gr_read_many(withr::local_tempdir(), "Q?", client = mock_echo()),
               class = "gr_no_sources")
  expect_error(gr_read_many("a.txt", "   ", client = mock_echo()), "non-empty")
})

# ---------------------------------------------------------------------------
# Isolation: the whole reason not to write the loop yourself
# ---------------------------------------------------------------------------

test_that("one unreadable document costs one row, not the run", {
  local_registries()
  d <- local_corpus()
  out <- quiet(gr_read_many(c(file.path(d, "a.txt"), "no-such-file.txt", file.path(d, "b.txt")),
                            "Q?", "fast", client = mock_echo()))
  expect_identical(out$summary$status, c("ok", "failed", "ok"))
  expect_true(is.na(out$summary$error[1]))
  expect_match(out$summary$error[2], "not found|No such|cannot")
  expect_true(is.na(out$summary$answer[2]))
  # The documents either side of the failure still produced answers.
  expect_false(any(is.na(out$summary$answer[c(1, 3)])))
})

test_that("a failed document is named by its path, not called inline text", {
  # source_label() calls anything that is not an existing file "<inline text>",
  # so three missing files produced three identical rows and no way to tell
  # which path failed.
  local_registries()
  out <- quiet(gr_read_many(c("missing/report.pdf", "also-gone.txt"),
                            "Q?", "fast", client = mock_echo()))
  expect_identical(out$summary$document, c("report.pdf", "also-gone.txt"))
  expect_identical(out$summary$status, c("failed", "failed"))

  # Raw text is still inline text, and is still read.
  txt <- quiet(gr_read_many("Revenue was 45.2 million dollars, per the filing.",
                            "Q?", "fast", client = mock_echo()))
  expect_identical(txt$summary$document, "<inline text>")
  expect_identical(txt$summary$status, "ok")
})

test_that("on_error = 'stop' aborts", {
  local_registries()
  d <- local_corpus()
  expect_error(quiet(gr_read_many(c(file.path(d, "a.txt"), "no-such-file.txt"),
                                  "Q?", "fast", client = mock_echo(), on_error = "stop")))
})

test_that("documents with the same name are still distinguishable", {
  local_registries()
  d1 <- withr::local_tempdir(); d2 <- withr::local_tempdir()
  writeLines("Revenue was 1.0 million dollars.", file.path(d1, "report.txt"))
  writeLines("Revenue was 2.0 million dollars.", file.path(d2, "report.txt"))
  out <- quiet(gr_read_many(c(file.path(d1, "report.txt"), file.path(d2, "report.txt")),
                            "Q?", "fast", client = mock_echo()))
  expect_identical(anyDuplicated(out$summary$document), 0L)
  expect_length(out$answers, 2L)
})

test_that("each document gets its own budget, so a long one cannot starve the rest", {
  # Sharing one trace would make max_calls count earlier documents against later
  # ones, and the same document would answer differently depending on where it
  # sat in the corpus -- the bug gr_compare() had between recipes.
  local_registries()
  d <- local_corpus()
  gr_options(max_calls = 1L)
  out <- quiet(gr_read_many(file.path(d, c("a.txt", "b.txt", "c.md")), "Q?", "fast",
                            client = mock_echo()))
  expect_identical(out$summary$status, c("ok", "ok", "ok"))
  expect_true(all(out$summary$calls == 1L))
  # And the parent trace still saw all three.
  expect_identical(out$trace$calls, 3L)
})

test_that("the corpus ceiling stops the run and says which documents were skipped", {
  local_registries()
  gr_register_model("priced", context_window = 100000L, max_output = 4096L,
                    input_usd = 1000, output_usd = 1000)
  d <- local_corpus()
  cl <- gr_backend_client(function(m, p) "an answer", model = "priced")
  # suppressMessages, not quiet(): quiet() also suppresses warnings, and the
  # warning is what is under test here.
  expect_warning(
    out <- suppressMessages(gr_read_many(file.path(d, c("a.txt", "b.txt", "c.md")), "Q?",
                                         "fast", client = cl, model = "priced",
                                         max_total_usd = 1e-6)),
    class = "gr_corpus_cost_cap")
  expect_identical(out$summary$status[1], "ok")
  expect_true(all(out$summary$status[-1] == "skipped"))
  expect_match(out$summary$error[2], "ceiling")
  expect_length(cl$calls(), 1L)          # nothing was spent after the ceiling
})

test_that("keep_answers = FALSE returns the table without the objects", {
  local_registries()
  d <- local_corpus()
  out <- quiet(gr_read_many(d, "Q?", "fast", client = mock_echo(), keep_answers = FALSE))
  expect_identical(nrow(out$summary), 3L)
  expect_length(out$answers, 0L)
  expect_false(any(is.na(out$summary$answer)))   # the answers are still in the table
})

# ---------------------------------------------------------------------------
# The store
# ---------------------------------------------------------------------------

test_that("a second run restores from the store instead of re-reading", {
  local_registries()
  d <- local_corpus()
  st <- withr::local_tempdir()
  n <- 0L
  cl <- gr_mock_client(function(m, p) { n <<- n + 1L; "an answer" })

  first <- quiet(gr_read_many(d, "Q?", "fast", client = cl, store = st))
  spent <- n
  expect_gt(spent, 0L)
  expect_true(all(first$summary$status == "ok"))

  second <- quiet(gr_read_many(d, "Q?", "fast", client = cl, store = st))
  expect_identical(n, spent)                       # nothing re-read
  expect_true(all(second$summary$status == "restored"))
  expect_identical(second$summary$answer, first$summary$answer)
  expect_identical(second$trace$calls, 0L)         # and nothing spent this run
  expect_length(second$answers, 3L)
})

test_that("the store is keyed on everything that changes the result", {
  local_registries()
  d <- withr::local_tempdir()
  f <- file.path(d, "a.txt")
  writeLines("Revenue was 45.2 million dollars.", f)
  st <- withr::local_tempdir()
  n <- 0L
  cl <- gr_mock_client(function(m, p) { n <<- n + 1L; "an answer" })
  run <- function(...) quiet(gr_read_many(f, ..., client = cl, store = st))

  run("Q1?", "fast"); base <- n
  run("Q1?", "fast")
  expect_identical(n, base)                        # same job, restored

  run("a different question?", "fast")
  expect_gt(n, base); base <- n

  run("Q1?", "thorough")                           # different pipeline
  expect_gt(n, base); base <- n

  # An edited document is a new job, not a stale hit.
  Sys.sleep(1.1)
  writeLines("Revenue was 99.9 million dollars.", f)
  run("Q1?", "fast")
  expect_gt(n, base)
})

test_that("a corrupt store entry is re-read, not an error", {
  local_registries()
  d <- withr::local_tempdir()
  writeLines("Revenue was 45.2 million dollars.", file.path(d, "a.txt"))
  st <- withr::local_tempdir()
  cl <- mock_echo("an answer")
  quiet(gr_read_many(d, "Q?", "fast", client = cl, store = st))

  entry <- list.files(st, pattern = "\\.rds$", full.names = TRUE)
  expect_length(entry, 1L)
  writeBin(as.raw(c(0x00, 0x01, 0x02)), entry)

  expect_no_error(out <- quiet(gr_read_many(d, "Q?", "fast", client = cl, store = st)))
  expect_identical(out$summary$status, "ok")       # re-read, not restored

  # Garbage bytes are caught by readRDS itself, so they prove little. An entry
  # that DESERIALISES cleanly and is simply the wrong shape -- an older format,
  # a half-written record, something else's file -- is the case that needs the
  # explicit check.
  quiet(gr_read_many(d, "Q?", "fast", client = cl, store = st))
  entry <- list.files(st, pattern = "\\.rds$", full.names = TRUE)[1]
  saveRDS(list(format = 99L, row = "not a data frame"), entry)
  expect_no_error(out2 <- quiet(gr_read_many(d, "Q?", "fast", client = cl, store = st)))
  expect_identical(out2$summary$status, "ok")
})

test_that("writing the store leaves no temporary files", {
  local_registries()
  d <- local_corpus()
  st <- withr::local_tempdir()
  quiet(gr_read_many(d, "Q?", "fast", client = mock_echo(), store = st))
  expect_length(list.files(st, pattern = "tmp"), 0L)
  expect_length(list.files(st, pattern = "\\.rds$"), 3L)
})

# ---------------------------------------------------------------------------
# gr_trace_cost
# ---------------------------------------------------------------------------

test_that("cost counts only the calls that were actually issued", {
  local_registries()
  gr_register_model("priced", context_window = 100000L, max_output = 4096L,
                    input_usd = 3, output_usd = 6)
  cache <- gr_cache(withr::local_tempdir())
  cl <- gr_cache_client(gr_backend_client(function(m, p) "an answer", model = "priced"), cache)

  paid <- gr_trace()
  invisible(gr_call(cl, "the question", model = "priced", trace = paid))
  cp <- gr_trace_cost(paid)
  expect_identical(nrow(cp), 1L)
  expect_identical(cp$model, "priced")
  expect_identical(cp$paid_calls, 1L)
  expect_gt(cp$usd, 0)

  free <- gr_trace()
  invisible(gr_call(cl, "the question", model = "priced", trace = free))
  cf <- gr_trace_cost(free)
  expect_identical(cf$calls, 1L)            # the call happened
  expect_identical(cf$paid_calls, 0L)       # and cost nothing
  expect_identical(cf$usd, 0)
  expect_identical(cf$paid_in, 0L)
  # The trace still reports the prompt size: shape is not the same as spend.
  expect_gt(gr_trace_summary(free)$tokens_in, 0L)
})

test_that("cost is reported per model, and an unpriced model is NA rather than free", {
  local_registries()
  gr_register_model("cheap", context_window = 100000L, max_output = 4096L,
                    input_usd = 1, output_usd = 1)
  gr_register_model("unpriced", context_window = 100000L, max_output = 4096L)
  tr <- gr_trace()
  cl <- gr_backend_client(function(m, p) "an answer", model = "cheap")
  invisible(gr_call(cl, "one", model = "cheap", trace = tr))
  invisible(gr_call(cl, "two", model = "unpriced", trace = tr))

  cost <- suppressWarnings(gr_trace_cost(tr))
  expect_identical(nrow(cost), 2L)
  expect_identical(cost$model, c("cheap", "unpriced"))
  expect_gt(cost$usd[cost$model == "cheap"], 0)
  expect_true(is.na(cost$usd[cost$model == "unpriced"]))
  # A total that silently dropped the unpriced model would be a lie.
  expect_true(is.na(sum(cost$usd)))
})

test_that("a trace with no model calls costs nothing and returns an empty frame", {
  cost <- gr_trace_cost(gr_trace())
  expect_identical(nrow(cost), 0L)
  expect_identical(sum(cost$usd), 0)
  expect_error(gr_trace_cost("not a trace"))
})
