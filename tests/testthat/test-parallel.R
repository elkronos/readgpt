# test-parallel.R -- parallel execution must not change what a run reports.
#
# `parallel = TRUE` was a speed flag that silently disabled the accounting. A
# worker is a separate process, so the trace it was handed was a copy and every
# call it recorded went away with it: a six-chunk map_reduce reported one call
# instead of seven, and gr_trace_cost() under-reported the bill in proportion to
# how parallel the run was. The answer was right, which is what made it hard to
# notice.
#
# There were no tests for `parallel = TRUE` in the suite at all. These are them.

skip_if_no_future <- function() {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
}

para_doc <- function(n = 6) {
  f <- tempfile(fileext = ".txt")
  writeLines(paste(sprintf("Paragraph %d. Revenue in region %d was %d million dollars this year.",
                           seq_len(n), seq_len(n), 40 + seq_len(n)), collapse = "\n\n"), f)
  f
}

# Everything the trace claims about a run, in one comparable row.
trace_shape <- function(ans) {
  s <- gr_trace_summary(ans$trace)
  cost <- gr_trace_cost(ans$trace)
  list(calls = as.integer(s$calls), steps = as.integer(s$steps),
       cached = as.integer(s$cached),
       tokens_in = as.integer(s$tokens_in), tokens_out = as.integer(s$tokens_out),
       usd = sum(cost$usd), labels = sort(vapply(ans$trace$steps,
                                                 function(x) as_chr1_p(x$label), character(1))))
}
as_chr1_p <- function(x) if (length(x)) as.character(x)[1] else NA_character_

read_both <- function(recipe, f, ...) {
  out <- lapply(c(FALSE, TRUE), function(par) {
    cl <- gr_mock_client(function(messages, params) "Revenue was 45.2 million dollars.")
    quiet(answer_document(f, "What was revenue?", recipe, client = cl,
                          max_tokens = 40, parallel = par, ...))
  })
  names(out) <- c("seq", "par")
  out
}

test_that("a parallel run reports exactly what the same sequential run reports", {
  skip_if_no_future()
  f <- para_doc()
  # Comparing the two paths to each other is necessary and NOT sufficient: a
  # change that loses the per-chunk calls on BOTH paths leaves them agreeing at
  # one call each, and the comparison passes. So each recipe also states, in
  # absolute terms, how many per-chunk calls its trace has to contain.
  cases <- list(list(recipe = "thorough", label = "map.answer",  least = 6L),
                list(recipe = "precise",  label = "skim.extract", least = 8L))
  for (case in cases) {
    both <- read_both(case$recipe, f)
    count <- function(a) sum(vapply(a$trace$steps, function(s) as_chr1_p(s$label),
                                    character(1)) == case$label)
    expect_gte(count(both$seq), case$least)
    expect_gte(count(both$par), case$least)
    expect_identical(trace_shape(both$par), trace_shape(both$seq), info = case$recipe)
    # The answer is unchanged, which it was before the fix too -- the bug was in
    # the accounting, not the reading.
    expect_identical(both$par$answer, both$seq$answer, info = case$recipe)
  }
})

test_that("the merge tree records its calls from workers too", {
  # tree_merge() fans out over merge GROUPS, which only form when the findings
  # do not fit one prompt. A model with a small context window is what makes
  # more than one group, and therefore what exercises the parallel path at all.
  skip_if_no_future()
  local_registries()
  gr_register_model("small-ctx", context_window = 1200L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  f <- para_doc(10)
  shape <- lapply(c(FALSE, TRUE), function(par) {
    cl <- gr_mock_client(function(messages, params)
      paste(rep("A finding about revenue in one of the regions.", 12), collapse = " "))
    a <- quiet(answer_document(f, "What was revenue?", "thorough", client = cl,
                               model = "small-ctx", max_tokens = 40, parallel = par))
    labels <- vapply(a$trace$steps, function(s) as_chr1_p(s$label), character(1))
    list(merges = sum(grepl("^reduce\\.level", labels)), calls = a$trace$calls)
  })
  expect_gte(shape[[1]]$merges, 2L)          # the tree really did branch
  expect_identical(shape[[2]], shape[[1]])   # and parallel recorded the same
})

test_that("every call a worker makes reaches the parent trace", {
  skip_if_no_future()
  f <- para_doc(6)
  both <- read_both("thorough", f)
  s <- gr_trace_summary(both$par$trace)
  # Six per-chunk calls plus the merge. Before the fix this was 1.
  expect_gte(s$calls, 6L)
  expect_gt(s$tokens_in, 0L)
  expect_false(any(is.na(gr_trace_cost(both$par$trace)$usd)))
  labels <- vapply(both$par$trace$steps, function(x) as_chr1_p(x$label), character(1))
  expect_true(sum(labels == "map.answer") >= 6L)
})

test_that("the assembled trace reads in input order, not completion order", {
  # Workers finish in whatever order they finish. A trace whose steps were
  # numbered by completion would differ from run to run, and two runs of the
  # same work would not be comparable.
  skip_if_no_future()
  f <- para_doc(6)
  cl <- gr_mock_client(function(messages, params) "Revenue was 45.2 million dollars.")
  a <- quiet(answer_document(f, "What was revenue?", "thorough", client = cl,
                             max_tokens = 40, parallel = TRUE))
  b <- quiet(answer_document(f, "What was revenue?", "thorough",
                             client = gr_mock_client(function(m, p) "Revenue was 45.2 million dollars."),
                             max_tokens = 40, parallel = TRUE))
  lab <- function(x) vapply(x$trace$steps, function(s) as_chr1_p(s$label), character(1))
  expect_identical(lab(a), lab(b))
  expect_identical(vapply(a$trace$steps, function(s) as.integer(s$step), integer(1)),
                   seq_along(a$trace$steps))
})

test_that("gr_lapply() hands the trace to its function either way", {
  # The sequential path must pass it through too, or the two paths diverge in
  # the one respect this whole change is about.
  gl <- readgpt:::gr_lapply
  tr <- readgpt:::gr_trace()
  got <- gl(1:3, function(i, trace) identical(trace, tr), parallel = FALSE, trace = tr)
  expect_true(all(unlist(got)))

  # And a NULL trace is not an error -- the OCR path has no trace to give.
  expect_identical(unlist(gl(1:3, function(i, trace) is.null(trace), parallel = FALSE)),
                   c(TRUE, TRUE, TRUE))
})

test_that("gr_lapply() absorbs worker traces, and is harmless without one", {
  skip_if_no_future()
  gl <- readgpt:::gr_lapply
  tr <- readgpt:::gr_trace()
  res <- gl(1:4, function(i, trace) {
    readgpt:::trace_record(trace, sprintf("w%d", i), list(),
                           readgpt:::gr_result(TRUE, "x", model = "mock-model",
                                               usage = list(input = 10L, output = 2L)))
    i * 2L
  }, parallel = TRUE, workers = 2, trace = tr)

  expect_identical(unlist(res), c(2L, 4L, 6L, 8L))     # results, in order
  expect_identical(tr$calls, 4L)                        # and every call recorded
  expect_identical(tr$tokens_in, 40L)
  expect_identical(tr$tokens_out, 8L)
  expect_identical(vapply(tr$steps, function(s) as_chr1_p(s$label), character(1)),
                   c("w1", "w2", "w3", "w4"))

  # No trace supplied: the work still runs and nothing errors.
  expect_identical(unlist(gl(1:3, function(i, trace) i + 1L, parallel = TRUE, workers = 2)),
                   c(2L, 3L, 4L))
})

test_that("a budget stop inside a worker is not lost", {
  # `budget_stop` is what marks an answer partial when the call cap bites. It is
  # set on the worker's trace, so it has to travel back with everything else.
  skip_if_no_future()
  gl <- readgpt:::gr_lapply
  tr <- readgpt:::gr_trace()
  gl(1:2, function(i, trace) { if (i == 2L) trace$budget_stop <- TRUE; i },
     parallel = TRUE, workers = 2, trace = tr)
  expect_true(tr$budget_stop)
})
