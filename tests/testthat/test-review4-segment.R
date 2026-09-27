# test-review4-segment.R -- segmenters whose requests fail without losing text.
#
# `contextual` with context_source = "llm" keeps a chunk without its header
# when the context call fails, and `proposition` keeps a batch as written when
# its call fails (or segments by sentence when every call does). No text is
# lost either way, so the failure is a recovered one: its trace error carries
# `recovered = TRUE`, and a document read in full is not labelled "failed".

# Three paragraphs of about 30 tokens, so each is its own contextual chunk at
# a 48-token cap. The second is the one a failing handler picks out.
r4_trial_text <- function() {
  c("We ran a randomised trial of the new treatment across nine sites, and 120 participants were enrolled in it.",
    "",
    "Adverse events were mild and self-limiting in the large majority of the cases that the sites reported.",
    "",
    "Costs per averted event were estimated at 3,140 dollars in the base case of the economic analysis.")
}

# Twelve paragraphs of about 75 tokens, so a 200-token proposition batch holds
# two of them and the document makes six batches.
r4_long_text <- function() {
  para <- function(i) {
    paste(rep(sprintf("Paragraph %d states that the trial enrolled %d patients and measured outcome %d.",
                      i, 100 + i, i), 3), collapse = " ")
  }
  vapply(1:12, para, character(1))
}

# The excerpt a context call is about. The prompt also carries the start of
# the whole document, so matching the prompt would match every call.
r4_excerpt <- function(body) sub("(?s).*<excerpt>\\n(.*)\\n</excerpt>.*", "\\1", body, perl = TRUE)

# Fails the context call for the excerpt about adverse events, and the
# proposition call for the batch holding paragraph 3; answers everything else.
r4_client <- function() {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    body <- messages[[length(messages)]]$content
    if (grepl("You situate an excerpt", sys, fixed = TRUE)) {
      if (grepl("Adverse events", r4_excerpt(body), fixed = TRUE)) stop("HTTP 503 service unavailable")
      return("This excerpt is part of the trial report.")
    }
    if (grepl("Decompose text", sys, fixed = TRUE)) {
      if (grepl("Paragraph 3 ", body, fixed = TRUE)) stop("HTTP 503 service unavailable")
      return('{"propositions": ["The trial enrolled 101 patients."]}')
    }
    "The trial enrolled 120 participants."
  })
}

r4_errors <- function(tr) {
  data.frame(label = vapply(tr$errors, function(e) as.character(e$label), character(1)),
             recovered = vapply(tr$errors, function(e) isTRUE(e$recovered), logical(1)),
             stringsAsFactors = FALSE)
}

r4_contextual <- function(parallel = FALSE) {
  gr_segment_spec("contextual", context_source = "llm", max_tokens = 48, parallel = parallel)
}

r4_proposition <- function() {
  gr_segment_spec("proposition", max_tokens = 400, proposition_batch_tokens = 200)
}

test_that("a failed context call that keeps its chunk is a recovered failure", {
  local_clean_cache()
  doc <- quiet(gr_ingest(paste(r4_trial_text(), collapse = "\n")))
  tr <- gr_trace()
  ch <- quiet(gr_segment(doc, r4_contextual(), client = r4_client(), trace = tr))
  # The chunk whose call failed is kept, whole, without a header.
  bare <- grepl("Adverse events", ch$chunks$text, fixed = TRUE)
  expect_true(any(bare))
  expect_false(any(startsWith(ch$chunks$text[bare], "[")))
  # Its failure is in the ledger, marked recovered, and so is the step.
  e <- r4_errors(tr)
  expect_identical(e$label, "segment.context")
  expect_identical(e$recovered, TRUE)
  failed <- Filter(function(s) identical(s$ok, FALSE), tr$steps)
  expect_length(failed, 1L)
  expect_true(isTRUE(failed[[1]]$recovered))
  # No other step claims to be a recovered failure.
  expect_identical(sum(vapply(tr$steps, function(s) isTRUE(s$recovered), logical(1))), 1L)
})

test_that("a proposition batch kept as written is a recovered failure", {
  local_clean_cache()
  doc <- quiet(gr_ingest(paste(r4_long_text(), collapse = "\n\n")))
  tr <- gr_trace()
  ch <- quiet(gr_segment(doc, r4_proposition(), client = r4_client(), trace = tr))
  expect_identical(ch$extra$batches_kept_as_written, 1L)
  expect_true(any(grepl("Paragraph 3 ", ch$chunks$text, fixed = TRUE)))
  e <- r4_errors(tr)
  expect_identical(e$label, "segment.proposition")
  expect_identical(e$recovered, TRUE)
})

test_that("every proposition batch failing, with the sentence fallback, is recovered", {
  local_clean_cache()
  doc <- quiet(gr_ingest(paste(r4_long_text(), collapse = "\n\n")))
  cl <- gr_mock_client(function(messages, params) stop("HTTP 503 service unavailable"))
  tr <- gr_trace()
  ch <- quiet(gr_segment(doc, r4_proposition(), client = cl, trace = tr))
  expect_identical(ch$method, "proposition->sentence")
  e <- r4_errors(tr)
  expect_true(nrow(e) > 1L)
  expect_true(all(e$label == "segment.proposition"))
  expect_true(all(e$recovered))
})

test_that("a segmenter marks only its own failures, not ones already in the trace", {
  local_clean_cache()
  doc <- quiet(gr_ingest(paste(r4_trial_text(), collapse = "\n")))
  cl <- r4_client()
  tr <- gr_trace()
  # An earlier failure in the same trace, of the same kind: not this run's.
  earlier <- gr_mock_client(function(messages, params) stop("HTTP 500"))
  invisible(gr_call(earlier, list(list(role = "system", content = "You situate an excerpt"),
                                  list(role = "user", content = "x")),
                    trace = tr, label = "segment.context"))
  ch <- quiet(gr_segment(doc, r4_contextual(), client = cl, trace = tr))
  e <- r4_errors(tr)
  expect_identical(e$label, c("segment.context", "segment.context"))
  expect_identical(e$recovered, c(FALSE, TRUE))
  expect_false(isTRUE(tr$steps[[1]]$recovered))
})

test_that("recovered steps are found by label when a batch's errors come from workers", {
  # A parallel batch's errors are absorbed from each worker's own trace, which
  # numbers its steps from 1, so an error's `step` does not locate the step in
  # the run's trace. What is marked is the failed step the batch added.
  tr <- gr_trace()
  readgpt:::trace_note(tr, "ingest")
  mark <- readgpt:::seg_trace_mark(tr)
  sub <- gr_trace()
  cl <- gr_mock_client(function(messages, params) stop("HTTP 503"))
  invisible(gr_call(cl, list(list(role = "user", content = "x")), trace = sub,
                    label = "segment.context"))
  readgpt:::trace_absorb(tr, sub)
  readgpt:::seg_mark_recovered(tr, mark, "segment.context")
  expect_true(isTRUE(tr$errors[[1]]$recovered))
  expect_false(isTRUE(tr$steps[[1]]$recovered))
  expect_true(isTRUE(tr$steps[[2]]$recovered))
  expect_identical(tr$steps[[2]]$label, "segment.context")
})

test_that("a parallel contextual segmentation marks its failed call recovered", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  local_registries()
  local_clean_cache()
  gr_options(workers = 2)
  doc <- quiet(gr_ingest(paste(r4_trial_text(), collapse = "\n")))
  tr <- gr_trace()
  readgpt:::trace_note(tr, "before")
  ch <- quiet(gr_segment(doc, r4_contextual(parallel = TRUE), client = r4_client(), trace = tr))
  e <- r4_errors(tr)
  expect_identical(e$label, "segment.context")
  expect_identical(e$recovered, TRUE)
  marked <- which(vapply(tr$steps, function(s) isTRUE(s$recovered), logical(1)))
  expect_length(marked, 1L)
  expect_identical(tr$steps[[marked]]$label, "segment.context")
  expect_false(tr$steps[[marked]]$ok)
})

test_that("gr_read_many() stores a document whose context call failed and was recovered", {
  local_clean_cache()
  d <- withr::local_tempdir()
  writeLines(r4_trial_text(), file.path(d, "a.txt"))
  st <- withr::local_tempdir()
  rec <- gr_recipe("ctx", segment = list(method = "contextual", context_source = "llm",
                                         max_tokens = 48),
                   read = "map_reduce")
  cl <- r4_client()
  failed <- 0L
  first <- withCallingHandlers(
    suppressMessages(gr_read_many(d, "How many participants?", rec, client = cl, store = st)),
    gr_document_failed = function(w) { failed <<- failed + 1L; invokeRestart("muffleWarning") },
    warning = function(w) invokeRestart("muffleWarning"))
  # Read in full: every excerpt reached the reader, one without its header.
  expect_identical(failed, 0L)
  expect_identical(first$summary$status, "ok")
  expect_true(is.na(first$summary$error))
  # The failed request is still in the ledger.
  e <- r4_errors(first$trace)
  expect_identical(e$recovered[e$label == "segment.context"], TRUE)
  expect_length(list.files(st, pattern = "\\.rds$"), 1L)
  # So a later run restores it rather than reading it again.
  cl$reset()
  again <- quiet(gr_read_many(d, "How many participants?", rec, client = cl, store = st))
  expect_identical(again$summary$status, "restored")
  expect_length(cl$calls(), 0L)
})

test_that("gr_read_many() stores a document with a proposition batch kept as written", {
  local_clean_cache()
  d <- withr::local_tempdir()
  writeLines(paste(r4_long_text(), collapse = "\n\n"), file.path(d, "a.txt"))
  st <- withr::local_tempdir()
  rec <- gr_recipe("prop", segment = list(method = "proposition", max_tokens = 400,
                                          proposition_batch_tokens = 200),
                   read = "map_reduce")
  out <- quiet(gr_read_many(d, "How many patients?", rec, client = r4_client(), store = st))
  expect_identical(out$summary$status, "ok")
  expect_true(is.na(out$summary$error))
  expect_length(list.files(st, pattern = "\\.rds$"), 1L)
})

test_that("a request that failed in the read still fails the document", {
  # The recovered mark is for the segmenter's own requests: a reading call
  # that failed still leaves the document unread in full.
  local_clean_cache()
  d <- withr::local_tempdir()
  writeLines(r4_trial_text(), file.path(d, "a.txt"))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("You situate an excerpt", messages[[1]]$content, fixed = TRUE)) {
      return("This excerpt is part of the trial report.")
    }
    stop("HTTP 503 service unavailable")
  })
  rec <- gr_recipe("ctx", segment = list(method = "contextual", context_source = "llm",
                                         max_tokens = 48),
                   read = "map_reduce")
  out <- quiet(gr_read_many(d, "How many participants?", rec, client = cl))
  expect_identical(out$summary$status, "failed")
  expect_false(any(r4_errors(out$trace)$recovered))
})

test_that("gr_extract() names no failure for a context call that was recovered", {
  local_clean_cache()
  d <- withr::local_tempdir()
  writeLines(r4_trial_text(), file.path(d, "a.txt"))
  f <- gr_fields(n = gr_field("Number of participants", type = "integer"))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("You situate an excerpt", messages[[1]]$content, fixed = TRUE)) {
      if (grepl("Adverse events", r4_excerpt(messages[[2]]$content), fixed = TRUE)) {
        stop("HTTP 503 service unavailable")
      }
      return("This excerpt is part of the trial report.")
    }
    ex <- messages[[length(messages)]]$content
    if (grepl("120", ex, fixed = TRUE)) {
      return(paste0('{"n":120,"n__quote":"We ran a randomised trial of the new treatment ',
                    'across nine sites, and 120 participants were enrolled in it."}'))
    }
    '{"n":null,"n__quote":null}'
  })
  x <- quiet(gr_extract(d, f, client = cl, recipe = "thorough", max_tokens = 48,
                        method = "contextual", context_source = "llm"))
  expect_identical(x$table$status, "ok")
  expect_identical(x$table$n, 120L)
  expect_true(is.na(x$table$error))
})
