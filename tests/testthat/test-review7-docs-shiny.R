# test-review7-docs-shiny.R
#
# The cross-file follow-ups that fell in the package documentation and the
# Shiny app: what an answer's `trace_steps` is, what a contextual chunk set's
# `extra` holds, and a history download that keeps its text in a C locale.

# The package source root, for files an installed copy leaves out; NULL when
# the tests run from one.
r7_source_file <- function(...) {
  f <- testthat::test_path("..", "..", ...)
  if (file.exists(f)) normalizePath(f) else NULL
}

# One `\item{`name`}{...}` of the roxygen in R/readgpt-package.R, as one line of
# text: the comment markers dropped and the white space run together, so a
# phrase may be matched across a line break. From the item's start to the next
# item, or to the end of the block's `\describe{}`.
r7_doc_item <- function(name) {
  f <- r7_source_file("R", "readgpt-package.R")
  skip_if(is.null(f), "the package source is not available")
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  x <- sub("^#' ?", "", x[startsWith(x, "#'")])
  txt <- gsub("\\s+", " ", paste(x, collapse = " "))
  open <- sprintf("\\item{`%s`}{", name)
  at <- regexpr(open, txt, fixed = TRUE)
  expect_true(at > 0, info = sprintf("no item for `%s`", name))
  rest <- substring(txt, at + nchar(open))
  end <- regexpr("\\item{", rest, fixed = TRUE)
  if (end > 0) substring(rest, 1L, end - 1L) else rest
}

# ---------------------------------------------------------------------------
# r2-export-serialization-fidelity-04 (read-core H7): the answer's
# `trace_steps` field is documented, and the shared trace is said to still
# hold every run.
# ---------------------------------------------------------------------------

test_that("the gr_answer docs say what trace_steps is and that a shared trace holds every run", {
  steps <- r7_doc_item("trace_steps")
  expect_match(steps, "Integer `c(first, last)`", fixed = TRUE)
  expect_match(steps, "the steps of `trace` this run made", fixed = TRUE)
  expect_match(steps, "[as_json()], `print()` and the audit report", fixed = TRUE)
  expect_match(steps, "does not carry other runs' prompts, calls or cost", fixed = TRUE)
  tr_item <- r7_doc_item("trace")
  expect_match(tr_item, "still holds every run recorded into it", fixed = TRUE)
  expect_match(tr_item, "`trace_steps` says which of its steps are this run's", fixed = TRUE)
  # What the field is, as the docs describe it.
  a <- new_answer("It says so.", "stuff", "Q?", integer(0), gr_trace())
  expect_true(is.integer(a$trace_steps))
  expect_named(a$trace_steps, c("first", "last"))
  expect_null(new_answer("It says so.", "stuff", "Q?", integer(0), NULL)$trace_steps)
})

test_that("an answer's record leaves out a run made on its trace after it, as documented", {
  cl <- gr_mock_client(function(messages, params) "It says so.")
  tr <- gr_trace()
  a1 <- quiet(answer_document("Public memo: revenue was 45.2 million in 2023.", "What was revenue?",
                              "fast", client = cl, trace = tr))
  quiet(answer_document("Second memo: the Sheffield site closed in May.", "What closed?",
                        "fast", client = cl, trace = tr))
  expect_false(grepl("Sheffield", as_json(a1), fixed = TRUE))
  expect_false(grepl("Sheffield", paste(utils::capture.output(print(a1)), collapse = "\n"),
                     fixed = TRUE))
  # The trace the answer points at is still the shared one, with both runs.
  expect_true(grepl("Sheffield", as_json(a1$trace), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# money-08 (segment): contextual's extras are in the gr_chunks docs.
# ---------------------------------------------------------------------------

test_that("the gr_chunks docs list contextual's extras", {
  extra <- r7_doc_item("extra")
  expect_match(extra, "`context_source` for `contextual`", fixed = TRUE)
  expect_match(extra, "`\"metadata\"` or `\"llm\"`", fixed = TRUE)
  expect_match(extra, "`blurbs_missing` (chunks left without a context line)", fixed = TRUE)
  expect_match(extra, paste("`blurbs_at_limit` (of those, how many were skipped at the run's",
                            "call or cost limit)"), fixed = TRUE)
  # And the chunk set carries them as described.
  local_clean_cache()
  txt <- paste(sprintf("Paragraph %d says site %d enrolled %d patients.", 1:6, 1:6, 10 * 1:6),
               collapse = "\n\n")
  meta <- quiet(gr_segment(txt, gr_segment_spec("contextual", max_tokens = 200)))
  expect_identical(meta$extra$context_source, "metadata")
  expect_null(meta$extra$blurbs_missing)
  cl <- gr_mock_client(function(messages, params) "This excerpt is about enrolment.")
  llm <- quiet(gr_segment(txt, gr_segment_spec("contextual", context_source = "llm",
                                                max_tokens = 200), client = cl))
  expect_identical(llm$extra$context_source, "llm")
  expect_identical(llm$extra$blurbs_missing, 0L)
  expect_identical(llm$extra$blurbs_at_limit, 0L)
  # "llm" asked for without a client records the one that ran.
  none <- quiet(gr_segment(txt, gr_segment_spec("contextual", context_source = "llm",
                                                 max_tokens = 200)))
  expect_identical(none$extra$context_source, "metadata")
})

# ---------------------------------------------------------------------------
# shiny-utf8 (records; residual of r2-locale-platform-portability-03): the
# history download is written as UTF-8 whatever the session's locale.
# ---------------------------------------------------------------------------

test_that("the app's history download keeps non-ASCII text in a C locale", {
  skip_if_not_installed("shiny")
  local_registries()
  local_clean_cache()
  gr_options(verbose = FALSE)
  app <- system.file("shiny", "app.R", package = "readgpt")
  skip_if(!nzchar(app), "the Shiny app is not available")
  dir <- withr::local_tempdir()
  writeLines(c("Revenue was 45.2 million dollars in fiscal 2024.", "",
               "Gross margin improved to 41 percent."), file.path(dir, "report.txt"))
  cl <- gr_mock_client(function(m, p) "Margin was \u2265 40 percent at Malm\u00f6.")
  e <- new.env(parent = globalenv())
  e$gr_client <- function(model = NULL, api_key = NULL, ...) cl
  e$shinyApp <- function(ui, server, ...) server
  if (!"package:shiny" %in% search()) {
    withr::defer(try(detach("package:shiny", character.only = TRUE), silent = TRUE))
  }
  withr::local_envvar(GPTREAD_DOC_ROOTS = normalizePath(dir))
  quiet(sys.source(app, envir = e, keep.source = FALSE))
  f <- normalizePath(file.path(dir, "report.txt"))
  # A session with no UTF-8: cron, a container, Rscript under LANG=C.
  withr::local_locale(c(LC_CTYPE = "C"))
  skip_if(isTRUE(l10n_info()[["UTF-8"]]), "a C locale could not be set")
  question <- "Was the margin \u2265 40 percent?"
  shiny::testServer(e$server, {
    session$setInputs(api_key = "not-a-real-key", model = "gpt-4o-mini", file = f,
                      clean_preset = "standard", clean_steps = NULL, ocr = "auto",
                      segmenter = "paragraph", max_tokens = 1200, overlap = 0,
                      min_tokens = 0, parallel = FALSE, max_cost = 2, readers = "stuff",
                      top_k = 6, cite = FALSE, temperature = NA)
    session$setInputs(question = question, go = 1)
    expect_length(history(), 1L)
    p <- output$dl
    bytes <- readBin(p, "raw", file.size(p))
    # It was written through the native encoding: "<U+2265>" for the sign.
    expect_length(grepRaw("<U+", bytes, fixed = TRUE), 0L)
    has <- function(s) length(grepRaw(charToRaw(enc2utf8(s)), bytes, fixed = TRUE)) > 0L
    expect_true(has(question))
    expect_true(has("Margin was \u2265 40 percent at Malm\u00f6."))
    txt <- rawToChar(bytes)
    Encoding(txt) <- "UTF-8"
    expect_true(validUTF8(txt))
    h <- jsonlite::parse_json(txt)
    expect_identical(charToRaw(enc2utf8(h[[1]]$question)), charToRaw(enc2utf8(question)))
  })
})
