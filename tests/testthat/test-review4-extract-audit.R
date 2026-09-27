# test-review4-extract-audit.R -- the last handoffs for extraction and the
# audit report: the value check using span_match()'s own boundary test, the
# first error an incomplete row names, and what the report says about a quote
# that is in the chunk word for word but does not state its value.

r4_file <- function(...) {
  f <- tempfile(fileext = ".txt")
  writeLines(c(...), f)
  f
}

r4_flat <- function(h) gsub("[[:space:]]+", " ", h)

r4_report <- function(...) {
  p <- tempfile(fileext = ".html")
  suppressMessages(gr_audit_report(p, ..., open = FALSE))
  r4_flat(paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
}

# ---------------------------------------------------------------------------
# H1: the value check agrees with span_match() on where a quote may stop
# ---------------------------------------------------------------------------

test_that("require_quote keeps a value whose quote stops before a superscript reference", {
  # PDF text keeps a superscript reference inline ("mortality12"). span_match()
  # verifies a sentence that stops before it; the value check had its own
  # boundary test, rejected it, and require_quote deleted the value.
  f <- gr_fields(n = gr_field("Number of participants", type = "integer"),
                 outcome = "The primary outcome")
  doc <- r4_file("The cohort—482 participants in all—was followed for two years.",
                 paste("The primary outcome was all-cause mortality12 at one year,",
                       "as specified in the protocol."))
  cl <- gr_mock_client(function(messages, params) {
    paste0('{"n":482,"outcome":"all-cause mortality at one year",',
           '"n__quote":"482 participants in all",',
           '"outcome__quote":"The primary outcome was all-cause mortality"}')
  })
  x <- quiet(gr_extract(doc, f, client = cl, recipe = "thorough", require_quote = TRUE))
  expect_identical(x$table$n, 482L)
  expect_identical(x$table$outcome, "all-cause mortality at one year")
  expect_identical(x$table$n_filled, 2L)
  expect_identical(x$table$n_unverified, 0L)
  expect_true(all(x$evidence$verified))

  bq <- readgpt:::quote_backs_value
  src <- "The primary outcome was all-cause mortality12 at one year."
  expect_true(bq("all-cause mortality at one year", "The primary outcome was all-cause mortality",
                 src, gr_field("outcome")))
  # The em dash is read as written, as span_match() reads it: not a minus sign.
  expect_true(bq(482L, "482 participants in all",
                 "The cohort—482 participants in all—was followed.",
                 gr_field("n", type = "integer")))
})

test_that("a clause of Thai or Khmer backs its value as span_match() verifies it", {
  # Scripts written without spaces between words: a clause starts and ends
  # between two letters, because there is nothing else to start or end at.
  thai <- paste0("การศึกษานี้",
                 "ดำเนินการใน",
                 "โรงพยาบาลสาม",
                 "แห่งในประเทศ",
                 "ไทย")
  thai_q <- paste0("ดำเนินการใน",
                   "โรงพยาบาลสาม",
                   "แห่ง")
  khmer <- paste0("ការសិក្សានេះ",
                  "ធ្វើឡើងនៅមន្",
                  "ទីរពេទ្យបីក្",
                  "នុងប្រទេសកម្",
                  "ពុជា")
  khmer_q <- paste0("ធ្វើឡើងនៅមន្",
                    "ទីរពេទ្យ")
  bq <- readgpt:::quote_backs_value
  setting <- gr_field("Where the study ran")
  for (cs in list(c(thai_q, thai), c(khmer_q, khmer))) {
    expect_true(isTRUE(readgpt:::span_match(cs[1], cs[2])$verified))
    expect_true(bq(cs[1], cs[1], cs[2], setting))
  }
})

test_that("the shared boundary test still refuses a quote cut out of a number or a word", {
  bq <- readgpt:::quote_backs_value
  n <- gr_field("n", type = "integer")
  expect_false(bq(120L, "120 participants", "There were 1120 participants in all.", n))
  expect_false(bq(19L, "19 patients were seen", "In all, covid19 patients were seen.", n))
  expect_false(bq(20L, "enrolled 20", "They enrolled 20.5 thousand people.", n))
  # A one-word fragment backs a string only when it is the value, as a whole
  # word of it.
  expect_false(bq("20", "20", "The dose was 20.5 mg.", gr_field("dose")))
  expect_true(bq("Pfizer", "Pfizer", "Funded by Pfizer.", gr_field("funder")))
})

test_that("a placeholder is still a value only when a whole word of the quote spells it", {
  ps <- readgpt:::placeholder_stated
  expect_true(ps("None", "Conflicts of interest: None."))
  expect_false(ps("None", "None"))
  expect_false(ps("None", "Nonetheless, the trial continued."))
})

# ---------------------------------------------------------------------------
# corpus-1: the first error an incomplete row names is never a recovered one
# ---------------------------------------------------------------------------

test_that("an incomplete row does not name a recovered embeddings failure as its error", {
  local_registries()
  local_clean_cache()
  # Fails as the built-in "api" embedder does on an endpoint with no
  # embeddings: the request is in the trace, marked recovered.
  gr_register_embedder("gone-404-r4", function(texts, params) {
    tr <- params$trace
    if (inherits(tr, "gr_trace")) {
      readgpt:::trace_record(tr, "embed.request", list(),
                             gr_result(FALSE, model = "bad-embed", status = 404L,
                                       error = "Embeddings request to 'bad-embed' failed: HTTP 404"),
                             params = list(model = "bad-embed"))
      tr$errors[[length(tr$errors)]]$recovered <- TRUE
    }
    stop("the request to 'bad-embed' did not return embeddings")
  })
  gr_options(embedder = "gone-404-r4")
  say3 <- function(s) paste(rep(s, 3), collapse = " ")
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(paste(c(say3("We ran a randomised controlled trial of the new treatment across sites."),
                     say3("We enrolled 120 participants in total over the recruitment window.")),
                   collapse = "\n\n"), f)
  fields <- gr_fields(design = "The study design",
                      n = gr_field("Number of participants", type = "integer"))
  # The excerpts holding the sample size come back as prose: failed requests
  # the trace records as successful, so its only error is the recovered one.
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("enrolled", messages[[length(messages)]]$content, fixed = TRUE)) {
      return("Sorry, I cannot help with that.")
    }
    paste0('{"design":"randomised controlled trial","n":null,"design__quote":',
           '"We ran a randomised controlled trial of the new treatment across sites.",',
           '"n__quote":null}')
  })
  x <- quiet(gr_extract(f, fields, client = cl, recipe = "thorough", max_tokens = 48,
                        method = "semantic", keep_answers = TRUE))
  errs <- x$answers[[1]]$trace$errors
  expect_true(length(errs) >= 1L)
  expect_true(all(vapply(errs, function(e) isTRUE(e$recovered), logical(1))))
  expect_identical(x$table$status, "incomplete")
  expect_match(x$table$error, "extraction request(s) failed", fixed = TRUE)
  expect_false(grepl("404", x$table$error, fixed = TRUE))
  expect_false(grepl("first error", x$table$error, fixed = TRUE))
})

test_that("extraction_table() names the first unrecovered error and never a recovered one", {
  f <- gr_fields(n = gr_field("n", type = "integer"))
  mk <- function(...) {
    tr <- gr_trace()
    tr$errors <- list(...)
    structure(list(answer = '{"n":null}', trace = tr,
                   notes = list(record = list(n = NULL), failed_calls = 1L, chunks = 2L),
                   evidence = NULL), class = "gr_answer")
  }
  embed_404 <- list(step = 1L, label = "embed.request", recovered = TRUE,
                    error = "Embeddings request to 'bad-embed' failed: HTTP 404")
  chat_429 <- list(step = 2L, label = "extract.chunk", error = "HTTP 429 rate limited")
  summ <- data.frame(document = c("a", "b"), document_id = c("x", "y"), status = "ok",
                     duplicate_of = NA_character_, error = NA_character_,
                     stringsAsFactors = FALSE)
  tab <- readgpt:::extraction_table(c("a", "b"),
                                    list(a = mk(embed_404), b = mk(embed_404, chat_429)), f, summ)
  expect_identical(tab$status, c("incomplete", "incomplete"))
  expect_false(grepl("404", tab$error[1], fixed = TRUE))
  expect_false(grepl("first error", tab$error[1], fixed = TRUE))
  expect_match(tab$error[2], "first error: HTTP 429 rate limited", fixed = TRUE)
  expect_false(grepl("404", tab$error[2], fixed = TRUE))
})

# ---------------------------------------------------------------------------
# corpus-8: a verbatim quote that does not state its value is not "no span"
# ---------------------------------------------------------------------------

r4_unstated <- function() {
  a <- r4_file("Twenty-four patients were enrolled at two sites. The trial ran for a year.")
  fl <- gr_fields(n = gr_field("Sample size", type = "integer"))
  cl <- gr_mock_client(function(messages, params)
    '{"n":5000,"n__quote":"Twenty-four patients were enrolled at two sites."}')
  quiet(gr_extract(a, fl, client = cl, keep_answers = TRUE))
}

test_that("gr_flow() does not call a verbatim quote that fails the value check 'no verbatim span'", {
  x <- r4_unstated()
  expect_false(x$evidence$verified)
  expect_identical(x$evidence$match, 1)
  fl <- gr_flow(extraction = x)
  row <- fl[fl$stage == "values unsupported", ]
  expect_identical(row$n, 1L)
  expect_false(grepl("no verbatim span", row$note, fixed = TRUE))
  expect_match(row$note, "does not state the value", fixed = TRUE)
})

test_that("the audit report tells a quote not found from one that does not state its value", {
  x <- r4_unstated()
  h <- r4_report(extraction = x)
  expect_false(grepl("no verbatim span", h, fixed = TRUE))
  expect_false(grepl("could not be found in the chunk cited", h, fixed = TRUE))
  expect_match(h, "1 found in the chunk cited but not stating the value cited", fixed = TRUE)

  # A quote that is not in the chunk is still reported as not found.
  a <- r4_file("Twenty-four patients were enrolled at two sites. The trial ran for a year.")
  fl <- gr_fields(n = gr_field("Sample size", type = "integer"))
  cl <- gr_mock_client(function(messages, params)
    '{"n":24,"n__quote":"Twenty-four people took part in the study."}')
  y <- quiet(gr_extract(a, fl, client = cl))
  expect_lt(y$evidence$match, 1)
  hy <- r4_report(extraction = y)
  expect_match(hy, "1 could not be found in the chunk cited", fixed = TRUE)
  expect_false(grepl("not stating the value", hy, fixed = TRUE))
})

test_that("an extraction's answer page does not say a verbatim quote was not found", {
  x <- r4_unstated()
  h <- r4_report(answer = x$answers[[1]])
  expect_false(grepl("Quoted, but not found in this chunk", h, fixed = TRUE))
  expect_false(grepl("could not be found in the chunk they cite", h, fixed = TRUE))
  expect_match(h, paste("Quoted, and found in this chunk, but it does not state the value it",
                        "is cited for: &ldquo;Twenty-four patients were enrolled at two sites."),
               fixed = TRUE)
  expect_match(h, paste("1 quotation(s) found in the chunk they cite do not state the value",
                        "they are cited for."), fixed = TRUE)
})
