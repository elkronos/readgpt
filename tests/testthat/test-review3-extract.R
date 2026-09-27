# test-review3-extract.R -- regressions a verification of the extraction fixes
# found: honest numbers written as words or in journal formats rejected by the
# new value check, multi-passage quotes that span_match() accepts but the value
# check did not, placeholder strings kept as values, a failed adjudication or
# header call making a fully read document "failed", and print wording that
# called a verbatim quote "no verbatim span".

r3_file <- function(...) {
  f <- tempfile(fileext = ".txt")
  writeLines(paste(c(...), collapse = "\n\n"), f)
  f
}

# Paragraphs long enough that the paragraph segmenter at max_tokens = 48 cuts
# them into separate chunks.
r3_say <- function(s) paste(rep(s, 3), collapse = " ")

# One document holding `text`, one field of `type`, and a model that fills it
# with `value` and quotes `quote`. Returns the lax and the strict extraction.
r3_extract <- function(text, value, type = "integer", quote = text) {
  f <- r3_file("Background text about the trial design.", text)
  fl <- gr_fields(x = gr_field("The value", type = type))
  js <- as.character(jsonlite::toJSON(list(x = value, x__quote = quote),
                                      auto_unbox = TRUE, digits = NA))
  cl <- gr_mock_client(function(messages, params) js)
  list(lax = quiet(gr_extract(f, fl, client = cl, recipe = "fast")),
       strict = quiet(gr_extract(f, fl, client = cl, recipe = "fast", require_quote = TRUE)))
}

test_that("a number is verified however an honest quote writes it", {
  # Each of these verified under the old span check and was then rejected by
  # the value check, which read only ASCII digit strings: require_quote
  # deleted a value its verbatim quote states.
  cases <- list(
    list("Twenty-four patients were enrolled at two sites.", 24),
    list("Sixty-two patients were randomised to the two arms.", 62),
    list("Participants were randomised to one of three arms.", 3),
    list("One hundred and twenty adults were enrolled.", 120),
    list("No participants died during follow-up.", 0),
    list("The association was significant (p = 3.2 x 10\u22125).", 3.2e-5, "number"),
    list("The association was significant (p = 3\u00b72 \u00d7 10\u207b\u2075).", 3.2e-5, "number"),
    list("The registry covered 1.2 million adults.", 1200000),
    list("A total of 1 204 patients were randomised.", 1204),
    list("In total 1\u2009204 adults took part.", 1204),
    list("A total of 1\u00a0204 patients were randomised.", 1204),
    list("Le coefficient \u00e9tait de 0,45 dans le groupe.", 0.45, "number"),
    list("The effect size was d = 0,84 in the trial.", 0.84, "number"),
    list("The hazard ratio was 0\u00b784 (95% CI 0\u00b772\u20130\u00b794).", 0.84, "number"),
    list("Of the participants, 54% were women.", 0.54, "number"),
    list("This phase III trial enrolled 120 patients.", 3),
    list("\u5171\u7eb3\u5165\u4e00\u767e\u4e8c\u5341\u4f8b\u60a3\u8005\u3002", 120),
    # A language whose number words are not read keeps the old span check.
    list("Se incluyeron veinticuatro pacientes en el estudio.", 24),
    # Still fine before and after.
    list("A total of 1,204 patients were randomised.", 1204),
    list("Participants were aged 18\u201365 years.", 18),
    list("The effect was -0.35 overall.", -0.35, "number")
  )
  for (cs in cases) {
    type <- if (length(cs) > 2L) cs[[3]] else "integer"
    x <- r3_extract(cs[[1]], cs[[2]], type)
    expect_true(isTRUE(x$lax$evidence$verified), info = cs[[1]])
    expect_identical(x$lax$table$n_unverified, 0L, info = cs[[1]])
    expect_equal(x$strict$table$x, cs[[2]], info = cs[[1]])
  }
})

test_that("a number the quote does not state is still unverified, in any form", {
  cases <- list(
    list("We enrolled 120 people.", 5000),
    list("Twenty-four patients were enrolled at two sites.", 5000),
    list("Twelve patients died during follow-up.", 0),
    list("The hazard ratio was 0.84 overall.", 0.48, "number"),
    # The middle dot is a decimal point, not a separator between 0 and 84.
    list("The hazard ratio was 0\u00b784 (95% CI 0\u00b772\u20130\u00b794).", 84, "number"),
    # A number the quote does state, in digits, is still required to match.
    list("Se incluyeron 120 pacientes en el estudio.", 24)
  )
  for (cs in cases) {
    type <- if (length(cs) > 2L) cs[[3]] else "integer"
    x <- r3_extract(cs[[1]], cs[[2]], type)
    expect_false(isTRUE(x$lax$evidence$verified), info = cs[[1]])
    expect_identical(x$lax$evidence$match, 1, info = cs[[1]])
    expect_identical(x$lax$table$n_unverified, 1L, info = cs[[1]])
    expect_true(is.na(x$strict$table$x), info = cs[[1]])
  }
  # A fragment of English that states no number is not a passage in a foreign
  # language: no fallback to the span check.
  bq <- readgpt:::quote_backs_value
  n <- gr_field("n", type = "integer")
  expect_false(bq(9999L, "Patients enrolled", "Patients enrolled were followed up.", n))
  expect_false(bq(9999L, "the", "We enrolled 120 people. The trial was funded.", n))
  expect_false(bq(120L, "120 participants", "There were 1120 participants in all.", n))
})

test_that("quote_numbers() reads each form once and invents none", {
  qn <- function(s) sort(readgpt:::quote_numbers(readgpt:::normalise_for_match(s)))
  expect_identical(qn("one hundred and twenty"), 120)
  expect_identical(qn("two thousand three hundred and five"), 2305)
  expect_identical(qn("twenty-first"), 21)
  expect_identical(qn("nineteen ninety-five"), c(19, 95))
  expect_identical(qn("1.2 million adults"), c(1.2, 1.2e6))
  expect_identical(qn("\u4e24\u4e07"), 20000)
  expect_identical(qn("\u4e8c\u3007\u4e8c\u4e09"), 2023)
  expect_identical(qn("3\u4e07"), c(3, 30000))
  expect_identical(qn("\uff11\uff12\uff10"), 120)
  expect_identical(qn("2 x 105 cells"), c(2, 105, 2e5))
  expect_identical(qn("given iv over 30 min"), 30)
  expect_identical(qn("grade II/III toxicity"), c(2, 3))
})

test_that("a quotation of several passages carries its value into gr_extract()", {
  # span_match() checks such a quote passage by passage, but the value check
  # read it as one string, so the evidence said verified = FALSE, match = 1
  # and require_quote deleted n = 240.
  f <- gr_fields(n = gr_field("Participants randomised", type = "integer"))
  a <- r3_file(paste("In total, 240 patients were randomised to two arms.",
                     "Both arms received the drug for eight weeks. Follow-up lasted 12 weeks."))
  quotes <- c(
    "In total, 240 patients were randomised to two arms. [...] Follow-up lasted 12 weeks.",
    "In total, 240 patients were randomised to two arms.\nFollow-up lasted 12 weeks.",
    "**In total, 240 patients were randomised to two arms.**",
    "In total, 240 patients ... randomised to two arms."
  )
  for (q in quotes) {
    cl <- gr_mock_client(function(messages, params)
      as.character(jsonlite::toJSON(list(n = 240, n__quote = q), auto_unbox = TRUE)))
    x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", require_quote = TRUE,
                          keep_answers = TRUE))
    expect_identical(x$table$n, 240L, info = q)
    expect_identical(x$table$n_unverified, 0L, info = q)
    expect_true(x$evidence$verified, info = q)
    expect_false(x$answers[[1]]$partial, info = q)
  }
  # Every passage must still be in the chunk, and the value in one of them.
  bq <- readgpt:::quote_backs_value
  src <- "We enrolled 120 adults from three clinics. All 120 completed it."
  expect_true(bq(120L, "We enrolled 120 adults ... All 120 completed it.", src,
                 gr_field("n", type = "integer")))
  expect_false(bq(120L, "We enrolled 120 adults ... All 130 completed it.", src,
                  gr_field("n", type = "integer")))
  expect_false(bq(5000L, "We enrolled 120 adults ... All 120 completed it.", src,
                  gr_field("n", type = "integer")))
  # Splitting must not let an elision drop a negation, or join one sentence's
  # subject to another's claim.
  neg <- "In the trial, the drug did not reduce mortality at 12 months."
  expect_false(bq("reduced mortality", "the drug did ... reduce mortality at 12 months.", neg,
                  gr_field("effect")))
  expect_false(bq("reduced mortality", "the drug did\nreduce mortality at 12 months.", neg,
                  gr_field("effect")))
  expect_false(bq(30, "Revenue ... rose 30%.", "Revenue fell 12%. Costs rose 30%.",
                  gr_field("x", type = "number")))
  expect_true(bq(120L, "All 120 completed it.\nWe enrolled 120 adults.",
                 "We enrolled 120 adults. It lasted a year. All 120 completed it.",
                 gr_field("n", type = "integer")))
})

test_that("a placeholder string is a value only when the document says it", {
  f <- gr_fields(country = "Country where the trial ran",
                 n = gr_field("Number of participants", type = "integer"))
  a <- r3_file(r3_say("Abstract: this study examined an exercise program for adults with back pain."),
               paste(r3_say("Setting: recruitment ran across several clinics in the region for a year."),
                     "The trial ran in Kenya and enrolled 120 adults."))
  cl <- gr_mock_client(function(messages, params) {
    ex <- messages[[length(messages)]]$content
    if (grepl("Kenya", ex, fixed = TRUE)) {
      return(paste0('{"country":"Kenya","country__quote":"The trial ran in Kenya and enrolled ',
                    '120 adults.","n":120,"n__quote":"The trial ran in Kenya and enrolled 120 adults."}'))
    }
    '{"country":"N/A","country__quote":"N/A","n":null,"n__quote":null}'
  })
  # "N/A" quoting "N/A" beat the later chunk's verbatim "Kenya", and under
  # require_quote deleted it.
  for (rq in c(FALSE, TRUE)) {
    x <- quiet(gr_extract(a, f, client = cl, max_tokens = 48, require_quote = rq))
    expect_identical(x$table$country, "Kenya", info = rq)
    expect_true(is.na(x$table$conflicts), info = rq)
    expect_identical(x$table$n_filled, 2L, info = rq)
    expect_identical(x$table$n_unverified, 0L, info = rq)
  }

  # A document that reports nothing still reports nothing.
  f2 <- gr_fields(funding = "Funding source", coi = "Declared conflicts of interest")
  b <- r3_file("We describe the history of a local library and its reading rooms in detail.")
  cl2 <- gr_mock_client(function(messages, params)
    '{"funding":"N/A","funding__quote":"N/A","coi":"None","coi__quote":"None"}')
  y <- quiet(gr_extract(b, f2, client = cl2))
  expect_true(is.na(y$table$funding))
  expect_true(is.na(y$table$coi))
  expect_identical(y$table$n_filled, 0L)
  expect_identical(y$table$n_unverified, 0L)
  fl <- gr_flow(extraction = y)
  expect_identical(fl$n[fl$stage == "  reported nothing"], 1L)

  # A sentence that is there but does not say "None" is not the document
  # saying it; one that does say it is.
  c3 <- r3_file("The authors declare no competing interests. Conflicts of interest: None.")
  run <- function(q) {
    cl3 <- gr_mock_client(function(messages, params)
      as.character(jsonlite::toJSON(list(funding = NA, funding__quote = NA, coi = "None",
                                         coi__quote = q), auto_unbox = TRUE, na = "null")))
    quiet(gr_extract(c3, f2, client = cl3))$table$coi
  }
  expect_true(is.na(run("The authors declare no competing interests.")))
  expect_true(is.na(run("Not a sentence from this document at all.")))
  expect_identical(run("Conflicts of interest: None."), "None")
})

test_that("a failed adjudication leaves a fully read document ok", {
  d <- withr::local_tempdir()
  writeLines(c(r3_say("We ran a randomised trial of the new treatment across sites with 120 participants enrolled."),
               "",
               r3_say("In the end 118 participants were analysed after two withdrew from the trial.")),
             file.path(d, "a.txt"))
  f <- gr_fields(design = "The study design",
                 n = gr_field("Number of participants", type = "integer"))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("Two or more parts of one document", messages[[1]]$content, fixed = TRUE)) {
      stop("HTTP 503 service unavailable")
    }
    ex <- messages[[length(messages)]]$content
    if (grepl("118", ex, fixed = TRUE)) {
      return(paste0('{"design":"randomised trial","n":118,"design__quote":null,"n__quote":',
                    '"In the end 118 participants were analysed after two withdrew from the trial."}'))
    }
    paste0('{"design":"randomised trial","n":120,',
           '"design__quote":"We ran a randomised trial of the new treatment across sites with 120 participants enrolled.",',
           '"n__quote":"We ran a randomised trial of the new treatment across sites with 120 participants enrolled."}')
  })
  x <- quiet(gr_extract(d, f, client = cl, recipe = "thorough", max_tokens = 48,
                        resolve = "model", keep_answers = TRUE))
  # The first value is kept, the conflict recorded, and the row is a read.
  expect_identical(x$table$n, 120L)
  expect_identical(x$table$conflicts, "n")
  expect_identical(x$table$status, "ok")
  expect_identical(x$table$n_filled, 2L)
  expect_identical(x$table$n_unverified, 0L)
  expect_true(is.na(x$table$error))
  # The failure is in the trace, marked as one the read recovered from.
  errs <- x$answers[[1]]$trace$errors
  expect_length(errs, 1L)
  expect_identical(errs[[1]]$label, "extract.resolve")
  expect_true(isTRUE(errs[[1]]$recovered))
  fl <- gr_flow(extraction = x)
  expect_identical(fl$n[fl$stage == "  failed to read"], 0L)
})

test_that("a failed request outside extraction does not blank a read document", {
  d <- withr::local_tempdir()
  writeLines(c(r3_say("We ran a randomised trial of the new treatment across sites with 120 participants enrolled."),
               "",
               r3_say("Adverse events were mild and self-limiting in the large majority of the cases seen.")),
             file.path(d, "a.txt"))
  f <- gr_fields(n = gr_field("Number of participants", type = "integer"))
  k <- 0L
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("You situate an excerpt", messages[[1]]$content, fixed = TRUE)) {
      k <<- k + 1L
      if (k == 2L) stop("HTTP 503 service unavailable")
      return("This excerpt is part of the trial report.")
    }
    ex <- messages[[length(messages)]]$content
    if (grepl("120", ex, fixed = TRUE)) {
      return(paste0('{"n":120,"n__quote":"We ran a randomised trial of the new treatment ',
                    'across sites with 120 participants enrolled."}'))
    }
    '{"n":null,"n__quote":null}'
  })
  x <- quiet(gr_extract(d, f, client = cl, recipe = "thorough", max_tokens = 48,
                        method = "contextual", context_source = "llm"))
  # Every extraction request was answered, and the excerpt without its header
  # was still read: the row has its value and counts, as at eeff205, and is
  # not a document "failed to read" with no values.
  expect_identical(x$table$n, 120L)
  expect_identical(x$table$n_filled, 1L)
  expect_identical(x$table$status, "ok")
  expect_false(grepl("not read in full", x$table$error %||% "", fixed = TRUE))
  fl <- gr_flow(extraction = x)
  expect_identical(fl$n[fl$stage == "  failed to read"], 0L)
})

test_that("extraction_table() names an auxiliary failure only when it did not recover", {
  f <- gr_fields(n = gr_field("n", type = "integer"))
  mk <- function(recovered) {
    tr <- gr_trace()
    tr$errors <- list(list(step = 1L, label = "extract.resolve", error = "HTTP 503",
                           recovered = recovered))
    structure(list(answer = '{"n":120}', trace = tr,
                   notes = list(record = list(n = 120L), failed_calls = 0L, chunks = 2L),
                   evidence = NULL), class = "gr_answer")
  }
  summ <- data.frame(document = c("a", "b"), document_id = c("x", "y"),
                     status = "failed", duplicate_of = NA_character_,
                     error = "1 request(s) failed, so the document was not read in full (first error: HTTP 503)",
                     stringsAsFactors = FALSE)
  tab <- readgpt:::extraction_table(c("a", "b"), list(a = mk(TRUE), b = mk(FALSE)), f, summ)
  # Either way every excerpt was read, so both rows are reads.
  expect_identical(tab$status, c("ok", "ok"))
  expect_identical(tab$n_filled, c(1L, 1L))
  expect_true(is.na(tab$error[1]))
  expect_match(tab$error[2], "every extraction request succeeded; 1 other request", fixed = TRUE)
  expect_match(tab$error[2], "HTTP 503", fixed = TRUE)
})

test_that("print() does not call a verbatim quote that fails the value check 'no verbatim span'", {
  a <- r3_file("Twenty-four patients were enrolled at two sites. The trial ran for a year.")
  fl <- gr_fields(n = gr_field("Sample size", type = "integer"))
  cl <- gr_mock_client(function(messages, params)
    '{"n":5000,"n__quote":"Twenty-four patients were enrolled at two sites."}')
  x <- quiet(gr_extract(a, fl, client = cl))
  expect_false(x$evidence$verified)
  expect_identical(x$evidence$match, 1)
  out <- paste(capture.output(print(x)), collapse = "\n")
  expect_false(grepl("no verbatim span", out, fixed = TRUE))
  expect_match(out, "1 value(s) not verified", fixed = TRUE)
  expect_match(out, "100% verbatim in the cited chunk, 0% verified", fixed = TRUE)
})
