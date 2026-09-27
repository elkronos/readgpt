# test-review-extract.R -- regressions for defects an adversarial review found in
# the extraction layer: fields lost to failed requests reported as "not
# reported", declared values read as missing, a supported value discarded
# because an earlier chunk gave it without a quote, a value "verified" by a
# quote that does not carry it, numbers rounded in the answer text, and model
# text handed to a JSON reader that also opens files and URLs.

rx_file <- function(...) {
  f <- tempfile(fileext = ".txt")
  writeLines(paste(c(...), collapse = "\n\n"), f)
  f
}

# Paragraphs long enough that the paragraph segmenter at max_tokens = 48 cuts
# them into separate chunks (the cap has a floor, so short fixtures collapse
# into one chunk and a per-chunk failure cannot be staged).
rx_say <- function(s) paste(rep(s, 3), collapse = " ")

test_that("fields lost to a failed request are not reported as 'not reported'", {
  f <- gr_fields(design = "The study design",
                 n = gr_field("Number of participants", type = "integer"))
  a <- rx_file(rx_say("We ran a randomised controlled trial of the new treatment across sites."),
               rx_say("We enrolled 120 participants in total over the recruitment window."))
  cl <- gr_mock_client(function(messages, params) {
    ex <- messages[[length(messages)]]$content
    if (grepl("enrolled", ex, fixed = TRUE)) stop("HTTP 429 rate limited")
    paste0('{"design":"randomised controlled trial","n":null,',
           '"design__quote":"We ran a randomised controlled trial of the new treatment across sites.",',
           '"n__quote":null}')
  })
  x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 48))

  # The value that was read is real and stays; the row says it was not read in
  # full, and why, so the empty `n` is not a finding about the study.
  expect_identical(x$table$design, "randomised controlled trial")
  expect_true(is.na(x$table$n))
  expect_identical(x$table$status, "incomplete")
  expect_identical(x$table$n_filled, 1L)
  expect_match(x$table$error, "[0-9]+ of [0-9]+ extraction request\\(s\\) failed")
  expect_match(x$table$error, "HTTP 429 rate limited", fixed = TRUE)
  expect_output(print(x), "1 incomplete")
  expect_output(print(x), "read only in part")

  # Downstream, the row is not a document that "reported nothing", and it is
  # not a study a synthesis may describe as silent about `n`: gr_synthesise()
  # and gr_claims() draw only on rows whose status is one of these.
  fl <- gr_flow(extraction = x)
  expect_identical(fl$n[fl$stage == "  reported nothing"], 0L)
  expect_false(x$table$status %in% c("ok", "restored", "duplicate"))

  # And the answer's own notes no longer list the empty field as not reported.
  y <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 48,
                        keep_answers = TRUE))
  expect_identical(y$answers[[1]]$notes$not_reported, character(0))
  expect_identical(y$answers[[1]]$notes$unknown, "n")
})

test_that("a document whose every extraction request failed is a failed read", {
  f <- gr_fields(design = "The study design",
                 n = gr_field("Number of participants", type = "integer"))
  d <- withr::local_tempdir()
  writeLines("We ran a randomised controlled trial. We enrolled 120 participants.",
             file.path(d, "a.txt"))
  writeLines("We ran a cohort study. We enrolled 300 participants.", file.path(d, "b.txt"))
  cl <- gr_mock_client(function(messages, params) {
    ex <- messages[[length(messages)]]$content
    if (grepl("cohort", ex, fixed = TRUE)) stop("HTTP 500 server error")
    paste0('{"design":"randomised controlled trial","n":120,',
           '"design__quote":"We ran a randomised controlled trial.",',
           '"n__quote":"We enrolled 120 participants."}')
  })
  y <- quiet(gr_extract(d, f, client = cl))
  b <- y$table$document == "b.txt"
  expect_identical(y$table$status[!b], "ok")
  expect_identical(y$table$status[b], "failed")
  # Never read, so no count at all rather than "reports none of the fields".
  expect_true(is.na(y$table$n_filled[b]))
  expect_true(is.na(y$table$n_unverified[b]))
  expect_match(y$table$error[b], "every extraction request failed (1 of 1)", fixed = TRUE)
  expect_match(y$table$error[b], "HTTP 500 server error", fixed = TRUE)
  fl <- gr_flow(extraction = y)
  expect_identical(fl$n[fl$stage == "  reported nothing"], 0L)
  expect_identical(fl$n[fl$stage == "  failed to read"], 1L)
})

test_that("a declared enum value is kept whatever it spells", {
  # The documented schema uses "null" for a null result. It was read as a
  # missing value, so every null study dropped out as "not reported".
  cf <- readgpt:::coerce_field
  e <- gr_field("Direction", type = "enum", values = c("positive", "null", "NA", "none"))
  expect_identical(cf("null", e), "null")
  expect_identical(cf("NA", e), "NA")
  expect_identical(cf("none", e), "none")
  expect_null(cf("negative", e))
  # Not a declared value: still missing.
  expect_null(cf("null", gr_field("Direction", type = "enum", values = c("positive", "mixed"))))
  # Where those words cannot be a value, they are still missing rather than,
  # for a boolean, FALSE.
  expect_null(cf("none", gr_field("f", type = "boolean")))
  expect_null(cf("N/A", gr_field("n", type = "integer")))
  expect_null(cf("not reported", gr_field("s")))
})

test_that("'None' backed by a quote is a finding; 'None' with no quote is nothing", {
  f <- gr_fields(
    design  = "The study design",
    outcome = gr_field("Direction of the primary result", type = "enum",
                       values = c("positive", "null", "mixed")),
    harms   = "Serious adverse events",
    coi     = "Declared conflicts of interest",
    funding = "Funding source"
  )
  a <- rx_file("This was a randomised controlled trial of drug X versus placebo.",
               paste("The primary outcome showed a null result with no difference between arms.",
                     "Serious adverse events: None. Conflicts of interest: None."))
  cl <- gr_mock_client(function(messages, params) {
    as.character(jsonlite::toJSON(list(
      design = "randomised controlled trial",
      design__quote = "This was a randomised controlled trial of drug X versus placebo.",
      outcome = "null",
      outcome__quote = "The primary outcome showed a null result with no difference between arms.",
      harms = "None", harms__quote = "Serious adverse events: None.",
      coi = "None", coi__quote = "Conflicts of interest: None.",
      # The Python-style "nothing here": no sentence behind it.
      funding = "None", funding__quote = NA
    ), auto_unbox = TRUE, na = "null"))
  })
  x <- quiet(gr_extract(a, f, client = cl))
  expect_identical(x$table$outcome, "null")
  expect_identical(x$table$harms, "None")
  expect_identical(x$table$coi, "None")
  expect_true(is.na(x$table$funding))
  expect_identical(x$table$n_filled, 4L)
  expect_identical(x$table$n_unverified, 0L)
  expect_setequal(x$evidence$field, c("design", "outcome", "harms", "coi"))
  expect_true(all(x$evidence$verified))
})

test_that("agreeing chunks lend the value their best quote", {
  # The abstract gives n = 120 with no quote; the methods give the same value
  # with the exact sentence. The first hit used to be the only one looked at,
  # so the value was unsupported and require_quote deleted it.
  abs_txt <- rx_say("Abstract: this trial of one hundred twenty adults examined an exercise program.")
  meth_txt <- paste(rx_say("Methods: recruitment ran across several clinics in the region for a year."),
                    "We enrolled 120 participants in the trial.")
  a <- rx_file(abs_txt, meth_txt)
  cl <- gr_mock_client(function(messages, params) {
    ex <- messages[[length(messages)]]$content
    if (grepl("We enrolled 120", ex, fixed = TRUE)) {
      return('{"n":120,"n__quote":"We enrolled 120 participants in the trial."}')
    }
    if (grepl("Abstract:", ex, fixed = TRUE)) return('{"n":120,"n__quote":null}')
    '{"n":null,"n__quote":null}'
  })
  f <- gr_fields(n = gr_field("Sample size", type = "integer"))
  for (rq in c(FALSE, TRUE)) {
    x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 48,
                          require_quote = rq, keep_answers = TRUE))
    expect_identical(x$table$n, 120L)
    expect_identical(x$table$n_filled, 1L)
    expect_identical(x$table$n_unverified, 0L)
    expect_identical(nrow(x$evidence), 1L)
    expect_true(x$evidence$verified)
    expect_identical(x$evidence$quote, "We enrolled 120 participants in the trial.")
    expect_false(x$answers[[1]]$partial)
  }
})

test_that("a quote that does not carry the value does not verify it", {
  f <- gr_fields(n = gr_field("Number of participants", type = "integer"),
                 funded = gr_field("Industry funded", type = "boolean"),
                 design = "The study design")
  a <- rx_file("We ran a randomised controlled trial. We enrolled 120 people. The trial was funded by Pfizer.")
  run <- function(json, rq = FALSE) {
    cl <- gr_mock_client(function(messages, params) json)
    quiet(gr_extract(a, f, client = cl, require_quote = rq))
  }
  nulls <- '"funded":null,"funded__quote":null,"design":null,"design__quote":null'

  # A real sentence, a fabricated number.
  x <- run(paste0('{"n":5000,"n__quote":"We enrolled 120 people.",', nulls, '}'))
  expect_identical(x$table$n, 5000L)                     # kept, not discarded...
  expect_identical(x$table$n_unverified, 1L)             # ...but not verified
  expect_false(x$evidence$verified)
  expect_identical(x$evidence$match, 1)                  # the sentence IS there
  # And the strict policy drops it.
  s <- run(paste0('{"n":5000,"n__quote":"We enrolled 120 people.",', nulls, '}'), rq = TRUE)
  expect_true(is.na(s$table$n))
  expect_identical(s$table$n_unverified, 1L)

  # One-character and one-word fragments point at nothing.
  for (q in c("2", "the", "We enrolled")) {
    y <- run(sprintf('{"n":5000,"n__quote":"%s",%s}', q, nulls), rq = TRUE)
    expect_true(is.na(y$table$n), info = q)
  }
  # A fragment of a larger number is not that number.
  z <- run(paste0('{"n":12,"n__quote":"enrolled 12",', nulls, '}'))
  expect_identical(z$table$n_unverified, 1L)

  # A boolean or a string cannot be looked for in its quote, but a single word
  # that is not the value is not a quotation.
  w <- run(paste0('{"n":null,"n__quote":null,"funded":true,"funded__quote":"trial",',
                  '"design":"cohort study","design__quote":"trial"}'))
  expect_identical(w$table$n_unverified, 2L)

  # The honest version verifies, including a number written with separators.
  ok <- run(paste0('{"n":120,"n__quote":"We enrolled 120 people.","funded":true,',
                   '"funded__quote":"The trial was funded by Pfizer.",',
                   '"design":"randomised controlled trial",',
                   '"design__quote":"We ran a randomised controlled trial."}'), rq = TRUE)
  expect_identical(ok$table$n, 120L)
  expect_identical(ok$table$n_unverified, 0L)
  expect_true(all(ok$evidence$verified))
  bq <- readgpt:::quote_backs_value
  expect_true(bq(1204L, "A total of 1,204 were randomised.", "A total of 1,204 were randomised.",
                 gr_field("n", type = "integer")))
  expect_true(bq(0.05, "significant at p < .05 overall", "It was significant at p < .05 overall.",
                 gr_field("p", type = "number")))
  expect_true(bq("Pfizer", "Pfizer", "Funded by Pfizer.", gr_field("funder")))
})

test_that("extracted quotes are checked against the source text, not a model's rewrite", {
  # A chunk may carry `source_text`: the document text without anything a model
  # wrote into `text` (a contextual header, rewritten propositions). A quote of
  # the model-written part is not a quotation of the document.
  f <- gr_fields(design = "The study design")
  ch <- quiet(gr_segment("We ran a cohort study of adults.",
                         list(method = "paragraph", max_tokens = 100)))
  ch$chunks$source_text <- ch$chunks$text
  ch$chunks$text <- paste("Context: this paper is a randomised controlled trial.", ch$chunks$text)
  cl <- gr_mock_client(function(messages, params) {
    '{"design":"randomised controlled trial","design__quote":"this paper is a randomised controlled trial"}'
  })
  ans <- quiet(gr_read(ch, "extract", cl, gr_read_spec("extract", fields = f)))
  expect_false(ans$evidence$verified)
  expect_identical(ans$notes$unsupported, "design")
  expect_true(ans$partial)

  # The same quote of the document's own words verifies; NA means `text` is source.
  cl2 <- gr_mock_client(function(messages, params) {
    '{"design":"cohort study","design__quote":"We ran a cohort study of adults."}'
  })
  ans2 <- quiet(gr_read(ch, "extract", cl2, gr_read_spec("extract", fields = f)))
  expect_true(ans2$evidence$verified)
  ch$chunks$source_text <- NA_character_
  ans3 <- quiet(gr_read(ch, "extract", cl, gr_read_spec("extract", fields = f)))
  expect_true(ans3$evidence$verified)
})

test_that("the answer text keeps every digit of an extracted number", {
  f <- gr_fields(p = gr_field("P value", type = "number"),
                 effect = gr_field("Effect size", type = "number"),
                 n = gr_field("Participants", type = "integer"))
  a <- rx_file("We enrolled 120 participants in the trial.",
               "The effect was d = 0.84321 with p = 0.00003 overall.")
  cl <- gr_mock_client(function(messages, params) {
    paste0('{"p":0.00003,"p__quote":"p = 0.00003","effect":0.84321,',
           '"effect__quote":"d = 0.84321","n":120,"n__quote":"120 participants"}')
  })
  x <- quiet(gr_extract(a, f, client = cl, recipe = "fast", keep_answers = TRUE))
  got <- jsonlite::fromJSON(x$summary$answer)
  expect_identical(got$p, 3e-05)
  expect_identical(got$effect, 0.84321)
  # The store fallback rebuilds the table from that text, so it must agree.
  ans <- x$answers[[1]]
  ans$notes$record <- NULL
  rec <- readgpt:::answer_record(ans, f)
  expect_identical(rec$p, 3e-05)
  expect_identical(rec$effect, 0.84321)
})

test_that("the stored-answer fallback never opens a file or URL the text names", {
  f <- gr_fields(design = "The study design")
  js <- tempfile(fileext = ".json")
  writeLines('{"design":"read from a file, not the answer"}', js)
  ans <- structure(list(answer = js, notes = list()), class = "gr_answer")
  rec <- readgpt:::answer_record(ans, f)
  expect_null(rec$design)
  url <- structure(list(answer = "http://127.0.0.1:9/never.json", notes = list()),
                   class = "gr_answer")
  expect_null(readgpt:::answer_record(url, f)$design)
})
