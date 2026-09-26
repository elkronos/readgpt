# test-review3-verify.R -- the third pass over quotation checking: what the
# whole-word check broke that the plain substring test got right, what it
# still let through, and what it cost; plus the replay record and the parallel
# help text.

sm <- function(span, src) readgpt:::span_match(span, src)
verified <- function(span, src) isTRUE(sm(span, src)$verified)

skim_client <- function(quote, answer = "The answer.") {
  gr_mock_client(function(messages, params) {
    if (grepl("You extract evidence", messages[[1]]$content, fixed = TRUE)) return(quote)
    answer
  })
}
skim_read <- function(doc, quote) {
  ch <- quiet(gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 400)))
  quiet(gr_read(ch, "What does it say?", skim_client(quote), "skim"))
}

# ---------------------------------------------------------------------------
# read-1, cross-2, corpus-6: scripts written without spaces between words
# ---------------------------------------------------------------------------

zh_doc <- "\u672c\u7814\u7a76\u5171\u7eb3\u5165120\u540d\u60a3\u8005\uff0c\u5e73\u5747\u5e74\u9f8445\u5c81\u3002"
zh_quote <- "\u5171\u7eb3\u5165120\u540d\u60a3\u8005"

test_that("a clause quoted from Chinese, Japanese or Thai verifies", {
  expect_identical(sm(zh_quote, zh_doc), list(verified = TRUE, match = 1))
  expect_true(verified("\u6cbb\u7597\u7ec4\u7684\u6b7b\u4ea1\u7387\u964d\u4f4e",
                       "\u7ed3\u679c\u663e\u793a\u6cbb\u7597\u7ec4\u7684\u6b7b\u4ea1\u7387\u964d\u4f4e\u4e8612%\u3002"))
  expect_true(verified("120\u540d\u306e\u60a3\u8005\u3092\u767b\u9332\u3057\u305f",
                       "\u672c\u8a66\u9a13\u3067\u306f120\u540d\u306e\u60a3\u8005\u3092\u767b\u9332\u3057\u305f\u3002"))
  expect_true(verified("\u0e1c\u0e39\u0e49\u0e1b\u0e48\u0e27\u0e22 120 \u0e04\u0e19",
                       paste0("\u0e01\u0e32\u0e23\u0e28\u0e36\u0e01\u0e29\u0e32\u0e19\u0e35\u0e49\u0e21\u0e35",
                              "\u0e1c\u0e39\u0e49\u0e1b\u0e48\u0e27\u0e22 120 \u0e04\u0e19\u0e40\u0e02\u0e49",
                              "\u0e32\u0e23\u0e48\u0e27\u0e21")))
  # Digits are still whole numbers: 20 is not found in 120.
  changed <- sm("\u5171\u7eb3\u516520\u540d\u60a3\u8005", zh_doc)
  expect_false(changed$verified)
  # And a clause with a character changed scores what it shares, not 0.
  expect_gt(changed$match, 0)
  expect_lt(changed$match, 1)
})

test_that("a Chinese skim quotation is not partial and an extracted value is kept", {
  ja <- paste0("\u672c\u8a66\u9a13\u3067\u306f120\u540d\u306e\u60a3\u8005\u3092\u767b\u9332\u3057\u305f",
               "\u3002\u8ffd\u8de1\u671f\u9593\u306f\u4e8c\u5e74\u9593\u3067\u3042\u3063\u305f\u3002")
  for (case in list(c(zh_doc, zh_quote),
                    c(ja, "120\u540d\u306e\u60a3\u8005\u3092\u767b\u9332\u3057\u305f"))) {
    a <- skim_read(case[1], case[2])
    expect_false(a$partial)
    expect_true(all(a$evidence$verified))
    expect_true(all(gr_verify_evidence(a)$verified))
  }

  doc <- withr::local_tempfile(fileext = ".txt")
  writeLines(paste0("\u672c\u7814\u7a76\u4e3a\u968f\u673a\u5bf9\u7167\u8bd5\u9a8c\u3002",
                    "\u672c\u7814\u7a76\u5171\u7eb3\u5165120\u540d\u60a3\u8005\uff0c",
                    "\u6765\u81ea\u4e5d\u4e2a\u4e34\u5e8a\u4e2d\u5fc3\u3002",
                    "\u968f\u8bbf\u65f6\u95f4\u4e3a\u4e24\u5e74\u3002"), doc, useBytes = TRUE)
  f <- gr_fields(n = gr_field("Number of participants", type = "integer"), design = "Study design")
  cl <- gr_mock_client(function(m, p) paste0(
    '{"n":120,"design":"\u968f\u673a\u5bf9\u7167\u8bd5\u9a8c",',
    '"n__quote":"', zh_quote, '",',
    '"design__quote":"\u672c\u7814\u7a76\u4e3a\u968f\u673a\u5bf9\u7167\u8bd5\u9a8c"}'))
  x <- quiet(gr_extract(doc, f, client = cl, recipe = "thorough", require_quote = TRUE))
  expect_identical(x$table$n[1], 120L)
  expect_identical(x$table$design[1], "\u968f\u673a\u5bf9\u7167\u8bd5\u9a8c")
  expect_identical(x$table$n_unverified[1], 0L)
})

# ---------------------------------------------------------------------------
# read-2: markdown bold and significance stars in the source
# ---------------------------------------------------------------------------

test_that("a source that really contains '**' can still be quoted", {
  md <- "**Revenue** rose 12% to 4.2 million euros in 2023."
  stars <- "Treatment effect was 0.45** (0.12) in the main model."
  expect_true(verified(md, md))
  expect_true(verified("**Revenue** rose", "**Revenue** rose"))
  expect_true(verified(stars, stars))
  # Bold is emphasis on either side: dropped by the model, or added by it.
  expect_true(verified("Revenue rose 12% to 4.2 million euros in 2023.", md))
  expect_true(verified("**Headache was reported by 12 patients.**",
                       "Headache was reported by 12 patients."))
  # A significance star is not bold. Adding one to a table that has none, or
  # dropping one from a table that has it, is not the table.
  expect_false(verified("Treatment effect was 0.45** (0.12)",
                        "Treatment effect was 0.45 (0.12) in the main model."))
  expect_false(verified("Treatment effect was 0.45 (0.12)", stars))
})

test_that("a chunk from a markdown file verifies as its own evidence", {
  md <- withr::local_tempfile(fileext = ".md")
  writeLines(c("# Annual report", "", "**Revenue** rose 12% to 4.2 million euros in 2023.", "",
               "| Arm | Effect |", "|---|---|", "| Treatment | 0.45** (0.12) |", "",
               "Costs were flat."), md)
  ch <- quiet(gr_segment(gr_ingest(md), list(method = "paragraph", max_tokens = 400)))
  for (r in c("stuff", "retrieve")) {
    a <- quiet(gr_read(ch, "How did revenue change?", mock_echo("Revenue rose 12%."), r))
    v <- gr_verify_evidence(a, ch)
    expect_true(all(v$verified), label = r)
    expect_true(all(v$match == 1), label = r)
  }
  a <- skim_read(md, "**Revenue** rose 12% to 4.2 million euros in 2023.")
  expect_false(a$partial)
  a <- skim_read(md, "Treatment | 0.45** (0.12)")
  expect_false(a$partial)
})

test_that("verbatim rows are compared whole, not as a model's list of passages", {
  # Text a reader copied out of the chunk is the chunk's own: its markers are
  # the document's, not a model's formatting.
  src <- "- Item one costs 5 dollars.\n- **Item** two costs 7 dollars."
  expect_identical(readgpt:::span_match(src, src, whole = TRUE), list(verified = TRUE, match = 1))
  expect_true(is.na(readgpt:::span_match("...", src, whole = TRUE)$verified))
  # A chunk set that does not match still reports the evidence unverified.
  expect_false(readgpt:::span_match("Revenue rose 12%.", "Costs were flat.", whole = TRUE)$verified)
})

# ---------------------------------------------------------------------------
# read-3: a hard line wrap is not a passage break
# ---------------------------------------------------------------------------

wrapped <- paste0("The trial enrolled patients across nine sites and the median age was\n",
                  "45. Most patients were male and the drug did not\n",
                  "reduce mortality at one year.")

test_that("a changed figure or a dropped word at a line wrap does not verify", {
  bad <- c("the median age was\n54. Most patients were male",
           "the median age was\n5. Most patients were male",
           "Most patients were male and the drug did\nreduce mortality at one year.",
           "nine sites\nreduce mortality at one year.")
  for (s in bad) {
    m <- sm(s, wrapped)
    expect_false(m$verified, label = s)
    expect_lt(m$match, 1, label = s)
  }
  # The same wrap, copied faithfully, verifies.
  expect_true(verified("the median age was\n45. Most patients were male", wrapped))
  expect_true(verified(wrapped, wrapped))
})

test_that("a wrapped .txt file keeps a changed figure out of a skim answer", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c(strsplit(wrapped, "\n")[[1]], "", "Second paragraph here."), f)
  a <- skim_read(f, "the median age was\n54. Most patients were male")
  expect_true(a$partial)
  expect_false(gr_verify_evidence(a)$verified)
  a <- skim_read(f, "Most patients were male and the drug did\nreduce mortality at one year.")
  expect_true(a$partial)
  a <- skim_read(f, "the median age was\n45. Most patients were male")
  expect_false(a$partial)
})

test_that("passages the model separated still verify on their own", {
  src <- paste("Overall, 25% of patients had nausea during the trial.",
               "The study was funded by the national research council.",
               "Headache was reported by 12 patients.")
  good <- c("Overall, 25% of patients had nausea during the trial.\nHeadache was reported by 12 patients.",
            "Overall, 25% of patients had nausea during the trial\nHeadache was reported by 12 patients",
            "1. Overall, 25% of patients had nausea\n2. Headache was reported by 12 patients",
            "- Overall, 25% of patients had\n  nausea during the trial\n- Headache was reported by 12 patients",
            "(a) Headache was reported by 12 patients.",
            "\u201cOverall, 25% of patients had nausea\u201d\n\u201cfunded by the national research council\u201d")
  for (s in good) expect_true(verified(s, src), label = s)
})

test_that("the pieces of an elided quotation have to be in order", {
  src <- "Revenue grew by 3%. Costs fell 12%."
  expect_true(verified("Revenue grew ... Costs fell 12%", src))
  m <- sm("Costs fell ... Revenue grew", src)
  expect_false(m$verified)
  expect_lt(m$match, 1)
  expect_false(verified("Costs fell 12% [...] Revenue grew by 3%", src))
})

# ---------------------------------------------------------------------------
# read-4, read-core-01: typography around a figure is not a different figure
# ---------------------------------------------------------------------------

test_that("an em dash is not a minus sign, whichever way it is written", {
  for (dash in c("\u2014", "--", "\u2015")) {
    src <- paste0("The cohort", dash, "42 patients in all", dash, "was followed.")
    expect_true(verified("42 patients in all", src), label = dash)
  }
  expect_true(verified("1,204 patients were randomised by March.",
                       "Enrolment exceeded the target\u20141,204 patients were randomised by March."))
  expect_true(verified("12 patients died.", "In the cohort\u201412 patients died."))
  # An en dash between two words is punctuation too; before a number after a
  # space it is a minus sign, and between numbers a range.
  expect_true(verified("42 patients", "The cohort\u201342 patients in all."))
  expect_false(verified("12% year on year", "Change in revenue: \u201312% year on year."))
  expect_false(verified("30 patients", "Between 20\u201330 patients were seen."))
  # A minus sign, a hyphen and a compound are still what they were.
  expect_false(verified("12% year on year", "Change in revenue: -12% year on year."))
  expect_false(verified("12% year on year", "Change in revenue: \u221212% year on year."))
  expect_false(verified("19 patients were excluded", "COVID-19 patients were excluded."))
})

test_that("a superscript reference glued to a word does not hide the word", {
  expect_true(verified("as previously reported",
                       "Mortality was lower, as previously reported12 and confirmed later."))
  expect_true(verified("The primary outcome was all-cause mortality.",
                       "The primary outcome was all-cause mortality12. It was assessed at one year."))
  a <- skim_read("Mortality was lower in the treated group, as previously reported12 and confirmed here.",
                 "Mortality was lower in the treated group, as previously reported")
  expect_false(a$partial)
  # A number glued to a word is still not that number alone.
  expect_false(verified("19 patients", "In covid19 patients"))
})

test_that("a year closing a sentence before a footnote number is the year", {
  expect_true(verified("Enrolment ended in 2019.", "Enrolment ended in 2019.4 Patients were then followed."))
  expect_true(verified("Enrolment ended in 2019", "Enrolment ended in 2019.4,5 Patients were followed."))
  # Anything else followed by a full stop and a digit is a decimal.
  expect_false(verified("Revenue rose to 45.", "Revenue rose to 45.2 million dollars."))
  expect_false(verified("The dose was 45.", "The dose was 45.5 Gy in 25 fractions."))
  expect_false(verified("In 2019.", "The index stood at 2019.45 at the close."))
})

test_that("a decimal with no leading zero and a hyphenated negation are caught", {
  expect_false(verified("5 mg twice daily for twelve weeks.",
                        "Patients received .5 mg twice daily for twelve weeks."))
  expect_false(verified("inferior to placebo at 12 weeks.",
                        "The drug was non-inferior to placebo at 12 weeks."))
  expect_false(verified("significant reduction in mortality",
                        "There was a non-significant reduction in mortality."))
  expect_false(verified("The patients were HIV", "The patients were HIV-negative at entry."))
  # A full stop closing a sentence with no space after it is not a decimal point.
  expect_true(verified("5 patients died", "It ended the trial.5 patients died in all."))
  # A dash used as punctuation next to a word is not a hyphen.
  expect_true(verified("patients were followed", "The cohort--patients were followed."))

  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("Of these, .5 thousand patients were randomised in the first year.", f)
  cl <- gr_mock_client(function(m, p) '{"n":5,"n__quote":"5 thousand patients were randomised"}')
  x <- quiet(gr_extract(f, gr_fields(n = gr_field("Number randomised", type = "integer")),
                        client = cl, recipe = "thorough", require_quote = TRUE))
  expect_true(is.na(x$table$n[1]))
})

# ---------------------------------------------------------------------------
# read-6: digits grouped with a no-break or thin space
# ---------------------------------------------------------------------------

test_that("a dropped digit group is caught when digits are grouped with a thin space", {
  for (sp in c("\u202f", "\u00a0", "\u2009", "\u2007")) {
    src <- paste0("Nous avons inclus 1", sp, "200 patients.")
    expect_false(verified("200 patients", src), label = sprintf("U+%04X", utf8ToInt(sp)))
    expect_false(verified("inclus 1", src))
    expect_true(verified(paste0("1", sp, "200 patients"), src))
    expect_true(verified("1 200 patients", src))
  }
  expect_false(verified("200 000 patients", "We saw 1\u202f200\u202f000 patients."))
  # A space between a number and its unit, or two numbers, is not a group.
  expect_true(verified("5 mg", "a dose of 5\u00a0mg daily"))
  # An ASCII space is ambiguous ("arm 1 200 patients"), and stays as it was.
  expect_true(verified("200 patients", "Nous avons inclus 1 200 patients."))
})

# ---------------------------------------------------------------------------
# cross-8: what the check costs
# ---------------------------------------------------------------------------

test_that("the source the boundary test reads lines up with the normalised text", {
  ms <- readgpt:::match_source
  nm <- readgpt:::normalise_for_match
  x <- c("  A\u00a0 b\t\tc\u2014d \u2013 e\u202f1\u2009200  ", "\u00a0lead", "trail\u202f",
         "\u201cQ\u201d \u2018q\u2019 x\u2212y", "", " ", "\u00a0", "\u00a0 ", "\u0130STANBUL \u00c9T\u00c9",
         "\u5171\u7eb3\u5165 120\u540d", "a\n\nb\r\nc")
  for (s in x) {
    got <- ms(s)
    expect_identical(got$text, nm(s), label = s)
    expect_identical(got$n, nchar(got$text), label = s)
    expect_identical(length(got$cp), got$n + 7L)
  }
})

test_that("a short word inside many longer ones is checked without rescanning the source", {
  skip_on_cran()
  # Every occurrence of "in" but the last is inside "within". Checking each by
  # copying the rest of the source took seconds on sources this size.
  src <- paste(c(rep("within", 40000), "in the trial"), collapse = " ")
  t <- system.time(m <- sm("participants in the cohort", src))[["elapsed"]]
  expect_false(m$verified)
  expect_equal(m$match, 0.5)
  expect_lt(t, 2)
  expect_identical(readgpt:::found_at("in the trial", src), nchar(src) - 11L)
  # Occurrences that overlap one another are all tried.
  expect_identical(readgpt:::found_at("1 1", "11 1 1"), 4L)
  expect_identical(readgpt:::found_at("aa", "aaa aa"), 5L)
  expect_identical(readgpt:::found_at("the", "the trial and the", from = 2L), 15L)
})

# ---------------------------------------------------------------------------
# cross-3: the model a read asked for is on the record
# ---------------------------------------------------------------------------

test_that("the pre-flight record names the model a read asked for", {
  ch <- quiet(gr_segment(sample_doc(2, 2), list(method = "paragraph", max_tokens = 150)))
  tr <- gr_trace()
  a <- quiet(gr_read(ch, "Q?", mock_echo(), list(reader = "skim", skim_model = "gpt-4o-mini"),
                     trace = tr))
  pf <- Filter(function(s) identical(s$label, "preflight"), tr$steps)[[1]]$detail
  # The client's model, which a replay has to ask for again, though it is not
  # a setting and most of the run's calls went to the skim model.
  expect_identical(pf$model, mock_echo()$model)
  expect_null(pf$settings$model)
  expect_identical(pf$settings$skim_model, "gpt-4o-mini")
})
