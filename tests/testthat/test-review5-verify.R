# test-review5-verify.R -- the fifth pass over quotation checking: what the
# fourth pass's elision and line-wrap rules still let through outside English
# and across a line break, what the whole-word check still refused that the
# document states, and what the check cost on a long source.
#
# Each block names the finding and says what the old behaviour was.

sm <- function(span, src, ...) readgpt:::span_match(span, src, ...)
verified <- function(span, src, ...) isTRUE(sm(span, src, ...)$verified)

skim_read <- function(doc, quote) {
  ch <- quiet(gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 400)))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("You extract evidence", messages[[1]]$content, fixed = TRUE)) return(quote)
    "The answer."
  })
  quiet(gr_read(ch, "What does it say?", cl, "skim"))
}
txt_file <- function(lines) {
  f <- withr::local_tempfile(fileext = ".txt", .local_envir = parent.frame())
  writeLines(lines, f, useBytes = TRUE)
  f
}

# ---------------------------------------------------------------------------
# verify-1, read-1, corpus-4, read-core-05: an elision that drops a negation
# outside English. The gap was checked against an English word list, and a
# Chinese or Thai negation sits inside a run of letters no word split finds,
# so these came back verified with match 1 and gr_extract() kept the value.
# ---------------------------------------------------------------------------

test_that("an elision that drops a negation does not verify in other languages", {
  dropped <- list(
    # Chinese: <mei you>, <wei neng>, <wu>, <bing wei>
    c("\u8be5\u836f\u7269\u2026\u2026\u964d\u4f4e\u4e00\u5e74\u6b7b\u4ea1\u7387",
      "\u8bd5\u9a8c\u4e2d\uff0c\u8be5\u836f\u7269\u6ca1\u6709\u964d\u4f4e\u4e00\u5e74\u6b7b\u4ea1\u7387\u3002"),
    c("\u8be5\u836f\u7269\u2026\u2026\u964d\u4f4e\u6b7b\u4ea1\u7387",
      "\u7ed3\u679c\u663e\u793a\uff0c\u8be5\u836f\u7269\u672a\u80fd\u964d\u4f4e\u6b7b\u4ea1\u7387\u3002"),
    c("\u6cbb\u7597\u7ec4\u4e0e\u5bf9\u7167\u7ec4\u2026\u2026\u663e\u8457\u5dee\u5f02",
      "\u6cbb\u7597\u7ec4\u4e0e\u5bf9\u7167\u7ec4\u65e0\u663e\u8457\u5dee\u5f02\u3002"),
    c("\u6cbb\u7597\u7ec4\u7684\u6b7b\u4ea1\u7387\u2026\u2026\u964d\u4f4e",
      "\u7ed3\u679c\u663e\u793a\u6cbb\u7597\u7ec4\u7684\u6b7b\u4ea1\u7387\u5e76\u672a\u964d\u4f4e\u3002"),
    # Thai: <mai dai>
    c("\u0e22\u0e32\u2026\u0e25\u0e14\u0e2d\u0e31\u0e15\u0e23\u0e32\u0e01\u0e32\u0e23\u0e15\u0e32\u0e22",
      paste0("\u0e1c\u0e25\u0e01\u0e32\u0e23\u0e28\u0e36\u0e01\u0e29\u0e32\u0e1e\u0e1a\u0e27\u0e48\u0e32",
             "\u0e22\u0e32\u0e44\u0e21\u0e48\u0e44\u0e14\u0e49\u0e25\u0e14\u0e2d\u0e31\u0e15\u0e23\u0e32",
             "\u0e01\u0e32\u0e23\u0e15\u0e32\u0e22\u0e02\u0e2d\u0e07\u0e1c\u0e39\u0e49\u0e1b\u0e48\u0e27\u0e22")),
    # Korean: <eopneun>
    c("\uc774 \uc57d\uc740 \ud6a8\uacfc\uac00 ... \uac83\uc73c\ub85c \ub098\ud0c0\ub0ac\ub2e4",
      "\uc774 \uc57d\uc740 \ud6a8\uacfc\uac00 \uc5c6\ub294 \uac83\uc73c\ub85c \ub098\ud0c0\ub0ac\ub2e4."),
    c("le traitement ... r\u00e9duit la mortalit\u00e9 \u00e0 un an",
      "Dans cet essai, le traitement n'a pas r\u00e9duit la mortalit\u00e9 \u00e0 un an."),
    c("Le m\u00e9dicament n'a ... r\u00e9duit la mortalit\u00e9",
      "Le m\u00e9dicament n'a pas r\u00e9duit la mortalit\u00e9."),
    c("O medicamento ... reduziu a mortalidade", "No ensaio, o medicamento n\u00e3o reduziu a mortalidade."),
    c("\u041f\u0440\u0435\u043f\u0430\u0440\u0430\u0442 ... \u0441\u043d\u0438\u0437\u0438\u043b",
      "\u041f\u0440\u0435\u043f\u0430\u0440\u0430\u0442 \u043d\u0435 \u0441\u043d\u0438\u0437\u0438\u043b."),
    c("senkte das Medikament die Sterblichkeit ... signifikant.",
      "Im Hauptmodell senkte das Medikament die Sterblichkeit nicht signifikant."),
    c("the drug ... reduced mortality", "The drug hardly reduced mortality.")
  )
  for (x in dropped) {
    m <- sm(x[1], x[2])
    expect_false(m$verified, label = x[1])
    expect_lt(m$match, 1, label = x[1])
  }

  # Faithful elisions in the same languages still verify.
  kept <- list(
    c("\u8be5\u836f\u7269\u2026\u2026\u964d\u4f4e\u4e86\u4e00\u5e74\u6b7b\u4ea1\u7387",
      "\u8be5\u836f\u7269\u663e\u8457\u964d\u4f4e\u4e86\u4e00\u5e74\u6b7b\u4ea1\u7387\u3002"),
    c("\u5171\u7eb3\u5165120\u540d\u60a3\u8005\u2026\u2026\u6765\u81ea\u4e5d\u4e2a\u4e34\u5e8a\u4e2d\u5fc3",
      "\u672c\u7814\u7a76\u5171\u7eb3\u5165120\u540d\u60a3\u8005\uff0c\u6765\u81ea\u4e5d\u4e2a\u4e34\u5e8a\u4e2d\u5fc3\u3002"),
    c("le traitement ... a r\u00e9duit la mortalit\u00e9", "le traitement \u00e9tudi\u00e9 a r\u00e9duit la mortalit\u00e9."),
    c("\u041f\u0440\u0435\u043f\u0430\u0440\u0430\u0442 ... \u0441\u043d\u0438\u0437\u0438\u043b",
      "\u041f\u0440\u0435\u043f\u0430\u0440\u0430\u0442 \u0437\u043d\u0430\u0447\u0438\u0442\u0435\u043b\u044c\u043d\u043e \u0441\u043d\u0438\u0437\u0438\u043b.")
  )
  for (x in kept) expect_identical(sm(x[1], x[2]), list(verified = TRUE, match = 1), label = x[1])
})

test_that("a gap in a script the negation lists cannot read is refused", {
  gn <- readgpt:::gap_negated
  # Georgian and Tamil letters: nothing here can tell whether they negate.
  expect_true(gn(" \u10d0\u10e0 "))
  expect_true(gn(" \u0b87\u0bb2\u0bcd\u0bb2\u0bc8 "))
  expect_false(gn(" given daily, "))
  expect_false(gn(" 12% "))
  expect_true(gn(" wasn't "))
  expect_true(gn(" NOT "))
  # "no." before a number or an identifier is the abbreviation.
  expect_false(gn(" (registration no. isrctn12345) "))
  expect_true(gn(" had no effect on "))
  # The gap rule takes the normalised text as a string too, the way the
  # extract reader's quotation check reads a chunk.
  expect_false(readgpt:::elision_gap_ok("the drug did not reduce mortality", 12L, 18L))
  expect_true(readgpt:::elision_gap_ok("the drug did then reduce mortality", 12L, 19L))
})

test_that("a dropped negation outside English makes a skim answer partial and drops the value", {
  zh <- paste0("\u8bd5\u9a8c\u4e2d\uff0c\u8be5\u836f\u7269\u6ca1\u6709\u964d\u4f4e\u4e00\u5e74",
               "\u6b7b\u4ea1\u7387\u3002\u4e0d\u826f\u4e8b\u4ef6\u76f8\u4f3c\u3002")
  q <- "\u8be5\u836f\u7269\u2026\u2026\u964d\u4f4e\u4e00\u5e74\u6b7b\u4ea1\u7387"
  a <- skim_read(zh, q)
  expect_true(a$partial)
  expect_false(gr_verify_evidence(a)$verified)

  f <- txt_file("Dans cet essai, le traitement n'a pas r\u00e9duit la mortalit\u00e9 \u00e0 un an.")
  cl <- gr_mock_client(function(m, p) paste0(
    '{"effect":"r\u00e9duit la mortalit\u00e9",',
    '"effect__quote":"le traitement ... r\u00e9duit la mortalit\u00e9 \u00e0 un an"}'))
  x <- quiet(gr_extract(f, gr_fields(effect = gr_field("Effect on mortality")), client = cl,
                        recipe = "thorough", require_quote = TRUE))
  expect_true(is.na(x$table$effect[1]))
  expect_identical(x$table$n_unverified[1], 1L)
})

test_that("an elision over a bracketed citation verifies, and a sentence in brackets still ends one", {
  expect_true(verified("the drug reduced pain ... in 240 patients over 12 weeks.",
                       paste("In the main trial the drug reduced pain (Smith et al. 2019; Jones et al.",
                             "2020) in 240 patients over 12 weeks. Adverse events were mild.")))
  expect_true(verified("The trial ... enrolled 240 patients",
                       "The trial (registration no. ISRCTN12345) enrolled 240 patients."))
  expect_true(verified("The cohort ... was followed for two years",
                       "The cohort (mean age 54.6 years; 48% women) was followed for two years."))
  # A negation inside the brackets is still left out.
  expect_false(verified("the drug ... reduced mortality", "The drug (not the placebo) reduced mortality."))
  # "(See Table 2.)" ends with a sentence end of its own, and the sentence
  # before it ended too.
  expect_false(verified("Revenue ... rose 30%", "Revenue fell 12% (see Table 2.) Costs rose 30%."))
})

# ---------------------------------------------------------------------------
# verify-2, read-3, read-core-05: a line that opens with a capital started a
# new passage checked on its own, so a quotation that dropped a word or a
# whole line at a hard wrap verified when the next line began with a name,
# an acronym or a German noun; and passages the model separated (a blank
# line, bullets, quote marks) were checked with nothing between them, so they
# could drop a "not" too.
# ---------------------------------------------------------------------------

test_that("a quotation that skips words at a wrap before a capital does not verify", {
  bad <- list(
    c("The drug reduced mortality in patients enrolled in\nAsia or Africa.",
      "The drug reduced mortality in patients enrolled in\nEurope but not in patients enrolled in\nAsia or Africa."),
    c("In der Studie gab es\nVerbesserung der Mortalit\u00e4t nach einem Jahr.",
      "In der Studie gab es keine\nVerbesserung der Mortalit\u00e4t nach einem Jahr."),
    c("Patients who were\nHispanic were excluded from the analysis.",
      "Patients who were not\nHispanic were excluded from the analysis."),
    c("Patients\nHIV infection were enrolled.", "Patients without\nHIV infection were enrolled."),
    c("Patients were treated at\nMayo Clinic after 2019.",
      "Patients were treated at\nJohns Hopkins, and none were enrolled at\nMayo Clinic after 2019."),
    # The two lines from different sentences read as one false sentence.
    c("Patients were treated at\nMayo Clinic after 2019.",
      "Patients were treated at Johns Hopkins. None were treated at\nMayo Clinic after 2019.")
  )
  for (x in bad) {
    m <- sm(x[1], x[2])
    expect_false(m$verified, label = x[1])
    expect_lt(m$match, 1, label = x[1])
  }
  # The same text quoted faithfully verifies, wrapped or not.
  for (x in bad[1:5]) expect_true(verified(x[2], x[2]), label = x[2])
  expect_true(verified("Results\nMortality fell by 12%.", "Results\n\nMortality fell by 12%. Costs rose."))
})

test_that("passages the model separated may not leave words out of one sentence", {
  src <- "In the main analysis the drug did not reduce mortality at 12 months. Adverse events were mild."
  for (q in c("the drug did\n\nreduce mortality at 12 months.",
              "- the drug did\n- reduce mortality at 12 months.",
              "\"the drug did\" \"reduce mortality at 12 months.\"",
              "the drug did ...\nreduce mortality at 12 months.")) {
    m <- sm(q, src)
    expect_false(m$verified, label = q)
    expect_lt(m$match, 1, label = q)
  }
  # From different sentences, in either order, or with only punctuation
  # between them, separate passages still verify.
  two <- paste("Overall, 25% of patients had nausea during the trial. In addition,",
               "headache was reported by 12 patients.")
  for (q in c("- 25% of patients had nausea\n- headache was reported by 12 patients",
              "- Headache was reported by 12 patients.\n- Overall, 25% of patients had nausea",
              "Overall, 25% of patients had nausea during the trial\nIn addition, headache was reported")) {
    expect_true(verified(q, two), label = q)
  }
  expect_true(verified("Revenue grew in the second quarter\nCosts fell sharply.",
                       "Revenue grew in the second quarter of 2023. Costs fell sharply."))
  expect_true(verified("- the cohort\n- 42 patients", "The cohort\u201442 patients in all."))
})

test_that("separate passages are paired with the nearest occurrence, and a copy of one place verifies", {
  # "the drug" occurs twice: the second is right before the second passage.
  expect_true(verified("- the drug\n- was well tolerated",
                       "The drug reduced pain. The drug was well tolerated."))
  expect_true(verified("rose\nU.S. sales", "Costs rose, fell and rose\nU.S. sales too."))
  # Only the nearer occurrence pairs with the second passage, and it drops a
  # negation.
  expect_false(verified("- the drug\n- reduce mortality",
                        "The drug was given daily. The drug did not reduce mortality."))
  # A wrapped line ending with a quote mark or a dash is joined as written,
  # before its edges are trimmed.
  s <- "The panel called it \"the Oxford rule\"\nWHO adopted it in 2019."
  expect_true(verified(s, s))
  s <- "Doses were raised slowly -\nFDA guidance allowed it."
  expect_true(verified(s, s))
  # An ellipsis the source itself has is no elision to judge.
  expect_true(verified("the results were mixed. \u2026 Costs rose later.",
                       "He said the results were mixed. \u2026 Costs rose later."))
  expect_true(verified("\u2026: .5, and more", "Values \u2026: .5, and more were seen."))
})

test_that("a skim quotation that drops a line of a wrapped file is partial", {
  f <- txt_file(c("Patients who were not", "Hispanic were excluded from the analysis.", "",
                  "Second paragraph here."))
  a <- skim_read(f, "Patients who were\nHispanic were excluded from the analysis.")
  expect_true(a$partial)
  expect_false(gr_verify_evidence(a)$verified)
  a <- skim_read(f, "Patients who were not\nHispanic were excluded from the analysis.")
  expect_false(a$partial)
})

# ---------------------------------------------------------------------------
# verify-3: a quotation, or a verbatim chunk, beginning with a decimal written
# without a leading zero (".45", ".001") or with an ellipsis before a number
# lost its full stop to the edge trim and was refused, because the source has
# a full stop before the digit. The package's own chunks failed to match
# themselves.
# ---------------------------------------------------------------------------

test_that("a decimal with no leading zero at the start of a quotation verifies", {
  expect_true(verified(".5 mg twice daily", "Patients received .5 mg twice daily for twelve weeks."))
  expect_true(verified(".45 (95% CI .30 to .58)", "r = .45 (95% CI .30 to .58)"))
  expect_true(verified(".001", "p < .001."))
  expect_true(verified("...12 patients were excluded after screening.",
                       "...12 patients were excluded after screening."))
  expect_true(verified("...12 patients were excluded", "Of these, 12 patients were excluded."))
  expect_identical(readgpt:::trim_quote_edges("\".45 in all.\""), ".45 in all")
  expect_identical(readgpt:::trim_quote_edges("...12 patients"), "12 patients")
  # The kept full stop is the number's: a digit, a sign or a stop before it
  # in the source is a different number.
  for (src in c("The correlation was 1.45 in the treatment group.",
                "The correlation was 0.45 in the treatment group.",
                "The correlation was -.45 in the treatment group.")) {
    expect_false(verified(".45 in the treatment group", src), label = src)
  }
  expect_false(verified("5 mg twice daily", "Patients received .5 mg twice daily."))
})

test_that("a chunk that begins with a bare decimal or an ellipsis verifies as its own evidence", {
  for (s in c(".45 for engagement and retention, .30 for engagement and tenure.",
              "...12 patients were excluded after screening.")) {
    expect_identical(sm(s, s, whole = TRUE), list(verified = TRUE, match = 1), label = s)
  }
  f <- txt_file("The correlation was r = .45 between engagement and retention.")
  cl <- gr_mock_client(function(m, p) '{"r":0.45,"r__quote":".45 between engagement and retention"}')
  x <- quiet(gr_extract(f, gr_fields(r = gr_field("Correlation", type = "number")), client = cl,
                        recipe = "thorough", require_quote = TRUE))
  expect_equal(x$table$r[1], 0.45)
  expect_identical(x$table$n_unverified[1], 0L)
})

# ---------------------------------------------------------------------------
# read-4, read-core-01: the default cleaner turns an unspaced en dash into a
# hyphen, and a hyphen between a letter and a digit read as a minus sign or a
# compound, so "the target<en dash>1,204 patients" refused a quotation of
# "1,204 patients". A long enough word before it and a long number after it
# make it a dash; compounds and signs are still what they were.
# ---------------------------------------------------------------------------

test_that("a cleaned en dash after a word does not hide the number after it", {
  expect_true(verified("1,204 patients were randomised by March.",
                       "Enrolment exceeded the target-1,204 patients were randomised by March."))
  f <- txt_file("Enrolment exceeded the target\u20131,204 patients were randomised by March.")
  cl <- gr_mock_client(function(m, p) '{"n":1204,"n__quote":"1,204 patients were randomised by March."}')
  x <- quiet(gr_extract(f, gr_fields(n = gr_field("Number randomised", type = "integer")),
                        client = cl, recipe = "thorough", require_quote = TRUE))
  expect_identical(x$table$n[1], 1204L)
  a <- skim_read(f, "1,204 patients were randomised by March.")
  expect_false(a$partial)

  expect_false(verified("19 patients were excluded", "COVID-19 patients were excluded."))
  expect_false(verified("3 adverse events occurred", "In all, grade-3 adverse events occurred."))
  expect_false(verified("2019 levels were higher", "Our pre-2019 levels were higher."))
  expect_false(verified("12% year on year", "Change in revenue: -12% year on year."))
  expect_false(verified("30 patients", "Between 20-30 patients were seen."))
})

# ---------------------------------------------------------------------------
# verify-5: every Unicode number counted as a digit, so a circled list number
# before a figure, or a superscript footnote mark after one, read as the
# number carrying on and a faithful quotation was refused.
# ---------------------------------------------------------------------------

test_that("a circled list number or a footnote mark is not part of the number", {
  expect_true(verified("120\u4f8b\u60a3\u8005\u63a5\u53d7\u6cbb\u7597",
                       "\u2460120\u4f8b\u60a3\u8005\u63a5\u53d7\u6cbb\u7597\uff1b\u2461\u968f\u8bbf\u4e24\u5e74\u3002"))
  expect_true(verified("2\u578b\u7cd6\u5c3f\u75c5\u60a3\u8005120\u4f8b",
                       "\u24602\u578b\u7cd6\u5c3f\u75c5\u60a3\u8005120\u4f8b\uff1b\u2461\u5bf9\u7167\u7ec480\u4f8b\u3002"))
  expect_true(verified("The trial enrolled 482.", "The trial enrolled 482\u00b9. Follow-up lasted two years."))
  expect_true(verified("Enrolment ended in 2019.", "Enrolment ended in 2019\u00b2. Patients were followed."))
  expect_true(verified("Enrolment ended in 2019.", "Enrolment ended in 2019.\u2074 Patients were followed."))
  # A superscript on 10 is an exponent, and a fraction carries on.
  expect_false(verified("The dose was 10", "The dose was 10\u2076 cells."))
  expect_false(verified("mixed 1", "mixed 1\u00bd cups"))
  expect_identical(readgpt:::char_kind(c(0x2460L, 0x2474L, 0x2776L, 0x3251L, 0xB9L, 0xBDL, 0xFF11L)),
                   c(0L, 0L, 0L, 0L, 2L, 2L, 2L))

  f <- txt_file("The trial enrolled 482\u00b9. Follow-up lasted two years.")
  cl <- gr_mock_client(function(m, p) '{"n":482,"n__quote":"The trial enrolled 482."}')
  x <- quiet(gr_extract(f, gr_fields(n = gr_field("Participants", type = "integer")), client = cl,
                        recipe = "thorough", require_quote = TRUE))
  expect_identical(x$table$n[1], 482L)
})

test_that("a Korean quotation may stop before a particle, not inside a word", {
  src <- "\uc774 \uc5f0\uad6c\uc5d0\ub294 \ud658\uc790 120\uba85\uc774 \ucc38\uc5ec\ud588\ub2e4."
  expect_true(verified("\ud658\uc790 120\uba85", src))
  expect_false(verified("\ud658\uc790 12\uba85", src))
  # A word cut out of a longer one is not that word: <an jeon> in <bul an jeon>.
  expect_false(verified("\uc548\uc804\ud55c \uc57d\ubb3c\uc774\ub2e4",
                        "\uc774\uac83\uc740 \ubd88\uc548\uc804\ud55c \uc57d\ubb3c\uc774\ub2e4."))
})

# ---------------------------------------------------------------------------
# cross-8, verify-4: what the check costs. An elided quotation of common words
# read every placement's gap by copying it out of the source, a near-verbatim
# Chinese quotation was scored a character at a time from every start, and
# every source was lower-cased a character at a time through chartr().
# ---------------------------------------------------------------------------

test_that("the source the boundary test reads still lines up with the normalised text", {
  ms <- readgpt:::match_source
  for (s in c("ABC \u00c9T\u00c9 \u0130STANBUL \u0391\u0392 \u0414\u0416 \uff21\uff22 \u2160 \u24b6",
              "\u5171\u7eb3\u5165 120\u540d", "", "Mixed \u5171 CASE")) {
    got <- ms(s)
    expect_identical(got$text, readgpt:::normalise_for_match(s), label = s)
    expect_identical(got$memo$lc, utf8ToInt(got$text), label = s)
    expect_identical(readgpt:::fold_case(s), readgpt:::lower_text(s), label = s)
  }
})

test_that("an elided quotation of common words is checked quickly and correctly on a long source", {
  skip_on_cran()
  set.seed(5)
  filler <- c("The model is fitted to the data.", "Then the report is printed for the user.",
              "The drug did not reduce mortality in the older cohort.", "Caf\u00e9 notes were kept.")
  big <- paste(c(sample(filler, 6000, TRUE), "In the end the extension is not loaded."), collapse = " ")
  t <- system.time(m <- sm("the ... is not", big))[["elapsed"]]
  expect_true(m$verified)
  expect_lt(t, 3)
  t <- system.time(m <- sm("the drug did ... reduce mortality", big))[["elapsed"]]
  expect_false(m$verified)
  expect_lt(t, 3)
})

test_that("a near-verbatim Chinese quotation is scored without a search per character pair", {
  skip_on_cran()
  set.seed(6)
  pool <- c(0x7684, 0x4e00:0x4e60, 0x60a3, 0x8005)
  sent <- function() paste0(intToUtf8(sample(pool, sample(15:40, 1), TRUE)), "\u3002")
  doc <- paste(replicate(3000, sent()), collapse = "")
  cp <- utf8ToInt(doc)
  q <- cp[40001:40300]
  q[150] <- utf8ToInt("\u9519")
  t <- system.time(m <- sm(intToUtf8(q), doc))[["elapsed"]]
  expect_false(m$verified)
  expect_gt(m$match, 0.4)
  expect_lt(t, 1.5)
})
