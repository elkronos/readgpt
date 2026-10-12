# test-synthesise-figures.R -- the numbers in a written review, checked against
# the rows each sentence cites.
#
# Before: the citation check walked a sentence back to a row and stopped.
# "d = 0.71 [study 1]" over a row reporting 0.41 was a clean, fully cited
# sentence, and a revision pass could turn 0.41 into 0.71 after the fact with
# every guard passing it. Each test here failed on that code, or pins down the
# forms of a faithful copy the check must not cry wolf at.

fig_table <- function() {
  data.frame(
    document = c("a.pdf", "b.pdf", "c.pdf"),
    status = "ok",
    design = c("randomised trial", "cohort study", "randomised trial"),
    n = c(120L, 900L, 1204L),
    effect = c("d = 0.613 (95% CI 0.32-0.90)", "OR 1.8 (1.2 to 2.7)", "mean difference -0.12"),
    followup = c("twelve weeks", "12 months", "24 weeks"),
    revenue = c("$45,200,000", NA, "54% female"),
    # Withheld from the writer as bibliographic; see the year and title tests.
    year = c(2019L, 2021L, 2018L),
    title = c("A 36-week pilot", "Cohort", "Trial"),
    stringsAsFactors = FALSE)
}

# A client that writes `text` for every section, and revises with `revise`.
fig_client <- function(text, revise = NULL) {
  gr_mock_client(function(messages, params) {
    last <- messages[[length(messages)]]$content
    if (grepl("<draft>", last, fixed = TRUE)) {
      d <- sub("(?s)^.*<draft>\n", "", last, perl = TRUE)
      d <- sub("(?s)\n</draft>.*$", "", d, perl = TRUE)
      return(if (is.function(revise)) revise(d) else d)
    }
    text
  })
}

fig_synth <- function(text, ..., revise = NULL, question = "Does it work?",
                      outline = c(Findings = "What they found")) {
  cl <- fig_client(text, revise)
  s <- suppressMessages(gr_synthesise(fig_table(), question = question, outline = outline,
                                      client = cl, cite_style = "marker", ...))
  attr(s, "client") <- cl
  s
}

warn_classes <- function(expr) {
  classes <- character(0)
  value <- withCallingHandlers(suppressMessages(expr), warning = function(w) {
    classes <<- c(classes, class(w)[1])
    invokeRestart("muffleWarning")
  })
  list(value = value, classes = classes)
}

# ---------------------------------------------------------------------------
# Reading a sentence's figures.
# ---------------------------------------------------------------------------

test_that("a sentence's figures are read in every form a review writes them", {
  f <- function(s) readgpt:::figure_find(s)
  x <- f("The effect was moderate, d = 0.61 [95% CI 0.32 to 0.90] [study 3].")
  expect_identical(x$written, c("0.61", "0.32", "0.90"))
  expect_identical(x$value, c("0.61", "0.32", "0.9"))
  # Grouped thousands and a decimal with no leading zero are one figure each;
  # a significance threshold is not a figure.
  x <- f("The trial enrolled n = 1,204 adults (p < .001) [study 5].")
  expect_identical(x$value, "1204")
  x <- f("Correlations were weak (r = .21, p = .03) [study 2].")
  expect_identical(x$value, c("0.21", "0.03"))
  # An en-dash range is two figures, and a minus sign is not part of one.
  x <- f("Odds rose (OR 1.8, 95% CI 1.2\u20132.7); effects ran from \u22120.12 to 0.45 [studies 1-3].")
  expect_identical(x$value, c("1.8", "1.2", "2.7", "0.12", "0.45"))
  # Scale words and percentages are kept with the figure.
  x <- f("Revenue reached 45.2 million and costs $3.1bn; 23.5% responded [study 2].")
  expect_identical(x$value, c("45.2", "3.1", "23.5"))
  expect_identical(x$scale, c(6L, 9L, 0L))
  expect_identical(x$pct, c(FALSE, FALSE, TRUE))
  # Qualifiers are read off the words before the figure.
  x <- f("About 1,200 took part, more than 1,000 finished and fewer than 50 withdrew [study 3].")
  expect_identical(x$qualifier, c("about", "over", "under"))
})

test_that("ids, names, ordinals, decades, conventions and small counts are not figures", {
  f <- function(s) readgpt:::figure_find(s)$value
  expect_identical(f("Two trials agreed [study 12] [studies 14-16] [study 3; study 17]."),
                   character(0))
  expect_identical(f("As study 12 found, and [claim 3] says [study 12]."), character(0))
  # An unparsed citation's numbers are ids nobody checked, not figures.
  expect_identical(f("All of them [studies 11 to 17]."), character(0))
  expect_identical(f("In COVID-19 patients IL-6, HbA1c and the SF-36 were read [study 2]."),
                   character(0))
  expect_identical(f("The 2nd trial, from the 1990s, ran 3 arms in 2 sites [study 8]."),
                   character(0))
  expect_identical(f("Estimates had a 95% CI and a 90 percent confidence interval [study 1]."),
                   character(0))
  # A small count as a percentage or scaled is a figure.
  expect_identical(f("Mortality was 8% in a city of 3 million [study 1]."), c("8", "3"))
  expect_identical(f("An OR of 1.0 [study 1]."), "1")
})

test_that("figures compare in one canonical form", {
  canon <- readgpt:::figure_canon
  expect_identical(canon(c(".61", "0.610", "45.20", "1,204", "007", "0.0", "1\u00a0204")),
                   c("0.61", "0.61", "45.2", "1204", "7", "0", "1204"))
  shift <- readgpt:::figure_shift
  expect_identical(shift("45.2", 6L), "45200000")
  expect_identical(shift("45", -2L), "0.45")
  expect_identical(shift("0.5", -2L), "0.005")
  # "five" inside "forty-five" is not a five of its own.
  expect_setequal(readgpt:::figure_words("twelve weeks, forty-five per cent, a dozen, three"),
                  c(12, 45, 3))
})

test_that("a figure is compared at the precision it was written to", {
  hit <- function(sentence, row) {
    f <- readgpt:::figure_find(sentence)
    readgpt:::figure_hit(readgpt:::figure_targets(f)[[1]], readgpt:::figure_values(row))
  }
  # Rounding what the row wrote is copying it, a half either way.
  expect_true(hit("d = 0.61", "d = 0.613"))
  expect_true(hit("d = .6", "d = 0.613"))
  expect_true(hit("d = 0.13", "d = 0.125"))
  expect_true(hit("d = 0.12", "d = 0.125"))
  expect_false(hit("d = 0.62", "d = 0.613"))
  # "0.30" as written is 0.295 to 0.305; 0.32 is not in it.
  expect_false(hit("30%", "proportion 0.32"))
  expect_true(hit("45%", "proportion 0.453"))
  expect_true(hit("45.2 million", "$45,213,000"))
  expect_true(hit("45,200,000", "45.2 million"))
  # What this works out from a row is matched exactly, never rounded: a
  # pooled 0.38 is not a row's 38.5% read as 0.385.
  expect_false(hit("d = 0.38", "remission 38.5%"))
  expect_true(hit("0.385", "remission 38.5%"))
})

# ---------------------------------------------------------------------------
# The check on a written review.
# ---------------------------------------------------------------------------

test_that("a faithful copy is found however it is written, and nothing is reported", {
  text <- paste(
    "The first trial found a moderate effect, d = .61 [95% CI 0.32 to 0.90] [study 1].",
    "Together the trials enrolled 1324 participants [study 1] [study 3].",
    "The cohort followed 900 people for 12 months (OR 1.80, 95% CI 1.2\u20132.7) [study 2].",
    "Revenue reached 45.2 million dollars over 12 weeks [study 1].",
    "About 1,200 adults took part in the largest trial, 54% of them female [study 3].",
    "Its effect was \u22120.12 (p < .001), in a 2018 trial [study 3].",
    "Of the 3 studies, 12 months was the longest follow-up [study 2].")
  r <- warn_classes(fig_synth(text))
  s <- r$value
  expect_false("gr_synth_unbacked_figures" %in% r$classes)
  fig <- s$figures
  expect_true(nrow(fig) >= 15L)
  expect_true(all(fig$backed))
  expect_identical(unique(fig$stage), "draft")
  expect_identical(s$sections$n_unbacked, 0L)
  # What each was found in is said, so an auditor can see which were derived.
  expect_identical(fig$basis[fig$number == "1324"], "total of the cited rows")
  expect_identical(fig$basis[fig$number == "1,200"], "approximate")
  expect_identical(fig$basis[fig$number == "2018"], "year of a cited row")
  expect_identical(fig$studies[fig$number == "1324"], "1, 3")
})

test_that("a figure the cited row does not state is reported, and the section is not partial", {
  text <- paste("The first trial found d = 0.71 [study 1].",
                "It enrolled 900 participants [study 1].",
                "The cohort followed 900 people [study 2].")
  r <- warn_classes(fig_synth(text))
  s <- r$value
  expect_true("gr_synth_unbacked_figures" %in% r$classes)
  bad <- s$figures[!s$figures$backed, ]
  # 0.71 is in no row; 900 is study 2's, not study 1's. Checked against the
  # whole table, the second passed.
  expect_identical(bad$number, c("0.71", "900"))
  expect_identical(bad$studies, c("1", "1"))
  expect_true(all(is.na(bad$basis)))
  expect_match(bad$sentence[1], "d = 0.71 [study 1].", fixed = TRUE)
  expect_identical(s$sections$n_unbacked, 2L)
  # `partial` means a citation cannot be trusted, and these are all honest.
  expect_false(s$sections$partial)
  expect_identical(s$sections$n_unknown, 0L)
  # The data frame is flat, for printing, the audit and a CSV.
  expect_false(any(vapply(s$figures, is.list, logical(1))))
  out <- capture.output(print(s))
  expect_true(any(grepl("2 FIGURE(S) NOT IN THE ROWS CITED", out, fixed = TRUE)))
  expect_true(any(grepl("2 figure(s) in the text are not in the rows their sentence cites",
                        out, fixed = TRUE)))
  expect_warning(fig_synth(text), "0.71 in 'Findings'", class = "gr_synth_unbacked_figures")
})

test_that("arithmetic on the page, and points between two cited figures, are not reported", {
  # 11.8% is 120 of 1,020, both in the sentence, where the reader can redo it.
  s <- quiet(fig_synth(paste("Of 1,020 people, 120 (11.8%) were in the trial and 900 (88.2%)",
                             "in the cohort [study 1] [study 2].")))
  fig <- s$figures
  expect_true(all(fig$backed))
  expect_identical(fig$basis[fig$number %in% c("11.8", "88.2")],
                   rep("worked out in the sentence", 2L))
  # Not when one side of the working is missing from the page.
  s <- quiet(fig_synth("Of the trial's participants, 13.3% were in the trial [study 1] [study 2]."))
  expect_false(s$figures$backed)
  # A difference in points that two cited figures give is allowed.
  s <- quiet(fig_synth("The odds ratio sat 0.9 points above its lower limit [study 2]."))
  expect_identical(s$figures$basis, "difference of two cited figures")
  # The same figure, not in points, is a figure of its own.
  s <- quiet(fig_synth("The odds ratio was 0.9 [study 2]."))
  expect_false(s$figures$backed)
})

test_that("a total is of a field that is a number in every row, not of text that holds one", {
  s <- quiet(fig_synth("Together they enrolled 1,020 people [study 1] [study 2]."))
  expect_identical(s$figures$basis, "total of the cited rows")
  # followup is text: 12 months and 24 weeks are not 36 of anything.
  s <- quiet(fig_synth("Together they ran 36 weeks [study 2] [study 3]."))
  expect_false(s$figures$backed)
})

test_that("a sentence citing nothing is checked against every row its section was shown", {
  r <- warn_classes(fig_synth(paste("Overall, 900 people were followed.",
                                    "Overall, 45% of participants improved.")))
  fig <- r$value$figures
  expect_identical(fig$backed, c(TRUE, FALSE))
  expect_identical(fig$basis[1], "rows shown")
  expect_true(all(is.na(fig$studies)))
  expect_true("gr_synth_unbacked_figures" %in% r$classes)
})

test_that("the question and the brief back a figure; a withheld column does not, but its year does", {
  s <- quiet(fig_synth("Trials in adults over 65 ran 36 weeks, from a 2019 pilot [study 1].",
                       question = "Does it work in adults over 65?"))
  fig <- s$figures
  expect_identical(fig$basis[fig$number == "65"], "question or brief")
  # The title is withheld from the writer as bibliographic, so its 36 was not
  # shown and cannot back a figure: as the writer was shown the rows.
  expect_false(fig$backed[fig$number == "36"])
  # The year is withheld too, but it is the study's and the rendered citation
  # says the same.
  expect_identical(fig$basis[fig$number == "2019"], "year of a cited row")
  s <- quiet(fig_synth("Trials ran 36 weeks [study 1].",
                       outline = c(Findings = "Trials of 36 weeks or more")))
  expect_identical(s$figures$basis, "question or brief")
})

test_that("the check sends nothing and changes no request", {
  s <- quiet(fig_synth("The first trial found d = 0.71 [study 1]."))
  cl <- attr(s, "client")
  expect_length(cl$calls(), 1L)
  steps <- s$trace$steps
  note <- Filter(function(st) identical(st$label, "synthesise.figures"), steps)
  expect_length(note, 1L)
  expect_identical(note[[1]]$kind, "local")
  expect_identical(note[[1]]$detail$unbacked, 1L)
  # The request is the one made without the check: the same prompt, word for
  # word, so a cached or recorded write-up still replays.
  msgs <- cl$calls()[[1]]$messages
  expect_length(msgs, 3L)
  expect_identical(msgs[[1]]$content, sprintf(readgpt:::.gr_prompts$synthesise_system, "Findings"))
  expect_identical(msgs[[2]]$content,
                   "Review question: Does it work?\n\nSection: Findings\nThis section must cover: What they found")
})

# ---------------------------------------------------------------------------
# With claims: the claims given to the section back what rests on their rows.
# ---------------------------------------------------------------------------

fig_claims_client <- function(write) {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      return(paste0('{"claims":[',
                    '{"claim":"Trials cut symptoms by a third, 33%, at 16 weeks.","kind":"finding",',
                    '"supported_by":[1,3],"contradicted_by":[],"moderator":null,"scope":null},',
                    '{"claim":"Cohorts saw a 7.5 point gain.","kind":"finding",',
                    '"supported_by":[2],"contradicted_by":[],"moderator":null,"scope":null}]}'))
    }
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return(paste0('{"sections":[{"heading":"Trials","brief":"a","claims":[1],"rationale":null},',
                    '{"heading":"Cohorts","brief":"b","claims":[2],"rationale":null}]}'))
    }
    all_txt <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                     collapse = " ")
    for (h in names(write)) if (grepl(paste0("Section: ", h), all_txt, fixed = TRUE)) return(write[[h]])
    "Nothing."
  })
}

test_that("with claims, a figure from a claim on the cited rows is found, one from another is not", {
  write <- c(Trials = "Trials cut symptoms by 33% at 16 weeks [study 1] [study 3].",
             Cohorts = "The cohort saw a 7.5 point gain, or 33% [study 2].")
  cl <- fig_claims_client(write)
  tab <- fig_table()
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  o <- quiet(gr_outline(cm, client = cl))
  r <- warn_classes(gr_synthesise(tab, outline = o, question = "Q?", client = cl, claims = cm,
                                  cite_style = "marker"))
  fig <- r$value$figures
  tr <- fig[fig$section == "Trials", ]
  expect_true(all(tr$backed))
  expect_identical(unique(tr$basis), "claim")
  co <- fig[fig$section == "Cohorts", ]
  expect_identical(co$basis[co$number == "7.5"], "claim")
  # 33% is claim 1's, given to another section and resting on other rows.
  expect_false(co$backed[co$number == "33"])
  expect_identical(r$value$sections$n_unbacked, c(0L, 1L))
  expect_true("gr_synth_unbacked_figures" %in% r$classes)
})

# ---------------------------------------------------------------------------
# Revision: a pass may not add a figure, and the revised text is checked again.
# ---------------------------------------------------------------------------

test_that("figure_guard() allows any form of the draft's figures and refuses a new one", {
  g <- readgpt:::figure_guard
  before <- paste("A trial of 1,204 adults found d = 0.61 over twelve weeks [study 1].",
                  "Revenue was 45.2 million [study 2].")
  expect_true(g(before, paste("A trial of 1204 adults found d = .61 over 12 weeks [study 1].",
                              "Revenue was $45,200,000 [study 2]."))$ok)
  # Dropping a figure is the cut pass's business, not this guard's.
  expect_true(g(before, "A trial of 1,204 adults found an effect [study 1].")$ok)
  r <- g(before, "A trial of 1,204 adults found d = 0.71 over 12 weeks [study 1].")
  expect_false(r$ok)
  expect_identical(r$reason, "the revision introduced a figure the draft did not have (0.71)")
  r <- g(before, "A trial of 1,204 adults found d = 0.71 in 1,300 [study 1].")
  expect_identical(r$reason, "the revision introduced figures the draft did not have (0.71, 1,300)")
  # Rounding is a changed figure here: the draft is what the rows were checked
  # against.
  expect_false(g("d = 0.613 [study 1].", "d = 0.61 [study 1].")$ok)
  # A citation's ids are not figures, and neither is a heading.
  expect_true(g("A benefit [study 1].", "## 2024 update\n\nA benefit [study 1] [studies 12-14].")$ok)
})

test_that("a pass that changes a figure is discarded, and the draft stands", {
  text <- "The first trial found d = 0.61 [study 1]. The cohort followed 900 people [study 2]."
  r <- warn_classes(fig_synth(text, coherence = "register",
                              revise = function(d) sub("0.61", "0.71", d, fixed = TRUE)))
  s <- r$value
  expect_true("gr_revision_new_figure" %in% r$classes)
  expect_false(s$coherence$kept)
  expect_true(s$coherence$ran)
  expect_identical(s$coherence$reason,
                   "the revision introduced a figure the draft did not have (0.71)")
  expect_identical(s$text, s$draft)
  expect_match(s$text, "d = 0.61", fixed = TRUE)
  # No revision kept: only the draft was checked, and it is clean.
  expect_identical(unique(s$figures$stage), "draft")
  expect_false("gr_synth_unbacked_figures" %in% r$classes)
})

test_that("a pass may not write back a figure an earlier pass cut", {
  text <- "A trial found d = 0.61 [study 1], with 900 followed [study 2]. It replicated [study 1]."
  cut <- function(d) sub(", with 900 followed [study 2]", " [study 2]", d, fixed = TRUE)
  r <- warn_classes(fig_synth(text, coherence = c("cut", "register"), revise = function(d) {
    if (grepl("with 900", d, fixed = TRUE)) cut(d)
    else sub("It replicated", "It replicated in 900 people", d, fixed = TRUE)
  }))
  s <- r$value
  expect_identical(s$coherence$kept, c(TRUE, FALSE))
  expect_match(s$coherence$reason[2], "(900)", fixed = TRUE)
  expect_true("gr_revision_new_figure" %in% r$classes)
})

test_that("the figure guard only adds refusals: an escalating pass is still reported as one", {
  text <- "Two trials suggest a possible benefit of 0.61 [study 1] [study 3]."
  r <- warn_classes(fig_synth(text, coherence = "register", revise = function(d) {
    "## Findings\n\nAll trials demonstrate a benefit of 0.71 [study 1] [study 3]."
  }))
  expect_true("gr_revision_escalated" %in% r$classes)
  expect_false("gr_revision_new_figure" %in% r$classes)
  expect_match(r$value$coherence$reason, "all", fixed = TRUE)
})

test_that("the revised text is checked again: a kept pass that swaps two figures is reported", {
  # Same figures, same markers, same hedges: every guard on the revision passes
  # it, and it puts each number on the wrong study.
  text <- "A trial enrolled 120 people [study 1]. A cohort followed 900 people [study 2]."
  swap <- function(d) {
    d <- sub("120 people", "XXX people", d, fixed = TRUE)
    d <- sub("900 people", "120 people", d, fixed = TRUE)
    sub("XXX people", "900 people", d, fixed = TRUE)
  }
  r <- warn_classes(fig_synth(text, coherence = "structure", revise = swap))
  s <- r$value
  expect_true(s$coherence$kept)
  draft <- s$figures[s$figures$stage == "draft", ]
  final <- s$figures[s$figures$stage == "final", ]
  expect_true(all(draft$backed))
  expect_identical(final$number, c("900", "120"))
  expect_false(any(final$backed))
  expect_identical(final$section, c("Findings", "Findings"))
  # The sections are the draft, which is clean; the warning is about the text
  # as published.
  expect_identical(s$sections$n_unbacked, 0L)
  expect_true("gr_synth_unbacked_figures" %in% r$classes)
  expect_warning(fig_synth(text, coherence = "structure", revise = swap), "revised review",
                 class = "gr_synth_unbacked_figures")
  out <- capture.output(print(s))
  expect_true(any(grepl("2 figure(s) in the revised text", out, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# The audit report, the help, and syntheses made before the check.
# ---------------------------------------------------------------------------

test_that("the audit report lists the figures found nowhere, and what the rest were found in", {
  s <- quiet(fig_synth(paste("The first trial found d = 0.71 [study 1].",
                             "The cohort followed 900 people [study 2].")))
  p <- withr::local_tempfile(fileext = ".html")
  quiet(gr_audit_report(p, synthesis = s, open = FALSE))
  html <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_match(html, "Figures checked against the rows", fixed = TRUE)
  expect_match(html, "1 figure(s) in this section are not in the rows their sentence cites",
               fixed = TRUE)
  expect_match(html, "<td>0.71</td>", fixed = TRUE)
  expect_match(html, "<td>cited row</td>", fixed = TRUE)
  expect_match(html, "Found means copied, not right", fixed = TRUE)
  # A partial-section note is not what this is.
  expect_false(grepl("This section is marked partial", html, fixed = TRUE))

  clean <- quiet(fig_synth("The cohort followed 900 people [study 2]."))
  quiet(gr_audit_report(p, synthesis = clean, open = FALSE))
  html <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_match(html, "Every figure was found in what its sentence was written from.", fixed = TRUE)
})

test_that("a synthesis made before the check prints and reports as before", {
  s <- quiet(fig_synth("The first trial found d = 0.71 [study 1]."))
  s$figures <- NULL
  s$sections$n_unbacked <- NULL
  out <- capture.output(print(s))
  expect_false(any(grepl("FIGURE", out, fixed = TRUE)))
  p <- withr::local_tempfile(fileext = ".html")
  expect_no_error(quiet(gr_audit_report(p, synthesis = s, open = FALSE)))
  html <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_false(grepl("Figures checked against the rows", html, fixed = TRUE))
})

test_that("the gr_synthesise() help names the figure check and the revision it refuses", {
  f <- testthat::test_path("..", "..", "R", "synthesise.R")
  skip_if(!file.exists(f), "the package source is not available")
  src <- readLines(f, warn = FALSE)
  doc <- gsub("\\s+", " ", paste(sub("^#' ?", "", src[startsWith(src, "#'")]), collapse = " "))
  expect_match(doc, "introduced a figure the draft did not have", fixed = TRUE)
  expect_match(doc, "`gr_revision_new_figure`", fixed = TRUE)
  expect_match(doc, "`gr_synth_unbacked_figures`", fixed = TRUE)
  expect_match(doc, "\\item{`figures`}", fixed = TRUE)
  expect_match(doc, "`n_unbacked`", fixed = TRUE)
  expect_match(doc, "It does not make a section partial", fixed = TRUE)
})
