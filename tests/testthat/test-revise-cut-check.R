# test-revise-cut-check.R -- a `cut` pass that did not cut is not kept as one.
#
# Every check on a revision asked whether it was safe; none asked whether the
# pass did its job. A `cut` that came back as long as it went out -- the draft
# handed back, a polish, a few words added -- passed them all, was kept, and
# `$coherence` reported a kept cut with no number to show nothing was cut.

cc_warns <- function(expr) {
  classes <- character(0); msgs <- character(0)
  value <- withCallingHandlers(suppressMessages(expr), warning = function(w) {
    classes <<- c(classes, class(w)[1]); msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, classes = classes, messages = msgs)
}

# One pass, straight through revise_once(), with `revision` as the reply.
cc_revise <- function(draft, revision, pass = "cut", trace = gr_trace()) {
  cl <- gr_mock_client(function(m, p) revision)
  cc_warns(readgpt:::revise_once(draft, "Q?", cl, gr_read_spec("stuff", model = "mock-model"),
                                 trace, NULL, pass))
}

cc_draft <- paste0("## Findings\n\nIt is important to note that two small trials may suggest a ",
                   "modest benefit [study 1] [study 2]. One cohort could show harm [study 3].")

cc_table <- function() {
  data.frame(document = c("a.pdf", "b.pdf", "c.pdf"), document_id = c("h1", "h2", "h3"),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_, design = c("RCT", "RCT", "cohort"),
             finding = c("benefit", "benefit", "harm"), stringsAsFactors = FALSE)
}

# A writer whose section is cc_draft's prose, and whose passes are named by the
# opening of their system prompts; a pass not named hands its draft back.
cc_client <- function(passes = list()) {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    last <- messages[[length(messages)]]$content
    if (grepl("<draft>", last, fixed = TRUE)) {
      d <- sub("(?s)^.*<draft>\n", "", last, perl = TRUE)
      d <- sub("(?s)\n</draft>.*$", "", d, perl = TRUE)
      for (p in names(passes)) if (startsWith(sys, p)) return(passes[[p]](d))
      return(d)
    }
    sub("^## Findings\n\n", "", cc_draft)
  })
}

cc_synth <- function(cl, coherence = TRUE) {
  cc_warns(gr_synthesise(cc_table(), outline = c(Findings = "what was found"), question = "Q?",
                         client = cl, coherence = coherence, cite_style = "marker"))
}

drop_filler <- function(d) sub("It is important to note that two", "Two", d, fixed = TRUE)

test_that("prose is counted in words a reader reads, without headings or markers", {
  f <- readgpt:::prose_words
  expect_identical(f("## Findings\n\nTwo trials suggest a benefit [study 1] [study 2]."), 5L)
  # Writing the markers as a range, or deleting the heading, is not cutting.
  expect_identical(f("Two trials suggest a benefit [studies 1-2]."), 5L)
  expect_identical(f("Two trials suggest a benefit [study 1; study 2 p. 4]."), 5L)
  # A dash between spaces is no word; "12%" and "all-cause" are one each.
  expect_identical(f("All-cause deaths fell 12% -- a small change."), 7L)
  expect_identical(f(""), 0L)
  expect_identical(f(NULL), 0L)
  expect_identical(f(cc_draft), 19L)
  expect_identical(f(drop_filler(cc_draft)), 13L)
})

test_that("a cut handed back as it went out is discarded, with the counts", {
  # Before: kept, and reported as a kept cut.
  r <- cc_revise(cc_draft, cc_draft)
  expect_false(r$value$kept)
  expect_true(r$value$ran)
  expect_identical(r$value$reason,
                   "the cut pass did not shorten the review: 19 words before, 19 after")
  expect_identical(r$classes, "gr_revision_uncut")
  expect_match(r$messages, "did not shorten the review: 19 words of prose before, 19 after",
               fixed = TRUE)
  expect_identical(r$value$words_before, 19L)
  expect_identical(r$value$words_after, 19L)
  expect_null(r$value$text)
})

test_that("a cut that came back longer, or only rearranged its markup, is discarded", {
  longer <- paste(cc_draft, "This is worth bearing in mind.")
  r <- cc_revise(cc_draft, longer)
  expect_false(r$value$kept)
  expect_identical(r$value$reason,
                   "the cut pass did not shorten the review: 19 words before, 25 after")
  # Fewer tokens, same prose: the heading gone and two markers written as one.
  markup <- sub("## Findings\n\n", "", sub("[study 1] [study 2]", "[studies 1-2]", cc_draft,
                                           fixed = TRUE), fixed = TRUE)
  expect_lt(gr_count_tokens(markup), gr_count_tokens(cc_draft))
  r <- cc_revise(cc_draft, markup)
  expect_false(r$value$kept)
  expect_identical(r$classes, "gr_revision_uncut")
})

test_that("a cut that removes anything at all is kept, with no minimum share", {
  r <- cc_revise(cc_draft, drop_filler(cc_draft))
  expect_true(r$value$kept)
  expect_identical(r$value$text, drop_filler(cc_draft))
  expect_identical(r$classes, character(0))
  expect_identical(r$value$words_before, 19L)
  expect_identical(r$value$words_after, 13L)
  expect_true(is.na(r$value$reason))
  # One word of a long review is still a cut.
  long <- paste(c("## Findings", rep("Two small trials may suggest a modest benefit [study 1].", 40)),
                collapse = "\n\n")
  r <- cc_revise(long, sub("modest benefit", "benefit", long, fixed = TRUE))
  expect_true(r$value$kept)
  expect_identical(r$value$words_before - r$value$words_after, 1L)
})

test_that("the length is judged last, so an unsafe cut is reported for what makes it unsafe", {
  # Not shorter AND escalated: the escalation is the reason, and nothing was
  # measured.
  r <- cc_revise(cc_draft, sub("may suggest", "clearly demonstrate", cc_draft, fixed = TRUE))
  expect_false(r$value$kept)
  expect_identical(r$classes, "gr_revision_escalated")
  expect_null(r$value$words_before)
  # Not shorter AND re-cited: the citation check's reason.
  r <- cc_revise(cc_draft, paste(cc_draft, "It works [study 1]."))
  expect_identical(r$classes, "gr_coherence_rejected")
})

test_that("only the cut is held to a length", {
  # A register pass that adds words is kept.
  r <- cc_revise(cc_draft, sub("One cohort", "A single cohort study", cc_draft, fixed = TRUE),
                 pass = "register")
  expect_true(r$value$kept)
  expect_identical(r$value$words_after, 21L)
  # And so is one that removes half of padded prose by polishing it,
  # every hedge kept: words cannot tell that from cutting, so it is reported,
  # not refused.
  padded <- paste0("It is important to note that, taken together, the two trials appear to ",
                   "suggest that the drug may be associated with a modest benefit [study 1] ",
                   "[study 2].")
  polished <- "Together, the two trials appear to suggest the drug may modestly help [study 1] [study 2]."
  r <- cc_revise(padded, polished, pass = "register")
  expect_true(r$value$kept)
  expect_identical(c(r$value$words_before, r$value$words_after), c(24L, 12L))
  r <- cc_revise(cc_draft, cc_draft, pass = "structure")
  expect_true(r$value$kept)
  expect_identical(r$classes, character(0))
})

test_that("the measurement is in the trace as a local step, and costs no request", {
  tr <- gr_trace()
  cc_revise(cc_draft, cc_draft, trace = tr)
  labels <- vapply(tr$steps, function(s) s$label, character(1))
  expect_identical(labels, c("synthesise.revise.cut", "synthesise.revise.cut.length"))
  st <- tr$steps[[2]]
  expect_identical(st$kind, "local")
  expect_identical(st$detail, list(words_before = 19L, words_after = 19L, ok = FALSE))
  expect_identical(as.integer(tr$calls), 1L)
})

test_that("$coherence carries the counts, and the next pass runs on what the cut was given", {
  cl <- cc_client()
  r <- cc_synth(cl)
  co <- r$value$coherence
  expect_identical(co$pass, c("structure", "cut", "register"))
  expect_identical(co$kept, c(TRUE, FALSE, TRUE))
  expect_true(is.integer(co$words_before))
  expect_true(is.integer(co$words_after))
  expect_identical(co$words_before, c(19L, 19L, 19L))
  expect_identical(co$words_after, c(19L, 19L, 19L))
  expect_identical(co$reason[2],
                   "the cut pass did not shorten the review: 19 words before, 19 after")
  expect_true("gr_revision_uncut" %in% r$classes)
  # The request each pass sends is the one it always sent: the check is made
  # on the reply, and no prompt says anything about it.
  calls <- Filter(function(x) grepl("<draft>", x$messages[[3]]$content %||% "", fixed = TRUE),
                  cl$calls())
  expect_length(calls, 3L)
  expect_identical(calls[[2]]$messages[[1]]$content, readgpt:::.gr_revise_prompts[["cut"]])
  # The cut was discarded, so the register pass was given the structure pass's text.
  expect_identical(calls[[3]]$messages[[3]]$content, calls[[2]]$messages[[3]]$content)

  # A cut that cuts is kept, and the register pass is given its text.
  r <- cc_synth(cc_client(list("You cut" = drop_filler)))
  co <- r$value$coherence
  expect_identical(co$kept, c(TRUE, TRUE, TRUE))
  expect_identical(co$words_after[2], 13L)
  expect_identical(co$words_before[3], 13L)
  expect_false("gr_revision_uncut" %in% r$classes)
  expect_match(r$value$text, "## Findings\n\nTwo small trials may suggest", fixed = TRUE)
})

test_that("a pass not sent, or refused before it was measured, has no counts", {
  empty <- readgpt:::synth_revise(cc_draft, "Q?", gr_mock_client(function(m, p) "x"),
                                  gr_read_spec("stuff", model = "mock-model"), gr_trace(),
                                  passes = character(0))$report
  expect_identical(nrow(empty), 0L)
  expect_identical(empty$words_before, integer(0))
  expect_identical(empty$words_after, integer(0))
  fr <- paste0("Trois petits essais suggèrent un bénéfice modeste ",
               "[study 1] [study 2]. Une cohorte pourrait montrer un risque [study 3].")
  r <- cc_warns(readgpt:::synth_revise(fr, "Q?", gr_mock_client(function(m, p) fr),
                                       gr_read_spec("stuff", model = "mock-model"), gr_trace(),
                                       passes = "cut"))
  expect_identical(r$value$report$words_before, NA_integer_)
  expect_identical(r$value$report$words_after, NA_integer_)
  r <- cc_synth(cc_client(list("You cut" = function(d) paste(d, "It works [study 1]."))),
                coherence = "cut")
  expect_identical(r$value$coherence$words_before, NA_integer_)
})

test_that("the audit report shows the counts beside each pass", {
  syn <- cc_synth(cc_client())$value
  p <- withr::local_tempfile(fileext = ".html")
  quiet(gr_audit_report(p, synthesis = syn, open = FALSE))
  h <- gsub("[[:space:]]+", " ", paste(readLines(p, encoding = "UTF-8", warn = FALSE),
                                       collapse = "\n"))
  expect_match(h, paste0("<tr><th>pass</th><th>ran</th><th>kept</th><th>words_before</th>",
                         "<th>words_after</th><th>reason</th></tr>"), fixed = TRUE)
  expect_match(h, paste0("<td>cut</td><td>TRUE</td><td>FALSE</td><td class=\"num\">19</td>",
                         "<td class=\"num\">19</td><td>the cut pass did not shorten the review: ",
                         "19 words before, 19 after</td>"), fixed = TRUE)
  # A synthesis saved before the counts existed still reports its passes.
  old <- syn
  old$coherence <- old$coherence[, c("pass", "ran", "kept", "lost", "added", "reason")]
  quiet(gr_audit_report(p, synthesis = old, open = FALSE))
  h <- paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  expect_match(h, "<th>pass</th><th>ran</th><th>kept</th><th>reason</th>", fixed = TRUE)
})

test_that("the gr_synthesise() help names the new reason, its class and the counts", {
  f <- testthat::test_path("..", "..", "R", "synthesise.R")
  skip_if(!file.exists(f), "the package source is not available")
  src <- readLines(f, warn = FALSE)
  doc <- gsub("\\s+", " ", paste(sub("^#' ?", "", src[startsWith(src, "#'")]), collapse = " "))
  expect_match(doc, "the cut pass did not shorten the review", fixed = TRUE)
  expect_match(doc, "`gr_revision_uncut`", fixed = TRUE)
  expect_match(doc, "`words_before` and `words_after`", fixed = TRUE)
})
