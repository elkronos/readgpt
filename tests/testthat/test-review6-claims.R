# test-review6-claims.R -- the sixth pass over the claims layer and the revision guard.
#
# Each test failed on the code before its fix, and says what that code did: a
# flat reconcile reply merged every claim into one, a merge absorbed an opposite
# claim with nothing to show for it, a failed reconcile left no mark on the
# result, a moderator naming a constant column was accepted as an explanation,
# custom bibliographic columns reached the claims model and the gap list, the
# outline's reply limit could not be raised, and a revision could de-cite,
# re-cite or detach a claim -- or be written in French -- and still be kept.

r6c_table <- function(design = c("RCT", "RCT", "RCT")) {
  data.frame(document = c("a.pdf", "b.pdf", "c.pdf"), document_id = c("h1", "h2", "h3"),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_, design = design,
             finding = c("benefit", "benefit", "no benefit"), stringsAsFactors = FALSE)
}

r6c_claim <- function(text, sup, con = integer(0), moderator = NULL, kind = "finding") {
  sprintf(paste0('{"claim":"%s","kind":"%s","supported_by":[%s],"contradicted_by":[%s],',
                 '"moderator":%s,"scope":null}'),
          text, kind, paste(sup, collapse = ","), paste(con, collapse = ","),
          if (is.null(moderator)) "null" else sprintf('"%s"', moderator))
}

# Verified claim rows, as claims_reconcile() receives them.
r6c_rows <- function(...) {
  js <- sprintf('{"claims":[%s]}', paste(c(...), collapse = ","))
  readgpt:::claims_verify(readgpt:::claim_rows(jsonlite::fromJSON(js)$claims),
                          readgpt:::synth_studies(r6c_table(c("RCT", "cohort", "RCT")), FALSE))$claims
}

r6c_warns <- function(expr) {
  classes <- character(0); msgs <- character(0)
  value <- withCallingHandlers(suppressMessages(expr), warning = function(w) {
    classes <<- c(classes, class(w)[1]); msgs <<- c(msgs, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, classes = classes, messages = msgs)
}

# ---------------------------------------------------------------------------
# synthesis-13: a flat `groups` array is not one group
# ---------------------------------------------------------------------------

test_that("a flat groups array leaves the claims unmerged, with a warning", {
  # Before: {"groups":[1,2,3]} was read as ONE group of three. Opposite claims
  # collapsed into the longest one's wording, supported by every study.
  claims <- r6c_rows(r6c_claim("Trials in adults found a benefit.", 1),
                     r6c_claim("Surveys of children found no effect.", 2),
                     r6c_claim("Harms were rare.", 3))
  cl <- gr_mock_client(function(m, p) '{"groups":[1,2,3]}')
  r <- r6c_warns(readgpt:::claims_reconcile(claims, "Q?", cl, gr_read_spec("stuff"), gr_trace()))
  expect_identical(nrow(r$value), 3L)
  expect_identical(lengths(r$value$.support), c(1L, 1L, 1L))
  expect_true("gr_claims_unmerged" %in% r$classes)
  expect_match(r$messages[r$classes == "gr_claims_unmerged"], "not a list of groups", fixed = TRUE)
  expect_true(isTRUE(attr(r$value, "unmerged")))
  # A real single group of three still merges: it arrives as a 1 x 3 matrix.
  cl <- gr_mock_client(function(m, p) '{"groups":[[1,2,3]]}')
  expect_identical(nrow(quiet(readgpt:::claims_reconcile(claims, "Q?", cl, gr_read_spec("stuff"),
                                                         gr_trace()))), 1L)
})

# ---------------------------------------------------------------------------
# model-output-11: what a merge absorbed is on the record
# ---------------------------------------------------------------------------

test_that("a merged claim names the wordings it absorbed", {
  # Before: the "did not improve" wording took studies 1 and 2 from the
  # "improved" claim with note NA -- the merge left no trace at all.
  claims <- r6c_rows(
    r6c_claim("The treatment improved symptoms in clinical samples.", 1),
    r6c_claim("The treatment did not improve symptoms in the community samples studied.", 2:3))
  cl <- gr_mock_client(function(m, p) '{"groups":[[1,2]]}')
  got <- quiet(readgpt:::claims_reconcile(claims, "Q?", cl, gr_read_spec("stuff"), gr_trace()))
  expect_identical(nrow(got), 1L)
  expect_match(got$note, "merged with: 'The treatment improved symptoms in clinical samples.'",
               fixed = TRUE)
  # The same claim twice is a merge with nothing to report.
  same <- r6c_rows(r6c_claim("It works.", 1), r6c_claim("It  works.", 2))
  got <- quiet(readgpt:::claims_reconcile(same, "Q?", cl, gr_read_spec("stuff"), gr_trace()))
  expect_true(is.na(got$note))
})

test_that("a group mixing a finding with a gap is warned about", {
  claims <- r6c_rows(r6c_claim("Trials find the effect.", 1),
                     r6c_claim("No study followed up past a year.", 2, kind = "gap"))
  cl <- gr_mock_client(function(m, p) '{"groups":[[1,2]]}')
  r <- r6c_warns(readgpt:::claims_reconcile(claims, "Q?", cl, gr_read_spec("stuff"), gr_trace()))
  expect_true("gr_claims_merged" %in% r$classes)
  expect_match(r$value$note, "merged with:", fixed = TRUE)
  # Claims of one kind merge without that warning.
  claims$kind <- "finding"
  r <- r6c_warns(readgpt:::claims_reconcile(claims, "Q?", cl, gr_read_spec("stuff"), gr_trace()))
  expect_false("gr_claims_merged" %in% r$classes)
})

# ---------------------------------------------------------------------------
# r3-synthesis-layer-call-sizing-05: an unmerged result says so
# ---------------------------------------------------------------------------

r6c_batched <- function(reconcile) {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("Group the ones", sys, fixed = TRUE)) return(reconcile(params))
    txt <- paste(vapply(messages, function(m) m$content, ""), collapse = "\n")
    ids <- regmatches(txt, gregexpr("(?<=\\[study )[0-9]+", txt, perl = TRUE))[[1]]
    sprintf('{"claims":[%s]}', r6c_claim("It works.", ids))
  })
}

test_that("claims a failed reconcile left unmerged are marked on the result", {
  # Before: a warning at most, and nothing on the object -- print(), the audit
  # and gr_synthesise() saw a complete claims table.
  tab <- r6c_table()
  cl <- r6c_batched(function(p) gr_result(TRUE, '{"groups":[[1,2', finish_reason = "length"))
  r <- r6c_warns(gr_claims(tab, question = "Q?", client = cl, max_claim_tokens = 440))
  cm <- r$value
  expect_identical(nrow(cm$claims), 3L)
  expect_true(cm$unmerged)
  expect_true("gr_claims_unmerged" %in% r$classes)
  expect_output(print(cm), "UNMERGED")
  # A reconcile that ran leaves the flag off.
  cl <- r6c_batched(function(p) '{"groups":[[1,2,3]]}')
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl, max_claim_tokens = 440))
  expect_identical(nrow(cm$claims), 1L)
  expect_false(cm$unmerged)
})

test_that("the reconcile reply limit grows with the number of claims", {
  # Before: the reply limit was max_claim_tokens whatever the count, so a large
  # corpus's reconcile reply was cut off and every claim came back unmerged.
  seen <- NULL
  n <- 400L
  claims <- r6c_rows(r6c_claim("Seed.", 1))[rep(1L, n), ]
  claims$claim <- sprintf("Claim number %d.", seq_len(n))
  cl <- gr_mock_client(function(m, p) { seen <<- p$max_output; '{"groups":[]}' })
  quiet(readgpt:::claims_reconcile(claims, "Q?", cl,
                                   gr_read_spec("stuff", model = "mock-model",
                                                max_answer_tokens = 1600L), gr_trace()))
  expect_gte(seen, readgpt:::reconcile_reply_tokens(n))
  expect_gt(seen, 1600L)
})

test_that("a reconcile reply cut off by the window is not blamed on the reply limit", {
  local_registries()
  gr_register_model("recon-small", context_window = 700L, max_output = 600L,
                    input_usd = 0, output_usd = 0)
  claims <- r6c_rows(r6c_claim("Seed.", 1))[rep(1L, 40L), ]
  claims$claim <- sprintf("Claim number %d says something moderately long about the evidence.",
                          seq_len(40L))
  cl <- gr_mock_client(function(m, p) gr_result(TRUE, '{"groups":[[1,', finish_reason = "length"))
  r <- r6c_warns(readgpt:::claims_reconcile(
    claims, "Q?", cl, gr_read_spec("stuff", model = "recon-small", max_answer_tokens = 400L),
    gr_trace()))
  msg <- r$messages[r$classes == "gr_claims_unmerged"]
  expect_match(msg, "context window", fixed = TRUE)
  expect_no_match(msg, "raise `max_claim_tokens`", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# synthesis-12: a moderator has to tell the two sides apart
# ---------------------------------------------------------------------------

test_that("a moderator naming a column that does not vary is cleared", {
  # Before: every study is an RCT, the claim came back "distinguished by:
  # design", and gr_gaps() dropped it from the unexplained disagreements while
  # reporting that design never varies.
  cl <- gr_mock_client(function(m, p) sprintf('{"claims":[%s]}',
    r6c_claim("Most trials found a benefit.", 1:2, 3L, moderator = "design")))
  cm <- quiet(gr_claims(r6c_table(), question = "Q?", client = cl))
  expect_true(is.na(cm$claims$moderator))
  expect_match(cm$claims$note, "cleared moderator 'design'", fixed = TRUE)
  expect_true(any(grepl("does not separate", cm$dropped$reason, fixed = TRUE)))
  g <- gr_gaps(cm)
  expect_true("disagreement unexplained" %in% g$kind)

  # A column that does separate the sides is kept, case and spacing aside.
  cm <- quiet(gr_claims(r6c_table(c("RCT", " rct", "cohort")), question = "Q?", client = cl))
  expect_identical(cm$claims$moderator, "design")
  expect_true(is.na(cm$claims$note))
  # ...and one where a value turns up on both sides is not.
  cm <- quiet(gr_claims(r6c_table(c("RCT", "cohort", "Cohort")), question = "Q?", client = cl))
  expect_true(is.na(cm$claims$moderator))
})

test_that("a numeric moderator must separate the sides by a threshold", {
  sp <- readgpt:::moderator_split
  used <- data.frame(study = 1:4, n = c(40, 60, 900, 50), dose = c("10", "20", "5", "15"),
                     stringsAsFactors = FALSE)
  # Every value differs, so as sets the sides are always disjoint; that explains nothing.
  expect_match(sp(used, "n", c(1L, 3L), c(2L, 4L)), "overlap", fixed = TRUE)
  expect_true(is.na(sp(used, "n", c(3L), c(1L, 2L, 4L))))
  expect_true(is.na(sp(used, "dose", c(2L, 4L), c(1L, 3L))))
  # Unreported on one side is not evidence of a split.
  used$site <- c("urban", "urban", NA, "not reported")
  expect_match(sp(used, "site", 1:2, 3:4), "does not report", fixed = TRUE)
  # An uncontested claim has no sides to separate.
  expect_true(is.na(sp(used, "n", 1:2, integer(0))))
})

test_that("a moderator is checked again after a merge changes the sides", {
  # Each batch's claim is split by design; merged, both designs are on both sides.
  tab <- r6c_table(c("RCT", "cohort", "cohort"))
  tab <- rbind(tab, transform(tab, document = paste0("x", document),
                              document_id = paste0("x", document_id),
                              design = c("cohort", "RCT", "RCT")))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("Group the ones", messages[[1]]$content, fixed = TRUE)) return('{"groups":[[1,2]]}')
    txt <- paste(vapply(messages, function(m) m$content, ""), collapse = "\n")
    ids <- as.integer(regmatches(txt, gregexpr("(?<=\\[study )[0-9]+", txt, perl = TRUE))[[1]])
    sprintf('{"claims":[%s]}', r6c_claim("It works.", ids[1], ids[2], moderator = "design"))
  })
  # 520 tokens: three studies a batch, so two batches.
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl, max_claim_tokens = 520))
  expect_identical(nrow(cm$claims), 1L)
  expect_identical(cm$claims$n_contradict, 2L)
  expect_true(is.na(cm$claims$moderator))
  expect_match(cm$claims$note, "cleared moderator 'design'", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# synthesis-09 and synthesis-08: bibliographic columns stay out
# ---------------------------------------------------------------------------

r6c_bib_table <- function() {
  tab <- r6c_table(c("RCT", "RCT", "cohort"))
  tab$first_author <- c("Smith, J.", "Garcia, M.", "Smith, J.")
  tab$pub_year <- c("2019", "2022", "2019")
  tab$journal <- "Lancet"
  tab
}

test_that("gr_claims(bib =) withholds custom bibliographic columns from the model", {
  # Before: gr_claims() had no `bib`, so `first_author` reached the prompt, and
  # a moderator naming it was kept.
  seen <- NULL
  cl <- gr_mock_client(function(m, p) {
    seen <<- paste(vapply(m, function(x) x$content, ""), collapse = "\n")
    sprintf('{"claims":[%s]}', r6c_claim("Some found a benefit.", 1:2, 3L,
                                         moderator = "first_author"))
  })
  cm <- quiet(gr_claims(r6c_bib_table(), question = "Q?", client = cl,
                        bib = list(authors = "first_author", year = "pub_year")))
  expect_no_match(seen, "first_author|pub_year|Smith|Garcia|journal|Lancet")
  expect_match(seen, "design: cohort", fixed = TRUE)
  expect_true(is.na(cm$claims$moderator))
  expect_setequal(cm$hidden, c("first_author", "pub_year", "journal"))
  # A `bib` naming a column that is not there is an error, as in gr_synthesise().
  expect_error(quiet(gr_claims(r6c_bib_table(), question = "Q?", client = cl,
                               bib = list(authors = "nope"))), class = "gr_bad_bib")
})

test_that("gr_gaps leaves bibliographic columns out of the gaps", {
  # Before: "no variation (venue): every study reports 'Lancet'" and its like
  # went to the closing section as gaps the writer had to state.
  cl <- gr_mock_client(function(m, p) sprintf('{"claims":[%s]}', r6c_claim("A.", 1:3)))
  tab <- r6c_bib_table()
  names(tab)[names(tab) == "first_author"] <- "authors"
  names(tab)[names(tab) == "pub_year"] <- "year"
  tab$authors <- "Smith, J."
  tab$year <- "2019"
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  g <- gr_gaps(cm)
  expect_false(any(c("authors", "year", "journal") %in% g$dimension))
  # Custom names: withheld because gr_claims() was told, and because gr_gaps() is.
  tab2 <- r6c_bib_table()
  tab2$first_author <- "Smith, J."
  cm2 <- quiet(gr_claims(tab2, question = "Q?", client = cl,
                         bib = list(authors = "first_author", year = "pub_year")))
  expect_false(any(c("first_author", "pub_year") %in% gr_gaps(cm2)$dimension))
  cm3 <- quiet(gr_claims(tab2, question = "Q?", client = cl))
  expect_true("first_author" %in% gr_gaps(cm3)$dimension)
  expect_false("first_author" %in% gr_gaps(cm3, bib = list(authors = "first_author"))$dimension)
  # A claims table saved before `hidden` existed still hides the conventional names.
  cm$hidden <- NULL
  expect_false(any(c("authors", "year", "journal") %in% gr_gaps(cm)$dimension))
})

# ---------------------------------------------------------------------------
# r3-synthesis-layer-call-sizing-04: the outline's reply limit
# ---------------------------------------------------------------------------

r6c_many_claims <- function(n) {
  tab <- r6c_table()
  body <- paste(vapply(seq_len(n), function(i) r6c_claim(sprintf("Claim %d.", i), 1L), ""),
                collapse = ",")
  quiet(gr_claims(tab, question = "Q?",
                  client = gr_mock_client(function(m, p) sprintf('{"claims":[%s]}', body))))
}

test_that("gr_outline's reply limit can be raised and grows with the claims", {
  # Before: a literal 1200, with `max_outline_tokens` an unused argument.
  seen <- NULL
  cl <- gr_mock_client(function(m, p) {
    seen <<- p$max_output
    '{"sections":[{"heading":"A","brief":"b","claims":[1],"rationale":null}]}'
  })
  cm <- r6c_many_claims(2L)
  quiet(gr_outline(cm, client = cl))
  expect_identical(seen, 1200L)
  quiet(gr_outline(cm, client = cl, max_outline_tokens = 4000))
  expect_identical(seen, 4000L)
  big <- r6c_many_claims(300L)
  quiet(gr_outline(big, client = cl))
  expect_gt(seen, 1200L)
  expect_identical(seen, readgpt:::outline_reply_tokens(300L, 6L, gr_model_info("mock-model")))
  # A reasoning model spends part of the limit thinking, so it gets more.
  local_registries()
  gr_register_model("outline-think", context_window = 200000L, max_output = 64000L,
                    input_usd = 0, output_usd = 0, reasoning = TRUE)
  quiet(gr_outline(cm, client = cl, model = "outline-think"))
  expect_gt(seen, 1200L + 3000L)
})

test_that("a cut-off outline names the argument that raises its limit", {
  cm <- r6c_many_claims(2L)
  cl <- gr_mock_client(function(m, p) gr_result(TRUE, '{"sections":[{"heading":"T',
                                                finish_reason = "length"))
  r <- r6c_warns(gr_outline(cm, client = cl, max_outline_tokens = 900))
  msg <- r$messages[r$classes == "gr_outline_truncated"]
  expect_match(msg, "stopped at the 900-token reply limit", fixed = TRUE)
  expect_match(msg, "max_outline_tokens", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# synthesis-06: citations moved without changing the set
# ---------------------------------------------------------------------------

r6c_revise <- function(draft, revision, pass = "register") {
  cl <- gr_mock_client(function(m, p) revision)
  r6c_warns(readgpt:::revise_once(draft, "Q?", cl, gr_read_spec("stuff", model = "mock-model"),
                                  gr_trace(), NULL, pass))
}

test_that("a marker moved past the full stop still makes its sentence a claim", {
  # Before: "[study 1] [study 2]" became a sentence of its own, so the universal
  # in the sentence it cites was never counted.
  expect_false(readgpt:::strength_guard(
    "The two trials reported a benefit in adults [study 1] [study 2].",
    "All trials always show a benefit in every adult. [study 1] [study 2]")$ok)
  expect_equal(readgpt:::claim_sentences("A benefit in adults. [study 1] [study 2]"),
               "A benefit in adults. [study 1] [study 2]")
  # An honest revision that only moves the marker is still allowed.
  expect_true(readgpt:::strength_guard(
    "The two trials may suggest a benefit in adults [study 1] [study 2].",
    "The two trials may suggest a benefit in adults. [study 1] [study 2]")$ok)
})

test_that("taking the citation off a claim that stays is refused", {
  # Before: the set of cited studies was unchanged -- study 1 is cited in the
  # other sentence -- so the uncited universal claim was kept.
  draft <- paste("Participants over 65 in that trial had more adverse events [study 1].",
                 "The trial may suggest a small benefit [study 1].")
  r <- r6c_revise(draft, paste("All patients over 65 always suffer more adverse events.",
                               "The trial may suggest a small benefit [study 1]."))
  expect_false(r$value$kept)
  expect_true("gr_coherence_rejected" %in% r$classes)
  expect_match(r$value$reason, "took the citation off 1 claim sentence", fixed = TRUE)
  # Even with an uncited sentence deleted to balance the count, the universal is seen.
  expect_false(readgpt:::strength_guard(
    paste("This section covers harms.", draft),
    paste("All patients over 65 always suffer more adverse events.",
          "The trial may suggest a small benefit [study 1]."))$ok)
  # Cutting a repeated claim whole is what the cut pass is for.
  r <- r6c_revise(paste(draft, "Older participants in that trial had more adverse events [study 1]."),
                  draft, pass = "cut")
  expect_true(r$value$kept)
})

test_that("citing a study more often than the draft did is refused", {
  # Before: new claim sentences reusing an existing marker passed -- same set,
  # and the hedge floor was capped at the draft's own count.
  r <- r6c_revise("The trial may suggest a small benefit [study 1].",
                  paste("The trial may suggest a small benefit [study 1].",
                        "The drug cures the disease [study 1]. It works in children [study 1]."))
  expect_false(r$value$kept)
  expect_match(r$value$reason, "study 1 (3 times, not 1)", fixed = TRUE)
  # The same citations written as a range are the same citations.
  r <- r6c_revise("Three trials may suggest a benefit [study 1] [study 2] [study 3].",
                  "Three trials may suggest a benefit [studies 1-3].")
  expect_true(r$value$kept)
})

# ---------------------------------------------------------------------------
# r2-non-latin-text-pipeline-04: a guard that cannot read the draft
# ---------------------------------------------------------------------------

test_that("a draft the guard cannot read is not revised", {
  # Before: the French register pass below was kept. English hedges and
  # boosters found nothing in it, so nothing could be refused.
  fr_draft <- paste0("Trois petits essais sugg\u00e8rent un b\u00e9n\u00e9fice modeste ",
                     "[study 1] [study 2]. Une cohorte pourrait montrer un risque [study 3].")
  fr_rev <- paste0("Tous les essais d\u00e9montrent clairement un b\u00e9n\u00e9fice ",
                   "[study 1] [study 2]. Le m\u00e9dicament est prouv\u00e9 s\u00fbr [study 3].")
  expect_false(readgpt:::strength_guard(fr_draft, fr_rev)$ok)
  cl <- gr_mock_client(function(m, p) fr_rev)
  r <- r6c_warns(readgpt:::synth_revise(fr_draft, "Q?", cl,
                                        gr_read_spec("stuff", model = "mock-model"),
                                        gr_trace(), "en fran\u00e7ais", "register"))
  expect_null(r$value$text)
  expect_false(r$value$report$ran)
  expect_true("gr_revision_unguarded" %in% r$classes)
  expect_length(cl$calls(), 0L)

  # Chinese: split at the ideographic full stop, and not revised either.
  zh <- paste0("\u4e09\u9879\u5c0f\u578b\u8bd5\u9a8c\u63d0\u793a\u53ef\u80fd\u6709\u76ca",
               "[study 1]\u3002\u4e00\u9879\u961f\u5217\u7814\u7a76\u53ef\u80fd\u663e\u793a",
               "\u98ce\u9669[study 2]\u3002")
  expect_length(readgpt:::split_sentences(zh), 2L)
  expect_false(readgpt:::guard_reads(zh))

  # An English draft revised into another language is refused too.
  expect_false(readgpt:::strength_guard(
    "Three small trials may suggest a modest benefit [study 1] [study 2].",
    "Tous les essais d\u00e9montrent un b\u00e9n\u00e9fice [study 1] [study 2].")$ok)
  # And English, with a Latin phrase and an acronym that look foreign, is read.
  expect_true(readgpt:::guard_reads(paste(
    "De novo variants in IL-6 may matter in von Willebrand disease [study 1].",
    "Two trials suggest a possible benefit [study 2].")))
})

test_that("an unreadable draft skips every pass through gr_synthesise", {
  fr <- paste0("Trois petits essais sugg\u00e8rent un b\u00e9n\u00e9fice modeste [study 1] ",
               "[study 2]. Une cohorte pourrait montrer un risque [study 3].")
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("<draft>", messages[[length(messages)]]$content, fixed = TRUE)) {
      return("Tous les essais d\u00e9montrent clairement un b\u00e9n\u00e9fice [study 1] [study 2].")
    }
    fr
  })
  r <- r6c_warns(gr_synthesise(r6c_table(), outline = c(Findings = "what"), question = "Q?",
                               client = cl, coherence = TRUE, cite_style = "marker"))
  expect_true("gr_revision_unguarded" %in% r$classes)
  expect_identical(r$value$coherence$pass, c("structure", "cut", "register"))
  expect_false(any(r$value$coherence$ran))
  expect_identical(r$value$text_marked, r$value$draft)
})
