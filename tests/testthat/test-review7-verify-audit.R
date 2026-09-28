# test-review7-verify-audit.R -- the cross-file follow-ups in R/read-verify.R,
# R/audit.R and R/extract-corpus.R. Each block names the handoff and says what
# the code did before.

# The report as one string, whitespace collapsed so wrapped prose matches.
r7_page <- function(...) {
  p <- withr::local_tempfile(fileext = ".html", .local_envir = parent.frame())
  quiet(gr_audit_report(p, ..., open = FALSE))
  gsub("[[:space:]]+", " ", paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
}

r7_files <- function(texts, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  for (nm in names(texts)) writeLines(texts[[nm]], file.path(d, nm))
  file.path(d, names(texts))
}

r7_verified <- function(quote, source) isTRUE(readgpt:::span_match(quote, source)$verified)

# ---------------------------------------------------------------------------
# h6-verify-linebreak-range: a hyphen or en dash at a line end was read as
# punctuation before a space, so the number or word after it verified alone.
# ---------------------------------------------------------------------------

test_that("a range broken across a line does not verify its second number alone", {
  txt <- "Participants were aged 18-\n65 years at entry to the trial."
  doc <- quiet(gr_ingest(txt))$text
  # The cleaner keeps the range broken across the line.
  expect_match(doc, "18-\n65", fixed = TRUE)
  # Was TRUE: "65 years" read as a number of its own.
  expect_false(r7_verified("65 years", doc))
  expect_false(r7_verified("65 years at entry", txt))
  none <- quiet(gr_ingest(txt, spec = list(clean = "none")))$text
  expect_false(r7_verified("65 years", none))
  # An en dash, and spacing or a carriage return around the break.
  expect_false(r7_verified("65 years", "aged 18–\n65 years at entry."))
  expect_false(r7_verified("65 years", "aged 18- \r\n  65 years at entry."))
  # As it is when the range is on one line.
  expect_false(r7_verified("65 years", "aged 18-65 years at entry."))
  # What is there still verifies: the whole range as copied, and the text
  # either side of it.
  expect_true(r7_verified("aged 18-\n65 years", doc))
  expect_true(r7_verified("18-\n65 years at entry", doc))
  expect_true(r7_verified("Participants were aged 18", doc))
  expect_true(r7_verified("at entry to the trial", doc))
  # A line break that follows no dash is only a space.
  expect_true(r7_verified("65 years", "aged over\n65 years at entry."))
  # And a dash spaced off before the break is not joined to a word after it.
  expect_true(r7_verified("not the placebo", "It was the drug -\nnot the placebo."))
})

test_that("a compound broken across a line does not verify either half alone", {
  doc <- quiet(gr_ingest("The Anglo-\nSaxon chronicle was read aloud to the class."))$text
  expect_match(doc, "Anglo-\nSaxon", fixed = TRUE)
  # Both were TRUE.
  expect_false(r7_verified("Saxon chronicle", doc))
  expect_false(r7_verified("The Anglo", doc))
  expect_true(r7_verified("The Anglo-\nSaxon chronicle", doc))
  expect_true(r7_verified("chronicle was read aloud", doc))
  # An en dash between words is punctuation, across a line as on one.
  expect_true(r7_verified("Jones reported it", "Smith–\nJones reported it."))
})

test_that("the text the boundary test reads still lines up with the normalised text", {
  ms <- readgpt:::match_source
  for (s in c("aged 18-\n65 years", "aged 18– \r\n 65", "end -\n", "Anglo-\nSaxon ÉTÉ")) {
    got <- ms(s)
    expect_identical(got$text, readgpt:::normalise_for_match(s), label = s)
    expect_identical(got$memo$lc, utf8ToInt(got$text), label = s)
    expect_identical(got$n, nchar(got$text), label = s)
  }
})

# ---------------------------------------------------------------------------
# h5-r3-07: the cleaner joins a compound broken at a line end unless the
# document writes it whole somewhere, and a faithful quotation with the
# hyphen then failed.
# ---------------------------------------------------------------------------

test_that("a quotation that keeps a hyphen the cleaner took out verifies", {
  doc <- quiet(gr_ingest("The trial was investigator-\nblinded and ran for two years."))$text
  expect_match(doc, "investigatorblinded", fixed = TRUE)
  # Was FALSE, 0.75.
  expect_true(r7_verified("The trial was investigator-blinded", doc))
  expect_identical(readgpt:::span_match("investigator-blinded", doc)$match, 1)
  # Where the source keeps one hyphen and loses another.
  expect_true(r7_verified("a double-blind, investigator-blinded trial",
                          "It was a double-blind, investigatorblinded trial."))
  # Still whole words: half a joined compound is not the word.
  expect_false(r7_verified("investigator", doc))
  expect_false(r7_verified("blinded and ran", doc))
  # Letters only: a hyphen beside a digit, a sign or a range is content.
  expect_false(r7_verified("COVID-19 patients", "Of the COVID19 patients, most recovered."))
  expect_false(r7_verified("20-30 patients", "We enrolled 2030 patients."))
  # Still the words of the source: a changed word is not rescued.
  expect_false(r7_verified("The trial was investigator-masked", doc))
})

test_that("taking hyphens out never lets an elision drop a negation", {
  src <- "The well-known drug was non-inferior in reducing mortality."
  # The elision leaves out "non-inferior", whose "non" negates. Joined on both
  # sides it would be "noninferior", which no negation list holds, so only a
  # quotation of one passage is read with its hyphens taken out.
  expect_false(r7_verified("The well-known drug was ... in reducing mortality", src))
  expect_false(r7_verified("The well-known drug was\n\nin reducing mortality", src))
  expect_true(r7_verified("The well-known drug was non-inferior", src))
})

test_that("a quotation found with its hyphens taken out is given its page", {
  rp <- readgpt:::resolve_evidence_pages
  blocks <- data.frame(text = c("Page one says revenue was 45.2 million dollars in total.",
                                "The trial was investigatorblinded and ran for two years."),
                       page = c(1L, 2L), section = c("Intro", "Methods"),
                       stringsAsFactors = FALSE)
  ev <- data.frame(chunk_id = 1L, page = NA_integer_, section = NA_character_,
                   text = "The trial was investigator-blinded", kind = "extracted",
                   verified = TRUE, match = 1, stringsAsFactors = FALSE)
  out <- rp(ev, blocks)
  # Was NA: the quotation was looked for as written, and found nowhere.
  expect_identical(out$page, 2L)
  expect_identical(out$section, "Methods")
  # A quotation found as written is placed as it was.
  ev$text <- "Page one says revenue was 45.2 million"
  expect_identical(rp(ev, blocks)$page, 1L)
  # One not there either way is left alone.
  ev$text <- "The trial was sponsor-blinded"
  expect_true(is.na(rp(ev, blocks)$page))
})

test_that("a skim quotation that keeps the hyphen verifies on the answer", {
  doc <- quiet(gr_ingest("The trial was investigator-\nblinded and ran for two years."))
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 200)))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("You extract evidence", messages[[1]]$content, fixed = TRUE)) {
      return("The trial was investigator-blinded")
    }
    "It was investigator-blinded."
  })
  ans <- quiet(gr_read(ch, "Was it blinded?", cl, "skim"))
  ev <- ans$evidence[ans$evidence$kind == "extracted", , drop = FALSE]
  expect_true(all(ev$verified))
  expect_true(all(gr_verify_evidence(ans)$verified[gr_verify_evidence(ans)$kind == "extracted"]))
})

# ---------------------------------------------------------------------------
# read-core H5: a trace shared across runs put every run's requests and cost
# under the one answer the report was about.
# ---------------------------------------------------------------------------

test_that("the report on one answer counts only its own run's requests", {
  cl <- gr_mock_client(function(messages, params) "It says so.")
  tr <- gr_trace()
  a1 <- quiet(answer_document("Public memo: revenue was 45.2 million in 2023.",
                              "What was revenue?", "fast", client = cl, trace = tr))
  calls1 <- tr$calls
  quiet(answer_document("Second memo: costs fell 3 percent.", "What happened to costs?",
                        "fast", client = cl, trace = tr))
  expect_gt(tr$calls, calls1)
  page <- r7_page(answer = a1)
  # Was the whole trace's count, both runs.
  expect_match(page, sprintf("<th>Requests</th><td>%d</td>", calls1), fixed = TRUE)
  own <- as.data.frame(readgpt:::answer_trace(a1))
  expect_match(page, sprintf("%d request(s),", nrow(own)), fixed = TRUE)
  expect_false(grepl(sprintf("%d request(s),", nrow(as.data.frame(tr))), page, fixed = TRUE))
  # And the cost table's row for it.
  expect_match(page, sprintf("<tr><td>answer</td><td class=\"num\">%d</td>", calls1), fixed = TRUE)
  # A trace made for the one answer is reported whole, as before.
  solo <- quiet(answer_document("Third memo: the budget is unchanged.", "What is the budget?",
                                "fast", client = cl))
  expect_match(r7_page(answer = solo), sprintf("<th>Requests</th><td>%d</td>", solo$trace$calls),
               fixed = TRUE)
})

test_that("a failure elsewhere on a shared trace is still flagged, and said to be the trace's", {
  cl <- gr_mock_client(function(messages, params) "It says so.")
  tr <- gr_trace()
  a1 <- quiet(answer_document("Public memo: revenue was 45.2 million in 2023.",
                              "What was revenue?", "fast", client = cl, trace = tr))
  quiet(answer_document("Second memo: costs fell 3 percent.", "What happened to costs?",
                        "fast", client = mock_dead(), trace = tr))
  expect_gt(length(tr$errors), 0L)
  page <- r7_page(answer = a1)
  # The safe side: an error on the shared trace is not known to be another
  # run's, so it is flagged, and the page says where it was counted.
  expect_match(page, "request(s) failed", fixed = TRUE)
  expect_match(page, "counted over the whole trace, which other runs share", fixed = TRUE)
  expect_false(grepl("Each request is listed below", page, fixed = TRUE))
})

test_that("a run whose every request failed is not reported as having read the document", {
  # The requests of this run all failed; another run on the same trace
  # succeeded. That run's reply is not this run having read something.
  tr <- gr_trace()
  a1 <- quiet(answer_document("Public memo: revenue was 45.2 million in 2023.",
                              "What was revenue?", "fast", client = mock_dead(), trace = tr))
  quiet(answer_document("Second memo: costs fell 3 percent.", "What happened to costs?",
                        "fast", client = gr_mock_client(function(m, p) "Costs fell."),
                        trace = tr))
  skip_if_not(is_not_found(a1$answer), "the failed read did not come back not found")
  a1$notes$error <- NULL
  page <- r7_page(answer = a1)
  expect_match(page, "No answer: the request that would have given one failed or was not sent",
               fixed = TRUE)
})

# ---------------------------------------------------------------------------
# read-core H6: a decomposed passage was folded one character for one, so the
# composed quotation the check had found in it was "not placed".
# ---------------------------------------------------------------------------

test_that("a composed quotation is marked in a decomposed passage", {
  nfd <- "Nous avons randomisé 120 patients dans cette étude. Le suivi a duré deux ans."
  nfc <- "Nous avons randomisé 120 patients dans cette étude."
  ch <- quiet(gr_segment(gr_ingest(nfd), list(method = "paragraph", max_tokens = 200)))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("You extract evidence", messages[[1]]$content, fixed = TRUE)) return(nfc)
    "They randomised 120 patients."
  })
  ans <- quiet(gr_read(ch, "How many were randomised?", cl, "skim"))
  expect_true(isTRUE(ans$evidence$verified[ans$evidence$kind == "extracted"][1]))
  page <- r7_page(answer = ans)
  # Was: "Quoted, and found in this chunk, but not placed in the text shown".
  expect_false(grepl("found in this chunk, but not placed", page, fixed = TRUE))
  expect_match(page, "<mark>Nous avons randomisé 120 patients dans cette étude</mark>",
               fixed = TRUE)
})

# ---------------------------------------------------------------------------
# screen HO-1: an inadequate sample of one stratum is judged by its interval,
# and the report worded it as too few eligible records.
# ---------------------------------------------------------------------------

test_that("the report says why a calibration falls short in the calibration's own words", {
  scr <- structure(list(table = data.frame(
    document = sprintf("d%04d.pdf", 1:500), decision = c(rep("exclude", 480), rep("include", 20)),
    reason = "r", stringsAsFactors = FALSE)), class = "gr_screening")
  ex <- gr_reference(scr, n = 20, of = "excluded", seed = 1)
  ex$human_decision <- "exclude"
  cal <- gr_calibrate(scr, ex)
  expect_false(cal$adequate)
  page <- r7_page(screening = scr, calibration = cal)
  # Was: "Only 0 eligible record(s) in the sample, below the 10".
  expect_false(grepl("Only 0 eligible", page, fixed = TRUE))
  expect_match(page, "points wide", fixed = TRUE)
  expect_match(page, "<p class='flag'>The interval on", fixed = TRUE)
  # A calibration saved before the note existed keeps the count sentence.
  old <- cal
  old$adequacy <- NULL
  expect_match(r7_page(screening = scr, calibration = old), "Only 0 eligible record(s)",
               fixed = TRUE)
  # An adequate one is not flagged either way.
  ok <- cal
  ok$adequate <- TRUE
  expect_false(grepl("points wide", r7_page(screening = scr, calibration = ok), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# screen HO-2 (model-output-12): an exclusion held back for naming a criterion
# outside the protocol showed the criterion with nothing to say so.
# ---------------------------------------------------------------------------

test_that("a criterion the protocol does not list is flagged in the screening table", {
  f <- r7_files(c(a.txt = "We randomly assigned 200 adults in Kenya. The trial was run in Kenya."))
  cl <- gr_mock_client(function(messages, params) paste0(
    '{"decision":"exclude","reason":"Run in Kenya.","criterion":"Conducted outside Europe",',
    '"quote":"The trial was run in Kenya."}'))
  s <- quiet(gr_screen(f, question = "Does it work?", include = "Reports a randomised comparison",
                       exclude = "Participants are children", client = cl))
  expect_false(s$table$criterion_valid)
  page <- r7_page(screening = s)
  expect_match(page, "<th>criterion</th><th>criterion_valid</th>", fixed = TRUE)
  expect_match(page, "<td><span class='flag'>FALSE</span></td>", fixed = TRUE)
  # A criterion the protocol lists is not flagged.
  cl2 <- gr_mock_client(function(messages, params) paste0(
    '{"decision":"exclude","reason":"Children.","criterion":"Participants are children",',
    '"quote":null}'))
  s2 <- quiet(gr_screen(f, question = "Does it work?", include = "Reports a randomised comparison",
                        exclude = "Participants are children", client = cl2))
  expect_true(s2$table$criterion_valid)
  expect_false(grepl("<span class='flag'>FALSE</span>", r7_page(screening = s2), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# extract audit-reported-nothing: a row with nothing filled but values that
# went unverified was "reported nothing" and "values unsupported" at once.
# ---------------------------------------------------------------------------

test_that("a row whose values could not be read did not report nothing", {
  f <- r7_files(c(a.txt = "We enrolled 120 participants, 60 per arm, in the trial."))
  fields <- gr_fields(n = gr_field("Number of participants", type = "integer"))
  cl <- gr_mock_client(function(m, p)
    '{"n":"120 (60 per arm)","n__quote":"We enrolled 120 participants"}')
  x <- quiet(gr_extract(f, fields, client = cl))
  expect_identical(x$table$n_filled, 0L)
  expect_identical(x$table$n_unverified, 1L)
  fl <- gr_flow(extraction = x)
  n <- function(stage) fl$n[fl$stage == stage]
  # Was 1 and 1.
  expect_identical(n("  reported nothing"), 0L)
  expect_identical(n("values unsupported"), 1L)
  # A row that really reports nothing still does; an unknown count is not zero.
  t <- data.frame(document = c("a", "b", "c"), status = "ok", n_filled = 0L,
                  n_unverified = c(0L, 2L, NA), duplicate_of = NA_character_,
                  stringsAsFactors = FALSE)
  fl <- gr_flow(extraction = structure(list(table = t), class = "gr_extraction"))
  expect_identical(fl$n[fl$stage == "  reported nothing"], 1L)
})

# ---------------------------------------------------------------------------
# claims r3-synthesis-layer-call-sizing-05: claims a failed reconcile left
# unmerged were counted in the flow and shown in the report as distinct.
# ---------------------------------------------------------------------------

r7_claims_table <- function() {
  data.frame(document = c("a.pdf", "b.pdf", "c.pdf"), document_id = c("h1", "h2", "h3"),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_, design = "RCT",
             finding = c("benefit", "benefit", "no benefit"), stringsAsFactors = FALSE)
}

r7_batched <- function(reconcile) {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("Group the ones", sys, fixed = TRUE)) return(reconcile(params))
    txt <- paste(vapply(messages, function(m) m$content, ""), collapse = "\n")
    ids <- regmatches(txt, gregexpr("(?<=\\[study )[0-9]+", txt, perl = TRUE))[[1]]
    sprintf(paste0('{"claims":[{"claim":"It works.","kind":"finding","supported_by":[%s],',
                   '"contradicted_by":[],"moderator":null,"scope":null}]}'),
            paste(ids, collapse = ","))
  })
}

test_that("claims left unreconciled are counted in the flow and flagged in the report", {
  tab <- r7_claims_table()
  cl <- r7_batched(function(p) readgpt:::gr_result(TRUE, '{"groups":[[1,2', finish_reason = "length"))
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl, max_claim_tokens = 440))
  expect_true(cm$unmerged)
  fl <- gr_flow(claims = cm)
  stage <- "claims not reconciled across batches"
  expect_identical(fl$n[fl$stage == stage], 1L)
  expect_match(fl$note[fl$stage == stage], "one finding may appear as more than one claim",
               fixed = TRUE)
  html <- paste(readgpt:::audit_claims(NULL, cm), collapse = " ")
  expect_match(html, "<p class='flag'>The claims from different batches of studies were not reconciled",
               fixed = TRUE)
  # A reconcile that ran: counted 0, and nothing flagged.
  cl <- r7_batched(function(p) '{"groups":[[1,2,3]]}')
  cm2 <- quiet(gr_claims(tab, question = "Q?", client = cl, max_claim_tokens = 440))
  expect_false(cm2$unmerged)
  expect_identical(gr_flow(claims = cm2)$n[gr_flow(claims = cm2)$stage == stage], 0L)
  expect_false(grepl("not reconciled", paste(readgpt:::audit_claims(NULL, cm2), collapse = " "),
                     fixed = TRUE))
  # A claims table saved before the field existed.
  cm2$unmerged <- NULL
  expect_identical(gr_flow(claims = cm2)$n[gr_flow(claims = cm2)$stage == stage], 0L)
})

# ---------------------------------------------------------------------------
# audit records-audit-07: print() summed a duplicate's unverified values and
# spans with its first copy's.
# ---------------------------------------------------------------------------

test_that("print() counts a duplicate's unverified values and spans once", {
  f <- r7_files(c(a.txt = "We enrolled 120 participants in the trial.",
                  b.txt = "We enrolled 120 participants in the trial."))
  fields <- gr_fields(n = gr_field("Number of participants", type = "integer"))
  cl <- gr_mock_client(function(m, p) '{"n":120,"n__quote":"We recruited 120 people"}')
  x <- quiet(gr_extract(f, fields, client = cl))
  expect_identical(x$table$status, c("ok", "duplicate"))
  expect_identical(x$table$n_unverified, c(1L, 1L))
  out <- paste(capture.output(print(x)), collapse = "\n")
  # Was "2 value(s) not verified" and "evidence: 2 span(s)".
  expect_match(out, "  1 value(s) not verified", fixed = TRUE)
  expect_match(out, "evidence: 1 span(s)", fixed = TRUE)
  # The same number gr_flow() gives.
  fl <- gr_flow(extraction = x)
  expect_identical(fl$n[fl$stage == "values unsupported"], 1L)
})
