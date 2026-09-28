# test-review6-audit.R -- the sixth pass on the audit report: the medium and
# low findings in R/audit.R. Each block names the finding and says what the
# report did before the fix.

# The report as one string, whitespace collapsed so wrapped prose matches.
r6_page <- function(...) {
  p <- withr::local_tempfile(fileext = ".html", .local_envir = parent.frame())
  quiet(gr_audit_report(p, ..., open = FALSE))
  gsub("[[:space:]]+", " ", paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = "\n"))
}

r6_files <- function(texts, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  for (nm in names(texts)) writeLines(texts[[nm]], file.path(d, nm))
  file.path(d, names(texts))
}

r6_table <- function(n = 4L) {
  data.frame(document = paste0(letters[seq_len(n)], ".pdf"),
             document_id = paste0("h", seq_len(n)), status = "ok",
             duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_,
             n = c(400L, 60L, 900L, 25L)[seq_len(n)],
             design = c("randomised trial", "cross-sectional", "randomised trial",
                        "qualitative")[seq_len(n)],
             stringsAsFactors = FALSE)
}

# Claims whose support names studies 7 and 12, which do not exist, and a
# moderator that is not a column; both claims survive. An outline with two
# sections, and a structure pass that merges them into one.
r6_claims_client <- function(revision = NULL) {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    all <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                 collapse = " ")
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      return(paste0(
        '{"claims":[{"claim":"Trials found a benefit.","kind":"finding","supported_by":[1,3,7],',
        '"contradicted_by":[],"moderator":"sample_type","scope":null},',
        '{"claim":"A survey found harm.","kind":"finding","supported_by":[2,12],',
        '"contradicted_by":[],"moderator":null,"scope":null}]}'))
    }
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return(paste0('{"sections":[{"heading":"Benefits","brief":"b","claims":[1],"rationale":null},',
                    '{"heading":"Harms","brief":"h","claims":[2],"rationale":null}]}'))
    }
    if (grepl("reorder a finished review", sys, fixed = TRUE)) {
      return(revision %||% paste0("## Overview\n\nTrials found a benefit [study 1] [study 3]. ",
                                  "A survey found harm [study 2]."))
    }
    if (grepl("Harms", all, fixed = TRUE)) return("A survey found harm [study 2].")
    "Trials found a benefit [study 1] [study 3]."
  })
}

# ---------------------------------------------------------------------------
# screen-protocol-03: any protocol handed to the report was presented as the
# criteria fixed in advance. Criteria edited after the run were listed as
# "Fixed before any document was read", and the criterion the screening
# actually used ("Animal study") was nowhere in the report.
# ---------------------------------------------------------------------------

test_that("a protocol edited after the run is flagged, and what the screening used is shown", {
  f <- r6_files(list(a.txt = "A randomised comparison of two treatments in adults."))
  cl <- gr_mock_client(function(m, p) paste0('{"decision":"include","reason":"r",',
                                              '"criterion":"Reports a randomised comparison",',
                                              '"quote":null}'))
  p <- gr_protocol("p", question = "Does it work?",
                   include = "Reports a randomised comparison", exclude = "Animal study")
  s <- quiet(gr_screen(f, protocol = p, client = cl))

  p2 <- p
  p2$include <- c("Reports a randomised comparison", "Follow-up of at least 5 years")
  p2$exclude <- "Industry funded"
  h <- r6_page(screening = s, protocol = p2)
  expect_match(h, "<p class='flag'>This is not the protocol the run was made under.", fixed = TRUE)
  expect_match(h, "The screening differs from it in its inclusion criteria and exclusion criteria.",
               fixed = TRUE)
  expect_false(grepl("Fixed before", h, fixed = TRUE))
  expect_match(h, "<h3>What the screening was run against</h3>", fixed = TRUE)
  expect_match(h, "<li>Animal study</li>", fixed = TRUE)

  # A changed question is a changed protocol too.
  p3 <- p
  p3$question <- "Is it cost-effective?"
  expect_match(r6_page(screening = s, protocol = p3), "differs from it in its question",
               fixed = TRUE)

  # The protocol the run used, reordered and re-spaced, is the same protocol.
  p4 <- p
  p4$include <- "  Reports a randomised comparison "
  h4 <- r6_page(screening = s, protocol = p4)
  expect_match(h4, "Fixed before the screening read any document: it recorded exactly these.",
               fixed = TRUE)
  expect_false(grepl("not the protocol the run was made under", h4, fixed = TRUE))

  # With no stage to compare against, the report does not vouch for it.
  syn_free <- r6_page(answer = quiet(answer_document(readgpt_example(), "What was revenue?",
                                                     "fast",
                                                     client = gr_mock_client(function(m, p) "45.2"))),
                      protocol = p)
  expect_match(syn_free, "cannot confirm these are the ones the run used", fixed = TRUE)
  expect_false(grepl("Fixed before", syn_free, fixed = TRUE))
})

test_that("a schema or an outline that is not the one the run used is flagged", {
  f <- r6_files(list(a.txt = "We ran a randomised trial."))
  fl <- gr_fields(design = "The study design")
  cl <- gr_mock_client(function(m, p)
    '{"design":"randomised trial","design__quote":"We ran a randomised trial."}')
  p <- gr_protocol("p", question = "What designs?", fields = fl,
                   outline = c(Designs = "Which designs were used"))
  x <- quiet(gr_extract(f, p, client = cl, recipe = "fast"))
  expect_match(r6_page(extraction = x, protocol = p), "Fixed before the extraction read any document",
               fixed = TRUE)
  p2 <- p
  p2$fields <- gr_fields(design = "The study design", n = gr_field("Sample size", type = "integer"))
  h <- r6_page(extraction = x, protocol = p2)
  expect_match(h, "The extraction differs from it in its schema.", fixed = TRUE)
  expect_match(h, "<h3>The schema the extraction used</h3>", fixed = TRUE)

  syn <- quiet(gr_synthesise(x, p, client = gr_mock_client(function(m, pp) "Trials [study 1].")))
  expect_false(grepl("differs from it", r6_page(synthesis = syn, protocol = p), fixed = TRUE))
  p3 <- p
  p3$outline <- c(Findings = "What was found")
  h3 <- r6_page(synthesis = syn, protocol = p3)
  expect_match(h3, "The write-up differs from it in its outline.", fixed = TRUE)
  expect_match(h3, "Outline, as the write-up recorded it", fixed = TRUE)
})

test_that("with no protocol, the report shows the criteria the screening recorded", {
  f <- r6_files(list(a.txt = "A randomised comparison of two treatments in adults."))
  cl <- gr_mock_client(function(m, p) paste0('{"decision":"include","reason":"r",',
                                              '"criterion":"Randomised","quote":null}'))
  s <- quiet(gr_screen(f, question = "Q?", include = "Randomised", exclude = "Animal study",
                       client = cl))
  h <- r6_page(screening = s)
  expect_match(h, "No protocol was given with the report.", fixed = TRUE)
  expect_match(h, "<li>Animal study</li>", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# security-04: a presigned address was copied whole into the report -- the
# document column, the headings and the fetch error -- signature and key id
# included.
# ---------------------------------------------------------------------------

test_that("a web address is shown without its credentials", {
  u <- "https://user:pw@files.example.org/paper.pdf?X-Amz-Signature=SECRETSIG&X-Amz-Credential=AKIAKEY#f"
  shown <- readgpt:::report_doc_name(u)
  expect_false(grepl("SECRETSIG|AKIAKEY|user:pw|pw@", shown))
  expect_match(shown, "^https://files\\.example\\.org/paper\\.pdf\\?\\[query hidden [0-9a-f]{6}\\]$")
  # The label a corpus gives it (no scheme), and two that differ only in the
  # query stay apart.
  a <- readgpt:::report_url(c("files.example.org/p.pdf?sig=AAA", "files.example.org/p.pdf?sig=BBB"))
  expect_false(any(grepl("AAA|BBB", a)))
  expect_false(identical(a[1], a[2]))
  # A file name is left alone, "#" in it included.
  expect_identical(readgpt:::report_url(c("report.txt", "2019/report.txt", "notes#2.txt",
                                          "data.v2/report.txt", NA)),
                   c("report.txt", "2019/report.txt", "notes#2.txt", "data.v2/report.txt", NA))
  expect_identical(readgpt:::report_url_text("Fetching 'https://h.org/x?t=SECRET' returned HTTP 403."),
                   sprintf("Fetching 'https://h.org/x?[query hidden %s]' returned HTTP 403.",
                           substr(readgpt:::gr_hash("?t=SECRET"), 1, 6)))
})

test_that("a corpus read from presigned links does not put the links' secrets in the report", {
  local_clean_cache()
  local_mocked_bindings(url_download = function(url, dest) {
    if (grepl("denied", url, fixed = TRUE)) return(list(status = 403L, type = ""))
    writeLines("Revenue was 45.2 million dollars.", dest)
    list(status = 200L, type = "text/plain")
  }, .package = "readgpt")
  urls <- c("https://bucket.example.org/p.txt?X-Amz-Credential=AKIAKEYID&X-Amz-Signature=SECRETSIG",
            "https://bucket.example.org/denied.txt?X-Amz-Signature=SECRETSIG2")
  co <- quiet(gr_read_many(urls, "What was revenue?", "fast",
                           client = gr_mock_client(function(m, p) "Revenue was 45.2 million.")))
  # The objects no longer hold the secrets either: the corpus labels a fetched
  # document, and the ingest keeps its source, with the query as a fingerprint
  # set off by a space (see url_shown()), which the report shows as it is.
  expect_false(any(grepl("SECRETSIG", co$summary$document, fixed = TRUE)))
  h <- r6_page(answer = co)
  expect_false(grepl("SECRETSIG|AKIAKEYID", h))
  expect_match(h, "bucket.example.org/p.txt [query hidden", fixed = TRUE)
  expect_match(h, "HTTP 403", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# r2-audit-report-truthfulness-01: the status line was chosen from `partial`
# alone, so an answer whose reader left it FALSE after a request failed was
# certified "Not partial: every request succeeded" above the table flagging
# the failed request.
# ---------------------------------------------------------------------------

test_that("a failed request is not reported as every request succeeding", {
  doc <- paste(sprintf(paste0("## Section %d\n\nParagraph %d says revenue in region %d was %d.5 ",
                              "million dollars in the year under review, a figure the board noted."),
                       1:6, 1:6, 1:6, 40 + 1:6), collapse = "\n\n")
  cl <- gr_mock_client(function(m, p) {
    body <- m[[2]]$content
    one <- lengths(regmatches(body, gregexpr("[chunk ", body, fixed = TRUE))) == 1L
    if (one && grepl("Paragraph 3 says", body, fixed = TRUE)) stop("HTTP 500 upstream")
    "Revenue was 43.5 million dollars."
  })
  a <- quiet(answer_document(doc, "What was revenue in region 3?",
                             list(segment = list(method = "paragraph", max_tokens = 60),
                                  read = list(reader = "ensemble",
                                              members = c("retrieve", "map_reduce"))),
                             client = cl))
  expect_gt(length(a$trace$errors), 0L)
  # An ensemble whose member failed a request is partial now (read-methods-01).
  expect_true(a$partial)
  h <- r6_page(answer = a)
  expect_false(grepl("Not partial", h, fixed = TRUE))
  expect_false(grepl("every request succeeded", h, fixed = TRUE))
  expect_match(h, "Partial: 1 request(s) failed; first error: HTTP 500 upstream", fixed = TRUE)
  # A reader that still leaves partial FALSE after a failure gets the fallback.
  b <- a
  b$partial <- FALSE
  b$notes$failed_calls <- NULL
  b$notes$partial_members <- NULL
  h <- r6_page(answer = b)
  expect_false(grepl("Not partial", h, fixed = TRUE))
  expect_match(h, paste("<p class='flag'>The reader did not mark this answer partial, but",
                        "1 request(s) failed (first error: HTTP 500 upstream)"), fixed = TRUE)
})

test_that("a clean answer is not partial, and a recovered failure is named as recovered", {
  a <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast",
                             client = gr_mock_client(function(m, p) "Revenue was 45.2 million.")))
  h <- r6_page(answer = a)
  expect_match(h, "<p class='ok'>Not partial: no request failed", fixed = TRUE)

  a$trace$errors <- list(list(step = 1L, label = "embed.request", recovered = TRUE,
                              error = "HTTP 404"))
  h2 <- r6_page(answer = a)
  expect_match(h2, "the 1 request(s) that failed were recovered by a fallback", fixed = TRUE)
  expect_false(grepl("no request failed", h2, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# r2-audit-report-truthfulness-08: a document whose only request failed was
# reported "Not found in the part of the document that was read".
# ---------------------------------------------------------------------------

test_that("an answer whose only request failed is no answer, not 'not found'", {
  f <- r6_files(list(a.txt = "Revenue was 45.2 million.",
                     b.txt = "The memo covers staffing only, in some detail."))
  cl <- gr_mock_client(function(m, p) {
    u <- paste(vapply(m, `[[`, "", "content"), collapse = "\n")
    if (grepl("staffing", u, fixed = TRUE)) stop("HTTP 500 Internal Server Error")
    "Revenue was 45.2 million."
  })
  co <- quiet(gr_read_many(f, "What was revenue?", "fast", client = cl))
  h <- r6_page(answer = co)
  expect_false(grepl("Not found in the part of the document that was read", h, fixed = TRUE))
  expect_match(h, paste("<h3>b.txt</h3> <p><strong>No answer: the request that would have given",
                        "one failed or was not sent"), fixed = TRUE)

  one <- quiet(answer_document(f[2], "What was revenue?", "fast", client = cl))
  h1 <- r6_page(answer = one)
  expect_match(h1, "No answer: the request that would have given one failed", fixed = TRUE)
  expect_false(grepl("Not found in", h1, fixed = TRUE))

  # A reader that read and found nothing still says "not found".
  none <- quiet(answer_document(f[2], "What was revenue?", "fast",
                                client = gr_mock_client(function(m, p) "NOT_IN_DOCUMENT")))
  expect_match(r6_page(answer = none), "<strong>Not found in the document.</strong>", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# r2-audit-report-truthfulness-07: "claims dropped" counted every $dropped
# row, including a study number taken off a claim that was kept and a
# moderator cleared from one: 3 for a run that dropped none.
# ---------------------------------------------------------------------------

test_that("the flow counts dropped claims apart from removed study numbers and moderators", {
  cm <- quiet(gr_claims(r6_table(), question = "Q?", client = r6_claims_client()))
  expect_equal(nrow(cm$claims), 2L)
  expect_equal(nrow(cm$dropped), 3L)
  fl <- gr_flow(claims = cm)
  n <- function(st) fl$n[trimws(fl$stage) == st]
  expect_identical(n("claims dropped"), 0L)
  expect_identical(n("study numbers removed"), 2L)
  expect_identical(n("moderators cleared"), 1L)

  gone <- gr_mock_client(function(messages, params) {
    if (grepl("turn a table of studies", messages[[1]]$content, fixed = TRUE)) {
      return(paste0('{"claims":[{"claim":"Real.","kind":"finding","supported_by":[1],',
                    '"contradicted_by":[],"moderator":null,"scope":null},',
                    '{"claim":"Invented.","kind":"finding","supported_by":[42],',
                    '"contradicted_by":[],"moderator":null,"scope":null}]}'))
    }
    '{"groups":[]}'
  })
  cm2 <- quiet(gr_claims(r6_table(), question = "Q?", client = gone))
  fl2 <- gr_flow(claims = cm2)
  expect_identical(fl2$n[fl2$stage == "claims dropped"], 1L)
  expect_identical(fl2$n[fl2$stage == "study numbers removed"], 1L)
})

# ---------------------------------------------------------------------------
# r2-audit-report-truthfulness-05, -03, money-10: the claims table's section
# column was empty for every claim; a kept revision was hidden behind the
# draft it replaced; and the claims and outline calls were in no row of the
# cost table.
# ---------------------------------------------------------------------------

test_that("the claims table shows each claim's section when it was recorded, and no empty column", {
  cl <- r6_claims_client()
  cm <- quiet(gr_claims(r6_table(), question = "Q?", client = cl))
  ol <- quiet(gr_outline(cm, client = cl))
  syn <- quiet(gr_synthesise(r6_table(), outline = ol, question = "Q?", client = cl,
                             claims = cm, cite_style = "marker"))
  h <- r6_page(synthesis = syn, claims = cm)
  claims_part <- sub("<h3>Every claim, study by study.*", "", h)
  if (is.null(syn[["claim_sections", exact = TRUE]])) {
    # Not recorded by this synthesis: said, and no column of dashes.
    expect_false(grepl("<th>section</th>", claims_part, fixed = TRUE))
    expect_match(claims_part, "Which section each claim was given to was not recorded", fixed = TRUE)
  }
  syn$claim_sections <- attr(ol, "claims")
  h2 <- r6_page(synthesis = syn, claims = cm)
  expect_match(h2, "<td>Trials found a benefit.</td>.*<td>Benefits</td></tr>")
  expect_match(h2, "<td>A survey found harm.</td>.*<td>Harms</td></tr>")
})

test_that("a kept revision is shown as what was written, with the draft apart", {
  cl <- r6_claims_client()
  cm <- quiet(gr_claims(r6_table(), question = "Q?", client = cl))
  ol <- quiet(gr_outline(cm, client = cl))
  syn <- quiet(gr_synthesise(r6_table(), outline = ol, question = "Q?", client = cl,
                             claims = cm, cite_style = "marker", coherence = "structure"))
  expect_true(any(syn$coherence$kept))
  h <- r6_page(synthesis = syn, claims = cm)
  expect_match(h, "<h2>What was written, and what it rests on</h2>", fixed = TRUE)
  expect_match(h, "A revision pass was kept, so this is the text as published", fixed = TRUE)
  expect_match(h, "<blockquote class='passage'>## Overview Trials found a benefit", fixed = TRUE)
  expect_match(h, "<td>Overview</td><td class=\"num\">2</td><td>b.pdf</td>", fixed = TRUE)
  expect_match(h, "<h3>Draft before revision</h3>", fixed = TRUE)
  expect_match(h, "<h4>Benefits</h4>", fixed = TRUE)
  expect_match(h, "<h3>Revision passes</h3>", fixed = TRUE)
  expect_lt(regexpr("## Overview", h, fixed = TRUE), regexpr("Draft before revision", h, fixed = TRUE))

  # Without a kept pass the sections are what was written, as before.
  plain <- quiet(gr_synthesise(r6_table(), outline = ol, question = "Q?", client = cl,
                               claims = cm, cite_style = "marker"))
  hp <- r6_page(synthesis = plain, claims = cm)
  expect_match(hp, "<h2>What was written, and what each section rests on</h2> <h3>Benefits</h3>",
               fixed = TRUE)
  expect_false(grepl("Draft before revision", hp, fixed = TRUE))
})

test_that("the cost table has the claims and outline calls, and only those", {
  cl <- r6_claims_client()
  tr <- gr_trace()
  # Something else on the same trace: a screening's calls, already a row of
  # their own, must not be counted again as claims.
  f <- r6_files(list(a.txt = "A randomised comparison."))
  s <- quiet(gr_screen(f, question = "Q?", include = "Randomised", trace = tr,
                       client = gr_mock_client(function(m, p) paste0(
                         '{"decision":"include","reason":"r","criterion":"Randomised",',
                         '"quote":null}'))))
  cm <- quiet(gr_claims(r6_table(), question = "Q?", client = cl, trace = tr))
  ol <- quiet(gr_outline(cm, client = cl))
  syn <- quiet(gr_synthesise(r6_table(), outline = ol, question = "Q?", client = cl,
                             claims = cm, cite_style = "marker"))
  mine <- sum(vapply(tr$steps, function(st) grepl("^(claims|outline)\\.", st$label %||% ""),
                     logical(1)))
  expect_gt(tr$calls, mine)
  h <- r6_page(screening = s, synthesis = syn, claims = cm)
  expect_match(h, sprintf("<tr><td>claims</td><td class=\"num\">%d</td>", mine), fixed = TRUE)
  expect_match(h, sprintf("<tr><td>synthesis</td><td class=\"num\">%d</td>", syn$trace$calls),
               fixed = TRUE)
  # From the synthesis alone, which carries its claims.
  expect_match(r6_page(synthesis = syn), "<tr><td>claims</td>", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# r2-audit-report-truthfulness-04, and the synthesis-2 handoff: a section
# marked partial for a failed batch of studies was shown with no flag; and a
# citation the revision left as a marker was not mentioned.
# ---------------------------------------------------------------------------

test_that("a section missing a batch of studies is flagged", {
  local_registries()
  gr_register_model("small-merge6", context_window = 3000, max_output = 400,
                    input_usd = 0, output_usd = 0)
  tab <- data.frame(document = sprintf("d%02d.pdf", 1:40), document_id = sprintf("h%02d", 1:40),
                    status = "ok", duplicate_of = NA_character_, n_filled = 2L,
                    n_unverified = 0L, conflicts = NA_character_,
                    finding = paste(rep("The intervention reduced the outcome modestly in adults.",
                                        9), collapse = " "),
                    stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(m, p) {
    u <- paste(vapply(m, `[[`, "", "content"), collapse = " ")
    if (grepl("<draft", u, fixed = TRUE)) {
      st <- sub(".*?<draft", "", u)
      ids <- unique(regmatches(st, gregexpr("\\[study [0-9]+\\]", st))[[1]])
      return(paste("Merged: a modest benefit", paste(ids, collapse = " "), "."))
    }
    if (grepl("[study 1]\n", u, fixed = TRUE)) stop("HTTP 500 batch")
    st <- sub(".*<studies>", "", u)
    paste("Draft: a benefit", regmatches(st, regexpr("\\[study [0-9]+\\]", st)), ".")
  })
  syn <- quiet(gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl,
                             model = "small-merge6", max_section_tokens = 300))
  expect_true(syn$sections$partial)
  expect_identical(syn$sections$n_unknown + syn$sections$n_unsupplied, 0L)
  # A synthesis made before the batch counts were recorded on the section.
  old <- syn
  old$sections[c("lost_batches", "capped_batches", "merge_failed")] <- NULL
  h <- r6_page(synthesis = old)
  expect_match(h, "<p class='flag'>This section is marked partial: a batch of the studies",
               fixed = TRUE)

  # When the synthesis records the batch counts, as gr_synthesise() now does,
  # the cause is named.
  expect_identical(syn$sections$lost_batches, 1L)
  h2 <- r6_page(synthesis = syn)
  expect_match(h2, "1 batch(es) of studies failed, so the studies in them are not in this section.",
               fixed = TRUE)
  expect_false(grepl("This section is marked partial", h2, fixed = TRUE))

  # An empty section says it is empty, not merely that it cites nothing.
  syn$sections$text <- ""
  expect_match(r6_page(synthesis = syn), "This section is empty", fixed = TRUE)
})

test_that("a citation the revision left as a marker is said in the report", {
  syn <- quiet(gr_synthesise(r6_table(2L), outline = c(Findings = "what"), question = "Q?",
                             client = gr_mock_client(function(m, p) "A benefit [study 1].")))
  syn$unrendered <- 2L
  expect_match(r6_page(synthesis = syn),
               "<p class='flag'>Study 2 is cited honestly but left as a [study N] marker",
               fixed = TRUE)
  syn$unrendered <- NULL
  expect_false(grepl("left as a [study N] marker", r6_page(synthesis = syn), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# records-audit-07: a duplicate's values and evidence spans were counted again
# -- "extracted from 2" beside "values unsupported 4", and "6 span(s); 4 could
# not be found" for 4 spans, 3 unverified.
# ---------------------------------------------------------------------------

test_that("a duplicate document's values and spans are not counted twice", {
  f <- r6_files(list(a.txt = "Revenue was 45.2 million. Staff was 12.",
                     b.txt = "Revenue was 45.2 million. Staff was 12.",
                     c.txt = "Revenue was 51.8 million. Staff was 30."))
  fields <- gr_fields(revenue = gr_field("Revenue", type = "number"),
                      staff = gr_field("Staff", type = "integer"))
  cl <- gr_mock_client(function(m, p) {
    u <- paste(vapply(m, `[[`, "", "content"), collapse = " ")
    if (grepl("45.2", u, fixed = TRUE)) {
      return(paste0('{"revenue":45.2,"staff":12,"revenue__quote":"Revenue was 45.2 million.",',
                    '"staff__quote":"Nowhere here."}'))
    }
    '{"revenue":51.8,"staff":30,"revenue__quote":"Not in text one.","staff__quote":"Not two."}'
  })
  x <- quiet(gr_extract(f, fields, client = cl, recipe = "fast"))
  expect_identical(x$table$duplicate_of, c(NA, "a.txt", NA))
  fl <- gr_flow(extraction = x)
  expect_identical(fl$n[fl$stage == "extracted from"], 2L)
  expect_identical(fl$n[fl$stage == "values unsupported"], 3L)
  h <- r6_page(extraction = x)
  expect_match(h, paste("4 span(s); <span class='flag'>3 could not be found in the chunk",
                        "cited</span>. 2 more for duplicate documents are left out"), fixed = TRUE)
  ev_rows <- regmatches(h, gregexpr("<tr><td>b\\.txt</td><td>(revenue|staff)</td>", h))[[1]]
  expect_length(ev_rows, 0L)
})

# ---------------------------------------------------------------------------
# r2-audit-report-truthfulness-10: the caveat said a span not found is left
# without a page, above a fabricated quote listed with its chunk's page.
# ---------------------------------------------------------------------------

test_that("the page caveat says an unplaced span keeps its chunk's page", {
  local_registries()
  gr_register_extractor("pgs6", "pgs6", function(path, opts) {
    data.frame(text = c("Page one says the trial enrolled 120 adults.",
                        "Page two reports the primary outcome improved."),
               page = c(1L, 2L), stringsAsFactors = FALSE)
  })
  f <- tempfile(fileext = ".pgs6")
  writeLines("x", f)
  cl <- gr_mock_client(function(m, p) '{"n":450,"n__quote":"The trial enrolled 450 adults in total."}')
  x <- quiet(gr_extract(f, gr_fields(n = gr_field("Participants", "integer")),
                        recipe = list(segment = list(method = "page", max_tokens = 400),
                                      read = list(reader = "extract")), client = cl))
  expect_false(x$evidence$verified)
  expect_identical(as.integer(x$evidence$page), 1L)
  h <- r6_page(extraction = x)
  expect_false(grepl("left without one rather than given the likeliest", h, fixed = TRUE))
  expect_match(h, paste("keeps the page of the chunk it was credited to, which says where that",
                        "chunk is, not where the sentence is"), fixed = TRUE)
})

# ---------------------------------------------------------------------------
# The extract-6 handoff: a verbatim quote that fails the value check is one
# that states the value in no form the check reads, which is not quite "does
# not state the value".
# ---------------------------------------------------------------------------

test_that("an unstated value is qualified by the forms the check reads", {
  f <- r6_files(list(a.txt = "Twenty-four patients were enrolled at two sites."))
  cl <- gr_mock_client(function(messages, params)
    '{"n":5000,"n__quote":"Twenty-four patients were enrolled at two sites."}')
  x <- quiet(gr_extract(f, gr_fields(n = gr_field("Sample size", type = "integer")),
                        client = cl, keep_answers = TRUE))
  expect_match(r6_page(extraction = x),
               "1 found in the chunk cited but not stating the value cited in any form the check reads",
               fixed = TRUE)
  expect_match(r6_page(answer = x$answers[[1]]),
               "sites.&rdquo; (not in any form the check reads)", fixed = TRUE)
})
