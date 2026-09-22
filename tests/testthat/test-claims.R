# test-claims.R -- the claims layer: claims, structure, emphasis, gaps, revision.
#
# The guarantee running through all of it is the same one the citation check
# already gives, moved one link earlier: a claim names the studies it rests on,
# and those numbers are VERIFIED against the table rather than trusted. Most of
# what follows is that check, seen from a different side.

claims_table <- function(n = 4L) {
  data.frame(document = paste0(letters[seq_len(n)], ".pdf"),
             document_id = paste0("h", seq_len(n)), status = "ok",
             duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_,
             n = c(400L, 60L, 900L, 25L)[seq_len(n)],
             design = c("randomised trial", "cross-sectional", "randomised trial",
                        "qualitative")[seq_len(n)],
             finding = c("supports", "contradicts", "supports", "mixed")[seq_len(n)],
             stringsAsFactors = FALSE)
}

# A client that answers each stage of the layer from a fixed script.
claims_client <- function(claims = NULL, sections = NULL, groups = NULL, section_text = NULL) {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    all_txt <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                     collapse = " ")
    # "turn a table of studies", not "list of CLAIMS": the claims-section prompt
    # contains the latter too, so the section calls were being answered with the
    # claims JSON.
    if (grepl("turn a table of studies", sys, fixed = TRUE)) return(claims %||% '{"claims":[]}')
    if (grepl("Group the ones", sys, fixed = TRUE)) return(groups %||% '{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) return(sections %||% '{"sections":[]}')
    if (!is.null(section_text)) return(section_text(all_txt))
    "Something [study 1]."
  })
}

one_claim <- paste0(
  '{"claims":[{"claim":"The effect appears in trials but not in surveys.",',
  '"kind":"finding","supported_by":[1,3],"contradicted_by":[2],',
  '"moderator":"design","scope":"two trials, one survey"}]}')

# ---------------------------------------------------------------------------
# gr_claims(): every number is checked.
# ---------------------------------------------------------------------------

test_that("a study number that is not in the table is dropped, and said so", {
  cl <- claims_client(claims = paste0(
    '{"claims":[{"claim":"A real claim.","kind":"finding","supported_by":[1,99],',
    '"contradicted_by":[],"moderator":null,"scope":null}]}'))
  cm <- quiet(gr_claims(claims_table(), question = "Does it work?", client = cl))
  expect_equal(nrow(cm$claims), 1L)
  expect_equal(cm$claims$n_support, 1L)
  expect_setequal(cm$support$study, 1L)
  expect_true(any(grepl("not in the table", cm$dropped$reason, fixed = TRUE)))
  expect_match(cm$claims$note, "99")
})

test_that("a claim left with no supporting study is dropped entirely", {
  # A claim attached to nothing is an opinion, and an opinion in a claims table
  # is indistinguishable from a finding by the time a section is written from it.
  cl <- claims_client(claims = paste0(
    '{"claims":[{"claim":"Invented.","kind":"finding","supported_by":[42],',
    '"contradicted_by":[],"moderator":null,"scope":null}]}'))
  cm <- suppressWarnings(quiet(gr_claims(claims_table(), question = "Q?", client = cl)))
  expect_equal(nrow(cm$claims), 0L)
  expect_true(any(grepl("no supporting study", cm$dropped$reason, fixed = TRUE)))
})

test_that("a moderator naming no column is cleared, and the claim survives", {
  # The claim may be sound; the EXPLANATION was invented. Dropping the claim
  # would lose a real finding over a bad reason for it.
  cl <- claims_client(claims = paste0(
    '{"claims":[{"claim":"Trials and surveys disagree.","kind":"finding",',
    '"supported_by":[1],"contradicted_by":[2],"moderator":"funding","scope":null}]}'))
  cm <- quiet(gr_claims(claims_table(), question = "Q?", client = cl))
  expect_equal(nrow(cm$claims), 1L)
  expect_true(is.na(cm$claims$moderator))
  expect_match(cm$claims$note, "funding")
  expect_true(any(grepl("not a column", cm$dropped$reason, fixed = TRUE)))
})

test_that("a study cannot both support and contradict one claim", {
  cl <- claims_client(claims = paste0(
    '{"claims":[{"claim":"Contradictory bookkeeping.","kind":"finding","supported_by":[1,2],',
    '"contradicted_by":[2],"moderator":null,"scope":null}]}'))
  cm <- quiet(gr_claims(claims_table(), question = "Q?", client = cl))
  expect_equal(cm$claims$n_contradict, 0L)
  expect_setequal(cm$support$study[cm$support$role == "supports"], c(1L, 2L))
})

test_that("as_id_list() survives every shape jsonlite makes of an array of arrays", {
  # simplifyVector = TRUE is not type-stable here: equal lengths give a MATRIX,
  # unequal lengths a list, a single element a bare vector. A caller that assumes
  # one of the three is broken by the other two, and silently -- the ids come out
  # transposed or recycled rather than missing.
  f <- readgpt:::as_id_list
  expect_identical(f(matrix(c(1L, 2L, 3L, 4L), nrow = 2, byrow = TRUE), 2L),
                   list(1:2, 3:4))
  expect_identical(f(list(c(1L, 2L), 3L), 2L), list(1:2, 3L))
  expect_identical(f(c(5L, 6L), 2L), list(5L, 6L))
  expect_identical(f(NULL, 2L), list(integer(0), integer(0)))
  expect_identical(f(list(integer(0), 7L), 2L), list(integer(0), 7L))
})

test_that("claim numbers and [study N] markers mean the same row", {
  # Both derive from one function. Two copies of the filter would drift the
  # moment one learned about a new status value, and the symptom would be a
  # review attributing findings to papers that do not contain them.
  tab <- claims_table()
  tab$status[2] <- "failed"
  cm <- quiet(gr_claims(tab, question = "Q?", client = claims_client(claims = one_claim)))
  syn_studies <- readgpt:::synth_studies(tab, FALSE)
  expect_identical(cm$studies$document_id, syn_studies$document_id)
  expect_identical(cm$studies$study, syn_studies$study)
})

test_that("claims from different batches are reconciled, and none is lost", {
  # Batched, each batch sees its own studies only, so one corpus-wide claim comes
  # back once per batch with disjoint support -- several narrow claims where
  # there is one broad one.
  per_batch <- paste0(
    '{"claims":[{"claim":"It works.","kind":"finding","supported_by":[1],',
    '"contradicted_by":[],"moderator":null,"scope":null},',
    '{"claim":"Something else entirely.","kind":"method","supported_by":[1],',
    '"contradicted_by":[],"moderator":null,"scope":null}]}')
  cl <- claims_client(claims = per_batch, groups = '{"groups":[[1,3],[2]]}')
  got <- readgpt:::claims_reconcile(
    readgpt:::claims_verify(
      rbind(readgpt:::claim_rows(jsonlite::fromJSON(per_batch)$claims),
            readgpt:::claim_rows(jsonlite::fromJSON(per_batch)$claims)),
      readgpt:::synth_studies(claims_table(), FALSE))$claims,
    "Q?", cl, gr_read_spec("stuff"), gr_trace())
  # Four claims in, three groups named, but claim 4 was never placed -- and it
  # keeps its own group rather than disappearing.
  expect_equal(nrow(got), 3L)
  expect_identical(got$claim_id, 1:3)
})

test_that("the reconcile pass cannot invent a claim or use a number that was not listed", {
  cl <- claims_client(groups = '{"groups":[[1,99],[2]]}')
  claims <- readgpt:::claims_verify(
    readgpt:::claim_rows(jsonlite::fromJSON(paste0(
      '{"claims":[{"claim":"One.","kind":"finding","supported_by":[1],"contradicted_by":[],',
      '"moderator":null,"scope":null},{"claim":"Two.","kind":"finding","supported_by":[2],',
      '"contradicted_by":[],"moderator":null,"scope":null}]}'))$claims),
    readgpt:::synth_studies(claims_table(), FALSE))$claims
  got <- readgpt:::claims_reconcile(claims, "Q?", cl, gr_read_spec("stuff"), gr_trace())
  expect_equal(nrow(got), 2L)
  expect_setequal(unlist(got$.support), c(1L, 2L))
  # Indexing `claims[c(1, 99), ]` does not error -- it produces a row of NAs, so
  # an out-of-range group number becomes a claim with no text rather than a
  # failure. Asserting on nrow() alone cannot see that.
  expect_setequal(got$claim, c("One.", "Two."))
  expect_false(anyNA(got$claim))
})

test_that("no claims is a warning and an empty object, not an error", {
  cl <- claims_client(claims = '{"claims":[]}')
  # suppressMessages(), not quiet(): quiet() suppresses warnings too, so
  # expect_warning() around it can never see one.
  cm <- NULL
  expect_warning(cm <- suppressMessages(gr_claims(claims_table(), question = "Q?", client = cl)),
                 class = "gr_no_claims")
  expect_s3_class(cm, "gr_claims")
  expect_equal(nrow(cm$claims), 0L)
  expect_error(gr_outline(cm, client = cl), class = "gr_no_claims")
})

test_that("gr_claims needs a question, because a claim is relative to one", {
  expect_error(gr_claims(claims_table(), client = claims_client()), class = "gr_no_question")
  expect_error(gr_claims(claims_table(), question = "Q?", protocol = gr_protocols("claims"),
                         client = claims_client()),
               class = "gr_protocol_unedited")
})

# ---------------------------------------------------------------------------
# Emphasis: an ordering, not a quality score.
# ---------------------------------------------------------------------------

test_that("study_weight orders by size and completeness, and not by design", {
  st <- readgpt:::synth_studies(claims_table(), FALSE)
  w <- readgpt:::study_weight(st)
  expect_gt(w[["3"]], w[["4"]])           # 900 participants over 25
  # Ranking designs would assert that a cohort beats a qualitative study, which
  # is a methodological claim this package has no standing to make. Design is a
  # grouping variable here, not a score, so changing it changes nothing.
  st2 <- st; st2$design <- rev(st2$design)
  expect_identical(unname(w), unname(readgpt:::study_weight(st2)))
  # An unverified value costs the row something.
  st3 <- st; st3$n_unverified <- c(0L, 0L, 2L, 0L)
  expect_lt(readgpt:::study_weight(st3)[["3"]], w[["3"]])
})

test_that("claim_order puts the broader claim first", {
  cm <- quiet(gr_claims(claims_table(), question = "Q?", client = claims_client(claims = paste0(
    '{"claims":[{"claim":"Narrow.","kind":"finding","supported_by":[4],',
    '"contradicted_by":[],"moderator":null,"scope":null},',
    '{"claim":"Broad.","kind":"finding","supported_by":[1,2,3],',
    '"contradicted_by":[],"moderator":null,"scope":null}]}'))))
  ord <- readgpt:::claim_order(cm$claims, cm$support, readgpt:::study_weight(cm$studies))
  expect_identical(cm$claims$claim[ord][1], "Broad.")
})

# ---------------------------------------------------------------------------
# gr_outline(): every claim lands somewhere, exactly once.
# ---------------------------------------------------------------------------

outline_fixture <- function(sections) {
  cl <- claims_client(claims = paste0(
    '{"claims":[{"claim":"Finding one.","kind":"finding","supported_by":[1],',
    '"contradicted_by":[],"moderator":null,"scope":null},',
    '{"claim":"Finding two.","kind":"finding","supported_by":[2],',
    '"contradicted_by":[],"moderator":null,"scope":null},',
    '{"claim":"Nobody studied adolescents.","kind":"gap","supported_by":[3],',
    '"contradicted_by":[],"moderator":null,"scope":null}]}'),
    sections = sections)
  list(claims = quiet(gr_claims(claims_table(), question = "Q?", client = cl)), client = cl)
}

test_that("every claim is assigned exactly once, and an empty section is dropped", {
  f <- outline_fixture(paste0(
    '{"sections":[{"heading":"A","brief":"first","claims":[1,2],"rationale":"together"},',
    '{"heading":"B","brief":"second","claims":[1],"rationale":null},',
    '{"heading":"Empty","brief":"nothing","claims":[],"rationale":null}]}'))
  o <- quiet(gr_outline(f$claims, client = f$client))
  map <- attr(o, "claims")
  expect_equal(sum(duplicated(map$claim_id)), 0L)
  expect_setequal(map$claim_id, 1:3)
  expect_false("Empty" %in% names(o))
  # A claim assigned twice keeps its first section: written up twice, a reader
  # has no way to tell it is one claim.
  expect_identical(map$section[map$claim_id == 1], "A")
})

test_that("gap claims and unplaced claims go to the closing section", {
  f <- outline_fixture('{"sections":[{"heading":"A","brief":"first","claims":[1,3],"rationale":null}]}')
  o <- quiet(gr_outline(f$claims, client = f$client))
  map <- attr(o, "claims")
  expect_identical(attr(o, "closing"), "What is missing")
  # Claim 3 is a gap and was put under "A"; reading a gap as a finding inverts it.
  expect_identical(map$section[map$claim_id == 3], "What is missing")
  # Claim 2 was never placed, and is not lost.
  expect_identical(map$section[map$claim_id == 2], "What is missing")
  expect_setequal(map$claim_id, 1:3)
})

test_that("an outline call that returns nothing usable still places every claim", {
  f <- outline_fixture('{"sections":[]}')
  o <- NULL
  expect_warning(o <- suppressMessages(gr_outline(f$claims, client = f$client)),
                 class = "gr_outline_failed")
  expect_setequal(attr(o, "claims")$claim_id, 1:3)
})

# ---------------------------------------------------------------------------
# gr_synthesise(claims = ): writing from an argument, not from rows.
# ---------------------------------------------------------------------------

synth_fixture <- function(section_text = NULL) {
  f <- outline_fixture(paste0(
    '{"sections":[{"heading":"Findings","brief":"what it finds","claims":[1,2],',
    '"rationale":"the core"}]}'))
  cl <- claims_client(claims = NULL, sections = NULL, section_text = section_text)
  list(claims = f$claims, outline = quiet(gr_outline(f$claims, client = f$client)), client = cl)
}

test_that("claims drawn from a different table are refused", {
  # A claim number and a [study N] marker are the same identifier. Draw the
  # claims from one table and write from another and every claim silently points
  # at a different row -- all valid numbers, all the wrong studies.
  f <- synth_fixture()
  other <- claims_table(3L)
  expect_error(
    gr_synthesise(other, outline = f$outline, question = "Q?", client = f$client,
                  claims = f$claims),
    class = "gr_claims_mismatch")
})

test_that("claims without an assignment are refused rather than silently ignored", {
  f <- synth_fixture()
  bare <- c(Findings = "what it finds")
  expect_error(
    gr_synthesise(claims_table(), outline = bare, question = "Q?", client = f$client,
                  claims = f$claims),
    class = "gr_no_claim_assignment")
})

test_that("a section is shown only the studies its own claims rest on", {
  seen <- new.env(parent = emptyenv()); seen$txt <- character(0)
  f <- synth_fixture(section_text = function(all) {
    seen$txt <- c(seen$txt, all)
    "Both agree [study 1] [study 2]."
  })
  quiet(gr_synthesise(claims_table(), outline = f$outline, question = "Q?",
                      client = f$client, claims = f$claims, cite_style = "marker"))
  findings <- seen$txt[grepl("what it finds", seen$txt, fixed = TRUE)][1]
  # Claims 1 and 2 rest on studies 1 and 2, so study 4 has no business here --
  # and under the old path every section saw every study.
  expect_true(grepl("[study 1]", findings, fixed = TRUE))
  expect_false(grepl("[study 4]", findings, fixed = TRUE))
  # The claims themselves reach the prompt, with a sentence budget.
  expect_true(grepl("[claim 1]", findings, fixed = TRUE))
  expect_match(findings, "sentence")
})

test_that("a section that drops one of its claims is partial and says which", {
  f <- synth_fixture(section_text = function(all) "Only the first [study 1].")
  # Every section with an unwritten claim raises one, so the warnings are
  # collected rather than matched one at a time.
  seen <- character(0)
  syn <- withCallingHandlers(
    suppressMessages(gr_synthesise(claims_table(), outline = f$outline, question = "Q?",
                                   client = f$client, claims = f$claims, cite_style = "marker")),
    warning = function(w) {
      seen <<- c(seen, class(w)[1])
      invokeRestart("muffleWarning")
    })
  expect_true("gr_claims_missed" %in% seen)
  row <- syn$sections[syn$sections$section == "Findings", ]
  expect_equal(row$n_claims, 2L)
  expect_equal(row$claims_missed, 1L)
  expect_true(row$partial)
})

test_that("claims = NULL writes exactly as it did before", {
  cl <- claims_client(section_text = function(all) "Something [study 1].")
  syn <- quiet(gr_synthesise(claims_table(), outline = c(Findings = "what"), question = "Q?",
                             client = cl, cite_style = "marker"))
  expect_equal(syn$sections$n_claims, 0L)
  expect_equal(syn$sections$claims_missed, 0L)
  expect_false(syn$sections$partial)
})

# ---------------------------------------------------------------------------
# gr_gaps(): counted, not asked.
# ---------------------------------------------------------------------------

test_that("gaps are computed from the table without calling a model", {
  cl <- claims_client(claims = paste0(
    '{"claims":[{"claim":"One study found it.","kind":"finding","supported_by":[1],',
    '"contradicted_by":[],"moderator":null,"scope":null},',
    '{"claim":"Trials and surveys disagree.","kind":"finding","supported_by":[1],',
    '"contradicted_by":[2],"moderator":null,"scope":null}]}'))
  cm <- quiet(gr_claims(claims_table(), question = "Q?", client = cl))
  before <- length(cl$calls())
  g <- gr_gaps(cm)
  expect_equal(length(cl$calls()), before)          # no call was made
  expect_s3_class(g, "gr_gaps")
  expect_true("unreplicated" %in% g$kind)
  expect_true("disagreement unexplained" %in% g$kind)
})

test_that("a category nobody studied needs the schema to be visible", {
  tab <- claims_table(); tab$design <- "cohort"
  cl <- claims_client(claims = paste0(
    '{"claims":[{"claim":"Cohorts find it.","kind":"finding","supported_by":[1,2],',
    '"contradicted_by":[],"moderator":null,"scope":null}]}'))
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))

  # Without the schema, "no qualitative studies" and "we never asked about
  # design" are the same picture.
  bare <- gr_gaps(cm)
  expect_false("declared but unstudied" %in% bare$kind)
  expect_true("no variation" %in% bare$kind)

  fields <- gr_fields(design = gr_field("The design", type = "enum",
                                        values = c("cohort", "randomised trial", "qualitative")))
  with_schema <- gr_gaps(cm, extraction = fields)
  expect_true("declared but unstudied" %in% with_schema$kind)
  expect_match(with_schema$detail[with_schema$kind == "declared but unstudied"], "qualitative")
})

test_that("selecting columns from a gaps table gives a table", {
  # `[` on a classed data frame keeps the class and drops every other attribute,
  # so g[, cols] was still dispatched to print.gr_gaps() -- which then reported
  # "NA study/studies" and "no schema given" about a perfectly good object,
  # because the attributes carrying both had just been thrown away by the subset.
  # The vignette is where that showed up: it printed the warning about a schema
  # that had in fact been supplied.
  tab <- claims_table(); tab$design <- "cohort"
  cl <- claims_client(claims = paste0(
    '{"claims":[{"claim":"One cohort found it.","kind":"finding","supported_by":[1],',
    '"contradicted_by":[],"moderator":null,"scope":null}]}'))
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  g <- gr_gaps(cm)
  expect_s3_class(g, "gr_gaps")
  expect_gt(nrow(g), 0L)

  cols <- g[, c("kind", "dimension")]
  expect_false(inherits(cols, "gr_gaps"))
  expect_s3_class(cols, "data.frame")
  expect_identical(names(cols), c("kind", "dimension"))
  # A row subset is still a subset, and the print method still says the truth
  # about the whole object.
  expect_equal(nrow(g[g$kind == "unreplicated", ]), 1L)
  expect_output(print(g), "over 4 study/studies")
})

test_that("the gaps a section may state are the ones that were computed", {
  cl <- claims_client(claims = one_claim)
  cm <- quiet(gr_claims(claims_table(), question = "Q?", client = cl))
  g <- gr_gaps(cm)
  lines <- readgpt:::render_gaps(g)
  expect_true(is.null(lines) || grepl("- ", lines, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# The revision passes, and the guard the citation check cannot make.
# ---------------------------------------------------------------------------

test_that("a revision that introduces a universal quantifier is refused", {
  # The largest possible strengthening, and the least likely to be a legitimate
  # edit. Same citations either way, so the citation check sees nothing.
  before <- "Two trials suggest a benefit [study 1] [study 2]."
  after  <- "All trials suggest a benefit [study 1] [study 2]."
  g <- readgpt:::strength_guard(before, after)
  expect_false(g$ok)
  expect_match(g$reason, "all")
})

test_that("a revision that introduces a booster is refused", {
  before <- "Two small trials suggest a benefit [study 1] [study 2]."
  after  <- "Two small trials demonstrate a benefit [study 1] [study 2]."
  g <- readgpt:::strength_guard(before, after)
  expect_false(g$ok)
  expect_match(g$reason, "demonstrat")
})

test_that("stripping the hedges off the sentences that survive is refused", {
  before <- paste("Two trials suggest a possible benefit [study 1].",
                  "A small survey may agree [study 2].")
  after  <- paste("Two trials found a benefit [study 1].",
                  "A survey agreed [study 2].")
  expect_false(readgpt:::strength_guard(before, after)$ok)
})

test_that("cutting a whole hedged sentence is allowed, because that is the job", {
  # Measured as a RATE, not a total: a cut carries that sentence's hedges away
  # with it, and a guard on totals would reject the pass for doing what it is for.
  before <- paste("Two trials suggest a possible benefit [study 1].",
                  "A small survey may agree [study 2].")
  after  <- "Two trials suggest a possible benefit [study 1]."
  expect_true(readgpt:::strength_guard(before, after)$ok)
})

test_that("prose that makes no claim can be rewritten freely", {
  # Only sentences carrying a citation are measured. Headings, framing and
  # transitions are exactly what an editing pass is supposed to be free to touch.
  before <- "This section is organised by design. Two trials suggest a benefit [study 1]."
  after  <- "We now turn to the evidence, all of which is clearly relevant. Two trials suggest a benefit [study 1]."
  expect_true(readgpt:::strength_guard(before, after)$ok)
})

test_that("an escalating revision is discarded and the draft is what you get", {
  base <- c("Included studies" = "Two trials suggest a possible benefit [study 1] [study 2].")
  cl <- gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("reorder a finished review", sys, fixed = TRUE)) {
      return("## Included studies\n\nAll trials demonstrate a benefit [study 1] [study 2].")
    }
    if (grepl("write the", sys, fixed = TRUE)) return(base[[1]])
    "x"
  })
  syn <- NULL
  expect_warning(
    syn <- suppressMessages(gr_synthesise(claims_table(2L),
                                          outline = c("Included studies" = "how many"),
                                          question = "Q?", client = cl, coherence = "structure",
                                          cite_style = "marker")),
    class = "gr_revision_escalated")
  expect_false(syn$coherence$kept)
  expect_identical(syn$text, syn$draft)
  expect_match(syn$coherence$reason, "all")
})

# ---------------------------------------------------------------------------
# The audit report, which is what makes the layer defensible.
# ---------------------------------------------------------------------------

test_that("the report carries every claim, its studies, and where it ended up", {
  f <- synth_fixture(section_text = function(all) "Both agree [study 1] [study 2].")
  syn <- quiet(gr_synthesise(claims_table(), outline = f$outline, question = "Q?",
                             client = f$client, claims = f$claims, cite_style = "marker"))
  p <- withr::local_tempfile(fileext = ".html")
  quiet(gr_audit_report(p, extraction = NULL, synthesis = syn, claims = f$claims))
  html <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_match(html, "What the review claims")
  expect_match(html, "Finding one.", fixed = TRUE)
  expect_match(html, "Every claim, study by study", fixed = TRUE)

  # And the flow counts the claims, so the numbers a methods section quotes are
  # in one table.
  fl <- gr_flow(extraction = NULL, claims = f$claims)
  expect_true("claims drawn" %in% fl$stage)
  expect_equal(fl$n[fl$stage == "claims drawn"], nrow(f$claims$claims))
})
