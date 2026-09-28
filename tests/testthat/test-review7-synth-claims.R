# test-review7-synth-claims.R -- the cross-file follow-ups that fell in
# R/synthesise.R and R/claims.R.
#
# Each test failed on the code before its change, and says what that code did:
# a question typed with an accent under a C locale sent as "<c3><a9>", and an
# accented closing heading stopping the write-up; an outline that did not say
# which claims it was made from; a write-up that did not keep which section
# each claim was given to, or why a section was partial; claims drawn with a
# column the write-up withholds, passed without a word; and help that listed
# fewer reasons for discarding a revision than the code has.

r7_table <- function() {
  data.frame(document = paste0(letters[1:4], ".pdf"), document_id = paste0("h", 1:4),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_,
             design = c("trial", "survey", "trial", "survey"), stringsAsFactors = FALSE)
}

r7_claim <- function(text, ids) {
  sprintf(paste0('{"claim":"%s","kind":"finding","supported_by":[%s],"contradicted_by":[],',
                 '"moderator":null,"scope":null}'), text, paste(ids, collapse = ","))
}

# Claims 1 (trials: studies 1, 3) and 2 (surveys: 2, 4), an outline putting
# claim 1 under `heads[1]` and claim 2 under `heads[2]`, and sections that cite
# their claims' studies. Every message sent is kept in `env$seen`.
r7_client <- function(env = new.env(), claims = NULL, heads = c("Trials", "Surveys"),
                      texts = c("Trials work.", "Surveys show harm.")) {
  env$seen <- list()
  force(claims)
  gr_mock_client(function(messages, params) {
    env$seen[[length(env$seen) + 1L]] <- messages
    sys <- messages[[1]]$content
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      return(claims %||% sprintf('{"claims":[%s,%s]}', r7_claim(texts[1], c(1, 3)),
                                 r7_claim(texts[2], c(2, 4))))
    }
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return(sprintf(paste0('{"sections":[{"heading":"%s","brief":"one","claims":[1],"rationale":null},',
                            '{"heading":"%s","brief":"two","claims":[2],"rationale":null}]}'),
                     heads[1], heads[2]))
    }
    if (grepl("does not cover", sys, fixed = TRUE)) return("Nothing else is known.")
    txt <- paste(vapply(messages[-1], function(m) as.character(m$content), ""), collapse = " ")
    if (grepl(texts[2], txt, fixed = TRUE)) return("They found harm [study 2] [study 4].")
    if (grepl(texts[1], txt, fixed = TRUE)) return("They found a benefit [study 1] [study 3].")
    "The studies [study 1] [study 2]."
  })
}

# The warnings a call raised, by class, with the value.
r7_warns <- function(expr) {
  w <- list()
  val <- withCallingHandlers(suppressMessages(expr), warning = function(c) {
    w[[length(w) + 1L]] <<- c
    invokeRestart("muffleWarning")
  })
  list(value = val, classes = unlist(lapply(w, class)),
       messages = vapply(w, conditionMessage, character(1)))
}

r7_source <- function(...) {
  f <- testthat::test_path("..", "..", ...)
  if (file.exists(f)) normalizePath(f) else NULL
}

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-01: what the caller typed reaches the prompts
# as characters under a C locale.
# ---------------------------------------------------------------------------

test_that("under a C locale the question, headings, register and gaps are sent intact", {
  # Before: the question typed with an accent was UTF-8 bytes marked
  # "unknown", and paste() joining it to the claims (labelled) sent "Qu'est-ce
  # qu'une <c3><a9>tude" to every section; an accented closing heading given
  # to gr_outline() sat among the model's labelled headings and trimws() over
  # both stopped gr_synthesise() with "input string 3 is invalid UTF-8".
  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!nzchar(old), "no LC_CTYPE to restore")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", old)))
  env <- new.env()
  texts <- c("Les essais montrent un b\u00e9n\u00e9fice.", "Les enqu\u00eates montrent un tort.")
  cl <- r7_client(env, heads = c("Essais", "Enqu\u00eates"), texts = texts)
  tab <- r7_table()
  # Read in under the C locale: unlabelled, and the same everywhere.
  tab$lieu <- unmarked("Montr\u00e9al")
  q <- unmarked("Qu'est-ce qu'une \u00e9tude r\u00e9v\u00e8le ?")
  closing <- unmarked("Ce qui manque \u00e0 l'\u00e9tude")
  style <- unmarked("soutenu, au pass\u00e9")
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(ok) || isTRUE(l10n_info()[["UTF-8"]]), "cannot switch to the C locale here")
  res <- tryCatch({
    cm <- quiet(gr_claims(tab, question = q, client = cl))
    o <- quiet(gr_outline(cm, client = cl, closing = closing))
    g <- gr_gaps(cm)
    s <- quiet(gr_synthesise(tab, outline = o, question = q, client = cl, claims = cm,
                             style = style, gaps = g))
    # Written from rows, the section's heading (from the model, labelled) is
    # in the system prompt beside the register.
    s2 <- quiet(gr_synthesise(tab, outline = o[1:2], question = q, client = cl, style = style))
    gl <- readgpt:::render_gaps(g)
    list(cm = cm, o = o, s = s, s2 = s2, gl = gl, enc = Encoding(cm$question))
  }, error = function(e) e)
  suppressWarnings(Sys.setlocale("LC_CTYPE", old))
  expect_false(inherits(res, "error"),
               label = if (inherits(res, "error")) conditionMessage(res) else "the run")
  skip_if(inherits(res, "error"))
  sent <- unlist(lapply(env$seen, function(m) vapply(m, function(x) as.character(x$content), "")))
  expect_gt(length(sent), 5L)
  escaped <- grepl("<c3>|<e0>", sent, useBytes = TRUE)
  expect_false(any(escaped), label = paste(substr(sent[escaped], 1, 120), collapse = " | "))
  expect_true(any(grepl("Qu'est-ce qu'une \u00e9tude", sent, fixed = TRUE)))
  expect_true(any(grepl("Register: soutenu, au pass\u00e9", sent, fixed = TRUE)))
  expect_true(any(grepl("Montr\u00e9al", sent, fixed = TRUE)))
  expect_identical(res$enc, "UTF-8")
  expect_identical(res$s$sections$section, c("Essais", "Enqu\u00eates", "Ce qui manque \u00e0 l'\u00e9tude"))
  expect_false(grepl("<c3>", res$s$text, fixed = TRUE))
  expect_false(grepl("<c3>", res$gl, fixed = TRUE))
})

test_that("a gap line quoting the table is not escaped beside one quoting a claim", {
  # Before: render_gaps() pasted an unlabelled detail read from the table with
  # a labelled one from a claim, and the first came out "Montr<c3><a9>al".
  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!nzchar(old), "no LC_CTYPE to restore")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", old)))
  g <- data.frame(kind = c("no variation", "unreplicated"), dimension = c("lieu", "claim 1"),
                  detail = c(unmarked("every study reports 'Montr\u00e9al'"),
                             "Les essais montrent un b\u00e9n\u00e9fice."),
                  n = c(4L, 1L), stringsAsFactors = FALSE)
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(ok) || isTRUE(l10n_info()[["UTF-8"]]), "cannot switch to the C locale here")
  out <- readgpt:::render_gaps(g)
  suppressWarnings(Sys.setlocale("LC_CTYPE", old))
  expect_false(grepl("<c3>", out, fixed = TRUE, useBytes = TRUE))
  expect_true(grepl("Montr\u00e9al", out, fixed = TRUE))
})

test_that("under a C locale an accented category declared in gr_fields() matches the table", {
  # Before: the category typed in gr_fields() was unlabelled and the value the
  # model returned labelled, so "enquete" with its accent never matched
  # itself: gr_gaps() reported it as declared and unstudied, and its pairing
  # with 'ville' as unstudied, though study 1 is exactly that.
  tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok", duplicate_of = NA_character_,
                    n_filled = 1L, n_unverified = 0L, conflicts = NA_character_,
                    design = c("enquête", "essai"), lieu = c("ville", "campagne"),
                    stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(messages, params) paste0(
    '{"claims":[{"claim":"x","kind":"finding","supported_by":[1,2],',
    '"contradicted_by":[],"moderator":null,"scope":null}]}'))
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  flds <- gr_fields(design = gr_field("d", type = "enum", values = unmarked(c("enquête", "essai"))),
                    lieu = gr_field("l", type = "enum", values = c("ville", "campagne")))
  want <- c("design = 'enquête' with lieu = 'campagne'", "design = 'essai' with lieu = 'ville'")
  expect_identical(as.data.frame(gr_gaps(cm, extraction = flds))$detail, want)
  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!nzchar(old), "no LC_CTYPE to restore")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", old)))
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(ok) || isTRUE(l10n_info()[["UTF-8"]]), "cannot switch to the C locale here")
  g <- gr_gaps(cm, extraction = flds)
  suppressWarnings(Sys.setlocale("LC_CTYPE", old))
  expect_false("declared but unstudied" %in% g$kind)
  expect_identical(as.data.frame(g)$detail, want)
})

# ---------------------------------------------------------------------------
# H1 (synthesis): gr_outline() says which claims it was derived from.
# ---------------------------------------------------------------------------

test_that("gr_outline() stamps its claims, and an outline from other claims is refused", {
  # Before: gr_outline() stamped nothing, so gr_synthesise() had nothing to
  # compare, and an outline from a re-run whose claims came back in another
  # order was accepted: each section argued the other's claims.
  cl <- r7_client()
  cm <- quiet(gr_claims(r7_table(), question = "Q?", client = cl))
  o <- quiet(gr_outline(cm, client = cl))
  expect_identical(attr(o, "claims_fingerprint"), readgpt:::claims_fingerprint(cm$claims))
  swapped <- r7_client(claims = sprintf('{"claims":[%s,%s]}', r7_claim("Surveys show harm.", c(2, 4)),
                                         r7_claim("Trials work.", c(1, 3))))
  cm2 <- quiet(gr_claims(r7_table(), question = "Q?", client = swapped))
  expect_error(quiet(gr_synthesise(r7_table(), outline = o, question = "Q?", client = cl,
                                   claims = cm2)), class = "gr_claims_mismatch")
  # The claims it was made from pass, and so does an outline gr_outline()
  # made when its call failed and every claim went to one section.
  expect_s3_class(quiet(gr_synthesise(r7_table(), outline = o, question = "Q?", client = cl,
                                      claims = cm)), "gr_synthesis")
  broken <- gr_mock_client(function(messages, params) stop("down"))
  o2 <- quiet(gr_outline(cm, client = broken))
  expect_identical(attr(o2, "claims_fingerprint"), readgpt:::claims_fingerprint(cm$claims))
  expect_error(quiet(gr_synthesise(r7_table(), outline = o2, question = "Q?", client = cl,
                                   claims = cm2)), class = "gr_claims_mismatch")
})

# ---------------------------------------------------------------------------
# r2-audit-report-truthfulness-05: the write-up keeps which section each claim
# was given to.
# ---------------------------------------------------------------------------

test_that("a synthesis from claims records which section each claim went to, and the audit shows it", {
  # Before: outline_vector() dropped the assignment and nothing kept it, so
  # the audit report left the claims table's section column out, saying the
  # assignment was not recorded.
  cl <- r7_client()
  cm <- quiet(gr_claims(r7_table(), question = "Q?", client = cl))
  o <- quiet(gr_outline(cm, client = cl))
  s <- quiet(gr_synthesise(r7_table(), outline = o, question = "Q?", client = cl, claims = cm))
  expect_identical(s$claim_sections,
                   data.frame(section = c("Trials", "Surveys"), claim_id = 1:2,
                              stringsAsFactors = FALSE))
  html <- paste(readgpt:::audit_claims(s), collapse = "\n")
  expect_false(grepl("was not recorded with this synthesis", html, fixed = TRUE))
  expect_match(html, "<th>section</th>", fixed = TRUE)
  expect_match(html, "<td>Surveys</td>", fixed = TRUE)
  # Without claims there is no assignment to keep, whatever the outline carried.
  s0 <- quiet(gr_synthesise(r7_table(), outline = o, question = "Q?", client = cl))
  expect_null(s0$claim_sections)
  expect_true("claim_sections" %in% names(s0))
})

# ---------------------------------------------------------------------------
# r2-audit-report-truthfulness-04: a section row says why it is partial.
# ---------------------------------------------------------------------------

# Forty studies of about 130 tokens each: several batches on a small model.
r7_big <- function(n = 40L) {
  data.frame(document = sprintf("d%02d.pdf", seq_len(n)), document_id = sprintf("h%02d", seq_len(n)),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_,
             finding = paste(rep("The intervention reduced the outcome modestly in adults.", 9),
                             collapse = " "), stringsAsFactors = FALSE)
}

r7_sent <- function(m) paste(vapply(m, function(x) as.character(x$content), ""), collapse = " ")

# A batch draft cites the first study it was sent; a merge cites what the drafts did.
r7_batch_reply <- function(m) {
  txt <- r7_sent(m)
  if (grepl("<draft", txt, fixed = TRUE)) {
    ids <- unique(regmatches(txt, gregexpr("\\[study [0-9]+\\]", txt))[[1]])
    return(paste0("Merged: a benefit ", paste(ids, collapse = " "), "."))
  }
  ids <- regmatches(txt, gregexpr("(?<=\\[study )[0-9]+(?=\\]\n)", txt, perl = TRUE))[[1]]
  sprintf("Draft: a benefit [study %s].", ids[1])
}

test_that("a section row counts its failed, unsent and unmerged batches, and the audit names them", {
  # Before: synth_section() counted them for its warnings and dropped them, so
  # the audit report could only say the section was "marked partial".
  local_registries()
  gr_register_model("r7-small", context_window = 3000, max_output = 400,
                    input_usd = 0, output_usd = 0)
  run <- function(cl) r7_warns(gr_synthesise(r7_big(), outline = c(Findings = "what"),
                                             question = "Q?", client = cl, model = "r7-small",
                                             max_section_tokens = 300))
  audit <- function(s) paste(readgpt:::audit_synthesis(s), collapse = "\n")

  # A clean run records zeroes, not NA.
  r <- run(gr_mock_client(function(m, p) r7_batch_reply(m)))
  sec <- r$value$sections
  expect_true(all(c("lost_batches", "capped_batches", "merge_failed") %in% names(sec)))
  expect_identical(sec$lost_batches, 0L)
  expect_identical(sec$capped_batches, 0L)
  expect_identical(sec$merge_failed, FALSE)

  # One batch's call fails.
  r <- run(gr_mock_client(function(m, p) {
    if (!grepl("<draft", r7_sent(m), fixed = TRUE) && grepl("[study 1]\n", r7_sent(m), fixed = TRUE)) {
      stop("batch endpoint down")
    }
    r7_batch_reply(m)
  }))
  expect_true("gr_synth_batch_failed" %in% r$classes)
  sec <- r$value$sections
  expect_identical(sec$lost_batches, 1L)
  expect_identical(sec$capped_batches, 0L)
  expect_identical(sec$merge_failed, FALSE)
  html <- audit(r$value)
  expect_match(html, "1 batch(es) of studies failed", fixed = TRUE)
  expect_false(grepl("This section is marked partial:", html, fixed = TRUE))

  # The merge fails.
  r <- run(gr_mock_client(function(m, p) {
    if (grepl("<draft", r7_sent(m), fixed = TRUE)) stop("merge endpoint down")
    r7_batch_reply(m)
  }))
  expect_true("gr_synth_merge_failed" %in% r$classes)
  sec <- r$value$sections
  expect_identical(sec$merge_failed, TRUE)
  expect_identical(sec$lost_batches, 0L)
  expect_match(audit(r$value), "drafted but not merged", fixed = TRUE)

  # The run's ceiling stops the batches.
  gr_options(max_calls = 2L)
  r <- run(gr_mock_client(function(m, p) r7_batch_reply(m)))
  expect_true("gr_synth_capped" %in% r$classes)
  sec <- r$value$sections
  expect_gt(sec$capped_batches, 0L)
  expect_identical(sec$lost_batches, 0L)
  expect_match(audit(r$value), "batch(es) of studies were not sent", fixed = TRUE)

  # A section that could not be sent at all carries the columns too, so the
  # rows still bind.
  u <- suppressWarnings(readgpt:::synth_unwritten("S", "b", 1:2, "too large"))
  expect_identical(u$row$lost_batches, 0L)
  expect_identical(u$row$merge_failed, FALSE)
})

# ---------------------------------------------------------------------------
# synthesis-09: claims drawn with a column the write-up withholds.
# ---------------------------------------------------------------------------

test_that("claims drawn with a column the write-up withholds as bibliographic are warned about", {
  # Before: gr_claims() without `bib` showed `first_author` to the claims
  # model; gr_synthesise(bib =) withheld it from the studies, and the claims,
  # which may name it, went into the prompts without a word.
  tab <- r7_table()
  tab$first_author <- c("Smith", "Okafor", "Garcia", "Chen")
  cl <- r7_client()
  bib <- list(authors = "first_author")
  o_of <- function(cm) quiet(gr_outline(cm, client = cl))
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  r <- r7_warns(gr_synthesise(tab, outline = o_of(cm), question = "Q?", client = cl,
                              claims = cm, bib = bib))
  expect_true("gr_claims_bib_mismatch" %in% r$classes)
  expect_match(paste(r$messages, collapse = " "), "'first_author'", fixed = TRUE)
  expect_s3_class(r$value, "gr_synthesis")

  # The same `bib` on both sides, or the defaults on both, say nothing.
  cmb <- quiet(gr_claims(tab, question = "Q?", client = cl, bib = bib))
  r <- r7_warns(gr_synthesise(tab, outline = o_of(cmb), question = "Q?", client = cl,
                              claims = cmb, bib = bib))
  expect_false("gr_claims_bib_mismatch" %in% r$classes)
  r <- r7_warns(gr_synthesise(tab, outline = o_of(cm), question = "Q?", client = cl, claims = cm))
  expect_false("gr_claims_bib_mismatch" %in% r$classes)
  # Withholding less in the write-up than in the claims is no leak.
  r <- r7_warns(gr_synthesise(tab, outline = o_of(cmb), question = "Q?", client = cl,
                              claims = cmb))
  expect_false("gr_claims_bib_mismatch" %in% r$classes)

  # A claims table saved before it recorded what it withheld withheld the
  # conventional names.
  old <- cm
  old$hidden <- NULL
  r <- r7_warns(gr_synthesise(tab, outline = o_of(old), question = "Q?", client = cl,
                              claims = old, bib = bib))
  expect_true("gr_claims_bib_mismatch" %in% r$classes)
  tab2 <- tab
  names(tab2)[names(tab2) == "first_author"] <- "authors"
  cm2 <- quiet(gr_claims(tab2, question = "Q?", client = cl))
  cm2$hidden <- NULL
  r <- r7_warns(gr_synthesise(tab2, outline = o_of(cm2), question = "Q?", client = cl,
                              claims = cm2))
  expect_false("gr_claims_bib_mismatch" %in% r$classes)
})

# ---------------------------------------------------------------------------
# synthesis-06, r2-non-latin-text-pipeline-04 and synthesis-09: the help says
# what the code does.
# ---------------------------------------------------------------------------

test_that("the gr_synthesise() help lists every reason a revision is discarded or not sent", {
  f <- r7_source("R", "synthesise.R")
  skip_if(is.null(f), "the package source is not available")
  src <- readLines(f, warn = FALSE)
  doc <- gsub("\\s+", " ", paste(sub("^#' ?", "", src[startsWith(src, "#'")]), collapse = " "))
  # Before: changed the citations, arrived truncated, or strengthened a claim.
  expect_match(doc, "cited a study more often than the draft did", fixed = TRUE)
  expect_match(doc, "took the citation marker off a claim sentence that stays", fixed = TRUE)
  expect_match(doc, "wrote a citation the check cannot read", fixed = TRUE)
  expect_match(doc, "`gr_coherence_rejected`", fixed = TRUE)
  # Before: nothing said a draft not in English is never revised.
  expect_match(doc, "`gr_revision_unguarded`", fixed = TRUE)
  expect_match(doc, "reported with `ran = FALSE`", fixed = TRUE)
  expect_match(doc, "a very short draft can be misjudged", fixed = TRUE)
  # Before: nothing said to pass the same `bib` along.
  expect_match(doc, "Pass the same `bib` to [gr_claims()] and [gr_gaps()]", fixed = TRUE)
  expect_match(doc, "`gr_claims_bib_mismatch`", fixed = TRUE)
  expect_match(doc, "\\item{`claim_sections`}", fixed = TRUE)
  expect_match(doc, "`lost_batches`", fixed = TRUE)

  f <- r7_source("R", "claims.R")
  skip_if(is.null(f), "the package source is not available")
  src <- readLines(f, warn = FALSE)
  doc <- gsub("\\s+", " ", paste(sub("^#' ?", "", src[startsWith(src, "#'")]), collapse = " "))
  expect_match(doc, "`attr(, \"claims_fingerprint\")`", fixed = TRUE)
})
