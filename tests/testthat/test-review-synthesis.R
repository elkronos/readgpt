# test-review-synthesis.R -- the claims layer and the write-up, where a check
# claimed more than it had checked.
#
# Every test here failed on the code before its fix. Each says what the old
# behaviour was, because the old behaviour is what a later change must not bring
# back: a citation to a study the model never saw passing as verified, a wrong
# author printed as a rendered citation, a truncated revision replacing the
# draft, a whole batch of studies lost to a reply limit, and a schema the
# default backend refuses outright.

rs_table <- function() {
  data.frame(document = paste0(letters[1:4], ".pdf"), document_id = paste0("h", 1:4),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_,
             authors = c("Smith, J. and Okafor, A.", "Garcia, M.", "Lee, K. and Petrov, D.",
                         "Okafor, B."),
             year = c(2019L, 2020L, 2021L, 2022L),
             design = c("randomised trial", "cross-sectional", "randomised trial", "qualitative"),
             finding = c("supports", "contradicts", "supports", "mixed"),
             stringsAsFactors = FALSE)
}

# One mock for the whole claims -> outline -> synthesise chain. `claims` and
# `sections` are the JSON replies; `write` maps a section heading to its prose.
rs_client <- function(claims, sections, write) {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    all_txt <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                     collapse = " ")
    if (grepl("turn a table of studies", sys, fixed = TRUE)) return(claims)
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) return(sections)
    for (h in names(write)) {
      if (grepl(paste0("Section: ", h), all_txt, fixed = TRUE)) return(write[[h]])
    }
    "Nothing more [study 1]."
  })
}

rs_claim <- function(text, ids) {
  sprintf(paste0('{"claim":"%s","kind":"finding","supported_by":[%s],"contradicted_by":[],',
                 '"moderator":null,"scope":null}'), text, paste(ids, collapse = ","))
}

# ---------------------------------------------------------------------------
# synthesis-02: a citation is checked against what the model was SHOWN.
# ---------------------------------------------------------------------------

test_that("a claims-mode section citing a study it was not given is partial, and the citation is not rendered", {
  # Before: the check compared against the whole table, so `[study 3]` -- a real
  # row the Trials section never saw -- counted as known. n_unknown 0, partial
  # FALSE, rendered "(Lee & Petrov, 2021)" and listed under References.
  tab <- rs_table()
  cl <- rs_client(
    claims = sprintf('{"claims":[%s,%s]}', rs_claim("Trials found a benefit.", 1),
                     rs_claim("Surveys disagree.", 2)),
    sections = paste0('{"sections":[{"heading":"Trials","brief":"t","claims":[1],"rationale":null},',
                      '{"heading":"Surveys","brief":"s","claims":[2],"rationale":null}]}'),
    write = c(Trials = "Trials found a benefit [study 1], confirmed elsewhere [study 3].",
              Surveys = "Surveys disagree [study 2]."))
  cm <- quiet(gr_claims(tab, question = "Does it work?", client = cl))
  o <- quiet(gr_outline(cm, client = cl))
  syn <- quiet(gr_synthesise(tab, outline = o, question = "Does it work?", client = cl,
                             claims = cm, cite_style = "author-year"))
  tr <- syn$sections[syn$sections$section == "Trials", ]
  expect_identical(tr$n_unsupplied, 1L)
  expect_identical(tr$n_unknown, 0L)       # the row exists; it was not supplied
  expect_identical(tr$n_cited, 1L)
  expect_true(tr$partial)
  # Left as the marker the model wrote, in the section and in the document.
  expect_match(tr$text, "confirmed elsewhere [study 3]", fixed = TRUE)
  expect_match(tr$text, "(Smith & Okafor, 2019)", fixed = TRUE)
  expect_match(syn$text, "confirmed elsewhere [study 3]", fixed = TRUE)
  expect_false(grepl("Lee & Petrov", syn$text, fixed = TRUE))
  expect_false(any(grepl("Petrov", syn$references, fixed = TRUE)))
  expect_false(3L %in% syn$citations$study)
  # The marked text keeps it, so re-running the check finds it again.
  expect_true(3L %in% readgpt:::cited_ids(tr$text_marked, "study"))
  expect_output(print(syn), "1 TO A STUDY IT WAS NOT GIVEN")
  # The other section is untouched.
  su <- syn$sections[syn$sections$section == "Surveys", ]
  expect_identical(su$n_unsupplied, 0L)
  expect_false(su$partial)
})

test_that("a study one section was given stays citable there when another section cites it unshown", {
  # Per section, not per study: study 3 is honest in "Replication", which was
  # shown it, and a fabrication in "Trials", which was not.
  tab <- rs_table()
  cl <- rs_client(
    claims = sprintf('{"claims":[%s,%s]}', rs_claim("Trials found a benefit.", 1),
                     rs_claim("It replicated.", 3)),
    sections = paste0('{"sections":[{"heading":"Trials","brief":"t","claims":[1],"rationale":null},',
                      '{"heading":"Replication","brief":"r","claims":[2],"rationale":null}]}'),
    write = c(Trials = "A benefit [study 1], confirmed elsewhere [study 3].",
              Replication = "It replicated [study 3]."))
  cm <- quiet(gr_claims(tab, question = "Does it work?", client = cl))
  o <- quiet(gr_outline(cm, client = cl))
  syn <- quiet(gr_synthesise(tab, outline = o, question = "Does it work?", client = cl,
                             claims = cm, cite_style = "author-year"))
  s <- syn$sections
  expect_identical(s$n_unsupplied[s$section == "Trials"], 1L)
  expect_identical(s$n_unsupplied[s$section == "Replication"], 0L)
  expect_match(syn$text, "confirmed elsewhere [study 3]", fixed = TRUE)
  expect_match(syn$text, "It replicated (Lee & Petrov, 2021)", fixed = TRUE)
  expect_true(any(grepl("Petrov", syn$references, fixed = TRUE)))
  expect_identical(syn$citations$study[syn$citations$section == "Replication"], 3L)
})

test_that("a claim may only rest on studies its own batch was shown", {
  # Before: claims_verify() checked every batch's claims against the whole
  # table, so batch 2 naming study 1 -- shown only to batch 1 -- kept it as
  # support, with nothing in $dropped.
  local_registries()
  gr_register_model("rs-tiny", context_window = 2500L, max_output = 300L,
                    input_usd = 0, output_usd = 0)
  n <- 12L
  tab <- data.frame(document = paste0("d", 1:n, ".pdf"), document_id = paste0("h", 1:n),
                    status = "ok", duplicate_of = NA_character_, n_filled = 2L,
                    n_unverified = 0L, conflicts = NA_character_,
                    notes = vapply(1:n, function(i) paste(rep(sprintf("detail%d word", i), 60),
                                                          collapse = " "), ""),
                    stringsAsFactors = FALSE)
  batch <- 0L
  cl <- gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    txt <- paste(vapply(messages, function(m) as.character(m$content), character(1)), collapse = " ")
    ids <- as.integer(regmatches(txt, gregexpr("(?<=\\[study )[0-9]+", txt, perl = TRUE))[[1]])
    batch <<- batch + 1L
    sprintf('{"claims":[%s]}', rs_claim(sprintf("Batch %d claim.", batch),
                                        if (batch == 1L) ids[1] else c(ids[1], 1L)))
  })
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl, model = "rs-tiny",
                        max_claim_tokens = 300L))
  expect_gt(batch, 1L)
  # Study 1 supports only the claim drawn from the batch that was shown it.
  expect_identical(unique(cm$support$claim_id[cm$support$study == 1L]),
                   cm$claims$claim_id[cm$claims$claim == "Batch 1 claim."])
  expect_true(any(grepl("not shown to the batch", cm$dropped$reason, fixed = TRUE)))
  expect_true(all(grepl("not shown to its batch", cm$claims$note[cm$claims$claim != "Batch 1 claim."],
                        fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# synthesis-03: an author list read wrongly falls back to markers.
# ---------------------------------------------------------------------------

test_that("common author-list formats give the right surnames, or none", {
  f <- readgpt:::bib_surnames
  # Vancouver / PubMed: the initials were taken as the surname.
  expect_identical(f("Smith JA, Okafor AB"), c("Smith", "Okafor"))
  expect_identical(f("Garcia R, Lee K, Wu X"), c("Garcia", "Lee", "Wu"))
  expect_identical(f("Smith J.A. and Okafor A.B."), c("Smith", "Okafor"))
  expect_identical(f("Li X, Wang Y"), c("Li", "Wang"))
  expect_identical(f("van der Berg P, Smith J"), c("van der Berg", "Smith"))
  # ", &" before the last author dropped that author.
  expect_identical(f("Smith, J., Okafor, A., & Lee, K."), c("Smith", "Okafor", "Lee"))
  expect_identical(f("Smith, J., & Okafor, A."), c("Smith", "Okafor"))
  # A four-letter first name sent "First Last" lists down the initials branch.
  expect_identical(f("John Smith, Mary Jones"), c("Smith", "Jones"))
  expect_identical(f("Anne Smith, Bea Okafor, and Carl Lee"), c("Smith", "Okafor", "Lee"))
  # An organisation is one author, kept whole.
  expect_identical(f("World Health Organization"), "World Health Organization")
  # Single authors with full given names, as records.R writes them.
  expect_identical(f("Chen, Wei"), "Chen")
  expect_identical(f("Smith, John and Okafor, Aisha"), c("Smith", "Okafor"))
  # What cannot be read confidently is NULL, not a guess.
  expect_null(f("Smith, Jones, Lee"))
  expect_null(f("Smith J, WHO Collaborating Group"))
  expect_null(f("J."))
})

test_that("a review cites PubMed-style author lists by the right names", {
  # Before: "A trial (JA & AB, 2019) and a cohort (Garcia & Lee K, 2022)".
  tab <- data.frame(document = c("a.pdf", "b.pdf", "c.pdf"), status = "ok",
                    duplicate_of = NA_character_, n_filled = 2L,
                    authors = c("Smith JA, Okafor AB", "Garcia R, Lee K, Wu X",
                                "Smith, J., Okafor, A., & Lee, K."),
                    year = c(2019L, 2022L, 2020L), design = c("RCT", "cohort", "RCT"),
                    stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(messages, params) "A trial [study 1], a cohort [study 2], a third [study 3].")
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl))
  expect_identical(s$cite_style, "author-year")
  expect_match(s$text, "A trial (Smith & Okafor, 2019)", fixed = TRUE)
  expect_match(s$text, "a cohort (Garcia et al., 2022)", fixed = TRUE)
  expect_match(s$text, "a third (Smith et al., 2020)", fixed = TRUE)
})

test_that("an author list that cannot be read makes the run fall back to markers", {
  tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok",
                    duplicate_of = NA_character_, n_filled = 2L,
                    authors = c("Smith, J.", "Smith, Jones, Lee"), year = c(2019L, 2020L),
                    design = c("RCT", "cohort"), stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(messages, params) "Two studies [study 1] [study 2].")
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl))
  expect_identical(s$cite_style, "marker")
  expect_match(s$text, "[study 1] [study 2]", fixed = TRUE)
  expect_warning(suppressMessages(
    gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl,
                  cite_style = "author-year")),
    class = "gr_cite_unresolvable")
})

# ---------------------------------------------------------------------------
# synthesis-04: every provider's "cut off" discards a revision.
# ---------------------------------------------------------------------------

test_that("a revision cut off at the limit is discarded, whatever the provider calls it", {
  # Before: only "length" was recognised. The default Responses API reports
  # "incomplete", so the cut-off revision replaced the draft.
  draft_secs <- c(Findings = "Three small trials suggest a modest benefit [study 1] [study 2].",
                  Gaps = "No study followed people beyond a year [study 1].")
  cut <- paste0("## Findings\n\nThree small trials suggest a modest benefit [study 1] [study 2].",
                "\n\n## Gaps\n\nNo study")
  tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok", duplicate_of = NA_character_,
                    n_filled = 1L, finding = c("x", "y"), stringsAsFactors = FALSE)
  for (fr in c("incomplete", "max_tokens", "max_output_tokens", "MAX_TOKENS", "length")) {
    cl <- gr_mock_client(function(messages, params) {
      sys <- messages[[1]]$content
      if (grepl("cut a finished review", sys, fixed = TRUE)) {
        r <- gr_result_for_test(TRUE, text = cut)
        r$finish_reason <- fr
        return(r)
      }
      h <- sub(".*write the '([^']+)'.*", "\\1", sys)
      draft_secs[[h]]
    })
    syn <- NULL
    expect_warning(syn <- suppressMessages(
      gr_synthesise(tab, outline = c(Findings = "f", Gaps = "g"), question = "Q?", client = cl,
                    coherence = "cut", cite_style = "marker")),
      class = "gr_coherence_truncated")
    expect_false(syn$coherence$kept, label = fr)
    expect_identical(syn$coherence$reason, "revision truncated", label = fr)
    expect_match(syn$text_marked, "beyond a year [study 1].", fixed = TRUE)
    expect_identical(syn$text, syn$draft, label = fr)
  }
})

# ---------------------------------------------------------------------------
# r3-synthesis-layer-call-sizing-01: batches are sized by the reply too.
# ---------------------------------------------------------------------------

rs_many <- function(n) {
  data.frame(document = sprintf("d%03d.pdf", seq_len(n)), document_id = sprintf("h%03d", seq_len(n)),
             status = "ok", duplicate_of = NA_character_, n_filled = 3L, n_unverified = 0L,
             conflicts = NA_character_,
             design = rep(c("randomised trial", "cohort", "survey"), length.out = n),
             finding = rep(c("supports", "no difference", "contradicts"), length.out = n),
             stringsAsFactors = FALSE)
}

# Answers like a model doing its job -- a claim per three studies, every study
# named -- and is cut off exactly as the API cuts it when the reply is longer
# than params$max_output. `cut_if` forces a cut on the batches it matches.
rs_claims_model <- function(cut_if = function(ids) FALSE) {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    txt <- paste(vapply(messages, function(m) as.character(m$content), character(1)), collapse = "\n")
    ids <- as.integer(regmatches(txt, gregexpr("(?<=\\[study )[0-9]+", txt, perl = TRUE))[[1]])
    grp <- split(ids, ceiling(seq_along(ids) / 3))
    full <- sprintf('{"claims":[%s]}', paste(vapply(grp, function(g) sprintf(paste0(
      '{"claim":"Across these studies the effect held in trials and weakened in surveys of ',
      'adult outpatients.","kind":"finding","supported_by":[%s],"contradicted_by":[],',
      '"moderator":"design","scope":"Trials and cohorts of adults in primary care."}'),
      paste(g, collapse = ",")), ""), collapse = ","))
    if (cut_if(ids) || sum(gr_count_tokens(full)) > params$max_output) {
      r <- gr_result_for_test(TRUE, text = readgpt:::gr_truncate_tokens(
        full, min(params$max_output, sum(gr_count_tokens(full)) - 5L), ""))
      r$finish_reason <- "length"
      return(r)
    }
    full
  })
}

test_that("a corpus whose claims do not fit one reply is batched until they do", {
  # Before: 120 studies went in one call (the input window allowed it) with a
  # 1600-token reply. The JSON was cut off, and gr_claims() returned no claims.
  cm <- NULL
  expect_no_warning(cm <- suppressMessages(
    gr_claims(rs_many(120L), question = "Does it work?", client = rs_claims_model())))
  expect_setequal(unique(cm$support$study), 1:120)
  draws <- Filter(function(s) identical(s$label, "claims.draw"), cm$trace$steps)
  expect_gt(length(draws), 1L)
})

test_that("a batch lost to the reply limit is reported with the studies it held", {
  # Before: with several batches, a failed one vanished -- the survivors' claims
  # hid it, and no warning was raised.
  cl <- rs_claims_model(cut_if = function(ids) 1L %in% ids)
  msgs <- character(0)
  cm <- withCallingHandlers(
    suppressMessages(gr_claims(rs_many(120L), question = "Does it work?", client = cl)),
    gr_claims_batch_failed = function(w) {
      expect_s3_class(w, "gr_claims_truncated")
      msgs <<- c(msgs, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_length(msgs, 1L)
  held <- min(readgpt:::claims_per_batch(1600L), 120L)   # study 1 is in the first batch
  expect_match(msgs, sprintf("holding %d of 120 studies", held), fixed = TRUE)
  expect_match(msgs, "max_claim_tokens", fixed = TRUE)
  expect_false(1L %in% cm$support$study)
  expect_true(120L %in% cm$support$study)
})

test_that("when every reply is cut off, the warning names the limit", {
  cl <- rs_claims_model(cut_if = function(ids) TRUE)
  msg <- NULL
  cm <- withCallingHandlers(
    suppressMessages(gr_claims(rs_many(10L), question = "Does it work?", client = cl)),
    gr_no_claims = function(w) { msg <<- conditionMessage(w); invokeRestart("muffleWarning") })
  expect_equal(nrow(cm$claims), 0L)
  expect_match(msg, "cut off at the 1600-token reply limit", fixed = TRUE)
  expect_match(msg, "holding 10 of 10 studies", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# r3-synthesis-layer-call-sizing-02: strict schemas list every property.
# ---------------------------------------------------------------------------

# OpenAI's strict-mode rules, at every object level: every property in
# `required`, nothing in `required` that is not a property, and
# additionalProperties false.
strict_problems <- function(node, path = "$") {
  if (!is.list(node)) return(character(0))
  g <- function(k) node[[k, exact = TRUE]]
  out <- character(0)
  if ("object" %in% unlist(g("type")) || !is.null(g("properties"))) {
    props <- names(g("properties"))
    req <- unlist(g("required"))
    if (!identical(g("additionalProperties"), FALSE)) {
      out <- c(out, sprintf("%s: additionalProperties is not false", path))
    }
    miss <- setdiff(props, req)
    if (length(miss)) out <- c(out, sprintf("%s: not in required: %s", path, paste(miss, collapse = ", ")))
    extra <- setdiff(req, props)
    if (length(extra)) out <- c(out, sprintf("%s: required but not a property: %s", path,
                                             paste(extra, collapse = ", ")))
    for (p in props) out <- c(out, strict_problems(g("properties")[[p]], paste0(path, ".", p)))
  }
  if (!is.null(g("items"))) out <- c(out, strict_problems(g("items"), paste0(path, "[]")))
  for (k in c("anyOf", "oneOf", "allOf")) {
    for (j in seq_along(g(k))) out <- c(out, strict_problems(g(k)[[j]], sprintf("%s.%s[%d]", path, k, j)))
  }
  out
}

test_that("every schema the claims layer sends obeys the strict-mode rules", {
  # Before: the claims schema left contradicted_by, moderator and scope out of
  # `required`, and the outline schema left out rationale. Sent with strict =
  # TRUE, OpenAI refuses both with a 400 -- every claims call failed on the
  # default client, and every outline fell back to one section.
  sent <- list()
  cl <- gr_mock_client(function(messages, params) {
    if (!is.null(params$schema)) sent[[length(sent) + 1L]] <<- params$schema
    sys <- messages[[1]]$content
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return('{"sections":[{"heading":"A","brief":"b","claims":[1],"rationale":null}]}')
    }
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    txt <- paste(vapply(messages, function(m) as.character(m$content), character(1)), collapse = "\n")
    first <- regmatches(txt, regexpr("(?<=\\[study )[0-9]+", txt, perl = TRUE))
    sprintf('{"claims":[%s]}', rs_claim("It holds.", first))
  })
  # 40 studies: more than one batch, so the reconcile pass runs.
  cm <- quiet(gr_claims(rs_many(40L), question = "Does it work?", client = cl))
  invisible(quiet(gr_outline(cm, client = cl)))
  # claims (per batch), claim_groups (the reconcile pass) and outline.
  expect_gte(length(unique(sent)), 3L)
  for (s in unique(sent)) expect_identical(strict_problems(s), character(0))
  for (s in list(readgpt:::.gr_claims_schema, readgpt:::.gr_reconcile_schema,
                 readgpt:::.gr_outline_schema)) {
    expect_identical(strict_problems(s), character(0))
  }
})
