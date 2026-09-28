# test-review5-synthesis.R -- the fifth pass over the write-up and its citations.
#
# Each test failed on the code before its fix, and says what that code did:
# an honest citation left as a marker because a revision deleted a fabricated
# one, a citation the batch merge invented rendered as a fact, a list cut off
# with an ellipsis cited as a two-author paper, "Brunet AL" cited as "Brun et
# al.", surname-first names with a degree split into two authors, surnames in
# capitals printed as given names, and joint organisation authors read as
# initials.

r5s_table <- function() {
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

r5s_claim <- function(text, ids) {
  sprintf(paste0('{"claim":"%s","kind":"finding","supported_by":[%s],"contradicted_by":[],',
                 '"moderator":null,"scope":null}'), text, paste(ids, collapse = ","))
}

# Claims -> outline -> synthesise. Claim 1 rests on study 1 and goes to the
# first section; claim 2 rests on `second` and goes to the second. `write`
# gives each section's text, and `revise` turns the draft a revision pass
# receives into the text it returns.
r5s_client <- function(write, second = 3L, revise = identity) {
  heads <- names(write)
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    last <- messages[[length(messages)]]$content
    all_txt <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                     collapse = " ")
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      return(sprintf('{"claims":[%s,%s]}', r5s_claim("First.", 1), r5s_claim("Second.", second)))
    }
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return(sprintf(paste0('{"sections":[{"heading":"%s","brief":"a","claims":[1],"rationale":null},',
                            '{"heading":"%s","brief":"b","claims":[2],"rationale":null}]}'),
                     heads[1], heads[2]))
    }
    if (grepl("<draft>", last, fixed = TRUE)) {
      d <- sub("(?s)^.*<draft>\n", "", last, perl = TRUE)
      d <- sub("(?s)\n</draft>.*$", "", d, perl = TRUE)
      if (is.function(revise)) return(revise(d))
      # A list names each pass by the opening of its system prompt.
      for (p in names(revise)) if (startsWith(sys, p)) return(revise[[p]](d))
      return(d)
    }
    for (h in heads) if (grepl(paste0("Section: ", h), all_txt, fixed = TRUE)) return(write[[h]])
    "Nothing is missing."
  })
}

r5s_synth <- function(cl, ...) {
  tab <- r5s_table()
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  o <- quiet(gr_outline(cm, client = cl))
  quiet(gr_synthesise(tab, outline = o, question = "Q?", client = cl, claims = cm, ...))
}

# ---------------------------------------------------------------------------
# synthesis-2 / synthesis-02: a revision that deletes the fabricated citation
# leaves the honest one alone.
# ---------------------------------------------------------------------------

test_that("a cut that deletes the fabricated citation still renders the honest one", {
  # Before: Trials cited study 3 without being given it, Surveys cited it
  # honestly, and the cut pass deleted Trials' clause. The drop in Trials'
  # count was read as a move, so the honest Surveys citation came out
  # "Surveys differed (Garcia, 2020) but one agreed [study 3]." with Lee &
  # Petrov missing from the references, and nothing flagged.
  write <- c(Trials = "Trials found a benefit [study 1], also seen elsewhere [study 3].",
             Surveys = "Surveys differed [study 2] but one agreed [study 3].")
  cut <- function(d) sub(", also seen elsewhere [study 3]", "", d, fixed = TRUE)
  cl <- r5s_client(write, second = c(2, 3), revise = cut)
  s <- r5s_synth(cl, cite_style = "author-year", coherence = "cut")
  expect_true(s$coherence$kept)
  expect_match(s$text, "Surveys differed (Garcia, 2020) but one agreed (Lee & Petrov, 2021).",
               fixed = TRUE)
  expect_true(any(grepl("Lee, K. and Petrov", s$references, fixed = TRUE)))
  expect_identical(s$unrendered, integer(0))
  expect_false(any(grepl("cited honestly", capture.output(print(s)), fixed = TRUE)))
  n <- r5s_synth(cl, cite_style = "numeric", coherence = "cut")
  expect_match(n$text, "Surveys differed (2) but one agreed (3).", fixed = TRUE)
  expect_true(any(grepl("^3\\. Lee", n$references)))
  # A combined marker in the honest section is rendered whole, not left whole.
  write2 <- c(Trials = write[["Trials"]], Surveys = "Surveys differed [study 2; study 3].")
  s2 <- r5s_synth(r5s_client(write2, second = c(2, 3), revise = cut),
                  cite_style = "author-year", coherence = "cut")
  expect_match(s2$text, "Surveys differed (Garcia, 2020; Lee & Petrov, 2021).", fixed = TRUE)
})

test_that("a reported clause moved into an honest sentence stays a marker, and is disclosed", {
  # The cut pass removes Trials' fabricated clause and writes it into the
  # Replication sentence, whose marker count does not change. Rendering there
  # would print the fabricated 40% as Lee & Petrov's finding.
  write <- c(Trials = "A benefit [study 1], with a 40% cut in deaths [study 3].",
             Replication = "It replicated [study 3].")
  swap <- function(d) {
    d <- sub(", with a 40% cut in deaths [study 3]", "", d, fixed = TRUE)
    sub("It replicated [study 3].", "It replicated, with a 40% cut in deaths [study 3].", d,
        fixed = TRUE)
  }
  cl <- r5s_client(write, revise = swap)
  s <- r5s_synth(cl, cite_style = "author-year", coherence = "cut")
  expect_true(s$coherence$kept)
  expect_match(s$text, "It replicated, with a 40% cut in deaths [study 3].", fixed = TRUE)
  expect_false(grepl("Lee & Petrov", s$text, fixed = TRUE))
  # The honest citation that may be among them is not lost in silence.
  expect_identical(s$unrendered, 3L)
  expect_true(any(grepl("study 3: cited honestly, but left as a marker",
                        capture.output(print(s)), fixed = TRUE)))
  tab <- r5s_table()
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  o <- quiet(gr_outline(cm, client = cl))
  expect_warning(suppressMessages(gr_synthesise(tab, outline = o, question = "Q?", client = cl,
                                                claims = cm, cite_style = "author-year",
                                                coherence = "cut")),
                 class = "gr_synth_unrendered")
})

test_that("reworded sentences are told apart by what they say, and traded claims are left", {
  write <- c(Trials = "A benefit [study 1], with a 40% cut in deaths [study 3].",
             Replication = "It replicated [study 3].")
  run <- function(f) r5s_synth(r5s_client(write, revise = f), cite_style = "author-year",
                               coherence = "register")
  # A register pass that polishes both sentences: each still reads as its own.
  both <- run(function(d) {
    d <- sub("A benefit", "There was a benefit", d, fixed = TRUE)
    sub("It replicated [study 3].", "The result was replicated [study 3].", d, fixed = TRUE)
  })
  expect_match(both$text, "The result was replicated (Lee & Petrov, 2021).", fixed = TRUE)
  expect_match(both$text, "with a 40% cut in deaths [study 3].", fixed = TRUE)
  # Two sections trading claims, both reworded, marker counts unchanged. The
  # count-based rule rendered "It replicated, with a 40% fall in deaths
  # (Lee & Petrov, 2021)."
  trade <- run(function(d) {
    d <- sub("A benefit [study 1], with a 40% cut in deaths [study 3].",
             "A benefit [study 1]. It was replicated [study 3].", d, fixed = TRUE)
    sub("It replicated [study 3].", "It replicated, with a 40% fall in deaths [study 3].", d,
        fixed = TRUE)
  })
  expect_match(trade$text, "It replicated, with a 40% fall in deaths [study 3].", fixed = TRUE)
  expect_false(grepl("Lee & Petrov", trade$text, fixed = TRUE))
  # The same trade, word for word: the fabricated sentence is left wherever it
  # went, the honest one rendered wherever it went. (A pure trade: leaving a
  # second "A benefit [study 1]." behind cites study 1 once more than the draft
  # did, and a pass that does that is now discarded.)
  verbatim <- run(function(d) {
    d <- sub("A benefit [study 1], with a 40% cut in deaths [study 3].",
             "SWAP", d, fixed = TRUE)
    d <- sub("It replicated [study 3].", "A benefit [study 1], with a 40% cut in deaths [study 3].",
             d, fixed = TRUE)
    sub("SWAP", "It replicated [study 3].", d, fixed = TRUE)
  })
  expect_match(verbatim$text, "## Replication\n\nA benefit (Smith & Okafor, 2019), with a 40% cut in deaths [study 3].",
               fixed = TRUE)
  expect_match(verbatim$text, "It replicated (Lee & Petrov, 2021).", fixed = TRUE)
})

test_that("each kept pass is followed in turn, so a cut then a polish keeps the honest citation", {
  # coherence = TRUE: the cut pass deletes Trials' fabricated clause, then the
  # register pass polishes Replication's honest sentence. Compared draft to
  # final, that is a reported marker gone and an honest sentence reworded,
  # which a pass writing the clause into it would also give; before, the
  # honest citation came out "The result was replicated [study 3]." with Lee &
  # Petrov dropped from the references.
  write <- c(Trials = "A benefit [study 1], with a 40% cut in deaths [study 3].",
             Replication = "It replicated [study 3].")
  cut <- function(d) sub(", with a 40% cut in deaths [study 3]", "", d, fixed = TRUE)
  polish <- function(d) sub("It replicated [study 3].", "The result was replicated [study 3].", d,
                            fixed = TRUE)
  s <- r5s_synth(r5s_client(write, revise = list("You cut" = cut, "You polish" = polish)),
                 cite_style = "author-year", coherence = TRUE)
  expect_identical(s$coherence$kept, c(TRUE, TRUE, TRUE))
  expect_match(s$text, "The result was replicated (Lee & Petrov, 2021).", fixed = TRUE)
  expect_true(any(grepl("Lee, K. and Petrov", s$references, fixed = TRUE)))
  expect_identical(s$unrendered, integer(0))
  # The reported words turning up in the honest sentence a pass later are
  # still the reported words.
  back <- function(d) sub("It replicated [study 3].", "It replicated, with a 40% cut in deaths [study 3].",
                          d, fixed = TRUE)
  s2 <- r5s_synth(r5s_client(write, revise = list("You cut" = cut, "You polish" = back)),
                  cite_style = "author-year", coherence = TRUE)
  expect_match(s2$text, "It replicated, with a 40% cut in deaths [study 3].", fixed = TRUE)
  expect_identical(s2$unrendered, 3L)
})

test_that("sentence_spans() hands back every character it was given", {
  f <- readgpt:::sentence_spans
  x <- paste0("## A\n\nOne claim [study 3 p. 4]. Two, e.g. this [study 1]!  Three?\n",
              "### Sub\n\nFour [studies 1-3].\n\n")
  sp <- f(x)
  expect_identical(paste(sp, collapse = ""), x)
  expect_true("One claim [study 3 p. 4]. " %in% sp)
  expect_true("Two, e.g. this [study 1]!  " %in% sp)
  expect_identical(f(""), character(0))
  expect_identical(f("No stop at all"), "No stop at all")
})

# ---------------------------------------------------------------------------
# synthesis-3 / synthesis-5: the merge of batch drafts is checked too.
# ---------------------------------------------------------------------------

test_that("a citation the batch merge adds that no draft made is reported and left", {
  # Before: the merge, shown only the drafts, appended "Also seen in [study 7]."
  # and the section read n_unsupplied 0, partial FALSE, with "(Author07, 2007)"
  # in the text and Author07 in the reference list.
  local_registries()
  gr_register_model("r5s-small", context_window = 3000, max_output = 400,
                    input_usd = 0, output_usd = 0)
  n <- 40L
  tab <- data.frame(document = sprintf("d%02d.pdf", 1:n), document_id = sprintf("h%02d", 1:n),
                    status = "ok", duplicate_of = NA_character_, n_filled = 2L,
                    n_unverified = 0L, conflicts = NA_character_,
                    authors = sprintf("Author%02d, A.", 1:n), year = 2000L + 1:n,
                    finding = paste(rep("The intervention reduced the outcome modestly in adults.",
                                        9), collapse = " "),
                    stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(m, p) {
    txt <- paste(vapply(m, `[[`, "", "content"), collapse = " ")
    if (grepl("<draft", txt, fixed = TRUE)) {
      drafts <- trimws(regmatches(txt, gregexpr("Draft[^<]*", txt))[[1]])
      return(paste("Merged:", paste(drafts, collapse = " "), "Also seen in [study 7]."))
    }
    blk <- m[[length(m)]]$content
    ids <- as.integer(regmatches(blk, gregexpr("(?<=\\[study )[0-9]+(?=\\])", blk, perl = TRUE))[[1]])
    sprintf("Draft: similar [study %d].", ids[length(ids)])
  })
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl,
                           model = "r5s-small", max_section_tokens = 300,
                           cite_style = "author-year"))
  expect_true(any(vapply(s$trace$steps, `[[`, "", "label") == "synthesise.merge"))
  expect_identical(s$sections$n_unsupplied, 1L)
  expect_true(s$sections$partial)
  expect_match(s$text, "Also seen in [study 7].", fixed = TRUE)
  expect_match(s$text, "similar (Author16, 2016)", fixed = TRUE)
  expect_false(7L %in% s$citations$study)
  expect_false(any(grepl("Author07", s$references, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# synthesis-03: a list cut off with an ellipsis is et al.
# ---------------------------------------------------------------------------

test_that("an author list cut off with an ellipsis, or a misspelt et al., is cited as et al.", {
  # Before: the ellipsis has no letter, so it was dropped as an empty slot and
  # "Smith J, Okafor A, ..." was cited "(Smith & Okafor, 2020)"; "Smith et.
  # al." was cited "(al., 2020)".
  k <- function(a) {
    row <- data.frame(authors = a, year = "2020", stringsAsFactors = FALSE)
    readgpt:::bib_key(row, list(authors = "authors", year = "year"))
  }
  ell <- intToUtf8(0x2026)
  for (a in c("Smith J, Okafor A, ...", paste0("Smith J, Okafor A, ", ell),
              "Smith J, Okafor A, [...]", "Smith J, Okafor A, . . .",
              "John Smith, Aisha Okafor, ...", "Smith, J., Okafor, A., ...",
              paste0("J Smith, A Okafor", ell), "Smith J, ..., Zhou Y", "Smith J, ...",
              "Smith et. al.", "Smith et al.,", "Smith et al.:",
              paste0("Smith et", intToUtf8(0xA0), "al."), "Smith J, Okafor A, etc.")) {
    expect_identical(k(a), "Smith et al., 2020", info = a)
  }
  tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok", duplicate_of = NA_character_,
                    n_filled = 2L, authors = c("Smith J, Okafor A, ...", "Garcia R, Lee K"),
                    year = c(2020L, 2021L), design = "RCT", stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(messages, params) "Finding 1 [study 1]. Finding 2 [study 2].")
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl))
  expect_match(s$text, "Finding 1 (Smith et al., 2020). Finding 2 (Garcia & Lee, 2021).",
               fixed = TRUE)
})

# ---------------------------------------------------------------------------
# synthesis-6 (low): "et al." is a word of its own.
# ---------------------------------------------------------------------------

test_that("a surname ending in -et before the initials AL is not et al.", {
  # Before: "Brunet AL" was cited "(Brun et al., 2020)", "Paquet AL" "(Paqu et
  # al.)" and "Ahmet Al" "(Ahm et al.)".
  f <- function(x) {
    s <- readgpt:::bib_surnames(x)
    paste0(paste(s, collapse = "|"), if (isTRUE(attr(s, "et_al"))) " et al." else "")
  }
  expect_identical(f("Brunet AL"), "Brunet")
  expect_identical(f("Paquet AL"), "Paquet")
  expect_identical(f("Gillet AL."), "Gillet")
  expect_identical(f("Smith J, Brunet AL"), "Smith|Brunet")
  expect_identical(f("Ahmet Al"), "Al")
  expect_identical(f("Smith ET AL."), "Smith et al.")
  expect_identical(f("Smith, et al."), "Smith et al.")
})

# ---------------------------------------------------------------------------
# synthesis-6: surname-first names with a degree, and what a degree leaves.
# ---------------------------------------------------------------------------

test_that("a surname-first name with a degree falls back to markers, not two authors", {
  # Before: "Smith, John, PhD" was cited "(Smith & John, 2019)", "Okafor,
  # Aisha, DPhil" "(Okafor & Aisha)", and "Smith, John (PhD)" "(Smith & (),
  # 2019)"; 41fe931 fell back to markers.
  f <- readgpt:::bib_surnames
  for (a in c("Smith, John, PhD", "Smith, John, MD, PhD", "Okafor, Aisha, DPhil",
              "Smith, John (PhD)")) {
    expect_null(f(a), info = a)
  }
  # What a stripped degree leaves in brackets is not a surname.
  expect_identical(as.character(f("John Smith (PhD)")), "Smith")
  # A one-word name with a degree of its own is still a byline's author.
  expect_identical(as.character(f("Smith MS, Okafor PhD")), c("Smith", "Okafor"))
  # Particles belong to the surname in a given-names-first name.
  expect_identical(as.character(f("Maria de la Cruz, PhD; Jan van der Berg, MD")),
                   c("de la Cruz", "van der Berg"))
  expect_identical(as.character(f("Pieter van der Berg and Aisha Okafor")),
                   c("van der Berg", "Okafor"))
  # A table's placeholder is no author.
  for (a in c("NR", "NA", "N/A", "not reported")) expect_identical(f(a), character(0), info = a)
  tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok", duplicate_of = NA_character_,
                    n_filled = 2L, authors = c("Smith, John, PhD", "Garcia, R."),
                    year = c(2019L, 2021L), design = c("RCT", "cohort"),
                    stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(messages, params) "A trial [study 1] and a cohort [study 2] agreed.")
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl))
  expect_identical(s$cite_style, "marker")
  expect_match(s$text, "A trial [study 1] and a cohort [study 2] agreed.", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# synthesis-2 (low): surnames printed in capitals in a byline.
# ---------------------------------------------------------------------------

test_that("a byline's surnames in capitals are not stripped as degrees or read as initials", {
  # Before: "MA Wei, PhD; LI Jun, PhD" was cited "(Wei & Jun, 2020)" -- given
  # names -- and "Wei MA, PhD" "(Wei, 2020)": MA was stripped as a degree, and
  # "Jun LI" read as Jun with the initials L. I.
  f <- readgpt:::bib_surnames
  for (a in c("MA Wei, PhD; LI Jun, PhD", "Wei MA, PhD; Jun LI, MD", "Thanh DO, PhD; Wei MA, MSc",
              "Jun LI, PhD", "Wei MA, PhD", "Wei WANG, MD, PhD", "Van Anh DO, MSc")) {
    expect_null(f(a), info = a)
  }
  # After a comma MA and DO are degrees, and longer capitals are a surname.
  expect_identical(as.character(f("John Smith, MA, PhD")), "Smith")
  expect_identical(as.character(f("Jane Doe, DO, MPH; John Roe, PhD")), c("Doe", "Roe"))
  expect_identical(as.character(f("Jean DUPONT, MD, PhD")), "DUPONT")
  expect_identical(as.character(f("John Smith III, PhD")), "Smith")
  # A group author is not a person, whatever its capitals.
  expect_identical(as.character(f("John Smith, PhD, Aisha Okafor, PhD, and the ABC Study Group")),
                   c("Smith", "Okafor"))
})

# ---------------------------------------------------------------------------
# synthesis-4: several organisations by acronym.
# ---------------------------------------------------------------------------

test_that("joint organisation authors by acronym are cited by name", {
  # Before: "UNICEF and WHO" was cited "(UNICEF, 2020)", WHO read as the
  # initials of a surname UNICEF, and "WHO and UNICEF", "NICE; SIGN" and the
  # five-agency SOFI report sent the whole review to markers.
  f <- function(x) as.character(readgpt:::bib_surnames(x))
  expect_identical(f("UNICEF and WHO"), c("UNICEF", "WHO"))
  expect_identical(f("WHO and UNICEF"), c("WHO", "UNICEF"))
  expect_identical(f("WHO & UNICEF"), c("WHO", "UNICEF"))
  expect_identical(f("WHO, UNICEF"), c("WHO", "UNICEF"))
  expect_identical(f("NICE; SIGN"), c("NICE", "SIGN"))
  expect_identical(f("FAO, IFAD, UNICEF, WFP and WHO"), c("FAO", "IFAD", "UNICEF", "WFP", "WHO"))
  expect_identical(f("European Food Safety Authority"), "European Food Safety Authority")
  expect_identical(f("EFSA Panel on Food Additives"), "EFSA Panel on Food Additives")
  # Surnames and initials in capitals are still people, or nothing.
  expect_null(readgpt:::bib_surnames("LEE, JA"))
  expect_null(readgpt:::bib_surnames("SMITH, JA, LEE, KB"))
  expect_null(readgpt:::bib_surnames("MD and RN"))
  tab <- data.frame(document = c("a.pdf", "b.pdf", "c.pdf"), status = "ok",
                    duplicate_of = NA_character_, n_filled = 2L,
                    authors = c("Smith, J. and Okafor, A.", "UNICEF and WHO",
                                "FAO, IFAD, UNICEF, WFP and WHO"),
                    year = c(2019L, 2020L, 2021L), design = "report", stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(messages, params) "S1 [study 1], S2 [study 2], S3 [study 3].")
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl))
  expect_match(s$text, "S1 (Smith & Okafor, 2019), S2 (UNICEF & WHO, 2020), S3 (FAO et al., 2021).",
               fixed = TRUE)
})
