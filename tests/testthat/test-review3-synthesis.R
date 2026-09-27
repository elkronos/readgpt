# test-review3-synthesis.R -- the third pass over the write-up and its citations.
#
# Each test failed on the code before its fix, and says what that code did:
# a fabricated citation rendered as a fact once a revision was kept, a batch
# draft citing a study its batch never held, a lone acronym author sending the
# review to markers, "Smith et al." cited as one author, degrees printed as
# surnames, Vietnamese names filed after "z", and claims drawn from a third of
# the corpus presented as complete.

r3s_table <- function() {
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

r3s_claim <- function(text, ids) {
  sprintf(paste0('{"claim":"%s","kind":"finding","supported_by":[%s],"contradicted_by":[],',
                 '"moderator":null,"scope":null}'), text, paste(ids, collapse = ","))
}

# Claims -> outline -> synthesise, with the Trials section given study 1 and
# citing study 3 as well, and the Replication section given study 3. `revise`
# turns the draft a revision pass receives into the text it returns.
r3s_client <- function(revise = NULL) {
  write <- c(Trials = "A benefit [study 1], confirmed elsewhere [study 3].",
             Replication = "It replicated [study 3].")
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    last <- messages[[length(messages)]]$content
    all_txt <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                     collapse = " ")
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      return(sprintf('{"claims":[%s,%s]}', r3s_claim("Trials found a benefit.", 1),
                     r3s_claim("It replicated.", 3)))
    }
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return(paste0('{"sections":[{"heading":"Trials","brief":"t","claims":[1],"rationale":null},',
                    '{"heading":"Replication","brief":"r","claims":[2],"rationale":null}]}'))
    }
    if (grepl("<draft>", last, fixed = TRUE)) {
      d <- sub("(?s)^.*<draft>\n", "", last, perl = TRUE)
      d <- sub("(?s)\n</draft>.*$", "", d, perl = TRUE)
      return(if (is.null(revise)) d else revise(d))
    }
    for (h in names(write)) if (grepl(paste0("Section: ", h), all_txt, fixed = TRUE)) return(write[[h]])
    "Nothing is missing."
  })
}

r3s_synth <- function(cl, ...) {
  tab <- r3s_table()
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  o <- quiet(gr_outline(cm, client = cl))
  quiet(gr_synthesise(tab, outline = o, question = "Q?", client = cl, claims = cm,
                      cite_style = "author-year", ...))
}

# ---------------------------------------------------------------------------
# synthesis-2 / synthesis-02: a kept revision renders a citation the section
# was not given, when another section was given that study.
# ---------------------------------------------------------------------------

test_that("a kept revision leaves a study the section was not given as a marker", {
  # Before: with a revision kept, $text was rendered with only the studies NO
  # section was given left as markers. Replication cited study 3 honestly, so
  # the Trials section's fabricated "[study 3]" came out "(Lee & Petrov, 2021)",
  # while print() said the markers were left unrendered.
  s <- r3s_synth(r3s_client(function(d) sub("A benefit", "There was a benefit", d, fixed = TRUE)),
                 coherence = "register")
  expect_true(s$coherence$kept)
  trials <- s$sections$section == "Trials"
  expect_identical(s$sections$n_unsupplied[trials], 1L)
  expect_true(s$sections$partial[trials])
  expect_match(s$text, "There was a benefit (Smith & Okafor, 2019), confirmed elsewhere [study 3].",
               fixed = TRUE)
  # The section that WAS given study 3 still renders it, and so it is listed.
  expect_match(s$text, "It replicated (Lee & Petrov, 2021).", fixed = TRUE)
  expect_true(any(grepl("Lee, K. and Petrov", s$references, fixed = TRUE)))
  expect_identical(s$text_marked, sub("A benefit", "There was a benefit",
                                      readgpt:::synth_document(data.frame(
                                        section = s$sections$section,
                                        text = s$sections$text_marked)), fixed = TRUE))
  # All three passes, as the README's claims example runs them.
  all3 <- r3s_synth(r3s_client(function(d) sub("A benefit", "There was a benefit", d, fixed = TRUE)),
                    coherence = TRUE)
  expect_true(any(all3$coherence$kept))
  expect_match(all3$text, "confirmed elsewhere [study 3].", fixed = TRUE)
  expect_match(all3$text, "It replicated (Lee & Petrov, 2021).", fixed = TRUE)
})

test_that("a revision that moves a reported marker leaves that study as a marker everywhere", {
  # The structure pass merges Trials into Replication, so which "[study 3]" was
  # the fabricated one can no longer be told. Rendering either could print the
  # fabrication as a fact; leaving both costs an honest citation in a review
  # already marked partial, and the study leaves the reference list with it.
  merged <- function(d) {
    paste0("## Replication\n\nA benefit [study 1], confirmed elsewhere [study 3]. ",
           "It replicated [study 3].\n\n## What is missing\n\nNothing is missing.")
  }
  s <- r3s_synth(r3s_client(merged), coherence = "structure")
  expect_true(s$coherence$kept)
  expect_match(s$text, "A benefit (Smith & Okafor, 2019), confirmed elsewhere [study 3]. It replicated [study 3].",
               fixed = TRUE)
  expect_false(grepl("Lee & Petrov", s$text, fixed = TRUE))
  expect_false(any(grepl("Lee, K.", s$references, fixed = TRUE)))
  expect_true(any(grepl("Smith, J.", s$references, fixed = TRUE)))
})

test_that("render_revised() follows the sections by heading, and leaves unattributed text alone", {
  used <- data.frame(study = 1:3, authors = c("Smith, J.", "Garcia, M.", "Lee, K."),
                     year = c("2019", "2020", "2021"), document = paste0(1:3, ".pdf"),
                     stringsAsFactors = FALSE)
  keys <- readgpt:::bib_keys(used, readgpt:::bib_columns(used))
  render <- function(x, leave) readgpt:::render_citations(x, used, keys, "author-year", leave = leave)
  heads <- c("A", "B")
  marked <- c("Honest [study 1]. Invented [study 3].", "Honest [study 3].")
  uns <- list(3L, integer(0))
  # Headings kept, a sentence added before the first: the text under no heading
  # leaves the reported study, each section keeps its own exceptions.
  txt <- paste0("Intro [study 3].\n\n## A\n\nHonest [study 1]. Invented [study 3].\n\n",
                "## B\n\nHonest [study 3].")
  r <- readgpt:::render_revised(txt, heads, marked, uns, render)
  expect_identical(r$text, paste0("Intro [study 3].\n\n## A\n\nHonest (Smith, 2019). Invented ",
                                  "[study 3].\n\n## B\n\nHonest (Lee, 2021)."))
  expect_identical(r$left, integer(0))
  # Nothing reported: rendered as one text, as before.
  r0 <- readgpt:::render_revised(txt, heads, marked, list(integer(0), integer(0)), render)
  expect_identical(r0$text, render(txt, integer(0)))
  # A heading renamed: the reported marker left section A, so study 3 is left
  # everywhere, and with no marker rendered it is `left` for the reference list.
  ren <- paste0("## Trials\n\nHonest [study 1]. Invented [study 3].\n\n## B\n\nHonest [study 3].")
  r2 <- readgpt:::render_revised(ren, heads, marked, uns, render)
  expect_match(r2$text, "Honest (Smith, 2019). Invented [study 3].", fixed = TRUE)
  expect_match(r2$text, "## B\n\nHonest [study 3].", fixed = TRUE)
  expect_identical(r2$left, 3L)
})

# ---------------------------------------------------------------------------
# synthesis-3: a batch draft is checked against the studies in its batch.
# ---------------------------------------------------------------------------

r3s_batch_table <- function(n = 40L) {
  data.frame(document = sprintf("d%02d.pdf", 1:n), document_id = sprintf("h%02d", 1:n),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_, authors = sprintf("Author%02d, A.", 1:n),
             year = 2000L + 1:n,
             finding = paste(rep("The intervention reduced the outcome modestly in adults.", 9),
                             collapse = " "),
             stringsAsFactors = FALSE)
}

# Batch drafts cite their own first study; the batch holding study 1 also cites
# study 40, which it was not given. With `honest`, the batch holding study 40
# cites it too.
r3s_batch_client <- function(honest = FALSE) {
  gr_mock_client(function(m, p) {
    txt <- paste(vapply(m, `[[`, "", "content"), collapse = " ")
    if (grepl("<draft", txt, fixed = TRUE)) {
      return(paste("Merged:", paste(trimws(regmatches(txt, gregexpr("Draft[^<]*", txt))[[1]]),
                                    collapse = " ")))
    }
    blk <- m[[length(m)]]$content
    ids <- as.integer(regmatches(blk, gregexpr("(?<=\\[study )[0-9]+(?=\\])", blk, perl = TRUE))[[1]])
    if (1L %in% ids) return("Draft A: a benefit [study 1], replicated in [study 40].")
    if (honest && 40L %in% ids) return("Draft C: as found [study 40].")
    sprintf("Draft B: similar [study %d].", ids[1])
  })
}

test_that("a batch draft citing a study outside its batch is reported and left as a marker", {
  # Before: the check ran against every study in the table, so the batch that
  # held studies 1-16 citing [study 40] passed: n_unsupplied 0, partial FALSE,
  # "(Author40, 2040)" in the text and Author40 in the reference list.
  local_registries()
  gr_register_model("r3s-small", context_window = 3000, max_output = 400,
                    input_usd = 0, output_usd = 0)
  s <- quiet(gr_synthesise(r3s_batch_table(), outline = c(Findings = "what"), question = "Q?",
                           client = r3s_batch_client(), model = "r3s-small",
                           max_section_tokens = 300, cite_style = "author-year"))
  expect_true(length(s$trace$steps) > 2L)           # drafted in batches and merged
  expect_identical(s$sections$n_unsupplied, 1L)
  expect_true(s$sections$partial)
  expect_match(s$text, "replicated in [study 40]", fixed = TRUE)
  expect_match(s$text, "a benefit (Author01, 2001)", fixed = TRUE)
  expect_false(40L %in% s$citations$study)
  expect_false(any(grepl("Author40", s$references, fixed = TRUE)))
})

test_that("a study another batch of the section was given is counted, but still rendered", {
  # Once the drafts are merged, the batch that held study 40 citing it and the
  # batch that did not are one text. Leaving the marker would un-render the
  # honest citation too, so it is rendered -- and the section says so.
  local_registries()
  gr_register_model("r3s-small", context_window = 3000, max_output = 400,
                    input_usd = 0, output_usd = 0)
  s <- quiet(gr_synthesise(r3s_batch_table(), outline = c(Findings = "what"), question = "Q?",
                           client = r3s_batch_client(honest = TRUE), model = "r3s-small",
                           max_section_tokens = 300, cite_style = "author-year"))
  expect_identical(s$sections$n_unsupplied, 1L)
  expect_true(s$sections$partial)
  expect_match(s$text, "replicated in (Author40, 2040)", fixed = TRUE)
  expect_true(40L %in% s$citations$study)
})

test_that("with claims, a batch citing a study from its claims block is not reported", {
  # Every claims batch is shown the claims block, which lists every study
  # behind every claim, so citing one outside the batch is what it was asked
  # to do. A per-batch check there would leave correct citations as markers.
  local_registries()
  gr_register_model("r3s-small", context_window = 3000, max_output = 400,
                    input_usd = 0, output_usd = 0)
  tab <- r3s_batch_table()
  tab$authors <- NULL; tab$year <- NULL
  cl <- gr_mock_client(function(m, p) {
    sys <- m[[1]]$content
    txt <- paste(vapply(m, `[[`, "", "content"), collapse = " ")
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      ids <- as.integer(regmatches(txt, gregexpr("(?<=\\[study )[0-9]+(?=\\])", txt, perl = TRUE))[[1]])
      return(sprintf(paste0('{"claims":[{"claim":"It works.","kind":"finding","supported_by":[%s],',
                            '"contradicted_by":[],"moderator":null,"scope":null}]}'),
                     paste(ids, collapse = ",")))
    }
    if (grepl("Group the ones", sys, fixed = TRUE)) {
      return(sprintf('{"groups":[[%s]]}', paste(1:40, collapse = ",")))
    }
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return('{"sections":[{"heading":"Findings","brief":"b","claims":[1],"rationale":null}]}')
    }
    if (grepl("<draft", txt, fixed = TRUE)) {
      return(paste("Merged:", paste(regmatches(txt, gregexpr("Draft[^<]*", txt))[[1]], collapse = " ")))
    }
    blk <- m[[length(m)]]$content
    ids <- as.integer(regmatches(blk, gregexpr("(?<=\\[study )[0-9]+(?=\\])", blk, perl = TRUE))[[1]])
    if (1L %in% ids) return("Draft A: it works [study 1], as in [study 40].")
    sprintf("Draft B: likewise [study %d].", ids[1])
  })
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl, model = "r3s-small",
                        max_claim_tokens = 2000))
  o <- quiet(gr_outline(cm, client = cl))
  s <- quiet(gr_synthesise(tab, outline = o, question = "Q?", client = cl, claims = cm,
                           model = "r3s-small", max_section_tokens = 300))
  f <- s$sections[s$sections$section == "Findings", ]
  expect_identical(f$n_unsupplied, 0L)
  expect_true(40L %in% s$citations$study[s$citations$section == "Findings"])
})

# ---------------------------------------------------------------------------
# synthesis-4: an organisation's acronym is its name.
# ---------------------------------------------------------------------------

test_that("a lone acronym author is cited by name, not read as initials", {
  # Before: bib_is_initials() took any one to four capitals as initials, so
  # "WHO" gave no surname and the whole review fell back to markers:
  # "A trial [study 1], the guideline [study 2] and a cohort [study 3]."
  f <- readgpt:::bib_surnames
  for (a in c("WHO", "CDC", "NICE", "OECD", "AHRQ")) expect_identical(f(a), a)
  expect_identical(f("WHO."), "WHO")
  # Initials with anything else are still initials, and an organisation mixed
  # into a list of people is still left unread.
  expect_null(f("J."))
  expect_null(f("Smith J, Okafor A; WHO"))
  expect_null(f("Smith J, WHO Collaborating Group"))
  expect_null(f("Smith, J.; ABC Study Group"))
  # A group author after two or more people: the old code cited the first list
  # as "Smith et al." and the rewrite sent the review to markers. The key needs
  # only the first surname and "et al.", whatever the group is called.
  grp <- data.frame(authors = c("Smith, J., Okafor, A., Lee, K., & the ABC Study Group",
                                "Horby P, Lim WS, Emberson JR; RECOVERY Collaborative Group"),
                    year = c(2021L, 2020L), stringsAsFactors = FALSE)
  expect_identical(readgpt:::bib_keys(grp, readgpt:::bib_columns(grp)),
                   c("Smith et al., 2021", "Horby et al., 2020"))
  expect_identical(as.character(f("Horby P, Lim WS, Emberson JR, et al; RECOVERY Collaborative Group")),
                   c("Horby", "Lim", "Emberson"))
  # "for the ... Investigators" is on whose behalf, not a third author.
  expect_null(f("Smith J, Okafor A, for the EPIC Investigators"))
  tab <- data.frame(document = c("a.pdf", "b.pdf", "c.pdf"), status = "ok",
                    duplicate_of = NA_character_, n_filled = 2L,
                    authors = c("Smith, J. and Okafor, A.", "WHO", "Garcia, R."),
                    year = c(2019L, 2020L, 2021L), design = c("RCT", "guideline", "cohort"),
                    stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(messages, params) {
    "A trial [study 1], the guideline [study 2] and a cohort [study 3]."
  })
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl))
  expect_identical(s$cite_style, "author-year")
  expect_match(s$text, "A trial (Smith & Okafor, 2019), the guideline (WHO, 2020) and a cohort (Garcia, 2021).",
               fixed = TRUE)
})

# ---------------------------------------------------------------------------
# synthesis-03: "et al." is cited as et al.
# ---------------------------------------------------------------------------

test_that("an author list ending in et al. is cited as et al., never as one or two authors", {
  # Before: "et al." was stripped and the key built from what was left, so
  # "Smith et al." rendered "(Smith, 2020)" and "Smith J, Okafor A, et al."
  # "(Smith & Okafor, 2021)": a multi-author paper cited as a one- or
  # two-author one.
  s <- readgpt:::bib_surnames("Smith et al.")
  expect_identical(as.character(s), "Smith")
  expect_true(isTRUE(attr(s, "et_al")))
  expect_null(attr(readgpt:::bib_surnames("Smith, J. and Okafor, A."), "et_al"))
  tab <- data.frame(document = c("a.pdf", "b.pdf", "c.pdf", "d.pdf"), status = "ok",
                    duplicate_of = NA_character_, n_filled = 2L,
                    authors = c("Smith et al.", "Smith J, Okafor A, et al.",
                                "Garcia, R., Lee, K., et al.", "Chen, W. and Wu, X."),
                    year = c(2020L, 2021L, 2022L, 2023L), design = "RCT",
                    stringsAsFactors = FALSE)
  cols <- readgpt:::bib_columns(tab)
  expect_identical(readgpt:::bib_keys(tab, cols),
                   c("Smith et al., 2020", "Smith et al., 2021", "Garcia et al., 2022",
                     "Chen & Wu, 2023"))
  expect_identical(readgpt:::bib_keys(tab, cols, form = "narrative")[1], "Smith et al. (2020)")
  # An et al. list the rest of which cannot be read still falls back.
  expect_null(readgpt:::bib_surnames("Smith, Jones, Lee, et al."))
  cl <- gr_mock_client(function(messages, params) "One [study 1] and another [study 2].")
  syn <- quiet(gr_synthesise(tab[1:2, ], outline = c(Findings = "what"), question = "Q?", client = cl))
  expect_match(syn$text, "One (Smith et al., 2020) and another (Smith et al., 2021).", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# synthesis-6: degrees in a byline are not names.
# ---------------------------------------------------------------------------

test_that("a byline's degrees are taken out, not printed as surnames", {
  # Before: '(John Smith & Aisha Okafor, 2019)' -- given names printed as
  # surnames -- and 'John Smith MD, Aisha Okafor PhD' as '(John Smith & PhD, 2019)'.
  f <- function(x) as.character(readgpt:::bib_surnames(x))
  expect_identical(f("John Smith, MD; Aisha Okafor, PhD"), c("Smith", "Okafor"))
  expect_identical(f("John Smith, MD, and Aisha Okafor, PhD"), c("Smith", "Okafor"))
  expect_identical(f("John Smith, MD, Aisha Okafor, PhD"), c("Smith", "Okafor"))
  expect_identical(f("John Smith MD, Aisha Okafor PhD"), c("Smith", "Okafor"))
  expect_identical(f("Sarah Smith MBBS, John Doe PhD"), c("Smith", "Doe"))
  expect_identical(f("Sarah Smith PhD, John Doe PhD"), c("Smith", "Doe"))
  expect_identical(f("John Smith, M.D., and Aisha Okafor, Ph.D."), c("Smith", "Okafor"))
  expect_identical(f("John Smith, MD, MPH; Aisha Okafor, MBChB, PhD"), c("Smith", "Okafor"))
  expect_identical(f("John Smith, PhD"), "Smith")
  expect_identical(f("Mary Smith, MS; John Okafor, PhD"), c("Smith", "Okafor"))
  expect_identical(f("Smith, J., PhD"), "Smith")
  # A byline is not "Surname, Given": with its degrees out, "Smith, Okafor" is
  # two authors, not Smith with the given name Okafor.
  expect_identical(f("Smith MS, Okafor PhD"), c("Smith", "Okafor"))
  # With no degree that could only be a degree, two capitals are initials as
  # they always were: Vancouver, and a surname before initials after a comma.
  expect_identical(f("Smith MD, Jones RN"), c("Smith", "Jones"))
  expect_identical(f("Garcia Lopez, MD; Perez Ruiz, AB"), c("Garcia Lopez", "Perez Ruiz"))
  expect_identical(f("Ma J, Li X"), c("Ma", "Li"))
  tab <- data.frame(document = "a.pdf", status = "ok", duplicate_of = NA_character_,
                    n_filled = 2L, authors = "John Smith MD, Aisha Okafor PhD", year = 2019L,
                    design = "RCT", stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(messages, params) "A trial [study 1].")
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl))
  expect_match(s$text, "A trial (Smith & Okafor, 2019).", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# synthesis-7: every accented Latin letter files with its base letter.
# ---------------------------------------------------------------------------

test_that("Vietnamese, pinyin and other accented names file with their base letter", {
  # Before: only Latin-1 and Latin Extended-A were folded, so these letters
  # sorted after every ASCII letter: "Tran, Trinh, Tran-with-accents",
  # "Lu ... Luo, Lu-with-caron", "Nguyen A, Nguyen Z, Nguyen-with-accents".
  o <- function(x) x[readgpt:::bib_order(x)]
  tran <- "Trần, V."
  expect_identical(o(c("Tran, B.", "Trinh, H.", tran)), c("Tran, B.", tran, "Trinh, H."))
  dang <- "Đặng"
  expect_identical(o(c("Dang", "Davis", "DeSantis", dang)), c("Dang", dang, "Davis", "DeSantis"))
  lv <- "Lǚ"
  expect_identical(o(c("Lu", "Lukas", "Luo", lv)), c("Lu", lv, "Lukas", "Luo"))
  ng <- "Nguyễn, B."
  expect_identical(o(c("Nguyen, A.", "Nguyen, Z.", ng)), c("Nguyen, A.", ng, "Nguyen, Z."))
  hc <- "Ơn, T."   # O with horn
  expect_identical(o(c("Zhou, L.", hc, "Oakes, P.", "Owen, R.")),
                   c("Oakes, P.", hc, "Owen, R.", "Zhou, L."))
  expect_identical(readgpt:::bib_sort_key(c(tran, "Ọ", "ərdəm")),
                   c("tran, v.", "o", "erdem"))
  # The same under the C collation, and through the reference list.
  withr::local_collate("C")
  used <- data.frame(study = 1:3, authors = c("Trinh, H.", tran, "Tran, B."),
                     year = c("1999", "2001", "2000"), document = paste0(1:3, ".pdf"),
                     stringsAsFactors = FALSE)
  cols <- readgpt:::bib_columns(used)
  keys <- readgpt:::bib_keys(used, cols)
  refs <- readgpt:::reference_list(used, keys, 1:3, cols, "author-year")
  expect_identical(sub("^- ([^,]+),.*", "\\1", refs), c("Tran", "Trần", "Trinh"))
})

# ---------------------------------------------------------------------------
# synthesis-8: an unlabelled author string reads the same in any locale.
# ---------------------------------------------------------------------------

test_that("unlabelled UTF-8 author names give the same surnames under a C locale", {
  # Before, in a non-UTF-8 session: "(Åberg Å, 2019)" -- an initial kept, an
  # author dropped -- and "(García & Á, 2021)", because the regexes matched the
  # unlabelled bytes one at a time and "Å" was no capital.
  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!nzchar(old), "no LC_CTYPE to restore")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)), add = TRUE)
  a1 <- "Åberg Å, Lindqvist P"
  a2 <- "García JA, Pérez Á"
  want1 <- c("Åberg", "Lindqvist")
  want2 <- c("García", "Pérez")
  f <- function(x) as.character(readgpt:::bib_surnames(x))
  expect_identical(f(unmarked(a1)), want1)
  expect_identical(f(unmarked(a2)), want2)
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(ok), "cannot switch to the C locale here")
  expect_identical(f(unmarked(a1)), want1)
  expect_identical(f(unmarked(a2)), want2)
  expect_identical(f(unmarked("Öztürk, A. and Müller, K.")),
                   c("Öztürk", "Müller"))
})

# ---------------------------------------------------------------------------
# synthesis-5: studies a claims batch lost are shown downstream.
# ---------------------------------------------------------------------------

test_that("studies lost to a claims batch are counted in the flow, the report and the review", {
  # Before: gr_claims() recorded $lost and warned, and then everything
  # downstream read as complete: gr_flow() had no row for them, the report said
  # "1 claim(s) over 90 study/studies", and print(syn) said nothing.
  n <- 90L
  tab <- data.frame(document = sprintf("d%03d.pdf", 1:n), document_id = sprintf("h%03d", 1:n),
                    status = "ok", duplicate_of = NA_character_, n_filled = 3L,
                    n_unverified = 0L, conflicts = NA_character_,
                    design = rep(c("randomised trial", "cohort", "survey"), length.out = n),
                    finding = rep(c("supports", "no difference", "contradicts"), length.out = n),
                    stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    txt <- paste(vapply(messages, function(m) as.character(m$content), character(1)), collapse = "\n")
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return('{"sections":[{"heading":"Findings","brief":"b","claims":[1],"rationale":null}]}')
    }
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      ids <- as.integer(regmatches(txt, gregexpr("(?<=\\[study )[0-9]+(?=\\])", txt, perl = TRUE))[[1]])
      if (1L %in% ids || 31L %in% ids) {   # the first two batches are cut off
        return(readgpt:::gr_result(TRUE, '{"claims":[{"claim":"It wor', finish_reason = "length"))
      }
      return(sprintf(paste0('{"claims":[{"claim":"It works.","kind":"finding","supported_by":[%s],',
                            '"contradicted_by":[],"moderator":null,"scope":null}]}'),
                     paste(ids, collapse = ",")))
    }
    "It works [study 61] [study 62]."
  })
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  skip_if(length(cm$lost) != 60L, "the claims batches were not sized as this test assumes")
  fl <- gr_flow(claims = cm)
  expect_identical(fl$n[fl$stage == "studies lost to a claims batch"], 60L)
  o <- quiet(gr_outline(cm, client = cl))
  expect_warning(syn <- suppressMessages(gr_synthesise(tab, outline = o, question = "Q?",
                                                       client = cl, claims = cm)),
                 class = "gr_claims_partial")
  out <- capture.output(print(syn))
  expect_true(any(grepl("PARTIAL: written from claims that 60 of 90 studies", out, fixed = TRUE)))
  html <- paste(readgpt:::audit_claims(syn), collapse = "\n")
  expect_match(html, "1 claim(s) over 30 of 90 study/studies", fixed = TRUE)
  expect_match(html, "60 of the 90 studies contributed nothing", fixed = TRUE)
  expect_match(paste(readgpt:::audit_synthesis(syn), collapse = "\n"),
               "Written from claims that 60 of the 90 studies contributed nothing", fixed = TRUE)
  # A complete claims table reads as it did.
  full <- cm; full$lost <- integer(0); full$partial <- FALSE
  expect_identical(gr_flow(claims = full)$n[1], 0L)
  expect_false(grepl("contributed nothing",
                     paste(readgpt:::audit_claims(NULL, full), collapse = "\n"), fixed = TRUE))
})
