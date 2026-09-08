# test-synthesise.R -- the write-up, and whether its citations point anywhere.
#
# The property that matters is not prose quality, which no test can assess. It is
# that every claim can be walked back: a sentence cites a study, the study is a
# row that exists, and the row names the document it came from. A citation to a
# row that is not in the table is a fabrication, and it must be reported rather
# than read past.

synth_fields <- function() {
  gr_fields(design = "The study design",
            n = gr_field("Participants", type = "integer"))
}

# A client that extracts on one prompt shape and writes on the other.
synth_client <- function(section_text = "A trial [study 1] and a cohort [study 2].") {
  gr_mock_client(function(messages, params) {
    seen <- paste(vapply(messages, function(m) paste(as.character(m$content), collapse = ""),
                         character(1)), collapse = "\n")
    if (grepl("<studies>", seen, fixed = TRUE)) {
      return(if (is.function(section_text)) section_text(seen) else section_text)
    }
    if (grepl("randomised trial", seen, fixed = TRUE)) {
      return(paste0('{"design":"randomised trial","n":120,',
                    '"design__quote":"We ran a randomised trial.",',
                    '"n__quote":"We enrolled 120 people."}'))
    }
    if (grepl("cohort study", seen, fixed = TRUE)) {
      return(paste0('{"design":"cohort study","n":900,',
                    '"design__quote":"We ran a cohort study.",',
                    '"n__quote":"We followed 900 people."}'))
    }
    '{"design":null,"n":null,"design__quote":null,"n__quote":null}'
  })
}

two_studies <- function(cl, extra = character(0)) {
  f <- function(txt) { p <- tempfile(fileext = ".txt"); writeLines(txt, p); p }
  srcs <- c(f("We ran a randomised trial. We enrolled 120 people."),
            f("We ran a cohort study. We followed 900 people."), extra)
  quiet(gr_extract(srcs, synth_fields(), client = cl, recipe = "thorough", max_tokens = 40))
}

test_that("each section is one call against its own brief", {
  cl <- synth_client()
  x <- two_studies(cl)
  n_extract <- length(cl$calls())

  s <- quiet(gr_synthesise(x, question = "Does it work?",
                           outline = c("Included studies" = "How many, of what design",
                                       "Findings" = "What they found"),
                           client = cl))
  expect_s3_class(s, "gr_synthesis")
  expect_equal(nrow(s$sections), 2L)
  expect_identical(s$sections$section, c("Included studies", "Findings"))
  expect_identical(length(cl$calls()) - n_extract, 2L)

  # The brief reaches the model: a section written against the wrong one would
  # be indistinguishable from a section written against none.
  wrote <- Filter(function(c) grepl("<studies>", paste(vapply(c$messages,
    function(m) paste(as.character(m$content), collapse = ""), character(1)),
    collapse = "\n"), fixed = TRUE), cl$calls())
  seen <- vapply(wrote, function(c) paste(vapply(c$messages,
    function(m) paste(as.character(m$content), collapse = ""), character(1)),
    collapse = "\n"), character(1))
  expect_true(any(grepl("How many, of what design", seen, fixed = TRUE)))
  expect_true(any(grepl("What they found", seen, fixed = TRUE)))
  expect_true(all(grepl("Does it work?", seen, fixed = TRUE)))

  # The assembled document carries the headings; the sections do not repeat them.
  expect_true(grepl("## Included studies", s$text, fixed = TRUE))
  expect_true(grepl("## Findings", s$text, fixed = TRUE))
})

test_that("every citation resolves to a row, and the ones that do not are reported", {
  cl <- synth_client("A trial [study 1] and a cohort [study 2], plus [study 7].")
  x <- two_studies(cl)
  s <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl))

  expect_identical(s$sections$n_cited, 2L)
  expect_identical(s$sections$n_unknown, 1L)
  expect_true(s$sections$partial)
  expect_output(print(s), "TO A ROW THAT DOES NOT EXIST")

  # And the ones that do resolve carry the document they point at -- each its
  # own, not all of them glued together.
  expect_equal(nrow(s$citations), 2L)
  expect_identical(s$citations$study, c(1L, 2L))
  expect_false(anyDuplicated(s$citations$document_id) > 0L)
  expect_identical(s$citations$document, x$table$document[1:2])
  expect_identical(s$citations$document_id, x$table$document_id[1:2])
})

test_that("a section that cites nothing is flagged, not passed off as prose", {
  cl <- synth_client("Both studies broadly agreed with one another.")
  x <- two_studies(cl)
  s <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl))
  expect_identical(s$sections$n_cited, 0L)
  expect_identical(s$sections$n_unknown, 0L)
  expect_false(s$sections$partial)         # nothing fabricated, but
  expect_output(print(s), "NONE")          # visibly unsupported
  expect_equal(nrow(s$citations), 0L)
  expect_named(s$citations, c("section", "study", "document", "document_id"))
})

test_that("an empty section is partial", {
  cl <- synth_client("")
  x <- two_studies(cl)
  s <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl))
  expect_true(s$sections$partial)
})

test_that("a duplicate study is never written up twice", {
  # The error the whole pipeline exists to avoid, and the easiest place to make
  # it: the rows all look alike by the time they reach here.
  cl <- synth_client()
  f <- function(txt) { p <- tempfile(fileext = ".txt"); writeLines(txt, p); p }
  txt <- "We ran a randomised trial. We enrolled 120 people."
  x <- quiet(gr_extract(c(f(txt), f(txt)), synth_fields(), client = cl,
                        recipe = "thorough", max_tokens = 40))
  expect_identical(x$table$status, c("ok", "duplicate"))

  s <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl))
  expect_equal(nrow(s$studies), 1L)
  expect_identical(s$skipped, 1L)
  expect_output(print(s), "left out")
})

test_that("rows that were never read are left out, and rows with nothing in them", {
  cl <- synth_client()
  x <- two_studies(cl, extra = c("no-such-file.txt",
                                 { p <- tempfile(fileext = ".txt")
                                   writeLines("Nothing relevant in this one.", p); p }))
  expect_identical(x$table$status, c("ok", "ok", "failed", "ok"))
  expect_identical(x$table$n_filled[4], 0L)

  s <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl))
  expect_equal(nrow(s$studies), 2L)
  expect_identical(s$skipped, 2L)

  # Unless you ask for them: a document that reports none of the fields is a
  # finding, and a review may want to say so.
  s2 <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                            client = cl, include_unclear = TRUE))
  expect_equal(nrow(s2$studies), 3L)       # the failed one is still out
  expect_identical(s2$skipped, 1L)
})

test_that("the studies handed to the model carry their values and their document", {
  cl <- synth_client()
  x <- two_studies(cl)
  used <- x$table[1:2, ]
  used$study <- 1:2
  rendered <- readgpt:::render_studies(used)

  expect_length(rendered, 2L)
  expect_true(grepl("[study 1]", rendered[1], fixed = TRUE))
  expect_true(grepl("design: randomised trial", rendered[1], fixed = TRUE))
  expect_true(grepl("n: 120", rendered[1], fixed = TRUE))
  expect_true(grepl(used$document[1], rendered[1], fixed = TRUE))
  # A field nothing reported says so, rather than being silently absent: "not
  # reported" is a finding, and a review that cannot see it cannot report it.
  empty <- used[1, ]; empty$n <- NA_integer_; empty$study <- 1L
  expect_true(grepl("n: not reported", readgpt:::render_studies(empty), fixed = TRUE))
  # Bookkeeping columns are not offered to the model as findings.
  expect_false(grepl("n_filled", rendered[1], fixed = TRUE))
  expect_false(grepl("duplicate_of", rendered[1], fixed = TRUE))
})

test_that("a table too large for one prompt is written in batches, not truncated", {
  # Taking the first N rows, or summarising the table first, both drop studies
  # without saying which -- the one thing a review may not do.
  local_registries()
  gr_register_model("tiny-ctx", context_window = 900L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  tab <- data.frame(
    document = sprintf("paper%02d.txt", 1:12),
    document_id = sprintf("id%02d", 1:12),
    design = rep(paste(rep("a randomised controlled trial of some description", 6),
                       collapse = " "), 12),
    n = seq(100L, by = 10L, length.out = 12L),
    n_filled = 2L, n_unverified = 0L, conflicts = NA_character_,
    status = "ok", duplicate_of = NA_character_, error = NA_character_,
    stringsAsFactors = FALSE)

  seen_ids <- new.env(parent = emptyenv()); seen_ids$v <- integer(0)
  cl <- gr_mock_client(function(messages, params) {
    txt <- paste(vapply(messages, function(m) paste(as.character(m$content), collapse = ""),
                        character(1)), collapse = "\n")
    ids <- readgpt:::cited_ids(txt, "study")
    seen_ids$v <- c(seen_ids$v, ids)
    paste(sprintf("Point about [study %d].", ids), collapse = " ")
  })
  s <- quiet(gr_synthesise(tab, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl, model = "tiny-ctx", max_section_tokens = 200L))

  expect_gt(length(cl$calls()), 1L)                    # it batched
  expect_true(all(1:12 %in% seen_ids$v))               # and no study was dropped
  expect_identical(nrow(s$studies), 12L)
})

test_that("gr_synthesise() validates before spending anything", {
  cl <- synth_client()
  x <- two_studies(cl)
  n <- length(cl$calls())
  expect_error(gr_synthesise(x, question = "Q?", client = cl), class = "gr_no_outline")
  expect_error(gr_synthesise(x, outline = c(A = "a"), client = cl))
  expect_error(gr_synthesise(data.frame(), question = "Q?", outline = c(A = "a"),
                             client = cl), class = "gr_no_studies")
  expect_error(gr_synthesise(x, protocol = list(outline = c(A = "a")), client = cl),
               class = "gr_bad_protocol")
  expect_identical(length(cl$calls()), n)

  # Every row unusable is an error, not an empty document that reads as a result.
  dead <- x
  dead$table$status <- "failed"
  expect_error(gr_synthesise(dead, question = "Q?", outline = c(A = "a"), client = cl),
               class = "gr_no_studies")
})

test_that("a protocol supplies the outline and the question", {
  p <- gr_protocol("t", question = "Does the drug reduce events?",
                   fields = synth_fields(),
                   outline = c("Included studies" = "How many, of what design",
                               "Findings" = "What they found"))
  cl <- synth_client()
  x <- two_studies(cl)
  s <- quiet(gr_synthesise(x, p, client = cl))
  expect_identical(s$question, p$question)
  expect_identical(s$outline, p$outline)
  expect_identical(s$sections$section, names(p$outline))

  # An explicit argument still wins.
  s2 <- quiet(gr_synthesise(x, p, outline = c(Only = "Just this"), client = cl))
  expect_identical(s2$sections$section, "Only")
})


# ---------------------------------------------------------------------------
# Citations by name, a reference list, and the coherence pass.
#
# The model keeps writing `[study 3]`, because that is what can be checked
# exactly. Everything here happens to that marker AFTER the check, from the
# table, so a rendered citation is a fact about the extraction rather than
# something the model asserted.
# ---------------------------------------------------------------------------

bib_table <- function(n = 3L) {
  data.frame(
    document = c("smith.pdf", "lee.pdf", "garcia.pdf")[seq_len(n)],
    status = "ok", duplicate_of = NA_character_, n_filled = 2L,
    authors = c("Smith, J., Okafor, A.", "Lee, M., Petrov, K.", "Garcia, R.")[seq_len(n)],
    year = c(2019L, 2021L, 2022L)[seq_len(n)],
    title = c("Cognitive Load", "A Replication", "No Effect")[seq_len(n)],
    venue = c("J Educ Psych 44(2)", "Learn Instr 61(4)", "Appl Cogn Psych 36(1)")[seq_len(n)],
    design = c("RCT", "quasi-experimental", "RCT")[seq_len(n)],
    stringsAsFactors = FALSE)
}

bib_outline <- c("Included studies" = "how many", "Findings" = "what they found")

# Writes markers exactly as the real prompt asks for, so the tests exercise the
# rendering rather than a shape invented for them.
mock_writer <- function(revision = NULL,
                        sec = c("Included studies" = "Three studies [study 1] [study 2] [study 3].",
                                "Findings" = "Effects varied [study 1] and [study 3].")) {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("one argument", sys, fixed = TRUE)) return(revision %||% "")
    if (grepl("write the", sys, fixed = TRUE)) {
      h <- sub(".*write the '([^']+)'.*", "\\1", sys)
      if (h %in% names(sec)) return(sec[[h]])
      return("Something [study 1].")
    }
    "x"
  })
}

test_that("surnames come out of the shapes author lists actually take", {
  f <- readgpt:::bib_surnames
  expect_identical(f("Smith, J., Okafor, A."), c("Smith", "Okafor"))
  expect_identical(f("John Smith and Aisha Okafor"), c("Smith", "Okafor"))
  expect_identical(f("Smith J, Okafor A"), c("Smith", "Okafor"))
  expect_identical(f("Smith, J.; Okafor, A.; Lee, M."), c("Smith", "Okafor", "Lee"))
  expect_identical(f("Smith, J. & Okafor, A."), c("Smith", "Okafor"))
  # Mixed separators: the last author joined with "and" rather than a comma.
  expect_identical(f("OConnor, S. and van Dijk, T."), c("OConnor", "van Dijk"))
  # A lowercase particle is part of the surname. Requiring an initial capital
  # dropped these silently, which leaves the reference list short by an author
  # with nothing saying so.
  expect_identical(f("Chen, W., Dubois, M.-C., van der Berg, P."),
                   c("Chen", "Dubois", "van der Berg"))
  expect_identical(f("de la Cruz, M., Smith, J."), c("de la Cruz", "Smith"))
  expect_identical(f("Garcia, R."), "Garcia")
  expect_identical(f(""), character(0))
})

test_that("a citation key takes the form the sentence needs, and disambiguates a shared year", {
  tab <- bib_table()
  cols <- readgpt:::bib_columns(tab)
  expect_identical(cols$authors, "authors")
  expect_identical(cols$year, "year")

  par <- readgpt:::bib_keys(tab, cols)
  expect_identical(par, c("Smith & Okafor, 2019", "Lee & Petrov, 2021", "Garcia, 2022"))
  nar <- readgpt:::bib_keys(tab, cols, form = "narrative")
  expect_identical(nar[1], "Smith and Okafor (2019)")

  # Two studies by the same authors in the same year get a and b -- and BOTH of
  # them do. `sub()` is not vectorised over `replacement`, so the obvious
  # one-liner gave every duplicate the suffix "a" and left them identical,
  # which is the fault the suffix exists to fix.
  dup <- rbind(tab, tab[1, ]); dup$document[4] <- "smith2.pdf"
  k <- readgpt:::bib_keys(dup, cols)
  expect_identical(k[c(1, 4)], c("Smith & Okafor, 2019a", "Smith & Okafor, 2019b"))
  expect_equal(anyDuplicated(k), 0L)
  expect_identical(vapply(c(1, 26, 27), readgpt:::.gr_bib_suffix, character(1)),
                   c("a", "z", "aa"))

  # Three or more authors are "et al.", one is bare, and a missing year means no
  # key at all rather than a citation nobody can look up.
  many <- tab[1, ]; many$authors <- "Chen, W., Dubois, M., van der Berg, P."
  expect_identical(readgpt:::bib_key(many, cols), "Chen et al., 2019")
  noyr <- tab[1, ]; noyr$year <- NA_integer_
  expect_true(is.na(readgpt:::bib_key(noyr, cols)))
})

test_that("a review cites by name and carries a reference list", {
  syn <- quiet(gr_synthesise(bib_table(), outline = bib_outline, question = "Q?",
                             client = mock_writer()))
  expect_identical(syn$cite_style, "author-year")
  # Adjacent markers become ONE citation. Rendering them separately gives
  # "(Garcia, 2022) (Lee & Petrov, 2021)", which no journal would print and
  # which reads as separate assertions rather than one claim resting on three.
  expect_match(syn$text, "(Garcia, 2022; Lee & Petrov, 2021; Smith & Okafor, 2019)", fixed = TRUE)
  expect_false(grepl("[study", syn$text, fixed = TRUE))

  expect_match(syn$text, "## References", fixed = TRUE)
  expect_length(syn$references, 3L)
  # No doubled full stop where an authors field already ends in an initial.
  expect_false(any(grepl("..", syn$references, fixed = TRUE)))
  expect_match(syn$references[1], "Garcia, R. (2022).", fixed = TRUE)

  # The marker form is kept, so the citation check can be re-run on what was
  # published rather than only on what was drafted.
  expect_true("text_marked" %in% names(syn$sections))
  expect_match(syn$sections$text_marked[1], "[study 1]", fixed = TRUE)
  expect_setequal(readgpt:::cited_ids(syn$text_marked, "study"), 1:3)
})

test_that("only cited studies reach the reference list", {
  # A reference list carrying studies the prose never mentions claims a breadth
  # the review does not have, and is the easiest padding to produce by accident
  # here, where every row is right there.
  one <- mock_writer(sec = c("Included studies" = "One study [study 2].",
                             "Findings" = "It found something [study 2]."))
  syn <- quiet(gr_synthesise(bib_table(), outline = bib_outline, question = "Q?", client = one))
  expect_length(syn$references, 1L)
  expect_match(syn$references[1], "Lee")
})

test_that("an unnameable study falls back to markers rather than a name that may be wrong", {
  tab <- bib_table(); tab$year[2] <- NA_integer_
  expect_warning(
    syn <- suppressMessages(gr_synthesise(tab, outline = bib_outline, question = "Q?",
                                          client = mock_writer(), cite_style = "author-year")),
    class = "gr_cite_unresolvable")
  expect_identical(syn$cite_style, "marker")
  expect_match(syn$text, "[study 1]", fixed = TRUE)
  # There is still a reference list, but numbered by study rather than
  # alphabetical: prose citing `[study 2]` needs a list you can get to from
  # `[study 2]`, and an alphabetical one is unreachable from the text.
  expect_length(syn$references, 3L)
  expect_match(syn$references[2], "^2\\. ")
  # The study whose year was missing is listed without one, not with an
  # invented one.
  expect_match(syn$references[2], "Lee")
  expect_false(grepl("(NA)", syn$references[2], fixed = TRUE))

  # "auto" does the same thing without complaining: it is the mode that means
  # "name them if you can".
  expect_silent(s2 <- quiet(gr_synthesise(tab, outline = bib_outline, question = "Q?",
                                          client = mock_writer())))
  expect_identical(s2$cite_style, "marker")

  # And a table with no bibliographic columns at all is the ordinary case, not
  # an error.
  bare <- bib_table()[, c("document", "status", "duplicate_of", "n_filled", "design")]
  s3 <- quiet(gr_synthesise(bare, outline = bib_outline, question = "Q?", client = mock_writer()))
  expect_identical(s3$cite_style, "marker")
})

test_that("numeric style numbers the citations and the reference list together", {
  syn <- quiet(gr_synthesise(bib_table(), outline = bib_outline, question = "Q?",
                             client = mock_writer(), cite_style = "numeric"))
  expect_identical(syn$cite_style, "numeric")
  expect_match(syn$text, "(1, 2, 3)", fixed = TRUE)
  expect_match(syn$references[1], "^1\\. ")
})

test_that("the coherence pass is kept when it only reorganises prose", {
  good <- paste0("## Included studies\n\nThree studies [study 1] [study 2] [study 3].\n\n",
                 "## Findings\n\nBuilding on that, effects varied [study 1] and [study 3].")
  syn <- quiet(gr_synthesise(bib_table(), outline = bib_outline, question = "Q?",
                             client = mock_writer(revision = good), coherence = TRUE))
  expect_true(syn$coherence$ran)
  expect_true(syn$coherence$kept)
  expect_match(syn$text, "Building on that", fixed = TRUE)
  # Rendering happens after the pass, so the published text is named throughout.
  expect_false(grepl("[study", syn$text, fixed = TRUE))
  # And the un-revised version is kept for comparison.
  expect_false(identical(syn$text, syn$draft))
})

test_that("a coherence pass that changes what is cited is discarded", {
  # This step may reorganise prose. It may not change what the review cites --
  # that is the one way it could quietly undo the guarantee the rest of the
  # pipeline exists to give.
  base <- paste0("## Included studies\n\nThree studies [study 1] [study 2] [study 3].\n\n",
                 "## Findings\n\nEffects varied [study 1] and [study 3].")
  expect_warning(
    added <- suppressMessages(gr_synthesise(bib_table(), outline = bib_outline, question = "Q?",
                                            client = mock_writer(revision = paste0(base, " Also [study 9].")),
                                            coherence = TRUE)),
    class = "gr_coherence_rejected")
  expect_false(added$coherence$kept)
  expect_identical(added$coherence$added, 9L)
  expect_identical(added$text, added$draft)

  dropped_rev <- paste0("## Included studies\n\nThree studies [study 1].\n\n",
                        "## Findings\n\nEffects varied [study 1] and [study 3].")
  expect_warning(
    dropped <- suppressMessages(gr_synthesise(bib_table(), outline = bib_outline, question = "Q?",
                                              client = mock_writer(revision = dropped_rev),
                                              coherence = TRUE)),
    class = "gr_coherence_rejected")
  expect_false(dropped$coherence$kept)
  expect_identical(dropped$coherence$lost, 2L)
})

test_that("the register reaches both the section prompt and the coherence prompt", {
  cl <- mock_writer(revision = paste0("## Included studies\n\nThree studies [study 1] [study 2] ",
                                      "[study 3].\n\n## Findings\n\nEffects varied [study 1] and [study 3]."))
  quiet(gr_synthesise(bib_table(), outline = bib_outline, question = "Q?", client = cl,
                      style = "formal academic; hedge claims", coherence = TRUE))
  sys <- vapply(cl$calls(), function(x) x$messages[[1]]$content, character(1))
  expect_true(all(grepl("formal academic; hedge claims", sys, fixed = TRUE)))
  # Appended, not substituted: the rules about citing and not inventing survive
  # whatever voice is asked for.
  expect_true(any(grepl("Cite the record behind every claim", sys, fixed = TRUE)))
  expect_true(any(grepl("may not add anything", sys, fixed = TRUE)))
})
