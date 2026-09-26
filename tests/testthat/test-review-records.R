# test-review-records.R -- regressions from the adversarial review of the
# record set and the calibration: which paper a document is, which records are
# one work, what a BibTeX field says, and which frame a hand-screened sample
# came from. Each test failed on the code it was written against.

rr_ris <- function(path, recs) {
  lines <- unlist(lapply(recs, function(r) c(
    "TY  - JOUR", paste0("AU  - ", r$au), paste0("TI  - ", r$ti), paste0("PY  - ", r$py),
    if (!is.null(r$jo)) paste0("JO  - ", r$jo), if (!is.null(r$do)) paste0("DO  - ", r$do),
    if (!is.null(r$l1)) paste0("L1  - ", r$l1), "ER  - ", "")))
  # Bytes, as a reference manager writes them: a connection with an encoding
  # would translate through the native one, which in the C locale cannot spell
  # these titles.
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  path
}

rr_files <- function(names, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  for (f in names) writeLines(sprintf("Full text of %s. A trial with n = 120.", f), file.path(d, f))
  d
}

matched <- function(r) basename(r$records$file)

# ---------------------------------------------------------------------------
# records-audit-01: a weak match for an early record took a later record's file.
# ---------------------------------------------------------------------------

test_that("a surname is matched as a word of the filename, not as any substring", {
  files <- rr_files("cheng2020.txt")
  ris <- rr_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Chen, L.", ti = "Acupuncture for insomnia", py = 2020),
    list(au = "Cheng, W.", ti = "Tai chi for balance", py = 2020)))
  r <- gr_records(ris, files = files)
  # Chen's record was given Cheng's paper, and Cheng's went unretrieved.
  expect_identical(matched(r), c(NA, "cheng2020.txt"))
  expect_identical(r$records$retrieved, c(FALSE, TRUE))

  # With no Cheng in the export at all, the file is unmatched, not Chen's.
  alone <- rr_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Chen, L.", ti = "Acupuncture for insomnia", py = 2020)))
  r2 <- gr_records(alone, files = files)
  expect_identical(r2$records$retrieved, FALSE)
  expect_identical(basename(r2$unmatched_files), "cheng2020.txt")
})

test_that("a strong route for a later record beats a weak route for an earlier one", {
  files <- rr_files(c("Parkinson disease and exercise 2019.txt", "Sleep quality 2020.txt"))
  recs <- list(
    list(au = "Park, J.", ti = "Exercise habits of older adults", py = 2019, do = "10.1000/park.2019"),
    list(au = "Smith, A.", ti = "Parkinson disease and exercise", py = 2019),
    list(au = "Lee, K.", ti = "Napping in shift workers", py = 2020),
    list(au = "Nguyen, T.", ti = "Sleep quality", py = 2020))
  r <- gr_records(rr_ris(withr::local_tempfile(fileext = ".ris"), recs), files = files)
  expect_identical(matched(r), c(NA, "Parkinson disease and exercise 2019.txt",
                                 NA, "Sleep quality 2020.txt"))
  # And the order of the export decides nothing.
  rev_r <- gr_records(rr_ris(withr::local_tempfile(fileext = ".ris"), rev(recs)), files = files)
  expect_identical(matched(rev_r), rev(matched(r)))

  # Where it did harm: the bibliographic join put Park's authors, year and DOI
  # on Smith's trial.
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("fill a data-extraction form", messages[[1]]$content, fixed = TRUE)) {
      return('{"n": 120, "n__quote": "A trial with n = 120"}')
    }
    "{}"
  })
  ext <- quiet(gr_extract(r, gr_fields(n = gr_field("participants", type = "integer")),
                          client = cl))
  row <- ext$table[basename(ext$table$document) == "Parkinson disease and exercise 2019.txt", ]
  expect_identical(row$authors, "Smith, A.")
  expect_true(is.na(row$doi))
})

test_that("word boundaries still find the ways people actually name PDFs", {
  names <- c("SmithEtAl2019.txt", "van der Berg - 2020 - Spacing.txt", "O'Brien_2018.txt")
  # A non-ASCII filename only where the locale can spell one on disk.
  if (isTRUE(l10n_info()[["UTF-8"]])) names <- c(names, "M\u00fcller 2021.txt")
  files <- rr_files(names)
  ris <- rr_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Smith, J.", ti = "Cognitive load", py = 2019),
    list(au = "van der Berg, P.", ti = "Retrieval", py = 2020),
    list(au = "O'Brien, K.", ti = "Interleaving", py = 2018),
    list(au = "Muller, H.", ti = "Testing effect", py = 2021)))
  r <- gr_records(ris, files = files)
  expect_identical(matched(r)[seq_along(names)], names)
})

# ---------------------------------------------------------------------------
# records-audit-02: a duplicate claimed the document for a row nothing reads.
# ---------------------------------------------------------------------------

test_that("a file path carried only by a duplicate retrieves the kept record", {
  d <- withr::local_tempdir()
  rr_ris(file.path(d, "a_scopus.ris"), list(
    list(au = "Okafor, C.", ti = "Retrieval practice in secondary science classrooms",
         py = 2021, do = "10.1000/rp.2021.77")))
  rr_ris(file.path(d, "b_endnote.ris"), list(
    list(au = "Okafor, C.", ti = "Retrieval practice in secondary science classrooms",
         py = 2021, do = "10.1000/rp.2021.77", l1 = "internal-pdf://3847562.txt")))
  files <- rr_files("3847562.txt")
  r <- gr_records(d, files = files)
  expect_identical(r$records$duplicate_of, c(NA, 1L))
  # The kept row has the document; the duplicate claims nothing of its own.
  expect_identical(matched(r), c("3847562.txt", NA))
  expect_identical(r$records$retrieved, c(TRUE, NA))
  expect_equal(r$counts$n[r$counts$stage == "reports retrieved"], 1L)
  expect_equal(r$counts$n[r$counts$stage == "reports not retrieved"], 0L)
  expect_length(corpus_sources(r), 1L)

  # One library holding the item twice, the PDF attached to the second copy.
  lib <- rr_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Okafor, C.", ti = "Retrieval practice in secondary science classrooms", py = 2021),
    list(au = "Okafor, C.", ti = "Retrieval practice in secondary science classrooms", py = 2021,
         l1 = "internal-pdf://3847562.txt")))
  r2 <- gr_records(lib, files = files)
  expect_identical(r2$records$retrieved, c(TRUE, NA))
  expect_length(corpus_sources(r2), 1L)
})

# ---------------------------------------------------------------------------
# records-audit-03: title+year merged different studies.
# ---------------------------------------------------------------------------

test_that("letters outside ASCII are part of a title, and short titles merge nothing", {
  ris <- rr_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Wang, L.", ti = "2\u578b\u7cd6\u5c3f\u75c5\u60a3\u8005\u7684\u62a4\u7406", py = 2021),
    list(au = "Zhao, Y.", ti = "2\u578b\u7cd6\u5c3f\u75c5\u7684\u836f\u7269\u6cbb\u7597", py = 2021),
    list(au = "Ito, K.", ti = "Role of PPAR\u03b1 in hepatic steatosis", py = 2021),
    list(au = "Mori, S.", ti = "Role of PPAR\u03b3 in hepatic steatosis", py = 2021),
    list(au = "Chen, X.", ti = "COVID-19 \u5728\u6b66\u6c49\u513f\u7ae5\u4e2d\u7684\u4e34\u5e8a\u7279\u5f81", py = 2020),
    list(au = "Ivanov, P.", ti = "\u0418\u0441\u0441\u043b\u0435\u0434\u043e\u0432\u0430\u043d\u0438\u0435 COVID-19 \u0443 \u0434\u0435\u0442\u0435\u0439", py = 2020),
    list(au = "Smith, A.", ti = "Reply", py = 2020, jo = "Lancet"),
    list(au = "Smith, A.", ti = "Reply", py = 2020, jo = "BMJ")))
  r <- gr_records(ris)
  # Every one of these is a different study; each merge removed one silently.
  expect_true(all(is.na(r$records$duplicate_of)))
  expect_equal(r$counts$n[r$counts$stage == "duplicates removed"], 0L)
  expect_false(identical(readgpt:::title_key("PPAR\u03b1"), readgpt:::title_key("PPAR\u03b3")))
})

test_that("the title fallback still merges one work written two ways", {
  ris <- rr_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Garc\u00eda, R.", ti = "Efectos de la pr\u00e1ctica espaciada en adultos", py = 2018),
    list(au = "Garcia R", ti = "Efectos de la practica espaciada en adultos.", py = 2018),
    # Same long title and year, a different first author: not the same letter.
    list(au = "Jones, B.", ti = "Efectos de la practica espaciada en adultos", py = 2018)))
  r <- gr_records(ris)
  expect_identical(r$records$duplicate_of, c(NA, 1L, NA))
  # The fold is a table, so it does not depend on the platform's iconv.
  expect_identical(readgpt:::title_key("Pr\u00e1ctica \u00c9VORA \u00df"), "practicaevorass")
  expect_identical(readgpt:::title_key("Pra\u0301ctica"), "practica")
})

# ---------------------------------------------------------------------------
# records-audit-09: a DOI-less record never matched the same paper with a DOI.
# ---------------------------------------------------------------------------

test_that("a DOI-less record is a duplicate of the same paper exported with its DOI", {
  d <- withr::local_tempdir()
  writeLines(c("TY  - JOUR", "AU  - Smith, J.", "TI  - Spaced practice improves long-term retention",
               "PY  - 2019", "DO  - 10.1000/spaced.2019", "DB  - Scopus", "ER  -"),
             file.path(d, "a_scopus.ris"))
  writeLines(c("@article{smith2019, author = {Smith, J.},",
               "title = {Spaced practice improves long-term retention}, year = {2019}}"),
             file.path(d, "b_scholar.bib"))
  files <- rr_files("smith2019.txt")
  r <- gr_records(d, files = files)
  expect_identical(r$records$duplicate_of, c(NA, 1L))
  expect_equal(r$counts$n[r$counts$stage == "records screened"], 1L)
  expect_equal(r$counts$n[r$counts$stage == "reports retrieved"], 1L)
  expect_equal(r$counts$n[r$counts$stage == "reports not retrieved"], 0L)
  expect_length(r$unmatched_files, 0L)

  # Listed the other way round, the DOI-less record is kept and the DOI one goes.
  file.rename(file.path(d, "b_scholar.bib"), file.path(d, "0_scholar.bib"))
  expect_identical(gr_records(d)$records$duplicate_of, c(NA, 1L))
})

test_that("a title shared by two DOIs merges nothing, even through a DOI-less copy", {
  ris <- rr_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Smith, J.", ti = "Spacing and long-term retention", py = 2019),
    list(au = "Smith, J.", ti = "Spacing and long-term retention", py = 2019, do = "10.1000/aaa"),
    list(au = "Smith, J.", ti = "Spacing and long-term retention", py = 2019, do = "10.1000/bbb")))
  r <- gr_records(ris)
  # The DOI-less copy could be either paper. Merging it with the first would
  # then merge the second through it: two DOIs, one record, a study gone.
  expect_true(all(is.na(r$records$duplicate_of)))
  # And a later copy of one DOI still finds it.
  ris2 <- rr_ris(withr::local_tempfile(fileext = ".ris"), list(
    list(au = "Smith, J.", ti = "Spacing and long-term retention", py = 2019, do = "10.1000/aaa"),
    list(au = "Smith, J.", ti = "Spacing and long-term retention", py = 2019, do = "10.1000/bbb"),
    list(au = "Smith, J.", ti = "Another title entirely here", py = 2019, do = "10.1000/aaa")))
  expect_identical(gr_records(ris2)$records$duplicate_of, c(NA, NA, 1L))
})

# ---------------------------------------------------------------------------
# r-semantics-07: BibTeX values nested more than one brace deep.
# ---------------------------------------------------------------------------

test_that("BibTeX fields are read by brace depth, and the first occurrence wins", {
  f <- withr::local_tempfile(fileext = ".bib")
  writeLines(c("@article{muller2019,",
               "  author = {M{\\\"{u}}ller, J{\\\"{o}}rg and Smith, Anna},",
               "  title = {Spacing {{DNA}} effects, revisited},",
               "  year = {2019},",
               "  journal = {Memory {\\&} Cognition},",
               "  abstract = {We measured {$\\alpha = {0.5}$}, and in year = 1999 things happened.},",
               "  doi = {10.1000/abcdef}",
               "}"), f)
  r <- gr_records(f)$records
  # Both co-authors survive the nested accent, the title its comma, and the
  # year is not taken from inside the abstract.
  expect_identical(r$authors, "M\\\"uller, J\\\"org; Smith, Anna")
  expect_identical(readgpt:::bib_surnames(r$authors), c("M\\\"uller", "Smith"))
  expect_identical(r$title, "Spacing DNA effects, revisited")
  expect_identical(r$year, "2019")
  expect_identical(r$venue, "Memory & Cognition")
  expect_match(r$abstract, "things happened.", fixed = TRUE)
  expect_identical(r$doi, "10.1000/abcdef")

  g <- withr::local_tempfile(fileext = ".bib")
  writeLines(c("@article{q2020,",
               "  author = \"M{\\\"u}ller, Hans and Okafor, Ada\",",
               "  title = \"Part one\" # {, and part two},",
               "  year = 2020, year = 1999,",
               "  note = {a, b = c}",
               "}"), g)
  q <- gr_records(g)$records
  # A quote inside braces does not end a quoted value; `#` concatenates.
  expect_identical(q$authors, "M\\\"uller, Hans; Okafor, Ada")
  expect_identical(q$title, "Part one, and part two")
  expect_identical(q$year, "2020")

  # A hand-edited file missing the comma between two fields still reads both,
  # as the regex did by accident.
  h <- withr::local_tempfile(fileext = ".bib")
  writeLines(c("@article{h,", "  title = {No comma after me}", "  year = {2018}", "}"), h)
  expect_identical(gr_records(h)$records$title, "No comma after me")
  expect_identical(gr_records(h)$records$year, "2018")
})

# ---------------------------------------------------------------------------
# screen-protocol-02: stacked frames in memory, and a reference from another run.
# ---------------------------------------------------------------------------

rr_screening <- function(dec) {
  structure(list(table = data.frame(document = sprintf("d%04d.pdf", seq_along(dec)),
                                    decision = dec, reason = "r", stringsAsFactors = FALSE)),
            class = "gr_screening")
}

test_that("frames stacked with rbind() in memory are refused like the same CSV", {
  N <- 400
  dec <- c(rep("include", 40), rep("unclear", 10), rep("exclude", N - 50))
  truth <- c(rep("include", 45), rep("exclude", N - 45))
  names(truth) <- sprintf("d%04d.pdf", seq_len(N))
  scr <- rr_screening(dec)
  kept <- gr_reference(scr, n = Inf, of = "kept")
  excl <- gr_reference(scr, n = 60, of = "excluded", seed = 3)
  for (both in list(rbind(excl, kept), rbind(kept, excl))) {
    both$human_decision <- unname(truth[both$document])
    # It kept the first argument's attributes and was scored as that frame:
    # "40.9% eligible among the excluded", about 143 studies "lost".
    expect_error(gr_calibrate(scr, both), class = "gr_mixed_frame")
  }
  # Naming a frame the rows do not fit is refused too, not scored.
  both <- rbind(excl, kept)
  both$human_decision <- unname(truth[both$document])
  expect_error(gr_calibrate(scr, both, of = "excluded"), class = "gr_reference_mismatch")
  # Each frame on its own still works, and projects from its own size.
  excl$human_decision <- unname(truth[excl$document])
  cal <- gr_calibrate(scr, excl)
  expect_identical(cal$frame$of, "excluded")
  expect_equal(cal$projected$frame_n, N - 50L)
  expect_equal(cal$projected$lost, 0)
})

test_that("a reference from a run whose decisions changed is refused, not projected", {
  N <- 200
  scr <- rr_screening(c(rep("include", 20), rep("exclude", N - 20)))
  ref <- gr_reference(scr, n = 20, of = "excluded", seed = 9)
  ref$human_decision <- "exclude"
  ref$human_decision[1:5] <- "include"
  # The re-run kept the five eligible rows. Scored as exclusions they reported
  # 25% eligible among the excluded, 45 studies projected lost, and no misses.
  tab <- scr$table
  tab$decision[match(ref$document[1:5], tab$document)] <- "include"
  expect_error(gr_calibrate(structure(list(table = tab), class = "gr_screening"), ref),
               class = "gr_reference_mismatch")
  # Against the run it was drawn from, it calibrates.
  expect_equal(gr_calibrate(scr, ref)$projected$lost, 0.25 * (N - 20))
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-02: file order must not follow LC_COLLATE.
# ---------------------------------------------------------------------------

test_that("exports and inventories are ordered the same way under every collation", {
  d <- withr::local_tempdir()
  for (nm in c("adams", "Baker", "zhou")) {
    writeLines(c("TY  - JOUR", paste0("AU  - ", nm, ", A."), paste0("TI  - Paper by ", nm),
                 "PY  - 2020", "ER  - "), file.path(d, paste0(nm, ".ris")))
  }
  # Byte order puts "Baker" first; an English collation puts "adams" first, and
  # that is what the laptop gave while cron, Docker and R CMD check gave this.
  bytes <- c("Baker.ris", "adams.ris", "zhou.ris")
  old <- Sys.getlocale("LC_COLLATE")
  withr::defer(Sys.setlocale("LC_COLLATE", old))
  for (loc in c("C", "en_US.UTF-8", "en_US", "English_United States.1252")) {
    if (!nzchar(suppressWarnings(Sys.setlocale("LC_COLLATE", loc)))) next
    expect_identical(basename(readgpt:::export_paths(d)), bytes, info = loc)
    expect_identical(gr_inventory(d)$files$file, bytes, info = loc)
    expect_identical(gr_records(d)$records$authors, c("Baker, A.", "adams, A.", "zhou, A."),
                     info = loc)
  }
})
