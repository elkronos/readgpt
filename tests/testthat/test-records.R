# test-records.R -- the search export, and the numbers a review reports from it.
#
# The parsing is the easy half. What these test is the dialect drift between
# what Scopus, PubMed, Web of Science and Mendeley each call the same field, and
# the two judgements that can attribute one paper's findings to another: whether
# two records are one work, and which document on disk a record refers to.
# Nothing here calls a model.

ris_file <- function(lines, env = parent.frame()) {
  p <- withr::local_tempfile(fileext = ".ris", .local_envir = env)
  writeLines(lines, p); p
}

test_that("RIS parses across the dialects databases actually emit", {
  p <- ris_file(c(
    "TY  - JOUR", "AU  - Smith, J.", "AU  - Okafor, A.",
    "TI  - Cognitive load and retention", "JO  - Journal of Educational Psychology",
    "PY  - 2019", "DO  - https://doi.org/10.1037/EDU0000123",
    "AB  - We conducted a randomised trial of spaced practice",
    "against massed practice in 482 adult learners.",
    "L1  - files/smith2019.pdf", "DB  - Scopus", "ER  - "))
  r <- readgpt:::records_from_ris(readLines(p, warn = FALSE), "scopus.ris")

  expect_equal(nrow(r), 1L)
  # Repeated AU tags are one author list.
  expect_identical(r$authors, "Smith, J.; Okafor, A.")
  expect_identical(r$year, "2019")
  # A DOI arrives bare, as a URL and with a "doi:" prefix; one shape, or two
  # exports of one paper cannot be recognised as one paper.
  expect_identical(r$doi, "10.1037/edu0000123")
  # A wrapped abstract is joined to the tag it continues, not dropped. Half an
  # abstract is worse than none, because nothing looks wrong.
  expect_match(r$abstract, "482 adult learners", fixed = TRUE)
  expect_identical(r$file, "files/smith2019.pdf")
  expect_identical(r$database, "Scopus")
})

test_that("PubMed's DP is a date, not a database", {
  # DP is "database provider" in the RIS specification and "date of publication"
  # in what PubMed exports. Read as a provider, every PubMed record loses its
  # year -- and a citation with no year is not one anybody can follow up.
  p <- ris_file(c("TY  - JOUR", "AU  - Smith J", "TI  - A trial", "JF  - J Educ Psychol",
                  "DP  - 2019 Mar", "AN  - 30987654", "ER  -"))
  r <- readgpt:::records_from_ris(readLines(p, warn = FALSE), "pubmed.ris")
  expect_identical(r$year, "2019")
  expect_identical(r$venue, "J Educ Psychol")
  expect_identical(r$accession, "30987654")
  expect_true(is.na(r$database))

  # Dates come in several shapes and a year is whatever four digits are there.
  for (d in c("2019", "2019/03/12", "2019 Mar", "c2019", "12 March 2019")) {
    p2 <- ris_file(c("TY  - JOUR", "TI  - x", paste("PY  -", d), "ER  -"))
    expect_identical(readgpt:::records_from_ris(readLines(p2, warn = FALSE), "x")$year, "2019",
                     info = d)
  }
  # And something that is not a date does not become one.
  p3 <- ris_file(c("TY  - JOUR", "TI  - x", "PY  - in press", "ER  -"))
  expect_true(is.na(readgpt:::records_from_ris(readLines(p3, warn = FALSE), "x")$year))
})

test_that("a missing final ER does not lose the last record", {
  # Real exports truncate. Losing the last record silently is the worst way to
  # be wrong here: the count is off by one and nothing says so.
  p <- ris_file(c("TY  - JOUR", "TI  - First", "PY  - 2019", "ER  - ",
                  "TY  - JOUR", "TI  - Second", "PY  - 2020"))
  r <- readgpt:::records_from_ris(readLines(p, warn = FALSE), "x")
  expect_equal(nrow(r), 2L)
  expect_identical(r$title, c("First", "Second"))
})

test_that("BibTeX survives braces, escapes and multi-word surnames", {
  p <- withr::local_tempfile(fileext = ".bib")
  writeLines(c(
    "@article{Chen2020,",
    "  author = {Chen, W. and Dubois, M.-C. and van der Berg, P.},",
    "  title = {Spacing and {DNA} sequence recall},",
    "  journal = {Memory {\\&} Cognition},",
    "  year = {2020},",
    "  doi = {10.3758/s13421-020-01099-1}",
    "}"), p)
  r <- readgpt:::records_from_bib(readLines(p, warn = FALSE), "mendeley.bib")

  expect_equal(nrow(r), 1L)
  # Capitalisation-protecting braces are not part of the value, and a regex that
  # stops at the first "}" would cut the title at {DNA}.
  expect_identical(r$title, "Spacing and DNA sequence recall")
  # An escaped ampersand is an ampersand, not a backslash and an ampersand.
  expect_identical(r$venue, "Memory & Cognition")
  # " and " becomes the same separator RIS produces, so downstream sees one
  # convention rather than two.
  expect_identical(r$authors, "Chen, W.; Dubois, M.-C.; van der Berg, P.")
  expect_identical(readgpt:::bib_surnames(r$authors), c("Chen", "Dubois", "van der Berg"))
})

test_that("the parser is chosen by content, not by extension", {
  # Web of Science writes RIS into a .txt and Scholar writes BibTeX into one.
  p <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("TY  - JOUR", "TI  - In a txt file", "PY  - 2019", "ER  -"), p)
  expect_identical(readgpt:::read_export(p)$title, "In a txt file")

  b <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("@article{x, title = {Also a txt file}, year = {2020}}"), b)
  expect_identical(readgpt:::read_export(b)$title, "Also a txt file")

  # And something that is neither says so rather than contributing silently.
  junk <- withr::local_tempfile(fileext = ".ris")
  writeLines(c("just some prose", "with no tags at all"), junk)
  expect_warning(out <- readgpt:::read_export(junk), class = "gr_unknown_export")
  expect_equal(nrow(out), 0L)
})

test_that("the same work from three databases is one record", {
  d <- withr::local_tempdir()
  writeLines(c("TY  - JOUR", "AU  - Smith, J.", "TI  - Cognitive load and retention",
               "PY  - 2019", "DO  - 10.1037/edu0000123", "DB  - Scopus", "ER  -"),
             file.path(d, "scopus.ris"))
  writeLines(c("TY  - JOUR", "AU  - Smith J", "TI  - Cognitive Load and Retention.",
               "DP  - 2019 Mar", "DO  - 10.1037/EDU0000123", "ER  -"),
             file.path(d, "pubmed.ris"))
  writeLines(c("@article{s, author = {Smith, J.}, title = {Cognitive load and retention},",
               "year = {2019}, doi = {10.1037/edu0000123}}"), file.path(d, "m.bib"))

  r <- gr_records(d)
  expect_equal(nrow(r$records), 3L)
  expect_equal(sum(is.na(r$records$duplicate_of)), 1L)
  expect_equal(r$counts$n[r$counts$stage == "records identified"], 3L)
  expect_equal(r$counts$n[r$counts$stage == "duplicates removed"], 2L)
  expect_equal(r$counts$n[r$counts$stage == "records screened"], 1L)
  # Every duplicate points at the row it repeats, so the drop is auditable.
  expect_true(all(r$records$duplicate_of[!is.na(r$records$duplicate_of)] == 1L))
})

test_that("a title match cannot merge two papers that have different DOIs", {
  # Similar titles happen. Merging two studies is the error the whole pipeline
  # exists to avoid, and a DOI is the thing that settles it.
  d <- withr::local_tempdir()
  writeLines(c("TY  - JOUR", "TI  - Spacing and retention", "PY  - 2019",
               "DO  - 10.1000/aaa", "ER  -",
               "TY  - JOUR", "TI  - Spacing and retention", "PY  - 2019",
               "DO  - 10.1000/bbb", "ER  -"), file.path(d, "a.ris"))
  r <- gr_records(d)
  expect_equal(sum(is.na(r$records$duplicate_of)), 2L)

  # With no DOI on either, the title-and-year fallback does merge them.
  d2 <- withr::local_tempdir()
  writeLines(c("TY  - JOUR", "TI  - Spacing and retention", "PY  - 2019", "ER  -",
               "TY  - JOUR", "TI  - Spacing and Retention.", "PY  - 2019", "ER  -"),
             file.path(d2, "a.ris"))
  expect_equal(sum(is.na(gr_records(d2)$records$duplicate_of)), 1L)
  # Unless you say not to.
  expect_equal(sum(is.na(gr_records(d2, dedupe = "doi")$records$duplicate_of)), 2L)
  expect_equal(sum(is.na(gr_records(d2, dedupe = "none")$records$duplicate_of)), 2L)
})

test_that("records are matched to documents, and unmatched ones are a finding", {
  files <- withr::local_tempdir()
  for (f in c("smith2019.pdf", "garcia2022.pdf")) writeLines("x", file.path(files, f))
  writeLines("x", file.path(files, "unrelated.pdf"))
  d <- withr::local_tempdir()
  writeLines(c("TY  - JOUR", "AU  - Smith, J.", "TI  - Cognitive load", "PY  - 2019", "ER  -",
               "TY  - JOUR", "AU  - Garcia, R.", "TI  - No effect", "PY  - 2022", "ER  -",
               "TY  - JOUR", "AU  - Nobody, N.", "TI  - Never obtained", "PY  - 2020", "ER  -"),
             file.path(d, "a.ris"))
  r <- gr_records(d, files = files)

  # Author-surname-and-year is how people name downloaded PDFs, and without it
  # the stricter routes match almost nothing in a real folder.
  expect_identical(basename(r$records$file), c("smith2019.pdf", "garcia2022.pdf", NA))
  expect_identical(r$records$retrieved, c(TRUE, TRUE, FALSE))
  # A record with no document is part of the review -- "sought but not
  # retrieved" -- not something a folder listing can even represent.
  expect_equal(r$counts$n[r$counts$stage == "reports not retrieved"], 1L)
  expect_equal(r$counts$n[r$counts$stage == "reports retrieved"], 2L)
  expect_identical(basename(r$unmatched_files), "unrelated.pdf")
  expect_output(print(r), "match no record")
})

test_that("an ambiguous author and year matches nothing rather than guessing", {
  # Two Smith 2019 papers and one smith2019.pdf. Letting the first claim it is a
  # coin flip presented as a match, and is how one paper's findings end up
  # attributed to another.
  files <- withr::local_tempdir()
  writeLines("x", file.path(files, "smith2019.pdf"))
  d <- withr::local_tempdir()
  writeLines(c("TY  - JOUR", "AU  - Smith, J.", "TI  - First paper", "PY  - 2019", "ER  -",
               "TY  - JOUR", "AU  - Smith, J.", "TI  - Second paper", "PY  - 2019", "ER  -"),
             file.path(d, "a.ris"))
  r <- gr_records(d, files = files)
  expect_identical(r$records$retrieved, c(FALSE, FALSE))
  expect_length(r$unmatched_files, 1L)

  # An explicit path in the export is not ambiguous, and still wins.
  d2 <- withr::local_tempdir()
  writeLines(c("TY  - JOUR", "AU  - Smith, J.", "TI  - First paper", "PY  - 2019",
               "L1  - smith2019.pdf", "ER  -",
               "TY  - JOUR", "AU  - Smith, J.", "TI  - Second paper", "PY  - 2019", "ER  -"),
             file.path(d2, "a.ris"))
  r2 <- gr_records(d2, files = files)
  expect_identical(r2$records$retrieved, c(TRUE, FALSE))
})

test_that("gr_search refuses to record a source without its query", {
  # "We searched PubMed" is not a search strategy. PRISMA item 7 asks for the
  # full strategy for at least one database so that it could be repeated.
  expect_error(gr_search(c("PubMed", "Scopus")), class = "gr_bad_search")
  expect_error(gr_search(c(PubMed = "")), class = "gr_bad_search")
  expect_error(gr_search(c(PubMed = "q"), dates = c("2026-01-01", "2026-01-02")),
               class = "gr_bad_search")
  expect_warning(gr_search(c(PubMed = "q"), dates = "last Tuesday"),
                 class = "gr_bad_search_date")

  s <- gr_search(c(PubMed = "spaced practice[tiab]", Scopus = "TITLE-ABS-KEY(spacing)"),
                 dates = "2026-02-14", registration = "PROSPERO CRD1")
  expect_length(s$databases, 2L)
  expect_identical(unname(s$dates), c("2026-02-14", "2026-02-14"))
  expect_output(print(s), "PROSPERO CRD1")
  # Not registering is a legitimate answer; not saying so is not.
  expect_output(print(gr_search(c(PubMed = "q"))), "NOT REGISTERED")
})

test_that("a record set is a corpus source, and its counts reach the flow diagram", {
  files <- withr::local_tempdir()
  writeLines(c("A randomised trial of spacing with 482 participants."),
             file.path(files, "smith2019.txt"))
  writeLines(c("A cohort study of spacing with 611 participants."),
             file.path(files, "lee2021.txt"))
  d <- withr::local_tempdir()
  writeLines(c("TY  - JOUR", "AU  - Smith, J.", "TI  - Cognitive load", "PY  - 2019",
               "DO  - 10.1000/aaa", "ER  -",
               "TY  - JOUR", "AU  - Smith, J.", "TI  - Cognitive load", "PY  - 2019",
               "DO  - 10.1000/aaa", "ER  -",
               "TY  - JOUR", "AU  - Lee, M.", "TI  - A replication", "PY  - 2021", "ER  -",
               "TY  - JOUR", "AU  - Gone, G.", "TI  - Never obtained", "PY  - 2020", "ER  -"),
             file.path(d, "a.ris"))
  recs <- gr_records(d, files = files,
                     search = gr_search(c(Scopus = "q"), dates = "2026-02-14"))

  out <- quiet(gr_read_many(recs, "How many participants?", "fast", client = mock_echo("AN ANSWER")))
  # The duplicate and the unretrieved record are not read; the two documents are.
  expect_equal(nrow(out$summary), 2L)
  expect_setequal(out$summary$document, c("smith2019.txt", "lee2021.txt"))

  fl <- gr_flow(records = recs)
  expect_identical(fl$stage[1], "records identified")
  expect_equal(fl$n[fl$stage == "records identified"], 4L)
  expect_equal(fl$n[fl$stage == "duplicates removed"], 1L)
  expect_equal(fl$n[fl$stage == "reports not retrieved"], 1L)
})

test_that("bibliographic identity comes from the export, not from a model", {
  # The one part of a citation that must be exactly right had, until the export
  # existed, the loosest guarantee in the pipeline: a model reading a title page.
  files <- withr::local_tempdir()
  writeLines("A randomised trial of spacing with 482 participants.",
             file.path(files, "smith2019.txt"))
  d <- withr::local_tempdir()
  writeLines(c("TY  - JOUR", "AU  - Smith, J.", "AU  - Okafor, A.",
               "TI  - Cognitive load and retention", "JO  - J Educ Psychol",
               "PY  - 2019", "DO  - 10.1000/aaa", "ER  -"), file.path(d, "a.ris"))
  recs <- gr_records(d, files = files)

  cl <- gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("fill a data-extraction form", sys, fixed = TRUE)) {
      return('{"design":"randomised trial","design__quote":"A randomised trial of spacing"}')
    }
    "The trial reported a benefit [study 1]."
  })
  ext <- quiet(gr_extract(recs, gr_fields(design = gr_field("study design")), client = cl))
  expect_identical(ext$table$authors, "Smith, J.; Okafor, A.")
  expect_identical(ext$table$year, "2019")

  syn <- quiet(gr_synthesise(ext, question = "Q?", outline = c(Findings = "what"), client = cl))
  expect_identical(syn$cite_style, "author-year")
  expect_match(syn$text, "(Smith & Okafor, 2019)", fixed = TRUE)
  # The separator records.R uses internally is not a citation convention.
  expect_match(syn$references[1], "Smith, J., Okafor, A.", fixed = TRUE)
  expect_false(grepl(";", syn$references[1], fixed = TRUE))

  # And the model was still never shown any of it.
  shown <- paste(vapply(cl$calls(), function(x)
    paste(vapply(x$messages, function(m) as.character(m$content), character(1)), collapse = " "),
    character(1)), collapse = " ")
  writing <- Filter(function(x) grepl("<studies>", paste(vapply(x$messages,
    function(m) as.character(m$content), character(1)), collapse = " "), fixed = TRUE), cl$calls())
  wrote <- paste(vapply(writing, function(x)
    paste(vapply(x$messages, function(m) as.character(m$content), character(1)), collapse = " "),
    character(1)), collapse = " ")
  expect_false(grepl("Okafor", wrote, fixed = TRUE))
})

test_that("gr_records rejects what it cannot read, clearly", {
  expect_error(gr_records(withr::local_tempdir()), class = "gr_no_exports")
  expect_error(gr_records("no-such-file.ris"), class = "gr_no_exports")
  empty <- withr::local_tempdir()
  writeLines("nothing useful", file.path(empty, "a.ris"))
  expect_error(suppressWarnings(gr_records(empty)), class = "gr_no_exports")
  expect_error(gr_records(ris_file(c("TY  - JOUR", "TI  - x", "ER  -")), search = "not a search"),
               class = "gr_bad_search")
})
