# test-review6-records.R -- the medium and low findings of the review for the
# record set, the protocol file and the folder survey that the earlier passes
# left. Each test failed on f5061ab (the head before this pass) unless it says
# it guards a case that already came out right.

r6_write <- function(lines, path, bytes = FALSE) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeLines(if (bytes) lines else enc2utf8(lines), con, useBytes = TRUE)
  path
}

r6_ris <- function(recs, path) {
  r6_write(unlist(lapply(recs, function(r) c(
    "TY  - JOUR", paste0("AU  - ", r[[1]]), paste0("TI  - ", r[[2]]), paste0("PY  - ", r[[3]]),
    "ER  - ", ""))), path)
}

r6_files <- function(names, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  for (f in names) writeLines(sprintf("Full text of %s.", f), file.path(d, f))
  d
}

r6_matched <- function(r) ifelse(is.na(r$records$file), NA_character_, basename(r$records$file))

# ---------------------------------------------------------------------------
# r-semantics-10: a wrapped abstract line that starts like a tag.
# ---------------------------------------------------------------------------

test_that("a wrapped abstract line starting 'ER-positive' does not end the record", {
  f <- r6_write(c("TY  - JOUR", "AU  - Smith, J.", "TI  - Endocrine therapy outcomes",
                  "AB  - We followed 400 women with", "ER-positive tumours for ten years and found",
                  "HER2-negative disease had the best survival.", "T2-weighted images were read.",
                  "PY  - 2019", "DO  - 10.1000/xyz123", "ER  - "),
                withr::local_tempfile(fileext = ".ris"))
  r <- gr_records(f)$records
  expect_identical(nrow(r), 1L)
  expect_identical(r$year, "2019")
  expect_identical(r$doi, "10.1000/xyz123")
  expect_identical(r$abstract, paste("We followed 400 women with ER-positive tumours for ten years",
                                     "and found HER2-negative disease had the best survival.",
                                     "T2-weighted images were read."))
})

test_that("a tag with one space before its hyphen is still read, and a bare ER still ends a record", {
  # Guards what the looser pattern accepted and real files do: "TY - JOUR".
  f <- r6_write(c("TY - JOUR", "AU - Lee, K.", "TI - One space before the hyphen", "PY - 2020",
                  "ER -", "TY  - JOUR", "AU  - Kim, J.", "TI  - A second record here", "PY  - 2021",
                  "ER-"),
                withr::local_tempfile(fileext = ".ris"))
  r <- gr_records(f)$records
  expect_identical(r$title, c("One space before the hyphen", "A second record here"))
  expect_identical(r$year, c("2020", "2021"))
})

# ---------------------------------------------------------------------------
# records-audit-13: braces that make an organisation one author.
# ---------------------------------------------------------------------------

test_that("a braced corporate author stays one author, and accents become letters", {
  f <- r6_write(c(
    "@article{a, author = {{Centers for Disease Control and Prevention}},",
    "  title = {Guidelines for the long term thing}, year = {2020}}",
    "@article{b, author = {Garc{\\'\\i}a, Mar{\\'\\i}a and M{\\\"u}ller, Hans and {World Health Organization}},",
    "  title = {A study of recall}, year = {2019}}"), withr::local_tempfile(fileext = ".bib"))
  r <- gr_records(f)$records
  expect_identical(r$authors[1], "Centers for Disease Control and Prevention")
  expect_identical(r$authors[2], "Garc\u00eda, Mar\u00eda; M\u00fcller, Hans; World Health Organization")
  fa <- readgpt:::record_first_author(r$authors[1])
  expect_true(fa$corporate)
})

# ---------------------------------------------------------------------------
# records-audit-12: PubMed's own format (.nbib).
# ---------------------------------------------------------------------------

test_that("a PubMed .nbib file is read, not dropped as 'not RIS'", {
  d <- withr::local_tempdir()
  r6_write(c("PMID- 12345678", "OWN - NLM", "STAT- MEDLINE",
             "TI  - Spaced practice and long-term retention in adults: a randomised",
             "      controlled trial.", "LID - S0000-0000(19)00000-0 [pii]",
             "LID - 10.1000/spaced.2019 [doi]", "AB  - We tested spacing.", "DP  - 2019 Mar 5",
             "FAU - Smith, John", "AU  - Smith J", "CN  - RECOVERY Collaborative Group",
             "FAU - Okafor, Ada", "AU  - Okafor A", "VI  - 12", "IP  - 3", "PG  - 45-67",
             "JT  - Journal of Educational Psychology", "TA  - J Educ Psychol",
             "OT  - spacing", "OT  - retention", "",
             "PMID- 23456789", "TI  - Another study of recall.", "DP  - 2020", "AU  - Lee K",
             "AID - 10.1000/recall.2020 [doi]", "TA  - Mem Cognit", ""),
           file.path(d, "pubmed-set.nbib"))
  r6_ris(list(list("Kim, J.", "A record only Scopus has", "2018")), file.path(d, "scopus.ris"))
  expect_no_warning(r <- gr_records(d))
  expect_identical(r$counts$n[r$counts$stage == "records identified"], 3L)
  x <- r$records[r$records$source_file == "pubmed-set.nbib", ]
  expect_identical(x$title[1], "Spaced practice and long-term retention in adults: a randomised controlled trial.")
  expect_identical(x$authors, c("Smith, John; RECOVERY Collaborative Group; Okafor, Ada", "Lee K"))
  expect_identical(x$year, c("2019", "2020"))
  expect_identical(x$doi, c("10.1000/spaced.2019", "10.1000/recall.2020"))
  expect_identical(x$venue, c("Journal of Educational Psychology", "Mem Cognit"))
  expect_identical(x$accession, c("12345678", "23456789"))
  expect_identical(x$keywords[1], "spacing; retention")
  expect_identical(c(x$volume[1], x$issue[1], x$pages[1]), c("12", "3", "45-67"))
})

# ---------------------------------------------------------------------------
# records-audit-10: an export saved as Windows-1252.
# ---------------------------------------------------------------------------

test_that("a Windows-1252 export is read, with a warning naming it", {
  rec <- c("TY  - JOUR", "AU  - M\u00fcller, H.", "TI  - R\u00e9p\u00e9tition espac\u00e9e et m\u00e9moire",
           "PY  - 2019", "ER  - ")
  lat <- iconv(rec, "UTF-8", "CP1252")
  # The accent in the first record: every detection test failed on it, and the
  # file was dropped as "not RIS".
  f <- r6_write(lat, withr::local_tempfile(fileext = ".ris"), bytes = TRUE)
  expect_warning(r <- gr_records(f), class = "gr_export_encoding")
  expect_identical(r$records$authors, "M\u00fcller, H.")
  expect_identical(r$records$title, "R\u00e9p\u00e9tition espac\u00e9e et m\u00e9moire")
  # Further down, past the lines the format is guessed from: the parser
  # stopped gr_records() with "input string 2 is invalid in this locale".
  clean <- unlist(lapply(1:12, function(i) c("TY  - JOUR", sprintf("AU  - Author%d, A.", i),
                                              sprintf("TI  - Clean title number %d here", i),
                                              "PY  - 2019", "ER  - ")))
  g <- r6_write(c(clean, lat), withr::local_tempfile(fileext = ".ris"), bytes = TRUE)
  expect_warning(r <- gr_records(g), class = "gr_export_encoding")
  expect_identical(nrow(r$records), 13L)
  expect_identical(r$records$authors[13], "M\u00fcller, H.")
})

# ---------------------------------------------------------------------------
# records-audit-08: route 3, a filename that is the start of a title.
# ---------------------------------------------------------------------------

test_that("a short numbered filename is not the start of a title", {
  d <- withr::local_tempdir()
  r6_ris(list(list("Zhou, Q.", "2019 novel coronavirus outcomes in adults", "2020"),
              list("Kaur, P.", "3D printed models for surgical planning", "2021")), file.path(d, "a.ris"))
  r <- gr_records(file.path(d, "a.ris"), files = r6_files(c("1.pdf", "2.pdf", "3.pdf")))
  expect_identical(r6_matched(r), c(NA_character_, NA_character_))
  expect_length(r$unmatched_files, 3L)
})

test_that("a filename that begins two works' titles goes to neither", {
  d <- withr::local_tempdir()
  # Baker's and Chen's titles share their first 24 letters, so neither has a
  # title key; that left Adams the only claimant of a file that begins all
  # three titles.
  r6_ris(list(list("Adams, R.", "Effects of spaced practice on vocabulary", "2018"),
              list("Baker, S.", "Effects of spaced practice in surgery training", "2020"),
              list("Chen, L.", "Effects of spaced practice in surgery trainees", "2021")),
         file.path(d, "a.ris"))
  r <- gr_records(file.path(d, "a.ris"), files = r6_files("Effects of spaced practice.pdf"))
  expect_identical(r6_matched(r), rep(NA_character_, 3L))
  expect_identical(basename(r$unmatched_files), "Effects of spaced practice.pdf")
  # A filename that is the whole title still finds it.
  r <- gr_records(file.path(d, "a.ris"),
                  files = r6_files("Effects of spaced practice on vocabulary.pdf"))
  expect_identical(r6_matched(r), c("Effects of spaced practice on vocabulary.pdf", NA, NA))
})

# ---------------------------------------------------------------------------
# r3-scale-at-review-sizes-01: BibTeX in bytes.
# ---------------------------------------------------------------------------

test_that("BibTeX entries are sliced in bytes, correctly around multi-byte letters", {
  expect_warning(e <- readgpt:::bib_entries(c(
    "@article{a, author = {M\u00fcller, J\u00fcrgen}, title = {{\u00c9tude} \u2265 18}}",
    "@comment{skip me}", "@book{b, title = {\u4e2d\u6587 title}}", "@article{c, title = {unclosed")),
    class = "gr_bib_unterminated")
  expect_length(e, 2L)
  expect_identical(e[[1]], "a, author = {M\u00fcller, J\u00fcrgen}, title = {{\u00c9tude} \u2265 18}")
  expect_identical(Encoding(e[[1]]), "UTF-8")
  expect_identical(e[[2]], "b, title = {\u4e2d\u6587 title}")
  expect_warning(readgpt:::bib_entries("x\n@article{c, title = {unclosed"),
                 "on line 2", class = "gr_bib_unterminated")
})

test_that("a large BibTeX file with one non-ASCII letter is not quadratic", {
  skip_on_cran()
  n <- 4000
  ent <- sprintf("@article{k%d,\n  author = {%s},\n  title = {{Study} number %d},\n  abstract = {%s}\n}",
                 seq_len(n), c("M\u00fcller, Hans", rep("Smith, John", n - 1L)), seq_len(n),
                 strrep("spaced practice and retention ", 30))
  # f5061ab: about 40 s for this (every character position found from the start
  # of the file); in bytes it is a fraction of a second.
  t <- system.time(e <- readgpt:::bib_entries(enc2utf8(ent)))[["elapsed"]]
  expect_length(e, n)
  expect_lt(t, 10)
})

# ---------------------------------------------------------------------------
# records-audit-15: a folder among several export paths.
# ---------------------------------------------------------------------------

test_that("a folder in a vector of exports is read, and an unreadable export is named", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "wos"))
  r6_ris(list(list("Kim, J.", "A record only Scopus has", "2018")), file.path(d, "scopus.ris"))
  r6_ris(list(list("Lee, J.", "A record in the Web of Science folder", "2019")), file.path(d, "wos", "b.ris"))
  r6_ris(list(list("Ng, P.", "Another Web of Science record here", "2020")), file.path(d, "wos", "a.ris"))
  expect_no_warning(r <- gr_records(c(file.path(d, "wos"), file.path(d, "scopus.ris"))))
  expect_identical(basename(r$exports), c("a.ris", "b.ris", "scopus.ris"))
  expect_identical(r$counts$n[r$counts$stage == "records identified"], 3L)
  # A file named on its own and again through its folder is read once.
  r <- gr_records(c(file.path(d, "wos", "a.ris"), file.path(d, "wos")))
  expect_identical(basename(r$exports), c("a.ris", "b.ris"))
  expect_identical(r$counts$n[r$counts$stage == "records identified"], 2L)
  # A folder with no exports in it says so.
  dir.create(file.path(d, "empty"))
  expect_warning(gr_records(c(file.path(d, "empty"), file.path(d, "scopus.ris"))),
                 class = "gr_unknown_export")
})

test_that("an export that cannot be opened warns instead of vanishing", {
  skip_on_os("windows")
  d <- withr::local_tempdir()
  r6_ris(list(list("Kim, J.", "A record only Scopus has", "2018")), file.path(d, "scopus.ris"))
  locked <- r6_ris(list(list("Lee, J.", "A record that cannot be read", "2019")), file.path(d, "locked.ris"))
  Sys.chmod(locked, "000")
  withr::defer(Sys.chmod(locked, "644"))
  skip_if(isTRUE(tryCatch(length(readLines(locked)) > 0L, error = function(e) FALSE, warning = function(w) FALSE)),
          "the file is still readable (running as root?)")
  expect_warning(r <- gr_records(c(locked, file.path(d, "scopus.ris"))),
                 "locked.ris", class = "gr_export_unreadable")
  expect_identical(nrow(r$records), 1L)
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-12: lower case for title keys.
# ---------------------------------------------------------------------------

test_that("title keys lower Greek and Cyrillic capitals the same way in a C locale", {
  # On glibc, tolower() in a C locale lowers A-Z alone. macOS lowers the rest
  # too, so on this platform the test guards rather than reproduces.
  up <- "\u0391\u039d\u0391\u039b\u03a5\u03a3\u0397 \u0418\u0421\u0421\u041b\u0415\u0414\u041e\u0412\u0410\u041d\u0418\u0415 of outcomes"
  low <- readgpt:::lower_text(up)
  withr::local_locale(c(LC_CTYPE = "C"))
  expect_identical(readgpt:::title_key(up), readgpt:::title_key(low))
  expect_identical(readgpt:::title_key("Sj\u00f6gren's syndrome and dry eye"),
                   readgpt:::title_key("SJOGREN'S SYNDROME AND DRY EYE"))
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-03: the protocol file's encoding.
# ---------------------------------------------------------------------------

test_that("a protocol with accents and symbols round-trips in a C locale", {
  inc <- "Adults aged \u2265 18 years"
  q <- "R\u00e9tention chez les adultes ?"
  f <- withr::local_tempfile(fileext = ".json")
  # Written in a UTF-8 session, read in a C locale.
  gr_protocol_save(gr_protocol("p", question = q, include = inc), f)
  withr::local_locale(c(LC_CTYPE = "C"))
  back <- gr_protocol_read(f)
  expect_identical(charToRaw(back$include), charToRaw(enc2utf8(inc)))
  expect_identical(Encoding(back$question), "UTF-8")
  # Written in a C locale: the file holds the UTF-8 bytes, not "<U+2265>".
  g <- withr::local_tempfile(fileext = ".json")
  gr_protocol_save(gr_protocol("p", question = q, include = inc), g)
  raw <- readBin(g, "raw", file.size(g))
  expect_false(grepl("<U+", rawToChar(raw), fixed = TRUE))
  expect_true(grepl(rawToChar(charToRaw(enc2utf8(inc))), rawToChar(raw), fixed = TRUE, useBytes = TRUE))
  expect_identical(charToRaw(gr_protocol_read(g)$include), charToRaw(enc2utf8(inc)))
})

# ---------------------------------------------------------------------------
# screen-protocol-14: the search, and headings, in the protocol file.
# ---------------------------------------------------------------------------

test_that("a protocol's search is saved with it and read back the same", {
  s <- gr_search(databases = c(PubMed = "x AND y", PubMed = "x OR y", Scopus = "TITLE(x)"),
                 dates = c("2026-01-01", "2026-02-01", NA), limits = c("English", "2000 onwards"),
                 registration = "PROSPERO CRD42026000000", other = "Reference lists")
  p <- gr_protocol("p", question = "Q?", include = "Adults", search = s)
  f <- withr::local_tempfile(fileext = ".json")
  expect_no_warning(gr_protocol_save(p, f))
  back <- gr_protocol_read(f)
  expect_identical(back$search, s)
  expect_identical(back, p)
  expect_output(print(p), "search   : PubMed, PubMed, Scopus")
  expect_error(gr_protocol("p", question = "Q?", search = list(PubMed = "x")), class = "gr_bad_protocol")
  # A protocol without one is the same object and file as before.
  plain <- gr_protocol("p", question = "Q?")
  expect_false("search" %in% names(plain))
  gr_protocol_save(plain, f)
  expect_false(grepl("\"search\"", paste(readLines(f), collapse = "\n"), fixed = TRUE))
})

test_that("what a protocol file cannot hold is said, not dropped", {
  p <- gr_protocol("p", question = "Q?")
  p$notes <- "attached by hand"
  f <- withr::local_tempfile(fileext = ".json")
  expect_warning(gr_protocol_save(p, f), "'notes'", class = "gr_protocol_unsaved")
  expect_warning(dup <- gr_protocol("p", question = "Q?",
                                    outline = c(Findings = "a", Findings = "b", Methods = "c")),
                 "'Findings'", class = "gr_duplicate_heading")
  expect_warning(gr_protocol_save(dup, f), "Findings.1", class = "gr_duplicate_heading")
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-10: a Windows-1252 text file in the survey.
# ---------------------------------------------------------------------------

test_that("the survey reads a Windows-1252 text file as ingestion does", {
  d <- withr::local_tempdir()
  txt <- rep("R\u00e9sum\u00e9 of the caf\u00e9 study \u2013 \u201cquoted\u201d", 20)
  r6_write(iconv(txt, "UTF-8", "CP1252"), file.path(d, "notes_cp1252.txt"), bytes = TRUE)
  r6_write(iconv(c("<html><body><p>R\u00e9sum\u00e9 caf\u00e9</p></body></html>"), "UTF-8", "CP1252"),
           file.path(d, "page.html"), bytes = TRUE)
  inv <- gr_inventory(d)
  expect_identical(inv$files$status, c("ready", "ready"))
  expect_true(all(inv$files$tokens > 0))
  expect_identical(inv$totals$readable, 2L)
})

# ---------------------------------------------------------------------------
# screen-protocol-12: a vector of paths in the survey.
# ---------------------------------------------------------------------------

test_that("a vector of paths keeps each file's folder, walks a folder, and carries the path", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "2019")); dir.create(file.path(d, "2020"))
  writeLines("The 2019 report text.", file.path(d, "2019", "report.txt"))
  file.create(file.path(d, "2020", "report.txt"))
  writeLines("Notes on the 2020 round.", file.path(d, "2020", "notes.md"))
  src <- c(file.path(d, "2019", "report.txt"), file.path(d, "2020", "report.txt"), file.path(d, "2020"))
  inv <- gr_inventory(src)
  f <- inv$files
  expect_identical(f$file, c("2019/report.txt", "2020/notes.md", "2020/report.txt"))
  expect_identical(f$folder, c("2019", "2020", "2020"))
  expect_identical(f$status, c("ready", "ready", "empty"))
  expect_true(all(file.exists(f$path)))
  expect_identical(normalizePath(f$path), normalizePath(file.path(d, f$file)))
  expect_true(is.na(inv$root))
})

# ---------------------------------------------------------------------------
# screen-protocol-07 and r3-scale-at-review-sizes-08: the PDF probe.
# ---------------------------------------------------------------------------

r6_pdf <- function(path, pages) {
  body <- paste(rep("The cohort had four hundred and eighty two participants.", 3), collapse = " ")
  grDevices::pdf(path)
  for (p in pages) {
    graphics::plot.new()
    if (p == "text") for (l in 1:6) graphics::text(0.5, l / 7, cex = 0.5, body)
    if (p == "image") graphics::rasterImage(matrix(stats::runif(400), 20), 0, 0, 1, 1)
  }
  grDevices::dev.off()
  path
}

test_that("the PDF probe samples across the document, not its first pages", {
  skip_if_not_installed("pdftools")
  d <- withr::local_tempdir()
  # A born-digital report behind a picture cover and two blank pages.
  r6_pdf(file.path(d, "digital_with_cover.pdf"), c("image", "blank", "blank", rep("text", 30)))
  # A scanned article behind a text cover sheet, as databases deliver them.
  r6_pdf(file.path(d, "scanned_with_cover.pdf"), c("text", rep("image", 30)))
  # A third of it text, the rest scanned.
  r6_pdf(file.path(d, "partly_scanned.pdf"), c(rep("text", 10), rep("image", 20)))
  inv <- gr_inventory(d)
  f <- inv$files[order(inv$files$file), ]
  expect_identical(f$file, c("digital_with_cover.pdf", "partly_scanned.pdf", "scanned_with_cover.pdf"))
  expect_identical(f$status, c("ready", "ready", "needs_ocr"))
  expect_identical(f$ocr_pages, c(0L, 20L, 31L))
  expect_match(f$note[3], "no text layer on the 3 of 31 pages sampled")
  expect_match(f$note[2], "about 20 of 30 pages have no text layer (estimated from 3 sampled)", fixed = TRUE)
  # A "ready" file that is mostly scan is said to be one.
  expect_output(print(inv), "1 file(s) have some pages with no text layer (about 20 in all)", fixed = TRUE)
})

test_that("the PDF probe extracts only the pages it samples", {
  skip_if_not_installed("pdftools")
  d <- withr::local_tempdir()
  r6_pdf(file.path(d, "long.pdf"), rep("text", 60))
  real <- pdftools::pdf_text
  pages_read <- integer(0)
  local_mocked_bindings(pdf_text = function(pdf, ...) {
    pages_read <<- c(pages_read, pdftools::pdf_info(pdf)$pages)
    real(pdf, ...)
  }, .package = "pdftools")
  inv <- gr_inventory(d, max_pdf_pages = 3)
  expect_identical(inv$files$status, "ready")
  expect_identical(pages_read, 3L)
  expect_gt(inv$files$tokens, 0)
})
