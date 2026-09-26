# test-review-ingest.R -- ingestion defects found in review, one block per finding.

# ---------------------------------------------------------------------------
# ingest-01: a document-scoped cleaner deleted every block it edited in part.
# ---------------------------------------------------------------------------

test_that("a running head is dropped from a block without the block's body text", {
  out <- gr_clean(sprintf("RUNNING HEAD\nBody %s unique", c("one", "two", "three", "four")),
                  steps = "headers_footers")
  expect_identical(as.character(out),
                   sprintf("Body %s unique", c("one", "two", "three", "four")))
  expect_identical(attr(out, "gr_clean_log")$headers_footers$chars_removed,
                   4L * nchar("RUNNING HEAD\n"))

  # Through the 'scan' preset: every page opens with the running head in the
  # same paragraph as its first body line. The head goes; the body stays.
  pages <- vapply(1:6, function(i) sprintf(paste0(
    "THE HISTORY OF ROME\nPage body %d: the senate met and voted on grain prices.\n\n",
    "Second paragraph of page %d discusses the legions."), i, i), "")
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(paste(pages, collapse = "\n\n"), f)
  d <- quiet(gr_ingest(f, gr_ingest_spec(clean = "scan"), cache = FALSE))
  expect_identical(nrow(d$blocks), 12L)
  for (i in 1:6) expect_match(d$text, sprintf("Page body %d: the senate", i), fixed = TRUE)
  expect_false(grepl("HISTORY OF ROME", d$text, fixed = TRUE))
})

test_that("a References heading inside a block cuts the block there, not the whole block", {
  body <- sprintf("Body paragraph %d describes the trial methods and results in detail.", 1:60)
  refs <- paste(sprintf("%d. Author%d A. Title of paper %d. Journal %d.", 1:12, 1:12, 1:12,
                        2000 + 1:12), collapse = "\n")
  last <- paste0("In conclusion, the intervention reduced mortality by 12 percent.\nReferences\n",
                 refs)
  d <- quiet(gr_ingest(paste(c(body, last), collapse = "\n\n"), "academic", cache = FALSE))
  expect_identical(utils::tail(d$blocks$text, 1),
                   "In conclusion, the intervention reduced mortality by 12 percent.")
  expect_false(grepl("Author3 A.", d$text, fixed = TRUE))
  expect_identical(nrow(d$blocks), 61L)

  # A block the step removes entirely still comes back empty, block for block.
  out <- gr_clean(c(body, "References", refs), steps = "references")
  expect_length(out, 62L)
  expect_identical(as.character(out[61:62]), c("", ""))
  expect_identical(as.character(out[1:60]), body)
})

test_that("a document-scoped step that rewrites lines is mapped back or reported", {
  local_registries()
  txt <- c("Alpha line one.\nAlpha line two.", "Beta paragraph.")
  # Rewrites text inside lines but keeps every line: mapped line for line.
  gr_register_cleaner("upper_doc", scope = "document", fn = function(x, o) toupper(x))
  expect_no_warning(out <- gr_clean(txt, steps = "upper_doc"))
  expect_identical(as.character(out), toupper(txt))

  # Rewrites and merges lines: it cannot be matched back, so it is applied to
  # each block on its own, with a warning, and no block is emptied.
  gr_register_cleaner("join_doc", scope = "document",
                      fn = function(x, o) gsub("\n", " ", toupper(x), fixed = TRUE))
  expect_warning(out <- gr_clean(txt, steps = "join_doc"), class = "gr_clean_unmapped")
  expect_identical(as.character(out), c("ALPHA LINE ONE. ALPHA LINE TWO.", "BETA PARAGRAPH."))
})

# ---------------------------------------------------------------------------
# ingest-02: the hyphenation step joined number ranges into one wrong number.
# ---------------------------------------------------------------------------

test_that("hyphenation rejoins split words and leaves number ranges alone", {
  out <- gr_clean(paste0("Participants aged 18-\n65 years were enrolled between 2010-\n",
                         "2015 (see pages 112-\n118)."))
  expect_false(grepl("1865|20102015|112118", out))
  expect_match(out, "aged 18-\n65 years", fixed = TRUE)
  expect_match(out, "between 2010-\n2015", fixed = TRUE)

  expect_identical(as.character(gr_clean("mito-\nchondria", steps = "hyphenation")),
                   "mitochondria")
  expect_identical(as.character(gr_clean("naï-\nve", steps = "hyphenation")),
                   "naïve")
  # A capital after the break is a compound, and a blank line is a paragraph
  # break, not a line the word continues on.
  expect_identical(as.character(gr_clean("Anglo-\nSaxon", steps = "hyphenation")),
                   "Anglo-\nSaxon")
  expect_identical(as.character(gr_clean("ends with a dash-\n\nnext paragraph",
                                         steps = "hyphenation")),
                   "ends with a dash-\n\nnext paragraph")

  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("Participants aged 18-", "65 years were enrolled at four sites."), f)
  expect_match(quiet(gr_ingest(f, cache = FALSE))$text, "aged 18-\n65 years", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# ingest-04: the HTML extractor lost, duplicated and ran together text.
# ---------------------------------------------------------------------------

test_that("HTML is read in document order, each piece of text once", {
  skip_if_not_installed("xml2")
  p <- withr::local_tempfile(fileext = ".html")
  writeLines(c(
    "<!DOCTYPE html><html><head><title>T</title><style>p {color: red}</style></head><body>",
    "<h1>Trial report</h1>",
    "<div class='abstract'>The primary outcome improved by 12 percent\n   with the new drug.</div>",
    "<ul><li><p>Adverse events were rare in both arms.</p></li>",
    "<li>Nausea <ul><li>mild</li></ul></li></ul>",
    "<blockquote><p>A quoted statement from the lead investigator.</p></blockquote>",
    "<table><tr><th>Arm</th><th>Deaths</th></tr>",
    "<tr><td><p>Placebo</p></td><td>42</td></tr></table>",
    "<p>Address: 1 Main St<br>Springfield<br/>USA</p>",
    "<h5>Funding</h5>",
    "<p>Funded by the national institute.</p>",
    "<section><span>A conclusion in a span</span> <em>inside</em> a section.</section>",
    "<script>var hidden = 'not content';</script><!-- a comment -->",
    "</body></html>"), p)
  d <- quiet(gr_ingest(p, gr_ingest_spec(clean = "none"), cache = FALSE))
  b <- d$blocks
  expect_identical(b$text, c(
    "Trial report",
    "The primary outcome improved by 12 percent with the new drug.",
    "Adverse events were rare in both arms.",
    "Nausea", "mild",
    "A quoted statement from the lead investigator.",
    "Arm | Deaths", "Placebo | 42",
    "Address: 1 Main St\nSpringfield\nUSA",
    "Funding",
    "Funded by the national institute.",
    "A conclusion in a span inside a section."))
  expect_identical(b$kind, c("heading", "body", "body", "body", "body", "body", "table",
                             "table", "body", "heading", "body", "body"))
  expect_identical(b$section, rep(c("Trial report", "Funding"), c(9L, 3L)))
})

test_that("an HTML table inside a cell is read as rows of its own after its row", {
  skip_if_not_installed("xml2")
  p <- withr::local_tempfile(fileext = ".html")
  writeLines(paste0(
    "<html><body><table><caption>Table 2. Outcomes</caption>",
    "<thead><tr><th>Outcome</th><th>Result</th></tr></thead><tbody>",
    "<tr><td>Mortality</td><td><p>12</p><p>(5%)</p></td></tr>",
    "<tr><td>Subgroups<table><tr><td>Men</td><td>7</td></tr></table></td><td>see below</td></tr>",
    "</tbody></table></body></html>"), p)
  b <- quiet(gr_ingest(p, gr_ingest_spec(clean = "none"), cache = FALSE))$blocks
  expect_identical(b$text, c("Table 2. Outcomes", "Outcome | Result", "Mortality | 12 (5%)",
                             "Subgroups | see below", "Men | 7"))
})

# ---------------------------------------------------------------------------
# ingest-06: the OCR engine was built in the parent and died in the workers.
# ---------------------------------------------------------------------------

test_that("parallel OCR builds its engine in the process that reads the page", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  skip_if_not_installed("magick")
  # A magick image stands in for the tesseract engine: both are external
  # pointers, and both are dead once copied into another process. Built only in
  # the caller, as it was, the engine reached each worker dead and every page
  # failed.
  res <- quiet(readgpt:::ocr_pdf_pages(
    "scan.pdf", pages = c("", "", ""), needs = c(TRUE, TRUE, TRUE),
    opts = list(parallel = TRUE),
    engine = function(lang) magick::image_blank(7, 2),
    read_page = function(path, i, dpi) i,
    ocr = function(img, eng) sprintf("Page %d read by a %d-pixel engine.", img,
                                     magick::image_info(eng)$width),
    workers = 2L))
  expect_identical(res$pages, sprintf("Page %d read by a 7-pixel engine.", 1:3))
  expect_identical(res$unread, integer(0))
  expect_identical(res$ocr_done, c(TRUE, TRUE, TRUE))
})

test_that("a page whose OCR fails keeps its text layer", {
  warned <- character(0)
  res <- withCallingHandlers(
    readgpt:::ocr_pdf_pages(
      "doc.pdf", pages = c("", "A born-digital page with a good text layer.", "Page three."),
      needs = c(TRUE, TRUE, FALSE), opts = list(parallel = FALSE),
      engine = function(lang) "engine",
      read_page = function(path, i, dpi) i,
      ocr = function(img, eng) stop("engine crashed")),
    gr_ocr_failed = function(w) {
      warned <<- c(warned, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_length(warned, 2L)
  expect_identical(res$pages, c("", "A born-digital page with a good text layer.", "Page three."))
  # Unread only where nothing became text, as when OCR is not installed.
  expect_identical(res$unread, 1L)
  expect_identical(res$ocr_done, c(FALSE, FALSE, FALSE))
})

# ---------------------------------------------------------------------------
# r-semantics-13: every function hashed alike, so re-registered code was
# served from the ingest cache as the old code had left it.
# ---------------------------------------------------------------------------

test_that("gr_hash() tells functions apart and leaves other values as they were", {
  h <- readgpt:::gr_hash
  expect_false(identical(h(list(f = function(x) x)), h(list(f = function(x) x + 1))))
  f <- function(x) x
  expect_identical(h(list(f = f)), h(list(f = f)))
  # The same code closing over different values is different behaviour.
  make <- function(p) function(x) gsub(p, "", x)
  expect_false(identical(h(make("a")), h(make("b"))))
  expect_identical(h(sum), h(sum))

  # Keys already in caches and stores are unchanged for everything else.
  expect_identical(h(list(a = 1, b = "x", c = list(d = TRUE, e = NULL))), "aca57022de60e0b9")
  expect_identical(h(list("inline", "Some text.", list(clean = "standard", ocr = "auto"))),
                   "00cd3e9370520e46")
  expect_identical(h(c(x = 1L, y = 2L)), "3382b10e81b997fa")
})

test_that("a cleaner or extractor registered again is not served from the cache", {
  local_registries()
  local_clean_cache()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("Introduction paragraph with enough characters to pass the filter.", "",
               "CONFIDENTIAL: patient details here.", "",
               "Another normal paragraph of text that is long enough to stay."), f)
  spec <- gr_ingest_spec(clean = c("drop_conf", "collapse_whitespace"))
  gr_register_cleaner("drop_conf", function(x, o) x)
  expect_match(quiet(gr_ingest(f, spec))$text, "CONFIDENTIAL", fixed = TRUE)
  gr_register_cleaner("drop_conf", function(x, o) gsub("(?mi)^CONFIDENTIAL.*$", "", x, perl = TRUE))
  expect_false(grepl("CONFIDENTIAL", quiet(gr_ingest(f, spec))$text, fixed = TRUE))

  txt <- paste0("Some prose paragraph long enough to keep around here.\n\n",
                "CONFIDENTIAL line that should go away.")
  gr_register_cleaner("drop_conf2", function(x, o) x)
  expect_match(quiet(gr_ingest(txt, "drop_conf2"))$text, "CONFIDENTIAL", fixed = TRUE)
  gr_register_cleaner("drop_conf2", function(x, o) gsub("(?mi)^CONFIDENTIAL.*$", "", x, perl = TRUE))
  expect_false(grepl("CONFIDENTIAL", quiet(gr_ingest(txt, "drop_conf2"))$text, fixed = TRUE))

  # A function among the cleaner options is part of the settings too.
  gr_register_cleaner("apply_fn", function(x, o) if (is.function(o$fn)) o$fn(x) else x)
  up <- quiet(gr_ingest(txt, gr_ingest_spec(clean = "apply_fn",
                                            cleaner_opts = list(fn = function(x) toupper(x)))))
  low <- quiet(gr_ingest(txt, gr_ingest_spec(clean = "apply_fn",
                                             cleaner_opts = list(fn = function(x) tolower(x)))))
  expect_match(up$text, "SOME PROSE", fixed = TRUE)
  expect_match(low$text, "some prose", fixed = TRUE)

  z <- withr::local_tempfile(fileext = ".zzz")
  writeLines("raw content", z)
  gr_register_extractor("zzz", "zzz", function(path, spec) "OLD extractor output, long enough.")
  expect_match(quiet(gr_ingest(z))$text, "OLD", fixed = TRUE)
  gr_register_extractor("zzz", "zzz", function(path, spec) "NEW extractor output, long enough.")
  expect_match(quiet(gr_ingest(z))$text, "NEW", fixed = TRUE)

  # And the same settings with the same code still come from the cache.
  gr_options(verbose = TRUE)          # restored by local_registries()
  a <- quiet(gr_ingest(f, spec))
  expect_message(b <- gr_ingest(f, spec), "cached", fixed = TRUE)
  expect_identical(a, b)
})
