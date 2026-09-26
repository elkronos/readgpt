# test-review3-ingest.R -- the third pass on ingestion: regressions and partial
# fixes that verification found in the earlier fixes.
#
# Each block names the finding and says what the earlier fix did wrong.

html_blocks_of <- function(html, ext = ".html") {
  p <- withr::local_tempfile(fileext = ext)
  writeLines(html, p)
  readgpt:::extract_html(p, list())
}

# ---------------------------------------------------------------------------
# ingest-3 / ingest-04: a table used to lay out a page was read as one row,
# so its headings, sections and paragraphs became a single "table" block.
# ---------------------------------------------------------------------------

test_that("a table that lays out a page is read as the page, with headings and sections", {
  skip_if_not_installed("xml2")
  b <- html_blocks_of(paste0(
    "<html><body><table><tr><td><a href='/'>Home</a> <a href='/a'>About</a></td><td>",
    "<h1>Annual Report 2024</h1><p>Revenue grew 10 percent.</p>",
    "<h2>Operations</h2><p>The company opened two plants.</p>",
    "<ul><li>Plant A in Dayton</li><li>Plant B in Toledo</li></ul>",
    "<h2>Outlook</h2><p>We expect growth.</p></td></tr></table></body></html>"))
  # Before: one block, "Home About | Annual Report 2024 Revenue grew ...", kind
  # "table", section NA.
  expect_identical(b$text, c("Home About", "Annual Report 2024", "Revenue grew 10 percent.",
                             "Operations", "The company opened two plants.", "Plant A in Dayton",
                             "Plant B in Toledo", "Outlook", "We expect growth."))
  expect_identical(b$kind, c("body", "heading", "body", "heading", "body", "body", "body",
                             "heading", "body"))
  expect_identical(b$section, c(NA, "Annual Report 2024", "Annual Report 2024",
                                rep("Operations", 4), "Outlook", "Outlook"))

  # The whole page in one cell, and a layout of header, nav + content and
  # footer rows, with the content in a table of its own.
  b <- html_blocks_of(paste0(
    "<html><body><table><tr><td><h1>Annual report</h1><p>First paragraph of the report.</p>",
    "<h2>Results</h2><p>Revenue rose 12 percent.</p></td></tr></table></body></html>"))
  expect_identical(b$text, c("Annual report", "First paragraph of the report.", "Results",
                             "Revenue rose 12 percent."))
  expect_identical(b$section, rep(c("Annual report", "Results"), each = 2L))
  b <- html_blocks_of(paste0(
    "<html><body><table><tr><td colspan=2>Logo</td></tr><tr><td>Nav</td><td><table><tr><td>",
    "<h1>Title</h1><p>P1.</p><h2>Sec</h2><p>P2.</p></td></tr></table></td></tr>",
    "<tr><td colspan=2>Foot</td></tr></table></body></html>"))
  expect_identical(b$text, c("Logo", "Nav", "Title", "P1.", "Sec", "P2.", "Foot"))
  expect_false("table" %in% b$kind)

  # An e-mail laid out in a one-cell table keeps its paragraphs apart.
  b <- html_blocks_of(paste0("<html><body><table><tr><td><p>Hi Jane,</p>",
                             "<p>Thanks for your order.</p><p>Regards</p></td></tr></table>",
                             "</body></html>"))
  expect_identical(b$text, c("Hi Jane,", "Thanks for your order.", "Regards"))

  # A data table inside the layout is still read row by row, in its place.
  b <- html_blocks_of(paste0(
    "<html><body><table><tr><td><h1>Report</h1><p>Intro.</p><table><tr><th>Arm</th>",
    "<th>Deaths</th></tr><tr><td>Placebo</td><td>42</td></tr></table><p>After.</p>",
    "</td></tr></table></body></html>"))
  expect_identical(b$text, c("Report", "Intro.", "Arm | Deaths", "Placebo | 42", "After."))
  expect_identical(b$kind, c("heading", "body", "table", "table", "body"))
})

test_that("a data table keeps its rows, whatever its cells and caption hold", {
  skip_if_not_installed("xml2")
  # Headings in the header cells or the caption do not make a table a layout.
  b <- html_blocks_of(paste0(
    "<html><body><table><caption><h3>Table 1. Plans</h3></caption>",
    "<tr><th><h3>Basic</h3></th><th><h3>Pro</h3></th></tr>",
    "<tr><td>$10</td><td>$20</td></tr><tr><td>1 user</td><td>5 users</td></tr></table>",
    "</body></html>"))
  expect_identical(b$text, c("Table 1. Plans", "Basic | Pro", "$10 | $20", "1 user | 5 users"))
  expect_identical(b$kind, c("body", "table", "table", "table"))

  # One row of values is still a row.
  expect_identical(html_blocks_of(paste0("<html><body><table><tr><td>Name:</td>",
                                         "<td>John</td></tr></table></body></html>"))$text,
                   "Name: | John")

  # Code in a cell keeps its lines and indentation, as a <pre> anywhere else
  # does. Before: "Ex | def f(): return 1".
  b <- html_blocks_of(paste0("<html><body><table><tr><td>Ex</td><td><pre>def f():\n",
                             "    return 1</pre></td></tr><tr><td>b</td><td>c</td></tr></table>",
                             "</body></html>"))
  expect_identical(b$text, c("Ex | def f():\n    return 1", "b | c"))

  # Markup in the table but in no row is read, before the rows, as a browser
  # shows it. Before: it was dropped.
  b <- html_blocks_of(paste0("<html><body><table><p>Note on the table.</p><tr><td>a</td>",
                             "<td>b</td></tr><tr><td>c</td><td>d</td></tr></table>",
                             "<table><p>A table with no rows.</p></table></body></html>"))
  expect_identical(b$text, c("Note on the table.", "a | b", "c | d", "A table with no rows."))
  # A table straight inside another, or inside a row but outside its cells, is
  # read once.
  b <- html_blocks_of(paste0("<html><body><table><table><tr><td>in1</td><td>in2</td></tr>",
                             "</table><tr><td>a</td><td>b</td></tr><tr><td>c</td><td>d</td></tr>",
                             "</table><table><tr><table><tr><td>in3</td><td>in4</td></tr></table>",
                             "<td>e</td><td>f</td></tr><tr><td>g</td><td>h</td></tr></table>",
                             "</body></html>"))
  expect_identical(b$text, c("in1 | in2", "a | b", "c | d", "e | f", "in3 | in4", "g | h"))
})

# ---------------------------------------------------------------------------
# ingest-4: the HTML walker took quadratic time on long runs of inline text.
# ---------------------------------------------------------------------------

test_that("reading HTML takes time in proportion to the page", {
  skip_on_cran()
  skip_if_not_installed("xml2")
  timed <- function(html, reps = 1L) {
    p <- withr::local_tempfile(fileext = ".html")
    writeLines(html, p)
    min(vapply(seq_len(reps), function(i) {
      system.time(readgpt:::extract_html(p, list()))[["elapsed"]]
    }, numeric(1)))
  }
  br_page <- function(n) paste0("<html><body>", paste0("line ", seq_len(n), collapse = "<br>"),
                                "</body></html>")
  cell_page <- function(n) {
    paste0("<html><body><table><tr><td>a</td><td>",
           paste0("<span>w", seq_len(n), " </span>", collapse = ""),
           "</td></tr><tr><td>b</td><td>c</td></tr></table></body></html>")
  }
  # Eight times the text. Appending each piece with c() copied the run every
  # time, so it took some 50 times as long (30,000 lines: 7 seconds).
  small <- timed(br_page(4000L), reps = 3L)
  expect_lt(timed(br_page(32000L), reps = 2L) / max(small, 0.01), 20)
  small <- timed(cell_page(4000L), reps = 3L)
  expect_lt(timed(cell_page(32000L), reps = 2L) / max(small, 0.01), 20)

  # And the text is all there, once.
  p <- withr::local_tempfile(fileext = ".html")
  writeLines(br_page(3L), p)
  expect_identical(readgpt:::extract_html(p, list())$text, "line 1\nline 2\nline 3")
})

# ---------------------------------------------------------------------------
# ingest-8: a document-scoped cleaner that cut the text inside a line fell
# back to running on each block, where the cut never fired.
# ---------------------------------------------------------------------------

test_that("a document-scoped cut inside a line keeps the blocks before it and drops the rest", {
  local_registries()
  gr_register_cleaner("cut_refs", scope = "document",
                      fn = function(x, o) sub("(?s)References\\s*\n.*$", "", x, perl = TRUE))
  x <- c("1 Introduction", "Body of the intro, long enough.", "Some results here.",
         "6 References", "[1] Smith J. A paper. 2020.", "[2] Doe J. Another paper. 2021.")
  # Before: a gr_clean_unmapped warning, and all six blocks kept.
  expect_no_warning(out <- gr_clean(x, steps = "cut_refs"))
  expect_identical(as.character(out), c(x[1:3], "6 ", "", ""))

  # A cut in the middle of a block's line keeps that block up to the cut.
  gr_register_cleaner("cut_word", scope = "document",
                      fn = function(x, o) sub("(?si)see references.*$", "", x, perl = TRUE))
  expect_no_warning(out <- gr_clean(c("Intro text.", "More text.\nMethods, see references below.",
                                      "[1] A."), steps = "cut_word"))
  expect_identical(as.character(out), c("Intro text.", "More text.\nMethods, ", ""))

  # Through ingestion: the bibliography is not sent on.
  d <- quiet(gr_ingest(paste(x, collapse = "\n\n"),
                       gr_ingest_spec(clean = c("cut_refs", "collapse_whitespace")),
                       cache = FALSE))
  expect_false(grepl("Smith", d$text, fixed = TRUE))
  expect_match(d$text, "Some results here.", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# ingest-9: the hyphenation step stopped rejoining words split in capitals,
# and words split at a Windows line end.
# ---------------------------------------------------------------------------

test_that("hyphenation rejoins words split in capitals and at CRLF line ends", {
  hy <- function(x) as.character(gr_clean(x, steps = "hyphenation"))
  # Before: all three were left split.
  expect_identical(hy("IN NO EVENT SHALL THE AUTHORS BE LIA-\nBLE FOR ANY CLAIM"),
                   "IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM")
  expect_identical(hy("LIMITATION OF LIA-\nBILITY. In no event"),
                   "LIMITATION OF LIABILITY. In no event")
  expect_identical(hy("mito-\r\nchondria"), "mitochondria")
  expect_identical(hy("TO HOLD HARM-\r\nLESS AND INDEM-\nNIFY THE"),
                   "TO HOLD HARMLESS AND INDEMNIFY THE")
  expect_identical(hy("ÉCHÉANCE DE LA RESPONSA-\nBILITÉ ÉTENDUE"),
                   "ÉCHÉANCE DE LA RESPONSABILITÉ ÉTENDUE")

  # In mixed-case text, capitals either side of the break are two
  # abbreviations, and a capital after it a compound; both are left alone, as
  # are number ranges and a break across a blank line.
  for (x in c("the HIV-\nAIDS epidemic", "The WHO-\nUNICEF report", "US-\nAmerican relations",
              "Anglo-\nSaxon", "aged 18-\r\n65 years", "SEE LIA-\n\nBLE")) {
    expect_identical(hy(x), x)
  }
  expect_identical(readgpt:::rejoin_hyphenated(c("well-\nknown", NA, "LIA-\nBLE")),
                   c("wellknown", NA, "LIABLE"))
})

# ---------------------------------------------------------------------------
# r-semantics-13: a cleaner or extractor registered again was still served
# from the ingest cache when its code called a global helper that had changed.
# ---------------------------------------------------------------------------

test_that("registering a cleaner or extractor again starts a new cache entry", {
  local_registries()
  local_clean_cache()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("Introduction paragraph with enough characters to pass the filter.", "",
               "CONFIDENTIAL: patient details here.", "",
               "Another normal paragraph of text that is long enough to stay."), f)
  has_conf <- function(spec) grepl("CONFIDENTIAL", quiet(gr_ingest(f, spec))$text, fixed = TRUE)

  # The same code, calling a helper that was fixed in between. Before: the
  # second ingest came from the cache, CONFIDENTIAL and all.
  env <- new.env()
  env$helper <- function(x) x
  cleaner <- local(function(x, o) helper(x), env)
  gr_register_cleaner("dc_helper", cleaner)
  expect_true(has_conf(gr_ingest_spec(clean = "dc_helper")))
  env$helper <- function(x) gsub("(?mi)^CONFIDENTIAL.*$", "", x, perl = TRUE)
  gr_register_cleaner("dc_helper", cleaner)
  expect_false(has_conf(gr_ingest_spec(clean = "dc_helper")))

  # A pattern read from outside the function.
  env$pat <- "TYPO"
  cleaner <- local(function(x, o) gsub(pat, "", x, perl = TRUE), env)
  gr_register_cleaner("dc_pat", cleaner)
  expect_true(has_conf(gr_ingest_spec(clean = "dc_pat")))
  env$pat <- "(?mi)^CONFIDENTIAL.*$"
  gr_register_cleaner("dc_pat", cleaner)
  expect_false(has_conf(gr_ingest_spec(clean = "dc_pat")))

  # An extractor whose parser is a helper.
  z <- withr::local_tempfile(fileext = ".zzz")
  writeLines("raw content", z)
  env$parse_zzz <- function(path) "OLD parser output, long enough to keep."
  ex <- local(function(path, opts) data.frame(text = parse_zzz(path)), env)
  gr_register_extractor("zzz", "zzz", ex)
  expect_match(quiet(gr_ingest(z))$text, "OLD", fixed = TRUE)
  env$parse_zzz <- function(path) "NEW parser output, long enough to keep."
  gr_register_extractor("zzz", "zzz", ex)
  expect_match(quiet(gr_ingest(z))$text, "NEW", fixed = TRUE)

  # Nothing registered in between: the cache still answers.
  gr_options(verbose = TRUE)          # restored by local_registries()
  expect_message(gr_ingest(z), "cached", fixed = TRUE)
})
