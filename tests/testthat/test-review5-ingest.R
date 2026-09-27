# test-review5-ingest.R -- the fifth pass on ingestion: regressions that the
# second verification found in the earlier fixes.
#
# Each block names the finding and says what the earlier fix did wrong.

r5_html_blocks <- function(html) {
  p <- withr::local_tempfile(fileext = ".html")
  writeLines(paste0("<html><body>", html, "</body></html>"), p)
  readgpt:::extract_html(p, list())
}

r5_hy <- function(x) as.character(gr_clean(x, steps = "hyphenation"))

# ---------------------------------------------------------------------------
# ingest-9 / ingest-4: rejoining words split in capitals took time in
# proportion to the length of the block times the number of breaks in it.
# ---------------------------------------------------------------------------

test_that("rejoining hyphenated words takes time in proportion to the block", {
  skip_on_cran()
  unit <- paste0("IN NO EVENT SHALL THE AUTHORS BE LIA-\nBLE FOR ANY CLAIM, the HIV-\n",
                 "AIDS epidemic and mito-\nchondria. \u2019")
  timed <- function(k, reps) {
    x <- paste(rep(unit, k), collapse = " ")
    min(vapply(seq_len(reps), function(i) {
      system.time(readgpt:::rejoin_hyphenated(x))[["elapsed"]]
    }, numeric(1)))
  }
  # Eight times the text. Before: some 60 times as long (8,000 units: 20 s;
  # a 2 MB block took a minute, against 0.05 s at the first release).
  small <- timed(1000L, reps = 3L)
  expect_lt(timed(8000L, reps = 2L) / max(small, 0.01), 20)

  # And every join is still made, and every pair in prose kept.
  y <- readgpt:::rejoin_hyphenated(paste(rep(unit, 500L), collapse = " "))
  expect_identical(lengths(regmatches(y, gregexpr("LIABLE", y, fixed = TRUE))), 500L)
  expect_identical(lengths(regmatches(y, gregexpr("HIV-\nAIDS", y, fixed = TRUE))), 500L)
  expect_identical(lengths(regmatches(y, gregexpr("mitochondria", y, fixed = TRUE))), 500L)
})

# ---------------------------------------------------------------------------
# ingest-3 (first listing): one abbreviation beside a pair in mixed-case prose
# was taken for text set in capitals, and the pair was joined.
# ---------------------------------------------------------------------------

test_that("abbreviation compounds in mixed-case prose are not joined", {
  # Before: "LCMS/MS", "PETCT", "MALDITOF", "A PETCT scan", "A USUK", "HIVAIDS",
  # "NSFDFG", "PISATIMSS", and "USUK" after a heading in capitals.
  for (x in c("Plasma samples were analysed by LC-\nMS/MS using a C18 column.",
              "Patients underwent 18F-FDG PET-\nCT imaging",
              "Spectra were acquired by MALDI-\nTOF MS in positive mode.",
              "A PET-\nCT scan was performed.",
              "A US-\nUK trade deal",
              "In Phase II HIV-\nAIDS trials",
              "funded by NIH NSF-\nDFG grants",
              "results from OECD PISA-\nTIMSS comparisons",
              "RESULTS\nUS-\nUK trade grew in 2019.",
              "the HIV-\nAIDS epidemic", "The WHO-\nUNICEF report", "the NATO-\nRUSSIA council")) {
    expect_identical(r5_hy(x), x)
  }

  # Text set in capitals is still rejoined, a side or both of it in capitals.
  expect_identical(r5_hy("THE COMPANY SHALL NOT BE LIA-\nBLE FOR ANY DAMAGES"),
                   "THE COMPANY SHALL NOT BE LIABLE FOR ANY DAMAGES")
  expect_identical(r5_hy("LIMITATION OF LIA-\nBILITY. In no event"),
                   "LIMITATION OF LIABILITY. In no event")
  expect_identical(r5_hy("Some text here.\nLIA-\nBILITY OF THE PARTIES"),
                   "Some text here.\nLIABILITY OF THE PARTIES")
  expect_identical(r5_hy("TOTAL LIA-\nBILITY"), "TOTAL LIABILITY")
  expect_identical(r5_hy("Le CONTRAT DE LI-\nCENCE"), "Le CONTRAT DE LICENCE")
  expect_identical(r5_hy("A NON-\nEXCLUSIVE LICENCE"), "A NONEXCLUSIVE LICENCE")
  expect_identical(r5_hy("L'\u00c9TAT FRAN-\n\u00c7AIS"), "L'\u00c9TAT FRAN\u00c7AIS")
  # A heading on lines of its own is rejoined, as the first release did.
  # Before: left split, because the next line was mixed case.
  expect_identical(r5_hy("8. INDEMNI-\nFICATION\nEach party shall indemnify"),
                   "8. INDEMNIFICATION\nEach party shall indemnify")
  # A break at every line of a run is rejoined at every line.
  expect_identical(r5_hy("TO HOLD HARM-\r\nLESS AND INDEM-\nNIFY THE"),
                   "TO HOLD HARMLESS AND INDEMNIFY THE")
  expect_identical(r5_hy("AB-\nCD-\nEF"), "ABCDEF")
  # A lone word of capitals in prose looks like an abbreviation pair and is
  # left split; the trade-off is documented.
  expect_identical(r5_hy("Please read CARE-\nFULLY before signing"),
                   "Please read CARE-\nFULLY before signing")
  # The text around the pairs is left exactly as it was.
  expect_identical(r5_hy("\nLIA-\nBLE\n\n"), "\nLIABLE\n\n")
  expect_identical(r5_hy("mito-\nchondria at 10-\n20 C"), "mitochondria at 10-\n20 C")

  # Through ingestion, with the default cleaning.
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("Plasma samples were analysed by LC-", "MS/MS using a C18 column. A PET-",
               "CT scan was done in Phase II HIV-", "AIDS trials.", "",
               "THE LICENSOR SHALL NOT BE LIA-", "BLE FOR ANY CLAIM."), f)
  d <- quiet(gr_ingest(f, cache = FALSE))
  expect_match(d$text, "by LC-\nMS/MS", fixed = TRUE)
  expect_match(d$text, "A PET-\nCT scan", fixed = TRUE)
  expect_match(d$text, "Phase II HIV-\nAIDS", fixed = TRUE)
  expect_match(d$text, "BE LIABLE FOR ANY CLAIM", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# ingest-2 / ingest-3 / ingest-04: any heading in a cell made a data table a
# layout, and so did a single row whose cells held a <p>; and a layout of
# several rows without headings ran its paragraphs together.
# ---------------------------------------------------------------------------

test_that("a data table with headings in its cells keeps its rows and the section around it", {
  skip_if_not_installed("xml2")
  # A header row styled as headings. Before: four heading blocks, then every
  # value a block of its own, and the paragraph after the table filed under
  # "HR (95% CI)".
  b <- r5_html_blocks(paste0(
    "<h2>Results</h2><p>Table 2 gives the outcomes.</p>",
    "<table><tr><td><h4>Arm</h4></td><td><h4>Patients</h4></td><td><h4>Deaths</h4></td>",
    "<td><h4>HR (95% CI)</h4></td></tr>",
    "<tr><td>Placebo</td><td>412</td><td>61</td><td>1.00</td></tr>",
    "<tr><td>Drug A</td><td>409</td><td>38</td><td>0.61 (0.41-0.92)</td></tr></table>",
    "<p>Mortality was lower with drug A.</p>"))
  expect_identical(b$text, c("Results", "Table 2 gives the outcomes.",
                             "Arm | Patients | Deaths | HR (95% CI)", "Placebo | 412 | 61 | 1.00",
                             "Drug A | 409 | 38 | 0.61 (0.41-0.92)",
                             "Mortality was lower with drug A."))
  expect_identical(b$kind, c("heading", "body", "table", "table", "table", "body"))
  expect_identical(b$section, rep("Results", 6L))

  # Rows that label a group. Before: "Event", "Drug (n=120)", ... "3", "9"
  # each a block, and "Non-serious" the section of what followed.
  b <- r5_html_blocks(paste0(
    "<h2>Table 2. Adverse events</h2><table><tr><th>Event</th><th>Drug (n=120)</th>",
    "<th>Placebo (n=118)</th><th>p</th></tr><tr><td colspan=\"4\"><h4>Serious</h4></td></tr>",
    "<tr><td>Death</td><td>3</td><td>9</td><td>0.07</td></tr>",
    "<tr><td colspan=\"4\"><h4>Non-serious</h4></td></tr>",
    "<tr><td>Headache</td><td>22</td><td>15</td><td>0.3</td></tr></table><p>After.</p>"))
  expect_identical(b$text, c("Table 2. Adverse events", "Event | Drug (n=120) | Placebo (n=118) | p",
                             "Serious", "Death | 3 | 9 | 0.07", "Non-serious",
                             "Headache | 22 | 15 | 0.3", "After."))
  expect_identical(b$kind, c("heading", rep("table", 5L), "body"))
  expect_identical(unique(b$section), "Table 2. Adverse events")

  # Row labels as headings, and a heading in a table inside a cell.
  b <- r5_html_blocks(paste0("<h2>Specs</h2><table><tr><td><h5>Speed</h5></td><td>10 ms</td></tr>",
                             "<tr><td><h5>Weight</h5></td><td>2 kg</td></tr></table><p>After.</p>"))
  expect_identical(b$text, c("Specs", "Speed | 10 ms", "Weight | 2 kg", "After."))
  expect_identical(unique(b$section), "Specs")
  b <- r5_html_blocks(paste0("<table><tr><th>Item</th><th>Detail</th></tr><tr><td>A</td><td>",
                             "<table><tr><td><h3>Sub</h3></td></tr></table></td></tr>",
                             "<tr><td>B</td><td>2</td></tr></table><p>After.</p>"))
  expect_identical(b$text, c("Item | Detail", "A | ", "Sub", "B | 2", "After."))
  expect_false("heading" %in% b$kind)
  # A table of one cell that holds only a heading is still the page's heading
  # when it lays out the page itself.
  b <- r5_html_blocks(paste0("<table><tr><td><h1>Newsletter</h1></td></tr></table>",
                             "<p>This month's news.</p>"))
  expect_identical(b$kind, c("heading", "body"))
  expect_identical(b$section, c("Newsletter", "Newsletter"))
})

test_that("a single row of values stays a row, whatever wraps the values", {
  skip_if_not_installed("xml2")
  # Before: "Name" and "Value 42", two body blocks.
  b <- r5_html_blocks("<table><tr><td><p>Name</p></td><td><p>Value 42</p></td></tr></table>")
  expect_identical(b$text, "Name | Value 42")
  expect_identical(b$kind, "table")
  # R's help pages set the arguments of a one-argument function this way.
  b <- r5_html_blocks(paste0("<h3>Arguments</h3><table role=\"presentation\"><tr>",
                             "<td><code id=\"x\">x</code></td><td><p>an R object.</p></td></tr>",
                             "</table>"))
  expect_identical(b$text, c("Arguments", "x | an R object."))
  expect_identical(r5_html_blocks(paste0("<table><tr><td>Symptoms</td><td><ul><li>fever</li>",
                                         "<li>cough</li></ul></td></tr></table>"))$text,
                   "Symptoms | fever cough")
  expect_identical(r5_html_blocks(paste0("<table><tr><td>Label</td></tr><tr><td><div>12</div>",
                                         "</td></tr></table>"))$text, c("Label", "12"))
  # A cell of a data grid may hold two paragraphs, or a heading with a line
  # under it, and still be a value.
  b <- r5_html_blocks(paste0("<table><tr><th>Plan</th><th>Price</th></tr><tr><td><h4>Basic</h4>",
                             "<p>For one person</p></td><td>$10</td></tr><tr><td><h4>Pro</h4>",
                             "<p>For teams</p></td><td>$20</td></tr></table>"))
  expect_identical(b$text, c("Plan | Price", "Basic For one person | $10", "Pro For teams | $20"))
  b <- r5_html_blocks(paste0("<table><tr><td colspan=3><p>Table 1</p></td></tr><tr><td><p>Outcome",
                             "</p></td><td><p>Drug</p></td><td><p>Placebo</p></td></tr><tr><td>",
                             "<p>Death</p><p>(all cause)</p></td><td><p>3</p></td><td><p>9</p></td>",
                             "</tr></table>"))
  expect_identical(b$text, c("Table 1", "Outcome | Drug | Placebo", "Death (all cause) | 3 | 9"))
})

test_that("a layout of several rows keeps its paragraphs apart, with or without headings", {
  skip_if_not_installed("xml2")
  # A header row, a navigation and content row, and a footer row. Before:
  # "Home About | First paragraph. Second paragraph." as one table block.
  b <- r5_html_blocks(paste0("<table><tr><td colspan=\"2\">Logo</td></tr><tr><td>Home<br>About</td>",
                             "<td><p>First paragraph.</p><p>Second paragraph.</p></td></tr>",
                             "<tr><td colspan=\"2\">Copyright</td></tr></table>"))
  expect_identical(b$text, c("Logo", "Home\nAbout", "First paragraph.", "Second paragraph.",
                             "Copyright"))
  expect_false("table" %in% b$kind)

  # An older site with <font> headings.
  b <- r5_html_blocks(paste0(
    "<table><tr><td colspan=\"2\"><font size=\"+3\">My Site</font></td></tr><tr><td>",
    "<a href=\"/\">Home</a><br><a href=\"/a\">About</a></td><td><font size=\"+2\"><b>Welcome",
    "</b></font><p>First para of welcome.</p><p>Second para.</p><font size=\"+2\"><b>News</b>",
    "</font><p>News para.</p></td></tr><tr><td colspan=\"2\">(c) 1999</td></tr></table>"))
  expect_identical(b$text, c("My Site", "Home\nAbout", "Welcome", "First para of welcome.",
                             "Second para.", "News", "News para.", "(c) 1999"))

  # An HTML e-mail: a wrapper cell, then a table with a two-column row.
  b <- r5_html_blocks(paste0(
    "<table width=\"100%\"><tr><td align=\"center\"><table width=\"600\"><tr><td colspan=\"2\">",
    "<img src=\"logo.png\" alt=\"Logo\"></td></tr><tr><td><img src=\"a.png\"></td><td><p><b>Big ",
    "news</b></p><p>We launched a product.</p></td></tr><tr><td colspan=\"2\"><p>Dear customer,",
    "</p><p>Thanks for being with us this year.</p></td></tr><tr><td colspan=\"2\">Unsubscribe",
    "</td></tr></table></td></tr></table>"))
  expect_identical(b$text, c("Big news", "We launched a product.", "Dear customer,",
                             "Thanks for being with us this year.", "Unsubscribe"))
  expect_false("table" %in% b$kind)

  # A header row of cells is a data table's, not a layout's.
  b <- r5_html_blocks(paste0("<table><tr><th colspan=2>Contact</th></tr><tr><td>Address</td>",
                             "<td><p>1 Main St</p><p>Springfield</p></td></tr></table>"))
  expect_identical(b$text, c("Contact", "Address | 1 Main St Springfield"))
})

test_that("deciding whether a table lays out the page takes time in proportion to it", {
  skip_on_cran()
  skip_if_not_installed("xml2")
  timed <- function(html, reps = 1L) {
    p <- withr::local_tempfile(fileext = ".html")
    writeLines(paste0("<html><body>", html, "</body></html>"), p)
    min(vapply(seq_len(reps), function(i) {
      system.time(readgpt:::extract_html(p, list()))[["elapsed"]]
    }, numeric(1)))
  }
  column <- function(n) paste0("<table>", paste0("<tr><td><p>row ", seq_len(n), "</p></td></tr>",
                                                 collapse = ""), "</table>")
  labelled <- function(n) {
    paste0("<table><tr><th>A</th><th>B</th></tr>",
           paste0("<tr><td><h4>r", seq_len(n), "</h4></td><td>", seq_len(n), "</td></tr>",
                  collapse = ""), "</table>")
  }
  # Eight times the rows. Telling cells apart by their path in the page is
  # quadratic in the rows: libxml2 counts the siblings before each one.
  small <- timed(column(2000L), reps = 3L)
  expect_lt(timed(column(16000L), reps = 2L) / max(small, 0.01), 20)
  small <- timed(labelled(2000L), reps = 3L)
  expect_lt(timed(labelled(16000L), reps = 2L) / max(small, 0.01), 20)
})

# ---------------------------------------------------------------------------
# ingest-8: a document-scoped cleaner that removed whole lines AND cut the
# text short inside a line fell back to running on each block.
# ---------------------------------------------------------------------------

test_that("a document-scoped cleaner that drops lines and cuts inside one is matched back", {
  local_registries()
  gr_register_cleaner("tidy_paper", scope = "document", function(x, o) {
    x <- gsub("(?m)^Page \\d+ of \\d+\n", "", x, perl = TRUE)
    sub("(?s)References\\s*\n.*$", "", x, perl = TRUE)
  })
  blocks <- c("1 Introduction\nWe study X.", "Page 1 of 3", "2 Methods\nWe did Y.", "Page 2 of 3",
              "6 References", "[1] Smith 2020.", "Page 3 of 3", "[2] Doe 2021.")
  # Before: a gr_clean_unmapped warning, all eight blocks kept, the page lines
  # and the bibliography with them.
  expect_no_warning(out <- gr_clean(blocks, steps = "tidy_paper"))
  expect_identical(as.character(out), c(blocks[1], "", blocks[3], "", "6 ", "", "", ""))

  # A section taken out of the middle, from inside a line to a later line.
  gr_register_cleaner("drop_ack", scope = "document", function(x, o) {
    sub("(?s)Acknowledgements\\s*\n.*?(?=\n\\d+ References)", "", x, perl = TRUE)
  })
  b2 <- c("4 Discussion", "Text of discussion.", "5 Acknowledgements", "We thank A.", "We thank B.",
          "6 References", "[1] X.")
  expect_no_warning(out <- gr_clean(b2, steps = "drop_ack"))
  expect_identical(as.character(out), c(b2[1:2], "5 ", "", "", b2[6:7]))

  # A kept line is matched to the same line, not to the start of a line the
  # step dropped that begins the same way.
  gr_register_cleaner("rh_cut", scope = "document", function(x, o) {
    x <- gsub("(?m)^Results of the trial\n", "", x, perl = TRUE)
    sub("(?s)References\\s*\n.*$", "", x, perl = TRUE)
  })
  expect_no_warning(out <- gr_clean(c("Results of the trial\nIntro.", "Results", "Body.",
                                      "6 References", "[1] A."), steps = "rh_cut"))
  expect_identical(as.character(out), c("Intro.", "Results", "Body.", "6 ", ""))

  # Rewriting text inside lines is still not passed off as a match.
  gr_register_cleaner("cut_upper", scope = "document", function(x, o) {
    toupper(sub("(?s)References\\s*\n.*$", "", x, perl = TRUE))
  })
  expect_warning(gr_clean(blocks, steps = "cut_upper"), class = "gr_clean_unmapped")
})
