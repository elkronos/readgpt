# test-review6-extractors.R
#
# Regression tests for the medium and low findings in the extractors
# (R/ingest-extract.R, R/ingest-pdf.R, R/ingest-url.R, R/ingest-docx.R) left
# after the earlier review passes: Word equations, Markdown headings and code
# chunks, PDF pages that were never read, PDF headings found inside
# paragraphs or missed in IEEE papers, words hyphenated across PDF blocks,
# page encodings declared only by the server, UTF-16 text, Word files that
# unpack to gigabytes, and downloads with no size limit or destination check.

r6_w_ns <- paste0('xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
                  'xmlns:m="http://schemas.openxmlformats.org/officeDocument/2006/math"')

# A Word file built from its document part, with any other entries given as
# name = content (a character string, or raw bytes).
r6_docx <- function(body, extra = list(), env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(dir, "word"))
  writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
                    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
                    '<Default Extension="xml" ContentType="application/xml"/></Types>'),
             file.path(dir, "[Content_Types].xml"))
  writeLines(sprintf(paste0('<?xml version="1.0" encoding="UTF-8"?>',
                            '<w:document %s><w:body>%s</w:body></w:document>'),
                     r6_w_ns, paste(body, collapse = "")), file.path(dir, "word", "document.xml"))
  for (nm in names(extra)) {
    dir.create(dirname(file.path(dir, nm)), recursive = TRUE, showWarnings = FALSE)
    if (is.raw(extra[[nm]])) writeBin(extra[[nm]], file.path(dir, nm))
    else writeLines(extra[[nm]], file.path(dir, nm))
  }
  out <- withr::local_tempfile(fileext = ".docx", .local_envir = env)
  withr::with_dir(dir, utils::zip(out, files = list.files(".", recursive = TRUE, all.files = TRUE),
                                  flags = "-q"))
  out
}

r6_skip_docx <- function() {
  skip_if_not_installed("xml2")
  skip_if(!nzchar(Sys.which("zip")), "no zip program to build a Word file with")
}

r6_wp <- function(...) paste0("<w:p>", ..., "</w:p>")
r6_wr <- function(text) sprintf('<w:r><w:t xml:space="preserve">%s</w:t></w:r>', text)
r6_mr <- function(text) sprintf("<m:r><m:t>%s</m:t></m:r>", text)

r6_blocks <- function(f, ...) {
  quiet(gr_ingest(f, gr_ingest_spec(clean = "none", ...), cache = FALSE))$blocks
}

# ---------------------------------------------------------------------------
# ingest-10: equations in a Word file
# ---------------------------------------------------------------------------

test_that("an equation in a Word paragraph is read in its place", {
  r6_skip_docx()
  f <- r6_docx(c(
    r6_wp(r6_wr("The pooled estimate was "),
          "<m:oMath>", r6_mr("OR = 0.62 (95% CI 0.48 to 0.80)"), "</m:oMath>",
          r6_wr(" across all trials.")),
    # A display equation on a paragraph of its own.
    r6_wp("<m:oMathPara><m:oMath>", r6_mr("p"), r6_mr(" = 0.003"), "</m:oMath></m:oMathPara>"),
    r6_wp(r6_wr("Heterogeneity was moderate."))))
  b <- r6_blocks(f)
  expect_identical(b$text, c("The pooled estimate was OR = 0.62 (95% CI 0.48 to 0.80) across all trials.",
                             "p = 0.003", "Heterogeneity was moderate."))
})

test_that("an equation's structure is written out, so its figures keep their meaning", {
  r6_skip_docx()
  frac <- paste0("<m:f><m:num>", r6_mr("1"), "</m:num><m:den>", r6_mr("2"), "</m:den></m:f>")
  sup <- paste0("<m:sSup><m:e>", r6_mr("I"), "</m:e><m:sup>", r6_mr("2"), "</m:sup></m:sSup>")
  sub <- paste0("<m:sSub><m:e>", r6_mr("x"), "</m:e><m:sub>", r6_mr("i+1"), "</m:sub></m:sSub>")
  root <- paste0("<m:rad><m:radPr><m:degHide m:val=\"1\"/></m:radPr><m:deg/><m:e>",
                 r6_mr("n"), "</m:e></m:rad>")
  f <- r6_docx(c(
    r6_wp(r6_wr("Half is "), "<m:oMath>", frac, "</m:oMath>", r6_wr(".")),
    r6_wp("<m:oMathPara><m:oMath>", sup, r6_mr(" = 48%"), "</m:oMath><m:oMath>", sub,
          "</m:oMath></m:oMathPara>"),
    r6_wp(r6_wr("Scale by "), "<m:oMath>", root, "</m:oMath>",
          # Text a tracked change deleted is not read.
          "<m:oMath>", r6_mr("+1"), '<w:del w:id="1" w:author="a"><m:r><m:t>999</m:t></m:r></w:del>',
          "</m:oMath>")))
  b <- r6_blocks(f)
  # Joined without structure these read "12", "I2" and "xi+1".
  expect_identical(b$text, c("Half is 1/2.", "I^2 = 48% x_(i+1)", "Scale by sqrt(n) +1"))
})

# ---------------------------------------------------------------------------
# ingest-13: Markdown headings and code chunks
# ---------------------------------------------------------------------------

test_that("a Markdown heading with text on the next line starts its section", {
  f <- withr::local_tempfile(fileext = ".Rmd")
  writeLines(c("# Report", "", "Intro text for the report.", "",
               "## Methods", "We randomised 200 patients.", "",
               "## Results", "Mortality fell by 7 points.", "",
               "```{r}", "x <- 1", "", "# Fit the model", "", "fit <- lm(y ~ x)", "```", "",
               "~~~", "# not a heading either", "~~~", "",
               "Closing remarks."), f)
  b <- r6_blocks(f)
  expect_identical(b$kind, c("heading", "body", "heading", "body", "heading", "body", "code",
                             "code", "body"))
  expect_identical(b$section, c("Report", "Report", "Methods", "Methods", "Results", "Results",
                                "Results", "Results", "Results"))
  expect_identical(b$text[4], "We randomised 200 patients.")
  # The chunk is one block, blank lines and all.
  expect_identical(b$text[7], "```{r}\nx <- 1\n\n# Fit the model\n\nfit <- lm(y ~ x)\n```")
})

test_that("a fence left open runs to the end, and a heading can interrupt a paragraph", {
  f <- withr::local_tempfile(fileext = ".md")
  writeLines(c("Some text before.", "## Heading here ##", "Text after.", "", "```",
               "# still code"), f)
  b <- r6_blocks(f)
  expect_identical(b$text, c("Some text before.", "## Heading here ##", "Text after.",
                             "```\n# still code"))
  expect_identical(b$section, c(NA, "Heading here", "Heading here", "Heading here"))
  expect_identical(b$kind, c("body", "heading", "body", "code"))
})

# ---------------------------------------------------------------------------
# ingest-07: PDF pages never read
# ---------------------------------------------------------------------------

r6_scans_pdf <- function(stamp = TRUE, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".pdf", .local_envir = env)
  grDevices::pdf(f, width = 8.5, height = 11)
  graphics::plot.new()
  graphics::text(0, 0.9, "Cover letter: the attached pages are the signed clinical study report.",
                 adj = 0)
  for (i in 2:4) {
    graphics::plot.new()
    graphics::plot.window(c(0, 1), c(0, 1))
    graphics::rasterImage(matrix(seq(0, 1, length.out = 100), 10), 0.1, 0.1, 0.9, 0.9)
    if (stamp) graphics::text(0.9, 0.02, sprintf("ACME-00100%d", i), adj = 1, cex = 0.6)
  }
  invisible(grDevices::dev.off())
  f
}

test_that("scanned pages with only a stamp in their text layer count as unread", {
  skip_if_not_installed("pdftools")
  skip_if(requireNamespace("tesseract", quietly = TRUE) && requireNamespace("magick", quietly = TRUE),
          "OCR is installed, so the pages would be read")
  local_clean_cache()
  f <- r6_scans_pdf()
  expect_warning(doc <- suppressMessages(gr_ingest(f, cache = FALSE)),
                 class = "gr_ocr_unavailable")
  expect_identical(doc$stats$unread_pages, 2:4)
  expect_identical(doc$stats$pages, 4L)
  a <- quiet(gr_read(gr_segment(doc), "What does the report conclude?",
                     mock_echo("The report is signed."), "stuff"))
  expect_true(a$partial)
})

test_that("pages with no text are unread under ocr = 'never' too", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  f <- r6_scans_pdf(stamp = FALSE)
  doc <- quiet(gr_ingest(f, gr_ingest_spec(ocr = "never"), cache = FALSE))
  expect_identical(doc$stats$unread_pages, 2:4)
  expect_identical(doc$stats$pages, 4L)
  # The same with the page layout left as it is.
  raw <- quiet(gr_ingest(f, gr_ingest_spec(ocr = "never", layout = "raw"), cache = FALSE))
  expect_identical(raw$stats$unread_pages, 2:4)
})

test_that("a page with a short line of text is still read, not unread", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  f <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 8.5, height = 11)
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Annual Report 2024")
  graphics::plot.new()
  graphics::text(0, 0.9, "Revenue was 45.2 million dollars in fiscal 2024 across the group.", adj = 0)
  invisible(grDevices::dev.off())
  doc <- quiet(gr_ingest(f, cache = FALSE))
  expect_length(doc$stats$unread_pages, 0L)
  expect_match(doc$text, "Annual Report 2024", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# ingest-09: a paragraph's last line taken for a heading
# ---------------------------------------------------------------------------

test_that("a paragraph's last line is not a heading, even when it is a section name", {
  m <- readgpt:::heading_matcher()
  expect_true(is.na(m("findings.")))
  expect_true(is.na(m("Results,")))
  expect_identical(m("Findings"), "Findings")
  expect_identical(m("Results:"), "Results:")
  # A bookmark that ends in a question mark still matches its line.
  q <- readgpt:::heading_matcher(c("Why does it matter?"))
  expect_identical(q("Why does it matter?"), "Why does it matter?")

  page <- c("Introduction", "",
            "We studied 400 adults in a randomised trial and compare the estimates with earlier",
            "findings.", "The trial ran in 12 centres.", "",
            "We also looked at the change in the rate of", "findings", "over the two years.", "",
            "Methods", "Participants were randomised centrally.")
  b <- readgpt:::pdf_page_blocks(list(page), readgpt:::heading_matcher())
  expect_identical(b$kind, c("heading", "body", "body", "heading", "body"))
  expect_identical(b$section, c("Introduction", "Introduction", "Introduction", "Methods", "Methods"))
  expect_match(b$text[2], "with earlier\nfindings.\nThe trial", fixed = TRUE)
})

test_that("a PDF whose paragraph wraps onto a section name keeps one section", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  f <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 8.5, height = 11)
  graphics::plot.new()
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.window(c(0, 1), c(0, 1))
  y <- 0.95
  for (l in c("Introduction", "",
              "We studied 400 adults in a randomised trial and compare the estimates with earlier",
              "findings.", "The trial ran in 12 centres and enrolled patients over two years.", "",
              "Methods", "", "Participants were randomised by a central computer system.")) {
    graphics::text(0.05, y, l, adj = 0)
    y <- y - 0.025
  }
  invisible(grDevices::dev.off())
  b <- quiet(gr_ingest(f, gr_ingest_spec(clean = "none"), cache = FALSE))$blocks
  expect_identical(b$section[b$kind == "heading"], c("Introduction", "Methods"))
  expect_false("findings." %in% b$section)
})

# ---------------------------------------------------------------------------
# r3-real-pdf-two-column-layout-06: IEEE-style headings
# ---------------------------------------------------------------------------

test_that("lettered subsections and small-caps titles match the PDF's bookmarks", {
  titles <- c("Introduction", "Methods", "Participants", "Outcomes", "Discussion", "Conclusion")
  m <- readgpt:::heading_matcher(titles)
  expect_identical(m("I. I NTRODUCTION"), "Introduction")
  expect_identical(m("II. M ETHODS"), "Methods")
  expect_identical(m("A. Participants"), "Participants")
  expect_identical(m("C. Outcomes"), "Outcomes")
  expect_identical(m("IV. D ISCUSSION"), "Discussion")
  expect_identical(m("V. C ONCLUSION"), "Conclusion")
  expect_true(is.na(m("A. Smith and B. Jones")))
  # Without bookmarks, a small-caps standard name is still a heading.
  plain <- readgpt:::heading_matcher()
  expect_identical(plain("IV. D ISCUSSION"), "IV. D ISCUSSION")
  expect_identical(plain("B. Methods"), "B. Methods")
})

# ---------------------------------------------------------------------------
# r3-real-pdf-two-column-layout-08: words hyphenated across blocks
# ---------------------------------------------------------------------------

test_that("a word hyphenated across a page break or a spaced-out column is rejoined", {
  pages <- list(
    c("Participants were allocated to the multi-"),
    c("component intervention. It ran for a year.", "", "Outcomes were adjusted for charac-", "",
      "teristics of the sites."))
  b <- readgpt:::pdf_page_blocks(pages, readgpt:::heading_matcher())
  # The rest of the word moves back to its page; the rest of the text keeps
  # its own.
  expect_identical(b$text, c("Participants were allocated to the multi-\ncomponent",
                             "intervention. It ran for a year.",
                             "Outcomes were adjusted for charac-\nteristics of the sites."))
  expect_identical(b$page, c(1L, 2L, 2L))
  # A hyphen before a capital is a compound, and is left.
  kept <- readgpt:::pdf_page_blocks(list("The Anglo-", c("Saxon charters survive.")),
                                    readgpt:::heading_matcher())
  expect_identical(kept$text, c("The Anglo-", "Saxon charters survive."))
  cleaned <- readgpt:::gr_clean(b$text, steps = "hyphenation")
  expect_identical(as.character(cleaned)[1], "Participants were allocated to the multicomponent")
})

# ---------------------------------------------------------------------------
# ingest-11: the charset a page or server declares
# ---------------------------------------------------------------------------

r6_latin <- "Patients in M\u00fcnchen received 5 \u00b5g \u2013 a \u201csmall\u201d dose."
r6_cp1252 <- function(x) iconv(x, "UTF-8", "CP1252", toRaw = TRUE)[[1]]

test_that("a Windows-1252 web page is read as such, with or without a meta tag", {
  skip_if_not_installed("xml2")
  local_clean_cache()
  page <- function(meta, body) {
    f <- withr::local_tempfile(fileext = ".html", .local_envir = parent.frame())
    writeBin(c(charToRaw(paste0("<html><head>", meta, "</head><body><p>")), body,
               charToRaw("</p></body></html>")), f)
    f
  }
  for (meta in c("", '<meta charset="windows-1252">',
                 '<meta http-equiv="Content-Type" content="text/html; charset=iso-8859-1">')) {
    doc <- quiet(gr_ingest(page(meta, r6_cp1252(r6_latin)), gr_ingest_spec(clean = "none"),
                           cache = FALSE))
    expect_identical(doc$text, r6_latin)
  }
  # A page saved as UTF-8 under an old latin1 declaration is read as UTF-8.
  utf8 <- page('<meta charset="iso-8859-1">', charToRaw(enc2utf8(r6_latin)))
  expect_identical(quiet(gr_ingest(utf8, gr_ingest_spec(clean = "none"), cache = FALSE))$text,
                   r6_latin)
})

test_that("the charset in a server's Content-Type is used for the page", {
  skip_if_not_installed("xml2")
  local_clean_cache()
  body <- withr::local_tempfile()
  writeBin(c(charToRaw("<p>"), r6_cp1252(r6_latin), charToRaw("</p>")), body)
  local_mocked_bindings(url_download = function(url, dest) {
    file.copy(body, dest, overwrite = TRUE)
    list(status = 200L, type = "text/html; charset=windows-1252")
  }, .package = "readgpt")
  doc <- quiet(gr_ingest("https://example.org/latin", gr_ingest_spec(clean = "none"),
                         cache = FALSE))
  expect_identical(doc$text, r6_latin)
})

# ---------------------------------------------------------------------------
# ingest-15: UTF-16 text
# ---------------------------------------------------------------------------

r6_table <- "Site\tN\nNorth\t212\nSouth\t270\nEast\t118\nWest\t99\n"

test_that("UTF-16 and UTF-32 text files are read, with or without a byte-order mark", {
  local_clean_cache()
  write_as <- function(bytes) {
    f <- withr::local_tempfile(fileext = ".txt", .local_envir = parent.frame())
    writeBin(bytes, f)
    f
  }
  enc <- function(x, to) iconv(x, "UTF-8", to, toRaw = TRUE)[[1]]
  files <- list(
    le_bom = write_as(c(as.raw(c(0xff, 0xfe)), enc(r6_table, "UTF-16LE"))),
    be_bom = write_as(c(as.raw(c(0xfe, 0xff)), enc(r6_table, "UTF-16BE"))),
    le = write_as(enc(r6_table, "UTF-16LE")),
    u32 = write_as(c(as.raw(c(0xff, 0xfe, 0x00, 0x00)), enc(r6_table, "UTF-32LE"))),
    utf8_bom = write_as(c(as.raw(c(0xef, 0xbb, 0xbf)), charToRaw(r6_table))))
  for (f in files) {
    doc <- quiet(gr_ingest(f, gr_ingest_spec(clean = "none"), cache = FALSE))
    expect_identical(doc$text, sub("\n$", "", r6_table))
  }
})

test_that("a download declared as UTF-16 text is read, not refused", {
  local_clean_cache()
  body <- withr::local_tempfile()
  writeBin(c(as.raw(c(0xff, 0xfe)), iconv(r6_table, "UTF-8", "UTF-16LE", toRaw = TRUE)[[1]]), body)
  expect_identical(readgpt:::url_extension("text/plain; charset=utf-16", "https://x.org/t", body),
                   "txt")
  local_mocked_bindings(url_download = function(url, dest) {
    file.copy(body, dest, overwrite = TRUE)
    list(status = 200L, type = "text/plain; charset=utf-16")
  }, .package = "readgpt")
  doc <- quiet(gr_ingest("https://example.org/utf16", gr_ingest_spec(clean = "none"),
                         cache = FALSE))
  expect_identical(doc$text, sub("\n$", "", r6_table))
})

# ---------------------------------------------------------------------------
# security-02: a Word file that unpacks to gigabytes
# ---------------------------------------------------------------------------

test_that("only the parts of a Word file that are read are unpacked, and within a limit", {
  r6_skip_docx()
  local_clean_cache()
  # 8 MB of zeros compresses to a few KB.
  f <- r6_docx(r6_wp(r6_wr("The trial enrolled 482 participants across nine sites.")),
               extra = list("word/media/junk.bin" = raw(8 * 1024^2),
                            "customXml/item1.xml" = "<a/>"))
  expect_lt(file.size(f), 1024^2)
  dir <- withr::local_tempdir()
  took <- readgpt:::docx_unzip(f, dir)
  expect_identical(took, "word/document.xml")
  expect_identical(list.files(dir, recursive = TRUE), "word/document.xml")
  expect_match(quiet(gr_ingest(f, cache = FALSE))$text, "482 participants", fixed = TRUE)
  # Text parts that unpack to more than the limit are refused.
  expect_error(readgpt:::docx_unzip(f, withr::local_tempdir(), max_bytes = 100),
               class = "gr_too_large")
  # Images past the limit are left out of OCR, with a warning.
  g <- r6_docx(r6_wp(r6_wr("Figure text.")),
               extra = list("word/media/a.png" = raw(2000), "word/media/b.png" = raw(2000)))
  d2 <- withr::local_tempdir()
  expect_warning(took <- readgpt:::docx_unzip(g, d2, media = TRUE, max_bytes = 3000),
                 class = "gr_docx_media_skipped")
  expect_identical(took, c("word/document.xml", "word/media/a.png"))
})

# ---------------------------------------------------------------------------
# security-03: downloads with no size limit and no destination check
# ---------------------------------------------------------------------------

test_that("addresses on this machine or a private network are recognised", {
  local <- c("http://localhost/x", "http://foo.localhost/", "http://127.0.0.1/", "http://127.1/",
             "http://2130706433/", "http://0x7f000001/", "http://0177.0.0.1/", "http://10.1.2.3/",
             "http://172.31.255.255/", "http://192.168.1.1:80/a", "http://100.64.0.1/",
             "http://169.254.169.254/latest/meta-data/", "http://0.0.0.0/", "http://[::1]/",
             "http://[fe80::1%25eth0]/", "http://[fd00::1]/", "http://[::ffff:127.0.0.1]/",
             "http://[::ffff:a9fe:a9fe]/", "http://user:pw@127.0.0.1/",
             "http://metadata.google.internal/computeMetadata/v1/", "http://127.0.0.1./")
  public <- c("https://example.org/a", "http://8.8.8.8/", "http://172.32.0.1/",
              "http://[2001:4860:4860::8888]/", "http://100.128.0.1/", "http://10.example.com/")
  expect_true(all(vapply(local, readgpt:::url_is_local, logical(1))))
  expect_false(any(vapply(public, readgpt:::url_is_local, logical(1))))
})

test_that("a local address is refused before anything is fetched", {
  local_clean_cache()
  withr::local_options(readgpt.allow_local_urls = NULL)
  fetched <- FALSE
  local_mocked_bindings(GET = function(...) { fetched <<- TRUE; stop("fetched") },
                        .package = "httr")
  expect_error(quiet(gr_ingest("http://169.254.169.254/latest/meta-data/iam/security-credentials/r")),
               class = "gr_url_error", regexp = "private network")
  expect_false(fetched)
})

test_that("a redirect is checked before it is followed", {
  asked <- character(0)
  local_mocked_bindings(GET = function(url, ...) {
    asked <<- c(asked, url)
    loc <- if (grepl("example.org/start", url)) "/next" else "http://169.254.169.254/latest/"
    structure(list(url = url, status_code = 302L, headers = list(location = loc)),
              class = "response")
  }, .package = "httr")
  expect_error(readgpt:::url_download("https://example.org/start", withr::local_tempfile(),
                                      allow_local = FALSE, lookup = function(host) NULL),
               class = "gr_url_error", regexp = "169.254.169.254")
  expect_identical(asked, c("https://example.org/start", "https://example.org/next"))
})

test_that("a public name for a private address is refused, and a checked name is pinned", {
  pinned <- NULL
  local_mocked_bindings(GET = function(url, ...) {
    for (a in list(...)) if (inherits(a, "request")) pinned <<- c(pinned, a$options$resolve)
    structure(list(url = url, status_code = 200L, headers = list(`content-type` = "text/plain")),
              class = "response")
  }, .package = "httr")
  expect_error(readgpt:::url_download("http://169.254.169.254.nip.io/latest/", withr::local_tempfile(),
                                      allow_local = FALSE,
                                      lookup = function(host) "169.254.169.254"),
               class = "gr_url_error", regexp = "resolves to 169.254.169.254")
  expect_null(pinned)
  # A name that resolves to public addresses is fetched from those addresses
  # and no others, so it cannot be pointed elsewhere after the check.
  got <- readgpt:::url_download("https://Example.org/report", withr::local_tempfile(),
                                allow_local = FALSE,
                                lookup = function(host) c("93.184.215.14", "2606:2800:21f:cb07::1"))
  expect_identical(got$status, 200L)
  expect_identical(pinned, "example.org:443:93.184.215.14,[2606:2800:21f:cb07::1]")
})

test_that("a download is refused once it passes the size limit", {
  skip_on_cran()
  # A server in another R process, on a free port of this machine: it serves
  # an endless body, and a 50 MB one that says so in Content-Length.
  script <- withr::local_tempfile(fileext = ".R")
  portfile <- withr::local_tempfile()
  writeLines(c(
    "s <- NULL",
    "for (k in 1:50) { port <- sample(20000:60000, 1)",
    "  s <- tryCatch(serverSocket(port), error = function(e) NULL); if (!is.null(s)) break }",
    sprintf("writeLines(c(as.character(port), Sys.getpid()), %s)", deparse(portfile)),
    "chunk <- as.raw(rep(65L, 65536))",
    "repeat {",
    "  con <- tryCatch(socketAccept(s, blocking = TRUE, open = 'r+b'), error = function(e) NULL)",
    "  if (is.null(con)) next",
    "  req <- readLines(con, n = 1, warn = FALSE)",
    "  repeat { l <- readLines(con, n = 1, warn = FALSE); if (!length(l) || l %in% c('', '\\r')) break }",
    "  len <- if (grepl('/declared', req)) 'Content-Length: 50000000\\r\\n' else ''",
    "  try({ writeBin(charToRaw(paste0('HTTP/1.1 200 OK\\r\\nContent-Type: text/plain\\r\\n', len,",
    "                                 'Connection: close\\r\\n\\r\\n')), con)",
    "        for (i in 1:2000) writeBin(chunk, con) }, silent = TRUE)",
    "  try(close(con), silent = TRUE)",
    "}"), script)
  system2(file.path(R.home("bin"), "Rscript"), script, wait = FALSE, stdout = FALSE, stderr = FALSE)
  for (i in 1:100) {
    if (file.exists(portfile) && length(readLines(portfile, warn = FALSE)) == 2L) break
    Sys.sleep(0.1)
  }
  info <- readLines(portfile, warn = FALSE)
  skip_if(length(info) != 2L, "could not start a local server")
  withr::defer(tools::pskill(as.integer(info[2])))
  base <- sprintf("http://127.0.0.1:%s", info[1])
  dest <- withr::local_tempfile()
  expect_error(readgpt:::url_download(paste0(base, "/declared"), dest, max_bytes = 1e6,
                                      allow_local = TRUE),
               class = "gr_too_large")
  # Refused from the declared length, before anything was written.
  expect_true(!file.exists(dest) || file.size(dest) == 0)
  expect_error(readgpt:::url_download(paste0(base, "/endless"), dest, max_bytes = 2e6,
                                      allow_local = TRUE),
               class = "gr_too_large")
  expect_lt(file.size(dest), 4e6)
  # Through gr_ingest the partial file is removed, and the error says so.
  withr::local_options(readgpt.allow_local_urls = TRUE)
  local_mocked_bindings(.gr_max_download_bytes = 1e6, .package = "readgpt")
  expect_error(quiet(gr_ingest(paste0(base, "/endless"), cache = FALSE)),
               class = "gr_too_large", regexp = "Download it yourself")
})
