# test-docx-url.R
#
# Word files read in the shape a reader sees them (tables row by row, notes
# beside what cites them, headings by style name in any language), and
# documents given as web addresses. The Word files are built here from their
# XML parts, so each test says exactly what the file contains.

W_NS <- paste0('xmlns:w="http://schemas.openxmlformats.org/wordprocessingml/2006/main" ',
               'xmlns:mc="http://schemas.openxmlformats.org/markup-compatibility/2006"')

wrun <- function(text) sprintf('<w:r><w:t xml:space="preserve">%s</w:t></w:r>', text)

wpara <- function(text = NULL, style = NULL, runs = NULL, outline = NULL) {
  ppr <- c(if (!is.null(style)) sprintf('<w:pStyle w:val="%s"/>', style),
           if (!is.null(outline)) sprintf('<w:outlineLvl w:val="%d"/>', outline))
  paste0("<w:p>", if (length(ppr)) paste0("<w:pPr>", paste(ppr, collapse = ""), "</w:pPr>"),
         if (!is.null(text)) wrun(text), paste(runs, collapse = ""), "</w:p>")
}

wnote_ref <- function(id, what = "footnote") {
  sprintf('<w:r><w:%sReference w:id="%s"/></w:r>', what, id)
}

# A cell is text, or a paragraph already written as XML.
wtable <- function(rows) {
  cell_xml <- function(cell) {
    paste0("<w:tc>", if (startsWith(cell, "<w:p>")) cell else wpara(cell), "</w:tc>")
  }
  paste0("<w:tbl>", paste(vapply(rows, function(r) {
    paste0("<w:tr>", paste(vapply(r, cell_xml, character(1)), collapse = ""), "</w:tr>")
  }, character(1)), collapse = ""), "</w:tbl>")
}

wstyle <- function(id, name, based = NULL, outline = NULL) {
  sprintf('<w:style w:type="paragraph" w:styleId="%s"><w:name w:val="%s"/>%s%s</w:style>',
          id, name,
          if (!is.null(based)) sprintf('<w:basedOn w:val="%s"/>', based) else "",
          if (!is.null(outline)) sprintf('<w:pPr><w:outlineLvl w:val="%d"/></w:pPr>', outline)
          else "")
}

write_docx <- function(body, styles = NULL, footnotes = NULL, endnotes = NULL) {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  dir.create(file.path(dir, "word"))
  writeLines(paste0('<?xml version="1.0" encoding="UTF-8" standalone="yes"?>',
                    '<Types xmlns="http://schemas.openxmlformats.org/package/2006/content-types">',
                    '<Default Extension="xml" ContentType="application/xml"/></Types>'),
             file.path(dir, "[Content_Types].xml"))
  writeLines(sprintf(paste0('<?xml version="1.0" encoding="UTF-8"?>',
                            '<w:document %s><w:body>%s</w:body></w:document>'),
                     W_NS, paste(body, collapse = "")), file.path(dir, "word", "document.xml"))
  if (!is.null(styles)) {
    writeLines(sprintf('<?xml version="1.0"?><w:styles %s>%s</w:styles>', W_NS,
                       paste(styles, collapse = "")), file.path(dir, "word", "styles.xml"))
  }
  notes <- function(items, what) {
    paste0(sprintf('<w:%s w:type="separator" w:id="-1"><w:p><w:r><w:separator/></w:r></w:p></w:%s>',
                   what, what),
           paste(vapply(names(items), function(id) sprintf(
             '<w:%s w:id="%s"><w:p><w:r><w:%sRef/></w:r>%s</w:p></w:%s>',
             what, id, what, wrun(items[[id]]), what), character(1)), collapse = ""))
  }
  if (!is.null(footnotes)) {
    writeLines(sprintf('<?xml version="1.0"?><w:footnotes %s>%s</w:footnotes>', W_NS,
                       notes(footnotes, "footnote")), file.path(dir, "word", "footnotes.xml"))
  }
  if (!is.null(endnotes)) {
    writeLines(sprintf('<?xml version="1.0"?><w:endnotes %s>%s</w:endnotes>', W_NS,
                       notes(endnotes, "endnote")), file.path(dir, "word", "endnotes.xml"))
  }
  out <- tempfile(fileext = ".docx")
  withr::with_dir(dir, utils::zip(out, files = list.files(".", recursive = TRUE,
                                                           all.files = TRUE), flags = "-q"))
  out
}

skip_without_docx_tools <- function() {
  skip_if_not_installed("xml2")
  skip_if(!nzchar(Sys.which("zip")), "no zip program to build a Word file with")
}

read_docx_blocks <- function(f) {
  quiet(gr_ingest(f, gr_ingest_spec(clean = "none"), cache = FALSE))$blocks
}

test_that("a Word table is read one row at a time, cells kept together", {
  skip_without_docx_tools()
  f <- write_docx(c(wpara("Enrolment by site."),
                    wtable(list(c("Site", "Participants"), c("North", "212"),
                                c("South", "270")))))
  b <- read_docx_blocks(f)
  expect_identical(b$text[b$kind == "table"],
                   c("Site | Participants", "North | 212", "South | 270"))
  # No cell is a paragraph of its own.
  expect_false(any(b$text %in% c("North", "212")))
})

test_that("rows a content control wraps are read, and a nested table is read as rows", {
  skip_without_docx_tools()
  row <- function(...) paste0("<w:tr>", paste0("<w:tc>", vapply(c(...), wpara, ""), "</w:tc>",
                                                collapse = ""), "</w:tr>")
  sdt <- function(x) paste0("<w:sdt><w:sdtPr/><w:sdtContent>", x, "</w:sdtContent></w:sdt>")
  f <- write_docx(paste0(
    "<w:tbl>", row("Action", "Owner"),
    sdt(paste0(sdt(row("Renew the ethics approval", "Dr Okafor")),
               sdt(row("Send data to the registry", "J. Lin")))),
    '<w:customXml w:element="item">', row("Book the audit", "K. Ade"), "</w:customXml>",
    "</w:tbl>"))
  expect_identical(read_docx_blocks(f)$text,
                   c("Action | Owner", "Renew the ethics approval | Dr Okafor",
                     "Send data to the registry | J. Lin", "Book the audit | K. Ade"))

  g <- write_docx(paste0("<w:tbl><w:tr><w:tc>", wpara("Enrolment"), "</w:tc><w:tc>",
                         wtable(list(c("Site", "Participants"), c("North", "212"))),
                         wpara(), "</w:tc></w:tr></w:tbl>"))
  expect_identical(read_docx_blocks(g)$text,
                   c("Enrolment | ", "Site | Participants", "North | 212"))
})

test_that("a non-breaking hyphen and a symbol keep their place in the text", {
  skip_without_docx_tools()
  f <- write_docx(wpara(runs = paste0(
    '<w:r><w:t xml:space="preserve">Response was 10</w:t><w:noBreakHyphen/>',
    '<w:t xml:space="preserve">15% (p </w:t><w:sym w:font="Symbol" w:char="F0A3"/>',
    '<w:t xml:space="preserve"> 0.05, </w:t><w:sym w:font="Symbol" w:char="F061"/>',
    '<w:t xml:space="preserve"> = 0.8, </w:t><w:sym w:font="Arial" w:char="00B1"/>',
    '<w:t xml:space="preserve">2)</w:t></w:r>')))
  expect_identical(read_docx_blocks(f)$text,
                   "Response was 10-15% (p \u2264 0.05, \u03b1 = 0.8, \u00b12)")
})

test_that("footnotes and endnotes are read beside what cites them", {
  skip_without_docx_tools()
  f <- write_docx(
    c(wpara(runs = c(wrun("The cohort comprised 482 participants."), wnote_ref("1"))),
      wpara("A paragraph between."),
      wpara(runs = c(wrun("Data were pooled."), wnote_ref("3", "endnote"))),
      wtable(list(c("Site", "Adherence"),
                  c("North", wpara(runs = c(wrun("91%"), wnote_ref("4"))))))),
    footnotes = list("1" = "Adherence was measured by pill count.",
                     "2" = "A note nothing cites.",
                     "4" = "Self-reported."),
    endnotes = list("3" = "Data are available on request."))
  b <- read_docx_blocks(f)
  at <- function(x) match(x, b$text)
  expect_identical(at("[Footnote 1] Adherence was measured by pill count."),
                   at("The cohort comprised 482 participants.") + 1L)
  expect_identical(at("[Endnote 1] Data are available on request."),
                   at("Data were pooled.") + 1L)
  # Numbered in the order the text cites them, and a note cited in a table
  # follows its row.
  expect_identical(at("[Footnote 2] Self-reported."), at("North | 91%") + 1L)
  # One that nothing cites is still part of the document, at the end.
  expect_identical(b$text[nrow(b)], "[Footnote 3] A note nothing cites.")
  expect_true(all(b$kind[grepl("^\\[(Foot|End)note", b$text)] == "footnote"))
  # The separators Word keeps with the notes are not notes.
  expect_identical(sum(b$kind == "footnote"), 4L)
})

test_that("headings are found by style name, in any language", {
  skip_without_docx_tools()
  f <- write_docx(
    c(wpara("Studienbericht", style = "Titel"),
      wpara("Einleitung", style = "berschrift1"),
      wpara("Der Text der Einleitung."),
      wpara("A style whose ID starts with Heading.", style = "HeadingNote"),
      wpara("Methoden", style = "Kapitel"),
      wpara("Der Text der Methoden."),
      wpara("Ergebnisse", outline = 1L),
      wpara("Der Text der Ergebnisse.")),
    styles = c(wstyle("Standard", "Normal"), wstyle("Titel", "Title"),
               wstyle("berschrift1", "heading 1", based = "Standard", outline = 0L),
               # A custom heading, based on one: its outline level is inherited.
               wstyle("Kapitel", "Chapter", based = "berschrift1"),
               wstyle("HeadingNote", "Heading Note")))
  b <- read_docx_blocks(f)
  expect_identical(b$text[b$kind == "heading"],
                   c("Studienbericht", "Einleitung", "Methoden", "Ergebnisse"))
  expect_identical(b$section[b$text == "Der Text der Methoden."], "Methoden")
  expect_identical(b$section[b$text == "Der Text der Ergebnisse."], "Ergebnisse")
  expect_identical(b$kind[b$text == "A style whose ID starts with Heading."], "body")
})

test_that("without a styles part the English style IDs still mark headings", {
  skip_without_docx_tools()
  f <- write_docx(c(wpara("Introduction", style = "Heading1"), wpara("Body text here.")))
  b <- read_docx_blocks(f)
  expect_identical(b$kind, c("heading", "body"))
  expect_identical(b$section, c("Introduction", "Introduction"))
})

test_that("each piece of text is read once, and runs keep their spacing", {
  skip_without_docx_tools()
  box <- paste0('<w:r><mc:AlternateContent><mc:Choice Requires="wps"><w:drawing><w:txbxContent>',
                wpara("Boxed text."), "</w:txbxContent></w:drawing></mc:Choice><mc:Fallback>",
                "<w:pict><w:txbxContent>", wpara("Boxed text."),
                "</w:txbxContent></w:pict></mc:Fallback></mc:AlternateContent></w:r>")
  f <- write_docx(c(
    wpara(runs = "<w:r><w:t>Name:</w:t><w:tab/><w:t>Value</w:t></w:r>"),
    wpara(runs = c(wrun("Anchor paragraph."), box)),
    wpara(runs = "<w:moveFrom><w:r><w:t>Moved text.</w:t></w:r></w:moveFrom>"),
    wpara(runs = "<w:moveTo><w:r><w:t>Moved text.</w:t></w:r></w:moveTo>"),
    wpara(runs = "<w:del><w:r><w:delText>Deleted text.</w:delText></w:r></w:del>"),
    paste0("<w:sdt><w:sdtPr/><w:sdtContent>", wpara("Inside a content control."),
           "</w:sdtContent></w:sdt>")))
  b <- read_docx_blocks(f)
  expect_identical(b$text, c("Name: Value", "Anchor paragraph.", "Boxed text.", "Moved text.",
                             "Inside a content control."))
})

# ---------------------------------------------------------------------------
# Web addresses
# ---------------------------------------------------------------------------

serve <- function(file, type = "", status = 200L, env = parent.frame()) {
  local_mocked_bindings(url_download = function(url, dest) {
    file.copy(file, dest, overwrite = TRUE)
    list(status = status, type = type)
  }, .package = "readgpt", .env = env)
}

test_that("a web address is downloaded and read as what it serves", {
  local_clean_cache()
  serve(readgpt_example(), type = "text/markdown; charset=utf-8")
  doc <- quiet(gr_ingest("https://example.org/reports/annual"))
  expect_identical(doc$source, "https://example.org/reports/annual")
  # Read with the Markdown extractor, so its headings are sections.
  expect_true(any(doc$blocks$kind == "heading"))
  expect_match(doc$text, "45.2 million", fixed = TRUE)

  a <- quiet(answer_document("https://example.org/reports/annual", "What was revenue?",
                             client = mock_echo()))
  expect_identical(a$document$source, "https://example.org/reports/annual")
  expect_identical(a$trace$meta$source, "https://example.org/reports/annual")
})

test_that("an address ending in an extension is fetched, not looked for on disk", {
  local_clean_cache()
  serve(readgpt_example(), type = "text/plain")
  doc <- quiet(gr_ingest("https://example.org/files/report.md"))
  expect_identical(doc$source, "https://example.org/files/report.md")
  # Served as plain text, still read as Markdown: the address says what it is.
  expect_true(any(doc$blocks$kind == "heading"))
})

test_that("space around an address, or inside it, does not make it text", {
  local_clean_cache()
  asked <- character(0)
  local_mocked_bindings(url_download = function(url, dest) {
    asked <<- c(asked, url)
    file.copy(readgpt_example(), dest, overwrite = TRUE)
    list(status = 200L, type = "text/markdown")
  }, .package = "readgpt")
  for (u in c(" https://example.org/reports/a", "https://example.org/reports/b\n",
              "https://example.org/reports/annual report")) {
    doc <- quiet(gr_ingest(u, cache = FALSE))
    expect_identical(doc$source, trimws(u))
  }
  expect_identical(asked, c("https://example.org/reports/a", "https://example.org/reports/b",
                            "https://example.org/reports/annual%20report"))
  # A line of text that starts with an address and goes on is still text.
  expect_identical(quiet(gr_ingest("https://example.org is our site.\nIt has reports.",
                                   cache = FALSE))$source, "<inline text>")
})

test_that("a failed download is an error that names the address", {
  local_clean_cache()
  serve(readgpt_example(), status = 404L)
  expect_error(quiet(gr_ingest("https://example.org/missing.pdf")),
               class = "gr_url_error", regexp = "HTTP 404")
  local_mocked_bindings(url_download = function(url, dest) stop("could not resolve host"),
                        .package = "readgpt")
  expect_error(quiet(gr_ingest("https://no-such-host.invalid/doc")),
               class = "gr_url_error", regexp = "could not resolve host")
})

test_that("the type of a download comes from the server, the address, then its bytes", {
  f <- tempfile()
  writeLines("plain words", f)
  ext <- readgpt:::url_extension
  expect_identical(ext("application/pdf", "https://x.org/get", f), "pdf")
  expect_identical(ext("text/html; charset=UTF-8", "https://x.org/", f), "html")
  # A type no extractor reads is passed over for the address.
  expect_identical(ext("application/octet-stream", "https://x.org/a/report.PDF?dl=1#p2", f), "pdf")
  pdf <- tempfile()
  writeBin(charToRaw("%PDF-1.4 rest of file"), pdf)
  expect_identical(ext("", "https://x.org/get", pdf), "pdf")
  html <- tempfile()
  writeLines("<!DOCTYPE html><html><body>Hi</body></html>", html)
  expect_identical(ext(NULL, "https://x.org/", html), "html")
  expect_identical(ext("", "https://x.org/", f), "txt")
  # HTML labelled as plain text, as raw file hosts label everything, is HTML.
  expect_identical(ext("text/plain; charset=utf-8", "https://x.org/raw/page", html), "html")
  # Bytes that are not text, of a type no extractor reads, are not read as text.
  bin <- tempfile()
  writeBin(as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0x00, 0x01, 0x00, 0x02)), bin)
  expect_true(is.na(ext("application/msword", "https://x.org/protocol.doc", bin)))
  # Nor is a zip that is not a Word file.
  skip_if(!nzchar(Sys.which("zip")), "no zip program")
  d <- withr::local_tempdir()
  writeLines("a,b", file.path(d, "sheet.csv"))
  z <- tempfile(fileext = ".zip")
  withr::with_dir(d, utils::zip(z, "sheet.csv", flags = "-q"))
  expect_true(is.na(ext("application/zip", "https://x.org/data", z)))
})

test_that("a download no extractor reads is refused, not read as text", {
  local_clean_cache()
  bin <- tempfile()
  writeBin(as.raw(c(0xd0, 0xcf, 0x11, 0xe0, 0x00, 0x01, 0x00, 0x02)), bin)
  serve(bin, type = "application/msword")
  expect_error(quiet(gr_ingest("https://example.org/files/protocol.doc")),
               class = "gr_unsupported_format", regexp = "application/msword")
})

test_that("each address is cached as itself", {
  local_clean_cache()
  one <- tempfile(fileext = ".txt")
  two <- tempfile(fileext = ".txt")
  writeLines("The first report says revenue was 45.2 million.", one)
  writeLines("The second report says revenue was 51.8 million.", two)
  local_mocked_bindings(url_download = function(url, dest) {
    file.copy(if (grepl("first", url)) one else two, dest, overwrite = TRUE)
    list(status = 200L, type = "text/plain")
  }, .package = "readgpt")
  a <- quiet(gr_ingest("https://example.org/first"))
  b <- quiet(gr_ingest("https://example.org/second"))
  expect_match(a$text, "45.2", fixed = TRUE)
  expect_match(b$text, "51.8", fixed = TRUE)
})

test_that("a corpus of addresses is labelled by address", {
  local_clean_cache()
  serve(readgpt_example(), type = "text/markdown")
  out <- quiet(gr_read_many(c("https://example.org/a/report", "https://example.net/b/report"),
                            "What was revenue?", "fast", client = mock_echo()))
  expect_identical(out$summary$document, c("example.org/a/report", "example.net/b/report"))
  expect_true(all(out$summary$status %in% c("ok", "duplicate")))
})
