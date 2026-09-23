# ingest-extract.R -- AXIS 1a: bytes -> structured blocks.
#
# WHY THIS FILE EXISTS
# `parse_text()` was one 70-line function that extracted, cleaned, chunked and
# cached, with every decision hard-coded. Extraction defects it carried:
#
#   * OCR triggered only when EVERY page was empty (`all(nchar(...) == 0)`), so
#     a scanned PDF with one born-digital cover page silently lost every
#     scanned page -- and the 20-character sanity check passed, so no warning.
#   * Page text was joined with a literal "--- Page Break ---" marker, which
#     then survived cleaning and chunking and was sent to the model as document
#     content.
#   * DOCX extraction unzipped into a tempdir it never cleaned up.
#   * Page and section provenance was destroyed at extraction time, so a citation
#     could never point back to a page.
#
# Extraction now returns *blocks* -- text plus provenance (page, section, kind)
# -- and is a registry, so adding a format is a `gr_register_extractor()` call
# rather than a new branch in someone else's if/else ladder.

#' Register a document extractor
#'
#' @param name Short name for the extractor.
#' @param extensions Character vector of file extensions it claims.
#' @param fn Function of `(path, opts)` returning a data frame with columns
#'   `text`, and optionally `page`, `section`, `kind`. Pages it could not turn
#'   into text can be listed in `attr(result, "gr_unread_pages")`; an answer
#'   drawn from the document is then marked partial, as it is for a PDF page
#'   that needed OCR and did not get it.
#' @param description One-line description shown by [gr_extractors()].
#' @return Invisibly, `name`.
#' @seealso [gr_extractors()], [gr_ingest()], [gr_register_cleaner()]
#' @family ingest functions
#' @export
#' @examples
#' # A minimal extractor for tab-separated files: one block per row.
#' gr_register_extractor("tsv", "tsv", description = "TSV, one block per row",
#'   fn = function(path, opts) {
#'     d <- utils::read.delim(path, stringsAsFactors = FALSE)
#'     data.frame(text = apply(d, 1, paste, collapse = " | "), kind = "table")
#'   })
#' subset(gr_extractors(), name == "tsv")
gr_register_extractor <- function(name, extensions, fn, description = "") {
  if (!is.function(fn)) gr_abort("`fn` must be a function of (path, opts).")
  registry_set("extractors", name, list(name = name, extensions = tolower(extensions),
                                        fn = fn, description = description))
}

#' List registered extractors
#' @return A data frame with `name`, `extensions`, `description`, `needs` (the
#'   packages the extractor cannot run without, comma-separated, `""` for none)
#'   and `available` (whether they are all installed now). OCR packages are not
#'   in `needs`: a PDF with a text layer reads without them.
#' @seealso [gr_register_extractor()], [gr_ingest()]
#' @family ingest functions
#' @export
#' @examples
#' gr_extractors()
#'
#' # What this installation can read right now.
#' gr_extractors()[, c("name", "needs", "available")]
gr_extractors <- function() {
  reg <- gr_state$extractors
  if (!length(reg)) return(data.frame())
  do.call(rbind, lapply(reg, function(e) {
    need <- .gr_extractor_deps[[as_chr1(e$name)]] %||% character(0)
    data.frame(
      name = e$name, extensions = paste(e$extensions, collapse = ", "),
      description = e$description,
      needs = paste(need, collapse = ", "),
      available = !length(missing_extractor_deps(e$name)),
      stringsAsFactors = FALSE)
  }))
}

#' Find the extractor that claims an extension.
#'
#' LAST registration wins, matching [gr_register_model()]'s "registered entries
#' take precedence" rule. Scanning forwards let the built-ins keep every
#' extension forever: `gr_register_extractor("my_pdf", "pdf", ...)` appeared to
#' succeed, showed up in `gr_extractors()`, and was then never called, because
#' the built-in `pdf` entry was reached first. An override that silently does
#' nothing is worse than one that is refused.
#' @noRd
extractor_for <- function(ext) {
  reg <- gr_state$extractors
  for (e in rev(reg)) if (tolower(ext) %in% e$extensions) return(e)
  NULL
}

#' Normalise any extractor return value into the canonical block frame.
#' @noRd
as_blocks <- function(x) {
  if (is.character(x)) x <- data.frame(text = x, stringsAsFactors = FALSE)
  if (!is.data.frame(x)) gr_abort("An extractor must return a character vector or a data frame.")
  if (!"text" %in% names(x)) gr_abort("Extractor output needs a `text` column.")
  # A zero-row frame must short-circuit: `df$page <- NA_integer_` on 0 rows is
  # an error ("replacement has 1 row, data has 0"), not a no-op.
  if (!nrow(x)) {
    return(data.frame(text = character(0), page = integer(0), section = character(0),
                      kind = character(0), stringsAsFactors = FALSE))
  }
  x$text <- vapply(x$text, as_chr1, character(1), USE.NAMES = FALSE)
  if (!"page" %in% names(x))    x$page <- NA_integer_
  if (!"section" %in% names(x)) x$section <- NA_character_
  if (!"kind" %in% names(x))    x$kind <- "body"
  x <- x[has_content(x$text), c("text", "page", "section", "kind"), drop = FALSE]
  rownames(x) <- NULL
  x
}

# ---------------------------------------------------------------------------
# Built-in extractors
# ---------------------------------------------------------------------------

#' Read a text file and guarantee valid UTF-8 before anything looks at it.
#'
#' `readLines(encoding = "UTF-8")` LABELS the bytes as UTF-8; it does not
#' convert them. A latin1 or CP1252 file therefore arrived mislabelled, and the
#' first regex to touch it -- `trimws()` inside `paragraphs_of()` -- aborted the
#' whole ingest with "input string 1 is invalid UTF-8". Transcoding happened
#' later in `gr_ingest()`, which was already too late.
#' @noRd
read_text_lines <- function(path) {
  to_utf8(readLines(path, warn = FALSE))
}

#' @noRd
extract_txt <- function(path, opts) {
  lines <- read_text_lines(path)
  as_blocks(data.frame(text = paragraphs_of(paste(lines, collapse = "\n")),
                       stringsAsFactors = FALSE))
}

#' @noRd
extract_md <- function(path, opts) {
  lines <- read_text_lines(path)
  txt <- paste(lines, collapse = "\n")
  paras <- paragraphs_of(txt)
  if (!length(paras)) return(as_blocks(data.frame(text = character(0))))
  # Track the current heading so downstream segmenters can group by section.
  section <- NA_character_
  secs <- character(length(paras)); kinds <- character(length(paras))
  for (i in seq_along(paras)) {
    h <- regmatches(paras[i], regexpr("^#{1,6}[ \t]+.*$", paras[i], perl = TRUE))
    if (length(h)) section <- trimws(sub("^#+[ \t]+", "", h[1]))
    secs[i] <- section
    kinds[i] <- if (length(h)) "heading"
    else if (grepl("^```", paras[i])) "code"
    else if (grepl("^\\s*\\|.*\\|", paras[i])) "table"
    else "body"
  }
  as_blocks(data.frame(text = paras, section = secs, kind = kinds, stringsAsFactors = FALSE))
}

#' @noRd
extract_html <- function(path, opts) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    gr_abort("Reading HTML needs the 'xml2' package.", class = "gr_missing_dep")
  }
  doc <- xml2::read_html(path)
  xml2::xml_remove(xml2::xml_find_all(doc, "//script|//style|//nav|//footer"))
  nodes <- xml2::xml_find_all(doc, "//h1|//h2|//h3|//h4|//p|//li|//pre|//td|//blockquote")
  txt <- trimws(xml2::xml_text(nodes))
  tag <- xml2::xml_name(nodes)
  keep <- nzchar(txt)
  txt <- txt[keep]; tag <- tag[keep]
  if (!length(txt)) return(as_blocks(data.frame(text = character(0))))
  section <- NA_character_; secs <- character(length(txt))
  for (i in seq_along(txt)) {
    if (grepl("^h[1-6]$", tag[i])) section <- txt[i]
    secs[i] <- section
  }
  kinds <- ifelse(grepl("^h[1-6]$", tag), "heading",
                  ifelse(tag == "pre", "code", ifelse(tag == "td", "table", "body")))
  as_blocks(data.frame(text = txt, section = secs, kind = kinds, stringsAsFactors = FALSE))
}

#' @noRd
extract_pdf <- function(path, opts) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    gr_abort("Reading PDFs needs the 'pdftools' package.", class = "gr_missing_dep")
  }
  pages <- pdftools::pdf_text(path)
  pages <- vapply(pages, as_chr1, character(1), USE.NAMES = FALSE)

  # PER-PAGE OCR decision. The old code demanded that *every* page be empty
  # before OCRing anything, so mixed scanned/digital PDFs silently lost content.
  ocr_mode <- as_chr1(opts$ocr %||% "auto")
  # as.numeric(), not as.integer(): Inf -- OCR every page -- is a threshold, and
  # as.integer(Inf) is NA.
  min_chars <- as.numeric(opts$ocr_min_chars %||% 40L)
  needs <- switch(ocr_mode,
                  never  = rep(FALSE, length(pages)),
                  always = rep(TRUE,  length(pages)),
                  nchar(trimws(pages)) < min_chars)
  # Pages whose content never became text. They travel with the document (as
  # `stats$unread_pages`) so an answer drawn from it is marked partial: a warning
  # printed once at the console is gone by the time anyone reads the answer, and
  # a cached document does not raise it again at all.
  unread <- integer(0)
  if (any(needs)) {
    if (!requireNamespace("tesseract", quietly = TRUE) ||
        !requireNamespace("magick", quietly = TRUE)) {
      gr_warn(sprintf(paste0("%d of %d PDF pages have little or no text layer and need OCR, but ",
                             "'tesseract' and/or 'magick' are not installed. Those pages are ",
                             "being returned empty rather than silently dropped."),
                      sum(needs), length(pages)), class = "gr_ocr_unavailable")
      # Only pages that came out with no text at all. A page under the threshold
      # still has its text layer, which is read (a cover, a divider, a figure
      # with a caption); counting it as unread made most born-digital PDFs
      # partial, and under ocr = "always" every page.
      unread <- which(needs & !nzchar(trimws(pages)))
    } else {
      gr_msg(sprintf("OCR-ing %d of %d PDF page(s).", sum(needs), length(pages)))
      eng <- tesseract::tesseract(as_chr1(opts$ocr_lang %||% "eng"))
      ocr_res <- gr_lapply(which(needs), function(i, trace) {
        tryCatch({
          img <- magick::image_read_pdf(path, pages = i, density = as.numeric(opts$ocr_dpi %||% 300))
          list(text = as_chr1(tesseract::ocr(img, engine = eng)), failed = FALSE)
        }, error = function(e) {
          gr_warn(sprintf("OCR failed on page %d: %s", i, conditionMessage(e)),
                  class = "gr_ocr_failed")
          list(text = "", failed = TRUE)
        })
      }, parallel = opts$parallel, label = "OCR page")
      pages[needs] <- vapply(ocr_res, function(r) as_chr1(r$text), character(1),
                             USE.NAMES = FALSE)
      unread <- which(needs)[vapply(ocr_res, function(r) isTRUE(r$failed), logical(1))]
    }
  }

  # Emit one block per paragraph per page. Page provenance is a real column, not
  # a "--- Page Break ---" marker glued into the text where it would be read as
  # document content.
  out <- do.call(rbind, lapply(seq_along(pages), function(i) {
    p <- paragraphs_of(pages[i])
    if (!length(p)) return(NULL)
    data.frame(text = p, page = i, section = NA_character_, kind = "body",
               stringsAsFactors = FALSE)
  }))
  out <- as_blocks(out %||% data.frame(text = character(0)))
  attr(out, "gr_unread_pages") <- unread
  out
}

#' @noRd
extract_docx <- function(path, opts) {
  if (!requireNamespace("xml2", quietly = TRUE)) {
    gr_abort("Reading DOCX needs the 'xml2' package.", class = "gr_missing_dep")
  }
  dir <- tempfile("readgpt_docx_")
  # The old code never removed its unzip directory; temp files leaked for the
  # life of the session.
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  utils::unzip(path, exdir = dir)

  doc_xml <- file.path(dir, "word", "document.xml")
  blocks <- data.frame(text = character(0), section = character(0), kind = character(0),
                       stringsAsFactors = FALSE)
  if (file.exists(doc_xml)) {
    x <- xml2::read_xml(doc_xml)
    ns <- xml2::xml_ns(x)
    paras <- xml2::xml_find_all(x, ".//w:p", ns)
    txt <- vapply(paras, function(p) {
      paste(xml2::xml_text(xml2::xml_find_all(p, ".//w:t", ns)), collapse = "")
    }, character(1))
    style <- vapply(paras, function(p) {
      s <- xml2::xml_find_first(p, ".//w:pStyle", ns)
      if (inherits(s, "xml_missing")) "" else as_chr1(xml2::xml_attr(s, "val", ns))
    }, character(1))
    keep <- nzchar(trimws(txt))
    txt <- txt[keep]; style <- style[keep]
    if (length(txt)) {
      is_head <- grepl("^Heading|^Title", style, ignore.case = TRUE)
      section <- NA_character_; secs <- character(length(txt))
      for (i in seq_along(txt)) { if (is_head[i]) section <- txt[i]; secs[i] <- section }
      blocks <- data.frame(text = txt, section = secs,
                           kind = ifelse(is_head, "heading", "body"), stringsAsFactors = FALSE)
    }
  } else if (requireNamespace("readtext", quietly = TRUE)) {
    blocks <- data.frame(text = paragraphs_of(as_chr1(readtext::readtext(path)$text)),
                         section = NA_character_, kind = "body", stringsAsFactors = FALSE)
  }

  media <- file.path(dir, "word", "media")
  if (!identical(as_chr1(opts$ocr %||% "auto"), "never") && dir.exists(media) &&
      requireNamespace("tesseract", quietly = TRUE)) {
    imgs <- list.files(media, full.names = TRUE,
                       pattern = "\\.(png|jpe?g|tiff?|bmp)$", ignore.case = TRUE)
    if (length(imgs)) {
      gr_msg(sprintf("OCR-ing %d embedded image(s) in DOCX.", length(imgs)))
      eng <- tesseract::tesseract(as_chr1(opts$ocr_lang %||% "eng"))
      ocr <- vapply(imgs, function(f) tryCatch(as_chr1(tesseract::ocr(f, engine = eng)),
                                               error = function(e) ""),
                    character(1), USE.NAMES = FALSE)
      ocr <- ocr[has_content(ocr)]
      if (length(ocr)) {
        blocks <- rbind(blocks, data.frame(text = ocr, section = "[embedded images]",
                                           kind = "ocr", stringsAsFactors = FALSE))
      }
    }
  }
  as_blocks(blocks)
}

#' @noRd
extract_image <- function(path, opts) {
  if (!requireNamespace("tesseract", quietly = TRUE)) {
    gr_abort("Reading images needs the 'tesseract' package.", class = "gr_missing_dep")
  }
  eng <- tesseract::tesseract(as_chr1(opts$ocr_lang %||% "eng"))
  as_blocks(data.frame(text = paragraphs_of(as_chr1(tesseract::ocr(path, engine = eng))),
                       kind = "ocr", stringsAsFactors = FALSE))
}

#' @noRd
register_builtin_extractors <- function() {
  # csv and tsv are here because gr_inventory() documents counting their tokens
  # and gr_read_many() dropped them: a folder of exported tables surveyed as
  # readable and then read as nothing. They are plain text with separators, and
  # extract_txt() is what plain text gets.
  gr_register_extractor("txt",   c("txt", "text", "log", "csv", "tsv"), extract_txt,
                        "Plain text, including delimited files")
  gr_register_extractor("md",    c("md", "markdown", "rmd", "qmd"), extract_md,
                        "Markdown, keeping heading structure")
  gr_register_extractor("html",  c("html", "htm", "xhtml"), extract_html,
                        "HTML via xml2, keeping heading structure")
  gr_register_extractor("pdf",   "pdf", extract_pdf,
                        "PDF with per-page OCR fallback and page provenance")
  gr_register_extractor("docx",  c("docx", "dotx"), extract_docx,
                        "Word, keeping heading styles; OCRs embedded images")
  gr_register_extractor("image", c("png", "jpg", "jpeg", "tif", "tiff", "bmp", "gif"),
                        extract_image, "Image OCR")
  invisible(NULL)
}
