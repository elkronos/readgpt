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
                                        fn = fn, description = description,
                                        registered = registration_stamp()))
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
  # Not content: code, styling, page chrome by the package's long-standing
  # choice, what shows only without scripts, and the choices of a drop-down.
  xml2::xml_remove(xml2::xml_find_all(doc, paste0(
    "//script|//style|//nav|//footer|//noscript|//template|//svg|//select")))
  as_blocks(html_blocks(doc))
}

#' Elements a browser starts on a line of their own. Text anywhere else -- in a
#' span, a link, or straight inside a div -- runs on with the text around it.
#' @noRd
.gr_html_block_tags <- c(
  "address", "article", "aside", "blockquote", "body", "caption", "center", "dd", "details",
  "dialog", "dir", "div", "dl", "dt", "fieldset", "figcaption", "figure", "form", "h1", "h2",
  "h3", "h4", "h5", "h6", "header", "hgroup", "hr", "legend", "li", "main", "menu", "ol", "p",
  "pre", "section", "summary", "table", "tbody", "td", "tfoot", "th", "thead", "tr", "ul")

#' The blocks of a parsed HTML page, in reading order.
#'
#' Reading a fixed list of tags (h1-h4, p, li, pre, td, blockquote) and the
#' whole text of each lost text that sat straight in a div, span or section --
#' how most pages are built -- and every th and h5/h6; read a p inside an li,
#' blockquote or td twice, once through its parent; ran words either side of a
#' <br> together; and made each table cell a block of its own, a value apart
#' from its label.
#'
#' So the page is walked in document order instead. Each block-level element
#' ends the run of text before it and starts a new one, so nothing is read
#' twice and nothing between block tags is lost. Whitespace in the source is
#' layout and collapses to one space; a <br> is a line break. A heading is a
#' `heading` block and the `section` of what follows it. A table is one block
#' per row, `table`, its cells joined with " | ", with a table inside a cell read
#' as rows of its own after the row that holds it, as the Word extractor does.
#' A table that lays out the page rather than holding data is read as the rest
#' of the page is; see `is_layout` below.
#' @noRd
html_blocks <- function(doc) {
  blocks <- .gr_html_block_tags
  # Looked up once: xml_find_all() otherwise collects the namespaces by walking
  # the whole page on every call, once per table row.
  ns <- xml2::xml_ns(doc)
  find <- function(x, xpath) xml2::xml_find_all(x, xpath, ns)
  # Grown by doubling, as in docx_blocks(): appending one block at a time copies
  # the vectors every time.
  size <- 64L; n <- 0L
  text <- character(size); section <- character(size); kind <- character(size)
  current <- NA_character_
  emit <- function(t, k) {
    if (!nzchar(t)) return(invisible(NULL))
    if (n == size) {
      size <<- size * 2L
      length(text) <<- size; length(section) <<- size; length(kind) <<- size
    }
    n <<- n + 1L
    text[n] <<- t
    if (identical(k, "heading")) current <<- t
    section[n] <<- current
    kind[n] <<- k
  }
  squish <- function(s) {
    s <- gsub(" *\n *", "\n", gsub(" {2,}", " ", s, perl = TRUE), perl = TRUE)
    trimws(s)
  }
  source_text <- function(node) gsub("[ \t\r\n\f]+", " ", xml2::xml_text(node), perl = TRUE)

  # All the text under a node on one line, block boundaries and line breaks
  # read as spaces: for a heading, a caption, or a table cell. A table inside
  # a cell is left to read_table(), which reads it after the row. A <pre> keeps
  # its line breaks, as it does outside a table: they are part of the code.
  flat_text <- function(node) {
    segs <- text_pieces(); run <- text_pieces()
    close_run <- function() {
      s <- run$take()
      if (nzchar(s)) s <- squish(s)
      if (nzchar(s)) segs$add(s)
    }
    rec <- function(nd) {
      for (ch in xml2::xml_contents(nd)) {
        ty <- xml2::xml_type(ch)
        if (identical(ty, "text")) {
          run$add(source_text(ch))
        } else if (identical(ty, "element")) {
          nm <- tolower(xml2::xml_name(ch))
          if (identical(nm, "table")) next
          if (identical(nm, "pre")) {
            close_run()
            code <- trimws(xml2::xml_text(ch))
            if (nzchar(code)) segs$add(code)
            next
          }
          if (identical(nm, "br") || nm %in% blocks) run$add(" ")
          rec(ch)
          if (nm %in% blocks) run$add(" ")
        }
      }
    }
    rec(node)
    close_run()
    segs$take(" ")
  }

  # A table that lays out a page rather than holding data. Pages built on a
  # layout table -- older sites, HTML e-mail, generated reports -- put the
  # whole article in one cell, and reading that row by row made it one "table"
  # block with no headings, no sections and no paragraph breaks. A data
  # table's cells hold values, and keep the row format even when a value is
  # written as a <p> or two, or a cell holds a heading.
  #
  # What a cell holds is counted in paragraphs: the innermost block elements
  # in it, with a list or a table inside it counted as one. A cell of two or
  # more holds page content. A layout is then:
  #   * a table of one cell holding anything -- but not a lone heading when
  #     the table sits in a cell of a data table, where it is a value too;
  #   * a stack of rows -- at most one of them split into several cells, and
  #     no header cells -- with a cell of page content, or with a layout table
  #     (a heading beside other content in one cell) inside one of its cells:
  #     a banner row, a navigation and content row, and a footer row;
  #   * a grid (any other table) only when one cell holds a heading beside
  #     other content and at least half of the table's text: the article.
  # A heading on its own in a cell -- a header row styled as headings, the
  # label of a group of rows -- is a value. Taking any heading for a layout
  # split such data tables into one block per cell, a value apart from its
  # label, and made the last header cell the section of the page after the
  # table; a single row whose cells each held one <p> was split the same way.
  # Headings in the header cells, the caption, or a table inside a cell of a
  # grid say nothing about the table around them.
  #
  # A grid laid out without such a heading (a logo and a search box above a
  # navigation and content row, say) is still read row by row: it cannot be
  # told from a data table whose cells hold paragraphs.
  heading_names <- paste0("h", 1:6)
  unit_names <- c("p", "div", "section", "article", "center", "header", "main", "aside", "figure",
                  "form", "fieldset", "address", "blockquote", "pre", heading_names)
  whole_names <- c("ul", "ol", "dl", "table")
  any_of <- function(nm) paste0("self::", nm, collapse = " or ")
  # The paragraphs of a cell of the table at nesting `%1$d`, relative to the
  # cell: its innermost blocks, not inside a list, and a list or a table
  # inside it whole.
  cell_paras <- paste0(
    ".//*[", any_of(c(unit_names, whole_names)), "][count(ancestor::table) = %1$d]",
    "[not(ancestor::*[", any_of(c("ul", "ol", "dl")), "][count(ancestor::table) = %1$d])]",
    "[", any_of(whole_names), " or not(.//*[", any_of(c(unit_names, whole_names)), "])]")
  # The cells, at that nesting, that hold two paragraphs or more; and the data
  # cells among them with a heading.
  crowded_xpath <- paste0(".//*[self::td or self::th][count(ancestor::table) = %1$d]",
                          "[count(", cell_paras, ") > 1]")
  crowded_heading_xpath <- paste0(".//td[count(ancestor::table) = %1$d]",
                                  "[.//*[", any_of(heading_names), "][count(ancestor::table) = %1$d]]",
                                  "[count(", cell_paras, ") > 1]")
  deep_heading_xpath <- paste0(".//*[", any_of(heading_names), "][count(ancestor::table) > %d]")
  block_xpath <- paste0(".//*[", any_of(c(setdiff(unit_names, heading_names), whole_names)),
                        "][not(ancestor::caption)]")
  any_heading_xpath <- paste0(".//*[", any_of(heading_names), "][not(ancestor::caption)]")
  text_size <- function(node) nchar(gsub("\\s+", "", xml2::xml_text(node), perl = TRUE))
  # Each query below is a single pass over the table. Grouping the paragraphs
  # by the path of their cell instead was quadratic in the number of rows:
  # libxml2 finds a node's path by counting the siblings before it.
  is_layout <- function(tbl, cells, level, in_data) {
    n_cells <- sum(lengths(cells))
    if (!n_cells) return(FALSE)
    if (n_cells == 1L) {
      return(length(find(tbl, block_xpath)) > 0L ||
               length(find(tbl, sprintf(crowded_xpath, level))) > 0L ||
               (!in_data && length(find(tbl, any_heading_xpath)) > 0L))
    }
    if (sum(lengths(cells) > 1L) <= 1L &&
        !length(find(tbl, sprintf(".//th[count(ancestor::table) = %d]", level)))) {
      if (length(find(tbl, sprintf(crowded_xpath, level)))) return(TRUE)
      # A layout table in one of the cells: a heading in a table inside it,
      # beside other content in its cell.
      deep <- find(tbl, sprintf(deep_heading_xpath, level))
      for (lv in sort(unique(xml2::xml_find_num(deep, "count(ancestor::table)")))) {
        if (length(find(tbl, sprintf(crowded_heading_xpath, lv)))) return(TRUE)
      }
      return(FALSE)
    }
    held <- find(tbl, sprintf(crowded_heading_xpath, level))
    length(held) > 0L && any(2L * text_size(held) >= text_size(tbl))
  }

  stray_xpath <- paste0(
    "(./node()[not(self::tr or self::thead or self::tbody or self::tfoot or self::caption ",
    "or self::colgroup or self::col)] | ",
    "./*[self::thead or self::tbody or self::tfoot]/node()[not(self::tr)] | ",
    ".//tr[count(ancestor::table) = %d]/node()[not(self::td or self::th or .//table or ",
    "self::table)])",
    "[not(self::text()) or normalize-space(.) != '']",
    "[not(.//*[(self::tr or self::td or self::th) and count(ancestor::table) = %d])]")

  read_table <- function(tbl, in_data = FALSE) {
    # Rows and cells of THIS table, however thead/tbody wrap them, and not
    # those of a table nested in one of its cells.
    level <- length(find(tbl, "ancestor-or-self::table"))
    rows <- find(tbl, sprintf(".//tr[count(ancestor::table) = %d]", level))
    cell_xpath <- sprintf(".//*[self::th or self::td][count(ancestor::table) = %d]", level)
    cells <- lapply(rows, function(row) {
      # Cells are the row's own children in all but broken markup, and asking
      # for those is much cheaper than a query per row.
      kids <- xml2::xml_children(row)
      if (all(tolower(xml2::xml_name(kids)) %in% c("td", "th"))) kids else find(row, cell_xpath)
    })
    if (is_layout(tbl, cells, level, in_data)) return(walk(tbl))
    # Markup that is in the table but in none of its rows or cells -- a <p>
    # before the first row, a div after the last -- is not dropped: a browser
    # shows it before the table, and so it is read there. (A table in a row
    # outside its cells is read after the row, with the ones inside them.)
    stray <- find(tbl, sprintf(stray_xpath, level, level))
    if (length(stray)) {
      visit(stray)
      flush()
    }
    for (cap in find(tbl, "./caption")) emit(flat_text(cap), "body")
    has_nested <- length(find(tbl, ".//table")) > 0L
    for (r in seq_along(rows)) {
      vals <- vapply(cells[[r]], flat_text, character(1))
      if (any(nzchar(vals))) emit(paste(vals, collapse = " | "), "table")
      if (!has_nested) next
      for (inner in find(rows[[r]], sprintf(".//table[count(ancestor::table) = %d]", level))) {
        read_table(inner, in_data = TRUE)
      }
    }
  }

  # The run of inline text since the last block boundary.
  run <- text_pieces()
  flush <- function() {
    s <- run$take()
    if (nzchar(s)) emit(squish(s), "body")
  }
  walk <- function(node) visit(xml2::xml_contents(node))
  visit <- function(nodes) {
    for (ch in nodes) {
      ty <- xml2::xml_type(ch)
      if (identical(ty, "text")) {
        run$add(source_text(ch))
        next
      }
      if (!identical(ty, "element")) next            # comments and the like
      nm <- tolower(xml2::xml_name(ch))
      if (identical(nm, "br")) {
        run$add("\n")
      } else if (!nm %in% blocks) {
        walk(ch)                                       # inline: part of the run
      } else {
        flush()
        if (grepl("^h[1-6]$", nm)) emit(flat_text(ch), "heading")
        else if (identical(nm, "pre")) emit(trimws(xml2::xml_text(ch)), "code")
        else if (identical(nm, "table")) read_table(ch)
        else walk(ch)
        flush()
      }
    }
  }
  root <- xml2::xml_find_first(doc, "//body", ns)
  if (inherits(root, "xml_missing")) root <- xml2::xml_root(doc)
  if (!inherits(root, "xml_missing")) walk(root)
  flush()

  keep <- seq_len(n)
  data.frame(text = text[keep], section = section[keep], kind = kind[keep],
             stringsAsFactors = FALSE)
}

#' A string built from many pieces, appended one at a time.
#'
#' `x <- c(x, piece)` copies `x` on every append, so a run of n pieces cost
#' O(n^2): a paragraph of 80,000 spans took 35 seconds to read, and a page of
#' 30,000 <br> lines 5. The pieces go in a vector grown by doubling instead,
#' and `take()` pastes them once, returns the string and starts over.
#' @noRd
text_pieces <- function() {
  buf <- character(16L); len <- 0L
  list(
    add = function(s) {
      if (len == length(buf)) length(buf) <<- 2L * length(buf)
      len <<- len + 1L
      buf[len] <<- s
      invisible(NULL)
    },
    take = function(collapse = "") {
      if (!len) return("")
      s <- paste(buf[seq_len(len)], collapse = collapse)
      len <<- 0L
      s
    })
}

#' @noRd
extract_pdf <- function(path, opts) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    gr_abort("Reading PDFs needs the 'pdftools' package.", class = "gr_missing_dep")
  }
  pages <- pdftools::pdf_text(path)
  pages <- vapply(pages, as_chr1, character(1), USE.NAMES = FALSE)
  # Pages whose text came from OCR rather than the text layer. Tesseract already
  # returns a page in reading order, so the column pass below leaves them alone.
  ocr_done <- rep(FALSE, length(pages))

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
      res <- ocr_pdf_pages(path, pages, needs, opts)
      pages <- res$pages
      unread <- res$unread
      ocr_done <- res$ocr_done
    }
  }

  # Emit one block per paragraph per page. Page provenance is a real column, not
  # a "--- Page Break ---" marker glued into the text where it would be read as
  # document content.
  if (identical(opts[["layout", exact = TRUE]] %||% "auto", "raw")) {
    out <- do.call(rbind, lapply(seq_along(pages), function(i) {
      p <- paragraphs_of(pages[i])
      if (!length(p)) return(NULL)
      data.frame(text = p, page = i, section = NA_character_, kind = "body",
                 stringsAsFactors = FALSE)
    }))
  } else {
    # Reading order: running heads and feet out, two columns read one after the
    # other, and headings found so the blocks carry sections. See ingest-pdf.R.
    lines <- drop_running_lines(lapply(pages, page_lines))
    for (i in which(!ocr_done)) {
      g <- column_gutter(lines[[i]])
      if (!is.na(g)) lines[[i]] <- reorder_columns(lines[[i]], g)
    }
    out <- pdf_page_blocks(lines, heading_matcher(pdf_outline_titles(path)))
    if (!nrow(out)) out <- NULL
  }
  out <- as_blocks(out %||% data.frame(text = character(0)))
  attr(out, "gr_unread_pages") <- unread
  out
}

#' OCR the pages of a PDF marked in `needs`.
#'
#' Returns `pages` with the OCR text in place, `unread` (pages that never
#' became text) and `ocr_done` (pages whose text came from OCR). `engine`,
#' `read_page` and `ocr` are tesseract and magick; they are arguments so the
#' mechanism can be exercised without them.
#'
#' The engine is an external pointer, and a pointer does not survive being
#' copied into another process. Built once in the caller and captured by the
#' page function, it reached every parallel worker dead, so under
#' `parallel = TRUE` every page "failed" OCR. It is still built here first, so
#' a bad `ocr_lang` stops the read as it always did, and rebuilt in any other
#' process that reads a page, once per process.
#'
#' A page whose OCR fails keeps the text layer it had. Replacing it with ""
#' threw away good text: under `ocr = "always"` a born-digital PDF whose OCR
#' failed lost every page. As when OCR is not installed, a page counts as
#' unread only when nothing of it became text.
#' @noRd
ocr_pdf_pages <- function(path, pages, needs, opts,
                          engine = function(lang) tesseract::tesseract(lang),
                          read_page = function(path, i, dpi) {
                            magick::image_read_pdf(path, pages = i, density = dpi)
                          },
                          ocr = function(img, eng) tesseract::ocr(img, engine = eng),
                          workers = NULL) {
  force(engine); force(read_page); force(ocr)
  lang <- as_chr1(opts$ocr_lang %||% "eng")
  dpi <- as.numeric(opts$ocr_dpi %||% 300)
  eng <- engine(lang)
  eng_pid <- Sys.getpid()
  idx <- which(needs)
  res <- gr_lapply(idx, function(i, trace) {
    tryCatch({
      if (!identical(eng_pid, Sys.getpid())) {
        eng <<- engine(lang)
        eng_pid <<- Sys.getpid()
      }
      list(text = as_chr1(ocr(read_page(path, i, dpi), eng)), failed = FALSE)
    }, error = function(e) {
      gr_warn(sprintf("OCR failed on page %d: %s", i, conditionMessage(e)),
              class = "gr_ocr_failed")
      list(text = "", failed = TRUE)
    })
  }, parallel = opts$parallel, workers = workers, label = "OCR page")
  failed <- vapply(res, function(r) isTRUE(r$failed), logical(1))
  pages[idx[!failed]] <- vapply(res[!failed], function(r) as_chr1(r$text), character(1))
  ocr_done <- rep(FALSE, length(pages))
  ocr_done[idx[!failed]] <- TRUE
  list(pages = pages, ocr_done = ocr_done,
       unread = idx[failed & !nzchar(trimws(pages[idx]))])
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
    # Tables row by row, notes beside what cites them, headings by style name;
    # see ingest-docx.R.
    blocks <- docx_blocks(dir)
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
                        "Word: headings, tables by row, footnotes and endnotes; OCRs embedded images")
  gr_register_extractor("image", c("png", "jpg", "jpeg", "tif", "tiff", "bmp", "gif"),
                        extract_image, "Image OCR")
  invisible(NULL)
}
