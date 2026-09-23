# ingest-docx.R: the text of a Word file, in the order and shape a reader sees.
#
# A .docx is a zip of XML parts, and the text is in word/document.xml. Reading
# every paragraph element there, as the first version did, got three things
# wrong:
#
#   - A table is paragraphs inside cells, so every cell became a paragraph of
#     its own: a row of a results table came out as four unrelated lines, with
#     each value separated from its label.
#   - Footnotes and endnotes live in word/footnotes.xml and word/endnotes.xml,
#     which were never opened, so whatever a document put in its notes was not
#     in the text at all.
#   - No heading was ever recognised. The style was read without its namespace,
#     which always gives NA, so no Word file had sections. The rule it meant to
#     apply, a style ID starting "Heading", would still have missed most
#     documents written in another language: Word translates the ID (a German
#     "Heading 1" is "berschrift1") and keeps the style's NAME in English, and
#     word/styles.xml maps one to the other.
#
# Nothing here needs more than xml2, which the extractor already required.

#' The WordprocessingML namespace a part uses: the usual one, or the one "strict"
#' Open XML files use.
#' @noRd
docx_ns <- function(x) {
  uris <- unname(xml2::xml_ns(x))
  w <- uris[grepl("wordprocessingml(/2006)?/main$", uris)][1]
  c(w = if (is.na(w)) "http://schemas.openxmlformats.org/wordprocessingml/2006/main" else w)
}

#' Characters of the Symbol font, which Word's Insert Symbol uses for Greek
#' letters and mathematical signs, by their code in that font. A `w:sym` run
#' names the font and the code rather than holding the character.
#' @noRd
.gr_symbol_font <- c(
  "20" = " ", "22" = "\u2200", "24" = "\u2203", "25" = "%", "27" = "\u220b", "2B" = "+",
  "2D" = "\u2212", "3C" = "<", "3D" = "=", "3E" = ">",
  "41" = "\u0391", "42" = "\u0392", "43" = "\u03a7", "44" = "\u0394", "45" = "\u0395",
  "46" = "\u03a6", "47" = "\u0393", "48" = "\u0397", "49" = "\u0399", "4B" = "\u039a",
  "4C" = "\u039b", "4D" = "\u039c", "4E" = "\u039d", "4F" = "\u039f", "50" = "\u03a0",
  "51" = "\u0398", "52" = "\u03a1", "53" = "\u03a3", "54" = "\u03a4", "55" = "\u03a5",
  "57" = "\u03a9", "58" = "\u039e", "59" = "\u03a8", "5A" = "\u0396",
  "61" = "\u03b1", "62" = "\u03b2", "63" = "\u03c7", "64" = "\u03b4", "65" = "\u03b5",
  "66" = "\u03c6", "67" = "\u03b3", "68" = "\u03b7", "69" = "\u03b9", "6B" = "\u03ba",
  "6C" = "\u03bb", "6D" = "\u03bc", "6E" = "\u03bd", "6F" = "\u03bf", "70" = "\u03c0",
  "71" = "\u03b8", "72" = "\u03c1", "73" = "\u03c3", "74" = "\u03c4", "75" = "\u03c5",
  "77" = "\u03c9", "78" = "\u03be", "79" = "\u03c8", "7A" = "\u03b6",
  "A3" = "\u2264", "A5" = "\u221e", "AB" = "\u2194", "AC" = "\u2190", "AD" = "\u2191",
  "AE" = "\u2192", "AF" = "\u2193", "B0" = "\u00b0", "B1" = "\u00b1", "B3" = "\u2265",
  "B4" = "\u00d7", "B5" = "\u221d", "B7" = "\u2022", "B8" = "\u00f7", "B9" = "\u2260",
  "BA" = "\u2261", "BB" = "\u2248", "D6" = "\u221a", "D7" = "\u22c5", "D8" = "\u00ac",
  "D9" = "\u2227", "DA" = "\u2228", "E5" = "\u2211"
)

#' The character a `w:sym` run stands for. Symbol-font codes are looked up; a
#' code outside the private-use range that fonts like Symbol and Wingdings map
#' into is the character itself. Anything else is a space, so the words either
#' side of it do not run together.
#' @noRd
docx_symbol <- function(font, code) {
  code <- toupper(as_chr1(code, ""))
  n <- suppressWarnings(strtoi(code, 16L))
  if (is.na(n)) return(" ")
  if (grepl("^symbol$", as_chr1(font, ""), ignore.case = TRUE)) {
    key <- sprintf("%02X", n %% 256L)
    if (!is.na(.gr_symbol_font[key])) return(unname(.gr_symbol_font[key]))
    return(" ")
  }
  if (n >= 0xE000 && n <= 0xF8FF) " " else intToUtf8(n)
}

#' The text of a paragraph as it reads: runs in order, a tab or a line break as
#' a space, a non-breaking hyphen as a hyphen, a symbol as its character. Text
#' in a text box belongs to the box, which is read on its own, and text a
#' tracked change moved away is read where it was moved to.
#'
#' `depth` is how many text boxes the paragraph sits inside: a run belongs to it
#' when it sits inside exactly as many, and one inside more is in a text box the
#' paragraph anchors.
#' @noRd
docx_para_text <- function(p, ns, depth = 0L) {
  bits <- xml2::xml_find_all(p, sprintf(paste0(
    ".//w:r[count(ancestor::w:txbxContent) = %d and not(ancestor::w:moveFrom)]",
    "/*[self::w:t or self::w:tab or self::w:br or self::w:cr or self::w:noBreakHyphen ",
    "or self::w:sym]"), as.integer(depth)), ns)
  if (!length(bits)) return("")
  kind <- xml2::xml_name(bits)
  txt <- rep(" ", length(bits))
  is_t <- kind == "t"
  txt[is_t] <- xml2::xml_text(bits[is_t])
  txt[kind == "noBreakHyphen"] <- "-"
  for (i in which(kind == "sym")) {
    txt[i] <- docx_symbol(xml2::xml_attr(bits[[i]], "w:font", ns),
                          xml2::xml_attr(bits[[i]], "w:char", ns))
  }
  gsub("[ \t]+", " ", paste(txt, collapse = ""))
}

#' Which paragraph styles are headings, by style ID.
#'
#' A style is a heading when its name is one of Word's built-in heading names
#' ("heading 1" to "heading 9", "Title"), or when it sets an outline level,
#' directly or through the style it is based on. That second rule is what a
#' custom heading style ("Chapter", based on "Heading 1") relies on. NULL when
#' the file has no styles part.
#' @noRd
docx_heading_styles <- function(path) {
  if (!file.exists(path)) return(NULL)
  s <- tryCatch(xml2::read_xml(path), error = function(e) NULL)
  if (is.null(s)) return(NULL)
  ns <- docx_ns(s)
  styles <- xml2::xml_find_all(s, "//w:style[@w:type='paragraph']", ns)
  if (!length(styles)) return(logical(0))
  attr_of <- function(xpath) {
    vapply(styles, function(st) {
      node <- xml2::xml_find_first(st, xpath, ns)
      if (inherits(node, "xml_missing")) NA_character_
      else as_chr1(xml2::xml_attr(node, "w:val", ns), NA_character_)
    }, character(1))
  }
  id <- xml2::xml_attr(styles, "w:styleId", ns)
  name <- tolower(attr_of("./w:name"))
  based <- attr_of("./w:basedOn")
  level <- suppressWarnings(as.integer(attr_of("./w:pPr/w:outlineLvl")))
  # An outline level is inherited through basedOn. Resolved a fixed number of
  # times, so a chain that loops cannot hang the extractor.
  for (pass in seq_len(10L)) {
    missing <- is.na(level) & !is.na(based)
    if (!any(missing)) break
    level[missing] <- level[match(based[missing], id)]
  }
  head <- grepl("^(heading [1-9]|title)$", name) | (!is.na(level) & level <= 8L)
  stats::setNames(head, id)
}

#' Footnotes or endnotes by ID. The separators Word keeps in the same part carry
#' a `w:type` and are not notes.
#' @noRd
docx_notes <- function(path, what) {
  if (!file.exists(path)) return(list())
  x <- tryCatch(xml2::read_xml(path), error = function(e) NULL)
  if (is.null(x)) return(list())
  ns <- docx_ns(x)
  notes <- xml2::xml_find_all(x, sprintf("//w:%s[not(@w:type) or @w:type='normal']", what), ns)
  if (!length(notes)) return(list())
  ns <- c(ns, mc = "http://schemas.openxmlformats.org/markup-compatibility/2006")
  txt <- vapply(notes, function(n) {
    paras <- xml2::xml_find_all(n, ".//w:p[not(ancestor::mc:Fallback)]", ns)
    trimws(paste(vapply(paras, docx_para_text, character(1), ns = ns), collapse = " "))
  }, character(1))
  ids <- xml2::xml_attr(notes, "w:id", ns)
  keep <- nzchar(txt) & !is.na(ids)
  stats::setNames(as.list(txt[keep]), ids[keep])
}

#' The blocks of an unzipped Word file, in reading order.
#'
#' Paragraphs are `body`, or `heading` when their style is one; the heading is
#' the `section` of what follows it. A table is one block per row, `table`, its
#' cells joined with " | ", and a table inside a cell is read as rows of its own
#' after the row that holds it. A footnote or endnote is a `footnote` block
#' placed straight after the paragraph or row that first cites it, numbered in
#' the order the text cites them, so it is read with what it annotates; one that
#' nothing cites goes at the end. A text box is read after the paragraph it is
#' anchored to.
#' @noRd
docx_blocks <- function(dir) {
  x <- xml2::read_xml(file.path(dir, "word", "document.xml"))
  ns <- c(docx_ns(x), mc = "http://schemas.openxmlformats.org/markup-compatibility/2006")
  heading_styles <- docx_heading_styles(file.path(dir, "word", "styles.xml"))
  notes <- list(footnote = docx_notes(file.path(dir, "word", "footnotes.xml"), "footnote"),
                endnote = docx_notes(file.path(dir, "word", "endnotes.xml"), "endnote"))
  cited <- list(footnote = character(0), endnote = character(0))
  label <- c(footnote = "Footnote", endnote = "Endnote")

  # Grown by doubling: appending to a vector one block at a time copied it
  # every time, which made a long document take quadratic time.
  size <- 256L; n <- 0L
  text <- character(size); section <- character(size); kind <- character(size)
  current <- NA_character_
  emit <- function(t, k) {
    if (n == size) {
      size <<- size * 2L
      length(text) <<- size; length(section) <<- size; length(kind) <<- size
    }
    n <<- n + 1L
    text[n] <<- t
    section[n] <<- current
    kind[n] <<- k
  }
  is_heading <- function(p) {
    props <- xml2::xml_find_all(p, "./w:pPr/w:outlineLvl | ./w:pPr/w:pStyle", ns)
    if (!length(props)) return(FALSE)
    nm <- xml2::xml_name(props)
    val <- xml2::xml_attr(props, "w:val", ns)
    if ("outlineLvl" %in% nm) {
      lvl <- suppressWarnings(as.integer(val[nm == "outlineLvl"][1]))
      return(!is.na(lvl) && lvl <= 8L)
    }
    id <- as_chr1(val[nm == "pStyle"][1], "")
    # A style the styles part does not define (or no styles part at all): the
    # English style IDs are all there is to go on.
    if (!id %in% names(heading_styles)) return(grepl("^Heading|^Title", id, ignore.case = TRUE))
    isTRUE(unname(heading_styles[id]))
  }
  cite_notes <- function(node) {
    refs <- xml2::xml_find_all(node, ".//w:footnoteReference | .//w:endnoteReference", ns)
    if (!length(refs)) return(invisible(NULL))
    what <- sub("Reference$", "", xml2::xml_name(refs))
    ids <- xml2::xml_attr(refs, "w:id", ns)
    for (i in seq_along(refs)) {
      w <- what[i]; id <- ids[i]
      if (is.na(id) || id %in% cited[[w]] || is.null(notes[[w]][[id]])) next
      cited[[w]] <<- c(cited[[w]], id)
      emit(sprintf("[%s %d] %s", label[[w]], length(cited[[w]]), notes[[w]][[id]]), "footnote")
    }
    invisible(NULL)
  }
  read_table <- function(tbl, depth) {
    # Rows and cells of THIS table, wherever a content control or custom XML
    # puts them, and not those of a table nested in one of its cells.
    level <- length(xml2::xml_find_all(tbl, "ancestor-or-self::w:tbl", ns))
    rows <- xml2::xml_find_all(tbl, sprintf(".//w:tr[count(ancestor::w:tbl) = %d]", level), ns)
    for (row in rows) {
      cells <- xml2::xml_find_all(row, sprintf(".//w:tc[count(ancestor::w:tbl) = %d]", level), ns)
      vals <- vapply(cells, function(cell) {
        paras <- xml2::xml_find_all(cell, sprintf(
          ".//w:p[not(ancestor::mc:Fallback) and count(ancestor::w:tbl) = %d]", level), ns)
        trimws(paste(vapply(paras, function(p) docx_para_text(
          p, ns, length(xml2::xml_find_all(p, "ancestor::w:txbxContent", ns))),
          character(1)), collapse = " "))
      }, character(1))
      if (any(nzchar(vals))) emit(paste(vals, collapse = " | "), "table")
      cite_notes(row)
      nested <- xml2::xml_find_all(row, sprintf(".//w:tbl[count(ancestor::w:tbl) = %d]", level), ns)
      for (inner in nested) read_table(inner, depth)
    }
  }
  walk <- function(nodes, depth) {
    for (node in nodes) {
      nm <- xml2::xml_name(node)
      if (identical(nm, "p")) {
        t <- trimws(docx_para_text(node, ns, depth))
        if (nzchar(t)) {
          if (is_heading(node)) {
            current <<- t
            emit(t, "heading")
          } else {
            emit(t, "body")
          }
        }
        cite_notes(node)
        # A text box drawn two ways (DrawingML, and VML for older readers)
        # holds its text twice; the fallback copy is not read.
        boxes <- xml2::xml_find_all(node, sprintf(paste0(
          ".//w:txbxContent[not(ancestor::mc:Fallback) and ",
          "count(ancestor::w:txbxContent) = %d]"), as.integer(depth)), ns)
        for (b in boxes) walk(xml2::xml_children(b), depth + 1L)
      } else if (identical(nm, "tbl")) {
        read_table(node, depth)
      } else if (nm %in% c("sdt", "customXml", "smartTag")) {
        inner <- xml2::xml_find_first(node, "./w:sdtContent", ns)
        walk(xml2::xml_children(if (inherits(inner, "xml_missing")) node else inner), depth)
      }
    }
  }
  body <- xml2::xml_find_first(x, "/w:document/w:body", ns)
  if (!inherits(body, "xml_missing")) walk(xml2::xml_children(body), 0L)

  # Notes nothing in the text cites, still part of the document.
  current <- NA_character_
  for (w in c("footnote", "endnote")) {
    for (id in setdiff(names(notes[[w]]), cited[[w]])) {
      cited[[w]] <- c(cited[[w]], id)
      emit(sprintf("[%s %d] %s", label[[w]], length(cited[[w]]), notes[[w]][[id]]), "footnote")
    }
  }
  keep <- seq_len(n)
  data.frame(text = text[keep], section = section[keep], kind = kind[keep],
             stringsAsFactors = FALSE)
}
