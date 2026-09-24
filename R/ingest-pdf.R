# ingest-pdf.R -- the text of PDF pages, put back in reading order.
#
# pdftools::pdf_text() returns each page as the page LOOKS: lines of text, with
# spaces standing in for position. That is faithful, and for anything but a
# plain single-column page it is not the order a person reads in:
#
#   - Two columns come out side by side, so every line holds the start of a
#     line from each column and the two sentences are spliced together.
#   - The running head and foot repeat on every page and land in the middle of
#     the text wherever a page breaks.
#   - Headings are only lines, so a PDF had no sections at all: `structural`
#     chunking had nothing to follow and evidence could name only a page.
#
# The functions here work on those lines, page by page, before the text is cut
# into paragraphs. They look only at the layout pdftools already reports, so
# they add no dependency, and `gr_ingest_spec(layout = "raw")` turns them off.

#' The lines of one page, with trailing spaces removed.
#' @noRd
page_lines <- function(page) {
  page <- as_chr1(page)
  if (!nzchar(page)) return(character(0))
  sub("[ \t]+$", "", strsplit(normalise_newlines(page), "\n", fixed = TRUE)[[1]])
}

#' The forms in which a running line is recognised on different pages. Case
#' and spacing never matter. Numbers matter to `running_key()`, and not to
#' `running_key(numbers = FALSE)`, which is how "Page 3 of 12" and "Page 4 of
#' 12" are one running foot.
#' @noRd
running_key <- function(x, numbers = TRUE) {
  x <- tolower(gsub("[[:space:]]+", " ", trimws(x)))
  if (numbers) x else gsub("[0-9]+", "#", x)
}

#' Drop running heads and feet.
#'
#' A line is one when, among the first or last `depth` lines with text on a
#' page, it recurs on at least `share` of the pages (and on three at least).
#' Only those edge lines are candidates, so a line repeated in the body of the
#' document is never touched, and a line longer than `max_chars` (a paragraph,
#' not a line) is never one.
#'
#' A line recurs when it is the same text, or, for a line of at most
#' `max_words` words, the same text once its numbers are ignored. The word limit
#' is what keeps the second rule to page numbers and running feet: with numbers
#' ignored, the first line of a table or the last line of a paragraph can read
#' the same from page to page, and it is content.
#' @noRd
drop_running_lines <- function(pages, depth = 2L, share = 0.4, min_pages = 3L,
                               max_chars = 200L, max_words = 5L) {
  n <- length(pages)
  if (n < min_pages) return(pages)
  edges <- lapply(pages, function(l) {
    txt <- which(nzchar(trimws(l)) & nchar(trimws(l)) <= max_chars)
    all_txt <- which(nzchar(trimws(l)))
    list(top = intersect(utils::head(all_txt, depth), txt),
         bottom = intersect(utils::tail(all_txt, depth), txt))
  })
  short <- function(x) lengths(strsplit(trimws(x), "[[:space:]]+")) <= max_words
  need <- max(min_pages, ceiling(share * n))
  recurring <- function(side, numbers) {
    keys <- unlist(lapply(seq_len(n), function(i) {
      l <- pages[[i]][edges[[i]][[side]]]
      if (!numbers) l <- l[short(l)]
      unique(running_key(l, numbers))
    }), use.names = FALSE)
    if (!length(keys)) return(character(0))
    tab <- table(keys)
    names(tab)[tab >= need]
  }
  keys <- list(top = list(same = recurring("top", TRUE), numbered = recurring("top", FALSE)),
               bottom = list(same = recurring("bottom", TRUE),
                             numbered = recurring("bottom", FALSE)))
  if (!length(unlist(keys))) return(pages)
  lapply(seq_len(n), function(i) {
    l <- pages[[i]]
    drop <- unlist(lapply(c("top", "bottom"), function(side) {
      idx <- edges[[i]][[side]]
      k <- keys[[side]]
      idx[running_key(l[idx]) %in% k$same |
            (short(l[idx]) & running_key(l[idx], FALSE) %in% k$numbered)]
    }), use.names = FALSE)
    if (length(drop)) l[-unique(drop)] else l
  })
}

#' Where a page's two columns divide, as a character position, or NA.
#'
#' A gutter is a run of at least two spaces with text on both sides, at the same
#' position on at least five lines and on `min_share` of the lines that reach
#' across it. On a single-column page the text runs through the middle of almost
#' every line, so no position qualifies; lines that stop short of a position say
#' nothing either way. The share is low on purpose: the first page of a paper
#' often has a full-width abstract above two columns, and those columns still
#' need reading in order.
#'
#' A table lines up too, so the lines split at the gutter must also carry text
#' on both sides: at least `min_side` characters each, as a column of prose
#' does and a column of labels or figures does not. Reading a table one column
#' after the other would separate every value from its label.
#' @noRd
column_gutter <- function(lines, min_lines = 5L, min_share = 0.3, min_side = 20L) {
  lines <- lines[nzchar(trimws(lines))]
  if (length(lines) < min_lines) return(NA_integer_)
  chars <- lapply(lines, function(l) strsplit(l, "", fixed = TRUE)[[1]])
  width <- max(lengths(chars))
  if (width < 40L) return(NA_integer_)
  gap <- matrix(FALSE, length(chars), width)
  cross <- matrix(FALSE, length(chars), width)
  for (i in seq_along(chars)) {
    sp <- chars[[i]] == " "
    text_at <- which(!sp)
    if (!length(text_at)) next
    first <- min(text_at)
    last <- max(text_at)
    r <- rle(sp)
    ends <- cumsum(r$lengths)
    starts <- ends - r$lengths + 1L
    wide <- r$values & r$lengths >= 2L & starts > first & ends < last
    for (k in which(wide)) gap[i, starts[k]:ends[k]] <- TRUE
    cross[i, first:last] <- !gap[i, first:last]
  }
  lo <- max(2L, floor(width * 0.25))
  hi <- min(width - 1L, ceiling(width * 0.75))
  support <- colSums(gap)[lo:hi]
  against <- colSums(cross)[lo:hi]
  best <- max(support)
  if (best < min_lines) return(NA_integer_)
  # The middle of the widest stretch at the best support, so a gutter that
  # wanders by a character from line to line is still inside it.
  at <- which(support == best)
  run <- split(at, cumsum(c(1L, diff(at) != 1L)))
  run <- run[[which.max(lengths(run))]]
  p <- run[ceiling(length(run) / 2)]
  if (support[p] / (support[p] + against[p]) < min_share) return(NA_integer_)
  at <- p + lo - 1L
  split_lines <- which(gap[, at])
  side <- function(from, to) vapply(chars[split_lines], function(ch) {
    part <- ch[seq(from, min(to, length(ch)))]
    sum(part != " ")
  }, integer(1))
  if (stats::median(side(1L, at - 1L)) < min_side ||
      stats::median(side(at + 1L, width)) < min_side) {
    return(NA_integer_)
  }
  as.integer(at)
}

#' Read a two-column page left column first.
#'
#' A line whose text runs through the gutter (a title, an abstract, a figure set
#' across both columns) stays where it is and divides the page into bands, and
#' within each band the left column is read before the right. No blank line is
#' put between the two: a paragraph that runs from the foot of one column to the
#' head of the next is one paragraph.
#' @noRd
reorder_columns <- function(lines, gutter) {
  out <- character(0)
  left <- character(0)
  right <- character(0)
  for (l in lines) {
    ch <- strsplit(l, "", fixed = TRUE)[[1]]
    n <- length(ch)
    crossing <- n >= gutter && (ch[gutter] != " " ||
      (gutter > 1L && gutter < n && ch[gutter - 1L] != " " && ch[gutter + 1L] != " "))
    if (crossing) {
      out <- c(out, left, right, l)
      left <- character(0)
      right <- character(0)
    } else {
      left <- c(left, sub("[ \t]+$", "", paste(ch[seq_len(min(n, gutter - 1L))], collapse = "")))
      right <- c(right, if (n > gutter) paste(ch[(gutter + 1L):n], collapse = "") else "")
    }
  }
  c(out, left, right)
}

#' The titles in a PDF's bookmarks, in document order.
#' @noRd
pdf_outline_titles <- function(path) {
  toc <- tryCatch(pdftools::pdf_toc(path), error = function(e) NULL)
  found <- character(0)
  walk <- function(node, depth) {
    if (!is.list(node) || depth > 20L) return(invisible(NULL))
    title <- as_chr1(node[["title", exact = TRUE]], "")
    if (nzchar(trimws(title))) found <<- c(found, title)
    for (child in node[["children", exact = TRUE]] %||% list()) walk(child, depth + 1L)
    invisible(NULL)
  }
  walk(toc, 0L)
  unique(trimws(found))
}

#' The form in which a heading is recognised: case, spacing, leading section
#' numbers ("2.", "2.1", "IV.") and trailing punctuation ignored.
#' @noRd
heading_key <- function(x) {
  x <- tolower(gsub("[[:space:]]+", " ", trimws(x)))
  x <- sub("^(\\d+(\\.\\d+)*\\.?|[ivxlcdm]+\\.)\\s+", "", x, perl = TRUE)
  sub("[[:punct:][:space:]]+$", "", x, perl = TRUE)
}

#' Section names common enough in reports and papers to be recognised as
#' headings when a PDF has no bookmarks. Only a line that is nothing but one of
#' these (numbered or not) counts, so body text cannot match.
#' @noRd
.gr_pdf_headings <- c("abstract", "summary", "executive summary", "introduction",
                      "background", "method", "methods", "materials and methods",
                      "methodology", "results", "findings", "discussion", "conclusion",
                      "conclusions", "limitations", "recommendations", "references",
                      "bibliography", "acknowledgements", "acknowledgments", "appendix")

#' A function that says whether a line is a heading, and if so which.
#' @noRd
heading_matcher <- function(titles = character(0)) {
  keys <- heading_key(titles)
  function(line) {
    t <- trimws(line)
    if (!nzchar(t)) return(NA_character_)
    k <- heading_key(t)
    if (!nzchar(k)) return(NA_character_)
    j <- match(k, keys)
    if (!is.na(j)) return(titles[[j]])
    if (k %in% .gr_pdf_headings || grepl("^appendix [a-z0-9]{1,3}$", k)) return(t)
    NA_character_
  }
}

#' Paragraph blocks for a document's pages, each with its page and the heading
#' it falls under. A heading carries over from one page to the next, as it does
#' for a reader.
#' @noRd
pdf_page_blocks <- function(pages, is_heading) {
  text <- character(0); page <- integer(0); section <- character(0); kind <- character(0)
  current <- NA_character_
  add <- function(p, i, what) {
    if (!length(p)) return(invisible(NULL))
    text <<- c(text, p)
    page <<- c(page, rep(i, length(p)))
    section <<- c(section, rep(current, length(p)))
    kind <<- c(kind, rep(what, length(p)))
    invisible(NULL)
  }
  for (i in seq_along(pages)) {
    buf <- character(0)
    for (l in pages[[i]]) {
      h <- is_heading(l)
      if (is.na(h)) {
        buf <- c(buf, l)
        next
      }
      add(paragraphs_of(paste(buf, collapse = "\n")), i, "body")
      buf <- character(0)
      current <- h
      add(trimws(l), i, "heading")
    }
    add(paragraphs_of(paste(buf, collapse = "\n")), i, "body")
  }
  data.frame(text = text, page = page, section = section, kind = kind,
             stringsAsFactors = FALSE)
}
