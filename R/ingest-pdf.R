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
#' A short document is held to the pattern rather than the count: a line on
#' every page but the first (a title page is often styled apart), or on every
#' even or every odd page after the first (a two-sided layout alternates its
#' heads), is running on two pages. Otherwise a paper of three to six pages kept
#' its heads, since no one of them reaches three pages.
#'
#' A line recurs when it is the same text, or the same text once its numbers
#' are ignored, and then only when the numbers behave as a page number does:
#' each one is the same on every page or goes up with the page, one for one.
#' The rows of a table continued across pages read the same with their numbers
#' ignored ("1971 33.8 4.2 5120") and were dropped from the edge of every page;
#' their numbers do neither. For a line of more than `max_words` words the
#' number that changes must also stand at its start or end, as a page number
#' set in a running head does ("IEEE TRANSACTIONS ON ..., AUGUST 2026   3"),
#' and not inside a sentence that happens to name the page.
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
  need <- max(min_pages, ceiling(share * n))
  pattern <- list(setdiff(seq_len(n), 1L), seq(2L, n, by = 2L),
                  setdiff(seq(1L, n, by = 2L), 1L))
  pattern <- pattern[lengths(pattern) >= 2L]
  enough <- function(on) {
    length(on) >= need || any(vapply(pattern, function(p) all(p %in% on), logical(1)))
  }
  # Whether the numbers of one line on each page (rows, in page order) behave
  # as a running line's do. Most steps must move with the page, not all, so a
  # page left unnumbered or a restart does not undo it.
  page_like <- function(nums, on, key) {
    if (anyDuplicated(on)) return(FALSE) # twice at one edge of a page: rows of a table
    v <- do.call(rbind, nums)
    changes <- apply(v, 2, function(x) length(unique(x)) > 1L)
    follows <- apply(v, 2, function(x) mean(diff(x) == diff(on)) >= 0.5)
    if (!all(follows[changes])) return(FALSE)
    words <- strsplit(key, " ", fixed = TRUE)[[1]]
    if (length(words) <= max_words) return(TRUE)
    ends <- logical(ncol(v))
    ends[1] <- words[1] == "#"
    ends[ncol(v)] <- ends[ncol(v)] || words[length(words)] == "#"
    all(ends[changes])
  }
  drop_side <- function(side) {
    rec <- do.call(rbind, lapply(seq_len(n), function(i) {
      idx <- edges[[i]][[side]]
      if (!length(idx)) return(NULL)
      data.frame(page = i, line = idx, same = running_key(pages[[i]][idx]),
                 loose = running_key(pages[[i]][idx], FALSE), stringsAsFactors = FALSE)
    }))
    if (is.null(rec)) return(rec)
    hit <- logical(nrow(rec))
    for (k in unique(rec$same)) {
      at <- rec$same == k
      if (enough(unique(rec$page[at]))) hit[at] <- TRUE
    }
    for (k in unique(rec$loose[!hit & grepl("#", rec$loose, fixed = TRUE)])) {
      at <- which(rec$loose == k)
      at <- at[order(rec$page[at])]
      if (!enough(unique(rec$page[at]))) next
      nums <- lapply(rec$same[at], function(x)
        as.numeric(regmatches(x, gregexpr("[0-9]+", x))[[1]]))
      if (page_like(nums, rec$page[at], k)) hit[at] <- TRUE
    }
    rec[hit, c("page", "line"), drop = FALSE]
  }
  drop <- rbind(drop_side("top"), drop_side("bottom"))
  if (is.null(drop) || !nrow(drop)) return(pages)
  lapply(seq_len(n), function(i) {
    l <- pages[[i]]
    d <- unique(drop$line[drop$page == i])
    if (length(d)) l[-d] else l
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
#'
#' A page that fails these tests is not given up on: when the baselines of the
#' two columns do not line up, pdftotext gives each column's line a row of its
#' own, and on a last page the right column can be a few lines long, so few
#' lines have text on both sides of the gutter. `one_sided_gutter()` looks for
#' the right column's edge instead, under its own tests.
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
  none <- function() one_sided_gutter(chars, lo, hi, min_side = min_side)
  best <- max(support)
  if (best < min_lines) return(none())
  # The middle of the widest stretch at the best support, so a gutter that
  # wanders by a character from line to line is still inside it.
  at <- which(support == best)
  run <- split(at, cumsum(c(1L, diff(at) != 1L)))
  run <- run[[which.max(lengths(run))]]
  p <- run[ceiling(length(run) / 2)]
  if (support[p] / (support[p] + against[p]) < min_share) return(none())
  at <- p + lo - 1L
  split_lines <- which(gap[, at])
  side <- function(from, to) vapply(chars[split_lines], function(ch) {
    part <- ch[seq(from, min(to, length(ch)))]
    sum(part != " ")
  }, integer(1))
  if (stats::median(side(1L, at - 1L)) < min_side ||
      stats::median(side(at + 1L, width)) < min_side) {
    return(none())
  }
  as.integer(at)
}

#' The gutter of a page whose two columns seldom share a row, from where the
#' right column starts: the lines that hold only the right column (or that
#' reach it across a run of spaces) begin at one position in the middle of the
#' page, and few lines run through it. A row where pdftotext left one space
#' between a left line and a right one starting there counts among the lines
#' that start there, but cannot count alone: two lines at least must start
#' there after a run of spaces. Both columns must read as prose, as in
#' `column_gutter()`, and the left one must be a column of `min_left` lines at
#' least, so that a few rows of a two-column table on a page of prose are not
#' taken for one. Returns the position just left of the right column, or NA.
#' @noRd
one_sided_gutter <- function(chars, lo, hi, min_lines = 3L, min_share = 0.5,
                             min_side = 20L, min_left = 5L) {
  wide_starts <- function(ch) {
    j <- which(ch != " ")
    j[j >= max(3L, lo) & j <= hi & ch[pmax(1L, j - 1L)] == " " & ch[pmax(1L, j - 2L)] == " "]
  }
  cnt <- tabulate(unlist(lapply(chars, wide_starts)), nbins = hi + 1L)
  near <- cnt + c(0L, utils::head(cnt, -1L)) + c(cnt[-1L], 0L)
  if (max(near) < 2L) return(NA_integer_)
  r <- which.max(near)
  r <- intersect((r - 1L):(r + 1L), seq_along(cnt))
  r <- r[which.max(cnt[r])]
  starts_at <- function(ch, wide) any(vapply((r - 1L):(r + 1L), function(p) {
    p <= length(ch) && ch[p] != " " && ch[p - 1L] == " " && (!wide || ch[p - 2L] == " ")
  }, logical(1)))
  wide <- vapply(chars, starts_at, logical(1), wide = TRUE)
  tight <- !wide & vapply(chars, starts_at, logical(1), wide = FALSE)
  # A line with text on both sides and no run of spaces at the edge runs
  # through it. A row with one space there is counted so too, even when it
  # also counts as starting there: a line of prose has a word starting near
  # any position.
  through <- vapply(chars, function(ch) {
    if (length(ch) < r || !any(ch[seq_len(r - 3L)] != " ")) return(FALSE)
    near <- ch[(r - 3L):min(length(ch), r + 2L)] == " "
    !any(near[-1L] & near[-length(near)])
  }, logical(1))
  if (sum(wide) < 2L || sum(wide | tight) < min_lines ||
      sum(wide) / (sum(wide) + sum(through)) < min_share) {
    return(NA_integer_)
  }
  count <- function(ch, from, to) {
    to <- min(to, length(ch))
    if (from > to) 0L else sum(ch[from:to] != " ")
  }
  right <- vapply(chars[wide | tight], count, integer(1), from = r - 1L, to = .Machine$integer.max)
  left <- vapply(chars[!through], count, integer(1), from = 1L, to = r - 2L)
  left <- left[left > 0L]
  if (sum(left >= min_side) < min_left || stats::median(right) < min_side ||
      stats::median(left) < min_side) {
    return(NA_integer_)
  }
  as.integer(r - 1L)
}

#' Read a two-column page left column first.
#'
#' A line that spans both columns (a title, an abstract, a table or figure set
#' across the page) stays where it is and divides the page into bands, and
#' within each band the left column is read before the right. No blank line is
#' put between the two: a paragraph that runs from the foot of one column to the
#' head of the next is one paragraph. `column_plan()` decides which lines span
#' and where the others divide.
#'
#' A row with text in one column only leaves a blank line in the other, which
#' is right where that column has space (before a heading, around a table).
#' It is not when the two columns' baselines do not line up: pdftotext then
#' gives every line a row of its own, and a blank between every two lines cut
#' each column into one-line paragraphs, with its hyphenated words left broken.
#' So no blank is left where the other column's text runs on across the row, a
#' line ending mid-sentence on one side of it and text on the other.
#' @noRd
reorder_columns <- function(lines, gutter, min_side = 20L) {
  plan <- column_plan(lines, gutter, min_side)
  parts <- lapply(seq_along(lines), function(i) {
    ch <- plan$chars[[i]]
    n <- length(ch)
    s <- plan$split[i]
    c(sub("[ \t]+$", "", paste(ch[seq_len(min(n, s - 1L))], collapse = "")),
      if (n > s) paste(ch[(s + 1L):n], collapse = "") else "")
  })
  txt <- lapply(1:2, function(side) trimws(vapply(parts, `[`, character(1), side)))
  has <- lapply(txt, nzchar)
  runs_on <- function(side) {
    nxt <- c(has[[side]][-1L], FALSE) & !c(plan$span[-1L], TRUE)
    prv <- c(FALSE, grepl("[[:lower:],-]$", utils::head(txt[[side]], -1L)) &
               !utils::head(plan$span, -1L))
    !has[[side]] & has[[3L - side]] & prv & nxt
  }
  skip_left <- runs_on(1L)
  skip_right <- runs_on(2L)
  # The blank rows below the left column's last line and above the right
  # column's first are where the columns end and start, not a paragraph
  # break: left in, they parted a word hyphenated across the columns.
  band <- function(left, right) {
    filled <- function(x) which(nzchar(trimws(x)))
    if (length(filled(left)) && length(filled(right))) {
      left <- left[seq_len(max(filled(left)))]
      right <- right[min(filled(right)):length(right)]
    }
    c(left, right)
  }
  out <- character(0)
  left <- character(0)
  right <- character(0)
  for (i in seq_along(lines)) {
    if (plan$span[i]) {
      out <- c(out, band(left, right), lines[i])
      left <- character(0)
      right <- character(0)
      next
    }
    if (!skip_left[i]) left <- c(left, parts[[i]][1])
    if (!skip_right[i]) right <- c(right, parts[[i]][2])
  }
  c(out, band(left, right))
}

#' Where the text of `ch[from:to]` starts and ends, how many characters it
#' has, and its widest run of spaces. A run after a section number ("2    Methods")
#' is not counted: pdftotext sets the number apart, and the line is still a
#' heading in a column, not a row of cells.
#' @noRd
line_part <- function(ch, from, to) {
  to <- min(to, length(ch))
  if (from > to) return(NULL)
  t <- which(ch[from:to] != " ")
  if (!length(t)) return(NULL)
  s <- from + min(t) - 1L
  e <- from + max(t) - 1L
  r <- rle(ch[s:e] == " ")
  inner <- r$lengths[r$values]
  first_word <- paste(ch[s:(s + r$lengths[1] - 1L)], collapse = "")
  if (length(inner) && grepl("^([0-9]+(\\.[0-9]+)*\\.?|[IVX]+\\.?|[A-Z]\\.)$", first_word)) {
    inner <- inner[-1L]
  }
  list(start = s, end = e, chars = length(t), gap = max(0L, inner))
}

#' How each line of a two-column page is read: whether it spans the page
#' (`span`), and if not, the position that divides its left part from its
#' right (`split`). The lines' characters come back too (`chars`).
#'
#' Whether a line spans is not read from the gutter position alone, because
#' pdftotext's grid is not that regular:
#'
#'   - A row of a full-width table can have a blank at the gutter by chance.
#'     Split there, its label stayed in the left column and its values went to
#'     the foot of the right one, into another section. So a line with a blank
#'     at the gutter is read as two columns only when one side reads as a line
#'     of its column: it starts at that column's usual left edge and has no
#'     wide gap inside, as a row of cells does. When that side is too short to
#'     tell (the end of a paragraph), the lines around it decide.
#'   - A left line and a right line set on one row can have a single space
#'     between them, or run a character into the gutter. Read as spanning, the
#'     two were spliced into one line and the band was broken in two, putting
#'     the right column's text before the rest of the left one. Such a row is
#'     split where the right column's line starts: at that column's usual left
#'     edge, after a word the left line broke with a hyphen, or (the right line
#'     pulled left of its column) one space after a left line that ends at the
#'     left column's right margin. It is only looked for between lines that
#'     are plainly in two columns, since a line of full-width prose has a word
#'     boundary near the gutter too.
#'   - The first and last lines of a page must read as a line of each column
#'     on both sides. A running head that survived `drop_running_lines()` spans
#'     the page, and split, its right half was put where the left column ends,
#'     inside a sentence.
#' @noRd
column_plan <- function(lines, gutter, min_side = 20L) {
  gutter <- as.integer(gutter)
  chars <- lapply(lines, function(l) strsplit(l, "", fixed = TRUE)[[1]])
  nl <- length(chars)
  first <- vapply(chars, function(ch) {
    t <- which(ch != " ")
    if (length(t)) min(t) else NA_integer_
  }, integer(1))
  last <- vapply(chars, function(ch) {
    t <- which(ch != " ")
    if (length(t)) max(t) else NA_integer_
  }, integer(1))
  text_at <- function(ch, j) j >= 1L && j <= length(ch) && ch[j] != " "
  kind <- vapply(seq_len(nl), function(i) {
    ch <- chars[[i]]
    if (is.na(first[i])) return("blank")
    if (last[i] < gutter) return("left")
    if (first[i] >= gutter) return("right")
    if (!text_at(ch, gutter) && !(text_at(ch, gutter - 1L) && text_at(ch, gutter + 1L))) {
      return("split")
    }
    "cross"
  }, character(1))
  lpart <- lapply(seq_len(nl), function(i) {
    if (kind[i] %in% c("split", "left")) line_part(chars[[i]], 1L, gutter - 1L)
  })
  rpart <- lapply(seq_len(nl), function(i) {
    switch(kind[i], split = line_part(chars[[i]], gutter + 1L, length(chars[[i]])),
           right = line_part(chars[[i]], 1L, length(chars[[i]])), NULL)
  })
  # The columns' usual edges: where left lines start (L) and end (E, the right
  # margin of a justified column), and where right lines start (R).
  pick <- function(parts, what) unlist(lapply(parts, function(p) p[[what]]), use.names = FALSE)
  modal <- function(x, default) {
    if (length(x)) as.integer(names(which.max(table(x)))) else as.integer(default)
  }
  L <- modal(pick(lpart, "start"), 1L)
  R <- modal(pick(rpart, "start"), gutter + 1L)
  full <- Filter(function(p) !is.null(p) && p$chars >= min_side, lpart)
  E <- if (length(full)) as.integer(stats::median(pick(full, "end"))) else gutter - 1L
  column_like <- function(p, edge) {
    !is.null(p) && p$start >= edge - 2L && p$start <= edge + 6L && p$gap < 3L
  }
  long <- function(p) !is.null(p) && p$chars >= min_side
  # A long left line can run into the gutter and still stop short of the right
  # column, and a right line can start a character or two before the gutter:
  # neither holds anything of the other column.
  for (i in which(kind == "cross")) {
    whole <- line_part(chars[[i]], 1L, length(chars[[i]]))
    if (last[i] < R - 1L && column_like(whole, L)) {
      kind[i] <- "left"
    } else if (first[i] >= R - 2L) {
      kind[i] <- "right"
      rpart[[i]] <- whole
    }
  }
  merge_point <- function(ch) {
    n <- length(ch)
    sp <- which(ch == " ")
    sp <- sp[sp > 2L & sp < n]
    sp <- sp[ch[sp - 1L] != " " & ch[sp + 1L] != " "]
    broke <- sp[ch[sp - 1L] == "-" & grepl("[[:alpha:]]", ch[sp - 2L]) &
                  sp - 1L >= E - 10L & sp + 1L >= R - 12L & sp + 1L <= R + 1L]
    aligned <- sp[sp + 1L >= R - 1L & sp + 1L <= R + 1L & sp - 1L >= E - 6L]
    # Pulled left of its column, a right line starts one space after a left
    # line that ends at the column's right margin.
    pulled <- sp[sp + 1L >= R - 10L & sp + 1L <= R - 2L & sp - 1L >= E - 6L & sp - 1L <= E + 3L]
    ways <- list(hyphen = broke, edge = aligned, pulled = pulled)
    for (way in names(ways)) {
      cand <- ways[[way]]
      if (!length(cand)) next
      s <- cand[which.min(abs(cand - 1L - E))]
      rp <- line_part(ch, s + 1L, n)
      if (column_like(line_part(ch, 1L, s - 1L), L) && !is.null(rp) && rp$gap < 3L) {
        return(list(at = s, how = way))
      }
    }
    NULL
  }
  body <- which(kind != "blank")
  edge <- seq_len(nl) %in% c(utils::head(body, 1L), utils::tail(body, 1L))
  cls <- ifelse(kind == "blank", "blank", "two")
  split <- rep(gutter, nl)
  how <- character(nl)
  for (i in seq_len(nl)) {
    if (kind[i] == "left") {
      split[i] <- max(gutter, last[i] + 1L)
    } else if (kind[i] == "right") {
      split[i] <- min(gutter, first[i] - 1L)
      if (edge[i] && !column_like(rpart[[i]], R)) cls[i] <- "span"
    } else if (kind[i] == "split") {
      lo <- column_like(lpart[[i]], L)
      ro <- column_like(rpart[[i]], R)
      cls[i] <- if (lo && ro) {
        "two"
      } else if (edge[i] || (!lo && !ro)) {
        "span"
      } else if ((lo && long(lpart[[i]])) || (ro && long(rpart[[i]]))) {
        "two"
      } else {
        "unsure"
      }
    } else if (kind[i] == "cross") {
      m <- merge_point(chars[[i]])
      if (is.null(m)) {
        cls[i] <- "span"
      } else {
        cls[i] <- "merge"
        split[i] <- as.integer(m$at)
        how[i] <- m$how
      }
    }
  }
  # An undecided line is read in two columns only between lines that are, on
  # both sides, with no blank row and no more than two other undecided lines
  # between. A word broken with a hyphen is plain enough evidence to reach
  # across blank rows (a footnote beside the column keeps its own line spacing)
  # and to stand at the foot of the page, below which there may be only a page
  # number. A right line pulled left of its column is the weakest evidence,
  # since nearly any line of full-width prose has a word boundary there: it
  # needs text in the right column on both sides, where a line with nothing
  # right of the gutter (a heading, or a paragraph's last line) will do for
  # the others.
  bound <- function(i, step) {
    j <- i + step
    passed <- 0L
    while (j >= 1L && j <= nl) {
      if (cls[j] == "blank") {
        if (how[i] != "hyphen") return("blank")
      } else if (cls[j] %in% c("unsure", "merge")) {
        passed <- passed + 1L
        if (passed > 2L) return("run")
      } else if (cls[j] == "span") {
        return(if (edge[j]) "end" else "span")
      } else {
        return(if (kind[j] %in% c("split", "right")) "two" else "left")
      }
      j <- j + step
    }
    "end"
  }
  pending <- which(cls %in% c("unsure", "merge"))
  ok <- vapply(pending, function(i) {
    fits <- if (how[i] == "pulled") "two" else c("two", "left")
    down <- bound(i, 1L)
    bound(i, -1L) %in% fits && (down %in% fits || (how[i] == "hyphen" && down == "end"))
  }, logical(1))
  cls[pending] <- ifelse(ok, "two", "span")
  split[pending[!ok]] <- gutter
  list(span = cls == "span", split = split, chars = chars)
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
