# read-verify.R -- did the model quote the document, or invent the quote?
#
# WHY THIS FILE EXISTS
# `ans$evidence` is what an answer rests on, and for most readers it is verbatim
# chunk text -- true by construction, because the package put it there. For the
# `skim` reader it is not: `skim` asks the model to extract the passages that
# bear on the question, and what comes back is whatever the model chose to
# write. It is *presented* as a quotation and nothing checked that it was one.
#
# That is the worst kind of gap. A fabricated citation is more convincing than a
# fabricated answer, because it looks like the thing that would let you check.
#
# Checking costs nothing. The chunk is right there, the span is right there, and
# whether one contains the other is a string operation. So it happens on every
# run, is recorded on the answer, and an unverifiable span makes the answer
# `partial` -- the same signal every other kind of degradation raises.
#
# The comparison is deliberately forgiving about typography and unforgiving
# about content. Models normalise whitespace, straighten quotes and turn en
# dashes into hyphens when they quote, and none of that is fabrication. Changing
# a number is.

#' Normalise text for quotation matching.
#'
#' Folds away exactly the differences a model introduces when it quotes
#' faithfully: whitespace, curly quotes, dashes, and case. Nothing else -- in
#' particular no stemming and no punctuation stripping, because "revenue fell"
#' and "revenue fell 12%" must not compare equal.
#' @noRd
normalise_for_match <- function(x) {
  x <- fold_for_match(to_utf8(as.character(x)))
  trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
}

#' The character-for-character part of normalise_for_match(): quote marks,
#' dashes and spaces to their plain forms, and lower case. Each character stays
#' one character, which is what lets the evidence page find a normalised
#' quotation in the original text (see normalised_with_map()).
#'
#' `keep = TRUE` folds only the quote marks, the hyphens and the minus sign,
#' and keeps case, the en dash, em dash and horizontal bar, and the no-break
#' and thin spaces as they were: what match_source() hands the boundary test.
#' @noRd
fold_for_match <- function(x, keep = FALSE) {
  # \u escapes, not literals: R CMD check flags non-ASCII bytes in R sources,
  # and a source file whose meaning depends on its own encoding is the bug this
  # package has already been bitten by twice.
  x <- gsub("[\u2018\u2019\u201a\u201b\u2032]", "'", x, perl = TRUE)
  x <- gsub("[\u201c\u201d\u201e\u201f\u2033]", '"', x, perl = TRUE)
  if (keep) return(gsub("[\u2010\u2011\u2012\u2212]", "-", x, perl = TRUE))
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2015\u2212]", "-", x, perl = TRUE)
  x <- gsub("[\u00a0\u2007\u2009\u202f]", " ", x, perl = TRUE)
  lower_text(x)
}

#' A source made ready for found_at().
#'
#' `text` is what normalise_for_match() makes of it, and is what a quotation
#' is looked for in. `cp` is the same text as code points, character for
#' character, but with what folding would hide from the boundary test kept as
#' it was: an em or en dash, which folds to a hyphen and then reads as a minus
#' sign ("cohort<em dash>42"), a no-break or thin space, which folds to a space
#' and then hides a digit group ("1 200" as SI and French typography write
#' it), and case. `kind` is char_kind() of each. Both are padded, two zeros
#' before and five after (0 is no character), so the characters around any
#' occurrence can be read without a bounds check: character `i` of `text` is
#' element `i + 2`.
#' @noRd
match_source <- function(x) {
  marks <- fold_for_match(to_utf8(as_chr1(x, "")), keep = TRUE)
  # One character for every run of spacing, as normalise_for_match() collapses
  # it, except that a lone grouping space stays what it was; then none at
  # either end.
  marks <- gsub("[[:space:]\u00a0\u2007\u2009\u202f]{2,}|[[:space:]]", " ", marks, perl = TRUE)
  cp <- utf8ToInt(marks)
  spacing <- function(x) x == 32L | x == 0xa0L | x == 0x2007L | x == 0x2009L | x == 0x202fL
  n <- length(cp)
  from <- 1L + (n > 0L && spacing(cp[1L]))
  to <- n - (n > 1L && spacing(cp[n]))
  if (from > 1L || to < n) {
    cp <- if (to >= from) cp[from:to] else integer(0)
    marks <- intToUtf8(cp)
  }
  hi <- which(cp > 127L)
  kind <- .gr_ascii_kind[replace(cp, hi, 0L) + 1L]
  text <- marks
  if (length(hi)) {
    u <- unique(cp[hi])
    kind[hi] <- char_kind(u)[match(cp[hi], u)]
    # The dashes and spaces kept for the boundary test, folded now, code
    # point by code point.
    folded <- cp
    folded[folded == 0x2013L | folded == 0x2014L | folded == 0x2015L] <- 45L
    folded[folded != 32L & spacing(folded)] <- 32L
    if (!identical(folded, cp)) text <- intToUtf8(folded)
  }
  list(text = lower_text(text), cp = c(0L, 0L, cp, integer(5L)), kind = c(0L, 0L, kind, integer(5L)),
       n = length(cp))
}

#' char_kind() of every ASCII code point, by code point + 1.
#' @noRd
.gr_ascii_kind <- c(integer(48), rep(2L, 10), integer(7), rep(1L, 26), integer(6), rep(1L, 26),
                    integer(5))

#' Markdown bold taken out: a model bolds what it quotes, and ingest keeps
#' "**Revenue**" from a .md file. Paired markers only, which "0.45**" (a
#' significance star) is not. `edges` also drops a marker opening or closing a
#' passage, for bold a model ran across two of them; not one after a digit.
#' @noRd
strip_bold <- function(x, edges = FALSE) {
  x <- gsub("(?<![\\p{L}\\p{N}*])\\*\\*(?=[^\\s*])(.+?)(?<=[^\\s*])\\*\\*(?![\\p{L}\\p{N}*])",
            "\\1", x, perl = TRUE)
  if (!edges) return(x)
  x <- gsub("^([[:space:]]*)\\*\\*(?=[^\\s*])", "\\1", x, perl = TRUE)
  gsub("(?<=[^\\s*\\p{N}])\\*\\*([[:space:]]*)$", "\\1", x, perl = TRUE)
}

#' Strip the punctuation a model puts around a quotation, and nothing else.
#'
#' Quotes get wrapped in quote marks, prefixed and suffixed with ellipses to mark
#' truncation, and given a trailing full stop the source did not have. None of
#' that is fabrication, and flagging it would make `partial` noisy enough to stop
#' meaning anything.
#'
#' Deliberately narrow: quote marks, ellipses, commas, semicolons, colons, full
#' stops, dashes and space. Not `%`, not `)`, not any digit -- those carry
#' content, and a quotation that changed one is exactly what this is for.
#' @noRd
trim_quote_edges <- function(x) {
  edge <- "[\"'\u2026.,;:[:space:]]+"
  strip <- function(s) gsub(paste0("^", edge, "|", edge, "$"), "", s, perl = TRUE)
  x <- strip(x)
  # Dashes are the awkward case. A trailing one is always typographic -- an em
  # dash the model appended, normalised to a hyphen. A LEADING one may be a
  # minus sign, and a minus sign is content: "-5%" and "5%" are opposite claims,
  # and folding them together let a sign-flipped quotation verify as exact. So a
  # leading dash goes only when what follows is not a number.
  x <- sub("^-+(?![0-9.])", "", x, perl = TRUE)
  x <- sub("-+$", "", x, perl = TRUE)
  trimws(strip(x))
}

#' The scripts written without spaces between words: Chinese, Japanese, Thai,
#' Lao, Khmer and Myanmar, the set lexical_terms() indexes by character pairs.
#' A clause quoted from one of them starts and ends between two letters,
#' because there is nothing else to start or end at, so a letter of these
#' scripts never marks a quotation as cut out of a word.
#' @noRd
.gr_unspaced <- "\\p{Han}\\p{Hiragana}\\p{Katakana}\\p{Thai}\\p{Lao}\\p{Khmer}\\p{Myanmar}"

#' What each code point is to the boundary test: 2 a digit, 1 a letter or mark
#' of a script that puts spaces between words, and 0 anything else --
#' punctuation, a space, a letter of an unspaced script (.gr_unspaced), or no
#' character at all (NA, past either end of the text). Unicode classes rather
#' than `[[:alnum:]]`, whose meaning changed with the locale.
#' @noRd
char_kind <- function(cp) {
  if (length(cp) == 1L && !is.na(cp) && cp < 128L) {
    return(if ((cp >= 97L && cp <= 122L) || (cp >= 65L && cp <= 90L)) 1L
           else if (cp >= 48L && cp <= 57L) 2L else 0L)
  }
  out <- integer(length(cp))
  ok <- !is.na(cp)
  out[ok & ((cp >= 97L & cp <= 122L) | (cp >= 65L & cp <= 90L))] <- 1L
  out[ok & cp >= 48L & cp <= 57L] <- 2L
  hi <- which(ok & cp > 127L)
  if (length(hi)) {
    ch <- intToUtf8(cp[hi], multiple = TRUE)
    out[hi[grepl("^[\\p{L}\\p{M}]$", ch, perl = TRUE) &
             !grepl(sprintf("^[%s]$", .gr_unspaced), ch, perl = TRUE)]] <- 1L
    out[hi[grepl("^\\p{N}$", ch, perl = TRUE)]] <- 2L
  }
  out
}

#' Which occurrences of a span, whose code points are `sc`, start and end on a
#' boundary in `src`, a match_source(). One flag per start in `at`.
#'
#' A raw substring test verified "5% of patients" against "25% of patients",
#' "12% year on year" against "-12%", and "enrolled 20." against "enrolled 200
#' patients": the figure changed and the check said exact. So where the span
#' starts or ends with a letter or a digit, the source must not carry on the
#' word or the number past it. What counts as carrying on:
#'
#' * a letter beside a letter, a digit beside a digit, or a letter before a
#'   digit ("covid19", not "19"); a hyphen between two letters ("non-inferior",
#'   not "inferior");
#' * before a number, a minus sign or a hyphen ("-12%", "20-30", "COVID-19"),
#'   an en dash not between words (": <en dash>12%", "20<en dash>30"), a
#'   decimal point ("0.5", ".5") and a thousands separator ("1,204"), or a
#'   no-break or thin space grouping digits ("1 200");
#' * after a number, a decimal or thousands separator and a digit ("45.2"), or
#'   a grouping space and a digit group.
#'
#' And what does not: an em dash, written as one or as "--", or an en dash
#' between words, is punctuation ("the cohort<em dash>42 patients"); a digit
#' after a word is a superscript reference that PDF text keeps inline ("as
#' previously reported12"), and one before a word an affiliation mark or a
#' number before its unit; a year followed by a full stop and a footnote
#' number opening the next sentence ("ended in 2019.4 Patients were") is not a
#' decimal; a full stop after a word ends a sentence ("the trial.5 patients").
#' Letters of the scripts written without spaces are no boundary at all
#' (.gr_unspaced).
#' @noRd
boundaries_ok <- function(at, sc, src) {
  n <- length(sc)
  cp <- src$cp
  kind <- src$kind
  ends <- sc[c(1L, n)]
  ends <- if (all(ends < 128L)) {
    ((ends >= 97L & ends <= 122L) | (ends >= 65L & ends <= 90L)) + 2L * (ends >= 48L & ends <= 57L)
  } else char_kind(ends)
  ok <- TRUE
  # Code points: 45 hyphen, 44 comma, 46 full stop, 0x2013 en dash, and the
  # no-break, figure, thin and narrow no-break spaces that group digits.
  group <- function(x) x == 0xa0L | x == 0x2007L | x == 0x2009L | x == 0x202fL

  if (ends[1L] > 0L) {
    b1 <- cp[at + 1L]
    k1 <- kind[at + 1L]
    k2 <- kind[at]
    ok <- if (ends[1L] == 1L) {
      k1 != 1L & !(b1 == 45L & k2 == 1L)
    } else {
      three <- n >= 3L && all(char_kind(sc[1:3]) == 2L) && (n == 3L || char_kind(sc[4L]) != 2L)
      # "--" is an em dash as plain text writes it, and as the ligatures
      # cleaner writes it too, so never a minus sign.
      k1 == 0L & !(b1 == 45L & cp[at] != 45L) & !(b1 == 0x2013L & k2 != 1L) &
        !(b1 == 44L & k2 == 2L) & !(b1 == 46L & k2 != 1L) & !(group(b1) & k2 == 2L & three)
    }
  }
  if (ends[2L] > 0L) {
    e <- at + n + 1L                      # the last character of the span, padded
    a1 <- cp[e + 1L]
    k1 <- kind[e + 1L]
    k2 <- kind[e + 2L]
    ok <- ok & if (ends[2L] == 1L) {
      k1 != 1L & !(a1 == 45L & k2 == 1L)
    } else {
      digit_group <- group(a1) & k2 == 2L & kind[e + 3L] == 2L & kind[e + 4L] == 2L &
        kind[e + 5L] != 2L
      carries <- (a1 == 44L | a1 == 46L) & k2 == 2L
      # A year closing a sentence, then a footnote number: "2019.4 Patients".
      # Only a year, and only before a capital: "45.5 Gy" is a decimal.
      if (any(carries & a1 == 46L) && n >= 4L &&
          grepl("(?<![0-9.,])(?:1[5-9]|20)[0-9]{2}$", intToUtf8(sc), perl = TRUE)) {
        for (i in which(carries & a1 == 46L)) {
          to <- min(src$n + 2L, e[i] + 24L)
          tail <- intToUtf8(cp[(e[i] + 1L):to])
          pat <- paste0("^\\.[0-9]{1,3}(?:[,\u2013-][0-9]{1,3})*(?:[[:space:]\u00a0\u2009\u202f]+\\p{Lu}",
                        if (to == src$n + 2L) "|$" else "", ")")
          if (grepl(pat, tail, perl = TRUE)) carries[i] <- FALSE
        }
      }
      k1 == 0L & !carries & !digit_group
    }
  }
  rep_len(ok, length(at))
}

#' Where `s` first occurs in a source as whole words and whole numbers.
#'
#' `src` is a string or a match_source(); `s` is normalised already. Every
#' occurrence at or after `from` is tried, in order, and the first that starts
#' and ends on a boundary (boundaries_ok()) is the answer.
#' @return A character position in the normalised source, or NA.
#' @noRd
found_at <- function(s, src, from = 1L) {
  if (!is.list(src)) src <- match_source(src)
  if (!nzchar(s)) return(NA_integer_)
  # The first occurrence first: one search, and for most spans it is the
  # answer. Listing every occurrence of a common word in a long source on each
  # call made the check several times slower than the substring test it
  # replaced.
  at <- regexpr(s, src$text, fixed = TRUE)
  if (at < 0L) return(NA_integer_)
  sc <- utf8ToInt(s)
  if (at >= from && boundaries_ok(at, sc, src)) return(as.integer(at))
  # Then the first not glued to an ASCII letter or digit, the cheap part of
  # the test, so "in" skips "within" in one search. Then all of those,
  # overlapping ones included, which gregexpr() with `fixed = TRUE` steps over.
  pat <- occurrence_patterns(s, sc)
  at <- regexpr(pat$first, src$text, perl = TRUE)
  if (at < 0L) return(NA_integer_)
  if (at >= from && boundaries_ok(at, sc, src)) return(as.integer(at))
  at <- gregexpr(pat$every, src$text, perl = TRUE)[[1]]
  at <- as.integer(at[at >= from])
  if (!length(at)) return(NA_integer_)
  hit <- at[boundaries_ok(at, sc, src)]
  if (length(hit)) hit[1] else NA_integer_
}

#' The regular expressions found_at() and found_every() look for `s`, whose
#' code points are `sc`, with: `first` the first occurrence not glued to an
#' ASCII letter or digit, `every` each of them, overlapping ones included.
#' @noRd
occurrence_patterns <- function(s, sc) {
  ends <- char_kind(sc[c(1L, length(sc))])
  quoted <- paste0("\\Q", gsub("\\E", "\\E\\\\E\\Q", s, fixed = TRUE), "\\E")
  before <- c("", "(?<![a-z])", "(?<![0-9a-z])")[ends[1L] + 1L]
  after <- c("", "(?![a-z])", "(?![0-9a-z])")[ends[2L] + 1L]
  list(first = paste0(before, quoted, after),
       every = paste0(before, "(?=", quoted, after, ")"))
}

#' Every place `s` occurs in `src`, a match_source(), as whole words and whole
#' numbers, in order. found_at() is the fast way to the first of them.
#' @noRd
found_every <- function(s, src) {
  if (!nzchar(s)) return(integer(0))
  sc <- utf8ToInt(s)
  at <- gregexpr(occurrence_patterns(s, sc)$every, src$text, perl = TRUE)[[1]]
  if (at[1] < 0L) return(integer(0))
  at <- as.integer(at)
  at[boundaries_ok(at, sc, src)]
}

#' Does `s` occur in `src` as whole words and whole numbers? See found_at().
#' @noRd
found_whole <- function(s, src) !is.na(found_at(s, src))

#' Do the passages of one elided quotation all occur, in order, and joined
#' only where an elision may join them?
#'
#' "A ... B" says that B follows A in the source, so each passage is looked for
#' after the end of the one before it. Checked separately, "Costs fell ...
#' Revenue grew" verified against "Revenue grew by 3%. Costs fell 12%."
#'
#' It also says that what the elision leaves out does not change what A and B
#' say together, and each piece being in the source does not show that: "the
#' drug did ... reduce mortality" verified against "the drug did not reduce
#' mortality", and "Revenue ... rose 30%" against "Revenue fell 12%. Costs
#' rose 30%.". So what lies between them has to pass elision_gap_ok().
#'
#' Each piece is taken where it first occurs after the one before, which is
#' where it is for nearly every quotation and costs one search a piece. Only
#' when a gap there fails are the other occurrences tried (elided_chain()): a
#' passage may occur twice, once as it is quoted.
#' @noRd
found_in_order <- function(pieces, src) {
  if (!length(pieces)) return(FALSE)
  if (!is.list(src)) src <- match_source(src)
  n <- length(pieces)
  at <- integer(n)
  from <- 1L
  for (i in seq_len(n)) {
    at[i] <- found_at(pieces[i], src, from)
    # The first occurrence of each after the one before is as early as any
    # can be, so when one cannot be found no order of them can.
    if (is.na(at[i])) return(FALSE)
    from <- at[i] + nchar(pieces[i])
  }
  if (n < 2L) return(TRUE)
  ends <- at + nchar(pieces) - 1L
  gaps <- vapply(seq_len(n - 1L), function(i) elision_gap_ok(src, ends[i], at[i + 1L]),
                 logical(1))
  all(gaps) || elided_chain(pieces, src)
}

#' found_in_order() over every occurrence of every piece: can the pieces be
#' placed in order, each gap passing elision_gap_ok()? Remembers, for each
#' piece and each place the one before it ends, whether the rest can follow,
#' so no placement is tried twice. A source that repeats the pieces hundreds
#' of times could still ask for millions of gaps to be read, so the search
#' reads at most `budget` of them, and a quotation it could not place within
#' that is not verified: a check that gives up says so, as any other does.
#' @noRd
elided_chain <- function(pieces, src, budget = 2000L) {
  n <- length(pieces)
  len <- nchar(pieces)
  occ <- lapply(pieces, found_every, src = src)
  seen <- new.env(parent = emptyenv())
  left <- budget
  rest <- function(i, prev) {
    key <- paste(i, prev)
    if (!is.null(seen[[key]])) return(seen[[key]])
    ok <- FALSE
    for (a in occ[[i]][occ[[i]] > prev]) {
      if (i > 1L) {
        if (left <= 0L) break
        left <<- left - 1L
        if (!elision_gap_ok(src, prev, a)) next
      }
      if (i == n || rest(i + 1L, a + len[i] - 1L)) {
        ok <- TRUE
        break
      }
    }
    assign(key, ok, envir = seen)
    ok
  }
  rest(1L, 0L)
}

#' Does what an elision leaves out keep what the passages either side of it
#' say?
#'
#' `prev` is where the passage before the elision ends in `src$text` (a
#' match_source()), and `at` where the one after it starts. The rule
#' passage_gaps_ok() holds an extracted value's quotation to, with the same
#' list of negations: within one sentence, the words left out must not include
#' a negation (.gr_negation_words), or "the drug did ... reduce mortality" is
#' quoted from "the drug did not reduce mortality"; across a sentence end, the
#' passage after the elision must start a sentence, or "Revenue ... rose 30%"
#' is quoted from "Revenue fell 12%. Costs rose 30%.". A sentence ends at a
#' full stop, question or exclamation mark, colon or semicolon followed by a
#' space or by the next passage, as passage_gaps_ok() reads one, or at the
#' full-width marks of Chinese and Japanese, which no space follows.
#' @noRd
elision_gap_ok <- function(src, prev, at) {
  gap <- substr(src$text, prev + 1L, at - 1L)
  stops <- "[.!?;:\u3002\uff01\uff1f\uff1b\uff1a]"
  if (!grepl("[.!?;:][\"')\\]]*(?:\\s|$)|[\u3002\uff01\uff1f\uff1b\uff1a]", gap, perl = TRUE)) {
    w <- regmatches(gap, gregexpr("[\\p{L}']+", gap, perl = TRUE))[[1]]
    return(!any(w %in% .gr_negation_words | grepl("n't$", w)))
  }
  # The passage after the gap starts a sentence when the gap ends with one's
  # end, past any space, quote mark or opening bracket. The gap alone decides:
  # it holds a sentence end, so what those leave of it is never empty.
  grepl(paste0(stops, "[\"')\\]]*[\\s\"'(\\[]*$"), gap, perl = TRUE)
}

#' The passages a model's quotation is made of.
#'
#' The extraction prompt asks for "the passages" verbatim, and models give
#' several: in paragraphs, as a bulleted or numbered list, in separate quote
#' marks, or joined with an elision ("...", "[...]"). Checked as one string,
#' two verbatim passages that are not adjacent in the source failed, and so did
#' any bullet or bold marker, marking an entirely faithful extraction partial
#' and dropping correct values under `require_quote`.
#'
#' But a line break is not always a passage break. Chunk text keeps the hard
#' line wraps of a .txt or PDF document, and a model copying it verbatim keeps
#' them too, so the next line carries on the same sentence. Checked line by
#' line, "the median age was\n54. Most patients" verified against "was\n45.
#' Most" (the "45." read as a list number) and "the drug did\nreduce" against
#' "did not\nreduce". So a line starts a new passage only after a blank line,
#' at a list marker, where the line before ends a sentence, or where it opens
#' with a capital or a quote mark; any other line is the one before it,
#' wrapped. A number is a list marker only as a list's first item, 1, or the
#' next after the item before.
#'
#' @return A list of passages, each a character vector of the pieces an elision
#'   splits it into, normalised and trimmed, to be found in that order (see
#'   found_in_order()). `plain` is the same with markdown bold taken out, or
#'   NULL where there is none. `whole = TRUE` takes the span as one piece, for
#'   text that is the chunk's own rather than what a model wrote.
#' @noRd
quote_passages <- function(span, whole = FALSE) {
  x <- to_utf8(as_chr1(span, ""))
  tidy <- function(p) {
    p <- vapply(p, function(s) trim_quote_edges(normalise_for_match(s)), character(1),
                USE.NAMES = FALSE)
    p[nzchar(p) & grepl("[\\p{L}\\p{N}]", p, perl = TRUE)]
  }
  if (whole) {
    p <- tidy(x)
    return(list(raw = if (length(p)) list(p) else list(), plain = NULL))
  }
  if (!nzchar(x)) return(list(raw = list(), plain = NULL))
  items <- unlist(lapply(strsplit(x, "\n[[:space:]]*\n", perl = TRUE)[[1]], quote_items),
                  use.names = FALSE)
  if (!length(items)) return(list(raw = list(), plain = NULL))
  # One quoted passage closing and the next opening.
  items <- unlist(strsplit(items, "[\"\u201d][[:space:]]*[,;]?[[:space:]]+[\"\u201c]", perl = TRUE),
                  use.names = FALSE)
  elision <- paste0("\\[[[:space:]]*(?:\\.{2,}|\u2026)[[:space:]]*\\]|",
                    "\\([[:space:]]*(?:\\.{2,}|\u2026)[[:space:]]*\\)|",
                    "\\.{3,}|\u2026|(?:\\.[[:space:]]){2,}\\.")
  pieces <- function(it) lapply(strsplit(it, elision, perl = TRUE), tidy)
  raw <- pieces(items)
  keep <- lengths(raw) > 0L
  plain <- NULL
  if (any(grepl("**", items, fixed = TRUE))) {
    plain <- pieces(strip_bold(items, edges = TRUE))
    plain[!lengths(plain)] <- raw[!lengths(plain)]
    plain <- plain[keep]
  }
  list(raw = raw[keep], plain = plain)
}

#' The passages of one block of a quotation (no blank line in it): list items,
#' marker taken off, and lines joined where a line wraps. See quote_passages().
#' @noRd
quote_items <- function(block) {
  lines <- strsplit(block, "\n", fixed = TRUE)[[1]]
  lines <- lines[grepl("[^[:space:]]", lines)]
  bullet <- "^[[:space:]]*(?:[\u2022\u2023\u2043\u2219\u25aa\u25cf\u25e6][[:space:]]*|[-*+][[:space:]]+)"
  enum <- "^[[:space:]]*\\([0-9a-zA-Z]\\)[[:space:]]+"
  numbered <- "^[[:space:]]*([0-9]{1,2})[.)][[:space:]]+"
  ends <- "[.!?;:\u2026\u3002\uff01\uff1f\uff1b\uff1a][\"'\u201d\u2019)\\]]*[[:space:]]*$"
  opens <- "^[[:space:]]*(?:[\"\u201c]|['\u2018(\\[]?\\p{Lu})"
  items <- character(0)
  ended <- TRUE          # the text so far ends a passage: true at the start
  in_list <- FALSE
  number <- NA_integer_  # the number of the last numbered item
  for (ln in lines) {
    k <- suppressWarnings(as.integer(sub(paste0(numbered, ".*$"), "\\1", ln, perl = TRUE)))
    if (!grepl(numbered, ln, perl = TRUE)) k <- NA_integer_
    marker <- if (grepl(bullet, ln, perl = TRUE)) bullet
              else if (grepl(enum, ln, perl = TRUE) && (ended || in_list)) enum
              else if (!is.na(k) && ((ended && k == 1L) || identical(k, number + 1L))) numbered
    if (!is.null(marker)) {
      items <- c(items, sub(marker, "", ln, perl = TRUE))
      if (identical(marker, numbered)) number <- k
      in_list <- TRUE
    } else if (!length(items) || ended || grepl(opens, ln, perl = TRUE)) {
      items <- c(items, ln)
      in_list <- FALSE
    } else {
      items[length(items)] <- paste(items[length(items)], ln)
    }
    ended <- grepl(ends, ln, perl = TRUE)
  }
  items
}

#' The units a passage's run is counted in: its words, and in a script written
#' without spaces its characters. There a clause is one "word", so a quotation
#' with one character changed scored 0, as a sentence sharing nothing does.
#' `sep` is what comes before each unit in the passage: a space, or nothing.
#' @noRd
quote_units <- function(s) {
  m <- gregexpr(sprintf("[%s][\\p{M}\\p{Lm}]*|[^ %s]+", .gr_unspaced, .gr_unspaced), s,
                perl = TRUE)[[1]]
  if (m[1] < 0L) return(list(words = character(0), sep = character(0)))
  len <- attr(m, "match.length")
  ends <- m + len - 1L
  list(words = substring(s, m, ends),
       sep = substring(s, c(1L, ends[-length(ends)] + 1L), m - 1L))
}

#' The longest run of consecutive words from `span` that appears in `source`.
#'
#' Only reached when the span is not an exact quotation, so the runs found here
#' are short and the loop is cheap. A run measure rather than a word-overlap one
#' because overlap cannot tell a quotation from a paraphrase built out of the
#' same vocabulary, and that is the distinction the whole check exists to make.
#' A run counts only as whole words (found_at()), so "5% of patients"
#' against "25% of patients" scores the run it really shares and not 1.
#' `sep` joins the words (see quote_units()); `source_norm` is a normalised
#' string or a match_source().
#' @noRd
longest_quoted_run <- function(span_words, source_norm, sep = " ") {
  n <- length(span_words)
  if (!n) return(0L)
  src <- if (is.list(source_norm)) source_norm else match_source(source_norm)
  if (!nzchar(src$text)) return(0L)
  if (n > 300L) { span_words <- span_words[seq_len(300L)]; n <- 300L }
  sep <- rep_len(sep, n)
  # Each distinct word is looked up once: a paraphrase repeats its short ones.
  words <- unique(span_words)
  which_word <- match(span_words, words)
  alone <- rep(NA, length(words))
  best <- 0L
  for (i in seq_len(n)) {
    if (n - i + 1L <= best) break                 # cannot beat `best` from here
    w <- which_word[i]
    if (is.na(alone[w])) alone[w] <- !is.na(found_at(span_words[i], src))
    if (!alone[w]) next
    best <- max(best, 1L)
    run <- span_words[i]
    j <- i + 1L
    while (j <= n) {
      run <- paste0(run, sep[j], span_words[j])
      if (is.na(found_at(run, src))) break
      best <- max(best, j - i + 1L)
      j <- j + 1L
    }
  }
  best
}

#' How much of a passage that was not found is still a quotation: the
#' fraction of its units its longest run carries, for the piece that matches
#' worst. Pieces each found, but out of the order an elision claims or across
#' a gap it may not leave out (elision_gap_ok()), are scored as one run, which
#' they are not.
#' @noRd
passage_score <- function(pieces, src) {
  frac <- function(p) {
    u <- quote_units(p)
    if (!length(u$words)) return(1)
    longest_quoted_run(u$words, src, u$sep) / length(u$words)
  }
  runs <- vapply(pieces, frac, numeric(1), USE.NAMES = FALSE)
  if (length(pieces) > 1L && all(runs >= 1)) return(frac(paste(pieces, collapse = " ")))
  min(runs)
}

#' Does one span appear in one source?
#'
#' The span is split into the passages it is made of (quote_passages()), and it
#' is verified when every passage appears in the source as whole words and
#' whole numbers (found_at()), the pieces of an elided passage in order.
#' Markdown bold is emphasis, in the quotation or the source: a passage is
#' looked for as written and, failing that, with bold taken out of both sides,
#' never out of one only. `whole = TRUE` compares the span whole, for
#' evidence that is the chunk's own text: nothing in it is a model's list or
#' bold, and taking its markers out made the package's own verbatim text fail
#' to match itself.
#'
#' @return `list(verified, match)`. `match` is 1 for an exact quotation and
#'   otherwise, for the weakest passage, the fraction of its words carried by
#'   its longest consecutive run in the source -- so 0.9 is a quotation with a
#'   word changed, and 0.1 is a sentence that shares some vocabulary and
#'   nothing else.
#' @noRd
span_match <- function(span, source, whole = FALSE) {
  q <- quote_passages(span, whole = whole)
  if (!length(q$raw)) return(list(verified = NA, match = NA_real_))
  txt <- as_chr1(source, "")
  src <- match_source(txt)
  if (!nzchar(src$text)) return(list(verified = NA, match = NA_real_))
  plain <- NULL
  if (!whole && (!is.null(q$plain) || grepl("**", txt, fixed = TRUE))) {
    plain <- match_source(strip_bold(txt))
  }
  alt <- q$plain %||% q$raw
  found <- vapply(seq_along(q$raw), function(i)
    found_in_order(q$raw[[i]], src) || (!is.null(plain) && found_in_order(alt[[i]], plain)),
    logical(1))
  if (all(found)) return(list(verified = TRUE, match = 1))
  score <- vapply(which(!found), function(i) {
    s <- passage_score(q$raw[[i]], src)
    if (!is.null(plain)) s <- max(s, passage_score(alt[[i]], plain))
    s
  }, numeric(1))
  list(verified = FALSE, match = round(min(score), 3))
}

#' @noRd
verify_spans <- function(spans, sources, whole = FALSE) {
  n <- length(spans)
  if (!n) return(data.frame(verified = logical(0), match = numeric(0)))
  sources <- rep(sources, length.out = n)
  whole <- rep(whole, length.out = n)
  out <- lapply(seq_len(n), function(i) span_match(spans[[i]], sources[[i]], whole = whole[[i]]))
  data.frame(verified = vapply(out, function(x) as.logical(x$verified), logical(1)),
             match = vapply(out, function(x) as.numeric(x$match), numeric(1)),
             stringsAsFactors = FALSE)
}


#' The plural a citation marker may be written with.
#'
#' "studies" is irregular, so it cannot be derived from "study" by rule.
#' @noRd
.gr_cite_plural <- c(study = "stud(?:y|ies)", chunk = "chunks?")

#' The grammar of a citation marker.
#'
#' ONE definition, used by the checker and by the renderer. They had two, and
#' they disagreed: the checker matched only `[study 3]` while the renderer also
#' understood `[studies 1 and 2]` -- the combined form the synthesis prompt
#' explicitly asks for. So a section citing three studies that way rendered as
#' "(Garcia, 2022; Lee & Petrov, 2021; Smith & Okafor, 2019)" while the check
#' reported it cited nothing: no reference list for the studies it named, and,
#' worse, `[studies 1 and 99]` over three studies passed as clean. A fabricated
#' citation slipping through the fabrication check is the exact failure this
#' pipeline exists to prevent, and the two regexes drifting apart is how it got
#' there. They cannot drift now.
#'
#' Models write more than the prompt shows: numbers joined by commas,
#' semicolons, "and" or "&" (the Oxford comma included), the word repeated
#' ("[study 1; study 7]"), any kind of space. Each of those used to match
#' nothing, so a fabricated id written that way was never looked at. This is
#' the listed form, which render_citations() turns into references by reading
#' every number in it as an id -- right for every list it accepts. The check
#' reads two forms more; see cite_grammar().
#' @noRd
cite_pattern <- function(word) {
  cite_grammar(word)$listed
}

#' The citation grammar: the listed form, and the full form the check reads.
#'
#' The full form adds a range ("1-7", with a hyphen or any dash), every id of
#' which is cited, and a trailing locator such as the page and section
#' render_chunks() prints ("[chunk 3 p.2, <section sign> Methods]"), none of
#' whose numbers is an id. Read as a list of numbers, "[studies 1-7]" would
#' render as studies 1 and 7, so the renderer leaves those two forms as they
#' were written, and the check reads them properly: whatever is rendered has
#' been checked, and nothing a model cites escapes the check.
#' @noRd
cite_grammar <- function(word) {
  w <- .gr_cite_plural[[word]] %||% sprintf("%ss?", word)
  sp <- "[[:space:]\u00a0\u2007\u2009\u202f]"
  range <- sprintf("[0-9]+(?:%s*[-\u2010\u2011\u2012\u2013\u2014\u2015\u2212]%s*[0-9]+)?", sp, sp)
  sep <- sprintf("%s*(?:[,;&]|and)(?:%s*(?:and|&))?%s*", sp, sp, sp)
  # The word may be repeated before any id after the first: "[study 1, study 7]".
  ids_of <- function(num) sprintf("%s%s+%s(?:%s(?:%s%s+)?%s)*", w, sp, num, sep, w, sp, num)
  # A page or section after the ids. It may name anything, "Study population"
  # included, but not another id: "[chunk 1 p.7, chunk 9]" must not hide 9.
  locator <- sprintf(paste0("(?:%s*[,;:]?%s*(?:pp?\\.|pages?\\b|paras?\\.|paragraphs?\\b|",
                            "sec(?:tion)?s?\\b\\.?|lines?\\b|\u00a7)(?:(?!%s%s+[0-9])[^\\]])*)?"),
                     sp, sp, w, sp)
  list(word = w, space = sp, number = range,
       listed = sprintf("\\[%s\\]", ids_of("[0-9]+")),
       ids = sprintf("\\[%s", ids_of(range)),
       marker = sprintf("\\[%s%s\\]", ids_of(range), locator))
}

#' The ids one matched marker names, ranges expanded.
#'
#' Read from the list alone, never the locator, so "[chunk 3 p.12]" cites 3 and
#' not 12. A range is every id in it: "[studies 1-7]" over two studies cites
#' five that do not exist. A range too long to be a citation is kept as its two
#' ends, which is enough to report it.
#' @noRd
cite_marker_ids <- function(marker, word) {
  g <- cite_grammar(word)
  head <- regmatches(marker, regexpr(g$ids, marker, perl = TRUE, ignore.case = TRUE))
  if (!length(head)) return(integer(0))
  toks <- regmatches(head, gregexpr(g$number, head, perl = TRUE))[[1]]
  unlist(lapply(toks, function(t) {
    ends <- as.integer(regmatches(t, gregexpr("[0-9]+", t))[[1]])
    if (length(ends) < 2L) return(ends)
    lo <- min(ends); hi <- max(ends)
    if (hi - lo > 1000L) c(lo, hi) else seq.int(lo, hi)
  }), use.names = FALSE) %||% integer(0)
}

#' Numbered ids a piece of generated text claims to cite.
#'
#' One grammar, two callers: `[chunk 3]` in a cited answer and `[study 7]` in a
#' synthesised section. They are the same check for the same reason -- a citation
#' pointing at something that was never supplied is a fabrication, and the most
#' convincing kind, because it looks like the thing that would let you check.
#' Read with the full grammar, ranges and locators included.
#' @noRd
cited_ids <- function(text, word) {
  txt <- as_chr1(text)
  m <- gregexpr(cite_grammar(word)$marker, txt, perl = TRUE, ignore.case = TRUE)
  hits <- regmatches(txt, m)[[1]]
  if (!length(hits)) return(integer(0))
  ids <- unlist(lapply(hits, cite_marker_ids, word = word), use.names = FALSE)
  if (!length(ids)) return(integer(0))
  unique(as.integer(ids))
}

#' Brackets that open like a citation and do not parse as one.
#'
#' `[chunk nine]`, `[studies 1 to 7]`: whatever the model meant, the ids in it
#' cannot be checked, and a check that silently skips what it cannot read is
#' how a fabricated citation passed as clean. Callers report these and mark the
#' result partial rather than treat them as citing nothing.
#' @noRd
unparsed_citations <- function(text, word) {
  txt <- as_chr1(text, "")
  g <- cite_grammar(word)
  loose <- regmatches(txt, gregexpr(sprintf("\\[%s%s[^\\]\\[]*\\]", g$word, g$space), txt,
                                    perl = TRUE, ignore.case = TRUE))[[1]]
  if (!length(loose)) return(character(0))
  ok <- grepl(sprintf("^%s$", g$marker), loose, perl = TRUE, ignore.case = TRUE)
  unique(loose[!ok])
}

#' @noRd
cited_chunks <- function(text) cited_ids(text, "chunk")

#' What kind of thing a reader puts in `evidence$text`.
#' @noRd
.gr_evidence_kind <- c(
  stuff = "verbatim", retrieve = "verbatim", rerank = "verbatim",
  iterative = "verbatim",
  skim = "extracted", extract = "extracted", screen = "extracted",
  map_reduce = "answer", refine = "answer", hierarchical = "answer",
  preview = "mixed", ensemble = "mixed"
)

#' Check that quoted evidence really is in the document
#'
#' `ans$evidence` says what an answer rests on. For most readers those spans are
#' verbatim chunk text and are true by construction. For `skim` they are what
#' the model chose to write when asked to extract the relevant passages. They
#' are *presented* as quotations, and this is what checks that they are.
#'
#' A fabricated citation is more convincing than a fabricated answer, because it
#' looks like the thing that would let you check. Verification is a string
#' operation on text you already have, so it costs nothing and there is no
#' reason not to do it.
#'
#' @param answer A [gr_answer].
#' @param chunks The [gr_chunks] the answer was read from. Needed for readers
#'   whose evidence is verbatim, where the comparison is against the chunk the
#'   span claims to come from: its `source_text` where the segmenter recorded
#'   one (the document text behind a chunk whose `text` carries model-written
#'   context or propositions), and its `text` otherwise. `skim` answers
#'   already carry their sources, so they can be checked without it; an
#'   `ensemble` needs it for the rows
#'   its verbatim members contributed, even though its `skim` rows do not.
#'
#'   Pass the chunks the answer was actually read from. Chunk ids are positional,
#'   so a *different* chunk set of the same size will match on id and compare
#'   each span against unrelated text, reporting `verified = FALSE` for evidence
#'   that is perfectly sound. An id the chunk set does not contain reports `NA`,
#'   because there was nothing to compare against.
#' @return A data frame with one row per evidence span: `chunk_id`, `kind`
#'   (`"verbatim"`, `"extracted"` or `"answer"`, **per row**; an `ensemble`
#'   mixes them in one table, and the same value appears as the `kind` column on
#'   `ans$evidence` itself), `verified`,
#'   `match` and `span` (the first 60 characters). `verified` is `NA` where the
#'   question does not apply: a `map_reduce` evidence row is a per-chunk
#'   *answer*, not a quotation, and asking whether it appears in the chunk is a
#'   category error.
#'
#'   For an `extract` answer, a quote has to carry the value it is cited for as
#'   well as appear in the chunk, and only the reader can check the first: it
#'   knows the value. A row the reader marked `verified = FALSE` for that
#'   reason stays `FALSE` here, with a `match` of 1 when the sentence itself is
#'   in the document. It is there, and it does not say this.
#'
#' @section What the numbers mean:
#' `match` is 1 for an exact quotation once whitespace, quote marks, dashes and
#' case are folded away. These are the differences a faithful quotation introduces.
#' It has to match whole words and whole numbers: "5%" is not found in "25%",
#' nor "12%" in "-12%", nor "20" in "200", nor "200" in "1 200" written with a
#' thin space. Chinese, Japanese and Thai put no spaces between words, so a
#' clause quoted from them may start and end anywhere; numbers in them are
#' still whole. Markdown bold is not text, in the quotation or the document.
#' A span made of several passages (in paragraphs, as a list, in separate
#' quote marks, or joined by "..." or "\[...\]") is checked passage by passage
#' and is verified when every passage is found, the parts either side of an
#' elision in that order. What an elision leaves out may not be a negation
#' ("the drug did ... reduce mortality" is not in "the drug did not reduce
#' mortality"), and where it leaves out the end of a sentence the part after
#' it has to start one ("Revenue ... rose 30%" is not in "Revenue fell 12%.
#' Costs rose 30%."). A line break inside a sentence is a line wrap, not a
#' new passage: the lines are checked as one.
#' Below 1 it is the fraction of the span's words carried by its longest
#' consecutive **run** in the source, for the passage that matches worst; in
#' a script written without spaces, the fraction of its characters.
#'
#' Read that number with its shape in mind. Because it measures a run, *where*
#' the change falls matters as much as how much changed: altering the last word
#' of a ten-word span leaves a run of nine and scores 0.9, while altering a word
#' in the middle splits the span and scores about 0.5. So a mid-sentence change
#' (a swapped figure, the case this exists to catch) lands near 0.5, not
#' near 0.9. Below roughly 0.3 there is no quotation left at all, only shared
#' vocabulary. A run measure is still the right one: word overlap cannot tell a
#' quotation from a paraphrase assembled out of the same words.
#'
#' @section Citations:
#' With `cite = TRUE` a reader asks the model to mark its sources as
#' `[chunk 3]`. Every answer is checked for citations pointing at chunks that
#' were never sent, whatever this function is called with; the result is
#' `ans$notes$cited_unknown`, and an answer carrying one is `partial`. Lists
#' and ranges are read (`[chunks 1, 2, and 9]`, `[chunks 1-9]`), and a bracket
#' that opens like a citation but cannot be read, such as `[chunk nine]`, is
#' listed in `ans$notes$cited_unparsed` and makes the answer `partial` too.
#'
#' @seealso [gr_answer], [gr_read()], [is_not_found()]
#' @export
#' @examples
#' # A model that quotes faithfully.
#' honest <- gr_mock_client(function(messages, params) {
#'   txt <- messages[[length(messages)]]$content
#'   if (grepl("You extract evidence", messages[[1]]$content, fixed = TRUE)) {
#'     return("Revenue rose to 45.2 million dollars.")
#'   }
#'   "Revenue was 45.2 million dollars."
#' })
#'
#' doc <- "Revenue rose to 45.2 million dollars.\n\nHeadcount grew to 1,204."
#' ch <- gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 40))
#' ans <- gr_read(ch, "What was revenue?", honest, "skim")
#' gr_verify_evidence(ans)
#'
#' # A model that invents one. The span is fluent, plausible, and not in the
#' # document. That is exactly the case a reader cannot catch by eye.
#' liar <- gr_mock_client(function(messages, params) {
#'   if (grepl("You extract evidence", messages[[1]]$content, fixed = TRUE)) {
#'     return("Revenue rose to 88.9 billion dollars on record demand.")
#'   }
#'   "Revenue was 88.9 billion dollars."
#' })
#' bad <- gr_read(ch, "What was revenue?", liar, "skim")
#' gr_verify_evidence(bad)
#' bad$partial
gr_verify_evidence <- function(answer, chunks = NULL) {
  if (!inherits(answer, "gr_answer")) gr_abort("`answer` must be a gr_answer.")
  ev <- answer$evidence
  empty <- data.frame(chunk_id = integer(0), kind = character(0), verified = logical(0),
                      match = numeric(0), span = character(0), stringsAsFactors = FALSE)
  if (is.null(ev) || !nrow(ev)) return(empty)

  # Per row, falling back to the reader only for an answer built before evidence
  # tables carried a kind. An `ensemble` has no single kind -- its members
  # contribute verbatim spans and per-chunk answers to one table -- and treating
  # the whole table as one kind is what made a correct map_reduce member report
  # its answers as fabricated quotations.
  kind <- if (!is.null(ev$kind)) as.character(ev$kind) else {
    k <- unname(.gr_evidence_kind[as_chr1(answer$reader, "")])
    rep(if (is.na(k)) "verbatim" else k, nrow(ev))
  }

  # Per row, like `kind`. An ensemble's table has `source_text` for its `skim`
  # rows and NA for everyone else, so taking the column as the whole answer left
  # every verbatim row unverifiable even when `chunks` had been supplied.
  sources <- if (!is.null(ev$source_text)) as.character(ev$source_text) else rep(NA_character_, nrow(ev))
  if (inherits(chunks, "gr_chunks")) {
    gap <- is.na(sources)
    # The document text a chunk came from, not its `text` where a segmenter
    # wrote into that: a contextual header or rewritten propositions are the
    # segmenting model's words, and a quotation of them is not in the document.
    sources[gap] <- reader_source_text(chunks$chunks)[
      match(ev$chunk_id[gap], chunks$chunks$chunk_id)]
  }
  if (all(is.na(sources))) sources <- NULL

  res <- data.frame(verified = rep(NA, nrow(ev)), match = rep(NA_real_, nrow(ev)))
  # "answer" rows are that chunk's answer, not a quotation from it. Asking
  # whether one appears in the chunk is a category error, and reporting FALSE
  # would mark every correct run unverified.
  # No `!is.na(sources)` term: span_match() already returns NA for an absent
  # source, and a second guard that no test can distinguish from the first is
  # a branch nothing exercises. The guarantee is tested where it lives.
  checkable <- kind != "answer" & !is.na(kind)
  if (!is.null(sources) && any(checkable)) {
    # A verbatim row is the chunk's own text, compared whole: read as a
    # model's list of passages with bold markers, a chunk from a .md file
    # ("**Revenue** rose") failed to match itself.
    got <- verify_spans(ev$text[checkable], sources[checkable],
                        whole = kind[checkable] == "verbatim")
    res$verified[checkable] <- got$verified
    res$match[checkable] <- got$match
  }
  # A quote cited for an extracted value has to carry that value as well as
  # occur in the chunk. The extract reader checks both (quote_backs_value())
  # and a span check can only check the second, so recomputed from the span
  # alone "We enrolled 120 people." cited for n = 5000 came back TRUE here and
  # FALSE on the answer. The reader's FALSE stands, beside the span's match.
  stored <- ev[["verified"]]
  field <- ev[["field"]]
  if (!is.null(stored) && !is.null(field)) {
    refused <- kind == "extracted" & !is.na(field) & !is.na(stored) & !as.logical(stored)
    res$verified[refused] <- FALSE
  }

  data.frame(chunk_id = ev$chunk_id, kind = kind,
             verified = res$verified, match = res$match,
             span = paste0(substr(ev$text, 1, 60),
                           ifelse(nchar(ev$text) > 60, "...", "")),
             stringsAsFactors = FALSE)
}

#' Give each quoted span the page it is actually on.
#'
#' A chunk's page is the page of the text it was packed from, and when it was
#' packed from more than one page there is no single right answer -- `meta_over()`
#' reports `NA` there rather than naming one of them. But an evidence row is not
#' a chunk: it is a specific sentence, and a specific sentence *is* on one page.
#'
#' So where the span can be found in the document's blocks, the page comes from
#' the block that contains it, not from the chunk that happened to carry it. That
#' turns a citation from "somewhere in this chunk" into "page 7", which is the
#' difference between a reference a reader can check and one they cannot.
#'
#' Matching uses `normalise_for_match()`, the same rule `span_match()` verifies
#' with, so a span that verified against its chunk cannot fail to locate against
#' the document for a difference in whitespace or quote characters.
#'
#' A span found on several pages -- a repeated heading, a running footer -- is
#' left alone: the first hit would be a guess dressed as a fact. So is a span
#' that cannot be found at all, which is exactly the unverified case, where the
#' chunk-level fallback is already as much as is known.
#' @noRd
resolve_evidence_pages <- function(evidence, blocks) {
  if (!is.data.frame(evidence) || !nrow(evidence)) return(evidence)
  if (!is.data.frame(blocks) || !nrow(blocks) || is.null(blocks$page)) return(evidence)
  if (all(is.na(blocks$page))) return(evidence)

  src <- normalise_for_match(blocks$text)
  page <- blocks$page
  section <- blocks$section
  for (i in seq_len(nrow(evidence))) {
    s <- trim_quote_edges(normalise_for_match(evidence$text[[i]]))
    if (!nzchar(s)) next
    hit <- which(vapply(src, function(b) nzchar(b) && grepl(s, b, fixed = TRUE),
                        logical(1), USE.NAMES = FALSE))
    if (!length(hit)) next
    pg <- unique(page[hit][!is.na(page[hit])])
    if (length(pg) == 1L) evidence$page[[i]] <- pg
    if (!is.null(section)) {
      sc <- unique(section[hit][!is.na(section[hit])])
      if (length(sc) == 1L && is.na(evidence$section[[i]])) evidence$section[[i]] <- sc
    }
  }
  evidence
}
