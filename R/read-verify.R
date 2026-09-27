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
  fold_case(x)
}

#' lower_text() of each string, worked out one distinct code point at a time.
#' The result is the same; the cost is not. `chartr()` and `tolower()` read
#' every character of a long text, and where the locale is not UTF-8 a
#' Chinese section of 80,000 characters, none of which has a case, took
#' seconds.
#' @noRd
fold_case <- function(x) {
  for (i in seq_along(x)) {
    s <- x[i]
    if (is.na(s) || !nzchar(s)) next
    cp <- utf8ToInt(s)
    if (anyNA(cp)) {
      x[i] <- lower_text(s)
      next
    }
    low <- lower_cp(cp)
    if (!identical(low, cp)) x[i] <- intToUtf8(low)
  }
  x
}

#' lower_text() of code points `cp`: each distinct code point that has a case
#' (a capital, a Roman numeral or a circled capital) is lowered once.
#' @noRd
lower_cp <- function(cp) {
  up <- cp >= 65L & (cp <= 90L | cp > 127L)
  if (!any(up)) return(cp)
  u <- unique(cp[up])
  ch <- intToUtf8(u, multiple = TRUE)
  cased <- grepl("[\\p{Lu}\\p{Lt}\\p{Nl}\u24b6-\u24cf]", ch, perl = TRUE)
  if (!any(cased)) return(cp)
  low <- vapply(lower_text(ch[cased]), function(l) utf8ToInt(l)[1L], integer(1), USE.NAMES = FALSE)
  hit <- match(cp, u[cased])
  cp[!is.na(hit)] <- low[hit[!is.na(hit)]]
  cp
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
#' element `i + 2`. `memo` holds what is worked out from the source once and
#' read many times (sentence_marks()).
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
  low <- cp
  if (length(hi)) {
    u <- unique(cp[hi])
    kind[hi] <- char_kind(u)[match(cp[hi], u)]
    # The dashes and spaces kept for the boundary test, folded now, code
    # point by code point.
    low[low == 0x2013L | low == 0x2014L | low == 0x2015L] <- 45L
    low[low != 32L & spacing(low)] <- 32L
  }
  low <- lower_cp(low)
  # `lc` is `text` as code points, for reading a stretch of it far into a
  # long source (gap_text()).
  memo <- new.env(parent = emptyenv())
  memo$lc <- low
  list(text = if (identical(low, cp)) marks else intToUtf8(low), cp = c(0L, 0L, cp, integer(5L)),
       kind = c(0L, 0L, kind, integer(5L)), n = length(cp), memo = memo)
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
#' content, and a quotation that changed one is exactly what this is for. Nor
#' a lone full stop before a digit: ".45" and ".001" are how APA style writes a
#' correlation and a p value, and trimmed to "45" and "001" they were numbers
#' the source does not state (boundaries_ok() reads the kept stop as the
#' number's start). An ellipsis before a digit still goes.
#' @noRd
trim_quote_edges <- function(x) {
  edge <- "[\"'\u2026.,;:[:space:]]+"
  strip <- function(s) {
    # A full stop before a digit, not after another stop, opens a decimal.
    decimal <- grepl("^[\"'\u2026.,;:[:space:]]*(?<![.\u2026])\\.[0-9]", s, perl = TRUE)
    s <- gsub(paste0("^", edge, "|", edge, "$"), "", s, perl = TRUE)
    s[decimal] <- paste0(".", s[decimal])
    s
  }
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
#' punctuation, a space, a letter of an unspaced script (.gr_unspaced), a
#' circled or bracketed number (a list mark, never part of a number: a quote
#' of the item after "<circled 1>" starts at its first digit), or no character
#' at all (NA, past either end of the text). A superscript digit is still a
#' digit, which after a number may be an exponent ("10<superscript 6> cells");
#' boundaries_ok() tells it from a footnote mark. Unicode classes rather than
#' `[[:alnum:]]`, whose meaning changed with the locale.
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
    x <- cp[hi]
    # Enclosed Alphanumerics, the dingbat circled digits, the circled and
    # bracketed numbers of the CJK blocks, and the digits with a full stop or
    # comma of the supplement.
    enclosed <- (x >= 0x2460L & x <= 0x24FFL) | (x >= 0x2776L & x <= 0x2793L) |
      (x >= 0x3220L & x <= 0x325FL) | (x >= 0x3280L & x <= 0x32BFL) |
      (x >= 0x1F100L & x <= 0x1F10CL)
    out[hi[enclosed]] <- 0L
  }
  out
}

#' Particles written on to a Korean word ("120<myeong>" + "<i>"): a quotation
#' may stop before one, as an English one stops before a space. None of them
#' negates.
#' @noRd
.gr_hangul_particles <- c(
  "\uc774", "\uac00", "\uc740", "\ub294", "\uc744", "\ub97c", "\uc758", "\uc5d0",
  "\uc5d0\uc11c", "\uc5d0\uac8c", "\uaed8", "\uaed8\uc11c", "\ub85c", "\uc73c\ub85c", "\uc640",
  "\uacfc", "\ub3c4", "\ub9cc", "\uae4c\uc9c0", "\ubd80\ud130", "\ubcf4\ub2e4", "\ucc98\ub7fc",
  "\uc774\ub2e4", "\uc600\ub2e4", "\uc774\uc5c8\ub2e4", "\uc774\uba70", "\uc774\uace0",
  "\uc774\ub098", "\ub098", "\uc5d0\ub294", "\uc5d0\uc11c\ub294", "\uc73c\ub85c\ub294",
  "\ub85c\ub294", "\uc5d0\ub3c4", "\uacfc\ub294", "\uc640\ub294", "\uc5d0\uc11c\ub3c4",
  "\uc774\ub77c\ub294", "\ub77c\ub294", "\uc73c\ub85c\uc368", "\ub85c\uc368",
  "\uc774\uc5c8\uc73c\uba70", "\uc600\uc73c\uba70", "\uc774\uc5c8\uace0", "\uc600\uace0")

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
#'   no-break or thin space grouping digits ("1 200"); before a number the
#'   span writes with no leading zero (".45"), any digit, sign or letter;
#' * after a number, a decimal or thousands separator and a digit ("45.2"), or
#'   a grouping space and a digit group.
#'
#' And what does not: an em dash, written as one or as "--", or an en dash
#' between words, is punctuation ("the cohort<em dash>42 patients"); so is a
#' hyphen after a word of five lower-case letters or more before a number of
#' three digits or more, which is what the default cleaner makes of an en dash
#' ("the target-1,204 patients"), and which a compound is not ("COVID-19",
#' "grade-3", "pre-2019"); a digit after a word is a superscript reference
#' that PDF text keeps inline ("as previously reported12"), and one before a
#' word an affiliation mark or a number before its unit; a year followed by a
#' full stop and a footnote number opening the next sentence ("ended in 2019.4
#' Patients were") is not a decimal, nor is a superscript digit after a number
#' other than 10, or after a full stop, a digit going on; a full stop after a word ends a sentence
#' ("the trial.5 patients"), and one after another full stop is an ellipsis
#' ("...12 patients"); a Korean particle ends a word (.gr_hangul_particles).
#' Letters of the scripts written without spaces are no boundary at all
#' (.gr_unspaced).
#' @noRd
boundaries_ok <- function(at, sc, src) {
  if (!length(at)) return(logical(0))
  n <- length(sc)
  cp <- src$cp
  kind <- src$kind
  ends <- sc[c(1L, n)]
  # Every occurrence is the same characters, so the source says what kind the
  # span's own end characters are, without reading their Unicode classes.
  ends <- if (all(ends < 128L)) {
    ((ends >= 97L & ends <= 122L) | (ends >= 65L & ends <= 90L)) + 2L * (ends >= 48L & ends <= 57L)
  } else kind[at[1L] + c(2L, n + 1L)]
  ok <- TRUE
  # Code points: 45 hyphen, 44 comma, 46 full stop, 0x2013 en dash, and the
  # no-break, figure, thin and narrow no-break spaces that group digits.
  group <- function(x) x == 0xa0L | x == 0x2007L | x == 0x2009L | x == 0x202fL
  b1 <- cp[at + 1L]
  k1 <- kind[at + 1L]
  k2 <- kind[at]
  # Before a number: nothing that carries it on, or signs it.
  number_starts <- function(dash_ok = FALSE) {
    k1 == 0L & !(b1 == 45L & cp[at] != 45L & !dash_ok) & !(b1 == 0x2013L & k2 != 1L) &
      !(b1 == 44L & k2 == 2L) & !(b1 == 46L & k2 != 1L & cp[at] != 46L)
  }

  if (ends[1L] == 1L) {
    ok <- k1 != 1L & !(b1 == 45L & k2 == 1L)
  } else if (ends[1L] == 2L) {
    three <- n >= 3L && all(kind[at[1L] + 2:4] == 2L) && (n == 3L || kind[at[1L] + 5L] != 2L)
    # "--" is an em dash as plain text writes it, and as the ligatures
    # cleaner writes it too, so never a minus sign.
    dash_ok <- b1 == 45L & cp[at] != 45L
    if (any(dash_ok)) {
      long <- grepl("^[0-9]{3}|^[0-9]{1,3},[0-9]{3}", intToUtf8(sc[seq_len(min(n, 7L))]), perl = TRUE)
      dash_ok[dash_ok] <- long & vapply(at[dash_ok], function(a)
        a >= 7L && grepl("^\\p{Ll}{5}$", intToUtf8(cp[(a - 4L):a]), perl = TRUE), logical(1))
    }
    ok <- number_starts(dash_ok) & !(group(b1) & k2 == 2L & three)
  } else if (n >= 2L && sc[1L] == 46L && sc[2L] >= 48L && sc[2L] <= 57L) {
    # ".45": the full stop is the number's, so what is before it must not be.
    ok <- number_starts() & !(b1 == 46L)
  }
  if (ends[2L] > 0L) {
    e <- at + n + 1L                      # the last character of the span, padded
    a1 <- cp[e + 1L]
    k1 <- kind[e + 1L]
    k2 <- kind[e + 2L]
    ok <- ok & if (ends[2L] == 1L) {
      fine <- k1 != 1L & !(a1 == 45L & k2 == 1L)
      hangul <- function(x) x >= 0xAC00L & x <= 0xD7A3L
      if (!all(fine) && hangul(sc[n])) {
        for (i in which(!fine & hangul(a1))) {
          run <- cp[(e[i] + 1L):(e[i] + 5L)]
          len <- match(FALSE, hangul(run), nomatch = 6L) - 1L
          fine[i] <- len <= 4L && kind[e[i] + len + 1L] != 1L &&
            intToUtf8(run[seq_len(len)]) %in% .gr_hangul_particles
        }
      }
      fine
    } else {
      digit_group <- group(a1) & k2 == 2L & kind[e + 3L] == 2L & kind[e + 4L] == 2L &
        kind[e + 5L] != 2L
      # A superscript digit after a number is an exponent on 10 ("10<sup 6>")
      # and a footnote mark on anything else ("enrolled 482<sup 1>.", "in
      # 2019<sup 2>"); after a full stop, always a footnote.
      sup <- function(x) x == 0xB9L | x == 0xB2L | x == 0xB3L | x == 0x2070L |
        (x >= 0x2074L & x <= 0x2079L)
      mark <- sup(a1) & !grepl("(?<![0-9.,])10$", intToUtf8(sc), perl = TRUE)
      carries <- (a1 == 44L | a1 == 46L) & k2 == 2L & !sup(cp[e + 2L])
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
      (k1 == 0L | mark) & !carries & !digit_group
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
  at <- match_every(pat$every, src)
  at <- at[at >= from]
  if (!length(at)) return(NA_integer_)
  hit <- at[boundaries_ok(at, sc, src)]
  if (length(hit)) hit[1] else NA_integer_
}

#' Every match of `pattern` in `src$text` (a match_source()), as character
#' positions. The match is made on bytes and turned into characters here:
#' once a text is UTF-8, gregexpr() counts characters from its start for every
#' match, and every occurrence of a common word in a long source took seconds.
#' The patterns found_at() and found_every() use match the same bytes either
#' way: a literal, and lookarounds on ASCII letters and digits, which no byte
#' of a longer UTF-8 character is.
#' @noRd
match_every <- function(pattern, src) {
  m <- gregexpr(pattern, src$text, perl = TRUE, useBytes = TRUE)[[1]]
  if (m[1L] < 0L) return(integer(0))
  at <- as.integer(m)
  starts <- byte_starts(src)
  if (is.null(starts)) at else findInterval(at, starts)
}

#' The byte each character of `src$text` starts at, or NULL for ASCII text,
#' where they are the same; kept in the source's memo.
#' @noRd
byte_starts <- function(src) {
  memo <- src$memo
  if (is.environment(memo) && !is.null(memo$starts)) {
    return(if (isFALSE(memo$starts)) NULL else memo$starts)
  }
  lc <- if (is.environment(memo) && !is.null(memo$lc)) memo$lc else utf8ToInt(src$text)
  out <- if (!length(lc) || all(lc < 128L)) NULL
         else cumsum(c(1L, 1L + (lc >= 0x80L) + (lc >= 0x800L) + (lc >= 0x10000L)))[seq_along(lc)]
  if (is.environment(memo)) memo$starts <- if (is.null(out)) FALSE else out
  out
}

#' The regular expressions found_at() and found_every() look for `s`, whose
#' code points are `sc`, with: `first` the first occurrence not glued to an
#' ASCII letter or digit, `every` each of them, overlapping ones included.
#' @noRd
occurrence_patterns <- function(s, sc) {
  ends <- char_kind(sc[c(1L, length(sc))])
  quoted <- literal_pattern(s)
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
  at <- match_every(occurrence_patterns(s, sc)$every, src)
  if (!length(at)) return(integer(0))
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
#' so no placement is tried twice. Only an occurrence the gap rule could pass
#' is read at all: one in the sentence the piece before ends in, or one that
#' opens a sentence after it (sentence_marks()). Reading every later
#' occurrence of a common word against every earlier one took seconds on a
#' large source. A source that repeats the pieces hundreds of times could
#' still ask for many gaps to be read, so the search reads at most `budget` of
#' them, and a quotation it could not place within that is not verified: a
#' check that gives up says so, as any other does.
#' @noRd
elided_chain <- function(pieces, src, budget = 2000L) {
  n <- length(pieces)
  len <- nchar(pieces)
  occ <- lapply(pieces, found_every, src = src)
  if (!all(lengths(occ))) return(FALSE)
  marks <- sentence_marks(src)
  opened <- lapply(occ, sentence_opened, marks = marks)
  seen <- new.env(parent = emptyenv())
  left <- budget
  rest <- function(i, prev) {
    key <- paste(i, prev)
    if (!is.null(seen[[key]])) return(seen[[key]])
    ok <- FALSE
    take <- occ[[i]] > prev
    if (i > 1L) {
      o <- opened[[i]]
      take <- take & (occ[[i]] <= stop_after(marks, prev) | (!is.na(o) & o > prev) |
                        occ[[i]] - prev <= 21L)
    }
    for (a in occ[[i]][take]) {
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

#' A sentence end: a full stop, question or exclamation mark, colon or
#' semicolon, past any closing quote mark or bracket, before a space or the
#' end of the text; or one of the full-width marks of Chinese and Japanese,
#' which no space follows. `.gr_sentence_opens` is one with the spaces and
#' opening quote marks and brackets after it, up to where a sentence starts.
#' @noRd
.gr_sentence_stop <- "[.!?;:][\"')\\]]*(?=\\s|$)|[\u3002\uff01\uff1f\uff1b\uff1a]"
.gr_sentence_opens <- "[.!?;:\u3002\uff01\uff1f\uff1b\uff1a][\"')\\]]*[\\s\"'(\\[]*"

#' An aside in brackets that does not itself end with a sentence end: a
#' citation "(Smith et al. 2019; Jones et al. 2020)", "(mean age 54.6 years;
#' 48% women)". A full stop or semicolon inside one does not end the sentence
#' around it. "(See Table 2.)" may, and is not one.
#' @noRd
.gr_aside <- "\\([^()]{0,200}[^().!?]\\)|\\[[^\\[\\]]{0,200}[^\\[\\].!?]\\]"

#' Where the sentences of a match_source() end, worked out once per source
#' and kept in its `memo`: `p` and `e` the first and last character of each
#' sentence end (.gr_sentence_stop) outside an aside (.gr_aside), and `os`
#' and `oe` the first and last character of each run that ends one and leads
#' up to the next (.gr_sentence_opens). Read from the code points rather than
#' with those expressions, which on a long UTF-8 text cost the square of its
#' length (match_every()); they say what is read.
#' @noRd
sentence_marks <- function(src) {
  memo <- src$memo
  if (is.environment(memo) && !is.null(memo$marks)) return(memo$marks)
  lc <- if (is.environment(memo) && !is.null(memo$lc)) memo$lc else utf8ToInt(src$text)
  n <- length(lc)
  at <- function(i) lc[pmin(i, n)] * (i <= n)          # 0 past the end
  space <- function(x) x == 32L | (x >= 9L & x <= 13L)
  closer <- function(x) x == 34L | x == 39L | x == 41L | x == 93L
  opener <- function(x) x == 34L | x == 39L | x == 40L | x == 91L | space(x)
  run <- function(from, fits) {
    to <- from
    repeat {
      more <- fits(at(to + 1L)) & to < n
      if (!any(more)) return(to)
      to[more] <- to[more] + 1L
    }
  }
  wide <- which(lc == 0x3002L | lc == 0xFF01L | lc == 0xFF1FL | lc == 0xFF1BL | lc == 0xFF1AL)
  narrow <- which(lc == 46L | lc == 33L | lc == 63L | lc == 59L | lc == 58L)
  e <- run(narrow, closer)
  ends <- e >= n | space(at(e + 1L))
  p <- c(narrow[ends], wide)
  e <- c(e[ends], wide)
  o <- order(p)
  p <- p[o]
  e <- e[o]
  if (length(p)) {
    g <- rbind(asides(lc, 40L, 41L), asides(lc, 91L, 93L))
    if (nrow(g)) {
      g <- g[order(g[, 1L]), , drop = FALSE]
      k <- findInterval(p, g[, 1L])
      aside <- k > 0L & p < cummax(g[, 2L])[pmax(k, 1L)]
      p <- p[!aside]
      e <- e[!aside]
    }
  }
  os <- sort(c(narrow, wide))
  oe <- run(run(os, closer), opener)
  marks <- list(p = p, e = e, os = os, oe = oe)
  if (is.environment(memo)) memo$marks <- marks
  marks
}

#' The innermost bracket pairs of code points `lc`, opening with `open` and
#' closing with `close`, that .gr_aside reads as an aside: 1 to 201
#' characters inside, the last of them not a sentence end. A two-column
#' matrix of the opening and closing positions.
#' @noRd
asides <- function(lc, open, close) {
  o <- which(lc == open)
  cl <- which(lc == close)
  none <- matrix(integer(0), ncol = 2L)
  if (!length(o) || !length(cl)) return(none)
  ev <- c(o, cl)
  ty <- rep(c(1L, 2L), c(length(o), length(cl)))[order(ev)]
  ev <- sort(ev)
  k <- which(ty[-length(ty)] == 1L & ty[-1L] == 2L)
  s <- ev[k]
  f <- ev[k + 1L]
  keep <- f - s - 1L >= 1L & f - s - 1L <= 201L & !(lc[pmax(f - 1L, 1L)] %in% c(46L, 33L, 63L))
  if (!any(keep)) return(none)
  cbind(s[keep], f[keep])
}

#' For each position in `prev`, the last character of the first sentence end
#' after it, or Inf.
#' @noRd
stop_after <- function(marks, prev) {
  k <- findInterval(prev, marks$p) + 1L
  out <- rep(Inf, length(prev))
  has <- k <= length(marks$p)
  out[has] <- marks$e[k[has]]
  out
}

#' For each start in `at`, the position of the sentence end that the text
#' just before it closes (past closing quote marks, spaces and opening
#' brackets), or NA where the text before it does not end a sentence.
#' @noRd
sentence_opened <- function(at, marks) {
  j <- findInterval(at - 1L, marks$os)
  ok <- j > 0L
  out <- rep(NA_integer_, length(at))
  ok[ok] <- marks$oe[j[ok]] >= at[ok] - 1L
  out[ok] <- marks$os[j[ok]]
  out
}

#' What lies between positions `prev` and `at` of a match_source(), as its
#' `text` has it, read from the code points so that a gap far into a long
#' source is not found by counting characters from its start.
#' @noRd
gap_text <- function(src, prev, at) {
  if (at - prev <= 1L) return("")
  lc <- src$memo$lc
  if (!is.null(lc)) return(intToUtf8(lc[(prev + 1L):(at - 1L)]))
  x <- src$cp[(prev + 3L):(at + 1L)]
  x[x == 0x2013L | x == 0x2014L | x == 0x2015L] <- 45L
  x[x == 0xa0L | x == 0x2007L | x == 0x2009L | x == 0x202fL] <- 32L
  intToUtf8(lower_cp(x))
}

#' Does what an elision leaves out keep what the passages either side of it
#' say?
#'
#' `prev` is where the passage before the elision ends in `src$text` (a
#' match_source(), or the normalised text itself), and `at` where the one
#' after it starts. The rule passage_gaps_ok() holds an extracted value's
#' quotation to: within one sentence, the words left out must not include a
#' negation (gap_negated()), or "the drug did ... reduce mortality" is quoted
#' from "the drug did not reduce mortality"; across a sentence end, the passage
#' after the elision must start a sentence, or "Revenue ... rose 30%" is
#' quoted from "Revenue fell 12%. Costs rose 30%.". A sentence ends as
#' .gr_sentence_stop says, but not inside a bracketed aside (.gr_aside), and
#' also where the next passage follows a full stop directly. One sentence's
#' worth of gap is at most 2000 characters; past that the elision is refused.
#' A short gap with no word in it passes (wordy()).
#' @noRd
elision_gap_ok <- function(src, prev, at) {
  if (!is.list(src)) return(elision_gap_text_ok(substr(src, prev + 1L, at - 1L)))
  if (at - prev <= 21L && !wordy(gap_text(src, prev, at))) return(TRUE)
  marks <- sentence_marks(src)
  opened <- sentence_opened(at, marks)
  opens <- !is.na(opened) && opened > prev
  if (opens || at - 1L >= stop_after(marks, prev)) return(opens)
  at - prev <= 2001L && !gap_negated(gap_text(src, prev, at))
}

#' elision_gap_ok() for the gap as a string, with only the asides inside it
#' set aside.
#' @noRd
elision_gap_text_ok <- function(gap) {
  if (nchar(gap) <= 20L && !wordy(gap)) return(TRUE)
  bare <- gsub(.gr_aside, " ", gap, perl = TRUE)
  if (!grepl(.gr_sentence_stop, bare, perl = TRUE)) {
    return(nchar(gap) <= 2000L && !gap_negated(gap))
  }
  # The passage after the gap starts a sentence when the gap ends with one's
  # end, past any space, quote mark or opening bracket.
  grepl(paste0(.gr_sentence_opens, "$"), gap, perl = TRUE)
}

#' Does a stretch of text hold a letter, a digit or a mathematical sign ("<",
#' "=")? A short gap with none of them leaves nothing out: the passages
#' either side of it are the source's own, as when the source itself has an
#' ellipsis there.
#' @noRd
wordy <- function(gap) grepl("[\\p{L}\\p{N}\\p{Sm}]", gap, perl = TRUE)

#' Words that negate what follows them, in the languages the package reads,
#' lower case: what an elision inside one sentence may not leave out
#' (gap_negated()).
#' @noRd
.gr_negations <- unique(c(
  # English
  "not", "no", "never", "neither", "nor", "none", "nobody", "nothing", "nowhere", "without",
  "cannot", "non", "failed", "fail", "fails", "failing", "unable", "lack", "lacked", "lacking",
  "lacks", "absent", "absence", "hardly", "scarcely", "barely", "seldom", "rarely", "little",
  "less",
  # French
  "ne", "pas", "jamais", "aucun", "aucune", "aucuns", "aucunes", "rien", "sans", "ni", "nul",
  "nulle", "gu\u00e8re", "nullement",
  # Spanish, Portuguese, Italian
  "nunca", "jam\u00e1s", "ning\u00fan", "ninguno", "ninguna", "ningunos", "ningunas", "nada",
  "nadie", "sin", "tampoco", "n\u00e3o", "nao", "nenhum", "nenhuma", "nenhuns", "nenhumas",
  "ningu\u00e9m", "sem", "nem", "tampouco", "mai", "nessun", "nessuno", "nessuna", "niente",
  "nulla", "senza", "n\u00e9", "neanche", "nemmeno", "neppure",
  # German, Dutch, the Nordic languages
  "nicht", "kein", "keine", "keinen", "keinem", "keiner", "keines", "nie", "niemals", "nichts",
  "niemand", "ohne", "weder", "nirgends", "niet", "geen", "nooit", "niets", "zonder",
  "nergens", "inte", "ej", "icke", "ingen", "inget", "inga", "aldrig", "utan", "ikke", "ikkje",
  "aldri", "uden", "uten", "hverken", "varken", "ingenting", "ekki", "engin", "aldrei",
  # Central and Eastern Europe
  "bez", "nigdy", "\u017caden", "\u017cadna", "\u017cadne", "\u017cadnego", "\u017cadnych",
  "nic", "nikt", "ani", "nikdy", "\u017e\u00e1dn\u00fd", "\u017e\u00e1dn\u00e1",
  "\u017e\u00e1dn\u00e9", "nikdo", "nincs", "nincsen", "nincsenek", "soha", "semmi", "senki",
  "n\u00e9lk\u00fcl", "nu", "niciun", "nicio", "nimic", "nimeni", "niciodat\u0103",
  "f\u0103r\u0103", "nici", "nije", "nisu", "nikad", "nikada", "brez", "nav", "n\u0117ra", "nuk",
  # Other languages written in Latin letters
  "de\u011fil", "yok", "hi\u00e7", "hi\u00e7bir", "asla", "ei", "eiv\u00e4t", "emme", "ette",
  "eik\u00e4", "ilman", "tidak", "tak", "bukan", "belum", "tanpa", "tiada", "kh\u00f4ng",
  "ch\u01b0a", "ch\u1eb3ng", "ch\u1ea3", "hindi", "wala", "hapana", "bila", "sio", "siyo",
  "ddim", "nid", "n\u00ed", "n\u00edl", "mhux", "ebda", "deyil", "yox", "emas", "tsy",
  # Cyrillic
  "\u043d\u0435", "\u043d\u0435\u0442", "\u043d\u0438", "\u043d\u0456",
  "\u043d\u0435\u043c\u0430\u0454", "\u043d\u0435\u043c\u0430", "\u043d\u044f\u043c\u0430",
  "\u043d\u0438\u043a\u043e\u0433\u0434\u0430", "\u043d\u0456\u043a\u043e\u043b\u0438",
  "\u043d\u0438\u043a\u043e\u0433\u0430", "\u043d\u0438\u0447\u0435\u0433\u043e",
  "\u043d\u0456\u0447\u043e\u0433\u043e", "\u043d\u0438\u043a\u0442\u043e",
  "\u043d\u0456\u0445\u0442\u043e", "\u043d\u0438\u0447\u0442\u043e", "\u0431\u0435\u0437",
  "\u043d\u0435\u043b\u044c\u0437\u044f", "\u0435\u043c\u0435\u0441", "\u0436\u043e\u049b",
  "\u0431\u0438\u0448", "\u04af\u0433\u04af\u0439",
  # Greek
  "\u03b4\u03b5\u03bd", "\u03bc\u03b7\u03bd", "\u03bc\u03b7", "\u03cc\u03c7\u03b9",
  "\u03bf\u03cd\u03c4\u03b5", "\u03bc\u03b7\u03b4\u03ad", "\u03c7\u03c9\u03c1\u03af\u03c2",
  "\u03bf\u03c5\u03b4\u03ad\u03bd", "\u03ba\u03b1\u03bd\u03ad\u03bd\u03b1\u03c2",
  "\u03ba\u03b1\u03bc\u03af\u03b1", "\u03ba\u03b1\u03bd\u03ad\u03bd\u03b1",
  # Arabic, Hebrew, Persian, Urdu
  "\u0644\u0627", "\u0644\u0645", "\u0644\u0646", "\u0644\u064a\u0633",
  "\u0644\u064a\u0633\u062a", "\u063a\u064a\u0631", "\u0628\u062f\u0648\u0646",
  "\u062f\u0648\u0646", "\u0628\u0644\u0627", "\u0639\u062f\u0645", "\u0648\u0644\u0627",
  "\u0648\u0644\u0645", "\u0648\u0644\u0646", "\u0648\u0644\u064a\u0633",
  "\u0648\u063a\u064a\u0631", "\u0648\u0628\u062f\u0648\u0646", "\u0641\u0644\u0627",
  "\u0641\u0644\u0645", "\u05dc\u05d0", "\u05d0\u05d9\u05df", "\u05d0\u05d9\u05e0\u05d5",
  "\u05d0\u05d9\u05e0\u05d4", "\u05d0\u05d9\u05e0\u05dd", "\u05d0\u05d9\u05e0\u05df",
  "\u05d1\u05dc\u05d9", "\u05dc\u05dc\u05d0", "\u05de\u05d1\u05dc\u05d9", "\u05d5\u05dc\u05d0",
  "\u05d5\u05d0\u05d9\u05df", "\u0646\u0647", "\u0647\u06cc\u0686", "\u0646\u06c1\u06cc\u06ba",
  "\u0646\u06c1",
  # Hindi, Bengali; Korean (the adverb; the other negations are in the marks)
  "\u0928\u0939\u0940\u0902", "\u0928\u0939\u0940", "\u0928", "\u092e\u0924",
  "\u092c\u093f\u0928\u093e", "\u09a8\u09be", "\u09a8\u09df", "\u09a8\u09af\u09bc",
  "\u09a8\u09c7\u0987", "\u09a8\u09bf", "\uc548"
))

#' Negations written inside a run of letters, which no word list can pull
#' out: in Chinese and Japanese the characters for not, no, never and
#' without, and "lack"; the Japanese negative endings; the Korean negative
#' verbs and endings, written on to the word; and the Thai negations.
#' Characters that also start other words (<wei> of "future", <bu> of
#' "adverse") make an elision over those refused too, which is the safe way
#' to be wrong.
#' @noRd
.gr_negation_marks <- paste0(
  "[\u4e0d\u6ca1\u6c92\u672a\u65e0\u7121\u975e\u5426\u52ff\u83ab\u6bcb\u5f17]|\u7f3a\u4e4f|",
  "\u306a\u3044|\u306a\u304b\u3063|\u306a\u304f|\u306a\u3057|[\u305a\u306c]|\u307e\u305b\u3093|",
  "[\uc54a\uc5c6\ubabb]|\uc544[\ub2c8\ub2cc\ub2d8]|",
  "\u0e44\u0e21\u0e48|\u0e44\u0e23\u0e49|\u0e1b\u0e23\u0e32\u0e28\u0e08\u0e32\u0e01|",
  "\u0e21\u0e34\u0e44\u0e14\u0e49|\u0e21\u0e34\u0e43\u0e0a\u0e48")

#' A letter of a script whose negations gap_negated() cannot read: any but
#' Latin, Cyrillic, Greek, Chinese, Japanese, Korean, Thai, Arabic, Hebrew,
#' Devanagari and Bengali.
#' @noRd
.gr_unread_letter <- paste0(
  "(?![\\p{Common}\\p{Inherited}\\p{Latin}\\p{Cyrillic}\\p{Greek}\\p{Han}\\p{Hiragana}",
  "\\p{Katakana}\\p{Hangul}\\p{Thai}\\p{Arabic}\\p{Hebrew}\\p{Devanagari}\\p{Bengali}])\\p{L}")

#' Does a stretch of one sentence, lower case, hold a negation?
#'
#' A word of .gr_negations, an English contraction ("wasn't") or French
#' elision ("n'a"), or a mark of .gr_negation_marks anywhere in it. A gap
#' with a letter of a script the lists do not cover (.gr_unread_letter) is
#' counted as negated: what cannot be read is refused, not waved through.
#' "no." before a number or an identifier is the abbreviation ("registration
#' no. ISRCTN12345").
#' @noRd
gap_negated <- function(gap) {
  if (!nzchar(gap)) return(FALSE)
  wide <- grepl("[^\\x01-\\x7f]", gap, perl = TRUE)
  if (wide) {
    if (grepl(.gr_negation_marks, gap, perl = TRUE)) return(TRUE)
    if (grepl(.gr_unread_letter, gap, perl = TRUE)) return(TRUE)
  } else if (!grepl("[A-Za-z]", gap)) {
    return(FALSE)
  }
  if (grepl("no.", gap, fixed = TRUE)) {
    gap <- gsub("(?<![\\p{L}\\p{M}])no\\.(?=[[:space:]]*\\p{L}*[0-9])", " ", gap, perl = TRUE)
  }
  w <- regmatches(gap, gregexpr("[\\p{L}\\p{M}']+", gap, perl = TRUE))[[1]]
  w <- gsub("^'+|'+$", "", w)
  w <- if (wide) fold_case(w) else tolower(w)
  any(w %in% .gr_negations) || any(grepl("n't$|^n'", w))
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
#' with a quote mark; any other line is the one before it, wrapped. A line
#' that opens with a capital after one that ends no sentence may be either (a
#' new passage, or a sentence wrapped before a name, an acronym or a German
#' noun), and the source decides which (quote_reading()). A number is a list
#' marker only as a list's first item, 1, or the next after the item before.
#'
#' @return A list of passages, each a character vector of the pieces an elision
#'   splits it into, normalised and trimmed, to be found in that order (see
#'   found_in_order()). `plain` is the same with markdown bold taken out, or
#'   NULL where there is none. `join` says, for each passage, how the quotation
#'   joins it to the one before (quote_reading()): "wrap" for a line opening
#'   with a capital after one ending no sentence, "elide" where an elision
#'   stands between them, and "sep" otherwise. `items` (and `plain_items`,
#'   with bold taken out) is each passage as written, which quote_reading()
#'   joins where a line turns out to be wrapped. `whole = TRUE` takes the span
#'   as one piece, untrimmed, for text that is the chunk's own rather than
#'   what a model wrote.
#' @noRd
quote_passages <- function(span, whole = FALSE) {
  x <- to_utf8(as_chr1(span, ""))
  none <- list(raw = list(), plain = NULL, join = character(0), items = character(0),
               plain_items = NULL)
  if (whole) {
    # The chunk's own text is found at its own start: nothing about its edges
    # is a model's packaging, and trimming ".45" or "...12" off it made a
    # chunk fail to match itself.
    p <- quote_content(normalise_for_match(x))
    return(if (length(p)) list(raw = list(p), plain = NULL, join = "sep", items = x,
                               plain_items = NULL) else none)
  }
  if (!nzchar(x)) return(none)
  blocks <- lapply(strsplit(x, "\n[[:space:]]*\n", perl = TRUE)[[1]], quote_items)
  items <- unlist(lapply(blocks, `[[`, "items"), use.names = FALSE)
  if (!length(items)) return(none)
  join <- unlist(lapply(blocks, function(b) ifelse(seq_along(b$wrap) > 1L & b$wrap, "wrap", "sep")),
                 use.names = FALSE)
  # One quoted passage closing and the next opening.
  split <- strsplit(items, "[\"\u201d][[:space:]]*[,;]?[[:space:]]+[\"\u201c]", perl = TRUE)
  join <- unlist(lapply(seq_along(split), function(i)
    c(join[i], rep("sep", max(0L, length(split[[i]]) - 1L)))[seq_along(split[[i]])]),
    use.names = FALSE)
  items <- unlist(split, use.names = FALSE)
  if (!length(items)) return(none)
  elision <- .gr_elision
  # An elision closing one passage or opening the next joins the two.
  closes <- grepl(paste0("(?:", elision, ")[[:space:]\"'\u201d\u2019*]*$"), items, perl = TRUE)
  opens <- grepl(paste0("^[[:space:]\"'\u201c\u2018*]*(?:", elision, ")"), items, perl = TRUE)
  join[c(FALSE, closes[-length(items)] | opens[-1L])] <- "elide"
  pieces <- item_pieces
  raw <- pieces(items)
  keep <- lengths(raw) > 0L
  # An item left with nothing to find passes its join on to the next: an
  # elision on a line of its own still joins the lines either side of it.
  held <- character(0)
  for (i in seq_along(items)) {
    if (!keep[i]) {
      held <- c(held, join[i])
    } else if (length(held)) {
      all_j <- c(held, join[i])
      join[i] <- if ("elide" %in% all_j) "elide" else if ("sep" %in% all_j) "sep" else "wrap"
      held <- character(0)
    }
  }
  plain <- NULL
  plain_items <- NULL
  if (any(grepl("**", items, fixed = TRUE))) {
    plain_items <- strip_bold(items, edges = TRUE)
    plain <- pieces(plain_items)
    empty <- !lengths(plain)
    plain[empty] <- raw[empty]
    plain_items[empty] <- items[empty]
    plain <- plain[keep]
    plain_items <- plain_items[keep]
  }
  list(raw = raw[keep], plain = plain, join = join[keep], items = items[keep],
       plain_items = plain_items)
}

#' An elision in a quotation: three or more full stops, an ellipsis character,
#' spaced full stops, or either of those in brackets.
#' @noRd
.gr_elision <- paste0("\\[[[:space:]]*(?:\\.{2,}|\u2026)[[:space:]]*\\]|",
                      "\\([[:space:]]*(?:\\.{2,}|\u2026)[[:space:]]*\\)|",
                      "\\.{3,}|\u2026|(?:\\.[[:space:]]){2,}\\.")

#' The pieces of a normalised string worth looking for: not empty, and with a
#' letter or a digit in them.
#' @noRd
quote_content <- function(p) p[nzchar(p) & grepl("[\\p{L}\\p{N}]", p, perl = TRUE)]

#' Each passage of a quotation, as written, split at its elisions into the
#' pieces to be found, normalised and trimmed of the edges a model adds.
#' @noRd
item_pieces <- function(items) {
  lapply(strsplit(items, .gr_elision, perl = TRUE), function(p) {
    quote_content(vapply(p, function(s) trim_quote_edges(normalise_for_match(s)), character(1),
                         USE.NAMES = FALSE))
  })
}

#' The passages of one block of a quotation (no blank line in it): list items,
#' marker taken off, and lines joined where a line wraps. `wrap` flags an item
#' begun only because its line opens with a capital after a line that ends no
#' sentence. See quote_passages().
#' @noRd
quote_items <- function(block) {
  lines <- strsplit(block, "\n", fixed = TRUE)[[1]]
  lines <- lines[grepl("[^[:space:]]", lines)]
  bullet <- "^[[:space:]]*(?:[\u2022\u2023\u2043\u2219\u25aa\u25cf\u25e6][[:space:]]*|[-*+][[:space:]]+)"
  enum <- "^[[:space:]]*\\([0-9a-zA-Z]\\)[[:space:]]+"
  numbered <- "^[[:space:]]*([0-9]{1,2})[.)][[:space:]]+"
  ends <- "[.!?;:\u2026\u3002\uff01\uff1f\uff1b\uff1a][\"'\u201d\u2019)\\]]*[[:space:]]*$"
  quoted <- "^[[:space:]]*[\"\u201c]"
  capital <- "^[[:space:]]*['\u2018(\\[]?\\p{Lu}"
  items <- character(0)
  wrap <- logical(0)
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
      wrap <- c(wrap, FALSE)
      if (identical(marker, numbered)) number <- k
      in_list <- TRUE
    } else if (!length(items) || ended || grepl(quoted, ln, perl = TRUE) ||
               grepl(capital, ln, perl = TRUE)) {
      items <- c(items, ln)
      wrap <- c(wrap, length(items) > 1L && !ended && !grepl(quoted, ln, perl = TRUE))
      in_list <- FALSE
    } else {
      items[length(items)] <- paste(items[length(items)], ln)
    }
    ended <- grepl(ends, ln, perl = TRUE)
  }
  list(items = items, wrap = wrap)
}

#' Does a quotation, read as `passages` joined as `join` says (quote_passages()),
#' occur in `src`, a match_source()?
#'
#' Each passage has to be found (found_in_order()). So do the joins between
#' them, which say something too:
#'
#' * A line opening with a capital after one that ends no sentence ("wrap") is
#'   a new passage only where the source ends a sentence after the first line
#'   or starts one with the second. Anywhere else it is the same sentence
#'   wrapped, and the two lines are one passage: read as two, "Patients who
#'   were\nHispanic were excluded" verified against "Patients who were
#'   not\nHispanic were excluded", and a quotation that skipped a whole
#'   wrapped line verified too.
#' * Two passages the quotation separates may come from anywhere in the
#'   source, in any order, but not from one sentence with words between them:
#'   that is an elision the quotation does not mark, and "- the drug did\n-
#'   reduce mortality" verified against "the drug did not reduce mortality".
#'   Where an elision does mark it ("elide"), what it leaves out is held to the
#'   rule an elision inside a passage is (gap_negated()). Every place the first
#'   passage ends is checked against the next place the second starts.
#'
#' `items` is each passage as the quotation writes it (quote_passages()); two
#' wrapped lines are joined as written and then trimmed, since what trimming
#' takes off the end of a line is the middle of the passage.
#'
#' @return `list(verified, passages, found, joined)`: `passages` as read (wrapped
#'   lines joined), which were found, and which joins held.
#' @noRd
quote_reading <- function(passages, join, src, items = NULL) {
  n <- length(passages)
  join <- rep_len(if (length(join)) join else "sep", n)
  if (n > 1L && any(join[-1L] == "wrap")) {
    written <- length(items) == n
    out <- passages[1L]
    oj <- join[1L]
    oi <- if (written) items[1L] else character(0)
    for (k in 2:n) {
      m <- length(out)
      prev <- out[[m]]
      if (join[k] == "wrap") {
        a <- prev[length(prev)]
        b <- passages[[k]][1L]
        if (!sentence_ends_after(a, src) && !sentence_starts_at(b, src)) {
          # The two lines as written, joined and then trimmed: the edges
          # trimmed off each piece are the middle of the wrapped passage.
          if (written) {
            oi[m] <- paste(oi[m], items[k])
            out[[m]] <- item_pieces(oi[m])[[1L]]
          } else {
            out[[m]] <- c(prev[-length(prev)], paste(a, b), passages[[k]][-1L])
          }
          next
        }
      }
      out[[m + 1L]] <- passages[[k]]
      oj[m + 1L] <- if (join[k] == "wrap") "sep" else join[k]
      if (written) oi[m + 1L] <- items[k]
    }
    passages <- out
    join <- oj
    n <- length(passages)
  }
  found <- vapply(passages, found_in_order, logical(1), src = src, USE.NAMES = FALSE)
  joined <- rep(TRUE, n)
  if (n > 1L && all(found)) {
    for (k in 2:n) {
      p <- passages[[k - 1L]]
      joined[k] <- passages_join_ok(p[length(p)], passages[[k]][1L], join[k], src)
    }
  }
  list(verified = all(found) && all(joined), passages = passages, found = found, joined = joined)
}

#' How much of a quotation quote_reading() did not find is still a quotation:
#' the lowest passage_score() of a passage it did not find or of the two
#' passages either side of a join that did not hold.
#' @noRd
reading_score <- function(r, src) {
  s <- vapply(which(!r$found), function(i) passage_score(r$passages[[i]], src), numeric(1))
  if (all(r$found)) {
    s <- vapply(which(!r$joined), function(k) {
      p <- r$passages[[k - 1L]]
      passage_score(c(p[length(p)], r$passages[[k]][1L]), src)
    }, numeric(1))
  }
  if (length(s)) min(s) else 1
}

#' The piece `s`, normalised, as a regular expression that matches it literally.
#' @noRd
literal_pattern <- function(s) paste0("\\Q", gsub("\\E", "\\E\\\\E\\Q", s, fixed = TRUE), "\\E")

#' Does the source end a sentence right after some occurrence of the piece
#' `s`, past any closing quote mark or bracket, or end there?
#' @noRd
sentence_ends_after <- function(s, src) {
  at <- gregexpr(paste0("(?=", literal_pattern(s), "[\"')\\]]*(?:", .gr_sentence_stop, "|$))"),
                 src$text, perl = TRUE)[[1]]
  at[1L] > 0L && any(boundaries_ok(as.integer(at), utf8ToInt(s), src))
}

#' Does some occurrence of the piece `s` start a sentence: at the start of the
#' source or after a sentence end, past spaces and opening quote marks and
#' brackets?
#' @noRd
sentence_starts_at <- function(s, src) {
  m <- gregexpr(paste0("(?:^[\\s\"'(\\[]*|", .gr_sentence_opens, ")(?=", literal_pattern(s), ")"),
                src$text, perl = TRUE)[[1]]
  m[1L] > 0L && any(boundaries_ok(as.integer(m) + attr(m, "match.length"), utf8ToInt(s), src))
}

#' May the passage ending with the piece `a` be followed, as the quotation
#' separates them, by the one starting with `b`? Each occurrence of `a` is
#' paired with the first occurrence of `b` after it that no other occurrence
#' of `a` comes nearer to. Where one pair has nothing but spacing and
#' punctuation between them, the quotation is that place, copied. Otherwise
#' no pair may sit in one sentence with words between them (or, for an
#' "elide" join, a negation between them); see quote_reading().
#' @noRd
passages_join_ok <- function(a, b, join, src) {
  pa <- found_every(a, src)
  pb <- found_every(b, src)
  if (!length(pa) || !length(pb)) return(TRUE)
  ea <- pa + nchar(a) - 1L
  k <- findInterval(ea, pb) + 1L
  near <- k <= length(pb)
  near[near] <- c(ea[-1L] >= pb[pmin(k, length(pb))][-length(ea)], TRUE)[near]
  if (!any(near)) return(TRUE)
  ea <- ea[near]
  nb <- pb[k[near]]
  marks <- sentence_marks(src)
  opened <- sentence_opened(nb, marks)
  apart <- nb - 1L >= stop_after(marks, ea) | (!is.na(opened) & opened > ea)
  bad <- FALSE
  for (i in seq_along(ea)) {
    len <- nb[i] - ea[i] - 1L
    gap <- NULL
    if (len <= 20L) {
      gap <- gap_text(src, ea[i], nb[i])
      if (!wordy(gap)) return(TRUE)
    }
    if (apart[i] || bad) next
    bad <- !identical(join, "elide") || len > 2000L ||
      gap_negated(gap %||% gap_text(src, ea[i], nb[i]))
  }
  !bad
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
#' Only reached when the span is not an exact quotation. A run measure rather
#' than a word-overlap one because overlap cannot tell a quotation from a
#' paraphrase built out of the same vocabulary, and that is the distinction
#' the whole check exists to make. A run counts only as whole words
#' (found_at()), so "5% of patients" against "25% of patients" scores the run
#' it really shares and not 1. `sep` joins the words (see quote_units());
#' `source_norm` is a normalised string or a match_source().
#'
#' A near-verbatim quotation has long runs, and in a script written without
#' spaces every character is a unit, so growing each run a unit at a time from
#' every start cost the square of its length: seconds for a Chinese document.
#' The run from one start carries on at least as far from the next, so each
#' run starts where the one before reached, after one search confirms it.
#' @noRd
longest_quoted_run <- function(span_words, source_norm, sep = " ") {
  n <- length(span_words)
  if (!n) return(0L)
  src <- if (is.list(source_norm)) source_norm else match_source(source_norm)
  if (!nzchar(src$text)) return(0L)
  if (n > 300L) { span_words <- span_words[seq_len(300L)]; n <- 300L }
  sep <- rep_len(sep, n)
  run_of <- function(i, j) paste0(span_words[i], paste0(sep[(i + 1L):j], span_words[(i + 1L):j],
                                                       collapse = ""))
  # Each distinct word is looked up once: a paraphrase repeats its short ones.
  words <- unique(span_words)
  which_word <- match(span_words, words)
  alone <- rep(NA, length(words))
  best <- 0L
  reach <- 0L                                     # where the run before ended
  for (i in seq_len(n)) {
    if (n - i + 1L <= best) break                 # cannot beat `best` from here
    w <- which_word[i]
    if (is.na(alone[w])) alone[w] <- !is.na(found_at(span_words[i], src))
    if (!alone[w]) next
    j <- i
    run <- span_words[i]
    if (reach > i) {
      whole <- run_of(i, reach)
      if (!is.na(found_at(whole, src))) {
        j <- reach
        run <- whole
      }
    }
    while (j < n) {
      longer <- paste0(run, sep[j + 1L], span_words[j + 1L])
      if (is.na(found_at(longer, src))) break
      run <- longer
      j <- j + 1L
    }
    best <- max(best, j - i + 1L)
    reach <- j
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
#' whole numbers (found_at()), the pieces of an elided passage in order, and
#' the passages are joined only as the source allows (quote_reading()).
#' Markdown bold is emphasis, in the quotation or the source: the quotation is
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
  r <- quote_reading(q$raw, q$join, src, q$items)
  if (r$verified) return(list(verified = TRUE, match = 1))
  # The bold-free reading, built only when the one as written fails.
  if (!whole && (!is.null(q$plain) || grepl("**", txt, fixed = TRUE))) {
    plain <- match_source(strip_bold(txt))
    p <- quote_reading(q$plain %||% q$raw, q$join, plain, q$plain_items %||% q$items)
    if (p$verified) return(list(verified = TRUE, match = 1))
    return(list(verified = FALSE,
                match = round(max(reading_score(r, src), reading_score(p, plain)), 3)))
  }
  list(verified = FALSE, match = round(reading_score(r, src), 3))
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
#' thin space, nor ".45" in "1.45". Chinese, Japanese and Thai put no spaces
#' between words, so a clause quoted from them may start and end anywhere;
#' numbers in them are still whole, and a circled list number before one is
#' not part of it. Markdown bold is not text, in the quotation or the
#' document.
#' A span made of several passages (in paragraphs, as a list, in separate
#' quote marks, or joined by "..." or "\[...\]") is checked passage by passage
#' and is verified when every passage is found, the parts either side of an
#' elision in that order. What an elision leaves out may not be a negation
#' ("the drug did ... reduce mortality" is not in "the drug did not reduce
#' mortality"). That is checked in the major European languages, Russian,
#' Greek, Turkish, Arabic, Hebrew, Hindi and Bengali by their negation words,
#' and in Chinese, Japanese, Korean and Thai by the characters and endings
#' that negate; what an elision leaves out in a script none of those cover is
#' refused. Where an elision leaves out the end of a sentence the part after
#' it has to start one ("Revenue ... rose 30%" is not in "Revenue fell 12%.
#' Costs rose 30%."); a full stop inside a bracketed citation does not end
#' one. A line break inside a sentence is a line wrap, not a new passage: the
#' lines are checked as one, even where the next line opens with a capital,
#' unless the document ends a sentence after the first line or starts one
#' with the second. Passages the quotation separates (by a blank line, as a
#' list, in quote marks) may come from anywhere in the document, but not from
#' one sentence with words left out between them, which is an elision the
#' quotation did not mark. None of this reads meaning: a quotation that stops
#' before a negation later in its sentence still verifies, as it always has.
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
