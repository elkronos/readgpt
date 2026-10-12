# synthesise-figures.R -- the figures a written review states, looked for in
# the rows each sentence cites.
#
# WHY THIS FILE EXISTS
# The citation check walks a sentence back to a row: `[study 3]` names a row
# that exists and that the section was given. It says nothing about what the
# sentence says that row says, and the numbers are the part of a review a
# reader copies into their own work. "d = 0.61 [study 3]" over a row reporting
# 0.41, or "1,240 participants" over a row of 1,204, passed every check the
# write-up made: the marker was honest, so the sentence was clean. And a
# revision pass could turn 0.41 into 0.61 after the fact, since the guard on
# revisions counts hedges and markers and never looks at a digit.
#
# A figure is the one part of a claim that can be checked without reading it:
# either the cited row has the number or it does not. So every figure in every
# sentence is looked for in what the writer was shown for that sentence -- the
# rows it cites as render_studies() showed them, the question, the section's
# brief, and with claims the claims resting on those rows -- and one found
# nowhere is reported. It checks transcription, not truth: a figure the row
# carries can still be the wrong figure for the sentence, and the row can be
# wrong. What it rules out is the figure that came from nowhere.
#
# It is forgiving about form, because a writer that copies 0.613 as .61, 1,204
# as 1204 or 45.2 million as 45,200,000 has copied it, and a check that cried
# wolf at each of those would be switched off. It is not forgiving about
# precision: a figure is compared with what backs it at the precision the
# sentence wrote it to, so "30%" is not 0.32 rounded to 0.3, nor a pooled
# "d = 0.38" a row's 38.5% read as a proportion and rounded. Comparing rounded
# strings found both. Nothing here sends a request or changes one, so it runs
# on every write-up; and it marks no section partial, which means a citation
# that cannot be trusted.
#
# A revision gets the stricter rule. A pass may not add anything, so a figure in
# the revision that its draft did not have, in any form, is never an edit:
# figure_guard() refuses it, as strength_guard() refuses a dropped hedge.

#' Scale words a figure may carry, and the power of ten each stands for.
#'
#' Spelled out after a space ("45.2 million"), abbreviated only when glued to
#' the figure ("$45.2m", "3bn"): "5 m" is as often five metres. A scaled
#' figure is also read at its full value, so "45.2 million" in a sentence is
#' found in a row that says 45,200,000, and the other way round.
#' @noRd
.gr_figure_scale <- c(thousand = 3L, million = 6L, billion = 9L, trillion = 12L,
                      k = 3L, m = 6L, mn = 6L, b = 9L, bn = 9L, tn = 12L)

#' @noRd
.gr_figure_scale_re <- paste0("^(?:[[:space:]]*(thousand|million|billion|trillion)\\b|",
                              "(k|m|mn|b|bn|tn)\\b)")

#' What may follow a figure to make it a percentage, which is also read as a
#' proportion: "45%" in a sentence is found in a row that says 0.45.
#' @noRd
.gr_figure_pct <- "^[[:space:]]*(?:%|\uff05|per[[:space:]]?cent\\b|percent\\b|pc\\b)"

#' What may follow a figure to make it a difference in points ("a 9.4
#' percentage-point gain", "4.5 points"), which may be the difference of two
#' figures in the rows cited: "38.5% vs 29.1%". Not a percentage of anything.
#' @noRd
.gr_figure_points <- "^[[:space:]-]*(?:percentage[[:space:]-]+)?points?\\b|^[[:space:]]*pp\\b"

#' Number words a row may use where the review writes digits. Read in what
#' backs a figure only, never as a figure to check: "three trials" in a review
#' restates nothing a row has to carry, and "a 12-week trial" over a row that
#' says "twelve weeks" copied it.
#' @noRd
.gr_figure_units <- c(zero = 0L, one = 1L, two = 2L, three = 3L, four = 4L, five = 5L,
                      six = 6L, seven = 7L, eight = 8L, nine = 9L, ten = 10L, eleven = 11L,
                      twelve = 12L, thirteen = 13L, fourteen = 14L, fifteen = 15L,
                      sixteen = 16L, seventeen = 17L, eighteen = 18L, nineteen = 19L,
                      dozen = 12L, hundred = 100L)

#' @noRd
.gr_figure_tens <- c(twenty = 20L, thirty = 30L, forty = 40L, fifty = 50L, sixty = 60L,
                     seventy = 70L, eighty = 80L, ninety = 90L)

#' A figure: digits grouped in threes by commas or the no-break and thin
#' spaces, an optional decimal part, or a decimal with no leading zero (".61",
#' as APA style writes a correlation). Not a sign: "-0.4" and "a fall of 0.4"
#' state the same figure, and which way it went is a word matter this does not
#' read. A range "0.32-0.90", with a hyphen or a dash, is two figures.
#' @noRd
.gr_figure_re <- paste0(
  "(?<![0-9.,])(?:[0-9]{1,3}(?:[,\u00a0\u2009\u202f][0-9]{3})+(?![0-9])|[0-9]+)(?:\\.[0-9]+)?",
  "|(?<![0-9.])\\.[0-9]+")

#' Numbers that are conventions rather than findings, taken out before a
#' sentence is read: the level of an interval ("95% CI" restates no row) and a
#' significance threshold ("p < .001", over a row reporting p = 0.0004). Each
#' pattern keeps what it captured and drops the number.
#' @noRd
.gr_figure_conventions <- c(
  paste0("(?i)\\b(?:80|90|95|99)(?:\\.0+)?[[:space:]]*(?:%|per[[:space:]]?cent)",
         "([[:space:]]*(?:CIs?|CrIs?|PIs?|UIs?|confidence|credible|prediction|uncertainty)\\b)"),
  paste0("(?i)(\\bp[[:space:]]*(?:-?[[:space:]]*values?)?[[:space:]]*",
         "(?:<=?|>=?|=<|=>|\u2264|\u2265)[[:space:]]*)0?\\.(?:1|05|01|005|001|0001)(?![0-9])"))

#' Words before a figure that make it approximate or a bound, and so let it
#' stand for a value near it: "about 1,200 participants" over a row of 1,204,
#' "more than 1,000" over the same. `about` is within a tenth either way;
#' `over` up to a quarter above; `under` up to a quarter below. Read only after
#' every other test has failed, so "over 12 weeks" over a row of 12 weeks is
#' found as written.
#' @noRd
.gr_figure_qualifiers <- c(
  about = paste0("(?:\\b(?:about|approximately|approx\\.?|around|roughly|nearly|almost|",
                 "close to|some|circa|ca\\.|c\\.|an estimated|estimated)|~|\u2248|\u223c)$"),
  over = paste0("(?:\\b(?:more than|over|above|at least|exceeding|in excess of|upwards of|",
                "greater than|higher than)|>=?|\u2265)$"),
  under = paste0("(?:\\b(?:fewer than|less than|under|below|up to|at most|no more than|",
                 "lower than)|<=?|\u2264)$"))

#' @noRd
figure_none <- function() {
  list(written = character(0), value = character(0), digits = integer(0), pct = logical(0),
       points = logical(0), scale = integer(0), qualifier = character(0))
}

#' A figure written as digits, in one canonical form: no grouping, no leading
#' zeros before the point, none trailing after it. "0.61" and ".61", "45.2"
#' and "45.20", "1,204" and "1204" are the same string.
#' @noRd
figure_canon <- function(x) {
  if (!length(x)) return(character(0))
  x <- gsub("[^0-9.]", "", x)
  has <- grepl(".", x, fixed = TRUE)
  int <- sub("\\..*$", "", x)
  frac <- ifelse(has, sub("^[^.]*\\.", "", x), "")
  int <- sub("^0+", "", int)
  int[!nzchar(int)] <- "0"
  frac <- sub("0+$", "", frac)
  ifelse(nzchar(frac), paste0(int, ".", frac), int)
}

#' A canonical figure with its decimal point moved `k` places right (`k < 0`:
#' left), by moving the point in the string, so "45.2" times a million is
#' exactly 45200000 and not whatever the product of two doubles rounds to.
#' @noRd
figure_shift <- function(x, k) {
  if (!length(x) || !k) return(x)
  has <- grepl(".", x, fixed = TRUE)
  int <- sub("\\..*$", "", x)
  frac <- ifelse(has, sub("^[^.]*\\.", "", x), "")
  if (k > 0L) {
    digits <- paste0(int, frac, strrep("0", k))
    at <- nchar(int) + k
  } else {
    digits <- paste0(strrep("0", -k), int, frac)
    at <- nchar(int)
  }
  figure_canon(paste0(substr(digits, 1L, at), ".", substring(digits, at + 1L)))
}

#' A text with every citation in it taken out, so the ids are not read as
#' figures: the markers the check reads, brackets that open like one and do
#' not parse ("[studies 1 to 7]"), claim markers a writer echoes from its
#' prompt, and ids written bare in prose ("as study 12 found"). The bare form
#' is the marker's own grammar without its bracket.
#' @noRd
figure_unmark <- function(txt, word = "study") {
  g <- cite_grammar(word)
  txt <- gsub(g$marker, " ", txt, perl = TRUE, ignore.case = TRUE)
  txt <- gsub(sprintf("\\[%s%s[^\\]\\[]*\\]", g$word, g$space), " ", txt, perl = TRUE,
              ignore.case = TRUE)
  txt <- gsub("\\[claims?[^]\\[]*\\]", " ", txt, perl = TRUE, ignore.case = TRUE)
  # `ids` is "\\[" and then the list of ids; without its first two characters
  # it is the list alone.
  txt <- gsub(paste0("\\b", substring(g$ids, 3L)), " ", txt, perl = TRUE, ignore.case = TRUE)
  gsub(paste0("\\bclaims?", g$space, "+[0-9]+(?:", g$space, "*(?:[,;&]|and)", g$space,
              "*[0-9]+)*"), " ", txt, perl = TRUE, ignore.case = TRUE)
}

#' The figures a text states.
#'
#' `strict = TRUE` reads a sentence being checked, and leaves out what is not
#' a finding: a number glued to a letter or to a word by a hyphen ("COVID-19",
#' "IL-6", "HbA1c", "SF-36"), an ordinal ("2nd"), a decade ("1990s"), the
#' conventions in .gr_figure_conventions, and a whole number below ten that is
#' not a percentage or scaled. Those last are counts of studies, arms, doses
#' and phases as often as findings, and a review writes them as it likes; a
#' check that flagged "2 of the trials" would be ignored for the figures that
#' matter. `strict = FALSE` reads what backs a figure, and keeps every number.
#'
#' Returns the figures as `written` (the digits as they stand), `value`
#' (canonical), `digits` (decimal places as written: the precision it was
#' given to), `pct`, `points`, `scale` (a power of ten, 0 for none) and
#' `qualifier` (one of the names of .gr_figure_qualifiers, or NA).
#' @noRd
figure_find <- function(text, word = "study", strict = TRUE) {
  txt <- fold_width(mark_utf8(as_chr1(text, "")))
  if (!grepl("[0-9]", txt)) return(figure_none())
  txt <- figure_unmark(txt, word)
  if (strict) {
    for (p in .gr_figure_conventions) txt <- gsub(p, "\\1", txt, perl = TRUE)
  }
  m <- gregexpr(.gr_figure_re, txt, perl = TRUE)[[1]]
  if (m[1] < 0L) return(figure_none())
  st <- as.integer(m)
  en <- st + attr(m, "match.length") - 1L
  written <- substring(txt, st, en)
  before <- substring(txt, pmax(1L, st - 40L), st - 1L)
  after <- substring(txt, en + 1L, en + 24L)
  value <- figure_canon(written)
  digits <- nchar(sub("^[^.]*\\.?", "", gsub("[^0-9.]", "", written)))
  pct <- grepl(.gr_figure_pct, after, perl = TRUE, ignore.case = TRUE)
  points <- grepl(.gr_figure_points, after, perl = TRUE, ignore.case = TRUE)
  hit <- regexpr(.gr_figure_scale_re, after, perl = TRUE, ignore.case = TRUE)
  sw <- rep("", length(after))
  sw[hit > 0L] <- lower_text(gsub("[^A-Za-z]", "", regmatches(after, hit)))
  scale <- ifelse(nzchar(sw), .gr_figure_scale[sw], 0L)
  scale[is.na(scale)] <- 0L
  b <- lower_text(sub("[[:space:]$\u00a3\u20ac\u00a5(]*$", "", before, perl = TRUE))
  qualifier <- rep(NA_character_, length(b))
  for (q in rev(names(.gr_figure_qualifiers))) {
    qualifier[grepl(.gr_figure_qualifiers[[q]], b, perl = TRUE)] <- q
  }
  keep <- rep(TRUE, length(value))
  if (strict) {
    glued <- grepl("\\p{L}[-\u2010\u2011\u2013]?$", before, perl = TRUE)
    ordinal <- grepl("^(?:st|nd|rd|th)\\b", after, perl = TRUE, ignore.case = TRUE)
    decade <- grepl("^[0-9]{3}0$", written) & grepl("^'?s\\b", after, perl = TRUE)
    small <- digits == 0L & nchar(value) == 1L & !pct & scale == 0L
    keep <- !(glued | ordinal | decade | small)
  }
  list(written = written[keep], value = value[keep], digits = as.integer(digits[keep]),
       pct = pct[keep], points = points[keep], scale = as.integer(scale[keep]),
       qualifier = qualifier[keep])
}

#' The number words in a text, as values: "twelve", "forty-five", "a dozen".
#' Up to ninety-nine, and "hundred".
#' @noRd
figure_words <- function(text) {
  low <- lower_text(as_chr1(text, ""))
  if (!grepl("[a-z]", low)) return(numeric(0))
  units <- .gr_figure_units[.gr_figure_units >= 1L & .gr_figure_units <= 9L]
  tens_re <- sprintf("\\b(%s)(?:[- \u2010\u2011]+(%s))?\\b",
                     paste(names(.gr_figure_tens), collapse = "|"),
                     paste(names(units), collapse = "|"))
  m <- gregexpr(tens_re, low, perl = TRUE)
  tens <- regmatches(low, m)[[1]]
  out <- vapply(tens, function(t) {
    w <- strsplit(t, "[- \u2010\u2011]+", perl = TRUE)[[1]]
    as.numeric(.gr_figure_tens[[w[1]]] + if (length(w) > 1L) units[[w[2]]] else 0L)
  }, numeric(1), USE.NAMES = FALSE)
  # Taken out before the units are read, or "forty-five" was 5 as well as 45.
  regmatches(low, m) <- list(rep(" ", length(tens)))
  one <- regmatches(low, gregexpr(sprintf("\\b(?:%s)\\b",
                                          paste(names(.gr_figure_units), collapse = "|")),
                                  low, perl = TRUE))[[1]]
  unique(c(out, as.numeric(.gr_figure_units[one])))
}

#' Every value a text could be backing, in every form a faithful writer may
#' give it. `raw` is what the text wrote: its figures, read with digits
#' grouped by a space and with a decimal comma as well, and its number words.
#' `derived` is what this works out from them: a scaled figure at full value,
#' a percentage as a proportion.
#'
#' Apart because a figure may round what a row wrote and not what this worked
#' out. Rounding both, a pooled "d = 0.38" no study reported matched a row's
#' "38.5%", read as 0.385 and rounded; the writer that turns a row's
#' percentage into a proportion copies it exactly or has done arithmetic.
#' @noRd
figure_values <- function(text, word = "study") {
  txt <- as_chr1(text, "")
  if (!nzchar(txt)) return(list(raw = numeric(0), derived = numeric(0)))
  f <- figure_find(txt, word, strict = FALSE)
  sc <- f$scale > 0L
  derived <- c(unlist(Map(figure_shift, f$value[sc], f$scale[sc]), use.names = FALSE),
               figure_shift(f$value[f$pct], -2L))
  # Digits grouped by an ordinary space ("1 204", as SI style writes it), and
  # a decimal comma ("45,2"): ambiguous in a sentence being checked, but in
  # what backs it, reading them both ways can only find more.
  grp <- regmatches(txt, gregexpr("(?<![0-9.,])[0-9]{1,3}(?: [0-9]{3})+(?![0-9])", txt,
                                  perl = TRUE))[[1]]
  dec <- regmatches(txt, gregexpr("(?<![0-9.,])[0-9]+,[0-9]{1,2}(?![0-9])", txt,
                                  perl = TRUE))[[1]]
  raw <- c(f$value, figure_canon(grp), figure_canon(sub(",", ".", dec, fixed = TRUE)))
  list(raw = unique(c(as.numeric(raw), figure_words(txt))),
       derived = unique(as.numeric(derived)))
}

#' Several figure_values() as one.
#' @noRd
figure_pool <- function(vals) {
  list(raw = unique(unlist(lapply(vals, `[[`, "raw"), use.names = FALSE)),
       derived = unique(unlist(lapply(vals, `[[`, "derived"), use.names = FALSE)))
}

#' What each figure of a sentence may equal, and how near: as written, to
#' within half a unit in the last place it was written to (0.61 is 0.605 to
#' 0.615, so 0.613 backs it and 0.62 does not); at full value when scaled, to
#' the same places ("45.2 million" is 45,150,000 to 45,250,000); and as a
#' proportion when a percentage ("45%" is 0.445 to 0.455). `exact` compares
#' values alone, for figure_guard().
#' @noRd
figure_targets <- function(f, exact = FALSE) {
  lapply(seq_along(f$value), function(i) {
    d <- f$digits[i]
    t <- as.numeric(f$value[i])
    tol <- 0.5 * 10^-d
    if (f$scale[i] > 0L) {
      t <- c(t, as.numeric(figure_shift(f$value[i], f$scale[i])))
      tol <- c(tol, 0.5 * 10^(f$scale[i] - d))
    }
    if (f$pct[i]) {
      t <- c(t, as.numeric(figure_shift(f$value[i], -2L)))
      tol <- c(tol, 0.5 * 10^-(d + 2L))
    }
    if (exact) tol[] <- 0
    # For the doubles the strings became: 0.615 - 0.61 is not 0.005 exactly.
    list(t = t, tol = tol + 1e-9 * pmax(1, abs(t)))
  })
}

#' Whether a value in `pool` is one of a figure's targets: a raw value to the
#' figure's precision, a derived one exactly (see figure_values()). A plain
#' vector is raw: a total, or a value worked out in the sentence, may be
#' rounded as the writer gives it.
#' @noRd
figure_hit <- function(tg, pool) {
  if (!is.list(pool)) pool <- list(raw = pool, derived = numeric(0))
  for (j in seq_along(tg$t)) {
    if (any(abs(pool$raw - tg$t[j]) <= tg$tol[j])) return(TRUE)
    if (any(abs(pool$derived - tg$t[j]) <= 1e-9 * max(1, abs(tg$t[j])))) return(TRUE)
  }
  FALSE
}

#' Whether a qualified figure ("about 1,200") is near enough a value in `pool`.
#' @noRd
figure_near <- function(tg, pool, qualifier) {
  if (is.list(pool)) pool <- unlist(pool, use.names = FALSE)
  if (!length(pool) || is.na(qualifier)) return(FALSE)
  any(vapply(tg$t, function(c) {
    if (c <= 0) return(FALSE)
    switch(qualifier,
           about = any(abs(pool - c) <= 0.1 * c),
           over = any(pool >= c & pool <= 1.25 * c),
           under = any(pool <= c & pool >= 0.75 * c),
           FALSE)
  }, logical(1)))
}

#' Values worked out from two other figures of the same sentence, so the
#' working is on the page: "54 of 120 (45%)", "from 29.1% to 38.5%, a rise of
#' 9.4", "fell from 12 to 9, by 25%". A percentage may be one figure of the
#' other or the change between them; anything else, their difference.
#' `ops` is every number in the sentence, the figure's own among them.
#' @noRd
figure_worked <- function(ops, own, pct) {
  ops <- ops[!is.na(ops)]
  at <- match(own, ops)
  if (!is.na(at)) ops <- ops[-at]
  if (length(ops) < 2L) return(numeric(0))
  a <- rep(ops, each = length(ops))
  b <- rep(ops, times = length(ops))
  pair <- rep(seq_along(ops), each = length(ops)) != rep(seq_along(ops), times = length(ops))
  a <- a[pair]
  b <- b[pair]
  if (pct) c(100 * a[b > 0] / b[b > 0], 100 * abs(a[a > 0] - b[a > 0]) / a[a > 0]) else abs(a - b)
}

#' The differences between every two values of `v`.
#' @noRd
figure_diffs <- function(v) {
  if (length(v) < 2L) return(numeric(0))
  d <- abs(outer(v, v, "-"))
  d[upper.tri(d)]
}

#' Totals of each field over a set of rows, where every row gives that field
#' one figure: "together they enrolled 1,020 people [study 1] [study 2]" over
#' rows of 120 and 900. `vals` is a numeric matrix, NA where a row's field held
#' none or several, one row per study.
#' @noRd
figure_sums <- function(vals) {
  if (is.null(dim(vals)) || nrow(vals) < 2L || !ncol(vals)) return(numeric(0))
  s <- colSums(vals)
  unique(s[!is.na(s)])
}

#' Years a text carries, for a figure that is one: 1500 to 2099, standing alone.
#' @noRd
figure_years <- function(text) {
  unique(regmatches(text, gregexpr("(?<![0-9.,])(?:1[5-9]|20)[0-9]{2}(?![0-9])", text,
                                   perl = TRUE))[[1]])
}

#' A marked text cut at its headings: one piece per heading, with the text
#' before the first heading under NA.
#' @noRd
figure_pieces <- function(text) {
  lines <- strsplit(as_chr1(text, ""), "\n", fixed = TRUE)[[1]]
  if (!length(lines)) return(list(heading = character(0), text = character(0)))
  is_head <- grepl("^#{1,6}[[:space:]]", lines)
  group <- cumsum(is_head)
  heads <- c(NA_character_,
             trimws(sub("[ \t]+#+[ \t]*$", "", sub("^#{1,6}[[:space:]]+", "", lines[is_head]))))
  g <- sort(unique(group))
  list(heading = heads[g + 1L],
       text = vapply(g, function(k) paste(lines[group == k & !is_head], collapse = "\n"),
                     character(1)))
}

#' @noRd
figure_empty_table <- function() {
  data.frame(stage = character(0), section = character(0), sentence = character(0),
             number = character(0), studies = character(0), backed = logical(0),
             basis = character(0), stringsAsFactors = FALSE)
}

#' Check every figure in a write-up against what its sentence was written from.
#'
#' Sentence by sentence, as strength_guard() measures them
#' (measured_sentences(): a run of markers after the full stop belongs to the
#' sentence before it). A sentence citing studies is checked against those
#' rows, as render_studies() showed them to the writer; one citing nothing,
#' against every row its section was shown. Either way also against the
#' question, the heading and brief, the register, the gaps a closing section
#' was given, and with claims the claims given to the section that rest on a
#' cited study. A figure found in none of those is also allowed when it is the
#' total of a numeric field over the cited rows; a year a cited row carries in any
#' column (a withheld `year` included: it is the study's, and the rendered
#' citation says the same); the number of studies cited, shown or written
#' from; worked out from two other figures of the sentence; a difference in
#' points between two figures of the cited rows; or, qualified ("about",
#' "more than"), near a value in them.
#'
#' The draft is checked section by section against what each section was
#' shown, mirroring synth_section(): with claims, the studies behind its
#' claims; a closing section written from the gaps, the studies the gaps name.
#' `final` is the kept revision, if there is one, and is one text whose
#' sections a revision may have merged or renamed, so each of its sentences is
#' checked against what any section was shown.
#'
#' Returns `table` (one row per figure: `stage`, `section`, `sentence`,
#' `number` as written, `studies` comma-joined or NA, `backed`, and `basis`,
#' what it was found in), and `n_unbacked`, per row of `sections`, counting
#' the draft.
#' @noRd
synth_figures <- function(sections, final, used, rendered, question, claims = NULL,
                          assign_map = NULL, gaps = NULL, closing = NA_character_,
                          style = NULL, hide = character(0), trace = NULL, word = "study") {
  n_sec <- NROW(sections)
  if (!n_sec || !nrow(used)) return(list(table = figure_empty_table(), n_unbacked = integer(n_sec)))
  ids <- used$study
  # Each row as the writer saw it, without its "[study N]" line: N is the id.
  row_vals <- lapply(sub("^[^\n]*\n?", "", rendered), figure_values, word = word)
  # Every column for a year, the withheld ones included; see above.
  row_years <- lapply(seq_len(nrow(used)), function(i) {
    figure_years(paste(vapply(used[i, , drop = FALSE], function(v) as_chr1(v, ""),
                              character(1)), collapse = "\n"))
  })
  fields <- study_fields(used, hide)
  # A field is totalled only where it is a bare number in every row cited, as
  # a sample size is. Totalling any field that held one figure added "8
  # sessions" to "12 weekly sessions" and backed a "20%" with it.
  one <- function(v) {
    v <- trimws(as_chr1(v, ""))
    if (!grepl("^[0-9][0-9,.]*$", v)) return(NA_real_)
    f <- figure_find(v, word, strict = FALSE)
    if (length(f$value) == 1L) as.numeric(f$value) else NA_real_
  }
  field_vals <- matrix(vapply(fields, function(fl) {
    vapply(seq_len(nrow(used)), function(i) one(used[[fl]][i]), numeric(1))
  }, numeric(nrow(used))), nrow = nrow(used), ncol = length(fields))

  claim_vals <- list()
  claim_rows <- list()
  if (!is.null(claims) && NROW(claims$claims)) {
    cw <- claims$claims
    for (k in seq_len(nrow(cw))) {
      key <- as.character(cw$claim_id[k])
      txt <- c(cw$claim[k], if (!is.null(cw$moderator)) cw$moderator[k],
               if (!is.null(cw$scope)) cw$scope[k])
      claim_vals[[key]] <- figure_values(paste(txt[!is.na(txt)], collapse = "\n"), word = word)
      claim_rows[[key]] <- unique(claims$support$study[claims$support$claim_id == cw$claim_id[k]])
    }
  }
  fixed_text <- function(...) {
    x <- unlist(list(...), use.names = FALSE)
    paste(x[!is.na(x)], collapse = "\n")
  }

  # What each draft section was shown, by the rules synth_section() wrote it
  # under.
  ctx <- lapply(seq_len(n_sec), function(i) {
    heading <- sections$section[i]
    cid <- if (is.null(assign_map)) integer(0) else
      as.integer(assign_map$claim_id[as.character(assign_map$section) == heading])
    by_claims <- !is.null(claims) && length(cid) > 0L
    g <- if (!is.na(closing) && identical(heading, closing)) gaps else NULL
    fixed <- fixed_text(question, heading, sections$brief[i], style, g)
    shown <- if (by_claims) {
      unique(claims$support$study[claims$support$claim_id %in% cid])
    } else if (!is.null(claims) && is_nonblank(g)) {
      intersect(cited_ids(fixed, word), ids)
    } else ids
    list(shown = shown, claims = if (by_claims) as.character(cid) else character(0),
         fixed = figure_values(fixed, word = word))
  })
  everything <- list(
    shown = sort(unique(unlist(lapply(ctx, `[[`, "shown"), use.names = FALSE))),
    claims = unique(unlist(lapply(ctx, `[[`, "claims"), use.names = FALSE)),
    fixed = figure_values(fixed_text(question, sections$section, sections$brief, style, gaps),
                          word = word))

  check <- function(sentence, cx) {
    f <- figure_find(sentence, word)
    if (!length(f$value)) return(NULL)
    cited <- intersect(cited_ids(sentence, word), ids)
    rows <- match(if (length(cited)) cited else cx$shown, ids)
    pool <- figure_pool(row_vals[rows])
    cl <- cx$claims[vapply(cx$claims, function(k) {
      !length(cited) || any(claim_rows[[k]] %in% cited)
    }, logical(1))]
    cpool <- figure_pool(claim_vals[cl])
    sums <- if (length(cited) >= 2L) figure_sums(field_vals[rows, , drop = FALSE]) else numeric(0)
    years <- unique(unlist(row_years[rows], use.names = FALSE))
    counts <- as.character(unique(c(if (length(cited)) length(cited), length(cx$shown),
                                    length(ids))))
    ops <- as.numeric(figure_find(sentence, word, strict = FALSE)$value)
    tgs <- figure_targets(f)
    basis <- vapply(seq_along(tgs), function(k) {
      tg <- tgs[[k]]
      plain <- !f$pct[k] && !f$scale[k] && !f$digits[k]
      if (figure_hit(tg, pool)) return(if (length(cited)) "cited row" else "rows shown")
      if (figure_hit(tg, sums)) return("total of the cited rows")
      if (figure_hit(tg, cpool)) return("claim")
      if (figure_hit(tg, cx$fixed)) return("question or brief")
      if (plain && f$value[k] %in% years) {
        return(if (length(cited)) "year of a cited row" else "year of a row")
      }
      if (plain && f$value[k] %in% counts) return("number of studies")
      own <- list(t = tg$t[1], tol = tg$tol[1])
      if (figure_hit(own, figure_worked(ops, tg$t[1], f$pct[k]))) {
        return("worked out in the sentence")
      }
      if (f$points[k] && length(pool$raw) > 1L) {
        # Across the rows when they are few, within each row otherwise: a
        # sentence citing nothing in a review of two hundred studies would
        # otherwise difference every value of all of them with every other.
        d <- if (length(pool$raw) <= 300L) figure_diffs(pool$raw) else
          unlist(lapply(row_vals[rows], function(r) figure_diffs(r$raw)), use.names = FALSE)
        if (figure_hit(own, d)) return("difference of two cited figures")
      }
      near <- c(unlist(pool, use.names = FALSE), sums, unlist(cpool, use.names = FALSE),
                unlist(cx$fixed, use.names = FALSE))
      if (figure_near(tg, near, f$qualifier[k])) return("approximate")
      NA_character_
    }, character(1))
    data.frame(sentence = rep(trimws(sentence), length(basis)), number = f$written,
               studies = if (length(cited)) paste(sort(cited), collapse = ", ") else NA_character_,
               backed = !is.na(basis), basis = basis, stringsAsFactors = FALSE)
  }
  run <- function(text, cx, stage, section) {
    sents <- measured_sentences(text, word)
    out <- do.call(rbind, lapply(sents, check, cx = cx))
    if (is.null(out)) return(NULL)
    cbind(data.frame(stage = stage, section = section, stringsAsFactors = FALSE), out)
  }

  draft <- lapply(seq_len(n_sec), function(i) {
    run(as_chr1(sections$text[i], ""), ctx[[i]], "draft", sections$section[i])
  })
  n_unbacked <- vapply(draft, function(d) if (is.null(d)) 0L else sum(!d$backed), integer(1))
  fin <- NULL
  if (!is.null(final)) {
    pc <- figure_pieces(final)
    fin <- lapply(seq_along(pc$text), function(k) run(pc$text[k], everything, "final", pc$heading[k]))
  }
  tab <- do.call(rbind, c(draft, fin))
  if (is.null(tab)) tab <- figure_empty_table()
  rownames(tab) <- NULL

  # Said about the text that is published: the revision if one was kept. The
  # draft's own count stays on `$sections`.
  stage <- if (is.null(final)) "draft" else "final"
  bad <- tab[tab$stage == stage & !tab$backed, , drop = FALSE]
  trace_note(trace, "synthesise.figures",
             list(checked = sum(tab$stage == stage), unbacked = nrow(bad), stage = stage))
  if (nrow(bad)) {
    eg <- utils::head(bad, 5L)
    eg <- sprintf("%s in '%s'", eg$number, ifelse(is.na(eg$section), "(no heading)", eg$section))
    gr_warn(sprintf(paste0("%d figure(s) in the %s are not in the rows their sentence cites, nor ",
                           "in anything else the writer was shown (%s%s). Each is arithmetic the ",
                           "sentence does not show, or the writer's invention: check it against ",
                           "its study before quoting it (see `$figures`). No section is ",
                           "marked partial for this, which is about citations, and those are ",
                           "checked apart."),
                    nrow(bad), if (is.null(final)) "review" else "revised review",
                    paste(eg, collapse = "; "), if (nrow(bad) > 5L) "; ..." else ""),
            class = "gr_synth_unbacked_figures")
  }
  list(table = tab, n_unbacked = n_unbacked)
}

#' Refuse a revision that wrote a figure its draft did not have.
#'
#' The passes may reorder, cut and polish; none may add, so a figure in the
#' revision that the draft gives in no form -- as written, grouped or not,
#' scaled, as a proportion, or in words -- did not come from the draft. Not
#' rounded: "0.61" for the draft's "0.613" is a changed figure, however small
#' the change, and the draft is what the rows were checked against. Read as
#' figure_find() reads a sentence, so a count under ten, an interval's level and
#' a citation's ids are not figures here either.
#' @noRd
figure_guard <- function(before, after, word = "study") {
  f <- figure_find(paste(split_sentences(after), collapse = "\n"), word)
  if (!length(f$value)) return(list(ok = TRUE, reason = NA_character_))
  had <- figure_values(before, word = word)
  new <- !vapply(figure_targets(f, exact = TRUE), figure_hit, logical(1), pool = had)
  if (!any(new)) return(list(ok = TRUE, reason = NA_character_))
  added <- unique(f$written[new])
  list(ok = FALSE, reason = sprintf("the revision introduced %s the draft did not have (%s)",
                                    if (length(added) == 1L) "a figure" else "figures",
                                    paste(added, collapse = ", ")))
}
