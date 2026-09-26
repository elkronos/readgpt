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
#' @noRd
fold_for_match <- function(x) {
  # \u escapes, not literals: R CMD check flags non-ASCII bytes in R sources,
  # and a source file whose meaning depends on its own encoding is the bug this
  # package has already been bitten by twice.
  x <- gsub("[\u2018\u2019\u201a\u201b\u2032]", "'", x, perl = TRUE)
  x <- gsub("[\u201c\u201d\u201e\u201f\u2033]", '"', x, perl = TRUE)
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2015\u2212]", "-", x, perl = TRUE)
  x <- gsub("[\u00a0\u2007\u2009\u202f]", " ", x, perl = TRUE)
  tolower(x)
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

#' Does `s` occur in `src` as whole words and whole numbers?
#'
#' A raw substring test verified "5% of patients" against "25% of patients",
#' "12% year on year" against "-12%", and "enrolled 20." against "enrolled 200
#' patients": the figure changed and the check said exact. So an occurrence
#' counts only where it starts and ends on a boundary. Where the span starts
#' with a letter or digit the source must not continue one before it, nor carry
#' a minus sign or a decimal or thousands separator in front of a digit; where
#' it ends with one, the source must not carry on the word or the number after
#' it. Every occurrence is tried, not just the first.
#' @noRd
found_whole <- function(s, src) {
  if (!grepl(s, src, fixed = TRUE)) return(FALSE)
  n <- nchar(s)
  first <- substr(s, 1L, 1L)
  last <- substr(s, n, n)
  word <- function(ch) nzchar(ch) && grepl("[[:alnum:]]", ch)
  digit <- function(ch) nzchar(ch) && grepl("[0-9]", ch)
  check_start <- word(first)
  check_end <- word(last)
  if (!check_start && !check_end) return(TRUE)
  from <- 1L
  repeat {
    at <- regexpr(s, substring(src, from), fixed = TRUE)
    if (at < 0L) return(FALSE)
    at <- from + as.integer(at) - 1L
    b1 <- if (at > 1L) substr(src, at - 1L, at - 1L) else ""
    b2 <- if (at > 2L) substr(src, at - 2L, at - 2L) else ""
    a1 <- substr(src, at + n, at + n)
    a2 <- substr(src, at + n + 1L, at + n + 1L)
    ok_start <- !check_start ||
      !(word(b1) || (digit(first) && (b1 == "-" || (b1 %in% c(".", ",") && digit(b2)))))
    ok_end <- !check_end ||
      !(word(a1) || (digit(last) && a1 %in% c(".", ",") && digit(a2)))
    if (ok_start && ok_end) return(TRUE)
    from <- at + 1L
  }
}

#' The passages a model's quotation is made of, each normalised and trimmed.
#'
#' The extraction prompt asks for "the passages" verbatim, and models give
#' several: on separate lines, as a bulleted or numbered list, in separate
#' quote marks, or joined with an elision ("...", "[...]"). Checked as one
#' string, two verbatim passages that are not adjacent in the source failed,
#' and so did any bullet or bold marker, marking an entirely faithful
#' extraction partial and dropping correct values under `require_quote`. Each
#' passage is checked on its own instead; a passage with no letter or digit in
#' it carries nothing to check.
#' @noRd
quote_passages <- function(span) {
  x <- to_utf8(as_chr1(span, ""))
  if (!nzchar(x)) return(character(0))
  lines <- strsplit(x, "[\r\n]+", perl = TRUE)[[1]]
  # List markers: a bullet glyph, or a dash, star, plus or short number
  # followed by a space. "-5%" keeps its sign: a dash needs a space after it.
  lines <- sub("^[[:space:]]*[\u2022\u2023\u2043\u2219\u25aa\u25cf\u25e6][[:space:]]*", "",
               lines, perl = TRUE)
  lines <- sub("^[[:space:]]*(?:[-*+]|[0-9]{1,2}[.)]|\\([0-9a-z]\\))[[:space:]]+", "",
               lines, perl = TRUE)
  # Markdown bold is emphasis, not text.
  lines <- gsub("**", "", lines, fixed = TRUE)
  elision <- paste0("\\[[[:space:]]*(?:\\.{2,}|\u2026)[[:space:]]*\\]|",
                    "\\([[:space:]]*(?:\\.{2,}|\u2026)[[:space:]]*\\)|",
                    "\\.{3,}|\u2026|(?:\\.[[:space:]]){2,}\\.|",
                    # One quoted passage closing and the next opening.
                    "[\"\u201d][[:space:]]*[,;]?[[:space:]]+[\"\u201c]")
  pieces <- unlist(strsplit(lines, elision, perl = TRUE), use.names = FALSE)
  pieces <- vapply(pieces, function(p) trim_quote_edges(normalise_for_match(p)), character(1),
                   USE.NAMES = FALSE)
  pieces[nzchar(pieces) & grepl("[[:alnum:]]", pieces)]
}

#' The longest run of consecutive words from `span` that appears in `source`.
#'
#' Only reached when the span is not an exact quotation, so the runs found here
#' are short and the loop is cheap. A run measure rather than a word-overlap one
#' because overlap cannot tell a quotation from a paraphrase built out of the
#' same vocabulary, and that is the distinction the whole check exists to make.
#' A run counts only as whole words (found_whole()), so "5% of patients"
#' against "25% of patients" scores the run it really shares and not 1.
#' @noRd
longest_quoted_run <- function(span_words, source_norm) {
  n <- length(span_words)
  if (!n || !nzchar(source_norm)) return(0L)
  if (n > 300L) { span_words <- span_words[seq_len(300L)]; n <- 300L }
  best <- 0L
  for (i in seq_len(n)) {
    if (n - i + 1L <= best) break                 # cannot beat `best` from here
    j <- i
    while (j <= n) {
      if (!found_whole(paste(span_words[i:j], collapse = " "), source_norm)) break
      best <- max(best, j - i + 1L)
      j <- j + 1L
    }
  }
  best
}

#' Does one span appear in one source?
#'
#' The span is split into the passages it is made of (quote_passages()), and it
#' is verified when every passage appears in the source as whole words and
#' whole numbers (found_whole()).
#'
#' @return `list(verified, match)`. `match` is 1 for an exact quotation and
#'   otherwise, for the weakest passage, the fraction of its words carried by
#'   its longest consecutive run in the source -- so 0.9 is a quotation with a
#'   word changed, and 0.1 is a sentence that shares some vocabulary and
#'   nothing else.
#' @noRd
span_match <- function(span, source) {
  pieces <- quote_passages(span)
  src <- normalise_for_match(source)
  if (!length(pieces)) return(list(verified = NA, match = NA_real_))
  if (!nzchar(src)) return(list(verified = NA, match = NA_real_))
  found <- vapply(pieces, found_whole, logical(1), src = src, USE.NAMES = FALSE)
  if (all(found)) return(list(verified = TRUE, match = 1))
  score <- vapply(pieces[!found], function(s) {
    words <- strsplit(s, " ", fixed = TRUE)[[1]]
    words <- words[nzchar(words)]
    longest_quoted_run(words, src) / length(words)
  }, numeric(1), USE.NAMES = FALSE)
  list(verified = FALSE, match = round(min(score), 3))
}

#' @noRd
verify_spans <- function(spans, sources) {
  n <- length(spans)
  if (!n) return(data.frame(verified = logical(0), match = numeric(0)))
  sources <- rep(sources, length.out = n)
  out <- lapply(seq_len(n), function(i) span_match(spans[[i]], sources[[i]]))
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
#' nor "12%" in "-12%", nor "20" in "200". A span made of several passages (on
#' separate lines, as a list, or joined by "..." or "\[...\]") is checked passage
#' by passage and is verified when every passage is found.
#' Below 1 it is the fraction of the span's words carried by its longest
#' consecutive **run** in the source, for the passage that matches worst.
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
    got <- verify_spans(ev$text[checkable], sources[checkable])
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
