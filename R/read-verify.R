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
  x <- to_utf8(as.character(x))
  # \u escapes, not literals: R CMD check flags non-ASCII bytes in R sources,
  # and a source file whose meaning depends on its own encoding is the bug this
  # package has already been bitten by twice.
  x <- gsub("[\u2018\u2019\u201a\u201b\u2032]", "'", x, perl = TRUE)
  x <- gsub("[\u201c\u201d\u201e\u201f\u2033]", '"', x, perl = TRUE)
  x <- gsub("[\u2010\u2011\u2012\u2013\u2014\u2015\u2212]", "-", x, perl = TRUE)
  x <- gsub("[\u00a0\u2007\u2009\u202f]", " ", x, perl = TRUE)
  x <- tolower(x)
  trimws(gsub("[[:space:]]+", " ", x, perl = TRUE))
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

#' The longest run of consecutive words from `span` that appears in `source`.
#'
#' Only reached when the span is not an exact substring, so the runs found here
#' are short and the loop is cheap. A run measure rather than a word-overlap one
#' because overlap cannot tell a quotation from a paraphrase built out of the
#' same vocabulary, and that is the distinction the whole check exists to make.
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
      if (!grepl(paste(span_words[i:j], collapse = " "), source_norm, fixed = TRUE)) break
      best <- max(best, j - i + 1L)
      j <- j + 1L
    }
  }
  best
}

#' Does one span appear in one source?
#'
#' @return `list(verified, match)`. `match` is 1 for an exact quotation and
#'   otherwise the fraction of the span's words carried by its longest
#'   consecutive run in the source -- so 0.9 is a quotation with a word changed,
#'   and 0.1 is a sentence that shares some vocabulary and nothing else.
#' @noRd
span_match <- function(span, source) {
  s <- trim_quote_edges(normalise_for_match(span))
  src <- normalise_for_match(source)
  if (!nzchar(s)) return(list(verified = NA, match = NA_real_))
  if (!nzchar(src)) return(list(verified = NA, match = NA_real_))
  if (grepl(s, src, fixed = TRUE)) return(list(verified = TRUE, match = 1))
  words <- strsplit(s, " ", fixed = TRUE)[[1]]
  words <- words[nzchar(words)]
  if (!length(words)) return(list(verified = NA, match = NA_real_))
  list(verified = FALSE,
       match = round(longest_quoted_run(words, src) / length(words), 3))
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

#' Chunk ids an answer claims to cite.
#'
#' Matches the `[chunk 3]` form the cited answer prompt asks for.
#' @noRd
cited_chunks <- function(text) {
  m <- gregexpr("\\[chunk[[:space:]]+([0-9]+)\\]", as_chr1(text), perl = TRUE,
                ignore.case = TRUE)
  hits <- regmatches(as_chr1(text), m)[[1]]
  if (!length(hits)) return(integer(0))
  unique(as.integer(gsub("[^0-9]", "", hits)))
}

#' What kind of thing a reader puts in `evidence$text`.
#' @noRd
.gr_evidence_kind <- c(
  stuff = "verbatim", retrieve = "verbatim", rerank = "verbatim",
  iterative = "verbatim", page = "verbatim",
  skim = "extracted", extract = "extracted", screen = "extracted",
  map_reduce = "answer", refine = "answer", hierarchical = "answer",
  ensemble = "mixed"
)

#' Check that quoted evidence really is in the document
#'
#' `ans$evidence` says what an answer rests on. For most readers those spans are
#' verbatim chunk text and are true by construction. For `skim` they are what
#' the model chose to write when asked to extract the relevant passages -- they
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
#'   span claims to come from. `skim` answers already carry their sources, so
#'   they can be checked without it -- and an `ensemble` needs it for the rows
#'   its verbatim members contributed, even though its `skim` rows do not.
#'
#'   Pass the chunks the answer was actually read from. Chunk ids are positional,
#'   so a *different* chunk set of the same size will match on id and compare
#'   each span against unrelated text, reporting `verified = FALSE` for evidence
#'   that is perfectly sound. An id the chunk set does not contain reports `NA`,
#'   because there was nothing to compare against.
#' @return A data frame with one row per evidence span: `chunk_id`, `kind`
#'   (`"verbatim"`, `"extracted"` or `"answer"`, **per row** -- an `ensemble`
#'   mixes them in one table, and the same value appears as the `kind` column on
#'   `ans$evidence` itself), `verified`,
#'   `match` and `span` (the first 60 characters). `verified` is `NA` where the
#'   question does not apply -- a `map_reduce` evidence row is a per-chunk
#'   *answer*, not a quotation, and asking whether it appears in the chunk is a
#'   category error.
#'
#' @section What the numbers mean:
#' `match` is 1 for an exact quotation once whitespace, quote marks, dashes and
#' case are folded away -- the differences a faithful quotation introduces.
#' Below 1 it is the fraction of the span's words carried by its longest
#' consecutive **run** in the source.
#'
#' Read that number with its shape in mind. Because it measures a run, *where*
#' the change falls matters as much as how much changed: altering the last word
#' of a ten-word span leaves a run of nine and scores 0.9, while altering a word
#' in the middle splits the span and scores about 0.5. So a mid-sentence change
#' -- a swapped figure, the case this exists to catch -- lands near 0.5, not
#' near 0.9. Below roughly 0.3 there is no quotation left at all, only shared
#' vocabulary. A run measure is still the right one: word overlap cannot tell a
#' quotation from a paraphrase assembled out of the same words.
#'
#' @section Citations:
#' With `cite = TRUE` a reader asks the model to mark its sources as
#' `[chunk 3]`. Every answer is checked for citations pointing at chunks that
#' were never sent, whatever this function is called with; the result is
#' `ans$notes$cited_unknown`, and an answer carrying one is `partial`.
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
#' # document -- which is exactly the case a reader cannot catch by eye.
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
    sources[gap] <- chunks$chunks$text[match(ev$chunk_id[gap], chunks$chunks$chunk_id)]
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
