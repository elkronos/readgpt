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
  # `-` last, unescaped: "\\-" inside an R string literal is not a valid escape
  # and the file would not parse. A hyphen at the end of a bracket class is
  # literal, which is what is wanted.
  edge <- "[\"'\u2026.,;:[:space:]-]+"
  trimws(gsub(paste0("^", edge, "|", edge, "$"), "", x, perl = TRUE))
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
  skim = "extracted",
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
#'   they can be checked without it.
#' @return A data frame with one row per evidence span: `chunk_id`, `kind`
#'   (`"verbatim"`, `"extracted"`, `"answer"` or `"mixed"`), `verified`,
#'   `match` and `span` (the first 60 characters). `verified` is `NA` where the
#'   question does not apply -- a `map_reduce` evidence row is a per-chunk
#'   *answer*, not a quotation, and asking whether it appears in the chunk is a
#'   category error.
#'
#' @section What the numbers mean:
#' `match` is 1 for an exact quotation once whitespace, quote marks, dashes and
#' case are folded away -- the differences a faithful quotation introduces.
#' Below 1 it is the fraction of the span's words carried by its longest
#' consecutive run in the source, so 0.9 is a quotation with a word changed and
#' 0.1 is a sentence sharing vocabulary and nothing else.
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
#'   if (grepl("Extract", messages[[1]]$content, fixed = TRUE)) {
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
#'   if (grepl("Extract", messages[[1]]$content, fixed = TRUE)) {
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

  kind <- unname(.gr_evidence_kind[as_chr1(answer$reader, "")])
  if (is.na(kind)) kind <- "verbatim"

  sources <- if (!is.null(ev$source_text)) ev$source_text
             else if (inherits(chunks, "gr_chunks")) {
               chunks$chunks$text[match(ev$chunk_id, chunks$chunks$chunk_id)]
             } else NULL

  res <- if (identical(kind, "answer") || is.null(sources)) {
    data.frame(verified = rep(NA, nrow(ev)), match = rep(NA_real_, nrow(ev)))
  } else {
    verify_spans(ev$text, sources)
  }

  data.frame(chunk_id = ev$chunk_id, kind = kind,
             verified = res$verified, match = res$match,
             span = paste0(substr(ev$text, 1, 60),
                           ifelse(nchar(ev$text) > 60, "...", "")),
             stringsAsFactors = FALSE)
}
