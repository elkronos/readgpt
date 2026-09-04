# segment-core.R -- AXIS 2 machinery: the chunk object and shared packing logic.
#
# WHY THIS FILE EXISTS
# The old repository had three chunkers (`chunk_text_naive`, `chunk_text_semantic`,
# `chunk_text_minimal`) with copy-pasted, subtly divergent logic and no shared
# guarantees:
#
#   * `chunk_text_semantic()` did `for (i in 2:length(paragraphs))`. With one
#     paragraph that is `c(2, 1)`, so `paragraphs[2]` is NA and the function died
#     with "missing value where TRUE/FALSE needed" -- on the DEFAULT code path,
#     because `answer_question()`'s default mode vector includes "Semantic".
#   * `chunk_text_semantic()` never enforced its own token limit: an oversized
#     paragraph was emitted whole, 50x over the cap.
#   * `chunk_text_naive()` returned `NULL` (not `character(0)`) when nothing
#     survived, and `parse_text()` cached and returned that NULL.
#   * `chunk_text_minimal()` split on the literal "\n\n" while the others split
#     on `\n{2,}`, so identical documents chunked differently depending on which
#     code path reached them.
#   * All three divided by a caller-supplied limit with no guard: a zero limit
#     produced `ceiling(n/0) = Inf` and a negative limit produced negative group
#     indices, which `split()` orders ascending -- feeding the document to the
#     model BACKWARDS in blocks.
#   * None supported overlap, none carried provenance, none reported anything.
#
# Everything here funnels through `new_chunks()`, which enforces the shape
# invariants once: non-empty, ordered, provenance-carrying and reportable. The
# TOKEN CAP is enforced separately, by `gr_segment()`, after the segmenter
# returns -- so a registered segmenter that ignores the cap is corrected rather
# than trusted.

#' Build a `gr_chunks`, the object every segmenter must return
#'
#' Exported because it is part of the extension API: [gr_segment()] rejects
#' anything that does not inherit `"gr_chunks"`, so a custom segmenter
#' registered with [gr_register_segmenter()] cannot be written without this.
#' Using it also gets you the shared invariants for free -- blank units dropped,
#' `chunk_id` assigned in order, tokens and characters measured, provenance
#' recycled to match -- so your segmenter behaves like the built-ins wherever
#' the rest of the package touches it.
#'
#' The token cap is NOT enforced here. [gr_segment()] checks it after your
#' function returns and re-splits anything oversized, so a segmenter that
#' ignores `spec$max_tokens` produces a warning and correct chunks rather than
#' an HTTP 400.
#'
#' @param text Character vector of chunk texts. Blank entries are dropped.
#' @param method Your segmenter's name. Record a fallback here if you took one
#'   (`"semantic->paragraph"`); [gr_chunk_stats()] surfaces it.
#' @param spec The `gr_segment_spec` passed to your segmenter.
#' @param page,section,block_id Provenance, one value per chunk or one recycled
#'   value. Leave as `NA` rather than guessing: wrong provenance sends a reader
#'   to the wrong page with full confidence.
#' @param extra Named list of segmenter-specific detail, kept on `$extra`.
#' @return A [gr_chunks].
#' @seealso [gr_register_segmenter()], [gr_chunks], [gr_segment()],
#'   [gr_chunk_stats()], [new_answer()]
#' @family segmentation functions
#' @export
#' @examples
#' # One chunk per bullet, with the source block recorded.
#' doc <- gr_ingest("Findings:\n\n- Revenue rose.\n- Costs fell.\n- Margin widened.")
#' ch <- new_chunks(trimws(strsplit(doc$text, "\n(?=-)", perl = TRUE)[[1]]),
#'                  method = "by_bullet", spec = gr_segment_spec(max_tokens = 100),
#'                  block_id = 1L)
#' gr_chunk_stats(ch)
new_chunks <- function(text, method, spec, page = NA_integer_, section = NA_character_,
                       block_id = NA_integer_, extra = list()) {
  text <- vapply(text %||% character(0), as_chr1, character(1), USE.NAMES = FALSE)
  keep <- has_content(text)
  text <- text[keep]
  rep_to <- function(v) {
    v <- if (length(v) == 1L) rep(v, length(keep)) else v
    if (length(v) != length(keep)) v <- rep(v, length.out = length(keep))
    v[keep]
  }
  n <- length(text)
  df <- data.frame(
    chunk_id = seq_len(n),
    text = text,
    tokens = if (n) gr_count_tokens(text) else integer(0),
    chars = if (n) nchar(text) else integer(0),
    page = rep_to(page),
    section = rep_to(section),
    block_id = rep_to(block_id),
    stringsAsFactors = FALSE
  )
  structure(list(chunks = df, method = method, spec = spec, extra = extra),
            class = "gr_chunks")
}

#' Greedy packing of units into chunks under a token cap, with optional overlap.
#'
#' Shared by every segmenter, so overlap, minimum size and the hard cap behave
#' identically no matter which segmentation strategy you pick.
#' @noRd
pack_units <- function(units, max_tokens, overlap_tokens = 0L, min_tokens = 0L,
                       joiner = "\n\n", meta = NULL, can_split = TRUE) {
  units <- vapply(units %||% character(0), as_chr1, character(1), USE.NAMES = FALSE)
  keep <- has_content(units)
  units <- units[keep]
  if (!is.null(meta)) meta <- meta[keep, , drop = FALSE]
  if (!length(units)) {
    return(list(text = character(0), meta = meta[0, , drop = FALSE]))
  }
  max_tokens <- as.integer(clamp(max_tokens, 16, Inf))
  overlap_tokens <- as.integer(clamp(overlap_tokens, 0, max_tokens - 1L))
  min_tokens <- as.integer(clamp(min_tokens, 0, max_tokens))

  # Any single unit that exceeds the cap is hard-split first, so the packer only
  # ever deals with units that can fit. `can_split = FALSE` (used by the `page`
  # segmenter, where a chunk boundary is semantically meaningful) keeps the unit
  # whole but reports the overflow instead of silently emitting it.
  exploded <- list(); emeta <- list()
  for (i in seq_along(units)) {
    tks <- gr_count_tokens(units[i])
    if (tks <= max_tokens || !can_split) {
      exploded[[length(exploded) + 1L]] <- units[i]
      emeta[[length(emeta) + 1L]] <- if (is.null(meta)) NULL else meta[i, , drop = FALSE]
    } else {
      parts <- hard_split(units[i], max_tokens)
      for (p in parts) {
        exploded[[length(exploded) + 1L]] <- p
        emeta[[length(emeta) + 1L]] <- if (is.null(meta)) NULL else meta[i, , drop = FALSE]
      }
    }
  }
  units <- unlist(exploded, use.names = FALSE)
  meta <- if (is.null(meta)) NULL else do.call(rbind, emeta)

  out <- character(0); out_meta <- list()
  buf <- character(0); buf_tokens <- 0L; buf_start <- 1L
  flush <- function(end_idx) {
    if (!length(buf)) return(invisible(NULL))
    out[[length(out) + 1L]] <<- paste(buf, collapse = joiner)
    # `lst[[k]] <- NULL` DELETES rather than appends, so with no meta the list
    # never grew and the runt-merge loop below indexed past its end. Store a
    # placeholder so positions stay aligned with `out`.
    out_meta[[length(out) ]] <<- if (is.null(meta)) NA else
      meta_over(meta, buf_start, max(end_idx, buf_start))
    invisible(NULL)
  }
  i <- 1L
  while (i <= length(units)) {
    tks <- gr_count_tokens(units[i])
    if (length(buf) && buf_tokens + tks > max_tokens) {
      flush(i - 1L)
      # Carry the tail of the finished chunk forward as overlap context.
      if (overlap_tokens > 0L) {
        # Trim the carried-over tail so tail + the incoming unit still fits.
        # Without this the chunk could reach 2 * max_tokens - 1, and the
        # cap-enforcement pass in gr_segment() would then re-cut it on token
        # boundaries -- shredding the very overlap this is here to create.
        room <- max(max_tokens - tks, 0L)
        want <- min(overlap_tokens, room)
        tail_txt <- if (want > 0L) tail_by_tokens(paste(buf, collapse = joiner), want) else ""
        buf <- if (nzchar(tail_txt)) tail_txt else character(0)
        buf_tokens <- if (length(buf)) gr_count_tokens(paste(buf, collapse = joiner)) else 0L
      } else {
        buf <- character(0); buf_tokens <- 0L
      }
      buf_start <- i
    }
    if (!length(buf)) buf_start <- i
    buf <- c(buf, units[i]); buf_tokens <- buf_tokens + tks
    i <- i + 1L
  }
  flush(length(units))

  # Merge runt chunks forward so a stray one-line paragraph does not become its
  # own API call.
  if (min_tokens > 0L && length(out) > 1L) {
    merged <- character(0); mmeta <- list()
    for (j in seq_along(out)) {
      tks <- gr_count_tokens(out[[j]])
      # Measure the JOINED text, not the sum of the two counts. Every estimate
      # carries a fixed per-call allowance, so adding two counts double-counts
      # it and reported a merge as over-cap when the merged text fit -- runts
      # that could have been absorbed became their own billed API call.
      cand <- if (length(merged)) paste(merged[[length(merged)]], out[[j]], sep = joiner) else ""
      if (length(merged) && tks < min_tokens && gr_count_tokens(cand) <= max_tokens) {
        merged[[length(merged)]] <- cand
        # The absorbed runt's provenance has to be folded in too. Keeping only
        # the host chunk's meta made the merged chunk claim a page it now only
        # partly comes from -- the same lie, arrived at from the other side.
        mmeta[[length(merged)]] <- combine_meta(mmeta[[length(merged)]],
                                                if (j <= length(out_meta)) out_meta[[j]] else NA)
      } else {
        merged[[length(merged) + 1L]] <- out[[j]]
        mmeta[[length(merged)]] <- if (j <= length(out_meta)) out_meta[[j]] else NA
      }
    }
    out <- merged; out_meta <- mmeta
  }
  out <- unlist(out, use.names = FALSE) %||% character(0)
  keep_meta <- Filter(function(m) is.data.frame(m), out_meta)
  meta_out <- if (is.null(meta) || !length(keep_meta)) NULL else do.call(rbind, keep_meta)
  # Provenance is worse than useless when it is off by one: a chunk that claims
  # to come from page 4 when it came from page 7 sends the reader to the wrong
  # place with full confidence. If the rows and the chunks ever disagree, drop
  # the provenance rather than emit a plausible-looking lie.
  if (!is.null(meta_out) && nrow(meta_out) != length(out)) meta_out <- NULL
  list(text = out, meta = meta_out)
}

#' Provenance for a chunk built from units `from:to`.
#'
#' A chunk that packs three paragraphs from two pages used to report the FIRST
#' one's page, which is right for the opening sentence and wrong for everything
#' after it -- and wrong in the most expensive way, because a citation that names
#' a specific page is checked by turning to that page. Where the units agree the
#' value stands; where they do not, the honest answer is that this chunk is not
#' from one page, and `NA` says so.
#'
#' The precise answer -- which page a particular QUOTE is on -- needs the quote,
#' and is resolved later by `resolve_evidence_pages()`. This is the fallback for
#' everything that has no quote to work from.
#' @noRd
meta_over <- function(meta, from, to) {
  row <- meta[from, , drop = FALSE]
  if (to <= from) return(row)
  span <- meta[from:to, , drop = FALSE]
  for (nm in names(row)) {
    v <- span[[nm]]
    u <- unique(v[!is.na(v)])
    if (length(u) > 1L) row[[nm]] <- NA
  }
  row
}

#' @noRd
combine_meta <- function(a, b) {
  if (!is.data.frame(a)) return(if (is.data.frame(b)) b else a)
  if (!is.data.frame(b)) return(a)
  for (nm in intersect(names(a), names(b))) {
    if (!identical(a[[nm]], b[[nm]])) a[[nm]] <- NA
  }
  a
}

#' Split one oversized unit at the best available boundary.
#'
#' Tries sentences, then words, then characters. The character level is what
#' makes the cap enforceable for text with no usable whitespace. Never divides by
#' a non-positive number, and always makes progress, so it cannot loop forever or
#' reverse the text.
#' @noRd
hard_split <- function(text, max_tokens) {
  text <- as_chr1(text)
  max_tokens <- as.integer(clamp(max_tokens, 16, Inf))
  if (gr_count_tokens(text) <= max_tokens) return(text)

  emit <- function(units, joiner) {
    out <- character(0); buf <- character(0); tks <- 0L
    for (u in units) {
      ut <- gr_count_tokens(u)
      if (length(buf) && tks + ut > max_tokens) {
        out <- c(out, paste(buf, collapse = joiner)); buf <- character(0); tks <- 0L
      }
      if (ut > max_tokens) {
        # A single unit still too large. Word granularity first; if the unit has
        # no usable whitespace (base64, a data URI, a long URL, a CJK run, a
        # minified line) fall through to CHARACTERS. Without that last resort the
        # token cap was unenforceable: hard_split returned the oversized text
        # unchanged and gr_segment's "enforcement" pass re-ran the same function
        # and got the same result back.
        if (length(buf)) { out <- c(out, paste(buf, collapse = joiner)); buf <- character(0); tks <- 0L }
        w <- words_of(u)
        if (length(w) > 1L) {
          out <- c(out, split_by_budget(w, " ", max_tokens))
        } else {
          out <- c(out, split_by_budget(strsplit(u, "", fixed = TRUE)[[1]], "", max_tokens))
        }
        next
      }
      buf <- c(buf, u); tks <- tks + ut
    }
    if (length(buf)) out <- c(out, paste(buf, collapse = joiner))
    out[has_content(out)]
  }

  s <- sentences_of(text)
  if (length(s) > 1L) return(emit(s, " "))
  emit(words_of(text), " ")
}

#' Greedily pack atomic units into groups that each fit the token budget.
#'
#' Unlike the fixed-stride slicing it replaces, this re-measures after every
#' addition, so a group can never exceed `max_tokens` however the units
#' tokenize. A single unit that still does not fit is emitted alone -- at
#' character granularity that means one character, which always fits.
#' @noRd
split_by_budget <- function(units, joiner, max_tokens) {
  units <- units[nzchar(units)]
  if (!length(units)) return(character(0))
  out <- character(0); buf <- character(0)
  for (u in units) {
    cand <- c(buf, u)
    if (length(buf) && gr_count_tokens(paste(cand, collapse = joiner)) > max_tokens) {
      out <- c(out, paste(buf, collapse = joiner))
      buf <- u
    } else {
      buf <- cand
    }
  }
  if (length(buf)) out <- c(out, paste(buf, collapse = joiner))
  out[nzchar(out)]
}

#' Take the last `n` tokens of a string, snapped to a sentence boundary when one
#' is close by (so overlap does not begin mid-sentence).
#' @noRd
tail_by_tokens <- function(text, n) {
  text <- as_chr1(text)
  n <- as.integer(clamp(n, 0, Inf))
  if (n <= 0L || !nzchar(text)) return("")
  if (gr_count_tokens(text) <= n) return(text)
  s <- sentences_of(text)
  if (length(s) > 1L) {
    acc <- character(0)
    for (k in rev(seq_along(s))) {
      cand <- c(s[k], acc)
      if (gr_count_tokens(paste(cand, collapse = " ")) > n) break
      acc <- cand
    }
    if (length(acc)) return(paste(acc, collapse = " "))
  }
  w <- words_of(text)
  lo <- 0L; hi <- length(w)
  while (lo < hi) {
    mid <- as.integer((lo + hi + 1L) %/% 2L)
    if (gr_count_tokens(paste(utils::tail(w, mid), collapse = " ")) <= n) lo <- mid else hi <- mid - 1L
  }
  if (lo == 0L) "" else paste(utils::tail(w, lo), collapse = " ")
}

#' @export
print.gr_chunks <- function(x, ...) {
  d <- x$chunks
  cat(sprintf("<gr_chunks> method=%s  n=%d\n", x$method, nrow(d)))
  if (nrow(d)) {
    cat(sprintf("  tokens: min %d / median %d / mean %.0f / max %d / total %d\n",
                min(d$tokens), stats::median(d$tokens), mean(d$tokens),
                max(d$tokens), sum(d$tokens)))
    cap <- x$spec$max_tokens %||% NA
    if (!is.na(cap)) cat(sprintf("  cap=%d  over-cap chunks: %d\n", cap, sum(d$tokens > cap)))
    if (!all(is.na(d$section))) {
      cat(sprintf("  sections: %d distinct\n", length(unique(stats::na.omit(d$section)))))
    }
    cat(sprintf("  first: %s\n", substr(d$text[1], 1, 100)))
  }
  invisible(x)
}

#' Summary statistics for a chunk set
#'
#' Useful for comparing segmentation strategies before spending any API budget.
#'
#' @param chunks A [gr_chunks] object.
#' @return A one-row data frame: `method`, `n`, `total_tokens`, `min`, `median`,
#'   `mean`, `max`, `over_cap`. `method` reports any fallback that occurred.
#'   `total_tokens` exceeds the document's own token count when overlap is on --
#'   that difference is the duplication overlap buys you.
#' @seealso [gr_segment()], [gr_segmenters()]
#' @family segmentation functions
#' @export
#' @examples
#' doc <- gr_ingest(readgpt_example())
#'
#' # What overlap actually costs, before any model call.
#' do.call(rbind, lapply(c(0, 30, 60), function(ov)
#'   gr_chunk_stats(gr_segment(doc, list(method = "sentence", max_tokens = 120,
#'                                       overlap_tokens = ov)))))
gr_chunk_stats <- function(chunks) {
  stopifnot(inherits(chunks, "gr_chunks"))
  d <- chunks$chunks
  if (!nrow(d)) {
    return(data.frame(method = chunks$method, n = 0L, total_tokens = 0L,
                      min = NA_integer_, median = NA_real_, mean = NA_real_,
                      max = NA_integer_, over_cap = 0L, stringsAsFactors = FALSE))
  }
  cap <- chunks$spec$max_tokens %||% Inf
  data.frame(method = chunks$method, n = nrow(d), total_tokens = sum(d$tokens),
             min = min(d$tokens), median = stats::median(d$tokens),
             mean = round(mean(d$tokens), 1), max = max(d$tokens),
             over_cap = sum(d$tokens > cap), stringsAsFactors = FALSE)
}

#' @export
as_json.gr_chunks <- function(x, pretty = TRUE, ...) {
  as_json.default(list(method = x$method, spec = unclass(x$spec),
                       stats = as.list(gr_chunk_stats(x)), chunks = x$chunks),
                  pretty = pretty, ...)
}
