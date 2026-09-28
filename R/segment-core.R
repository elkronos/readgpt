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
#' Using it also gets you the shared invariants for free (blank units dropped,
#' `chunk_id` assigned in order, tokens and characters measured, provenance
#' recycled to match), so your segmenter behaves like the built-ins wherever
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
#' @param source_text For a segmenter that puts text a model wrote into a
#'   chunk (a context line, a rewrite): the document text each chunk was made
#'   from, one value per chunk or one recycled value. It becomes the
#'   `source_text` column, which is what quotes from the chunk are checked
#'   against. `NA`, or leaving it out, says the chunk's `text` is the
#'   document's own.
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
                       block_id = NA_integer_, extra = list(), source_text = NULL) {
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
  # Only when given, so a chunk set whose text is all the document's own looks
  # as it always has. as.character(): an all-NA vector is logical.
  if (!is.null(source_text)) df$source_text <- as.character(rep_to(source_text))
  structure(list(chunks = df, method = method, spec = spec, extra = extra),
            class = "gr_chunks")
}

#' Greedy packing of units into chunks under a token cap, with optional overlap.
#'
#' Shared by every segmenter, so overlap, minimum size and the hard cap behave
#' identically no matter which segmentation strategy you pick.
#'
#' Besides the text and its provenance, `span` gives, for each chunk, the first
#' and last of the caller's `units` it was packed from (overlap carried in from
#' the chunk before is not counted), for a segmenter that has to say where a
#' chunk came from in terms the provenance columns cannot hold. The provenance
#' itself does count the overlap: a chunk that opens with a sentence carried
#' from page 3 is not wholly from page 4.
#' @noRd
pack_units <- function(units, max_tokens, overlap_tokens = 0L, min_tokens = 0L,
                       joiner = "\n\n", meta = NULL, can_split = TRUE) {
  units <- vapply(units %||% character(0), as_chr1, character(1), USE.NAMES = FALSE)
  keep <- has_content(units)
  units <- units[keep]
  origin <- which(keep)
  if (!is.null(meta)) meta <- meta[keep, , drop = FALSE]
  if (!length(units)) {
    return(list(text = character(0), meta = meta[0, , drop = FALSE],
                span = data.frame(first = integer(0), last = integer(0))))
  }
  max_tokens <- as.integer(clamp(max_tokens, 16, Inf))
  overlap_tokens <- as.integer(clamp(overlap_tokens, 0, max_tokens - 1L))
  min_tokens <- as.integer(clamp(min_tokens, 0, max_tokens))

  # Any single unit that exceeds the cap is hard-split first, so the packer only
  # ever deals with units that can fit. `can_split = FALSE` (used by the `page`
  # segmenter, where a chunk boundary is semantically meaningful) keeps the unit
  # whole but reports the overflow instead of silently emitting it.
  exploded <- list(); emeta <- list(); eorigin <- integer(0)
  utk <- gr_count_tokens(units)
  for (i in seq_along(units)) {
    tks <- utk[i]
    if (tks <= max_tokens || !can_split) {
      exploded[[length(exploded) + 1L]] <- units[i]
      emeta[[length(emeta) + 1L]] <- if (is.null(meta)) NULL else meta[i, , drop = FALSE]
      eorigin <- c(eorigin, origin[i])
    } else {
      parts <- hard_split(units[i], max_tokens)
      for (p in parts) {
        exploded[[length(exploded) + 1L]] <- p
        emeta[[length(emeta) + 1L]] <- if (is.null(meta)) NULL else meta[i, , drop = FALSE]
        eorigin <- c(eorigin, origin[i])
      }
    }
  }
  units <- unlist(exploded, use.names = FALSE)
  origin <- eorigin
  meta <- if (is.null(meta)) NULL else do.call(rbind, emeta)
  utk <- gr_count_tokens(units)

  out <- character(0); own <- character(0); out_meta <- list(); out_span <- list()
  buf <- character(0); buf_tokens <- 0L; buf_start <- 1L
  # `from[k]` is the earliest unit `buf[k]` holds text of. `carried` says that
  # buf[1] is the overlap tail copied from the chunk before, which is not this
  # chunk's own text: a runt merged backward takes only its own text (`own`),
  # because its host already ends with that tail.
  from <- integer(0); carried <- FALSE
  flush <- function(end_idx) {
    if (!length(buf)) return(invisible(NULL))
    k <- length(out) + 1L
    end <- max(end_idx, buf_start)
    out[[k]] <<- paste(buf, collapse = joiner)
    own[[k]] <<- paste(if (carried) buf[-1L] else buf, collapse = joiner)
    # `lst[[k]] <- NULL` DELETES rather than appends, so with no meta the list
    # never grew and the runt-merge loop below indexed past its end. Store a
    # placeholder so positions stay aligned with `out`. The provenance runs
    # from the first unit the carried tail came from, not from the first new
    # unit: leaving the tail out made a chunk that opens on page 3 and runs on
    # to page 4 claim page 4, where meta_over() says NA.
    out_meta[[k]] <<- if (is.null(meta)) NA else meta_over(meta, from[1L], end)
    out_span[[k]] <<- origin[c(buf_start, end)]
    invisible(NULL)
  }
  # The first unit the tail of the buffer reaches back into. A tail is the
  # buffer's last words (tail_by_tokens() cuts at sentences or words, or for
  # text written without spaces inside the last one), and the joiner is
  # whitespace whenever there is overlap, so counting words back from the end
  # finds where it starts.
  tail_from <- function(b, f, tail_txt) {
    need <- length(words_of(tail_txt))
    k <- length(b); have <- length(words_of(b[k]))
    while (k > 1L && have < need) { k <- k - 1L; have <- have + length(words_of(b[k])) }
    f[k]
  }
  i <- 1L
  repeat {
    done <- i > length(units)
    if (length(buf) && (done || buf_tokens + utk[i] > max_tokens)) {
      # The running sum bounds the joined text's count only for a tokenizer
      # that never counts two texts joined as more than the two apart. 'chars'
      # does (it counts the joiner), and so may a custom one; a chunk packed on
      # the sum then went over the cap and was re-cut by gr_segment(), losing
      # its overlap. Measure the chunk once as it is finished and hand back
      # trailing units until it fits. For the default tokenizer this never
      # fires, and it costs one count per chunk.
      while (length(buf) > carried + 1L &&
             gr_count_tokens(paste(buf, collapse = joiner)) > max_tokens) {
        buf <- buf[-length(buf)]; from <- from[-length(from)]
        i <- i - 1L; done <- FALSE
      }
      flush(i - 1L)
      if (done) break
      tks <- utk[i]
      buf_prev <- buf; from_prev <- from
      buf <- character(0); from <- integer(0); carried <- FALSE; buf_tokens <- 0L
      # Carry the tail of the finished chunk forward as overlap context.
      if (overlap_tokens > 0L) {
        # Trim the carried-over tail so tail + the incoming unit still fits.
        # Without this the chunk could reach 2 * max_tokens - 1, and the
        # cap-enforcement pass in gr_segment() would then re-cut it on token
        # boundaries -- shredding the very overlap this is here to create.
        # Measured joined to the unit, for the same reason as above.
        joined <- paste(buf_prev, collapse = joiner)
        want <- min(overlap_tokens, max(max_tokens - tks, 0L))
        tail_txt <- if (want > 0L) tail_by_tokens(joined, want) else ""
        while (nzchar(tail_txt)) {
          excess <- gr_count_tokens(paste(tail_txt, units[i], sep = joiner)) - max_tokens
          if (excess <= 0L) break
          want <- want - excess
          tail_txt <- if (want > 0L) tail_by_tokens(joined, want) else ""
        }
        if (nzchar(tail_txt)) {
          buf <- tail_txt; from <- tail_from(buf_prev, from_prev, tail_txt); carried <- TRUE
          buf_tokens <- gr_count_tokens(tail_txt)
        }
      }
      buf_start <- i
    }
    if (done) break
    if (!length(buf)) buf_start <- i
    buf <- c(buf, units[i]); from <- c(from, i); buf_tokens <- buf_tokens + utk[i]
    i <- i + 1L
  }

  # Merge runt chunks forward so a stray one-line paragraph does not become its
  # own API call.
  if (min_tokens > 0L && length(out) > 1L) {
    merged <- character(0); mmeta <- list(); mspan <- list()
    for (j in seq_along(out)) {
      tks <- gr_count_tokens(out[[j]])
      # Measure the JOINED text, not the sum of the two counts. Every estimate
      # carries a fixed per-call allowance, so adding two counts double-counts
      # it and reported a merge as over-cap when the merged text fit -- runts
      # that could have been absorbed became their own billed API call.
      # Only the runt's OWN text joins its host: it opens with an overlap tail
      # copied from the end of the host, and merging that too put the same
      # sentences twice in a row inside one chunk -- counted twice by any
      # reader that tallies or lists what it finds.
      cand <- if (length(merged)) paste(merged[[length(merged)]], own[[j]], sep = joiner) else ""
      if (length(merged) && tks < min_tokens && gr_count_tokens(cand) <= max_tokens) {
        merged[[length(merged)]] <- cand
        # The absorbed runt's provenance has to be folded in too. Keeping only
        # the host chunk's meta made the merged chunk claim a page it now only
        # partly comes from -- the same lie, arrived at from the other side.
        mmeta[[length(merged)]] <- combine_meta(mmeta[[length(merged)]],
                                                if (j <= length(out_meta)) out_meta[[j]] else NA)
        mspan[[length(merged)]] <- c(mspan[[length(merged)]][1], out_span[[j]][2])
      } else {
        merged[[length(merged) + 1L]] <- out[[j]]
        mmeta[[length(merged)]] <- if (j <= length(out_meta)) out_meta[[j]] else NA
        mspan[[length(merged)]] <- out_span[[j]]
      }
    }
    out <- merged; out_meta <- mmeta; out_span <- mspan
  }
  out <- unlist(out, use.names = FALSE) %||% character(0)
  keep_meta <- Filter(function(m) is.data.frame(m), out_meta)
  meta_out <- if (is.null(meta) || !length(keep_meta)) NULL else do.call(rbind, keep_meta)
  # Provenance is worse than useless when it is off by one: a chunk that claims
  # to come from page 4 when it came from page 7 sends the reader to the wrong
  # place with full confidence. If the rows and the chunks ever disagree, drop
  # the provenance rather than emit a plausible-looking lie.
  if (!is.null(meta_out) && nrow(meta_out) != length(out)) meta_out <- NULL
  span <- data.frame(first = vapply(out_span, `[`, integer(1), 1L),
                     last = vapply(out_span, `[`, integer(1), 2L))
  list(text = out, meta = meta_out, span = span)
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
#'
#' Every piece fits the cap as measured. A word still over the cap on its own
#' (a base64 blob, a long URL) goes down to characters; the old version gave it
#' back alone and whole whenever its sentence had other words. And a piece
#' packed on the sum of its parts' counts is checked as joined: the sum bounds
#' the joined count only for a tokenizer that never counts two texts joined as
#' more than the two apart, and 'chars' counts the joining spaces. Either way
#' gr_segment() ran this once and passed what came back, over the cap.
#' @noRd
hard_split <- function(text, max_tokens) {
  text <- as_chr1(text)
  max_tokens <- as.integer(clamp(max_tokens, 16, Inf))
  if (gr_count_tokens(text) <= max_tokens) return(text)

  # A unit still too large on its own. Word granularity first; if the unit has
  # no usable whitespace (base64, a data URI, a long URL, a CJK run, a
  # minified line) fall through to CHARACTERS. Without that last resort the
  # token cap was unenforceable: hard_split returned the oversized text
  # unchanged and gr_segment's "enforcement" pass re-ran the same function
  # and got the same result back.
  split_one <- function(u) {
    w <- words_of(u)
    if (length(w) <= 1L) return(split_by_budget(strsplit(u, "", fixed = TRUE)[[1]], "", max_tokens))
    pieces <- split_by_budget(w, " ", max_tokens)
    big <- gr_count_tokens(pieces) > max_tokens
    if (!any(big)) return(pieces)
    unlist(lapply(seq_along(pieces), function(k) if (big[k]) split_one(pieces[k]) else pieces[k]),
           use.names = FALSE)
  }
  emit <- function(units, joiner) {
    ut <- gr_count_tokens(units)
    out <- list(); buf <- integer(0); tks <- 0L
    joined <- function(ix) paste(units[ix], collapse = joiner)
    # Emit the buffer, or when it measures over the cap joined, the longest
    # leading run of it that fits; the rest stays buffered for the next piece,
    # so a tokenizer that counts joiners costs a unit per piece, not a runt.
    close_some <- function() {
      piece <- joined(buf)
      n <- if (gr_count_tokens(piece) <= max_tokens) length(buf) else
        max(longest_fit(length(buf), function(m)
          gr_count_tokens(joined(buf[seq_len(m)])) <= max_tokens), 1L)
      out[[length(out) + 1L]] <<- if (n == length(buf)) piece else joined(buf[seq_len(n)])
      buf <<- buf[-seq_len(n)]; tks <<- sum(ut[buf])
      invisible(NULL)
    }
    for (k in seq_along(units)) {
      while (length(buf) && tks + ut[k] > max_tokens) close_some()
      if (ut[k] > max_tokens) {
        out[[length(out) + 1L]] <- split_one(units[k])
        next
      }
      buf <- c(buf, k); tks <- tks + ut[k]
    }
    while (length(buf)) close_some()
    out <- unlist(out, use.names = FALSE)
    out[has_content(out)]
  }

  s <- sentences_of(text)
  if (length(s) > 1L) return(emit(s, " "))
  emit(words_of(text), " ")
}

#' The largest `k` in `1:n` for which `ok(k)` holds, where `ok` holds up to
#' some point and fails after it (a longer run of text counts more tokens).
#'
#' Doubles `k` until `ok` fails, then bisects, so it costs O(log k) calls
#' rather than the k of growing a run one unit at a time -- which, re-counting
#' the whole run at each step, was quadratic. The answer always passed `ok()`
#' itself, so a tokenizer that is not quite monotone can cost length, never
#' the cap. 0 when `ok(1)` fails.
#' @noRd
longest_fit <- function(n, ok) {
  n <- as.integer(n)
  if (is.na(n) || n < 1L || !ok(1L)) return(0L)
  lo <- 1L; hi <- 2L
  while (hi <= n && ok(hi)) { lo <- hi; hi <- hi * 2L }
  hi <- min(hi - 1L, n)
  while (lo < hi) {
    mid <- (lo + hi + 1L) %/% 2L
    if (ok(mid)) lo <- mid else hi <- mid - 1L
  }
  lo
}

#' Greedily pack atomic units into groups that each fit the token budget.
#'
#' Unlike the fixed-stride slicing it replaces, this measures each group as
#' joined, so a group can never exceed `max_tokens` however the units
#' tokenize. A single unit that still does not fit is emitted alone -- at
#' character granularity that means one character, which always fits. Each
#' group is found by longest_fit(), not by re-measuring after every unit, so a
#' blob split into characters costs O(n log n), not O(n^2).
#' @noRd
split_by_budget <- function(units, joiner, max_tokens) {
  units <- units[nzchar(units)]
  if (!length(units)) return(character(0))
  out <- list(); pos <- 1L; n <- length(units)
  while (pos <= n) {
    k <- longest_fit(n - pos + 1L, function(m)
      gr_count_tokens(paste(units[pos:(pos + m - 1L)], collapse = joiner)) <= max_tokens)
    k <- max(k, 1L)
    out[[length(out) + 1L]] <- paste(units[pos:(pos + k - 1L)], collapse = joiner)
    pos <- pos + k
  }
  out <- unlist(out, use.names = FALSE)
  out[nzchar(out)]
}

#' Scripts written without spaces between words. A paragraph of them is one
#' "word" to a whitespace split.
#' @noRd
.gr_unspaced_script <- "[\\p{Han}\\p{Hiragana}\\p{Katakana}\\p{Thai}\\p{Lao}\\p{Khmer}\\p{Myanmar}]"

#' Take the last `n` tokens of a string, snapped to a sentence boundary when one
#' is close by (so overlap does not begin mid-sentence).
#'
#' Text written without spaces (Chinese, Japanese, Thai) is cut inside its last
#' "word" when not even that fits, as gr_truncate_tokens() does. The word-level
#' search found no word that fitted and returned "", so overlap silently did
#' nothing for those scripts. Latin text keeps whole words: a word longer than
#' the whole overlap is carried as nothing rather than as a fragment.
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
  lo <- longest_fit(length(w), function(m)
    gr_count_tokens(paste(utils::tail(w, m), collapse = " ")) <= n)
  if (lo > 0L) return(paste(utils::tail(w, lo), collapse = " "))
  last <- mark_utf8(w[length(w)])
  if (length(w) > 1L && !grepl(.gr_unspaced_script, last, perl = TRUE)) return("")
  ch <- strsplit(last, "", fixed = TRUE)[[1]]
  k <- longest_fit(length(ch), function(m)
    gr_count_tokens(paste(utils::tail(ch, m), collapse = "")) <= n)
  if (k == 0L) "" else paste(utils::tail(ch, k), collapse = "")
}

#' @export
print.gr_chunks <- function(x, ...) {
  d <- x$chunks
  cat(sprintf("<gr_chunks> method=%s  n=%d\n", x$method, nrow(d)))
  if (nrow(d)) {
    # `%s` and format(): median() averages the two middle values on an
    # even-length vector, so it returns a DOUBLE, and sprintf("%d", 27.5) is an
    # error rather than a coercion. gr_segment() auto-prints at top level, so
    # the canonical interactive call failed on any document whose two middle
    # chunks had token counts of different parity -- about one in four.
    cat(sprintf("  tokens: min %d / median %s / mean %.0f / max %d / total %d\n",
                min(d$tokens), format(stats::median(d$tokens)), mean(d$tokens),
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
#'   `total_tokens` exceeds the document's own token count when overlap is on;
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
