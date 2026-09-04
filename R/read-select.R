# read-select.R -- which chunks go in the prompt, and in what order.
#
# WHY THIS FILE EXISTS
# Top-k by cosine similarity answers "which chunks are most like the question",
# which is not quite the question you wanted answered. If three chunks say the
# same thing, all three score highly, all three go in the prompt, and you pay
# for the second and third to tell the model something it has already been told.
# The budget that could have held a fourth, different chunk holds a duplicate.
#
# Maximal marginal relevance is the standard fix: pick greedily, and at each
# step trade a chunk's relevance against how much it duplicates what is already
# selected. It costs nothing -- the vectors are already computed -- and it is off
# by default, because changing what goes in the prompt changes answers and that
# should be a decision, not a surprise.
#
# Ordering is a separate question with a separate, better-evidenced reason.
# Transformers attend measurably better to the beginning and end of a long
# context than to its middle -- the "lost in the middle" effect. That is not a
# claim about which chunks to choose; it is a claim about where to put the ones
# you chose. Note that this is the opposite of the intuition it resembles: the
# primacy and recency effects in human memory come from rehearsal and
# interference, mechanisms a transformer does not have. The reason to care here
# is positional attention, and it applies to placement, not selection.

#' Greedy maximal marginal relevance.
#'
#' At each step pick the candidate maximising
#' `lambda * relevance - (1 - lambda) * max similarity to anything already picked`.
#' `lambda = 1` is exactly top-k, which is why it is the default everywhere.
#'
#' Similarity is computed against the last selection only and folded into a
#' running maximum, so this is O(n*k) rather than the O(n^2) an explicit
#' chunk-by-chunk similarity matrix would cost. On a thousand chunks that is the
#' difference between a few thousand multiplications and eight megabytes of
#' matrix nobody reads twice.
#'
#' @param rel Numeric relevance per candidate. `-Inf` marks a candidate that
#'   must not be selected at all.
#' @param emb Row-per-candidate embedding matrix, L2-normalised, so a
#'   cross-product is cosine similarity.
#' @param k How many to select.
#' @param lambda 1 = pure relevance, 0 = pure diversity.
#' @return Selected indices, in selection order (most relevant first).
#' @noRd
mmr_select <- function(rel, emb, k, lambda = 1) {
  n <- length(rel)
  k <- as.integer(max(0L, min(as.integer(k), n)))
  if (k == 0L) return(integer(0))
  eligible <- which(is.finite(rel))
  if (!length(eligible)) return(integer(0))
  k <- min(k, length(eligible))

  # lambda >= 1, or nothing to measure redundancy with, is plain top-k. Taking
  # this path rather than running the loop with a zero diversity weight keeps the
  # default bit-for-bit identical to the behaviour it replaced.
  if (!is.finite(lambda) || lambda >= 1 || !is.matrix(emb) || nrow(emb) != n || ncol(emb) < 1L) {
    return(eligible[order(rel[eligible], decreasing = TRUE)][seq_len(k)])
  }

  # A non-finite row poisons everything downstream of it: one NaN reaches every
  # candidate through `pmax()`, every score becomes NaN, `which.max()` returns
  # integer(0), `sel` stops growing and the `while` NEVER TERMINATES. Not an
  # error and not a crash -- a hang, which is the worst failure a library can
  # hand you. A zero vector L2-normalised is 0/0, so any chunk an embedder finds
  # no words in produces one.
  #
  # Neutralise the ROW, do not drop the CANDIDATE. `cosine_against()` already
  # maps such a row to a relevance of 0, so the chunk is a legitimate if
  # unpromising choice and top-k will happily take it. Dropping it here instead
  # meant `mmr < 1` quietly returned fewer chunks than `top_k` asked for while
  # `mmr = 1` returned the full set -- a diversity setting silently shrinking
  # the evidence. A zero vector contributes zero similarity, which is the right
  # answer for "we cannot tell how redundant this is".
  bad_row <- !is.finite(rowSums(emb))
  if (any(bad_row)) emb[bad_row, ] <- 0

  sel <- eligible[which.max(rel[eligible])]
  best_sim <- rep(-Inf, n)
  while (length(sel) < k) {
    s <- as.numeric(emb %*% emb[sel[length(sel)], ])
    best_sim <- pmax(best_sim, s)
    cand <- setdiff(eligible, sel)
    if (!length(cand)) break
    score <- lambda * rel[cand] - (1 - lambda) * best_sim[cand]
    nxt <- cand[which.max(score)]
    # Belt as well as braces: which.max() on an all-NA vector is integer(0), and
    # appending nothing would leave the loop condition unchanged forever.
    if (!length(nxt)) break
    sel <- c(sel, nxt)
  }
  sel
}

#' Arrange already-selected chunks for the prompt.
#'
#' @param idx Selected row indices, most relevant first.
#' @param how `"relevance"` (unchanged), `"document"` (the order they appear in
#'   the document), or `"edges"`.
#' @return `idx`, reordered.
#' @noRd
arrange_context <- function(idx, how = "relevance") {
  how <- as_chr1(how, "relevance")
  if (identical(how, "relevance")) return(idx)
  # Document order is meaningful for two chunks; only "edges" needs a middle to
  # bury anything in. The length guard used to sit above this branch, so
  # `context_order = "document"` was a silent no-op whenever fewer than three
  # chunks were selected -- which is the common case for a top-k reader.
  if (identical(how, "document")) return(sort(idx))
  if (length(idx) < 3L) return(idx)
  if (!identical(how, "edges")) return(idx)
  # Ranks 1,2,3,4,5,6 become 1,3,5,6,4,2: the best chunk first, the second best
  # last, and the weakest buried in the middle where attention is thinnest.
  odd <- idx[seq(1L, length(idx), by = 2L)]
  even <- idx[seq(2L, length(idx), by = 2L)]
  c(odd, rev(even))
}
