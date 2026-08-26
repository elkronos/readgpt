# segment.R -- AXIS 2: the public segmentation entry point.

#' Describe a segmentation configuration
#'
#' @param method Segmenter name; see `gr_segmenters()`.
#' @param max_tokens Hard cap on chunk size, in tokens. Always enforced -- a
#'   segmenter cannot emit an oversized chunk, which the old `chunk_text_semantic()`
#'   routinely did.
#' @param overlap_tokens Tokens of trailing context copied into the start of the
#'   next chunk. Overlap is what stops an answer straddling a boundary from being
#'   lost by both chunks. Honoured by every segmenter except `page` (a page is
#'   the unit) and `proposition` (propositions are already self-contained).
#' @param min_tokens A chunk smaller than this is merged **backward** into the
#'   chunk before it. Ignored by `page` and `fixed`.
#' @param separators For `method = "recursive"`: the cascade, strongest first.
#' @param prefix_section For `method = "structural"`: prepend the heading.
#' @param semantic_window,semantic_percentile For `method = "semantic"`: how many
#'   sentences to embed together, and how extreme a distance counts as a
#'   boundary (higher = fewer, larger chunks).
#' @param context_source For `method = "contextual"`: `"metadata"` (free) or
#'   `"llm"` (one call per chunk).
#' @param proposition_batch_tokens For `method = "proposition"`: batch size.
#' @param parallel Parallelise per-chunk model work in segmenters that do any.
#' @param ... Extra fields for custom segmenters.
#' @return A list of class `gr_segment_spec`.
#'
#' @section Out-of-range values:
#' Arguments are clamped into a usable range and the change is warned about,
#' never applied silently: `max_tokens` \[32, 1e6\], `overlap_tokens`
#' \[0, max_tokens - 1\], `min_tokens` \[0, max_tokens\], `semantic_window`
#' \[1, 10\], `semantic_percentile` \[50, 99.5\], `proposition_batch_tokens`
#' \[200, 4000\].
#'
#' @seealso [gr_segmenters()] for the available methods and their costs,
#'   [gr_segment()], [gr_chunk_stats()]
#' @family segmentation functions
#' @export
#' @examples
#' gr_segment_spec("semantic", max_tokens = 800, overlap_tokens = 80)$overlap_tokens
#'
#' # An impossible setting is corrected loudly.
#' suppressWarnings(gr_segment_spec("paragraph", max_tokens = 100,
#'                                  overlap_tokens = 500)$overlap_tokens)
gr_segment_spec <- function(method = "paragraph", max_tokens = 1200L,
                            overlap_tokens = 0L, min_tokens = 0L,
                            separators = NULL, prefix_section = TRUE,
                            semantic_window = 2L, semantic_percentile = 90,
                            context_source = c("metadata", "llm"),
                            proposition_batch_tokens = 900L, parallel = NULL, ...) {
  context_source <- match.arg(context_source)
  # Out-of-range settings are clamped rather than accepted, but the clamp is
  # ANNOUNCED. Silently rewriting a user's parameter is how v1 ended up feeding
  # documents to the model backwards: a negative token limit went straight into
  # `split(tokens, ceiling(seq_along(tokens) / limit))` with nothing said.
  max_tokens <- clamp_warn(max_tokens, 32, 1e6, "max_tokens")
  overlap_tokens <- clamp_warn(overlap_tokens, 0, max_tokens - 1L, "overlap_tokens")
  min_tokens <- clamp_warn(min_tokens, 0, max_tokens, "min_tokens")
  structure(c(list(method = method, max_tokens = max_tokens,
                   overlap_tokens = overlap_tokens, min_tokens = min_tokens,
                   separators = separators, prefix_section = isTRUE(prefix_section),
                   semantic_window = clamp_warn(semantic_window, 1, 10, "semantic_window"),
                   semantic_percentile = clamp_warn(semantic_percentile, 50, 99.5,
                                                    "semantic_percentile", integer = FALSE),
                   context_source = context_source,
                   proposition_batch_tokens = clamp_warn(proposition_batch_tokens, 200, 4000,
                                                         "proposition_batch_tokens"),
                   parallel = parallel), list(...)),
            class = "gr_segment_spec")
}

#' Segment a document into chunks
#'
#' Segmentation is deliberately separate from ingestion: one extraction can feed
#' many different segmentations, so comparing chunking strategies costs one file
#' read, not one per strategy.
#'
#' @param doc A `gr_document` from `gr_ingest()`, or anything `gr_ingest()`
#'   accepts.
#' @param spec A `gr_segment_spec`, a segmenter name, or a named list.
#' @param client A `gr_client`, needed only by `semantic`, `proposition` and
#'   `contextual(context_source = "llm")`.
#' @param trace Optional `gr_trace`.
#' @return A [gr_chunks] object. If the requested segmenter could not run it
#'   falls back and records the fallback in `$method`, e.g.
#'   `"semantic->paragraph"` when no `client` was supplied.
#' @seealso [gr_segmenters()], [gr_segment_spec()], [gr_chunk_stats()], [gr_chunks]
#' @family segmentation functions
#' @export
#' @examples
#' doc <- gr_ingest(readgpt_example())
#'
#' # The same document, three boundary hypotheses. No API calls.
#' do.call(rbind, lapply(c("fixed", "paragraph", "sentence", "structural"),
#'   function(m) gr_chunk_stats(gr_segment(doc, list(method = m, max_tokens = 120)))))
gr_segment <- function(doc, spec = NULL, client = NULL, trace = NULL) {
  if (!inherits(doc, "gr_document")) doc <- gr_ingest(doc, trace = trace)
  spec <- as_segment_spec(spec)
  seg <- registry_get("segmenters", spec$method, "segmenters")

  if (isTRUE(seg$needs_client) && is.null(client) && !seg$name %in% names(.gr_self_warning_segmenters)) {
    # The built-ins each emit their own, more specific warning naming what they
    # fall back TO. Emitting a generic one here as well produced two warnings
    # for one event, which trains people to ignore both. A third-party segmenter
    # that declares needs_client and stays silent still gets this one.
    gr_warn(sprintf("Segmenter '%s' needs a client and none was supplied; it will fall back.",
                    spec$method), class = "gr_segment_fallback")
  }
  gr_msg(sprintf("Segmenting with '%s' (cap %d tokens, overlap %d).",
                 spec$method, spec$max_tokens, spec$overlap_tokens))

  out <- seg$fn(doc, spec, client, trace)
  if (!inherits(out, "gr_chunks")) gr_abort(sprintf("Segmenter '%s' did not return a gr_chunks object.", spec$method))

  # Final invariant check. A segmenter that violates the cap is a bug in the
  # segmenter, not something to pass downstream where it becomes an HTTP 400.
  over <- which(out$chunks$tokens > spec$max_tokens)
  if (length(over)) {
    gr_msg(sprintf("Enforcing the token cap on %d oversized chunk(s) from '%s'.",
                   length(over), spec$method))
    src <- out$chunks
    pieces <- lapply(seq_len(nrow(src)), function(i) {
      if (src$tokens[i] <= spec$max_tokens) src$text[i]
      else hard_split(src$text[i], spec$max_tokens)
    })
    # Carry provenance across the split: every piece inherits the page, section
    # and block of the chunk it came from. Rebuilding without this silently
    # turned page/section into NA and broke citations for the whole run.
    rep_n <- vapply(pieces, length, integer(1))
    out2 <- new_chunks(unlist(pieces, use.names = FALSE), out$method, spec,
                       page = rep(src$page, rep_n),
                       section = rep(src$section, rep_n),
                       block_id = rep(src$block_id, rep_n))
    out2$extra <- c(out$extra, list(cap_enforced = length(over)))
    out <- out2
  }
  if (!nrow(out$chunks)) {
    gr_abort(sprintf("Segmenter '%s' produced no chunks from a %d-token document.",
                     spec$method, doc$stats$tokens), class = "gr_empty_chunks")
  }
  trace_note(trace, "segment", c(list(method = spec$method),
                                 as.list(gr_chunk_stats(out))[-1]))
  out
}

#' @noRd
as_segment_spec <- function(spec) {
  if (inherits(spec, "gr_segment_spec")) return(spec)
  if (is.null(spec)) return(gr_segment_spec())
  if (is.character(spec) && length(spec) == 1L) return(gr_segment_spec(method = spec))
  if (is.list(spec)) return(do.call(gr_segment_spec, spec))
  gr_abort("`segment` must be a gr_segment_spec, a segmenter name, or a named list.")
}
