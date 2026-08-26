# pipeline.R -- the three axes bound into one named configuration.
#
# WHY THIS FILE EXISTS
# `answer_question()` decided ONE chunking method for the whole run --
# `if ("Semantic" %in% mode) "semantic" else "naive"` -- and then looped over
# modes sharing a single `chunks` object. Selecting a second mode therefore
# changed the first mode's input, and its answer:
#
#     mode = "Chunked"                 -> 1 chunk,  2 API calls
#     mode = c("Chunked", "Semantic")  -> 6 chunks, 14 API calls, different answer
#
# A recipe binds ingest + segment + read together, so a run of several recipes
# is several independent pipelines that cannot contaminate each other. Extraction
# is still shared through the ingest cache, so the isolation costs nothing.

#' Bind an ingestion, segmentation and reading configuration together
#'
#' A recipe is the unit [gr_compare()] treats as one experiment. Fixing two axes
#' and varying the third is what makes a difference in the answer attributable
#' to the change you made -- which is exactly what the previous release could not
#' do, because its modes shared one chunk object.
#'
#' @param name Label used in results and traces.
#' @param ingest A `gr_ingest_spec`, preset name, or named list.
#' @param segment A `gr_segment_spec`, segmenter name, or named list.
#' @param read A `gr_read_spec`, reader name, or named list.
#' @return A `gr_recipe`: a list of `name`, `ingest`, `segment` and `read`, each
#'   a validated spec. It has a `print()` method that shows all three at once.
#' @seealso [gr_recipes()] for the built-ins, [answer_document()] to run one,
#'   [gr_compare()] to run several, [gr_ingest_spec()], [gr_segment_spec()],
#'   [gr_read_spec()]
#' @export
#' @examples
#' gr_recipe("semantic_topk", segment = list(method = "semantic", max_tokens = 800),
#'           read = list(reader = "retrieve", top_k = 6))
#'
#' # Start from a built-in and change one axis -- the other two stay fixed, so
#' # any difference in the answer is attributable to the change.
#' r <- gr_recipes("thorough")
#' r$segment <- gr_segment_spec(method = "structural", max_tokens = 1200)
#' r$name <- "thorough_structural"
#' r
gr_recipe <- function(name = NULL, ingest = NULL, segment = NULL, read = NULL) {
  ing <- as_ingest_spec(ingest)
  seg <- as_segment_spec(segment)
  rd  <- as_read_spec(read)
  structure(list(
    name = as_chr1(name %||% sprintf("%s+%s", seg$method, rd$reader)),
    ingest = ing, segment = seg, read = rd
  ), class = "gr_recipe")
}

#' @export
print.gr_recipe <- function(x, ...) {
  cat(sprintf("<gr_recipe '%s'>\n", x$name))
  cat(sprintf("  ingest  : clean=%s ocr=%s\n",
              paste(as.character(x$ingest$clean), collapse = "+"), x$ingest$ocr))
  cat(sprintf("  segment : %s (max %d tok, overlap %d, min %d)\n",
              x$segment$method, x$segment$max_tokens, x$segment$overlap_tokens,
              x$segment$min_tokens))
  cat(sprintf("  read    : %s [%s] model=%s\n", x$read$reader,
              tryCatch(gr_reader_signature(x$read), error = function(e) "?"), x$read$model))
  invisible(x)
}

#' Ready-made recipes
#'
#' Named starting points that pair a segmentation strategy with a reader that
#' suits it. Each is a plain `gr_recipe`, so you can modify any field.
#'
#' @param name Optional recipe name, or a character vector of names; omit to
#'   list them all. A single name returns a `gr_recipe`; several return a named
#'   list of them.
#' @return A `gr_recipe` when `name` is a single string, otherwise a named list
#'   of `gr_recipe`s.
#' @seealso [gr_recipe()] to build your own, [answer_document()], [gr_compare()],
#'   [gr_segmenters()] and [gr_readers()] for the pieces they are made of
#' @export
#' @examples
#' # What each built-in actually is, in one table.
#' do.call(rbind, lapply(names(gr_recipes()), function(n) {
#'   r <- gr_recipes(n)
#'   data.frame(recipe = n, clean = paste(as.character(r$ingest$clean), collapse = "+"),
#'              segment = r$segment$method, max_tokens = r$segment$max_tokens,
#'              reader = r$read$reader)
#' }))
#'
#' gr_recipes("precise")
gr_recipes <- function(name = NULL) {
  r <- list(
    fast = gr_recipe("fast",
      segment = list(method = "paragraph", max_tokens = 4000),
      read = list(reader = "stuff", on_overflow = "warn")),
    precise = gr_recipe("precise",
      segment = list(method = "sentence", max_tokens = 600, overlap_tokens = 60),
      read = list(reader = "skim", cite = TRUE)),
    needle = gr_recipe("needle",
      segment = list(method = "semantic", max_tokens = 500, overlap_tokens = 50),
      read = list(reader = "retrieve", top_k = 8, cite = TRUE)),
    thorough = gr_recipe("thorough",
      segment = list(method = "paragraph", max_tokens = 1200, overlap_tokens = 120),
      read = list(reader = "map_reduce")),
    survey = gr_recipe("survey",
      segment = list(method = "structural", max_tokens = 1500),
      read = list(reader = "hierarchical", fan_in = 5)),
    narrative = gr_recipe("narrative",
      segment = list(method = "paragraph", max_tokens = 1500),
      read = list(reader = "refine")),
    scanned = gr_recipe("scanned",
      ingest = list(clean = "scan", ocr = "auto"),
      segment = list(method = "page", max_tokens = 2000),
      read = list(reader = "rerank", rerank_candidates = 25, top_k = 6)),
    research = gr_recipe("research",
      ingest = list(clean = "academic"),
      segment = list(method = "structural", max_tokens = 900, overlap_tokens = 90),
      read = list(reader = "iterative", top_k = 5, max_rounds = 4, cite = TRUE)),
    consensus = gr_recipe("consensus",
      segment = list(method = "recursive", max_tokens = 1000, overlap_tokens = 100),
      read = list(reader = "ensemble", members = c("retrieve", "map_reduce", "refine"))),
    # Reproduces the old defaults -- digit-stripping, word-count budgeting, no
    # overlap -- so the change in behaviour can be measured rather than asserted.
    legacy_v1 = gr_recipe("legacy_v1",
      ingest = list(clean = "legacy_v1"),
      segment = list(method = "paragraph", max_tokens = 3000, overlap_tokens = 0),
      read = list(reader = "map_reduce"))
  )
  if (is.null(name)) return(r)
  if (!is.character(name)) {
    gr_abort(sprintf("`name` must be a character vector of recipe names, not %s. Available: %s.",
                     class(name)[1], paste(names(r), collapse = ", ")))
  }
  # Vectorised. `gr_recipes(c("fast", "precise"))` used to die on "the condition
  # has length > 1" -- which is exactly the call you make to hand a set of
  # recipes to gr_compare().
  unknown <- setdiff(name, names(r))
  if (length(unknown)) {
    gr_abort(sprintf("Unknown recipe(s): %s. Available: %s.",
                     paste(sprintf("'%s'", unknown), collapse = ", "),
                     paste(names(r), collapse = ", ")))
  }
  if (length(name) == 1L) r[[name]] else r[name]
}

#' @noRd
as_recipe <- function(x, fallback_name = NULL) {
  if (inherits(x, "gr_recipe")) return(x)
  if (is.character(x) && length(x) == 1L) {
    known <- names(gr_recipes())
    if (x %in% known) return(gr_recipes(x))
    # A bare reader name is also accepted, paired with the default segmenter.
    if (x %in% names(gr_state$readers)) return(gr_recipe(x, read = x))
    gr_abort(sprintf("'%s' is neither a recipe (%s) nor a reader (%s).",
                     x, paste(known, collapse = ", "),
                     paste(sort(names(gr_state$readers)), collapse = ", ")))
  }
  if (is.list(x)) {
    if (any(c("ingest", "segment", "read") %in% names(x))) {
      return(do.call(gr_recipe, utils::modifyList(list(name = fallback_name), x)))
    }
    gr_abort("A recipe list needs at least one of `ingest`, `segment`, `read`.")
  }
  gr_abort("Expected a gr_recipe, a recipe/reader name, or a named list.")
}
