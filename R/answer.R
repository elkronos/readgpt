# answer.R -- the top-level orchestrator.
#
# WHY THIS FILE EXISTS
# `answer_question()` had six separate defects packed into 60 lines:
#
#   1. `match.arg(mode, several.ok = TRUE)` on a five-element default meant that
#      calling `answer_question(f, q)` with no mode ran ALL FIVE modes -- 41 API
#      calls where the obvious reading of the signature suggests one.
#   2. One `chunk_method` and one `chunks` object were shared by every mode, so
#      modes contaminated each other (see pipeline.R).
#   3. `answers[[m]] <- NULL` DELETES the element in R, so a mode whose API call
#      failed vanished from the result. With two modes requested and one failing,
#      the caller got a bare unnamed string and the Shiny UI labelled it with
#      both mode names.
#   4. No `tryCatch` around the loop, so one failure discarded every
#      already-paid-for answer.
#   5. `...` was never forwarded to `parse_text()`, so no ingestion option was
#      reachable from the only public entry point -- and passing one anyway
#      (`remove_numbers = FALSE`) was silently swallowed by a downstream `...`.
#   6. `refine_answer(chunks, question, ans, ...)` forwarded `...` into a
#      function with no `...` formal, so any extra argument produced
#      "unused arguments (temperature = 0.2)". It also called `search_text()`,
#      which is not defined anywhere in the repository -- `refine = TRUE` could
#      never work.

#' Answer a question about a document
#'
#' One recipe, one pipeline: ingest, segment, read. The answer and its full
#' trace come from a single run, so the trace always explains the answer you got.
#'
#' @param source File path, or raw text.
#' @param question The question.
#' @param recipe A `gr_recipe`, a recipe name from `gr_recipes()`, a reader name,
#'   or a named list of `ingest`/`segment`/`read`.
#' @param client A `gr_client`; one is built from options when omitted.
#' @param return `"answer"` (a `gr_answer`), `"text"` (the string), or `"json"`
#'   (answer plus trace, serialised).
#' @param trace Optional `gr_trace` to accumulate into.
#' @param ... Convenience overrides applied to the recipe: any `gr_read_spec`,
#'   `gr_segment_spec` or `gr_ingest_spec` field (for example `model`,
#'   `max_tokens`, `top_k`, `clean`). Unknown names raise an error instead of
#'   being silently discarded.
#' @return Depends on `return`. The `gr_answer` carries `$partial` -- check it
#'   before trusting `$answer`.
#' @seealso [gr_recipes()] for the built-in pipelines, [gr_compare()] to run
#'   several, [gr_answer] for the returned object, [gr_options()] for the cost
#'   and call caps
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "The answer is 42.")
#' txt <- "Chapter one.\n\nThe answer to the great question is 42, as recorded."
#' answer_document(txt, "What is the answer?", "fast", client = cl, return = "text")
answer_document <- function(source, question, recipe = "thorough", client = NULL,
                            return = c("answer", "text", "json"), trace = NULL, ...) {
  return <- match.arg(return)
  if (!is_nonblank(question)) gr_abort("`question` must be a non-empty string.")
  rec <- apply_overrides(as_recipe(recipe), list(...))
  client <- client %||% gr_client(model = rec$read$model)
  trace <- trace %||% gr_trace(meta = list(recipe = rec$name, question = question,
                                           source = source_label(source)))

  doc <- gr_ingest(source, rec$ingest, trace = trace)
  chunks <- gr_segment(doc, rec$segment, client = client, trace = trace)
  ans <- gr_read(chunks, question, client, rec$read, trace = trace)
  # A quote is a sentence, and a sentence is on one page. The reader only sees
  # chunks, which may span several; the document is in scope here, so this is
  # where a citation stops being "somewhere in chunk 4" and becomes "page 7".
  ans$evidence <- resolve_evidence_pages(ans$evidence, doc$blocks)
  ans$recipe <- rec$name
  ans$document <- list(source = doc$source, stats = doc$stats)
  ans$segmentation <- as.list(gr_chunk_stats(chunks))

  switch(return,
    answer = ans,
    text = ans$answer,
    json = as_json(ans))
}

#' Run several recipes over one document and compare them
#'
#' Each recipe is an independent pipeline, so a recipe's answer is identical
#' whether it is run alone or alongside others. Extraction is shared through the
#' ingest cache, and segmentation is shared between recipes whose segment specs
#' are identical -- so comparing five readers over one chunking costs one
#' chunking, not five.
#'
#' Recipes that resolve to identical ingestion, identical segmentation *and* an
#' identical read spec are collapsed with a warning rather than billed twice. A
#' shared reader signature alone is not enough: two `retrieve` recipes with
#' different `top_k` share a signature and are genuinely different runs.
#'
#' @param source File path or raw text.
#' @param question The question.
#' @param recipes A character vector of recipe names, or a list of `gr_recipe`s.
#' @param client A `gr_client`.
#' @param allow_duplicates Run duplicates anyway (useful at temperature > 0 to
#'   measure variance).
#' @param on_error `"continue"` keeps going and records the failure;
#'   `"stop"` aborts the whole comparison.
#' @param ... Overrides applied to every recipe.
#' @return A list with `answers` (named list of [gr_answer]), `summary` (a data
#'   frame with columns `recipe`, `segmenter`, `chunks`, `reader`, `signature`,
#'   `partial`, `chunks_used`, `answer_chars`, `not_found`, `error`), `trace`
#'   (shared across all recipes, so it records every recipe's calls) and
#'   `document` (source and ingestion stats).
#' @seealso [answer_document()], [gr_recipes()], [gr_recipe()], [gr_answer]
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
#'
#' # Three pipelines over one document. Extraction is shared, so this costs one
#' # extraction, not three; only the segmentation and the reader vary.
#' cmp <- gr_compare(readgpt_example(), "What was revenue?",
#'                   c("fast", "precise", "needle"), client = cl)
#' cmp$summary[, c("recipe", "segmenter", "chunks", "reader", "signature", "chunks_used")]
#'
#' # One trace covers all three, so this is the cost of the whole comparison.
#' gr_trace_summary(cmp$trace)
gr_compare <- function(source, question, recipes = c("fast", "needle", "thorough"),
                       client = NULL, allow_duplicates = FALSE,
                       on_error = c("continue", "stop"), ...) {
  on_error <- match.arg(on_error)
  if (!is_nonblank(question)) gr_abort("`question` must be a non-empty string.")
  # A single gr_recipe IS a list (name/ingest/segment/read), so iterating it
  # walked its four FIELDS and tried to treat each as a recipe. Wrap it.
  if (inherits(recipes, "gr_recipe")) recipes <- list(recipes)
  if (!length(recipes)) {
    gr_abort(paste0("`recipes` is empty. Pass at least one recipe name, reader name, ",
                    "gr_recipe or spec list -- see gr_recipes() for the built-ins."),
             class = "gr_no_recipes")
  }
  recs <- lapply(seq_along(recipes), function(i) {
    nm <- if (!is.null(names(recipes))) names(recipes)[i] else NULL
    apply_overrides(as_recipe(recipes[[i]], fallback_name = nm), list(...))
  })
  names(recs) <- vapply(recs, function(r) r$name, character(1))
  if (anyDuplicated(names(recs))) names(recs) <- make.unique(names(recs), sep = "#")

  # Distinctness check: identical segmentation + identical reader signature
  # means identical work. This is precisely the Chunked/Semantic collapse.
  # Hash the WHOLE read spec, not a hand-picked subset. The subset omitted
  # rerank_candidates, min_score, temperature, max_levels, on_overflow,
  # skim_model and the token caps -- all of which change which chunks reach the
  # model or how it answers -- so genuinely different recipes were silently
  # collapsed into one and never run.
  keys <- vapply(recs, function(r) paste0(
    gr_hash(unclass(r$segment)), "|",
    tryCatch(gr_reader_signature(r$read), error = function(e) r$read$reader), "|",
    gr_hash(unclass(r$read)), "|",
    gr_hash(unclass(r$ingest))), character(1))
  if (!allow_duplicates && anyDuplicated(keys)) {
    dups <- split(names(recs), keys)
    dups <- dups[vapply(dups, length, integer(1)) > 1L]
    for (g in dups) {
      gr_warn(sprintf(paste0("Recipes %s are the same pipeline: identical ingestion, identical ",
                             "segmentation and the same reader signature. Running only '%s'. ",
                             "Pass allow_duplicates = TRUE to run them anyway."),
                      paste(sprintf("'%s'", g), collapse = ", "), g[1]),
              class = "gr_duplicate_recipe")
    }
    recs <- recs[!duplicated(keys)]
  }

  trace <- gr_trace(meta = list(question = question, recipes = names(recs),
                                source = source_label(source)))
  doc <- gr_ingest(source, recs[[1]]$ingest, trace = trace)
  client <- client %||% gr_client(model = recs[[1]]$read$model)

  seg_cache <- new.env(parent = emptyenv())
  answers <- list()
  for (nm in names(recs)) {
    r <- recs[[nm]]
    out <- tryCatch({
      d <- if (identical(gr_hash(unclass(r$ingest)), gr_hash(unclass(recs[[1]]$ingest)))) doc
           else gr_ingest(source, r$ingest, trace = trace)
      # Key on the document's actual TEXT, not its character count. Counting
      # characters meant any length-preserving cleaner produced a cache hit on
      # different text, and one recipe was handed another recipe's chunks --
      # order-dependent, silent, and wrong.
      skey <- gr_hash(list(d$text, unclass(r$segment)))
      ch <- seg_cache[[skey]]
      if (is.null(ch)) { ch <- gr_segment(d, r$segment, client = client, trace = trace)
                         seg_cache[[skey]] <- ch }
      # Each recipe gets its own budget accounting, then its steps are folded
      # into the shared trace. Sharing the trace outright meant `max_calls`
      # counted earlier recipes against later ones, so the same recipe returned
      # a different answer depending on its position in the comparison.
      sub <- gr_trace(meta = list(recipe = nm))
      a <- gr_read(ch, question, client, r$read, trace = sub)
      trace_absorb(trace, sub)
      a$recipe <- nm
      a$segmentation <- as.list(gr_chunk_stats(ch))
      a
    }, error = function(e) {
      if (identical(on_error, "stop")) stop(e)
      gr_warn(sprintf("Recipe '%s' failed: %s", nm, conditionMessage(e)))
      # A failed recipe is RECORDED, not deleted. `answers[[nm]] <- NULL` in the
      # old code removed the key entirely and left the caller unable to tell
      # which mode had failed.
      a <- new_answer(.NOT_FOUND, r$read$reader, question, integer(0), trace,
                      partial = TRUE, notes = list(error = conditionMessage(e)))
      a$recipe <- nm
      a
    })
    answers[[nm]] <- out
  }

  summary <- do.call(rbind, lapply(names(answers), function(nm) {
    a <- answers[[nm]]
    seg <- a$segmentation %||% list(method = NA_character_, n = NA_integer_)
    data.frame(recipe = nm, segmenter = as_chr1(seg$method, NA_character_),
               chunks = as.integer(seg$n %||% NA), reader = a$reader,
               signature = as_chr1(a$signature, NA_character_),
               partial = a$partial,
               chunks_used = length(a$chunks_used),
               answer_chars = nchar(a$answer),
               not_found = is_not_found(a$answer),
               error = as_chr1(a$notes$error, NA_character_),
               stringsAsFactors = FALSE)
  }))
  list(answers = answers, summary = summary, trace = trace,
       document = list(source = doc$source, stats = doc$stats))
}

#' Apply flat `...` overrides onto the right axis of a recipe.
#'
#' Unknown names are an ERROR. In the old code they were swallowed by a
#' downstream `...` -- `answer_question(f, q, remove_numbers = FALSE)` ran
#' cleanly and stripped every digit anyway.
#' @noRd
apply_overrides <- function(rec, overrides) {
  if (!length(overrides)) return(rec)
  if (is.null(names(overrides)) || any(!nzchar(names(overrides)))) {
    gr_abort("Overrides passed through `...` must be named.")
  }
  ing_f <- setdiff(names(formals(gr_ingest_spec)), "...")
  seg_f <- setdiff(names(formals(gr_segment_spec)), "...")
  rd_f  <- setdiff(names(formals(gr_read_spec)), "...")
  # `method` is ambiguous; `parallel` legitimately applies to both.
  seg_only <- setdiff(seg_f, c(ing_f, rd_f))
  rd_only  <- setdiff(rd_f, c(ing_f, seg_f))
  ing_only <- setdiff(ing_f, c(seg_f, rd_f))

  ing <- unclass(rec$ingest); seg <- unclass(rec$segment); rd <- unclass(rec$read)
  for (nm in names(overrides)) {
    v <- overrides[[nm]]
    if (nm %in% seg_only)       seg[[nm]] <- v
    else if (nm %in% rd_only)   rd[[nm]]  <- v
    else if (nm %in% ing_only)  ing[[nm]] <- v
    else if (nm == "parallel") { seg$parallel <- v; rd$parallel <- v; ing$parallel <- v }
    else if (nm == "method")    seg$method <- v
    else {
      gr_abort(sprintf(paste0("Unknown override '%s'. Ingest fields: %s. Segment fields: %s. ",
                              "Read fields: %s."),
                       nm, paste(ing_f, collapse = ", "), paste(seg_f, collapse = ", "),
                       paste(rd_f, collapse = ", ")), class = "gr_unknown_override")
    }
  }
  gr_recipe(rec$name, ingest = ing, segment = seg, read = rd)
}
