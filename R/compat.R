# compat.R -- shims for the v1 API.
#
# These keep old scripts running while making the behaviour change visible. Each
# shim warns once per session, maps onto the new pipeline, and -- importantly --
# does NOT reproduce the v1 bugs. If you want v1's actual behaviour for an A/B
# comparison, use `recipe = "legacy_v1"`, which reproduces the old cleaning and
# chunking deliberately rather than by accident.

.warn_once <- local({
  seen <- character(0)
  function(id, msg) {
    if (id %in% seen) return(invisible(NULL))
    # Emit FIRST, mark second. Marking first meant that a handler which turned
    # the warning into an error -- options(warn = 2), or a tryCatch(warning=)
    # that aborts -- consumed the one and only notice: the user saw a failure,
    # fixed their handler, ran again, and got no deprecation warning at all.
    gr_warn(msg, class = "gr_deprecated")
    seen <<- c(seen, id)
    invisible(NULL)
  }
})

#' Forget which deprecation warnings have been emitted.
#'
#' Session state that persists across `test_that()` blocks makes a test suite
#' order-dependent: whichever test runs first sees the warning and the rest do
#' not. Tests call this in `setup`/`on.exit` so each one starts clean.
#' @noRd
.warn_once_reset <- function() {
  environment(.warn_once)$seen <- character(0)
  invisible(NULL)
}

#' Deprecated: answer a question using v1 mode names
#'
#' Maps the old `mode` strings onto recipes. Unlike v1, each mode runs as an
#' isolated pipeline, so selecting several modes no longer changes any of their
#' answers, and `mode` no longer defaults to running all five.
#'
#' @param file_path Path to the document.
#' @param question The question.
#' @param mode One or more of `"Retrieval"`, `"Chunked"`, `"Semantic"`,
#'   `"Hierarchical"`, `"MultiPass"`. Defaults to `"Chunked"` only.
#' @param use_parallel Run per-chunk calls in parallel.
#' @param refine Request chunk-level citations in the answer. v1's `refine`
#'   verification pass is not reproduced -- it could never run, because it
#'   called a `search_text()` function that was never defined.
#' @param return_json Return the answers, the comparison summary and the trace
#'   as JSON instead of the answer string.
#' @param client A `gr_client`.
#' @param ... Passed to `answer_document()`.
#' @return A string, or a named list of strings when several modes are given.
#' @seealso [answer_document()], [gr_compare()], [gr_recipes()]
#' @family v1 compatibility
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "45.2 million dollars")
#'
#' # v1 style, still works, warns once.
#' suppressWarnings(
#'   answer_question(gptread_example(), "What was revenue?", mode = "Chunked", client = cl))
#'
#' # The modern equivalent.
#' answer_document(gptread_example(), "What was revenue?", "thorough",
#'                 client = cl, return = "text")
answer_question <- function(file_path, question, mode = "Chunked", use_parallel = FALSE,
                            refine = FALSE, return_json = FALSE, client = NULL, ...) {
  .warn_once("answer_question", paste0(
    "answer_question() is the v1 interface and is deprecated. Use answer_document() or ",
    "gr_compare(). Note three behaviour changes: mode no longer defaults to all five modes; ",
    "each mode now runs as an isolated pipeline so modes cannot alter each other's answers; ",
    "and digits are no longer stripped from documents by default."))

  map <- list(
    Retrieval    = gr_recipe("Retrieval",
                     segment = list(method = "paragraph", max_tokens = 1200),
                     read = list(reader = "skim")),
    Chunked      = gr_recipe("Chunked",
                     segment = list(method = "paragraph", max_tokens = 1200),
                     read = list(reader = "map_reduce")),
    # v1's "Semantic" was Chunked with a length-sorted chunk list. It is now a
    # genuinely different pipeline: embedding-based boundaries and top-k reading.
    Semantic     = gr_recipe("Semantic",
                     segment = list(method = "semantic", max_tokens = 800, overlap_tokens = 80),
                     read = list(reader = "retrieve", top_k = 8)),
    Hierarchical = gr_recipe("Hierarchical",
                     segment = list(method = "paragraph", max_tokens = 1200),
                     read = list(reader = "hierarchical")),
    MultiPass    = gr_recipe("MultiPass",
                     segment = list(method = "paragraph", max_tokens = 1200),
                     read = list(reader = "ensemble", members = c("skim", "map_reduce")))
  )
  bad <- setdiff(mode, names(map))
  if (length(bad)) gr_abort(sprintf("Unknown mode(s): %s.", paste(bad, collapse = ", ")))

  recs <- map[mode]
  if (refine) recs <- lapply(recs, function(r) { r$read$cite <- TRUE; r })
  cmp <- gr_compare(file_path, question, recs, client = client,
                    parallel = use_parallel, allow_duplicates = TRUE, ...)
  if (return_json) {
    # `cmp$trace` is an environment. Passing it straight to the encoder failed
    # with "cannot unclass an environment", so return_json = TRUE -- the only
    # thing this argument does -- could never succeed.
    return(as_json(list(answers = lapply(cmp$answers, function(a) a$answer),
                        summary = cmp$summary, trace = trace_as_list(cmp$trace))))
  }
  out <- lapply(cmp$answers, function(a) a$answer)
  if (length(out) == 1L) out[[1]] else out
}

#' Deprecated: parse a document into text chunks
#'
#' @param file_path Path to the document.
#' @param chunk_token_limit Maximum tokens per chunk.
#' @param chunk_method `"naive"` (mapped to `"paragraph"`) or `"semantic"`.
#' @param remove_whitespace,remove_special_chars,remove_numbers v1 cleaning flags.
#' @param ocr_lang OCR language.
#' @param client A `gr_client`, needed for `chunk_method = "semantic"`.
#' @return A character vector of chunk texts.
#' @seealso [gr_ingest()], [gr_segment()], [gr_chunk_stats()]
#' @family v1 compatibility
#' @export
#' @examples
#' # v1 style, still works, warns once.
#' length(suppressWarnings(parse_text(gptread_example(), chunk_token_limit = 200)))
#'
#' # The modern equivalent, which also reports what it did.
#' gr_chunk_stats(gr_segment(gr_ingest(gptread_example()),
#'                           list(method = "paragraph", max_tokens = 200)))
parse_text <- function(file_path, chunk_token_limit = 3000, chunk_method = c("naive", "semantic"),
                       remove_whitespace = TRUE, remove_special_chars = FALSE,
                       remove_numbers = FALSE, ocr_lang = "eng", client = NULL) {
  chunk_method <- match.arg(chunk_method)
  .warn_once("parse_text", paste0(
    "parse_text() is deprecated; use gr_ingest() then gr_segment(). Two v1 defaults have ",
    "changed because they were destructive: remove_numbers and remove_special_chars now ",
    "default to FALSE. v1 stripped every digit from every document by default, which made ",
    "any question about a figure, date or percentage unanswerable."))
  steps <- c("page_numbers", "hyphenation", "control_chars", "ligatures")
  if (remove_whitespace) steps <- c(steps, "collapse_whitespace")
  if (remove_special_chars) steps <- c(steps, "remove_punctuation")
  if (remove_numbers) steps <- c(steps, "remove_numbers")
  doc <- gr_ingest(file_path, gr_ingest_spec(clean = steps, ocr_lang = ocr_lang))
  seg <- gr_segment(doc, gr_segment_spec(
    method = if (chunk_method == "semantic") "semantic" else "paragraph",
    max_tokens = chunk_token_limit), client = client)
  seg$chunks$text
}

#' @noRd
.compat_read <- function(chunks, question, reader, client, ..., return_json = FALSE) {
  txt <- if (inherits(chunks, "gr_chunks")) chunks$chunks$text else as.character(chunks)
  doc <- gr_ingest(paste(txt, collapse = "\n\n"))
  ch <- new_chunks(txt, "precomputed", gr_segment_spec(max_tokens = max(gr_count_tokens(txt), 32L)))
  tr <- gr_trace(meta = list(reader = reader))
  a <- gr_read(ch, question, client %||% gr_client(), gr_read_spec(reader = reader, ...), trace = tr)
  if (return_json) as_json(a) else a$answer
}

#' Deprecated: chunk-by-chunk reading
#'
#' Superseded by [gr_read()] with `reader = "map_reduce"`. Kept so v1 scripts
#' keep running; it warns once per session and does **not** reproduce v1's
#' unbounded merge prompt, which produced HTTP 400s once the per-chunk answers
#' outgrew the context window. For v1's actual behaviour, use
#' `recipe = "legacy_v1"`.
#'
#' @param chunks Character vector of chunks, or a `gr_chunks`.
#' @param question The question.
#' @param client A `gr_client`.
#' @param return_json Return the whole answer object, including its trace, as JSON.
#' @param ... Passed to `gr_read_spec()`.
#' @return A single string when `return_json` is `FALSE`; otherwise a
#'   `json`-classed string holding the answer, its notes and the full trace.
#' @seealso [gr_read()] and `reader = "map_reduce"`, [gr_readers()],
#'   [answer_document()], [gr_recipes()] for `"legacy_v1"`
#' @family v1 compatibility
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "45.2 million dollars")
#' chunks <- suppressWarnings(parse_text(gptread_example(), chunk_token_limit = 200))
#'
#' # v1 style, still works, warns once.
#' suppressWarnings(gpt_read_chunked(chunks, "What was revenue?", client = cl))
#'
#' # The modern equivalent, which also reports what it did.
#' ch <- gr_segment(gptread_example(), list(method = "paragraph", max_tokens = 200))
#' gr_read(ch, "What was revenue?", gr_mock_client(function(m, p) "45.2 million dollars"),
#'         "map_reduce")$notes$chunks
gpt_read_chunked <- function(chunks, question, client = NULL, return_json = FALSE, ...) {
  .warn_once("gpt_read_chunked", "gpt_read_chunked() is deprecated; use gr_read(..., 'map_reduce').")
  .compat_read(chunks, question, "map_reduce", client, ..., return_json = return_json)
}

#' Deprecated: evidence-extraction reading
#'
#' Superseded by [gr_read()] with `reader = "skim"`, which extracts verbatim
#' evidence from every chunk and then consolidates it -- so `ans$evidence` holds
#' passages from the document rather than the model's paraphrase of them.
#'
#' @inheritParams gpt_read_chunked
#' @return A single string when `return_json` is `FALSE`; otherwise a
#'   `json`-classed string holding the answer, its notes and the full trace.
#' @seealso [gr_read()] and `reader = "skim"`, [gr_readers()], [gr_answer] for
#'   the evidence table
#' @family v1 compatibility
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "45.2 million dollars")
#' chunks <- suppressWarnings(parse_text(gptread_example(), chunk_token_limit = 200))
#' suppressWarnings(gpt_read_retrieval(chunks, "What was revenue?", client = cl))
#'
#' # The modern equivalent.
#' ch <- gr_segment(gptread_example(), list(method = "paragraph", max_tokens = 200))
#' gr_read(ch, "What was revenue?", gr_mock_client(function(m, p) "45.2 million dollars"),
#'         "skim")$reader
gpt_read_retrieval <- function(chunks, question, client = NULL, return_json = FALSE, ...) {
  .warn_once("gpt_read_retrieval", "gpt_read_retrieval() is deprecated; use gr_read(..., 'skim').")
  .compat_read(chunks, question, "skim", client, ..., return_json = return_json)
}

#' Deprecated: hierarchical reading
#'
#' Superseded by [gr_read()] with `reader = "hierarchical"`. The new reader
#' recurses until the summaries fit the context window; v1 summarised exactly
#' once and overflowed past roughly 32 chunks.
#'
#' @inheritParams gpt_read_chunked
#' @return A single string when `return_json` is `FALSE`; otherwise a
#'   `json`-classed string holding the answer, its notes and the full trace.
#' @seealso [gr_read()] and `reader = "hierarchical"`, [gr_read_spec()] for
#'   `fan_in` and `max_levels`, [gr_readers()]
#' @family v1 compatibility
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "45.2 million dollars")
#' chunks <- suppressWarnings(parse_text(gptread_example(), chunk_token_limit = 200))
#' suppressWarnings(gpt_read_hierarchical(chunks, "What was revenue?", client = cl))
#'
#' # The modern equivalent.
#' ch <- gr_segment(gptread_example(), list(method = "paragraph", max_tokens = 200))
#' gr_read(ch, "What was revenue?", gr_mock_client(function(m, p) "45.2 million dollars"),
#'         "hierarchical")$reader
gpt_read_hierarchical <- function(chunks, question, client = NULL, return_json = FALSE, ...) {
  .warn_once("gpt_read_hierarchical",
             "gpt_read_hierarchical() is deprecated; use gr_read(..., 'hierarchical'). The new reader recurses until the summaries fit; v1 summarised exactly once and could overflow the context window.")
  .compat_read(chunks, question, "hierarchical", client, ..., return_json = return_json)
}

#' Deprecated: multi-pass reading
#'
#' Superseded by [gr_read()] with `reader = "ensemble"`. v1 re-ran Retrieval and
#' Chunked verbatim, so selecting them alongside MultiPass paid for each twice;
#' the ensemble now requires its members to have different traversal signatures,
#' and says so when they collapse onto the same traversal anyway.
#'
#' @inheritParams gpt_read_chunked
#' @return A single string when `return_json` is `FALSE`; otherwise a
#'   `json`-classed string holding the answer, its notes and the full trace.
#' @seealso [gr_read()] and `reader = "ensemble"`, [gr_reader_signature()],
#'   [gr_readers()]
#' @family v1 compatibility
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "45.2 million dollars")
#' chunks <- suppressWarnings(parse_text(gptread_example(), chunk_token_limit = 200))
#' suppressWarnings(gpt_read_multipass(chunks, "What was revenue?", client = cl))
#'
#' # The modern equivalent.
#' ch <- gr_segment(gptread_example(), list(method = "paragraph", max_tokens = 200))
#' gr_read(ch, "What was revenue?", gr_mock_client(function(m, p) "45.2 million dollars"),
#'         "ensemble")$reader
gpt_read_multipass <- function(chunks, question, client = NULL, return_json = FALSE, ...) {
  .warn_once("gpt_read_multipass",
             "gpt_read_multipass() is deprecated; use gr_read(..., 'ensemble'). v1 re-ran Retrieval and Chunked verbatim, so selecting them alongside MultiPass paid for each twice.")
  .compat_read(chunks, question, "ensemble", client,
               members = c("skim", "map_reduce"), ..., return_json = return_json)
}
