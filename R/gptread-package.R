#' gptread: control how a language model reads a document
#'
#' Document question answering, with the three decisions that actually drive
#' answer quality pulled apart into independent, swappable axes:
#'
#' \describe{
#'   \item{\strong{ingest}}{bytes to clean text: which extractor, which cleaning
#'     steps. See [gr_ingest()], [gr_ingest_spec()], [gr_cleaners()]; it returns
#'     a [gr_document].}
#'   \item{\strong{segment}}{text to chunks: where the boundaries fall, how big,
#'     how much overlap. See [gr_segment()], [gr_segment_spec()],
#'     [gr_segmenters()]; it returns [gr_chunks].}
#'   \item{\strong{read}}{chunks to an answer: which chunks reach the model, in
#'     what call pattern. See [gr_read()], [gr_read_spec()], [gr_readers()]; it
#'     returns a [gr_answer].}
#' }
#'
#' Any ingest x any segmenter x any reader composes. [gr_recipe()] binds one of
#' each into a named pipeline; [answer_document()] runs one; [gr_compare()] runs
#' several over one document and reports how they differ.
#'
#' @section Getting started:
#' ```r
#' Sys.setenv(OPENAI_API_KEY = "sk-...")
#'
#' # No key yet? Everything below works offline against gr_mock_client() and the
#' # bundled document at gptread_example().
#'
#' # One question, one pipeline.
#' ans <- answer_document("report.pdf", "What was Q3 revenue?", recipe = "needle")
#' ans$answer
#' ans$partial          # TRUE means something degraded -- check this first
#'
#' # Which pipeline suits this document? Compare, then commit.
#' cmp <- gr_compare("report.pdf", "What was Q3 revenue?",
#'                   c("fast", "needle", "thorough"))
#' cmp$summary
#' ```
#'
#' @section Choosing a strategy:
#' \tabular{ll}{
#'   \strong{your document} \tab \strong{start with} \cr
#'   fits in one context window \tab `"fast"` \cr
#'   one fact buried in a long report \tab `"needle"` \cr
#'   needs every mention found \tab `"thorough"` \cr
#'   long, with headings, needs a synthesis \tab `"survey"` \cr
#'   scanned PDF, forms, invoices \tab `"scanned"` \cr
#'   an argument that develops across the text \tab `"narrative"` \cr
#'   multi-hop question over a paper \tab `"research"` \cr
#'   high stakes, want cross-checking \tab `"consensus"` \cr
#'   short, and you want every sentence weighed \tab `"precise"` \cr
#' }
#'
#' `"legacy_v1"` is the tenth: it reproduces the previous release's ingestion
#' and chunking deliberately, so a change in behaviour can be measured against
#' old results rather than assumed.
#'
#' See [gr_recipes()] for what each one actually configures. When in doubt,
#' `gr_compare()` two or three on your own document and read `cmp$summary`.
#'
#' @section Cost control:
#' Two rails are on by default and are checked **before** the first request:
#' `max_cost_usd` (5) and `max_calls` (400). Both are [gr_options()]. A run that
#' trips one raises a classed error naming the option to change. `max_calls` is
#' re-checked before every subsequent call, so a run that hits it mid-flight
#' returns a `partial` answer rather than continuing to spend.
#'
#' Preview segmentation for free before committing to a reader:
#' ```r
#' doc <- gr_ingest("report.pdf")
#' gr_chunk_stats(gr_segment(doc, list(method = "sentence", max_tokens = 400)))
#' ```
#'
#' @section Diagnosing an answer:
#' Every degradation is recorded, never silent. See [gr_answer] for the full
#' object, and `vignette`-free quick reference:
#' ```r
#' ans$partial     # did anything degrade?
#' ans$notes       # what: dropped_chunks, failed_calls, error, degraded_to_bm25
#' print(ans$trace)          # calls, tokens, first error
#' as_json(ans)              # every prompt and response from this one run
#' ```
#'
#' @section Extending it:
#' Each axis is a registry, so additions behave exactly like built-ins:
#' [gr_register_extractor()], [gr_register_cleaner()],
#' [gr_register_segmenter()], [gr_register_reader()], [gr_register_model()].
#'
#' @seealso [answer_document()], [gr_compare()], [gr_recipes()], [gr_options()]
#' @keywords internal
"_PACKAGE"


#' The result of one reading run
#'
#' Returned by [gr_read()] and, with three extra fields, by [answer_document()].
#'
#' @section Fields:
#' \describe{
#'   \item{`answer`}{Character(1). Always a single string. The sentinel
#'     `"NOT_IN_DOCUMENT"` means the model reported the document does not
#'     contain the answer -- test it with `is_not_found()`-style matching rather
#'     than substring search.}
#'   \item{`partial`}{Logical(1). `TRUE` when anything degraded: a call failed,
#'     chunks were dropped, a cap was hit, a strategy fell back. **Check this
#'     before trusting an answer.**}
#'   \item{`notes`}{List. Why it is partial, and per-reader detail:
#'     `dropped_chunks`, `failed_calls`, `error`, `merge_levels`,
#'     `degraded_to_bm25`, `stop_reason`, `call_cap_reached`, and so on.}
#'   \item{`evidence`}{Data frame or `NULL`, with columns `chunk_id`, `text`,
#'     `page`, `section`, `score`. **What `text` holds depends on the reader**:
#'     verbatim chunk text for `stuff`, `retrieve`, `rerank` and `iterative`;
#'     model-extracted passages for `skim`; per-chunk model answers for
#'     `map_reduce`. `refine` and `hierarchical` return `NULL`. `page` is
#'     populated only for PDF sources; `score` only for `retrieve` (cosine) and
#'     `rerank` (0-10, model-judged).}
#'   \item{`chunks_used`}{Integer vector of `chunk_id`s that CONTRIBUTED to the answer. For the
#'     per-chunk readers this is a subset of the chunks actually sent -- a chunk
#'     that answered `NOT_IN_DOCUMENT` was read and paid for but is not listed.
#'     `notes$chunks` reports how many were sent.}
#'   \item{`reader`, `signature`}{Which strategy ran, and its traversal
#'     signature (see [gr_reader_signature()]).}
#'   \item{`question`}{The question, as asked.}
#'   \item{`trace`}{The [gr_trace] for this run. In a [gr_compare()] the trace is
#'     shared across recipes, so it records every recipe's calls.}
#'   \item{`recipe`, `document`, `segmentation`}{Added by [answer_document()] and
#'     [gr_compare()]: the recipe name, the source and ingestion stats, and the
#'     chunk statistics the reader saw.}
#' }
#'
#' @section Methods:
#' `print()` shows the answer, the partial flag, the call count and an evidence
#' summary; [as_json()] serialises the answer together with every prompt and
#' response from the same run.
#' @seealso [answer_document()] and [gr_read()] which return one, [gr_compare()]
#'   to compare several, [is_not_found()] to test the sentinel, [as_json()],
#'   [new_answer()] to build one in a custom reader
#' @family reading functions
#' @name gr_answer
#' @seealso [gr_read()], [answer_document()], [as_json()]
#' @examples
#' cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
#' ans <- answer_document(gptread_example(), "What was revenue?", "fast", client = cl)
#' ans$partial
#' ans$evidence[, c("chunk_id", "page", "score")]
NULL


#' A set of document chunks
#'
#' Returned by [gr_segment()]. Summarise with [gr_chunk_stats()].
#'
#' @section Fields:
#' \describe{
#'   \item{`chunks`}{Data frame, one row per chunk, with columns `chunk_id`,
#'     `text`, `tokens`, `chars`, `page`, `section`, `block_id`.}
#'   \item{`method`}{The segmenter that ran. **If a segmenter fell back this
#'     records the fallback**, e.g. `"semantic->paragraph"` when no client was
#'     supplied, or `"page->paragraph"` for a source with no page provenance.}
#'   \item{`spec`}{The [gr_segment_spec()] used.}
#'   \item{`extra`}{Method-specific detail: `boundaries` and
#'     `embedding_source` for `semantic`, `propositions` for `proposition`,
#'     `cap_enforced` when oversized chunks had to be split.}
#' }
#'
#' @section Methods:
#' `print()` shows the method, the chunk count and the token distribution;
#' [gr_chunk_stats()] returns the same as a one-row data frame you can `rbind`;
#' [as_json()] serialises the spec, the stats and every chunk.
#' @seealso [gr_segment()] which returns one, [gr_chunk_stats()],
#'   [gr_segmenters()], [as_json()], [new_chunks()] to build one in a custom
#'   segmenter
#' @family segmentation functions
#' @name gr_chunks
#' @seealso [gr_segment()], [gr_chunk_stats()], [gr_segmenters()]
#' @examples
#' ch <- gr_segment(gptread_example(), list(method = "sentence", max_tokens = 120))
#' ch$method
#' head(ch$chunks[, c("chunk_id", "tokens", "section")])
NULL


#' An ingested document
#'
#' Returned by [gr_ingest()].
#'
#' @section Fields:
#' \describe{
#'   \item{`blocks`}{Data frame of cleaned text blocks with provenance:
#'     `text`, `page`, `section`, `kind`, `block_id`. `page` is set only by the
#'     PDF extractor; `kind` is one of `"body"`, `"heading"`, `"code"`,
#'     `"table"`, `"ocr"`.}
#'   \item{`text`}{All blocks joined with blank lines.}
#'   \item{`source`}{Absolute path, or `"<inline text>"` when the input was a
#'     string rather than a file.}
#'   \item{`spec`}{The [gr_ingest_spec()] used.}
#'   \item{`stats`}{`blocks`, `chars`, `chars_removed`, `tokens`, `pages`,
#'     `clean_steps`, and `clean_log` (characters removed per cleaning step --
#'     useful when cleaning ate more than you expected).}
#' }
#'
#' @section Methods:
#' `print()` shows the source, block count and token total; [as_json()]
#' serialises the stats and every block with its provenance.
#' @seealso [gr_ingest()] which returns one, [gr_ingest_spec()],
#'   [gr_extractors()], [gr_cleaners()], [as_json()], [gr_segment()] for the
#'   next axis
#' @family ingest functions
#' @name gr_document
#' @seealso [gr_ingest()], [gr_ingest_spec()], [gr_cleaners()]
#' @examples
#' doc <- gr_ingest(gptread_example())
#' doc$stats$tokens
#' vapply(doc$stats$clean_log, function(s) s$chars_removed, integer(1))
NULL


#' The result of one model call
#'
#' Returned by [gr_call()]. Designed so a caller can never be handed `NULL` or
#' `character(0)` where a string is expected.
#'
#' @section Fields:
#' \describe{
#'   \item{`ok`}{Logical(1). `FALSE` for transport failures, HTTP errors, and
#'     **successful calls that returned no text** (a refusal or content filter).}
#'   \item{`text`}{Character(1), always. `""` when `ok` is `FALSE`. Never `NULL`,
#'     never `character(0)`, and newlines are preserved.}
#'   \item{`error`}{Character or `NULL`. What went wrong, including the HTTP body
#'     for API-side errors.}
#'   \item{`usage`}{List with `input` and `output` token counts as reported by
#'     the API.}
#'   \item{`status`, `finish_reason`, `retryable`, `model`, `raw`}{HTTP status,
#'     the API's stop reason, whether a retry could help, the model id, and the
#'     parsed response body.}
#' }
#'
#' @section Methods:
#' `print()` shows `ok`, the model, the token usage and either the text or the
#' error.
#' @seealso [gr_call()] which returns one, [gr_client()], [gr_mock_client()]
#' @name gr_result
#' @seealso [gr_call()], [gr_client()], [gr_mock_client()]
#' @examples
#' res <- gr_call(gr_mock_client(function(m, p) "hello"), "hi")
#' c(ok = res$ok, text = res$text)
NULL


#' Path to the bundled example document
#'
#' A short Markdown report with headings, figures and dates, used by the
#' examples in this package so they run without an API key or a file of your
#' own.
#'
#' @return Absolute path to the file.
#' @export
#' @examples
#' gr_ingest(gptread_example())
gptread_example <- function() {
  system.file("extdata", "annual_report.md", package = "gptread", mustWork = TRUE)
}
