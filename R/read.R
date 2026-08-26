# read.R -- AXIS 3: the public reading entry point and the distinctness check.

#' Register a reading strategy
#'
#' Axis 3 is a registry, so how the model is made to *read* a chunk set is
#' yours to define -- and a registered reader is a first-class one: it appears in
#' [gr_readers()], can be named in a [gr_recipe()] or an `ensemble`, and is
#' subject to the same pre-flight cost and call caps as the built-ins.
#'
#' @param name Reader name. Re-registering an existing name replaces it.
#' @param fn Function of `(chunks, question, client, spec, trace)` returning a
#'   `gr_answer`. See the section below.
#' @param signature Traversal signature, `"select|calls|state"`. Two readers with
#'   the same signature are the same methodology under two names; [gr_compare()]
#'   uses it, together with the ingest and segment specs, to decide whether two
#'   recipes are the same experiment, and `ensemble` refuses members that share
#'   one.
#' @param description One-line description, shown by [gr_readers()].
#' @param cost_calls Human-readable call count in terms of N chunks (`"N + 1"`,
#'   `"1 + embeddings"`), shown by [gr_readers()].
#' @return Invisibly, `name`.
#'
#' @section Writing a reader:
#' Your function receives:
#' \describe{
#'   \item{`chunks`}{A [gr_chunks]. `chunks$chunks` is the data frame:
#'     `chunk_id`, `text`, `tokens`, `chars`, `page`, `section`, `block_id`.}
#'   \item{`question`}{The question, already validated as non-blank.}
#'   \item{`client`}{Pass it to [gr_call()]; never construct your own.}
#'   \item{`spec`}{A [gr_read_spec()]. Honour at least `model`,
#'     `max_answer_tokens` and `temperature`.}
#'   \item{`trace`}{Pass it to every [gr_call()] so your calls are counted, and
#'     check `readgpt:::trace_can_call(trace)` before each one so the run cap is
#'     respected.}
#' }
#' Return [new_answer()]. Budget your prompt with [gr_budget()] rather than
#' assuming the document fits.
#'
#' @seealso [new_answer()] to build the return value, [gr_readers()],
#'   [gr_reader_signature()], [gr_read()], [gr_answer], [gr_budget()]
#' @family reading functions
#' @export
#' @examples
#' # A reader that answers from the single longest chunk.
#' gr_register_reader("longest", signature = "one|1|none", cost_calls = "1",
#'   description = "answer from the longest chunk only",
#'   fn = function(chunks, question, client, spec, trace) {
#'     d <- chunks$chunks
#'     i <- which.max(d$tokens)
#'     res <- gr_call(client, list(
#'       list(role = "user", content = paste0(d$text[i], "\n\nQuestion: ", question))),
#'       model = spec$model, trace = trace, label = "longest.answer")
#'     new_answer(res$text, "longest", question, d$chunk_id[i], trace)
#'   })
#'
#' ch <- gr_segment(readgpt_example(), list(method = "sentence", max_tokens = 120))
#' gr_read(ch, "What was revenue?", gr_mock_client(function(m, p) "45.2 million"),
#'         "longest")$answer
gr_register_reader <- function(name, fn, signature, description = "", cost_calls = "") {
  if (!is.function(fn)) gr_abort("`fn` must be a function of (chunks, question, client, spec, trace).")
  if (!grepl("^[^|]+\\|[^|]+\\|[^|]+$", as_chr1(signature))) {
    gr_abort("`signature` must have the form 'select|calls|state', e.g. 'topk|1|none'.")
  }
  registry_set("readers", name, list(name = name, fn = fn, signature = signature,
                                     description = description, cost_calls = cost_calls))
}

#' List registered reading strategies
#'
#' The catalogue for axis 3, and the cheapest way to choose one: the `signature`
#' column tells you how a reader traverses the chunks and `cost_calls` tells you
#' what that costs, both before you spend anything.
#'
#' @return A data frame with one row per registered reader: `name`, `signature`
#'   (see [gr_reader_signature()]), `cost_calls` -- a formula in N, the number
#'   of chunks, not a number -- and `description`.
#' @seealso [gr_read()], [gr_reader_signature()], [gr_register_reader()],
#'   [gr_read_spec()], [gr_compare()] to run several and compare
#' @family reading functions
#' @export
#' @examples
#' # Grouped by how they select chunks, which is the real taxonomy:
#' # `all|...` readers see every chunk, `topk|...` readers see a selection.
#' r <- gr_readers()
#' r[order(r$signature), c("name", "signature", "cost_calls")]
gr_readers <- function() registry_table("readers", c("signature", "cost_calls", "description"))

#' The traversal signature of a reader
#'
#' The signature is how the package tells two reading *methodologies* apart from
#' two names for the same thing. It encodes which chunks reach the model
#' (`select`), the call pattern (`calls`), and whether information flows between
#' calls (`state`).
#'
#' @param reader Reader name, or a `gr_read_spec`.
#' @return A single string `"select|calls|state"`, e.g. `"topk|1|none"`. For
#'   `ensemble` the member list is appended in braces
#'   (`"ensemble|sum+1|none{map_reduce+retrieve}"`), so two ensembles with
#'   different members are correctly seen as different experiments.
#' @seealso [gr_readers()] for every signature at once, [gr_compare()] which
#'   uses this to detect duplicate recipes, [gr_register_reader()]
#' @family reading functions
#' @export
#' @examples
#' # Same document, different traversal: one call over everything, versus one
#' # call per chunk plus merges.
#' gr_reader_signature("stuff")
#' gr_reader_signature("map_reduce")
#'
#' # An ensemble's signature carries its members, so two differently-composed
#' # ensembles are not mistaken for each other.
#' gr_reader_signature(gr_read_spec("ensemble", members = c("retrieve", "refine")))
gr_reader_signature <- function(reader) {
  if (inherits(reader, "gr_read_spec")) {
    base <- registry_get("readers", reader$reader, "readers")$signature
    if (identical(reader$reader, "ensemble")) {
      base <- paste0(base, "{", paste(sort(reader$members %||% character(0)), collapse = "+"), "}")
    }
    return(base)
  }
  registry_get("readers", as_chr1(reader), "readers")$signature
}

#' Describe a reading configuration
#'
#' @param reader Reader name; see `gr_readers()`.
#' @param model Chat model id.
#' @param temperature Sampling temperature, or `NULL` to omit the field. You do
#'   not need to null it yourself for reasoning models: it is dropped
#'   automatically for any model whose registry entry has
#'   `supports_temperature = FALSE`, which includes the default model. Check with
#'   `gr_model_info(model)$supports_temperature`.
#' @param max_answer_tokens Output cap for final answers and merges.
#' @param max_chunk_tokens Output cap for per-chunk calls.
#' @param max_summary_tokens Output cap for summarisation calls.
#' @param top_k For `retrieve`, `rerank` and `iterative`: chunks to use.
#' @param min_score For `retrieve`: chunks scoring below this are dropped, but
#'   if that would leave nothing the single best chunk is used anyway. `-Inf`
#'   disables the filter. Not a cosine similarity when embeddings fall back to
#'   lexical vectors -- the score is then a blend of cosine and BM25.
#' @param rerank_candidates,rerank_min_score For `rerank`: how many chunks to
#'   score, and the score below which a chunk is discarded.
#' @param fan_in,max_levels For `hierarchical`: summaries combined per call, and
#'   the recursion depth cap.
#' @param max_rounds For `iterative`: retrieve-assess cycles.
#' @param members For `ensemble`: the reader names to combine.
#' @param cite Ask for chunk-level citations (`[chunk 3]`) in the answer. Map
#'   those ids back to pages via `ans$evidence`. Forced off for `hierarchical`,
#'   which answers from summaries: summaries carry no `[chunk N]` ids, so asking
#'   for citations there asks the model to invent them.
#' @param skim_model,summary_model Optional cheaper models for the per-chunk
#'   stages. `skim_model` is used by `skim`'s extraction **and** `rerank`'s
#'   relevance scoring; `summary_model` by `hierarchical`'s summarisation.
#' @param parallel Run per-chunk calls in parallel.
#' @param delay_between_calls Seconds to sleep between sequential calls, for
#'   rate-limit shaping. Honoured by `map_reduce`, `refine` and `skim`; the
#'   other readers do not sleep.
#' @param on_overflow For `stuff`: `"warn"` (truncate and say so) or `"error"`.
#' @param ... Extra fields for custom readers.
#' @return A list of class `gr_read_spec`.
#'
#' @section Out-of-range values:
#' Numeric arguments are clamped into a usable range and the change is warned
#' about, never applied silently: `top_k` \[1, 1e4\], `fan_in` \[2, 32\],
#' `max_levels` \[1, 12\], `max_rounds` \[1, 20\], `rerank_candidates` \[1, 1e4\],
#' `rerank_min_score` \[0, 10\], `delay_between_calls` \[0, 600\],
#' token caps \[16, 1e6\].
#'
#' @seealso [gr_readers()] for the available readers and their call costs,
#'   [gr_read()], [gr_recipe()]
#' @family reading functions
#' @export
#' @examples
#' gr_read_spec("retrieve", top_k = 8)$top_k
#'
#' # Out-of-range settings are corrected loudly, not quietly.
#' suppressWarnings(gr_read_spec("hierarchical", fan_in = 999)$fan_in)
gr_read_spec <- function(reader = "map_reduce", model = NULL, temperature = NULL,
                         max_answer_tokens = 1500L, max_chunk_tokens = 700L,
                         max_summary_tokens = 500L, top_k = 6L, min_score = -Inf,
                         rerank_candidates = 20L, rerank_min_score = 4,
                         fan_in = 5L, max_levels = 5L, max_rounds = 4L,
                         members = NULL, cite = FALSE,
                         skim_model = NULL, summary_model = NULL,
                         parallel = NULL, delay_between_calls = 0,
                         on_overflow = c("warn", "error"), ...) {
  on_overflow <- match.arg(on_overflow)
  spec <- structure(c(list(
    reader = reader,
    model = as_chr1(model %||% gr_options("model")),
    temperature = temperature %||% gr_options("temperature"),
    max_answer_tokens = clamp_warn(max_answer_tokens, 16, 1e6, "max_answer_tokens"),
    max_chunk_tokens = clamp_warn(max_chunk_tokens, 16, 1e6, "max_chunk_tokens"),
    max_summary_tokens = clamp_warn(max_summary_tokens, 16, 1e6, "max_summary_tokens"),
    top_k = clamp_warn(top_k, 1, 1e4, "top_k"),
    min_score = min_score,
    rerank_candidates = clamp_warn(rerank_candidates, 1, 1e4, "rerank_candidates"),
    rerank_min_score = clamp_warn(rerank_min_score, 0, 10, "rerank_min_score", integer = FALSE),
    fan_in = clamp_warn(fan_in, 2, 32, "fan_in"),
    max_levels = clamp_warn(max_levels, 1, 12, "max_levels"),
    max_rounds = clamp_warn(max_rounds, 1, 20, "max_rounds"),
    members = members,
    cite = isTRUE(cite),
    skim_model = skim_model,
    summary_model = summary_model,
    parallel = parallel %||% gr_options("parallel"),
    delay_between_calls = clamp_warn(delay_between_calls, 0, 600, "delay_between_calls",
                                     integer = FALSE),
    on_overflow = on_overflow
  ), list(...)), class = "gr_read_spec")
  registry_get("readers", spec$reader, "readers")   # fail fast on a typo
  spec
}

#' Read chunks and answer a question
#'
#' The third axis. Reading is a separate decision from segmentation because the
#' call pattern -- which chunks reach the model, in how many requests, and
#' whether anything flows between them -- is where both cost and answer quality
#' are actually decided. The same chunk set can be read nine ways;
#' [gr_readers()] lists them with what each costs.
#'
#' @param chunks A `gr_chunks` object from `gr_segment()`.
#' @param question The question.
#' @param client A `gr_client`.
#' @param spec A `gr_read_spec`, a reader name, or a named list.
#' @param trace Optional `gr_trace`; one is created when omitted.
#' @return A [gr_answer]. Check `$partial` before trusting `$answer`.
#' @seealso [gr_readers()], [gr_read_spec()], [gr_answer], [answer_document()]
#' @family reading functions
#' @export
#' @examples
#' ch <- gr_segment(readgpt_example(), list(method = "sentence", max_tokens = 120))
#'
#' # The same chunks, two strategies, two very different call patterns. Each gets
#' # its own client and trace, so the call counts are comparable.
#' run <- function(reader, ...) {
#'   cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
#'   tr <- gr_trace()
#'   a <- gr_read(ch, "What was revenue?", cl, c(list(reader = reader), list(...)), trace = tr)
#'   data.frame(reader = a$reader, signature = a$signature,
#'              calls = length(cl$calls()), chunks_used = length(a$chunks_used))
#' }
#' rbind(run("retrieve", top_k = 2), run("map_reduce"))
gr_read <- function(chunks, question, client, spec = NULL, trace = NULL) {
  if (!inherits(chunks, "gr_chunks")) gr_abort("`chunks` must come from gr_segment().")
  if (!is_nonblank(question)) gr_abort("`question` must be a non-empty string.")
  if (!inherits(client, "gr_client")) gr_abort("`client` must come from gr_client() or gr_mock_client().")
  spec <- as_read_spec(spec)
  trace <- trace %||% gr_trace(meta = list(reader = spec$reader))
  rd <- registry_get("readers", spec$reader, "readers")

  gr_msg(sprintf("Reading with '%s' (%s) over %d chunk(s).",
                 spec$reader, rd$signature, nrow(chunks$chunks)))
  preflight(chunks, spec, trace)

  out <- rd$fn(chunks, question, client, spec, trace)
  if (!inherits(out, "gr_answer")) {
    gr_abort(sprintf("Reader '%s' did not return a gr_answer object.", spec$reader))
  }
  if (isTRUE(trace$budget_stop)) {
    out$partial <- TRUE
    out$notes$call_cap_reached <- gr_options("max_calls")
  }
  out$signature <- rd$signature
  out
}

#' Pre-flight cost and call-count guard.
#'
#' The previous release had nothing like this, which is why an unrecognised
#' model name could silently turn a 10,000-word document into ~10,001 API calls.
#' @noRd
preflight <- function(chunks, spec, trace) {
  n <- nrow(chunks$chunks)
  est_calls <- switch(spec$reader,
    stuff = 1L, retrieve = 1L,
    map_reduce = n + ceiling(n / 5),
    refine = n, skim = n + 1L,
    rerank = min(spec$rerank_candidates, n) + 1L,
    hierarchical = n + ceiling(n / spec$fan_in) + 1L,
    iterative = spec$max_rounds * 2L,
    ensemble = 1L + sum(vapply(spec$members %||% c("retrieve", "map_reduce"),
                               function(m) as.integer(switch(m,
                                 stuff = 1L, retrieve = 1L, refine = n, skim = n + 1L,
                                 map_reduce = n + ceiling(n / 5),
                                 rerank = min(spec$rerank_candidates, n) + 1L,
                                 iterative = spec$max_rounds * 2L,
                                 hierarchical = n + ceiling(n / spec$fan_in) + 1L,
                                 n)), integer(1))),
    n)
  est_calls <- as.integer(est_calls)
  # `gr_options(max_calls = NULL)` is the documented way to remove the cap, and
  # NULL is genuinely storable. `is.finite(NULL)` is logical(0), so the `if`
  # below failed with "argument is of length zero" -- every read aborted with an
  # internal error the moment a user turned the cap off.
  cap <- as_num1(gr_options("max_calls"), Inf)
  # The cap applies to the RUN, so calls already made in this trace count --
  # otherwise a comparison of six recipes could pass six individually-fine
  # pre-flight checks and still blow the budget.
  already <- if (inherits(trace, "gr_trace")) trace$calls else 0L
  if (is.finite(cap) && (already + est_calls) > cap) {
    gr_abort(sprintf(paste0("Reader '%s' over %d chunks would need about %d more model calls ",
                            "(%d already made this run), above the %d-call cap. Use a larger ",
                            "`max_tokens` when segmenting (fewer chunks), pick a top-k reader, ",
                            "or raise gr_options(max_calls = ...)."),
                     spec$reader, n, est_calls, already, cap), class = "gr_call_cap")
  }
  est_in <- sum(chunks$chunks$tokens) + est_calls * 200L
  # Size the completion estimate by the LARGER of the two caps: only the
  # per-chunk readers use max_chunk_tokens, and the rest size their answers with
  # max_answer_tokens, so using the per-chunk cap under-estimated output by
  # whatever ratio the user chose -- measured at 97x in one configuration.
  est_out <- est_calls * max(spec$max_chunk_tokens, spec$max_answer_tokens,
                             spec$max_summary_tokens)
  cost <- gr_estimate_cost(spec$model, est_in, est_out)
  budget <- gr_options("max_cost_usd")
  if (is.na(cost) && !is.null(budget) && is.finite(budget)) {
    # No pricing for this model, so the cap cannot be enforced. Say so rather
    # than silently running with no spending guard at all.
    gr_warn(sprintf(paste0("Model '%s' has no pricing in the registry, so the $%.2f ",
                           "max_cost_usd cap cannot be checked for this run. Register prices ",
                           "with gr_register_model(input_usd =, output_usd =) to enable it."),
                    spec$model, budget), class = "gr_cost_uncheckable")
  }
  if (!is.na(cost) && !is.null(budget) && is.finite(budget) && cost > budget) {
    gr_abort(sprintf(paste0("Estimated worst-case cost for '%s' over %d chunks is about $%.2f ",
                            "(~%d calls), above the $%.2f limit. Raise it with ",
                            "gr_options(max_cost_usd = ...) or choose a cheaper configuration."),
                     spec$reader, n, cost, est_calls, budget), class = "gr_cost_cap")
  }
  trace_note(trace, "preflight", list(reader = spec$reader, chunks = n,
                                      est_calls = est_calls,
                                      est_cost_usd = if (is.na(cost)) NULL else round(cost, 4)))
  invisible(NULL)
}

#' @noRd
as_read_spec <- function(spec) {
  if (inherits(spec, "gr_read_spec")) return(spec)
  if (is.null(spec)) return(gr_read_spec())
  if (is.character(spec) && length(spec) == 1L) return(gr_read_spec(reader = spec))
  if (is.list(spec)) return(do.call(gr_read_spec, spec))
  gr_abort("`read` must be a gr_read_spec, a reader name, or a named list.")
}
