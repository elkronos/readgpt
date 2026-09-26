# read.R -- AXIS 3: the public reading entry point and the distinctness check.

#' Register a reading strategy
#'
#' Axis 3 is a registry, so how the model is made to *read* a chunk set is
#' yours to define. A registered reader is a first-class one: it appears in
#' [gr_readers()], can be named in a [gr_recipe()] or an `ensemble`, and is
#' subject to the same call and spending limits as the built-ins.
#'
#' @param name Reader name. Re-registering an existing name replaces it.
#'   `"auto"` is reserved for [answer_document()].
#' @param fn Function of `(chunks, question, client, spec, trace)` returning a
#'   `gr_answer`. See the section below.
#' @param signature Traversal signature, `"select|calls|state"`. Two readers with
#'   the same signature are the same methodology under two names; [gr_compare()]
#'   uses it, together with the ingest and segment specs, to decide whether two
#'   recipes are the same experiment, and `ensemble` refuses members that share
#'   one. A `select` of `"all"` says the reader sends every chunk, so a run
#'   whose chunks alone would cost more than `max_cost_usd` is refused before
#'   its first request.
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
#'     `max_answer_tokens` and `temperature`. [gr_read()] has already filled
#'     `model` from the client when the spec named none.}
#'   \item{`trace`}{Pass it to every [gr_call()] so your calls are counted and
#'     priced, and check `readgpt:::trace_can_call(trace)` before each one so
#'     the run's call and spending limits are respected.}
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
  # answer_document(recipe = "auto") would never reach a reader of that name.
  if (identical(as_chr1(name, ""), "auto")) {
    gr_abort(paste0("'auto' is reserved: answer_document() uses it to choose a recipe. ",
                    "Register the reader under another name."), class = "gr_reserved_name")
  }
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
#'   (see [gr_reader_signature()]), `cost_calls` (a formula in N, the number
#'   of chunks, not a number) and `description`.
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
#' @param model Chat model id. `NULL` (the default) means the model of the
#'   client the spec is read with, so `gr_client(model = "gpt-4o-mini")` is the
#'   model that answers, is budgeted for and is billed. A model named here, or
#'   in a recipe, is used whatever the client's.
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
#'   lexical vectors: the score is then a blend of cosine and BM25.
#' @param mmr Diversity of selection, for `retrieve` and `iterative`. `1` (the
#'   default) is plain top-k. Below 1, chunks are picked greedily by
#'   `mmr * relevance - (1 - mmr) * similarity to what is already picked`, so
#'   three chunks saying the same thing do not all get in and pay for each other.
#'   Costs nothing: the vectors are already computed. `0.7` is a reasonable
#'   place to start; `0` selects for novelty alone and will happily pick
#'   irrelevant chunks because they are different.
#' @param restate Whether to repeat the question before the excerpts as well as
#'   after them: `"auto"` when the body is long enough to bury the first ask,
#'   `"always"`, or `"never"`. A setting rather than a rule, because whether it
#'   helps is a question about your corpus and your model. Point [gr_compare()]
#'   at two recipes differing only in this and find out.
#' @param context_order Where the selected chunks sit in the prompt.
#'   `"relevance"` (default) is most relevant first; `"document"` restores the
#'   order they appear in the document, which reads better when chunks are
#'   consecutive; `"edges"` puts the strongest first and second-strongest last,
#'   burying the weakest in the middle, because transformers attend measurably
#'   better to the beginning and end of a long context than to its middle.
#'   Selection is unaffected. This decides only placement, and it applies to
#'   `retrieve` and `rerank`, the two readers that put several ranked chunks in
#'   one prompt.
#' @param rerank_candidates,rerank_min_score For `rerank`: how many chunks to
#'   score, and the score below which a chunk is discarded. The candidates are
#'   the chunks the word-matching prefilter ranks highest. When no chunk shares
#'   a word with the question, it cannot rank them, so they are picked by
#'   embedding similarity instead, or, when embeddings cannot rank them either,
#'   spread evenly over the document and the answer marked partial. Either way
#'   the run warns (`gr_rerank_prefilter`) and `notes$prefilter` says which.
#' @param fan_in,max_levels For `hierarchical`: summaries combined per call, and
#'   the recursion depth cap.
#' @param max_rounds For `iterative`: retrieve-assess cycles.
#' @param preview_tokens For `preview`: the cap on the outline the planner sees.
#'   The outline is built from section labels, sizes and short excerpts (never
#'   the full text), and per-section excerpts shrink until the whole thing fits,
#'   so every section stays visible to the planner rather than the outline being
#'   truncated and some sections never being offered to it at all. The planner is
#'   an LLM call about a long document and so prone to exactly the degradation
#'   this package manages; keeping its input small is the mitigation.
#' @param members For `ensemble`: the reader names to combine.
#' @param cite Ask for chunk-level citations (`[chunk 3]`) in the answer. Map
#'   those ids back to pages via `ans$evidence`. Forced off for `hierarchical`,
#'   which answers from summaries: summaries carry no `[chunk N]` ids, so asking
#'   for citations there asks the model to invent them.
#' @param skim_model,summary_model Optional cheaper models for the per-chunk
#'   stages. `skim_model` is used by `skim`'s extraction **and** `rerank`'s
#'   relevance scoring; `summary_model` by `hierarchical`'s summarisation.
#' @param parallel Run per-chunk calls in parallel. Needs the `future` and
#'   `future.apply` packages; without them the run is sequential and says so.
#'
#'   The trace is complete either way: workers keep their own and the parent
#'   absorbs them, in input order, so a parallel run reports the same calls,
#'   tokens and cost as the same run made sequentially. Two things do not cross
#'   the process boundary. The limits in [gr_options()] are checked before a
#'   batch is sent and not inside it, since a worker cannot see what the others
#'   spend. So the pre-flight check, which runs in the parent, holds a parallel
#'   run to its worst case, every reply at its cap and as many merge or
#'   summary levels as replies that size need, against both `max_calls` and
#'   `max_cost_usd`; and each batch goes to the workers only when it fits what
#'   the run has left at its own worst case, and otherwise runs one request at
#'   a time, each checked. And a client that keeps its
#'   own log in a closure, such as [gr_mock_client()], only sees the calls made
#'   in this process; ask the trace instead.
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
#' `rerank_min_score` \[0, 10\], `preview_tokens` \[100, 1e5\],
#' `delay_between_calls` \[0, 600\],
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
                         mmr = 1, context_order = c("relevance", "document", "edges"),
                         rerank_candidates = 20L, rerank_min_score = 4,
                         fan_in = 5L, max_levels = 5L, max_rounds = 4L,
                         preview_tokens = 1200L, restate = c("auto", "always", "never"),
                         members = NULL, cite = FALSE,
                         skim_model = NULL, summary_model = NULL,
                         parallel = NULL, delay_between_calls = 0,
                         on_overflow = c("warn", "error"), ...) {
  on_overflow <- match.arg(on_overflow)
  context_order <- match.arg(context_order)
  restate <- match.arg(restate)
  warn_near_miss(list(...), names(formals(gr_read_spec)), "read")
  spec <- structure(c(list(
    reader = reader,
    # NULL is kept, not filled from gr_options("model"): filled here, every
    # reader passed a model to gr_call(), which only falls back to the client's
    # when given none, so a client built for another model was never the one
    # asked, budgeted for or billed. gr_read() fills it from the client.
    model = if (is.null(model)) NULL else as_chr1(model),
    temperature = temperature %||% gr_options("temperature"),
    max_answer_tokens = clamp_warn(na_default(max_answer_tokens, 1500L, "max_answer_tokens"), 16, 1e6, "max_answer_tokens"),
    max_chunk_tokens = clamp_warn(na_default(max_chunk_tokens, 700L, "max_chunk_tokens"), 16, 1e6, "max_chunk_tokens"),
    max_summary_tokens = clamp_warn(na_default(max_summary_tokens, 500L, "max_summary_tokens"), 16, 1e6, "max_summary_tokens"),
    top_k = clamp_warn(na_default(top_k, 6L, "top_k"), 1, 1e4, "top_k"),
    # -Inf is the documented "no floor", so a value that cannot be compared has
    # to be refused rather than silently becoming one: a relevance floor that is
    # not applied is the same failure as a cost ceiling that is not enforced.
    min_score = if (identical(min_score, -Inf)) -Inf else
      na_default(min_score, -Inf, "min_score"),
    # NA falls back to the DEFAULT, not to the bottom of the range. clamp_warn()
    # maps NA to `lo`, which for this setting is 0 -- pure diversity, documented
    # as "will happily pick irrelevant chunks because they are different". A
    # missing value must not select the most destructive end of a scale.
    mmr = clamp_warn(na_default(mmr, 1, "mmr"), 0, 1, "mmr", integer = FALSE),
    context_order = context_order,
    restate = restate,
    rerank_candidates = clamp_warn(na_default(rerank_candidates, 20L, "rerank_candidates"), 1, 1e4, "rerank_candidates"),
    rerank_min_score = clamp_warn(na_default(rerank_min_score, 4, "rerank_min_score"), 0, 10, "rerank_min_score", integer = FALSE),
    fan_in = clamp_warn(na_default(fan_in, 5L, "fan_in"), 2, 32, "fan_in"),
    max_levels = clamp_warn(na_default(max_levels, 5L, "max_levels"), 1, 12, "max_levels"),
    max_rounds = clamp_warn(na_default(max_rounds, 4L, "max_rounds"), 1, 20, "max_rounds"),
    preview_tokens = clamp_warn(na_default(preview_tokens, 1200L, "preview_tokens"), 100, 1e5,
                               "preview_tokens"),
    members = members,
    cite = isTRUE(cite),
    skim_model = skim_model,
    summary_model = summary_model,
    parallel = parallel %||% gr_options("parallel"),
    delay_between_calls = clamp_warn(na_default(delay_between_calls, 0, "delay_between_calls"),
                                     0, 600, "delay_between_calls", integer = FALSE),
    on_overflow = on_overflow
  ), list(...)), class = "gr_read_spec")
  registry_get("readers", spec$reader, "readers")   # fail fast on a typo
  spec
}

#' Read chunks and answer a question
#'
#' The third axis. Reading is a separate decision from segmentation because the
#' call pattern (which chunks reach the model, in how many requests, and
#' whether anything flows between them) is where both cost and answer quality
#' are actually decided. The same chunk set can be read many ways;
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
  # What the caller configured, for the trace: a model that merely follows the
  # client is not a setting, as one that followed gr_options() was not.
  settings <- read_settings(spec)
  spec <- resolve_read_model(spec, client)
  trace <- trace %||% gr_trace(meta = list(reader = spec$reader))
  rd <- registry_get("readers", spec$reader, "readers")

  gr_msg(sprintf("Reading with '%s' (%s) over %d chunk(s).",
                 spec$reader, rd$signature, nrow(chunks$chunks)))
  # A limit that stopped an earlier read on the same trace did not stop this
  # one. The trace keeps that record unless this read is stopped too.
  if (inherits(trace, "gr_trace")) {
    earlier <- list(stop = isTRUE(trace$budget_stop), reason = trace$stop_reason %||% NA_character_)
    trace$budget_stop <- FALSE
    trace$stop_reason <- NA_character_
    on.exit(if (!isTRUE(trace$budget_stop)) {
      trace$budget_stop <- earlier$stop
      trace$stop_reason <- earlier$reason
    }, add = TRUE)
  }
  rec <- warning_recorder()
  out <- withCallingHandlers({
    preflight(chunks, spec, trace, client = client, question = question, settings = settings)
    rd$fn(chunks, question, client, spec, trace)
  }, gr_warning = rec$record)
  if (!inherits(out, "gr_answer")) {
    gr_abort(sprintf("Reader '%s' did not return a gr_answer object.", spec$reader))
  }
  if (isTRUE(trace$budget_stop)) {
    out$partial <- TRUE
    if (identical(trace$stop_reason, "cost")) {
      out$notes$cost_cap_reached <- gr_options("max_cost_usd")
    } else {
      out$notes$call_cap_reached <- gr_options("max_calls")
    }
  }
  # Pages that never became text are missing from every chunk, so nothing this
  # reader did could have seen them. The answer rests on part of the document.
  unread <- chunks[["unread_pages", exact = TRUE]] %||% integer(0)
  if (length(unread)) {
    out$partial <- TRUE
    out$notes["unread_pages"] <- list(unread)
  }
  out$warnings <- c(chunks[["warnings", exact = TRUE]] %||% character(0), rec$get())
  out$signature <- rd$signature
  out
}

#' Pre-flight cost and call-count guard.
#'
#' The previous release had nothing like this, which is why an unrecognised
#' model name could silently turn a 10,000-word document into ~10,001 API calls.
#'
#' Only what is known refuses a run: how many requests the reader will make,
#' and, for a reader that sends every chunk, what sending them costs. What the
#' replies will cost is not known, and neither is which chunks a top-k reader
#' will pick. The old check priced every reply at its token cap and refused a
#' run at an estimate of $5.54 that cost $0.11. Now a run is refused before it
#' starts only when it cannot finish under `max_cost_usd`, and
#' `trace_can_call()` stops any run once its spending reaches the limit. The
#' worst case is still recorded in the trace, and still refuses a parallel
#' read: a batch sent to workers cannot be stopped part way through.
#'
#' `client` is what the run is billed through: an ellmer chat bills every call
#' as its own model whatever the request names. `question` sizes the merge and
#' summary levels of the worst case, as the readers size them.
#' @noRd
preflight <- function(chunks, spec, trace, client = NULL, question = "",
                      settings = read_settings(spec)) {
  n <- nrow(chunks$chunks)
  readers <- if (identical(spec$reader, "ensemble")) spec$members %||% c("retrieve", "map_reduce")
             else spec$reader
  ensemble <- identical(spec$reader, "ensemble")
  # Requests to the embeddings endpoint count toward max_calls too (see
  # embed_api()). Left out, a cap too small for them passed this check, and
  # the run stopped part way: its embeddings refused, a lexical fallback in
  # their place, and the answer never asked for.
  embeds <- embed_requests(client, readers, chunks$chunks$text, question, spec)
  # The model requests one reader is expected to make; with its embedding
  # requests, what refuses a run against max_calls. One function for a reader
  # alone and as an ensemble member, so the two estimates cannot drift apart.
  model_calls_for <- function(r) as.integer(switch(r,
    stuff = 1L, retrieve = 1L,
    # `screen` reads the opening of the document in ONE call whatever its size.
    # Falling through to the default `n` made the pre-flight warn about the cost
    # of forty calls for a job that costs one, on every long document.
    screen = 1L,
    map_reduce = n + ceiling(n / 5),
    refine = n, skim = n + 1L,
    extract = n,
    rerank = min(spec$rerank_candidates, n) + 1L,
    hierarchical = n + ceiling(n / spec$fan_in) + 1L,
    # A step a round and the answer after them. This was max_rounds * 2, which
    # stood for a query embedding and a step a round; embeddings are counted
    # apart now, the chunks' included, which it never counted.
    iterative = spec$max_rounds + 1L,
    n))
  calls_for <- function(r) model_calls_for(r) + embeds[[r]]
  est_calls <- if (ensemble) 1L + sum(vapply(readers, calls_for, integer(1)))
               else calls_for(spec$reader)
  est_calls <- as.integer(est_calls)
  embed_calls <- as.integer(sum(embeds))

  # The reader warns about an unrecognised model itself; once is enough.
  quiet_model <- function(expr) suppressWarnings(expr, classes = "gr_unknown_model")
  # The worst case: every reply at its cap, and as many levels as replies that
  # size take to fit the window. `hierarchical` recurses up to max_levels, and
  # map_reduce's merge and skim's consolidation are tree_merge() trees. Counted
  # from `n` alone, a parallel run's "worst case" held two of hierarchical's
  # five levels and none of a merge tree's, and the run spent 14% past a bound
  # it was said to be held to.
  out_cap <- function(m, cap) {
    mo <- tryCatch(as.numeric(quiet_model(gr_model_info(m))$max_output),
                   error = function(e) NA_real_)
    as.numeric(if (length(mo) != 1L || is.na(mo)) cap else min(cap, mo))
  }
  room <- function(system, restate = "never") tryCatch(as.numeric(quiet_model(gr_budget(
    spec$model, reserve_output = spec$max_answer_tokens,
    overhead = prompt_overhead(question, system, restate)))$input), error = function(e) NA_real_)
  worst_for <- function(r) {
    base <- list(calls = calls_for(r), input = 0)
    got <- switch(r,
      map_reduce = {
        m <- merge_tree_worst(n, out_cap(spec$model, spec$max_chunk_tokens),
                              out_cap(spec$model, spec$max_answer_tokens),
                              room(.gr_prompts$merge_system))
        if (!is.null(m)) list(calls = n + m$calls, input = m$input)
      },
      skim = {
        piece <- out_cap(spec$skim_model %||% spec$model, spec$max_chunk_tokens)
        fit <- room(answer_system(spec$cite), spec$restate)
        body <- n * piece
        m <- if (is.na(fit)) NULL
             else if (body <= fit) list(calls = 0, input = 0)
             else merge_tree_worst(n, piece, out_cap(spec$model, spec$max_answer_tokens),
                                   room(.gr_prompts$summarise_system))
        if (!is.null(m)) list(calls = n + m$calls + 1, input = m$input + min(body, fit))
      },
      hierarchical = summary_levels_worst(
        n, out_cap(spec$summary_model %||% spec$model, spec$max_summary_tokens),
        room(answer_system(FALSE), spec$restate), spec$fan_in, spec$max_levels),
      NULL)
    got %||% base
  }
  worst_parts <- lapply(readers, worst_for)
  # Never below the expected count: a worst case under the estimate is not one.
  part_calls <- vapply(worst_parts, function(w) as.numeric(w$calls), numeric(1))
  worst_calls <- as.integer(max(est_calls, (if (ensemble) 1 else 0) + sum(part_calls)))
  level_in <- sum(vapply(worst_parts, function(w) as.numeric(w$input), numeric(1)))

  # Only readers that send batches: stuff, retrieve and screen make one
  # request, and refine and iterative make theirs one at a time, each checked.
  batches <- function(r) !r %in% c("stuff", "retrieve", "screen", "refine", "iterative")
  parallel_read <- isTRUE(spec$parallel) && any(vapply(readers, batches, logical(1))) &&
    requireNamespace("future", quietly = TRUE) && requireNamespace("future.apply", quietly = TRUE)

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
    gr_abort(sprintf(paste0("Reader '%s' over %d chunks would need about %d more %s ",
                            "(%s%d already made this run), above the %s-call cap. Use a larger ",
                            "`max_tokens` when segmenting (fewer chunks), pick a top-k reader, ",
                            "or raise gr_options(max_calls = ...)."),
                     spec$reader, n, est_calls,
                     if (embed_calls > 0L) "requests" else "model calls",
                     if (embed_calls > 0L) sprintf("%d of them for embeddings; ", embed_calls)
                     else "",
                     already, format(cap, scientific = FALSE)),
             class = "gr_call_cap")
  }
  # A parallel batch is not stopped part way, so a parallel read is held to its
  # worst case against the call cap as it is against the spending limit.
  if (is.finite(cap) && parallel_read && (already + worst_calls) > cap) {
    gr_abort(sprintf(paste0("With parallel = TRUE, requests go out in batches that cannot be ",
                            "stopped part way, so a parallel read is held to its worst case: ",
                            "'%s' over %d chunks can need up to %d more model calls (%d already ",
                            "made this run), above the %s-call cap. Raise it with ",
                            "gr_options(max_calls = ...), or read without parallel = TRUE, where ",
                            "the cap is checked before every request."),
                     spec$reader, n, worst_calls, already, format(cap, scientific = FALSE)),
             class = "gr_call_cap")
  }
  tok <- sum(chunks$chunks$tokens)
  # Priced below as model requests: an embedding request has no reply, and
  # what it sends goes at the embedding model's price, a small fraction of the
  # chat model's.
  model_worst <- worst_calls - embed_calls
  # What the worst case sends: every chunk, the fixed prompt of every request,
  # and what the merge and summary levels are handed, which is replies at
  # their caps.
  est_in <- tok + model_worst * 200L + level_in
  # Size the completion estimate by the LARGER of the two caps: only the
  # per-chunk readers use max_chunk_tokens, and the rest size their answers with
  # max_answer_tokens, so using the per-chunk cap under-estimated output by
  # whatever ratio the user chose -- measured at 97x in one configuration.
  # This is the worst case, recorded below; it refuses only a parallel read.
  est_out <- model_worst * max(spec$max_chunk_tokens, spec$max_answer_tokens,
                               spec$max_summary_tokens)
  # An ellmer chat answers, and is billed, as the model it was built with,
  # whatever a request names, and the trace prices its calls by that model. So
  # that is the model to price: pricing the recipe's instead found a price,
  # raised no warning, and ran a limit that the calls, billed as a model with
  # no registered price, could never reach.
  billed <- client_billed_model(client)
  # Every request at the price of the dearest model the run uses: skim_model and
  # summary_model take most of the requests of the readers that use them, and a
  # bound priced at `model` alone understated a run whose per-chunk model was
  # the dear one.
  models <- billed %||% unique(c(spec$model, spec$skim_model, spec$summary_model))
  priced_worst <- vapply(models, function(m) as.numeric(quiet_model(
    gr_estimate_cost(m, est_in, est_out))), numeric(1))
  # The dearest PRICED model. One model without a price made max() NA, and the
  # parallel refusal below, which needs a number, was then skipped for the whole
  # run, the priced model that receives the batch included. Only a run with no
  # priced model at all has no worst case.
  worst <- if (all(is.na(priced_worst))) NA_real_ else max(priced_worst, na.rm = TRUE)
  # What sending every chunk once costs, for each reader in the run that sends
  # them all, at the price of the model that receives them: skim and extract
  # send chunks to skim_model and hierarchical to summary_model, which exist to
  # be cheaper. stuff sends what fits in one request. A reader that picks chunks
  # adds nothing, since what it sends is not known until it has ranked them.
  sends_all <- function(r) startsWith(as_chr1(tryCatch(
    registry_get("readers", r, "readers")$signature, error = function(e) ""), ""), "all|")
  floor_usd <- function(r) {
    to <- billed %||% switch(r, skim = , extract = spec$skim_model %||% spec$model,
                             hierarchical = spec$summary_model %||% spec$model, spec$model)
    sent <- tok
    if (identical(r, "stuff")) {
      room <- tryCatch(quiet_model(gr_budget(spec$model,
                                             reserve_output = spec$max_answer_tokens)$input),
                       error = function(e) tok)
      sent <- min(tok, room)
    }
    as.numeric(quiet_model(gr_estimate_cost(to, sent, 0)))
  }
  sending <- readers[vapply(readers, sends_all, logical(1))]
  every_chunk <- length(sending) > 0L
  floors <- vapply(sending, floor_usd, numeric(1))
  # The same rule as `worst`: an ensemble member read by an unpriced model has
  # an unknown share, and summing it made the whole floor NA and waived the
  # refusal for the members whose share is known.
  input_cost <- if (!every_chunk) 0 else if (all(is.na(floors))) NA_real_
                else sum(floors, na.rm = TRUE)
  budget <- gr_options("max_cost_usd")
  if (!is.null(budget) && is.finite(budget)) {
    limit <- format(budget, scientific = FALSE)
    # No pricing for a model, so what its requests cost is not counted. Say so
    # rather than silently running with no spending guard at all.
    unit <- vapply(models, function(m) as.numeric(quiet_model(gr_estimate_cost(m, 1, 1))),
                   numeric(1))
    unpriced <- models[is.na(unit)]
    if (length(unpriced)) {
      gr_warn(sprintf(paste0("%s no pricing in the registry, so what %s requests cost cannot be ",
                             "counted against the $%s max_cost_usd limit. Register prices with ",
                             "gr_register_model(input_usd =, output_usd =) to enable it."),
                      if (length(unpriced) == 1L) sprintf("Model '%s' has", unpriced)
                      else sprintf("Models %s have", paste0("'", unpriced, "'", collapse = ", ")),
                      if (length(unpriced) == 1L) "its" else "their", limit),
              class = "gr_cost_uncheckable")
    }
    # What this run has spent already counts, as calls already made count
    # against the call cap: several readers on one trace are one run.
    spent <- if (inherits(trace, "gr_trace")) as_num1(trace$spent_usd, 0) else 0
    if (limit_reached(spent, budget)) {
      gr_abort(sprintf(paste0("This run has already spent $%s, which reaches the $%s limit. ",
                              "Raise it with gr_options(max_cost_usd = ...)."),
                       fmt_usd(spent), limit),
               class = "gr_cost_cap")
    }
    # A limit of nothing: a model that costs nothing may run, and the first
    # request to one that has a price would already pass it.
    if (budget <= 0 && any(!is.na(unit) & unit > 0)) {
      gr_abort(sprintf(paste0("The spending limit is $0, and '%s' has a price. Raise it with ",
                              "gr_options(max_cost_usd = ...), or use a model registered at no cost."),
                       models[!is.na(unit) & unit > 0][1]),
               class = "gr_cost_cap")
    }
    already <- if (spent > 0) sprintf(", and this run has already spent $%s", fmt_usd(spent)) else ""
    if (every_chunk && !is.na(input_cost) && spent + input_cost > budget) {
      gr_abort(sprintf(paste0("Reading this document with '%s' sends every chunk at least once, ",
                              "which costs about $%s before any reply (%d chunk(s), %s input ",
                              "tokens)%s, above the $%s limit. Raise it with ",
                              "gr_options(max_cost_usd = ...), or use a reader that sends only the ",
                              "chunks it picks, such as recipe = \"needle\"."),
                       spec$reader, fmt_usd(input_cost), n,
                       format(tok, big.mark = ",", scientific = FALSE), already, limit),
               class = "gr_cost_cap")
    }
    if (parallel_read && !is.na(worst) && spent + worst > budget) {
      gr_abort(sprintf(paste0("With parallel = TRUE, requests go out in batches that cannot be ",
                              "stopped part way, so a parallel read is held to its worst case: ",
                              "every reply at its cap, about $%s for '%s' over %d chunk(s)%s, ",
                              "above the $%s limit. Raise it with gr_options(max_cost_usd = ...), ",
                              "or read without parallel = TRUE, where spending is checked before ",
                              "every request."),
                       fmt_usd(worst), spec$reader, n, already, limit),
               class = "gr_cost_cap")
    }
  }
  trace_note(trace, "preflight", list(reader = spec$reader, chunks = n,
                                      est_calls = est_calls,
                                      # Of those, requests to the embeddings
                                      # endpoint.
                                      embed_calls = embed_calls,
                                      # Every reply at its cap and every level
                                      # that takes: what `est_cost_usd` prices.
                                      worst_calls = worst_calls,
                                      # Every chunk once, for a reader that sends
                                      # them all: the floor that can refuse a run.
                                      est_input_usd = if (is.na(input_cost)) NULL
                                                      else round(input_cost, 4),
                                      # Every reply at its cap: an upper bound.
                                      est_cost_usd = if (is.na(worst)) NULL else round(worst, 4),
                                      # What was changed from the defaults, so
                                      # the trace records the configuration and
                                      # not just the reader's name.
                                      settings = settings))
  invisible(NULL)
}

#' The requests to the embeddings endpoint each of `readers` will send.
#'
#' `retrieve` embeds the question with every chunk; `iterative` every chunk,
#' then one query a round, the first being the question; `rerank` embeds as
#' `retrieve` does when no chunk shares a word with the question and it has
#' more chunks than candidates (rerank_prefilter()). Only the built-in "api"
#' embedder sends requests: a client's own embed function, a registered one
#' such as "lexical", and a backend or replay client (which gr_embed() moves to
#' lexical vectors) send none. A text in the session's embedding cache is not
#' sent again, and with the cache on a reader finds what an earlier ensemble
#' member embedded there. 64 texts to a request, gr_embed()'s default. A
#' query not known yet (iterative's later rounds) counts as a request of its
#' own.
#' @return An integer per reader, named by reader.
#' @noRd
embed_requests <- function(client, readers, texts, question, spec) {
  out <- stats::setNames(integer(length(readers)), readers)
  if (!inherits(client, "gr_client")) return(out)
  emb <- tryCatch(resolve_embedder(client), error = function(e) NULL)
  if (is.null(emb) || !identical(emb$fn, embed_api) || inherits(client, "gr_replay_client") ||
      as_chr1(client[["api", exact = TRUE]], "") %in% c("backend", "replay")) {
    return(out)
  }
  cache <- isTRUE(gr_options("cache_embeddings"))
  model <- as_chr1(client[["embedding_model", exact = TRUE]], "")
  # The key embed_api() files a vector under. Should the two drift apart, a
  # cached text is counted as sent, which can refuse a run near the cap but
  # never lets one start that the cap stops part way.
  endpoint <- paste0(as_chr1(client[["base_url", exact = TRUE]], "?"), "|",
                     as_chr1(client[[".client_id", exact = TRUE]], "<url-addressed>"))
  earlier <- character(0)
  requests <- function(x) {
    if (cache && length(x)) {
      keys <- vapply(x, function(t) embed_cache_key("api", model, t, endpoint), character(1),
                     USE.NAMES = FALSE)
      held <- vapply(keys, function(k) !is.null(gr_state$embed_cache[[k]]), logical(1),
                     USE.NAMES = FALSE)
      x <- x[!held & !keys %in% earlier]
      earlier <<- c(earlier, keys)
    }
    as.integer(ceiling(length(x) / 64))
  }
  n <- length(texts)
  for (r in readers) {
    out[[r]] <- switch(r,
      retrieve = requests(c(question, texts)),
      iterative = requests(texts) + requests(question) + as.integer(spec$max_rounds) - 1L,
      rerank = if (min(spec$rerank_candidates, n) < n && shares_no_word(texts, question))
                 requests(c(question, texts)) else 0L,
      0L)
  }
  out
}

#' The requests tree_merge() makes at worst, and the tokens they are handed.
#'
#' `k` findings of up to `piece` tokens are grouped greedily into merges that
#' fit `room`, each merge replying with up to `answer` tokens, level after
#' level, until one merge takes everything that is left. A group of one is
#' passed on without a request, and tree_merge() gives up after seven levels or
#' a level that does not shrink the pile. NULL when the room is not known.
#' @noRd
merge_tree_worst <- function(k, piece, answer, room) {
  if (length(room) != 1L || is.na(room) || room <= 0) return(NULL)
  if (k <= 1) return(list(calls = 0, input = 0))
  calls <- 0
  input <- 0
  for (level in seq_len(7L)) {
    per <- max(1, floor(room / max(piece, 1)))
    if (k <= per) return(list(calls = calls + 1, input = input + k * piece))
    groups <- ceiling(k / per)
    if (per >= 2) calls <- calls + (k %/% per) + ((k %% per) >= 2)
    input <- input + k * piece
    if (groups >= k) break
    k <- groups
    piece <- answer
  }
  list(calls = calls, input = input)
}

#' The requests `hierarchical` makes at worst, and the tokens they are handed.
#'
#' Every summary at `summary` tokens: levels of `fan`-way summaries until they
#' fit `room` or `max_levels` is reached, as read_hierarchical() recurses, then
#' one answer. The first level's input is the document, counted by the caller.
#' @noRd
summary_levels_worst <- function(n, summary, room, fan, max_levels) {
  if (length(room) != 1L || is.na(room)) return(NULL)
  calls <- n
  input <- 0
  cur <- n
  level <- 1L
  while (cur * summary > room && level < max_levels) {
    level <- level + 1L
    prev <- cur
    input <- input + cur * summary
    cur <- ceiling(cur / fan)
    calls <- calls + cur
    if (cur >= prev) break
  }
  list(calls = calls + 1, input = input + min(cur * summary, room))
}

#' The model a spec reads with: the one it names, or else the client's.
#' @noRd
resolve_read_model <- function(spec, client) {
  if (!is.null(spec$model)) return(spec)
  m <- as_chr1(client[["model", exact = TRUE]], "")
  spec$model <- if (nzchar(m)) m else as_chr1(gr_options("model"))
  spec
}

#' The model a client's calls are billed as, when the client fixes it rather
#' than each request naming it.
#'
#' An ellmer chat answers with the model it was built with whatever a request
#' asks for, and gr_ellmer_client() reports that model on every result, which
#' is the model the trace prices a call by. NULL for every other client, whose
#' calls are billed as the model each request names.
#' @noRd
client_billed_model <- function(client) {
  if (!inherits(client, "gr_ellmer_client")) return(NULL)
  m <- as_chr1(client[["model", exact = TRUE]], "")
  if (nzchar(m)) m else NULL
}

#' @noRd
as_read_spec <- function(spec) {
  if (inherits(spec, "gr_read_spec")) return(spec)
  if (is.null(spec)) return(gr_read_spec())
  if (is.character(spec) && length(spec) == 1L) return(gr_read_spec(reader = spec))
  if (is.list(spec)) return(do.call(gr_read_spec, spec))
  gr_abort("`read` must be a gr_read_spec, a reader name, or a named list.")
}
