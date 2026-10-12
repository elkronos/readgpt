# core-replay.R -- re-run a recorded run without an API key.
#
# WHY THIS FILE EXISTS
# `gr_trace` already records every prompt, every response and every token count
# for a run. That record was write-only: you could read it, print it and
# serialise it, but you could not *run* it. So a published result was something
# a reader had to take on trust and pay to reproduce, and a bug report was a
# description of a run rather than the run itself.
#
# A trace is a complete transcript of the only non-deterministic part of the
# pipeline. Everything else -- extraction, cleaning, segmentation, ranking,
# merging, budgeting -- is a pure function of its input. So a trace plus the
# source document is enough to reproduce a run exactly, with no key and no
# spend, provided the model calls are answered from the transcript instead of
# the network. That is all this file does.
#
# WHAT REPLAY IS NOT
# It is not a mock. A mock invents answers; a replay client returns the answer
# the model actually gave, matched to the prompt that actually produced it. And
# it is not a cache: a cache makes a *future* run cheaper, while a replay makes
# a *past* run checkable by someone who was not there.

#' Save a trace to a file
#'
#' Writes the trace as JSON, in the form [as_json()] produces. The file is
#' everything [gr_replay_client()] needs, so this is how a run leaves the
#' session it happened in.
#'
#' @param trace A `gr_trace`.
#' @param path Destination file.
#' @return `path`, invisibly.
#' @seealso [gr_replay_client()], [as_json()], [gr_trace()]
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
#' ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl)
#'
#' f <- tempfile(fileext = ".json")
#' gr_trace_save(ans$trace, f)
#' file.exists(f)
gr_trace_save <- function(trace, path) {
  stopifnot(inherits(trace, "gr_trace"))
  path <- as_chr1(path)
  if (!nzchar(path)) gr_abort("`path` must be a file name.")
  dir <- dirname(path)
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(as.character(as_json(trace, pretty = TRUE)), path, useBytes = TRUE)
  invisible(path)
}

#' A client that answers from a recorded run
#'
#' Replays the responses in a trace instead of calling a model. Give it the
#' trace from a run and the same document and question, and you get that run
#' back (same answers, same evidence, same merge decisions) with no API key,
#' no network and no spend.
#'
#' This is what makes a published result checkable. Ship the trace next to the
#' paper and a reader can reproduce the run rather than take it on trust. It is
#' also the cheapest possible bug report: a trace file is a re-runnable
#' recording of exactly what went wrong.
#'
#' @param source A `gr_trace`; a path to a file written by [gr_trace_save()];
#'   the JSON text [as_json()] writes for a trace or for an answer (whose trace
#'   is used); or an already-parsed list in that shape. Parse a file yourself
#'   with `jsonlite::fromJSON(path, simplifyVector = FALSE)`: the default
#'   simplification turns the steps into a data frame, which loses what a
#'   replay needs, and is refused with a message that says so.
#' @param strict If `TRUE` (default), a prompt with no recorded response raises
#'   a `gr_replay_miss` error. That is usually what you want: a miss means the
#'   replay has diverged from the recording, and continuing would produce a
#'   result that looks like the original but is not. With `FALSE` a miss returns
#'   a failed [gr_result] instead, so a partially recorded trace still runs.
#' @return An object of class `gr_replay_client`, usable anywhere a
#'   [gr_client()] is. It also carries `$stats()` and `$missed()`.
#'
#' @section Matching:
#' A response is matched on the exact prompt messages plus the model id the
#' call asked for, and the reasoning effort it was sent with when it had one
#' (see `effort` in [gr_read_spec()]), so a replay under another effort misses
#' rather than passing for the recording. A replay under the same effort
#' matches even where this session's registry does not list a model as a
#' reasoning model and the recording session's did (a gateway model
#' registered with [gr_register_model()] there and not here), and a call
#' through an ellmer chat, which sends no effort, is matched on the one it
#' asked for. The model id can differ from the
#' model the trace records as
#' answering: a [gr_ellmer_client()] answers with its chat's model whatever the
#' recipe asked for, and its runs replay all the same.
#'
#' The replay client's own model, which a read that names none asks for, is
#' the model the recorded reads asked for. One trace can hold reads through
#' clients built for different models, and a recording made by readgpt 0.5.0
#' asked for `gr_options("model")` on a read and for the client's model
#' everywhere else; a call that asks for the replay client's model and finds
#' nothing under it is answered from the one other such model that holds the
#' same prompt. A model the recorded settings named (`model`, `skim_model`,
#' `summary_model`) is never used that way, so a replay that leaves one out
#' misses. When a
#' run issued the same prompt more than once (which happens at a temperature
#' above zero, and in readers that revisit a chunk), the recorded responses are
#' returned in the order they were produced. Once they are exhausted the last
#' one repeats.
#'
#' @section Embeddings:
#' A trace records each request to an embeddings endpoint (a step labelled
#' `"embed.request"`, counted in `calls` and priced like any other request),
#' but not the vectors that came back, so a replay has nothing to answer those
#' requests with. A run that embedded (the `semantic` segmenter, the
#' `retrieve` and `iterative` readers, `rerank` when word overlap finds
#' nothing, and so the `"needle"` recipe) therefore replays only when the
#' recording used a **deterministic** embedder and the replay uses the
#' **same** one: the vectors are then computed again, exactly. Both
#' conditions, because replaying an API-embedded run with a deterministic
#' local embedder would compute vectors the original never saw while looking
#' exact. Anything else, including a run recorded with the default `"api"`
#' embedder (or a [gr_mock_client()]'s embed handler), warns with class
#' `gr_replay_no_embeddings` and falls back to hashed lexical vectors. Those
#' place semantic cuts and rank chunks differently, so such a replay usually
#' sends prompts the recording does not hold: a strict replay stops with
#' `gr_replay_miss`, whose message names this cause, and a non-strict one gets
#' failed calls for them and does not give the recorded answer. Record a run
#' you intend to publish or replay with `gr_options(embedder = "lexical")`, or
#' with your own embedder registered as `deterministic = TRUE`.
#'
#' @section Limits:
#' A saved trace says whether a limit cut the run short (`budget_stop`,
#' `stop_reason`), and a replay of such a run stops where it stopped when it
#' runs under the limits it was recorded with. Replayed calls count against
#' `max_calls` as they did when recorded. When the spending limit stopped the
#' recorded run, each replayed call also counts what it cost when recorded
#' against `max_cost_usd` (in the trace's `replayed_usd`; nothing is spent);
#' for any other run it counts nothing, so a replay of an expensive run is not
#' stopped by the replaying session's limit. A replay under a higher limit, or
#' none, that asks for a call past the recorded stop gets a `gr_replay_miss`
#' saying so, and one under a lower limit stops sooner, marked partial as the
#' run would have been.
#'
#' @section The recipe "auto" chose:
#' A recording of [answer_document()] with `recipe = "auto"` holds the recipe
#' the choice picked for each document and question, and a replay of the same
#' document and question repeats that choice rather than making it again. What
#' the choice rests on (the token count, the registered models, the client's
#' model) can differ between the session that recorded a run and the one
#' replaying it, and a different choice would send prompts the recording does
#' not have.
#'
#' @section What does not replay:
#' Traces do not record the JSON schema a call requested, so two calls that
#' differ only by schema share a recording.
#'
#' @seealso [gr_trace_save()], [gr_cache()] for making future runs cheap,
#'   [gr_mock_client()] for invented answers rather than recorded ones
#' @export
#' @examples
#' # A run.
#' cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
#' ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl)
#'
#' # The same run, from the recording, with no client and no key.
#' rp <- gr_replay_client(ans$trace)
#' again <- answer_document(readgpt_example(), "What was revenue?", "fast", client = rp)
#' identical(again$answer, ans$answer)
#'
#' rp$stats()
#'
#' # Through a file, which is how a run reaches someone else.
#' f <- tempfile(fileext = ".json")
#' gr_trace_save(ans$trace, f)
#' answer_document(readgpt_example(), "What was revenue?", "fast",
#'                 client = gr_replay_client(f))$answer
gr_replay_client <- function(source, strict = TRUE) {
  # Read once: a file is parsed, and a trace listed, a single time for all the
  # lookups below.
  source <- replay_read(source)
  steps <- replay_steps(source)
  embed_source <- replay_embed_source(source)
  auto_choices <- replay_auto_choices(source)
  if (!length(steps)) {
    gr_abort(paste0("That trace has no recorded model calls, so there is nothing to replay. ",
                    "A run that made no calls (every reader failed, or the budget stopped it ",
                    "before the first request) produces an empty recording."),
             class = "gr_replay_empty")
  }

  idx <- new.env(parent = emptyenv())
  idx$full <- list()      # prompt + model  -> recorded responses, in order
  idx$prompt <- list()    # prompt only     -> models seen, for diagnostics
  idx$cursor <- list()
  idx$hits <- 0L
  idx$repeats <- 0L
  idx$misses <- list()
  idx$auto_used <- rep(FALSE, nrow(auto_choices))

  idx$effort <- list()    # prompt + model  -> efforts seen, for diagnostics

  for (st in steps) {
    kf <- replay_key(st$prompt, st$model, st$effort)
    kp <- replay_key(st$prompt, NULL)
    km <- replay_key(st$prompt, st$model)
    idx$full[[kf]] <- c(idx$full[[kf]], list(st))
    idx$prompt[[kp]] <- unique(c(idx$prompt[[kp]], as_chr1(st$model, "?")))
    idx$effort[[km]] <- unique(c(idx$effort[[km]], as_chr1(st$effort, "<none>")))
  }

  # The model a replayed read asks for when it names none is this client's, so
  # it has to be the one the recorded reads asked for. Their pre-flight notes
  # say which that was. The model most calls asked for is only a guess at it,
  # and a wrong one when a cheaper skim_model or summary_model made most of
  # them: the replay then asked for that model on the answer call, found
  # nothing recorded under it and stopped with gr_replay_miss. A recording
  # without the notes falls back to the guess.
  #
  # One client's model cannot be every model a recording needs when its reads
  # followed clients built for different models, or when it was made by 0.5.0,
  # whose reads asked for gr_options("model") and whose segmenters, extraction
  # and claims asked for the client's model. The others are kept apart, and
  # answer only a call that asked for this client's model and found nothing
  # under it. See replay_default_models().
  models <- vapply(steps, function(s) as_chr1(s$model, NA_character_), character(1))
  models <- models[!is.na(models)]
  defaults <- replay_default_models(source, models)
  default_model <- if (!is.na(defaults$read)) defaults$read
                   else if (length(models)) replay_most_frequent(models) else "replay"

  structure(list(
    model = default_model, api = "replay", base_url = "replay://",
    embedding_model = "replay-embed", max_retries = 0L, retry_pause_base = 0,
    timeout = 1, extra_body = list(),
    strict = isTRUE(strict), .idx = idx,
    .alt_models = defaults$alts,
    # Derived from the recording, not from this object: two replays of the same
    # trace are the same thing and should share a store, while replays of
    # DIFFERENT recordings must not -- without this a corpus store served one
    # recording's answers while replaying another, which is exactly the failure
    # the client identity was introduced to prevent.
    .client_id = paste0("replay-", gr_hash(lapply(steps, function(s)
      c(s$answered_model, s$response, unlist(lapply(s$prompt, function(m) m$content)))))),
    # Which embedder the RECORDING used, so a replay can tell an embedding it
    # can reproduce from one it cannot. See gr_embed().
    embed_source = embed_source,
    # Which limit cut the recorded run short, or NA, so a miss past the stop
    # can say why it missed. A trace saved before this was recorded says
    # nothing, and its misses keep the general message.
    .recorded_stop = if (isTRUE(source$budget_stop)) as_chr1(source$stop_reason, "limit")
                     else NA_character_,
    # Whether a replayed call counts what it cost when recorded against
    # max_cost_usd: only when the spending limit stopped the recorded run, so
    # the replay stops at the same call. A run the limit never stopped replays
    # whatever limit the replaying session has, as it always did; counted
    # there, a $12 recording made under a raised limit stopped at the default
    # $5 when someone else replayed it.
    .replay_costs = isTRUE(source$budget_stop) &&
      identical(as_chr1(source$stop_reason, ""), "cost"),
    # The recipe answer_document()'s "auto" chose in the recording for a
    # document and question, found by their key. Replayed rather than chosen
    # again: what the choice rests on (the token count, the registered models,
    # the client) can differ between the session that recorded a run and the
    # one replaying it. Several recorded choices for one key are handed out in
    # the order they were made.
    auto_choice = function(key = NULL) {
      hit <- which(auto_choices$key == as_chr1(key, "") & !idx$auto_used)
      if (!length(hit)) return(NULL)
      idx$auto_used[hit[1]] <- TRUE
      auto_choices$chose[hit[1]]
    },
    n_recorded = length(steps),
    stats = function() data.frame(
      recorded = length(steps), distinct = length(idx$full),
      hits = idx$hits, repeats = idx$repeats, misses = length(idx$misses),
      stringsAsFactors = FALSE
    ),
    missed = function() idx$misses
  ), class = c("gr_replay_client", "gr_client"))
}

#' @export
print.gr_replay_client <- function(x, ...) {
  s <- x$stats()
  cat(sprintf("<gr_replay_client> %d recorded call(s), %d distinct prompt(s), model=%s%s\n",
              s$recorded, s$distinct, x$model, if (x$strict) "" else " [non-strict]"))
  cat(sprintf("  %d hit(s), %d repeat(s), %d miss(es)\n", s$hits, s$repeats, s$misses))
  invisible(x)
}

# --- internals -------------------------------------------------------------

#' A recording as a plain list, in the shape trace_as_list() gives, from any
#' source gr_replay_client() takes. Raises when it cannot be one, saying why.
#'
#' Each misread used to end in a message about something else. A trace parsed
#' with jsonlite's default simplification has its steps as a data frame, and
#' walking that walked its columns: the replay found no calls and said the run
#' had made none. The JSON text of a trace or an answer was taken for a file
#' name, and reported as "No such trace file" followed by the whole JSON.
#' @noRd
replay_read <- function(source) {
  obj <- source
  if (inherits(source, "gr_trace")) {
    obj <- trace_as_list(source)
  } else if (is.character(source) && length(source) == 1L && !is.na(source)) {
    text <- inherits(source, "json") ||
      (!file.exists(source) && grepl("^\\s*\\{", source, useBytes = TRUE))
    if (!text && !file.exists(source)) {
      gr_abort(sprintf("No such trace file: %s", source), class = "gr_file_not_found")
    }
    obj <- tryCatch(if (text) jsonlite::parse_json(as.character(source), simplifyVector = FALSE)
                    else jsonlite::fromJSON(source, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (is.null(obj)) {
      gr_abort(if (text) "Could not parse that JSON text as a trace. Write it with as_json() or gr_trace_save()."
               else sprintf("Could not parse '%s' as a trace. Write it with gr_trace_save().", source),
               class = "gr_replay_unreadable")
    }
  }
  if (!is.list(obj)) {
    gr_abort(paste0("`source` must be a gr_trace, a path written by gr_trace_save(), the JSON ",
                    "text of a trace, or a parsed trace."),
             class = "gr_replay_unreadable")
  }
  # An answer, or its JSON: its trace is the recording.
  if (is.null(obj[["steps", exact = TRUE]]) && !is.null(obj[["trace", exact = TRUE]])) {
    inner <- obj[["trace", exact = TRUE]]
    if (inherits(inner, "gr_trace")) inner <- trace_as_list(inner)
    if (is.list(inner) && !is.null(inner[["steps", exact = TRUE]])) obj <- inner
  }
  steps <- obj[["steps", exact = TRUE]]
  if (is.data.frame(steps)) {
    gr_abort(paste0("This trace's steps were read as a data frame, which is what ",
                    "jsonlite::fromJSON() makes of them with its default simplifyVector = TRUE, ",
                    "and that loses the shape a replay needs. Pass the file's path to ",
                    "gr_replay_client(), or read it with ",
                    "jsonlite::fromJSON(path, simplifyVector = FALSE)."),
             class = "gr_replay_unreadable")
  }
  if (is.null(steps) || !is.list(steps)) {
    gr_abort(paste0("`source` holds no list of `steps`, so it is not a trace. Pass a gr_trace, ",
                    "a file written by gr_trace_save(), or the JSON text as_json() writes."),
             class = "gr_replay_unreadable")
  }
  obj
}

#' Model steps from a trace, a parsed trace, or a file.
#' @noRd
replay_steps <- function(source) {
  obj <- replay_read(source)
  steps <- obj$steps
  out <- list()
  for (st in steps) {
    if (!is.list(st)) next
    if (identical(as_chr1(st$kind, ""), "local")) next   # segmentation, ranking, merges
    prompt <- replay_prompt(st$prompt)
    if (!length(prompt)) next
    tok <- st$tokens %||% list()
    params <- if (is.list(st$params)) st$params else list()
    requested <- as_chr1(params[["model", exact = TRUE]], NA_character_)
    answered <- as_chr1(st$model, NA_character_)
    out[[length(out) + 1L]] <- list(
      prompt = prompt,
      # The model the call ASKED for, which is what a replayed gr_call() looks
      # up. The step's own `model` is the one that answered, and a client that
      # reports the provider's model (gr_ellmer_client() always does; so can a
      # backend handler) records a different name there: keyed on that, no
      # prompt of such a run was ever found, and every replay stopped with
      # gr_replay_miss on its first call. A trace without `params` (an older
      # or hand-built one) falls back to the answering model.
      model = if (!is.na(requested)) requested else answered,
      # Kept for what the replayed result reports, so pricing and display name
      # the model the recording says answered.
      answered_model = if (!is.na(answered)) answered else requested,
      ok = isTRUE(st$ok),
      response = as_chr1(st$response, ""),
      error = if (isTRUE(st$ok)) NULL else as_chr1(st$error, "recorded failure"),
      tokens = list(input = as_int1(tok$input, 0L), output = as_int1(tok$output, 0L)),
      # Carried through, because a caller acts on it: gr_synthesise(coherence =
      # TRUE) rejects a revision that stopped for "length". Dropping it here
      # made every replayed call look like a clean stop, so the replay kept a
      # truncated revision the live run had thrown away, published a different
      # document, and reported 0 misses -- certifying itself as an exact
      # reproduction of a run it had not reproduced.
      finish_reason = as_chr1(st$finish_reason, NA_character_),
      label = as_chr1(st$label, "call"),
      # The reasoning effort the request was sent with, or NULL for none, which
      # is every step recorded without the setting. Part of the key a replayed
      # call is looked up by: see replay_key(). And whether it was only asked
      # for, by a call through an ellmer chat, which its replay records again.
      effort = replay_step_effort(params),
      unsent_effort = replay_step_effort(params, unsent = TRUE),
      # What the call counted against max_cost_usd when it was recorded, which
      # the replayed call counts again (see trace_record()).
      budget_usd = replay_step_budget(st, if (!is.na(answered)) answered else requested)
    )
  }
  out
}

#' What a recorded call counted against the spending limit.
#'
#' The step says, in a trace written since it began to. An older one is priced
#' from its tokens at the model that answered, as the recording session priced
#' it, and a call it answered from a cache or a replay counted nothing.
#' @noRd
replay_step_budget <- function(st, model) {
  if (!is.null(st$budget_usd)) return(max(as_num1(st$budget_usd, 0), 0))
  if (isTRUE(st$cached)) return(0)
  tok <- if (is.list(st$tokens)) st$tokens else list()
  usd <- tryCatch(suppressWarnings(as.numeric(gr_estimate_cost(
    as_chr1(model, "unknown"), as_int1(tok$input, 0L), as_int1(tok$output, 0L)))),
    error = function(e) NA_real_)
  if (length(usd) == 1L && is.finite(usd) && usd > 0) usd else 0
}

#' A recording as a plain list, or NULL when it cannot be read. For the
#' lookups beside replay_steps() that find a note in the recording: those make
#' do without it, and replay_read() is what reports a source it cannot use.
#' @noRd
replay_source_list <- function(source) {
  tryCatch(replay_read(source), error = function(e) NULL)
}

#' The model a replay client stands for, and the others a recording needs.
#'
#' gr_read() notes on every pre-flight the model the read asked for
#' (`detail$model`). A read whose settings name a model asks for that model
#' again when it is replayed, so only the reads that followed their client say
#' what the replay client's own model has to be: `read` is that model, and NA
#' when no note says (a recording with no read), so the caller falls back to
#' the model most calls asked for.
#'
#' `alts` are the other models calls that followed a client asked for, which
#' replay_lookup() tries, one prompt at a time, for a call that asked for the
#' replay client's model and found nothing under it:
#'
#' - Reads through clients built for different models (one trace passed to
#'   both): `read` is the noted model most calls asked for, and the other
#'   noted models are `alts`.
#' - A recording made by 0.5.0, whose notes do not name the model. Its reads
#'   asked for gr_options("model") whatever the client, and its segmenters,
#'   extraction and claims asked for the client's own model; its replay asked
#'   for gr_options("model") on every read and for the model most calls asked
#'   for everywhere else, so its recordings of a read with a cheaper
#'   skim_model or summary_model replayed. `read` is gr_options("model") when
#'   the recording holds calls under it, as that version assumed, or else the
#'   only model left, or the one most of them asked for; `alts` is the model
#'   most of the rest asked for.
#'
#' A model any note names as a setting (`model`, `skim_model`,
#' `summary_model`) is never in `alts`, nor a 0.5.0 recording's `read`: the
#' calls that followed a client did not ask for it, and a replay that leaves
#' out a recorded skim_model has to miss, as it did in 0.5.0, rather than be
#' answered from the calls that setting made.
#' @noRd
replay_default_models <- function(source, models) {
  none <- list(read = NA_character_, alts = character(0))
  obj <- replay_source_list(source)
  if (is.null(obj) || !length(models)) return(none)
  notes <- Filter(function(st) is.list(st) && identical(as_chr1(st$label, ""), "preflight"),
                  obj$steps %||% list())
  if (!length(notes)) return(none)
  details <- lapply(notes, function(st) if (is.list(st$detail)) st$detail else list())
  settings <- lapply(details, function(d) if (is.list(d$settings)) d$settings else list())
  followed <- vapply(settings, function(s) is.null(s[["model", exact = TRUE]]), logical(1))
  if (!any(followed)) return(none)
  named <- unlist(lapply(settings, function(s) vapply(
    c("model", "skim_model", "summary_model"),
    function(k) as_chr1(s[[k, exact = TRUE]], NA_character_), character(1))), use.names = FALSE)
  named <- unique(named[!is.na(named) & nzchar(named)])
  noted <- vapply(details[followed], function(d) as_chr1(d[["model", exact = TRUE]], NA_character_),
                  character(1))
  noted <- unique(noted[!is.na(noted) & nzchar(noted)])

  if (length(noted)) {
    if (length(noted) == 1L) return(list(read = noted, alts = character(0)))
    under <- models[models %in% noted]
    read <- if (length(under)) replay_most_frequent(under) else noted[1]
    return(list(read = read, alts = setdiff(noted, c(read, named))))
  }
  # Every note that followed a client is silent about its model: 0.5.0.
  if (any(vapply(details, function(d) !is.null(d[["model", exact = TRUE]]), logical(1)))) {
    return(none)
  }
  pool <- models[!models %in% named]
  if (!length(pool)) return(none)
  opt <- as_chr1(gr_options("model"), "")
  cand <- unique(pool)
  read <- if (opt %in% cand) opt else if (length(cand) == 1L) cand else replay_most_frequent(pool)
  rest <- pool[pool != read]
  list(read = read, alts = if (length(rest)) replay_most_frequent(rest) else character(0))
}

#' The model most of `models` name. Ties go to the first in sorted order.
#' @noRd
replay_most_frequent <- function(models) {
  names(sort(table(models), decreasing = TRUE))[1]
}

#' Which embedder produced the vectors in the recorded run, if any.
#'
#' The transcript holds each embeddings request but not the vectors it
#' returned -- but `gr_embed()` writes a local note saying which embedder it
#' used, and that is enough. A replay can reproduce a run's ranking only when
#' it uses the SAME embedder and that embedder is deterministic. Without this the check was
#' "is the current embedder deterministic", which is not the same question: a
#' run recorded through an API and replayed with a deterministic local embedder
#' would have claimed to be exact while ranking chunks by different vectors.
#'
#' `NA` when the run embedded nothing (no ranking to reproduce) or when the
#' recording used more than one embedder.
#' @noRd
replay_embed_source <- function(source) {
  obj <- replay_source_list(source)
  if (is.null(obj)) return(NA_character_)
  srcs <- unlist(lapply(obj$steps %||% list(), function(st) {
    if (!is.list(st) || !identical(as_chr1(st$label, ""), "embed")) return(NULL)
    as_chr1((st$detail %||% list())$source, NA_character_)
  }), use.names = FALSE)
  srcs <- unique(srcs[!is.na(srcs)])
  if (length(srcs) == 1L) srcs else NA_character_
}

#' The choices "auto" made in a recording: the key of the document and question
#' each was made for, and the recipe it picked, in the order they were made.
#' @noRd
replay_auto_choices <- function(source) {
  none <- data.frame(key = character(0), chose = character(0), stringsAsFactors = FALSE)
  obj <- replay_source_list(source)
  if (is.null(obj)) return(none)
  rows <- lapply(obj$steps %||% list(), function(st) {
    if (!is.list(st) || !identical(as_chr1(st$label, ""), "auto_recipe")) return(NULL)
    d <- st$detail %||% list()
    data.frame(key = as_chr1(d$key, ""), chose = as_chr1(d$chose, NA_character_),
               stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, c(list(none), rows))
  out[!is.na(out$chose) & out$chose %in% c("fast", "thorough") & nzchar(out$key), , drop = FALSE]
}

#' Normalise a recorded prompt to a plain list of role/content pairs.
#' @noRd
replay_prompt <- function(p) {
  if (is.null(p) || !is.list(p) || !length(p)) return(list())
  # A single unwrapped message, as `$` on a one-element list can produce.
  if (!is.null(names(p)) && "content" %in% names(p)) p <- list(p)
  out <- lapply(p, function(m) {
    if (is.character(m)) return(list(role = "user", content = as_chr1(m)))
    if (!is.list(m)) return(NULL)
    list(role = as_chr1(m$role, "user"), content = as_chr1(m$content, ""))
  })
  Filter(Negate(is.null), out)
}

#' @noRd
replay_key <- function(messages, model, effort = NULL) {
  # key_text() is what makes a trace replayable from a FILE. A saved trace comes
  # back through jsonlite, which does not necessarily hand back a string labelled
  # the way the original was, and `digest()` hashes the label along with the
  # bytes -- so before this every non-ASCII prompt missed on replay-from-file
  # while every ASCII one hit. See the note on key_text() in core-cache.R.
  #
  # The reasoning effort, when the call has one, for the reason the model is
  # here: it is part of what the call asked for. Left out, a run replayed at
  # another effort would be answered with the recorded replies and report 0
  # misses, certifying a run it had not reproduced. Only when there is one, so
  # every recording made without it keeps the keys it was made with.
  gr_hash(c("readgpt-replay-v1",
            if (is.null(model)) "<any-model>" else as_chr1(model, "?"),
            if (!is.null(effort)) paste0("effort:", as_chr1(effort)),
            key_text(unlist(lapply(messages, function(m) c(as_chr1(m$role), as_chr1(m$content))),
                            use.names = FALSE))))
}

#' The effort a recorded step's request carried, or NULL.
#'
#' NULL for a step with none, and for anything a hand-edited or damaged trace
#' holds there that is not one string: a recording that cannot say what effort
#' it used is a recording made without one. A step through an ellmer chat
#' keeps the effort its call asked for as `unsent_effort` (see model_call()),
#' and is looked up by it all the same, since a replay of the run asks for it
#' again; `unsent = TRUE` says whether that is where it came from.
#' @noRd
replay_step_effort <- function(params, unsent = FALSE) {
  one <- function(nm) {
    e <- as_chr1(if (is.list(params)) params[[nm, exact = TRUE]], NA_character_)
    if (is.na(e) || !has_visible(e)) NULL else e
  }
  sent <- one("effort")
  if (unsent) return(is.null(sent) && !is.null(one("unsent_effort")))
  sent %||% one("unsent_effort")
}

#' Answer one call from the recording.
#'
#' `label` and `effort_map` are the call's own label and `effort` setting.
#' `params$effort` is that setting resolved for this call and sent only to a
#' model this session's registry says reasons, and the recording was made
#' under the registry of the session that made it. A gateway model registered
#' there as a reasoning model, and not here, was recorded at the effort and is
#' asked for without it, so every call missed, with a message saying the call
#' had asked for no effort. So when the effort this session would send finds
#' nothing, the effort the call asked for is tried, and each of the
#' recording's other models is looked up at the effort it would be sent, not
#' the one resolved for this client's model.
#' @noRd
replay_lookup <- function(client, messages, model, params, label = "call", effort_map = NULL) {
  idx <- client$.idx
  effort <- params[["effort", exact = TRUE]]
  effort_map <- effort_map %||% effort
  asked <- effort_for(effort_map, label)
  held <- function(m, e) {
    k <- replay_key(messages, m, e)
    if (!is.null(idx$full[[k]])) k
  }
  kf <- held(model, effort)
  if (is.null(kf) && is.null(effort) && !is.null(asked)) kf <- held(model, asked)
  # The recording's other models that calls following a client asked for (see
  # replay_default_models()), for a call that asked for this client's model.
  # Only when exactly one holds the prompt: two would be a guess between two
  # recorded answers, and that is reported as the miss it is.
  alts <- as.character(client[[".alt_models", exact = TRUE]] %||% character(0))
  if (is.null(kf) && length(alts) &&
      identical(as_chr1(model, ""), as_chr1(client$model, ""))) {
    ka <- unlist(lapply(alts, function(a) {
      # Only looked up, never called, so a model this session does not know is
      # not warned about, nor refused under unknown_model_action = "error".
      info <- tryCatch(suppressWarnings(gr_model_info(a)), error = function(e) NULL)
      ea <- effort_to_send(effort_map, label, info)
      held(a, ea) %||% (if (is.null(ea) && !is.null(asked)) held(a, asked))
    }))
    if (length(ka) == 1L) kf <- ka[[1]]
  }
  recorded <- if (!is.null(kf)) idx$full[[kf]]

  if (is.null(recorded)) {
    other <- idx$prompt[[replay_key(messages, NULL)]]
    # The same prompt for the same model at another effort: said first, since
    # "replay the run with that model" would name the model it already asked.
    efforts <- if (is.list(idx$effort)) idx$effort[[replay_key(messages, model)]] else NULL
    detail <- if (!is.null(efforts)) {
      say <- function(e) if (identical(e, "<none>")) "no effort" else sprintf("effort '%s'", e)
      # What the call asked for, which this session's registry may not send.
      sprintf(paste0(" The same prompt IS recorded for this model at reasoning %s, and this call ",
                     "asked for %s; replay the run with the `effort` it was recorded with."),
              paste(vapply(efforts, say, character(1)), collapse = " or "),
              say(as_chr1(effort %||% asked, "<none>")))
    } else if (!is.null(other)) {
      sprintf(" The same prompt IS recorded under model %s; replay the run with that model.",
              paste(sprintf("'%s'", other), collapse = " or "))
    } else {
      paste0(sprintf(paste0(" The recording holds %d distinct prompt(s); this is not one of them, ",
                            "so the replay has diverged from the run that produced it"),
                     length(idx$full)),
             replay_miss_causes(client))
    }
    idx$misses <- c(idx$misses, list(list(model = as_chr1(model, "?"), messages = messages)))
    msg <- paste0("No recorded response for this prompt.", detail)
    if (isTRUE(client$strict)) gr_abort(msg, class = "gr_replay_miss")
    return(gr_result(FALSE, error = msg, status = 0L, model = model, cached = TRUE))
  }

  pos <- (idx$cursor[[kf]] %||% 0L) + 1L
  if (pos > length(recorded)) {
    # More calls than the recording holds. Repeating the last response keeps the
    # run going and is counted, so `$stats()` says the replay was not exact.
    pos <- length(recorded)
    idx$repeats <- idx$repeats + 1L
  } else {
    idx$cursor[[kf]] <- pos
  }
  idx$hits <- idx$hits + 1L
  st <- recorded[[pos]]

  res <- gr_result(
    ok = st$ok, text = st$response, error = st$error, status = NA_integer_,
    usage = list(input = st$tokens$input, output = st$tokens$output),
    model = as_chr1(st$answered_model, model),
    # From the recording. Hard-coded NA here made every replayed call look like
    # a clean stop, including the ones the live run rejected for being cut off.
    finish_reason = as_chr1(st$finish_reason, NA_character_),
    cached = TRUE
  )
  # What the call cost when it was recorded, for a recording the spending
  # limit stopped. Nothing is spent, but the trace counts it against
  # max_cost_usd, so a replay stops where the recorded run did. Counted as
  # nothing, it never reached the limit that stopped the run, asked for the
  # call after the stop, and missed.
  if (isTRUE(client[[".replay_costs", exact = TRUE]])) res$replay_usd <- as_num1(st$budget_usd, 0)
  # A recorded call that asked for an effort it did not send (an ellmer chat's)
  # is traced the same way when replayed (see model_call()).
  if (isTRUE(st$unsent_effort)) res$unsent_effort <- TRUE
  res
}

#' Why a prompt the replay asked for may be missing from the recording, for the
#' gr_replay_miss message: the two causes a replay can see for itself, or the
#' usual suspects when neither applies.
#' @noRd
replay_miss_causes <- function(client) {
  idx <- client$.idx
  why <- character(0)
  if (is.environment(idx) && isTRUE(idx$embed_degraded)) {
    why <- c(why, paste0(
      "this replay could not reproduce the recording's embeddings (see the ",
      "gr_replay_no_embeddings warning), so its semantic cuts or chunk ranking can differ from ",
      "the recording's and send prompts the recording does not hold. A run that embeds replays ",
      "exactly only when it was recorded, and is replayed, with the same deterministic embedder, ",
      "such as gr_options(embedder = 'lexical')"))
  }
  stop <- as_chr1(client[[".recorded_stop", exact = TRUE]], NA_character_)
  if (!is.na(stop)) {
    what <- switch(stop, calls = "call cap (max_calls)", cost = "spending limit (max_cost_usd)",
                   "limits")
    why <- c(why, sprintf(paste0(
      "the recorded run was cut short by its %s after %d recorded call(s), so a replay under a ",
      "higher limit, or none, asks for calls the recording never made. Replay it under the ",
      "limits it was recorded with"), what, as_int1(client$n_recorded, 0L)))
  }
  if (!length(why)) return(" (a different document, question, recipe or segmenter).")
  paste0(": ", paste(why, collapse = "; and "), ".")
}
