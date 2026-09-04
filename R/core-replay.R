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
#' back -- same answers, same evidence, same merge decisions -- with no API key,
#' no network and no spend.
#'
#' This is what makes a published result checkable. Ship the trace next to the
#' paper and a reader can reproduce the run rather than take it on trust. It is
#' also the cheapest possible bug report: a trace file is a re-runnable
#' recording of exactly what went wrong.
#'
#' @param source A `gr_trace`, a path to a file written by [gr_trace_save()], or
#'   an already-parsed list in that shape.
#' @param strict If `TRUE` (default), a prompt with no recorded response raises
#'   a `gr_replay_miss` error. That is usually what you want: a miss means the
#'   replay has diverged from the recording, and continuing would produce a
#'   result that looks like the original but is not. With `FALSE` a miss returns
#'   a failed [gr_result] instead, so a partially recorded trace still runs.
#' @return An object of class `gr_replay_client`, usable anywhere a
#'   [gr_client()] is. It also carries `$stats()` and `$missed()`.
#'
#' @section Matching:
#' A response is matched on the exact prompt messages plus the model id. When a
#' run issued the same prompt more than once -- which happens at a temperature
#' above zero, and in readers that revisit a chunk -- the recorded responses are
#' returned in the order they were produced. Once they are exhausted the last
#' one repeats.
#'
#' @section What does not replay:
#' Embeddings are not model calls and are not recorded in a trace, so readers
#' that embed (`retrieve`, and the `semantic` segmenter) fall back to hashed
#' lexical vectors under replay and warn with class `gr_replay_no_embeddings`.
#' Their chunk *ranking* may therefore differ from the original run even though
#' every recorded answer is reproduced exactly. Traces also do not record the
#' JSON schema a call requested, so two calls that differ only by schema share a
#' recording.
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
  steps <- replay_steps(source)
  embed_source <- replay_embed_source(source)
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

  for (st in steps) {
    kf <- replay_key(st$prompt, st$model)
    kp <- replay_key(st$prompt, NULL)
    idx$full[[kf]] <- c(idx$full[[kf]], list(st))
    idx$prompt[[kp]] <- unique(c(idx$prompt[[kp]], as_chr1(st$model, "?")))
  }

  models <- vapply(steps, function(s) as_chr1(s$model, NA_character_), character(1))
  models <- models[!is.na(models)]
  default_model <- if (length(models)) names(sort(table(models), decreasing = TRUE))[1] else "replay"

  structure(list(
    model = default_model, api = "replay", base_url = "replay://",
    embedding_model = "replay-embed", max_retries = 0L, retry_pause_base = 0,
    timeout = 1, extra_body = list(),
    strict = isTRUE(strict), .idx = idx,
    # Which embedder the RECORDING used, so a replay can tell an embedding it
    # can reproduce from one it cannot. See gr_embed().
    embed_source = embed_source,
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

#' Model steps from a trace, a parsed trace, or a file.
#' @noRd
replay_steps <- function(source) {
  obj <- source
  if (is.character(source) && length(source) == 1L) {
    if (!file.exists(source)) {
      gr_abort(sprintf("No such trace file: %s", source), class = "gr_file_not_found")
    }
    obj <- tryCatch(jsonlite::fromJSON(source, simplifyVector = FALSE),
                    error = function(e) NULL)
    if (is.null(obj)) {
      gr_abort(sprintf("Could not parse '%s' as a trace. Write it with gr_trace_save().",
                       source), class = "gr_replay_unreadable")
    }
  } else if (inherits(source, "gr_trace")) {
    obj <- trace_as_list(source)
  }
  if (!is.list(obj)) {
    gr_abort("`source` must be a gr_trace, a path written by gr_trace_save(), or a parsed trace.",
             class = "gr_replay_unreadable")
  }
  steps <- obj$steps %||% list()
  out <- list()
  for (st in steps) {
    if (!is.list(st)) next
    if (identical(as_chr1(st$kind, ""), "local")) next   # segmentation, ranking, merges
    prompt <- replay_prompt(st$prompt)
    if (!length(prompt)) next
    tok <- st$tokens %||% list()
    out[[length(out) + 1L]] <- list(
      prompt = prompt,
      model = as_chr1(st$model %||% (st$params %||% list())$model, NA_character_),
      ok = isTRUE(st$ok),
      response = as_chr1(st$response, ""),
      error = if (isTRUE(st$ok)) NULL else as_chr1(st$error, "recorded failure"),
      tokens = list(input = as_int1(tok$input, 0L), output = as_int1(tok$output, 0L)),
      label = as_chr1(st$label, "call")
    )
  }
  out
}

#' Which embedder produced the vectors in the recorded run, if any.
#'
#' Embeddings are not model calls, so they are not in the transcript -- but
#' `gr_embed()` writes a local note saying which embedder it used, and that is
#' enough. A replay can reproduce a run's ranking only when it uses the SAME
#' embedder and that embedder is deterministic. Without this the check was
#' "is the current embedder deterministic", which is not the same question: a
#' run recorded through an API and replayed with a deterministic local embedder
#' would have claimed to be exact while ranking chunks by different vectors.
#'
#' `NA` when the run embedded nothing (no ranking to reproduce) or when the
#' recording used more than one embedder.
#' @noRd
replay_embed_source <- function(source) {
  obj <- source
  if (inherits(source, "gr_trace")) obj <- trace_as_list(source)
  else if (is.character(source) && length(source) == 1L && file.exists(source)) {
    obj <- tryCatch(jsonlite::fromJSON(source, simplifyVector = FALSE),
                    error = function(e) NULL)
  }
  if (!is.list(obj)) return(NA_character_)
  srcs <- unlist(lapply(obj$steps %||% list(), function(st) {
    if (!is.list(st) || !identical(as_chr1(st$label, ""), "embed")) return(NULL)
    as_chr1((st$detail %||% list())$source, NA_character_)
  }), use.names = FALSE)
  srcs <- unique(srcs[!is.na(srcs)])
  if (length(srcs) == 1L) srcs else NA_character_
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
replay_key <- function(messages, model) {
  # key_text() is what makes a trace replayable from a FILE. A saved trace comes
  # back through jsonlite, which does not necessarily hand back a string labelled
  # the way the original was, and `digest()` hashes the label along with the
  # bytes -- so before this every non-ASCII prompt missed on replay-from-file
  # while every ASCII one hit. See the note on key_text() in core-cache.R.
  gr_hash(c("readgpt-replay-v1",
            if (is.null(model)) "<any-model>" else as_chr1(model, "?"),
            key_text(unlist(lapply(messages, function(m) c(as_chr1(m$role), as_chr1(m$content))),
                            use.names = FALSE))))
}

#' Answer one call from the recording.
#' @noRd
replay_lookup <- function(client, messages, model, params) {
  idx <- client$.idx
  kf <- replay_key(messages, model)
  recorded <- idx$full[[kf]]

  if (is.null(recorded)) {
    other <- idx$prompt[[replay_key(messages, NULL)]]
    detail <- if (!is.null(other)) {
      sprintf(" The same prompt IS recorded under model %s -- replay the run with that model.",
              paste(sprintf("'%s'", other), collapse = " or "))
    } else {
      sprintf(" The recording holds %d distinct prompt(s); this is not one of them, so the replay has diverged from the run that produced it (a different document, question, recipe or segmenter).",
              length(idx$full))
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

  gr_result(
    ok = st$ok, text = st$response, error = st$error, status = NA_integer_,
    usage = list(input = st$tokens$input, output = st$tokens$output),
    model = as_chr1(st$model, model), finish_reason = NA_character_,
    cached = TRUE
  )
}
