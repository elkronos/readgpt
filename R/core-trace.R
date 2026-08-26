# core-trace.R -- one run, one trace.
#
# WHY THIS FILE EXISTS
# The Shiny app called `answer_question()` twice per submission: once for the
# answer, once with `return_json = TRUE` for the "chain of thought" tab. That
# doubled every user's API bill, and at any temperature above 0 the displayed
# reasoning was not the reasoning behind the displayed answer -- two independent
# generations shown as if one explained the other.
#
# Tracing is now a side-channel on a single run. `answer_document()` always
# produces both the answer and its trace; `return_json` merely chooses which to
# hand back. There is no path that re-runs the pipeline to get a trace.

#' Create a run trace
#'
#' @param run_id Optional identifier; generated when omitted.
#' @param meta Named list of run-level metadata.
#' @return A `gr_trace`. It is an environment, so it accumulates by reference:
#'   pass the same trace to several calls and they all record into it. Fields:
#'   `run_id`, `started`, `meta`, `steps`, `calls`, `tokens_in`, `tokens_out`,
#'   `errors`, `budget_stop`.
#' @seealso [gr_trace_summary()], [as_json()], [gr_answer]
#' @export
#' @examples
#' tr <- gr_trace(meta = list(purpose = "demo"))
#' cl <- gr_mock_client(function(m, p) "an answer")
#' ch <- gr_segment(gptread_example(), list(method = "sentence", max_tokens = 150))
#' invisible(gr_read(ch, "What was revenue?", cl, "map_reduce", trace = tr))
#' print(tr)
gr_trace <- function(run_id = NULL, meta = list()) {
  e <- new.env(parent = emptyenv())
  e$run_id <- as_chr1(run_id %||% gr_new_id("run"))
  e$started <- Sys.time()
  e$meta <- meta
  e$steps <- list()
  e$calls <- 0L
  e$tokens_in <- 0L
  e$tokens_out <- 0L
  e$errors <- list()
  e$budget_stop <- FALSE
  structure(e, class = "gr_trace")
}

#' @noRd
trace_record <- function(trace, label, messages, result, params = list()) {
  if (is.null(trace) || !inherits(trace, "gr_trace")) return(invisible(NULL))
  trace$calls <- trace$calls + 1L
  trace$tokens_in <- trace$tokens_in + as.integer(result$usage$input %||% 0L)
  trace$tokens_out <- trace$tokens_out + as.integer(result$usage$output %||% 0L)
  if (!isTRUE(result$ok)) {
    trace$errors <- c(trace$errors, list(list(step = length(trace$steps) + 1L, label = label,
                                              error = as_chr1(result$error))))
  }
  trace$steps <- c(trace$steps, list(list(
    step = length(trace$steps) + 1L,
    label = as_chr1(label),
    at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    model = as_chr1(result$model %||% params$model, NA_character_),
    ok = isTRUE(result$ok),
    prompt = lapply(messages, function(m) list(role = m$role, content = m$content)),
    response = result$text,
    error = if (isTRUE(result$ok)) NULL else as_chr1(result$error),
    tokens = list(input = result$usage$input %||% 0L, output = result$usage$output %||% 0L),
    params = params[setdiff(names(params), "schema")]
  )))
  invisible(NULL)
}

#' Record a non-model step (segmentation, ranking, a local computation).
#' @noRd
trace_note <- function(trace, label, detail = list()) {
  if (is.null(trace) || !inherits(trace, "gr_trace")) return(invisible(NULL))
  trace$steps <- c(trace$steps, list(list(
    step = length(trace$steps) + 1L, label = as_chr1(label),
    at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    ok = TRUE, kind = "local", detail = detail
  )))
  invisible(NULL)
}

#' Fold a child trace's steps and counters into a parent.
#'
#' Used by `gr_compare()` so each recipe is budgeted independently while the
#' comparison still reports one combined trace.
#' @noRd
trace_absorb <- function(parent, child) {
  if (!inherits(parent, "gr_trace") || !inherits(child, "gr_trace")) return(invisible(NULL))
  off <- length(parent$steps)
  parent$steps <- c(parent$steps, lapply(child$steps, function(st) {
    st$step <- st$step + off
    st$recipe <- as_chr1(child$meta$recipe, NA_character_)
    st
  }))
  parent$calls <- parent$calls + child$calls
  parent$tokens_in <- parent$tokens_in + child$tokens_in
  parent$tokens_out <- parent$tokens_out + child$tokens_out
  parent$errors <- c(parent$errors, child$errors)
  if (isTRUE(child$budget_stop)) parent$budget_stop <- TRUE
  invisible(NULL)
}

#' Would one more call exceed the run's call cap?
#'
#' Strategies consult this before fanning out. The old code had no equivalent,
#' which is why a negative token budget could turn into one API call per word
#' with nothing to stop it.
#' @noRd
trace_can_call <- function(trace, n = 1L) {
  if (is.null(trace) || !inherits(trace, "gr_trace")) return(TRUE)
  cap <- gr_options("max_calls")
  if (is.null(cap) || !is.finite(cap)) return(TRUE)
  ok <- (trace$calls + n) <= cap
  if (!ok) trace$budget_stop <- TRUE
  ok
}

#' Summarise a trace
#' @param trace A `gr_trace`.
#' @return A one-row data frame: `run_id`, `calls`, `steps`, `tokens_in`,
#'   `tokens_out`, `errors`, `elapsed_s`. There is no cost column -- combine
#'   `tokens_in`/`tokens_out` with [gr_estimate_cost()] for that.
#' @seealso [gr_trace()], [as_json()], [gr_estimate_cost()]
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "45.2 million dollars")
#' ans <- answer_document(gptread_example(), "What was revenue?", "thorough", client = cl)
#' gr_trace_summary(ans$trace)
#' gr_estimate_cost("gpt-4o", ans$trace$tokens_in, ans$trace$tokens_out)
gr_trace_summary <- function(trace) {
  stopifnot(inherits(trace, "gr_trace"))
  data.frame(
    run_id = trace$run_id,
    calls = trace$calls,
    steps = length(trace$steps),
    tokens_in = trace$tokens_in,
    tokens_out = trace$tokens_out,
    errors = length(trace$errors),
    elapsed_s = round(as.numeric(difftime(Sys.time(), trace$started, units = "secs")), 2),
    stringsAsFactors = FALSE
  )
}

#' @export
print.gr_trace <- function(x, ...) {
  s <- gr_trace_summary(x)
  cat(sprintf("<gr_trace %s>  %d steps, %d model calls, %d in / %d out tokens, %d error(s)\n",
              s$run_id, s$steps, s$calls, s$tokens_in, s$tokens_out, s$errors))
  labs <- vapply(x$steps, function(st) as_chr1(st$label), character(1))
  if (length(labs)) {
    tab <- table(labs)
    cat("  steps:", paste(sprintf("%s x%d", names(tab), as.integer(tab)), collapse = ", "), "\n")
  }
  if (length(x$errors)) {
    cat("  first error:", substr(as_chr1(x$errors[[1]]$error), 1, 160), "\n")
  }
  invisible(x)
}

#' Serialise an object to JSON
#'
#' A generic so traces, answers, chunk sets and documents all serialise
#' consistently and safely (`auto_unbox` plus `null = "null"`, so a missing
#' field appears as `null` rather than vanishing -- assigning `NULL` into an R
#' list *deletes the key*, which is why the old code's `final_answer` field
#' silently disappeared from the JSON whenever a call failed).
#'
#' @param x Object to serialise. Methods exist for [gr_answer], `gr_trace`,
#'   [gr_chunks] and [gr_document]; anything else falls back to a plain
#'   `jsonlite` conversion.
#' @param pretty Whether to indent.
#' @param ... Passed to `jsonlite::toJSON()`.
#' @return A `json`-classed character string. `NULL` fields are written as
#'   `null` rather than dropped.
#' @seealso [gr_trace_summary()], [gr_answer]
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "45.2 million dollars")
#' ans <- answer_document(gptread_example(), "What was revenue?", "fast", client = cl)
#'
#' # The answer plus every prompt and response from the same single run.
#' txt <- as_json(ans)
#' names(jsonlite::fromJSON(txt))
as_json <- function(x, pretty = TRUE, ...) UseMethod("as_json")

#' @export
as_json.default <- function(x, pretty = TRUE, ...) {
  jsonlite::toJSON(x, pretty = pretty, auto_unbox = TRUE, null = "null",
                   na = "null", force = TRUE, ...)
}

#' A trace as a plain, serialisable list.
#'
#' A `gr_trace` is an environment so that nested readers can write to one shared
#' record. Environments cannot be serialised: handing one to `jsonlite::toJSON()`
#' fails with "cannot unclass an environment", which is what happened to every
#' caller that put a trace inside a larger list before encoding it. Anything that
#' embeds a trace in JSON goes through this.
#' @noRd
trace_as_list <- function(x) {
  if (is.null(x) || !inherits(x, "gr_trace")) return(NULL)
  list(
    run_id = x$run_id,
    started = format(x$started, "%Y-%m-%dT%H:%M:%OS3"),
    meta = x$meta,
    summary = as.list(gr_trace_summary(x)),
    steps = x$steps,
    errors = x$errors
  )
}

#' @export
as_json.gr_trace <- function(x, pretty = TRUE, ...) {
  as_json.default(trace_as_list(x), pretty = pretty, ...)
}
