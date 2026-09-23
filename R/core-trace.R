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
#'   `run_id`, `started`, `meta`, `steps`, `calls`, `cached`, `tokens_in`,
#'   `tokens_out`, `errors`, `budget_stop`, `stop_reason`, `spent_usd`.
#'   `cached` counts the calls answered from a [gr_cache()] or a
#'   [gr_replay_client()] rather than the network, so `calls - cached` is what
#'   the run paid for.
#'
#'   `budget_stop` is `TRUE` once a limit stopped the run, and `stop_reason`
#'   says which: `"calls"` for `max_calls`, `"cost"` for `max_cost_usd` (see
#'   [gr_options()]). `spent_usd` is what the calls so far cost, the figure
#'   `max_cost_usd` is checked against. A call to a model with no registered
#'   price adds nothing to it, so [gr_trace_cost()] is the full account.
#'
#'   `as.data.frame()` on a trace returns one row per request; see below.
#'
#' @section One row per request:
#' `as.data.frame(trace)` has one row for each request the run made, in the
#' order they were made, and none for local steps such as segmentation:
#' \describe{
#'   \item{`step`}{The step's number in `trace$steps`, where the full record is.}
#'   \item{`document`}{The document the request was about, when the run
#'     recorded one: the file name, web address or `"<inline text>"`.}
#'   \item{`recipe`}{The recipe the request belonged to, when recorded.}
#'   \item{`stage`}{What the request was for, such as `"map.answer"` or
#'     `"reduce"`.}
#'   \item{`model`, `ok`, `cached`}{The model, whether a usable reply came
#'     back, and whether it came from a [gr_cache()] or a [gr_replay_client()].}
#'   \item{`tokens_in`, `tokens_out`}{The size of the prompt and the reply.}
#'   \item{`usd`}{What the request cost, 0 when it came from a cache. `NA`
#'     when the model has no registered price, as in [gr_trace_cost()], whose
#'     total the column adds up to.}
#'   \item{`seconds`}{How long the request took, retries included. `NA` for a
#'     trace written by a version of readgpt that did not time requests.}
#'   \item{`error`}{The error, or `NA`.}
#'   \item{`prompt`, `reply`}{The messages sent, each as `"[role] text"`, and
#'     the text that came back.}
#' }
#' @param x A `gr_trace`.
#' @param row.names Optional row names for the result.
#' @param optional,... Ignored; part of the [as.data.frame()] generic.
#' @seealso [gr_trace_summary()], [as_json()], [gr_answer], [gr_cache()]
#' @export
#' @examples
#' tr <- gr_trace(meta = list(purpose = "demo"))
#' cl <- gr_mock_client(function(m, p) "an answer")
#' ch <- gr_segment(readgpt_example(), list(method = "sentence", max_tokens = 150))
#' invisible(gr_read(ch, "What was revenue?", cl, "map_reduce", trace = tr))
#' print(tr)
#'
#' # One row per request, with what each cost and how long it took.
#' reqs <- as.data.frame(tr)
#' reqs[, c("step", "stage", "tokens_in", "tokens_out", "usd", "seconds")]
gr_trace <- function(run_id = NULL, meta = list()) {
  e <- new.env(parent = emptyenv())
  e$run_id <- as_chr1(run_id %||% gr_new_id("run"))
  e$started <- Sys.time()
  # The version that produced the run, so a trace read back later says which
  # readgpt wrote it. Prompts and readers change between versions.
  e$meta <- c(list(readgpt = tryCatch(as.character(utils::packageVersion("readgpt")),
                                      error = function(e) NA_character_)),
              meta)
  e$steps <- list()
  e$calls <- 0L
  e$cached <- 0L
  e$tokens_in <- 0L
  e$tokens_out <- 0L
  e$errors <- list()
  e$budget_stop <- FALSE
  # What the calls made so far cost, priced as they are recorded, so the
  # spending limit is enforced while a run is going and not only estimated
  # before it starts. A call to a model with no registered price adds nothing,
  # which makes this a floor on the spend: a floor that reaches the limit means
  # the spend has too. gr_trace_cost() is the full account, and says "unknown"
  # where this cannot.
  e$spent_usd <- 0
  # "calls" or "cost": which limit set `budget_stop`.
  e$stop_reason <- NA_character_
  structure(e, class = "gr_trace")
}

#' @noRd
trace_record <- function(trace, label, messages, result, params = list(), seconds = NA_real_) {
  if (is.null(trace) || !inherits(trace, "gr_trace")) return(invisible(NULL))
  trace$calls <- trace$calls + 1L
  if (isTRUE(result$cached)) trace$cached <- trace$cached + 1L
  trace$tokens_in <- trace$tokens_in + as.integer(result$usage$input %||% 0L)
  trace$tokens_out <- trace$tokens_out + as.integer(result$usage$output %||% 0L)
  # Priced by the model the step records, as gr_trace_cost() prices it. A call
  # answered from a cache or a replay spent nothing.
  if (!isTRUE(result$cached)) {
    usd <- tryCatch(suppressWarnings(as.numeric(gr_estimate_cost(
      as_chr1(result$model %||% params$model, "unknown"),
      result$usage$input %||% 0L, result$usage$output %||% 0L))),
      error = function(e) NA_real_)
    if (length(usd) == 1L && !is.na(usd)) trace$spent_usd <- (trace$spent_usd %||% 0) + usd
  }
  if (!isTRUE(result$ok)) {
    trace$errors <- c(trace$errors, list(list(step = length(trace$steps) + 1L, label = label,
                                              error = mark_utf8(as_chr1(result$error)))))
  }
  trace$steps <- c(trace$steps, list(list(
    step = length(trace$steps) + 1L,
    label = as_chr1(label),
    at = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3"),
    model = as_chr1(result$model %||% params$model, NA_character_),
    ok = isTRUE(result$ok),
    cached = isTRUE(result$cached),
    # mark_utf8(), not enc2utf8(). A trace is serialised by jsonlite, and
    # jsonlite escapes bytes it cannot interpret in the CURRENT LOCALE: an
    # unmarked string holding UTF-8 bytes came out of `as_json()` as the literal
    # text "caf<c3><a9>" on any machine whose locale is not UTF-8. The bytes
    # were always right; nothing had told R what they were. Labelling them here
    # -- converting nothing -- is what makes a saved trace portable, and is what
    # a replay from a file depends on.
    prompt = lapply(messages, function(m) list(role = m$role,
                                               content = mark_utf8(as_chr1(m$content)))),
    response = mark_utf8(as_chr1(result$text)),
    error = if (isTRUE(result$ok)) NULL else mark_utf8(as_chr1(result$error)),
    tokens = list(input = result$usage$input %||% 0L, output = result$usage$output %||% 0L),
    # Wall-clock time of the request, retries and their pauses included.
    seconds = round(as_num1(seconds, NA_real_), 3),
    # Recorded because a replay has to be able to reach the same decisions the
    # live run reached, and this is one a caller acts on:
    # gr_synthesise(coherence = TRUE) discards a revision whose finish_reason is
    # "length" -- a review with its ending cut off. Unrecorded, the replay saw
    # NA, kept the truncated revision, published a different document, and
    # reported 0 misses: it certified itself as an exact reproduction of a run
    # it had not reproduced.
    finish_reason = as_chr1(result$finish_reason, NA_character_),
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
  # Exact names: `$recipe` would match the `recipes` a comparison's trace keeps.
  src <- child$meta[["source", exact = TRUE]]
  parent$steps <- c(parent$steps, lapply(child$steps, function(st) {
    st$step <- st$step + off
    st$recipe <- as_chr1(child$meta[["recipe", exact = TRUE]], NA_character_)
    # Which document a folded-in step was about, so a corpus trace can still be
    # read request by request. A step folded in twice keeps its first document.
    if (is.null(st$source) && !is.null(src)) st$source <- as_chr1(src, NA_character_)
    st
  }))
  parent$calls <- parent$calls + child$calls
  parent$cached <- (parent$cached %||% 0L) + (child$cached %||% 0L)
  parent$tokens_in <- parent$tokens_in + child$tokens_in
  parent$tokens_out <- parent$tokens_out + child$tokens_out
  parent$errors <- c(parent$errors, child$errors)
  parent$spent_usd <- (parent$spent_usd %||% 0) + (child$spent_usd %||% 0)
  if (isTRUE(child$budget_stop)) {
    parent$budget_stop <- TRUE
    parent$stop_reason <- child$stop_reason %||% NA_character_
  }
  invisible(NULL)
}

#' Label the steps from `from` on with the document and recipe they were for,
#' where a step does not say already. For a trace the caller passed in, which
#' may hold other runs, the trace's own meta cannot say which run a step was.
#' @noRd
trace_stamp <- function(trace, from, source = NULL, recipe = NULL) {
  if (!inherits(trace, "gr_trace")) return(invisible(NULL))
  n <- length(trace$steps)
  if (from > n) return(invisible(NULL))
  idx <- seq.int(from, n)
  trace$steps[idx] <- lapply(trace$steps[idx], function(st) {
    if (is.null(st$source) && !is.null(source)) st$source <- as_chr1(source, NA_character_)
    if (is.null(st$recipe) && !is.null(recipe)) st$recipe <- as_chr1(recipe, NA_character_)
    st
  })
  invisible(NULL)
}

#' A parent trace to fold a stage into, validated.
#'
#' `trace` does two jobs at once: it is the ledger of what a run did, and it is
#' the counter `trace_can_call()` measures `max_calls` against. Those want
#' opposite things when several stages of one review share it -- the ledger
#' should accumulate, the counter must not, or screening's calls are charged
#' against the write-up's per-stage ceiling and every section comes back blank.
#'
#' So a `trace` argument on a STAGE means "the parent to fold this stage's
#' accounting into". The stage still runs on its own, exactly as each document
#' inside gr_read_many() does, and `$trace` on the result is the stage's own.
#' @noRd
as_parent_trace <- function(trace, arg = "trace") {
  if (is.null(trace)) return(NULL)
  if (!inherits(trace, "gr_trace")) {
    gr_abort(sprintf("`%s` must come from gr_trace(), or be NULL.", arg),
             class = "gr_bad_trace")
  }
  trace
}

#' May the run make another call?
#'
#' Strategies consult this before every call. It says no when the call cap would
#' be passed, or when what the run has spent has reached the spending limit, and
#' it records which of the two stopped the run. The old code had no equivalent,
#' which is why a negative token budget could turn into one API call per word
#' with nothing to stop it. The spending limit used to be checked only once,
#' against an estimate that priced every reply at its cap, so a run was refused
#' at an estimate fifty times what it would have cost, and nothing checked what
#' a run that did start went on to spend.
#'
#' The cost of a request is known only once it is made, so a run can pass the
#' limit by what one request costs. A parallel batch is checked before it is
#' sent and not inside it; see gr_lapply() and preflight().
#' @noRd
trace_can_call <- function(trace, n = 1L) {
  if (is.null(trace) || !inherits(trace, "gr_trace")) return(TRUE)
  cap <- gr_options("max_calls")
  if (!is.null(cap) && is.finite(cap) && (trace$calls + n) > cap) {
    trace$budget_stop <- TRUE
    trace$stop_reason <- "calls"
    return(FALSE)
  }
  if (limit_reached(trace$spent_usd %||% 0, gr_options("max_cost_usd"))) {
    trace$budget_stop <- TRUE
    trace$stop_reason <- "cost"
    return(FALSE)
  }
  TRUE
}

#' Has a run that spent `spent` reached the spending limit `limit`?
#'
#' At or past it, except that spending nothing never reaches a limit: under
#' `max_cost_usd = 0` a model registered at no cost still runs, as it did when
#' the limit was only an estimate, and a priced one is stopped.
#' @noRd
limit_reached <- function(spent, limit) {
  if (is.null(limit) || !is.finite(limit)) return(FALSE)
  spent > limit || (spent == limit && spent > 0)
}

#' A dollar amount for a message: cents when it is at least a cent, and two
#' significant figures below that, so a limit of $0.003 is not reported as
#' being passed by "$0.00".
#' @noRd
fmt_usd <- function(x) {
  x <- as_num1(x, NA_real_)
  if (is.na(x) || !is.finite(x)) return(format(x))
  if (abs(x) >= 0.01) sprintf("%.2f", x) else format(signif(x, 2), scientific = FALSE)
}

#' The limit that stopped a run, by name, for the messages readers leave.
#' @noRd
cap_name <- function(trace) {
  if (inherits(trace, "gr_trace") && identical(trace$stop_reason, "cost")) "spending limit"
  else "call cap"
}

#' Warn that a batch of `n` requests cannot all be made, naming the limit and
#' the setting that raises it.
#' @noRd
warn_capped_batch <- function(trace, who, n, advice) {
  if (identical(cap_name(trace), "spending limit")) {
    gr_warn(sprintf(paste0("%s needs %d calls but the run has spent $%s, which reaches the $%s ",
                           "spending limit; raise gr_options(max_cost_usd =)."),
                    who, n, fmt_usd(trace$spent_usd),
                    format(gr_options("max_cost_usd"), scientific = FALSE)),
            class = "gr_cost_cap")
  } else {
    # %s, not %d: max_calls is a whole DOUBLE, and %d refuses one beyond the
    # integer range.
    gr_warn(sprintf("%s needs %d calls but the run cap is %s; %s or raise gr_options(max_calls =).",
                    who, n, format(gr_options("max_calls"), scientific = FALSE), advice),
            class = "gr_call_cap")
  }
}

#' Summarise a trace
#' @param trace A `gr_trace`.
#' @return A one-row data frame: `run_id`, `calls`, `cached`, `steps`,
#'   `tokens_in`, `tokens_out`, `errors`, `elapsed_s`. There is no cost column
#'   -- combine `tokens_in`/`tokens_out` with [gr_estimate_cost()] for that.
#'
#'   `cached` is how many of those calls were answered from a [gr_cache()] or a
#'   [gr_replay_client()]. Their tokens are still counted in `tokens_in` and
#'   `tokens_out`, because that is how large the prompts and replies were; they
#'   were simply not paid for again. A run with `cached == calls` cost nothing,
#'   so feeding its token counts to [gr_estimate_cost()] gives you what the run
#'   *would* have cost, not what it did.
#' @seealso [gr_trace()], [as_json()], [gr_estimate_cost()], [gr_cache()]
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "45.2 million dollars")
#' ans <- answer_document(readgpt_example(), "What was revenue?", "thorough", client = cl)
#' gr_trace_summary(ans$trace)
#' gr_estimate_cost("gpt-4o", ans$trace$tokens_in, ans$trace$tokens_out)
gr_trace_summary <- function(trace) {
  stopifnot(inherits(trace, "gr_trace"))
  data.frame(
    run_id = trace$run_id,
    calls = trace$calls,
    cached = as.integer(trace$cached %||% 0L),
    steps = length(trace$steps),
    tokens_in = trace$tokens_in,
    tokens_out = trace$tokens_out,
    errors = length(trace$errors),
    elapsed_s = round(as.numeric(difftime(Sys.time(), trace$started, units = "secs")), 2),
    stringsAsFactors = FALSE
  )
}

#' @rdname gr_trace
#' @export
as.data.frame.gr_trace <- function(x, row.names = NULL, optional = FALSE, ...) {
  # The requests gr_trace_cost() prices, so the two agree on what a run cost.
  steps <- Filter(function(s) !identical(s$kind, "local") && !is.null(s$tokens), x$steps)
  chr <- function(f) vapply(steps, function(s) as_chr1(f(s), NA_character_), character(1))
  tin <- vapply(steps, function(s) as_int1(s$tokens$input, NA_integer_), integer(1))
  tout <- vapply(steps, function(s) as_int1(s$tokens$output, NA_integer_), integer(1))
  cached <- vapply(steps, function(s) isTRUE(s$cached), logical(1))
  model <- chr(function(s) s$model)
  # A cached request is priced at no tokens, as gr_trace_cost() prices it: 0
  # for a model with a price, NA for one without.
  usd <- vapply(seq_along(steps), function(i) {
    tryCatch(suppressWarnings(as.numeric(gr_estimate_cost(
      as_chr1(model[i], "unknown"), if (cached[i]) 0L else tin[i],
      if (cached[i]) 0L else tout[i]))), error = function(e) NA_real_)
  }, numeric(1))
  prompt <- vapply(steps, function(s) {
    parts <- vapply(s$prompt %||% list(), function(m) sprintf("[%s] %s", as_chr1(m$role, "?"),
                                                              as_chr1(m$content, "")),
                    character(1))
    paste(parts, collapse = "\n\n")
  }, character(1))
  out <- data.frame(
    step = vapply(steps, function(s) as_int1(s$step, NA_integer_), integer(1)),
    document = chr(function(s) s$source %||% x$meta[["source", exact = TRUE]]),
    recipe = chr(function(s) s$recipe %||% x$meta[["recipe", exact = TRUE]]),
    stage = chr(function(s) s$label),
    model = model,
    ok = vapply(steps, function(s) isTRUE(s$ok), logical(1)),
    cached = cached,
    tokens_in = tin,
    tokens_out = tout,
    usd = usd,
    seconds = vapply(steps, function(s) as_num1(s$seconds, NA_real_), numeric(1)),
    error = chr(function(s) s$error),
    prompt = prompt,
    reply = chr(function(s) s$response),
    stringsAsFactors = FALSE)
  if (!is.null(row.names)) rownames(out) <- row.names
  out
}

#' @export
print.gr_trace <- function(x, ...) {
  s <- gr_trace_summary(x)
  cat(sprintf("<gr_trace %s>  %d steps, %d model calls%s, %d in / %d out tokens, %d error(s)\n",
              s$run_id, s$steps, s$calls,
              if (s$cached > 0L) sprintf(" (%d cached)", s$cached) else "",
              s$tokens_in, s$tokens_out, s$errors))
  labs <- vapply(x$steps, function(st) as_chr1(st$label), character(1))
  if (length(labs)) {
    tab <- table(labs)
    cat("  steps:", paste(sprintf("%s x%d", names(tab), as.integer(tab)), collapse = ", "), "\n")
  }
  cat(sprintf("  cost: %s\n", format_trace_cost(x)))
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
#' ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl)
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
