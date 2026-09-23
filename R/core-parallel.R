# core-parallel.R -- parallelism that actually parallelises.
#
# WHY THIS FILE EXISTS
# The old code threaded a `use_parallel` flag through every function and called
# `future.apply::future_lapply()` when it was TRUE. But `future::plan()` was
# never called anywhere in the repository, so the default `sequential` strategy
# applied and `future_lapply` ran the work one item at a time in the calling
# process -- identical to `lapply`, plus globals-detection overhead. The README
# advertised "Incorporates parallel processing"; the flag was decorative.
#
# There was also a latent trap: had someone added `plan(multisession)`, the
# Shiny app's `Sys.setenv(OPENAI_API_KEY = ...)` would not have propagated to
# worker processes, so `get_api_key()` would have called `stop()` inside every
# worker.
#
# `gr_lapply()` sets up a plan when asked, restores the caller's plan on exit,
# and explicitly exports the API key to workers.

#' The parts of `gr_state` a worker process needs to behave like its parent.
#'
#' Not the caches: `doc_cache` and `embed_cache` are environments holding whole
#' documents, and shipping them to every worker would cost more than the work.
#' Not `client`, which the closure already carries.
#' @noRd
.gr_worker_state <- c("extractors", "cleaners", "segmenters", "readers", "embedders",
                      "protocols", "models", "model_patterns", "tokenizers")

#' Apply a function over a list, optionally in parallel.
#'
#' Falls back to sequential -- with a warning, not silently -- when the future
#' packages are unavailable.
#' @noRd

gr_lapply <- function(x, fn, parallel = NULL, workers = NULL, key = NULL, label = "task",
                      trace = NULL) {
  # In this process, one item after another, with a progress line when someone
  # is watching (see core-progress.R).
  sequential <- function() {
    p <- progress_start(length(x), label, trace)
    done <- 0L
    with_progress(p, lapply(x, function(item) {
      out <- fn(item, trace)
      done <<- done + 1L
      progress_tick(p, done)
      out
    }))
  }
  parallel <- isTRUE(parallel %||% gr_options("parallel"))
  if (!parallel || length(x) <= 1L) return(sequential())
  # A run that has reached a limit sends no batch. Run in this process, each
  # item checks the run's own trace and returns without a request; handed to
  # workers, each of which starts a count of its own, every item would be sent.
  # Inside a batch the workers cannot see what the run spends, which is why
  # preflight() holds a parallel read to its worst case.
  if (inherits(trace, "gr_trace") && !trace_can_call(trace)) return(sequential())

  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    gr_warn(paste0("parallel = TRUE needs the 'future' and 'future.apply' packages; ",
                   "running sequentially instead."), class = "gr_parallel_unavailable")
    # Each item is handed the trace, as in every other branch. Leaving it out
    # once made this fallback fail with "argument \"trace\" is missing" after
    # ingestion, segmentation and any calls already paid for.
    return(sequential())
  }

  workers <- as.integer(clamp(workers %||% gr_options("workers"), 1, 32))
  # Workers are separate processes: the API key lives in this process's
  # environment and must be carried across explicitly.
  key <- as_chr1(key %||% tryCatch(gr_api_key(), error = function(e) ""))
  opts <- gr_options()
  # The registries travel too. `future.packages = "readgpt"` re-runs .onLoad()
  # in the worker, which registers the BUILT-INS and nothing else -- so a model
  # registered with gr_register_model() was unknown there, and the worker read
  # with the fallback context window instead of the one the caller set (4000
  # output tokens against a real ceiling of 512). Options crossed and the
  # registry they refer to did not, which is worse than neither crossing:
  # gr_options(tokenizer = "mytok") arrived naming a function that was not
  # there, and the parallel run aborted where the sequential one succeeded.
  regs <- mget(.gr_worker_state, envir = gr_state, ifnotfound = list(NULL))

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = workers)
  gr_msg(sprintf("Running %d %s(s) across %d workers.", length(x), label, workers))

  # A worker is a separate PROCESS, so the trace it is handed is a copy and
  # every call it records is thrown away with it. That is not a cosmetic loss:
  # the trace is where the token totals, gr_trace_cost() and the run's own
  # account of what it did all come from, so `parallel = TRUE` used to buy speed
  # by silently under-reporting the run in proportion to how parallel it was.
  #
  # Each worker therefore gets its OWN trace, returns it alongside the result,
  # and the parent absorbs them. Absorbing in the order `future_lapply()`
  # returns -- which is the order of `x`, not of completion -- means the
  # assembled trace reads the same whether or not the work was parallel.
  parent_meta <- if (inherits(trace, "gr_trace")) trace$meta else list()
  wrapped <- function(item) {
    if (nzchar(key)) Sys.setenv(OPENAI_API_KEY = key)
    gr_state$options <- opts
    for (nm in names(regs)) if (!is.null(regs[[nm]])) assign(nm, regs[[nm]], envir = gr_state)
    sub <- gr_trace(meta = parent_meta)
    list(value = fn(item, sub), trace = sub)
  }
  out <- future.apply::future_lapply(x, wrapped, future.seed = TRUE,
                                     future.globals = TRUE, future.packages = "readgpt")
  for (r in out) trace_absorb(trace, r$trace)
  lapply(out, `[[`, "value")
}
