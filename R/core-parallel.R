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

#' Apply a function over a list, optionally in parallel.
#'
#' Falls back to sequential -- with a warning, not silently -- when the future
#' packages are unavailable.
#' @noRd
gr_lapply <- function(x, fn, parallel = NULL, workers = NULL, key = NULL, label = "task") {
  parallel <- isTRUE(parallel %||% gr_options("parallel"))
  if (!parallel || length(x) <= 1L) return(lapply(x, fn))

  if (!requireNamespace("future", quietly = TRUE) ||
      !requireNamespace("future.apply", quietly = TRUE)) {
    gr_warn(paste0("parallel = TRUE needs the 'future' and 'future.apply' packages; ",
                   "running sequentially instead."), class = "gr_parallel_unavailable")
    return(lapply(x, fn))
  }

  workers <- as.integer(clamp(workers %||% gr_options("workers"), 1, 32))
  # Workers are separate processes: the API key lives in this process's
  # environment and must be carried across explicitly.
  key <- as_chr1(key %||% tryCatch(gr_api_key(), error = function(e) ""))
  opts <- gr_options()

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)
  future::plan(future::multisession, workers = workers)
  gr_msg(sprintf("Running %d %s(s) across %d workers.", length(x), label, workers))

  wrapped <- function(item) {
    if (nzchar(key)) Sys.setenv(OPENAI_API_KEY = key)
    gr_state$options <- opts
    fn(item)
  }
  future.apply::future_lapply(x, wrapped, future.seed = TRUE,
                              future.globals = TRUE, future.packages = "readgpt")
}
