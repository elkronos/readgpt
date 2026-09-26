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
#'
#' `fn(item, trace)` is expected to make at most one model request per item,
#' which every caller does. `client` is the client those requests go to; when
#' it is not given it is looked for in the frames `fn` was written in, where
#' every caller's `client` is. `item_usd` is the most one item can cost, its
#' prompt with the reply at its cap. Without it, a spending limit is the
#' caller's to check before the batch, as preflight() does for a read.
#' @noRd

gr_lapply <- function(x, fn, parallel = NULL, workers = NULL, key = NULL, label = "task",
                      trace = NULL, client = NULL, item_usd = NULL) {
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
  # Room for one more call is not room for the batch. This used to be the only
  # check, so a batch went out whole whenever the run had any headroom at all:
  # a parallel proposition segmentation under max_calls = 20 and a $0.02 limit
  # made 40 calls and spent $0.41, and a hierarchical read's later levels went
  # 13% past max_calls. Workers cannot see what the run spends, so the whole
  # batch has to fit before it is sent; when it does not, it runs here, where
  # every item is checked and the run stops where the sequential one would.
  if (inherits(trace, "gr_trace")) {
    short <- batch_shortfall(trace, length(x), item_usd)
    if (!is.null(short)) {
      gr_msg(sprintf(paste0("Running %d %s(s) one at a time: sent to workers together they could ",
                            "pass the run's %s, and a batch cannot be stopped part way."),
                     length(x), label, short))
      return(sequential())
    }
  }
  client <- client %||% closure_client(fn)
  # A replay hands out the responses recorded for a prompt in the order they
  # were recorded, and counts its hits and misses as it goes. Each worker got a
  # copy of the cursor and threw its moves away, so a prompt recorded twice
  # replayed its first response twice, and $stats() reported an exact replay
  # with no misses. A replay makes no requests, so running it here costs
  # nothing but the illusion of speed.
  if (inherits(client, "gr_replay_client")) {
    gr_msg(sprintf("Running %d %s(s) one at a time: a replay hands out its recording in order.",
                   length(x), label))
    return(sequential())
  }

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
  # The user's own functions travel inside those, and inside the client: a mock
  # or backend handler, a tokenizer, a registered reader. One written at the
  # top level of a script is serialised by reference to the global environment,
  # which is empty in a fresh worker, so a handler that called a helper from
  # the same script failed on every chunk ("could not find function") and the
  # read came back NOT_IN_DOCUMENT, and a tokenizer that did aborted the run.
  # What they refer to in the global environment is found here and sent with
  # the batch, which future assigns into the worker's global environment.
  user <- tryCatch(user_globals(list(client, regs)), error = function(e) e)
  if (inherits(user, "error")) {
    gr_warn(sprintf(paste0("parallel = TRUE could not work out what your own functions (a client ",
                           "handler, a tokenizer, a registered strategy) need from your workspace ",
                           "(%s); running sequentially instead."), conditionMessage(user)),
            class = "gr_parallel_unavailable")
    return(sequential())
  }

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
  # The same holds for what the client keeps: a mock's or backend's log of the
  # calls it answered, and an attached cache's hit, miss and write counts. A
  # worker's copy recorded them and was thrown away, so $calls() came back
  # empty after a parallel run and gr_cache_stats() counted one call in seven.
  # Each item reports what it added, and the parent adds it back in the order
  # of `x`, as it does the traces. Written out here rather than as package
  # helpers, because a worker resolves package functions in the readgpt it
  # loads, which is not always the one that sent the batch.
  client_state <- function(cl) {
    if (!is.list(cl)) return(NULL)
    log <- cl[[".log", exact = TRUE]]
    cache <- cl[[".cache", exact = TRUE]]
    list(log = if (is.environment(log)) log,
         stats = if (inherits(cache, "gr_cache") && is.environment(cache$.stats)) cache$.stats)
  }
  state <- client_state(client)
  wrapped <- function(item) {
    if (nzchar(key)) Sys.setenv(OPENAI_API_KEY = key)
    gr_state$options <- opts
    for (nm in names(regs)) if (!is.null(regs[[nm]])) assign(nm, regs[[nm]], envir = gr_state)
    sub <- gr_trace(meta = parent_meta)
    n_calls <- length(state$log$calls)
    n_embeds <- length(state$log$embeds)
    counts <- function() c(state$stats$hits %||% 0L, state$stats$misses %||% 0L,
                           state$stats$writes %||% 0L)
    before <- counts()
    value <- fn(item, sub)
    list(value = value, trace = sub,
         calls = state$log$calls[seq_along(state$log$calls) > n_calls],
         embeds = state$log$embeds[seq_along(state$log$embeds) > n_embeds],
         cache = counts() - before)
  }
  out <- future.apply::future_lapply(x, wrapped, future.seed = TRUE,
                                     future.globals = user$globals,
                                     future.packages = unique(c("readgpt", user$packages)))
  for (r in out) {
    trace_absorb(trace, r$trace)
    if (!is.null(state$log)) {
      state$log$calls <- c(state$log$calls, r$calls)
      state$log$embeds <- c(state$log$embeds, r$embeds)
    }
    if (!is.null(state$stats)) {
      state$stats$hits <- state$stats$hits + as.integer(r$cache[1])
      state$stats$misses <- state$stats$misses + as.integer(r$cache[2])
      state$stats$writes <- state$stats$writes + as.integer(r$cache[3])
    }
  }
  # A batch that passed a limit anyway -- one whose caller vouched for its
  # spending and was wrong -- is recorded as having done so. The workers'
  # traces never stop anything, so absorbing them could not say it.
  if (inherits(trace, "gr_trace")) {
    cap <- gr_options("max_calls")
    limit <- gr_options("max_cost_usd")
    if (!is.null(cap) && is.finite(cap) && trace$calls > cap) {
      trace$budget_stop <- TRUE
      trace$stop_reason <- "calls"
    } else if (!is.null(limit) && is.finite(limit) && (trace$spent_usd %||% 0) > limit) {
      trace$budget_stop <- TRUE
      trace$stop_reason <- "cost"
    }
  }
  lapply(out, `[[`, "value")
}

#' Which limit a batch of `n` items could pass, or NULL when it fits.
#'
#' Measured without recording a stop: the batch then runs in this process,
#' where it is the items that are refused, and only a refusal stops a run.
#' Each item makes at most one request, so `n` more calls is the most a batch
#' adds; `item_usd`, when known, prices each at its worst.
#' @noRd
batch_shortfall <- function(trace, n, item_usd = NULL) {
  cap <- gr_options("max_calls")
  if (!is.null(cap) && is.finite(cap) && trace$calls + n > cap) return("call cap")
  limit <- gr_options("max_cost_usd")
  if (is.null(limit) || !is.finite(limit) || is.null(item_usd)) return(NULL)
  # A model with no price adds nothing to what the trace has spent, so the
  # limit cannot see it in this process either; it is counted as the trace
  # counts it.
  per <- as_num1(item_usd, 0)
  if ((trace$spent_usd %||% 0) + n * max(per, 0) > limit) return("spending limit")
  NULL
}

#' The client a batch's function calls, found in the frames it was written in.
#'
#' Only below the package namespace: past it lie the search path and the
#' user's workspace, where a `client` is not the one the batch uses.
#' @noRd
closure_client <- function(fn) {
  env <- if (is.function(fn)) environment(fn)
  if (!is.environment(env)) return(NULL)
  top <- topenv(env)
  while (!identical(env, top) && !identical(env, emptyenv())) {
    cl <- get0("client", envir = env, inherits = FALSE)
    if (inherits(cl, "gr_client")) return(cl)
    env <- parent.env(env)
  }
  NULL
}

#' What the user's own functions need from the global environment.
#'
#' Every closure reachable through `x` (lists, a few levels deep) whose home is
#' the user's workspace rather than a package is scanned, recursively, by
#' future's own globals finder. A name the function finds in an environment of
#' its own travels with it already and is left out, so a factory's `k` is not
#' written over a workspace `k`.
#' @noRd
user_globals <- function(x) {
  fns <- list()
  walk <- function(v, depth) {
    if (is.function(v)) {
      if (!is.primitive(v) && identical(topenv(environment(v)), globalenv())) {
        fns[[length(fns) + 1L]] <<- v
      }
    } else if (is.list(v) && depth < 4L) {
      for (el in v) walk(el, depth + 1L)
    }
  }
  walk(x, 0L)
  globals <- list()
  packages <- character(0)
  home <- function(name, env) {
    while (!identical(env, emptyenv())) {
      if (exists(name, envir = env, inherits = FALSE)) return(env)
      env <- parent.env(env)
    }
    NULL
  }
  for (f in fns) {
    probe <- new.env(parent = environment(f))
    assign(".gr_fn", f, envir = probe)
    gp <- future::getGlobalsAndPackages(quote(.gr_fn()), envir = probe, globals = TRUE)
    packages <- c(packages, gp$packages)
    for (nm in setdiff(names(gp$globals), c(".gr_fn", names(globals)))) {
      # `[<-` with a list, so a global that is NULL is sent rather than dropped.
      if (identical(home(nm, environment(f)), globalenv())) globals[nm] <- list(gp$globals[[nm]])
    }
  }
  list(globals = globals, packages = unique(packages))
}
