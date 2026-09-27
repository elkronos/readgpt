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
#'
#' An item that raises an error in a worker stops that worker's share of the
#' batch, and the error is raised once every item's trace is in the run's, so
#' the calls already made stay counted. A worker process that dies takes the
#' traces of its items with it; future raises that before anything returns.
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
  #
  # Here, but as the workers ran it: each item with a trace of its own, folded
  # in afterwards. A worker checks no limit against what the rest of the run
  # has spent, so a recorded batch that went past the spending limit made
  # every one of its calls; checked against the run's trace, its replay
  # stopped part way through, short of the recording, and said nothing of it.
  if (inherits(client, "gr_replay_client")) {
    if (as.integer(clamp(workers %||% gr_options("workers"), 1, 32)) <= 1L) return(sequential())
    gr_msg(sprintf("Running %d %s(s) one at a time: a replay hands out its recording in order.",
                   length(x), label))
    parent_meta <- if (inherits(trace, "gr_trace")) trace$meta else list()
    wrapped <- worker_item(fn, "", list(), list(), parent_meta, NULL)
    return(batch_collect(lapply(x, wrapped), trace, NULL))
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
  # One worker is this process by another route: future evaluates the futures
  # of a one-worker multisession plan here, so each item updated the client's
  # own log and cache counters in place, and the merge below then added what
  # the item reported a second time. $calls() and gr_cache_stats() counted
  # every call twice. One worker gains nothing, so the batch runs here.
  if (workers <= 1L) return(sequential())
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
  # or backend handler, a tokenizer. One written at the top level of a script
  # is serialised by reference to the global environment, which is empty in a
  # fresh worker, so a handler that called a helper from the same script failed
  # on every chunk ("could not find function") and the read came back
  # NOT_IN_DOCUMENT, and a tokenizer that did aborted the run. What they refer
  # to in the global environment is found here and sent with the batch, which
  # future assigns into the worker's global environment. Only for the functions
  # a worker runs (see worker_functions()): a registered embedder over a 320 MB
  # matrix, which no batch calls, went to every worker too, and past a
  # future.globals.maxSize of 500 MiB aborted a run that used to work.
  user <- tryCatch(user_globals(worker_functions(client, opts)), error = function(e) e)
  if (inherits(user, "error")) {
    gr_warn(sprintf(paste0("parallel = TRUE could not work out what your own functions (a client ",
                           "handler, a tokenizer) need from your workspace (%s); running ",
                           "sequentially instead."), conditionMessage(user)),
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
  # of `x`, as it does the traces. Written out here and in worker_item()'s
  # closure rather than as package helpers, because a worker resolves package
  # functions in the readgpt it loads, which is not always the one that sent
  # the batch.
  client_state <- function(cl) {
    if (!is.list(cl)) return(NULL)
    log <- cl[[".log", exact = TRUE]]
    cache <- cl[[".cache", exact = TRUE]]
    list(log = if (is.environment(log)) log,
         stats = if (inherits(cache, "gr_cache") && is.environment(cache$.stats)) cache$.stats)
  }
  state <- client_state(client)
  wrapped <- worker_item(fn, key, opts, regs, parent_meta, state)
  # A limit on what a future may carry (future.globals.maxSize) is checked as
  # each future is made and launched: a FutureError from the plan, or a plain
  # error from measuring the globals, as future versions differ. Every future
  # carries the same globals, and besides them only its share of `x` (indices,
  # or a few findings), so the first is refused before any item has run, and
  # the batch can run here instead of the whole run aborting. future.apply
  # warns that it is cancelling first; that warning is held back, and passed
  # on only when the error is.
  too_big <- function(e) {
    grepl("size of the globals|exceeds the maximum allowed size", conditionMessage(e))
  }
  cancelled <- NULL
  # The caller's random number stream is put back afterwards: future.seed =
  # TRUE draws the workers' seeds from it and moves it on, so a script that
  # drew random numbers after a read drew different ones with parallel = TRUE.
  out <- keep_caller_rng(tryCatch(withCallingHandlers(
    future.apply::future_lapply(x, wrapped, future.seed = TRUE,
                                future.globals = user$globals,
                                future.packages = unique(c("readgpt", user$packages))),
    warning = function(w) {
      if (grepl("Canceling all iterations", conditionMessage(w), fixed = TRUE)) {
        cancelled <<- w
        invokeRestart("muffleWarning")
      }
    }),
    error = function(e) {
      if (!too_big(e)) {
        if (!is.null(cancelled)) warning(cancelled)
        stop(e)
      }
      e
    }))
  if (inherits(out, "error")) {
    # future's message names the total and the limit first, then every global.
    sizes <- regmatches(conditionMessage(out),
                        gregexpr("[0-9.]+ (bytes|[KMGTP]iB)", conditionMessage(out)))[[1]]
    over <- if (length(sizes) >= 2L) sprintf(" (%s, against %s)", sizes[1], sizes[2]) else ""
    gr_warn(sprintf(paste0("parallel = TRUE could not send the batch to workers: what it carries%s ",
                           "is more than options(future.globals.maxSize =) allows. Raise the limit ",
                           "to run it in parallel; running sequentially instead."), over),
            class = "gr_parallel_unavailable")
    return(sequential())
  }
  batch_collect(out, trace, state)
}

#' Fold what each item of a batch returned (see worker_item()) into the run,
#' in the order of the batch, and hand back the values.
#'
#' `state` is the client's log and cache counters, to which only what a worker
#' process did is added: an item run in this process has updated them already.
#' @noRd
batch_collect <- function(out, trace, state) {
  me <- Sys.getpid()
  for (r in out) {
    trace_absorb(trace, r$trace)
    # (A one-worker batch no longer goes to workers, but future decides where a
    # future runs, not gr_lapply().)
    if (identical(r$pid, me)) next
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
    } else if (!is.null(limit) && is.finite(limit) && budget_spent(trace) > limit) {
      trace$budget_stop <- TRUE
      trace$stop_reason <- "cost"
    }
  }
  # An item that failed is raised only now, after every item's trace and calls
  # are in. Raised inside the batch, it took every worker's trace with it, and
  # the calls already made and paid for were gone from the run's account.
  failed <- Filter(function(r) !is.null(r$error), out)
  if (length(failed)) stop(failed[[1]]$error)
  lapply(out, `[[`, "value")
}

#' Evaluate `expr`, then put the caller's random number stream back as it was.
#'
#' Nothing in this package may move the caller's stream (see
#' with_private_rng()). The workers' seeds still come from it, so a run is as
#' reproducible as before; it is only no longer moved on by one.
#' @noRd
keep_caller_rng <- function(expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  expr
}

#' The function a worker runs for each item of a batch.
#'
#' Made by this function rather than inside gr_lapply(), because a closure
#' goes to a worker with every variable of the frame it was made in. Made
#' there, it carried the whole input list and the workspace globals gathered
#' for the batch, which future_lapply() sends as globals as well, so each of
#' them reached every worker twice. This frame holds only what an item needs.
#' @noRd
worker_item <- function(fn, key, opts, regs, parent_meta, state) {
  force(fn); force(key); force(opts); force(regs); force(parent_meta); force(state)
  # Set once an item fails. A worker runs its share of the batch in order, in
  # its own copy of this frame, and the items after a failure are not run, as
  # a sequential run would not run them.
  failed <- FALSE
  function(item) {
    if (nzchar(key)) Sys.setenv(OPENAI_API_KEY = key)
    # Empty for a batch run in this process, whose options are already these.
    if (length(opts)) gr_state$options <- opts
    for (nm in names(regs)) if (!is.null(regs[[nm]])) assign(nm, regs[[nm]], envir = gr_state)
    sub <- gr_trace(meta = parent_meta)
    n_calls <- length(state$log$calls)
    n_embeds <- length(state$log$embeds)
    counts <- function() c(state$stats$hits %||% 0L, state$stats$misses %||% 0L,
                           state$stats$writes %||% 0L)
    before <- counts()
    # Caught and handed back with the item's trace, for gr_lapply() to raise
    # once every trace is in, rather than raised here, which discarded them.
    err <- NULL
    value <- if (failed) NULL else tryCatch(fn(item, sub), error = function(e) {
      err <<- e
      NULL
    })
    if (!is.null(err)) failed <<- TRUE
    # The process it ran in: gr_lapply() adds back only what a worker did.
    list(value = value, error = err, trace = sub, pid = Sys.getpid(),
         calls = state$log$calls[seq_along(state$log$calls) > n_calls],
         embeds = state$log$embeds[seq_along(state$log$embeds) > n_embeds],
         cache = counts() - before)
  }
}

#' The user's own functions a batch runs in its workers.
#'
#' Every item of every batch makes its one request through gr_call(), which
#' calls the client's handler (a mock's or a backend's) and counts tokens with
#' the tokenizer in use. Nothing else of the user's runs there: readers,
#' segmenters, embedders, cleaners and extractors, and the client's embedding
#' handler, run in the process that builds the batch. What those refer to in
#' the workspace stays there, as it always did.
#' @noRd
worker_functions <- function(client, opts) {
  tok <- opts$tokenizer
  list(handler = if (is.list(client)) client[["handler", exact = TRUE]],
       tokenizer = if (is_nonblank(tok)) (gr_state$tokenizers %||% list())[[tok]])
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
  if (budget_spent(trace) + n * max(per, 0) > limit) return("spending limit")
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
    # No size limit here: this only gathers. Whether the batch is too big to
    # send is for the plan to say when it is sent, where gr_lapply() runs it
    # here instead and says why.
    gp <- future::getGlobalsAndPackages(quote(.gr_fn()), envir = probe, globals = TRUE,
                                        maxSize = +Inf)
    packages <- c(packages, gp$packages)
    for (nm in setdiff(names(gp$globals), c(".gr_fn", names(globals)))) {
      # `[<-` with a list, so a global that is NULL is sent rather than dropped.
      if (identical(home(nm, environment(f)), globalenv())) globals[nm] <- list(gp$globals[[nm]])
    }
  }
  list(globals = globals, packages = unique(packages))
}
