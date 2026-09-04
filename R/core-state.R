# core-state.R -- package-private mutable state and user-facing options.
#
# The previous codebase kept its document cache in `.GlobalEnv` under
# `.doc_cache`, which leaked between Shiny sessions and could never be cleared.
# All mutable state now lives in one package-private environment, is namespaced,
# and is individually clearable.

gr_state <- new.env(parent = emptyenv())

gr_state$counter        <- 0L
gr_state$extractors     <- list()
gr_state$cleaners       <- list()
gr_state$segmenters     <- list()
gr_state$readers        <- list()
gr_state$embedders      <- list()
gr_state$models         <- list()
gr_state$model_patterns <- list()
gr_state$doc_cache      <- new.env(parent = emptyenv())
gr_state$embed_cache    <- new.env(parent = emptyenv())
gr_state$tokenizer      <- NULL
gr_state$client         <- NULL

gr_defaults <- list(
  verbose             = TRUE,
  tokenizer           = "heuristic",
  model               = "gpt-5.6-terra",
  embedding_model     = "text-embedding-3-small",
  api_base            = "https://api.openai.com/v1",
  api                 = "responses",   # "responses" | "chat"
  temperature         = NULL,          # NULL = let the model decide / omit
  max_retries         = 4L,
  retry_pause_base    = 2,
  request_timeout     = 120,
  # Fraction of the context window left unfilled, to absorb tokenizer error.
  safety_margin       = 0.10,
  # Hard floor on how much room is always left for a completion.
  min_output_tokens   = 256L,
  cache_documents     = TRUE,
  cache_embeddings    = TRUE,
  # Where gr_cache() keeps model responses. NULL means a directory under
  # tempdir(), resolved at call time -- a package must not write to a user's
  # filesystem unasked, and a literal tempdir() here would be frozen at install
  # time rather than evaluated per session.
  cache_dir           = NULL,
  # Which registered embedder to use. NULL means "whatever the client brought,
  # otherwise api" -- the meaning is in the value, so restoring a saved option
  # list cannot change which embedder is chosen.
  embedder            = NULL,
  parallel            = FALSE,
  workers             = 4L,
  # Refuse to start a run whose *estimated* cost exceeds this (USD). NULL = off.
  max_cost_usd        = 5,
  # Refuse to issue more than this many model calls in one run.
  max_calls           = 400L,
  unknown_model_action = "warn"        # "warn" | "error"
)

#' Get or set package options
#'
#' `gr_options()` with no arguments returns the full option list. Called with
#' a single string it returns that option. Called with `name = value` pairs it
#' sets them and invisibly returns the previous values, so it composes with
#' `on.exit()`.
#'
#' Options are read at *call* time, never captured at load time, so changing an
#' option mid-session affects subsequent runs.
#'
#' @param ... Nothing, a single option name, `name = value` pairs, or a named
#'   list (the form `gr_options()` itself returns, so a saved value can be
#'   restored directly).
#' @return The option list, a single option value, or (when setting) the old
#'   values invisibly.
#'
#' @section Options:
#' \describe{
#'   \item{`verbose` (TRUE)}{Print progress for each ingest/segment/read stage.}
#'   \item{`model` ("gpt-5.6-terra")}{Default chat model. Note the default is a
#'     reasoning model, which does not accept `temperature`.}
#'   \item{`embedding_model` ("text-embedding-3-small")}{Default embedding model.}
#'   \item{`tokenizer` ("heuristic")}{Token counter: `"heuristic"` (conservative,
#'     no dependencies), `"words"`, `"chars"`, `"tiktoken"` (needs reticulate),
#'     or a name registered via [gr_set_tokenizer()].}
#'   \item{`api` ("responses")}{`"responses"` or `"chat"` request shape.}
#'   \item{`api_base` ("https://api.openai.com/v1")}{API root; point this at a
#'     proxy or a compatible endpoint.}
#'   \item{`temperature` (NULL)}{Default sampling temperature. `NULL` omits the
#'     field. Dropped automatically for models that reject it.}
#'   \item{`max_retries` (4)}{Retries for transient failures. HTTP 400 is never
#'     retried -- a malformed request stays malformed.}
#'   \item{`retry_pause_base` (2)}{Seconds; exponential backoff base.}
#'   \item{`request_timeout` (120)}{Per-request timeout, seconds.}
#'   \item{`safety_margin` (0.10)}{Fraction of the context window left unused to
#'     absorb tokenizer error. Not a spending cap -- see `max_cost_usd`.}
#'   \item{`min_output_tokens` (256)}{Floor on the completion room [gr_budget()]
#'     reserves *when `reserve_output` is not given explicitly*. An explicit
#'     `reserve_output` is honoured down to 1.}
#'   \item{`cache_documents` (TRUE)}{Cache ingestion per file + settings. The key
#'     covers file size, mtime and every option that changes the output.}
#'   \item{`cache_embeddings` (TRUE)}{Cache embeddings per text + model.}
#'   \item{`embedder` (NULL)}{Which registered embedder to use. `NULL` means the
#'     one the client carries, if any, and otherwise `"api"`. Naming one here
#'     overrides both. See [gr_embedders()]; recording a run with a
#'     *deterministic* embedder is what lets a [gr_replay_client()] reproduce its
#'     chunk ranking.}
#'   \item{`cache_dir` (NULL)}{Directory [gr_cache()] stores model responses in.
#'     `NULL` means a per-session directory under `tempdir()`, which costs
#'     nothing and disappears with the session. Set it to a real path, such as
#'     `tools::R_user_dir("readgpt", "cache")`, to keep responses across
#'     sessions and make a long run resumable.}
#'   \item{`parallel` (FALSE)}{Run per-chunk calls concurrently. Needs the
#'     future and future.apply packages; without them it warns and runs
#'     sequentially.}
#'   \item{`workers` (4)}{Worker processes when `parallel` is TRUE.}
#'   \item{`max_cost_usd` (5)}{Refuse a run whose pre-flight estimate exceeds
#'     this, in USD. `NULL` disables the check.}
#'   \item{`max_calls` (400)}{Hard cap on model calls per run, checked before
#'     the first call and again before every subsequent one. `NULL` removes the
#'     cap.}
#'   \item{`unknown_model_action` ("warn")}{`"warn"` or `"error"` when a model id
#'     is not in the registry.}
#' }
#'
#' @seealso [gr_register_model()] to correct a model's limits,
#'   [gr_set_tokenizer()], [gr_budget()], [gr_cache()]
#' @export
#' @examples
#' old <- gr_options(verbose = FALSE)
#' gr_options("verbose")
#' gr_options(old)
gr_options <- function(...) {
  args <- list(...)
  merged <- function() {
    out <- gr_defaults
    cur <- as.list(gr_state$options %||% list())
    for (nm in names(cur)) out[nm] <- list(cur[[nm]])
    out
  }
  if (length(args) == 0L) return(merged())
  if (length(args) == 1L && is.list(args[[1]]) && !is.null(names(args[[1]]))) {
    args <- args[[1]]
  } else if (length(args) == 1L && is.character(args[[1]]) && is.null(names(args))) {
    nm <- args[[1]]
    cur <- merged()
    if (!nm %in% names(cur)) {
      gr_abort(sprintf("Unknown option '%s'. Known options: %s.",
                       nm, paste(names(gr_defaults), collapse = ", ")))
    }
    return(cur[[nm]])
  }
  if (is.null(names(args)) || any(!nzchar(names(args)))) {
    gr_abort("gr_options() setters must be named, e.g. gr_options(model = 'gpt-5.6-terra').")
  }
  unknown <- setdiff(names(args), names(gr_defaults))
  if (length(unknown)) {
    gr_abort(sprintf("Unknown option(s): %s. Known options: %s.",
                     paste(unknown, collapse = ", "), paste(names(gr_defaults), collapse = ", ")))
  }
  cur <- as.list(gr_state$options %||% list())
  # merged(), not modifyList(). modifyList() DELETES a key whose value is NULL,
  # so once an option had been *stored* as NULL -- which is what restoring a
  # saved list does for `temperature`, `max_cost_usd`, `cache_dir` or `embedder`
  # -- the returned "old" value came back with the name NA and the next
  # gr_options(old) failed with "Unknown option(s): NA". That is the documented
  # on.exit() pattern breaking on the second use, and it is the same NULL trap
  # the setter below was already fixed for.
  old <- merged()[names(args)]
  # modifyList() DELETES a key whose value is NULL, which silently reverted the
  # option to its default -- so `gr_options(max_cost_usd = NULL)`, documented as
  # disabling the cost cap, quietly restored the $5 cap instead. Assign directly
  # so NULL is stored as a value.
  for (nm in names(args)) cur[nm] <- list(args[[nm]])
  gr_state$options <- cur
  invisible(old)
}

#' Clear the in-memory document and embedding caches.
#'
#' Not to be confused with [gr_cache_clear()], which empties an on-disk cache of
#' *model responses*. These two are different caches with nearly the same name,
#' which is why this one is `gr_flush_caches()`: while it was called
#' `gr_cache_clear()` it shadowed the exported function of that name -- R
#' collates `R/core-state.R` after `R/core-cache.R`, so the internal definition
#' silently won and the package exported this function under the other one's
#' documentation.
#' @param what One or more of "documents", "embeddings", "all".
#' @return Invisibly, the names cleared.
#' @noRd
gr_flush_caches <- function(what = "all") {
  what <- match.arg(what, c("all", "documents", "embeddings"), several.ok = TRUE)
  if ("all" %in% what) what <- c("documents", "embeddings")
  if ("documents"  %in% what) gr_state$doc_cache   <- new.env(parent = emptyenv())
  if ("embeddings" %in% what) gr_state$embed_cache <- new.env(parent = emptyenv())
  invisible(what)
}

#' Generic registry machinery.
#'
#' Every pluggable axis (extractors, cleaners, segmenters, readers) uses this,
#' so third-party additions behave exactly like built-ins.
#' @noRd
registry_set <- function(slot, name, entry) {
  if (!is_nonblank(name)) gr_abort("Registry name must be a non-empty string.")
  reg <- gr_state[[slot]]
  reg[[name]] <- entry
  gr_state[[slot]] <- reg
  invisible(name)
}

#' @noRd
registry_get <- function(slot, name, what = slot) {
  reg <- gr_state[[slot]]
  if (!is_nonblank(name) || is.null(reg[[name]])) {
    gr_abort(sprintf("Unknown %s '%s'. Registered: %s.",
                     sub("s$", "", what), as_chr1(name, "<missing>"),
                     paste(sort(names(reg)), collapse = ", ")),
             class = "gr_unknown_method")
  }
  reg[[name]]
}

#' @noRd
registry_table <- function(slot, cols) {
  reg <- gr_state[[slot]]
  if (!length(reg)) {
    empty <- as.data.frame(stats::setNames(
      rep(list(character(0)), length(cols) + 1L), c("name", cols)),
      stringsAsFactors = FALSE)
    return(empty)
  }
  rows <- lapply(names(reg), function(nm) {
    e <- reg[[nm]]
    vals <- lapply(cols, function(cl) as_chr1(e[[cl]], ""))
    stats::setNames(c(list(nm), vals), c("name", cols))
  })
  df <- do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
  df <- df[order(df$name), , drop = FALSE]
  # Drop row names. rbind() over one-row frames keeps the registry key as a row
  # name, so every printed table showed the name twice -- once as a row label and
  # again in the `name` column.
  rownames(df) <- NULL
  df
}
