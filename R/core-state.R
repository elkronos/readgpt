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
gr_state$protocols      <- list()
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
  # Extra HTTP headers on every request. A corporate gateway in front of OpenAI
  # and Anthropic rarely takes a plain bearer: Azure OpenAI authenticates with
  # `api-key`, API Management wants a subscription key, and most want a
  # correlation or cost-centre id. Set once here and every client inherits it.
  api_headers         = character(0),
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

#' What each numeric option is allowed to be, checked where it is SET.
#'
#' Every consumer used to invent its own opinion of a bad value, and the
#' opinions pointed the wrong way. `is.finite(max_cost_usd)` as a guard meant NA
#' or a value read from a config file as text silently removed the cost cap --
#' $710 spent against a $5 ceiling, no warning. `trace_can_call()` did the same
#' for `max_calls`, and disagreed with `preflight()`, which parsed a character
#' cap and enforced it: one option, two answers. `as.integer(min_output_tokens)`
#' turned 1e10 into NA and `gr_budget()` then raised a bare simpleError.
#'
#' Two kinds, treated differently on purpose. A CEILING -- `max_cost_usd`,
#' `max_calls` -- is refused when it cannot be compared: falling back to anything
#' would be choosing a limit the user did not set, and every fallback here had
#' meant "no limit". NULL and Inf are the two explicit ways to say "no limit".
#' A TUNING knob keeps its current value, with a warning, when the new one
#' cannot be read, and is clamped into range, as `gr_read_spec()` and
#' `gr_segment_spec()` clamp theirs -- `gr_options(workers =
#' parallel::detectCores())` is a normal thing to write, and detectCores() can
#' return NA.
#' @noRd
.gr_option_rules <- list(
  # The ranges are the ones the code that reads each option clamps to --
  # gr_client() for the three request settings, gr_lapply() for `workers` -- so
  # a value accepted here is the value used, and a warning's "using N" is true.
  safety_margin     = list(kind = "tune", lo = 0, hi = 0.5,  whole = FALSE),
  min_output_tokens = list(kind = "tune", lo = 0, hi = 1e6,  whole = TRUE),
  max_retries       = list(kind = "tune", lo = 0, hi = 10,   whole = TRUE),
  retry_pause_base  = list(kind = "tune", lo = 0, hi = 60,   whole = FALSE),
  request_timeout   = list(kind = "tune", lo = 1, hi = 3600, whole = FALSE),
  workers           = list(kind = "tune", lo = 1, hi = 32,   whole = TRUE),
  # NULL is temperature's default -- "let the model decide" -- so it falls back
  # to NULL; and Inf is out of range for it rather than meaning "no limit",
  # which is the ceilings' meaning and went on the wire as "temperature":"Inf".
  temperature       = list(kind = "tune", lo = 0, hi = 2, whole = FALSE, null_ok = TRUE),
  max_cost_usd      = list(kind = "ceiling", lo = 0, whole = FALSE),
  # lo = 0: "make no calls at all" is a meaningful instruction, and the tests
  # that assert a reader degrades rather than spending use it.
  max_calls         = list(kind = "ceiling", lo = 0, whole = TRUE)
)

#' @noRd
check_option <- function(name, value) {
  rule <- .gr_option_rules[[name]]
  if (is.null(rule)) return(value)
  what <- function(v) if (is.null(v)) "NULL" else if (length(v) != 1L)
    sprintf("a length-%d value", length(v)) else if (is.na(v)) "NA" else
    sprintf("a %s", class(v)[1])
  usable <- !is.null(value) && length(value) == 1L && is.numeric(value) && !is.na(value)

  if (identical(rule$kind, "ceiling")) {
    if (is.null(value)) return(NULL)
    if (!usable) {
      gr_abort(sprintf(paste0("gr_options(%s = ) must be a single number or NULL, not %s. A limit ",
                              "that cannot be compared is not a limit, so this is refused here ",
                              "rather than ignored later."), name, what(value)),
               class = "gr_bad_option")
    }
    if (value < rule$lo) {
      gr_abort(sprintf("gr_options(%s = ) cannot be negative; got %s.", name, format(value)),
               class = "gr_bad_option")
    }
    if (!is.finite(value)) return(Inf)
    # A whole number kept as a DOUBLE. as.integer() is what turned 1e10 into NA.
    return(if (isTRUE(rule$whole)) floor(value) else value)
  }

  # A tuning knob. A number written as text is still a number, as it is for the
  # same settings passed to gr_budget() or a spec constructor.
  if (is.null(value) && isTRUE(rule$null_ok)) return(NULL)
  if (!usable && is.character(value) && length(value) == 1L) {
    parsed <- suppressWarnings(as.numeric(value))
    if (!is.na(parsed)) { value <- parsed; usable <- TRUE }
  }
  if (!usable) {
    # Keep the CURRENT value, not the package default: with safety_margin set
    # to 0.3, a stray NA dropping it to the default 0.1 is the "a missing value
    # pushes the input budget up" defect, moved into the setter.
    cur <- gr_options(name)
    gr_warn(sprintf("gr_options(%s = ) cannot be %s; keeping the current value (%s).",
                    name, what(value), if (is.null(cur)) "NULL" else format(cur)),
            class = "gr_bad_option")
    return(cur)
  }
  out <- min(max(value, rule$lo), rule$hi)
  # `!=`, not identical(): max(4L, 1) is the double 4, and identical(4, 4L) is
  # FALSE, so every integer setting -- `workers = 4L` -- was reported as out
  # of range.
  if (out != value) {
    gr_warn(sprintf("gr_options(%s = %s) is outside [%s, %s]; using %s.", name, format(value),
                    format(rule$lo), format(rule$hi), format(out)),
            class = "gr_bad_option")
  }
  if (isTRUE(rule$whole)) floor(out) else out
}

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
#'   \item{`api_headers` (none)}{Named character vector of extra HTTP headers
#'     sent with every request, for gateways that do not authenticate with a
#'     bearer token. Inherited by every [gr_client()] that does not name its
#'     own; see that function's `headers` argument for the rules.}
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
#' @section Checked values:
#' Numeric options are checked when they are set, and the two kinds are treated
#' differently because a wrong value costs different things.
#'
#' The ceilings, `max_cost_usd` and `max_calls`, refuse anything that is not a
#' single number of zero or more, with an error of class `gr_bad_option`: a
#' limit that cannot be compared is not a limit, and ignoring it spends money.
#' `NULL` and `Inf` both mean "no limit".
#'
#' The tuning settings -- `safety_margin` \[0, 0.5\], `min_output_tokens`
#' \[0, 1e6\], `max_retries` \[0, 10\], `retry_pause_base` \[0, 60\],
#' `request_timeout` \[1, 3600\], `workers` \[1, 32\] and `temperature`
#' \[0, 2\] -- read a number written as text as that number. A value they cannot
#' read, such as `NA` or `"x"`, warns (`gr_bad_option`) and leaves the current
#' setting unchanged; a value outside the range is clamped into it, with the
#' same warning. Whole-number settings are rounded down. `temperature` also
#' takes `NULL`, its default.
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
  for (nm in names(args)) args[nm] <- list(check_option(nm, args[[nm]]))
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
