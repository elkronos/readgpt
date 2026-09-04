# core-cache.R -- a content-addressed cache for model responses.
#
# WHY THIS FILE EXISTS
# Everything in this package is deterministic except the model call. Ingestion,
# cleaning, segmentation, ranking and merging are pure functions of their input,
# and `gr_chunk_stats()` already lets you compare chunkings for nothing. The
# model call is the only step that costs money, the only step that can fail
# halfway through a long run, and -- above a temperature of zero -- the only
# step that does not return the same thing twice.
#
# That combination is what made iterating expensive. Changing a merge prompt,
# fixing a typo in a question, or resuming after call 340 of 900 died all
# re-issued every earlier call at full price, and at temperature > 0 the earlier
# answers came back *different*, so the thing you were debugging moved while you
# were looking at it.
#
# A cache keyed on the exact request fixes both. A re-run is free, and it is
# also *identical*, which is what makes a result you publish something another
# person can check.
#
# WHAT IS AND IS NOT IN THE KEY
# In: the messages, the model, the resolved `max_output`, the temperature, the
# JSON schema and its name, the API shape and base URL, and any extra body
# fields. Out: the API key, the timeout, the retry policy, the trace and the
# label -- none of which can change what the model returns.
#
# FAILURES ARE NEVER CACHED. A transport error, a rate limit or a refusal is a
# property of the moment, not of the request. Caching one would turn a blip into
# a permanent wrong answer that no amount of re-running could clear.

#' A response cache
#'
#' Wrap a client with [gr_cache_client()] and every successful model call is
#' written to disk, keyed on the exact request. Re-issuing the same request
#' returns the stored response without touching the network: free, instant, and
#' byte-identical even at a temperature above zero.
#'
#' The default location is under [tempdir()], so a cache costs nothing and
#' disappears with the session. That is the right default for a package -- it
#' writes nothing to your filesystem you did not ask for -- but it is not what
#' you want for a long experiment. Pass a real directory, or
#' `tools::R_user_dir("readgpt", "cache")`, to keep entries across sessions and
#' make a run resumable after a crash.
#'
#' @param dir Directory for cache entries. Defaults to the `cache_dir` option.
#'   Created on first write, not here.
#' @param read,write Whether to read existing entries and write new ones. Set
#'   `write = FALSE` to run against a frozen cache; set `read = FALSE` to
#'   refresh entries that are already stored.
#' @return An object of class `gr_cache`. It holds a directory and a counter
#'   environment, so copies of it share the same statistics.
#'
#' @section What is stored:
#' One small RDS file per entry, sharded into subdirectories by the first two
#' characters of the key. Each file holds the [gr_result] -- text, token usage,
#' model, finish reason -- with the raw parsed API response dropped, because
#' nothing downstream reads it and keeping it multiplied the cache size for no
#' benefit. The prompt is **not** stored, only its hash, so a cache directory
#' does not accumulate copies of your documents. The response itself is stored
#' in full, and a model response can of course quote the document it read.
#'
#' @section Caching a stochastic call:
#' At a temperature above zero a cache hit replays one sample rather than
#' drawing a new one. That is the point -- it is what makes a run reproducible
#' -- but it means a cached sweep does not explore. Use a fresh cache directory,
#' or `read = FALSE`, when you want new draws.
#'
#' @seealso [gr_cache_client()] to attach one, [gr_cache_stats()],
#'   [gr_cache_clear()], [gr_replay_client()] for reproducing a recorded run
#' @export
#' @examples
#' cache <- gr_cache(dir = file.path(tempdir(), "readgpt-example-cache"))
#' cache
#'
#' # Nothing is written until a call is cached.
#' gr_cache_stats(cache)[c("entries", "hits", "misses")]
gr_cache <- function(dir = NULL, read = TRUE, write = TRUE) {
  st <- new.env(parent = emptyenv())
  st$hits <- 0L
  st$misses <- 0L
  st$writes <- 0L
  structure(list(
    dir   = as_chr1(dir %||% gr_options("cache_dir") %||% default_cache_dir()),
    read  = isTRUE(read),
    write = isTRUE(write),
    .stats = st
  ), class = "gr_cache")
}

#' @export
print.gr_cache <- function(x, ...) {
  s <- gr_cache_stats(x)
  cat(sprintf("<gr_cache> %s\n", x$dir))
  cat(sprintf("  %d entr%s, %s on disk; %d hit(s), %d miss(es), %d write(s)%s\n",
              s$entries, if (identical(s$entries, 1L)) "y" else "ies",
              format_bytes(s$bytes), s$hits, s$misses, s$writes,
              if (!x$read || !x$write)
                sprintf(" [%s]", paste(c(if (!x$read) "read off", if (!x$write) "write off"),
                                       collapse = ", "))
              else ""))
  invisible(x)
}

#' Attach a cache to a client
#'
#' Returns the same client with the cache attached, so a cached mock client is
#' still a mock client and `$calls()` still records only the calls that were
#' actually issued -- which is exactly how you check that the cache is working.
#'
#' @param client A [gr_client()] or [gr_mock_client()].
#' @param cache A [gr_cache()]. Pass `NULL` to detach.
#' @return The client, with the cache attached.
#' @seealso [gr_cache()], [gr_cache_stats()]
#' @export
#' @examples
#' calls <- 0
#' cl <- gr_mock_client(function(m, p) { calls <<- calls + 1; "42" })
#' cl <- gr_cache_client(cl, gr_cache(file.path(tempdir(), "readgpt-doc-cache")))
#'
#' gr_call(cl, "What is the answer?")$text
#' gr_call(cl, "What is the answer?")$text   # served from the cache
#' calls                                     # the handler ran once
#'
#' # A cache hit is marked, so a trace can tell spending from replay.
#' gr_call(cl, "What is the answer?")$cached
gr_cache_client <- function(client, cache = gr_cache()) {
  if (!inherits(client, "gr_client")) {
    gr_abort("`client` must come from gr_client() or gr_mock_client().")
  }
  if (!is.null(cache) && !inherits(cache, "gr_cache")) {
    gr_abort("`cache` must come from gr_cache(), or be NULL to detach.")
  }
  client$.cache <- cache
  client
}

#' Cache statistics
#'
#' @param cache A [gr_cache()].
#' @return A one-row data frame: `dir`, `entries`, `bytes`, `hits`, `misses`,
#'   `writes`. `entries` and `bytes` are read from disk, so they include entries
#'   written by earlier sessions; the three counters cover this session only.
#' @seealso [gr_cache()], [gr_cache_clear()]
#' @export
#' @examples
#' cache <- gr_cache(file.path(tempdir(), "readgpt-stats-cache"))
#' cl <- gr_cache_client(gr_mock_client(function(m, p) "hi"), cache)
#' invisible(gr_call(cl, "one"))
#' invisible(gr_call(cl, "one"))
#' gr_cache_stats(cache)[c("entries", "hits", "misses", "writes")]
gr_cache_stats <- function(cache) {
  stopifnot(inherits(cache, "gr_cache"))
  files <- cache_files(cache$dir)
  data.frame(
    dir = cache$dir,
    entries = length(files),
    bytes = if (length(files)) sum(file.size(files), na.rm = TRUE) else 0,
    hits = cache$.stats$hits,
    misses = cache$.stats$misses,
    writes = cache$.stats$writes,
    stringsAsFactors = FALSE
  )
}

#' Delete every entry in a cache
#'
#' Removes the stored responses and resets the session counters. The directory
#' itself is left in place.
#'
#' @param cache A [gr_cache()].
#' @return The number of entries removed, invisibly.
#' @seealso [gr_cache()], [gr_cache_stats()]
#' @export
#' @examples
#' cache <- gr_cache(file.path(tempdir(), "readgpt-clear-cache"))
#' cl <- gr_cache_client(gr_mock_client(function(m, p) "hi"), cache)
#' invisible(gr_call(cl, "something"))
#' gr_cache_clear(cache)
#' gr_cache_stats(cache)$entries
gr_cache_clear <- function(cache) {
  stopifnot(inherits(cache, "gr_cache"))
  files <- cache_files(cache$dir)
  if (length(files)) unlink(files)
  cache$.stats$hits <- 0L
  cache$.stats$misses <- 0L
  cache$.stats$writes <- 0L
  invisible(length(files))
}

# --- internals -------------------------------------------------------------

#' Every entry file under a cache directory.
#' A per-session cache directory under `tempdir()`.
#'
#' Resolved on every call, never stored in `gr_defaults`: a `tempdir()` written
#' into that list would be evaluated when the package is installed and then
#' baked into the namespace image, pointing every later session at a directory
#' that no longer exists.
#' @noRd
default_cache_dir <- function() file.path(tempdir(), "readgpt-cache")

#' @noRd
cache_files <- function(dir) {
  if (!nzchar(dir) || !dir.exists(dir)) return(character(0))
  list.files(dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
}

#' The key for one request.
#'
#' Everything that can change the response goes in; nothing else does. The
#' leading version string is what lets a future change to the key scheme
#' invalidate old entries instead of silently mixing two schemes in one
#' directory.
#' @noRd
cache_key <- function(client, messages, model, max_output, temperature,
                      schema, schema_name, extra) {
  parts <- c(
    "readgpt-cache-v1",
    as_chr1(client$api, "?"),
    as_chr1(client$base_url, "?"),
    # Present only on clients whose behaviour is an R closure rather than a URL.
    # Without it two different mock or backend clients shared entries, because
    # api, base_url and model are identical constants for all of them.
    as_chr1(client$.client_id, "<url-addressed>"),
    as_chr1(model, "?"),
    format(as.integer(max_output)),
    if (is.null(temperature)) "<null>" else format(temperature, digits = 15L),
    as_chr1(schema_name, "result"),
    if (is.null(schema)) "<no-schema>" else gr_hash(schema),
    gr_hash(client$extra_body %||% list()),
    gr_hash(extra %||% list()),
    key_text(unlist(lapply(messages, function(m) c(as_chr1(m$role), as_chr1(m$content))),
                    use.names = FALSE))
  )
  gr_hash(parts)
}

#' Normalise text before it is hashed into a key.
#'
#' `digest()` hashes the serialised R object, and a string's declared encoding
#' is part of that object. So the SAME BYTES marked "UTF-8" and marked "unknown"
#' hash differently -- which meant a prompt that had been round-tripped through
#' JSON (as a saved trace is) never matched the same prompt held in memory, and
#' any non-ASCII prompt missed on replay while every ASCII one hit. Both key
#' schemes normalise through the package's own decoder first, so a key depends
#' on the characters and not on how they happen to be labelled.
#'
#' The one thing this merges is line endings: `to_utf8()` normalises CRLF, so
#' two prompts differing only in that share a key. Ingestion has already done
#' the same to every chunk, so it can only arise for a hand-built message, and
#' the model would not have distinguished them either.
#' @noRd
key_text <- function(x) {
  if (!length(x)) return(character(0))
  to_utf8(x)
}

#' @noRd
cache_path <- function(dir, key) {
  file.path(dir, substr(key, 1L, 2L), paste0(key, ".rds"))
}

#' Look one request up.
#'
#' A cache is an optimisation, never a source of errors: an unreadable,
#' truncated or half-written file is a miss, not a failure. Returning the error
#' would mean a corrupted byte on disk could stop a run that has nothing wrong
#' with it.
#' @noRd
cache_get <- function(cache, key) {
  if (!isTRUE(cache$read)) return(NULL)
  path <- cache_path(cache$dir, key)
  if (!file.exists(path)) {
    cache$.stats$misses <- cache$.stats$misses + 1L
    return(NULL)
  }
  entry <- tryCatch(readRDS(path), error = function(e) NULL, warning = function(w) NULL)
  ok <- is.list(entry) && identical(entry$format, 1L) && inherits(entry$result, "gr_result")
  if (!ok) {
    cache$.stats$misses <- cache$.stats$misses + 1L
    return(NULL)
  }
  cache$.stats$hits <- cache$.stats$hits + 1L
  res <- entry$result
  res$cached <- TRUE
  res
}

#' Store one response.
#'
#' Written to a temporary name in the same directory and then renamed, so a
#' crash or an interrupt mid-write cannot leave a half file that a later run
#' would have to distinguish from a real one. A failed write is silent: the run
#' continues uncached rather than dying over a full disk or a read-only mount.
#' @noRd
cache_put <- function(cache, key, res) {
  if (!isTRUE(cache$write) || is.null(key)) return(invisible(FALSE))
  path <- cache_path(cache$dir, key)
  dir <- dirname(path)
  if (!dir.exists(dir) && !dir.create(dir, recursive = TRUE, showWarnings = FALSE)) {
    return(invisible(FALSE))
  }
  res$raw <- NULL          # nothing downstream reads it; keeping it bloats the cache
  res$cached <- FALSE      # what is stored is an original, not a replay
  entry <- list(format = 1L, key = key, created = Sys.time(), result = res)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  ok <- tryCatch({
    saveRDS(entry, tmp, compress = TRUE)
    file.rename(tmp, path)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok)) {
    if (file.exists(tmp)) unlink(tmp)
    return(invisible(FALSE))
  }
  cache$.stats$writes <- cache$.stats$writes + 1L
  invisible(TRUE)
}

#' @noRd
format_bytes <- function(n) {
  n <- as.numeric(n)[1]
  if (!is.finite(n)) return("?")
  units <- c("B", "KB", "MB", "GB")
  i <- 1L
  while (n >= 1024 && i < length(units)) { n <- n / 1024; i <- i + 1L }
  sprintf(if (i == 1L) "%.0f %s" else "%.1f %s", n, units[i])
}
