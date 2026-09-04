# core-client.R -- the single place that talks to a model API.
#
# WHY THIS FILE EXISTS
# `process_api_call()` had five separate defects that leaked into every caller:
#
#   1. It returned a bare `NULL` on transport failure and `character(0)` when
#      the API returned `content: null`. Callers wrote `if (!nzchar(x))`, which
#      is `NA` for `character(0)` -- an "argument is not interpretable as
#      logical" crash *after* the money was spent. Other callers ran `sapply()`
#      over responses; one NULL turned the result into a list, `nzchar()` then
#      deparsed it, and the literal string "NULL" was spliced into the next
#      prompt as if it were document content.
#   2. It warned on HTTP 400 and then called `httr::stop_for_status()` anyway,
#      so the documented "returns NULL on failure" contract was false and a
#      single bad chunk aborted a whole multi-mode run.
#   3. It shipped `max_tokens` equal to the model's full output limit with no
#      regard for prompt length, guaranteeing 400s on long prompts.
#   4. It carried a branch (`if (!inherits(response, "response")) status <- 200`)
#      that existed only so the test fakes would work -- production code shaped
#      by test convenience.
#   5. `gsub("\n+", " ", result)` flattened every answer, destroying lists,
#      tables and code blocks unconditionally.
#
# The replacement returns a typed `gr_result` that is *always* safe to index,
# distinguishes retryable from terminal failures, preserves formatting, and
# never itself calls `stop()` for an API-side error -- the caller decides.

#' Resolve the API key
#'
#' Looks in `key`, then the `readgpt.api_key` option, then `OPENAI_API_KEY`.
#' Never prints or logs the key.
#'
#' @param key Optional explicit key.
#' @return The key string. Raises a `gr_auth_error` if none is found.
#'
#' @section No key set:
#' A run without a key does **not** raise. Every model call fails, the answer is
#' the `"NOT_IN_DOCUMENT"` sentinel and `ans$partial` is `TRUE`. Where the error
#' text appears depends on the reader: the single-call readers (`stuff`,
#' `retrieve`) put `"No API key available."` in `ans$notes$error`, while the
#' per-chunk readers report `notes$failed_calls` instead. `print(ans$trace)`
#' shows the error for every reader. Call this function directly if you would
#' rather fail fast.
#' @seealso [gr_client()]
#' @export
#' @examples
#' # Resolution order: explicit argument, then the option, then the env var.
#' previous <- Sys.getenv("OPENAI_API_KEY", unset = NA)
#' Sys.setenv(OPENAI_API_KEY = "sk-example")
#' gr_api_key()
#' gr_api_key("sk-explicit-wins")
#'
#' # No key set is not an error until a call is made -- see the section above.
#' Sys.unsetenv("OPENAI_API_KEY")
#' tryCatch(gr_api_key(), gr_auth_error = function(e) conditionMessage(e))
#'
#' # Restore. Sys.setenv(x = NA) would leave the variable set to "NA", not unset.
#' if (!is.na(previous)) Sys.setenv(OPENAI_API_KEY = previous)
gr_api_key <- function(key = NULL) {
  k <- as_chr1(key %||% getOption("readgpt.api_key", "") %||% "")
  if (!nzchar(k)) k <- as_chr1(Sys.getenv("OPENAI_API_KEY"))
  if (!nzchar(k)) {
    gr_abort(paste0("No API key. Set OPENAI_API_KEY, or options(readgpt.api_key = '...'), ",
                    "or pass `api_key` to gr_client()."), class = "gr_auth_error")
  }
  k
}

#' Construct a model client
#'
#' The client is an object, not a global. Passing it explicitly is what lets a
#' Shiny app serve two users with two different keys in one R process -- the old
#' code called `Sys.setenv(OPENAI_API_KEY = ...)`, which is process-wide, so the
#' second user's key silently billed the first user's requests.
#'
#' @param model Default chat model id.
#' @param api `"responses"` (default) or `"chat"`.
#' @param api_key Optional key; resolved lazily at call time if omitted.
#' @param base_url API base, for proxies and compatible endpoints.
#' @param embedding_model Default embedding model id.
#' @param max_retries,retry_pause_base Retry policy for transient failures.
#' @param timeout Per-request timeout in seconds.
#' @param extra_body Named list merged into every request body.
#' @return An object of class `gr_client`: a list of the settings above.
#'   Constructing one makes no request and does not require a key -- the key is
#'   resolved at call time by [gr_api_key()].
#' @seealso [gr_call()] to use it, [gr_mock_client()] to work offline,
#'   [gr_cache_client()] to make repeat calls free, [gr_replay_client()] to
#'   re-run a recorded run, [gr_api_key()] for key resolution, [gr_options()]
#'   for the defaults, [gr_result] for what a call returns
#' @export
#' @examples
#' # A client is a value, not a global. Two of them, two keys, one R process --
#' # which is what makes a Shiny app serving two users safe.
#' a <- gr_client(model = "gpt-4o",  api_key = "sk-user-a")
#' b <- gr_client(model = "gpt-4.1", api_key = "sk-user-b", timeout = 30)
#' vapply(list(a = a, b = b), function(cl) cl$model, character(1))
#'
#' # What that model can actually take.
#' unlist(gr_model_info(a$model)[c("context_window", "max_output")])
#'
#' # No key needed to develop against the pipeline.
#' gr_call(gr_mock_client(function(m, p) "hi there"), "hello")$text
#'
#' \dontrun{
#' cl <- gr_client(model = "gpt-5.6-terra")
#' gr_call(cl, list(list(role = "user", content = "Say hi.")), max_output = 20)
#' }
gr_client <- function(model = NULL, api = NULL, api_key = NULL, base_url = NULL,
                      embedding_model = NULL, max_retries = NULL,
                      retry_pause_base = NULL, timeout = NULL, extra_body = list()) {
  structure(list(
    model            = as_chr1(model %||% gr_options("model")),
    api              = match.arg(as_chr1(api %||% gr_options("api")), c("responses", "chat")),
    api_key          = api_key,          # may be NULL -> resolved at call time
    base_url         = sub("/+$", "", as_chr1(base_url %||% gr_options("api_base"))),
    embedding_model  = as_chr1(embedding_model %||% gr_options("embedding_model")),
    max_retries      = as.integer(clamp(max_retries %||% gr_options("max_retries"), 0, 10)),
    retry_pause_base = clamp(retry_pause_base %||% gr_options("retry_pause_base"), 0, 60),
    timeout          = clamp(timeout %||% gr_options("request_timeout"), 1, 3600),
    extra_body       = extra_body
  ), class = "gr_client")
}

#' A deterministic offline client for tests, demos and dry runs
#'
#' `handler` receives `(messages, params)` and returns either a string or a
#' `gr_result`. Every call is recorded in `$calls()`, so tests can assert on the
#' exact prompts a strategy produced -- which is how you prove two reading
#' strategies are actually different.
#'
#' Three things to know about the mock. It registers two model ids
#' (`"mock-model"`, `"mock-embed"`) in the session's model registry the first
#' time it is called, so they appear in [gr_models()] afterwards. Its default
#' handler returns plain text, so readers that need JSON-schema output
#' (`rerank`, `iterative`) take their documented degraded path unless your
#' handler returns valid JSON for those prompts. And [gr_embed()] short-circuits
#' for mock clients: it always reports `embedding_source = "api"` and never
#' takes the lexical fallback, so that degradation cannot be exercised offline.
#'
#' @param handler Function of `(messages, params)` returning a string or a
#'   [gr_result].
#' @param embed_handler Function of `(texts, params)` returning a numeric matrix
#'   with one row per input.
#' @return An object of class `gr_client`, with `$calls()`, `$embeds()` and
#'   `$reset()`.
#' @seealso [gr_client()], [gr_call()], [gr_result], [readgpt_example()] for a
#'   document to run against
#' @export
#' @examples
#' cl <- gr_mock_client(function(messages, params) "mock answer")
#' gr_call(cl, list(list(role = "user", content = "hi")))$text
#'
#' # `$calls()` is how you prove two reading strategies differ: it records the
#' # exact prompts each one sent.
#' cl$reset()
#' ch <- gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 150))
#' invisible(gr_read(ch, "What was revenue?", cl, "skim"))
#' table(vapply(cl$calls(), function(x) x$label, character(1)))
gr_mock_client <- function(handler = NULL, embed_handler = NULL) {
  # Registered so the mock does not trip the unknown-model warning on every call.
  if (is.null(gr_state$models[["mock-model"]])) {
    gr_register_model("mock-model", context_window = 128000L, max_output = 16384L,
                      input_usd = 0, output_usd = 0)
    gr_register_model("mock-embed", context_window = 8191L, max_output = 0L,
                      input_usd = 0, output_usd = 0, kind = "embedding", dimensions = 64L)
  }
  log <- new.env(parent = emptyenv())
  log$calls <- list()
  log$embeds <- list()
  handler <- handler %||% function(messages, params) "mock answer"
  embed_handler <- embed_handler %||% function(texts, params) {
    # Deterministic, content-derived pseudo-embeddings. Unlike the old
    # `set.seed(nchar(text)); runif(768)`, these depend on the *characters*, so
    # two different texts of equal length do not collide, and no global RNG is
    # touched.
    t(vapply(texts, function(tx) {
      b <- utf8ToInt(enc2utf8(as_chr1(tx, " ")))
      v <- numeric(64)
      for (i in seq_along(b)) v[(b[i] %% 64L) + 1L] <- v[(b[i] %% 64L) + 1L] + 1
      n <- sqrt(sum(v^2)); if (n == 0) v else v / n
    }, numeric(64), USE.NAMES = FALSE))
  }
  structure(list(
    model = "mock-model", api = "mock", base_url = "mock://", embedding_model = "mock-embed",
    max_retries = 0L, retry_pause_base = 0, timeout = 1, extra_body = list(),
    handler = handler, embed_handler = embed_handler, .log = log,
    calls  = function() log$calls,
    embeds = function() log$embeds,
    reset  = function() { log$calls <- list(); log$embeds <- list(); invisible(NULL) }
  ), class = c("gr_mock_client", "gr_client"))
}

#' The result of one model call
#' @noRd
gr_result <- function(ok, text = "", error = NULL, status = NA_integer_,
                      usage = list(input = 0L, output = 0L), model = NA_character_,
                      finish_reason = NA_character_, retryable = FALSE, raw = NULL,
                      cached = FALSE) {
  structure(list(
    ok = isTRUE(ok),
    # ALWAYS character(1); never NULL/character(0). And always LABELLED: bytes
    # arrive correct but unmarked from plenty of sources, and an unmarked string
    # is at the mercy of the session locale the moment anything serialises,
    # compares or counts characters in it. mark_utf8() labels, it does not
    # convert, so this cannot corrupt a response the way enc2utf8() would.
    text = mark_utf8(as_chr1(text)),
    error = error,
    status = status,
    usage = usage,
    model = model,
    finish_reason = finish_reason,
    retryable = isTRUE(retryable),
    raw = raw,
    # TRUE when the response came from a cache or a recording rather than the
    # network. `usage` still reports the real token counts of the original call,
    # so the trace can say how big the prompt was; `cached` is what stops those
    # tokens from being counted as money spent this run.
    cached = isTRUE(cached)
  ), class = "gr_result")
}

#' @export
print.gr_result <- function(x, ...) {
  cat(sprintf("<gr_result> ok=%s model=%s in=%s out=%s%s%s\n",
              x$ok, as_chr1(x$model, "?"), x$usage$input %||% 0, x$usage$output %||% 0,
              if (isTRUE(x$cached)) " cached" else "",
              if (!x$ok) paste0(" error=", as_chr1(x$error)) else ""))
  if (nzchar(x$text)) cat(substr(x$text, 1, 400), if (nchar(x$text) > 400) " ..." else "", "\n")
  invisible(x)
}

#' Call a model
#'
#' @param client A `gr_client`.
#' @param messages A list of `list(role=, content=)` items, or a single string,
#'   which is wrapped as one user message. `role` may be `"system"`,
#'   `"developer"`, `"user"` or `"assistant"`.
#' @param model Overrides the client default.
#' @param max_output Maximum completion tokens. Clamped to the model's limit
#'   and to what the context window actually leaves after the prompt.
#' @param temperature Sampling temperature; dropped automatically for reasoning
#'   models that reject it.
#' @param schema Optional JSON Schema (as a list) requesting structured output.
#' @param schema_name Name for the schema.
#' @param trace Optional `gr_trace` to record this call into.
#' @param label Short label for the trace entry.
#' @param ... Extra top-level body fields.
#' @return A [gr_result]. **Never** `NULL`; `$text` is always a single string,
#'   `""` on failure. A successful HTTP call that returned no text (a refusal, a
#'   content filter) is reported as `ok = FALSE`, so an empty completion is never
#'   passed downstream as evidence. `$cached` is `TRUE` when the response came
#'   from a [gr_cache()] or a [gr_replay_client()] instead of the network.
#' @seealso [gr_client()], [gr_mock_client()], [gr_result], [gr_budget()],
#'   [gr_cache_client()], [gr_replay_client()]
#' @export
#' @examples
#' cl <- gr_mock_client(function(messages, params) "The answer is 42.")
#' res <- gr_call(cl, "What is the answer?")
#' c(ok = res$ok, text = res$text)
#'
#' # A failed call still returns a usable object.
#' bad <- gr_call(gr_mock_client(function(m, p) stop("network down")), "hi")
#' c(ok = bad$ok, text = sprintf("<%s>", bad$text), error = bad$error)
gr_call <- function(client, messages, model = NULL, max_output = NULL,
                    temperature = NULL, schema = NULL, schema_name = "result",
                    trace = NULL, label = "call", ...) {
  if (!inherits(client, "gr_client")) gr_abort("`client` must come from gr_client() or gr_mock_client().")
  messages <- normalise_messages(messages)
  model <- as_chr1(model %||% client$model)
  info <- gr_model_info(model)

  prompt_tokens <- sum(gr_count_tokens(vapply(messages, function(m) as_chr1(m$content), character(1))))
  headroom <- info$context_window - prompt_tokens - 32L
  max_output <- as.integer(clamp(max_output %||% info$max_output, 1, info$max_output))
  if (headroom < max_output) max_output <- as.integer(max(headroom, 0L))

  if (max_output <= 0L) {
    res <- gr_result(FALSE, error = sprintf(
      paste0("Prompt is %d tokens but '%s' has a %d-token context window, leaving no room for a ",
             "reply. Segment more aggressively (lower `max_tokens` on the segmenter) or use a ",
             "larger-context model."),
      prompt_tokens, model, info$context_window), status = 0L, model = model)
    trace_record(trace, label, messages, res, params = list(model = model))
    return(res)
  }

  params <- list(model = model, max_output = max_output, temperature = temperature,
                 schema_name = schema_name, prompt_tokens = prompt_tokens)

  # One dispatch, one exit, one trace entry. The mock and HTTP paths each used
  # to `return()` after recording their own trace entry, so every feature that
  # had to sit between the request and the response -- caching, replay -- would
  # have had to be written twice and kept in step by hand.
  res <- client_dispatch(client, messages, model, max_output, temperature,
                         schema, schema_name, info, params, label, list(...))
  trace_record(trace, label, messages, res, params)
  res
}

#' Where a prepared request actually goes.
#'
#' Order matters. A replay client never reaches the network at all. A cache is
#' consulted before the request is issued and written only after it succeeds --
#' a failure is a property of the moment, not of the request, and caching one
#' would make a transient blip permanent.
#' @noRd
client_dispatch <- function(client, messages, model, max_output, temperature,
                            schema, schema_name, info, params, label, extra) {
  if (inherits(client, "gr_replay_client")) {
    return(replay_lookup(client, messages, model, params))
  }

  cache <- client$.cache
  key <- NULL
  if (inherits(cache, "gr_cache")) {
    key <- cache_key(client, messages, model, max_output, temperature,
                     schema, schema_name, extra)
    hit <- cache_get(cache, key)
    if (!is.null(hit)) return(hit)
  }

  res <- if (inherits(client, "gr_mock_client")) {
    mock_dispatch(client, messages, model, params, label)
  } else {
    body <- build_request_body(client, messages, model, max_output, temperature,
                              schema, schema_name, info, extra)
    url <- paste0(client$base_url,
                  if (identical(client$api, "responses")) "/responses" else "/chat/completions")
    out <- http_call(client, url, body)
    out$model <- model
    out
  }

  if (inherits(cache, "gr_cache") && isTRUE(res$ok)) cache_put(cache, key, res)
  res
}

#' Run a mock client's handler and normalise whatever it returned.
#' @noRd
mock_dispatch <- function(client, messages, model, params, label) {
  prompt_tokens <- as_int1(params$prompt_tokens, 0L)
  out <- tryCatch(client$handler(messages, params), error = function(e) e)
  res <- if (inherits(out, "gr_result")) {
    # Re-normalise: a hand-built gr_result from a user handler can violate the
    # character(1) contract that every caller downstream relies on.
    gr_result(out$ok, text = out$text, error = out$error, status = out$status %||% NA_integer_,
              usage = out$usage %||% list(input = 0L, output = 0L),
              model = out$model %||% model, finish_reason = out$finish_reason %||% NA_character_,
              retryable = isTRUE(out$retryable), raw = out$raw, cached = isTRUE(out$cached))
  }
  else if (inherits(out, "error")) gr_result(FALSE, error = conditionMessage(out), model = model)
  else {
    # A handler that returns "" is the mock's version of the API returning
    # `content: null` -- a refusal, a content filter, a tool-call response. The
    # real client reports that as ok = FALSE, and `gr_result`'s documented
    # invariant says so. Reporting it as a successful empty answer here made
    # the mock disagree with production on the one case readers most need to
    # handle, and made the invariant untestable offline.
    txt <- as_chr1(out)
    if (!nzchar(trimws(txt))) {
      gr_result(FALSE, text = "", error = "empty completion", model = model,
                finish_reason = "empty",
                usage = list(input = prompt_tokens, output = 0L))
    } else {
      gr_result(TRUE, text = txt, model = model,
                usage = list(input = prompt_tokens, output = sum(gr_count_tokens(txt))))
    }
  }
  client$.log$calls <- c(client$.log$calls, list(list(messages = messages, params = params,
                                                      result = res, label = label)))
  res
}

#' @noRd
normalise_messages <- function(messages) {
  if (is.character(messages)) {
    txt <- paste(messages, collapse = "\n")
    if (!nzchar(trimws(txt))) gr_abort("`messages` is empty; there is nothing to send.")
    messages <- list(list(role = "user", content = txt))
  }
  # A single unwrapped message. Without this, each *element* became its own
  # message and the first one carried the literal content "user".
  if (is.list(messages) && !is.null(names(messages)) && "content" %in% names(messages)) {
    messages <- list(messages)
  }
  if (!is.list(messages) || !length(messages)) gr_abort("`messages` must be a non-empty list.")
  lapply(messages, function(m) {
    if (is.character(m)) m <- list(role = "user", content = m)
    role <- as_chr1(m$role, "user")
    if (!role %in% c("system", "user", "assistant", "developer")) {
      gr_abort(sprintf("Unsupported message role '%s'.", role))
    }
    list(role = role, content = as_chr1(m$content))
  })
}

#' @noRd
build_request_body <- function(client, messages, model, max_output, temperature,
                               schema, schema_name, info, extra) {
  temperature <- temperature %||% gr_options("temperature")
  # Reasoning models reject `temperature`; sending it is a hard 400.
  if (isTRUE(info$reasoning) || !isTRUE(info$supports_temperature)) temperature <- NULL

  if (identical(client$api, "responses")) {
    sys <- vapply(Filter(function(m) m$role %in% c("system", "developer"), messages),
                  function(m) m$content, character(1))
    rest <- Filter(function(m) !m$role %in% c("system", "developer"), messages)
    body <- list(
      model = model,
      input = lapply(rest, function(m) list(role = m$role, content = m$content)),
      max_output_tokens = max_output
    )
    if (length(sys)) body$instructions <- paste(sys, collapse = "\n\n")
    if (!is.null(temperature)) body$temperature <- temperature
    if (!is.null(schema)) {
      body$text <- list(format = list(type = "json_schema", name = schema_name,
                                      strict = TRUE, schema = schema))
    }
  } else {
    body <- list(model = model, messages = lapply(messages, function(m)
      list(role = m$role, content = m$content)), max_tokens = max_output)
    if (!is.null(temperature)) body$temperature <- temperature
    if (!is.null(schema)) {
      body$response_format <- list(type = "json_schema",
                                   json_schema = list(name = schema_name, strict = TRUE,
                                                      schema = schema))
    }
  }
  utils::modifyList(utils::modifyList(body, client$extra_body %||% list()), extra %||% list())
}

# Status codes worth retrying. 400 is NOT here: a malformed request will be
# malformed on every attempt, and retrying it four times was pure waste.
.retryable_status <- c(408L, 409L, 425L, 429L, 500L, 502L, 503L, 504L)

#' @noRd
http_call <- function(client, url, body) {
  key <- tryCatch(gr_api_key(client$api_key), error = function(e) NULL)
  if (is.null(key)) {
    return(gr_result(FALSE, error = "No API key available.", status = 0L))
  }
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    resp <- tryCatch(
      httr::POST(url,
                 httr::add_headers(Authorization = paste("Bearer", key)),
                 httr::content_type_json(),
                 httr::timeout(client$timeout),
                 body = body, encode = "json"),
      error = function(e) e
    )
    if (inherits(resp, "condition")) {
      if (attempt > client$max_retries) {
        return(gr_result(FALSE, error = paste0("Transport error: ", conditionMessage(resp)),
                         status = 0L, retryable = TRUE))
      }
      Sys.sleep(backoff_delay(client$retry_pause_base, attempt))
      next
    }
    status <- httr::status_code(resp)
    if (status >= 200 && status < 300) return(parse_response(resp, client$api))
    detail <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"),
                       error = function(e) "<unreadable body>")
    detail <- substr(as_chr1(detail), 1, 800)
    if (status %in% .retryable_status && attempt <= client$max_retries) {
      wait <- retry_after(resp) %||% backoff_delay(client$retry_pause_base, attempt)
      gr_msg(sprintf("HTTP %d; retrying in %.1fs (attempt %d/%d).",
                     status, wait, attempt, client$max_retries + 1L))
      Sys.sleep(wait)
      next
    }
    return(gr_result(FALSE, error = sprintf("HTTP %d: %s", status, detail),
                     status = status, retryable = status %in% .retryable_status))
  }
}

#' Exponential backoff with real jitter.
#'
#' The seed used to be a pure function of `attempt`, so every client and every
#' parallel worker retrying attempt N slept for the identical duration -- the
#' delay was labelled "jitter" but had none, and a rate-limited fleet re-collided
#' on every round. Mixing in the process id and the clock decorrelates them. The
#' draw still goes through a private RNG, so the caller's `set.seed()` stream is
#' untouched and any simulation running alongside is unaffected.
#' @noRd
backoff_delay <- function(base, attempt) {
  cap <- min(base * 2^(attempt - 1L), 60)
  if (cap <= 0) return(0)
  seed <- as.integer((as.numeric(Sys.getpid()) * 7919 +
                      as.numeric(Sys.time()) * 1000 + attempt * 104729) %% 2147483647)
  with_private_rng(seed, stats::runif(1, cap * 0.5, cap))
}

#' @noRd
retry_after <- function(resp) {
  h <- httr::headers(resp)
  v <- suppressWarnings(as.numeric(h[["retry-after"]] %||% NA))
  if (is.finite(v) && v >= 0) min(v, 60) else NULL
}

#' @noRd
parse_response <- function(resp, api) {
  parsed <- tryCatch(httr::content(resp, as = "parsed", type = "application/json",
                                   encoding = "UTF-8"),
                     error = function(e) NULL)
  if (is.null(parsed)) {
    return(gr_result(FALSE, error = "Could not parse API response as JSON.",
                     status = httr::status_code(resp)))
  }
  fld <- function(x, nm) if (is.list(x)) x[[nm, exact = TRUE]] else NULL
  err <- fld(parsed, "error")
  if (!is.null(err)) {
    # `error` may be an object or a bare string, depending on the endpoint.
    msg <- if (is.list(err)) as_chr1(fld(err, "message"), "API returned an error object.")
           else as_chr1(err, "API returned an error object.")
    return(gr_result(FALSE, error = msg, status = httr::status_code(resp), raw = parsed))
  }
  text <- extract_text(parsed, api)
  ug <- fld(parsed, "usage")
  int1 <- function(v) { v <- suppressWarnings(as.integer(v)[1]); if (is.na(v)) 0L else v }
  usage <- list(
    input  = int1(fld(ug, "input_tokens")  %||% fld(ug, "prompt_tokens")     %||% 0L),
    output = int1(fld(ug, "output_tokens") %||% fld(ug, "completion_tokens") %||% 0L)
  )
  ch <- fld(parsed, "choices")
  finish <- as_chr1(fld(parsed, "status") %||%
                    (if (is.list(ch) && length(ch) && is.list(ch[[1]])) fld(ch[[1]], "finish_reason")) %||%
                    NA_character_, NA_character_)

  # A refusal is not an answer. Returning ok = TRUE here let the refusal text be
  # spliced into the next prompt as if it were document evidence.
  if (is_refusal(parsed)) {
    return(gr_result(FALSE, text = "", status = httr::status_code(resp), usage = usage,
                     finish_reason = "refusal", raw = parsed,
                     error = paste0("Model refused the request: ", substr(text, 1, 200))))
  }
  # An empty completion is a *successful* call with no content -- report it as
  # a failure so callers do not splice "" into a downstream prompt as evidence.
  if (!nzchar(trimws(text))) {
    return(gr_result(FALSE, text = "", status = httr::status_code(resp), usage = usage,
                     finish_reason = finish, raw = parsed,
                     error = sprintf("Model returned no text content (finish reason: %s).",
                                     as_chr1(finish, "unknown"))))
  }
  gr_result(TRUE, text = text, status = httr::status_code(resp), usage = usage,
            finish_reason = finish, raw = parsed)
}

#' Pull assistant text out of either API shape.
#'
#' Note what this deliberately does NOT do: collapse newlines. The old code ran
#' `gsub("\n+", " ", result)` on every response, flattening lists, tables and
#' code blocks into one line.
#' @noRd
extract_text <- function(parsed, api) {
  # `[[` with exact = TRUE throughout. `$` on a list partial-matches, so
  # `parsed$output` silently returned `output_text` and then failed on it.
  # Every branch is also defensive about shape: a 200 response whose fields are
  # a different type than expected must produce "", not an uncaught error.
  if (!is.list(parsed)) return("")
  fld <- function(x, nm) if (is.list(x)) x[[nm, exact = TRUE]] else NULL

  ot <- fld(parsed, "output_text")
  if (is.list(ot)) ot <- unlist(ot, use.names = FALSE)
  if (is.character(ot) && length(ot)) return(trimws(paste(ot, collapse = "")))

  out <- fld(parsed, "output")
  if (is.list(out) && length(out)) {
    parts <- unlist(lapply(out, function(item) {
      if (!is.list(item)) return(NULL)
      if (!identical(as_chr1(fld(item, "type"), "message"), "message")) return(NULL)
      cont <- fld(item, "content")
      if (is.character(cont)) return(cont)
      if (!is.list(cont)) return(NULL)
      vapply(cont, function(cc) {
        if (is.character(cc)) return(as_chr1(cc))
        if (!is.list(cc)) return("")
        if (identical(as_chr1(fld(cc, "type")), "refusal")) return(as_chr1(fld(cc, "refusal")))
        as_chr1(fld(cc, "text"))
      }, character(1))
    }), use.names = FALSE)
    parts <- parts[!is.na(parts) & nzchar(parts)]
    if (length(parts)) return(trimws(paste(parts, collapse = "")))
  }

  ch <- fld(parsed, "choices")
  if (is.list(ch) && length(ch) && is.list(ch[[1]])) {
    msg <- fld(ch[[1]], "message")
    if (is.character(msg)) return(trimws(as_chr1(msg)))
    if (is.list(msg)) {
      ref <- fld(msg, "refusal")
      if (!is.null(ref)) return(as_chr1(ref))
      return(trimws(as_chr1(fld(msg, "content"))))
    }
  }
  ""
}

#' Was the response a model refusal rather than an answer?
#' @noRd
is_refusal <- function(parsed) {
  if (!is.list(parsed)) return(FALSE)
  fld <- function(x, nm) if (is.list(x)) x[[nm, exact = TRUE]] else NULL
  out <- fld(parsed, "output")
  if (is.list(out)) {
    for (item in out) {
      cont <- if (is.list(item)) fld(item, "content") else NULL
      if (is.list(cont)) for (cc in cont) {
        if (is.list(cc) && identical(as_chr1(fld(cc, "type")), "refusal")) return(TRUE)
      }
    }
  }
  ch <- fld(parsed, "choices")
  if (is.list(ch) && length(ch) && is.list(ch[[1]])) {
    msg <- fld(ch[[1]], "message")
    if (is.list(msg) && !is.null(fld(msg, "refusal"))) return(TRUE)
  }
  FALSE
}

#' Call a model and parse a JSON-schema response into a list
#' @noRd
gr_call_json <- function(client, messages, schema, schema_name = "result", ...) {
  res <- gr_call(client, messages, schema = schema, schema_name = schema_name, ...)
  if (!res$ok) return(list(ok = FALSE, value = NULL, result = res))
  val <- tryCatch(jsonlite::fromJSON(res$text, simplifyVector = TRUE),
                  error = function(e) NULL)
  if (is.null(val)) {
    # Some endpoints wrap JSON in a code fence even under strict mode.
    stripped <- gsub("^\\s*```(?:json)?\\s*|\\s*```\\s*$", "", res$text, perl = TRUE)
    val <- tryCatch(jsonlite::fromJSON(stripped, simplifyVector = TRUE),
                    error = function(e) NULL)
  }
  # A bare 7, "text", [1,2,3] or true is valid JSON but not an object. Callers
  # index into `value` with `$`, which errors on an atomic vector, so anything
  # that is not a named list is treated as a parse failure and routed to the
  # reader's documented degraded path instead.
  ok <- !is.null(val) && (is.list(val) || (is.data.frame(val))) && length(names(val)) > 0
  list(ok = ok, value = if (ok) val else NULL, result = res)
}
