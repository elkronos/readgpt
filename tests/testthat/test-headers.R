# test-headers.R -- extra HTTP headers on a client.
#
# The package could authenticate exactly one way: `Authorization: Bearer` with
# whatever gr_api_key() resolved. That covers OpenAI and every proxy that
# imitates it, and nothing else -- not Azure OpenAI's `api-key`, not an API
# Management subscription key, not a gateway that requires a cost-centre id. A
# private company's endpoint is usually one of those, so `base_url` on its own
# was not enough to reach it.
#
# Three things have to hold for `headers` to be safe rather than merely
# possible, and they are what this file pins: what actually reaches the wire,
# what a header is allowed to contain, and what is never printed.

test_that("normalise_headers() takes a vector or a list, and trims", {
  nh <- readgpt:::normalise_headers

  h <- nh(c("api-key" = "abc", "X-Team" = "ml"))
  expect_identical(h, c("api-key" = "abc", "X-Team" = "ml"))

  # A list is what you get from a config file read with jsonlite or yaml, so it
  # has to work without the caller reshaping it first.
  expect_identical(nh(list("api-key" = "abc")), c("api-key" = "abc"))

  # A trailing newline or space is what a token pasted out of a config file or
  # read with readLines() carries, and it is a 401 that looks like a wrong key.
  expect_identical(nh(c("api-key" = "  abc  ")), c("api-key" = "abc"))

  expect_identical(nh(NULL), stats::setNames(character(0), character(0)))
  expect_identical(nh(character(0)), stats::setNames(character(0), character(0)))
})

test_that("normalise_headers() keeps NA, which is the suppression marker", {
  h <- readgpt:::normalise_headers(c("api-key" = "abc", Authorization = NA))
  expect_true(is.na(h[["Authorization"]]))
  expect_identical(h[["api-key"]], "abc")
})

test_that("normalise_headers() refuses what is not a header", {
  nh <- readgpt:::normalise_headers

  expect_error(nh(c("abc")), class = "gr_header_error")
  expect_error(nh(c("api-key" = "abc", "def")), class = "gr_header_error")
  expect_error(nh(c("api key" = "abc")), class = "gr_header_error")   # space
  expect_error(nh(c("api:key" = "abc")), class = "gr_header_error")   # colon
  expect_error(nh(list("api-key" = c("a", "b"))), class = "gr_header_error")
  expect_error(nh(42), class = "gr_header_error")
  expect_error(nh(mean), class = "gr_header_error")

  # `c(Authorization = NA)` is a LOGICAL vector, and it is the exact spelling of
  # "suppress the bearer and send nothing else". Requiring character here
  # rejected the documented use.
  expect_true(is.na(nh(c(Authorization = NA))[["Authorization"]]))
  expect_identical(nh(c("X-Retries" = 3)), c("X-Retries" = "3"))

  # Every character RFC 7230 allows in a field name must survive, or the guard
  # is rejecting real headers to catch imaginary ones.
  odd <- "X-a1!#$%&'*+.^_|~`-"
  expect_identical(names(nh(stats::setNames("v", odd))), odd)
})

test_that("an empty header value is refused, because curl would drop it", {
  # `c("api-key" = Sys.getenv("GATEWAY_KEY"))` with that variable unset is the
  # likeliest way to get here, and curl sends no header at all for an empty
  # value -- so the request leaves without the credential and returns a 401
  # that looks like a wrong key rather than a missing one.
  err <- tryCatch(readgpt:::normalise_headers(c("api-key" = Sys.getenv("NOT_SET_ANYWHERE"))),
                  gr_header_error = function(e) e)
  expect_s3_class(err, "gr_header_error")
  expect_match(conditionMessage(err), "Sys.getenv")

  # Whitespace only is the same thing after trimming.
  expect_error(readgpt:::normalise_headers(c("api-key" = "   ")),
               class = "gr_header_error")

  # NA is how you suppress a header on purpose, and stays legal.
  expect_silent(readgpt:::normalise_headers(c("api-key" = NA)))
})

test_that("a control character in a header value is refused, and not echoed", {
  # CR/LF inside a header value is request splitting: the value ends the header
  # and starts one the caller never wrote. These values come from environment
  # variables and config files, which is exactly where a stray line ending
  # comes from.
  err <- tryCatch(readgpt:::normalise_headers(c("api-key" = "secret\r\nX-Evil: 1")),
                  gr_header_error = function(e) e)
  expect_s3_class(err, "gr_header_error")
  expect_match(conditionMessage(err), "api-key")
  # The name identifies the header; the value is a credential, and an error
  # message is the one place a credential is guaranteed to be printed.
  expect_false(grepl("secret", conditionMessage(err), fixed = TRUE))

  expect_error(readgpt:::normalise_headers(c("api-key" = "a\tb")),
               class = "gr_header_error")
})

test_that("duplicate header names collapse case-insensitively, last wins", {
  # HTTP field names are case-insensitive but curl is not: given both
  # `Authorization` and `authorization` it sends two headers, and which one the
  # gateway honours is its business rather than ours.
  h <- readgpt:::normalise_headers(c("X-Key" = "first", "x-key" = "second"))
  expect_length(h, 1L)
  expect_identical(unname(h), "second")
  expect_identical(names(h), "x-key")
})

test_that("request_headers() sends a bearer when only a key is set", {
  cl <- gr_client(api_key = "sk-test", base_url = "https://x.invalid")
  h <- readgpt:::request_headers(cl)
  expect_identical(h, c(Authorization = "Bearer sk-test"))
})

test_that("request_headers() keeps the bearer alongside extra headers", {
  cl <- gr_client(api_key = "sk-test", base_url = "https://x.invalid",
                  headers = c("X-Correlation-Id" = "run-7", "X-Cost-Centre" = "1234"))
  h <- readgpt:::request_headers(cl)
  expect_identical(h[["Authorization"]], "Bearer sk-test")
  expect_identical(h[["X-Correlation-Id"]], "run-7")
  expect_identical(h[["X-Cost-Centre"]], "1234")
  expect_length(h, 3L)
})

test_that("a named Authorization replaces the automatic one, whatever its case", {
  cl <- gr_client(api_key = "sk-test", base_url = "https://x.invalid",
                  headers = c(authorization = "Token other"))
  h <- readgpt:::request_headers(cl)
  # Exactly one authorization header, and it is the caller's. Two would leave
  # the gateway to pick, and it would pick the one being replaced.
  expect_length(h, 1L)
  expect_identical(unname(h), "Token other")
  expect_identical(tolower(names(h)), "authorization")
})

test_that("headers make the API key optional", {
  withr::local_envvar(OPENAI_API_KEY = NA)
  withr::local_options(readgpt.api_key = NULL)

  # A gateway authenticated by `api-key` needs no bearer, and nothing here can
  # tell which of a stranger's headers is the credential.
  cl <- gr_client(base_url = "https://gw.invalid", headers = c("api-key" = "abc"))
  h <- readgpt:::request_headers(cl)
  expect_identical(h, c("api-key" = "abc"))
  expect_false("Authorization" %in% names(h))

  # No key and no headers is still the fast local failure it always was.
  bare <- gr_client(base_url = "https://gw.invalid")
  expect_null(readgpt:::request_headers(bare))
})

test_that("NA suppresses the bearer even when a key would resolve", {
  # The case this exists for: OPENAI_API_KEY set for one client in the session,
  # and a second client pointed at a company gateway that must not receive it.
  withr::local_envvar(OPENAI_API_KEY = "sk-personal")
  cl <- gr_client(base_url = "https://gw.invalid",
                  headers = c("api-key" = "corp", Authorization = NA))
  h <- readgpt:::request_headers(cl)
  expect_identical(h, c("api-key" = "corp"))
  expect_false(any(grepl("sk-personal", h, fixed = TRUE)))
})

test_that("gr_client() validates headers at construction, not at call time", {
  expect_error(gr_client(headers = c("bad name" = "v")), class = "gr_header_error")
  expect_error(gr_client(headers = c("api-key" = "a\nb")), class = "gr_header_error")
})

test_that("a malformed set inherited from the option blames the option", {
  local_registries()
  gr_options(api_headers = c("bad name" = "v"))
  err <- tryCatch(gr_client(), gr_header_error = function(e) e)
  expect_s3_class(err, "gr_header_error")
  # Naming `headers` here would send the reader to a line they did not write.
  expect_match(conditionMessage(err), "api_headers option")
})

test_that("gr_client() inherits the api_headers option, and can opt out", {
  local_registries()
  gr_options(api_headers = c("X-Team" = "ml"))

  expect_identical(gr_client()$headers, c("X-Team" = "ml"))
  expect_identical(gr_client(headers = c("api-key" = "k"))$headers, c("api-key" = "k"))
  # An explicit empty set is a decision, not a missing argument.
  expect_length(gr_client(headers = character(0))$headers, 0L)
})

# ---------------------------------------------------------------------------
# What reaches the wire.
# ---------------------------------------------------------------------------

# Captures the headers of every config httr::POST was handed, without a network.
capture_post_headers <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$headers <- NULL
  seen$url <- NULL
  testthat::local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      seen$url <- url
      seen$headers <- unlist(lapply(list(...), function(cfg) cfg$headers))
      # A returned condition is the transport-failure path, so the caller stops
      # here having already told us everything we asked.
      simpleError("no network in tests")
    },
    .package = "httr", .env = env)
  seen
}

test_that("http_call() puts the client's headers on the request", {
  cl <- gr_client(api_key = "sk-test", base_url = "https://gw.invalid", api = "chat",
                  max_retries = 0L,
                  headers = c("api-key" = "corp", "X-Correlation-Id" = "run-7"))
  seen <- capture_post_headers()

  res <- readgpt:::http_call(cl, "https://gw.invalid/chat/completions", list())
  expect_false(res$ok)                       # no network; that is not the point

  expect_identical(seen$headers[["api-key"]], "corp")
  expect_identical(seen$headers[["X-Correlation-Id"]], "run-7")
  expect_identical(seen$headers[["Authorization"]], "Bearer sk-test")
  expect_identical(seen$headers[["Content-Type"]], "application/json")
})

test_that("http_call() reaches the network on headers alone, with no key", {
  withr::local_envvar(OPENAI_API_KEY = NA)
  withr::local_options(readgpt.api_key = NULL)
  cl <- gr_client(base_url = "https://gw.invalid", api = "chat", max_retries = 0L,
                  headers = c("api-key" = "corp"))
  seen <- capture_post_headers()

  res <- readgpt:::http_call(cl, "https://gw.invalid/chat/completions", list())
  # It got as far as the request. Before `headers`, this returned "No API key
  # available." without one.
  expect_identical(seen$url, "https://gw.invalid/chat/completions")
  expect_identical(seen$headers[["api-key"]], "corp")
  expect_false("Authorization" %in% names(seen$headers))
  expect_match(res$error, "no network in tests")
})

test_that("http_call() still fails locally when there is no auth at all", {
  withr::local_envvar(OPENAI_API_KEY = NA)
  withr::local_options(readgpt.api_key = NULL)
  cl <- gr_client(base_url = "https://gw.invalid", max_retries = 0L)
  seen <- capture_post_headers()

  res <- readgpt:::http_call(cl, "https://gw.invalid/chat/completions", list())
  expect_false(res$ok)
  expect_identical(res$error, "No API key available.")
  expect_null(seen$url)                      # not one request spent
})

test_that("the embeddings endpoint uses the same headers as the chat endpoint", {
  # A gateway that needs `api-key` for chat needs it for embeddings too. These
  # were two copies of the same three lines, which is how one of them ends up a
  # release behind the other.
  withr::local_envvar(OPENAI_API_KEY = NA)
  withr::local_options(readgpt.api_key = NULL)
  cl <- gr_client(base_url = "https://gw.invalid", headers = c("api-key" = "corp"))
  seen <- capture_post_headers()

  expect_error(
    readgpt:::embed_api(c("one", "two"),
                        list(client = cl, model = "text-embedding-3-small",
                             cache = FALSE, embedder = "api", batch_size = 8L)),
    class = "gr_embed_error")

  expect_identical(seen$url, "https://gw.invalid/embeddings")
  expect_identical(seen$headers[["api-key"]], "corp")
})

# ---------------------------------------------------------------------------
# What is never printed, and what the cache ignores.
# ---------------------------------------------------------------------------

test_that("printing a client shows header names and no header values", {
  # There was no print method at all, so a client echoed at the console printed
  # as a plain list -- api_key included, in full. Adding headers would have put
  # a second credential in the same place.
  cl <- gr_client(api_key = "sk-secret-value", base_url = "https://gw.invalid",
                  headers = c("api-key" = "corp-secret-value", "X-Team" = "ml"))
  out <- paste(utils::capture.output(print(cl)), collapse = "\n")

  expect_false(grepl("sk-secret-value", out, fixed = TRUE))
  expect_false(grepl("corp-secret-value", out, fixed = TRUE))
  expect_match(out, "api-key")
  expect_match(out, "X-Team")
  expect_match(out, "https://gw.invalid", fixed = TRUE)
})

test_that("headers are not part of the cache key", {
  # Same reason `api_key` is not: a rotating bearer or a per-request correlation
  # id would make every lookup a miss. Documented on gr_client(), pinned here.
  a <- gr_client(api_key = "sk-test", base_url = "https://gw.invalid",
                 headers = c("X-Correlation-Id" = "run-1"))
  b <- gr_client(api_key = "sk-test", base_url = "https://gw.invalid",
                 headers = c("X-Correlation-Id" = "run-2"))
  msgs <- list(list(role = "user", content = "hello"))
  args <- list(msgs, "gpt-4o", 64L, NULL, NULL, "result", list())

  expect_identical(do.call(readgpt:::cache_key, c(list(a), args)),
                   do.call(readgpt:::cache_key, c(list(b), args)))
})
