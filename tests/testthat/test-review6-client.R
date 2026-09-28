# test-review6-client.R -- the medium and low findings of the adversarial
# review in the model client, the model registry, the response cache and the
# tokenizer: core-client.R, core-models.R, core-cache.R, core-tokenize.R.

# A parsed httr response, for exercising parse_response() and http_call()
# without a server.
r6_resp <- function(json, status = 200L) structure(
  list(content = charToRaw(json), status_code = as.integer(status),
       headers = list(`content-type` = "application/json; charset=utf-8"),
       all_headers = list(), url = "mock://"), class = "response")

# The bytes jsonlite makes of a lone low-surrogate escape.
r6_bad_bytes <- function(before = "Revenue was 45 million ", after = "") {
  x <- paste0(before, rawToChar(as.raw(c(0xed, 0xb0, 0x80))), after)
  Encoding(x) <- "UTF-8"
  x
}

# ---------------------------------------------------------------------------
# cache-trace-07: a schema reply that does not parse is never cached
# ---------------------------------------------------------------------------

test_that("a structured reply cut off mid-JSON is not cached, so re-running recovers", {
  # The cache stored any ok = TRUE reply. For a schema call the JSON is parsed
  # afterwards, so a one-off truncated reply was replayed on every re-run: the
  # rerank chunks it hit stayed unscored until the cache was cleared.
  n <- 0L
  cl <- gr_mock_client(function(messages, params) {
    n <<- n + 1L
    if (n == 1L) '{"score": 9, "reason": "states rev' else '{"score": 9, "reason": "ok"}'
  })
  cache <- gr_cache(withr::local_tempdir())
  cl <- gr_cache_client(cl, cache)
  schema <- list(type = "object", properties = list(score = list(type = "number")))
  ok <- vapply(1:3, function(i) readgpt:::gr_call_json(cl, "Rate this.", schema = schema)$ok,
               logical(1))
  expect_identical(ok, c(FALSE, TRUE, TRUE))
  expect_identical(n, 2L)                       # the third came from the cache
  expect_identical(gr_cache_stats(cache)$writes, 1L)
})

test_that("an unparseable structured reply already in a cache is asked again", {
  # A cache written before the check holds the broken reply. It is not
  # replayed; the fresh reply replaces it.
  replies <- c('{"decision": "incl', '{"decision": "include"}')
  n <- 0L
  cl <- gr_mock_client(function(messages, params) { n <<- n + 1L; replies[min(n, 2L)] })
  cache <- gr_cache(withr::local_tempdir())
  cl <- gr_cache_client(cl, cache)
  schema <- list(type = "object", properties = list(decision = list(type = "string")))
  # gr_call() has no test of the reply, so it stores the broken one, as the
  # cache did for everyone before.
  first <- gr_call(cl, "Screen this.", schema = schema)
  expect_true(first$ok)
  out <- readgpt:::gr_call_json(cl, "Screen this.", schema = schema)
  expect_true(out$ok)
  expect_identical(out$value$decision, "include")
  expect_identical(n, 2L)
  again <- readgpt:::gr_call_json(cl, "Screen this.", schema = schema)
  expect_true(again$result$cached)              # the good reply is what is stored now
  expect_identical(n, 2L)
})

# ---------------------------------------------------------------------------
# client-08: a key with a control character is refused, a stray line ending
# is dropped
# ---------------------------------------------------------------------------

test_that("an API key carrying CR/LF is refused rather than injecting a header", {
  withr::local_envvar(OPENAI_API_KEY = NA)
  withr::local_options(readgpt.api_key = NULL)
  cl <- gr_client(api_key = "sk-local-test\r\nX-Injected: yes", api = "chat",
                  base_url = "https://x.invalid", max_retries = 0L)
  err <- expect_error(readgpt:::request_headers(cl), class = "gr_auth_error")
  expect_match(conditionMessage(err), "control character")
  expect_false(grepl("sk-local-test", conditionMessage(err), fixed = TRUE))
  # Nothing is sent: the run stops before the request.
  sent <- 0L
  testthat::local_mocked_bindings(
    POST = function(...) { sent <<- sent + 1L; simpleError("no network") }, .package = "httr")
  expect_error(gr_call(cl, "hi"), class = "gr_auth_error")
  expect_identical(sent, 0L)
  expect_error(readgpt:::stop_if_no_credentials(cl), class = "gr_auth_error")
  expect_output(print(cl), "key refused")
  # Through the environment variable too.
  withr::local_envvar(OPENAI_API_KEY = "sk-a\nsk-b")
  expect_error(gr_api_key(), class = "gr_auth_error")
})

test_that("the trailing newline a key file leaves is dropped, not sent", {
  withr::local_envvar(OPENAI_API_KEY = NA)
  withr::local_options(readgpt.api_key = "sk-from-file\n")
  expect_identical(gr_api_key(), "sk-from-file")
  h <- readgpt:::request_headers(gr_client())
  expect_identical(h[["Authorization"]], "Bearer sk-from-file")
  expect_identical(gr_api_key("  sk-explicit\r\n"), "sk-explicit")
  # Whitespace alone is no key.
  withr::local_options(readgpt.api_key = " \n")
  expect_error(gr_api_key(), class = "gr_auth_error")
})

# ---------------------------------------------------------------------------
# client-10: a small cap on a reasoning model leaves room for its reasoning
# ---------------------------------------------------------------------------

test_that("a short call to a reasoning model is sent with room for its reasoning", {
  local_registries()
  seen <- NULL
  testthat::local_mocked_bindings(
    http_call = function(client, url, body) { seen <<- body; readgpt:::gr_result(TRUE, "ok") },
    .package = "readgpt")
  cl <- gr_client(api_key = "sk-not-real", base_url = "https://x.invalid")
  tr <- gr_trace()
  invisible(gr_call(cl, "Context for this chunk?", model = "gpt-5.6-terra", max_output = 90L,
                    trace = tr))
  expect_identical(seen$max_output_tokens, 2048L)
  # What the caller asked for is what the trace (and the cache key) records.
  expect_identical(tr$steps[[1]]$params$max_output, 90L)
  chat <- gr_client(api = "chat", api_key = "sk-not-real", base_url = "https://x.invalid")
  invisible(gr_call(chat, "Score?", model = "gpt-5-mini", max_output = 200L))
  expect_identical(seen$max_completion_tokens, 2048L)
  # A cap already that large, and every non-reasoning model, are sent as asked.
  invisible(gr_call(cl, "Long answer", model = "gpt-5.6-terra", max_output = 5000L))
  expect_identical(seen$max_output_tokens, 5000L)
  invisible(gr_call(cl, "Short", model = "gpt-4o", max_output = 90L))
  expect_identical(seen$max_output_tokens, 90L)
  # Never beyond the model's own limit.
  gr_register_model("small-reasoner", context_window = 8000L, max_output = 1000L,
                    reasoning = TRUE, supports_temperature = FALSE)
  invisible(gr_call(cl, "Short", model = "small-reasoner", max_output = 90L))
  expect_identical(seen$max_output_tokens, 1000L)
})

# ---------------------------------------------------------------------------
# client-07: chat content given as typed parts
# ---------------------------------------------------------------------------

test_that("chat content sent as an array of parts keeps only the text parts", {
  parse <- readgpt:::parse_response
  body <- paste0('{"choices":[{"message":{"content":[',
                 '{"type":"thinking","thinking":[{"type":"text","text":"let me think"}]},',
                 '{"type":"text","text":"Revenue was 45.2 million."}]},"finish_reason":"stop"}]}')
  r <- parse(r6_resp(body), "chat")
  expect_true(r$ok)
  expect_identical(r$text, "Revenue was 45.2 million.")
  expect_false(grepl("list(", r$text, fixed = TRUE))
  # Reasoning only: no reply, and it says so.
  only <- parse(r6_resp(paste0('{"choices":[{"message":{"content":[{"type":"thinking",',
                               '"thinking":"hmm"}]},"finish_reason":"stop"}]}')), "chat")
  expect_false(only$ok)
  expect_identical(only$text, "")
  # A refusal part is a refusal.
  ref <- parse(r6_resp(paste0('{"choices":[{"message":{"content":[{"type":"refusal",',
                              '"refusal":"I cannot help with that."}]}}]}')), "chat")
  expect_false(ref$ok)
  expect_identical(ref$finish_reason, "refusal")
  # A plain string is unchanged.
  plain <- parse(r6_resp('{"choices":[{"message":{"content":"from chat"}}]}'), "chat")
  expect_identical(plain$text, "from chat")
})

# ---------------------------------------------------------------------------
# client-09: a missing output price is unknown, not free
# ---------------------------------------------------------------------------

test_that("a chat model cannot be registered with half a price", {
  local_registries()
  expect_error(gr_register_model("my-model", 200000, 64000, input_usd = 3),
               class = "gr_bad_setting")
  expect_error(gr_register_model("my-model", 200000, 64000, output_usd = 15),
               class = "gr_bad_setting")
  expect_null(readgpt:::gr_state$models[["my-model"]])
  # Both, neither, and an embedding model's input price alone are fine.
  expect_silent(gr_register_model("priced", 200000, 64000, input_usd = 3, output_usd = 15))
  expect_silent(gr_register_model("unpriced", 200000, 64000))
  expect_silent(gr_register_model("an-embedder", 8191, 0, input_usd = 0.02, kind = "embedding"))
  expect_equal(gr_estimate_cost("an-embedder", 1e6, 0), 0.02)
  expect_true(is.na(gr_estimate_cost("unpriced", 1, 1)))
})

test_that("gr_estimate_cost() is NA when replies cannot be priced", {
  local_registries()
  # An entry that reached the registry some other way.
  readgpt:::registry_set("models", "half-priced", list(
    id = "half-priced", context_window = 200000L, max_output = 64000L, input_usd = 3,
    output_usd = NA_real_, kind = "chat"))
  expect_true(is.na(gr_estimate_cost("half-priced", 1e4, 1e6)))
  expect_true(is.na(gr_estimate_cost("half-priced", 1, 1)))    # preflight's probe
  expect_equal(gr_estimate_cost("half-priced", 1e6, 0), 3)       # no output, no question
})

# ---------------------------------------------------------------------------
# contracts-09: unknown_model_action = "error" covers family-pattern matches
# ---------------------------------------------------------------------------

test_that("unknown_model_action = 'error' stops a family-pattern guess too", {
  local_registries()
  gr_options(unknown_model_action = "error")
  expect_error(gr_model_info("gpt-4-0314-custom"), class = "gr_unknown_model")
  expect_error(gr_model_info("gpt-5.6-tera"), class = "gr_unknown_model")
  expect_error(gr_call(gr_mock_client(), "hi", model = "gpt-5.6-tera"),
               class = "gr_unknown_model")
  # Registered, built-in and alias ids are known and unaffected.
  expect_identical(gr_model_info("gpt-4o")$source, "builtin")
  expect_identical(gr_model_info("chatgpt-4o-latest")$source, "alias")
  gr_options(unknown_model_action = "warn")
  expect_warning(info <- gr_model_info("gpt-4-0314-custom"), class = "gr_unknown_model")
  expect_false(info$certain)
})

# ---------------------------------------------------------------------------
# model-output-08: an extraction reply naming none of the schema's fields
# ---------------------------------------------------------------------------

test_that("under allow_empty, only {} or an object using the schema's names is a reply", {
  schema <- list(type = "object", properties = list(design = list(type = "string"),
                                                    n = list(type = "integer")))
  ask <- function(reply) readgpt:::gr_call_json(gr_mock_client(function(m, p) reply), "x",
                                                schema = schema, allow_empty = TRUE)
  for (reply in c('{"extraction":{"design":"randomised trial","n":120}}',
                  '{"Design":"randomised trial","N":120}', "[]", '[{"design":"RCT"}]')) {
    expect_false(ask(reply)$ok, info = reply)
  }
  for (reply in c("{}", '{"design":"RCT","n":null}', '{"design":null,"notes":"x"}')) {
    expect_true(ask(reply)$ok, info = reply)
  }
})

test_that("a wrapped extraction reply is a failed read, not a complete negative", {
  f <- tempfile(fileext = ".txt")
  writeLines(c("Background text about the trial design.", "",
               "We randomly assigned 120 patients in a randomised trial."), f)
  fl <- gr_fields(design = gr_field("Study design"),
                  n = gr_field("Sample size", type = "integer"))
  cl <- gr_mock_client(function(m, p) '{"extraction":{"design":"randomised trial","n":120}}')
  x <- quiet(gr_extract(f, fl, client = cl, recipe = "fast"))
  expect_false(identical(x$table$status, "ok"))
  expect_match(x$table$error, "failed")
  # A reply that really says nothing is still the complete negative it was.
  cl0 <- gr_mock_client(function(m, p) "{}")
  y <- quiet(gr_extract(f, fl, client = cl0, recipe = "fast"))
  expect_identical(y$table$status, "ok")
  expect_identical(y$table$n_filled, 0L)
})

# ---------------------------------------------------------------------------
# tokenize-embed-06: tables, digit lists and short lines are not under-counted
# ---------------------------------------------------------------------------

# Deterministic texts of the kinds the per-word estimate under-counted.
r6_fixtures <- function() {
  i <- 1:40
  list(
    digits = paste(rep(0:9, 60), collapse = " "),
    aligned_table = paste(sprintf("%-12s %8d %8.2f %6.3f", paste0("Row", i), (i * 137L) %% 9973L,
                                  i * 1.37, i / 41), collapse = "\n"),
    tsv = paste(sprintf("%d\t%d\t%.2f\tyes", i, (i * 7L) %% 100L, i / 40), collapse = "\n"),
    clinical_table = paste(c("Characteristic | Treatment (n=482) | Control (n=479) | p",
                             sprintf("Variable %d | %d (%.1f%%) | %d (%.1f%%) | %.3f", i, i * 9L,
                                     i * 1.9, i * 8L, i * 1.7, i / 97)), collapse = "\n"),
    indented_code = paste(rep(c("def compute(x, y):", "    if x > 0 and y is not None:",
                                "        result = [v * 2 for v in range(x)]",
                                "        return {'total': sum(result), 'n': len(result)}", ""), 12),
                          collapse = "\n"),
    short_lines = paste(rep(paste("Hello, world. The quick brown fox jumps over the lazy dog;",
                                  "it was, however, rather tired."), 20), collapse = "\n"),
    hazard_ratios = paste(sprintf("HR %.2f (95%% CI %.2f-%.2f), p=%.3f;", 0.5 + i / 40, 0.3 + i / 100,
                                  1 + i / 20, i / 41), collapse = " ")
  )
}

test_that("the heuristic is at or above real BPE counts on tables, digits and code", {
  local_registries()
  gr_set_tokenizer("heuristic")
  # Counted offline with the cl100k_base and o200k_base encodings (the two
  # agree on every one of these). The old estimate came in under on six of
  # the seven -- 465 for the digit list, 280 for the TSV.
  real <- c(digits = 1199, aligned_table = 672, tsv = 399, clinical_table = 1017,
            indented_code = 552, short_lines = 440, hazard_ratios = 960)
  fx <- r6_fixtures()
  est <- stats::setNames(gr_count_tokens(unlist(fx)), names(fx))
  for (nm in names(real)) expect_gte(est[[nm]], real[[nm]], label = nm)
})

test_that("the heuristic never counts fewer tokens than the pre-tokenizer's pieces", {
  local_registries()
  gr_set_tokenizer("heuristic")
  # Every piece cl100k's pre-tokenizer cuts is at least one token, so their
  # number is a hard lower bound on the real count. Written out here rather
  # than taken from the package, so the check does not grade itself.
  pieces <- function(x) {
    re <- paste0("'(?i:[sdmt]|ll|ve|re)|[^\\r\\nA-Za-z0-9]?[A-Za-z]+|[0-9]{1,3}|",
                 " ?[^\\sA-Za-z0-9]+[\\r\\n]*|\\s*[\\r\\n]+|\\s+(?!\\S)|\\s+")
    length(regmatches(x, gregexpr(re, x, perl = TRUE))[[1]])
  }
  texts <- c(unlist(r6_fixtures()),
             paste(sprintf("%d", 1:500), collapse = "\n"),
             paste(rep("    x <- f(a, b)\n    if (x) {\n        y[[1]] <- 0\n    }", 30), collapse = "\n"),
             "Table 2\nArm | N | Events\nA | 482 | 31\nB | 479 | 44")
  for (t in texts) expect_gte(gr_count_tokens(t), pieces(t), label = substr(t, 1, 30))
  # Plain prose keeps the per-word figure it had.
  prose <- paste(rep("the quick brown fox jumps over the lazy dog", 20), collapse = " ")
  expect_identical(gr_count_tokens(prose), 198L)
})

test_that("counting stays fast on a large document", {
  local_registries()
  gr_set_tokenizer("heuristic")
  big <- paste(rep(unlist(r6_fixtures()), 60), collapse = "\n\n")
  expect_gt(nchar(big), 500000L)
  el <- system.time(n <- gr_count_tokens(big))[["elapsed"]]
  expect_gt(n, 0L)
  expect_lt(el, 5)
})

# ---------------------------------------------------------------------------
# tokenize-embed-05: truncation keeps the budget's worth of text, and its lines
# ---------------------------------------------------------------------------

test_that("a long unbroken run after one short word is cut inside, not dropped", {
  local_registries()
  gr_set_tokenizer("heuristic")
  cjk <- paste("\u6458\u8981:", strrep("\u516c\u53f8\u6536\u5165\u589e\u957f\u3002", 300))
  out <- gr_truncate_tokens(cjk, 2000)
  expect_lte(gr_count_tokens(out), 2000L)
  expect_gt(gr_count_tokens(out), 1800L)        # it used to keep 10 tokens
  expect_true(startsWith(cjk, sub(" ...[truncated]", "", out, fixed = TRUE)))
  # A refine excerpt: its chunk header, then one long chunk.
  excerpt <- paste0("[chunk 7 p.3]\n", strrep("\u516c\u53f8\u6536\u5165\u589e\u957f", 600))
  cut <- gr_truncate_tokens(excerpt, 3000)
  expect_gt(gr_count_tokens(cut), 2700L)
  # base64 after a label.
  b64 <- paste("data:", strrep("QUJDREVGR0hJSktMTU5PUFFSU1RVVldYWVo0NTY3ODkr", 200))
  expect_gt(gr_count_tokens(gr_truncate_tokens(b64, 500)), 400L)
  # Prose still ends on a whole word.
  prose <- paste(rep("alpha beta gamma delta", 60), collapse = " ")
  p <- gr_truncate_tokens(prose, 40, marker = "")
  expect_true(grepl("(alpha|beta|gamma|delta)$", p))
  expect_true(startsWith(prose, p))
})

test_that("truncation keeps line breaks and paragraph structure", {
  local_registries()
  gr_set_tokenizer("heuristic")
  tab <- "Table 2\nArm | N | Events\nA | 482 | 31\nB | 479 | 44\nC | 470 | 50\nD | 460 | 20"
  out <- gr_truncate_tokens(tab, 30)
  expect_lte(gr_count_tokens(out), 30L)
  expect_match(out, "Table 2\nArm | N | Events\nA | 482", fixed = TRUE)
  findings <- paste(c("Finding one is here.", "Finding two is here.", "Finding three is here."),
                    collapse = "\n\n---\n\n")
  kept <- gr_truncate_tokens(findings, 20, marker = "")
  expect_true(startsWith(findings, kept))
  expect_match(kept, "\n\n---\n\n", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-01: user text is labelled before it is sent
# ---------------------------------------------------------------------------

test_that("a message's unlabelled UTF-8 text is labelled before it is counted or sent", {
  q <- unmarked("Quelle \u00e9tait la population d'\u00c9vora ?")
  expect_identical(Encoding(q), "unknown")
  m <- readgpt:::normalise_messages(list(list(role = "user", content = q)))
  expect_identical(Encoding(m[[1]]$content), "UTF-8")
  expect_identical(Encoding(readgpt:::normalise_messages(q)[[1]]$content), "UTF-8")
  # What gr_truncate_tokens() returns is labelled too; the embeddings request
  # body is built from it.
  expect_identical(Encoding(gr_truncate_tokens(q, 100)), "UTF-8")
  expect_identical(Encoding(gr_truncate_tokens(paste(rep(q, 50), collapse = " "), 30)), "UTF-8")
})

test_that("under a C locale the request body carries the characters, not <c3><a9>", {
  q <- unmarked("Quelle \u00e9tait la population d'\u00c9vora ?")
  wire <- NULL
  testthat::local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      # What httr does with encode = "json".
      wire <<- as.character(jsonlite::toJSON(body, auto_unbox = TRUE))
      simpleError("no network in tests")
    }, .package = "httr")
  cl <- gr_client(api_key = "sk-not-real", base_url = "https://x.invalid", api = "chat",
                  model = "gpt-4o", max_retries = 0L)
  withr::with_locale(c(LC_CTYPE = "C"), {
    skip_if(isTRUE(l10n_info()[["UTF-8"]]), "could not switch to a non-UTF-8 locale")
    invisible(gr_call(cl, paste0("Question: ", q)))
  })
  expect_false(grepl("<c3>", wire, fixed = TRUE))
  expect_true(grepl("\u00e9tait", enc2utf8(wire), fixed = TRUE) ||
                grepl("\\u00e9tait", wire, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# cache-trace-10: an empty cache directory is not the filesystem root
# ---------------------------------------------------------------------------

test_that("gr_cache() refuses an empty or missing directory", {
  local_registries()
  expect_error(gr_cache(dir = ""), class = "gr_bad_setting")
  expect_error(gr_cache(dir = NA), class = "gr_bad_setting")
  expect_error(gr_cache(dir = NA_character_), class = "gr_bad_setting")
  expect_error(gr_cache(dir = c("a", "b")), class = "gr_bad_setting")
  # An option left blank is an option not set.
  gr_options(cache_dir = "")
  expect_identical(gr_cache()$dir, readgpt:::default_cache_dir())
  # And a cache object with no directory never writes under the root.
  bad <- structure(list(dir = "", read = TRUE, write = TRUE,
                        .stats = list2env(list(hits = 0L, misses = 0L, writes = 0L))),
                   class = "gr_cache")
  expect_false(readgpt:::cache_put(bad, "ab00000000000000", readgpt:::gr_result(TRUE, "x")))
  expect_null(readgpt:::cache_get(bad, "ab00000000000000"))
})

# ---------------------------------------------------------------------------
# client-15: max_output = NA is the model's limit, not one token
# ---------------------------------------------------------------------------

test_that("an unreadable max_output warns and uses the model's limit", {
  cl <- gr_mock_client(function(m, p) sprintf("max_output=%d", p$max_output))
  expect_warning(r <- gr_call(cl, "hi", max_output = NA), class = "gr_bad_setting")
  expect_identical(r$text, "max_output=16384")
  expect_warning(r <- gr_call(cl, "hi", max_output = "lots"), class = "gr_bad_setting")
  expect_identical(r$text, "max_output=16384")
  expect_silent(r <- gr_call(cl, "hi", max_output = "200"))
  expect_identical(r$text, "max_output=200")
  expect_identical(gr_call(cl, "hi")$text, "max_output=16384")
})

# ---------------------------------------------------------------------------
# client-13: a backend call that failed after it was sent is still charged
# ---------------------------------------------------------------------------

test_that("a backend handler that raised is charged its prompt", {
  local_registries()
  gr_register_model("priced", 128000L, 4096L, input_usd = 3, output_usd = 15)
  cl <- gr_backend_client(function(messages, params) stop("lexical error: invalid string in json text"),
                          model = "priced")
  tr <- gr_trace()
  r <- gr_call(cl, "How many participants were recruited across the nine sites?",
               model = "priced", trace = tr)
  expect_false(r$ok)
  expect_gt(r$usage$input, 0L)
  expect_gt(tr$tokens_in, 0L)
  expect_gt(tr$spent_usd, 0)
})

test_that("a handler's gr_result without a model is priced as the model asked for", {
  local_registries()
  gr_register_model("priced", 128000L, 4096L, input_usd = 3, output_usd = 15)
  cl <- gr_backend_client(function(messages, params)
    readgpt:::gr_result(TRUE, "x", usage = list(input = 1e6, output = 1e5)), model = "priced")
  tr <- gr_trace()
  r <- gr_call(cl, "hello", model = "priced", trace = tr)
  expect_identical(r$model, "priced")
  expect_equal(tr$spent_usd, 4.5)
})

# ---------------------------------------------------------------------------
# client-14: out of quota is not retried
# ---------------------------------------------------------------------------

test_that("a 429 for an exhausted quota is not retried; a rate limit still is", {
  requests <- 0L
  body <- '{"error":{"message":"You exceeded your current quota.","type":"insufficient_quota","code":"insufficient_quota"}}'
  testthat::local_mocked_bindings(
    POST = function(...) { requests <<- requests + 1L; r6_resp(body, 429L) }, .package = "httr")
  cl <- gr_client(api_key = "sk-not-real", base_url = "https://x.invalid", api = "chat",
                  model = "gpt-4o", max_retries = 3L, retry_pause_base = 0)
  r <- gr_call(cl, "hi")
  expect_false(r$ok)
  expect_identical(requests, 1L)
  expect_false(r$retryable)
  expect_identical(r$status, 429L)
  expect_match(r$error, "quota")

  requests <- 0L
  body <- '{"error":{"message":"Rate limit reached.","type":"requests","code":"rate_limit_exceeded"}}'
  r <- quiet(gr_call(cl, "hi"))
  expect_identical(requests, 4L)
  expect_true(r$retryable)
})

# ---------------------------------------------------------------------------
# client-11: aliases follow a registered override of their target
# ---------------------------------------------------------------------------

test_that("an alias takes a registered override of its target, and keeps its own id", {
  local_registries()
  gr_register_model("gpt-4o", 128000L, 16384L, input_usd = 5, output_usd = 20)
  info <- gr_model_info("chatgpt-4o-latest")
  expect_identical(c(info$input_usd, info$output_usd), c(5, 20))
  expect_identical(info$id, "chatgpt-4o-latest")
  expect_identical(info$source, "alias")
  expect_equal(gr_estimate_cost("chatgpt-4o-latest", 1e6, 1e6),
               gr_estimate_cost("gpt-4o", 1e6, 1e6))
})

test_that("without an override an alias takes the built-in target, as before", {
  local_registries()
  info <- gr_model_info("gpt-4o-2024-08-06")
  expect_identical(info$input_usd, gr_model_info("gpt-4o")$input_usd)
  expect_identical(info$context_window, 128000L)
  expect_identical(info$id, "gpt-4o-2024-08-06")
  expect_true(info$certain)
})

# ---------------------------------------------------------------------------
# model-output-14: bytes that are not UTF-8 in a reply
# ---------------------------------------------------------------------------

test_that("a lone surrogate escape in an API reply is repaired, not a crash", {
  r <- expect_no_error(readgpt:::parse_response(r6_resp(paste0(
    '{"choices":[{"message":{"content":"Revenue was 45 million \\udc00"},',
    '"finish_reason":"stop"}]}')), "chat"))
  expect_true(r$ok)
  expect_true(validUTF8(r$text))
  expect_match(r$text, "Revenue was 45 million")
})

test_that("a reply with bytes that are not UTF-8 is still traced", {
  tr <- gr_trace()
  cl <- gr_mock_client(function(m, p) r6_bad_bytes())
  r <- expect_no_error(gr_call(cl, "hi", trace = tr))
  expect_true(r$ok)
  expect_true(validUTF8(r$text))
  expect_identical(tr$calls, 1L)
  # An error while reading a 200 is a traced failure, not an escape.
  testthat::local_mocked_bindings(parse_response = function(resp, api) stop("boom"),
                                  .package = "readgpt")
  testthat::local_mocked_bindings(POST = function(...) r6_resp("{}"), .package = "httr")
  http <- gr_client(api_key = "sk-not-real", base_url = "https://x.invalid", api = "chat",
                    model = "gpt-4o", max_retries = 0L)
  tr2 <- gr_trace()
  r2 <- expect_no_error(gr_call(http, "hi", trace = tr2))
  expect_false(r2$ok)
  expect_match(r2$error, "Could not read the API response")
  expect_identical(tr2$calls, 1L)
  expect_gt(tr2$tokens_in, 0L)                  # a 200 was billed; its prompt is counted
})

test_that("a surrogate escape inside a structured reply does not fail the extraction", {
  js <- '{"design":"RCT","design__quote":"We randomly \\udc00 assigned","n":null,"n__quote":null}'
  out <- readgpt:::gr_call_json(gr_mock_client(function(m, p) js), "x",
                                schema = list(type = "object"), allow_empty = TRUE)
  expect_true(out$ok)
  expect_true(validUTF8(out$value$design__quote))
  f <- tempfile(fileext = ".txt")
  writeLines(c("Background text about the trial design.", "",
               "We randomly assigned 120 patients in a randomised trial."), f)
  fl <- gr_fields(design = gr_field("Study design"),
                  n = gr_field("Sample size", type = "integer"))
  x <- quiet(gr_extract(f, fl, client = gr_mock_client(function(m, p) js), recipe = "fast"))
  expect_identical(x$table$status, "ok")
  expect_identical(x$table$design, "RCT")
})

# ---------------------------------------------------------------------------
# tokenize-embed-13: tiktoken counts in the right encodings, and document text
# that spells a special token is text
# ---------------------------------------------------------------------------

# Stand-ins for two tiktoken encodings. `encode()` raises as tiktoken does on a
# special-token string unless disallowed_special is emptied.
r6_encoding <- function(per_char) list(encode = function(x, disallowed_special = "all") {
  if (!(is.list(disallowed_special) && !length(disallowed_special)) &&
      grepl("<|endoftext|>", x, fixed = TRUE)) {
    stop("ValueError: Encountered text corresponding to disallowed special token")
  }
  seq_len(ceiling(nchar(x) * per_char))
})

test_that("tiktoken counts document text that spells a special token", {
  local_registries()
  seen <- list()
  testthat::local_mocked_bindings(
    tiktoken_available = function() TRUE,
    tiktoken_encodings = function(model = NULL) {
      seen[[length(seen) + 1L]] <<- model
      if (is.null(model)) list(r6_encoding(0.25), r6_encoding(0.5)) else list(r6_encoding(0.3))
    }, .package = "readgpt")
  gr_set_tokenizer("tiktoken")
  txt <- "Models end a document with <|endoftext|> in training data."
  expect_no_error(n <- gr_count_tokens(txt))
  # No model: the larger of the two encodings, so the count holds for both.
  expect_identical(n, as.integer(ceiling(nchar(txt) * 0.5)))
  expect_identical(gr_count_tokens(txt, model = "gpt-4"), as.integer(ceiling(nchar(txt) * 0.3)))
  # gr_call() counts its prompt in the encoding of the model it calls.
  seen <- list()
  invisible(gr_call(gr_mock_client(), txt, model = "mock-model"))
  expect_true("mock-model" %in% unlist(seen))
})
