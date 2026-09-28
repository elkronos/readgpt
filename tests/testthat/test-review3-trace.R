# test-review3-trace.R -- the third pass on the trace, embedding and JSON fixes:
# regressions the earlier fixes introduced, each checked against what the code
# did before any of them.
#
# Each block names the finding and says what the old behaviour was.

# Stands in for an OpenAI-shaped server: chat completions always answer, with a
# usage block of about a token per four characters; the embeddings endpoint
# answers with vectors and usage, or with `embed_status` when that is not 200.
r3_endpoint <- function(embed_status = 200L, env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$embed <- 0L
  seen$chat <- 0L
  respond <- function(url, status, out) {
    structure(list(url = url, status_code = status,
                   headers = structure(list(`content-type` = "application/json; charset=utf-8"),
                                       class = c("insensitive", "list")),
                   content = charToRaw(as.character(jsonlite::toJSON(out, auto_unbox = TRUE)))),
              class = "response")
  }
  testthat::local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      if (grepl("/embeddings$", url)) {
        seen$embed <- seen$embed + 1L
        if (embed_status != 200L) {
          return(respond(url, embed_status, list(error = list(message = "no such model"))))
        }
        texts <- unlist(body$input)
        return(respond(url, 200L, list(
          data = lapply(seq_along(texts), function(i) {
            list(embedding = as.list(c(nchar(texts[i]) %% 7 + 1, 1, i %% 3)))
          }),
          usage = list(prompt_tokens = sum(ceiling(nchar(texts) / 4))))))
      }
      seen$chat <- seen$chat + 1L
      all <- paste(vapply(body$messages, function(m) as.character(m$content), ""),
                   collapse = "\n")
      respond(url, 200L, list(
        id = "r3", object = "chat.completion", model = body$model,
        choices = list(list(index = 0, finish_reason = "stop",
                            message = list(role = "assistant",
                                           content = "Revenue was 45.2 million dollars."))),
        usage = list(prompt_tokens = ceiling(nchar(all) / 4), completion_tokens = 7)))
    },
    .package = "httr", .env = env)
  seen
}

r3_client <- function(embedding_model = "text-embedding-3-small") {
  gr_client(api_key = "sk-test", base_url = "https://r3.invalid/v1", api = "chat",
            model = "gpt-4o", embedding_model = embedding_model, max_retries = 0L)
}

r3_doc <- function(n = 40L) {
  paste(sprintf("Paragraph %d describes operations in region %d and the staff there.",
                seq_len(n), seq_len(n)), collapse = "\n\n")
}

# ---------------------------------------------------------------------------
# cache-2: a failed embeddings request that the lexical fallback replaced is a
# recovered failure (contract C1), so it does not fail the document. Before
# the fixes the request was never recorded; after them it went into
# trace$errors like any failure, and gr_read_many() called every document on
# an endpoint without embeddings "failed ... not read in full" and never
# stored it.
# ---------------------------------------------------------------------------

test_that("a failed embeddings request the fallback replaced is marked recovered", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- r3_endpoint(embed_status = 404L)
  tr <- gr_trace()
  expect_warning(e <- gr_embed(r3_client(), c("one", "two"), trace = tr),
                 class = "gr_embed_fallback")
  expect_identical(seen$embed, 1L)
  expect_true(isTRUE(attr(e, "embedding_fallback")))
  # Still a request, still counted and still in the ledger as a failure.
  expect_identical(tr$calls, 1L)
  expect_length(tr$errors, 1L)
  expect_match(tr$errors[[1]]$error, "HTTP 404")
  expect_true(isTRUE(tr$errors[[1]]$recovered))
  st <- tr$steps[[tr$errors[[1]]$step]]
  expect_identical(st$label, "embed.request")
  expect_false(st$ok)
  expect_true(isTRUE(st$recovered))
  # And it serialises, so a saved trace says so too.
  j <- jsonlite::fromJSON(as.character(as_json(tr)), simplifyVector = FALSE)
  expect_true(isTRUE(j$errors[[1]]$recovered))
})

test_that("a failure nothing recovered from is not marked", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- r3_endpoint(embed_status = 404L)
  cl <- r3_client()

  # fallback = "error": the caller is stopped, so the input was not embedded.
  tr <- gr_trace()
  expect_error(gr_embed(cl, c("one", "two"), trace = tr, fallback = "error"),
               class = "gr_embed_error")
  expect_length(tr$errors, 1L)
  expect_null(tr$errors[[1]]$recovered)

  # fallback = "none": an empty matrix, nothing embedded.
  tr <- gr_trace()
  e <- gr_embed(cl, c("one", "two"), trace = tr, fallback = "none")
  expect_identical(dim(e), c(0L, 0L))
  expect_null(tr$errors[[1]]$recovered)

  # Only this call's failures: one already in the trace keeps its status.
  tr <- gr_trace()
  readgpt:::trace_record(tr, "map.answer", list(list(role = "user", content = "q")),
                         gr_result(FALSE, error = "HTTP 503: down", status = 503L,
                                   model = "gpt-4o"))
  expect_warning(gr_embed(cl, c("one", "two"), trace = tr), class = "gr_embed_fallback")
  expect_length(tr$errors, 2L)
  expect_null(tr$errors[[1]]$recovered)
  expect_true(isTRUE(tr$errors[[2]]$recovered))
})

test_that("an endpoint without embeddings no longer fails every document of a corpus", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- r3_endpoint(embed_status = 404L)
  dir <- withr::local_tempdir()
  a <- file.path(dir, "a.txt")
  b <- file.path(dir, "b.txt")
  writeLines(r3_doc(), a)
  writeLines(gsub("operations", "research", r3_doc()), b)
  store <- file.path(dir, "store")
  out <- quiet(gr_read_many(c(a, b), "What was revenue?", recipe = "needle",
                            client = r3_client("bad-embed"), store = store))
  expect_gt(seen$embed, 0L)
  # Every failure in the run was an embeddings request the fallback replaced.
  errs <- out$trace$errors
  expect_gt(length(errs), 0L)
  expect_true(all(vapply(errs, function(e) isTRUE(e$recovered), logical(1))))
  # The answer still says what the fallback cost it, exactly as before.
  expect_true(all(vapply(out$answers, function(x) isTRUE(x$partial), logical(1))))

  # The document-level half of contract C1 (failed_note() passes over a
  # recovered failure). Asserted outright: this test used to probe
  # failed_note() first and skip when it did not, so the one regression it
  # guards against reported a skip instead of a failure (review 5,
  # corpus-trace-5).
  expect_identical(out$summary$status, c("ok", "ok"))
  expect_true(all(is.na(out$summary$error)))
  expect_length(list.files(store, recursive = TRUE), 2L)
})

test_that("a request that failed without sending tokens costs nothing, priced or not", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- r3_endpoint(embed_status = 404L)
  tr <- gr_trace()
  readgpt:::trace_record(tr, "map.answer", list(list(role = "user", content = "q")),
                         gr_result(TRUE, text = "a", model = "gpt-4o",
                                   usage = list(input = 1000L, output = 10L)))
  # "bad-embed" has no registered price. The request failed, so it cost 0, and
  # the run's cost is still known row by row.
  quiet(gr_embed(r3_client("bad-embed"), c("one", "two"), trace = tr))
  df <- as.data.frame(tr)
  expect_identical(df$stage, c("map.answer", "embed.request"))
  expect_identical(df$usd[2], 0)
  expect_equal(df$usd[1], gr_estimate_cost("gpt-4o", 1000L, 10L))
  # A request that did send tokens to an unpriced model is still unknown.
  readgpt:::trace_record(tr, "map.answer", list(list(role = "user", content = "q2")),
                         gr_result(TRUE, text = "a", model = "no-such-model",
                                   usage = list(input = 5L, output = 1L)))
  expect_true(is.na(as.data.frame(tr)$usd[3]))
})

# ---------------------------------------------------------------------------
# cache-4: embedding tokens are counted apart from the model calls' tokens.
# They went into trace$tokens_in, so the documented estimate,
# gr_estimate_cost(model, tokens_in, tokens_out), priced them at the chat
# model's rate: 20 to 100 times what a run that embeds cost. Before the fixes
# they were not counted at all, and the estimate covered the model calls.
# ---------------------------------------------------------------------------

test_that("embeddings requests count as calls, but their tokens stay out of tokens_in", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- r3_endpoint()
  tr <- gr_trace()
  e <- gr_embed(r3_client(), c("alpha beta gamma delta", "epsilon zeta"), batch_size = 1L,
                trace = tr)
  expect_identical(attr(e, "embedding_source"), "api")
  expect_identical(tr$calls, 2L)                    # the limits still see them
  expect_identical(tr$tokens_in, 0L)
  expect_identical(tr$tokens_out, 0L)
  expect_identical(tr$embed_tokens, 6L + 3L)
  s <- gr_trace_summary(tr)
  expect_identical(s$embed_calls, 2L)
  expect_identical(s$embed_tokens, 9L)
  expect_identical(s$tokens_in, 0L)
  # Still priced, at the embedding model.
  cost <- gr_trace_cost(tr)
  expect_identical(cost$model, "text-embedding-3-small")
  expect_identical(cost$paid_in, 9L)
  expect_true(all(vapply(tr$steps[vapply(tr$steps, function(st) identical(st$label, "embed.request"),
                                         logical(1))],
                         function(st) identical(st$kind, "embedding"), logical(1))))
})

test_that("the documented estimate prices a run that embeds by its model calls", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- r3_endpoint()
  ans <- quiet(answer_document(r3_doc(120L), "What was revenue?", "needle", client = r3_client()))
  s <- gr_trace_summary(ans$trace)
  expect_gt(s$embed_calls, 0L)
  expect_identical(s$calls, seen$chat + seen$embed)
  cost <- gr_trace_cost(ans$trace)
  chat <- cost[cost$model == "gpt-4o", ]
  emb <- cost[cost$model == "text-embedding-3-small", ]
  # tokens_in and tokens_out are exactly the model calls' tokens, and
  # embed_tokens the embeddings requests'.
  expect_identical(s$tokens_in, chat$paid_in)
  expect_identical(s$tokens_out, chat$paid_out)
  expect_identical(s$embed_tokens, emb$paid_in)
  expect_equal(gr_estimate_cost("gpt-4o", s$tokens_in, s$tokens_out), chat$usd)
  # gr_trace_cost() is still the whole bill.
  expect_equal(sum(cost$usd), chat$usd + emb$usd)
  expect_lt(gr_estimate_cost("gpt-4o", s$tokens_in, s$tokens_out), sum(cost$usd))
})

test_that("embedding tokens survive being folded into a parent trace", {
  parent <- gr_trace()
  child <- gr_trace()
  child$embed_tokens <- 40L
  child$tokens_in <- 7L
  readgpt:::trace_absorb(parent, child)
  readgpt:::trace_absorb(parent, child)
  expect_identical(parent$embed_tokens, 80L)
  expect_identical(parent$tokens_in, 14L)
})

# ---------------------------------------------------------------------------
# cache-6: print() called every embeddings request a "model call", so a needle
# run that made one model call printed "7 model calls".
# ---------------------------------------------------------------------------

test_that("print() lists embeddings requests apart from model calls", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- r3_endpoint(embed_status = 404L)
  tr <- gr_trace()
  readgpt:::trace_record(tr, "map.answer", list(list(role = "user", content = "q")),
                         gr_result(TRUE, text = "a", model = "gpt-4o",
                                   usage = list(input = 10L, output = 2L)))
  # The first request fails and the fallback takes over, so one is sent.
  quiet(gr_embed(r3_client(), c("one", "two"), batch_size = 1L, trace = tr))
  out <- paste(capture.output(print(tr)), collapse = "\n")
  expect_match(out, "1 model calls, 1 embeddings request(s) (0 tokens), 10 in / 2 out tokens",
               fixed = TRUE)
  expect_match(out, "1 error(s) (1 recovered by a fallback)", fixed = TRUE)

  # A run that embeds nothing prints as it always did.
  plain <- gr_trace()
  readgpt:::trace_record(plain, "map.answer", list(list(role = "user", content = "q")),
                         gr_result(TRUE, text = "a", model = "gpt-4o",
                                   usage = list(input = 10L, output = 2L)))
  out <- capture.output(print(plain))[1]
  expect_true(endsWith(out, "1 steps, 1 model calls, 10 in / 2 out tokens, 0 error(s)"))
})

# ---------------------------------------------------------------------------
# cache-5: as_json(digits = NA) kept 15 significant digits, so a number that
# needs 16 or 17 (a 16-digit identifier, a large amount with cents) was written
# as a different number. jsonlite's four-decimal default, used before the
# fixes, wrote those exactly.
# ---------------------------------------------------------------------------

test_that("as_json() writes numbers that need 16 or 17 digits exactly", {
  x <- list(assets = 12345678901234.56, id = 1234567890123456, max = 2^53,
            p = 0.00003, d = 0.84321, tenth = 0.1, n = 3000000001, big = 1e15)
  txt <- as.character(as_json(x, pretty = FALSE))
  expect_identical(txt, paste0('{"assets":12345678901234.56,"id":1234567890123456,',
                               '"max":9007199254740992,"p":3e-05,"d":0.84321,"tenth":0.1,',
                               '"n":3000000001,"big":1e+15}'))
  back <- jsonlite::fromJSON(txt)
  for (k in names(x)) expect_identical(back[[k]], x[[k]], info = k)
  # Pretty-printed too, and inside vectors and data frames.
  y <- list(v = c(1234567890123456, 0.5), df = data.frame(a = c(12345678901234.56, 2)))
  back <- jsonlite::fromJSON(as.character(as_json(y)))
  expect_identical(back$v, y$v)
  expect_identical(back$df$a, y$df$a)
})

test_that("as_json() leaves numbers 15 digits write exactly as they were", {
  x <- list(p = 0.00003, d = 0.84321, tenth = 0.1, n = 3000000001, neg = -2.5,
            int = 7L, na = NA_real_, s = "text")
  expect_identical(as.character(as_json(x, pretty = FALSE)),
                   as.character(jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA,
                                                 na = "null", null = "null")))
  # A caller can still round, as jsonlite lets them.
  expect_identical(as.character(as_json(list(a = 1234567890123456, b = 1 / 3),
                                        pretty = FALSE, digits = 2)),
                   as.character(jsonlite::toJSON(list(a = 1234567890123456, b = 1 / 3),
                                                 auto_unbox = TRUE, digits = 2)))
  expect_identical(as.character(as_json(list(b = 1 / 3), pretty = FALSE, digits = I(3))),
                   '{"b":0.333}')
})

test_that("whether a number survives is judged by a JSON reader, not R's own", {
  # R's reader is not correctly rounded everywhere (on arm64 "1e-300" reads as
  # the double below the nearest one), so it cannot be the judge of what 15
  # digits write exactly. Every value here reads back identical, and the
  # nearest double to 1e-300 is written as 1e-300.
  x <- jsonlite::fromJSON("[1e-300, 5e-324, 1.7976931348623157e308, 0.1, 123456.7]")
  txt <- as.character(as_json(list(v = x), pretty = FALSE))
  expect_identical(jsonlite::fromJSON(txt)$v, x)
  expect_match(txt, "[1e-300,", fixed = TRUE)
  y <- c(1e-300, 2^-1074, 1 / 3, 2 / 3, 0.1 + 0.2, pi * 1e200, -exp(1) * 1e-200)
  expect_identical(jsonlite::fromJSON(as.character(as_json(list(v = y))))$v, y)
})

test_that("rewriting the digits never touches text inside a string", {
  s <- "1.23456789012346e+15 and 0.10000000000000001, \"quoted\" 42 café"
  x <- list(s = s, id = 1234567890123456, "12345678901234.561" = 1)
  txt <- as_json(x, pretty = FALSE)
  back <- jsonlite::fromJSON(as.character(txt))
  expect_identical(back$s, s)
  expect_identical(back$id, 1234567890123456)
  expect_identical(names(back)[3], "12345678901234.561")
  expect_s3_class(txt, "json")
  expect_identical(Encoding(as.character(txt)), "UTF-8")
})

test_that("an extraction's answer text keeps a 16-digit number", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("Total assets were 12345678901234.56 dollars.", "",
               "The registry identifier is 1234567890123456."), f)
  cl <- gr_mock_client(function(m, p) '{"assets": 12345678901234.56, "id": 1234567890123456}')
  fields <- gr_fields(assets = gr_field("Total assets", type = "number"),
                      id = gr_field("Registry identifier", type = "number"))
  x <- quiet(gr_extract(f, fields, client = cl))
  expect_identical(x$table$id, 1234567890123456)
  expect_identical(x$summary$answer, '{"assets":12345678901234.56,"id":1234567890123456}')
  back <- jsonlite::fromJSON(x$summary$answer)
  expect_identical(back$assets, x$table$assets)
  expect_identical(back$id, x$table$id)
})

test_that("an absorbed error still names the step that failed", {
  # trace_absorb() moved the child's steps after the parent's but left each
  # error's step number as it was, so the error pointed at one of the parent's
  # own steps, and trace_mark_recovered() marked that one instead.
  parent <- gr_trace()
  readgpt:::trace_record(parent, "first", list(list(role = "user", content = "a")),
                         readgpt:::gr_result(TRUE, text = "ok"))
  child <- gr_trace()
  readgpt:::trace_record(child, "fails", list(list(role = "user", content = "b")),
                         readgpt:::gr_result(FALSE, error = "HTTP 500"))
  readgpt:::trace_absorb(parent, child)
  e <- parent$errors[[length(parent$errors)]]
  expect_identical(parent$steps[[e$step]]$label, "fails")
})
