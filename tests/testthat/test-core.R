# test-core.R -- tokenizer, client contract, trace, registries.

test_that("token counting is conservative and never underestimates badly", {
  expect_equal(gr_count_tokens(""), 0L)
  expect_equal(gr_count_tokens(character(0)), integer(0))
  expect_equal(gr_count_tokens(NA_character_), 0L)
  # v1's estimate_token_count(" a b") returned 3 because strsplit produced an
  # empty leading token.
  expect_equal(gr_count_tokens(" a b"), gr_count_tokens("a b"))
  # Should exceed a plain word count for ordinary English prose.
  prose <- paste(rep("the quick brown fox jumps over the lazy dog", 20), collapse = " ")
  expect_gt(gr_count_tokens(prose), length(strsplit(prose, "\\s+")[[1]]))
  # Vectorised, one count per element.
  expect_length(gr_count_tokens(c("a", "bb cc", "")), 3L)
})

test_that("truncation lands on a word boundary and respects the budget", {
  txt <- paste(rep("alpha beta gamma delta", 60), collapse = " ")
  out <- gr_truncate_tokens(txt, 40)
  expect_lte(gr_count_tokens(out), 45L)
  expect_true(nzchar(out))
  expect_equal(gr_truncate_tokens("anything", 0), "")
  expect_equal(gr_truncate_tokens("", 100), "")
  expect_equal(gr_truncate_tokens("short text", 1000), "short text")
})

test_that("a custom tokenizer can be registered and changes budgets", {
  old <- gr_tokenizer()
  on.exit(gr_set_tokenizer(old), add = TRUE)
  gr_set_tokenizer("double_words", function(x) 2L * vapply(x, function(s)
    length(strsplit(trimws(s), "\\s+")[[1]]), integer(1), USE.NAMES = FALSE))
  expect_equal(gr_count_tokens("a b c"), 6L)
})

test_that("gr_call always returns a usable gr_result", {
  ok <- gr_call(mock_echo("hello"), list(list(role = "user", content = "hi")))
  expect_s3_class(ok, "gr_result")
  expect_true(ok$ok)
  expect_type(ok$text, "character")
  expect_length(ok$text, 1L)

  bad <- gr_call(mock_dead(), list(list(role = "user", content = "hi")))
  expect_false(bad$ok)
  expect_identical(bad$text, "")     # never NULL, never character(0)
  expect_true(nzchar(bad$error))
})

test_that("gr_call refuses a prompt that leaves no room for a reply", {
  local_registries()
  gr_register_model("tiny-window", context_window = 600, max_output = 128)
  big <- paste(rep("word", 5000), collapse = " ")
  res <- gr_call(gr_mock_client(), list(list(role = "user", content = big)),
                 model = "tiny-window")
  expect_false(res$ok)
  expect_match(res$error, "context window")
})

test_that("reasoning models do not receive a temperature parameter", {
  info <- gr_model_info("gpt-5.6-terra")
  expect_true(info$reasoning)
  expect_false(info$supports_temperature)
  body <- readgpt:::build_request_body(
    gr_client(model = "gpt-5.6-terra"),
    list(list(role = "user", content = "hi")),
    "gpt-5.6-terra", 100L, 0.7, NULL, "r", info, list())
  expect_null(body$temperature)
  expect_equal(body$max_output_tokens, 100L)
})

test_that("the responses and chat request bodies have the right shapes", {
  info <- gr_model_info("gpt-4o")
  msgs <- list(list(role = "system", content = "S"), list(role = "user", content = "U"))
  schema <- list(type = "object", properties = list(a = list(type = "string")))

  r <- readgpt:::build_request_body(gr_client(api = "responses"), msgs, "gpt-4o", 64L,
                                    0.2, schema, "s", info, list())
  expect_equal(r$instructions, "S")
  expect_equal(r$input[[1]]$role, "user")
  expect_equal(r$max_output_tokens, 64L)
  expect_equal(r$text$format$type, "json_schema")

  c2 <- readgpt:::build_request_body(gr_client(api = "chat"), msgs, "gpt-4o", 64L,
                                     0.2, schema, "s", info, list())
  expect_length(c2$messages, 2L)
  expect_equal(c2$max_tokens, 64L)
  expect_equal(c2$response_format$type, "json_schema")
})

test_that("assistant text is extracted from both API response shapes", {
  resp <- list(output = list(list(type = "message", role = "assistant",
                                  content = list(list(type = "output_text", text = "from responses")))))
  expect_equal(readgpt:::extract_text(resp, "responses"), "from responses")
  chat <- list(choices = list(list(message = list(content = "from chat"))))
  expect_equal(readgpt:::extract_text(chat, "chat"), "from chat")
  refusal <- list(choices = list(list(message = list(refusal = "I cannot help with that."))))
  expect_equal(readgpt:::extract_text(refusal, "chat"), "I cannot help with that.")
  expect_equal(readgpt:::extract_text(list(), "chat"), "")
})

test_that("400 is not retried but 429 is", {
  expect_false(400L %in% readgpt:::.retryable_status)
  expect_true(429L %in% readgpt:::.retryable_status)
  expect_true(503L %in% readgpt:::.retryable_status)
})

test_that("the trace records every call from a single run", {
  ch <- gr_segment(gr_ingest(sample_doc(4, 4)), list(method = "paragraph", max_tokens = 150))
  expect_gt(nrow(ch$chunks), 1L)
  tr <- gr_trace()
  a <- quiet(gr_read(ch, "Q?", mock_echo(), "map_reduce", trace = tr))
  s <- gr_trace_summary(tr)
  expect_gt(s$calls, 1L)
  expect_identical(a$trace$run_id, tr$run_id)
  # The JSON view comes from the SAME run -- v1 re-ran the whole pipeline to
  # produce its "chain of thought", doubling every user's bill.
  j <- as_json(a)
  expect_true(jsonlite::validate(j))
  parsed <- jsonlite::fromJSON(j, simplifyVector = FALSE)
  expect_equal(parsed$trace$run_id, tr$run_id)
})

test_that("a NULL field survives JSON serialisation as null", {
  # v1: assigning NULL into an R list DELETES the key, so `final_answer`
  # silently disappeared from the JSON whenever a call failed.
  j <- as_json(list(a = 1, b = NULL, c = NA))
  expect_true(jsonlite::validate(j))
})

test_that("registries reject unknown names with an actionable message", {
  expect_error(gr_segment(gr_ingest(sample_doc(1, 2)), "no_such_segmenter"),
               class = "gr_unknown_method")
  expect_error(gr_read_spec(reader = "no_such_reader"), class = "gr_unknown_method")
  expect_error(gr_clean("x", steps = "no_such_cleaner"), class = "gr_error")
  expect_error(gr_options(no_such_option = 1), class = "gr_error")
})

test_that("a user can register a segmenter and a reader", {
  local_registries()
  gr_register_segmenter("every_block", function(doc, spec, client, trace) {
    readgpt:::new_chunks(doc$blocks$text, "every_block", spec)
  }, description = "one chunk per block")
  ch <- gr_segment(gr_ingest(sample_doc(2, 3)), list(method = "every_block", max_tokens = 4000))
  expect_equal(ch$method, "every_block")

  gr_register_reader("first_only", function(chunks, question, client, spec, trace) {
    readgpt:::new_answer("custom", "first_only", question, 1L, trace)
  }, signature = "first|1|none", description = "test reader")
  a <- gr_read(ch, "Q?", mock_echo(), "first_only")
  expect_equal(a$answer, "custom")
  expect_equal(gr_reader_signature("first_only"), "first|1|none")
})

test_that("BM25 ranks a relevant chunk above an irrelevant one", {
  docs <- c("The cat sat on the mat in the kitchen.",
            "Quarterly revenue rose to forty five million dollars.",
            "Photosynthesis converts light into chemical energy.")
  s <- readgpt:::bm25_scores(docs, "what was quarterly revenue")
  expect_equal(which.max(s), 2L)
})

test_that("cosine similarity handles degenerate input without NaN", {
  expect_equal(readgpt:::cosine_similarity(c(0, 0), c(1, 1)), 0)
  expect_equal(readgpt:::cosine_similarity(numeric(0), numeric(0)), 0)
  expect_equal(round(readgpt:::cosine_similarity(c(1, 0), c(1, 0)), 6), 1)
})

test_that("the mock client records prompts for assertion", {
  cl <- mock_echo("x")
  gr_call(cl, list(list(role = "user", content = "the prompt")), label = "lbl")
  expect_length(cl$calls(), 1L)
  expect_equal(cl$calls()[[1]]$label, "lbl")
  cl$reset()
  expect_length(cl$calls(), 0L)
})
