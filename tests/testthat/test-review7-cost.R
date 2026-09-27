# test-review7-cost.R -- the cross-file follow-ups for the pricing helpers of
# the segmenters and the extract reader, the trace's meta, the screening and
# field entry points, and the ellmer adapter's parse failures.
#
# Each block names the handoff and says what the old behaviour was.

# ---------------------------------------------------------------------------
# client-10 (H1): the worst case a parallel batch is held to prices the output
# cap a reasoning model is actually sent, not the short cap the caller asked
# for. A reasoning model is sent at least 2048 tokens of room
# (reasoning_output_cap()), so a 90-token context line was priced at 90.
# ---------------------------------------------------------------------------

r7_reasoner <- function(id = "r7-reasoner", context_window = 400000L, max_output = 128000L) {
  gr_register_model(id, context_window = context_window, max_output = max_output,
                    input_usd = 1, output_usd = 10, reasoning = TRUE,
                    supports_temperature = FALSE)
  id
}

test_that("a segmenter's request to a reasoning model is priced at the cap it is sent", {
  local_registries()
  cl <- gr_mock_client(function(m, p) "x")
  cl$model <- r7_reasoner()
  expect_equal(readgpt:::seg_call_usd(cl, 1000, 90L),
               gr_estimate_cost("r7-reasoner", 1000, 2048))
  expect_equal(readgpt:::seg_call_usd(cl, 1000, 2000L),
               gr_estimate_cost("r7-reasoner", 1000, 2048))
  # A cap at or above the floor is sent, and priced, as it is.
  expect_equal(readgpt:::seg_call_usd(cl, 1000, 3000L),
               gr_estimate_cost("r7-reasoner", 1000, 3000))
  # Within what the context window leaves after the prompt.
  cl$model <- r7_reasoner("r7-tight", context_window = 1500L, max_output = 1000L)
  expect_equal(readgpt:::seg_call_usd(cl, 1000, 90L),
               gr_estimate_cost("r7-tight", 1000, 1500 - 1000 - 32))
  # A model that does not reason is priced at the cap it was asked for.
  gr_register_model("r7-plain", context_window = 128000L, max_output = 16384L,
                    input_usd = 1, output_usd = 10)
  cl$model <- "r7-plain"
  expect_equal(readgpt:::seg_call_usd(cl, 1000, 90L), gr_estimate_cost("r7-plain", 1000, 90))
})

test_that("an extraction request to a reasoning model is priced at the cap it is sent", {
  local_registries()
  cl <- gr_mock_client(function(m, p) "x")
  r7_reasoner()
  expect_equal(readgpt:::extract_item_usd(cl, "r7-reasoner", 1000, 100L),
               gr_estimate_cost("r7-reasoner", 1000, 2048))
  # Never above the model's own ceiling.
  r7_reasoner("r7-short", max_output = 500L)
  expect_equal(readgpt:::extract_item_usd(cl, "r7-short", 1000, 100L),
               gr_estimate_cost("r7-short", 1000, 500))
  # An ellmer chat bills as its own model, and is priced as that model reasons.
  ellmer_like <- structure(list(model = "r7-reasoner"),
                           class = c("gr_ellmer_client", "gr_backend_client", "gr_client"))
  gr_register_model("r7-plain", context_window = 128000L, max_output = 16384L,
                    input_usd = 1, output_usd = 10)
  expect_equal(readgpt:::extract_item_usd(ellmer_like, "r7-plain", 1000, 100L),
               gr_estimate_cost("r7-reasoner", 1000, 2048))
})

test_that("contextual segmentation on a reasoning model holds its batch to the cap it sends", {
  local_registries()
  local_clean_cache()
  cl <- gr_mock_client(function(m, p) "Where this excerpt sits.")
  cl$model <- r7_reasoner()
  seen <- list()
  real <- readgpt:::gr_lapply
  local_mocked_bindings(gr_lapply = function(x, fn, ..., client = NULL, item_usd = NULL) {
    seen[[length(seen) + 1L]] <<- item_usd
    real(x, fn, ..., client = client, item_usd = item_usd)
  })
  doc <- quiet(gr_ingest(paste(sprintf("Paragraph %d says the trial enrolled %d patients.",
                                       1:12, 100 + 1:12), collapse = "\n\n")))
  quiet(gr_segment(doc, gr_segment_spec("contextual", context_source = "llm", max_tokens = 60),
                   client = cl))
  expect_length(seen, 1L)
  # Was the price of 90 output tokens and the prompt: about $0.001 here.
  expect_gte(seen[[1]], gr_estimate_cost("r7-reasoner", 0, 2048))
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-01 (H2): what as_json() writes is labelled,
# so a question typed under a non-UTF-8 locale is saved as its characters. A
# trace's meta was written as "caf<c3><a9>", while the prompts beside it were
# clean.
# ---------------------------------------------------------------------------

test_that("a saved trace keeps an unlabelled question intact under a C locale", {
  q <- unmarked("Quelle \u00e9tait la population d'\u00c9vora ?")
  expect_identical(Encoding(q), "unknown")
  f <- withr::local_tempfile(fileext = ".json")
  withr::with_locale(c(LC_CTYPE = "C"), {
    skip_if(isTRUE(l10n_info()[["UTF-8"]]), "could not switch to a non-UTF-8 locale")
    tr <- gr_trace(meta = list(question = q, recipes = q))
    # Filled after the trace was made, as callers do.
    tr$meta$source <- q
    readgpt:::trace_note(tr, "segment.local", list(section = q))
    gr_trace_save(tr, f)
    txt <- as.character(as_json(tr, pretty = FALSE))
  })
  b <- readBin(f, "raw", file.size(f))
  expect_identical(length(grepRaw("<c3>", b, fixed = TRUE)), 0L)
  expect_identical(length(grepRaw(charToRaw(enc2utf8("\u00e9tait")), b, fixed = TRUE, all = TRUE)),
                   4L)
  expect_false(grepl("<c3>", txt, fixed = TRUE, useBytes = TRUE))
  back <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_identical(back$meta$question, enc2utf8("Quelle \u00e9tait la population d'\u00c9vora ?"))
  # A field that lists things is still an array at length one.
  expect_true(is.list(back$meta$recipes))
})

test_that("as_json() labels strings, names and factor levels, and leaves the object alone", {
  x <- list(a = I(unmarked("caf\u00e9")), b = c(k = "v"), n = 1:3,
            latin = iconv("caf\u00e9", "UTF-8", "latin1"),
            f = factor(unmarked("\u00e9t\u00e9")), na = NA_character_)
  names(x)[2] <- unmarked("\u00c9vora")
  y <- readgpt:::label_utf8(stats::setNames(unmarked(c("caf\u00e9", NA)), unmarked(c("\u00e9", "b"))))
  expect_identical(Encoding(y), c("UTF-8", "unknown"))
  expect_identical(Encoding(names(y)), c("UTF-8", "unknown"))
  expect_true(is.na(y[[2]]))
  withr::with_locale(c(LC_CTYPE = "C"), {
    skip_if(isTRUE(l10n_info()[["UTF-8"]]), "could not switch to a non-UTF-8 locale")
    txt <- as.character(as_json(x, pretty = FALSE))
  })
  expect_false(grepl("<c3>", txt, fixed = TRUE, useBytes = TRUE))
  back <- jsonlite::parse_json(txt)
  expect_identical(back$a, list(enc2utf8("caf\u00e9")))
  expect_identical(names(back)[2], enc2utf8("\u00c9vora"))
  expect_identical(back$f, enc2utf8("\u00e9t\u00e9"))
  expect_identical(back$latin, enc2utf8("caf\u00e9"))
  expect_identical(back$n, list(1L, 2L, 3L))
  # Only the JSON: the caller's object keeps the strings it had.
  expect_identical(Encoding(x$a), "unknown")
  expect_s3_class(x$a, "AsIs")
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-01 (H3): a field's description and values are
# labelled where they come in. Under a C locale an unlabelled enum value went
# out in the schema as "r<c3><a9>duit", and a reply giving the declared value
# did not match it. A screening's criteria are written intact by as_json(),
# and are not relabelled in the object, where the audit compares them with the
# protocol's.
# ---------------------------------------------------------------------------

test_that("an enum value typed under a C locale matches the reply that gives it", {
  v <- unmarked("r\u00e9duit")
  withr::with_locale(c(LC_CTYPE = "C"), {
    skip_if(isTRUE(l10n_info()[["UTF-8"]]), "could not switch to a non-UTF-8 locale")
    f <- gr_field(unmarked("Direction de l'\u00e9ffet"), type = "enum", values = c(v, "stable"))
    got <- readgpt:::coerce_field(list("r\u00e9duit"), f)
    schema <- as.character(jsonlite::toJSON(readgpt:::fields_schema(gr_fields(dir = f)),
                                            auto_unbox = TRUE))
  })
  expect_identical(Encoding(f$values[1]), "UTF-8")
  expect_identical(Encoding(f$description), "UTF-8")
  expect_identical(got, enc2utf8("r\u00e9duit"))
  expect_false(grepl("<c3>", schema, fixed = TRUE, useBytes = TRUE))
})

r7_screen_c <- function(protocol = FALSE) {
  crit <- "Adultes \u00e2g\u00e9s de plus de 65 ans"
  q <- unmarked("Le traitement r\u00e9duit-il les chutes ?")
  cl <- gr_mock_client(function(m, p) {
    paste0('{"decision":"exclude","reason":"Des enfants.","criterion":"', crit, '","quote":null}')
  })
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f), add = TRUE)
  writeBin(charToRaw(enc2utf8("Nous avons \u00e9tudi\u00e9 des enfants.\n")), f)
  out <- NULL
  withr::with_locale(c(LC_CTYPE = "C"), {
    skip_if(isTRUE(l10n_info()[["UTF-8"]]), "could not switch to a non-UTF-8 locale")
    p <- gr_protocol("r7", question = q, include = unmarked(crit))
    s <- if (protocol) quiet(gr_screen(f, protocol = p, client = cl)) else
      quiet(gr_screen(f, question = q, include = unmarked(crit), client = cl))
    out <- list(crit = crit, p = p, s = s, json = as.character(as_json(s, pretty = FALSE)),
                drift = readgpt:::protocol_drift(p, s))
  })
  out
}

test_that("a screening typed under a C locale is written with its criteria intact", {
  local_registries()
  local_clean_cache()
  got <- r7_screen_c()
  expect_identical(got$s$table$decision, "exclude")
  expect_identical(got$s$table$criterion_valid, TRUE)
  expect_false(grepl("<c3>", got$json, fixed = TRUE, useBytes = TRUE))
  back <- jsonlite::fromJSON(got$json)
  expect_identical(back$include, enc2utf8(got$crit))
  expect_identical(back$table$criterion, enc2utf8(got$crit))
})

test_that("a screening run from a protocol under a C locale is not reported as drifting", {
  local_registries()
  local_clean_cache()
  got <- r7_screen_c(protocol = TRUE)
  expect_identical(got$drift$checked, "the screening")
  expect_identical(got$drift$differ, character(0))
})

# ---------------------------------------------------------------------------
# client-13 (H6): a structured reply ellmer could not read was billed, and the
# chat kept the turn's token counts; the adapter raised instead, so the trace
# charged the local count of the prompt and no reply at all.
# ---------------------------------------------------------------------------

r7_skip_without_ellmer_05 <- function() {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("httr2")
  chat <- ellmer::chat_anthropic(model = "claude-test", credentials = function() "k")
  skip_if(!is.function(chat$get_model_object), "this ellmer has no model object (before 0.5.0)")
}

r7_anthropic <- function(content, status = 200L) {
  body <- if (status == 200L) {
    sprintf(paste0('{"id":"m","type":"message","role":"assistant","model":"claude-test",',
                   '"content":%s,"stop_reason":"end_turn","stop_sequence":null,',
                   '"usage":{"input_tokens":1000,"output_tokens":250}}'), content)
  } else {
    '{"type":"error","error":{"type":"invalid_request_error","message":"bad request"}}'
  }
  httr2::response(status_code = status, headers = list(`Content-Type` = "application/json"),
                  body = charToRaw(body))
}

r7_schema <- list(type = "object", properties = list(a = list(type = "string")),
                  required = list("a"))

test_that("an ellmer structured reply that cannot be read is charged what ellmer recorded", {
  r7_skip_without_ellmer_05()
  local_registries()
  gr_register_model("claude-test", context_window = 200000L, max_output = 64000L,
                    input_usd = 3, output_usd = 15)
  httr2::local_mocked_responses(function(req) {
    r7_anthropic('[{"type":"text","text":"I would rather not answer in JSON."}]')
  })
  cl <- gr_ellmer_client(ellmer::chat_anthropic(model = "claude-test", credentials = function() "k"))
  tr <- gr_trace()
  res <- quiet(readgpt:::gr_call_json(cl, "Question?", schema = r7_schema, trace = tr))
  expect_false(res$ok)
  step <- tr$steps[[1]]
  expect_false(step$ok)
  expect_match(step$error, "no JSON", fixed = TRUE)
  # Was input = the local count of "Question?" and output = 0.
  expect_identical(as.integer(step$tokens$input), 1000L)
  expect_identical(as.integer(step$tokens$output), 250L)
  expect_equal(tr$spent_usd, gr_estimate_cost("claude-test", 1000, 250))
  expect_length(tr$errors, 1L)
})

test_that("an ellmer request that failed before any reply is still charged its prompt", {
  r7_skip_without_ellmer_05()
  local_registries()
  gr_register_model("claude-test", context_window = 200000L, max_output = 64000L,
                    input_usd = 3, output_usd = 15)
  httr2::local_mocked_responses(function(req) r7_anthropic(NULL, status = 400L))
  cl <- gr_ellmer_client(ellmer::chat_anthropic(model = "claude-test", credentials = function() "k"))
  tr <- gr_trace()
  res <- quiet(readgpt:::gr_call_json(cl, "Question?", schema = r7_schema, trace = tr))
  expect_false(res$ok)
  step <- tr$steps[[1]]
  expect_match(step$error, "400", fixed = TRUE)
  expect_gt(step$tokens$input, 0L)
  expect_identical(as.integer(step$tokens$output), 0L)
})
