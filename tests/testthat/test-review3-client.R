# test-review3-client.R -- regressions from the third review pass over the
# model client: the ellmer adapter's structured replies (core-backend.R) and
# the model registry (core-models.R).
#
# These drive REAL ellmer chats with httr2's mocked responses, because every
# defect here lived between ellmer's parsing of the provider's reply and this
# package's reading of it, which a stub chat_structured() skips.

skip_without_ellmer_05 <- function() {
  skip_if_not_installed("ellmer")
  skip_if_not_installed("httr2")
  chat <- ellmer::chat_anthropic(model = "claude-test", credentials = function() "k")
  skip_if(!is.function(chat$get_model_object), "this ellmer has no model object (before 0.5.0)")
}

# A raw JSON body, so that a null in the reply reaches ellmer as null.
# (httr2::response_json() writes R's NULL as {}.)
json_response <- function(body) {
  httr2::response(status_code = 200L, headers = list(`Content-Type` = "application/json"),
                  body = charToRaw(body))
}

# Anthropic answering a structured call through its tool, with `data` (JSON
# text) as the tool's input.
anthropic_tool_reply <- function(data, stop_reason = "tool_use", output_tokens = 7L) {
  json_response(sprintf(paste0(
    '{"id":"m","type":"message","role":"assistant","model":"claude-test",',
    '"content":[{"type":"tool_use","id":"tu","name":"_structured_tool_call","input":%s}],',
    '"stop_reason":"%s","stop_sequence":null,',
    '"usage":{"input_tokens":10,"output_tokens":%d}}'),
    if (is.null(data)) "{}" else sprintf('{"data":%s}', data), stop_reason, output_tokens))
}

claude_client <- function() {
  gr_register_model("claude-test", context_window = 200000L, max_output = 64000L,
                    input_usd = 3, output_usd = 15)
  gr_ellmer_client(ellmer::chat_anthropic(model = "claude-test", credentials = function() "k"))
}

# --- client-1 / cross-1 / r2-export-serialization-fidelity-02 ---------------

test_that("an ellmer extraction keeps every digit the model sent", {
  skip_without_ellmer_05()
  local_registries()
  httr2::local_mocked_responses(function(req) anthropic_tool_reply(paste0(
    '{"p_value":0.00003,"effect":0.049999,"r":0.123456789012,',
    '"p_value__quote":"The difference was significant (p = 0.00003).",',
    '"effect__quote":"The standardised effect was 0.049999.",',
    '"r__quote":"The correlation was 0.123456789012."}')))
  doc <- tempfile(fileext = ".txt")
  writeLines(c("Methods. We randomised 120 adults.",
               "The difference was significant (p = 0.00003).",
               "The standardised effect was 0.049999.",
               "The correlation was 0.123456789012."), doc)
  ex <- quiet(gr_extract(doc, gr_fields(p_value = gr_field("the p-value", "number"),
                                        effect = gr_field("the effect size", "number"),
                                        r = gr_field("the correlation", "number")),
                         client = claude_client()))
  # jsonlite's default four decimals wrote these as 0, 0.05 and 0.1235, and
  # the quotes then failed to back the rounded values.
  expect_identical(ex$table$status, "ok")
  expect_equal(ex$table$p_value, 3e-05)
  expect_equal(ex$table$effect, 0.049999)
  expect_equal(ex$table$r, 0.123456789012)
  expect_identical(ex$table$n_unverified, 0L)
})

test_that("a schema asked for in the prompt keeps its numbers too", {
  skip_if_not_installed("ellmer")
  local_registries()
  gr_register_model("stub-model", context_window = 128000L, max_output = 4096L,
                    input_usd = 0, output_usd = 0)
  seen <- new.env()
  chat <- local({
    make <- function() {
      self <- new.env(parent = emptyenv())
      self$chat <- function(user, echo = "none") { seen$user <- user; '{"a": 1}' }
      self$chat_structured <- function(user, type = NULL, echo = "none") list(a = 1)
      self$clone <- function(deep = FALSE) make()
      self$set_turns <- function(value) invisible(self)
      self$set_system_prompt <- function(value) invisible(self)
      self$get_model <- function() "stub-model"
      self
    }
    make()
  })
  odd <- list(type = "object", properties = list(
    a = list(anyOf = list(list(type = "number", minimum = 0.00003)))))
  quiet(readgpt:::gr_call_json(gr_ellmer_client(chat), "Question?", schema = odd))
  expect_match(seen$user, "3e-05", fixed = TRUE)
})

# --- client-2 ----------------------------------------------------------------

reconcile_claims <- function() {
  claims <- data.frame(claim = c("Trials find the effect.", "Surveys measure recall differently.",
                                 "No study followed up past a year."),
                       kind = c("finding", "method", "gap"), moderator = NA_character_,
                       scope = NA_character_, stringsAsFactors = FALSE)
  claims$.support <- list(1L, 2L, 3L)
  claims$.contradict <- list(integer(0), integer(0), integer(0))
  claims
}

test_that("an ellmer reconcile saying every claim is distinct keeps every claim", {
  skip_without_ellmer_05()
  local_registries()
  groups <- '{"groups":[[1],[2],[3]]}'
  httr2::local_mocked_responses(function(req) anthropic_tool_reply(groups))
  cl <- claude_client()
  # The reply text is the provider's JSON: an array of one-element arrays. It
  # used to arrive flattened to [1,2,3], which reads as a single group.
  res <- quiet(readgpt:::gr_call_json(cl, "Group these.", schema = readgpt:::.gr_reconcile_schema,
                                      schema_name = "claim_groups", max_output = 800L))
  expect_identical(res$result$text, '{"groups":[[1],[2],[3]]}')

  spec <- gr_read_spec("stuff", model = "claude-test", max_answer_tokens = 800L)
  claims <- reconcile_claims()
  out <- quiet(readgpt:::claims_reconcile(claims, "Does it work?", cl, spec, gr_trace()))
  expect_identical(nrow(out), 3L)
  expect_setequal(out$kind, c("finding", "method", "gap"))
  expect_identical(lengths(out$.support), c(1L, 1L, 1L))

  # A real merge still merges.
  groups <- '{"groups":[[1,3],[2]]}'
  out <- quiet(readgpt:::claims_reconcile(claims, "Does it work?", cl, spec, gr_trace()))
  expect_identical(nrow(out), 2L)
  expect_setequal(unlist(out$.support), 1:3)
})

# --- client-3 ----------------------------------------------------------------

test_that("an ellmer structured reply cut off at the cap is reported as cut off", {
  skip_without_ellmer_05()
  local_registries()
  body <- NULL
  httr2::local_mocked_responses(function(req) body)
  cl <- claude_client()
  gr_register_model("gpt-test", context_window = 200000L, max_output = 64000L,
                    input_usd = 1, output_usd = 1)
  clo <- gr_ellmer_client(ellmer::chat_openai(model = "gpt-test", credentials = function() "k"))
  responses_cut <- json_response(paste0(
    '{"id":"r","object":"response","status":"incomplete",',
    '"incomplete_details":{"reason":"max_output_tokens"},"model":"gpt-test",',
    '"output":[{"type":"message","id":"m","status":"incomplete","role":"assistant",',
    '"content":[{"type":"output_text","text":"{\\"claims\\":[{\\"claim\\":\\"A\\",\\"ki",',
    '"annotations":[]}]}],"usage":{"input_tokens":10,"output_tokens":1600,"total_tokens":1610}}'))
  cases <- list(
    list(cl, anthropic_tool_reply(NULL, "max_tokens", 1600L)),
    list(cl, anthropic_tool_reply('{"claims":[{"claim":"A","kind":"finding"}]}', "max_tokens", 1600L)),
    list(clo, responses_cut))
  for (i in seq_along(cases)) {
    body <- cases[[i]][[2]]
    r <- quiet(readgpt:::gr_call_json(cases[[i]][[1]], "Draw claims.",
                                      schema = readgpt:::.gr_claims_schema,
                                      schema_name = "claims", max_output = 1600L))
    expect_false(r$ok, info = paste("case", i))
    expect_false(r$result$ok, info = paste("case", i))                  # nothing of the reply survives
    expect_identical(r$result$finish_reason, "length", info = paste("case", i))   # was NA
    expect_true(readgpt:::reply_cut_off(r$result), info = paste("case", i))
    expect_match(r$result$error, "truncated", info = paste("case", i))
    # Billed for the cap it hit, not recorded as free.
    expect_identical(as.integer(r$result$usage$output), 1600L, info = paste("case", i))
  }

  # Any other failure is the plain failure it was.
  body <- httr2::response(status_code = 400L, headers = list(`Content-Type` = "application/json"),
                          body = charToRaw(paste0('{"type":"error","error":',
                                                  '{"type":"invalid_request_error","message":"bad"}}')))
  r <- quiet(readgpt:::gr_call_json(cl, "Draw claims.", schema = readgpt:::.gr_claims_schema,
                                    max_output = 1600L))
  expect_false(r$result$ok)
  expect_true(is.na(r$result$finish_reason))
  expect_false(readgpt:::reply_cut_off(r$result))
})

test_that("the stop reason is read from ellmer's truncation errors, styled or not", {
  reason <- function(msg) readgpt:::ellmer_error_finish_reason(list(), simpleError(msg))
  expect_identical(reason("Response was truncated because it hit the `max_tokens` limit."),
                   "max_tokens")
  expect_identical(reason(paste0("\033[1m\033[22mResponse was truncated because it hit the ",
                                 "`max_tokens` limit.\n\033[36mi\033[39m Increase `max_tokens`.")),
                   "max_tokens")
  expect_identical(reason("Response was truncated because it exceeded the model's context window."),
                   "context_window")
  # Anything else is not a cut-off.
  expect_true(is.na(reason("Response was filtered by the provider's content moderation policy.")))
  expect_true(is.na(reason("HTTP 500 Internal Server Error.")))
})

test_that("gr_claims() through ellmer says a cut-off batch was cut off", {
  skip_without_ellmer_05()
  local_registries()
  httr2::local_mocked_responses(function(req) anthropic_tool_reply(NULL, "max_tokens", 1600L))
  tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok", duplicate_of = NA_character_,
                    n_filled = 2L, n_unverified = 0L, conflicts = NA_character_,
                    design = c("randomised trial", "survey"), finding = c("supports", "none"),
                    stringsAsFactors = FALSE)
  said <- character(0)
  cm <- withCallingHandlers(
    gr_claims(tab, question = "Does it work?", client = claude_client()),
    gr_no_claims = function(w) said <<- c(said, conditionMessage(w)),
    warning = function(w) invokeRestart("muffleWarning"))
  expect_length(said, 1L)
  # It said "1 call(s) failed", which gives no hint that the limit is the fix.
  expect_match(said, "cut off at the [0-9]+-token reply limit")
  expect_no_match(said, "call(s) failed", fixed = TRUE)
  step <- Filter(function(s) identical(s$label, "claims.draw"), cm$trace$steps)[[1]]
  expect_identical(step$finish_reason, "length")
})

# --- r-semantics-01 (remaining) ----------------------------------------------

test_that("a nullable enum sent to an OpenAI-family chat can still be null", {
  skip_without_ellmer_05()
  local_registries()
  gr_register_model("gpt-test", context_window = 200000L, max_output = 64000L,
                    input_usd = 1, output_usd = 1)
  seen <- new.env(); seen$bodies <- list()
  reply <- '{"design":null,"design__quote":null}'
  httr2::local_mocked_responses(function(req) {
    seen$bodies[[length(seen$bodies) + 1L]] <- req$body$data
    if (grepl("anthropic", req$url)) return(anthropic_tool_reply(reply))
    json_response(sprintf(paste0(
      '{"id":"r","object":"response","status":"completed","model":"gpt-test",',
      '"output":[{"type":"message","id":"m","status":"completed","role":"assistant",',
      '"content":[{"type":"output_text","text":%s,"annotations":[]}]}],',
      '"usage":{"input_tokens":10,"output_tokens":5,"total_tokens":15}}'),
      jsonlite::toJSON(reply, auto_unbox = TRUE)))
  })
  schema <- readgpt:::fields_schema(gr_fields(
    design = gr_field("design", "enum", values = c("RCT", "quasi"))))
  prop_json <- function(p) as.character(jsonlite::toJSON(p, auto_unbox = TRUE, null = "null"))

  clo <- gr_ellmer_client(ellmer::chat_openai(model = "gpt-test", credentials = function() "k"))
  r <- quiet(readgpt:::gr_call_json(clo, "x", schema = schema, max_output = 200L))
  s <- seen$bodies[[1]]$text$format$schema
  # Strict mode lists every field as required, so null must be among the
  # values or the model has to pick one. ellmer sent ["RCT","quasi"].
  expect_true("design" %in% unlist(s$required))
  expect_match(prop_json(s$properties$design), '"enum":["RCT","quasi",null]', fixed = TRUE)
  expect_match(prop_json(s$properties$design), '"type":["string","null"]', fixed = TRUE)
  expect_true(r$ok)
  expect_null(r$value$design)

  # Anthropic takes an optional field as optional: unchanged.
  seen$bodies <- list()
  quiet(readgpt:::gr_call_json(claude_client(), "x", schema = schema, max_output = 200L))
  a <- seen$bodies[[1]]$tools[[1]]$input_schema$properties$data
  expect_false("design" %in% unlist(a$required))
  expect_match(prop_json(a$properties$design), '"enum":["RCT","quasi"]', fixed = TRUE)
})

test_that("only providers rendered in OpenAI's strict form get null among enum values", {
  skip_if_not_installed("ellmer")
  chat_with <- function(cls) list(get_provider = function() structure(list(), class = cls))
  expect_true(readgpt:::ellmer_strict_nullable(chat_with(
    c("ellmer::ProviderOpenAI", "ellmer::ProviderOpenAICompatible", "ellmer::Provider"))))
  expect_true(readgpt:::ellmer_strict_nullable(chat_with(
    c("ellmer::ProviderAzureOpenAI", "ellmer::ProviderOpenAICompatible", "ellmer::Provider"))))
  # These descend from the OpenAI-compatible provider but list only the
  # required fields, so an optional enum is already free to be absent.
  expect_false(readgpt:::ellmer_strict_nullable(chat_with(
    c("ellmer::ProviderOllama", "ellmer::ProviderOpenAICompatible", "ellmer::Provider"))))
  expect_false(readgpt:::ellmer_strict_nullable(chat_with(
    c("ellmer::ProviderAnthropic", "ellmer::Provider"))))
  expect_false(readgpt:::ellmer_strict_nullable(list()))

  schema <- readgpt:::fields_schema(gr_fields(
    arm = gr_field("arm", "enum", values = c("a", "b"))))
  # Unchanged without the flag, so the other providers see what they saw.
  expect_identical(readgpt:::ellmer_type(schema)@properties$arm@values, c("a", "b"))
  strict <- readgpt:::ellmer_type(schema, null_in_enum = TRUE)@properties$arm
  expect_true(strict@required)                           # not widened a second time
  expect_identical(strict@json$enum, list("a", "b", NULL))
  # A required enum is left as it was.
  req <- readgpt:::ellmer_type(readgpt:::.gr_screen_schema, null_in_enum = TRUE)
  expect_identical(req@properties$decision@values, c("include", "exclude", "unclear"))
})

# --- cache-3 (registry part) -------------------------------------------------

test_that("text-embedding-ada-002 is priced, so its embedding steps have a cost", {
  local_registries()
  expect_no_warning(info <- gr_model_info("text-embedding-ada-002"))
  expect_true(info$certain)
  expect_identical(info$kind, "embedding")
  expect_identical(as.integer(info$dimensions), 1536L)
  expect_equal(gr_estimate_cost("text-embedding-ada-002", 1e6), 0.10)

  tr <- gr_trace()
  readgpt:::trace_record(tr, "embed.request", list(list(role = "user", content = "x")),
                         gr_result(TRUE, text = "", model = "text-embedding-ada-002",
                                   usage = list(input = 5000L, output = 0L)))
  cost <- gr_trace_cost(tr)
  expect_false(anyNA(cost$usd))
  expect_equal(sum(cost$usd), 5000 / 1e6 * 0.10)
})
