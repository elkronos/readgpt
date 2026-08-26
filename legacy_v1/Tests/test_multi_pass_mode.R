library(testthat)
library(jsonlite)
library(withr)  # for local_bindings if needed

# ------------------------------------------------------------------------------
# Set a dummy API key so that get_api_key() succeeds.
# ------------------------------------------------------------------------------
Sys.setenv(OPENAI_API_KEY = "dummy")

# ------------------------------------------------------------------------------
# Source production files.
# Adjust these paths if necessary.
# ------------------------------------------------------------------------------
source("C:/repos/gpt_read/gpt_read/R/multi_pass_mode.R")
source("C:/repos/gpt_read/gpt_read/R/utils.R")
source("C:/repos/gpt_read/gpt_read/R/text_processing.R")

# ------------------------------------------------------------------------------
# Override helper functions AFTER sourcing production files.
# Use assign() in the global environment so that production functions look them up.
# ------------------------------------------------------------------------------

# Fake search_text: Returns dummy snippets for each keyword.
fake_search_text <- function(chunks, keywords, window_chars = 300) {
  sapply(keywords, function(kw) {
    paste("Dummy snippet for", kw, "from chunk:", 
          if (length(chunks) > 0) substr(chunks[1], 1, min(50, nchar(chunks[1]))) else "No chunk")
  })
}
assign("search_text", fake_search_text, envir = .GlobalEnv)

# Fake process_api_call:
# - If any message contains "refine" (case-insensitive), return a fake refined answer.
# - If any message contains "Merge these answers", return "merged answer".
fake_process_api_call <- function(messages, model, temperature, max_tokens,
                                  presence_penalty, frequency_penalty,
                                  num_retries, pause_base) {
  if (any(sapply(messages, function(m) grepl("refine", m$content, ignore.case = TRUE)))) {
    return("fake refined answer")
  }
  if (any(sapply(messages, function(m) grepl("Merge these answers", m$content, ignore.case = TRUE)))) {
    return("merged answer")
  }
  return("default fake answer")
}
assign("process_api_call", fake_process_api_call, envir = .GlobalEnv)

# Fake gpt_read_retrieval: Returns a fixed retrieval answer unless signaled to be empty.
fake_gpt_read_retrieval <- function(chunks, question, model = "gpt-3.5-turbo", use_parallel = FALSE, fallback = FALSE, ...) {
  if (grepl("empty_retrieval", question)) return("")
  return("fake retrieval answer")
}
assign("gpt_read_retrieval", fake_gpt_read_retrieval, envir = .GlobalEnv)

# Fake gpt_read_chunked: Returns a fixed chunked answer unless signaled to be empty.
fake_gpt_read_chunked <- function(chunks, question, model = "gpt-3.5-turbo", use_parallel = FALSE, ...) {
  if (grepl("empty_chunked", question)) return("")
  return("fake chunked answer")
}
assign("gpt_read_chunked", fake_gpt_read_chunked, envir = .GlobalEnv)

# ------------------------------------------------------------------------------
# Dummy test data.
# ------------------------------------------------------------------------------
dummy_chunks <- c("Test chunk content with keyword example.")
dummy_question <- "What is the test outcome?"
dummy_current_answer <- "initial answer"

# ------------------------------------------------------------------------------
# Begin UAT tests for multi_pass_mode functions.
# ------------------------------------------------------------------------------

test_that("refine_answer returns a refined answer string when return_json = FALSE", {
  result <- refine_answer(dummy_chunks, dummy_question, dummy_current_answer, return_json = FALSE)
  expect_true(is.character(result))
  # Expect our fake_process_api_call to trigger the refine branch.
  expect_equal(result, "fake refined answer")
})

test_that("refine_answer returns valid JSON chain-of-thought when return_json = TRUE", {
  result_json <- refine_answer(dummy_chunks, dummy_question, dummy_current_answer, return_json = TRUE)
  chain <- fromJSON(result_json)
  expect_true(is.list(chain))
  expect_equal(chain$phase, "refinement")
  expect_equal(chain$question, dummy_question)
  expect_equal(chain$current_answer, dummy_current_answer)
  expect_equal(chain$final_refined, "fake refined answer")
})

test_that("refine_answer accepts custom parameters", {
  result <- refine_answer(dummy_chunks, dummy_question, dummy_current_answer,
                          model = "gpt-4",
                          max_tokens = 512,
                          num_retries = 2,
                          pause_base = 1,
                          return_json = FALSE)
  expect_equal(result, "fake refined answer")
})

test_that("gpt_read_multipass returns merged answer when both retrieval and chunked answers are available", {
  result <- gpt_read_multipass(dummy_chunks, dummy_question, model = "gpt-3.5-turbo",
                               use_parallel = FALSE, return_json = FALSE)
  # With non-empty fake retrieval ("fake retrieval answer") and fake chunked ("fake chunked answer"),
  # the merging branch should call process_api_call and get "merged answer".
  expect_equal(result, "merged answer")
})

test_that("gpt_read_multipass returns valid JSON when return_json = TRUE", {
  result_json <- gpt_read_multipass(dummy_chunks, dummy_question, model = "gpt-3.5-turbo",
                                    use_parallel = TRUE, return_json = TRUE)
  chain <- fromJSON(result_json)
  expect_equal(chain$phase, "multi_pass")
  expect_equal(chain$question, dummy_question)
  expect_true("final_answer" %in% names(chain))
  expect_equal(chain$final_answer, "merged answer")
})

test_that("gpt_read_multipass falls back to chunked answer when retrieval answer is empty", {
  result <- gpt_read_multipass(dummy_chunks, "empty_retrieval test", model = "gpt-3.5-turbo",
                               use_parallel = FALSE, return_json = FALSE)
  # When retrieval is empty, the function should use the chunked answer.
  expect_equal(result, "fake chunked answer")
})

test_that("gpt_read_multipass falls back to retrieval answer when chunked answer is empty", {
  result <- gpt_read_multipass(dummy_chunks, "empty_chunked test", model = "gpt-3.5-turbo",
                               use_parallel = FALSE, return_json = FALSE)
  # When chunked is empty, the function should use the retrieval answer.
  expect_equal(result, "fake retrieval answer")
})

test_that("gpt_read_multipass concatenates answers when both retrieval and chunked are empty", {
  result <- gpt_read_multipass(dummy_chunks, "empty_retrieval empty_chunked test", model = "gpt-3.5-turbo",
                               use_parallel = FALSE, return_json = FALSE)
  # When both are empty, fallback concatenation should return an empty string.
  expect_equal(result, "")
})

test_that("gpt_read_multipass accepts custom parameters", {
  result <- gpt_read_multipass(dummy_chunks, dummy_question,
                               model = "gpt-4.5-preview-2025-02-27",
                               use_parallel = TRUE,
                               return_json = FALSE,
                               temperature = 0.2)
  # With non-empty retrieval and chunked answers, merging occurs.
  expect_equal(result, "merged answer")
})

cat("All UAT tests for multi_pass_mode functions passed successfully.\n")
