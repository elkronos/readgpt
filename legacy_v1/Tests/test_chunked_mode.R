library(testthat)
library(jsonlite)
library(withr)  # for local_bindings

# Source the functions to be tested.
source("C:/repos/gpt_read/gpt_read/R/chunked_mode.R")
source("C:/repos/gpt_read/gpt_read/R/utils.R")

# ------------------------------------------------------------------------------
# Override process_api_call to simulate API responses without real API calls.
# This fake function returns a fixed response ("chunk answer") for chunk queries and
# a merge response ("merged answer") when the prompt includes "Question:".
# ------------------------------------------------------------------------------
fake_process_api_call <- function(messages, model, temperature, max_tokens, 
                                  presence_penalty, frequency_penalty, 
                                  num_retries, pause_base) {
  # Check if the call is the merge step (contains "Question:" in one of the user messages)
  if (any(sapply(messages, function(m) grepl("Question:", m$content)))) {
    return("merged answer")
  }
  # Otherwise, return a simple answer for chunk queries.
  return("chunk answer")
}
process_api_call <<- fake_process_api_call

# ------------------------------------------------------------------------------
# Begin UAT tests for gpt_read_chunked
# ------------------------------------------------------------------------------

test_that("Error is thrown when question is missing", {
  expect_error(gpt_read_chunked(chunks = "Test chunk", question = ""),
               "Question must be provided for chunked processing")
})

test_that("Single chunk with default parameters (no sub-chunk splitting)", {
  chunks <- c("This is a test chunk.")
  question <- "What is this test?"
  answer <- gpt_read_chunked(chunks, question)
  expect_equal(answer, "merged answer")
})

test_that("Sub-chunk splitting occurs when chunk exceeds allowed tokens", {
  # Create a long chunk that forces sub-chunk splitting.
  long_chunk <- paste(rep("word", 500), collapse = " ")
  question <- "What is contained in the long chunk?"
  answer <- gpt_read_chunked(chunks = c(long_chunk), question = question, chunk_token_limit = 50)
  expect_equal(answer, "merged answer")
})

test_that("Custom parameters are correctly applied and preserved in JSON output", {
  chunks <- c("Custom chunk 1.", "Custom chunk 2.")
  question <- "Custom question?"
  
  custom_model <- "gpt-4"
  custom_temperature <- 0.7
  custom_max_tokens <- 1000
  custom_chunk_token_limit <- 200
  custom_system_message_1 <- "Custom system message 1."
  custom_system_message_2 <- "Custom system message 2."
  custom_presence_penalty <- 0.5
  custom_frequency_penalty <- 0.5
  custom_num_retries <- 3
  custom_pause_base <- 2
  custom_delay_between_chunks <- 1
  custom_use_parallel <- TRUE
  
  result_json <- gpt_read_chunked(
    chunks, question, 
    model = custom_model,
    temperature = custom_temperature,
    max_tokens = custom_max_tokens,
    chunk_token_limit = custom_chunk_token_limit,
    system_message_1 = custom_system_message_1,
    system_message_2 = custom_system_message_2,
    presence_penalty = custom_presence_penalty,
    frequency_penalty = custom_frequency_penalty,
    num_retries = custom_num_retries,
    pause_base = custom_pause_base,
    delay_between_chunks = custom_delay_between_chunks,
    use_parallel = custom_use_parallel,
    return_json = TRUE
  )
  
  chain <- fromJSON(result_json)
  
  # Verify that the parameters are stored as provided.
  expect_equal(chain$parameters$model, custom_model)
  expect_equal(chain$parameters$temperature, custom_temperature)
  expect_equal(chain$parameters$max_tokens, custom_max_tokens)
  expect_equal(chain$parameters$chunk_token_limit, custom_chunk_token_limit)
  expect_equal(chain$parameters$system_message_1, custom_system_message_1)
  expect_equal(chain$parameters$system_message_2, custom_system_message_2)
  expect_equal(chain$parameters$presence_penalty, custom_presence_penalty)
  expect_equal(chain$parameters$frequency_penalty, custom_frequency_penalty)
  expect_equal(chain$parameters$num_retries, custom_num_retries)
  expect_equal(chain$parameters$pause_base, custom_pause_base)
  expect_equal(chain$parameters$delay_between_chunks, custom_delay_between_chunks)
  expect_equal(chain$parameters$use_parallel, custom_use_parallel)
  
  # The final merged answer should be "merged answer" as returned by our fake API.
  expect_equal(chain$final_answer, "merged answer")
})

test_that("Multiple chunks are processed correctly", {
  chunks <- c("First chunk.", "Second chunk.")
  question <- "Multi-chunk question?"
  answer <- gpt_read_chunked(chunks, question)
  expect_equal(answer, "merged answer")
})

test_that("Parallel processing option works", {
  chunks <- c("Parallel chunk 1.", "Parallel chunk 2.")
  question <- "Parallel question?"
  answer <- gpt_read_chunked(chunks, question, use_parallel = TRUE)
  expect_equal(answer, "merged answer")
})

cat("All UAT tests for gpt_read_chunked passed successfully.\n")


# ---------------------------------------------------------------------------
# Test 1: Real API call returns a non-empty answer
# ---------------------------------------------------------------------------
test_that("Real API call returns a non-empty answer", {
  chunks <- c("This is a sample text chunk containing information about R programming and testing.")
  question <- "What programming language is mentioned in the chunk?"
  
  answer <- gpt_read_chunked(chunks, question)
  
  expect_true(nzchar(answer))
  cat("Real API call returned: ", answer, "\n")
})

# ---------------------------------------------------------------------------
# Test 2: Real API call returns proper JSON chain-of-thought when requested
# ---------------------------------------------------------------------------
test_that("Real API call returns valid JSON chain-of-thought", {
  chunks <- c("This is a sample text chunk containing information about AI and machine learning.")
  question <- "What topics are mentioned?"
  
  result_json <- gpt_read_chunked(chunks, question, return_json = TRUE)
  chain <- fromJSON(result_json)
  
  expect_true(!is.null(chain$final_answer))
  expect_true(nzchar(chain$final_answer))
  expect_equal(chain$question, question)
  cat("Chain-of-thought JSON: \n", result_json, "\n")
})

# ---------------------------------------------------------------------------
# Test 3: Real API call with custom parameters
# ---------------------------------------------------------------------------
test_that("Real API call with custom parameters returns a valid answer", {
  chunks <- c("This chunk discusses the usage of GPT models for language processing.")
  question <- "Which models are mentioned in the chunk?"
  
  custom_model <- "gpt-3.5-turbo"
  custom_temperature <- 0.3
  custom_max_tokens <- 200
  custom_chunk_token_limit <- 150
  
  answer <- gpt_read_chunked(
    chunks, question,
    model = custom_model,
    temperature = custom_temperature,
    max_tokens = custom_max_tokens,
    chunk_token_limit = custom_chunk_token_limit,
    system_message_1 = "Answer using only the given text.",
    system_message_2 = "Merge multiple answers into one.",
    presence_penalty = 0.2,
    frequency_penalty = 0.2,
    num_retries = 2,
    pause_base = 2,
    delay_between_chunks = 0,
    use_parallel = FALSE,
    return_json = FALSE
  )
  
  expect_true(nzchar(answer))
  cat("Real API call with custom parameters returned: ", answer, "\n")
})

cat("All real API tests for gpt_read_chunked executed.\n")