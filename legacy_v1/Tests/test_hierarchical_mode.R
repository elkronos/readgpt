# File: test-hierarchical_mode.R

library(testthat)
library(jsonlite)

# Source the main scripts (adjust the relative paths as needed)
source("../R/hierarchical_mode.R")
source("../R/utils.R")
source("../R/text_processing.R")

# -----------------------------------------------------------------------------
# Setup: Create fake implementations to override external calls.
# -----------------------------------------------------------------------------

# A fake process_api_call that returns a simulated summary or final answer.
fake_process_api_call <- function(messages, model = "gpt-3.5-turbo", temperature = 0.0, 
                                  max_tokens = 512, presence_penalty = 0.0, frequency_penalty = 0.0, 
                                  num_retries = 5, pause_base = 3) {
  # Check whether we are in the summarization phase or final answer phase.
  if (grepl("Summarize the following", messages[[2]]$content)) {
    return("summary of chunk")
  } else if (grepl("Based on the following summaries", messages[[2]]$content)) {
    return("final answer")
  }
  return("unknown response")
}

# A fake process_api_call that always returns an empty string (to trigger fallback).
fake_process_api_call_empty <- function(messages, model = "gpt-3.5-turbo", temperature = 0.0, 
                                        max_tokens = 512, presence_penalty = 0.0, frequency_penalty = 0.0, 
                                        num_retries = 5, pause_base = 3) {
  return("")
}

# A fake gpt_read_chunked to simulate the fallback mechanism.
fake_gpt_read_chunked <- function(chunks, question, model = "gpt-3.5-turbo", temperature = 0.0, 
                                  max_tokens = 1024, use_parallel = FALSE) {
  return("fallback answer")
}

# Save original functions to restore after tests.
orig_process_api_call <- process_api_call
orig_gpt_read_chunked <- if (exists("gpt_read_chunked")) gpt_read_chunked else NULL

# Utility functions to set and later restore our fake functions.
set_fake_functions <- function(fake_api, fake_chunked) {
  assign("process_api_call", fake_api, envir = .GlobalEnv)
  if (!is.null(fake_chunked)) {
    assign("gpt_read_chunked", fake_chunked, envir = .GlobalEnv)
  }
}

restore_original_functions <- function() {
  assign("process_api_call", orig_process_api_call, envir = .GlobalEnv)
  if (!is.null(orig_gpt_read_chunked)) {
    assign("gpt_read_chunked", orig_gpt_read_chunked, envir = .GlobalEnv)
  }
}

# -----------------------------------------------------------------------------
# Test Cases
# -----------------------------------------------------------------------------

test_that("Error when question is NULL or empty", {
  expect_error(gpt_read_hierarchical(chunks = c("chunk1", "chunk2"), question = NULL),
               "Question must be provided for hierarchical mode.")
  expect_error(gpt_read_hierarchical(chunks = c("chunk1", "chunk2"), question = "   "),
               "Question must be provided for hierarchical mode.")
})

test_that("Returns final answer using fake API call", {
  # Override external calls with our fake implementations.
  set_fake_functions(fake_process_api_call, fake_gpt_read_chunked)
  
  chunks <- c("This is chunk one.", "This is chunk two.")
  question <- "What is the summary?"
  # Custom non-default parameters.
  model <- "test-model"
  temperature <- 0.5
  summary_max_tokens <- 100
  answer_max_tokens <- 150
  use_parallel <- FALSE
  num_retries <- 3
  pause_base <- 2
  return_json <- FALSE
  
  result <- gpt_read_hierarchical(chunks = chunks, question = question, model = model,
                                  temperature = temperature,
                                  summary_max_tokens = summary_max_tokens, 
                                  answer_max_tokens = answer_max_tokens,
                                  use_parallel = use_parallel, num_retries = num_retries, 
                                  pause_base = pause_base,
                                  return_json = return_json)
  
  expect_type(result, "character")
  expect_equal(result, "final answer")
  
  restore_original_functions()
})

test_that("Returns JSON chain-of-thought when return_json is TRUE", {
  set_fake_functions(fake_process_api_call, fake_gpt_read_chunked)
  
  chunks <- c("Chunk one text.", "Chunk two text.")
  question <- "Detail explanation?"
  
  result <- gpt_read_hierarchical(chunks = chunks, question = question, return_json = TRUE)
  
  chain <- fromJSON(result)
  
  expect_equal(chain$question, question)
  expect_equal(chain$final_answer, "final answer")
  expect_equal(chain$parameters$model, "gpt-3.5-turbo")  # default model
  expect_true(!is.null(chain$chunk_summaries))
  expect_true(!is.null(chain$combined_summary))
  
  restore_original_functions()
})

test_that("Fallback mechanism works when summaries are empty", {
  # Use the fake API call that returns empty summaries.
  set_fake_functions(fake_process_api_call_empty, fake_gpt_read_chunked)
  
  chunks <- c("Chunk one.", "Chunk two.")
  question <- "Fallback test question?"
  
  result <- expect_warning(
    gpt_read_hierarchical(chunks = chunks, question = question, return_json = FALSE),
    "No relevant content found in summaries; falling back to direct chunked answering."
  )
  
  expect_equal(result, "fallback answer")
  
  restore_original_functions()
})

test_that("Processes use_parallel parameter correctly", {
  set_fake_functions(fake_process_api_call, fake_gpt_read_chunked)
  
  chunks <- c("Parallel chunk one.", "Parallel chunk two.")
  question <- "Parallel processing test?"
  
  result <- gpt_read_hierarchical(chunks = chunks, question = question, use_parallel = TRUE)
  
  expect_equal(result, "final answer")
  
  restore_original_functions()
})

test_that("Custom parameters are included in JSON output", {
  set_fake_functions(fake_process_api_call, fake_gpt_read_chunked)
  
  chunks <- c("Custom chunk one.", "Custom chunk two.")
  question <- "Custom test question?"
  custom_model <- "custom-model"
  custom_temperature <- 0.7
  custom_summary_max_tokens <- 200
  custom_answer_max_tokens <- 250
  custom_use_parallel <- TRUE
  custom_num_retries <- 2
  custom_pause_base <- 1
  
  result <- gpt_read_hierarchical(chunks = chunks, question = question, model = custom_model, 
                                  temperature = custom_temperature,
                                  summary_max_tokens = custom_summary_max_tokens, 
                                  answer_max_tokens = custom_answer_max_tokens,
                                  use_parallel = custom_use_parallel, 
                                  num_retries = custom_num_retries, 
                                  pause_base = custom_pause_base,
                                  return_json = TRUE)
  chain <- fromJSON(result)
  
  expect_equal(chain$parameters$model, custom_model)
  expect_equal(chain$parameters$temperature, custom_temperature)
  expect_equal(chain$parameters$summary_max_tokens, custom_summary_max_tokens)
  expect_equal(chain$parameters$answer_max_tokens, custom_answer_max_tokens)
  expect_equal(chain$parameters$use_parallel, custom_use_parallel)
  expect_equal(chain$parameters$num_retries, custom_num_retries)
  expect_equal(chain$parameters$pause_base, custom_pause_base)
  
  restore_original_functions()
})
