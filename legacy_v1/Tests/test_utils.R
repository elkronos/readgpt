#!/usr/bin/env Rscript

library(testthat)
library(stringr)
library(jsonlite)
library(future.apply)

# Source the real implementations from R/utils.R
source(file.path("..", "R", "utils.R"))

context("Testing utils.R functions")

test_that("estimate_token_count returns correct token count", {
  # Test empty string returns 0
  expect_equal(estimate_token_count(""), 0)
  
  # Test single word returns 1
  expect_equal(estimate_token_count("hello"), 1)
  
  # Test multiple words return correct count
  text <- "This is a test string."
  expected_count <- length(unlist(strsplit(text, "\\s+")))
  expect_equal(estimate_token_count(text), expected_count)
})

test_that("get_model_limits returns correct values for known models", {
  limits1 <- get_model_limits("gpt-3.5-turbo")
  expect_true(is.list(limits1))
  expect_true("context_window" %in% names(limits1))
  expect_true("output_tokens" %in% names(limits1))
  
  limits2 <- get_model_limits("gpt-3.5-turbo-1106")
  expect_equal(limits2$context_window, 16385)
  expect_equal(limits2$output_tokens, 4096)
  
  limits3 <- get_model_limits("gpt-4.5-preview-2025-02-27")
  expect_equal(limits3$context_window, 128000)
  expect_equal(limits3$output_tokens, 16384)
  
  limits4 <- get_model_limits("unknown-model")
  expect_equal(limits4$context_window, 4096)
  expect_equal(limits4$output_tokens, 4096)
})

test_that("get_api_key returns API key if set and errors if not", {
  # Backup original key
  old_key <- Sys.getenv("OPENAI_API_KEY")
  
  # When key is set, should return that key.
  Sys.setenv(OPENAI_API_KEY = "dummy_key")
  expect_equal(get_api_key(), "dummy_key")
  
  # When not set, should throw an error.
  Sys.setenv(OPENAI_API_KEY = "")
  expect_error(get_api_key(), "OpenAI API key not found")
  
  # Restore original key
  Sys.setenv(OPENAI_API_KEY = old_key)
})

test_that("process_api_call returns expected response with custom parameters", {
  # Create a dummy response object.
  dummy_response <- list(choices = list(list(message = list(content = "Test API response"))))
  
  # Create fake replacements for RETRY, content, and stop_for_status.
  fake_retry <- function(..., times, pause_base) { dummy_response }
  fake_content <- function(response, as = "parsed", simplifyVector = TRUE) { dummy_response }
  fake_stop_for_status <- function(response, task) { response }  # Override to do nothing
  
  with_mock(
    "httr::RETRY" = fake_retry,
    "httr::content" = fake_content,
    "httr::stop_for_status" = fake_stop_for_status,
    {
      msgs <- list(
        list(role = "system", content = "Test system message"),
        list(role = "user", content = "Test user message")
      )
      result <- process_api_call(
        messages = msgs, 
        model = "gpt-4", 
        temperature = 0.5, 
        max_tokens = 100,
        presence_penalty = 0.2, 
        frequency_penalty = 0.3, 
        num_retries = 3, 
        pause_base = 2
      )
      expect_equal(result, "Test API response")
    }
  )
})

test_that("log_query writes log entries to the specified file", {
  tmp_log <- tempfile()
  on.exit(unlink(tmp_log))
  
  question <- "What is the capital of France?"
  answer <- "Paris"
  log_query(question, answer, log_file = tmp_log)
  
  log_contents <- readLines(tmp_log)
  expect_true(any(grepl("Q: What is the capital of France?", log_contents)))
  expect_true(any(grepl("A: Paris", log_contents)))
})

test_that("compute_embedding returns a numeric vector of length 768", {
  emb <- compute_embedding("Test text for embedding")
  expect_true(is.numeric(emb))
  expect_equal(length(emb), 768)
})

test_that("cosine_similarity computes correct similarity", {
  vec1 <- c(1, 0)
  vec2 <- c(1, 0)
  expect_equal(cosine_similarity(vec1, vec2), 1)
  
  vec3 <- c(0, 1)
  expect_equal(cosine_similarity(vec1, vec3), 0)
})

test_that("sort_chunks_by_semantic sorts chunks by similarity to query", {
  chunks <- c("The cat sat on the mat.", "Dogs are great pets.", "I love my pet cat.")
  query <- "cat"
  sorted <- sort_chunks_by_semantic(chunks, query)
  # We expect that chunks containing "cat" appear first.
  expect_true(grepl("cat", sorted[1], ignore.case = TRUE))
  expect_true(grepl("cat", sorted[2], ignore.case = TRUE))
})
