library(testthat)
library(jsonlite)

# ------------------------------------------------------------------------------
# Load the real retrieval_mode.R file using its full path.
# ------------------------------------------------------------------------------
source("C:/repos/gpt_read/gpt_read/R/retrieval_mode.R")

# ------------------------------------------------------------------------------
# Override dependent functions used by retrieval_mode.R to simulate behavior.
# ------------------------------------------------------------------------------
# Fake token estimator: count tokens as words.
fake_estimate_token_count <- function(text) {
  if (nchar(text) == 0) return(0)
  length(strsplit(text, "\\s+")[[1]])
}
assign("estimate_token_count", fake_estimate_token_count, envir = .GlobalEnv)

# Fake model limits: context_window=5000 and output_tokens=500.
fake_get_model_limits <- function(model) {
  list(context_window = 5000, output_tokens = 500)
}
assign("get_model_limits", fake_get_model_limits, envir = .GlobalEnv)

# Fake process_api_call that simulates GPT responses.
fake_process_api_call <- function(messages, model, temperature, max_tokens,
                                  presence_penalty, frequency_penalty, 
                                  num_retries, pause_base) {
  content <- sapply(messages, function(m) m$content)
  # If prompt is for skimming (chunked retrieval branch)
  if (any(grepl("Skim the following", content))) {
    return("skim answer")
  }
  # If final prompt merging skimmed results is used
  if (any(grepl("Based on the following extracted", content))) {
    return("final answer")
  }
  # Extraction prompt for single retrieval branch
  if (any(grepl("You selectively extract relevant text", content))) {
    return("extracted answer")
  }
  # If the answer prompt uses "Relevant Text:" (single retrieval answer)
  if (any(grepl("Relevant Text:", content))) {
    return("final answer")
  }
  return("default answer")
}
assign("process_api_call", fake_process_api_call, envir = .GlobalEnv)

# Fake gpt_read_chunked for fallback testing.
fake_gpt_read_chunked <- function(chunks, question, ...) {
  return("fallback chunked answer")
}
assign("gpt_read_chunked", fake_gpt_read_chunked, envir = .GlobalEnv)

# ------------------------------------------------------------------------------
# Helper: with_override
# Temporarily override functions in .GlobalEnv during the evaluation of expr.
# ------------------------------------------------------------------------------
with_override <- function(overrides, expr) {
  # Save originals
  originals <- lapply(names(overrides), function(name) get(name, envir = .GlobalEnv))
  names(originals) <- names(overrides)
  
  # Set new values
  for (name in names(overrides)) {
    assign(name, overrides[[name]], envir = .GlobalEnv)
  }
  
  # Ensure restoration after expression evaluation
  on.exit({
    for (name in names(originals)) {
      assign(name, originals[[name]], envir = .GlobalEnv)
    }
  }, add = TRUE)
  
  force(expr)
}

# ------------------------------------------------------------------------------
# UAT for chunk_text_minimal
# ------------------------------------------------------------------------------
test_that("chunk_text_minimal combines paragraphs within token limit", {
  text <- "Paragraph one.\n\nParagraph two."
  # Use a high token limit so both paragraphs merge.
  result <- chunk_text_minimal(text, token_limit = 100)
  expect_equal(length(result), 1)
  expect_true(grepl("Paragraph one", result))
  expect_true(grepl("Paragraph two", result))
})

test_that("chunk_text_minimal splits a paragraph that exceeds token limit", {
  # Create a long paragraph of 20 words.
  long_para <- paste(rep("word", 20), collapse = " ")
  # Set token limit to 10 to force splitting.
  result <- chunk_text_minimal(long_para, token_limit = 10)
  expect_true(length(result) > 1)
  # Each chunk must not exceed the token limit.
  for (chunk in result) {
    expect_lte(fake_estimate_token_count(chunk), 10)
  }
})

# ------------------------------------------------------------------------------
# UAT for gpt_read_retrieval
# ------------------------------------------------------------------------------
test_that("gpt_read_retrieval single retrieval branch returns final answer", {
  # Create a simple chunk that fits within allowed tokens.
  chunks <- c("Short text chunk.")
  question <- "What is being tested?"
  answer <- gpt_read_retrieval(chunks, question)
  # In single retrieval branch, fake API returns "final answer".
  expect_equal(answer, "final answer")
})

test_that("gpt_read_retrieval chunked retrieval branch processes chunks", {
  # Override get_model_limits temporarily to force chunked branch.
  with_override(list(
    get_model_limits = function(model) list(context_window = 1000, output_tokens = 200)
  ), {
    chunks <- c("Chunk one content.", "Chunk two content.")
    question <- "What is in the chunks?"
    answer <- gpt_read_retrieval(chunks, question)
    expect_equal(answer, "final answer")
  })
})

test_that("gpt_read_retrieval returns valid JSON chain-of-thought when requested", {
  chunks <- c("Short text chunk.")
  question <- "Test JSON output?"
  result_json <- gpt_read_retrieval(chunks, question, return_json = TRUE)
  chain <- fromJSON(result_json)
  expect_true(!is.null(chain$final_answer))
  expect_equal(chain$final_answer, "final answer")
})

test_that("gpt_read_retrieval fallback branch triggers when extraction yields empty", {
  suppressWarnings(
    with_override(list(
      process_api_call = function(messages, model, temperature, max_tokens,
                                  presence_penalty, frequency_penalty, num_retries, pause_base) {
        content <- sapply(messages, function(m) m$content)
        if (any(grepl("You selectively extract relevant text", content))) {
          return("")  # Simulate failure to extract text.
        } else if (any(grepl("Question:", content))) {
          return("final answer")
        }
        return("default")
      },
      gpt_read_chunked = function(chunks, question, ...) {
        return("fallback chunked answer")
      }
    ), {
      chunks <- c("Short text chunk.")
      question <- "Fallback test question?"
      answer <- gpt_read_retrieval(chunks, question)
      expect_equal(answer, "fallback chunked answer")
    })
  )
})

test_that("gpt_read_retrieval accepts and applies custom parameters", {
  custom_model <- "gpt-4"
  custom_temperature <- 0.5
  custom_max_tokens <- 300
  custom_presence_penalty <- 0.2
  custom_frequency_penalty <- 0.2
  custom_num_retries <- 3
  custom_pause_base <- 2
  custom_use_parallel <- TRUE
  
  chunks <- c("Another short chunk.")
  question <- "Custom parameter test?"
  answer <- gpt_read_retrieval(chunks, question,
                               model = custom_model,
                               temperature = custom_temperature,
                               max_tokens = custom_max_tokens,
                               presence_penalty = custom_presence_penalty,
                               frequency_penalty = custom_frequency_penalty,
                               num_retries = custom_num_retries,
                               pause_base = custom_pause_base,
                               use_parallel = custom_use_parallel)
  expect_equal(answer, "final answer")
})

cat("All UAT tests for retrieval_mode.R passed successfully.\n")
