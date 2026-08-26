library(testthat)
library(jsonlite)

# Source the functions to be tested.
source("C:/repos/gpt_read/gpt_read/R/hierarchical_mode.R")
source("C:/repos/gpt_read/gpt_read/R/utils.R")

test_that("Real API call returns a substantive answer in hierarchical mode", {
  # A realistic chunk of text.
  chunks <- c("This excerpt explains the differences between GPT models, describing gpt-3.5-turbo as efficient for many language tasks and gpt-4 for more complex scenarios. The text further elaborates on technical improvements and usage contexts for each model.")
  
  question <- "Which GPT models are mentioned, and what are the key differences between them?"
  
  # Call the hierarchical mode function with real API calls.
  answer <- gpt_read_hierarchical(chunks, question)
  
  # The answer should be non-empty and not a placeholder.
  expect_true(nchar(trimws(answer)) > 0,
              info = "The API should return a substantive answer, not an empty string.")
  expect_false(tolower(trimws(answer)) %in% c("final answer", "merged answer"),
               info = "The API should return a substantive answer, not a placeholder.")
  
  cat("Real API call returned: ", answer, "\n")
})

library(testthat)
library(jsonlite)

# Source the functions to be tested.
source("C:/repos/gpt_read/gpt_read/R/hierarchical_mode.R")
source("C:/repos/gpt_read/gpt_read/R/utils.R")

test_that("Real API call returns JSON output in hierarchical mode", {
  # A realistic chunk of text.
  chunks <- c("This excerpt explains the differences between GPT models, describing gpt-3.5-turbo as efficient for many language tasks and gpt-4 for more complex scenarios. The text further elaborates on technical improvements and usage contexts for each model.")
  
  question <- "Which GPT models are mentioned, and what are the key differences between them?"
  
  # Call the hierarchical mode function with real API calls and JSON output.
  json_output <- gpt_read_hierarchical(chunks, question, return_json = TRUE)
  
  # Parse the JSON output.
  chain <- fromJSON(json_output)
  
  # Check that the JSON output contains the expected keys.
  expect_true("question" %in% names(chain))
  expect_true("final_answer" %in% names(chain))
  expect_true("parameters" %in% names(chain))
  expect_true("chunk_summaries" %in% names(chain))
  expect_true("combined_summary" %in% names(chain))
  
  # The final answer should be substantive.
  expect_true(nchar(trimws(chain$final_answer)) > 0,
              info = "The API should return a substantive final answer in the JSON output.")
  
  # Print the JSON output for inspection.
  cat("Real API call JSON output:\n", json_output, "\n")
})

