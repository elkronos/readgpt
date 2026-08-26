library(testthat)
library(jsonlite)

# Source the functions to be tested.
source("C:/repos/gpt_read/gpt_read/R/chunked_mode.R")
source("C:/repos/gpt_read/gpt_read/R/utils.R")

test_that("Real API call returns a real answer and prints JSON chain-of-thought", {
  chunks <- c("This chunk discusses the usage of GPT models for language processing. It explicitly mentions gpt-3.5-turbo and gpt-4 as examples of current models.")
  question <- "Which models are mentioned in the chunk?"
  
  # Call the API and request JSON chain-of-thought.
  result_json <- gpt_read_chunked(chunks, question, return_json = TRUE)
  
  # Print the JSON output.
  cat("Real API JSON chain-of-thought:\n", result_json, "\n")
  
  # Parse the JSON to extract the final answer.
  chain <- fromJSON(result_json)
  answer <- chain$final_answer
  
  # Check that the returned answer is not the placeholder text "merged answer".
  expect_false(tolower(trimws(answer)) == "merged answer",
               info = "The API should return a substantive answer, not the literal placeholder 'merged answer'.")
  
  # Optionally, ensure the answer has a minimum length.
  expect_true(nchar(trimws(answer)) > 20,
              info = "The API answer should be of reasonable length.")
})
