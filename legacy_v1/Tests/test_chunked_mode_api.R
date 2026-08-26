library(testthat)
library(jsonlite)

# Source the functions to be tested.
source("C:/repos/gpt_read/gpt_read/R/chunked_mode.R")
source("C:/repos/gpt_read/gpt_read/R/utils.R")

test_that("Real API call returns a real answer and not a placeholder", {
  chunks <- c("This chunk discusses the usage of GPT models for language processing. It explicitly mentions gpt-3.5-turbo and gpt-4 as examples of current models.")
  question <- "Which models are mentioned in the chunk?"
  
  answer <- gpt_read_chunked(chunks, question)
  
  # Check that the returned answer is not the placeholder text "merged answer".
  expect_false(tolower(trimws(answer)) == "merged answer",
               info = "The API should return a substantive answer, not the literal placeholder 'merged answer'.")
  
  cat("Real API call returned: ", answer, "\n")
})
