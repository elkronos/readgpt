library(testthat)
library(jsonlite)

# ------------------------------------------------------------------------------
# Source all production files to ensure that functions like gpt_read_retrieval are loaded.
# Adjust the path below if your production R files are located elsewhere.
# ------------------------------------------------------------------------------
prod_files <- list.files("C:/repos/gpt_read/gpt_read/R", full.names = TRUE, pattern = "\\.R$")
for (f in prod_files) {
  source(f)
}

test_that("Real API call in multi-pass mode returns a substantive answer and not a placeholder", {
  chunks <- c("This chunk discusses the usage of GPT models for language processing. It explicitly mentions gpt-3.5-turbo and gpt-4 as examples of current models.")
  question <- "Which models are mentioned in the chunk?"
  
  answer <- gpt_read_multipass(chunks, question)
  
  # Check that the returned answer is not the literal placeholder "merged answer".
  expect_false(tolower(trimws(answer)) == "merged answer",
               info = "The API should return a substantive answer, not the literal placeholder 'merged answer'.")
  
  # Also check that an answer is indeed returned.
  expect_true(nzchar(trimws(answer)),
              info = "The API should return a non-empty answer.")
  
  cat("Real API call in multi-pass mode returned: ", answer, "\n")
})

test_that("Real API call in multi-pass mode returns valid JSON chain-of-thought when requested", {
  chunks <- c("This chunk discusses the usage of GPT models for language processing. It explicitly mentions gpt-3.5-turbo and gpt-4 as examples of current models.")
  question <- "Which models are mentioned in the chunk?"
  
  result_json <- gpt_read_multipass(chunks, question, return_json = TRUE)
  chain <- fromJSON(result_json)
  
  expect_true(is.list(chain))
  expect_true("final_answer" %in% names(chain))
  expect_true(nzchar(trimws(chain$final_answer)),
              info = "The chain-of-thought must contain a non-empty final answer.")
  expect_equal(chain$question, question,
               info = "The JSON chain-of-thought should preserve the original question.")
  
  cat("Chain-of-thought JSON returned: \n", result_json, "\n")
})
