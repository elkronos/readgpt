# test_answer_question.R
# This file is intended to be run from RStudio (or similar) from the Tests folder.
# It uses the testthat package to run a series of tests that verify the behavior of the answer_question function.
# It mocks out helper functions from utils.R and text_processing.R to allow isolated testing.

library(testthat)

# Global variable to capture calls to mocks
called_functions <- list()

# Function to reset the global call record before each test
reset_called_functions <- function() {
  called_functions <<- list(
    parse_text = list(),
    sort_chunks_by_semantic = list(),
    gpt_read_retrieval = list(),
    gpt_read_chunked = list(),
    gpt_read_hierarchical = list(),
    gpt_read_multipass = list(),
    refine_answer = list(),
    log_query = list()
  )
}

# Helper function to record each call
record_call <- function(func_name, args) {
  called_functions[[func_name]] <<- c(called_functions[[func_name]], list(args))
}

# --- MOCK FUNCTIONS ---
# Override helper functions so that the UAT can test answer_question without relying on actual API calls

parse_text <<- function(file_path, chunk_token_limit = 3000, chunk_method = c("naive", "semantic"),
                        remove_whitespace = TRUE, remove_special_chars = TRUE, remove_numbers = TRUE,
                        ocr_lang = "eng") {
  record_call("parse_text", list(
    file_path = file_path,
    chunk_token_limit = chunk_token_limit,
    chunk_method = match.arg(chunk_method)
  ))
  # Return a dummy vector of text chunks
  return(c("chunk1", "chunk2"))
}

sort_chunks_by_semantic <<- function(chunks, query) {
  record_call("sort_chunks_by_semantic", list(chunks = chunks, query = query))
  # For testing, simply reverse the chunks to simulate reordering
  return(rev(chunks))
}

gpt_read_retrieval <<- function(chunks, question, use_parallel = FALSE, ...) {
  record_call("gpt_read_retrieval", list(
    chunks = chunks,
    question = question,
    use_parallel = use_parallel,
    extra = list(...)
  ))
  return("retrieval answer")
}

gpt_read_chunked <<- function(chunks, question, use_parallel = FALSE, ...) {
  record_call("gpt_read_chunked", list(
    chunks = chunks,
    question = question,
    use_parallel = use_parallel,
    extra = list(...)
  ))
  return("chunked answer")
}

gpt_read_hierarchical <<- function(chunks, question, use_parallel = FALSE, ...) {
  record_call("gpt_read_hierarchical", list(
    chunks = chunks,
    question = question,
    use_parallel = use_parallel,
    extra = list(...)
  ))
  return("hierarchical answer")
}

gpt_read_multipass <<- function(chunks, question, use_parallel = FALSE, ...) {
  record_call("gpt_read_multipass", list(
    chunks = chunks,
    question = question,
    use_parallel = use_parallel,
    extra = list(...)
  ))
  return("multipass answer")
}

refine_answer <<- function(chunks, question, answer, ...) {
  record_call("refine_answer", list(
    chunks = chunks,
    question = question,
    answer = answer,
    extra = list(...)
  ))
  return(paste(answer, "refined"))
}

log_query <<- function(question, answer, log_file = "gpt_read.log") {
  record_call("log_query", list(
    question = question,
    answer = answer,
    log_file = log_file
  ))
  # For testing, we do not write to any file.
}

# --- SOURCE THE MAIN SCRIPT ---
# Adjust the path as needed relative to the Tests folder.
source("../R/main.R")

# Create a temporary file that simulates a valid document for testing.
temp_file <- tempfile(pattern = "test_doc_", fileext = ".txt")
writeLines("This is a test document.\n\nIt has multiple paragraphs.\n\nEnd of document.", temp_file)

# --- TEST PLAN ---
#
# 1. Error Conditions:
#    - An invalid file path should trigger an error.
#    - A missing or empty question should trigger an error.
#
# 2. Mode Tests:
#    - For each mode ("Retrieval", "Chunked", "Semantic", "Hierarchical", "MultiPass"),
#      check that answer_question returns the expected answer and that the right helper
#      functions are called. For example, in Semantic mode the file should be parsed with
#      chunk_method "semantic" and sort_chunks_by_semantic should be called.
#
# 3. Parameter Passing:
#    - Check that use_parallel is passed correctly.
#    - Verify that additional parameters (e.g., model, temperature) are passed to the helper functions.
#
# 4. Refinement:
#    - When refine = TRUE, ensure that the refine_answer function is invoked and its output is used.
#

# --- TESTS ---

test_that("Error when file path is invalid", {
  reset_called_functions()
  expect_error(answer_question("non_existent_file.txt", "What is this?"),
               "Please provide a valid file path")
})

test_that("Error when question is empty", {
  reset_called_functions()
  expect_error(answer_question(temp_file, "   "),
               "Please provide a non-empty question")
})

test_that("Mode: Retrieval works correctly", {
  reset_called_functions()
  answer <- answer_question(temp_file, "What is this?", mode = "Retrieval",
                            use_parallel = TRUE, model = "dummy", temperature = 0.5)
  expect_equal(answer, "retrieval answer")
  
  # Check that parse_text was called with naive chunking (default for non-Semantic modes)
  expect_equal(called_functions$parse_text[[1]]$chunk_method, "naive")
  
  # Check that gpt_read_retrieval was called with the correct use_parallel value and extra parameters
  expect_true(length(called_functions$gpt_read_retrieval) > 0)
  expect_equal(called_functions$gpt_read_retrieval[[1]]$use_parallel, TRUE)
  expect_equal(called_functions$gpt_read_retrieval[[1]]$extra$model, "dummy")
  expect_equal(called_functions$gpt_read_retrieval[[1]]$extra$temperature, 0.5)
})

test_that("Mode: Chunked works correctly", {
  reset_called_functions()
  answer <- answer_question(temp_file, "Describe document?", mode = "Chunked",
                            use_parallel = FALSE)
  expect_equal(answer, "chunked answer")
  
  # Check that parse_text was called with naive chunking
  expect_equal(called_functions$parse_text[[1]]$chunk_method, "naive")
  
  # Check that gpt_read_chunked was called
  expect_true(length(called_functions$gpt_read_chunked) > 0)
})

test_that("Mode: Semantic works correctly", {
  reset_called_functions()
  answer <- answer_question(temp_file, "Analyze content", mode = "Semantic",
                            use_parallel = TRUE)
  # In Semantic mode, gpt_read_chunked is used (after semantic sorting)
  expect_equal(answer, "chunked answer")
  
  # Check that parse_text was called with semantic chunking
  expect_equal(called_functions$parse_text[[1]]$chunk_method, "semantic")
  
  # Check that sort_chunks_by_semantic was called
  expect_true(length(called_functions$sort_chunks_by_semantic) > 0)
})

test_that("Mode: Hierarchical works correctly", {
  reset_called_functions()
  answer <- answer_question(temp_file, "Summarize document", mode = "Hierarchical",
                            use_parallel = FALSE)
  expect_equal(answer, "hierarchical answer")
  
  # Check that parse_text was called with naive chunking (Hierarchical mode does not change chunking)
  expect_equal(called_functions$parse_text[[1]]$chunk_method, "naive")
  
  # Check that gpt_read_hierarchical was called
  expect_true(length(called_functions$gpt_read_hierarchical) > 0)
})

test_that("Mode: MultiPass works correctly", {
  reset_called_functions()
  answer <- answer_question(temp_file, "Deep analysis", mode = "MultiPass",
                            use_parallel = TRUE)
  expect_equal(answer, "multipass answer")
  
  # Check that parse_text was called with naive chunking (MultiPass mode does not change chunking)
  expect_equal(called_functions$parse_text[[1]]$chunk_method, "naive")
  
  # Check that gpt_read_multipass was called
  expect_true(length(called_functions$gpt_read_multipass) > 0)
})

test_that("Refine parameter triggers refinement", {
  reset_called_functions()
  answer <- answer_question(temp_file, "What is refined?", mode = "Chunked",
                            refine = TRUE)
  expect_equal(answer, "chunked answer refined")
  
  # Check that refine_answer was called
  expect_true(length(called_functions$refine_answer) > 0)
})

test_that("Additional parameters are passed to helper functions", {
  reset_called_functions()
  answer <- answer_question(temp_file, "Test extra params", mode = "Retrieval",
                            use_parallel = TRUE, model = "test-model", temperature = 0.7)
  expect_equal(answer, "retrieval answer")
  
  # Verify extra parameters passed in the gpt_read_retrieval call
  call_info <- called_functions$gpt_read_retrieval[[1]]$extra
  expect_equal(call_info$model, "test-model")
  expect_equal(call_info$temperature, 0.7)
})

# Clean up temporary file after tests
unlink(temp_file)
