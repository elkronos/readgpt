# test_main_all_modes_json.R
# Integration tests for answer_question() in main.R across all processing modes.
# This test loads all production R files from your production folder so that all helper functions are available.
#
# These tests make real API calls so ensure that OPENAI_API_KEY is set in your environment.
# For each mode, both plain text and JSON outputs are printed, and the JSON output is verified
# to include the expected keys, which differ by mode.

library(testthat)
library(jsonlite)

# Load all production R files.
prod_files <- list.files("C:/repos/gpt_read/gpt_read/R", full.names = TRUE, pattern = "\\.R$")
for (f in prod_files) {
  source(f)
}

# Create a temporary document file simulating a realistic document.
temp_file <- tempfile(pattern = "test_doc_", fileext = ".txt")
writeLines(
  c(
    "This is a sample document used for integration testing of answer_question().",
    "",
    "It contains multiple paragraphs to simulate a realistic document.",
    "It explains details about various GPT models, their usage contexts, and technical improvements.",
    "",
    "This document is used to exercise all processing modes: Retrieval, Chunked, Semantic, Hierarchical, and MultiPass.",
    "",
    "End of document."
  ),
  temp_file
)

# Define the processing modes to test.
modes <- c("Retrieval", "Chunked", "Semantic", "Hierarchical", "MultiPass")

# Define expected JSON keys per mode.
expected_keys <- list(
  Retrieval   = c("combined_text", "mode", "extraction_step", "answer_step", "final_answer"),
  Chunked     = c("parameters", "question", "chunk_queries", "final_chunk_responses", "combined_content", "merge_step", "final_answer"),
  Semantic    = c("parameters", "question", "chunk_queries", "final_chunk_responses", "combined_content", "merge_step", "final_answer"),
  Hierarchical= c("parameters", "question", "chunk_summaries", "relevant_summaries", "combined_summary", "final_prompt", "final_answer"),
  MultiPass   = c("phase", "question", "retrieval", "chunked", "merge_prompt", "msgs_merge", "merge_response", "final_answer")
)

for (mode in modes) {
  test_that(paste("answer_question returns both plain text and JSON output in", mode, "mode"), {
    skip_if(Sys.getenv("OPENAI_API_KEY") == "", "Skipping integration test: OPENAI_API_KEY not set")
    
    question <- paste("Integration test question for", mode, "mode: What are the key points of this document?")
    
    # Get plain text answer.
    answer_text <- answer_question(temp_file, question, mode = mode)
    
    # Get JSON output by passing return_json = TRUE.
    answer_json <- answer_question(temp_file, question, mode = mode, return_json = TRUE)
    
    cat("\n====================\n")
    cat("Mode:", mode, "\n")
    cat("Question:", question, "\n")
    cat("Text Answer:\n", answer_text, "\n")
    cat("JSON Output:\n", answer_json, "\n")
    cat("====================\n")
    
    # Check that the plain text answer is non-empty.
    expect_true(nchar(trimws(answer_text)) > 0,
                info = paste("Text answer for", mode, "mode should be non-empty."))
    
    # Parse the JSON output.
    parsed <- tryCatch(fromJSON(answer_json), error = function(e) NULL)
    expect_true(!is.null(parsed),
                info = paste("JSON output for", mode, "mode should be valid JSON."))
    
    # Check that the JSON output includes the expected keys for this mode.
    for (key in expected_keys[[mode]]) {
      expect_true(key %in% names(parsed),
                  info = paste("JSON output for", mode, "mode should contain the key:", key))
    }
    
    # Additionally, verify that the final answer in the JSON is substantive.
    if ("final_answer" %in% names(parsed)) {
      expect_true(nchar(trimws(parsed$final_answer)) > 0,
                  info = paste("The JSON output for", mode, "mode should have a non-empty 'final_answer'."))
    } else if ("answer" %in% names(parsed)) {
      expect_true(nchar(trimws(parsed$answer)) > 0,
                  info = paste("The JSON output for", mode, "mode should have a non-empty 'answer'."))
    }
  })
}

# Clean up the temporary document.
unlink(temp_file)
