#!/usr/bin/env Rscript

library(testthat)
library(stringr)
library(pdftools)
library(magick)
library(tesseract)
library(readtext)
library(future.apply)

# Source helper functions first so text_processing.R functions work properly.
source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "text_processing.R"))

context("Testing text_processing.R functions")

test_that("filter_text removes boilerplate content", {
  # Remove page numbers.
  input1 <- "Page 1\nThis is some content."
  output1 <- filter_text(input1)
  expect_false(grepl("Page 1", output1))
  
  # Remove figure/table captions.
  input2 <- "Figure 3: An example figure\nSome text."
  output2 <- filter_text(input2)
  expect_false(grepl("Figure 3", output2))
  
  # Remove URLs and emails.
  input3 <- "Visit https://example.com for details or email test@example.com."
  output3 <- filter_text(input3)
  expect_false(grepl("https://example.com", output3))
  expect_false(grepl("test@example.com", output3))
  
  # Remove reference section.
  input4 <- "Some content before.\nReferences\nReference 1\nReference 2"
  output4 <- filter_text(input4)
  expect_false(grepl("References", output4, ignore.case = TRUE))
})

test_that("chunk_text_naive splits text into correct chunks", {
  # Create text with several paragraphs separated by blank lines.
  text <- paste("Paragraph 1", "Paragraph 2 is a bit longer", "Paragraph 3", sep = "\n\n")
  
  # With a high token limit, the entire text should be one chunk.
  chunks1 <- chunk_text_naive(text, chunk_token_limit = 1000)
  expect_equal(length(chunks1), 1)
  
  # With a very low token limit (forcing splitting), expect multiple chunks.
  chunks2 <- chunk_text_naive(text, chunk_token_limit = 2)
  expect_true(length(chunks2) > 1)
})

test_that("chunk_text_semantic groups paragraphs by similarity", {
  # Create three paragraphs:
  # - Two that mention "cat" (should be grouped if the token limit allows)
  # - One distinct paragraph.
  para1 <- "The cat sat on the mat."
  para2 <- "My cat loves to nap on the mat."
  para3 <- "Dogs are playful animals."
  text <- paste(para1, para2, para3, sep = "\n\n")
  
  # Use a very low token limit to force splitting even when similarity is high.
  chunks <- chunk_text_semantic(text, chunk_token_limit = 10)
  # Expect at least two chunks.
  expect_true(length(chunks) >= 2)
  
  # With a high token limit, all paragraphs may be combined.
  chunks_high <- chunk_text_semantic(text, chunk_token_limit = 1000)
  expect_true(length(chunks_high) == 1)
})

test_that("parse_text returns cleaned chunks for a valid text file", {
  # Create a temporary text file.
  tmp_file <- tempfile(fileext = ".txt")
  on.exit(unlink(tmp_file))
  
  # Write sample text including extra whitespace, numbers, special chars, and a reference section.
  sample_text <- "   This is a test document. \n\nIt has numbers 12345 and special characters !@#$%.\n\nReferences\nSome reference list"
  writeLines(sample_text, tmp_file)
  
  # Parse using naive method.
  chunks_naive <- parse_text(tmp_file, chunk_token_limit = 1000, chunk_method = "naive",
                             remove_whitespace = TRUE, remove_special_chars = TRUE, remove_numbers = TRUE, ocr_lang = "eng")
  expect_true(length(chunks_naive) >= 1)
  
  # Check that numbers are removed.
  expect_false(grepl("\\d", chunks_naive[1]))
  # Check that the word "References" does not appear.
  expect_false(grepl("References", chunks_naive[1], ignore.case = TRUE))
  
  # Parse using semantic method.
  chunks_semantic <- parse_text(tmp_file, chunk_token_limit = 1000, chunk_method = "semantic",
                                remove_whitespace = TRUE, remove_special_chars = TRUE, remove_numbers = TRUE, ocr_lang = "eng")
  expect_true(length(chunks_semantic) >= 1)
  
  # Test that a nonexistent file produces an error.
  expect_error(parse_text("nonexistent_file.txt", chunk_method = "naive"),
               "File does not exist:")
})
