# text_processing.R

#' Filter out irrelevant or boilerplate content from text.
#'
#' Removes common noise such as page numbers, figures, tables, and reference sections.
#' @param text The raw extracted text.
#' @return Cleaned text with less irrelevant content.
filter_text <- function(text) {
  # Remove page number lines (e.g., "Page 1")
  text <- gsub("(?mi)^\\s*Page\\s+\\d+.*$", "", text, perl = TRUE)
  # Remove figure and table captions (simple heuristic)
  text <- gsub("(?mi)^\\s*(Figure|Table)\\s+\\d+.*$", "", text, perl = TRUE)
  # Remove URLs and email addresses
  text <- gsub("https?://\\S+|www\\.[^\\s]+", "", text)
  text <- gsub("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", "", text)
  # Optionally remove reference section if present as a heading
  if (grepl("(?mi)^\\s*References\\b", text, perl = TRUE)) {
    # Cut off everything from "References" to end, assuming it's a reference list
    text <- sub("(?mi)References.*", "", text, perl = TRUE)
  }
  return(text)
}

#' Split text into chunks with naive paragraph grouping.
#'
#' Splits text by paragraphs and combines them until a token limit is reached.
#' Ensures chunks do not exceed the chunk_token_limit (approximate).
#' @param text The input text to chunk.
#' @param chunk_token_limit Maximum tokens per chunk (approximate).
#' @return A character vector of text chunks.
chunk_text_naive <- function(text, chunk_token_limit = 3000) {
  paragraphs <- unlist(strsplit(text, "\n{2,}"))  # split on blank lines (paragraphs)
  paragraphs <- paragraphs[nchar(trimws(paragraphs)) > 0]
  chunks <- c()
  current_chunk <- ""
  for (para in paragraphs) {
    candidate <- if (current_chunk == "") para else paste(current_chunk, para, sep = "\n\n")
    if (estimate_token_count(candidate) <= chunk_token_limit) {
      current_chunk <- candidate
    } else {
      if (nzchar(current_chunk)) {
        chunks <- c(chunks, current_chunk)
      }
      if (estimate_token_count(para) > chunk_token_limit) {
        tokens <- unlist(strsplit(para, "\\s+"))
        token_groups <- split(tokens, ceiling(seq_along(tokens) / chunk_token_limit))
        para_chunks <- vapply(token_groups, paste, collapse = " ", FUN.VALUE = character(1))
        chunks <- c(chunks, para_chunks)
        current_chunk <- ""
      } else {
        current_chunk <- para
      }
    }
  }
  if (nzchar(current_chunk)) {
    chunks <- c(chunks, current_chunk)
  }
  return(chunks)
}

#' Split text into chunks using semantic-aware grouping.
#'
#' Attempts to split text at natural breakpoints (e.g., section boundaries) 
#' and keeps semantically related paragraphs together, under the token limit.
#' @param text The input text to chunk.
#' @param chunk_token_limit Maximum tokens per chunk (approximate).
#' @return A character vector of semantically grouped text chunks.
chunk_text_semantic <- function(text, chunk_token_limit = 3000) {
  paragraphs <- unlist(strsplit(text, "\n{2,}"))
  paragraphs <- paragraphs[nchar(trimws(paragraphs)) > 0]
  chunks <- c()
  current_chunk <- paragraphs[1]
  for (i in 2:length(paragraphs)) {
    para <- paragraphs[i]
    # Check semantic similarity with current chunk (using word overlap as a simple proxy)
    current_words <- tolower(gsub("[^[:alnum:]\\s]", "", current_chunk))
    next_words <- tolower(gsub("[^[:alnum:]\\s]", "", para))
    current_words <- unlist(strsplit(current_words, "\\s+"))
    next_words <- unlist(strsplit(next_words, "\\s+"))
    overlap <- length(intersect(current_words, next_words))
    if (overlap > 0 || nchar(para) < 200) {
      candidate <- paste(current_chunk, para, sep = "\n\n")
      if (estimate_token_count(candidate) <= chunk_token_limit) {
        current_chunk <- candidate
      } else {
        chunks <- c(chunks, current_chunk)
        current_chunk <- para
      }
    } else {
      chunks <- c(chunks, current_chunk)
      current_chunk <- para
    }
  }
  if (nzchar(current_chunk)) {
    chunks <- c(chunks, current_chunk)
  }
  return(chunks)
}

#' Parse a document file into cleaned text chunks.
#'
#' Supports PDF, DOCX, TXT, and image files. Performs OCR on scanned PDFs/images.
#' Applies filtering to remove boilerplate content and then splits into chunks.
#'
#' @param file_path Path to the document file.
#' @param chunk_token_limit Maximum tokens per chunk (default 3000).
#' @param chunk_method Chunking method: "naive" (by size) or "semantic".
#' @param remove_whitespace Trim extra whitespace? (default TRUE)
#' @param remove_special_chars Remove non-alphanumeric characters? (default TRUE)
#' @param remove_numbers Remove numeric characters? (default TRUE)
#' @param ocr_lang Language for OCR (default "eng").
#' @return A character vector of text chunks.
parse_text <- function(file_path, chunk_token_limit = 3000, chunk_method = c("naive", "semantic"),
                       remove_whitespace = TRUE, remove_special_chars = TRUE, remove_numbers = TRUE,
                       ocr_lang = "eng") {
  chunk_method <- match.arg(chunk_method)
  file_key <- normalizePath(file_path, winslash = "/", mustWork = FALSE)
  if (!exists(".doc_cache", envir = .GlobalEnv)) {
    assign(".doc_cache", new.env(parent = emptyenv()), envir = .GlobalEnv)
  }
  doc_cache <- get(".doc_cache", envir = .GlobalEnv)
  if (!is.null(doc_cache[[file_key]])) {
    message("Using cached text for this document.")
    return(doc_cache[[file_key]])
  }
  if (!file.exists(file_path)) {
    stop("File does not exist: ", file_path)
  }
  ext <- tolower(tools::file_ext(file_path))
  text <- ""
  text <- tryCatch({
    if (ext == "pdf") {
      pages <- pdftools::pdf_text(file_path)
      if (all(nchar(trimws(pages)) == 0)) {
        message("PDF appears to be scanned; performing OCR on each page...")
        pages <- future.apply::future_lapply(seq_len(pdftools::pdf_info(file_path)$pages), function(i) {
          img <- magick::image_read_pdf(file_path, pages = i)
          tesseract::ocr(img, engine = tesseract::tesseract(ocr_lang))
        })
      }
      paste(pages, collapse = "\n\n--- Page Break ---\n\n")
    } else if (ext == "docx") {
      doc <- readtext::readtext(file_path)$text
      unzip_dir <- tempfile()
      unzip(file_path, exdir = unzip_dir)
      media_dir <- file.path(unzip_dir, "word", "media")
      if (dir.exists(media_dir)) {
        image_files <- list.files(media_dir, full.names = TRUE)
        if (length(image_files) > 0) {
          message("Performing OCR on ", length(image_files), " image(s) in DOCX...")
          ocr_texts <- vapply(image_files, FUN.VALUE = character(1), FUN = function(img) {
            tryCatch(tesseract::ocr(img, engine = tesseract::tesseract(ocr_lang)),
                     error = function(e) "") })
          doc <- paste(doc, paste(ocr_texts, collapse = " "), sep = "\n\n")
        }
      }
      doc
    } else if (ext == "txt") {
      paste(readLines(file_path, encoding = "UTF-8"), collapse = "\n")
    } else if (ext %in% c("png", "jpg", "jpeg", "tif", "tiff")) {
      message("Running OCR on image file...")
      tesseract::ocr(file_path, engine = tesseract::tesseract(ocr_lang))
    } else {
      stop("Unsupported file type: .", ext)
    }
  }, error = function(e) {
    stop("Error during file parsing: ", e$message)
  })
  if (nchar(trimws(text)) < 20) {
    stop("Extracted very little text from the document. It might be empty or not readable.")
  }
  if (remove_whitespace) text <- stringr::str_trim(text)
  if (remove_special_chars) text <- stringr::str_replace_all(text, "[^[:alnum:]\\s[:punct:]]", " ")
  if (remove_numbers) text <- stringr::str_replace_all(text, "\\d+", " ")
  text <- filter_text(text)
  chunks <- if (chunk_method == "semantic") {
    chunk_text_semantic(text, chunk_token_limit)
  } else {
    chunk_text_naive(text, chunk_token_limit)
  }
  doc_cache[[file_key]] <- chunks
  return(chunks)
}
