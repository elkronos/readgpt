# main.R

#' Answer a question about a document using various processing modes.
#'
#' This high-level function orchestrates the document reading and question answering.
#' It allows different modes of operation:
#' - "Retrieval": extract relevant sections and query GPT once.
#' - "Chunked": split the document and query GPT chunk by chunk (Deep Thinking).
#' - "Semantic": use semantic-aware chunking and then sort chunks by semantic relevance before querying.
#' - "Hierarchical": summarize sections first, then answer using the summaries.
#' - "MultiPass": combine retrieval and chunked strategies for an answer.
#'
#' @param file_path Path to the document file.
#' @param question The question to ask about the document.
#' @param mode A character vector specifying one or more processing modes. Available modes are "Retrieval", "Chunked", "Semantic", "Hierarchical", and "MultiPass". The function will execute each specified mode.
#' @param use_parallel Whether to enable parallel processing for chunk-based modes (default FALSE).
#' @param refine If TRUE, perform an additional refinement pass on the final answer (default FALSE).
#' @param ... Additional arguments passed to lower-level functions (e.g., model, temperature, etc.).
#' @return If a single mode is specified, returns the answer as a character string. If multiple modes are provided, returns a named list of answers keyed by each mode.
answer_question <- function(file_path, question, mode = c("Retrieval", "Chunked", "Semantic", "Hierarchical", "MultiPass"),
                            use_parallel = FALSE, refine = FALSE, ...) {
  mode <- match.arg(mode, several.ok = TRUE)
  
  if (missing(file_path) || !file.exists(file_path)) {
    stop("Please provide a valid file path.")
  }
  if (missing(question) || nchar(trimws(question)) == 0) {
    stop("Please provide a non-empty question.")
  }
  
  message("Parsing document and preparing text chunks...")
  # If any mode is Semantic, use semantic chunking.
  chunk_method <- if ("Semantic" %in% mode) "semantic" else "naive"
  chunks <- parse_text(file_path, chunk_method = chunk_method)
  message("Document split into ", length(chunks), " chunks.")
  
  # For Semantic mode, sort the chunks by semantic similarity.
  if ("Semantic" %in% mode) {
    message("Sorting chunks by semantic relevance...")
    chunks <- sort_chunks_by_semantic(chunks, question)
  }
  
  # Process each selected mode.
  answers <- list()
  for (m in mode) {
    if (m == "Retrieval") {
      message("Mode: Retrieval - extracting relevant sections and answering...")
      answers[[m]] <- gpt_read_retrieval(chunks, question, use_parallel = use_parallel, ...)
    } else if (m == "Chunked") {
      message("Mode: Chunked - querying each chunk and aggregating answers...")
      answers[[m]] <- gpt_read_chunked(chunks, question, use_parallel = use_parallel, ...)
    } else if (m == "Semantic") {
      message("Mode: Semantic - using semantic sorting and chunked querying...")
      answers[[m]] <- gpt_read_chunked(chunks, question, use_parallel = use_parallel, ...)
    } else if (m == "Hierarchical") {
      message("Mode: Hierarchical - summarizing and then answering...")
      answers[[m]] <- gpt_read_hierarchical(chunks, question, use_parallel = use_parallel, ...)
    } else if (m == "MultiPass") {
      message("Mode: MultiPass - combining retrieval and chunked strategies...")
      answers[[m]] <- gpt_read_multipass(chunks, question, use_parallel = use_parallel, ...)
    }
  }
  
  # Optionally refine each answer.
  if (refine) {
    message("Refining the answers with an additional pass...")
    answers <- lapply(answers, function(ans) refine_answer(chunks, question, ans, ...))
  }
  
  log_query(question, answers)
  
  cat("\n====================\nQuestion: ", question, "\n")
  for (m in names(answers)) {
    cat("\nMode: ", m, "\nAnswer:\n", answers[[m]], "\n")
  }
  cat("\n====================\n")
  
  # If only one mode was provided, return its answer as plain text.
  if (length(answers) == 1) {
    return(invisible(answers[[1]]))
  } else {
    return(invisible(answers))
  }
}
