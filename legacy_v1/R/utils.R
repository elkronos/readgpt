# utils.R

#' Estimate token count (approximate) for a text string.
#'
#' Splits text on whitespace to approximate the number of tokens.
#' @param text A character string to tokenize.
#' @return Integer count of tokens (approximate).
estimate_token_count <- function(text) {
  tokens <- unlist(strsplit(text, "\\s+"))
  length(tokens)
}

#' Get context window and output token limits for a given model.
#'
#' Supports known OpenAI models; defaults to 4096 for unknown models.
#' @param model Model name (e.g., "gpt-3.5-turbo", "gpt-4").
#' @return A list with elements `context_window` and `output_tokens`.
get_model_limits <- function(model) {
  # Known model context sizes
  if (model %in% c("gpt-3.5-turbo-1106", "gpt-3.5-turbo")) {
    list(context_window = 16385, output_tokens = 4096)
  } else if (model %in% c("gpt-4.5-preview-2025-02-27")) {
    list(context_window = 128000, output_tokens = 16384)
  } else if (model %in% c("gpt-4o-2024-08-06", "chatgpt-4o-latest", "gpt-4o-mini-2024-07-18")) {
    list(context_window = 128000, output_tokens = 16384)
  } else if (grepl("gpt-4", model)) {
    list(context_window = 8192, output_tokens = 2048)  # example for GPT-4 default
  } else {
    list(context_window = 4096, output_tokens = 4096)
  }
}

#' Retrieve the OpenAI API key from environment or options.
#'
#' Looks for an API key in the OPENAI_API_KEY environment variable.
#' @return API key string.
#' @throws Error if API key is not set.
get_api_key <- function() {
  key <- Sys.getenv("OPENAI_API_KEY")
  if (nzchar(key)) {
    return(key)
  } else {
    stop("OpenAI API key not found. Please set the OPENAI_API_KEY environment variable.")
  }
}

#' Call the OpenAI Chat Completion API with retries and error handling.
#'
#' Uses exponential backoff to retry on failures. Logs warnings on error.
#'
#' @param messages A list of message objects (each with role and content) for the chat API.
#' @param model Model identifier (e.g., "gpt-3.5-turbo").
#' @param temperature Sampling temperature for the model.
#' @param max_tokens Maximum tokens in the response.
#' @param presence_penalty OpenAI presence penalty.
#' @param frequency_penalty OpenAI frequency penalty.
#' @param num_retries Number of retries for the API call.
#' @param pause_base Base pause (in seconds) for exponential backoff between retries.
#' @return The content string of the first choice response, or NULL on failure.
process_api_call <- function(messages, model = "gpt-3.5-turbo", temperature = 0.0, 
                             max_tokens = NULL, presence_penalty = 0.0, frequency_penalty = 0.0, 
                             num_retries = 5, pause_base = 3) {
  # Determine max_tokens if not set
  limits <- get_model_limits(model)
  if (is.null(max_tokens)) {
    max_tokens <- limits$output_tokens
  }
  # Prepare request body
  req_body <- list(model = model, temperature = temperature, max_tokens = max_tokens,
                   messages = messages, presence_penalty = presence_penalty, frequency_penalty = frequency_penalty)
  # Perform API request with retries
  response <- try(httr::RETRY("POST",
                              url = "https://api.openai.com/v1/chat/completions",
                              httr::add_headers(Authorization = paste("Bearer", get_api_key())),
                              httr::content_type_json(),
                              body = req_body, encode = "json",
                              times = num_retries, pause_base = pause_base),
                  silent = TRUE)
  if (inherits(response, "try-error")) {
    warning("Error in API request: ", conditionMessage(attr(response, "condition")))
    return(NULL)
  }
  
  # If response is not an httr response, assume a 200 status (for testing purposes)
  if (!inherits(response, "response")) {
    status <- 200
  } else {
    status <- httr::status_code(response)
  }
  
  if (status == 400) {
    error_details <- httr::content(response, as = "text", encoding = "UTF-8")
    warning("OpenAI API returned 400: ", error_details)
  }
  
  httr::stop_for_status(response, task = "OpenAI API request")
  content_list <- httr::content(response)
  if (length(content_list$choices) > 0) {
    # Extract the first response choice's content
    result <- content_list$choices[[1]]$message$content
  } else {
    result <- ""
  }
  result <- stringr::str_trim(gsub("\n+", " ", result))
  return(result)
}

#' Log a question and its answer to a file.
#'
#' Appends a timestamped Q&A entry to the log file for debugging or auditing.
#' @param question The question asked.
#' @param answer The answer returned.
#' @param log_file File path for the log (default "gpt_read.log").
log_query <- function(question, answer, log_file = "gpt_read.log") {
  entry <- paste(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "Q:", question, "\nA:", answer, "\n---\n")
  cat(entry, file = log_file, append = TRUE)
}

# -------------------------------------------------------------------
# New functions for semantic sorting using embeddings
# -------------------------------------------------------------------

#' Compute an embedding vector for a given text.
#'
#' (Replace this dummy implementation with a real embedding API call in production.)
#' @param text A character string.
#' @return A numeric vector representing the text embedding.
compute_embedding <- function(text) {
  # For demonstration purposes, simulate an embedding vector using a random vector.
  # The seed is set based on the text length to ensure consistent outputs for the same text.
  set.seed(nchar(text))
  runif(768)
}

#' Compute cosine similarity between two numeric vectors.
#'
#' @param vec1 A numeric vector.
#' @param vec2 A numeric vector.
#' @return The cosine similarity between the two vectors.
cosine_similarity <- function(vec1, vec2) {
  sum(vec1 * vec2) / (sqrt(sum(vec1^2)) * sqrt(sum(vec2^2)))
}

#' Sort text chunks by semantic similarity to a query.
#'
#' Computes an embedding for each chunk and for the query, then sorts the chunks in
#' descending order of cosine similarity with the query.
#'
#' @param chunks A character vector of text chunks.
#' @param query A character string representing the query.
#' @return A character vector of chunks sorted by relevance.
sort_chunks_by_semantic <- function(chunks, query) {
  query_embedding <- compute_embedding(query)
  similarities <- sapply(chunks, function(chunk) {
    chunk_embedding <- compute_embedding(chunk)
    cosine_similarity(query_embedding, chunk_embedding)
  })
  # Return chunks sorted in descending order of similarity.
  chunks[order(similarities, decreasing = TRUE)]
}
