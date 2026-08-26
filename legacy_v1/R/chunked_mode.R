# chunked_mode.R

#' Answer a question by querying each text chunk (Deep Thinking Mode).
#'
#' In this mode, the document is split into chunks which are each queried with the question.
#' Each chunk's response is then combined and a final answer is generated from all partial responses.
#' Optionally, the entire chain-of-thought (including prompts, responses, and parameters) can be returned as JSON.
#'
#' @param chunks A character vector of text chunks.
#' @param question The question to answer.
#' @param model GPT model to use for chunk querying (default "gpt-3.5-turbo").
#' @param temperature Temperature for the OpenAI API (default 0.0).
#' @param max_tokens Maximum tokens for each chunk response (defaults to model's output limit).
#' @param chunk_token_limit Manual override for allowed input tokens per chunk. If NULL, the limit is automatically calculated.
#' @param system_message_1 System prompt for chunk-level querying.
#' @param system_message_2 System prompt for final merging of answers.
#' @param presence_penalty Presence penalty for the API (default 0.0).
#' @param frequency_penalty Frequency penalty for the API (default 0.0).
#' @param num_retries Number of API retries for each call (default 5).
#' @param pause_base Base pause time for retry backoff (default 3).
#' @param delay_between_chunks Delay (seconds) between chunk requests to avoid rate limits (default 0).
#' @param use_parallel Whether to process chunks in parallel (default FALSE).
#' @param return_json If TRUE, returns the entire chain-of-thought as a JSON string.
#' @param ... Additional arguments passed to underlying functions.
#' @return A single consolidated answer string or a JSON chain-of-thought.
gpt_read_chunked <- function(chunks, question, model = "gpt-3.5-turbo", temperature = 0.0, max_tokens = NULL,
                             chunk_token_limit = NULL,
                             system_message_1 = "You are a helpful assistant. Answer the question using ONLY the given text. If the text does not contain the answer, say so.",
                             system_message_2 = "You are a content editor who will merge multiple pieces of answers into one comprehensive answer.",
                             presence_penalty = 0.0, frequency_penalty = 0.0,
                             num_retries = 5, pause_base = 3, delay_between_chunks = 0,
                             use_parallel = FALSE, return_json = FALSE, ...) {
  if (is.null(question) || nchar(trimws(question)) == 0) {
    stop("Question must be provided for chunked processing.")
  }
  
  # Initialize a chain-of-thought container.
  chain <- list()
  chain$parameters <- list(
    model = model,
    temperature = temperature,
    max_tokens = max_tokens,
    chunk_token_limit = chunk_token_limit,
    presence_penalty = presence_penalty,
    frequency_penalty = frequency_penalty,
    num_retries = num_retries,
    pause_base = pause_base,
    delay_between_chunks = delay_between_chunks,
    use_parallel = use_parallel,
    system_message_1 = system_message_1,
    system_message_2 = system_message_2
  )
  chain$question <- question
  
  # Determine token limits.
  model_limits <- get_model_limits(model)
  if (is.null(max_tokens)) {
    max_tokens <- model_limits$output_tokens  # Use model's full output size if not set.
  }
  
  # Use manual override if provided; otherwise, calculate allowed input tokens per chunk.
  if (!is.null(chunk_token_limit)) {
    allowed_input_tokens <- chunk_token_limit
  } else {
    allowed_input_tokens <- model_limits$context_window - max_tokens - estimate_token_count(question) - 50
  }
  
  # Define a function to query a single chunk (with sub-chunk splitting if necessary).
  query_chunk <- function(chunk) {
    local_chain <- list()
    local_chain$original_chunk <- chunk
    if (estimate_token_count(chunk) > allowed_input_tokens) {
      local_chain$method <- "sub_chunk_split"
      # Split the chunk into tokens and then into sub-chunks.
      tokens <- unlist(strsplit(chunk, "\\s+"))
      sub_chunks <- split(tokens, ceiling(seq_along(tokens) / allowed_input_tokens))
      sub_chunks <- lapply(sub_chunks, paste, collapse = " ")
      local_chain$sub_chunks <- sub_chunks
      sub_responses <- c()
      local_chain$sub_steps <- list()
      for (i in seq_along(sub_chunks)) {
        subchunk <- sub_chunks[[i]]
        msgs <- list(
          list(role = "system", content = system_message_1),
          list(role = "user", content = subchunk),
          list(role = "user", content = question)
        )
        sub_resp <- process_api_call(msgs, model = model, temperature = temperature, max_tokens = max_tokens,
                                     presence_penalty = presence_penalty, frequency_penalty = frequency_penalty,
                                     num_retries = num_retries, pause_base = pause_base)
        sub_responses <- c(sub_responses, sub_resp)
        local_chain$sub_steps[[paste0("sub_chunk_", i)]] <- list(
          prompt = msgs,
          response = sub_resp
        )
        if (delay_between_chunks > 0) Sys.sleep(delay_between_chunks)
      }
      aggregated_response <- paste(sub_responses, collapse = " ")
      local_chain$aggregated_response <- aggregated_response
      return(local_chain)
    } else {
      local_chain$method <- "single_chunk"
      msgs <- list(
        list(role = "system", content = system_message_1),
        list(role = "user", content = chunk),
        list(role = "user", content = question)
      )
      resp <- process_api_call(msgs, model = model, temperature = temperature, max_tokens = max_tokens,
                               presence_penalty = presence_penalty, frequency_penalty = frequency_penalty,
                               num_retries = num_retries, pause_base = pause_base)
      local_chain$prompt <- msgs
      local_chain$response <- resp
      return(local_chain)
    }
  }
  
  # Process all chunks.
  if (use_parallel) {
    chunk_results <- future.apply::future_lapply(chunks, query_chunk)
  } else {
    chunk_results <- lapply(chunks, query_chunk)
  }
  chain$chunk_queries <- chunk_results
  
  # Extract the final response from each chunk.
  final_chunk_responses <- sapply(chunk_results, function(res) {
    if (res$method == "sub_chunk_split") {
      return(res$aggregated_response)
    } else {
      return(res$response)
    }
  })
  chain$final_chunk_responses <- final_chunk_responses
  
  # Merge the responses from all chunks.
  if (all(nzchar(final_chunk_responses) == FALSE) ||
      all(grepl("not found|no information|not applicable", tolower(final_chunk_responses)))) {
    final_answer <- "The answer to the question was not found in the provided document."
    chain$final_answer <- final_answer
  } else {
    combined_content <- paste(final_chunk_responses, collapse = "\n\n")
    chain$combined_content <- combined_content
    merge_msgs <- list(
      list(role = "system", content = system_message_2),
      list(role = "user", content = combined_content),
      list(role = "user", content = paste("Question:", question))
    )
    merge_response <- process_api_call(merge_msgs, model = model, temperature = temperature, max_tokens = max_tokens,
                                       presence_penalty = presence_penalty, frequency_penalty = frequency_penalty,
                                       num_retries = num_retries, pause_base = pause_base)
    if (is.null(merge_response) || !nzchar(merge_response)) {
      final_answer <- paste(final_chunk_responses, collapse = " ")
    } else {
      final_answer <- merge_response
    }
    chain$merge_step <- list(
      prompt = merge_msgs,
      response = merge_response
    )
    chain$final_answer <- final_answer
  }
  
  # Return the chain-of-thought JSON if requested; otherwise, return the final answer.
  if (return_json) {
    return(jsonlite::toJSON(chain, pretty = TRUE, auto_unbox = TRUE))
  } else {
    return(stringr::str_trim(chain$final_answer))
  }
}
