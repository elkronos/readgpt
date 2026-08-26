# multi_pass_mode.R

#' Refine an existing answer using the document text (follow-up pass).
#'
#' This function takes an initial answer and tries to improve it by consulting the document again.
#' It uses the original question and the current answer to prompt GPT for any corrections or additions based on the text.
#' Optionally, the entire chain-of-thought (including prompts, responses, and parameters) is returned as JSON.
#'
#' @param chunks The text chunks of the document.
#' @param question The original question.
#' @param current_answer The current answer that we want to refine.
#' @param model GPT model to use for refinement (default "gpt-3.5-turbo").
#' @param max_tokens Maximum tokens for the refined answer (default 1024).
#' @param num_retries Number of API retries (default 5).
#' @param pause_base Base pause for retries (default 3).
#' @param return_json If TRUE, returns the chain-of-thought as a JSON string.
#' @return A refined answer string or a JSON chain-of-thought.
refine_answer <- function(chunks, question, current_answer, model = "gpt-3.5-turbo",
                          max_tokens = 1024, num_retries = 5, pause_base = 3,
                          return_json = FALSE) {
  chain <- list()
  chain$phase <- "refinement"
  chain$question <- question
  chain$current_answer <- current_answer
  
  # Use a subset of the document if possible for refinement:
  # For instance, extract keywords from the current answer and retrieve snippets.
  keywords <- unlist(strsplit(current_answer, "\\s+"))
  keywords <- keywords[nchar(keywords) > 4]  # use longer words as key terms
  snippets <- search_text(chunks, keywords[1:min(5, length(keywords))], window_chars = 300)
  doc_excerpt <- paste(snippets, collapse = "\n\n")
  chain$document_excerpt <- doc_excerpt
  
  refine_prompt <- paste(
    "Here is an initial answer to a question and some relevant excerpts from the document.",
    "Please refine the answer to be more accurate and complete using the provided document information.",
    "\n\nQuestion:\n", question,
    "\n\nCurrent Answer:\n", current_answer,
    "\n\nDocument Excerpts:\n", doc_excerpt
  )
  chain$refine_prompt <- refine_prompt
  msgs <- list(
    list(role = "system", content = "You are a fact-checker and editor improving the answer using the document."),
    list(role = "user", content = refine_prompt)
  )
  chain$msgs <- msgs
  
  refined <- process_api_call(msgs, model = model, temperature = 0.0, max_tokens = max_tokens,
                              presence_penalty = 0.0, frequency_penalty = 0.0,
                              num_retries = num_retries, pause_base = pause_base)
  chain$refined_response <- ifelse(is.null(refined), "", refined)
  final_refined <- ifelse(is.null(refined) || !nzchar(refined), current_answer, stringr::str_trim(refined))
  chain$final_refined <- final_refined
  
  if (return_json) {
    return(jsonlite::toJSON(chain, pretty = TRUE, auto_unbox = TRUE))
  } else {
    return(final_refined)
  }
}

#' Answer a question using a multi-pass approach (combining retrieval and chunked results).
#'
#' This method uses two different passes:
#' - First, a Retrieval mode pass extracts relevant text from the document.
#' - Second, a Chunked mode pass queries the document in a deep-thinking manner.
#' The results from both passes are then merged to produce a final answer.
#' Optionally, the entire chain-of-thought (with details from both passes and the merge) is returned as JSON.
#'
#' @param chunks Text chunks of the document.
#' @param question The question to answer.
#' @param model GPT model to use (default "gpt-3.5-turbo").
#' @param use_parallel Whether to use parallel processing for any chunked operations (default FALSE).
#' @param return_json If TRUE, returns the entire chain-of-thought as a JSON string.
#' @param ... Additional parameters passed to the underlying retrieval/chunked functions (e.g., temperature, max_tokens).
#' @return A combined answer string or a JSON chain-of-thought.
gpt_read_multipass <- function(chunks, question, model = "gpt-3.5-turbo", use_parallel = FALSE, 
                               return_json = FALSE, ...) {
  chain <- list()
  chain$phase <- "multi_pass"
  chain$question <- question
  
  message("Multi-pass strategy: performing Retrieval and Chunked modes...")
  
  # First pass: Retrieval mode (without falling back automatically)
  retrieval_answer <- gpt_read_retrieval(chunks, question, model = model, use_parallel = use_parallel, fallback = FALSE, ...)
  chain$retrieval <- list(answer = retrieval_answer)
  
  # Second pass: Chunked mode
  chunked_answer <- gpt_read_chunked(chunks, question, model = model, use_parallel = use_parallel, ...)
  chain$chunked <- list(answer = chunked_answer)
  
  # Merge the two answers.
  if (is.null(retrieval_answer) || !nzchar(trimws(retrieval_answer))) {
    merged_answer <- chunked_answer
    chain$merge_method <- "Only chunked answer available"
  } else if (is.null(chunked_answer) || !nzchar(trimws(chunked_answer))) {
    merged_answer <- retrieval_answer
    chain$merge_method <- "Only retrieval answer available"
  } else {
    merge_prompt <- paste("Answer from retrieval method:\n", retrieval_answer,
                          "\n\nAnswer from chunked method:\n", chunked_answer,
                          "\n\nMerge these answers into one comprehensive, accurate answer to the question.")
    chain$merge_prompt <- merge_prompt
    msgs_merge <- list(
      list(role = "system", content = "You are a moderator who combines answers from different approaches into one."),
      list(role = "user", content = merge_prompt)
    )
    chain$msgs_merge <- msgs_merge
    combined <- process_api_call(msgs_merge, model = model, temperature = 0.0, 
                                 max_tokens = get_model_limits(model)$output_tokens,
                                 presence_penalty = 0.0, frequency_penalty = 0.0, 
                                 num_retries = 5, pause_base = 3)
    chain$merge_response <- combined
    if (is.null(combined) || !nzchar(trimws(combined))) {
      merged_answer <- paste(retrieval_answer, chunked_answer, sep="\n\n---\n\n")
      chain$merge_method <- "Fallback concatenation"
    } else {
      merged_answer <- combined
      chain$merge_method <- "LLM merge"
    }
  }
  
  chain$final_answer <- stringr::str_trim(merged_answer)
  
  if (return_json) {
    return(jsonlite::toJSON(chain, pretty = TRUE, auto_unbox = TRUE))
  } else {
    return(chain$final_answer)
  }
}
