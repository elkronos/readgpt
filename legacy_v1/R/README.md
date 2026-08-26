# gpt_read
This repository contains several R scripts that implement document processing and question answering using GPT models. Each script provides functions for different processing modes and text handling techniques. Below is a summary of each function grouped by file.

## chunked_mode.R

- **gpt_read_chunked**  
  Splits a document into text chunks and queries each chunk with a specified question using GPT.  
  - **Key Features:**  
    - Dynamically splits oversized chunks into sub-chunks if necessary, ensuring token limits are respected.
    - Supports parallel processing for querying chunks.
    - Captures a detailed chain-of-thought including prompts, responses, parameters, and sub-step details.
    - Provides an option to return the entire chain-of-thought as a JSON string via the `return_json` parameter.
    - Merges individual chunk responses into one consolidated final answer, with built-in retry and delay mechanisms to manage rate limits.

## hierarchical_mode.R

- **gpt_read_hierarchical**  
  Implements a hierarchical two-pass reading mode for answering questions.  
  - **Key Features:**  
    - **Two-Pass Strategy:**  
      - **First Pass:** Each document chunk is summarized with a focus on the question, generating a higher-level digest of the content.
      - **Second Pass:** Non-empty summaries are combined to provide context for generating a detailed final answer.
    - **Configurable Token Limits:**  
      - Uses `summary_max_tokens` (default 512) for each chunk summary.
      - Uses `answer_max_tokens` (default 1024) for the final answer.
    - **Parallel Processing:**  
      - Supports summarizing chunks in parallel via the `use_parallel` parameter.
    - **Fallback Mechanism:**  
      - If no relevant content is found in the summaries, falls back to the chunked mode (`gpt_read_chunked`).
    - **Chain-of-Thought Logging:**  
      - Optionally returns the entire chain-of-thought (including parameters, prompts, responses, and intermediate summaries) as a JSON string using the `return_json` parameter.

## main.R

- **answer_question**  
  Orchestrates the entire document reading and question answering process using various processing modes.
  - **Key Features:**
    - **Multi-Mode Support:**  
      - **Retrieval:** Extracts relevant sections from the document and queries GPT once.
      - **Chunked:** Splits the document into chunks and queries GPT for each chunk individually.
      - **Semantic:** Uses semantic-aware chunking and sorts chunks by relevance before querying (invokes semantic chunking and sorting).
      - **Hierarchical:** Summarizes each chunk with respect to the question and then combines these summaries to generate a detailed answer.
      - **MultiPass:** Combines both retrieval and chunked strategies to generate a comprehensive answer.
    - **Document Parsing:**  
      - Reads the document from the provided file path and splits it into chunks using either naive or semantic methods.
      - For Semantic mode, sorts the chunks by their semantic similarity to the question.
    - **Parallel Processing:**  
      - Enables parallel processing for chunk-based modes via the `use_parallel` parameter.
    - **Answer Refinement:**  
      - Optionally refines each answer with an additional processing pass when the `refine` parameter is set to TRUE.
    - **Logging and Output:**  
      - Logs the question and corresponding answers.
      - Prints the question and answer(s) to the console.
      - Returns either a single answer or a named list of answers keyed by each mode.

## multi_pass_mode.R

- **refine_answer**  
  Refines an existing answer by reconsulting the document.  
  - **Key Features:**  
    - Extracts key keywords from the current answer and retrieves relevant document excerpts.
    - Prompts GPT to improve the answer's accuracy and completeness using the provided document information.
    - Optionally returns the entire chain-of-thought (including prompts, responses, and parameters) as a JSON string via the `return_json` parameter.

- **gpt_read_multipass**  
  Answers a question using a multi-pass approach by combining retrieval and chunked strategies.  
  - **Key Features:**  
    - **Dual-Pass Strategy:**  
      - **First Pass (Retrieval):** Extracts relevant sections from the document using a keyword-based retrieval approach.
      - **Second Pass (Chunked):** Processes the document in deep-thinking mode by querying each chunk.
    - **Merging Answers:**  
      - Merges the two answers using GPT to produce a final comprehensive answer.
      - Provides fallback concatenation if GPT-based merging fails.
    - **Parallel Processing:**  
      - Supports parallel processing for chunked operations via the `use_parallel` parameter.
    - **Chain-of-Thought Logging:**  
      - Optionally returns detailed processing logs and the entire chain-of-thought as a JSON string using the `return_json` parameter.

## packages.R

- **Package Installation and Loading**  
  Checks for and installs any missing required R packages before loading them silently.  
  - **Key Features:**  
    - Ensures that all necessary packages (e.g., shiny, future, pdftools, tesseract) are installed.
    - Loads packages in a way that suppresses startup messages.

## retrieval_mode.R

- **chunk_text_minimal**  
  Splits a document's text into as few chunks as possible without exceeding a given token limit.  
  - **Key Features:**  
    - First splits text into paragraphs (using double newlines) and then further splits by words if a paragraph exceeds the token limit.
    - Uses the helper function `estimate_token_count` to monitor token counts.
  
- **gpt_read_retrieval**  
  Retrieves an answer from a document using a GPT-based retrieval mode.  
  - **Key Features:**  
    - Combines all document chunks into a full text and checks if it fits within the allowed token limit.
    - If the full text exceeds the token limit, uses `chunk_text_minimal` to split the text into minimal chunks and performs targeted skimming on each chunk:
      - For each minimal chunk, a specialized prompt extracts only the information relevant to the question.
      - Skimmed responses are combined and then used to generate the final answer.
    - If the full text fits within the allowed token limit, applies a direct retrieval prompt to extract relevant text snippets.
    - Includes a fallback to generic chunked mode if no relevant text is extracted.
    - Optionally returns the entire chain-of-thought as a JSON string detailing all steps (prompts, responses, parameters).

## text_processing.R

- **filter_text**  
  Filters out irrelevant or boilerplate content from raw extracted text.  
  - **Key Features:**  
    - Removes page numbers, figure and table captions using regex patterns.
    - Eliminates URLs and email addresses.
    - Optionally removes an entire reference section starting from a heading.

- **chunk_text_naive**  
  Splits text into chunks using a naive paragraph grouping approach.  
  - **Key Features:**  
    - Splits text by blank lines (paragraphs) and combines them until the token limit (default 3000) is reached.
    - If a single paragraph exceeds the token limit, further splits it by individual words.

- **chunk_text_semantic**  
  Splits text into semantically coherent chunks using natural breakpoints.  
  - **Key Features:**  
    - Attempts to group related paragraphs together based on word overlap as a simple proxy for semantic similarity.
    - Ensures that each resulting chunk does not exceed the specified token limit (default 3000).

- **parse_text**  
  Reads and processes document files into cleaned text chunks.  
  - **Key Features:**  
    - Supports multiple file types: PDF, DOCX, TXT, and image files.
    - Performs OCR on scanned PDFs and image files when necessary.
    - Applies cleaning via `filter_text` and additional options to remove extra whitespace, non-alphanumeric characters, and numbers.
    - Splits the cleaned text into chunks using either naive or semantic methods based on the provided parameter.
    - Implements caching to optimize repeated parsing of the same document.
