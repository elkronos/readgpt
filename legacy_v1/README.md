# gpt_read: A Document Analysis and Question Answering Framework

gpt_read is a versatile tool that leverages GPT models to answer questions based on document content. It is designed to process documents, extract key information, and generate responses tailored to user queries.

## Overview

The repository implements several strategies for document processing, including direct text retrieval, chunk-based processing, semantic segmentation, hierarchical summarization, and multi-pass refinement. These approaches allow users to choose a method that best fits the document type and question complexity.

## Key Features

- **Multiple Processing Methods:**  
  Supports various [strategies](https://github.com/elkronos/gpt_read/blob/main/R/README.md) such as [retrieval](https://github.com/elkronos/gpt_read/blob/main/R/retrieval_mode.R), [chunking](https://github.com/elkronos/gpt_read/blob/main/R/chunked_mode.R), [hierarchical summarization](https://github.com/elkronos/gpt_read/blob/main/R/hierarchical_mode.R), and [combined multi-pass](https://github.com/elkronos/gpt_read/blob/main/R/multi_pass_mode.R) approaches, all callable from the [main.R](https://github.com/elkronos/gpt_read/blob/main/R/main.R) allows users the flexibility of handling diverse documents.

- **Broad Format Support:**  
  Works with PDFs, DOCX, TXT, and images (with OCR), making it applicable across a wide range of document formats.

- **Efficiency and Accuracy:**  
  Automates the extraction of relevant content, reducing manual review time while enabling users to compare different methods to achieve the best results.

- **Scalability:**  
  Incorporates parallel processing and dynamic chunking, ensuring it can manage documents of varying sizes and complexities.

## How It Works

The system reads and processes documents by splitting them into manageable segments. It then applies the selected processing strategy to extract and synthesize the information needed to answer the user's query. The result is an answer that reflects the key content of the document, with options for further refinement.

## Prerequisites

This system assumes you have a valid API key with OpenAI. To obtain an API key:
- Visit [OpenAI's API page](https://openai.com/api/).
- Register for an account and follow the instructions to generate your API key.
- Configure your environment to use this key as required by the system.
- The Shiny application will automatically prompt you for the key.

## Getting Started

1. **Select Document(s):**  
   Place your document in the designated folder. Supported formats include PDF, DOCX, TXT, and images (with OCR).

2. **Select Processing Method(s):**  
   Choose from one of the available strategies based on your document and the nature of your question. See the 'R' folder [README](https://github.com/elkronos/gpt_read/blob/main/R/README.md) for more details about each method.

3. **Ask Your Question:**  
   Use the provided functions to ask a question. The system will process the document and return a generated answer.

This framework offers a clear, systematic approach to document analysis, enabling efficient extraction of relevant information from a variety of sources.

## Repo Background and Purpose

This repository expands on the original [gpt_read](https://github.com/elkronos/openai_api/blob/main/assistants/gpt_read.R) function. Initially, gpt_read split text into chunks and queried GPT with each chunk plus a user question, then consolidated the responses. This method, designed for smaller context windows and high hallucination rates, was slow and API-intensive.

Inspired by projects like [LangChain](https://www.langchain.com/), I sought more control over how GPT ingests, extracts, and parses information. As API costs fell and context windows grew, I experimented with methods that mimic human reading: first skimming to identify key points, then revisiting details. For example, while `chunked_mode.R` refines the original approach, the retrieval method first filters relevant text before querying GPT, aiming for greater accuracy.

This seems too niche for a lot of people, but I am sharing should someone find they'd like to experiment with these sort of methods too.
