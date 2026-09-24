# Package index

## Ask a question

Start here. One call reads a document and answers a question, and the
answer says what it rests on and whether anything went wrong.

- [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
  : Answer a question about a document
- [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  : Run several recipes over one document and compare them
- [`gr_recipes()`](https://elkronos.github.io/readgpt/reference/gr_recipes.md)
  : Ready-made recipes
- [`gr_recipe()`](https://elkronos.github.io/readgpt/reference/gr_recipe.md)
  : Bind an ingestion, segmentation and reading configuration together
- [`gr_answer`](https://elkronos.github.io/readgpt/reference/gr_answer.md)
  : The result of one reading run
- [`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md)
  : Did the model report that the document does not contain the answer?
- [`readgpt_example()`](https://elkronos.github.io/readgpt/reference/readgpt_example.md)
  : Path to the bundled example document

## Connect to a model

Clients for OpenAI-compatible endpoints, any provider ellmer supports,
any function you write, and an offline stand-in for practice and tests.

- [`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md)
  : Construct a model client
- [`gr_api_key()`](https://elkronos.github.io/readgpt/reference/gr_api_key.md)
  : Resolve the API key
- [`gr_ellmer_client()`](https://elkronos.github.io/readgpt/reference/gr_ellmer_client.md)
  : Read documents through an ellmer chat
- [`gr_backend_client()`](https://elkronos.github.io/readgpt/reference/gr_backend_client.md)
  : Use any function as the model transport
- [`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md)
  : A deterministic offline client for tests, demos and dry runs
- [`gr_call()`](https://elkronos.github.io/readgpt/reference/gr_call.md)
  : Call a model
- [`gr_result`](https://elkronos.github.io/readgpt/reference/gr_result.md)
  : The result of one model call
- [`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md)
  : List every known model
- [`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md)
  : Look up a model's capabilities
- [`gr_model_limits()`](https://elkronos.github.io/readgpt/reference/gr_model_limits.md)
  : Context and output limits for a model
- [`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md)
  : Register a model (or override a built-in entry)

## Get text out of files

Extraction, cleaning and a survey of a folder before anything is spent.
See
[`vignette("ingest")`](https://elkronos.github.io/readgpt/articles/ingest.md).

- [`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md)
  : Ingest a document into cleaned, provenance-bearing text blocks
- [`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md)
  : Describe an ingestion configuration
- [`gr_document`](https://elkronos.github.io/readgpt/reference/gr_document.md)
  : An ingested document
- [`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md)
  : List registered extractors
- [`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md)
  : List registered cleaners
- [`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md)
  : Run the cleaning pipeline over a character vector
- [`gr_inventory()`](https://elkronos.github.io/readgpt/reference/gr_inventory.md)
  : What is in a folder, before you read any of it

## Cut documents into chunks

- [`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
  : Segment a document into chunks
- [`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md)
  : Describe a segmentation configuration
- [`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md)
  : List registered segmentation strategies
- [`gr_chunks`](https://elkronos.github.io/readgpt/reference/gr_chunks.md)
  : A set of document chunks
- [`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md)
  : Summary statistics for a chunk set

## Read the chunks

The reading strategies, the settings that choose and place chunks, and
the check on quoted evidence. See
[`vignette("readers")`](https://elkronos.github.io/readgpt/articles/readers.md).

- [`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md)
  : Read chunks and answer a question
- [`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md)
  : Describe a reading configuration
- [`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md)
  : List registered reading strategies
- [`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md)
  : The traversal signature of a reader
- [`gr_embed()`](https://elkronos.github.io/readgpt/reference/gr_embed.md)
  : Embed texts
- [`gr_embedders()`](https://elkronos.github.io/readgpt/reference/gr_embedders.md)
  : List registered embedding backends
- [`gr_verify_evidence()`](https://elkronos.github.io/readgpt/reference/gr_verify_evidence.md)
  : Check that quoted evidence really is in the document

## Many documents

- [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
  : Ask one question of many documents

## Systematic and literature reviews

From a search export to screened studies, an extraction table, a written
review and an audit report, with the screening measured against a
hand-screened sample.

- [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md)
  : Write down what a review is looking for
- [`gr_protocols()`](https://elkronos.github.io/readgpt/reference/gr_protocols.md)
  : Protocols that ship with the package, and any you have registered
- [`gr_protocol_save()`](https://elkronos.github.io/readgpt/reference/gr_protocol_save.md)
  [`gr_protocol_read()`](https://elkronos.github.io/readgpt/reference/gr_protocol_save.md)
  : Save a protocol to a file, and read one back
- [`gr_register_protocol()`](https://elkronos.github.io/readgpt/reference/gr_register_protocol.md)
  : Register a protocol
- [`gr_search()`](https://elkronos.github.io/readgpt/reference/gr_search.md)
  : Record how the search was run
- [`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md)
  : Read a search export, and say what the search found
- [`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)
  : Decide which documents a review should read
- [`gr_reference()`](https://elkronos.github.io/readgpt/reference/gr_reference.md)
  : Draw a sample to screen by hand
- [`gr_calibrate()`](https://elkronos.github.io/readgpt/reference/gr_calibrate.md)
  : Measure the screener against a hand-screened sample
- [`` `[`( ``*`<gr_reference_frame>`*`)`](https://elkronos.github.io/readgpt/reference/sub-.gr_reference_frame.md)
  : Subsetting a reference frame gives a plain data frame.
- [`gr_fields()`](https://elkronos.github.io/readgpt/reference/gr_fields.md)
  : Build an extraction schema
- [`gr_field()`](https://elkronos.github.io/readgpt/reference/gr_field.md)
  : Describe one field of an extraction schema
- [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  : Extract a typed schema from many documents
- [`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
  : The claims a table of studies supports
- [`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
  : Derive a review's sections from its claims
- [`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md)
  : What a body of work does not cover
- [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
  : Write a review from an extraction table
- [`gr_flow()`](https://elkronos.github.io/readgpt/reference/gr_flow.md)
  : Count what happened to every document
- [`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md)
  : Write the run out as an auditable report

## Cost, tokens and tracing

- [`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)
  : Get or set package options

- [`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md)
  : Estimate the USD cost of a set of calls

- [`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md)
  : Count tokens in text

- [`gr_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_tokenizer.md)
  : The active tokenizer

- [`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md)
  : Register or inspect the active tokenizer

- [`gr_truncate_tokens()`](https://elkronos.github.io/readgpt/reference/gr_truncate_tokens.md)
  :

  Truncate text to at most `n` tokens

- [`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)
  : Compute a usable input-token budget for one model call

- [`gr_trace()`](https://elkronos.github.io/readgpt/reference/gr_trace.md)
  [`as.data.frame(`*`<gr_trace>`*`)`](https://elkronos.github.io/readgpt/reference/gr_trace.md)
  : Create a run trace

- [`gr_trace_summary()`](https://elkronos.github.io/readgpt/reference/gr_trace_summary.md)
  : Summarise a trace

- [`gr_trace_cost()`](https://elkronos.github.io/readgpt/reference/gr_trace_cost.md)
  : What a run actually cost

- [`gr_trace_save()`](https://elkronos.github.io/readgpt/reference/gr_trace_save.md)
  : Save a trace to a file

- [`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md)
  : Serialise an object to JSON

## Caching and replaying runs

- [`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
  : A response cache
- [`gr_cache_client()`](https://elkronos.github.io/readgpt/reference/gr_cache_client.md)
  : Attach a cache to a client
- [`gr_cache_stats()`](https://elkronos.github.io/readgpt/reference/gr_cache_stats.md)
  : Cache statistics
- [`gr_cache_clear()`](https://elkronos.github.io/readgpt/reference/gr_cache_clear.md)
  : Delete every entry in a cache
- [`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
  : A client that answers from a recorded run

## Extending readgpt

Every axis is a registry. An addition is used exactly like a built-in.

- [`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md)
  : Register a document extractor

- [`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md)
  : Register a cleaning step

- [`gr_register_segmenter()`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md)
  : Register a segmentation strategy

- [`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md)
  : Register a reading strategy

- [`gr_register_embedder()`](https://elkronos.github.io/readgpt/reference/gr_register_embedder.md)
  : Register an embedding backend

- [`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)
  :

  Build a `gr_chunks`, the object every segmenter must return

- [`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)
  :

  Build a `gr_answer`, the object every reader must return

## Superseded

The entry points of the first version. They still work and warn once;
new code should use
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md)
and
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md).

- [`answer_question()`](https://elkronos.github.io/readgpt/reference/answer_question.md)
  : Deprecated: answer a question using v1 mode names
- [`parse_text()`](https://elkronos.github.io/readgpt/reference/parse_text.md)
  : Deprecated: parse a document into text chunks
- [`gpt_read_chunked()`](https://elkronos.github.io/readgpt/reference/gpt_read_chunked.md)
  : Deprecated: chunk-by-chunk reading
- [`gpt_read_hierarchical()`](https://elkronos.github.io/readgpt/reference/gpt_read_hierarchical.md)
  : Deprecated: hierarchical reading
- [`gpt_read_multipass()`](https://elkronos.github.io/readgpt/reference/gpt_read_multipass.md)
  : Deprecated: multi-pass reading
- [`gpt_read_retrieval()`](https://elkronos.github.io/readgpt/reference/gpt_read_retrieval.md)
  : Deprecated: evidence-extraction reading
