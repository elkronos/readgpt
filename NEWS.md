# readgpt 0.3.0

Two additions, one theme: the model call is the only part of this package that
costs money or fails to repeat itself, and neither of those had to be true
twice.

## New

* **A response cache.** `gr_cache()` stores each successful model response,
  keyed on the exact request, and `gr_cache_client()` attaches it to any
  client. A repeated request is free and byte-identical, so iterating on a
  prompt, resuming after a crash, or re-running an analysis whose last step
  changed no longer re-bills every earlier call. The key covers the messages,
  model, output cap, temperature, JSON schema, API shape and base URL — and
  nothing else, because nothing else reaches the model. Failures are never
  cached: a rate limit or a refusal is a property of the moment, and storing one
  would make a blip permanent. `gr_cache_stats()` and `gr_cache_clear()` report
  on and empty a cache.

  The default directory is under `tempdir()`, so a cache costs nothing and
  disappears with the session; set `gr_options(cache_dir = ...)` to keep entries
  across sessions and make a long run resumable.

* **An embedder registry.** `gr_register_embedder()` and `gr_embedders()` make
  embedding the sixth registry, alongside extractors, cleaners, segmenters,
  readers and models -- it was the one axis that was a chain of `inherits()`
  branches, so adding a local model meant editing the package and there was no
  way to ask what was available. `gr_options(embedder = )` switches every part
  of the package that embeds; two built-ins are registered, `"api"` and
  `"lexical"`.

  The registry carries something a branch cannot: whether an embedder is
  **deterministic**. That closes the replay gap. A replay now reproduces a run's
  chunk ranking exactly when the recording used a deterministic embedder *and*
  the replay uses the same one -- both conditions, checked against the embedder
  the trace actually recorded. Determinism alone is not enough: replaying an
  API-embedded run with a deterministic local embedder would compute vectors the
  original never saw while reporting itself exact.

  The embedding cache key now includes the embedder. Without it, switching
  embedders returned the previous one's vectors for the same text and model --
  two vector spaces silently mixed in one matrix, and a cosine similarity across
  them means nothing.

* **One question, many documents.** `gr_read_many()` runs one recipe over a
  vector of files, or a directory expanded by the extractor registry, and
  returns one tidy row per document -- `answer`, `not_found`, `partial`,
  `chunks_used`, `calls`, `cached`, tokens, `cost_usd`, `seconds`, `status` and
  `error`. It is the counterpart to `gr_compare()`, which runs several recipes
  over one document.

  It does four things a loop does not. One unreadable document costs one row
  rather than the run. Budgets are per document -- each gets its own trace, so
  `max_calls` applies as if it had been read alone and one enormous file cannot
  starve the rest -- under an optional corpus-wide `max_total_usd` ceiling, past
  which documents are marked `"skipped"` rather than quietly dropped. A `store =`
  directory makes a run resumable: each result is written as it completes and
  restored later, keyed on the document's path, size and mtime, the question,
  the whole pipeline and the model, so an edited document is a new job and not a
  stale hit. And every document's cost is recorded.

* **`gr_trace_cost()`** prices a run using each step's own model and counts only
  the calls that were actually issued -- a call served from a cache or a replay
  spent nothing however large its prompt was. One row per model; an unpriced
  model contributes `NA` rather than zero, so a total cannot quietly omit it.
  This is the missing half of the `cached` accounting added alongside the cache:
  token totals describe a run's shape, and this describes its bill.

* **A swappable transport.** `gr_backend_client()` makes any function of
  `(messages, params)` the model transport, and `gr_ellmer_client()` plugs in an
  `ellmer` chat -- so Anthropic, Google, Bedrock, Azure, Ollama and Hugging Face
  work with every reading strategy here. Nothing about ingesting, segmenting or
  reading a document depends on one HTTP dialect, and the package should not be
  reimplementing a transport layer R already has. Everything built around the
  call is unchanged: context budgeting, cost and call rails, provenance, traces,
  caching, replay and `gr_compare()`.

  Backends may also supply an embedding function; without one, readers that
  embed fall back to lexical vectors and warn (`gr_backend_no_embeddings`). A
  supplied embedder is checked for one row per input before its output reaches
  the ranking maths -- a wrong-shaped matrix would associate every chunk with
  another chunk's vector and the answer would look fine.

  Two ellmer limits are documented rather than papered over: sampling parameters
  belong to the chat object, so a per-call `temperature` is ignored and warned
  about once (`gr_ellmer_temperature`); and each call runs against a fresh deep
  clone with its turns cleared, so no history leaks between chunks and the
  caller's chat is never mutated.

* **Reproducible replay.** `gr_trace_save()` writes a run's trace to a file
  and `gr_replay_client()` answers from it, so a recorded run can be re-run
  exactly by someone with no API key and no budget. A trace already held every
  prompt and every response; it was write-only. Now a published result is
  something a reader can check rather than trust, and a bug report can be a
  re-runnable recording instead of a description.

  A prompt with no recorded response raises `gr_replay_miss` rather than
  inventing an answer — silent divergence would produce a result that looks like
  the original and is not. Embeddings are not model calls and are not recorded,
  so `retrieve` and the `semantic` segmenter fall back to lexical vectors under
  replay and warn (`gr_replay_no_embeddings`).

* **Cached calls are accounted separately.** `gr_trace` gains a `cached`
  counter, and `gr_trace_summary()` a `cached` column, so `calls - cached` is
  what a run actually paid for. `gr_result` gains `$cached`. Without this a
  fully cached run reported the same token totals as a fully paid one and
  `gr_estimate_cost()` quietly overstated the bill.

## Fixes

* `gr_cache_clear()` was two different functions. An internal helper of that
  name in `R/core-state.R` cleared the in-memory document and embedding caches,
  and R collates that file after `R/core-cache.R`, so the internal definition
  silently replaced the exported one — the package would have shipped the wrong
  implementation under the right documentation. The internal helper is now
  `gr_flush_caches()`, and a test asserts the shadowing has not returned.

* **A trace could not survive being written to a file.** `jsonlite` escapes
  bytes it cannot interpret in the *current locale*, so an unmarked string
  holding UTF-8 bytes came out of `as_json()` as the literal text
  `"caf<c3><a9>"` on any machine whose locale is not UTF-8. The bytes were
  always right; nothing had told R what they were. `trace_record()` now labels
  prompt, response and error text -- `mark_utf8()`, which labels and converts
  nothing, never `enc2utf8()`, which corrupts valid UTF-8 in exactly this
  situation. This was present in 0.2.0 and is the same defect class as the
  locale-dependent tokenizer fixed there: correct bytes, absent label, one
  locale in the test matrix.

* **`gr_result$text` is now always labelled.** An unlabelled response was at the
  mercy of the session locale the moment anything serialised, compared or
  counted characters in it, so two runs that produced identical bytes could
  compare unequal. Both cache and replay keys normalise text the same way, which
  is what lets a saved trace replay on a different machine.

* `gr_call()` had two exits, each recording its own trace entry. Anything that
  needed to sit between a request and its response had to be written twice and
  kept in step by hand. It now dispatches once and records once, and the
  normalisation that enforces the `gr_result` invariants on whatever a handler
  returned is shared by the mock and backend paths rather than duplicated.

## Behaviour changes

* `gr_trace_summary()` returns an extra `cached` column, between `calls` and
  `steps`. Code that indexes its result by position rather than by name will
  need updating.

# readgpt 0.2.0

A rewrite. The package is now a proper R package with three independent,
registry-based axes — **ingest**, **segment**, **read** — instead of five
entangled "modes". Old entry points still work and warn once.

## Why the rewrite

The five reading modes were not five methodologies. `Chunked` and `Semantic`
called the same function with the same arguments on the same chunk object and
returned byte-identical answers while billing twice; `MultiPass` re-ran two of
the others verbatim; `Hierarchical` was single-level map-reduce that overflowed
the context window past roughly 32 chunks. The "semantic" ordering came from
`set.seed(nchar(text)); runif(768)`, so the embedding was a function of string
length alone and two 48-character strings scored a cosine similarity of 1.0.

Distinctness is now a property the package can **check**. Every reader declares
a traversal signature (`select|calls|state`), and `gr_compare()` refuses to bill
you twice for two recipes that resolve to the same work.

## New

* **Three axes, each a registry.** 6 extractors, 14 individually toggleable
  cleaners, 9 segmenters and 9 readers, in any combination. `gr_recipe()` binds
  one of each; `answer_document()` runs one; `gr_compare()` runs several over
  one document and reports how they differ.
* **A public extension API.** `gr_register_extractor()`, `gr_register_cleaner()`,
  `gr_register_segmenter()`, `gr_register_reader()` and `gr_register_model()`,
  with `new_chunks()` and `new_answer()` to build what a custom segmenter or
  reader must return. Additions get the same token-cap enforcement, provenance
  handling, cost caps and reporting as the built-ins.
* **Overlap and minimum chunk size**, on every segmenter. The previous release
  had neither, so an answer straddling a boundary was lost by both chunks.
* **Provenance.** Extraction returns blocks carrying page, section and block id,
  and that survives cleaning and chunking, so `ans$evidence` can point back at
  where an answer came from.
* **A run trace.** One trace per run records every prompt, response, token count
  and local step, so the trace always explains the answer next to it — the
  previous Shiny app ran the pipeline twice per question and displayed the
  reasoning of a *different* generation.
* **Pre-flight cost and call caps**, checked before the first request:
  `gr_options(max_cost_usd =, max_calls =)`.
* **A pluggable, script-aware tokenizer**, biased to over-count, with an exact
  `tiktoken` backend when reticulate and Python `tiktoken` are available.
* **A data-driven model registry** with explicit match precedence and an `as_of`
  stamp, extensible with `gr_register_model()` for compatible endpoints.
* **`gr_chunk_stats()`**, so you can compare chunkings for free before spending
  anything on reading.

## Behaviour changes that will alter results

Three are deliberate. If you need the old behaviour for comparison, the
`"legacy"` recipe reproduces it.

* `mode` no longer defaults to running all five modes. `answer_question(f, q)`
  ran 41 API calls; it now runs one pipeline.
* Modes no longer share a chunk object, so selecting a second one cannot change
  the first one's answer.
* **Digits are no longer stripped from documents by default.** `remove_numbers`
  was `TRUE` and unreachable from the public entry point, so every figure, date
  and percentage was deleted before the model saw the document.

## Deprecated

`answer_question()`, `parse_text()`, `gpt_read_chunked()`,
`gpt_read_retrieval()`, `gpt_read_hierarchical()` and `gpt_read_multipass()`
still work and warn once per session. Each help page names its replacement.

## Fixes

Too many to list individually; `REVIEW.md` documents each with a reproduction.
The ones most likely to have affected real runs:

* An unrecognised model id produced a **negative** token budget, which turned a
  10,000-word document into roughly 10,001 API calls and fed it to the model
  backwards. `gr_budget()` is now the single arithmetic chokepoint and cannot
  return a non-positive value; it raises an actionable error instead.
* `grepl("gpt-4", model)` matched `gpt-4o`, treating a 128k-context model as
  8k and over-chunking by ~26x.
* The boilerplate filters were dead code: digit removal ran before the
  page/figure patterns that need digits to match. Cleaning steps are now staged
  so structure-aware steps always precede destructive ones.
* The reference-section cleaner deleted a body sentence and kept the whole
  bibliography.
* The ingestion cache key was the file path alone, so the first settings used in
  a session won for the rest of it — which made fine-grained control, the point
  of the package, unreachable.
* A failed API call was spliced into the next prompt as the literal string
  `"NULL"`; an empty completion crashed every mode.
* `refine = TRUE` could never work: it called a function that was not defined
  anywhere in the repository.
* The Shiny app stored the API key process-wide, so two browser sessions in one
  R process billed each other, and exposed the entire filesystem to the browser.
* **The same document produced different results on different machines.**
  `enc2utf8()` treats an *unmarked* string as native, so in a non-UTF-8 locale
  it re-encoded bytes that were already valid UTF-8. Downstream, the ligature
  and smart-quote cleaners stopped matching and `utf8ToInt()` fell back to
  counting bytes -- charging a one-codepoint em dash as three characters, a 50%
  token swing on the affected line. Token counts, chunk boundaries, budgets and
  cost estimates all varied with the locale R happened to start in. Text is now
  labelled before any conversion, and a test asserts that ingestion and
  segmentation are byte-identical under a C locale.
* `answer_document()` and `gr_compare()` called `basename()` on whatever was
  passed as `source`. Raw document text is a length-1 character vector, so the
  whole document went to `basename()` -- which is bounded by `PATH_MAX` and
  warns past it on macOS (1024 bytes, so most documents), and which put a
  mangled fragment of the document into the trace where the filename belongs.

## Testing

627 tests, all offline against a recording mock client, plus GitHub Actions
running `R CMD check` on three platforms and a fast suite that also executes
every README code block and diffs its documented output against real output.
