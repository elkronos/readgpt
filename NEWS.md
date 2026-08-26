# gptread 0.2.0

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
`"legacy_v1"` recipe reproduces it.

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
