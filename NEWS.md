# readgpt 0.4.0 (in development)

Reading a corpus for an answer and reading it for a *table* are different jobs.
This release is the second one.

## New

* **Extraction schemas.** `gr_fields()` describes what you want out of a
  document as typed fields rather than as a sentence — `gr_field("Number of
  participants randomised, not the number analysed", type = "integer")` — and
  `gr_extract()` applies one schema to a whole corpus, returning one tidy row
  per document and one column per field, of that field's type.

  The point is joinability. A paragraph about one paper cannot be compared with
  a paragraph about two hundred others; a table can be sorted, counted,
  filtered and published.

* **A new reader, `extract`.** Traversal signature `all|N+conflicts|none`:
  every chunk is asked to fill what it can and to leave the rest null, and the
  per-chunk answers are then reconciled. Reconciliation is arithmetic when the
  chunks agree, so it costs a call only where a document genuinely contradicts
  itself — `resolve = "model"` adjudicates those, `resolve = "first"` (the
  default) takes the earlier value and records the disagreement.

* **Every filled cell carries its provenance.** Each value is asked for the
  sentence it came from, and that sentence is checked against the chunk it was
  attributed to. `$evidence` is the long form of that — one row per supported
  cell, with `verified` and `match` — and `n_unverified` in the table counts the
  values that could not be tied to a verbatim span, whether because no quote was
  given or because the quote is not in the document. Nothing is discarded for
  failing; `require_quote = TRUE` makes it a policy for a protocol that needs
  one.

* **Protocols.** `gr_protocol()` writes down the three decisions a review must
  not make while it reads: which documents count (`include`/`exclude`), what to
  collect from them (a `gr_fields()` schema), and what the write-up has to cover
  (`outline`). Not because a model cannot infer them, but because a criterion
  invented while reading is a criterion fitted to what was found.

  It is the seventh registry: `gr_protocols()` lists what is available,
  `gr_register_protocol()` adds your own, and `gr_protocol_save()` /
  `gr_protocol_read()` round-trip one through a JSON file so it can be shared,
  diffed and cited alongside the results. Three templates ship —
  `bibliography`, `evidence_table` and `systematic_review` — as starting points,
  not standards: the package knows the shape a protocol has, not what your
  criteria should be.

  `gr_extract()` takes a protocol wherever it takes a schema, using its question
  and recipe unless you say otherwise.

* **Screening.** `gr_screen()` is the stage before extraction: one model call
  per document, a decision and a reason for every one, and nothing dropped on the
  way. Two hundred candidate papers extracted in full is two hundred times twenty
  calls; screened, it is two hundred calls, and most of them end the document's
  involvement.

  Three properties it is built around. Every document gets a decision — there is
  no retrieval step or relevance prefilter that could quietly remove a source
  before one is recorded, and a document that could not be read is
  `status = "failed"` with no decision rather than a silent absence. `"unclear"`
  is an answer, not a failure: forcing a binary decision out of an excerpt that
  does not settle the question is how automated screening loses studies, and
  those documents are for a person. And every decision names the criterion that
  produced it, because `table(x$table$criterion)` is what a flow diagram asks
  for.

  `screen_tokens =` caps what the model is shown, counted from the start of the
  document, so title-and-abstract screening is available deliberately rather than
  by accident; `truncated` and `seen_tokens` always say what was actually read.
  `x$included` is the argument to hand to `gr_extract()`.

  The reader behind it is `screen`, traversal `head|1|none`.

* **The same document is read once.** A source whose cleaned text repeats one
  already read this run is not read again: its row is filled in from the first
  copy, `status` is `"duplicate"` and the new `duplicate_of` column names the
  row it repeats. Nothing is dropped — every source you passed still has a row —
  so `subset(x$summary, is.na(duplicate_of))` is the deduplicated set and
  `sum(!is.na(x$summary$duplicate_of))` is the number to report as removed.

  A response cache already made the second copy's calls free. What it could not
  do was stop the duplicate appearing in the results as a second, independent
  document, which is how one study gets counted twice in a synthesis. The hash
  travels in the `store`, so a resumed run does not pay to rediscover it.

* **A citable document id.** `document_id` is the hash of a document's cleaned
  text, in the corpus summary, the extraction table and the evidence table.
  `document` is a filename: it changes when the file is renamed, collides
  between folders, and does not exist for a document passed as text. The id is
  the same string for the same document in every run and on every machine — and
  identical for two copies of it, which is the same fact as the duplicate
  detection above.

## Fixed

* **A quote now cites the page it is actually on.** A chunk is packed from
  several units, and a chunk packed from two pages reported the *first* one —
  right for its opening sentence, wrong for everything after it, and wrong in
  the most expensive way, because a citation that names a page is checked by
  turning to that page. Two changes: a chunk whose units disagree about the page
  (or the section) now reports `NA` rather than picking one, and an evidence span
  is located in the document's own blocks, so it gets the page of the block that
  contains it rather than the page of the chunk that carried it. A span found on
  several pages, or not found at all, is left alone — the first hit would be a
  guess dressed as a fact.

  Same for a runt paragraph absorbed into the previous chunk, which used to keep
  only the host's page.

## Behaviour

* `gr_call_json()` gains `allow_empty`, used only by `extract`, where a JSON
  object with no keys is a real answer — this excerpt supports none of the
  fields — rather than a broken one.

* A field cannot be named `document`, `document_id`, `status`, `duplicate_of`,
  `error`, `n_filled`, `n_unverified`, `conflicts`, `field`, `chunk_id`, `page`,
  `section`, `quote`, `verified` or `match`: those are the extraction table's own
  columns, and a field with one of those names would be silently overwritten.
  Nor may a name end in `__quote`, which collides with the companion span every
  field gets.

* `gr_read_many()`'s summary gains `document_id` and `duplicate_of`. A `store`
  written by 0.3.0 still resumes; its rows have neither, and both are filled
  with `NA` rather than being invented.

* A field a document does not report comes back `NA` with `status` `"ok"`, not
  `"failed"`. "Not reported" is a finding; the `status` column is what separates
  it from a document that was never read.

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

* **Quoted evidence is checked against the document.** `ans$evidence` is what an
  answer rests on, and for most readers those spans are verbatim chunk text --
  true because the package put them there. For `skim` they are what the model
  wrote when asked to extract the relevant passages: *presented* as quotations,
  with nothing checking that they were.

  Now they are checked, on every run. A span that is not in the chunk it is
  attributed to sets `notes$unverified_evidence` and makes the answer `partial`,
  like any other degradation. `gr_verify_evidence()` reports the detail:
  `chunk_id`, `kind`, `verified`, `match` and the span.

  The comparison is forgiving about typography and unforgiving about content.
  Whitespace, curly quotes, dashes, case and the punctuation a model wraps a
  quotation in are folded away, because none of that is fabrication and flagging
  it would make `partial` mean nothing. A changed number is not folded away.
  Below an exact match, `match` is the fraction of the span carried by its
  longest consecutive run in the source -- a run measure rather than word
  overlap, because overlap cannot tell a quotation from a paraphrase built out
  of the same vocabulary, which is the whole distinction.

  Citations are checked too, for every reader: an answer citing a chunk that was
  never sent to it sets `notes$cited_unknown` and is `partial`. A fabricated
  citation is more convincing than a fabricated answer, because it looks like
  the thing that would let you check.

  Both checks are local string operations on text already in hand. They cost
  nothing, so there is no option to turn them off.

* **A vignette**, `vignette("readgpt")`. It walks through the three axes and the
  decision each one represents, then through comparing recipes, the cost rails,
  caching, replay and reading a corpus. It builds against `gr_mock_client()` with
  `gr_options(embedder = "lexical")`, so it compiles offline, deterministically
  and with no API key -- on your machine and on CRAN's alike.

* **`mmr` and `context_order` on the read spec**, both off by default.

  `mmr` below 1 selects chunks by maximal marginal relevance -- relevance traded
  against redundancy with what is already selected -- so three paragraphs saying
  the same thing do not take all three top-k slots and pay for each other. It
  costs nothing, the vectors are already computed, and it applies to `retrieve`
  and `iterative`. `mmr = 1` is exactly top-k, bit for bit.

  `context_order` decides where the selected chunks sit in the prompt:
  `"relevance"` (default), `"document"`, or `"edges"`, which puts the strongest
  first and second-strongest last and buries the weakest in the middle, because
  transformers attend measurably better to the beginning and end of a long
  context than to its middle. Selection is unaffected -- this is placement only,
  for `retrieve` and `rerank`. It is not the primacy/recency effect it
  resembles: those come from rehearsal and interference in human memory, which a
  transformer does not have.

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

* **A missing or unusable setting falls back to its documented default**, rather
  than to whichever end of its range happens to be the lower bound.
  `gr_read_spec(max_answer_tokens = NA)` used to give 16 -- truncating every
  answer -- and `top_k = NA` gave 1, because `clamp()` maps an unusable value to
  `lo`. That is right for a bound and wrong for a setting. Applies to `mmr`,
  `top_k`, the three token caps, `rerank_candidates`, `rerank_min_score`,
  `fan_in`, `max_levels`, `max_rounds` and the segmenter's `max_tokens`, and to
  anything unusable, not only `NA`.

* **An embedding that *fell back* now sets `partial`.** The documentation says
  to check `partial` before trusting an answer and lists a lexical fallback as a
  degradation; it was recorded in `$notes` but not in the one flag readers are
  told to look at. A fallback is a degradation, and `gr_options(embedder =
  "lexical")` is a choice -- the two are now distinguished, and only the first
  sets the flag.

* **A citation of a chunk that was sent but did not contribute is no longer
  reported as a fabrication.** `notes$cited_unknown` compared citations against
  `chunks_used`, which for a per-chunk reader holds only the chunks that
  *answered* -- so a model faithfully citing a chunk that had replied "not in
  this excerpt" was flagged as inventing it, and the answer was silently
  downgraded to `partial`. A false positive in a hallucination check is the one
  place a false positive is least affordable.

* **`context_order = "document"` was a silent no-op below three chunks** -- the
  common case for a top-k reader. Only `"edges"` needs a middle to bury the
  weakest chunk in.

* **Warnings raised while evaluating a setting were silently swallowed.**
  `clamp()` did `x <- suppressWarnings(as.numeric(x))`, and `x` arrives as a
  promise -- so forcing it inside `suppressWarnings()` discarded everything
  raised while *evaluating the argument*, not merely the coercion warning that
  call exists to quiet. Every `clamp(f(...))` in the package lost `f()`'s
  warnings, across `gr_read_spec()`, `gr_segment_spec()` and `gr_ingest_spec()`.
  The argument is now forced first. Present in 0.2.0.

* **`gr_options()` did not compose with `on.exit()`, which is the one thing its
  documentation promised.** The "old" value it returns was built with
  `modifyList()`, which deletes a key whose value is `NULL` -- so once an option
  had been *stored* as `NULL`, which is exactly what restoring a saved list does
  for `temperature`, `max_cost_usd`, `cache_dir` and `embedder`, the returned
  name came back as `NA` and the next `gr_options(old)` failed with
  "Unknown option(s): NA". The second use of the documented pattern, in a
  function already fixed once for this same `NULL` trap in its setter.

* `ensemble` combined its members' evidence with a plain `rbind()`, which
  requires every member to produce the same columns. It does not -- only readers
  whose evidence is model-written carry verification columns -- so an ensemble of
  `skim` and `map_reduce` failed with "numbers of columns of arguments do not
  match" the moment verification was added. Evidence tables are now unioned.

* `gr_call()` had two exits, each recording its own trace entry. Anything that
  needed to sit between a request and its response had to be written twice and
  kept in step by hand. It now dispatches once and records once, and the
  normalisation that enforces the `gr_result` invariants on whatever a handler
  returned is shared by the mock and backend paths rather than duplicated.

## Behaviour changes

* `gr_trace_summary()` returns an extra `cached` column, between `calls` and
  `steps`. Code that indexes its result by position rather than by name will
  need updating.

* `min_score` is applied to `retrieve` *before* selection rather than after.
  Applied after, a chunk below the floor could displace one above it in the
  top-k and then be dropped, quietly returning fewer chunks than `top_k` asked
  for and giving no way to see why. A run with a finite `min_score` may now use
  more chunks than it did.

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
