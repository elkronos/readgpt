# Describe a reading configuration

Describe a reading configuration

## Usage

``` r
gr_read_spec(
  reader = "map_reduce",
  model = NULL,
  temperature = NULL,
  max_answer_tokens = 1500L,
  max_chunk_tokens = 700L,
  max_summary_tokens = 500L,
  top_k = 6L,
  min_score = -Inf,
  mmr = 1,
  context_order = c("relevance", "document", "edges"),
  rerank_candidates = 20L,
  rerank_min_score = 4,
  fan_in = 5L,
  max_levels = 5L,
  max_rounds = 4L,
  preview_tokens = 1200L,
  restate = c("auto", "always", "never"),
  members = NULL,
  cite = FALSE,
  skim_model = NULL,
  summary_model = NULL,
  parallel = NULL,
  delay_between_calls = 0,
  on_overflow = c("warn", "error"),
  ...
)
```

## Arguments

- reader:

  Reader name; see
  [`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md).

- model:

  Chat model id. `NULL` (the default) means the model of the client the
  spec is read with, so `gr_client(model = "gpt-4o-mini")` is the model
  that answers, is budgeted for and is billed. A model named here, or in
  a recipe, is used whatever the client's.

- temperature:

  Sampling temperature, or `NULL` to omit the field. You do not need to
  null it yourself for reasoning models: it is dropped automatically for
  any model whose registry entry has `supports_temperature = FALSE`,
  which includes the default model. Check with
  `gr_model_info(model)$supports_temperature`.

- max_answer_tokens:

  Output cap for final answers and merges.

- max_chunk_tokens:

  Output cap for per-chunk calls.

- max_summary_tokens:

  Output cap for summarisation calls.

- top_k:

  For `retrieve`, `rerank` and `iterative`: chunks to use.

- min_score:

  For `retrieve`: chunks scoring below this are dropped, but if that
  would leave nothing the single best chunk is used anyway. `-Inf`
  disables the filter. Not a cosine similarity when embeddings fall back
  to lexical vectors: the score is then a blend of cosine and BM25.

- mmr:

  Diversity of selection, for `retrieve` and `iterative`. `1` (the
  default) is plain top-k. Below 1, chunks are picked greedily by
  `mmr * relevance - (1 - mmr) * similarity to what is already picked`,
  so three chunks saying the same thing do not all get in and pay for
  each other. Costs nothing: the vectors are already computed. `0.7` is
  a reasonable place to start; `0` selects for novelty alone and will
  happily pick irrelevant chunks because they are different.

- context_order:

  Where the selected chunks sit in the prompt. `"relevance"` (default)
  is most relevant first; `"document"` restores the order they appear in
  the document, which reads better when chunks are consecutive;
  `"edges"` puts the strongest first and second-strongest last, burying
  the weakest in the middle, because transformers attend measurably
  better to the beginning and end of a long context than to its middle.
  Selection is unaffected. This decides only placement, and it applies
  to `retrieve` and `rerank`, the two readers that put several ranked
  chunks in one prompt.

- rerank_candidates, rerank_min_score:

  For `rerank`: how many chunks to score, and the score below which a
  chunk is discarded. The candidates are the chunks the word-matching
  prefilter ranks highest. When no chunk shares a word with the
  question, it cannot rank them, so they are picked by embedding
  similarity instead, or, when embeddings cannot rank them either,
  spread evenly over the document and the answer marked partial. Either
  way the run warns (`gr_rerank_prefilter`) and `notes$prefilter` says
  which.

- fan_in, max_levels:

  For `hierarchical`: summaries combined per call, and the recursion
  depth cap.

- max_rounds:

  For `iterative`: retrieve-assess cycles.

- preview_tokens:

  For `preview`: the cap on the outline the planner sees. The outline is
  built from section labels, sizes and short excerpts (never the full
  text), and per-section excerpts shrink until the whole thing fits, so
  every section stays visible to the planner rather than the outline
  being truncated and some sections never being offered to it at all.
  The planner is an LLM call about a long document and so prone to
  exactly the degradation this package manages; keeping its input small
  is the mitigation.

- restate:

  Whether to repeat the question before the excerpts as well as after
  them: `"auto"` when the body is long enough to bury the first ask,
  `"always"`, or `"never"`. A setting rather than a rule, because
  whether it helps is a question about your corpus and your model. Point
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  at two recipes differing only in this and find out.

- members:

  For `ensemble`: the reader names to combine.

- cite:

  Ask for chunk-level citations (`[chunk 3]`) in the answer. Map those
  ids back to pages via `ans$evidence`. Forced off for `hierarchical`,
  which answers from summaries: summaries carry no `[chunk N]` ids, so
  asking for citations there asks the model to invent them. For `skim`,
  when its evidence has to be consolidated to fit and no `[chunk N]`
  label survives the consolidation, citations are not asked for either
  (`notes$cite_dropped = TRUE`); when some survive, a citation is
  checked against those.

- skim_model, summary_model:

  Optional cheaper models for the per-chunk stages. `skim_model` is used
  by `skim`'s extraction **and** `rerank`'s relevance scoring;
  `summary_model` by `hierarchical`'s summarisation.

- parallel:

  Run per-chunk calls in parallel. Needs the `future` and `future.apply`
  packages; without them the run is sequential and says so.

  The trace is complete either way: workers keep their own and the
  parent absorbs them, in input order, so a parallel run reports the
  same calls, tokens and cost as the same run made sequentially. So is
  what the client keeps: a mock's or backend's `$calls()` and
  `$embeds()`, and the hit, miss and write counts of an attached cache,
  include the calls made in workers, added back in input order. A replay
  client always runs one request at a time, since it hands out its
  recording in order.

  Two things do not cross the process boundary. The limits in
  [`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)
  are checked before a batch is sent and not inside it, since a worker
  cannot see what the others spend. So the pre-flight check, which runs
  in the parent, holds a parallel run to its worst case, every reply at
  its cap and as many merge or summary levels as replies that size need,
  against both `max_calls` and `max_cost_usd`; and each batch goes to
  the workers only when it fits what the run has left at its own worst
  case, and otherwise runs one request at a time, each checked. And a
  handler that records calls in variables of its own (a closure passed
  to
  [`gr_backend_client()`](https://elkronos.github.io/readgpt/reference/gr_backend_client.md)
  or
  [`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md))
  records a worker's calls in that worker's copy, which this process
  never sees; ask the trace, or the client's `$calls()`, instead.

- delay_between_calls:

  Seconds to sleep between sequential calls, for rate-limit shaping.
  Honoured by `map_reduce`, `refine` and `skim`; the other readers do
  not sleep.

- on_overflow:

  For `stuff`: `"warn"` (truncate and say so) or `"error"`.

- ...:

  Extra fields for custom readers.

## Value

A list of class `gr_read_spec`.

## Out-of-range values

Numeric arguments are clamped into a usable range and the change is
warned about, never applied silently: `top_k` \[1, 1e4\], `fan_in` \[2,
32\], `max_levels` \[1, 12\], `max_rounds` \[1, 20\],
`rerank_candidates` \[1, 1e4\], `rerank_min_score` \[0, 10\],
`preview_tokens` \[100, 1e5\], `delay_between_calls` \[0, 600\], token
caps \[16, 1e6\].

## See also

[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md)
for the available readers and their call costs,
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`gr_recipe()`](https://elkronos.github.io/readgpt/reference/gr_recipe.md)

Other reading functions:
[`gr_answer`](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md),
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md),
[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)

## Examples

``` r
gr_read_spec("retrieve", top_k = 8)$top_k
#> [1] 8

# Out-of-range settings are corrected loudly, not quietly.
suppressWarnings(gr_read_spec("hierarchical", fan_in = 999)$fan_in)
#> [1] 32
```
