# readgpt: control how a language model reads a document

Document question answering, with the three decisions that actually
drive answer quality pulled apart into independent, swappable axes:

## Details

- **ingest**:

  bytes to clean text: which extractor, which cleaning steps. See
  [`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
  [`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
  [`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md);
  it returns a
  [gr_document](https://elkronos.github.io/readgpt/reference/gr_document.md).

- **segment**:

  text to chunks: where the boundaries fall, how big, how much overlap.
  See
  [`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
  [`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md),
  [`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md);
  it returns
  [gr_chunks](https://elkronos.github.io/readgpt/reference/gr_chunks.md).

- **read**:

  chunks to an answer: which chunks reach the model, in what call
  pattern. See
  [`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
  [`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
  [`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md);
  it returns a
  [gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md).

Any ingest x any segmenter x any reader composes.
[`gr_recipe()`](https://elkronos.github.io/readgpt/reference/gr_recipe.md)
binds one of each into a named pipeline;
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
runs one;
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
runs several over one document and reports how they differ.

## Getting started

    Sys.setenv(OPENAI_API_KEY = "sk-...")

    # No key yet? Everything below works offline against gr_mock_client() and the
    # bundled document at readgpt_example().

    # One question, one pipeline.
    ans <- answer_document("report.pdf", "What was Q3 revenue?", recipe = "needle")
    ans$answer
    ans$partial          # TRUE means something degraded; check this first

    # Which pipeline suits this document? Compare, then commit.
    cmp <- gr_compare("report.pdf", "What was Q3 revenue?",
                      c("fast", "needle", "thorough"))
    cmp$summary

## Choosing a strategy

|                                            |                |
|--------------------------------------------|----------------|
| **your document**                          | **start with** |
| fits in one context window                 | `"fast"`       |
| one fact buried in a long report           | `"needle"`     |
| needs every mention found                  | `"thorough"`   |
| long, with headings, needs a synthesis     | `"survey"`     |
| scanned PDF, forms, invoices               | `"scanned"`    |
| an argument that develops across the text  | `"narrative"`  |
| multi-hop question over a paper            | `"research"`   |
| high stakes, want cross-checking           | `"consensus"`  |
| short, and you want every sentence weighed | `"precise"`    |

`"legacy"` is the tenth: it reproduces the previous release's ingestion
and chunking deliberately, so a change in behaviour can be measured
against old results rather than assumed.

See
[`gr_recipes()`](https://elkronos.github.io/readgpt/reference/gr_recipes.md)
for what each one actually configures. When in doubt,
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
two or three on your own document and read `cmp$summary`.

## Cost control

Two limits are on by default, both
[`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md):
`max_cost_usd` (5) and `max_calls` (400). Before the first request, a
run is refused with a classed error naming the option to change when it
would need more calls than `max_calls`, or when its reader sends every
chunk and sending them would cost more than `max_cost_usd`. Both are
checked again before every request, so a run that reaches either one
stops and returns a `partial` answer rather than continuing to spend. A
parallel read that sends batches, which cannot be stopped part way, is
held to its worst case instead; see
[`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md).

Preview segmentation for free before committing to a reader:

    doc <- gr_ingest("report.pdf")
    gr_chunk_stats(gr_segment(doc, list(method = "sentence", max_tokens = 400)))

## Diagnosing an answer

Every degradation is recorded, never silent. See
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md)
for the full object, and `vignette`-free quick reference:

    ans$partial     # did anything degrade?
    ans$notes       # what: dropped_chunks, failed_calls, error, degraded_to_bm25
    print(ans$trace)          # calls, tokens, first error
    as_json(ans)              # every prompt and response from this one run

## Extending it

Each axis is a registry, so additions behave exactly like built-ins:
[`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
[`gr_register_segmenter()`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md),
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md).

## See also

[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md),
[`gr_recipes()`](https://elkronos.github.io/readgpt/reference/gr_recipes.md),
[`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)

## Author

**Maintainer**: Justin Chase <jchase.msu@gmail.com> \[copyright holder\]
