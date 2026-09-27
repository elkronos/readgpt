# A set of document chunks

Returned by
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md).
Summarise with
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md).

## Fields

- `chunks`:

  Data frame, one row per chunk, with columns `chunk_id`, `text`,
  `tokens`, `chars`, `page`, `section`, `block_id`, and sometimes
  `source_text`.

- `chunks$source_text`:

  Optional. The document text a chunk was made from, for a segmenter
  that puts text of its own into `text`: `proposition` fills it with the
  passage the propositions were written from, and `contextual` with the
  chunk's body without the bracketed context line. `NA`, or no such
  column, means `text` is the document's own. A quotation from a chunk
  is checked against `source_text` when it is there and not `NA`, and
  against `text` otherwise, so what the segmenter wrote cannot verify a
  quote. A chunk split to fit the token cap keeps the `source_text` of
  the chunk it was cut from. A custom segmenter that writes into `text`
  should set it with
  [`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)'s
  `source_text`, or quotes are checked against what it wrote.

- `method`:

  The segmenter that ran. **If a segmenter fell back this records the
  fallback**, e.g. `"semantic->paragraph"` when no client was supplied,
  or `"page->paragraph"` for a source with no page provenance.

- `spec`:

  The
  [`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md)
  used.

- `extra`:

  Method-specific detail: `boundaries` and `embedding_source` for
  `semantic`, `propositions` for `proposition`, `cap_enforced` when
  oversized chunks had to be split.

- `trace`:

  Set only when
  [`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
  was called without a `trace` and so made its own: the
  [gr_trace](https://elkronos.github.io/readgpt/reference/gr_trace.md)
  it recorded into, holding the requests the segmenter made (embeddings
  for `semantic`, model calls for `proposition` and for `contextual`
  with `context_source = "llm"`), which were held to
  `gr_options(max_calls =, max_cost_usd =)`. When a `trace` is passed
  they are recorded there instead, and this field is absent.

## Methods

[`print()`](https://rdrr.io/r/base/print.html) shows the method, the
chunk count and the token distribution;
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md)
returns the same as a one-row data frame you can `rbind`;
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md)
serialises the spec, the stats and every chunk.

## See also

[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
which returns one,
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md),
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md),
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md),
[`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)
to build one in a custom segmenter

[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md),
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md)

Other segmentation functions:
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md),
[`gr_register_segmenter()`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md),
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md),
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md),
[`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)

## Examples

``` r
ch <- gr_segment(readgpt_example(), list(method = "sentence", max_tokens = 120))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'sentence' (cap 120 tokens, overlap 0).
ch$method
#> [1] "sentence"
head(ch$chunks[, c("chunk_id", "tokens", "section")])
#>   chunk_id tokens      section
#> 1        1     88         <NA>
#> 2        2     92         <NA>
#> 3        3     93         <NA>
#> 4        4    106 Risk factors
#> 5        5    106         <NA>
#> 6        6     47         <NA>
```
