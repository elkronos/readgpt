# A set of document chunks

Returned by
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md).
Summarise with
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md).

## Fields

- `chunks`:

  Data frame, one row per chunk, with columns `chunk_id`, `text`,
  `tokens`, `chars`, `page`, `section`, `block_id`.

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
