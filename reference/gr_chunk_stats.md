# Summary statistics for a chunk set

Useful for comparing segmentation strategies before spending any API
budget.

## Usage

``` r
gr_chunk_stats(chunks)
```

## Arguments

- chunks:

  A
  [gr_chunks](https://elkronos.github.io/readgpt/reference/gr_chunks.md)
  object.

## Value

A one-row data frame: `method`, `n`, `total_tokens`, `min`, `median`,
`mean`, `max`, `over_cap`. `method` reports any fallback that occurred.
`total_tokens` exceeds the document's own token count when overlap is
on; that difference is the duplication overlap buys you.

## See also

[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md)

Other segmentation functions:
[`gr_chunks`](https://elkronos.github.io/readgpt/reference/gr_chunks.md),
[`gr_register_segmenter()`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md),
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md),
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md),
[`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)

## Examples

``` r
doc <- gr_ingest(readgpt_example())
#> Using cached ingestion for this document + settings.

# What overlap actually costs, before any model call.
do.call(rbind, lapply(c(0, 30, 60), function(ov)
  gr_chunk_stats(gr_segment(doc, list(method = "sentence", max_tokens = 120,
                                      overlap_tokens = ov)))))
#> Segmenting with 'sentence' (cap 120 tokens, overlap 0).
#> Segmenting with 'sentence' (cap 120 tokens, overlap 30).
#> Segmenting with 'sentence' (cap 120 tokens, overlap 60).
#>     method n total_tokens min median mean max over_cap
#> 1 sentence 6          532  47   92.5 88.7 106        0
#> 2 sentence 7          666  74   99.0 95.1 106        0
#> 3 sentence 9          860  79   94.0 95.6 109        0
```
