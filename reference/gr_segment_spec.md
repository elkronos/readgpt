# Describe a segmentation configuration

Describe a segmentation configuration

## Usage

``` r
gr_segment_spec(
  method = "paragraph",
  max_tokens = 1200L,
  overlap_tokens = 0L,
  min_tokens = 0L,
  separators = NULL,
  prefix_section = TRUE,
  semantic_window = 2L,
  semantic_percentile = 90,
  context_source = c("metadata", "llm"),
  proposition_batch_tokens = 900L,
  parallel = NULL,
  ...
)
```

## Arguments

- method:

  Segmenter name; see
  [`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md).

- max_tokens:

  Hard cap on chunk size, in tokens. Always enforced: a segmenter cannot
  emit an oversized chunk, which the old `chunk_text_semantic()`
  routinely did.

- overlap_tokens:

  Tokens of trailing context copied into the start of the next chunk.
  Overlap is what stops an answer straddling a boundary from being lost
  by both chunks. Honoured by every segmenter except `page` (a page is
  the unit) and `proposition` (propositions are already self-contained).

- min_tokens:

  A chunk smaller than this is merged **backward** into the chunk before
  it. Ignored by `page` and `fixed`.

- separators:

  For `method = "recursive"`: the cascade, strongest first.

- prefix_section:

  For `method = "structural"`: prepend the heading.

- semantic_window, semantic_percentile:

  For `method = "semantic"`: how many sentences to embed together, and
  how extreme a distance counts as a boundary (higher = fewer, larger
  chunks).

- context_source:

  For `method = "contextual"`: `"metadata"` (free) or `"llm"` (one call
  per chunk).

- proposition_batch_tokens:

  For `method = "proposition"`: batch size.

- parallel:

  Parallelise per-chunk model work in segmenters that do any.

- ...:

  Extra fields for custom segmenters.

## Value

A list of class `gr_segment_spec`.

## Out-of-range values

Arguments are clamped into a usable range and the change is warned
about, never applied silently: `max_tokens` \[32, 1e6\],
`overlap_tokens` \[0, max_tokens - 1\], `min_tokens` \[0, max_tokens\],
`semantic_window` \[1, 10\], `semantic_percentile` \[50, 99.5\],
`proposition_batch_tokens` \[200, 4000\].

## See also

[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md)
for the available methods and their costs,
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md)

Other segmentation functions:
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md),
[`gr_chunks`](https://elkronos.github.io/readgpt/reference/gr_chunks.md),
[`gr_register_segmenter()`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md),
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md),
[`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)

## Examples

``` r
gr_segment_spec("semantic", max_tokens = 800, overlap_tokens = 80)$overlap_tokens
#> [1] 80

# An impossible setting is corrected loudly.
suppressWarnings(gr_segment_spec("paragraph", max_tokens = 100,
                                 overlap_tokens = 500)$overlap_tokens)
#> [1] 99
```
