# Segment a document into chunks

Segmentation is deliberately separate from ingestion: one extraction can
feed many different segmentations, so comparing chunking strategies
costs one file read, not one per strategy.

## Usage

``` r
gr_segment(doc, spec = NULL, client = NULL, trace = NULL)
```

## Arguments

- doc:

  A `gr_document` from
  [`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
  or anything
  [`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md)
  accepts.

- spec:

  A `gr_segment_spec`, a segmenter name, or a named list.

- client:

  A `gr_client`, needed only by `semantic`, `proposition` and
  `contextual(context_source = "llm")`.

- trace:

  Optional `gr_trace`.

## Value

A [gr_chunks](https://elkronos.github.io/readgpt/reference/gr_chunks.md)
object. If the requested segmenter could not run it falls back and
records the fallback in `$method`, e.g. `"semantic->paragraph"` when no
`client` was supplied.

## See also

[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md),
[`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md),
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md),
[gr_chunks](https://elkronos.github.io/readgpt/reference/gr_chunks.md)

Other segmentation functions:
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md),
[`gr_chunks`](https://elkronos.github.io/readgpt/reference/gr_chunks.md),
[`gr_register_segmenter()`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md),
[`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md),
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md),
[`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)

## Examples

``` r
doc <- gr_ingest(readgpt_example())
#> Using cached ingestion for this document + settings.

# The same document, three boundary hypotheses. No API calls.
do.call(rbind, lapply(c("fixed", "paragraph", "sentence", "structural"),
  function(m) gr_chunk_stats(gr_segment(doc, list(method = m, max_tokens = 120)))))
#> Segmenting with 'fixed' (cap 120 tokens, overlap 0).
#> Segmenting with 'paragraph' (cap 120 tokens, overlap 0).
#> Segmenting with 'sentence' (cap 120 tokens, overlap 0).
#> Segmenting with 'structural' (cap 120 tokens, overlap 0).
#>       method n total_tokens min median  mean max over_cap
#> 1      fixed 5          528  49  120.0 105.6 120        0
#> 2  paragraph 6          532  47   90.0  88.7 116        0
#> 3   sentence 6          532  47   92.5  88.7 106        0
#> 4 structural 8          562  31   75.0  70.2 101        0
```
