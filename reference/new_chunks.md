# Build a `gr_chunks`, the object every segmenter must return

Exported because it is part of the extension API:
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
rejects anything that does not inherit `"gr_chunks"`, so a custom
segmenter registered with
[`gr_register_segmenter()`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md)
cannot be written without this. Using it also gets you the shared
invariants for free (blank units dropped, `chunk_id` assigned in order,
tokens and characters measured, provenance recycled to match), so your
segmenter behaves like the built-ins wherever the rest of the package
touches it.

## Usage

``` r
new_chunks(
  text,
  method,
  spec,
  page = NA_integer_,
  section = NA_character_,
  block_id = NA_integer_,
  extra = list()
)
```

## Arguments

- text:

  Character vector of chunk texts. Blank entries are dropped.

- method:

  Your segmenter's name. Record a fallback here if you took one
  (`"semantic->paragraph"`);
  [`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md)
  surfaces it.

- spec:

  The `gr_segment_spec` passed to your segmenter.

- page, section, block_id:

  Provenance, one value per chunk or one recycled value. Leave as `NA`
  rather than guessing: wrong provenance sends a reader to the wrong
  page with full confidence.

- extra:

  Named list of segmenter-specific detail, kept on `$extra`.

## Value

A
[gr_chunks](https://elkronos.github.io/readgpt/reference/gr_chunks.md).

## Details

The token cap is NOT enforced here.
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
checks it after your function returns and re-splits anything oversized,
so a segmenter that ignores `spec$max_tokens` produces a warning and
correct chunks rather than an HTTP 400.

## See also

[`gr_register_segmenter()`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md),
[gr_chunks](https://elkronos.github.io/readgpt/reference/gr_chunks.md),
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md),
[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)

Other segmentation functions:
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md),
[`gr_chunks`](https://elkronos.github.io/readgpt/reference/gr_chunks.md),
[`gr_register_segmenter()`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md),
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md),
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md)

## Examples

``` r
# One chunk per bullet, with the source block recorded.
doc <- gr_ingest("Findings:\n\n- Revenue rose.\n- Costs fell.\n- Margin widened.")
#> Ingested 2 block(s), ~21 tokens (0 chars removed by cleaning).
ch <- new_chunks(trimws(strsplit(doc$text, "\n(?=-)", perl = TRUE)[[1]]),
                 method = "by_bullet", spec = gr_segment_spec(max_tokens = 100),
                 block_id = 1L)
gr_chunk_stats(ch)
#>      method n total_tokens min median mean max over_cap
#> 1 by_bullet 4           28   6      7    7   8        0
```
