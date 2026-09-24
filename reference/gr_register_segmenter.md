# Register a segmentation strategy

Axis 2 is a registry, so "where does meaning break in this document?" is
a question you can answer for your own material rather than choosing
from a fixed list. A registered segmenter is a first-class one: it
appears in
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md),
can be named in a
[`gr_recipe()`](https://elkronos.github.io/readgpt/reference/gr_recipe.md),
and goes through the same token-cap enforcement and reporting as the
built-ins.

## Usage

``` r
gr_register_segmenter(
  name,
  fn,
  description = "",
  cost = c("free", "embedding", "llm"),
  needs_client = FALSE
)
```

## Arguments

- name:

  Segmenter name, used in specs and recipes. Re-registering an existing
  name replaces it.

- fn:

  Function of `(doc, spec, client, trace)` returning a `gr_chunks`.
  Build the return value with
  [`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md);
  [`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
  rejects anything else. `doc` is a
  [gr_document](https://elkronos.github.io/readgpt/reference/gr_document.md);
  `spec` carries `max_tokens`, `overlap_tokens` and `min_tokens`, which
  [pack_units-style](https://elkronos.github.io/readgpt/reference/new_chunks.md)
  helpers respect for you.

- description:

  One-line description, shown by
  [`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md).

- cost:

  `"free"`, `"embedding"` or `"llm"`: what one run spends, so a UI can
  warn before it is spent.

- needs_client:

  Whether the segmenter requires a client.
  [`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md)
  reports it, so a UI can check before offering the strategy. When
  `TRUE` and no client is supplied,
  [`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
  calls `fn` and then warns with class `"gr_segment_fallback"`, unless
  `fn` raised a warning of that class itself, so one fallback gives one
  warning. Raising your own is better, because it can name what `fn`
  fell back *to*:
  `warning(warningCondition("No client; using 'paragraph'.", class = "gr_segment_fallback"))`.
  Record the downgrade in the returned `method` (`"mine->paragraph"`) so
  it survives into
  [`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md).

## Value

Invisibly, `name`.

## See also

[`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)
to build the return value,
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md),
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md),
[`gr_recipe()`](https://elkronos.github.io/readgpt/reference/gr_recipe.md)

Other segmentation functions:
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md),
[`gr_chunks`](https://elkronos.github.io/readgpt/reference/gr_chunks.md),
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md),
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md),
[`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)

## Examples

``` r
# One chunk per bullet list. Build the result with the same helper the
# built-ins use, so the token cap and reporting still apply.
gr_register_segmenter("by_bullet", description = "one chunk per bullet",
  fn = function(doc, spec, client, trace) {
    units <- unlist(strsplit(doc$text, "\n(?=[-*])", perl = TRUE))
    new_chunks(units, "by_bullet", spec)
  })
subset(gr_segmenters(), name == "by_bullet")
#>        name cost needs_client          description
#> 1 by_bullet free        FALSE one chunk per bullet
```
