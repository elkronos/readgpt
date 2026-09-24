# Register a cleaning step

Register a cleaning step

## Usage

``` r
gr_register_cleaner(
  name,
  fn,
  stage = c("early", "late"),
  description = "",
  default_on = FALSE,
  scope = c("block", "document")
)
```

## Arguments

- name:

  Step name.

- fn:

  Function of `(text, opts)` returning cleaned text.

- stage:

  `"early"` (structure-preserving, e.g. boilerplate removal) or `"late"`
  (destructive normalisation, e.g. digit stripping). Early steps always
  run before late steps regardless of the order the user lists them.

- description:

  One-line description.

- default_on:

  Whether the step is enabled by the standard preset.

- scope:

  `"block"` (default) applies the step to each text block independently.
  `"document"` applies it once to all blocks joined together, which is
  required for anything that reasons about position or repetition across
  the whole document (dropping a trailing bibliography, or detecting a
  running head by how often a line recurs). A document-scoped step that
  was applied per block would simply never fire.

## Value

Invisibly, `name`.

## See also

[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md),
[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md)

Other ingest functions:
[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md),
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md),
[`gr_document`](https://elkronos.github.io/readgpt/reference/gr_document.md),
[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
[`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md)

## Examples

``` r
gr_register_cleaner("drop_confidential", stage = "early",
  description = "Remove CONFIDENTIAL banner lines",
  fn = function(x, o) gsub("(?mi)^\\s*CONFIDENTIAL.*$", "", x, perl = TRUE))

gr_clean("CONFIDENTIAL - DRAFT\n\nThe real content of the document.",
         steps = c("drop_confidential", "collapse_whitespace"))
#> [1] "The real content of the document."
#> attr(,"gr_clean_log")
#> attr(,"gr_clean_log")$drop_confidential
#> attr(,"gr_clean_log")$drop_confidential$step
#> [1] "drop_confidential"
#> 
#> attr(,"gr_clean_log")$drop_confidential$stage
#> [1] "early"
#> 
#> attr(,"gr_clean_log")$drop_confidential$scope
#> [1] "block"
#> 
#> attr(,"gr_clean_log")$drop_confidential$chars_removed
#> [1] 20
#> 
#> 
#> attr(,"gr_clean_log")$collapse_whitespace
#> attr(,"gr_clean_log")$collapse_whitespace$step
#> [1] "collapse_whitespace"
#> 
#> attr(,"gr_clean_log")$collapse_whitespace$stage
#> [1] "late"
#> 
#> attr(,"gr_clean_log")$collapse_whitespace$scope
#> [1] "block"
#> 
#> attr(,"gr_clean_log")$collapse_whitespace$chars_removed
#> [1] 2
#> 
#> 
```
