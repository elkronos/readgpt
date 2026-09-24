# Bind an ingestion, segmentation and reading configuration together

A recipe is the unit
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
treats as one experiment. Fixing two axes and varying the third is what
makes a difference in the answer attributable to the change you made.
That is exactly what the previous release could not do, because its
modes shared one chunk object.

## Usage

``` r
gr_recipe(name = NULL, ingest = NULL, segment = NULL, read = NULL)
```

## Arguments

- name:

  Label used in results and traces.

- ingest:

  A `gr_ingest_spec`, preset name, or named list.

- segment:

  A `gr_segment_spec`, segmenter name, or named list.

- read:

  A `gr_read_spec`, reader name, or named list.

## Value

A `gr_recipe`: a list of `name`, `ingest`, `segment` and `read`, each a
validated spec. It has a [`print()`](https://rdrr.io/r/base/print.html)
method that shows all three at once.

## See also

[`gr_recipes()`](https://elkronos.github.io/readgpt/reference/gr_recipes.md)
for the built-ins,
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
to run one,
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
to run several,
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
[`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md),
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md)

## Examples

``` r
gr_recipe("semantic_topk", segment = list(method = "semantic", max_tokens = 800),
          read = list(reader = "retrieve", top_k = 6))
#> <gr_recipe 'semantic_topk'>
#>   ingest  : clean=standard ocr=auto
#>   segment : semantic (max 800 tok, overlap 0, min 0)
#>   read    : retrieve [topk|1|none] model=gpt-5.6-terra

# Start from a built-in and change one axis. The other two stay fixed, so
# any difference in the answer is attributable to the change.
r <- gr_recipes("thorough")
r$segment <- gr_segment_spec(method = "structural", max_tokens = 1200)
r$name <- "thorough_structural"
r
#> <gr_recipe 'thorough_structural'>
#>   ingest  : clean=standard ocr=auto
#>   segment : structural (max 1200 tok, overlap 0, min 0)
#>   read    : map_reduce [all|N+logN|tree] model=gpt-5.6-terra
```
