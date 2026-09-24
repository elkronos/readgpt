# Run several recipes over one document and compare them

Each recipe is an independent pipeline, so a recipe's answer is
identical whether it is run alone or alongside others. Extraction is
shared through the ingest cache, and segmentation is shared between
recipes whose segment specs are identical, so comparing five readers
over one chunking costs one chunking, not five.

## Usage

``` r
gr_compare(
  source,
  question,
  recipes = c("fast", "needle", "thorough"),
  client = NULL,
  allow_duplicates = FALSE,
  on_error = c("continue", "stop"),
  ...
)
```

## Arguments

- source:

  File path, web address, or raw text; see
  [`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md).

- question:

  The question.

- recipes:

  A character vector of recipe names, or a list of `gr_recipe`s.

- client:

  A `gr_client`.

- allow_duplicates:

  Run duplicates anyway (useful at temperature \> 0 to measure
  variance).

- on_error:

  `"continue"` keeps going and records the failure; `"stop"` aborts the
  whole comparison.

- ...:

  Overrides applied to every recipe.

## Value

A list with `answers` (named list of
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md)),
`summary` (a data frame with columns `recipe`, `segmenter`, `chunks`,
`reader`, `signature`, `partial`, `chunks_used`, `answer_chars`,
`not_found`, `error`), `trace` (shared across all recipes, so it records
every recipe's calls) and `document` (source and ingestion stats).

## Details

Recipes that resolve to identical ingestion, identical segmentation
*and* an identical read spec are collapsed with a warning rather than
billed twice. A shared reader signature alone is not enough: two
`retrieve` recipes with different `top_k` share a signature and are
genuinely different runs.

## See also

[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
[`gr_recipes()`](https://elkronos.github.io/readgpt/reference/gr_recipes.md),
[`gr_recipe()`](https://elkronos.github.io/readgpt/reference/gr_recipe.md),
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")

# Three pipelines over one document. Extraction is shared, so this costs one
# extraction, not three; only the segmentation and the reader vary.
cmp <- gr_compare(readgpt_example(), "What was revenue?",
                  c("fast", "precise", "needle"), client = cl)
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
#> Segmenting with 'sentence' (cap 600 tokens, overlap 60).
#> Reading with 'skim' (all|N+1|none) over 2 chunk(s).
#> Segmenting with 'semantic' (cap 500 tokens, overlap 50).
#> Reading with 'retrieve' (topk|1|none) over 2 chunk(s).
cmp$summary[, c("recipe", "segmenter", "chunks", "reader", "signature", "chunks_used")]
#>    recipe segmenter chunks   reader    signature chunks_used
#> 1    fast paragraph      1    stuff   all|1|none           1
#> 2 precise  sentence      2     skim all|N+1|none           2
#> 3  needle  semantic      2 retrieve  topk|1|none           2

# One trace covers all three, so this is the cost of the whole comparison.
gr_trace_summary(cmp$trace)
#>                          run_id calls cached steps tokens_in tokens_out errors
#> 1 run_20260924000649.312_78dfe7     5      0    16      2098         65      0
#>   elapsed_s
#> 1       0.1
```
