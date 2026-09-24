# Deprecated: answer a question using v1 mode names

Maps the old `mode` strings onto recipes. Unlike v1, each mode runs as
an isolated pipeline, so selecting several modes no longer changes any
of their answers, and `mode` no longer defaults to running all five.

## Usage

``` r
answer_question(
  file_path,
  question,
  mode = "Chunked",
  use_parallel = FALSE,
  refine = FALSE,
  return_json = FALSE,
  client = NULL,
  ...
)
```

## Arguments

- file_path:

  Path to the document.

- question:

  The question.

- mode:

  One or more of `"Retrieval"`, `"Chunked"`, `"Semantic"`,
  `"Hierarchical"`, `"MultiPass"`. Defaults to `"Chunked"` only.

- use_parallel:

  Run per-chunk calls in parallel.

- refine:

  Request chunk-level citations in the answer. v1's `refine`
  verification pass is not reproduced: it could never run, because it
  called a `search_text()` function that was never defined.

- return_json:

  Return the answers, the comparison summary and the trace as JSON
  instead of the answer string.

- client:

  A `gr_client`.

- ...:

  Passed to
  [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md).

## Value

A string, or a named list of strings when several modes are given.

## See also

[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md),
[`gr_recipes()`](https://elkronos.github.io/readgpt/reference/gr_recipes.md)

Other v1 compatibility:
[`gpt_read_chunked()`](https://elkronos.github.io/readgpt/reference/gpt_read_chunked.md),
[`gpt_read_hierarchical()`](https://elkronos.github.io/readgpt/reference/gpt_read_hierarchical.md),
[`gpt_read_multipass()`](https://elkronos.github.io/readgpt/reference/gpt_read_multipass.md),
[`gpt_read_retrieval()`](https://elkronos.github.io/readgpt/reference/gpt_read_retrieval.md),
[`parse_text()`](https://elkronos.github.io/readgpt/reference/parse_text.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "45.2 million dollars")

# v1 style, still works, warns once.
suppressWarnings(
  answer_question(readgpt_example(), "What was revenue?", mode = "Chunked", client = cl))
#> Extracting 'annual_report.md' with the 'md' extractor.
#> Ingested 17 block(s), ~573 tokens (11 chars removed by cleaning).
#> Segmenting with 'paragraph' (cap 1200 tokens, overlap 0).
#> Reading with 'map_reduce' (all|N+logN|tree) over 1 chunk(s).
#> [1] "45.2 million dollars"

# The modern equivalent.
answer_document(readgpt_example(), "What was revenue?", "thorough",
                client = cl, return = "text")
#> Extracting 'annual_report.md' with the 'md' extractor.
#> Ingested 17 block(s), ~573 tokens (11 chars removed by cleaning).
#> Segmenting with 'paragraph' (cap 1200 tokens, overlap 120).
#> Reading with 'map_reduce' (all|N+logN|tree) over 1 chunk(s).
#> [1] "45.2 million dollars"
```
