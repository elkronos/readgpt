# Deprecated: chunk-by-chunk reading

Superseded by
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md)
with `reader = "map_reduce"`. Kept so v1 scripts keep running; it warns
once per session and does **not** reproduce v1's unbounded merge prompt,
which produced HTTP 400s once the per-chunk answers outgrew the context
window. For v1's actual behaviour, use `recipe = "legacy"`.

## Usage

``` r
gpt_read_chunked(chunks, question, client = NULL, return_json = FALSE, ...)
```

## Arguments

- chunks:

  Character vector of chunks, or a `gr_chunks`.

- question:

  The question.

- client:

  A `gr_client`.

- return_json:

  Return the whole answer object, including its trace, as JSON.

- ...:

  Passed to
  [`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md).

## Value

A single string when `return_json` is `FALSE`; otherwise a
`json`-classed string holding the answer, its notes and the full trace.

## See also

[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md)
and `reader = "map_reduce"`,
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
[`gr_recipes()`](https://elkronos.github.io/readgpt/reference/gr_recipes.md)
for `"legacy"`

Other v1 compatibility:
[`answer_question()`](https://elkronos.github.io/readgpt/reference/answer_question.md),
[`gpt_read_hierarchical()`](https://elkronos.github.io/readgpt/reference/gpt_read_hierarchical.md),
[`gpt_read_multipass()`](https://elkronos.github.io/readgpt/reference/gpt_read_multipass.md),
[`gpt_read_retrieval()`](https://elkronos.github.io/readgpt/reference/gpt_read_retrieval.md),
[`parse_text()`](https://elkronos.github.io/readgpt/reference/parse_text.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "45.2 million dollars")
chunks <- suppressWarnings(parse_text(readgpt_example(), chunk_token_limit = 200))
#> Extracting 'annual_report.md' with the 'md' extractor.
#> Ingested 17 block(s), ~573 tokens (11 chars removed by cleaning).
#> Segmenting with 'paragraph' (cap 200 tokens, overlap 0).

# v1 style, still works, warns once.
suppressWarnings(gpt_read_chunked(chunks, "What was revenue?", client = cl))
#> Ingested 17 block(s), ~573 tokens (0 chars removed by cleaning).
#> Reading with 'map_reduce' (all|N+logN|tree) over 4 chunk(s).
#> [1] "45.2 million dollars"

# The modern equivalent, which also reports what it did.
ch <- gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 200))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 200 tokens, overlap 0).
gr_read(ch, "What was revenue?", gr_mock_client(function(m, p) "45.2 million dollars"),
        "map_reduce")$notes$chunks
#> Reading with 'map_reduce' (all|N+logN|tree) over 4 chunk(s).
#> [1] 4
```
