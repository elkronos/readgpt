# Deprecated: evidence-extraction reading

Superseded by
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md)
with `reader = "skim"`, which extracts verbatim evidence from every
chunk and then consolidates it, so `ans$evidence` holds passages from
the document rather than the model's paraphrase of them.

## Usage

``` r
gpt_read_retrieval(chunks, question, client = NULL, return_json = FALSE, ...)
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
and `reader = "skim"`,
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md)
for the evidence table

Other v1 compatibility:
[`answer_question()`](https://elkronos.github.io/readgpt/reference/answer_question.md),
[`gpt_read_chunked()`](https://elkronos.github.io/readgpt/reference/gpt_read_chunked.md),
[`gpt_read_hierarchical()`](https://elkronos.github.io/readgpt/reference/gpt_read_hierarchical.md),
[`gpt_read_multipass()`](https://elkronos.github.io/readgpt/reference/gpt_read_multipass.md),
[`parse_text()`](https://elkronos.github.io/readgpt/reference/parse_text.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "45.2 million dollars")
chunks <- suppressWarnings(parse_text(readgpt_example(), chunk_token_limit = 200))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 200 tokens, overlap 0).
suppressWarnings(gpt_read_retrieval(chunks, "What was revenue?", client = cl))
#> Using cached ingestion for this document + settings.
#> Reading with 'skim' (all|N+1|none) over 4 chunk(s).
#> [1] "45.2 million dollars"

# The modern equivalent.
ch <- gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 200))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 200 tokens, overlap 0).
gr_read(ch, "What was revenue?", gr_mock_client(function(m, p) "45.2 million dollars"),
        "skim")$reader
#> Reading with 'skim' (all|N+1|none) over 4 chunk(s).
#> [1] "skim"
```
