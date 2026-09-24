# Deprecated: multi-pass reading

Superseded by
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md)
with `reader = "ensemble"`. v1 re-ran Retrieval and Chunked verbatim, so
selecting them alongside MultiPass paid for each twice; the ensemble now
requires its members to have different traversal signatures, and says so
when they collapse onto the same traversal anyway.

## Usage

``` r
gpt_read_multipass(chunks, question, client = NULL, return_json = FALSE, ...)
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
and `reader = "ensemble"`,
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md),
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md)

Other v1 compatibility:
[`answer_question()`](https://elkronos.github.io/readgpt/reference/answer_question.md),
[`gpt_read_chunked()`](https://elkronos.github.io/readgpt/reference/gpt_read_chunked.md),
[`gpt_read_hierarchical()`](https://elkronos.github.io/readgpt/reference/gpt_read_hierarchical.md),
[`gpt_read_retrieval()`](https://elkronos.github.io/readgpt/reference/gpt_read_retrieval.md),
[`parse_text()`](https://elkronos.github.io/readgpt/reference/parse_text.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "45.2 million dollars")
chunks <- suppressWarnings(parse_text(readgpt_example(), chunk_token_limit = 200))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 200 tokens, overlap 0).
suppressWarnings(gpt_read_multipass(chunks, "What was revenue?", client = cl))
#> Using cached ingestion for this document + settings.
#> Reading with 'ensemble' (ensemble|sum+1|none) over 4 chunk(s).
#> [1] "45.2 million dollars"

# The modern equivalent.
ch <- gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 200))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 200 tokens, overlap 0).
gr_read(ch, "What was revenue?", gr_mock_client(function(m, p) "45.2 million dollars"),
        "ensemble")$reader
#> Reading with 'ensemble' (ensemble|sum+1|none) over 4 chunk(s).
#> Warning: Ensemble members 'retrieve' and 'map_reduce' read the same chunks and returned the same answer. Their agreement is not independent corroboration.
#> [1] "ensemble"
```
