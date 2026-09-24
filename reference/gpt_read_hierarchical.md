# Deprecated: hierarchical reading

Superseded by
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md)
with `reader = "hierarchical"`. The new reader recurses until the
summaries fit the context window; v1 summarised exactly once and
overflowed past roughly 32 chunks.

## Usage

``` r
gpt_read_hierarchical(
  chunks,
  question,
  client = NULL,
  return_json = FALSE,
  ...
)
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
and `reader = "hierarchical"`,
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md)
for `fan_in` and `max_levels`,
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md)

Other v1 compatibility:
[`answer_question()`](https://elkronos.github.io/readgpt/reference/answer_question.md),
[`gpt_read_chunked()`](https://elkronos.github.io/readgpt/reference/gpt_read_chunked.md),
[`gpt_read_multipass()`](https://elkronos.github.io/readgpt/reference/gpt_read_multipass.md),
[`gpt_read_retrieval()`](https://elkronos.github.io/readgpt/reference/gpt_read_retrieval.md),
[`parse_text()`](https://elkronos.github.io/readgpt/reference/parse_text.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "45.2 million dollars")
chunks <- suppressWarnings(parse_text(readgpt_example(), chunk_token_limit = 200))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 200 tokens, overlap 0).
suppressWarnings(gpt_read_hierarchical(chunks, "What was revenue?", client = cl))
#> Using cached ingestion for this document + settings.
#> Reading with 'hierarchical' (all|N+tree+1|tree) over 4 chunk(s).
#> [1] "45.2 million dollars"

# The modern equivalent.
ch <- gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 200))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 200 tokens, overlap 0).
gr_read(ch, "What was revenue?", gr_mock_client(function(m, p) "45.2 million dollars"),
        "hierarchical")$reader
#> Reading with 'hierarchical' (all|N+tree+1|tree) over 4 chunk(s).
#> [1] "hierarchical"
```
