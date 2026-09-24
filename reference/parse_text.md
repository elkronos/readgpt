# Deprecated: parse a document into text chunks

Deprecated: parse a document into text chunks

## Usage

``` r
parse_text(
  file_path,
  chunk_token_limit = 3000,
  chunk_method = c("naive", "semantic"),
  remove_whitespace = TRUE,
  remove_special_chars = FALSE,
  remove_numbers = FALSE,
  ocr_lang = "eng",
  client = NULL
)
```

## Arguments

- file_path:

  Path to the document.

- chunk_token_limit:

  Maximum tokens per chunk.

- chunk_method:

  `"naive"` (mapped to `"paragraph"`) or `"semantic"`.

- remove_whitespace, remove_special_chars, remove_numbers:

  v1 cleaning flags.

- ocr_lang:

  OCR language.

- client:

  A `gr_client`, needed for `chunk_method = "semantic"`.

## Value

A character vector of chunk texts.

## See also

[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md),
[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md)

Other v1 compatibility:
[`answer_question()`](https://elkronos.github.io/readgpt/reference/answer_question.md),
[`gpt_read_chunked()`](https://elkronos.github.io/readgpt/reference/gpt_read_chunked.md),
[`gpt_read_hierarchical()`](https://elkronos.github.io/readgpt/reference/gpt_read_hierarchical.md),
[`gpt_read_multipass()`](https://elkronos.github.io/readgpt/reference/gpt_read_multipass.md),
[`gpt_read_retrieval()`](https://elkronos.github.io/readgpt/reference/gpt_read_retrieval.md)

## Examples

``` r
# v1 style, still works, warns once.
length(suppressWarnings(parse_text(readgpt_example(), chunk_token_limit = 200)))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 200 tokens, overlap 0).
#> [1] 4

# The modern equivalent, which also reports what it did.
gr_chunk_stats(gr_segment(gr_ingest(readgpt_example()),
                          list(method = "paragraph", max_tokens = 200)))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 200 tokens, overlap 0).
#>      method n total_tokens min median  mean max over_cap
#> 1 paragraph 4          525  34    156 131.2 179        0
```
