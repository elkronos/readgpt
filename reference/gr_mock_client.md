# A deterministic offline client for tests, demos and dry runs

`handler` receives `(messages, params)` and returns either a string or a
`gr_result`. Every call is recorded in `$calls()`, so tests can assert
on the exact prompts a strategy produced, which is how you prove two
reading strategies are actually different.

## Usage

``` r
gr_mock_client(handler = NULL, embed_handler = NULL)
```

## Arguments

- handler:

  Function of `(messages, params)` returning a string or a
  [gr_result](https://elkronos.github.io/readgpt/reference/gr_result.md).

- embed_handler:

  Function of `(texts, params)` returning a numeric matrix with one row
  per input.

## Value

An object of class `gr_client`, with `$calls()`, `$embeds()` and
`$reset()`.

## Details

Three things to know about the mock. It registers two model ids
(`"mock-model"`, `"mock-embed"`) in the session's model registry the
first time it is called, so they appear in
[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md)
afterwards. Its default handler returns plain text, so readers that need
JSON-schema output (`rerank`, `iterative`) take their documented
degraded path unless your handler returns valid JSON for those prompts.
And its `embed_handler` is used by
[`gr_embed()`](https://elkronos.github.io/readgpt/reference/gr_embed.md)
in preference to any registered embedder, reporting
`embedding_source = "api"`, so an offline run gets semantic-shaped
vectors rather than the lexical fallback. A mock embed handler that
fails or returns the wrong number of rows is still caught and still
degrades, like any other.

## See also

[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md),
[`gr_call()`](https://elkronos.github.io/readgpt/reference/gr_call.md),
[gr_result](https://elkronos.github.io/readgpt/reference/gr_result.md),
[`readgpt_example()`](https://elkronos.github.io/readgpt/reference/readgpt_example.md)
for a document to run against

## Examples

``` r
cl <- gr_mock_client(function(messages, params) "mock answer")
gr_call(cl, list(list(role = "user", content = "hi")))$text
#> [1] "mock answer"

# `$calls()` is how you prove two reading strategies differ: it records the
# exact prompts each one sent.
cl$reset()
ch <- gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 150))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 150 tokens, overlap 0).
invisible(gr_read(ch, "What was revenue?", cl, "skim"))
#> Reading with 'skim' (all|N+1|none) over 5 chunk(s).
table(vapply(cl$calls(), function(x) x$label, character(1)))
#> 
#>  skim.answer skim.extract 
#>            1            5 
```
