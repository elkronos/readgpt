# Embed texts

Embed texts

## Usage

``` r
gr_embed(
  client,
  texts,
  model = NULL,
  batch_size = 64L,
  cache = NULL,
  trace = NULL,
  fallback = c("lexical", "error", "none"),
  embedder = NULL
)
```

## Arguments

- client:

  A `gr_client`.

- texts:

  Character vector.

- model:

  Embedding model id; defaults to the client's.

- batch_size:

  Texts per request.

- cache:

  Use the session embedding cache.

- trace:

  Optional trace.

- fallback:

  What to do when the embedding request fails. **Defaults to
  `"lexical"`**: hashed bag-of-words vectors that measure word overlap,
  not meaning, so `semantic` segmentation and `retrieve` ranking become
  markedly less accurate. The substitution warns and is recorded, but
  the run continues. Use `"error"` to fail fast, or `"none"` to get an
  empty matrix.

- embedder:

  A registered embedder name (see
  [`gr_embedders()`](https://elkronos.github.io/readgpt/reference/gr_embedders.md)),
  or a function of `(texts, params)`. Defaults to the embed function
  supplied with the client, if any, and otherwise to
  `gr_options("embedder")`.

## Value

A numeric matrix, one row per input, carrying an `"embedding_source"`
attribute naming the embedder that produced it (`"api"` or `"lexical"`
for the built-ins). Always check it before treating the rows as
semantic. Rows from the API and lexical paths are L2-normalised. With
`fallback = "none"` and a failed request the result is a 0 x 0 matrix.

## See also

[`gr_embedders()`](https://elkronos.github.io/readgpt/reference/gr_embedders.md)
for what is registered,
[`gr_register_embedder()`](https://elkronos.github.io/readgpt/reference/gr_register_embedder.md)
to add one,
[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md),
[`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md)
for `method = "semantic"`,
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md)
for `reader = "retrieve"`,
[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md)
for the embedding models in the registry

## Examples

``` r
cl <- gr_mock_client()
e <- gr_embed(cl, c("cats sleep all day", "dogs bark all night",
                    "revenue rose to 45.2 million"))

# Always check this before treating the rows as semantic: on the lexical
# fallback they reflect word overlap, not meaning.
attr(e, "embedding_source")
#> [1] "api"

# Rows are L2-normalised, so the cross-product is cosine similarity.
round(e %*% t(e), 3)
#>       [,1] [,2]  [,3]
#> [1,] 1.000 0.71 0.537
#> [2,] 0.710 1.00 0.610
#> [3,] 0.537 0.61 1.000

# The semantic segmenter records the same thing, so a degraded run stays
# visible after the fact.
gr_segment(readgpt_example(), list(method = "semantic", max_tokens = 200),
           client = cl)$extra$embedding_source
#> Using cached ingestion for this document + settings.
#> Segmenting with 'semantic' (cap 200 tokens, overlap 0).
#> [1] "api"
```
