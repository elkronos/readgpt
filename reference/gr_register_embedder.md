# Register an embedding backend

Embedding is the sixth registry, alongside extractors, cleaners,
segmenters, readers and models. Register a function and every part of
the package that embeds (the `semantic` segmenter, the `retrieve` and
`iterative` readers) uses it, with no change to any of them.

## Usage

``` r
gr_register_embedder(name, fn, description = "", deterministic = FALSE)
```

## Arguments

- name:

  Short id, used as the value of `gr_options(embedder =)` and as the
  `"embedding_source"` recorded on the result.

- fn:

  Function of `(texts, params)` returning a numeric matrix with one row
  per input. `params` carries `client`, `model`, `batch_size`, `cache`
  and `trace`. Rows should be L2-normalised: everything downstream
  treats the cross-product as cosine similarity. Signal failure by
  raising; the caller applies its own `fallback` policy, which is not
  the embedder's business.

- description:

  One line, shown by
  [`gr_embedders()`](https://elkronos.github.io/readgpt/reference/gr_embedders.md).

- deterministic:

  `TRUE` if the same text always gives the same vector in any session on
  any machine. **Say so only if it is true.** It is what lets a
  [`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
  reproduce a run's chunk ranking instead of degrading to lexical
  vectors, and claiming it wrongly turns a replay from a recording into
  a plausible-looking fiction.

## Value

The name, invisibly.

## See also

[`gr_embedders()`](https://elkronos.github.io/readgpt/reference/gr_embedders.md),
[`gr_embed()`](https://elkronos.github.io/readgpt/reference/gr_embed.md),
[`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)
for `embedder`,
[`gr_backend_client()`](https://elkronos.github.io/readgpt/reference/gr_backend_client.md)
to supply an embedder alongside a transport

## Examples

``` r
# A local model, a company service, anything: it is a function of texts.
gr_register_embedder("first-letters",
  fn = function(texts, params) {
    m <- t(vapply(texts, function(tx) {
      v <- numeric(26)
      ltr <- utf8ToInt(tolower(substr(tx, 1, 40))) - 96L
      for (i in ltr[ltr >= 1 & ltr <= 26]) v[i] <- v[i] + 1
      n <- sqrt(sum(v^2)); if (n == 0) v else v / n
    }, numeric(26), USE.NAMES = FALSE))
    m
  },
  description = "Toy: letter counts", deterministic = TRUE)

gr_embedders()
#>            name deterministic
#> 1           api         FALSE
#> 2 first-letters          TRUE
#> 3       lexical          TRUE
#>                                                    description
#> 1                 Embeddings endpoint on the client's base URL
#> 2                                           Toy: letter counts
#> 3 Hashed bag-of-words; free, offline, word overlap not meaning

old <- gr_options(embedder = "first-letters")
attr(gr_embed(gr_client(), c("alpha", "beta")), "embedding_source")
#> [1] "first-letters"
gr_options(old)
```
