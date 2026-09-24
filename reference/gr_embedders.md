# List registered embedding backends

List registered embedding backends

## Usage

``` r
gr_embedders()
```

## Value

A data frame with one row per embedder: `name`, `deterministic`
(`"TRUE"`/`"FALSE"`) and `description`. `deterministic` is the column
that matters for reproducibility: only a deterministic embedder lets a
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
reproduce a run's chunk ranking.

## See also

[`gr_register_embedder()`](https://elkronos.github.io/readgpt/reference/gr_register_embedder.md),
[`gr_embed()`](https://elkronos.github.io/readgpt/reference/gr_embed.md),
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)

## Examples

``` r
gr_embedders()
#>      name deterministic
#> 1     api         FALSE
#> 2 lexical          TRUE
#>                                                    description
#> 1                 Embeddings endpoint on the client's base URL
#> 2 Hashed bag-of-words; free, offline, word overlap not meaning
```
