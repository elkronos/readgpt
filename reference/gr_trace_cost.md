# What a run actually cost

Costs a trace using each step's own model and counting only the calls
that were issued: a call served from a
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
or a
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
spent nothing, however many tokens its prompt contained.

## Usage

``` r
gr_trace_cost(trace)
```

## Arguments

- trace:

  A `gr_trace`.

## Value

A data frame with one row per model: `model`, `calls`, `paid_calls`,
`paid_in`, `paid_out`, `usd`. `sum(x$usd)` is the run's cost. A model
with no registered price contributes `NA`, so a total that silently
omitted an unpriced model is impossible.

## Details

This is why the token totals on
[`gr_trace_summary()`](https://elkronos.github.io/readgpt/reference/gr_trace_summary.md)
are not a bill. They report how large the prompts and replies were,
which is the right measure of a run's *shape*; a fully cached re-run has
the same shape as the original and cost nothing at all.

## See also

[`gr_trace_summary()`](https://elkronos.github.io/readgpt/reference/gr_trace_summary.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md),
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl)
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
gr_trace_cost(ans$trace)
#>           model calls paid_calls paid_in paid_out      usd
#> 1 gpt-5.6-terra     1          1     595       13 0.001346

# Priced by the model on the STEP (the recipe's model), not by the mock
# that answered. A cached re-run costs nothing for a different reason:
# paid_calls falls to zero while calls does not.
cache <- gr_cache(file.path(tempdir(), "readgpt-cost-example"))
again <- answer_document(readgpt_example(), "What was revenue?", "fast",
                         client = gr_cache_client(cl, cache))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
twice <- answer_document(readgpt_example(), "What was revenue?", "fast",
                         client = gr_cache_client(cl, cache))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
gr_trace_cost(twice$trace)[c("calls", "paid_calls", "usd")]
#>   calls paid_calls usd
#> 1     1          0   0
```
