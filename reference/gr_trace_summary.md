# Summarise a trace

Summarise a trace

## Usage

``` r
gr_trace_summary(trace)
```

## Arguments

- trace:

  A `gr_trace`.

## Value

A one-row data frame: `run_id`, `calls`, `cached`, `steps`, `tokens_in`,
`tokens_out`, `errors`, `elapsed_s`. There is no cost column; combine
`tokens_in`/`tokens_out` with
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md)
for that.

`cached` is how many of those calls were answered from a
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
or a
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md).
Their tokens are still counted in `tokens_in` and `tokens_out`, because
that is how large the prompts and replies were; they were simply not
paid for again. A run with `cached == calls` cost nothing, so feeding
its token counts to
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md)
gives you what the run *would* have cost, not what it did.

## See also

[`gr_trace()`](https://elkronos.github.io/readgpt/reference/gr_trace.md),
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "45.2 million dollars")
ans <- answer_document(readgpt_example(), "What was revenue?", "thorough", client = cl)
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 1200 tokens, overlap 120).
#> Reading with 'map_reduce' (all|N+logN|tree) over 1 chunk(s).
gr_trace_summary(ans$trace)
#>                          run_id calls cached steps tokens_in tokens_out errors
#> 1 run_20260924000659.468_5f1c13     1      0     4       595         10      0
#>   elapsed_s
#> 1      0.02
gr_estimate_cost("gpt-4o", ans$trace$tokens_in, ans$trace$tokens_out)
#> [1] 0.0015875
```
