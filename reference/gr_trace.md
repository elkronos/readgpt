# Create a run trace

Create a run trace

## Usage

``` r
gr_trace(run_id = NULL, meta = list())

# S3 method for class 'gr_trace'
as.data.frame(x, row.names = NULL, optional = FALSE, ...)
```

## Arguments

- run_id:

  Optional identifier; generated when omitted.

- meta:

  Named list of run-level metadata.

- x:

  A `gr_trace`.

- row.names:

  Optional row names for the result.

- optional, ...:

  Ignored; part of the
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html)
  generic.

## Value

A `gr_trace`. It is an environment, so it accumulates by reference: pass
the same trace to several calls and they all record into it. Fields:
`run_id`, `started`, `meta`, `steps`, `calls`, `cached`, `tokens_in`,
`tokens_out`, `errors`, `budget_stop`, `stop_reason`, `spent_usd`.
`cached` counts the calls answered from a
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
or a
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
rather than the network, so `calls - cached` is what the run paid for.

`budget_stop` is `TRUE` once a limit stopped the run, and `stop_reason`
says which: `"calls"` for `max_calls`, `"cost"` for `max_cost_usd` (see
[`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)).
`spent_usd` is what the calls so far cost, the figure `max_cost_usd` is
checked against. A call to a model with no registered price adds nothing
to it, so
[`gr_trace_cost()`](https://elkronos.github.io/readgpt/reference/gr_trace_cost.md)
is the full account.

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
trace returns one row per request; see below.

## One row per request

`as.data.frame(trace)` has one row for each request the run made, in the
order they were made, and none for local steps such as segmentation:

- `step`:

  The step's number in `trace$steps`, where the full record is.

- `document`:

  The document the request was about, when the run recorded one: the
  file name, web address or `"<inline text>"`.

- `recipe`:

  The recipe the request belonged to, when recorded.

- `stage`:

  What the request was for, such as `"map.answer"` or `"reduce"`.

- `model`, `ok`, `cached`:

  The model, whether a usable reply came back, and whether it came from
  a
  [`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
  or a
  [`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md).

- `tokens_in`, `tokens_out`:

  The size of the prompt and the reply.

- `usd`:

  What the request cost, 0 when it came from a cache. `NA` when the
  model has no registered price, as in
  [`gr_trace_cost()`](https://elkronos.github.io/readgpt/reference/gr_trace_cost.md),
  whose total the column adds up to.

- `seconds`:

  How long the request took, retries included. `NA` for a trace written
  by a version of readgpt that did not time requests.

- `error`:

  The error, or `NA`.

- `prompt`, `reply`:

  The messages sent, each as `"[role] text"`, and the text that came
  back.

## See also

[`gr_trace_summary()`](https://elkronos.github.io/readgpt/reference/gr_trace_summary.md),
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md),
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)

## Examples

``` r
tr <- gr_trace(meta = list(purpose = "demo"))
cl <- gr_mock_client(function(m, p) "an answer")
ch <- gr_segment(readgpt_example(), list(method = "sentence", max_tokens = 150))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'sentence' (cap 150 tokens, overlap 0).
invisible(gr_read(ch, "What was revenue?", cl, "map_reduce", trace = tr))
#> Reading with 'map_reduce' (all|N+logN|tree) over 5 chunk(s).
print(tr)
#> <gr_trace run_20260924000658.747_3116bd>  7 steps, 6 model calls, 1079 in / 36 out tokens, 0 error(s)
#>   steps: map.answer x5, preflight x1, reduce x1 
#>   cost: $0.0026 across gpt-5.6-terra

# One row per request, with what each cost and how long it took.
reqs <- as.data.frame(tr)
reqs[, c("step", "stage", "tokens_in", "tokens_out", "usd", "seconds")]
#>   step      stage tokens_in tokens_out      usd seconds
#> 1    2 map.answer       196          6 0.000464   0.000
#> 2    3 map.answer       206          6 0.000484   0.000
#> 3    4 map.answer       183          6 0.000438   0.001
#> 4    5 map.answer       189          6 0.000450   0.000
#> 5    6 map.answer       160          6 0.000392   0.000
#> 6    7     reduce       145          6 0.000362   0.000
```
