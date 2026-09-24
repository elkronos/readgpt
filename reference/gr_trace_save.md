# Save a trace to a file

Writes the trace as JSON, in the form
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md)
produces. The file is everything
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
needs, so this is how a run leaves the session it happened in.

## Usage

``` r
gr_trace_save(trace, path)
```

## Arguments

- trace:

  A `gr_trace`.

- path:

  Destination file.

## Value

`path`, invisibly.

## See also

[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md),
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md),
[`gr_trace()`](https://elkronos.github.io/readgpt/reference/gr_trace.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl)
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).

f <- tempfile(fileext = ".json")
gr_trace_save(ans$trace, f)
file.exists(f)
#> [1] TRUE
```
