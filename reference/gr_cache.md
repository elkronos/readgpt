# A response cache

Wrap a client with
[`gr_cache_client()`](https://elkronos.github.io/readgpt/reference/gr_cache_client.md)
and every successful model call is written to disk, keyed on the exact
request. Re-issuing the same request returns the stored response without
touching the network: free, instant, and byte-identical even at a
temperature above zero.

## Usage

``` r
gr_cache(dir = NULL, read = TRUE, write = TRUE)
```

## Arguments

- dir:

  Directory for cache entries. Defaults to the `cache_dir` option.
  Created on first write, not here.

- read, write:

  Whether to read existing entries and write new ones. Set
  `write = FALSE` to run against a frozen cache; set `read = FALSE` to
  refresh entries that are already stored.

## Value

An object of class `gr_cache`. It holds a directory and a counter
environment, so copies of it share the same statistics.

## Details

The default location is under
[`tempdir()`](https://rdrr.io/r/base/tempfile.html), so a cache costs
nothing and disappears with the session. That is the right default for a
package (it writes nothing to your filesystem you did not ask for), but
it is not what you want for a long experiment. Pass a real directory, or
`tools::R_user_dir("readgpt", "cache")`, to keep entries across sessions
and make a run resumable after a crash.

Cross-session reuse needs a client that can say what it is.
[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md)
and
[`gr_ellmer_client()`](https://elkronos.github.io/readgpt/reference/gr_ellmer_client.md)
can: an endpoint and a model describe what will answer next month as
well as today. A bare
[`gr_backend_client()`](https://elkronos.github.io/readgpt/reference/gr_backend_client.md)
cannot, because what answers is an R closure, so by default it gets a
fresh identity per object and its entries are session-scoped. Give it a
stable `id` to opt in.

## What is stored

One small RDS file per entry, sharded into subdirectories by the first
two characters of the key. Each file holds the
[gr_result](https://elkronos.github.io/readgpt/reference/gr_result.md)
(text, token usage, model, finish reason), with the raw parsed API
response dropped, because nothing downstream reads it and keeping it
multiplied the cache size for no benefit. The prompt is **not** stored,
only its hash, so a cache directory does not accumulate copies of your
documents. The response itself is stored in full, and a model response
can of course quote the document it read.

## Caching a stochastic call

At a temperature above zero a cache hit replays one sample rather than
drawing a new one. That is the point: it is what makes a run
reproducible. But it means a cached sweep does not explore. Use a fresh
cache directory, or `read = FALSE`, when you want new draws.

## See also

[`gr_cache_client()`](https://elkronos.github.io/readgpt/reference/gr_cache_client.md)
to attach one,
[`gr_cache_stats()`](https://elkronos.github.io/readgpt/reference/gr_cache_stats.md),
[`gr_cache_clear()`](https://elkronos.github.io/readgpt/reference/gr_cache_clear.md),
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
for reproducing a recorded run

## Examples

``` r
cache <- gr_cache(dir = file.path(tempdir(), "readgpt-example-cache"))
cache
#> <gr_cache> /tmp/RtmpRyE9R0/readgpt-example-cache
#>   0 entries, 0.0 B on disk; 0 hit(s), 0 miss(es), 0 write(s)

# Nothing is written until a call is cached.
gr_cache_stats(cache)[c("entries", "hits", "misses")]
#>   entries hits misses
#> 1       0    0      0
```
