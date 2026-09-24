# Attach a cache to a client

Returns the same client with the cache attached, so a cached mock client
is still a mock client and `$calls()` still records only the calls that
were issued, which is how you check that the cache is working.

## Usage

``` r
gr_cache_client(client, cache = gr_cache())
```

## Arguments

- client:

  A
  [`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md)
  or
  [`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md).

- cache:

  A
  [`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md).
  Pass `NULL` to detach.

## Value

The client, with the cache attached.

## See also

[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md),
[`gr_cache_stats()`](https://elkronos.github.io/readgpt/reference/gr_cache_stats.md)

## Examples

``` r
calls <- 0
cl <- gr_mock_client(function(m, p) { calls <<- calls + 1; "42" })
cl <- gr_cache_client(cl, gr_cache(file.path(tempdir(), "readgpt-doc-cache")))

gr_call(cl, "What is the answer?")$text
#> [1] "42"
gr_call(cl, "What is the answer?")$text   # served from the cache
#> [1] "42"
calls                                     # the handler ran once
#> [1] 1

# A cache hit is marked, so a trace can tell spending from replay.
gr_call(cl, "What is the answer?")$cached
#> [1] TRUE
```
