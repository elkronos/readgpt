# Delete every entry in a cache

Removes the stored responses and resets the session counters. The
directory itself is left in place.

## Usage

``` r
gr_cache_clear(cache)
```

## Arguments

- cache:

  A
  [`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md).

## Value

The number of entries removed, invisibly.

## See also

[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md),
[`gr_cache_stats()`](https://elkronos.github.io/readgpt/reference/gr_cache_stats.md)

## Examples

``` r
cache <- gr_cache(file.path(tempdir(), "readgpt-clear-cache"))
cl <- gr_cache_client(gr_mock_client(function(m, p) "hi"), cache)
invisible(gr_call(cl, "something"))
gr_cache_clear(cache)
gr_cache_stats(cache)$entries
#> [1] 0
```
