# Save a protocol to a file, and read one back

A protocol is meant to be written down before the reading starts, shared
with whoever is checking the work, and cited alongside the results. That
means a file, and JSON because it is exact and needs nothing installed.

## Usage

``` r
gr_protocol_save(protocol, path)

gr_protocol_read(path)
```

## Arguments

- protocol:

  A
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md).

- path:

  File path. `gr_protocol_read()` also accepts a JSON string.

## Value

`gr_protocol_save()` returns `path` invisibly; `gr_protocol_read()`
returns a `gr_protocol`.

## Details

The round trip is lossless for everything a protocol *is*. `recipe` is
the one thing it may not be: a `gr_recipe` object is written as its
name, because a file that pinned every clean and segmentation setting
would silently pin them for a reader on a different version.

## See also

[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md),
[`gr_protocols()`](https://elkronos.github.io/readgpt/reference/gr_protocols.md)

## Examples

``` r
p <- gr_protocols("bibliography")
f <- tempfile(fileext = ".json")
gr_protocol_save(p, f)
identical(gr_protocol_read(f)$question, p$question)
#> [1] TRUE
```
