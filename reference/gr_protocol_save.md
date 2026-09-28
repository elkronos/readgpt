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

The round trip is lossless for everything a protocol *is*, the search
included when one was given to
[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md).
`recipe` is the one thing it may not be: a `gr_recipe` object is written
as its name, because a file that pinned every clean and segmentation
setting would silently pin them for a reader on a different version. Two
outline sections with one heading cannot both be keys of the file's
outline, so the second is written as "Findings.1", with a warning; give
each section its own heading. Anything else attached to the object
(`p$notes <- ...`) is not written, and saving warns that it is not.

The file is UTF-8 whatever the session's encoding, and is read as UTF-8.
In a C locale, or a Windows session in a single-byte code page, a
criterion with an accent or a symbol in it (the greater-than-or-equal
sign of "aged 18 or over") was written as the text "\<U+2265\>", or as a
byte no other machine reads as that letter, and a correct file read in a
C locale came back as "\<89\>": the criteria then sent to the screening
model and printed in the audit report.

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
