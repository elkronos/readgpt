# Subsetting a reference frame gives a plain data frame.

`[` on an object whose class extends `data.frame` keeps the CLASS and
drops every other attribute, so `ref[, cols]` came back still claiming
to be a `gr_reference_frame` while `of`, `frame_n`, `screened_n` and
`seed` were gone, and gr_calibrate() then computed the corpus-wide
metric set from a stratified sample. The same trap, and the same fix, as
`[.gr_gaps`.

## Usage

``` r
# S3 method for class 'gr_reference_frame'
x[...]
```

## Arguments

- x:

  A `gr_reference_frame`.

- ...:

  Passed to the data frame method.

## Value

A plain data frame.
