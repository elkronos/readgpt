# Describe one field of an extraction schema

Describe one field of an extraction schema

## Usage

``` r
gr_field(description, type = "string", values = NULL)
```

## Arguments

- description:

  What to look for, in the words you would use to a research assistant.
  This is the whole instruction the model gets for this field, so
  "Number of participants randomised, not the number analysed" earns its
  length.

- type:

  One of `"string"`, `"integer"`, `"number"`, `"boolean"` or `"enum"`.
  An `"integer"` value above `.Machine$integer.max` is kept as a whole
  double, so that column is a double whenever one such value is present.
  A value that carries more than one number, such as "120 (60 per arm)",
  is recorded as missing rather than run together.

- values:

  For `type = "enum"`, the permitted values.

## Value

A `gr_field`.

## See also

[`gr_fields()`](https://elkronos.github.io/readgpt/reference/gr_fields.md),
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)

## Examples

``` r
gr_field("Number of participants randomised", type = "integer")
#> <gr_field integer> Number of participants randomised
gr_field("Overall direction of the result", type = "enum",
         values = c("positive", "null", "mixed", "negative"))
#> <gr_field enum(positive/null/mixed/negative)> Overall direction of the result
```
