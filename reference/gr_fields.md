# Build an extraction schema

A goal, expressed as the fields you want filled. Each argument is either
a description (a string field) or a
[`gr_field()`](https://elkronos.github.io/readgpt/reference/gr_field.md).

## Usage

``` r
gr_fields(...)
```

## Arguments

- ...:

  Named fields. A bare string is shorthand for
  `gr_field(string, type = "string")`.

## Value

A `gr_fields` object.

## Naming the fields

The names become column names, so keep them short and syntactic. The
*descriptions* carry the instruction, and they are worth writing
carefully: most extraction disagreements come from an ambiguous field
description rather than from the model.

## See also

[`gr_field()`](https://elkronos.github.io/readgpt/reference/gr_field.md),
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md)

## Examples

``` r
fields <- gr_fields(
  design  = "The study design, e.g. randomised controlled trial, cohort, case series",
  n       = gr_field("Number of participants randomised, not the number analysed",
                     type = "integer"),
  outcome = gr_field("Direction of the primary result",
                     type = "enum", values = c("positive", "null", "mixed")),
  funded  = gr_field("Whether industry funding is declared", type = "boolean")
)
fields
#> <gr_fields> 4 field(s)
#>   design         string    The study design, e.g. randomised controlled trial, cohort, 
#>   n              integer   Number of participants randomised, not the number analysed
#>   outcome        enum      Direction of the primary result
#>   funded         boolean   Whether industry funding is declared
names(fields)
#> [1] "design"  "n"       "outcome" "funded" 
```
