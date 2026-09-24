# Register a protocol

Register a protocol

## Usage

``` r
gr_register_protocol(name, protocol)
```

## Arguments

- name:

  Registry key.

- protocol:

  A
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md).

## Value

The name, invisibly.

## See also

[`gr_protocols()`](https://elkronos.github.io/readgpt/reference/gr_protocols.md),
[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md)

## Examples

``` r
p <- gr_protocol("mine", question = "What did each report conclude?",
                 fields = gr_fields(conclusion = "The report's own conclusion"))
gr_register_protocol("mine", p)
gr_protocols("mine")$question
#> [1] "What did each report conclude?"
```
