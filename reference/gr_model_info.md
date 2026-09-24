# Look up a model's capabilities

Resolution order: user-registered exact id, built-in exact id, alias,
family regex, then a conservative default. The returned list always
carries `certain`, which is `FALSE` when the answer came from a regex or
the default. Treat that as "verify before trusting for cost control".

## Usage

``` r
gr_model_info(model = NULL)
```

## Arguments

- model:

  Model id.

## Value

A named list: `id`, `context_window`, `max_output`, `input_usd` and
`output_usd` (per 1M tokens, used by
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md)),
`reasoning`, `supports_temperature`, `kind`, `dimensions`, `as_of`,
`certain`, and `source` (`"registered"`, `"builtin"`, `"alias"`,
`"pattern:..."` or `"default"`). `certain` is `FALSE` when the answer
came from a family regex or the fallback; verify before relying on it
for cost control.

## See also

[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md),
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)

Other cost and token functions:
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
[`gr_model_limits()`](https://elkronos.github.io/readgpt/reference/gr_model_limits.md),
[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
[`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md),
[`gr_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_tokenizer.md),
[`gr_truncate_tokens()`](https://elkronos.github.io/readgpt/reference/gr_truncate_tokens.md)

## Examples

``` r
gr_model_info("gpt-4o")[c("context_window", "max_output", "certain")]
#> $context_window
#> [1] 128000
#> 
#> $max_output
#> [1] 16384
#> 
#> $certain
#> [1] TRUE
#> 

# An unrecognised id warns and falls back to conservative limits.
suppressWarnings(gr_model_info("some-model-from-next-year")$certain)
#> [1] FALSE
```
