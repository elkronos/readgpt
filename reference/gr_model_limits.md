# Context and output limits for a model

A two-field convenience wrapper over
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md).

## Usage

``` r
gr_model_limits(model = NULL)
```

## Arguments

- model:

  Model id.

## Value

A list with exactly two elements: `context_window` and `output_tokens`.
(Note the second name differs from
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md)'s
`max_output`.)

## See also

[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)

Other cost and token functions:
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
[`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md),
[`gr_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_tokenizer.md),
[`gr_truncate_tokens()`](https://elkronos.github.io/readgpt/reference/gr_truncate_tokens.md)

## Examples

``` r
gr_model_limits("gpt-4o")
#> $context_window
#> [1] 128000
#> 
#> $output_tokens
#> [1] 16384
#> 
```
