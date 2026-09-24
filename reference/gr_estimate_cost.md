# Estimate the USD cost of a set of calls

Estimate the USD cost of a set of calls

## Usage

``` r
gr_estimate_cost(model, input_tokens, output_tokens = 0)
```

## Arguments

- model:

  Model id.

- input_tokens, output_tokens:

  Token counts (scalars or vectors; vectors are summed).

## Value

A single numeric USD figure, or `NA_real_` when the model has no pricing
in the registry. Pricing is seeded from the registry's `as_of` snapshot.
Treat it as an estimate, and use
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md)
to correct it.

## See also

[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
[`gr_trace_summary()`](https://elkronos.github.io/readgpt/reference/gr_trace_summary.md)

Other cost and token functions:
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md),
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_model_limits()`](https://elkronos.github.io/readgpt/reference/gr_model_limits.md),
[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
[`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md),
[`gr_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_tokenizer.md),
[`gr_truncate_tokens()`](https://elkronos.github.io/readgpt/reference/gr_truncate_tokens.md)

## Examples

``` r
gr_estimate_cost("gpt-4o", input_tokens = 120000, output_tokens = 4000)
#> [1] 0.34
gr_estimate_cost("a-model-with-no-pricing", 1000, 100)
#> Warning: Model 'a-model-with-no-pricing' is not in the registry. Falling back to a conservative 128000-token context with a 4096-token output reserve. Register the real limits with gr_register_model('a-model-with-no-pricing', context_window = ..., max_output = ...) so budgets and cost estimates are correct.
#> [1] NA
```
