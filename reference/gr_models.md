# List every known model

List every known model

## Usage

``` r
gr_models()
```

## Value

A data frame with `id`, `kind`, `context_window`, `max_output`,
`reasoning`, `input_usd_per_1m`, `output_usd_per_1m` and `as_of` (the
month the entry was recorded; a stale date is a prompt to verify).

## See also

[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md)

Other cost and token functions:
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_model_limits()`](https://elkronos.github.io/readgpt/reference/gr_model_limits.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
[`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md),
[`gr_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_tokenizer.md),
[`gr_truncate_tokens()`](https://elkronos.github.io/readgpt/reference/gr_truncate_tokens.md)

## Examples

``` r
subset(gr_models(), kind == "chat")[, c("id", "context_window", "as_of")]
#>               id context_window   as_of
#> 1  gpt-3.5-turbo          16385 2026-08
#> 2          gpt-4           8192 2026-08
#> 3    gpt-4-turbo         128000 2026-08
#> 4        gpt-4.1        1047576 2026-08
#> 5   gpt-4.1-mini        1047576 2026-08
#> 6   gpt-4.1-nano        1047576 2026-08
#> 7         gpt-4o         128000 2026-08
#> 8    gpt-4o-mini         128000 2026-08
#> 9          gpt-5         400000 2026-08
#> 10    gpt-5-mini         400000 2026-08
#> 11    gpt-5-nano         400000 2026-08
#> 12       gpt-5.4         400000 2026-08
#> 13  gpt-5.4-mini         400000 2026-08
#> 14       gpt-5.5         400000 2026-08
#> 15  gpt-5.6-luna        1050000 2026-08
#> 16   gpt-5.6-sol        1050000 2026-08
#> 17 gpt-5.6-terra        1050000 2026-08
#> 18    mock-model         128000 2026-09
```
