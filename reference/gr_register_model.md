# Register a model (or override a built-in entry)

Use this whenever the shipped registry is stale or you are pointing the
client at a compatible non-OpenAI endpoint. Registered entries take
precedence over everything built in.

## Usage

``` r
gr_register_model(
  id,
  context_window,
  max_output,
  input_usd = NA_real_,
  output_usd = NA_real_,
  reasoning = FALSE,
  supports_temperature = TRUE,
  kind = c("chat", "embedding"),
  dimensions = NA_integer_
)
```

## Arguments

- id:

  Model id string, exactly as the API expects it.

- context_window:

  Total context window in tokens.

- max_output:

  Maximum tokens the model will emit in one response.

- input_usd, output_usd:

  Price per 1M tokens; used for cost estimates.

- reasoning:

  Whether this is a reasoning model (affects prompt shape).

- supports_temperature:

  Whether the API accepts `temperature`.

- kind:

  `"chat"` or `"embedding"`.

- dimensions:

  Embedding dimensionality, for `kind = "embedding"`.

## Value

Invisibly, `id`.

## See also

[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md)
to see the registry,
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md)
for lookup and its `certain` flag,
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md)

Other cost and token functions:
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_model_limits()`](https://elkronos.github.io/readgpt/reference/gr_model_limits.md),
[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md),
[`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md),
[`gr_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_tokenizer.md),
[`gr_truncate_tokens()`](https://elkronos.github.io/readgpt/reference/gr_truncate_tokens.md)

## Examples

``` r
gr_register_model("my-local-llama", context_window = 32768, max_output = 4096)
gr_model_info("my-local-llama")[c("context_window", "source", "certain")]
#> $context_window
#> [1] 32768
#> 
#> $source
#> [1] "registered"
#> 
#> $certain
#> [1] TRUE
#> 

# Without prices, the cost cap cannot be checked; readgpt says so rather
# than assuming the run is free.
is.na(gr_estimate_cost("my-local-llama", 1000, 500))
#> [1] TRUE
```
