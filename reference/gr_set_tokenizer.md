# Register or inspect the active tokenizer

Register or inspect the active tokenizer

## Usage

``` r
gr_set_tokenizer(name, fn = NULL)
```

## Arguments

- name:

  One of the built-in tokenizers (`"heuristic"`, `"words"`, `"chars"`,
  `"tiktoken"`) or a name previously registered with a custom function.

- fn:

  Optional. A function taking a character vector and returning an
  integer vector of token counts. Supplying it registers a custom
  tokenizer under `name`.

## Value

Invisibly, the name of the newly active tokenizer.

## See also

[`gr_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_tokenizer.md),
[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md),
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)

Other cost and token functions:
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_model_limits()`](https://elkronos.github.io/readgpt/reference/gr_model_limits.md),
[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
[`gr_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_tokenizer.md),
[`gr_truncate_tokens()`](https://elkronos.github.io/readgpt/reference/gr_truncate_tokens.md)

## Examples

``` r
old <- gr_tokenizer()
gr_set_tokenizer("heuristic")
gr_count_tokens("the quick brown fox")
#> [1] 8

# Register your own; budgets recompute against it immediately.
gr_set_tokenizer("naive_words",
                 function(x) lengths(strsplit(trimws(x), "\\s+")))
gr_count_tokens("the quick brown fox")
#> [1] 4
gr_set_tokenizer(old)
```
