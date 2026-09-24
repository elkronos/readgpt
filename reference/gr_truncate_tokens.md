# Truncate text to at most `n` tokens

Truncation happens at a word boundary where the text has whitespace, and
at a character boundary where it does not (base64, a data URI, a CJK
run, a minified line); otherwise the cap would be unenforceable for
exactly the inputs that most need it. Returns `""` for empty input and
never returns `NA`.

## Usage

``` r
gr_truncate_tokens(text, n, marker = " ...[truncated]")
```

## Arguments

- text:

  A single string.

- n:

  Maximum token count.

- marker:

  Appended when truncation occurred; set `""` to suppress.

## Value

A single string. `""` when `n <= 0` or the input is blank. This is not
treated as an error.

## See also

[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md)

Other cost and token functions:
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_model_limits()`](https://elkronos.github.io/readgpt/reference/gr_model_limits.md),
[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
[`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md),
[`gr_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_tokenizer.md)

## Examples

``` r
gr_truncate_tokens(paste(rep("alpha beta gamma", 40), collapse = " "), 20)
#> [1] "alpha beta gamma alpha beta gamma alpha beta ...[truncated]"
gr_truncate_tokens("short enough already", 100)
#> [1] "short enough already"
```
