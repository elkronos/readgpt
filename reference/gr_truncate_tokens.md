# Truncate text to at most `n` tokens

The result is the start of `text` as written – line breaks, indentation
and paragraph breaks kept – followed by `marker`. The cut falls at the
end of a word where the text has whitespace. When the next unit is too
long to be a word (a CJK run, base64, a data URI, minified JSON, a long
URL: anything that alone would cost more than 16 tokens) the cut goes
inside it at a character, so a long unbroken run fills the budget
instead of being dropped whole; otherwise the cap would be
unenforceable, or nearly all of it wasted, for exactly the inputs that
most need it. Returns `""` for empty input and never returns `NA`.

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

  Appended when truncation occurred; set `""` to suppress. It is dropped
  when it would cost half the budget or more.

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
#> [1] "alpha beta gamma alpha beta gamma alpha ...[truncated]"
gr_truncate_tokens("short enough already", 100)
#> [1] "short enough already"

# A table keeps its rows.
cat(gr_truncate_tokens("Arm | N | Events\nA | 482 | 31\nB | 479 | 44\nC | 470 | 50", 24))
#> Arm | N | Events
#> A | 482 | ...[truncated]
```
