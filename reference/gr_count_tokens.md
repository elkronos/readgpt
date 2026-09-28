# Count tokens in text

Vectorised over `text`. Always returns a non-negative integer vector of
the same length; blank and `NA` entries count as 0.

## Usage

``` r
gr_count_tokens(text, model = NULL)
```

## Arguments

- text:

  Character vector.

- model:

  Optional model id; used only by tokenizers that are encoding-specific
  (e.g. `"tiktoken"`).

## Value

Integer vector of token counts: under the default tokenizer, an estimate
built to err high (see above for the one kind of text where it can fall
short).

## Details

The default `"heuristic"` tokenizer is a deliberate **over-estimate**,
not an exact count: it classifies each word, sums the per-script
contributions, and adds a small per-message framing allowance. For
numbers, tables, code and short lines it also counts the pieces a BPE
tokenizer splits text into first (digit groups, punctuation runs, line
breaks, indentation) and keeps the larger figure, line by line.
Overcounting wastes a little context; undercounting produces a hard API
failure after you have paid for the request. Rare long words (technical
and medical vocabulary) can still come in under the real count, by about
a tenth for a table of medical terms; that is what the `safety_margin`
of
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)
is for.

For counts from OpenAI's own tokenizer install reticulate plus Python
`tiktoken` and call `gr_set_tokenizer("tiktoken")`. Given `model`, it
counts in that model's encoding. Without one, which is how the package's
own budgets call it, it counts in both encodings current OpenAI models
use (`cl100k_base` and `o200k_base`) and keeps the larger, so the count
holds for any of them. Models from other providers tokenize differently,
and for them it is an estimate without the heuristic's padding. Text
that spells a special token (`<|endoftext|>`, in a paper about language
models) is counted as the plain text it is.

## See also

[`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md),
[`gr_truncate_tokens()`](https://elkronos.github.io/readgpt/reference/gr_truncate_tokens.md),
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)

Other cost and token functions:
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_model_limits()`](https://elkronos.github.io/readgpt/reference/gr_model_limits.md),
[`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
[`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md),
[`gr_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_tokenizer.md),
[`gr_truncate_tokens()`](https://elkronos.github.io/readgpt/reference/gr_truncate_tokens.md)

## Examples

``` r
gr_count_tokens(c("the quick brown fox", "", "a much longer sentence than that one"))
#> [1]  8  0 12

# Compare tokenizers on the same text.
old <- gr_tokenizer()
vapply(c("heuristic", "words", "chars"), function(t) {
  gr_set_tokenizer(t); gr_count_tokens("the quick brown fox jumps")
}, integer(1))
#> heuristic     words     chars 
#>         9         5         7 
gr_set_tokenizer(old)
```
