# Compute a usable input-token budget for one model call

This is the single arithmetic chokepoint that every prompt-building path
in the package must go through. It has three guarantees:

## Usage

``` r
gr_budget(
  model = NULL,
  reserve_output = NULL,
  overhead = 0,
  safety_margin = NULL
)
```

## Arguments

- model:

  Model id.

- reserve_output:

  Tokens to reserve for the completion. An explicit value is clamped to
  the model's `max_output`. `NULL` reserves
  `min(max_output, max(min_output_tokens, usable / 4))`, where `usable`
  is the context window after `safety_margin` and `min_output_tokens` is
  a
  [`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)
  setting (default 256).

- overhead:

  Tokens consumed by system prompts, question text and message framing
  that are not part of the document payload.

- safety_margin:

  Fraction of the context window left unused to absorb tokenizer error.
  Defaults to the `safety_margin` option; clamped to \[0, 0.5\].

## Value

A list with `input`, `output`, `context_window`, `overhead`, `margin`
and `certain` (`FALSE` when the model's limits were guessed rather than
known; see
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md)).

## Details

- the returned `input` budget is always **strictly positive**;

- `input + output <= context_window * (1 - safety_margin)`;

- if no positive budget exists, it raises a `gr_budget_error` naming the
  parameter to change, rather than returning a negative number that
  silently corrupts every downstream
  [`split()`](https://rdrr.io/r/base/split.html) and `while` loop.

## See also

[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md),
[`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md)

Other cost and token functions:
[`gr_count_tokens()`](https://elkronos.github.io/readgpt/reference/gr_count_tokens.md),
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
unlist(gr_budget("gpt-4o", reserve_output = 1024, overhead = 200))
#>          input         output context_window       overhead         margin 
#>       113976.0         1024.0       128000.0          200.0            0.1 
#>        certain 
#>            1.0 

# The same question leaves very different room on different models.
vapply(c("gpt-4", "gpt-4o", "gpt-5.6-terra"),
       function(m) gr_budget(m)$input, numeric(1))
#>         gpt-4        gpt-4o gpt-5.6-terra 
#>          5529         98816        817000 

# No positive budget is an error naming what to change, never a negative
# number that silently reverses the document downstream.
tryCatch(gr_budget("gpt-4", overhead = 9000),
         gr_budget_error = function(e) conditionMessage(e))
#> [1] "No positive input budget for model 'gpt-4': context 8192, usable after a 10% safety margin 7372, output reserve 256, fixed overhead 9000. Reduce `reserve_output`, shorten the question/system prompt, or use a model with a larger context window."
```
