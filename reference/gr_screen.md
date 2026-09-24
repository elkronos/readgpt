# Decide which documents a review should read

The stage before
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md).
One model call per document, a decision and a reason for every one, and
nothing dropped on the way.

## Usage

``` r
gr_screen(
  sources,
  protocol = NULL,
  question = NULL,
  include = NULL,
  exclude = NULL,
  recipe = "research",
  client = NULL,
  store = NULL,
  screen_tokens = NULL,
  on_error = c("continue", "stop"),
  max_total_usd = NULL,
  max_total_calls = NULL,
  keep_answers = FALSE,
  recursive = FALSE,
  trace = NULL,
  ...
)
```

## Arguments

- sources:

  As
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md):
  file paths, a directory, or raw text.

- protocol:

  A
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md)
  carrying the criteria and the review question. Give this, or
  `include`/`exclude` directly.

- question, include, exclude:

  The review question and the criteria, if you are not passing a
  protocol. Each criterion is one statement a document either meets or
  does not.

- recipe, client, store, on_error, max_total_usd, recursive,
  keep_answers:

  As
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md).

- screen_tokens:

  Cap what the model is shown, in tokens, counted from the start of the
  document. Leaving it unset shows as much as the model's context
  allows. Setting it to a few hundred is title-and-abstract screening,
  done deliberately: cheaper, and closer to what a human screener sees
  at this stage. Either way the `truncated` and `seen_tokens` columns
  say what was actually read.

- max_total_calls, trace:

  As
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md).
  Pass the same `trace` to
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  and
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
  and it accumulates the whole review, while each stage keeps its own
  for its own cost and its own ceiling.

- ...:

  Recipe overrides, as in
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md).

## Value

An object of class `gr_screening`:

- `table`:

  One row per document: `document`, `document_id`, `decision`, `reason`,
  `criterion`, `quote`, `verified`, `seen_tokens`, `document_tokens`,
  `truncated`, `status`, `duplicate_of`, `error`.

- `included`:

  The distinct sources whose decision was `"include"` (the paths, not
  the display labels). Duplicates are left out; they are the same study,
  and `table` still has their rows.
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  takes this, but prefer handing it the whole screening object: a
  character vector of paths cannot carry the search, so
  `gr_extract(screened)` keeps it and `gr_extract(screened$included)`
  does not.

- `records`:

  The
  [`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md)
  the run was made over, or `NULL`.

- `summary`,`answers`,`trace`,`store`:

  As
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md).

## Three decisions, not two

`decision` is `"include"`, `"exclude"` or `"unclear"`. The third is not
a failure mode; it is the answer when the excerpt does not settle the
question, and it is what stops an uncertain call from being recorded as
a confident one. Those documents are for a person to look at. Every
published evaluation of automated screening reaches the same conclusion:
reliable on the easy calls, not yet trustworthy alone on the hard ones.

A document that could not be read at all gets `status = "failed"` and no
decision. It is not excluded, and it is not silently absent: it is an
outstanding job. A review whose denominator is unknown is not a review.

## Reporting it

`table(x$table$decision)` is the screening result and
`table(x$table$criterion)` is the breakdown by exclusion criterion,
which is what a flow diagram asks for. Duplicates were removed before
screening, so `sum(!is.na(x$table$duplicate_of))` is the "duplicates
removed" count and every one of them still has a row; see
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md).

## See also

[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md),
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)

## Examples

``` r
cl <- gr_mock_client(function(messages, params) {
  '{"decision":"include","reason":"Reports a randomised comparison.",
    "criterion":"Reports a randomised comparison",
    "quote":"We randomly assigned participants to two groups."}'
})

f <- tempfile(fileext = ".txt")
writeLines("We randomly assigned participants to two groups.", f)

s <- gr_screen(f, question = "Does the treatment work?",
               include = "Reports a randomised comparison", client = cl)
#> [1/1] file1dcd145a5e43.txt
#> Extracting 'file1dcd145a5e43.txt' with the 'txt' extractor.
#> Ingested 1 block(s), ~15 tokens (0 chars removed by cleaning).
#> Segmenting with 'structural' (cap 900 tokens, overlap 90).
#> Reading with 'screen' (head|1|none) over 1 chunk(s).
s$table[, c("document", "decision", "reason")]
#>               document decision                           reason
#> 1 file1dcd145a5e43.txt  include Reports a randomised comparison.
```
