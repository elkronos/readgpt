# The claims a table of studies supports

Turns an extraction table into statements about the *literature*, each
naming the studies it rests on. It is the step that makes a write-up a
synthesis rather than an ordered recitation, and the step that lets a
section be written from an argument instead of from rows.

## Usage

``` r
gr_claims(
  extraction,
  question = NULL,
  protocol = NULL,
  client = NULL,
  model = NULL,
  temperature = NULL,
  max_claim_tokens = 1600L,
  include_unclear = FALSE,
  trace = NULL
)
```

## Arguments

- extraction:

  A
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  result, or a data frame shaped like its `$table`.

- question:

  The review question. Taken from `protocol` if omitted.

- protocol:

  A
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md);
  its `question` is used when `question` is not given.

- client:

  A
  [`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md).
  One is built from `model` if omitted.

- model, temperature, max_claim_tokens:

  Passed to the model call.

- include_unclear:

  Keep rows with nothing extracted. Off by default, the same as
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md).

- trace:

  A
  [`gr_trace()`](https://elkronos.github.io/readgpt/reference/gr_trace.md)
  to record into.

## Value

An object of class `gr_claims`:

- `claims`:

  One row per claim: `claim_id`, `claim`, `kind`, `moderator`, `scope`,
  `n_support`, `n_contradict`, `note`.

- `support`:

  Long: `claim_id`, `study`, `role` (`"supports"` or `"contradicts"`).
  Join it to `$studies` to reach documents and quotes.

- `studies`:

  The rows the claims were drawn from, numbered.

- `dropped`:

  What verification removed, and why.

## What makes a claim checkable

Every study number a claim names is verified against the table, exactly
as a `[study N]` marker in finished prose already is. A number that is
not there is dropped and counted rather than trusted, a claim left with
no supporting study is dropped entirely, and a `moderator` naming a
column the table does not have is cleared: it is an invented explanation
for a real disagreement. `$dropped` records all of it, so a claims table
that looks thin can be told apart from a literature that is.

The study numbers are the same ones
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
cites, because both derive them from one function. A claim resting on
study 3 and a sentence citing `[study 3]` point at the same row by
construction.

## Corpora too large for one prompt

The studies are batched, and claims are then reconciled across batches
in one further call that sees only the claim TEXTS. Without it a claim
holding across the whole corpus comes back once per batch with disjoint
support, which reads as several narrow claims instead of one broad one.
The reconcile pass may only group claims that already exist: every claim
it fails to place stays on its own rather than disappearing.

## See also

[`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
to derive sections from these claims,
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
to write from them,
[`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md)
for what they do not cover,
[`gr_protocols()`](https://elkronos.github.io/readgpt/reference/gr_protocols.md)
for the `claims` schema they want as input

## Examples

``` r
tab <- data.frame(
  document = c("a.pdf", "b.pdf"), status = "ok", duplicate_of = NA_character_,
  n_filled = 2L, n_unverified = 0L, conflicts = NA_character_,
  design = c("randomised trial", "cross-sectional"),
  finding = c("supports", "contradicts"), stringsAsFactors = FALSE)

cl <- gr_mock_client(function(messages, params) paste0(
  '{"claims":[{"claim":"The effect appears in trials but not in surveys.",',
  '"kind":"finding","supported_by":[1],"contradicted_by":[2],',
  '"moderator":"design","scope":"one trial, one survey"}]}'))

cm <- gr_claims(tab, question = "Does it work?", client = cl)
cm$claims[, c("claim", "moderator", "n_support", "n_contradict")]
#>                                              claim moderator n_support
#> 1 The effect appears in trials but not in surveys.    design         1
#>   n_contradict
#> 1            1
cm$support
#>   claim_id study        role
#> 1        1     1    supports
#> 2        1     2 contradicts
```
