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
  trace = NULL,
  bib = NULL
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

  Passed to the model call. `max_claim_tokens` is the reply limit for
  each call, and it also sets how many studies go into one: about
  `(max_claim_tokens - 400) / 40`. Raise it for fewer, larger batches.

- include_unclear:

  Keep rows with nothing extracted. Off by default, the same as
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md).

- trace:

  A
  [`gr_trace()`](https://elkronos.github.io/readgpt/reference/gr_trace.md)
  to record into.

- bib:

  Which columns carry bibliographic identity, as in
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md):
  a named list of `citation`, `authors`, `year`, `title`, `venue`,
  `doi`. Those columns are withheld from the model, so a claim cannot
  attribute a finding to a name, and cannot be moderated by one.
  Omitted, only the conventional names are withheld. Pass the same `bib`
  to
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
  and
  [`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md).

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

- `partial`:

  `TRUE` when some studies contributed nothing because their batch was
  cut off at the reply limit, failed, or was not sent for a call or cost
  limit.

- `lost`:

  The study numbers of those studies.

- `unmerged`:

  `TRUE` when claims from several batches could not be reconciled, so
  one finding may appear as more than one claim.

- `hidden`:

  The columns withheld from the model as bibliographic.

## What makes a claim checkable

Every study number a claim names is verified against the table, exactly
as a `[study N]` marker in finished prose already is, and against the
batch the claim was drawn from: a claim may only name studies its call
was shown. A number that fails either is dropped and counted rather than
trusted, a claim left with no supporting study is dropped entirely, and
a `moderator` naming a column the model was not shown is cleared: it is
an invented explanation for a real disagreement. So is a moderator that
does not tell the two sides of a contested claim apart by counting: a
column the table does not report for the studies on one side, a value
reported on both sides, or numbers whose ranges overlap. `$dropped`
records all of it, so a claims table that looks thin can be told apart
from a literature that is.

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
it fails to place stays on its own rather than disappearing. When it
cannot run – a call or cost limit, a failed or cut-off reply, a reply
that is not a list of groups, or more claims than fit one prompt – the
claims are kept unmerged, with a warning, and `$unmerged` is `TRUE`.

What the reconcile pass cannot check is MEANING. A merged claim keeps
one member's wording and the union of every member's studies, so if the
model groups two claims that say different things, the studies behind
one are listed as supporting the other's wording. Such a merge is not
detectable by counting; the merged claim's `note` names every other
wording it absorbed so that it can be seen, and a group mixing claims of
different `kind` is warned about.

A batch is limited by the reply as well as by the context window,
because the reply names every study it uses: with the default
`max_claim_tokens` a batch holds about 30 studies. A batch whose reply
is cut off, or whose call fails, is reported with the number of studies
it held rather than passed over.

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
