# Derive a review's sections from its claims

[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
takes an `outline` fixed before the reading, which makes the review's
structure the author's hypothesis rather than a finding. It is often the
strongest thing a review has to say: that a literature splits into three
incompatible operationalisations, say, and that the argument about
effect size is really an argument about measurement. This derives the
structure from the claims instead, and hands it back for you to accept
or replace.

## Usage

``` r
gr_outline(
  claims,
  question = NULL,
  client = NULL,
  model = NULL,
  temperature = NULL,
  max_sections = 6L,
  closing = "What is missing",
  trace = NULL,
  max_outline_tokens = NULL,
  effort = NULL,
  alternatives = 1L
)
```

## Arguments

- claims:

  A
  [`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
  result.

- question:

  The review question. Taken from `claims` if omitted.

- client:

  A
  [`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md).
  One is built from `model` if omitted.

- model, temperature:

  Passed to the model call.

- max_sections:

  Upper bound on sections, before the closing one.

- closing:

  The heading of the closing section, which receives gap claims and
  anything unplaced. `NULL` for no closing section.

- trace:

  A
  [`gr_trace()`](https://elkronos.github.io/readgpt/reference/gr_trace.md)
  to record into.

- max_outline_tokens:

  The reply limit for the outline call. The reply names every claim and
  writes a heading, brief and rationale per section, so by default it
  grows with both: `400 + 8` per claim `+ 60` per section, never below
  1200, plus 4000 on a model registered as a reasoning model, which
  spends part of the same limit before it writes. Clamped to what the
  model can emit. A reply cut off at it puts every claim in one section,
  with a warning.

- effort:

  Reasoning effort, or `NULL` (the default) to send none: one string, or
  a character vector named by stage, as `effort` in
  [`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md).
  The call here is labelled `outline.derive`, and each further
  alternative `outline.derive.alt2`, `outline.derive.alt3` and so on,
  which a name of `outline.derive` covers. A higher effort spends more
  of `max_outline_tokens` before the reply starts.

- alternatives:

  How many outlines to ask for, from 1 to 10. Each one past the first is
  one more call, made one after another. See "More than one outline".

## Value

A named character vector shaped exactly like the `outline` argument of
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
(headings as names, briefs as values), carrying `attr(, "claims")` (a
`section`/`claim_id` frame), `attr(, "rationale")`, `attr(, "closing")`
and `attr(, "claims_fingerprint")`, a hash of the claims' numbers and
texts. A claim number means something only within one
[`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
result, so
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
refuses the outline (`gr_claims_mismatch`) when the `claims` it is given
are not the ones the outline was derived from: a re-run, or claims
edited or reordered since.

With `alternatives` above 1 it also carries `attr(, "comparison")`,
`attr(, "alternatives")` (the other kept outlines, each shaped like this
one) and `attr(, "alternative")`, which says which outline of the
comparison it is.

## What is verified

Every claim is assigned to exactly one section. A claim number that does
not exist is dropped; a claim assigned twice keeps its first section; a
claim the reply never placed is put in the closing section rather than
lost, with a message saying so. A section left with no claims is
removed, except the closing one, which is allowed to be empty because it
is where
[`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md)
writes and gaps are not claims.

## More than one outline

The grouping is the review's argument, and one proposal gives nothing to
weigh it against. With `alternatives` above 1, each further outline is a
request of its own, shown the groupings proposed before it and asked to
group the claims by a different organising principle. Asking the same
question again would not do: the default model takes no temperature, and
the response cache answers an identical request with the reply it
stored. The first request is the one `alternatives = 1` sends, so the
first outline is the same whatever `alternatives` says, and a run
recorded without alternatives still replays. The others are labelled
`outline.derive.alt2` onwards in the trace, which also records which
were kept (`outline.compare`).

Each alternative is verified exactly as the first is. How far apart two
outlines are is then counted, not asked: the fraction of pairs of claims
that one puts in the same section and the other does not, 0 for the same
grouping under other headings and 1 for groupings that agree about no
pair. An alternative at 0 from an earlier one is dropped. So is one
whose call failed or was cut off, or none of whose sections held a claim
once verified, rather than replaced by a single section the way a failed
first outline is. None is requested when the first outline is such a
single section, or once the run reaches its call or cost limit. A
warning (`gr_outline_alternatives_short`) says when fewer outlines came
back distinct than were asked for, and why.

Nothing ranks them, and no model is asked which is best. The first
proposal is returned as it would be alone, carrying
`attr(, "comparison")`, one row per outline asked for: `alternative`,
`sections`, `claims_per_section` and `contested_per_section` (claims
with a contradicting study, per section, in heading order), `distance`
from the first, `nearest` (the distance to the closest earlier outline
kept), `kept`, a `note` saying why one was not kept, and the `headings`.
The kept alternatives are in `attr(, "alternatives")`. Choose one with
[`gr_outline_pick()`](https://elkronos.github.io/readgpt/reference/gr_outline_pick.md),
which keeps its headings and its claim assignment together.

## See also

[`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md),
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md),
[`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md),
[`gr_outline_pick()`](https://elkronos.github.io/readgpt/reference/gr_outline_pick.md)

## Examples

``` r
tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok",
                  duplicate_of = NA_character_, n_filled = 1L, n_unverified = 0L,
                  conflicts = NA_character_, finding = c("supports", "contradicts"),
                  stringsAsFactors = FALSE)
cl <- gr_mock_client(function(messages, params) {
  if (grepl("<claims>", paste(unlist(messages), collapse = " "), fixed = TRUE)) {
    return(paste0('{"sections":[{"heading":"What it finds","brief":"the claims",',
                  '"claims":[1],"rationale":"one topic"}]}'))
  }
  paste0('{"claims":[{"claim":"It works in trials.","kind":"finding",',
         '"supported_by":[1],"contradicted_by":[2],"moderator":null,"scope":null}]}')
})
cm <- gr_claims(tab, question = "Does it work?", client = cl)
o <- gr_outline(cm, client = cl)
o
#>                                            What it finds 
#>                                             "the claims" 
#>                                          What is missing 
#> "The questions this body of work cannot answer, and why" 
#> attr(,"claims")
#>         section claim_id
#> 1 What it finds        1
#> attr(,"rationale")
#>                                      What it finds 
#>                                        "one topic" 
#>                                    What is missing 
#> "Gaps, and anything the structure could not place" 
#> attr(,"closing")
#> [1] "What is missing"
#> attr(,"claims_fingerprint")
#> [1] "28ca983a36369f10"
attr(o, "claims")
#>         section claim_id
#> 1 What it finds        1
```
