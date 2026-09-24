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
  trace = NULL
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

## Value

A named character vector shaped exactly like the `outline` argument of
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
(headings as names, briefs as values), carrying `attr(, "claims")` (a
`section`/`claim_id` frame), `attr(, "rationale")` and
`attr(, "closing")`.

## What is verified

Every claim is assigned to exactly one section. A claim number that does
not exist is dropped; a claim assigned twice keeps its first section; a
claim the reply never placed is put in the closing section rather than
lost, with a message saying so. A section left with no claims is
removed, except the closing one, which is allowed to be empty because it
is where
[`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md)
writes and gaps are not claims.

## See also

[`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md),
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md),
[`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md)

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
attr(o, "claims")
#>         section claim_id
#> 1 What it finds        1
```
