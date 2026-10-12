# Choose one of the outlines gr_outline() compared

[`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
with `alternatives` above 1 returns its first proposal and attaches the
others it kept. This hands back the one you choose, with its own claim
assignment, rationale and closing section, so the headings, the claims
placed under each and the claims they were derived from stay one
consistent object. Copying `attr(, "claims")` from one outline onto
another's headings is what this avoids:
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
refuses a claim placed under a heading the outline does not have.

## Usage

``` r
gr_outline_pick(outline, alternative)
```

## Arguments

- outline:

  A
  [`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
  result made with `alternatives` above 1.

- alternative:

  Which outline, by its number in
  `attr(outline, "comparison")$alternative`: 1 is the first proposal.

## Value

The outline chosen, shaped like any
[`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
result, without the alternatives and comparison attached.

## Details

Nothing here ranks the outlines. `attr(outline, "comparison")` lays them
side by side: how many sections each has, how many claims and how many
contested claims each section holds, and how far each lies from the
first proposal. Which grouping makes the review's argument is the
author's call.

The outline returned carries `attr(, "alternative")`, which says which
one it is.
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
records it in `$outline_alternative` and in its trace, and
[`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md)
says which was used, so a review written from the second proposal says
so. Renaming its headings keeps that record. Moving claims between its
sections does not: the grouping is then your own, and the review is
recorded as one written from an outline of your own would be.

## See also

[`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md),
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)

## Examples

``` r
tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok",
                  duplicate_of = NA_character_, n_filled = 1L, n_unverified = 0L,
                  conflicts = NA_character_, design = c("trial", "survey"),
                  stringsAsFactors = FALSE)
cl <- gr_mock_client(function(messages, params) {
  seen <- paste(unlist(lapply(messages, `[[`, "content")), collapse = " ")
  if (grepl("<earlier_outlines>", seen, fixed = TRUE)) {
    return(paste0('{"sections":[{"heading":"Trials find it","brief":"the trials",',
                  '"claims":[1],"rationale":"design"},{"heading":"Surveys do not",',
                  '"brief":"the surveys","claims":[2],"rationale":"design"}]}'))
  }
  if (grepl("<claims>", seen, fixed = TRUE)) {
    return(paste0('{"sections":[{"heading":"What it finds","brief":"the claims",',
                  '"claims":[1,2],"rationale":"one topic"}]}'))
  }
  paste0('{"claims":[{"claim":"It works in trials.","kind":"finding",',
         '"supported_by":[1],"contradicted_by":[],"moderator":null,"scope":null},',
         '{"claim":"Surveys find no effect.","kind":"finding",',
         '"supported_by":[2],"contradicted_by":[],"moderator":null,"scope":null}]}')
})
cm <- gr_claims(tab, question = "Does it work?", client = cl)
o <- gr_outline(cm, client = cl, alternatives = 2)
attr(o, "comparison")[, c("alternative", "sections", "claims_per_section", "distance")]
#>   alternative sections claims_per_section distance
#> 1           1        2               2, 0        0
#> 2           2        3            1, 1, 0        1
o2 <- gr_outline_pick(o, 2)
names(o2)
#> [1] "Trials find it"  "Surveys do not"  "What is missing"
attr(o2, "claims")
#>          section claim_id
#> 1 Trials find it        1
#> 2 Surveys do not        2
```
