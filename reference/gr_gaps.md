# What a body of work does not cover

Reviews are read for the gap, and a gap a model was asked to notice is
an impression. This computes them instead, in R, from the extraction
table and the claims: a declared category nobody studied, a dimension
with no variation, an empty cell in a crosstab, a claim resting on one
study that nobody has tried to replicate, a disagreement nothing in the
table explains. Every row is a fact about the corpus that can be checked
by counting.

## Usage

``` r
gr_gaps(claims, extraction = NULL, max_cells = 40L, min_reported = 0.5)
```

## Arguments

- claims:

  A
  [`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
  result.

- extraction:

  The
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  result the claims came from. Only its `$fields` is used, and only to
  learn which categories were *declared*; without it a category nobody
  studied is indistinguishable from one nobody thought of.

- max_cells:

  Cap on reported empty combinations, which grow as the product of two
  fields' sizes.

- min_reported:

  A field missing for more than this fraction of studies is reported as
  not reported.

## Value

A data frame of class `gr_gaps`: `kind`, `dimension`, `detail`, `n`.

## Details

No model is called.
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
takes the result as `gaps =` and hands it to the closing section with an
instruction to state these and no others, which is what keeps the
written gap list attached to the table.

## See also

[`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md),
[`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md),
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)

## Examples

``` r
tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok",
                  duplicate_of = NA_character_, n_filled = 1L, n_unverified = 0L,
                  conflicts = NA_character_, design = c("cohort", "cohort"),
                  stringsAsFactors = FALSE)
cl <- gr_mock_client(function(messages, params) paste0(
  '{"claims":[{"claim":"One cohort found it.","kind":"finding","supported_by":[1],',
  '"contradicted_by":[],"moderator":null,"scope":"one study"}]}'))
cm <- gr_claims(tab, question = "Does it work?", client = cl)
gr_gaps(cm)
#> <gr_gaps> 2 gap(s) over 2 study/studies
#>   no schema given, so a category nobody studied cannot be told from one
#>   nobody asked about; pass `extraction =` for those
#>   no variation               1
#>   unreplicated               1
```
