# Record how the search was run

The part of a review that no tool can infer and every review must state:
which databases were searched, with what query, on what date, and what
limits were applied. PRISMA items 6 and 7 ask for it, and item 7 asks
for the full strategy for at least one database *so that it could be
repeated*.

## Usage

``` r
gr_search(
  databases,
  dates = NULL,
  limits = NULL,
  registration = NA_character_,
  other = NULL,
  notes = NULL
)
```

## Arguments

- databases:

  Named character vector or list: name of each source, value the query
  string run against it. The query is the point; a review reporting "we
  searched PubMed" without it cannot be repeated.

- dates:

  When each search was run, as `YYYY-MM-DD`. One value, or one per
  database. Searches drift: a review is a statement about a date.

- limits:

  Limits applied: language, publication years, study design filters.
  Free text; a review states them whatever they were.

- registration:

  Registration identifier (a PROSPERO number, say), or `NA` and say so.
  PRISMA item 24 asks either way.

- other:

  Any further sources: reference lists checked, experts contacted,
  registers, preprint servers, hand-searched journals.

- notes:

  Anything else the methods section has to say.

## Value

An object of class `gr_search`.

## Details

This function computes nothing. It exists so the answer travels with the
run instead of living in a lab notebook. It reaches
[`gr_flow()`](https://elkronos.github.io/readgpt/reference/gr_flow.md)
and the audit report, and it round-trips through
[`gr_protocol_save()`](https://elkronos.github.io/readgpt/reference/gr_protocol_save.md)
alongside the criteria, so what was searched and what was eligible are
one artifact.

## See also

[`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md),
[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md),
[`gr_flow()`](https://elkronos.github.io/readgpt/reference/gr_flow.md)

Other corpus functions:
[`gr_calibrate()`](https://elkronos.github.io/readgpt/reference/gr_calibrate.md),
[`gr_inventory()`](https://elkronos.github.io/readgpt/reference/gr_inventory.md),
[`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md),
[`gr_reference()`](https://elkronos.github.io/readgpt/reference/gr_reference.md)

## Examples

``` r
gr_search(
  databases = c(PubMed = "(spaced practice[tiab]) AND (retention[tiab])",
                Scopus = "TITLE-ABS-KEY(\"spaced practice\" AND retention)"),
  dates = "2026-02-14",
  limits = "English; 2000 onwards; primary studies only",
  registration = "PROSPERO CRD42026000000",
  other = "Reference lists of included studies hand-searched"
)
#> <gr_search> 2 source(s)
#>   PubMed  (searched 2026-02-14)
#>     (spaced practice[tiab]) AND (retention[tiab])
#>   Scopus  (searched 2026-02-14)
#>     TITLE-ABS-KEY("spaced practice" AND retention)
#>   limits : English; 2000 onwards; primary studies only
#>   also   : Reference lists of included studies hand-searched
#>   registration: PROSPERO CRD42026000000
```
