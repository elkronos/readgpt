# Count what happened to every document

The numbers a flow diagram is made of, from what a run already recorded.
Every source is accounted for at every stage it reached, so the
arithmetic closes: nothing leaves the count without a row saying why.

## Usage

``` r
gr_flow(screening = NULL, extraction = NULL, records = NULL, claims = NULL)
```

## Arguments

- screening:

  A `gr_screening` from
  [`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md),
  or `NULL`.

- extraction:

  A `gr_extraction` from
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
  or `NULL`.

- records:

  A
  [`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md),
  so the counts begin where the search did: records identified,
  duplicates removed, reports sought and reports never retrieved.
  Without one the diagram starts at "sources given", which is already
  past the step that decides whether the review can be repeated.

- claims:

  A
  [`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
  result, to add the claim-level counts.

## Value

A data frame of `stage`, `n` and `note`.

## See also

[`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md),
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md),
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)

## Examples

``` r
cl <- gr_mock_client(function(messages, params) {
  '{"decision":"include","reason":"Reports a revenue figure.",
    "criterion":"Reports a revenue figure","quote":null}'
})
f <- tempfile(fileext = ".txt"); writeLines("Revenue was 45.2 million.", f)
s <- gr_screen(f, question = "What was revenue?",
               include = "Reports a revenue figure", client = cl)
#> [1/1] file1dcd1d8406e4.txt
#> Extracting 'file1dcd1d8406e4.txt' with the 'txt' extractor.
#> Ingested 1 block(s), ~11 tokens (0 chars removed by cleaning).
#> Segmenting with 'structural' (cap 900 tokens, overlap 90).
#> Reading with 'screen' (head|1|none) over 1 chunk(s).
gr_flow(s)
#>                 stage n                                                  note
#> 1       sources given 1                                                      
#> 2  duplicates removed 0          same cleaned text as a document already seen
#> 3            screened 1                                                      
#> 4             include 1                                                      
#> 5             exclude 0                                                      
#> 6             unclear 0 the excerpt did not settle it; for a person to decide
#> 7   could not be read 0       no decision was recorded; these are outstanding
```
