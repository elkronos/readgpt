# Read a search export, and say what the search found

Reads RIS or BibTeX exports from one or more databases, removes the
records that are the same work, and matches what is left to the
documents you have on disk. It is the step before
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md),
and it is where a review's numbers come from: how many records were
identified, how many duplicates went, how many reports were sought and
how many were never obtained.

## Usage

``` r
gr_records(
  exports,
  files = NULL,
  search = NULL,
  dedupe = c("doi+title", "doi", "none")
)
```

## Arguments

- exports:

  Paths to `.ris`, `.txt`, `.bib` or `.bibtex` files, or a directory
  containing them. Several exports from several databases is the normal
  case and is what the duplicate counts are for.

- files:

  A directory of documents, or a character vector of paths, to match
  records against. Optional: a record set is useful before anything has
  been downloaded.

- search:

  A
  [`gr_search()`](https://elkronos.github.io/readgpt/reference/gr_search.md)
  describing how the export was produced. Not required, and the reason
  to supply it is that a review has to report it.

- dedupe:

  `"doi"` matches on DOI alone; `"doi+title"` (the default) falls back
  to normalised title and year when a DOI is missing, which is what
  catches the same conference paper indexed twice; `"none"` keeps
  everything.

## Value

An object of class `gr_records`:

- `records`:

  One row per distinct work, with `duplicate_of` naming the row a
  dropped record repeats, `file` the document matched to it, and
  `retrieved` whether one was found.

- `counts`:

  Identified, per database, duplicates removed, distinct records,
  reports sought, reports retrieved, reports not retrieved.

- `search`:

  The
  [`gr_search()`](https://elkronos.github.io/readgpt/reference/gr_search.md),
  or `NULL`.

- `unmatched_files`:

  Documents on disk that no record claims, usually a sign the export and
  the folder are out of step.

## Details

Nothing here calls a model. Which paper a record is, and whether two
records are one paper, are questions a DOI answers exactly.

## Why start here rather than at a folder

A folder of PDFs cannot say which databases were searched, with what
query, on what date, or how many records came back (PRISMA items 6, 7
and 16), and no care further down substitutes for them. It also cannot
say what is *missing*: a record with no PDF is "report not retrieved",
which is a finding about the review, and a folder represents it as
nothing at all.

It fixes something quieter too.
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
cites by author and year, and without an export those come from asking a
model to read a title page. That is the one part of a citation that must
be exactly right, resting on the loosest guarantee in the pipeline. From
an export they are data.

## See also

[`gr_search()`](https://elkronos.github.io/readgpt/reference/gr_search.md),
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md),
[`gr_flow()`](https://elkronos.github.io/readgpt/reference/gr_flow.md),
[`gr_inventory()`](https://elkronos.github.io/readgpt/reference/gr_inventory.md)

Other corpus functions:
[`gr_calibrate()`](https://elkronos.github.io/readgpt/reference/gr_calibrate.md),
[`gr_inventory()`](https://elkronos.github.io/readgpt/reference/gr_inventory.md),
[`gr_reference()`](https://elkronos.github.io/readgpt/reference/gr_reference.md),
[`gr_search()`](https://elkronos.github.io/readgpt/reference/gr_search.md)

## Examples

``` r
ris <- tempfile(fileext = ".ris")
writeLines(c("TY  - JOUR", "AU  - Smith, J.", "TI  - A trial of spacing",
             "PY  - 2019", "DO  - 10.1000/abc", "ER  - "), ris)
recs <- gr_records(ris)
recs
#> <gr_records> 1 record(s) from 1 export(s)
#>   file1dcd620c2057.ris 1
#>   records identified       1
#>   duplicates removed       0
#>   records screened         1
#>   reports sought           1
#>   reports retrieved        0
#>   reports not retrieved    1
#>   ! 1 record(s) have no document. They are part of the review and are
#>     reported as sought-but-not-retrieved, not quietly dropped.
#>   no gr_search() attached: the review cannot report what was searched
recs$records[, c("authors", "year", "title", "doi", "retrieved")]
#>     authors year              title         doi retrieved
#> 1 Smith, J. 2019 A trial of spacing 10.1000/abc     FALSE
```
