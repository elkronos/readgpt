# Register a document extractor

Register a document extractor

## Usage

``` r
gr_register_extractor(name, extensions, fn, description = "")
```

## Arguments

- name:

  Short name for the extractor.

- extensions:

  Character vector of file extensions it claims.

- fn:

  Function of `(path, opts)` returning a data frame with columns `text`,
  and optionally `page`, `section`, `kind`. Pages it could not turn into
  text can be listed in `attr(result, "gr_unread_pages")`; an answer
  drawn from the document is then marked partial, as it is for a PDF
  page that needed OCR and did not get it.

- description:

  One-line description shown by
  [`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md).

## Value

Invisibly, `name`.

## See also

[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md)

Other ingest functions:
[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md),
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md),
[`gr_document`](https://elkronos.github.io/readgpt/reference/gr_document.md),
[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md)

## Examples

``` r
# A minimal extractor for tab-separated files: one block per row.
gr_register_extractor("tsv", "tsv", description = "TSV, one block per row",
  fn = function(path, opts) {
    d <- utils::read.delim(path, stringsAsFactors = FALSE)
    data.frame(text = apply(d, 1, paste, collapse = " | "), kind = "table")
  })
subset(gr_extractors(), name == "tsv")
#>     name extensions            description needs available
#> tsv  tsv        tsv TSV, one block per row            TRUE
```
