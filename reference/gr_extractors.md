# List registered extractors

List registered extractors

## Usage

``` r
gr_extractors()
```

## Value

A data frame with `name`, `extensions`, `description`, `needs` (the
packages the extractor cannot run without, comma-separated, `""` for
none) and `available` (whether they are all installed now). OCR packages
are not in `needs`: a PDF with a text layer reads without them.

## See also

[`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md),
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md)

Other ingest functions:
[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md),
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md),
[`gr_document`](https://elkronos.github.io/readgpt/reference/gr_document.md),
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
[`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md)

## Examples

``` r
gr_extractors()
#>        name                          extensions
#> txt     txt            txt, text, log, csv, tsv
#> md       md              md, markdown, rmd, qmd
#> html   html                    html, htm, xhtml
#> pdf     pdf                                 pdf
#> docx   docx                          docx, dotx
#> image image png, jpg, jpeg, tif, tiff, bmp, gif
#>                                                                       description
#> txt                                         Plain text, including delimited files
#> md                                            Markdown, keeping heading structure
#> html                                     HTML via xml2, keeping heading structure
#> pdf                            PDF with per-page OCR fallback and page provenance
#> docx  Word: headings, tables by row, footnotes and endnotes; OCRs embedded images
#> image                                                                   Image OCR
#>           needs available
#> txt                  TRUE
#> md                   TRUE
#> html       xml2      TRUE
#> pdf    pdftools      TRUE
#> docx       xml2      TRUE
#> image tesseract      TRUE

# What this installation can read right now.
gr_extractors()[, c("name", "needs", "available")]
#>        name     needs available
#> txt     txt                TRUE
#> md       md                TRUE
#> html   html      xml2      TRUE
#> pdf     pdf  pdftools      TRUE
#> docx   docx      xml2      TRUE
#> image image tesseract      TRUE
```
