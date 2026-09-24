# Path to the bundled example document

A short Markdown report with headings, figures and dates, used by the
examples in this package so they run without an API key or a file of
your own.

## Usage

``` r
readgpt_example()
```

## Value

Absolute path to the file.

## Examples

``` r
gr_ingest(readgpt_example())
#> Using cached ingestion for this document + settings.
#> <gr_document> /home/runner/work/_temp/Library/readgpt/extdata/annual_report.md
#>   17 blocks, ~573 tokens, 1847 chars (11 removed by cleaning)
#>   cleaners: page_numbers, hyphenation, control_chars, ligatures, collapse_whitespace
#>   first block: # Northwind Instruments -- Annual Report 2024
```
