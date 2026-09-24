# Describe an ingestion configuration

Describe an ingestion configuration

## Usage

``` r
gr_ingest_spec(
  clean = "standard",
  ocr = c("auto", "always", "never"),
  ocr_lang = "eng",
  ocr_dpi = 300,
  ocr_min_chars = 40L,
  min_chars = 20L,
  extractor = NULL,
  parallel = NULL,
  cleaner_opts = list(),
  layout = c("auto", "raw")
)
```

## Arguments

- clean:

  A preset name (`"none"`, `"minimal"`, `"standard"`, `"academic"`,
  `"scan"`, `"legacy"`) or a character vector of cleaner names from
  [`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md).

- ocr:

  `"auto"` (OCR pages with no text layer), `"always"`, or `"never"`.

- ocr_lang:

  Tesseract language code.

- ocr_dpi:

  Render density for PDF OCR.

- ocr_min_chars:

  A page with fewer characters than this is treated as needing OCR under
  `ocr = "auto"`. Zero or more; `Inf` marks every page. Anything else
  warns and uses 40.

- min_chars:

  Refuse the document if less than this much text survives.

- extractor:

  Force a specific extractor name instead of dispatching on the file
  extension.

- parallel:

  Parallelise page-level OCR.

- cleaner_opts:

  Extra options passed to individual cleaners.

- layout:

  How a PDF page's text is put in reading order. `"auto"` drops running
  heads and feet (short lines repeated at the top or bottom of many
  pages), reads a page set in two columns one column after the other,
  and marks headings, taken from the PDF's bookmarks or recognised as a
  standard section name ("Introduction", "Methods" and so on), so blocks
  carry sections. `"raw"` keeps each page as pdftools lays it out. Other
  formats are not affected. Stored in the spec only when it is not
  `"auto"`, so a default spec keeps the cache and store keys it had
  before the setting existed.

## Value

A list of class `gr_ingest_spec`.

## See also

[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md)
for the individual step names,
[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md)
to preview a configuration on your own text

Other ingest functions:
[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md),
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md),
[`gr_document`](https://elkronos.github.io/readgpt/reference/gr_document.md),
[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
[`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md)

## Examples

``` r
# What each preset costs you, on a real document, before any model call.
vapply(c("none", "minimal", "standard", "academic", "scan"),
       function(p) gr_ingest(readgpt_example(), gr_ingest_spec(clean = p))$stats$chars,
       integer(1))
#> Using cached ingestion for this document + settings.
#> Extracting 'annual_report.md' with the 'md' extractor.
#> Ingested 19 block(s), ~582 tokens (0 chars removed by cleaning).
#> Using cached ingestion for this document + settings.
#> Extracting 'annual_report.md' with the 'md' extractor.
#> Ingested 15 block(s), ~540 tokens (92 chars removed by cleaning).
#> Extracting 'annual_report.md' with the 'md' extractor.
#> Ingested 17 block(s), ~573 tokens (11 chars removed by cleaning).
#>     none  minimal standard academic     scan 
#>     1858     1858     1847     1766     1847 

# Or name the steps yourself; see gr_cleaners() for what is available.
gr_ingest_spec(clean = c("page_numbers", "references"), ocr = "never")$clean
#> [1] "page_numbers" "references"  
```
