# An ingested document

Returned by
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md).

## Fields

- `blocks`:

  Data frame of cleaned text blocks with provenance: `text`, `page`,
  `section`, `kind`, `block_id`. `page` is set only by the PDF
  extractor; `kind` is one of `"body"`, `"heading"`, `"code"`,
  `"table"`, `"footnote"`, `"ocr"`.

- `text`:

  All blocks joined with blank lines.

- `source`:

  Absolute path, the web address a document was fetched from, or
  `"<inline text>"` when the input was a string of text.

- `spec`:

  The
  [`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md)
  used.

- `stats`:

  `blocks`, `chars`, `chars_removed`, `tokens`, `pages`, `clean_steps`,
  `clean_log` (characters removed per cleaning step, useful when
  cleaning ate more than you expected) and `unread_pages` (pages that
  never became text, such as scanned pages read without OCR; an answer
  drawn from the document is marked partial when there are any).

- `warnings`:

  Character. What readgpt warned about while extracting and cleaning,
  named by the warning's class. Kept with the document, so a copy served
  from the ingestion cache still carries them.

## Methods

[`print()`](https://rdrr.io/r/base/print.html) shows the source, block
count and token total;
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md)
serialises the stats and every block with its provenance.

## See also

[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md)
which returns one,
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md),
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md),
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
for the next axis

[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md)

Other ingest functions:
[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md),
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md),
[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
[`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md)

## Examples

``` r
doc <- gr_ingest(readgpt_example())
#> Using cached ingestion for this document + settings.
doc$stats$tokens
#> [1] 573
vapply(doc$stats$clean_log, function(s) s$chars_removed, integer(1))
#>        page_numbers         hyphenation       control_chars           ligatures 
#>                  12                   0                   0                  -1 
#> collapse_whitespace 
#>                   0 
```
