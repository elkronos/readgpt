# What is in a folder, before you read any of it

A deterministic survey of a directory or a set of paths: what is there,
which files a registered extractor can read, which PDFs are scans that
need OCR, how big the whole thing is, and roughly what one pass would
cost. It makes no model calls and needs no API key.

## Usage

``` r
gr_inventory(
  sources,
  recursive = TRUE,
  model = NULL,
  ocr_min_chars = 40L,
  max_pdf_pages = 3L
)
```

## Arguments

- sources:

  A directory, or a character vector of paths. A directory is walked;
  anything else is taken as given.

- recursive:

  Descend into subdirectories. `TRUE` here, unlike
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md),
  because the point of the function is to show you everything.

- model:

  Model id used for the cost floor. Defaults to `gr_options("model")`.

- ocr_min_chars:

  A PDF page with fewer than this many characters of extractable text is
  counted as needing OCR. Read exactly as
  [`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md)
  reads it, default included, so what this predicts is what ingestion
  will do.

- max_pdf_pages:

  Pages sampled per PDF for the text-layer probe. The whole point is to
  be fast on a big folder; a scan is obvious from a few pages. `Inf`
  reads every page.

## Value

An object of class `gr_inventory`:

- `files`:

  One row per file found, readable or not: `file` (the path relative to
  `sources`, so the folder it came from survives), `folder`, `ext`,
  `bytes`, `extractor` (`NA` when none claims it), `status`, `pages`,
  `ocr_pages`, `tokens` and `note`.

- `by_status`:

  Counts and sizes per status.

- `totals`:

  Files, readable files, bytes, known `tokens`, `tokens_unknown`
  (readable files whose size cannot be counted yet), and
  `cost_floor_usd`.

- `root`:

  The directory surveyed, or `NA`.

`status` is one of `"ready"` (an extractor claims it and there is text),
`"needs_ocr"` (a PDF whose pages have no text layer), `"needs_package"`
(an extractor claims it, but that extractor's package is not installed,
so reading it would abort), `"no_extractor"`, `"empty"` (zero bytes, or
nothing that reads as text; `note` says which), or `"unreadable"` (it
exists but could not be opened, or probing it raised an error; `note`
carries the reason).

No file can stop the survey. A folder is surveyed because nobody knows
what is in it yet, so a corrupt PDF, a binary file wearing a `.txt`
extension, a broken symlink or anything else unforeseen becomes a row
saying so, not an error. That is the same contract
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
gives a corpus run.

## Details

Run it before
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
or
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
on anything you have not read before. The three things it exists to
catch are the three that turn a corpus run into a confident wrong
answer: files no extractor claims (which would otherwise be dropped
without appearing anywhere), PDFs with no text layer (which extract to
nothing and then answer `NOT_IN_DOCUMENT`, indistinguishable from a
document that genuinely does not say), and a folder whose contents are
one level further down than you scanned.

## What it does not do

It does not decide anything for you. There is no automatic routing of
files to recipes here, and that is deliberate: a router that quietly
reads one document with `retrieve` and another with `stuff` gives you a
plausible answer built on part of a file with nothing saying so, and it
makes
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
meaningless because the corpus no longer had *a* configuration. Group
the rows yourself (`split(inv$files, inv$files$folder)` is usually all
it takes) and pass each group to the recipe you chose.

## Tokens, and what is left unknown

`tokens` is counted exactly where counting is cheap: plain text,
markdown, HTML, CSV, and PDFs from the pages actually probed, scaled by
page count. For formats needing an optional package that is not
installed, and for scans whose text does not exist until OCR runs, it is
`NA`, not a guess. Those files are counted in `totals$tokens_unknown`
rather than folded into the sum as zeroes, so the total is always a
floor and always says how far from complete it is.

`cost_floor_usd` is a floor: what a single call per document over that
much input would cost. Every per-chunk reader costs more, most of them
by a factor of the chunk count. It is there to catch the order of
magnitude (to tell four dollars from four hundred), not to be a quote.

## See also

[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md),
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md)
for the OCR settings this predicts

Other corpus functions:
[`gr_calibrate()`](https://elkronos.github.io/readgpt/reference/gr_calibrate.md),
[`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md),
[`gr_reference()`](https://elkronos.github.io/readgpt/reference/gr_reference.md),
[`gr_search()`](https://elkronos.github.io/readgpt/reference/gr_search.md)

## Examples

``` r
d <- tempfile(); dir.create(file.path(d, "2019"), recursive = TRUE)
writeLines("The 2019 cohort had 482 participants.", file.path(d, "2019", "report.txt"))
writeLines("notes", file.path(d, "notes.doc"))   # no extractor claims .doc

inv <- gr_inventory(d)
inv
#> <gr_inventory> /tmp/RtmpRyE9R0/file1dcd74c67009
#>   2 file(s), 44.0 B; 1 readable
#>   1 ready, 1 no_extractor
#>   tokens: 16   cost floor: $0.01 (gpt-5.6-terra, one call per document)
#>   ! 1 file(s) would be skipped: .doc (1). gr_register_extractor() adds a format.
inv$files[, c("file", "folder", "ext", "status", "tokens")]
#>              file folder ext       status tokens
#> 1 2019/report.txt   2019 txt        ready     16
#> 2       notes.doc      . doc no_extractor     NA
```
