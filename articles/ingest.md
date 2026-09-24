# Getting text out of files

Before a model can read a document, the text has to come out of the
file. That step, *ingestion*, is easy to overlook and hard to undo. Text
lost here, or damaged by too much clean-up, is text no model will see,
however good the rest of the pipeline is. This guide covers what readgpt
can read, what it keeps, what it removes, and how to check a folder
before spending anything on it.

New to readgpt?
[`vignette("readgpt")`](https://elkronos.github.io/readgpt/articles/readgpt.md)
introduces the package and the ideas used here.

``` r

library(readgpt)
old <- gr_options(verbose = FALSE)
```

## What ingestion produces

[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md)
reads a file and returns a *document*: the text split into *blocks*,
which are the paragraphs, headings and list items as the file has them.
readgpt includes a short example, an annual report written in Markdown:

``` r

doc <- gr_ingest(readgpt_example())
doc
#> <gr_document> /home/runner/work/_temp/Library/readgpt/extdata/annual_report.md
#>   17 blocks, ~573 tokens, 1847 chars (11 removed by cleaning)
#>   cleaners: page_numbers, hyphenation, control_chars, ligatures, collapse_whitespace
#>   first block: # Northwind Instruments -- Annual Report 2024
```

Each block records where it came from: the page, for formats that have
pages, and the section heading it sits under:

``` r

doc$blocks[1:6, c("block_id", "page", "section", "kind")]
#>   block_id page                                    section    kind
#> 1        1   NA Northwind Instruments — Annual Report 2024 heading
#> 2        2   NA                                    Summary heading
#> 3        3   NA                                    Summary    body
#> 4        4   NA                                    Summary    body
#> 5        5   NA                                 Operations heading
#> 6        6   NA                                 Operations    body
```

This *provenance* is what lets an answer point back to where it came
from: `ans$evidence` reports the page and section of every chunk an
answer rests on. `page` is `NA` here because Markdown has no pages; a
PDF fills it in.

`doc$text` is all the blocks joined, and `doc$stats` summarises the
document:

``` r

unlist(doc$stats[c("blocks", "chars", "tokens", "chars_removed")])
#>        blocks         chars        tokens chars_removed 
#>            17          1847           573            11
```

You rarely need to call
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md)
yourself, since
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
does it for you. Calling it directly shows you what the model will get.

## What readgpt can read

Each file type is handled by an *extractor*, chosen by the file’s
extension. `available` says whether the packages it needs are installed:

``` r

gr_extractors()[, c("name", "extensions", "available")]
#>        name                          extensions available
#> txt     txt            txt, text, log, csv, tsv      TRUE
#> md       md              md, markdown, rmd, qmd      TRUE
#> html   html                    html, htm, xhtml      TRUE
#> pdf     pdf                                 pdf      TRUE
#> docx   docx                          docx, dotx      TRUE
#> image image png, jpg, jpeg, tif, tiff, bmp, gif      TRUE
```

| extractor | what it keeps | needs |
|----|----|----|
| `txt` | paragraphs, split at blank lines | nothing |
| `md` | paragraphs, plus headings as sections and code blocks and tables marked as such | nothing |
| `html` | text inside `<h1>` to `<h4>`, `<p>`, `<li>`, `<td>`, `<pre>` and `<blockquote>`; text anywhere else (a bare `<div>`, `<h5>`, `<th>`) is dropped, as are scripts, styles, navigation bars and footers | `xml2` |
| `docx` | paragraphs, with headings as sections in any interface language; each table row as one block, its cells joined by a vertical bar; footnotes and endnotes after the paragraph that cites them; images inside the file are read by OCR when `tesseract` is installed | `xml2` |
| `pdf` | the text of each page in reading order, with its page number and headings as sections; pages with no text are read by OCR | `pdftools` (and `tesseract` and `magick` for OCR) |
| `image` | the text in a picture, by OCR | `tesseract` |

A file whose extension no extractor claims, such as an old `.doc` or a
spreadsheet, is not read.
[`gr_inventory()`](https://elkronos.github.io/readgpt/reference/gr_inventory.md),
below, finds those before a run rather than during one.

You can also pass text directly. A string that is not the path of an
existing file is read as the document itself:

``` r

memo <- gr_ingest("Revenue rose to 45.2 million dollars.\n\nHeadcount grew to 1,204.")
memo$blocks[, c("block_id", "text")]
#>   block_id                                  text
#> 1        1 Revenue rose to 45.2 million dollars.
#> 2        2              Headcount grew to 1,204.
```

The exception is a string that ends in an extension some extractor
claims but names no file. That is an error:

``` r

gr_ingest("anual-report.pdf")
#> Error in `gr_ingest()`:
#> ! File not found: 'anual-report.pdf'. If you meant to pass document text rather than a path, it must not end in something that looks like a file extension.
```

Any other string is read as text, including a path whose extension is
mistyped or is one no extractor claims. A string that looks like a path
(it has a `/` and an extension) also raises a warning, since the answer
that follows would be about a file name:

``` r

typo <- gr_ingest("reports/2024/annual-report.pfd")
#> Warning in gr_ingest("reports/2024/annual-report.pfd"):
#> 'reports/2024/annual-report.pfd' is not an existing file, and '.pfd' is not an
#> extension readgpt reads, so the string itself is being read as the document. If
#> it is a path, check it; readgpt reads .bmp, .csv, .docx, .dotx, .gif, .htm,
#> .html, .jpeg, .jpg, .log, .markdown, .md, .pdf, .png, .qmd, .rmd, .text, .tif,
#> .tiff, .tsv, .txt, .xhtml.
typo$text
#> [1] "reports/2024/annual-report.pfd"
typo$source
#> [1] "<inline text>"
```

`source` is how to tell the two apart: it holds the path when a file was
read, and `<inline text>` when the string itself was.

A web address is downloaded and read with the extractor for what the
server sent back: a specific type it declares, else the extension in the
address, else the first bytes of the file, which settle a PDF, a Word
file or an HTML page however they were labelled. A download no extractor
reads is refused rather than read as text. `source` holds the address,
and a download that fails is an error that says why:

``` r

doc <- gr_ingest("https://example.org/reports/annual-report-2024.pdf")
doc$source
#> [1] "https://example.org/reports/annual-report-2024.pdf"
```

Within a session an address is downloaded once; the next read of it
comes from the cache, as a file does.

## Sections and headings

For Markdown, HTML, Word and PDF files, each block records the heading
it falls under (the next section says how a PDF’s headings are found).
The example report has these sections:

``` r

unique(doc$blocks$section)
#> [1] "Northwind Instruments — Annual Report 2024"
#> [2] "Summary"                                   
#> [3] "Operations"                                
#> [4] "Risk factors"                              
#> [5] "Outlook"                                   
#> [6] "References"
```

Sections matter later. The `structural` way of cutting a document into
chunks never lets a chunk straddle two sections, and the `preview`
reading strategy plans its reading section by section. A plain text file
has no headings, so its blocks have no section.

## PDFs

A PDF is read page by page, and every block carries its page number.
That is what lets a quotation be traced to “page 12” rather than to
“somewhere in the file”.

``` r

pdf_file <- tempfile(fileext = ".pdf")
grDevices::pdf(pdf_file, width = 8.5, height = 11)
for (page in list(c("Summary", "Revenue was 45.2 million dollars in fiscal 2024."),
                  c("Outlook", "The board expects revenue of 50 million dollars in 2025."))) {
  graphics::plot.new()
  graphics::text(0, c(0.95, 0.9), page, adj = 0)
}
invisible(grDevices::dev.off())

report <- gr_ingest(pdf_file)
report$blocks[, c("page", "section", "kind", "text")]
#>   page section    kind                                                     text
#> 1    1 Summary heading                                                  Summary
#> 2    1 Summary    body         Revenue was 45.2 million dollars in fiscal 2024.
#> 3    2 Summary    body                                                  Outlook
#> 4    2 Summary    body The board expects revenue of 50 million dollars in 2025.
```

A PDF’s text comes out as the page looks, which is not always the order
a person reads in, so readgpt puts it back in reading order:

- A page set in two columns is read one column after the other. Without
  this, each line would join the start of a line from each column.
- Running heads and feet (short lines repeated at the top or bottom of
  many pages, such as a journal name or “Page 3 of 12”) are removed.
- Headings are found, so blocks carry sections as they do for Markdown
  and Word. They come from the PDF’s bookmarks when it has them.
  Otherwise a line that is only a standard section name (“Introduction”,
  “2. Methods”, “Results”) counts. That is how “Summary” was found
  above; “Outlook” is not a standard name, so it stayed text.

`gr_ingest_spec(layout = "raw")` turns all three off and keeps each page
as it is laid out. Footnotes, tables and pages in three or more columns
can still come out in an order that reads oddly, so check a few pages of
`report$text` before relying on a new kind of PDF.

## Scanned pages and OCR

A scanned PDF is a picture of text, with no text inside it to extract.
Reading it needs *optical character recognition* (OCR), which turns the
picture back into text. OCR is slow (seconds per page) and never
perfect, so readgpt uses it only where it is needed:

``` r

# The default: OCR any page with fewer than 40 characters of text.
gr_ingest("scan.pdf", gr_ingest_spec(ocr = "auto"))

# A page with a little text but mostly scanned content: raise the threshold.
gr_ingest("scan.pdf", gr_ingest_spec(ocr = "auto", ocr_min_chars = 200))

# Everything, or nothing.
gr_ingest("scan.pdf", gr_ingest_spec(ocr = "always"))
gr_ingest("born-digital.pdf", gr_ingest_spec(ocr = "never"))

# Another language, at a higher resolution.
gr_ingest("scan.pdf", gr_ingest_spec(ocr_lang = "deu", ocr_dpi = 400))
```

OCR on PDFs needs both `tesseract` and `magick`. Without them a scanned
page comes back nearly empty and readgpt warns (`gr_ocr_unavailable`).
The document lists those pages in `doc$stats$unread_pages`, and an
answer drawn from it is marked partial. A document with too little text
left is refused outright, as the next section shows. The `"scanned"`
recipe is set up for this kind of document.

## Cleaning

Extracted text is rarely clean. PDFs break words across lines with
hyphens, repeat the page number on every page, and use typographic
ligatures (“ﬁ” for “fi”). readgpt tidies this with *cleaners*. Each does
one job and can be turned on or off:

``` r

gr_cleaners()[, c("name", "stage", "default_on")]
#>                   name stage default_on
#> 1             captions early      FALSE
#> 2               emails early      FALSE
#> 3      headers_footers early      FALSE
#> 4          hyphenation early       TRUE
#> 5         page_numbers early       TRUE
#> 6           references early      FALSE
#> 7                 urls early      FALSE
#> 8           ascii_only  late      FALSE
#> 9  collapse_whitespace  late       TRUE
#> 10       control_chars  late       TRUE
#> 11           ligatures  late       TRUE
#> 12           lowercase  late      FALSE
#> 13      remove_numbers  late      FALSE
#> 14  remove_punctuation  late      FALSE
```

The five marked `default_on` run unless you say otherwise. Note what is
**off** by default, and why:

- `remove_numbers` replaces every digit with a space, which makes any
  question about a figure, date or percentage unanswerable.
- `captions` removes lines that look like figure and table captions,
  which destroys documents made mostly of tables.
- `urls` and `emails` remove links and addresses, which are sometimes
  the answer.
- `references` cuts everything from a line reading just “References”,
  “Bibliography” or “Works cited” to the end of the document, when that
  line is in the last 40% of the text. That loses the reference list if
  your question is about the sources. A Markdown heading such as
  `## References` is not such a line, so it does not trigger the cut.

Cleaning cannot be undone later: no reader can recover what it removes.
Know what each choice costs before you make it.

### Presets

Instead of naming cleaners one by one, use a *preset*:

| preset | what it runs |
|----|----|
| `"none"` | nothing |
| `"minimal"` | control characters and whitespace only |
| `"standard"` | the default: page numbers, broken hyphens, control characters, ligatures, whitespace |
| `"academic"` | standard, plus `captions` and `references` |
| `"scan"` | standard, plus repeated page headers and footers |
| `"legacy"` | what the first version of this package did, number stripping included; for comparison only |

Here is how much text each leaves of the example report:

``` r

presets <- c("none", "minimal", "standard", "academic", "scan", "legacy")
vapply(presets, function(p) gr_ingest(readgpt_example(), gr_ingest_spec(clean = p))$stats$chars,
       integer(1))
#>     none  minimal standard academic     scan   legacy 
#>     1858     1858     1847     1766     1847     1659
```

Or name the cleaners yourself:

``` r

tidy <- gr_ingest(readgpt_example(),
                  gr_ingest_spec(clean = c("page_numbers", "hyphenation", "collapse_whitespace")))
tidy$stats$chars
#> [1] 1846
```

### Seeing what cleaning removed

Every document records what each cleaner took out, in characters:

``` r

vapply(doc$stats$clean_log, function(step) step$chars_removed, integer(1))
#>        page_numbers         hyphenation       control_chars           ligatures 
#>                  12                   0                   0                  -1 
#> collapse_whitespace 
#>                   0
```

A negative number means a cleaner added characters, for example by
expanding “ﬁ” to “fi”. If a cleaner removed far more than you expected,
turn it off and compare.

[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md)
runs cleaners on any text, so you can try a configuration on a few of
your own lines first. Cleaners always run in two stages, `early` before
`late`, whatever order you list them in; otherwise number removal would
strip the “1” from “Page 1” before the page-number cleaner could
recognise the line:

``` r

cleaned <- gr_clean(c("Page 1", "Revenue rose to 45.2 million in 2024."),
                    steps = c("remove_numbers", "page_numbers"))
as.character(cleaned)
#> [1] ""                                  "Revenue rose to  .  million in  ."
```

The second line shows why `remove_numbers` is off by default.

## When too little text is left

A document with almost no text after cleaning (a scan read without OCR,
an empty file, a PDF of images) is refused rather than sent to the
model. A model shown nothing would answer “not in the document” for a
document that may well contain the answer:

``` r

gr_ingest("Tiny.")
#> Error in `gr_ingest()`:
#> ! Only 5 characters survived ingestion of '<inline text>' (minimum 20). The file may be empty, image-only with OCR disabled, or your cleaning steps may be too aggressive: 0 characters were removed by cleaners: page_numbers(-0) hyphenation(-0) control_chars(-0) ligatures(-0) collapse_whitespace(-0).
```

The limit is `min_chars` (20 characters by default) in
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md).

## Reading the same file twice

Ingestion is cached for the rest of the R session. Asking a second
question of the same file, or trying a different way of cutting it into
chunks, reuses the extracted text instead of reading the file again. The
cache notices when the file changes (its size or modification time) and
when any ingestion setting changes. Turn it off with
`gr_options(cache_documents = FALSE)`.

## Before reading a whole folder

A folder of documents hides problems that a single file makes obvious: a
few old `.doc` files nobody can read, scanned PDFs without text, an
empty file, or a subfolder you forgot about.
[`gr_inventory()`](https://elkronos.github.io/readgpt/reference/gr_inventory.md)
finds all of these in seconds, without calling a model and without an
API key:

``` r

inbox <- file.path(tempdir(), "inbox")
dir.create(inbox, showWarnings = FALSE)
invisible(file.copy(readgpt_example(), file.path(inbox, "annual_report.md"),
                    overwrite = TRUE))
writeLines("Headcount grew to 1,204 employees across nine sites.",
           file.path(inbox, "notes.txt"))
writeLines("(a Word 97 document)", file.path(inbox, "minutes.doc"))
invisible(file.create(file.path(inbox, "empty.txt")))

inv <- gr_inventory(inbox)
inv
#> <gr_inventory> /tmp/RtmpKqZsFa/inbox
#>   4 file(s), 1.9 KB; 2 readable
#>   2 ready, 1 no_extractor, 1 empty
#>   tokens: 536   cost floor: $0.01 (gpt-5.6-terra, one call per document)
#>   ! 1 file(s) would be skipped: .doc (1). gr_register_extractor() adds a format.
```

It lists every file, including the ones that will not be read, because
“one file will be skipped” is exactly what you need to know before the
run:

``` r

inv$files[, c("file", "extractor", "status", "tokens")]
#>               file extractor       status tokens
#> 1 annual_report.md        md        ready    518
#> 2        empty.txt       txt        empty     NA
#> 3      minutes.doc      <NA> no_extractor     NA
#> 4        notes.txt       txt        ready     18
```

The statuses:

| status | meaning |
|----|----|
| `ready` | an extractor can read it, and there is text |
| `needs_ocr` | a PDF whose pages have no text; it needs OCR |
| `needs_package` | an extractor can read it, but its package is not installed |
| `no_extractor` | no extractor reads this kind of file |
| `empty` | nothing to read |
| `unreadable` | it could not be opened; `note` says why |

`inv$totals` adds up the tokens and prices one request per readable file
with your model: its tokens in, and an assumed 500-token answer out.
Treat `cost_floor_usd` as a rough guide rather than a bound. Reading
strategies that make several requests per document cost more, and short
answers cost less. Files whose size is unknown until they are read by
OCR are not in the total; `tokens_unknown` counts them.

``` r

unlist(inv$totals[c("readable", "tokens", "cost_floor_usd")])
#>       readable         tokens cost_floor_usd 
#>       2.000000     536.000000       0.013072
```

## Adding your own

A format readgpt does not know, or a clean-up step your documents need,
can be added and then used exactly like a built-in one. A cleaner that
drops lines marked “CONFIDENTIAL”:

``` r

gr_register_cleaner("drop_confidential", stage = "early",
  fn = function(x, opts) gsub("(?mi)^\\s*CONFIDENTIAL.*$", "", x, perl = TRUE),
  description = "Remove lines marked CONFIDENTIAL")

gr_ingest("CONFIDENTIAL - internal only\n\nRevenue rose to 45.2 million dollars.",
          gr_ingest_spec(clean = c("drop_confidential", "collapse_whitespace")))$text
#> [1] "Revenue rose to 45.2 million dollars."
```

[`?gr_register_extractor`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md)
shows how to add a file format.

## Next

Once the text is out, the next decision is how to cut it into chunks and
how the model should read them.
[`vignette("readers")`](https://elkronos.github.io/readgpt/articles/readers.md)
covers the reading strategies;
[`vignette("tour")`](https://elkronos.github.io/readgpt/articles/tour.md)
shows the chunking methods.
