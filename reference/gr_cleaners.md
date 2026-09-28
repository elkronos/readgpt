# List registered cleaners

`default_on` marks the steps the `"standard"` preset runs. Steps are
always applied `"early"` stage first, whatever order you list them in.

## Usage

``` r
gr_cleaners()
```

## Value

A data frame with `name`, `stage`, `scope`, `default_on` and
`description`.

## Details

Two steps can only judge by what the document shows them. `hyphenation`
cannot tell a word split at a line end from a compound broken at its own
hyphen ("placebo-" / "controlled"); it keeps the hyphen when the
document writes the compound whole somewhere, and otherwise joins the
two halves. `headers_footers` finds running heads by where they stand on
their pages (a PDF's, form feeds in text, or `.pages` given to
[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md));
without pages it takes a short line with a letter in it repeated at the
edge of its block, so a label that is a block of its own and recurs can
go with them.

## See also

[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md)

Other ingest functions:
[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md),
[`gr_document`](https://elkronos.github.io/readgpt/reference/gr_document.md),
[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
[`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md)

## Examples

``` r
gr_cleaners()
#>                   name stage    scope default_on
#> 1             captions early    block      FALSE
#> 2               emails early    block      FALSE
#> 3      headers_footers early document      FALSE
#> 4          hyphenation early    block       TRUE
#> 5         page_numbers early    block       TRUE
#> 6           references early document      FALSE
#> 7                 urls early    block      FALSE
#> 8           ascii_only  late    block      FALSE
#> 9  collapse_whitespace  late    block       TRUE
#> 10       control_chars  late    block       TRUE
#> 11           ligatures  late    block       TRUE
#> 12           lowercase  late    block      FALSE
#> 13      remove_numbers  late    block      FALSE
#> 14  remove_punctuation  late    block      FALSE
#>                                                                                                                                                                                                                                                        description
#> 1                                                       Drop figure/table caption lines: 'Table 2.', 'Figure 3:', 'Fig. 3 |', 'Table 2 Baseline ...' or the label alone, not 'Table 2 shows ...' (OFF by default: a caption can be the only statement of a result)
#> 2                                                                                                                                                                                                                                           Remove email addresses
#> 3                                                                                                                         Drop running heads and feet: short lines repeated at the top or bottom of many pages, or at the edge of blocks where pages are not known
#> 4  Rejoin words split across line breaks by PDF layout ('mito-\\nchondria', 'LIA-\\nBLE' in capitals); a compound the document writes with its hyphen elsewhere ('follow-up') keeps it; number ranges ('18-\\n65') and compounds ('Anglo-\\nSaxon') are left alone
#> 5                                                                                                                           Drop decorated page-number lines ('Page 4', '- 12 -', '[12]', and '12.' unless the line before runs on into it); bare numbers are kept
#> 6                                                                                                                                                                                  Drop a trailing bibliography, from a References/Bibliography heading to the end
#> 7                                                                                                                                                                                                          Remove URLs (OFF by default: URLs are often the answer)
#> 8                                                                                                                                                                                                     Transliterate to ASCII (drops accents and non-Latin scripts)
#> 9                                                                                                                                                    Collapse runs of spaces/tabs and 2+ consecutive blank lines, and trim block edges; preserves paragraph breaks
#> 10                                                                                                                                                                                             Strip control and zero-width characters that survive PDF extraction
#> 11                                                                                                                                                                             Expand typographic ligatures and straighten smart quotes; en and em dashes are kept
#> 12                                                                                                                                                                                                     Lowercase everything (loses proper-noun and acronym signal)
#> 13                                                                                                                                                        Replace every digit with a space. OFF by default: this makes figures, dates and percentages unanswerable
#> 14                                                                                                                                                                                   Replace punctuation with spaces. OFF by default: destroys sentence boundaries
# The steps the "standard" preset runs:
subset(gr_cleaners(), default_on)$name
#> [1] "hyphenation"         "page_numbers"        "collapse_whitespace"
#> [4] "control_chars"       "ligatures"          
```
