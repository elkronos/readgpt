# Run the cleaning pipeline over a character vector

Exposed separately from
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md)
so you can see exactly what a cleaning configuration does to your own
text before committing a run to it. Cleaning is destructive and
irreversible from the model's point of view: whatever is removed here,
no reading strategy can recover.

## Usage

``` r
gr_clean(text, steps = NULL, opts = list())
```

## Arguments

- text:

  Character vector of block texts.

- steps:

  Character vector of cleaner **names** (see
  [`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md)),
  or `NULL` for the `default_on` set. Unlike
  [`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md)'s
  `clean` argument this does **not** accept preset names. Steps are
  reordered so every `"early"` cleaner runs before every `"late"` one,
  regardless of the order given. This is what stops digit removal from
  running before the page and figure filters that need digits to match.

- opts:

  Named list passed to every step.

## Value

The cleaned character vector, with a `"gr_clean_log"` attribute
recording characters removed per step.

## See also

[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md)
for the step names,
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md)
to use a configuration in a real run

Other ingest functions:
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md),
[`gr_document`](https://elkronos.github.io/readgpt/reference/gr_document.md),
[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
[`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
[`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md)

## Examples

``` r
# Listed late-then-early, but page_numbers still runs FIRST; otherwise
# remove_numbers eats the "1" and "Page" survives as body text.
out <- gr_clean(c("Page 1", "Revenue rose to 45.2 million in 2024."),
                steps = c("remove_numbers", "page_numbers"))
out[]
#> [1] ""                                  "Revenue rose to  .  million in  ."
#> attr(,"gr_clean_log")
#> attr(,"gr_clean_log")$page_numbers
#> attr(,"gr_clean_log")$page_numbers$step
#> [1] "page_numbers"
#> 
#> attr(,"gr_clean_log")$page_numbers$stage
#> [1] "early"
#> 
#> attr(,"gr_clean_log")$page_numbers$scope
#> [1] "block"
#> 
#> attr(,"gr_clean_log")$page_numbers$chars_removed
#> [1] 6
#> 
#> 
#> attr(,"gr_clean_log")$remove_numbers
#> attr(,"gr_clean_log")$remove_numbers$step
#> [1] "remove_numbers"
#> 
#> attr(,"gr_clean_log")$remove_numbers$stage
#> [1] "late"
#> 
#> attr(,"gr_clean_log")$remove_numbers$scope
#> [1] "block"
#> 
#> attr(,"gr_clean_log")$remove_numbers$chars_removed
#> [1] 4
#> 
#> 

# Every step reports what it took out.
vapply(attr(out, "gr_clean_log"), function(s) s$chars_removed, integer(1))
#>   page_numbers remove_numbers 
#>              6              4 
```
