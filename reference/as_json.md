# Serialise an object to JSON

A generic so traces, answers, chunk sets and documents all serialise
consistently and safely (`auto_unbox` plus `null = "null"`, so a missing
field appears as `null` rather than vanishing). Assigning `NULL` into an
R list *deletes the key*, which is why the old code's `final_answer`
field silently disappeared from the JSON whenever a call failed.

## Usage

``` r
as_json(x, pretty = TRUE, ...)
```

## Arguments

- x:

  Object to serialise. Methods exist for
  [gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md),
  `gr_trace`,
  [gr_chunks](https://elkronos.github.io/readgpt/reference/gr_chunks.md)
  and
  [gr_document](https://elkronos.github.io/readgpt/reference/gr_document.md);
  anything else (a review stage such as a screening or an extraction, a
  corpus, a comparison, a list of answers) falls back to a plain
  `jsonlite` conversion, in which a trace, answer, chunk set or document
  found at any depth is written as its own method writes it, and any
  other environment, a function, or a client (which holds the API key),
  none of which is data, is written as `null`.

- pretty:

  Whether to indent.

- ...:

  Passed to
  [`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html).

## Value

A `json`-classed character string. `NULL` fields are written as `null`
rather than dropped, and each number is written with as many significant
digits as it takes to read back as the same number: 15 for most, 16 or
17 for the few that need them
([`jsonlite::toJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
on its own rounds to four decimal places, and its `digits = NA` keeps 15
significant digits). Pass `digits` to round them. Otherwise a vector of
length one is written as a scalar, except in the fields of a trace that
list things (a comparison's `recipes`, the scores a ranking kept, the
cleaning steps an ingest ran), which are arrays at every length.

## See also

[`gr_trace_summary()`](https://elkronos.github.io/readgpt/reference/gr_trace_summary.md),
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "45.2 million dollars")
ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl)
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).

# The answer plus every prompt and response from the same single run.
txt <- as_json(ans)
names(jsonlite::fromJSON(txt))
#> [1] "answer"      "reader"      "question"    "partial"     "chunks_used"
#> [6] "notes"       "evidence"    "warnings"    "trace"      
```
