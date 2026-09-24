# Write a review from an extraction table

The last stage. Takes what
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
found and writes it up section by section, against an outline fixed in
advance, with every section citing the rows it rests on.

## Usage

``` r
gr_synthesise(
  extraction,
  protocol = NULL,
  outline = NULL,
  question = NULL,
  client = NULL,
  model = NULL,
  max_section_tokens = 1200L,
  temperature = NULL,
  include_unclear = FALSE,
  cite_style = c("auto", "marker", "author-year", "numeric"),
  bib = NULL,
  style = NULL,
  coherence = FALSE,
  references = TRUE,
  claims = NULL,
  gaps = NULL,
  trace = NULL
)
```

## Arguments

- extraction:

  A `gr_extraction` from
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
  or a data frame shaped like its `$table`.

- protocol:

  A
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md);
  its `outline` and `question` are used unless you give them directly.

- outline:

  The sections, as a named character vector: names are headings, values
  say what that section has to cover. As
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md).

- question:

  The review question, for framing.

- client:

  A `gr_client`.

- model, max_section_tokens, temperature:

  Overrides for the writing calls.

- include_unclear:

  Write from rows whose extraction was incomplete. Off by default: a row
  with nothing in it contributes nothing but its own absence, and the
  count of skipped rows is reported either way.

- cite_style:

  How citations appear in the finished prose. `"auto"` (the default)
  names the studies when the table can name all of them and uses markers
  when it cannot. `"author-year"` asks for names and warns if they
  cannot be produced; `"numeric"` gives `(1, 2)`; `"marker"` leaves
  `[study 1]` as written.

  The model always writes `[study N]`, whatever this is set to, and the
  rendering happens afterwards from the table. That is deliberate: a
  marker can be checked exactly against the rows that exist, whereas
  verifying an author-year string would mean matching a name the model
  wrote against a name in the table, and near-misses (Smith for Smyth,
  2019 for 2018) are both the errors that matter and the ones fuzzy
  matching forgives. A rendered citation is therefore a fact about the
  extraction rather than something the model asserted.
  `$sections$text_marked` and `$text_marked` keep the marker form so the
  check can be re-run on the published prose.

- bib:

  Which columns carry bibliographic identity, as a named list of
  `citation`, `authors`, `year`, `title`, `venue`, `doi`. Omitted, the
  conventional names are looked for, which is why
  `gr_protocols("bibliography")` works without configuration. The most
  reliable route is a `citation` field asked for during extraction:
  parsing an arbitrary author list is a heuristic, and where it cannot
  be done confidently the run falls back to markers rather than printing
  a name that may be wrong.

- style:

  A register instruction appended to the writing prompts, such as
  `"formal academic; hedge claims; past tense for findings"`. It governs
  how sections are written, never what they may say: the rules about
  citing every claim and inventing nothing hold whatever voice is asked
  for.

- coherence:

  Revision passes over the finished draft: `TRUE` for all three, `FALSE`
  for none, or any of `"structure"` (reorder and merge), `"cut"` (remove
  repetition) and `"register"` (polish sentences) by name. Each pass is
  forbidden from doing the others' job, and each is discarded (with
  `$draft` kept) if it changed the citations, arrived truncated, or
  strengthened a claim. Off by default: each is a call, and each is a
  chance for a model to touch finished prose.

- references:

  Append a `## References` section built from the studies the finished
  text actually cites. Alphabetical under `"author-year"`, numbered by
  study otherwise: the list is labelled by whatever the prose uses to
  point into it.

- claims:

  A
  [`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
  result. With it each section argues that section's claims and sees
  only the studies those claims rest on, rather than being handed every
  study and writing a paragraph per row. Needs an `outline` from
  [`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md),
  which carries the assignment.

- gaps:

  A
  [`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md)
  result, or lines of text. Given to the closing section
  [`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
  named, with an instruction to state those gaps and no others.

- trace:

  A
  [`gr_trace()`](https://elkronos.github.io/readgpt/reference/gr_trace.md)
  to fold this write-up's accounting into, as in
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md).
  It is a parent, not this stage's counter: `$trace` is still the
  write-up's own, so `gr_options(max_calls =)` bounds the write-up
  rather than being spent by the screening that came before it.

## Value

An object of class `gr_synthesis`:

- `text`:

  The whole write-up, as markdown, citations rendered and the reference
  list appended.

- `text_marked`:

  The same document with `[study N]` markers intact: what the citation
  check ran on.

- `draft`:

  The write-up before the coherence pass, for comparison.

- `references`:

  The reference list, or `NULL`.

- `cite_style`:

  The style actually used, which is not always the one asked for.

- `coherence`:

  One row per revision pass, or `NULL` if none ran: `pass` (which of
  `"structure"`, `"cut"`, `"register"`), `ran`, `kept`, `lost` and
  `added` (citations, if the revision changed them), and `reason` for
  anything discarded.

- `sections`:

  One row per section: `section`, `brief`, `text`, `n_cited`,
  `n_unknown`, `partial`.

- `citations`:

  Every citation, resolved to the row it points at, in long form:
  `section`, `study`, `document`, `document_id`.

- `studies`:

  The rows that were written from, with the `study` number each was
  cited by.

- `trace`:

  As
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md).

## Which rows are used

Rows that were never read (`status` `"failed"` or `"skipped"`) are left
out, and so are duplicates: a study counted twice is the error this
whole pipeline exists to avoid, and
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
has already marked them. The number left out is reported by
[`print()`](https://rdrr.io/r/base/print.html) and is in `$skipped`.

## What it costs

One call per section when the table fits one prompt, which is the usual
case: a hundred rows of a ten-field schema is a few thousand tokens. A
table too large for one prompt is written in batches and merged, so a
section costs batches + merges instead. Either way the cost is per
*section*, not per document: the expensive reading has already happened.

## See also

[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md),
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)

## Examples

``` r
fields <- gr_fields(design = "The study design",
                    n = gr_field("Participants", type = "integer"))
cl <- gr_mock_client(function(messages, params) {
  seen <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                collapse = " ")
  if (grepl("<studies>", seen, fixed = TRUE)) "One randomised trial of 120 people [study 1]."
  else '{"design":"randomised trial","n":120,
         "design__quote":"We ran a randomised trial.",
         "n__quote":"We enrolled 120 people."}'
})

f <- tempfile(fileext = ".txt")
writeLines("We ran a randomised trial. We enrolled 120 people.", f)
x <- gr_extract(f, fields, client = cl)
#> [1/1] file1dcd1821ed84.txt
#> Extracting 'file1dcd1821ed84.txt' with the 'txt' extractor.
#> Ingested 1 block(s), ~18 tokens (0 chars removed by cleaning).
#> Segmenting with 'structural' (cap 900 tokens, overlap 90).
#> Reading with 'extract' (all|N+conflicts|none) over 1 chunk(s).

s <- gr_synthesise(x, question = "Does it work?",
                   outline = c("Included studies" = "How many, of what design"),
                   client = cl)
#> [1/1] Included studies
s$sections[, c("section", "n_cited", "n_unknown")]
#>            section n_cited n_unknown
#> 1 Included studies       1         0
```
