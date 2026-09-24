# Extract a typed schema from many documents

The corpus counterpart to
[`gr_fields()`](https://elkronos.github.io/readgpt/reference/gr_fields.md):
applies one extraction schema to every document and returns a tidy
table, one row per document and one column per field, with a separate
long table saying where every value came from.

## Usage

``` r
gr_extract(
  sources,
  fields,
  goal = NULL,
  recipe = "research",
  client = NULL,
  store = NULL,
  resolve = c("first", "model"),
  require_quote = FALSE,
  on_error = c("continue", "stop"),
  max_total_usd = NULL,
  max_total_calls = NULL,
  keep_answers = FALSE,
  recursive = FALSE,
  trace = NULL,
  ...
)
```

## Arguments

- sources:

  As
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md):
  file paths, a directory, or raw text, and additionally a
  [`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)
  result. Passing the screening object rather than `screened$included`
  is what carries the search forward, so
  [`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md)
  can show it and the bibliographic fields the export supplied are
  joined to the table.

- fields:

  A
  [`gr_fields()`](https://elkronos.github.io/readgpt/reference/gr_fields.md)
  schema, or a
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md).
  A protocol carries its own schema, question and recipe, so passing one
  is the same as passing its three parts and it is the shorter way to
  say it.

- goal:

  One sentence of context for the extraction, such as "screening trials
  for a review of statins in primary prevention". It sharpens judgement
  calls about what counts as the primary outcome; it does not decide
  what is collected, because `fields` does that. Defaults to a neutral
  instruction.

- recipe:

  The ingest and segmentation to use. The reader is always `extract`,
  whatever the recipe says.

- client, store, on_error, max_total_usd, recursive:

  As
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md).
  `store` is worth setting for anything longer than a coffee break: an
  interrupted extraction resumes instead of restarting.

- resolve:

  What to do when two parts of one document give different values for
  the same field. `"first"` (default) takes the earlier one and records
  the disagreement in the `conflicts` column, costing nothing. `"model"`
  spends one extra call per disagreeing field to adjudicate.

- require_quote:

  Discard any value the model could not tie to a verbatim span in the
  chunk it cited. Off by default: an extracted value is never thrown
  away without being asked for, and `n_unverified` makes the same
  problem visible without destroying anything. Turn it on for a protocol
  that says no quote, no datum.

- max_total_calls, trace:

  As
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md).

- keep_answers:

  Keep the underlying
  [gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md)
  objects in `$answers`. They hold every chunk's source text, so for a
  large corpus this is what runs you out of memory; the tables do not
  need them.

- ...:

  Recipe overrides, as in
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md).
  `max_tokens =` is the one that matters here, because it decides how
  many chunks each document is cut into and therefore how many calls it
  costs.

## Value

An object of class `gr_extraction`:

- `table`:

  One row per document: `document`, one column per field in the schema
  and of that field's type, then `n_filled`, `n_unverified`,
  `conflicts`, `status`, `duplicate_of`, `error`. A document whose
  cleaned text repeats one already read is not read again (see
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)),
  so `subset(x$table, is.na(duplicate_of))` is the set of distinct
  documents.

- `evidence`:

  Long form, one row per supported cell: `document`, `document_id`,
  `field`, `chunk_id`, `page`, `section`, `quote`, `verified`, `match`.

- `fields`:

  The schema, so the table can be read without it.

- `summary`:

  The per-document run summary from
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md),
  including calls, tokens, cost and seconds.

- `answers`:

  The `gr_answer` objects, when `keep_answers = TRUE`.

- `trace`,`store`:

  As
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md).

## NA means two different things

A cell is `NA` either because the document was looked at and does not
report that field, or because the document was never successfully read.
The `status` column tells them apart, and it matters: "not reported" is
a finding you can publish, and "failed" is a job you have to redo.
Filtering an extraction table without checking `status` silently turns
the second into the first.

## What it costs

One call per chunk per document: every chunk is read, because a schema
field can be answered by a sentence anywhere in the paper and a
retrieval step that looked at the top eight chunks would miss it
silently. Reconciliation is free unless a document contradicts itself.
Cut the cost by segmenting more coarsely (`max_tokens =`), not by
looking at fewer chunks.

## Verifying it

Every filled cell is asked for the sentence it came from, and that
sentence is checked against the text of the chunk it was attributed to.
Nothing is ever discarded for failing: a paraphrase stays in `$evidence`
with `verified = FALSE` and the fraction of it that did match in
`match`.

`n_unverified` counts the cells in that row whose value could not be
tied to a verbatim span, either because no quote was given, or because
the quote is not in the chunk. That column is the one to look at before
believing a table: `n_unverified` of zero means every value in the row
can be pointed at in the document. A row where it is not zero is not
wrong, but it is unaudited, and the answer is marked `partial` to say
so. `require_quote = TRUE` turns the count into a policy and drops those
values instead.

See
[`gr_verify_evidence()`](https://elkronos.github.io/readgpt/reference/gr_verify_evidence.md)
for the same check on a single answer.

## What to cite

In descending order of how long it stays true: the `quote`, which is
verbatim text and findable in the document whatever anyone changes;
`page` and `section`, which come from the document itself and are
resolved per span, not per chunk, so a quote is credited to the page it
is on; `document_id`, the hash of the cleaned text, which is the same in
every run and on every machine.

`chunk_id` is none of those. It numbers the pieces the document was cut
into for *this* segmentation, so re-running with a different
`max_tokens` makes chunk 7 a different piece of text. It is a pointer
inside one run, useful for going back to `$answers`, and not a reference
to publish.

There is deliberately no attempt to guess a document's title or authors
from its filename or its first few lines. A heuristic like that fails
silently on preprints, reports and anything scanned, and a citation that
is wrong without saying so is worse than none. Ask for them the same way
as everything else: `gr_fields(title = , authors = , year = , doi = )`.
Each comes back with the sentence it was taken from and a check that the
sentence is really there.

## See also

[`gr_fields()`](https://elkronos.github.io/readgpt/reference/gr_fields.md),
[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md),
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md),
[`gr_verify_evidence()`](https://elkronos.github.io/readgpt/reference/gr_verify_evidence.md),
[`gr_trace_cost()`](https://elkronos.github.io/readgpt/reference/gr_trace_cost.md)

## Examples

``` r
fields <- gr_fields(
  design = "The study design",
  n      = gr_field("Number of participants", type = "integer")
)

# A mock that fills the form, so the example runs offline.
cl <- gr_mock_client(function(messages, params) {
  '{"design":"randomised controlled trial","n":120,
    "design__quote":"We ran a randomised controlled trial.",
    "n__quote":"We enrolled 120 participants."}'
})

f <- tempfile(fileext = ".txt")
writeLines("We ran a randomised controlled trial. We enrolled 120 participants.", f)

x <- gr_extract(f, fields, client = cl)
#> [1/1] file1dcd487ec805.txt
#> Extracting 'file1dcd487ec805.txt' with the 'txt' extractor.
#> Ingested 1 block(s), ~22 tokens (0 chars removed by cleaning).
#> Segmenting with 'structural' (cap 900 tokens, overlap 90).
#> Reading with 'extract' (all|N+conflicts|none) over 1 chunk(s).
x$table[, c("document", "design", "n", "n_unverified", "status")]
#>               document                      design   n n_unverified status
#> 1 file1dcd487ec805.txt randomised controlled trial 120            0     ok
x$evidence[, c("field", "quote", "verified")]
#>    field                                 quote verified
#> 1 design We ran a randomised controlled trial.     TRUE
#> 2      n         We enrolled 120 participants.     TRUE
```
