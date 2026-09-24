# Ask one question of many documents

The counterpart to
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md):
that runs several recipes over one document, this runs one recipe over
many. Returns one tidy row per document, so the result goes straight
into a data frame you can write out, join, or code against.

## Usage

``` r
gr_read_many(
  sources,
  question,
  recipe = "thorough",
  client = NULL,
  store = NULL,
  on_error = c("continue", "stop"),
  max_total_usd = NULL,
  max_total_calls = NULL,
  keep_answers = TRUE,
  recursive = FALSE,
  trace = NULL,
  ...
)
```

## Arguments

- sources:

  A character vector of file paths, or a single directory, or raw text,
  or a
  [`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md)
  or a
  [`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)
  result, either of which also carries the search forward to the audit.
  A directory, and a vector of paths that all exist, are both filtered
  to the extensions some registered extractor claims, so which files are
  picked up follows
  [`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
  including any you registered yourself. Raw text, a mixed vector and a
  [`list()`](https://rdrr.io/r/base/list.html) of sources are passed
  through untouched, and a web address is downloaded as
  [`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md)
  downloads one.

- question:

  The question, asked of every document.

- recipe:

  One recipe, applied to every document.

- client:

  A `gr_client`. Wrap it in
  [`gr_cache_client()`](https://elkronos.github.io/readgpt/reference/gr_cache_client.md)
  for a long run: with a durable cache directory, a restart pays for
  nothing it has already answered. A closure-backed client
  ([`gr_backend_client()`](https://elkronos.github.io/readgpt/reference/gr_backend_client.md)
  or
  [`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md))
  reuses a cache or a `store` across sessions only if it was given a
  stable `id`; see
  [`gr_backend_client()`](https://elkronos.github.io/readgpt/reference/gr_backend_client.md)
  for why.

- store:

  Optional directory. Each document's result is written there as it
  completes and restored on a later run instead of being read again.
  This is what makes a four-hour run survive being interrupted.

- on_error:

  `"continue"` (default) records the failure and moves on; `"stop"`
  aborts. One unreadable file in two hundred should not cost you the
  other hundred and ninety-nine.

- max_total_usd:

  Stop once the run has spent this much, marking the remaining documents
  `"skipped"`. This is a *corpus* ceiling and is separate from
  `gr_options(max_cost_usd =)`, which is a limit per document. It needs
  a model with a registered price: against one without, cost is
  *unknown* rather than zero, the ceiling cannot be enforced, and you
  get a `gr_corpus_cost_unknown` warning instead of a silent free pass.

- max_total_calls:

  Stop *before* a document once the run has made this many model calls,
  marking the rest `"skipped"`. The counterpart to `max_total_usd` for
  runs whose model has no registered price, and the only ceiling that
  bounds the run rather than each document: `gr_options(max_calls =)` is
  per document, so a corpus can make `length(sources)` times that many.
  Checked before each document, because a call ceiling noticed after the
  calls is not a ceiling. This means the run can overshoot by at most
  one document's worth, exactly as `max_total_usd` does.

- keep_answers:

  Keep every
  [gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md)
  in the result. Set `FALSE` for a large corpus, where holding every
  trace and evidence table is the thing that runs you out of memory.

- recursive:

  Descend into subdirectories when `sources` is a directory.

- trace:

  A
  [`gr_trace()`](https://elkronos.github.io/readgpt/reference/gr_trace.md)
  to fold this run's accounting into, so several stages of one review
  add up to one figure. It is a *parent*: this run still gets its own
  trace, which is what `$trace` returns and what
  `gr_options(max_calls =)` is measured against. Running a stage
  directly on a shared trace would charge the previous stage's calls
  against this one's ceiling. Omit it and there is no parent.

- ...:

  Overrides applied to the recipe, as in
  [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md).

## Value

An object of class `gr_corpus`: `summary` (one row per document),
`answers` (named list, empty when `keep_answers = FALSE`), `sources`
(the sources as read, aligned row for row with `summary`;
`summary$document` is a display label and cannot be turned back into a
path), `records` (the
[`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md)
the corpus came from, or `NULL`), `trace` (every call made *this run*)
and `store`.

## The summary

`document`, `document_id`, `answer`, `not_found`, `partial`, `reader`,
`chunks`, `chunks_used`, `calls`, `cached`, `tokens_in`, `tokens_out`,
`cost_usd`, `seconds`, `status`, `duplicate_of`, `error`, `warnings`.

`warnings` holds what readgpt warned about while reading that document,
joined with `" | "`, or `NA` when it raised nothing. The warnings still
print as they happen; this column is what lets you tie them to a
document afterwards.

`document` is a filename and `document_id` is the hash of the cleaned
text (and of the pages that never became text, when there are any). Cite
with the second: a filename changes when the file is renamed, collides
between folders, and does not exist at all for a document passed as
text, while the id is the same string for the same document in every run
and on every machine with the same OCR setup. Two copies of one paper
share an id, which is the same fact as the duplicate detection below.

`status` is `"ok"`, `"failed"`, `"skipped"` (the corpus ceiling was
reached first), `"restored"` (read from `store`, not re-read now) or
`"duplicate"` (see below). A document that `max_calls` or `max_cost_usd`
stopped before it was read in full is `"failed"` too, with the limit in
`error` and its partial answer in `answers`; it is not written to
`store`, so a resumed run with a higher limit reads it again. A restored
row keeps the numbers from when that document was first read, so its
`cost_usd` is what it cost then, not what this run spent. That is why
the run's own spend comes from `gr_trace_cost(x$trace)` and not from
summing the column.

## Documents that are the same document

The same paper reaches you from three databases under three filenames.
Each source is extracted and cleaned, and a document whose cleaned text
(and set of unread pages) is identical to one already read this run is
**not read again**: its row is filled in from the first copy, except for
`warnings`, which are its own; `status` is `"duplicate"` and
`duplicate_of` names the row it repeats. Nothing is dropped (every
source you passed still has a row), so
`subset(x$summary, is.na(duplicate_of))` is the deduplicated set and
`sum(!is.na(x$summary$duplicate_of))` is the number to report as
removed.

This is about the table, not the bill: a
[`gr_cache_client()`](https://elkronos.github.io/readgpt/reference/gr_cache_client.md)
already makes the second copy's calls free. What it could not do is stop
the duplicate from appearing in the results as a second, independent
document, which is how one study gets counted twice in a synthesis.

The comparison is exact, on cleaned text. Two typesettings of one paper
are two documents here; matching those needs bibliographic metadata, not
text.

## Budgets

Every document gets its own trace, so `gr_options(max_calls =)` and
`gr_options(max_cost_usd =)` apply per document exactly as they would if
you read it alone. One enormous document therefore cannot starve the
rest.

That is a deliberate design and it leaves the run itself unbounded: two
hundred documents under a 400-call ceiling is a *corpus* ceiling of
eighty thousand calls. The run-level ceilings are `max_total_calls`,
checked before each document, and `max_total_usd`, checked after each
one because what a document costs is not knowable until it has been
read. With neither set, the run says once what its worst case is rather
than leaving you to multiply.

## What this does not do

It reads documents one at a time. Per-document work is embarrassingly
parallel, and the per-worker traces the parallel helper already builds
would carry the accounting across, so the obstacle is not the trace: it
is that duplicate detection, the resume store and both run-level
ceilings are all order-dependent, and a parallel loop would have to
serialise on each of them. Within a document,
`gr_options(parallel = TRUE)` already applies.

## See also

[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md),
[`gr_cache_client()`](https://elkronos.github.io/readgpt/reference/gr_cache_client.md),
[`gr_trace_cost()`](https://elkronos.github.io/readgpt/reference/gr_trace_cost.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")

a <- tempfile(fileext = ".txt"); writeLines("Revenue was 45.2 million.", a)
b <- tempfile(fileext = ".txt"); writeLines("Revenue was 51.8 million.", b)

out <- gr_read_many(c(a, b), "What was revenue?", "fast", client = cl)
#> 2 document(s), and gr_options(max_calls) is 400 PER DOCUMENT, so this run may make up to 800 call(s). Pass max_total_calls = to cap the run.
#> [1/2] file1dcd2173fabc.txt
#> Extracting 'file1dcd2173fabc.txt' with the 'txt' extractor.
#> Ingested 1 block(s), ~11 tokens (0 chars removed by cleaning).
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
#> [2/2] file1dcd602b5e17.txt ($0.0003 spent so far)
#> Extracting 'file1dcd602b5e17.txt' with the 'txt' extractor.
#> Ingested 1 block(s), ~11 tokens (0 chars removed by cleaning).
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
out$summary[, c("document", "answer", "not_found", "status")]
#>               document                            answer not_found status
#> 1 file1dcd2173fabc.txt Revenue was 45.2 million dollars.     FALSE     ok
#> 2 file1dcd602b5e17.txt Revenue was 45.2 million dollars.     FALSE     ok

# A missing file is one bad row, not a failed run.
bad <- gr_read_many(c(a, "no-such-file.txt"), "What was revenue?", "fast", client = cl)
#> 2 document(s), and gr_options(max_calls) is 400 PER DOCUMENT, so this run may make up to 800 call(s). Pass max_total_calls = to cap the run.
#> [1/2] file1dcd2173fabc.txt
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
#> [2/2] no-such-file.txt ($0.0003 spent so far)
#> Warning: Document 'no-such-file.txt' failed: File not found: 'no-such-file.txt'. If you meant to pass document text rather than a path, it must not end in something that looks like a file extension.
bad$summary[, c("document", "status", "error")]
#>               document status
#> 1 file1dcd2173fabc.txt     ok
#> 2     no-such-file.txt failed
#>                                                                                                                                                       error
#> 1                                                                                                                                                      <NA>
#> 2 File not found: 'no-such-file.txt'. If you meant to pass document text rather than a path, it must not end in something that looks like a file extension.

# What the run actually cost, counting only calls that were really issued.
gr_trace_cost(out$trace)
#>           model calls paid_calls paid_in paid_out     usd
#> 1 gpt-5.6-terra     2          2     184       26 0.00068
```
