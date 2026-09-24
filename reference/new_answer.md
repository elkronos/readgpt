# Build a `gr_answer`, the object every reader must return

Exported because it is part of the extension API:
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md)
rejects anything that does not inherit `"gr_answer"`, so a custom reader
registered with
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md)
cannot be written without this. Using it also means your reader reports
`partial`, `evidence` and `notes` the same way the built-ins do, so
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
can put it in the same table.

## Usage

``` r
new_answer(
  text,
  reader,
  question,
  chunks_used,
  trace,
  evidence = NULL,
  partial = FALSE,
  notes = list(),
  chunks_sent = NULL
)
```

## Arguments

- text:

  The answer string. Use `"NOT_IN_DOCUMENT"` when the chunks did not
  contain the answer;
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  counts that separately from a failure.

- reader:

  Your reader's name, as registered.

- question:

  The question, carried through for the record.

- chunks_used:

  Integer `chunk_id`s that CONTRIBUTED to the answer, not every chunk
  you sent.

- trace:

  The `gr_trace` passed to your reader. Pass it through; do not create a
  new one, or your calls will not appear in the run's totals.

- evidence:

  Optional data frame of supporting spans; build it with the columns
  `chunk_id`, `text`, `page`, `section`, `score`.

- partial:

  `TRUE` if anything degraded: a failed call, a dropped chunk, a
  truncated prompt. Callers are told to check this before trusting
  `$answer`, so setting it honestly matters more than it looks.

- notes:

  Named list of whatever your reader wants to report.

- chunks_sent:

  Chunk ids the reader actually put in front of the model, when that is
  more than `chunks_used`. Used only to tell a fabricated citation from
  a faithful one: a per-chunk reader drops the chunks that answered "not
  in this excerpt", and citing one of those is not an invention.
  Defaults to `chunks_used`.

## Value

A
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md).

## See also

[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)

Other reading functions:
[`gr_answer`](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md),
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md)

## Examples

``` r
# The minimum a custom reader has to return.
tr <- gr_trace()
a <- new_answer("Revenue was 45.2 million.", "my_reader", "What was revenue?",
                chunks_used = 1L, trace = tr, notes = list(strategy = "first chunk"))
a$partial
#> [1] FALSE
a$notes$strategy
#> [1] "first chunk"
```
