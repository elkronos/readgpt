# The result of one reading run

Returned by
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md)
and, with three extra fields, by
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md).

## Fields

- `answer`:

  Character(1). Always a single string. The sentinel `"NOT_IN_DOCUMENT"`
  means the model reported the document does not contain the answer.
  Test it with
  [`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md)-style
  matching rather than substring search.

- `partial`:

  Logical(1). `TRUE` when anything degraded: a call failed, chunks were
  dropped, a cap was hit, a strategy fell back, text was cut to fit, or
  pages of the document never became text (a scan read without OCR).
  **Check this before trusting an answer.**

- `notes`:

  List. Why it is partial, and per-reader detail: `dropped_chunks`,
  `failed_calls`, `error`, `merge_levels`, `degraded_to_bm25`,
  `stop_reason`, `call_cap_reached`, `cost_cap_reached`,
  `summaries_truncated`, and so on. Three can be set for every reader:
  `cited_unknown`, chunk ids the answer cited that were never sent;
  `unverified_evidence`, the number of quoted spans that are not in the
  chunk they claim to come from; and `unread_pages`, the pages that
  never became text. Any of them makes the answer `partial`.

- `warnings`:

  Character. What readgpt warned about while the document was ingested,
  cut and read, named by the warning's class. The warnings still print
  as they happen; this copy stays with the answer, including when the
  document came from the ingestion cache and nothing was raised again.

- `evidence`:

  Data frame or `NULL`, with columns `chunk_id`, `text`, `page`,
  `section`, `score`, `kind`. **What `text` holds depends on the
  reader**: verbatim chunk text for `stuff`, `retrieve`, `rerank` and
  `iterative`; model-extracted passages for `skim`; per-chunk model
  answers for `map_reduce`. `refine` and `hierarchical` return `NULL`.
  `page` is populated only for PDF sources; `score` only for `retrieve`
  (cosine) and `rerank` (0-10, model-judged). Where the evidence is
  *model-written* (`skim`), three more columns appear: `source_text`,
  the chunk the span claims to quote, plus `verified` and `match` from
  checking one against the other; see
  [`gr_verify_evidence()`](https://elkronos.github.io/readgpt/reference/gr_verify_evidence.md).
  Readers whose evidence is verbatim chunk text do not carry them,
  because the span and its source are the same string. `kind` says what
  that row holds (`"verbatim"`, `"extracted"` or `"answer"`) per row,
  because an `ensemble` mixes them. A blank roxygen line inside a
  `\describe{}` item ends the item, which is why this is one paragraph.

- `chunks_used`:

  Integer vector of `chunk_id`s that CONTRIBUTED to the answer. For the
  per-chunk readers this is a subset of the chunks sent: a chunk that
  answered `NOT_IN_DOCUMENT` was read and paid for but is not listed.
  `notes$chunks` reports how many were sent.

- `reader`, `signature`:

  Which strategy ran, and its traversal signature (see
  [`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md)).

- `question`:

  The question, as asked.

- `trace`:

  The
  [gr_trace](https://elkronos.github.io/readgpt/reference/gr_trace.md)
  for this run. In a
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  the trace is shared across recipes, so it records every recipe's
  calls.

- `recipe`, `document`, `segmentation`:

  Added by
  [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
  and
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md):
  the recipe name, the source and ingestion stats, and the chunk
  statistics the reader saw.

## Methods

[`print()`](https://rdrr.io/r/base/print.html) shows the answer, the
calls, tokens and cost, where the evidence came from, and, when the
answer is partial, why;
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md)
serialises the answer together with every prompt and response from the
same run.

## See also

[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
and
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md)
which return one,
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
to compare several,
[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md)
to test the sentinel,
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md),
[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)
to build one in a custom reader

[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
[`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md)

Other reading functions:
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md),
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md),
[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)

## Examples

``` r
cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl)
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
ans$partial
#> [1] FALSE
ans$evidence[, c("chunk_id", "page", "score")]
#>   chunk_id page score
#> 1        1   NA    NA
```
