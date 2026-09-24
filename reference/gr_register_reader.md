# Register a reading strategy

Axis 3 is a registry, so how the model is made to *read* a chunk set is
yours to define. A registered reader is a first-class one: it appears in
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
can be named in a
[`gr_recipe()`](https://elkronos.github.io/readgpt/reference/gr_recipe.md)
or an `ensemble`, and is subject to the same call and spending limits as
the built-ins.

## Usage

``` r
gr_register_reader(name, fn, signature, description = "", cost_calls = "")
```

## Arguments

- name:

  Reader name. Re-registering an existing name replaces it. `"auto"` is
  reserved for
  [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md).

- fn:

  Function of `(chunks, question, client, spec, trace)` returning a
  `gr_answer`. See the section below.

- signature:

  Traversal signature, `"select|calls|state"`. Two readers with the same
  signature are the same methodology under two names;
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  uses it, together with the ingest and segment specs, to decide whether
  two recipes are the same experiment, and `ensemble` refuses members
  that share one. A `select` of `"all"` says the reader sends every
  chunk, so a run whose chunks alone would cost more than `max_cost_usd`
  is refused before its first request.

- description:

  One-line description, shown by
  [`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md).

- cost_calls:

  Human-readable call count in terms of N chunks (`"N + 1"`,
  `"1 + embeddings"`), shown by
  [`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md).

## Value

Invisibly, `name`.

## Writing a reader

Your function receives:

- `chunks`:

  A
  [gr_chunks](https://elkronos.github.io/readgpt/reference/gr_chunks.md).
  `chunks$chunks` is the data frame: `chunk_id`, `text`, `tokens`,
  `chars`, `page`, `section`, `block_id`.

- `question`:

  The question, already validated as non-blank.

- `client`:

  Pass it to
  [`gr_call()`](https://elkronos.github.io/readgpt/reference/gr_call.md);
  never construct your own.

- `spec`:

  A
  [`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md).
  Honour at least `model`, `max_answer_tokens` and `temperature`.

- `trace`:

  Pass it to every
  [`gr_call()`](https://elkronos.github.io/readgpt/reference/gr_call.md)
  so your calls are counted and priced, and check
  `readgpt:::trace_can_call(trace)` before each one so the run's call
  and spending limits are respected.

Return
[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md).
Budget your prompt with
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)
rather than assuming the document fits.

## See also

[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)
to build the return value,
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md),
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)

Other reading functions:
[`gr_answer`](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md),
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md),
[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)

## Examples

``` r
# A reader that answers from the single longest chunk.
gr_register_reader("longest", signature = "one|1|none", cost_calls = "1",
  description = "answer from the longest chunk only",
  fn = function(chunks, question, client, spec, trace) {
    d <- chunks$chunks
    i <- which.max(d$tokens)
    res <- gr_call(client, list(
      list(role = "user", content = paste0(d$text[i], "\n\nQuestion: ", question))),
      model = spec$model, trace = trace, label = "longest.answer")
    new_answer(res$text, "longest", question, d$chunk_id[i], trace)
  })

ch <- gr_segment(readgpt_example(), list(method = "sentence", max_tokens = 120))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'sentence' (cap 120 tokens, overlap 0).
gr_read(ch, "What was revenue?", gr_mock_client(function(m, p) "45.2 million"),
        "longest")$answer
#> Reading with 'longest' (one|1|none) over 6 chunk(s).
#> [1] "45.2 million"
```
