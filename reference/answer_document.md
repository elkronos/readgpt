# Answer a question about a document

One recipe, one pipeline: ingest, segment, read. The answer and its full
trace come from a single run, so the trace always explains the answer
you got.

## Usage

``` r
answer_document(
  source,
  question,
  recipe = "auto",
  client = NULL,
  return = c("answer", "text", "json"),
  trace = NULL,
  ...
)
```

## Arguments

- source:

  File path, web address, or raw text; see
  [`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md).

- question:

  The question.

- recipe:

  A `gr_recipe`, a recipe name from
  [`gr_recipes()`](https://elkronos.github.io/readgpt/reference/gr_recipes.md),
  a reader name, or a named list of `ingest`/`segment`/`read`. The
  default, `"auto"`, picks `"fast"` or `"thorough"` from the document's
  length; see "Choosing the recipe" below.

- client:

  A `gr_client`; one is built from options when omitted.

- return:

  `"answer"` (a `gr_answer`), `"text"` (the string), or `"json"` (answer
  plus trace, serialised).

- trace:

  Optional `gr_trace` to accumulate into.

- ...:

  Convenience overrides applied to the recipe: any `gr_read_spec`,
  `gr_segment_spec` or `gr_ingest_spec` field (for example `model`,
  `max_tokens`, `top_k`, `clean`). Unknown names raise an error instead
  of being silently discarded.

## Value

Depends on `return`. The `gr_answer` carries `$partial`; check it before
trusting `$answer`.

## Choosing the recipe

With `recipe = "auto"` the document is ingested first, and its length
decides how it is read. `"fast"` sends the whole document in one
request. It is used for a document of at most 50,000 tokens that also
fills no more than half of the room one request leaves for the document,
which is less on a model with a small context window. Anything longer is
read with `"thorough"`: one request per chunk, then the requests that
combine their answers. Both send every chunk, so the choice changes the
number of requests and the cost, not how much of the document is read.

The room is measured for the recipe's model and, unless `model` is
passed in `...`, for the client's model as well, since a client built
for another model may be what answers. A model whose limits readgpt has
to guess (see
[`gr_model_info()`](https://elkronos.github.io/readgpt/reference/gr_model_info.md))
always gets `"thorough"`; register its real limits with
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md).

The answer's `recipe` names the recipe used, `notes$auto_recipe` records
that `"auto"` chose it, and the trace has an `auto_recipe` step with the
token count and the limit the choice was made on. A
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
repeats the recorded choice rather than making it again. Overrides in
`...` apply to whichever recipe is chosen.
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md),
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
and the review functions need one fixed recipe and refuse `"auto"`.

## See also

[`gr_recipes()`](https://elkronos.github.io/readgpt/reference/gr_recipes.md)
for the built-in pipelines,
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
to run several,
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md)
for the returned object,
[`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)
for the cost and call caps

## Examples

``` r
cl <- gr_mock_client(function(m, p) "The answer is 42.")
txt <- "Chapter one.\n\nThe answer to the great question is 42, as recorded."
answer_document(txt, "What is the answer?", "fast", client = cl, return = "text")
#> Ingested 2 block(s), ~24 tokens (0 chars removed by cleaning).
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
#> [1] "The answer is 42."
```
