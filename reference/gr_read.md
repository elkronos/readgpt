# Read chunks and answer a question

The third axis. Reading is a separate decision from segmentation because
the call pattern (which chunks reach the model, in how many requests,
and whether anything flows between them) is where both cost and answer
quality are actually decided. The same chunk set can be read many ways;
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md)
lists them with what each costs.

## Usage

``` r
gr_read(chunks, question, client, spec = NULL, trace = NULL)
```

## Arguments

- chunks:

  A `gr_chunks` object from
  [`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md).

- question:

  The question.

- client:

  A `gr_client`.

- spec:

  A `gr_read_spec`, a reader name, or a named list.

- trace:

  Optional `gr_trace`; one is created when omitted.

## Value

A
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md).
Check `$partial` before trusting `$answer`.

## See also

[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)

Other reading functions:
[`gr_answer`](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md),
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md),
[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)

## Examples

``` r
ch <- gr_segment(readgpt_example(), list(method = "sentence", max_tokens = 120))
#> Using cached ingestion for this document + settings.
#> Segmenting with 'sentence' (cap 120 tokens, overlap 0).

# The same chunks, two strategies, two very different call patterns. Each gets
# its own client and trace, so the call counts are comparable.
run <- function(reader, ...) {
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  tr <- gr_trace()
  a <- gr_read(ch, "What was revenue?", cl, c(list(reader = reader), list(...)), trace = tr)
  data.frame(reader = a$reader, signature = a$signature,
             calls = length(cl$calls()), chunks_used = length(a$chunks_used))
}
rbind(run("retrieve", top_k = 2), run("map_reduce"))
#> Reading with 'retrieve' (topk|1|none) over 6 chunk(s).
#> Reading with 'map_reduce' (all|N+logN|tree) over 6 chunk(s).
#>       reader       signature calls chunks_used
#> 1   retrieve     topk|1|none     1           2
#> 2 map_reduce all|N+logN|tree     7           6
```
