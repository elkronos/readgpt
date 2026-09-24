# A client that answers from a recorded run

Replays the responses in a trace instead of calling a model. Give it the
trace from a run and the same document and question, and you get that
run back (same answers, same evidence, same merge decisions) with no API
key, no network and no spend.

## Usage

``` r
gr_replay_client(source, strict = TRUE)
```

## Arguments

- source:

  A `gr_trace`, a path to a file written by
  [`gr_trace_save()`](https://elkronos.github.io/readgpt/reference/gr_trace_save.md),
  or an already-parsed list in that shape.

- strict:

  If `TRUE` (default), a prompt with no recorded response raises a
  `gr_replay_miss` error. That is usually what you want: a miss means
  the replay has diverged from the recording, and continuing would
  produce a result that looks like the original but is not. With `FALSE`
  a miss returns a failed
  [gr_result](https://elkronos.github.io/readgpt/reference/gr_result.md)
  instead, so a partially recorded trace still runs.

## Value

An object of class `gr_replay_client`, usable anywhere a
[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md)
is. It also carries `$stats()` and `$missed()`.

## Details

This is what makes a published result checkable. Ship the trace next to
the paper and a reader can reproduce the run rather than take it on
trust. It is also the cheapest possible bug report: a trace file is a
re-runnable recording of exactly what went wrong.

## Matching

A response is matched on the exact prompt messages plus the model id.
When a run issued the same prompt more than once (which happens at a
temperature above zero, and in readers that revisit a chunk), the
recorded responses are returned in the order they were produced. Once
they are exhausted the last one repeats.

## Embeddings

Embeddings are not model calls and are not recorded in a trace, so
whether a replay reproduces a run's chunk *ranking* depends on how the
run embedded, and that is checked rather than assumed. The ranking
reproduces exactly when the recording used a **deterministic** embedder
and the replay uses the **same** one; both conditions, because replaying
an API-embedded run with a deterministic local embedder would compute
vectors the original never saw while looking exact. Anything else falls
back to hashed lexical vectors and warns with class
`gr_replay_no_embeddings`; every recorded answer is still reproduced,
but the ranking may differ. Record a run you intend to publish with
`gr_options(embedder = "lexical")`, or with your own embedder registered
as `deterministic = TRUE`.

## The recipe "auto" chose

A recording of
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
with `recipe = "auto"` holds the recipe the choice picked for each
document and question, and a replay of the same document and question
repeats that choice rather than making it again. What the choice rests
on (the token count, the registered models, the client's model) can
differ between the session that recorded a run and the one replaying it,
and a different choice would send prompts the recording does not have.

## What does not replay

Traces do not record the JSON schema a call requested, so two calls that
differ only by schema share a recording.

## See also

[`gr_trace_save()`](https://elkronos.github.io/readgpt/reference/gr_trace_save.md),
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
for making future runs cheap,
[`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md)
for invented answers rather than recorded ones

## Examples

``` r
# A run.
cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl)
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).

# The same run, from the recording, with no client and no key.
rp <- gr_replay_client(ans$trace)
again <- answer_document(readgpt_example(), "What was revenue?", "fast", client = rp)
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
identical(again$answer, ans$answer)
#> [1] TRUE

rp$stats()
#>   recorded distinct hits repeats misses
#> 1        1        1    1       0      0

# Through a file, which is how a run reaches someone else.
f <- tempfile(fileext = ".json")
gr_trace_save(ans$trace, f)
answer_document(readgpt_example(), "What was revenue?", "fast",
                client = gr_replay_client(f))$answer
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
#> [1] "Revenue was 45.2 million dollars."
```
