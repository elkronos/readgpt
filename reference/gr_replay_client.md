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

  A `gr_trace`; a path to a file written by
  [`gr_trace_save()`](https://elkronos.github.io/readgpt/reference/gr_trace_save.md);
  the JSON text
  [`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md)
  writes for a trace or for an answer (whose trace is used); or an
  already-parsed list in that shape. Parse a file yourself with
  `jsonlite::fromJSON(path, simplifyVector = FALSE)`: the default
  simplification turns the steps into a data frame, which loses what a
  replay needs, and is refused with a message that says so.

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

A response is matched on the exact prompt messages plus the model id the
call asked for. That can differ from the model the trace records as
answering: a
[`gr_ellmer_client()`](https://elkronos.github.io/readgpt/reference/gr_ellmer_client.md)
answers with its chat's model whatever the recipe asked for, and its
runs replay all the same.

The replay client's own model, which a read that names none asks for, is
the model the recorded reads asked for. One trace can hold reads through
clients built for different models, and a recording made by readgpt
0.5.0 asked for `gr_options("model")` on a read and for the client's
model everywhere else; a call that asks for the replay client's model
and finds nothing under it is answered from the one other such model
that holds the same prompt. A model the recorded settings named
(`model`, `skim_model`, `summary_model`) is never used that way, so a
replay that leaves one out misses. When a run issued the same prompt
more than once (which happens at a temperature above zero, and in
readers that revisit a chunk), the recorded responses are returned in
the order they were produced. Once they are exhausted the last one
repeats.

## Embeddings

A trace records each request to an embeddings endpoint (a step labelled
`"embed.request"`, counted in `calls` and priced like any other
request), but not the vectors that came back, so a replay has nothing to
answer those requests with. A run that embedded (the `semantic`
segmenter, the `retrieve` and `iterative` readers, `rerank` when word
overlap finds nothing, and so the `"needle"` recipe) therefore replays
only when the recording used a **deterministic** embedder and the replay
uses the **same** one: the vectors are then computed again, exactly.
Both conditions, because replaying an API-embedded run with a
deterministic local embedder would compute vectors the original never
saw while looking exact. Anything else, including a run recorded with
the default `"api"` embedder (or a
[`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md)'s
embed handler), warns with class `gr_replay_no_embeddings` and falls
back to hashed lexical vectors. Those place semantic cuts and rank
chunks differently, so such a replay usually sends prompts the recording
does not hold: a strict replay stops with `gr_replay_miss`, whose
message names this cause, and a non-strict one gets failed calls for
them and does not give the recorded answer. Record a run you intend to
publish or replay with `gr_options(embedder = "lexical")`, or with your
own embedder registered as `deterministic = TRUE`.

## Limits

A saved trace says whether a limit cut the run short (`budget_stop`,
`stop_reason`), and a replay of such a run stops where it stopped when
it runs under the limits it was recorded with. Replayed calls count
against `max_calls` as they did when recorded. When the spending limit
stopped the recorded run, each replayed call also counts what it cost
when recorded against `max_cost_usd` (in the trace's `replayed_usd`;
nothing is spent); for any other run it counts nothing, so a replay of
an expensive run is not stopped by the replaying session's limit. A
replay under a higher limit, or none, that asks for a call past the
recorded stop gets a `gr_replay_miss` saying so, and one under a lower
limit stops sooner, marked partial as the run would have been.

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
