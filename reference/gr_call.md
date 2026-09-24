# Call a model

Call a model

## Usage

``` r
gr_call(
  client,
  messages,
  model = NULL,
  max_output = NULL,
  temperature = NULL,
  schema = NULL,
  schema_name = "result",
  trace = NULL,
  label = "call",
  ...
)
```

## Arguments

- client:

  A `gr_client`.

- messages:

  A list of `list(role=, content=)` items, or a single string, which is
  wrapped as one user message. `role` may be `"system"`, `"developer"`,
  `"user"` or `"assistant"`.

- model:

  Overrides the client default.

- max_output:

  Maximum completion tokens. Clamped to the model's limit and to what
  the context window actually leaves after the prompt.

- temperature:

  Sampling temperature; dropped automatically for reasoning models that
  reject it.

- schema:

  Optional JSON Schema (as a list) requesting structured output.

- schema_name:

  Name for the schema.

- trace:

  Optional `gr_trace` to record this call into.

- label:

  Short label for the trace entry.

- ...:

  Extra top-level body fields.

## Value

A
[gr_result](https://elkronos.github.io/readgpt/reference/gr_result.md).
**Never** `NULL`; `$text` is always a single string, `""` on failure. A
successful HTTP call that returned no text (a refusal, a content filter)
is reported as `ok = FALSE`, so an empty completion is never passed
downstream as evidence. `$cached` is `TRUE` when the response came from
a
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
or a
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
instead of the network.

## See also

[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md),
[`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md),
[gr_result](https://elkronos.github.io/readgpt/reference/gr_result.md),
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_cache_client()`](https://elkronos.github.io/readgpt/reference/gr_cache_client.md),
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)

## Examples

``` r
cl <- gr_mock_client(function(messages, params) "The answer is 42.")
res <- gr_call(cl, "What is the answer?")
c(ok = res$ok, text = res$text)
#>                  ok                text 
#>              "TRUE" "The answer is 42." 

# A failed call still returns a usable object.
bad <- gr_call(gr_mock_client(function(m, p) stop("network down")), "hi")
c(ok = bad$ok, text = sprintf("<%s>", bad$text), error = bad$error)
#>             ok           text          error 
#>        "FALSE"           "<>" "network down" 
```
