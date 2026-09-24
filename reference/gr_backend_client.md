# Use any function as the model transport

Wraps an arbitrary R function as a client. `handler` receives
`(messages, params)` and returns a string or a
[gr_result](https://elkronos.github.io/readgpt/reference/gr_result.md);
everything else the package does (context budgeting, cost and call caps,
provenance, the run trace,
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md),
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md),
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md))
works exactly as it does for the built-in HTTP client.

## Usage

``` r
gr_backend_client(
  handler,
  embed = NULL,
  model = "backend-model",
  embedding_model = "backend-embed",
  id = NULL,
  max_retries = 0L,
  timeout = NULL
)
```

## Arguments

- handler:

  Function of `(messages, params)`. `messages` is a list of
  `list(role=, content=)` with roles `"system"`, `"developer"`, `"user"`
  or `"assistant"`. `params` carries `model`, `max_output`,
  `temperature`, `schema`, `schema_name` and `prompt_tokens`. Return a
  single string, or a
  [gr_result](https://elkronos.github.io/readgpt/reference/gr_result.md)
  for full control over token usage and failure reporting.

- embed:

  Optional function of `(texts, params)` returning a numeric matrix with
  one row per input. Without it, readers that embed (`retrieve`, and the
  `semantic` segmenter) fall back to hashed lexical vectors and warn.

- model, embedding_model:

  Model ids reported to the rest of the package. If `model` is not in
  the model registry you will get the usual unknown-model warning and a
  conservative context window;
  [`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md)
  fixes that, and getting it right matters because it is what sizes your
  chunks.

- id:

  Stable identity for this backend, used by
  [`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
  and by
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)'s
  `store`. **Read this before setting up a durable cache.**

  For
  [`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md)
  a cache key can be built from the API shape, base URL and model,
  because those completely describe what will answer: two clients with
  the same three give the same answers today and next month, which is
  what makes a cache safe to keep on disk. A backend has none of that
  (every one of them is `api = "backend"`, `base_url = "backend://"`),
  and the thing that actually answers is an R closure, whose behaviour
  this package cannot inspect.

  So the default is a fresh id per client object, which is *safe*: two
  different handlers can never trade answers. It is also
  *session-scoped*, so a cache or store will not be reused by a later
  session. Pass a stable `id` to get cross-session reuse. Pass one
  **only** when the handler does answer the same way every time, because
  that is the assertion you are making. Hashing the closure would not
  do: two handlers can share a body and differ in what they captured.

- max_retries, timeout:

  Recorded on the client for completeness. Retrying is the backend's
  business: this package does not retry a handler, because it cannot
  know whether the failure was transient.

## Value

An object of class `gr_backend_client`, usable anywhere a
[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md)
is, with `$calls()` recording every request made through it.

## Details

Use it to reach a provider this package does not speak to, a company
proxy, a local model, or another R package's client.
[`gr_ellmer_client()`](https://elkronos.github.io/readgpt/reference/gr_ellmer_client.md)
is this function with an `ellmer` chat object plugged in.

## Failure is data, not an exception

A handler that raises is caught and reported as a failed
[gr_result](https://elkronos.github.io/readgpt/reference/gr_result.md),
the same as an HTTP error, so one bad chunk does not abort a run. A
handler that returns `""` is treated as an empty completion, which this
package reports as `ok = FALSE` rather than passing `""` downstream as
evidence.

## See also

[`gr_ellmer_client()`](https://elkronos.github.io/readgpt/reference/gr_ellmer_client.md),
[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md),
[`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
[`gr_cache_client()`](https://elkronos.github.io/readgpt/reference/gr_cache_client.md)

## Examples

``` r
# Any function will do. This one is deterministic so the example is too.
cl <- gr_backend_client(function(messages, params) {
  user <- Filter(function(m) m$role == "user", messages)
  sprintf("I was asked %d thing(s) with a %d-token cap.",
          length(user), params$max_output)
}, model = "my-backend")

gr_call(cl, "What was revenue?", max_output = 128L)$text
#> Warning: Model 'my-backend' is not in the registry. Falling back to a conservative 128000-token context with a 4096-token output reserve. Register the real limits with gr_register_model('my-backend', context_window = ..., max_output = ...) so budgets and cost estimates are correct.
#> [1] "I was asked 1 thing(s) with a 128-token cap."

# It composes with everything else: caching, tracing, cost accounting.
tr <- gr_trace()
invisible(gr_call(cl, "again", trace = tr))
#> Warning: Model 'my-backend' is not in the registry. Falling back to a conservative 128000-token context with a 4096-token output reserve. Register the real limits with gr_register_model('my-backend', context_window = ..., max_output = ...) so budgets and cost estimates are correct.
gr_trace_summary(tr)[c("calls", "tokens_in", "tokens_out")]
#>   calls tokens_in tokens_out
#> 1     1         5         24
```
