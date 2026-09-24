# Construct a model client

The client is an object, not a global. Passing it explicitly is what
lets a Shiny app serve two users with two different keys in one R
process. The old code called `Sys.setenv(OPENAI_API_KEY = ...)`, which
is process-wide, so the second user's key silently billed the first
user's requests.

## Usage

``` r
gr_client(
  model = NULL,
  api = NULL,
  api_key = NULL,
  base_url = NULL,
  embedding_model = NULL,
  max_retries = NULL,
  retry_pause_base = NULL,
  timeout = NULL,
  extra_body = list(),
  headers = NULL
)
```

## Arguments

- model:

  Default chat model id.

- api:

  `"responses"` (default) or `"chat"`.

- api_key:

  Optional key; resolved lazily at call time if omitted.

- base_url:

  API base, for proxies and compatible endpoints.

- embedding_model:

  Default embedding model id.

- max_retries, retry_pause_base:

  Retry policy for transient failures.

- timeout:

  Per-request timeout in seconds.

- extra_body:

  Named list merged into every request body.

- headers:

  Named character vector of extra HTTP headers, for endpoints that do
  not authenticate with a bearer token. Defaults to the `api_headers`
  option. `NA` as a value suppresses a header rather than sending it,
  which is how the automatic `Authorization` is dropped.

## Value

An object of class `gr_client`: a list of the settings above.
Constructing one makes no request and does not require a key; the key is
resolved at call time by
[`gr_api_key()`](https://elkronos.github.io/readgpt/reference/gr_api_key.md).

## Endpoints behind a company gateway

`base_url` alone is enough when the gateway speaks the OpenAI shape and
takes `Authorization: Bearer`. It is not enough anywhere else, and most
corporate gateways are somewhere else: Azure OpenAI authenticates with
`api-key`, API Management adds a subscription key, and many require a
cost-centre or correlation id. `headers` covers those.

Two rules make it predictable. A header you name replaces the automatic
`Authorization` rather than joining it, matched without regard to case;
and naming any header at all makes the API key optional, because nothing
here can tell which of your headers is the credential. So a gateway with
its own scheme needs no `OPENAI_API_KEY` set at all:

    gr_client(
      base_url = "https://gateway.example.com/openai/v1", api = "chat",
      headers  = c("api-key" = Sys.getenv("GATEWAY_KEY"), Authorization = NA))

Headers are credentials and routing metadata, not part of what answers,
so they are excluded from the
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
key for the same reason `api_key` is. A rotating bearer or a per-request
correlation id would otherwise make every cache lookup miss. If a header
changes *which* model answers, give that client its own `base_url` or
`model` so the cache can tell them apart.

## See also

[`gr_call()`](https://elkronos.github.io/readgpt/reference/gr_call.md)
to use it,
[`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md)
to work offline,
[`gr_cache_client()`](https://elkronos.github.io/readgpt/reference/gr_cache_client.md)
to make repeat calls free,
[`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
to re-run a recorded run,
[`gr_api_key()`](https://elkronos.github.io/readgpt/reference/gr_api_key.md)
for key resolution,
[`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)
for the defaults,
[gr_result](https://elkronos.github.io/readgpt/reference/gr_result.md)
for what a call returns

## Examples

``` r
# A client is a value, not a global. Two of them, two keys, one R process.
# That is what makes a Shiny app serving two users safe.
a <- gr_client(model = "gpt-4o",  api_key = "sk-user-a")
b <- gr_client(model = "gpt-4.1", api_key = "sk-user-b", timeout = 30)
vapply(list(a = a, b = b), function(cl) cl$model, character(1))
#>         a         b 
#>  "gpt-4o" "gpt-4.1" 

# What that model can actually take.
unlist(gr_model_info(a$model)[c("context_window", "max_output")])
#> context_window     max_output 
#>         128000          16384 

# No key needed to develop against the pipeline.
gr_call(gr_mock_client(function(m, p) "hi there"), "hello")$text
#> [1] "hi there"

if (FALSE) { # \dontrun{
cl <- gr_client(model = "gpt-5.6-terra")
gr_call(cl, list(list(role = "user", content = "Say hi.")), max_output = 20)
} # }
```
