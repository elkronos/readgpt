# Resolve the API key

Looks in `key`, then the `readgpt.api_key` option, then
`OPENAI_API_KEY`. Never prints or logs the key.

## Usage

``` r
gr_api_key(key = NULL)
```

## Arguments

- key:

  Optional explicit key.

## Value

The key string. Raises a `gr_auth_error` if none is found.

## No key set

A run that would have to send a request without a key stops with a
`gr_auth_error` that says how to set one.
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
and
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
check before reading the document, so a missing key does not first cost
an OCR pass; everything else stops at the first request. A client that
sends its credential in `headers`, a mock, a backend or replay client,
and a client with a response cache attached are not affected.

## See also

[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md)

## Examples

``` r
# Resolution order: explicit argument, then the option, then the env var.
previous <- Sys.getenv("OPENAI_API_KEY", unset = NA)
Sys.setenv(OPENAI_API_KEY = "sk-example")
gr_api_key()
#> [1] "sk-example"
gr_api_key("sk-explicit-wins")
#> [1] "sk-explicit-wins"

# With no key set it says so. A run stops the same way, before its first
# request; see the section above.
Sys.unsetenv("OPENAI_API_KEY")
tryCatch(gr_api_key(), gr_auth_error = function(e) conditionMessage(e))
#> [1] "No API key. Set OPENAI_API_KEY, or options(readgpt.api_key = '...'), or pass `api_key` to gr_client()."

# Restore. Sys.setenv(x = NA) would leave the variable set to "NA", not unset.
if (!is.na(previous)) Sys.setenv(OPENAI_API_KEY = previous)
```
