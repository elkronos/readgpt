# The result of one model call

Returned by
[`gr_call()`](https://elkronos.github.io/readgpt/reference/gr_call.md).
Designed so a caller can never be handed `NULL` or `character(0)` where
a string is expected.

## Fields

- `ok`:

  Logical(1). `FALSE` for transport failures, HTTP errors, and
  **successful calls that returned no text** (a refusal or content
  filter).

- `text`:

  Character(1), always. `""` when `ok` is `FALSE`. Never `NULL`, never
  `character(0)`, and newlines are preserved.

- `error`:

  Character or `NULL`. What went wrong, including the HTTP body for
  API-side errors.

- `usage`:

  List with `input` and `output` token counts as reported by the API.

- `status`, `finish_reason`, `retryable`, `model`, `raw`:

  HTTP status, the API's stop reason, whether a retry could help, the
  model id, and the parsed response body.

## Methods

[`print()`](https://rdrr.io/r/base/print.html) shows `ok`, the model,
the token usage and either the text or the error.

## See also

[`gr_call()`](https://elkronos.github.io/readgpt/reference/gr_call.md)
which returns one,
[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md),
[`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md)

[`gr_call()`](https://elkronos.github.io/readgpt/reference/gr_call.md),
[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md),
[`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md)

## Examples

``` r
res <- gr_call(gr_mock_client(function(m, p) "hello"), "hi")
c(ok = res$ok, text = res$text)
#>      ok    text 
#>  "TRUE" "hello" 
```
