# Did the model report that the document does not contain the answer?

Every reader is instructed to reply with exactly `"NOT_IN_DOCUMENT"`
when the excerpts do not answer the question. Use this rather than
`grepl("NOT_IN_DOCUMENT", ans$answer)`: a real answer can quote the
sentinel ("the log said NOT_IN_DOCUMENT, but revenue was 45.2 million"),
and models do not reproduce the token byte-exactly. They wrap it in
quotes, bold it, or add a full stop. This matches the sentinel *alone*,
modulo that decoration, and treats a blank answer as not-found too.

## Usage

``` r
is_not_found(x)
```

## Arguments

- x:

  An answer string, or `ans$answer`.

## Value

`TRUE` if the string is the not-found sentinel (or blank).

## Details

The v1 test was `grepl("not found|no information|not applicable", ...)`
over the whole response, which threw away every answer that happened to
contain one of those phrases.

## See also

[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md),
whose `summary$not_found` column is this predicate applied per recipe

Other reading functions:
[`gr_answer`](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md),
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)

## Examples

``` r
is_not_found("NOT_IN_DOCUMENT")
#> [1] TRUE
is_not_found("**NOT_IN_DOCUMENT.**")     # models decorate it
#> [1] TRUE
is_not_found("")                          # nothing came back
#> [1] TRUE

# A real answer that merely mentions the sentinel is NOT not-found.
is_not_found("The log said NOT_IN_DOCUMENT, but revenue was 45.2 million.")
#> [1] FALSE
```
