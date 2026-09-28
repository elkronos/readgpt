# Did the model report that the document does not contain the answer?

Every reader is instructed to reply with exactly `"NOT_IN_DOCUMENT"`
when the excerpts do not answer the question. Use this rather than
`grepl("NOT_IN_DOCUMENT", ans$answer)`: a real answer can quote the
sentinel ("the log said NOT_IN_DOCUMENT, but revenue was 45.2 million"),
and models do not reproduce the token byte-exactly. They wrap it in
quotes, bold it, or add a full stop, in the punctuation of the language
they are writing: Japanese corner brackets, an ideographic full stop,
French guillemets. This matches the sentinel alone, modulo that
decoration (any punctuation, symbol or space around it), and treats a
blank answer as not-found too.

## Usage

``` r
is_not_found(x)
```

## Arguments

- x:

  An answer string, or `ans$answer`.

## Value

`TRUE` if the string is the not-found sentinel (or blank), or opens with
it as described above.

## Details

Models also explain themselves:
`"NOT_IN_DOCUMENT. The excerpt only covers costs."`,
`"NOT_IN_DOCUMENT (the excerpt covers costs only)"`, or the sentinel and
then a blank line and a sentence. A reply that OPENS with the sentinel,
written as the token (with its underscores, or in capitals), and breaks
off there (a full stop, colon, semicolon or comma before a space, an
ideographic full stop or comma, a dash, an opening bracket, or a line
break) is the not-found verdict whatever follows, and counts as
not-found. That includes `"NOT_IN_DOCUMENT. However, ..."`: the model's
verdict is the sentinel, and what it adds is not an answer to the
question. The sentinel anywhere else in a reply, or opening a sentence
that goes on ("NOT_IN_DOCUMENT is what the log printed"), is part of a
real answer.

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
is_not_found("NOT_IN_DOCUMENT. The excerpt only covers costs.")
#> [1] TRUE

# A real answer that merely mentions the sentinel is NOT not-found.
is_not_found("The log said NOT_IN_DOCUMENT, but revenue was 45.2 million.")
#> [1] FALSE
```
