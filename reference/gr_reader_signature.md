# The traversal signature of a reader

The signature is how the package tells two reading *methodologies* apart
from two names for the same thing. It encodes which chunks reach the
model (`select`), the call pattern (`calls`), and whether information
flows between calls (`state`).

## Usage

``` r
gr_reader_signature(reader)
```

## Arguments

- reader:

  Reader name, or a `gr_read_spec`.

## Value

A single string `"select|calls|state"`, e.g. `"topk|1|none"`. For
`ensemble` the member list is appended in braces
(`"ensemble|sum+1|none{map_reduce+retrieve}"`), so two ensembles with
different members are correctly seen as different experiments.

## See also

[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md)
for every signature at once,
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
which uses this to detect duplicate recipes,
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md)

Other reading functions:
[`gr_answer`](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md),
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md),
[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)

## Examples

``` r
# Same document, different traversal: one call over everything, versus one
# call per chunk plus merges.
gr_reader_signature("stuff")
#> [1] "all|1|none"
gr_reader_signature("map_reduce")
#> [1] "all|N+logN|tree"

# An ensemble's signature carries its members, so two differently-composed
# ensembles are not mistaken for each other.
gr_reader_signature(gr_read_spec("ensemble", members = c("retrieve", "refine")))
#> [1] "ensemble|sum+1|none{refine+retrieve}"
```
