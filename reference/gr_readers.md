# List registered reading strategies

The catalogue for axis 3, and the cheapest way to choose one: the
`signature` column tells you how a reader traverses the chunks and
`cost_calls` tells you what that costs, both before you spend anything.

## Usage

``` r
gr_readers()
```

## Value

A data frame with one row per registered reader: `name`, `signature`
(see
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md)),
`cost_calls` (a formula in N, the number of chunks, not a number) and
`description`.

## See also

[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md),
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
to run several and compare

Other reading functions:
[`gr_answer`](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
[`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md),
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md),
[`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)

## Examples

``` r
# Grouped by how they select chunks, which is the real taxonomy:
# `all|...` readers see every chunk, `topk|...` readers see a selection.
r <- gr_readers()
r[order(r$signature), c("name", "signature", "cost_calls")]
#>            name             signature
#> 12        stuff            all|1|none
#> 11         skim          all|N+1|none
#> 2       extract  all|N+conflicts|none
#> 5    map_reduce       all|N+logN|tree
#> 3  hierarchical     all|N+tree+1|tree
#> 7        refine         all|N|forward
#> 1      ensemble   ensemble|sum+1|none
#> 10       screen           head|1|none
#> 6       preview    planned|1+s+1|none
#> 9      retrieve           topk|1|none
#> 8        rerank         topk|m+1|none
#> 4     iterative topk|rounds*2|forward
#>                                    cost_calls
#> 12                                          1
#> 11                                      N + 1
#> 2  N (+1 per conflict with resolve = 'model')
#> 5                                  N + merges
#> 3                       N + fan-in levels + 1
#> 7                                           N
#> 1                          sum of members + 1
#> 10                                          1
#> 6                    1 + skimmed sections + 1
#> 9                              1 + embeddings
#> 8                                       m + 1
#> 4                        up to 2 x max_rounds
```
