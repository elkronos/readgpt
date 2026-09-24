# Ready-made recipes

Named starting points that pair a segmentation strategy with a reader
that suits it. Each is a plain `gr_recipe`, so you can modify any field.

## Usage

``` r
gr_recipes(name = NULL)
```

## Arguments

- name:

  Optional recipe name, or a character vector of names; omit to list
  them all. A single name returns a `gr_recipe`; several return a named
  list of them.

## Value

A `gr_recipe` when `name` is a single string, otherwise a named list of
`gr_recipe`s.

## `"auto"`

[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
defaults to `recipe = "auto"`. It is not a recipe but a choice between
two of these, made once the document is ingested: `"fast"` for a
document of at most 50,000 tokens (less on a model with a small context
window), `"thorough"` for anything longer. "Choosing the recipe" in
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
gives the whole rule. Functions that read several documents or compare
recipes take one fixed recipe, and refuse `"auto"`.

## See also

[`gr_recipe()`](https://elkronos.github.io/readgpt/reference/gr_recipe.md)
to build your own,
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md),
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md)
and
[`gr_readers()`](https://elkronos.github.io/readgpt/reference/gr_readers.md)
for the pieces they are made of

## Examples

``` r
# What each built-in actually is, in one table.
do.call(rbind, lapply(names(gr_recipes()), function(n) {
  r <- gr_recipes(n)
  data.frame(recipe = n, clean = paste(as.character(r$ingest$clean), collapse = "+"),
             segment = r$segment$method, max_tokens = r$segment$max_tokens,
             reader = r$read$reader)
}))
#>       recipe    clean    segment max_tokens       reader
#> 1       fast standard  paragraph       4000        stuff
#> 2    precise standard   sentence        600         skim
#> 3     needle standard   semantic        500     retrieve
#> 4   thorough standard  paragraph       1200   map_reduce
#> 5     survey standard structural       1500 hierarchical
#> 6  narrative standard  paragraph       1500       refine
#> 7    scanned     scan       page       2000       rerank
#> 8   research academic structural        900    iterative
#> 9  consensus standard  recursive       1000     ensemble
#> 10    legacy   legacy  paragraph       3000   map_reduce

gr_recipes("precise")
#> <gr_recipe 'precise'>
#>   ingest  : clean=standard ocr=auto
#>   segment : sentence (max 600 tok, overlap 60, min 0)
#>   read    : skim [all|N+1|none] model=gpt-5.6-terra
```
