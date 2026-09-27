# Check that quoted evidence really is in the document

`ans$evidence` says what an answer rests on. For most readers those
spans are verbatim chunk text and are true by construction. For `skim`
they are what the model chose to write when asked to extract the
relevant passages. They are *presented* as quotations, and this is what
checks that they are.

## Usage

``` r
gr_verify_evidence(answer, chunks = NULL)
```

## Arguments

- answer:

  A
  [gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md).

- chunks:

  The
  [gr_chunks](https://elkronos.github.io/readgpt/reference/gr_chunks.md)
  the answer was read from. Needed for readers whose evidence is
  verbatim, where the comparison is against the chunk the span claims to
  come from: its `source_text` where the segmenter recorded one (the
  document text behind a chunk whose `text` carries model-written
  context or propositions), and its `text` otherwise. `skim` answers
  already carry their sources, so they can be checked without it; an
  `ensemble` needs it for the rows its verbatim members contributed,
  even though its `skim` rows do not.

  Pass the chunks the answer was actually read from. Chunk ids are
  positional, so a *different* chunk set of the same size will match on
  id and compare each span against unrelated text, reporting
  `verified = FALSE` for evidence that is perfectly sound. An id the
  chunk set does not contain reports `NA`, because there was nothing to
  compare against.

## Value

A data frame with one row per evidence span: `chunk_id`, `kind`
(`"verbatim"`, `"extracted"` or `"answer"`, **per row**; an `ensemble`
mixes them in one table, and the same value appears as the `kind` column
on `ans$evidence` itself), `verified`, `match` and `span` (the first 60
characters). `verified` is `NA` where the question does not apply: a
`map_reduce` evidence row is a per-chunk *answer*, not a quotation, and
asking whether it appears in the chunk is a category error.

For an `extract` answer, a quote has to carry the value it is cited for
as well as appear in the chunk, and only the reader can check the first:
it knows the value. A row the reader marked `verified = FALSE` for that
reason stays `FALSE` here, with a `match` of 1 when the sentence itself
is in the document. It is there, and it does not say this.

## Details

A fabricated citation is more convincing than a fabricated answer,
because it looks like the thing that would let you check. Verification
is a string operation on text you already have, so it costs nothing and
there is no reason not to do it.

## What the numbers mean

`match` is 1 for an exact quotation once whitespace, quote marks, dashes
and case are folded away. These are the differences a faithful quotation
introduces. It has to match whole words and whole numbers: "5%" is not
found in "25%", nor "12%" in "-12%", nor "20" in "200", nor "200" in "1
200" written with a thin space. Chinese, Japanese and Thai put no spaces
between words, so a clause quoted from them may start and end anywhere;
numbers in them are still whole. Markdown bold is not text, in the
quotation or the document. A span made of several passages (in
paragraphs, as a list, in separate quote marks, or joined by "..." or
"\[...\]") is checked passage by passage and is verified when every
passage is found, the parts either side of an elision in that order.
What an elision leaves out may not be a negation ("the drug did ...
reduce mortality" is not in "the drug did not reduce mortality"), and
where it leaves out the end of a sentence the part after it has to start
one ("Revenue ... rose 30%" is not in "Revenue fell 12%. Costs rose
30%."). A line break inside a sentence is a line wrap, not a new
passage: the lines are checked as one. Below 1 it is the fraction of the
span's words carried by its longest consecutive **run** in the source,
for the passage that matches worst; in a script written without spaces,
the fraction of its characters.

Read that number with its shape in mind. Because it measures a run,
*where* the change falls matters as much as how much changed: altering
the last word of a ten-word span leaves a run of nine and scores 0.9,
while altering a word in the middle splits the span and scores about
0.5. So a mid-sentence change (a swapped figure, the case this exists to
catch) lands near 0.5, not near 0.9. Below roughly 0.3 there is no
quotation left at all, only shared vocabulary. A run measure is still
the right one: word overlap cannot tell a quotation from a paraphrase
assembled out of the same words.

## Citations

With `cite = TRUE` a reader asks the model to mark its sources as
`[chunk 3]`. Every answer is checked for citations pointing at chunks
that were never sent, whatever this function is called with; the result
is `ans$notes$cited_unknown`, and an answer carrying one is `partial`.
Lists and ranges are read (`[chunks 1, 2, and 9]`, `[chunks 1-9]`), and
a bracket that opens like a citation but cannot be read, such as
`[chunk nine]`, is listed in `ans$notes$cited_unparsed` and makes the
answer `partial` too.

## See also

[gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md),
[`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md),
[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md)

## Examples

``` r
# A model that quotes faithfully.
honest <- gr_mock_client(function(messages, params) {
  txt <- messages[[length(messages)]]$content
  if (grepl("You extract evidence", messages[[1]]$content, fixed = TRUE)) {
    return("Revenue rose to 45.2 million dollars.")
  }
  "Revenue was 45.2 million dollars."
})

doc <- "Revenue rose to 45.2 million dollars.\n\nHeadcount grew to 1,204."
ch <- gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 40))
#> Ingested 2 block(s), ~26 tokens (0 chars removed by cleaning).
#> Segmenting with 'paragraph' (cap 40 tokens, overlap 0).
ans <- gr_read(ch, "What was revenue?", honest, "skim")
#> Reading with 'skim' (all|N+1|none) over 1 chunk(s).
gr_verify_evidence(ans)
#>   chunk_id      kind verified match                                  span
#> 1        1 extracted     TRUE     1 Revenue rose to 45.2 million dollars.

# A model that invents one. The span is fluent, plausible, and not in the
# document. That is exactly the case a reader cannot catch by eye.
liar <- gr_mock_client(function(messages, params) {
  if (grepl("You extract evidence", messages[[1]]$content, fixed = TRUE)) {
    return("Revenue rose to 88.9 billion dollars on record demand.")
  }
  "Revenue was 88.9 billion dollars."
})
bad <- gr_read(ch, "What was revenue?", liar, "skim")
#> Reading with 'skim' (all|N+1|none) over 1 chunk(s).
gr_verify_evidence(bad)
#>   chunk_id      kind verified match
#> 1        1 extracted    FALSE 0.333
#>                                                     span
#> 1 Revenue rose to 88.9 billion dollars on record demand.
bad$partial
#> [1] TRUE
```
