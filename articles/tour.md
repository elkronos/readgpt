# A tour of readgpt

This is a short tour of the whole package, for readers who already know
the ideas. If you are new to language models, start with
[`vignette("readgpt")`](https://elkronos.github.io/readgpt/articles/readgpt.md),
which explains them;
[`vignette("ingest")`](https://elkronos.github.io/readgpt/articles/ingest.md)
and
[`vignette("readers")`](https://elkronos.github.io/readgpt/articles/readers.md)
cover two of the decisions below in depth.

Asking a language model a question about a document involves three
decisions, and they are usually made for you, invisibly, all at once.
This package makes them separate:

- **ingest**: get text out of the file, and decide what to throw away
- **segment**: cut that text into pieces small enough to send
- **read**: decide how the model works through those pieces to an answer

Each is a registry you can list, swap and extend. Any ingest × any
segmenter × any reader composes.

``` r

library(readgpt)
```

## Running this vignette without an API key

Everything below runs offline. Two settings make that work, and they are
also how you develop against this package without spending anything.

``` r

cl <- gr_mock_client(function(messages, params) "Revenue was 45.2 million dollars.")
old <- gr_options(verbose = FALSE, embedder = "lexical")
```

[`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md)
is a client whose handler is an ordinary R function. It goes everywhere
a real client goes and records every prompt it was sent, which is what
lets you check that a reading strategy is doing what you think before
you pay for it.

`embedder = "lexical"` selects hashed bag-of-words vectors instead of an
embeddings endpoint. They measure word overlap rather than meaning, so
they are a poor substitute in production, but they are free, offline,
and *deterministic*, which is what makes this document reproducible.
[`gr_embedders()`](https://elkronos.github.io/readgpt/reference/gr_embedders.md)
lists what is registered.

## A first run

[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
binds one ingest, one segmenter and one reader into a pipeline and runs
it. The bundled example is a short annual report.

``` r

ans <- answer_document(readgpt_example(), "What was revenue?", "thorough",
                       client = cl)
ans$answer
#> [1] "Revenue was 45.2 million dollars."
```

`"thorough"` names a recipe.
[`gr_recipes()`](https://elkronos.github.io/readgpt/reference/gr_recipes.md)
returns the built-ins, and each is just a binding of the three axes:

``` r

do.call(rbind, lapply(names(gr_recipes()), function(n) {
  r <- gr_recipes(n)
  data.frame(recipe = n, segment = r$segment$method, reader = r$read$reader)
}))
#>       recipe    segment       reader
#> 1       fast  paragraph        stuff
#> 2    precise   sentence         skim
#> 3     needle   semantic     retrieve
#> 4   thorough  paragraph   map_reduce
#> 5     survey structural hierarchical
#> 6  narrative  paragraph       refine
#> 7    scanned       page       rerank
#> 8   research structural    iterative
#> 9  consensus  recursive     ensemble
#> 10    legacy  paragraph   map_reduce
```

`gr_recipes("thorough")` prints one in full, showing every setting on
all three axes. A recipe is only a starting point: any of its settings
can be overridden in the call, and
[`gr_recipe()`](https://elkronos.github.io/readgpt/reference/gr_recipe.md)
builds one from scratch.

## Reading the answer

The answer is not a string. It is an object that says how it was arrived
at, and the first thing to look at is never `$answer`:

``` r

ans$partial
#> [1] FALSE
```

`partial` is `TRUE` whenever anything degraded: a call failed, chunks
were dropped for space, a budget stopped the run early, or an embedding
fell back to lexical vectors. **Check it before trusting the answer.**
When it is `TRUE`, `$notes` says what happened.

``` r

names(ans$notes)
#> [1] "chunks"       "answered"     "failed_calls" "merge_levels" "merge_ok"
```

`$evidence` is what the answer rests on. What `text` holds depends on
the reader: verbatim chunk text for `stuff`, `retrieve`, `rerank` and
`iterative`; model-extracted passages for `skim`; per-chunk model
answers for `map_reduce`.

``` r

ans$evidence[, c("chunk_id", "page", "section")]
#>   chunk_id page section
#> 1        1   NA      NA
```

And `$trace` records every prompt, response and token count from that
one run, not from a second run made to explain the first.

``` r

gr_trace_summary(ans$trace)[, c("calls", "cached", "tokens_in", "tokens_out")]
#>   calls cached tokens_in tokens_out
#> 1     1      0       595         13
```

## Did the model quote the document, or invent the quote?

For most readers `$evidence` is verbatim chunk text and is true by
construction: the package put it there. For `skim` it is not. `skim`
asks the model to extract the passages that bear on the question, and
what comes back is whatever the model chose to write, presented as a
quotation, with nothing checking that it was one.

That is the worst gap to leave. A fabricated citation is more convincing
than a fabricated answer, because it looks like the thing that would let
you check.

``` r

doc <- "Revenue rose to 45.2 million dollars.\n\nHeadcount grew to 1,204."
ch  <- gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 40))

extracting <- function(quote) gr_mock_client(function(messages, params) {
  if (grepl("You extract evidence", messages[[1]]$content, fixed = TRUE)) return(quote)
  "Revenue was 45.2 million dollars."
})

faithful <- gr_read(ch, "What was revenue?",
                    extracting("Revenue rose to 45.2 million dollars."), "skim")
gr_verify_evidence(faithful)[, c("kind", "verified", "match")]
#>        kind verified match
#> 1 extracted     TRUE     1
```

Now a model that invents one. The span is fluent, on topic, attributed
to a real chunk, and not in the document. You cannot catch this case by
eye:

``` r

invented <- gr_read(ch, "What was revenue?",
                    extracting("Revenue rose to 88.9 billion dollars on record demand."),
                    "skim")
gr_verify_evidence(invented)[, c("kind", "verified", "match")]
#>        kind verified match
#> 1 extracted    FALSE 0.333
invented$partial
#> [1] TRUE
invented$notes$unverified_evidence
#> [1] 1
```

The comparison is forgiving about typography and unforgiving about
content. Whitespace, curly quotes, dashes, case and the punctuation a
model wraps a quote in are all folded away, because none of that is
fabrication and flagging it would make `partial` stop meaning anything.
A changed number is not folded away. Below an exact match, `match` is
the fraction of the span carried by its longest consecutive run in the
source. It uses a run rather than word overlap, because overlap cannot
tell a quotation from a paraphrase built from the same words.

Citations get the same treatment, for every reader. An answer citing a
chunk that was never sent to it sets `notes$cited_unknown` and is
`partial`.

## Axis 2: segmentation is free to experiment with

Chunking decides what the model can possibly see together. It involves
no model calls at all, so you can compare strategies for nothing before
spending anything on reading:

``` r

doc <- gr_ingest(readgpt_example())
do.call(rbind, lapply(c("fixed", "paragraph", "sentence", "structural"), function(m)
  gr_chunk_stats(gr_segment(doc, list(method = m, max_tokens = 120)))))
#>       method n total_tokens min median  mean max over_cap
#> 1      fixed 5          528  49  120.0 105.6 120        0
#> 2  paragraph 6          532  47   90.0  88.7 116        0
#> 3   sentence 6          532  47   92.5  88.7 106        0
#> 4 structural 8          562  31   75.0  70.2 101        0
```

`over_cap` is the column to watch: a chunk over the cap is one the
reader will have to truncate or drop.
[`gr_segmenters()`](https://elkronos.github.io/readgpt/reference/gr_segmenters.md)
lists all nine, with which of them need a client (the `semantic` one
embeds).

Overlap matters too. Without it, an answer straddling a boundary is lost
by both chunks:

``` r

do.call(rbind, lapply(c(0, 40), function(ov)
  gr_chunk_stats(gr_segment(doc, list(method = "sentence", max_tokens = 120,
                                      overlap_tokens = ov)))))
#>     method n total_tokens min median mean max over_cap
#> 1 sentence 6          532  47   92.5 88.7 106        0
#> 2 sentence 8          752  79   97.0 94.0 106        0
```

## Axis 3: each reader works differently

Twelve strategies, each declaring a *traversal signature*: how it
selects chunks, how many calls it makes, and what state it carries
between them:

``` r

gr_readers()[, c("name", "signature", "cost_calls")]
#>            name             signature                    cost_calls
#> 1      ensemble   ensemble|sum+1|none            sum of members + 1
#> 2       extract  all|N+conflicts|none N + one per disagreeing field
#> 3  hierarchical     all|N+tree+1|tree         N + fan-in levels + 1
#> 4     iterative topk|rounds*2|forward          up to 2 x max_rounds
#> 5    map_reduce       all|N+logN|tree                    N + merges
#> 6       preview    planned|1+s+1|none      1 + skimmed sections + 1
#> 7        refine         all|N|forward                             N
#> 8        rerank         topk|m+1|none                         m + 1
#> 9      retrieve           topk|1|none                1 + embeddings
#> 10       screen           head|1|none                             1
#> 11         skim          all|N+1|none                         N + 1
#> 12        stuff            all|1|none                             1
```

The built-in readers all have different signatures, and `ensemble`
refuses members that share one. Two recipes that resolve to the same
ingestion, the same segmentation and the same read settings are the same
work, and
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
refuses to bill you twice for them.

``` r

cmp <- gr_compare(readgpt_example(), "What was revenue?",
                  c("fast", "precise", "thorough"), client = cl)
cmp$summary[, c("recipe", "segmenter", "chunks", "reader", "chunks_used", "not_found")]
#>     recipe segmenter chunks     reader chunks_used not_found
#> 1     fast paragraph      1      stuff           1     FALSE
#> 2  precise  sentence      2       skim           2     FALSE
#> 3 thorough paragraph      1 map_reduce           1     FALSE
```

Extraction is shared across recipes and segmentation is shared between
recipes whose segment specs match, so comparing three readers over one
chunking costs one chunking. One trace covers the whole comparison:

``` r

gr_trace_summary(cmp$trace)[, c("calls", "cached", "tokens_in")]
#>   calls cached tokens_in
#> 1     5      0      2024
```

`not_found` distinguishes “the document does not say” from a failure. So
does
[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md)
on any answer. A model that invents an answer is a worse outcome than
one that admits the document is silent, and the two must not look alike.

## Choosing which chunks, and where to put them

Top-k by similarity answers “which chunks are most like the question”,
which is not quite the question you wanted answered. If three paragraphs
say the same thing, all three score highly and all three go in the
prompt.

``` r

redundant <- paste(c(
  "Revenue was 45.2 million dollars in fiscal 2024.",
  "Total revenue reached 45.2 million dollars in the 2024 fiscal year.",
  "In fiscal 2024 the company recorded revenue of 45.2 million dollars.",
  "Headcount grew to 1,204 employees across nine clinical sites.",
  "The board approved a dividend of 0.42 dollars per share in March."),
  collapse = "\n\n")

ch <- gr_segment(gr_ingest(redundant), list(method = "paragraph", max_tokens = 40))
picked <- function(m) {
  gr_read(ch, "What was revenue?", cl,
          list(reader = "retrieve", top_k = 3, mmr = m))$chunks_used
}
rbind("mmr = 1 (top-k)" = picked(1), "mmr = 0.3" = picked(0.3))
#>                 [,1] [,2] [,3]
#> mmr = 1 (top-k)    1    3    2
#> mmr = 0.3          1    4    5
```

`mmr` below 1 trades relevance against redundancy, so the second and
third slots go to chunks that add something. It costs nothing, since the
vectors are already computed.

`context_order` is a separate decision about *placement*: transformers
attend better to the beginning and end of a long context than to its
middle, so `"edges"` puts the strongest chunk first and the
second-strongest last. It never changes which chunks were selected, only
where they sit.

## Rails, before you spend anything

Two caps are on by default. Both are checked before the reading step
makes its first request and again before every request after it, and the
cost cap is checked against what the run has spent so far. A parallel
read that sends batches, which cannot be stopped part way, is held to
its worst case before it starts:

``` r

unlist(gr_options()[c("max_cost_usd", "max_calls")])
#> max_cost_usd    max_calls 
#>            5          400
```

You can also ask what a run would cost before making it:

``` r

gr_estimate_cost("gpt-4o", input_tokens = 120000, output_tokens = 4000)
#> [1] 0.34
```

And what one did cost afterwards, counting only the calls issued, which
is not the same as the tokens it moved:

``` r

gr_trace_cost(ans$trace)[, c("model", "calls", "paid_calls", "usd")]
#>           model calls paid_calls      usd
#> 1 gpt-5.6-terra     1          1 0.001346
```

## Making a re-run free, and a result checkable

Every stage of this package except the model call is a pure function of
its input. The model call is the only step that costs money, the only
one that can die halfway through a long run, and above a temperature of
zero the only one that does not return the same thing twice. Two
features follow from that.

A **cache** stores each successful response against the exact request,
so a repeat is free and identical:

``` r

cache <- gr_cache(dir = file.path(tempdir(), "readgpt-vignette-cache"))
cached_cl <- gr_cache_client(cl, cache)

first  <- answer_document(readgpt_example(), "What was revenue?", "fast",
                          client = cached_cl)
second <- answer_document(readgpt_example(), "What was revenue?", "fast",
                          client = cached_cl)

gr_trace_summary(second$trace)[, c("calls", "cached")]
#>   calls cached
#> 1     1      1
```

`calls - cached` is what a run paid for. Failures are never cached: a
rate limit is a property of the moment, and storing one would make a
blip permanent.

A **replay** goes further. A trace already holds every prompt and every
response, so with the document it is enough to reproduce the run
exactly, with no key, no network and no spend:

``` r

f <- file.path(tempdir(), "run.json")
gr_trace_save(first$trace, f)

replayed <- answer_document(readgpt_example(), "What was revenue?", "fast",
                            client = gr_replay_client(f))
identical(replayed$answer, first$answer)
#> [1] TRUE
```

That is the difference between a result someone has to trust and one
they can check. A prompt with no recorded response raises
`gr_replay_miss` rather than inventing an answer, because a result that
looks like the original and is not is worse than no replay at all.

Embeddings are not model calls and are not in the trace, so a replay
reproduces chunk *ranking* only when the recording used a deterministic
embedder and the replay uses the same one. That is why this vignette set
`embedder = "lexical"` at the top.

## Many documents

[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
runs several recipes over one document.
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
runs one recipe over many, and gives you a row per document:

``` r

folder <- file.path(tempdir(), "reports")
dir.create(folder, showWarnings = FALSE)
writeLines("Revenue was 45.2 million dollars in fiscal 2024.",
           file.path(folder, "north.txt"))
writeLines("Revenue was 51.8 million dollars in fiscal 2025.",
           file.path(folder, "south.txt"))

out <- gr_read_many(folder, "What was revenue?", "fast", client = cl)
out$summary[, c("document", "not_found", "chunks_used", "calls", "status")]
#>    document not_found chunks_used calls status
#> 1 north.txt     FALSE           1     1     ok
#> 2 south.txt     FALSE           1     1     ok
```

One unreadable file is one `"failed"` row, not a dead run. Each document
gets its own budget, so one enormous file cannot starve the rest. The
run itself therefore needs its own ceiling, and there are two:
`max_total_calls`, checked before each document, and `max_total_usd`,
checked after one. `store =` makes the run resumable.

Passing the same `trace =` to
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md),
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
and
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
accumulates the whole review in one place. It is a *parent*: each stage
still keeps its own trace, which is what `$trace` returns and what
`gr_options(max_calls =)` is measured against, so the screening cannot
spend the write-up’s ceiling and the audit’s per-stage costs still add
up to the total.

## From a folder to a review

The three axes answer a question. A corpus job usually wants a *table*,
and a review wants a table plus the account of it. Four functions cover
that, and they compose in one direction:

[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md)
→
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)
→
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
→
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)

A protocol is what you fix before reading anything: which documents
count, what to collect from the ones that do, and what the write-up has
to cover. Fixing it first is the point: a criterion invented while
reading is a criterion fitted to what was found.

``` r

protocol <- gr_protocol(
  "revenue-review",
  question = "How did revenue change across the regional reports?",
  include  = "Reports a revenue figure",
  exclude  = "Is a forecast rather than a result",
  fields   = gr_fields(
    region  = "The region the report covers",
    revenue = gr_field("Revenue in millions of dollars", type = "number"),
    year    = gr_field("Fiscal year reported", type = "integer")
  ),
  outline  = c("Findings" = "How revenue compares across regions")
)
protocol
#> <gr_protocol 'revenue-review'>
#>   question : How did revenue change across the regional reports?
#>   include  : Reports a revenue figure
#>   exclude  : Is a forecast rather than a result
#>   fields   : region, revenue, year
#>   outline  : Findings
#>   recipe   : research
```

[`gr_protocols()`](https://elkronos.github.io/readgpt/reference/gr_protocols.md)
lists four templates to start from, and
[`gr_protocol_save()`](https://elkronos.github.io/readgpt/reference/gr_protocol_save.md)/[`gr_protocol_read()`](https://elkronos.github.io/readgpt/reference/gr_protocol_save.md)
round-trip one through a JSON file so it can be shared and cited
alongside the results.

Screening is one call per document. Every document gets a decision and a
reason, and nothing is dropped on the way. A file that could not be read
has no decision rather than a silent exclusion, and `"unclear"` is an
answer rather than a forced guess.

``` r

screener <- gr_mock_client(function(messages, params) {
  seen <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                collapse = " ")
  line <- regmatches(seen, regexpr("Revenue was [0-9.]+ million dollars in fiscal [0-9]+\\.", seen))
  sprintf('{"decision":"include","reason":"Reports a revenue figure.",
            "criterion":"Reports a revenue figure","quote":"%s"}', line)
})

screened <- gr_screen(folder, protocol, client = screener)
screened$table[, c("document", "decision", "criterion", "verified")]
#>    document decision                criterion verified
#> 1 north.txt  include Reports a revenue figure     TRUE
#> 2 south.txt  include Reports a revenue figure     TRUE
```

Extraction fills the schema from every chunk of each included document,
then reconciles. The result is one typed row per document, and every
filled cell carries the sentence it came from, checked against the chunk
it was attributed to.

``` r

extractor <- gr_mock_client(function(messages, params) {
  seen <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                collapse = " ")
  n <- if (grepl("45.2", seen, fixed = TRUE)) "45.2" else "51.8"
  yr <- if (grepl("2024", seen, fixed = TRUE)) 2024 else 2025
  rg <- if (grepl("45.2", seen, fixed = TRUE)) "north" else "south"
  line <- sprintf("Revenue was %s million dollars in fiscal %d.", n, yr)
  sprintf('{"region":"%s","revenue":%s,"year":%d,
            "region__quote":"%s","revenue__quote":"%s","year__quote":"%s"}',
          rg, n, yr, line, line, line)
})

table <- gr_extract(screened, protocol, client = extractor, recipe = "fast")
table$table[, c("document", "region", "revenue", "year", "n_unverified")]
#>    document region revenue year n_unverified
#> 1 north.txt  north    45.2 2024            0
#> 2 south.txt  south    51.8 2025            0
```

`n_unverified` is the column to look at before believing a row: zero
means every value in it can be pointed at in the document.
`table$evidence` is the long form: one row per supported cell, with the
quote, the page it is on, and whether the quote appears there.

``` r

table$evidence[, c("document_id", "field", "quote", "verified")]
#>        document_id   field                                            quote
#> 1 f0651bef6287278d  region Revenue was 45.2 million dollars in fiscal 2024.
#> 2 f0651bef6287278d revenue Revenue was 45.2 million dollars in fiscal 2024.
#> 3 f0651bef6287278d    year Revenue was 45.2 million dollars in fiscal 2024.
#> 4 e472ebfef45d9a3c  region Revenue was 51.8 million dollars in fiscal 2025.
#> 5 e472ebfef45d9a3c revenue Revenue was 51.8 million dollars in fiscal 2025.
#> 6 e472ebfef45d9a3c    year Revenue was 51.8 million dollars in fiscal 2025.
#>   verified
#> 1     TRUE
#> 2     TRUE
#> 3     TRUE
#> 4     TRUE
#> 5     TRUE
#> 6     TRUE
```

Finally the write-up, one call per section of the outline, citing the
rows it rests on:

``` r

writer <- gr_mock_client(function(messages, params) {
  "Revenue was higher in the southern region [study 2] than the northern [study 1]."
})

review <- gr_synthesise(table, protocol, client = writer)
review$citations
#>    section study  document      document_id
#> 1 Findings     2 south.txt e472ebfef45d9a3c
#> 2 Findings     1 north.txt f0651bef6287278d
cat(review$text)
#> ## Findings
#> 
#> Revenue was higher in the southern region [study 2] than the northern [study 1].
#> 
#> ## References
#> 
#> 1. (2024).
#> 2. (2025).
```

### Writing from claims instead of from rows

That write-up was drafted from the table, one section at a time, and the
prose shows it: the model walks the rows. The structure shows it too:
the outline was fixed before anything was read, so the shape of the
review is a hypothesis rather than a finding.

[`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
computes the relations between studies first. Each claim names what
supports it, what contradicts it, and which field distinguishes the two:

``` r

claim_writer <- gr_mock_client(function(messages, params) {
  sys <- messages[[1]]$content
  if (grepl("turn a table of studies", sys, fixed = TRUE)) {
    return(paste0('{"claims":[{"claim":"Revenue is higher in the south than the north.",',
                  '"kind":"finding","supported_by":[2],"contradicted_by":[1],',
                  '"moderator":"region","scope":"one report per region"},',
                  '{"claim":"The northern region reported for fiscal 2024.",',
                  '"kind":"finding","supported_by":[1],"contradicted_by":[],',
                  '"moderator":null,"scope":"one report"}]}'))
  }
  if (grepl("sections of a review", sys, fixed = TRUE)) {
    return(paste0('{"sections":[{"heading":"Where revenue is higher","brief":"the ',
                  'regional comparison","claims":[1,2],"rationale":"one comparison"}]}'))
  }
  "Revenue was higher in the south [study 2] than in the north [study 1]."
})

cm <- gr_claims(table, question = protocol$question, client = claim_writer)
cm$claims[, c("claim", "moderator", "n_support", "n_contradict")]
#>                                            claim moderator n_support
#> 1 Revenue is higher in the south than the north.    region         1
#> 2  The northern region reported for fiscal 2024.      <NA>         1
#>   n_contradict
#> 1            1
#> 2            0
```

Every study number in that table was checked against the extraction
before it got there. A number that is not in the table is dropped, a
claim left with no supporting study is dropped entirely, and a
`moderator` naming a column that does not exist is cleared. `cm$dropped`
says what went and why, so a thin claims table can be told apart from a
thin literature.

[`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
then derives the sections from the claims, and
[`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md)
computes what the corpus does not contain. It makes no model call, so
the gap list is something you can check by counting:

``` r

outline <- gr_outline(cm, client = claim_writer)
names(outline)
#> [1] "Where revenue is higher" "What is missing"
gr_gaps(cm, extraction = table)[, c("kind", "dimension", "detail")]
#>           kind dimension                                        detail
#> 1 unreplicated   claim 2 The northern region reported for fiscal 2024.
```

``` r

from_claims <- gr_synthesise(table, outline = outline, question = protocol$question,
                             client = claim_writer, claims = cm,
                             gaps = gr_gaps(cm, extraction = table))
from_claims$sections[, c("section", "n_claims", "claims_missed", "partial")]
#>                   section n_claims claims_missed partial
#> 1 Where revenue is higher        2             0   FALSE
#> 2         What is missing        0             0   FALSE
```

Each section now argues its own claims and is shown only the studies
those claims rest on. `claims_missed` is the citation check run
backwards: a section handed a claim and not writing it up did not do
what the outline promised, and says so.

[`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md)
writes the whole chain out as one self-contained HTML file for somebody
who did not run it:

``` r

gr_audit_report("audit.html", screening = screened, extraction = table,
                synthesis = review, protocol = protocol)
```

Every `[study n]` is parsed back out and checked against the rows that
exist; one pointing at a row that is not there is reported and marks the
section partial. That completes the chain: a sentence cites a study, the
study’s row cites a quote, and the quote was checked against the page it
is attributed to. None of it proves the sentence is true. It makes every
step of the way back to the document short enough to walk.

## Using a real model

Everything above used a mock. In production you need a client:

``` r

# An OpenAI-compatible endpoint:
cl <- gr_client(model = "gpt-4o")          # key from OPENAI_API_KEY

# Or any provider ellmer speaks to, such as Anthropic, Google, Bedrock or Ollama:
cl <- gr_ellmer_client(ellmer::chat_anthropic())

# Or anything at all:
cl <- gr_backend_client(function(messages, params) my_provider(messages))
```

Behind a company gateway, `base_url` is usually not enough on its own.
It is when the gateway speaks the OpenAI shape and takes
`Authorization: Bearer`; most do not. Azure OpenAI authenticates with
`api-key`, API Management adds a subscription key, and many require a
cost-centre or correlation id. `headers` covers those. Naming any header
makes the API key optional, so a gateway with its own scheme needs no
`OPENAI_API_KEY` set at all:

``` r

cl <- gr_client(
  base_url = "https://gateway.example.com/openai/v1", api = "chat",
  headers  = c("api-key" = Sys.getenv("GATEWAY_KEY"),
               "X-Cost-Centre" = "1234",
               # NA suppresses a header rather than sending it. Without this,
               # an OPENAI_API_KEY left set for another client in the same
               # session would be sent to the gateway as well.
               Authorization = NA))
```

`gr_options(api_headers = ...)` sets the same thing once for every
client, which is the form that belongs in a project’s `.Rprofile`.
Headers are excluded from the cache key, like the API key: a rotating
token or a per-request correlation id would otherwise make every lookup
a miss.

Without a key, a run stops before it reads the document, with an error
(`gr_auth_error`) that says how to set one. A client that authenticates
through `headers` needs no key.

## Extending it

Every axis is a registry, and an addition behaves exactly like a
built-in. It gets the same token caps, provenance and reporting, and it
can be named in a recipe or put in an `ensemble`.

``` r

gr_register_segmenter("by_bullet", description = "one chunk per bullet",
  fn = function(doc, spec, client, trace) {
    units <- unlist(strsplit(doc$text, "\n(?=[-*])", perl = TRUE))
    new_chunks(units, "by_bullet", spec)
  })

"by_bullet" %in% gr_segmenters()$name
#> [1] TRUE
```

[`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
[`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md),
[`gr_register_embedder()`](https://elkronos.github.io/readgpt/reference/gr_register_embedder.md)
and
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md)
work the same way. Each has a worked example in its help page.
