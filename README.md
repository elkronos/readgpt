# readgpt

[![R-CMD-check](https://github.com/elkronos/readgpt/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/elkronos/readgpt/actions/workflows/R-CMD-check.yaml)
[![tests](https://github.com/elkronos/readgpt/actions/workflows/tests.yaml/badge.svg)](https://github.com/elkronos/readgpt/actions/workflows/tests.yaml)

Fine-grained control over how a language model ingests, segments, and reads a
document.

The premise is that *how* you break a document up and *how* you make the model
read it are separate decisions worth experimenting with. This package makes them
separate in the code. Three independent axes, each a registry you can extend
without editing the package:

```
  source  ──▶  INGEST  ──▶  SEGMENT  ──▶  READ  ──▶  answer + trace
               │             │             │
               │             │             └─ 11 strategies, each with a
               │             │                distinct traversal signature
               │             └─ 9 segmenters + overlap / min-size / context
               └─ 6 extractors + 14 individually toggleable cleaners
```

Any ingest × any segmenter × any reader composes. `gr_recipe()` binds one of
each into a named pipeline. Recipes are isolated: a recipe run alongside others
produces a byte-identical `$answer` and `$chunks_used` to running it alone.

Which model answers is a fourth, independent choice. There is a built-in client
for OpenAI-compatible endpoints, and `gr_ellmer_client()` hands the transport to
[ellmer](https://ellmer.tidyverse.org/) — so Anthropic, Google, Bedrock, Azure,
Ollama and Hugging Face all work with every strategy below.

`vignette("readgpt")` is the guided tour: the three axes, what each decision
changes, and how to make a run cheap and reproducible. It builds and runs
offline, so you can follow it without a key.

Every console block below is real output from the bundled example document,
produced with `gr_mock_client()` standing in for the API, so you can reproduce
all of it without a key. Blocks that show an answer set
`cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")` and
pass `client = cl`.

---

## Requirements and install

Requires **R ≥ 4.1**. The only non-base hard dependencies are `digest`, `httr`
and `jsonlite`.

```r
# install.packages("remotes")
remotes::install_github("elkronos/readgpt")
```

Or from a local checkout:

```r
remotes::install_local("path/to/readgpt")
```

Optional, each checked at the point of use:

| package | needed for |
|---|---|
| `pdftools` | PDF text extraction |
| `tesseract` + `magick` | OCR of scanned pages and images |
| `xml2` | HTML **and DOCX** extraction |
| `future` + `future.apply` | `parallel = TRUE` (without them it warns and runs sequentially) |
| `shiny` | the bundled app |
| `reticulate` | the exact `tiktoken` tokenizer |
| `readtext` | DOCX fallback when the `xml2` path yields nothing |

Missing `pdftools`/`xml2`/`tesseract` at extraction time raises a clear error
naming the package. Missing OCR support mid-PDF only *warns* and returns those
pages empty.

## API key

```r
Sys.setenv(OPENAI_API_KEY = "sk-...")   # or
options(readgpt.api_key = "sk-...")     # or
cl <- gr_client(api_key = "sk-...")     # per-client, for multi-user Shiny
```

Resolution order is explicit argument → `readgpt.api_key` option →
`OPENAI_API_KEY`.

**Without a key nothing raises.** Every model call fails, you get
`"NOT_IN_DOCUMENT"` back, and the failure is reported on the answer object:

```r
ans$partial        # TRUE
ans$notes$error    # "No API key available."
```

Always check `ans$partial` before trusting an answer. Call `gr_api_key()`
yourself if you would rather fail fast.

Keep the key out of the repository. `.Renviron` and `.Rprofile` are both
gitignored here for that reason — `.Renviron` is the usual home for it:

```
OPENAI_API_KEY=sk-...
```

## Other providers, and other people's clients

The built-in client speaks one dialect: an OpenAI-compatible `/responses` or
`/chat/completions` endpoint. Nothing about ingesting, segmenting or reading a
document depends on that, so the transport is swappable.

`gr_ellmer_client()` uses an [ellmer](https://ellmer.tidyverse.org/) chat, which
covers roughly twenty providers including local models through Ollama:

```r
library(ellmer)

cl <- gr_ellmer_client(chat_anthropic(model = "claude-sonnet-4-5"))
answer_document("report.pdf", "What was revenue?", "thorough", client = cl)

# Or locally, for nothing:
gr_ellmer_client(chat_ollama(model = "llama3.1"))
```

Two things do not carry over, both by ellmer's design rather than by omission.
Sampling parameters belong to the chat object, so a `temperature` in a read spec
is ignored and warned about once — build a second chat if you need a second
temperature. And embeddings are separate: pass `embed =` a function returning one
row per text (a wrapper around `ragnar::embed_ollama()`, say), or `retrieve` and
the `semantic` segmenter fall back to lexical vectors and tell you so.

Anything else — a company proxy, a model behind a queue, a package this one has
never heard of — goes through `gr_backend_client()`, which makes any function
the transport:

```r
cl <- gr_backend_client(function(messages, params) {
  # `messages` is a list of list(role=, content=); `params` carries model,
  # max_output, temperature, schema. Return a string.
  my_provider(messages, max_tokens = params$max_output)
}, model = "my-model")
```

Everything the package does around the call is unchanged: context budgeting,
the cost and call rails, provenance, the run trace, caching, replay and
`gr_compare()`. Register the model's real limits with `gr_register_model()` —
the context window is what sizes your chunks, so a guessed one is not cosmetic.

If you are already using ellmer and ragnar, the division is: ragnar retrieves,
this package reads. `ragnar_retrieve()` gets you relevant chunks; `map_reduce`,
`refine`, `hierarchical`, `iterative`, `rerank` and `ensemble` are what happen
after that, with a traversal signature each and a bill you can see.

## Quick start

```r
library(readgpt)

ans <- answer_document("report.pdf", "What was Q3 revenue?", recipe = "needle")
ans$answer
ans$partial
```

`answer_document()` treats its first argument as a path when the file exists.
A string that *looks* like a path but does not exist is an error, not a
document — so a typo cannot silently become the text you ask questions about.

Not sure which pipeline suits your document? Compare, then commit. One
extraction is shared across all of them:

```r
cl  <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
cmp <- gr_compare(readgpt_example(), "What was revenue in fiscal 2024?",
                  c("fast", "needle", "thorough", "survey"), client = cl)
cmp$summary
#>     recipe  segmenter chunks       reader         signature partial chunks_used
#> 1     fast  paragraph      1        stuff        all|1|none   FALSE           1
#> 2   needle   semantic      2     retrieve       topk|1|none   FALSE           2
#> 3 thorough  paragraph      1   map_reduce   all|N+logN|tree   FALSE           1
#> 4   survey structural      6 hierarchical all|N+tree+1|tree   FALSE           6
#>   answer_chars not_found error
#> 1           33     FALSE  <NA>
#> ...
```

(The bundled example is deliberately small — 573 tokens by `gr_count_tokens()`
— so `fast` and `thorough` fit it in one chunk. On a real report they would not.)

Build a pipeline by hand:

```r
rec <- gr_recipe("my_pipeline",
  ingest  = list(clean = c("page_numbers", "hyphenation", "headers_footers")),
  segment = list(method = "structural", max_tokens = 800, overlap_tokens = 80),
  read    = list(reader = "skim", cite = TRUE, model = "gpt-5.6-terra"))

ans <- answer_document("thesis.pdf", "How was the sample recruited?", rec)
```

## Axis 1 — ingest

`gr_ingest()` turns bytes into cleaned text blocks that keep page and section
provenance.

Cleaning is a pipeline of named steps, each individually toggleable. Steps are
always applied `early` stage first, whatever order you list them in — that is
what stops digit removal from running before the page-number and figure filters
that need digits to match.

```r
gr_cleaners()[, c("name", "stage", "default_on")]
#>                   name stage default_on
#> 1             captions early      FALSE
#> 2               emails early      FALSE
#> 3      headers_footers early      FALSE
#> 4          hyphenation early       TRUE
#> 5         page_numbers early       TRUE
#> 6           references early      FALSE
#> 7                 urls early      FALSE
#> 8           ascii_only  late      FALSE
#> 9  collapse_whitespace  late       TRUE
#> 10       control_chars  late       TRUE
#> 11           ligatures  late       TRUE
#> 12           lowercase  late      FALSE
#> 13      remove_numbers  late      FALSE
#> 14  remove_punctuation  late      FALSE
```

The five `default_on` steps are exactly the `"standard"` preset. Note what is
**off**: `remove_numbers` (which makes every figure, date and percentage
unanswerable), `captions` (which destroys table-heavy documents), and `urls`
(URLs are often the answer). Presets: `none`, `minimal`, `standard` (default),
`academic`, `scan`, `legacy`.

`doc$stats$clean_log` reports characters removed per step, so you can see when
cleaning ate more than you expected.

## Axis 2 — segment

`gr_segment()` turns a document into chunks. Nine strategies, each a different
hypothesis about where meaning breaks:

| segmenter | boundary hypothesis | cost | needs client | provenance |
|---|---|---|---|---|
| `fixed` | meaning is uniform; cut on a ruler (the control condition) | free | no | none |
| `paragraph` | the author's paragraph breaks are real | free | no | page, section, block |
| `sentence` | sentences are atomic; pack tightly | free | no | page, section, block |
| `recursive` | use the strongest separator that still fits | free | no | none |
| `structural` | headings are boundaries; never merge across sections | free | no | page, section, block |
| `page` | the page is the unit — forms, invoices, scanned records | free | no | page, section, block |
| `semantic` | cut where consecutive embeddings diverge most | 1 embedding pass | yes | page, section, block |
| `contextual` | chunks prefixed with where they sit | free, or 1 call/chunk | only for `context_source = "llm"` | page, section, block |
| `proposition` | rewrite into standalone factual statements | 1 call per ~900-token batch | yes | none |

This table is generated from the registry, so you can check it rather than
trust it: `gr_segmenters()`.

`fixed` and `recursive` work on the concatenated document by design, so their
chunks carry no page or section — that is the cost of ignoring structure, and it
is reported as `NA` rather than guessed at.

**Degradation is explicit.** `page` on a source with no page provenance falls
back to `paragraph`; `semantic`, `proposition` and `contextual(context_source =
"llm")` need a client and fall back without one. Every fallback warns, and the
downgrade is recorded: in `$method` for `page`, `semantic` and `proposition`
(e.g. `"semantic->paragraph"`), and in `$extra$context_source` for `contextual`,
which keeps its method name because only its blurb source changed.

Orthogonal to the method: `max_tokens` (always enforced, on every segmenter),
plus `overlap_tokens` and `min_tokens` — honoured everywhere except `page`
(a page is the unit), `proposition` (overlap forced to 0) and `structural`,
where `min_tokens` applies only *within* a section, because that segmenter never
merges across section boundaries.

Compare chunkings for free, before spending anything on reading:

```r
doc <- gr_ingest(readgpt_example())
do.call(rbind, lapply(c("fixed", "paragraph", "sentence", "structural"),
  function(m) gr_chunk_stats(gr_segment(doc, list(method = m, max_tokens = 120)))))
#>       method n total_tokens min median  mean max over_cap
#> 1      fixed 5          528  49  120.0 105.6 120        0
#> 2  paragraph 6          532  47   90.0  88.7 116        0
#> 3   sentence 6          532  47   92.5  88.7 106        0
#> 4 structural 8          562  31   75.0  70.2 101        0
```

`semantic` needs a client for its embedding pass, so pass one (a
`gr_mock_client()` is fine for a dry run) or it falls back.

What overlap actually costs, in duplicated tokens:

```r
doc <- gr_ingest(readgpt_example())
do.call(rbind, lapply(c(0, 30, 60), function(ov)
  gr_chunk_stats(gr_segment(doc, list(method = "sentence", max_tokens = 120,
                                      overlap_tokens = ov)))))
#>     method n total_tokens min median mean max over_cap
#> 1 sentence 6          532  47   92.5 88.7 106        0
#> 2 sentence 7          666  74   99.0 95.1 106        0
#> 3 sentence 9          860  79   94.0 95.6 109        0
```

## Axis 3 — read

`gr_read()` answers the question. Eleven strategies, each with a **traversal
signature** — `select|calls|state` — which is how the package tells two
methodologies apart from two names for the same thing:

| reader | signature | calls | what makes it different |
|---|---|---|---|
| `stuff` | `all\|1\|none` | 1 | one prompt; **truncates with a warning** if the document does not fit (`on_overflow = "error"` to make that fatal) |
| `map_reduce` | `all\|N+logN\|tree` | N + merges | independent per-chunk answers, tree-reduced; parallel, order-free |
| `refine` | `all\|N\|forward` | N | sequential draft-and-revise; order matters, late evidence can overturn early |
| `skim` | `all\|N+1\|none` | N + 1 | per-chunk **evidence** extraction, then one synthesis from the verbatim text |
| `retrieve` | `topk\|1\|none` | 1 + embeddings | embed, rank, answer from top-k; one answer call regardless of length, though the embedding pass still scales |
| `rerank` | `topk\|m+1\|none` | m + 1 | BM25 prefilter, model scores candidates, answer from the winners |
| `hierarchical` | `all\|N+tree+1\|tree` | N + levels + 1 | recursively summarise until the summaries fit, then answer |
| `iterative` | `topk\|rounds*2\|forward` | ≤ 2 × rounds | agentic: the model names what it still needs, driving the next retrieval |
| `extract` | `all\|N+conflicts\|none` | N + one per disagreeing field | fills a typed schema from every chunk, then reconciles; a call only where the document contradicts itself |
| `screen` | `head\|1\|none` | 1 | one decision about the whole document, from its opening; include / exclude / unclear with a reason |
| `ensemble` | `ensemble\|sum+1\|none` | Σ members + 1 | several distinct readers, adjudicated; members must have different signatures |

`rerank` and `iterative` need JSON-schema structured output. Against an endpoint
without it they degrade — to BM25 ranking and to single-shot retrieve
respectively — with a warning and a note on the answer.

`gr_compare()` refuses to bill you twice for two configurations that resolve to
the same segmentation and the same signature.

## Choosing chunks, and where to put them

Two settings on the read spec, both off by default because changing what reaches
the model changes answers and that should be a decision rather than a surprise.

**`mmr` — stop paying for the same chunk three times.** Top-k by similarity
answers "which chunks are most like the question", which is not quite the
question you wanted. If three paragraphs say the same thing, all three score
highly and all three go in the prompt. Maximal marginal relevance picks greedily,
trading relevance against redundancy against what is already selected:

```r
cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
old <- gr_options(embedder = "lexical")
doc <- paste(c("Revenue was 45.2 million dollars in fiscal 2024.",
               "Total revenue reached 45.2 million dollars in the 2024 fiscal year.",
               "In fiscal 2024 the company recorded revenue of 45.2 million dollars.",
               "Headcount grew to 1,204 employees across nine clinical sites.",
               "The board approved a dividend of 0.42 dollars per share in March."),
             collapse = "\n\n")
ch <- gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 40))
picked <- function(m) gr_read(ch, "What was revenue?", cl,
                              list(reader = "retrieve", top_k = 3, mmr = m))$chunks_used
result <- rbind("mmr = 1 (top-k)" = picked(1), "mmr = 0.3" = picked(0.3))
gr_options(old)
result
#>                 [,1] [,2] [,3]
#> mmr = 1 (top-k)    1    3    2
#> mmr = 0.3          1    4    5
```

Top-k spends all three slots on the same fact. `mmr = 0.3` keeps the best chunk
and spends the other two on different ones. It costs nothing — the vectors are
already computed — and it applies to `retrieve` and `iterative`.

**`context_order` — where the chosen chunks sit.** Transformers attend
measurably better to the beginning and end of a long context than to its middle.
`"edges"` puts the strongest chunk first and the second-strongest last, burying
the weakest in the middle; `"document"` restores the order they appear in the
document, which reads better when chunks are consecutive. Selection is
unaffected — this decides placement only, for `retrieve` and `rerank`, the two
readers that put several ranked chunks in one prompt.

Note this is *not* the primacy-and-recency effect it resembles. Those come from
rehearsal and interference in human memory, mechanisms a transformer does not
have; the reason here is positional attention, and it argues about placement
rather than about what to select.

## Reading a run

Nothing degrades silently. Everything below is recorded on the answer.

```r
ans$partial     # TRUE means something degraded — check this first
ans$notes       # what: dropped_chunks, failed_calls, error, degraded_to_bm25, ...
ans$evidence    # what the answer rests on
print(ans$trace)
gr_trace_summary(ans$trace)
#>                          run_id calls cached steps tokens_in tokens_out errors
#> 1 run_20260904035101.469_68d50e     1      0     8       669         13      0
#>   elapsed_s
#> 1       0.3

gr_estimate_cost("gpt-4o", ans$trace$tokens_in, ans$trace$tokens_out)
as_json(ans)    # answer plus every prompt and response, from the same single run
```

`ans$evidence` is a data frame with `chunk_id`, `text`, `page`, `section`,
`score`, `kind`. **What `text` holds depends on the reader**: verbatim chunk text for
`stuff`, `retrieve`, `rerank` and `iterative`; model-*extracted* passages for
`skim`; per-chunk model *answers* for `map_reduce`. `refine` and `hierarchical`
return `NULL`. `page` is populated only for PDF sources. `score` is set only by
`retrieve` (cosine) and `rerank` (0–10, model-judged).

With `cite = TRUE` the model cites bracketed chunk ids (`[chunk 3]`); map those
back to pages through `ans$evidence`.

**Quoted evidence is checked.** For most readers the evidence is verbatim chunk
text and is true by construction. For `skim` it is what the model wrote when
asked to extract the relevant passages — presented as a quotation, and nothing
used to check that it was one. A fabricated citation is more convincing than a
fabricated answer, because it looks like the thing that would let you check.

```r
gr_verify_evidence(ans)          # chunk_id, kind, verified, match, span
```

`match` is 1 for an exact quotation once whitespace, quote marks, dashes and case
are folded away — the differences a faithful quotation introduces. Below 1 it is
the fraction of the span carried by its longest consecutive **run** in the
source, so *where* a change falls matters as much as how much changed: a changed
last word leaves a run of nine in ten and scores 0.9, while a changed word in the
middle splits the span and scores about 0.5. A swapped figure mid-sentence — the
case this exists to catch — lands near 0.5. Below about 0.3 there is no quotation
left, only shared vocabulary. A span that does not verify sets `ans$notes$unverified_evidence`
and makes the answer `partial`.

Citations are checked the same way, for every reader: an answer citing a chunk
that was never sent sets `ans$notes$cited_unknown`. Both checks are local string
operations on text you already have, so they cost nothing and always run.

Errors are classed, so you can catch a specific failure: `gr_auth_error`,
`gr_file_not_found`, `gr_empty_document`, `gr_unsupported_format`, `gr_overflow`,
`gr_call_cap`, `gr_cost_cap`, `gr_budget_error`, `gr_unknown_model`,
`gr_unknown_override`, `gr_no_recipes`, `gr_bad_ensemble`, `gr_missing_dep`.
Degradations are classed *warnings*: `gr_segment_fallback`, `gr_embed_fallback`,
`gr_rerank_degraded`, `gr_iterative_degraded`, `gr_ensemble_degenerate`,
`gr_duplicate_recipe`, `gr_deprecated`, `gr_clamped`, `gr_ocr_unavailable`.

`is_not_found(ans$answer)` tests the "the document does not contain this"
sentinel. Use it rather than `grepl("NOT_IN_DOCUMENT", ...)`: a real answer can
quote the sentinel, and models decorate it (`**NOT_IN_DOCUMENT.**`).

## When the answer is not what you expected

The trace is there so you never have to guess. In order of how often it is the
cause:

| symptom | check | likely cause |
|---|---|---|
| `NOT_IN_DOCUMENT`, but you can see the answer in the file | `nrow(ans$evidence)`, then `gr_chunk_stats()` | the chunk holding it never reached the model. Lower `max_tokens`, raise `top_k`, or switch to a reader whose `signature` starts `all\|` |
| the answer is right but thin | `ans$notes$chunks` vs `length(ans$chunks_used)` | most chunks answered `NOT_IN_DOCUMENT`. That is usually correct; if not, the boundaries are cutting the evidence in half — add `overlap_tokens` |
| `ans$partial` is `TRUE` | `ans$notes`, then `print(ans$trace)` | `failed_calls` (transport), `dropped_chunks` (did not fit), `call_cap_reached`, or a merge that degraded to concatenation |
| `ans$notes$unverified_evidence` is set | `gr_verify_evidence(ans)` | the model wrote a quotation that is not in the chunk it is attributed to. `match` says how far off; near 1 is a typo, near 0 is invention |
| `ans$notes$cited_unknown` is set | that value against `ans$chunks_used` | the answer cited a chunk that was never sent to it |
| figures, dates or percentages are missing | `doc$stats$clean_log` | a cleaning step removed them. `remove_numbers` is off by default; the `legacy` preset turns it on deliberately |
| a scanned PDF comes back nearly empty | `doc$stats$chars` per page, and any `gr_ocr_unavailable` warning | OCR did not run or is not installed. Force it with `gr_ingest_spec(ocr = "always")`, and check `tesseract` and `magick` are present |
| every chunk is the whole document | `nrow(doc$blocks)` | the file has no blank lines between paragraphs, so there is nothing to split on. Use `method = "sentence"` or `"fixed"` |
| the run costs far more than expected | `gr_readers()$cost_calls` | the reader is `O(N)` in chunks and your `max_tokens` is small. `gr_chunk_stats()` first; it is free |
| answers change between identical runs | `gr_read_spec()$temperature` | set `temperature = 0`, and note that reasoning models ignore it — `gr_model_info(m)$supports_temperature` |

Two habits make all of this cheaper: run `gr_chunk_stats()` before you spend
anything, and keep `gr_options(max_cost_usd = ...)` set to something you would
not mind paying by accident.

## Recipes

```r
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

| your document | start with |
|---|---|
| fits in one context window | `fast` |
| one fact buried in a long report | `needle` |
| needs every mention found | `thorough` |
| long, with headings, needs a synthesis | `survey` |
| scanned PDF, forms, invoices | `scanned` |
| an argument that develops across the text | `narrative` |
| multi-hop question over a paper | `research` |
| high stakes, want cross-checking | `consensus` |
| short, and you want every sentence weighed | `precise` |

`answer_document()` defaults to `"thorough"` — `map_reduce`, so its cost scales
with chunk count. Use `"fast"` or `"needle"` when that matters.

`legacy` deliberately reproduces the previous release's behaviour — digit
stripping, 3000-token paragraph chunks, no overlap — so you can measure the
difference rather than assume it.

## Cost and safety rails

Two rails are **on by default** and checked before the first request:

```r
gr_options(max_cost_usd = 5,     # refuse a run whose pre-flight estimate exceeds this
           max_calls    = 400)   # hard cap on model calls per run
```

`max_calls` is re-checked before every subsequent call, so a run that hits it
mid-flight returns a `partial` answer with `notes$call_cap_reached` rather than
continuing to spend. Both raise a classed error naming the option to change.

`gr_budget()` is the single arithmetic chokepoint for context math and is
incapable of returning a non-positive input budget — it raises an actionable
error instead. `gr_options()` documents all 21 settings; see `?gr_options`.

## Many documents

`gr_compare()` runs several recipes over one document. `gr_read_many()` runs one
recipe over many, and returns one tidy row per document — which is the shape the
work usually has: a folder, one question, and a table at the end.

```r
cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
reports <- file.path(tempdir(), "reports")
dir.create(reports, showWarnings = FALSE)
writeLines("Revenue was 45.2 million dollars in fiscal 2024.", file.path(reports, "north.txt"))
writeLines("Revenue was 51.8 million dollars in fiscal 2025.", file.path(reports, "south.txt"))

out <- gr_read_many(reports, "What was revenue?", "fast", client = cl)
out$summary[, c("document", "not_found", "chunks_used", "calls", "status")]
#>    document not_found chunks_used calls status
#> 1 north.txt     FALSE           1     1     ok
#> 2 south.txt     FALSE           1     1     ok
```

A directory is expanded by the extractor registry, so registering an extractor
changes which files get picked up. The full summary also carries `answer`,
`partial`, `reader`, `chunks`, `cached`, `tokens_in`, `tokens_out`, `cost_usd`,
`seconds` and `error` — `write.csv(out$summary, ...)` is a reasonable end to a
run.

Four things it does that a `lapply()` does not:

**One bad file costs one row.** An unreadable document gets `status = "failed"`
and its error in the `error` column; the other hundred and ninety-nine answers
survive. `on_error = "stop"` if you would rather it aborted.

**Budgets are per document.** Every document gets its own trace, so
`gr_options(max_calls =)` applies to each one exactly as if you had read it
alone — one enormous document cannot starve the rest. `max_total_usd` is the
corpus-wide ceiling; documents after it are marked `"skipped"` rather than
quietly dropped.

**A run can be resumed.** Point `store =` at a directory and each result is
written as it completes and restored on a later run. Combined with a durable
response cache, restarting a four-hour job costs approximately nothing:

```r
cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
durable <- gr_cache_client(cl, gr_cache(tools::R_user_dir("readgpt", "cache")))
gr_read_many(file.path(tempdir(), "reports"), "What was revenue?", "thorough",
             client = durable, store = file.path(tempdir(), "run-store"))
```

The store is keyed on the document's path, size and mtime, the question, the
whole pipeline and the model — so an edited document is a new job, not a stale
hit. A `gr_backend_client()` needs a stable `id` for a store or a cache to be
reused by a *later session*; `gr_client()` and `gr_ellmer_client()` already know
what they are.

**You can see what it cost.** `gr_trace_cost()` prices a run using each step's
own model and counts only the calls that were really issued:

```r
cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
run <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl)
gr_trace_cost(run$trace)
#>           model calls paid_calls paid_in paid_out      usd
#> 1 gpt-5.6-terra     1          1     595       13 0.001346
```

This is why the token totals on `gr_trace_summary()` are not a bill. They say
how large the prompts were, which is the right measure of a run's shape; a fully
cached re-run has the same shape and costs nothing. `paid_calls` is the column
that falls to zero. A model with no registered price contributes `NA` rather
than zero, so a total cannot quietly omit it.

## From a folder to a review

The three axes answer a question. A corpus job usually wants a *table*, and a
review wants a table plus the account of it. Four functions cover that:

`gr_protocol()` → `gr_screen()` → `gr_extract()` → `gr_synthesise()`

A protocol is what you fix **before** reading anything: which documents count,
what to collect from the ones that do, and what the write-up has to cover. That
is the point of it — a criterion invented while reading is a criterion fitted to
what was found. `gr_protocols()` lists three templates to start from, and
`gr_protocol_save()` round-trips one through a JSON file so it can be shared,
diffed and cited alongside the results.

```r
protocol <- gr_protocol(
  "revenue-review",
  question = "How did revenue change across the regional reports?",
  include  = "Reports a revenue figure",
  fields   = gr_fields(
    region  = "The region the report covers",
    revenue = gr_field("Revenue in millions of dollars", type = "number")
  ),
  outline  = c("Findings" = "How revenue compares across regions")
)

# One mock standing in for three stages, so the example runs offline.
reply <- function() gr_mock_client(function(messages, params) {
  seen <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                collapse = " ")
  line <- regmatches(seen, regexpr("Revenue was [0-9.]+ million[^.]*\\.", seen))
  if (grepl("<studies>", seen, fixed = TRUE))
    return("The southern region reported more [study 2] than the northern [study 1].")
  if (grepl("screen", seen))
    return(sprintf('{"decision":"include","reason":"Reports revenue.","criterion":"Reports a revenue figure","quote":"%s"}', line))
  sprintf('{"region":"%s","revenue":%s,"region__quote":"%s","revenue__quote":"%s"}',
          if (grepl("45.2", seen)) "north" else "south",
          if (grepl("45.2", seen)) "45.2" else "51.8", line, line)
})

reports  <- file.path(tempdir(), "reports")
screened <- gr_screen(reports, protocol, client = reply())
extracted <- gr_extract(screened$included, protocol, client = reply(), recipe = "fast")
review   <- gr_synthesise(extracted, protocol, client = reply())

extracted$table[, c("document", "region", "revenue", "n_unverified")]
#>    document region revenue n_unverified
#> 1 north.txt  north    45.2            0
#> 2 south.txt  south    51.8            0
```

**Screening is one call per document, and nothing is dropped.** Every document
gets a decision and a reason. There is no retrieval step that could quietly
remove a source before one is recorded; a file that could not be read gets
`status = "failed"` and *no* decision rather than a silent exclusion; and
`"unclear"` is an answer rather than a forced guess, because forcing a binary
call on an excerpt that does not settle it is how automated screening loses
studies. `screened$included` is the argument to hand to `gr_extract()`, and
`table(screened$table$criterion)` is the breakdown a flow diagram asks for.

**Extraction gives you a typed table, not prose.** A paragraph about one paper
cannot be compared with a paragraph about two hundred others; a table can be
sorted, counted, filtered and published. Each field is filled from every chunk
and then reconciled — free where the chunks agree, one call where the document
contradicts itself, and `conflicts` records that it did.

`n_unverified` is the column to look at before believing a row: zero means every
value in it can be pointed at in the document. `extracted$evidence` is the long
form, one row per supported cell, carrying the quote, the page it is on, and
whether the quote really appears there. Nothing is discarded for failing that
check — `require_quote = TRUE` makes discarding a choice rather than a surprise.

**The write-up is one call per section of the outline**, and every section cites
the rows it rests on:

```
## Findings

The southern region reported more [study 2] than the northern [study 1].
```

`review$citations` resolves each `[study n]` to its `document` and
`document_id`. The markers are parsed back out and checked against the rows that
exist; one pointing at a row that is not there is reported and marks the section
partial. Duplicates never reach the write-up — a study counted twice is the
error this whole path exists to avoid.

That completes the chain: a sentence cites a study, the study's row cites a
quote, and the quote was checked against the page it is attributed to. None of
that proves the sentence is true. It makes every step of the way back to the
document short enough to walk.

## Paying once

Every stage of this package except one is a pure function of its input.
Extraction, cleaning, segmentation, ranking and merging give the same result
every time, and `gr_chunk_stats()` lets you compare chunkings for nothing. The
model call is the only step that costs money, the only step that can die halfway
through a long run, and — above a temperature of zero — the only step that does
not return the same thing twice.

`gr_cache()` stores each successful response, keyed on the exact request. A
repeat is free and *identical*:

```r
cache  <- gr_cache(file.path(tempdir(), "readgpt-cache"))
cached <- gr_cache_client(cl, cache)

first  <- answer_document(readgpt_example(), "What was revenue?", "thorough", client = cached)
second <- answer_document(readgpt_example(), "What was revenue?", "thorough", client = cached)

gr_trace_summary(second$trace)[c("calls", "cached", "tokens_in")]
#>   calls cached tokens_in
#> 1     1      1       595
```

`cached` counts the calls answered from the cache rather than the network, so
`calls - cached` is what the run actually paid for. The token counts stay — that
is how big the prompts were — but a run with `cached == calls` cost nothing, so
do not hand its totals to `gr_estimate_cost()` and call the result a bill.

The key covers everything that can change the reply: the messages, the model,
the resolved output cap, the temperature, the JSON schema, the API shape and the
base URL. It does **not** cover the key, the timeout or the retry policy, none
of which the model sees. **Failures are never cached** — a rate limit or a
refusal is a property of the moment, and storing one would make a blip
permanent. At a temperature above zero a hit replays one sample instead of
drawing a new one, which is the point, but it means a cached sweep does not
explore; use a fresh directory when you want new draws.

The default directory is under `tempdir()`, so a cache costs nothing and
vanishes with the session. For a long run, point it somewhere real and the run
becomes resumable — restart after a crash and every completed call is already
paid for:

```r
gr_options(cache_dir = tools::R_user_dir("readgpt", "cache"))
```

## Replaying a run

A trace already records every prompt and every response. That makes it a
complete transcript of the only non-deterministic part of the pipeline — so a
trace plus the source document is enough to reproduce a run exactly, with no key
and no spend:

```r
run <- answer_document(readgpt_example(), "What was revenue?", "thorough", client = cl)
gr_trace_save(run$trace, file.path(tempdir(), "run.json"))

replayed <- answer_document(readgpt_example(), "What was revenue?", "thorough",
                            client = gr_replay_client(file.path(tempdir(), "run.json")))
identical(replayed$answer, run$answer)
#> [1] TRUE
```

This is the difference between a result someone has to trust and one they can
check. Ship the trace next to the paper and a reader reproduces the run instead
of paying to approximate it. It is also the cheapest bug report there is: a
trace file is a re-runnable recording of exactly what went wrong.

A prompt with no recorded response raises `gr_replay_miss` rather than inventing
an answer — a miss means the replay has diverged from the recording, and a
result that looks like the original but is not is worse than no replay at all.
Pass `strict = FALSE` to run a partial recording anyway.

Embeddings are not model calls, so a trace does not contain them. Whether a
replay can reproduce a run's chunk *ranking* therefore depends on how the run
embedded, and the answer is checked rather than assumed: the replay reproduces
the ranking exactly when the recording used a **deterministic** embedder and the
replay uses the **same** one. Both conditions, not either — replaying an
API-embedded run with a deterministic local embedder would compute vectors the
original never saw while looking exact. Anything else warns
(`gr_replay_no_embeddings`) and falls back to lexical vectors; every recorded
answer is still reproduced, but the ranking may differ. So a run you intend to
publish is worth recording with `gr_options(embedder = "lexical")`, or with your
own embedder registered as `deterministic = TRUE`:

```r
old <- gr_options(embedder = "lexical")
run <- answer_document(readgpt_example(), "What was revenue?", "needle", client = cl)
identical(
  answer_document(readgpt_example(), "What was revenue?", "needle",
                  client = gr_replay_client(run$trace))$chunks_used,
  run$chunks_used)
#> [1] TRUE
```

One thing still does not replay: a trace does not record the JSON schema a call
requested, so two calls differing only by schema share a recording.

## Extending it

Every axis is a registry, so additions behave exactly like built-ins:

```r
gr_register_segmenter("by_bullet", description = "one chunk per bullet",
  fn = function(doc, spec, client, trace) {
    units <- unlist(strsplit(doc$text, "\n(?=[-*])", perl = TRUE))
    new_chunks(units, "by_bullet", spec)
  })

gr_register_cleaner("drop_confidential", stage = "early",
  fn = function(x, o) gsub("(?mi)^\\s*CONFIDENTIAL.*$", "", x, perl = TRUE))

gr_register_model("my-local-llama", context_window = 32768, max_output = 4096)
```

`gr_register_extractor()`, `gr_register_reader()` and `gr_register_embedder()`
work the same way; each has a worked example in its help page.

Embedding is the sixth registry, and the one worth knowing about even if you
never add anything else — it is what makes `retrieve` and the `semantic`
segmenter work offline, for free, and reproducibly:

```r
gr_embedders()
#>      name deterministic
#> 1     api         FALSE
#> 2 lexical          TRUE
#>                                                    description
#> 1                 Embeddings endpoint on the client's base URL
#> 2 Hashed bag-of-words; free, offline, word overlap not meaning
```

`gr_options(embedder = "lexical")` switches every part of the package that
embeds. Register your own — a local model, a company service — and the
`semantic` segmenter and the `retrieve` and `iterative` readers all use it, with
no change to any of them. Set `deterministic = TRUE` only if the same text
always gives the same vector: that flag is what a replay checks (see below), and
claiming it wrongly turns a recording into a plausible-looking fiction.

Two constructors make the extension points usable: `new_chunks()` builds the
object a segmenter must return, and `new_answer()` builds the one a reader must
return. Using them is not optional — `gr_segment()` and `gr_read()` reject
anything else — and it is what gets your addition the same token-cap
enforcement, provenance handling and reporting the built-ins have. A registered
addition can then be named in a recipe, put in an `ensemble`, and compared
against a built-in with `gr_compare()`.

```r
gr_register_reader("longest", signature = "one|1|none", cost_calls = "1",
  description = "answer from the longest chunk only",
  fn = function(chunks, question, client, spec, trace) {
    d <- chunks$chunks
    i <- which.max(d$tokens)
    res <- gr_call(client, list(list(role = "user",
             content = paste0(d$text[i], "\n\nQ: ", question))),
           model = spec$model, trace = trace, label = "longest.answer")
    new_answer(res$text, "longest", question, d$chunk_id[i], trace,
               partial = !isTRUE(res$ok))
  })
```

## Shiny app

```r
shiny::runApp(system.file("shiny", package = "readgpt"))
```

Every axis is exposed as a control: cleaning preset or individual cleaners,
segmenter, `max_tokens`, overlap, minimum chunk size, reader (tick several to
compare), top-k, citations, model, temperature, cost cap. A free "Preview
chunking" button shows how your settings break the document up before you spend
anything, and the trace tab shows the trace of the run that produced the answer —
not a second billed pass.

Set `GPTREAD_DOC_ROOTS` (colon-separated) to control which folders the app can
read. **Unset, it defaults to `~/Documents`, falling back to your entire home
directory** — set it explicitly before exposing the app to anyone else.

## Running the tests

```bash
bash run-tests.sh              # install deps if needed, install, run the suite
bash run-tests.sh --check      # full R CMD check instead
bash run-tests.sh --no-install # skip dependency installation
```

Works from the package directory or the repository root. It uses a personal R
library, so it will not fail on a read-only system library.

Running the suite needs only four R packages: `testthat`, `withr`, `jsonlite`
and `httr`. The rest of `Suggests` gates optional features (PDF, OCR, HTML,
parallelism) that the tests do not exercise -- verified by running the full
suite in a library with those packages deliberately absent.

Every test uses an offline mock client, so **no API key is needed and no
requests are made**. By hand:

```bash
R CMD INSTALL . --no-byte-compile
Rscript -e 'library(readgpt); testthat::test_dir("tests/testthat", package = "readgpt")'
```

## Continuous integration

Two GitHub Actions workflows run on every push and pull request:

| workflow | what it does |
|---|---|
| `.github/workflows/tests.yaml` | installs, runs the suite, executes every README code block, diffs its documented output against real output, and asserts the documented counts still match the registries and that no two readers share a traversal signature |
| `.github/workflows/R-CMD-check.yaml` | full `R CMD check` on macOS and Ubuntu, current R and previous release |

The `tests` workflow is the fast signal — under a minute — so a broken change fails
before the check matrix finishes. Its output diff is not cosmetic: it is what
caught the same document producing different token counts on different
machines, which 613 passing tests in one locale did not. Neither workflow needs a secret;
`OPENAI_API_KEY` is explicitly set empty so a missing or leaked key can never
change a result.

## Migrating from v1

The old entry points still work and map onto the new pipeline, warning once:

| v1 | now |
|---|---|
| `answer_question(f, q, mode = "Chunked")` | `answer_document(f, q, "thorough")` |
| `parse_text(f, chunk_method = "semantic")` | `gr_ingest(f)` → `gr_segment(doc, "semantic")` |
| `gpt_read_chunked()` | `gr_read(ch, q, cl, "map_reduce")` |
| `gpt_read_retrieval()` | `gr_read(ch, q, cl, "skim")` |
| `gpt_read_hierarchical()` | `gr_read(ch, q, cl, "hierarchical")` |
| `gpt_read_multipass()` | `gr_read(ch, q, cl, "ensemble")` |

Three behaviour changes are deliberate and will alter your results:

1. **`mode` no longer defaults to all five modes.** `answer_question(f, q)` in v1
   ran every mode — 41 API calls on an 8-paragraph document, where one was
   expected.
2. **Modes no longer contaminate each other.** In v1, `mode = c("Chunked",
   "Semantic")` changed Chunked's chunking, its cost, and its answer relative to
   `mode = "Chunked"` alone.
3. **Digits are no longer stripped by default.** v1's `remove_numbers = TRUE`
   default was unreachable from `answer_question()`, so every figure, date and
   percentage was deleted before the model saw the document.

`v1`'s `refine = TRUE` is mapped to citations only. Its verification pass is not
reproduced because it could never run — it called a `search_text()` function
that was never defined anywhere in the repository.

The v1 source is preserved under `legacy/` for reference and A/B
comparison. It is not loaded by the package; to reproduce its ingestion and
chunking under the current code, use the `legacy` recipe.

The regression suite in `tests/testthat/` reproduces each of these defects
against the current code, so the claims above are checked rather than asserted.
