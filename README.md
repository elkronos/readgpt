# gptread

<!-- Badges assume github.com/elkronos/gpt_read -- adjust the owner/repo if yours differs. -->
[![R-CMD-check](https://github.com/elkronos/gpt_read/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/elkronos/gpt_read/actions/workflows/R-CMD-check.yaml)
[![tests](https://github.com/elkronos/gpt_read/actions/workflows/tests.yaml/badge.svg)](https://github.com/elkronos/gpt_read/actions/workflows/tests.yaml)

Fine-grained control over how a language model ingests, segments, and reads a
document.

The premise is that *how* you break a document up and *how* you make the model
read it are separate decisions worth experimenting with. This package makes them
separate in the code. Three independent axes, each a registry you can extend
without editing the package:

```
  source  ──▶  INGEST  ──▶  SEGMENT  ──▶  READ  ──▶  answer + trace
               │             │             │
               │             │             └─ 9 strategies, each with a
               │             │                distinct traversal signature
               │             └─ 9 segmenters + overlap / min-size / context
               └─ 6 extractors + 14 individually toggleable cleaners
```

Any ingest × any segmenter × any reader composes. `gr_recipe()` binds one of
each into a named pipeline. Recipes are isolated: a recipe run alongside others
produces a byte-identical `$answer` and `$chunks_used` to running it alone.

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
remotes::install_local("path/to/gptread")
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
options(gptread.api_key = "sk-...")     # or
cl <- gr_client(api_key = "sk-...")     # per-client, for multi-user Shiny
```

Resolution order is explicit argument → `gptread.api_key` option →
`OPENAI_API_KEY`.

**Without a key nothing raises.** Every model call fails, you get
`"NOT_IN_DOCUMENT"` back, and the failure is reported on the answer object:

```r
ans$partial        # TRUE
ans$notes$error    # "No API key available."
```

Always check `ans$partial` before trusting an answer. Call `gr_api_key()`
yourself if you would rather fail fast.

## Quick start

```r
library(gptread)

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
cmp <- gr_compare(gptread_example(), "What was revenue in fiscal 2024?",
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

(The bundled example is deliberately small — 523 tokens by `gr_count_tokens()`
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
`academic`, `scan`, `legacy_v1`.

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
doc <- gr_ingest(gptread_example())
do.call(rbind, lapply(c("fixed", "paragraph", "sentence", "structural"),
  function(m) gr_chunk_stats(gr_segment(doc, list(method = m, max_tokens = 120)))))
#>       method n total_tokens min median  mean max over_cap
#> 1      fixed 5          536  57  120.0 107.2 120        0
#> 2  paragraph 6          540  47   94.0  90.0 116        0
#> 3   sentence 6          540  47   94.5  90.0 106        0
#> 4 structural 8          556  25   75.0  69.5 101        0
```

`semantic` needs a client for its embedding pass, so pass one (a
`gr_mock_client()` is fine for a dry run) or it falls back.

What overlap actually costs, in duplicated tokens:

```r
doc <- gr_ingest(gptread_example())
do.call(rbind, lapply(c(0, 30, 60), function(ov)
  gr_chunk_stats(gr_segment(doc, list(method = "sentence", max_tokens = 120,
                                      overlap_tokens = ov)))))
#>     method n total_tokens min median mean max over_cap
#> 1 sentence 6          540  47   94.5 90.0 106        0
#> 2 sentence 7          674  74   99.0 96.3 106        0
#> 3 sentence 9          868  79   96.0 96.4 109        0
```

## Axis 3 — read

`gr_read()` answers the question. Nine strategies, each with a **traversal
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
| `ensemble` | `ensemble\|sum+1\|none` | Σ members + 1 | several distinct readers, adjudicated; members must have different signatures |

`rerank` and `iterative` need JSON-schema structured output. Against an endpoint
without it they degrade — to BM25 ranking and to single-shot retrieve
respectively — with a warning and a note on the answer.

`gr_compare()` refuses to bill you twice for two configurations that resolve to
the same segmentation and the same signature.

## Reading a run

Nothing degrades silently. Everything below is recorded on the answer.

```r
ans$partial     # TRUE means something degraded — check this first
ans$notes       # what: dropped_chunks, failed_calls, error, degraded_to_bm25, ...
ans$evidence    # what the answer rests on
print(ans$trace)
gr_trace_summary(ans$trace)
#>                          run_id calls steps tokens_in tokens_out errors elapsed_s
#> 1 run_20260825184837.437_4804a5     1     8       698         13      0      0.11

gr_estimate_cost("gpt-4o", ans$trace$tokens_in, ans$trace$tokens_out)
as_json(ans)    # answer plus every prompt and response, from the same single run
```

`ans$evidence` is a data frame with `chunk_id`, `text`, `page`, `section`,
`score`. **What `text` holds depends on the reader**: verbatim chunk text for
`stuff`, `retrieve`, `rerank` and `iterative`; model-*extracted* passages for
`skim`; per-chunk model *answers* for `map_reduce`. `refine` and `hierarchical`
return `NULL`. `page` is populated only for PDF sources. `score` is set only by
`retrieve` (cosine) and `rerank` (0–10, model-judged).

With `cite = TRUE` the model cites bracketed chunk ids (`[chunk 3]`); map those
back to pages through `ans$evidence`.

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
| figures, dates or percentages are missing | `doc$stats$clean_log` | a cleaning step removed them. `remove_numbers` is off by default; the `legacy_v1` preset turns it on deliberately |
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
#> 10 legacy_v1  paragraph   map_reduce
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

`legacy_v1` deliberately reproduces the previous release's behaviour — digit
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
error instead. `gr_options()` documents all 19 settings; see `?gr_options`.

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

`gr_register_extractor()` and `gr_register_reader()` work the same way; each has
a worked example in its help page.

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
shiny::runApp(system.file("shiny", package = "gptread"))
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
Rscript -e 'library(gptread); testthat::test_dir("tests/testthat", package = "gptread")'
```

## Continuous integration

Two GitHub Actions workflows run on every push and pull request:

| workflow | what it does |
|---|---|
| `tests.yaml` | installs, runs the suite, executes every README code block, and asserts the documented counts still match the registries and that no two readers share a traversal signature |
| `R-CMD-check.yaml` | full `R CMD check` on macOS and Ubuntu, current R and previous release |

`tests.yaml` is the fast signal — under a minute — so a broken change fails
before the check matrix finishes. Neither workflow needs a secret;
`OPENAI_API_KEY` is explicitly set empty so a missing or leaked key can never
change a result.

## What gets committed

`.gitignore` governs git; `.Rbuildignore` governs the tarball `R CMD build`
produces. They are different files with different jobs, and **a file excluded
from the tarball is still committed unless git is told otherwise**.

Committed: the package itself, plus `run-tests.sh` and `.github/` as ongoing
development tooling.

Ignored: `REVIEW.md`, `COMMIT_MSG.txt` and `migrate.sh` (one-time migration
scaffolding), `_old_gptread/`, `*.tar.gz`, `*.Rcheck/`, `.DS_Store`, editor
directories, and -- first in the file, because this package reads a key from
the environment -- `.Renviron`, `.Rprofile`, `.env`, `*.pem` and `.secrets`.

`.Rprofile` is ignored because the API-key section above documents it as a place
to put `OPENAI_API_KEY`. A project `.Rprofile` holding only settings is a
perfectly normal thing to commit; if that is yours, keep the key in `.Renviron`
instead and remove the line.

Check before you commit, not after:

```bash
bash migrate.sh --preflight
```

It lists what git would include, then greps that list for the usual mistakes
(archives, `.Rcheck` output, `.DS_Store`, `.Renviron`, `.Rprofile`, anything
matching an API key) and says, for each one, whether it is untracked — where a
`.gitignore` line fixes it — or **already tracked**, where it does not:
`.gitignore` never untracks anything, so a file that was committed before the
rule existed keeps being committed until you `git rm --cached` it. If such a
file held a real key, rotate the key; it is in the history either way.

It exits non-zero if it finds anything, so it drops straight into a pre-commit
hook:

```bash
printf '#!/bin/sh\nbash migrate.sh --preflight\n' > .git/hooks/pre-commit
chmod +x .git/hooks/pre-commit
```

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

The regression suite in `tests/testthat/` reproduces each of these defects
against the current code, so the claims above are checked rather than asserted.
