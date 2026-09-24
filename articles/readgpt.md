# Get started with readgpt

readgpt asks a large language model questions about your documents (a
report, a contract, a folder of research papers) and returns the answer
with a record of how it was reached: which parts of the document the
model saw, what it was asked, what it said, and what that cost. This
guide assumes no previous experience with language models. It explains
the few ideas you need, installs the package, and goes through a first
question from start to finish.

Everything below runs without an account or an internet connection, so
you can follow along for free.

## A few ideas first

**The model runs somewhere else.** A large language model (an *LLM*)
such as GPT is a program run by a *provider*: OpenAI, Anthropic, Google,
or a model on your own computer. readgpt sends it text over the internet
and reads the reply. Commercial providers charge for each request.

**Text is measured in tokens.** Models do not read characters or words;
they read *tokens*, pieces of words. In English a token is roughly four
characters, so a page of text is several hundred tokens. Providers
charge per token, both for the text you send and, at a higher rate, for
the text the model writes back.

``` r

library(readgpt)
```

``` r

gr_count_tokens("Revenue rose to 45.2 million dollars in fiscal 2024.")
#> [1] 20
```

readgpt estimates token counts itself, without asking the provider, so
it can work out what a request will cost before sending it.

**A model can only take in so much at once.** The most text a model
accepts in a single request is its *context window*. A long report may
not fit, and even when it does, models tend to overlook details in the
middle of a very long prompt. So documents are cut into smaller pieces,
called *chunks*, and the question is put to the chunks in some order.

**That makes three decisions**, and readgpt keeps them separate so you
can see and change each one:

1.  **Ingest**: get the text out of the file and tidy it.
2.  **Segment**: cut the text into chunks.
3.  **Read**: decide how the model works through the chunks. It can take
    all of them in one request, each one separately, only the most
    relevant few, and so on.

A fourth choice, which model answers, is independent of all three.

**Models can make things up.** A model may state something the document
never says, and it can do so fluently. readgpt cannot prevent that. It
records which text each answer rests on, checks quotations against the
document, and tells you when anything went wrong. The first example
below shows where to look.

## Installing

readgpt needs R 4.1 or later. Install it from GitHub:

``` r

install.packages(c("remotes", "knitr", "rmarkdown"))
remotes::install_github("elkronos/readgpt", build_vignettes = TRUE)
```

`build_vignettes = TRUE` also installs these guides, so
[`vignette()`](https://rdrr.io/r/utils/vignette.html) can open them.
Building them needs `knitr`, `rmarkdown` and Pandoc. Pandoc comes with
RStudio; without RStudio, install it from
<https://pandoc.org/installing.html> first, or leave out
`build_vignettes = TRUE`.

The package itself depends only on `digest`, `httr` and `jsonlite`. Some
file types and features need an extra package. Each is optional, so
install only what you use:

| to do this | install |
|----|----|
| read PDFs | `pdftools` |
| read Word documents and web pages | `xml2` |
| read scanned pages and images (OCR) | `tesseract` and `magick` |
| use a provider other than OpenAI | `ellmer` |
| run many requests at once | `future` and `future.apply` |
| count tokens exactly as OpenAI does | `reticulate`, with Python’s `tiktoken` |
| use the point-and-click app | `shiny` |

If you ask for something that needs a package you do not have, readgpt
says which one. It stops, unless it can carry on without it. For
example, it runs requests one at a time when the parallel packages are
missing.

## Connecting to a model

To use a real model you need an account with a provider and an *API
key*, a password that identifies your account to the provider’s servers.
For OpenAI, create one at <https://platform.openai.com>.

Keep the key out of your scripts. The usual place is your `.Renviron`
file, which R reads when it starts:

``` r

file.edit("~/.Renviron")
# add this line, save the file, then restart R:
# OPENAI_API_KEY=sk-...
```

[`gr_api_key()`](https://elkronos.github.io/readgpt/reference/gr_api_key.md)
checks that R can see it, and stops with an explanation if it cannot:

``` r

gr_api_key()
```

If the key is missing, a run stops before it reads your document and
gives the same explanation.

The README’s “Other providers” section covers Anthropic, Google, Azure,
company gateways, and free models running on your own machine through
Ollama.

## Practising without a key

For learning, a real model is unnecessary.
[`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md)
builds a stand-in that replies with whatever an R function returns, and
it can be used anywhere a real model can. This one always gives the same
answer, which is enough to see how everything else works:

``` r

cl <- gr_mock_client(function(messages, params) "Revenue was 45.2 million dollars.")
old <- gr_options(verbose = FALSE)
```

`verbose = FALSE` stops readgpt printing a line for each stage; leave it
on in your own work while you are learning. In an interactive session it
also keeps a line up to date with how far a long read has got and what
it has spent.
[`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)
returns the settings it replaced, so they can be restored later with
`gr_options(old)`.

## Your first question

[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
does all three steps in one call. readgpt includes a short example
document, an annual report:

``` r

ans <- answer_document(readgpt_example(), "What was revenue in 2024?", client = cl)
ans$answer
#> [1] "Revenue was 45.2 million dollars."
```

With a real model you would leave out `client =` and point it at your
own file:

``` r

ans <- answer_document("annual-report.pdf", "What was revenue in 2024?")
```

The first argument can be a path to a file, a web address, or the text
itself. A missing file whose name ends in an extension readgpt reads
(`.pdf`, `.docx`, `.md` and so on) is an error. Any other string is read
as the text of the document, including a path with a mistyped extension:
`"reports/2024/annual-report.pfd"` becomes a one-line document about a
file name. `ans$document$source` shows which happened: the path for a
file, the address for a download, `<inline text>` for a string.

## Checking the answer

`ans` holds much more than the answer. Look at three things before
relying on it.

**`partial` says whether anything went wrong.** It is `TRUE` if a
request failed, if chunks were left out because they did not fit, if a
limit on requests or spending stopped the run early, if a reading
strategy fell back to a simpler method, or if pages of the document
never became text (a scanned page read without OCR, for example):

``` r

ans$partial
#> [1] FALSE
```

When it is `TRUE`, printing the answer says why, and `ans$notes` holds
the details. Here is a “model” that fails every request, as a network
outage would:

``` r

broken <- gr_mock_client(function(messages, params) stop("no connection"))
bad <- answer_document(readgpt_example(), "What was revenue in 2024?",
                       client = broken)
bad
#> <gr_answer> reader=stuff (PARTIAL)
#>   Q: What was revenue in 2024?
#>   1 model call(s), 0 in / 0 out tokens, 1 error(s), $0.0000 across gpt-5.6-terra
#>   ---
#> Not found in the part of the document that was read.
#>   ---
#>   Partial because: first error: no connection
```

The run did not stop with an error. It finished, and the failure is
recorded on the result, so check `partial` every time. Any warnings
raised along the way are kept in `ans$warnings` as well as printed, so
they can still be read after the console has scrolled past them.

**[`is_not_found()`](https://elkronos.github.io/readgpt/reference/is_not_found.md)
says whether the document had the answer at all.** Every reading
strategy tells the model to reply with a fixed marker when the text it
was shown does not answer the question, so “the document does not say”
can be told apart from an answer:

``` r

is_not_found(ans$answer)
#> [1] FALSE
is_not_found(bad$answer)
#> [1] TRUE
```

For `bad`, the marker means “no answer came back”, and `partial` says
that was a failure rather than a finding.

**`evidence` shows what the answer rests on.** Each row is a chunk the
answer drew on, with where it came from: the page, for a PDF, and the
heading it sits under. This short report fitted in a single chunk
spanning several headings, so there is one row and both are empty:

``` r

ans$evidence[, c("chunk_id", "page", "section")]
#>   chunk_id page section
#> 1        1   NA      NA
```

On a long document there is a row for every chunk used, and `page` and
`section` tell you where to find the passage. What each row’s `text`
holds depends on the reading strategy. When the model is asked to quote
the document, readgpt checks every quotation against the text and marks
the answer `partial` if one is not there.
[`vignette("readers")`](https://elkronos.github.io/readgpt/articles/readers.md)
shows both.

To see the passages in place, write the answer out as a page.
[`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md)
shows the answer, whether it is partial, the cost, and each passage with
its page and section, with the quotation or the answer’s numbers
highlighted. In an interactive session it opens the page for you:

``` r

gr_audit_report("answer.html", answer = ans)
```

## What it cost

Every result carries a *trace*: each request sent, each reply, and the
tokens both used.

``` r

gr_trace_summary(ans$trace)[, c("calls", "tokens_in", "tokens_out")]
#>   calls tokens_in tokens_out
#> 1     1       599         13
gr_trace_cost(ans$trace)[, c("model", "calls", "usd")]
#>           model calls      usd
#> 1 gpt-5.6-terra     1 0.001354
```

[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) gives one
row per request, with its cost and how long it took. The `prompt` and
`reply` columns hold exactly what was sent and what came back:

``` r

as.data.frame(ans$trace)[, c("stage", "tokens_in", "tokens_out", "usd", "seconds")]
#>          stage tokens_in tokens_out      usd seconds
#> 1 stuff.answer       599         13 0.001354   0.001
```

`usd` is priced from readgpt’s list of known models
([`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md)).
A model it does not know is priced as unknown rather than free. You can
also estimate before sending anything. Here is a 120,000-token document
with a 4,000-token answer:

``` r

gr_estimate_cost(gr_options("model"), input_tokens = 120000, output_tokens = 4000)
#> [1] 0.288
```

Two safety limits are on from the start. readgpt stops reading a
document once it has spent \$5 or made 400 requests. Each request is
priced as it is made, so a run can pass \$5 by the cost of one request.
The answer is then marked partial, and printing it says which limit
stopped the run. A run that could not finish inside the limits is
refused before the first request: one that would need more than 400
requests, or one whose chunks would cost more than \$5 to send once:

``` r

unlist(gr_options()[c("max_cost_usd", "max_calls")])
#> max_cost_usd    max_calls 
#>            5          400
```

Change them with
[`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md),
for example `gr_options(max_cost_usd = 20)`.

## Choosing a recipe

[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
made the three decisions using a *recipe*, a named combination of
choices. The default, `"auto"`, picks one of two by the document’s
length: `"fast"`, which sends the whole document in one request, for up
to 50,000 tokens (less on a model with a small context window), and
`"thorough"`, one request per chunk, for anything longer. `ans$recipe`
says which it picked:

``` r

ans$recipe
#> [1] "fast"
```

The built-in recipes:

``` r

do.call(rbind, lapply(names(gr_recipes()), function(n) {
  r <- gr_recipes(n)
  data.frame(recipe = n, chunks = r$segment$method, reading = r$read$reader)
}))
#>       recipe     chunks      reading
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

A starting point for each kind of job:

| your document and question                   | try           |
|----------------------------------------------|---------------|
| short enough to send whole                   | `"fast"`      |
| one fact somewhere in a long document        | `"needle"`    |
| every mention has to be found                | `"thorough"`  |
| long, with headings, and you want a summary  | `"survey"`    |
| scanned pages, forms or invoices             | `"scanned"`   |
| an argument that builds through the text     | `"narrative"` |
| a question that needs several facts combined | `"research"`  |
| high stakes, and you want a second opinion   | `"consensus"` |

Pass the name as the third argument:

``` r

fast <- answer_document(readgpt_example(), "What was revenue in 2024?", "fast",
                        client = cl)
fast$reader
#> [1] "stuff"
```

Recipes differ a great deal in cost. `"thorough"` sends every chunk to
the model separately, so a long document means many requests; `"fast"`
and `"needle"` make one.
[`vignette("readers")`](https://elkronos.github.io/readgpt/articles/readers.md)
explains each reading strategy and when to use it.

## More than one document

For a folder of documents,
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
asks the same question of each file and returns one row per document.
For a literature review,
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)
decides which documents meet your criteria,
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
builds a table of the details you ask for, and
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
writes up the findings with each statement citing the study it came
from.
[`vignette("tour")`](https://elkronos.github.io/readgpt/articles/tour.md)
shows each of these briefly.

## Where to go next

- [`vignette("ingest")`](https://elkronos.github.io/readgpt/articles/ingest.md):
  getting text out of PDFs, Word files, web pages and scans, and
  cleaning it.
- [`vignette("readers")`](https://elkronos.github.io/readgpt/articles/readers.md):
  the reading strategies, what each costs, and how to choose.
- [`vignette("tour")`](https://elkronos.github.io/readgpt/articles/tour.md):
  everything else, briefly. It covers chunking, caching and replaying
  runs, many documents, reviews, and extending the package.
- [`?gr_options`](https://elkronos.github.io/readgpt/reference/gr_options.md):
  every setting, with its default.

`browseVignettes("readgpt")` lists all the guides.

## Glossary

**API key**: a password identifying your account to a provider.

**Chunk**: a piece of a document small enough to send to the model.

**Context window**: the most text a model accepts in one request.

**Embedding**: a list of numbers standing for a piece of text, used to
find chunks similar to a question.

**Evidence**: the text an answer rests on, in `ans$evidence`.

**LLM**: large language model, the program that reads and writes text.

**Mock client**: a stand-in for a model that runs an R function, for
practising and testing without cost.

**Partial**: `ans$partial` is `TRUE` when a request failed, chunks did
not fit and were left out, the run stopped early, a reading strategy
fell back to a simpler method, or pages of the document never became
text. Printing the answer says which.

**Prompt**: the text sent to a model in one request.

**Provider**: the company or program that runs the model.

**Reader**: a strategy for working through the chunks.

**Recipe**: a named combination of an ingest, a segmentation and a
reader.

**Segmenter**: a method of cutting text into chunks.

**Token**: the unit models read and providers charge by, roughly four
characters of English.

**Trace**: the record of every request and reply in a run.
