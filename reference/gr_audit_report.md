# Write the run out as an auditable report

One self-contained HTML file holding everything a reader needs to check
the work without running it: the protocol that was fixed in advance,
what happened to every document, every extracted value with the sentence
and page it came from, whether that sentence is really there, what was
written and which rows each claim rests on, and what the run cost.

## Usage

``` r
gr_audit_report(
  path,
  screening = NULL,
  extraction = NULL,
  synthesis = NULL,
  protocol = NULL,
  title = NULL,
  claims = NULL,
  records = NULL,
  calibration = NULL,
  answer = NULL,
  open = interactive()
)
```

## Arguments

- path:

  Where to write the file.

- screening, extraction, synthesis:

  The objects from
  [`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md),
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  and
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md).

- protocol:

  The
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md)
  the run was made under. Worth passing even when the other objects
  carry its pieces: the criteria as *written* are what a reader checks
  the decisions against.

- title:

  A heading for the report.

- claims:

  A
  [`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
  result, or `NULL` to take the one the synthesis carries. It adds the
  link the rest of the report cannot make: the claim a sentence is
  making, back to the studies meant to support it.

- records:

  A
  [`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md).
  Adds the search itself to the report (which sources, with what query,
  on what date) and starts the flow counts at identification. Without
  one the report says so, because a missing search is a defect in the
  review rather than in the report.

- calibration:

  A
  [`gr_calibrate()`](https://elkronos.github.io/readgpt/reference/gr_calibrate.md)
  result, to add a section saying how good the screening is:
  sensitivity, specificity, kappa, and how many eligible studies the
  screener threw away. Without it the report says what the run did and
  nothing about whether it did it well.

- answer:

  A
  [gr_answer](https://elkronos.github.io/readgpt/reference/gr_answer.md)
  from
  [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
  or a `gr_corpus` from
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md).
  Adds the question, the answer, whether it is complete, what it cost,
  and the passages it came from in document order; see "An answer"
  below.

- open:

  Open the report once it is written: in the RStudio viewer when the
  file is under [`tempdir()`](https://rdrr.io/r/base/tempfile.html), and
  otherwise in the web browser. By default only in an interactive
  session.

## Value

`path`, invisibly.

## Details

Pass whichever stages you ran. Nothing is required, and anything omitted
is simply absent from the report.

## What the report is for

Not for the person who ran it, who can index into `$evidence`. For the
reviewer, co-author or regulator who did not, and whose question is "how
do you know?" The chain it lays out is: a claim cites a study, the study
is a row, the row's values each cite a sentence, and each sentence was
checked against the page it was attributed to.

## What it does not tell you

Verification means a quoted sentence really occurs in the chunk it was
credited to. It does not mean the sentence supports the value extracted
from it, and it does not mean the value is right. A verified quote and a
wrong reading of it look identical here; what the check rules out is the
quote having been invented. The report says so, in the report, because a
column a reader over-reads is worse than no column.

## It does not flatter the run

Unverified quotes, documents that could not be read, screening calls the
model declined to make, fields nothing supported and citations pointing
at rows that do not exist are all counted near the top. An audit that
showed only what worked would look like diligence and be the opposite.

## An answer

With `answer`, the report shows the answer or says that it was not
found, whether it is partial and why, the recipe, reader, number of
requests and cost, and any warnings. Then the passages behind it,
grouped by chunk and in document order, each with its page, section and
chunk number:

- A quotation a reader copied out (`skim`, `extract`) is highlighted in
  the chunk it came from. One that is not in that chunk is listed under
  it and flagged.

- A chunk a reader sent whole (`stuff`, `retrieve`, `rerank`) is shown
  with the numbers from the answer highlighted where they occur. That
  shows where to look, not that the chunk supports the answer. A chunk
  the answer cites as `[chunk n]` is marked as cited.

- `map_reduce` answers each chunk and then combines the answers. Its
  answer from each chunk is shown as such: the model's words, not the
  document's. `refine` and `hierarchical` keep no passages, and the
  report says so.

A passage over 6,000 characters, such as a whole document sent in one
request, is cut to the text around what is highlighted, with each cut
shown as "\[...\]". Last comes one row per request, from
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on the
answer's trace (see
[`gr_trace()`](https://elkronos.github.io/readgpt/reference/gr_trace.md)),
without the prompts and replies.

A `gr_corpus` gives one row per document, then each document's answer
and passages. Answers are there only if the run kept them
(`keep_answers`).

## See also

[`gr_flow()`](https://elkronos.github.io/readgpt/reference/gr_flow.md),
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md),
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md),
[`gr_verify_evidence()`](https://elkronos.github.io/readgpt/reference/gr_verify_evidence.md),
[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)

## Examples

``` r
fields <- gr_fields(design = "The study design")
cl <- gr_mock_client(function(messages, params) {
  '{"design":"randomised trial","design__quote":"We ran a randomised trial."}'
})
f <- tempfile(fileext = ".txt"); writeLines("We ran a randomised trial.", f)
x <- gr_extract(f, fields, client = cl)
#> [1/1] file1dcd11e98ff6.txt
#> Extracting 'file1dcd11e98ff6.txt' with the 'txt' extractor.
#> Ingested 1 block(s), ~10 tokens (0 chars removed by cleaning).
#> Segmenting with 'structural' (cap 900 tokens, overlap 90).
#> Reading with 'extract' (all|N+conflicts|none) over 1 chunk(s).

out <- gr_audit_report(tempfile(fileext = ".html"), extraction = x, open = FALSE)
#> Audit report written to /tmp/RtmpRyE9R0/file1dcd82a077.html
file.exists(out)
#> [1] TRUE

# One answer and the passages behind it.
cl2 <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl2)
#> Using cached ingestion for this document + settings.
#> Segmenting with 'paragraph' (cap 4000 tokens, overlap 0).
#> Reading with 'stuff' (all|1|none) over 1 chunk(s).
page <- gr_audit_report(tempfile(fileext = ".html"), answer = ans, open = FALSE)
#> Audit report written to /tmp/RtmpRyE9R0/file1dcd71146c12.html
```
