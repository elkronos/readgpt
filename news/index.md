# Changelog

## readgpt 0.5.1

### New

- **A review that reads like a review: citations by name, a reference
  list, and a coherence pass.**
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
  produced prose citing `[study 3]`, with no reference list and no
  connective tissue between sections. It is now capable of the thing
  people need to publish.

  `cite_style` renders the markers as `(Smith & Okafor, 2019)`, merging
  adjacent markers into one citation, disambiguating a shared
  author-year as 2019a and 2019b, and handling the author-list shapes
  that turn up, including the lowercase particles (`van der Berg`,
  `de la Cruz`) that a requires-a-capital rule drops silently.
  `references = TRUE` appends a list built from the studies the finished
  text actually cites, alphabetical under author-year and numbered by
  study otherwise, because the list has to be labelled by whatever the
  prose uses to point into it.

  The writing model is never shown who wrote a study. Adding
  bibliographic fields to a schema put “authors: Smith, J., Okafor, A.”
  in front of a model asked to cite `[study 1]`, and a model that can
  see a name will write “Smith and Okafor (2019) found…” instead of the
  marker: a citation checked by nothing and rendered by nothing, which
  is the model asserting an attribution and the one thing this design
  exists to prevent. The document filename went the same way and for the
  same reason: academic PDFs are routinely called
  `Smith2019_CognitiveLoad.pdf`, which leaks an author and a year
  through the one field nobody thinks of as bibliographic. Both are
  withheld from the writing prompt and applied afterwards, so a model
  cannot misattribute a study whose authors it was never told. If a
  bibliographic value is also a finding, extract it a second time under
  a name of its own.

  The model still writes `[study 3]`, always, and rendering happens
  afterwards from the table. A marker can be checked exactly against the
  rows that exist; verifying “Smith & Okafor (2019)” would mean matching
  a name the model wrote against a name in the table, and near-misses
  (Smith for Smyth, 2019 for

  2018. are both the errors that matter and the ones fuzzy matching
        forgives. A rendered citation is therefore a fact about the
        extraction, and `$text_marked` keeps the marker form so the
        check can be re-run on what was published. A study that cannot
        be named makes the whole run fall back to markers rather than
        mixing names and numbers or inventing “n.d.”.

  `coherence = TRUE` runs the revision passes over the assembled draft,
  so the independently-written sections read as one argument (see “Three
  revision passes” below for what they are). Their output is checked
  rather than trusted, because this is the one step that could quietly
  undo the guarantee the rest of the pipeline exists to give: a revision
  that added a citation, dropped one, or ran into the model’s output
  limit is discarded with a warning and `$draft` is what you get. The
  passes are budgeted for the whole document, which is what they
  rewrite, rather than for a section.

  `style` carries a register
  (`"formal academic; hedge claims; past tense for findings"`) into both
  the section and coherence prompts, appended rather than substituted,
  so the rules about citing every claim and inventing nothing hold
  whatever voice is asked for.

- **[`gr_inventory()`](https://elkronos.github.io/readgpt/reference/gr_inventory.md):
  what is in a folder, before you read any of it.** Every other pre-run
  check here is per document and happens once the run is already going:
  `preflight()` estimates a document’s cost as
  [`gr_read()`](https://elkronos.github.io/readgpt/reference/gr_read.md)
  is called, and `extract_pdf()` decides a page needs OCR while
  extracting that page. Both are the right check in the wrong place for
  a corpus. Point
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  at four hundred PDFs and you learn that a hundred and eighty are
  scans, and that tesseract is not installed, after paying for the two
  hundred and twenty that were not. This is the same discipline one
  level up, and deliberately not an LLM step: whether a page carries a
  text layer is a character count, whether a file has an extractor is a
  lookup, and asking a model either would be slower, cost money and be
  less accurate than the answer already on disk.

  One row per file **including** the ones that will not be read, because
  “180 of your files were skipped” is the finding and a table of
  survivors cannot report it. A file whose extractor’s package is absent
  is `needs_package`, not `ready`, and is left out of the token total
  and the cost floor, because sizing a run that cannot happen is worse
  than not sizing it. A file reachable by two paths (a `latest -> v3`
  symlink beside the versions it points at) is surveyed once. No file
  can stop the survey either: a corrupt PDF, a binary file wearing a
  `.txt` extension, a broken symlink or anything unforeseen becomes a
  row saying so, which is the contract
  \[[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)\]
  has always given a corpus run.

  It chooses nothing for you: no automatic routing of files to recipes,
  because a router that reads one document with `retrieve` and another
  with `stuff` returns a plausible answer built on part of a file with
  nothing saying so, and it makes
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  meaningless: the corpus no longer had *a* configuration.

- **Three ways a directory could quietly read as less than you gave
  it.** Found while writing the above, all in `corpus_sources()` and all
  silent.

  Files whose extension no extractor claims were **dropped without a
  word**, so a folder of `.doc` files (not `.docx`) read as an empty
  corpus and a mixed folder read as however much of it happened to be
  supported. It now warns, counts them by extension, and carries the
  list on the result.

  A document **lost the folder it came from**:
  [`basename()`](https://rdrr.io/r/base/basename.html) turned
  `2019/report.txt` and `2020/report.txt` into one name, and
  [`make.unique()`](https://rdrr.io/r/base/make.unique.html) then
  separated them as `report.txt` and `report.txt#1`, discarding the
  meaningful half and replacing it with an index that depends on sort
  order. Somebody who had filed by year could not tell their own rows
  apart. Labels are now relative to the directory, so the folder
  survives into `$summary` and into the extraction table.

  And the empty-directory error **never mentioned `recursive`**, which
  is the commonest cause by far: the default does not descend, so a
  folder of subfolders looks empty. It now counts the readable files
  sitting below and says so.

- **`.csv` and `.tsv` are readable.** An extractor claims them, so a
  folder of exported tables is read rather than skipped.

- **`preview`: the reader that decides how to read before reading.**
  Every other reader treats all chunks alike: `stuff` sends them all,
  `map_reduce` answers from each in turn, `retrieve` ranks them by
  similarity. None of them plans. `preview` builds an outline from
  section labels, sizes and short excerpts, asks once which sections
  must be read in full, which are worth scanning, and which cannot bear
  on the question, then reads accordingly and answers. Its traversal
  signature is `planned|1+s+1|none` (`planned` is a selection mode no
  other reader has), and the plan comes back in `ans$notes$plan`, so
  “sections 3 and 5 were not read” is a finding you can see rather than
  a silent economy.

  Three things keep the planner honest, because it is itself an LLM call
  about a long document and so prone to exactly the degradation this
  package exists to manage. Its outline is built from metadata and
  excerpts, never the full text, and is capped by the new
  `preview_tokens` setting; per-section excerpts shrink until the whole
  outline fits, so no section is hidden from it by truncation. Its
  output is a fixed schema. And a section the plan does not mention
  defaults to **read**, never to skip, because silence must not be able
  to lose a document’s contents. When no usable plan comes back at all,
  the reader reads everything, warns `gr_preview_degraded`, and marks
  the answer partial.

  A plan that marks every section skip reads nothing. A document with no
  headings is planned by blocks rather than as one section, and a
  skimmed section whose excerpt had to be cut reports `tokens_truncated`
  and marks the answer partial.

  It is called `preview` and not `survey` because a recipe already owns
  `survey`, and `as_recipe()` resolves recipes before readers, so a
  reader of that name would have been silently unreachable by string. A
  name owned by both now warns (`gr_ambiguous_name`) instead of quietly
  resolving to one of its two meanings.

- **A trace records the configuration that produced it.** Trace meta
  carried the recipe’s *name* and nothing about how it was set up, so
  two runs of `"thorough"` with `top_k = 3` and `top_k = 8` produced
  traces (and a
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  summary) that could not be told apart. For a package whose point is
  comparing configurations, the configuration has to be in the record.
  The preflight and segment steps now carry whatever differs from the
  defaults, `gr_compare()$summary` gains a `settings` column naming it,
  and every trace stamps the readgpt version that wrote it. A run that
  changed nothing reports nothing: the record says what the run did, not
  thirty defaults.

- **[`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md)
  and
  [`gr_search()`](https://elkronos.github.io/readgpt/reference/gr_search.md):
  the review starts at the search, not at a folder.** Reads RIS and
  BibTeX exports from any number of databases, deduplicates by DOI,
  matches the surviving records to documents on disk, and reports the
  counts PRISMA item 16 asks for: identified, duplicates removed,
  screened, sought, retrieved, not retrieved.
  [`gr_flow()`](https://elkronos.github.io/readgpt/reference/gr_flow.md)
  and the audit report now begin at identification rather than at
  “sources given”, which was already past the step that decides whether
  a review can be repeated.
  [`gr_search()`](https://elkronos.github.io/readgpt/reference/gr_search.md)
  carries the databases, queries, dates, limits and registration (items
  6, 7 and 24), so what was searched travels with the run instead of
  living in a lab notebook.

  The records also supply the author and year that `cite_style` renders.
  From an export they are data, joined onto the extraction table by file
  path, rather than values a model read off a title page, which is the
  loosest guarantee in the pipeline, for the one part of a citation that
  must be exactly right. The writing model is still never shown them.

  No model is called. Which paper a record is, and whether two records
  are one paper, are questions a DOI answers exactly. Everything is base
  R: the parsing is not the hard part, the dialect drift between
  databases is, and a dependency would only move that somewhere this
  package cannot see it. `DP` alone is “database provider” in the RIS
  specification and “date of publication” in what PubMed exports; read
  as the former, every PubMed record loses its year.

  Two judgements can attribute one paper’s findings to another, and both
  refuse rather than guess. Two records that both carry DOIs are never
  merged on a title match. And a document is matched by the path the
  export gave, the DOI in the filename, the title, or
  first-author-and-year, but a file that two records could claim by the
  same route goes to neither, so two Smith 2019 papers and one
  `smith2019.pdf` claim nothing at all.

- **[`gr_reference()`](https://elkronos.github.io/readgpt/reference/gr_reference.md)
  and
  [`gr_calibrate()`](https://elkronos.github.io/readgpt/reference/gr_calibrate.md):
  how often the screener is wrong, measured rather than assumed.** Draw
  a sample of a screening run for a person to judge blind, read the
  completed CSV back, and get the rates that sample supports, each with
  a Wilson interval and each refused when the sample is too small. It is
  what turns “we used an LLM” into a claim with a number attached, and
  it is the number a methods section needs.

  Agreement between two model passes is not a substitute. Two passes
  share weights, priors and blind spots, so their errors are correlated:
  they agree most confidently where they are both wrong, and the figure
  would read as reliability while meaning self-consistency.

  **Which rows the sample came from decides which questions it can
  answer**, and computing the rest anyway is worse than computing
  nothing. A sample of exclusions contains no kept records, so
  sensitivity comes out 0% and specificity 100%, artifacts of the frame
  that look exactly like findings. That frame reports the false omission
  rate instead, projected across the whole discarded pile: “about 17
  eligible studies lost, 6 to 47”. The frame is written as a column in
  the CSV, because an attribute does not survive
  [`write.csv()`](https://rdrr.io/r/utils/write.table.html), Excel and a
  fortnight.

  `"unclear"` counts as kept, not as a miss. A deferral goes to a
  person, and penalising it would punish the behaviour that makes the
  screener safe. Two sensitivities are reported where the frame allows,
  as deployed and strict, and the gap is the human reading left.
  Accuracy is not reported at all: at a realistic inclusion rate a
  screener that excluded everything would score ~95%, so Cohen’s kappa
  is given instead. Intervals are Wilson rather than normal, because
  screening proportions sit at the ends of the scale where the textbook
  interval returns \[1, 1\] from five observations.

  A reference that mixes frames (every kept record
  [`rbind()`](https://rdrr.io/r/base/cbind.html)-ed onto a sample of the
  exclusions, which is what
  [`gr_reference()`](https://elkronos.github.io/readgpt/reference/gr_reference.md)’s
  own advice produces) is refused with the fix named, rather than
  averaged over strata sampled at different rates. One whose frame
  cannot be identified warns and says what it assumes, and `of =` lets a
  hand-built reference declare its frame.

- **`headers`: reaching an endpoint that does not authenticate with a
  bearer token.** `base_url` was enough only for a gateway that speaks
  the OpenAI shape and takes `Authorization: Bearer`, because that
  header was hardcoded. Most gateways standing in front of a company’s
  OpenAI and Anthropic tokens are somewhere else: Azure OpenAI
  authenticates with `api-key`, API Management adds a subscription key,
  and many want a cost-centre or correlation id. There was no way to
  express any of it.

  `gr_client(headers = )` takes a named character vector, and
  `gr_options(api_headers = )` sets one for every client, the form that
  belongs in a project’s `.Rprofile`. Two rules make it predictable. A
  header you name replaces the automatic `Authorization` rather than
  joining it, matched without regard to case, because HTTP field names
  are case-insensitive and curl is not: given both spellings it sends
  two headers and leaves the gateway to pick. And naming any header
  makes the API key optional, because nothing here can tell which of a
  stranger’s headers is the credential. `NA` as a value suppresses a
  header instead of sending it, which is how an `OPENAI_API_KEY` left
  set for another client in the same session is kept off the company
  gateway.

  The embeddings endpoint takes the same headers as the chat endpoint.
  They were two copies of the same three lines, which is how one of them
  ends up a release behind the other.

  Three things are refused rather than repaired. A name that is not a
  legal HTTP field name is a typo, not a header. A value carrying a
  control character is request splitting: CR/LF ends the header and
  starts one the caller never wrote, and these values come from
  environment variables and config files, which is exactly where a stray
  line ending comes from. And an empty value is refused because curl
  drops the header entirely, so
  `c("api-key" = Sys.getenv("GATEWAY_KEY"))` with that variable unset
  would otherwise leave without the credential and come back a 401 that
  looks like a wrong key rather than a missing one.

  Headers are excluded from the cache key, for the same reason the API
  key is: a rotating token or a per-request correlation id would make
  every lookup a miss. A header that changes *which* model answers
  belongs in `base_url` or `model`, where the cache can see it.

  [`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md)
  also prints now. It had no print method, so a client echoed at the
  console printed as a plain list with `api_key` in it, in full. Header
  values would have joined it there. Names are shown; values never are.

- **The `claims` protocol: a schema coded so studies can be compared,
  not only read.** The first step of a claims-level synthesis, and the
  constraint it removes is the one everything downstream was capped by:
  a synthesis can only relate studies on dimensions the extraction table
  holds.

  `evidence_table` records the design “in the paper’s own words”, which
  is right for a table a person reads and wrong for one that gets
  crosstabbed: “RCT”, “randomised trial” and “randomized controlled
  trial” become three designs, so a gap analysis reports a design as
  absent while three of them sit in the table. So `claims` codes
  `design` and `finding` as enums and pairs each with the paper’s own
  wording (`design_note`, `effect`), which is the same bargain the
  evidence quotes already make for values: the coding can be checked
  rather than trusted. `effect` stays a string deliberately, because
  effect metrics are not commensurable across studies, and coercing “d =
  0.42 (0.11, 0.73)” to a number is the shape of fault that turned “120
  (60 per arm)” into 12060.

  Two fields the other templates lack, because they are what a synthesis
  argues from. `measure` is how the construct was measured, which is
  what surfaces the strongest thing a review can say: that a
  disagreement about effect size is a disagreement about measurement.
  `limitation` is the one the *authors* state, in their words, so it is
  quotable and therefore checkable, unlike a limitation a model infers.

  `finding` is coded relative to **your** question rather than to an
  intervention (`supports` / `contradicts` / `mixed` /
  `no clear finding` / `not applicable`), which is what lets a
  non-interventional literature have contradictions at all, and it is
  why an unedited template is now refused. A run could previously screen
  a whole corpus against “REPLACE: the population the review is about”
  and extract against “REPLACE THIS with your review question”, paying
  in full for a table framed by an instruction to supply the framing.
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
  [`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)
  and
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
  now refuse a protocol whose question or criteria still begin with
  `REPLACE`, before anything is spent. The check is anchored on the
  whole word: there are real trials called REPLACE, and a review of one
  is not a template.

- **[`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md),
  [`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
  and
  [`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md):
  a review written from an argument instead of from rows.**
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)’s
  unit was the section, and sections came from an `outline` fixed before
  the reading; each was then drafted independently from the whole study
  table. Three things followed that no downstream editing could repair.
  The structure was the author’s hypothesis rather than a finding, and
  the strongest sentence a review contains is often structural. Studies
  arrived as rows, so the model wrote row by row: “Smith

  2019. found X. Garcia (2022) found Y.” And nothing computed relations
        between studies, which is what synthesis *is*.

  [`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
  turns the extraction table into statements about the literature, each
  naming the studies that support it, the studies that contradict it,
  and the field that distinguishes them. Every number is verified
  against the table, the check `cited_ids()` already makes on finished
  prose, moved one link earlier: an id that is not there is dropped and
  counted, a claim left with no supporting study is dropped entirely,
  and a `moderator` naming a column the table does not have is cleared,
  because an invented explanation for a real disagreement is the most
  convincing error this layer can make. `$dropped` records all of it, so
  a claims table that looks thin can be told from a literature that is.

  The study numbers are the ones
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
  cites, because both derive them from one function, and passing claims
  drawn from a different table is refused rather than trusted. Nothing
  downstream could detect that on its own: the numbers would all be
  valid and all mean other studies.

  [`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
  derives the sections from the claims and hands them back as an
  ordinary `outline` you can accept or replace, verifying that every
  claim lands in exactly one section. `gr_synthesise(claims = )` then
  gives each section its own claims and only the studies those claims
  rest on, ordered by breadth so a twelve-person pilot stops getting the
  same space as a two-thousand-person trial, and marks a section partial
  if it was handed a claim and did not write it up.
  [`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md)
  computes what the corpus does not contain (a declared category nobody
  studied, a dimension with no variation, an unreplicated claim, a
  disagreement nothing explains) in R, with no model call, so the gap
  list is a fact that can be checked by counting rather than an
  impression.

  The `claims` protocol from earlier in this release is the schema this
  wants as input, and
  [`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md)
  gained a claims section so one document reads claim → studies → quotes
  → pages. Its new `claims` and `records` arguments come after all of
  0.5.0’s, so a positional call written against 0.5.0 binds as it did.

  Ordering studies for emphasis deliberately does **not** rank designs.
  That would assert a cohort study beats a qualitative one, which is a
  methodological claim this package has no standing to make, and adding
  per-item scores into a total is what Cochrane says plainly is
  discouraged. Design is a grouping variable: what distinguishes the
  sides of a disagreement, and what
  [`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md)
  crosstabs. A principled weighting waits on an appraisal instrument.

- **Three revision passes, and a guard the citation check cannot make.**
  A revision must not change the citations and must not arrive
  truncated. Both checks are necessary and neither is sufficient,
  because the most damaging thing an editing pass does leaves the
  citations exactly where they were. Editing for impact means deleting
  hedges, and the hedges are where the uncertainty lives: “three small
  trials suggest a modest benefit” comes back as “trials show a
  benefit”, same markers, same studies, a claim the evidence does not
  carry.

  So revision is three passes (`"structure"`, `"cut"`, `"register"`),
  each forbidden from doing the others’ job, each run on what survived
  the last, and each measured for escalation. A revision is discarded if
  it introduces a universal quantifier that was not there, uses a
  booster stem more often than the draft did, or carries fewer hedges
  than the draft’s rate implies.

  All three are matched on whole words, so “outcomes improved” is not
  the booster “prove”, and the hedge “unproven” is not a booster at all.
  Boosters are compared by count, not by which stems are present, so
  saying “demonstrates” three times where the draft said it once is
  introducing it.

  The hedge test scales with how much claim-bearing prose survived, so a
  shorter revision may carry fewer hedges, but never none if the draft
  had any. What it does not promise is that every legitimate cut passes:
  removing the most heavily hedged sentence lowers the rate and is
  refused, leaving the draft standing. Hedges and universals are
  measured on sentences carrying a citation; boosters on all the prose
  except headings, because uncited framing between the claims is where
  “the evidence demonstrates a clear benefit” actually gets written.
  `coherence = TRUE` runs all three; name them to run fewer. A draft too
  long to leave room for its own rewrite is skipped rather than sent.

- **`iterative` was cutting the wrong end of what it had gathered, and
  never fitted its final prompt at all.** Two faults in the one reader
  that accumulates context across rounds.

  It pasted everything gathered and called
  [`gr_truncate_tokens()`](https://elkronos.github.io/readgpt/reference/gr_truncate_tokens.md)
  on the result, which keeps the head, so the chunks from the *most
  recent* round were the ones dropped. In a reader whose whole premise
  is “that did not answer it, go and get more”, it discarded the
  material it had just decided it needed. And it cut mid-chunk, which
  breaks the audit chain: a chunk cut in half no longer contains the
  sentence an answer quotes from it, so `verified` comes back false for
  a reason that has nothing to do with the document. Now it keeps whole
  chunks, ranked by the score that justified taking each one, so
  everything reaching the prompt is intact and still checkable.

  Worse, `chunks_used` and the evidence table were built from everything
  *seen*, including chunks truncation had removed from the prompt. The
  answer claimed support from text no model had been shown. Only what
  reached the prompt is reported now, `notes$chunks_dropped` says how
  much did not, and a run that dropped anything is partial.

  And the final answer call was never budgeted. Every other reader sizes
  its prompt to the context window; this one rendered everything
  gathered, so several rounds of `top_k` chunks went out over the window
  and the provider rejected the call after the entire loop had been paid
  for. When no gathered chunk fits one prompt, the loop stops and the
  reader returns `NOT_IN_DOCUMENT` marked partial.

- **The question is repeated at both ends of a long prompt.**
  `answer_messages()` already asked last, which is the position that
  gets followed, but over several thousand tokens of excerpt an
  instruction that appears once at the bottom is a long way from the
  top. It now also appears before the body when the body is long enough
  to bury it, inside the existing message rather than as a new one so
  nothing indexing those positions shifts. The `iterative` step prompt
  and
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)’s
  section prompt asked *first* and then handed over the bulk; both now
  ask again at the far end.

  `restate` is a \[gr_read_spec()\] setting (`"auto"`, `"always"`,
  `"never"`) rather than a rule, because whether repeating the question
  helps is a question about a particular corpus and model.
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  can take two recipes differing only in it and measure the difference,
  which is the honest way to settle it. The same goes for
  `context_order = "edges"`, which has been in the package since 0.4 on
  the strength of a published finding and has never been measured here.

- **Every prompt is now budgeted against the prompt that is actually
  sent.** The readers size their excerpts with
  `gr_budget(overhead = ...)`, and parts of the prompts were not in that
  arithmetic: `cite = TRUE` sends a longer system prompt than
  `cite = FALSE` and every cited read budgeted for the short one, and
  the `iterative` step prompt adds 88 tokens of instruction to the
  answer system prompt and budgeted for the answer prompt alone. The
  prompt then overran the window by exactly the amount nobody counted,
  and the provider refused the call after the run had been paid for.

  The system prompt is now built once and used for both the budget and
  the call wherever the two could drift, and `prompt_overhead()` counts
  the question twice unless restatement is off, because the question is
  restated at the far end of a long prompt (above). Reserving room that
  goes unused costs a little context; under-reserving cost the answer.

- **Six connections between the stages of a review.**

  *The run has its own ceiling.* `gr_options(max_calls =)` is per
  document by design (one enormous document must not starve the rest),
  and on its own that left the run unbounded: five documents under a
  30-call ceiling made 125 calls, and the only corpus ceiling,
  `max_total_usd`, is unenforceable against a model with no registered
  price and is checked only after a document has been paid for.
  `max_total_calls` is checked *before* each document, so the overshoot
  is bounded by one document’s own ceiling. With neither ceiling set,
  the run now says once what its worst case is instead of leaving you to
  multiply.

  *[`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
  stops when the ceiling says so.* One model call per section, and
  neither it nor the batched path checked `trace_can_call()`, so a run
  that had already spent its ceiling kept writing sections, one call
  each. A section the ceiling stops is marked partial and says why,
  rather than appearing as empty prose.

  *A folder and its file list behave the same.* `gr_read_many(dir)`
  skipped the files no extractor claims; `gr_read_many(list.files(dir))`
  handed each of them to an extractor and recorded a failed row. Every
  pipeline uses the second form (`gr_extract(screened$included)` is a
  character vector), so the stage that reads the most documents was
  getting the worse behaviour. Both forms now skip those files and warn,
  as described above. The filter applies only when every element is a
  file that exists, so raw text, a mixed vector and a
  [`list()`](https://rdrr.io/r/base/list.html) of sources are all
  untouched, and a `gr_records` still produces a failed row per
  unreadable file rather than dropping it. A record that was retrieved
  and could not be read is a fact a PRISMA count needs to keep. When the
  filter removes *everything*, the run aborts and says so.

  *The search travels with the corpus it produced.* `gr_screening` and
  `gr_extraction` carry the
  [`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md)
  they were run over, and
  [`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md)
  picks it up, so the report’s search section and flow diagram do not
  depend on handing the same object over a second time.
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  also takes the screening object itself, which is what carries the
  search across the hand-off: a character vector of paths cannot, so
  `gr_extract(screened)` keeps the search and the bibliographic fields
  while `gr_extract(screened$included)` keeps neither.

  *A review can be one trace.*
  [`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md),
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  and
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
  take a `trace =`, so the stages of one review add up to one figure.
  Each used to start its own, and nothing said what the review cost.

  The argument is a **parent**, not the stage’s own counter, and that
  distinction is the whole design: a `gr_trace` is both the ledger of
  what a run did and the counter `max_calls` is measured against, and
  those want opposite things. Each stage runs on its own trace (so
  `max_calls` limits each stage as it always has, and each stage’s
  `$trace` reports that stage alone) and folds into the parent at the
  end, exactly as each document inside
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
  does.

  *How good the screening is reaches the report.*
  `gr_audit_report(calibration = )` writes what
  [`gr_calibrate()`](https://elkronos.github.io/readgpt/reference/gr_calibrate.md)
  measured (sensitivity, specificity, kappa and the list of eligible
  studies the screener threw away) as a section.

  Not done, and said plainly rather than left to look like an oversight:
  the corpus loop still reads documents one at a time. The reason given
  for that, that a trace does not survive being sent to a worker, was
  stale, since the parallel helper already builds a trace per worker and
  absorbs them in order. The real obstacle is that duplicate detection,
  the resume store and both run-level ceilings are order-dependent, and
  a parallel loop would have to serialise on each. Within a document,
  `gr_options(parallel = TRUE)` already applies.

- **[`print()`](https://rdrr.io/r/base/print.html) says why an answer is
  partial, and what it cost.** Printing an answer showed “(PARTIAL)”
  with no reason, and the evidence only as chunk numbers. It now names
  what went wrong (requests that failed, with the first error; chunks
  that did not fit; pages never read; a fallback), shows the cost beside
  the token counts, gives each piece of evidence with its page and
  section, and says “Not found in the document” in words. `print(trace)`
  shows the cost too. `ans$answer` still holds the `NOT_IN_DOCUMENT`
  sentinel for code to test.

- **Warnings stay with the result.** A warning printed at the console is
  gone once a script moves on, and in a run over a folder it cannot be
  tied to a document. The warnings raised while a document is ingested,
  cut and read are now kept in `doc$warnings` and `ans$warnings`, named
  by class. A document served from the ingestion cache still carries
  them,
  [`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md)
  writes them, and
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)’s
  summary has a new last column, `warnings`, filled for failed documents
  too. They still print as before.

- **[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md)
  says what this installation can read**, in two new columns: `needs`,
  the packages an extractor cannot run without, and `available`, whether
  they are installed.

- **PDFs are read in reading order.**
  [`pdftools::pdf_text()`](https://docs.ropensci.org/pdftools//reference/pdftools.html)
  returns a page as it looks, so a page in two columns came out with
  every line joining the two columns, the running head and foot landed
  in the text at each page break, and a PDF had no sections. The PDF
  extractor now reads two columns one after the other, drops short lines
  repeated at the top or bottom of many pages, and marks headings, taken
  from the PDF’s bookmarks or recognised as a line that is only a
  standard section name (“Introduction”, “2. Methods”). `structural`
  chunking, `preview` and evidence locations now have sections to work
  with. `gr_ingest_spec(layout = "raw")` keeps each page as it is laid
  out. The setting is stored only when it is not the default, so the
  cache and store keys of existing specs do not change.

- **[`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
  picks a recipe from the document’s length.** The default recipe is now
  `"auto"`. A document of at most 50,000 tokens that fills no more than
  half of the room one request leaves for it is read with `"fast"`,
  which sends the whole document at once; the limit is lower on a model
  with a small context window. A longer document is read with
  `"thorough"`, as every document was before. `"thorough"` sends each
  chunk in a request of its own, so a 60-page report took some 30
  requests where one would do. Both send every chunk, so the choice
  changes the number of requests and the cost, not how much of the
  document is read. The room is measured for the recipe’s model and for
  the client’s, and a model whose limits are a guess always gets
  `"thorough"`. `ans$recipe` names the recipe used,
  `ans$notes$auto_recipe` records that `"auto"` chose it, the trace
  records the token count and the limit, and a replay repeats the
  recorded choice. Pass `"thorough"` to keep the old behaviour.
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md),
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  and the review functions refuse `"auto"` (class `gr_bad_recipe`): a
  corpus read with a different recipe per document has no one
  configuration to report. `"auto"` is reserved as a reader name for the
  same reason.

- **The spending limit is checked against what a run spends.**
  `max_cost_usd` was checked once, before the first request, against an
  estimate that priced every reply at its maximum length: a run was
  refused at an estimate of \$5.54 that cost \$0.11, and a run that did
  start was never checked again. Now each request is priced as it is
  recorded, and a run stops once what it has spent reaches the limit,
  returning a partial answer with
  \`notes\$cost_cap_reached`, which prints as "stopped at the $5 spending limit". The cost of a request is known only once it is made, so a run can pass the limit by one request. This applies wherever the call cap is checked: every reader, and the review stages that call a model, which now warn when a limit stopped them (`gr_claims_capped\`).

  A run is refused before it starts only when it cannot finish under the
  limit: when its reader sends every chunk and sending them once already
  costs more, priced at the model that receives them, `skim_model` and
  `summary_model` included. Under a limit of 0, a run whose model has a
  price is refused and a model registered at no cost runs. A parallel
  read that sends batches is the exception: a batch cannot be stopped
  part way, so the read is still held to its worst case before it
  starts, priced at the dearest model it uses, and no batch is sent once
  the run has reached a limit.

  In
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md),
  and so in
  [`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)
  and
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
  a document a limit stopped before it was read in full is `"failed"`,
  with the limit in `error` and the partial answer in `$answers`. It is
  not written to `store`, so a resumed run with a higher limit reads it
  again, and an extraction table does not report the fields in its
  unread part as absent.

  `trace$stop_reason` says which limit stopped a run, `"calls"` or
  `"cost"`, and `trace$spent_usd` holds what it has spent. A stop
  belongs to the read it happened in, so a later read on the same trace
  is not marked partial for it. Requests a limit stopped are no longer
  counted in `notes$failed_calls`: a run that spent its budget is not
  reported as having had its requests fail.

- **Word files keep their tables, notes and headings.** The Word
  extractor read every paragraph element in the document, so each table
  cell became a paragraph of its own and footnotes and endnotes were
  never read. It never found a heading either: the style was read
  without its namespace, which always gave nothing, so no Word file had
  sections, and the style ID it meant to match is one Word translates
  (“berschrift1” in a German document). A table row is now one block of
  kind `"table"`, its cells joined by ” \| “, including rows a content
  control wraps; a table inside a cell is read as rows of its own. A
  footnote or endnote is a block of kind `"footnote"` placed after the
  paragraph that cites it and numbered as the text cites them. Headings
  are found by style name, which Word does not translate, or by outline
  level, so a custom heading style based on a built-in one counts too;
  `structural` chunking and `preview` now have sections to work with.
  Text in a text box is read once, where it could appear four times,
  twice glued to the paragraph beside it; text a tracked change moved is
  read once rather than twice; and a non-breaking hyphen or a symbol
  inserted from the Symbol font is kept, so”10-15%” and “p ≤ 0.05” no
  longer lose their hyphen and sign.

- **A web address is a document.**
  [`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md),
  and so
  [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
  and
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md),
  now download an address starting `http://` or `https://` and read it
  with the extractor for what came back: a specific type the server
  declares, else the extension in the address, else the file’s first
  bytes, which settle a PDF, a Word file or an HTML page however they
  were labelled. A download no extractor reads is refused
  (`gr_unsupported_format`) rather than read as text, and a failed
  download is an error of class `gr_url_error`. The document’s `source`
  is the address and a corpus row is labelled with it. An address ending
  in `.pdf` used to be reported as a missing file, and any other address
  was read as a one-line document about itself.

- **Long reads show how far they have got.** In an interactive session
  with `verbose` on, a reader that sends a request per chunk keeps one
  line up to date with how many chunks it has read and what the run has
  spent, and removes the line when it finishes. `refine` does the same.
  A model with no registered price makes the line say the cost is
  unknown rather than show a figure that is too low.
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
  gives what the run has spent before each document. Scripts and knitted
  documents print nothing new.

- **One row per request.**
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) on a
  trace returns a row for each request: the document and recipe it
  belonged to, its stage, model, whether it succeeded and whether it
  came from a cache, tokens in and out, what it cost, how long it took,
  any error, and the prompt and reply. The trace now records each
  request’s time as `seconds`, and the `usd` column adds up to the total
  in
  [`gr_trace_cost()`](https://elkronos.github.io/readgpt/reference/gr_trace_cost.md).

- **The passages behind an answer, on one page.**
  [`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md)
  takes a `gr_answer` from
  [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
  or a `gr_corpus` from
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md),
  as `answer`. The page gives the question, the answer or that it was
  not found, whether it is partial and why, the cost, and the passages
  in document order with page, section and chunk. A quotation is
  highlighted where it stands in its chunk and flagged when it is not
  there; in a chunk sent whole, the answer’s numbers are highlighted; an
  answer written for one chunk is labelled as the model’s words. A long
  passage is cut to the text around what is highlighted. A corpus gets
  one row per document, then each answer. A new argument, `open`, shows
  any report once it is written, in the RStudio viewer or a browser; it
  is on by default in an interactive session.

- **A website.** The README, the guides, the reference for every
  function (grouped by task, with the first version’s entry points under
  “Superseded”) and this changelog are built into
  <https://elkronos.github.io/readgpt/> by a new workflow on every push
  to main. Pull requests build the site without publishing it, so a
  guide that fails to knit is caught before it is merged.

- **Guides that start from the beginning.**
  [`vignette("readgpt")`](https://elkronos.github.io/readgpt/articles/readgpt.md)
  is now a getting-started guide that assumes no experience with
  language models: tokens, context windows and chunks explained,
  installing, connecting to a provider, and a first question checked
  from start to finish.
  [`vignette("ingest")`](https://elkronos.github.io/readgpt/articles/ingest.md)
  covers every file format, OCR, each cleaner and preset, and surveying
  a folder with
  [`gr_inventory()`](https://elkronos.github.io/readgpt/reference/gr_inventory.md);
  [`vignette("readers")`](https://elkronos.github.io/readgpt/articles/readers.md)
  explains every reading strategy, compares them on one document, and
  gives a way to choose. The previous vignette continues as
  [`vignette("tour")`](https://elkronos.github.io/readgpt/articles/tour.md),
  and all of them run offline.

- **The README describes what the package now does.** The revision
  passes and the claim-strength guard, the claims layer, `restate`, and
  a short section on the registries you can query without spending
  anything
  ([`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md),
  [`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
  [`gr_reader_signature()`](https://elkronos.github.io/readgpt/reference/gr_reader_signature.md)
  and the rest; fourteen exported functions appeared in neither the
  README nor the vignette) are documented there for the first time, and
  the detail on ingestion and reading strategies has moved into the
  guides.

- **Plain punctuation in the help pages and messages.** The help pages,
  and the errors, warnings and printed summaries, no longer use a double
  hyphen as a dash. The prompts sent to models are unchanged, so
  recorded runs still replay.

### Fixed

- **A missing API key no longer looks like an answer.** Without a key
  every request failed on its own, and the answer came back as
  `NOT_IN_DOCUMENT` marked partial, which reads as “the document does
  not say”. The reason was only in the trace.
  [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md),
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
  and
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  now check for a credential before reading the document and stop with a
  `gr_auth_error` that says how to set `OPENAI_API_KEY`. Anything else
  stops at its first request, including the embeddings path, which used
  to fall back to lexical vectors. A client that authenticates through
  `headers`, mock, backend and replay clients, and a client with a
  response cache attached are not stopped up front.

- **Text that never reached a model makes the answer partial.** A PDF
  page that needed OCR and did not get it (the OCR packages missing, or
  OCR failing on that page) only raised a warning, once, and not at all
  when the document came from the cache, so the answer was not partial.
  Those pages are now listed in `doc$stats$unread_pages` and
  `ans$notes$unread_pages` and make the answer partial, and
  `stats$pages` counts them. `hierarchical` cutting its summaries to
  fit, and `refine` cutting an excerpt or its draft, now mark the answer
  partial too, as `preview` already did for a cut skim. A custom
  extractor can report its own unread pages in
  `attr(result, "gr_unread_pages")`.

- **A path with a mistyped extension is flagged.**
  `"reports/2024/annual-report.pfd"` is read as text, because the
  extension is not one readgpt reads, and the answer that followed was
  about a file name with nothing to say so. A one-line string with a
  directory separator and an unknown extension now raises
  `gr_path_as_text`.

- **A segmenter’s fallback gives one warning.**
  [`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
  decided before a segmenter ran whether to add its own
  `gr_segment_fallback` warning, from a list of built-in names. A
  registered segmenter that warned about its own fallback, as
  [`?gr_register_segmenter`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md)
  advises, got a second warning, and a silent one registered under a
  built-in’s name got none. The generic warning now comes after the
  segmenter has run, and only when it raised no warning of that class
  itself.

- **[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  answers carry what
  [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
  answers carry.** They had no `$document` and did not resolve evidence
  to pages. All three entry points now finish an answer the same way.

- **A reader reports no evidence for an answer it did not get.**
  `stuff`, `retrieve`, `skim`, `rerank`, `iterative` and `preview`
  listed the chunks they had gathered as the evidence even when the
  answer request failed, so a failed run printed “Evidence: chunk 1”
  under “Not found”.

- **`hierarchical` keeps the summaries it has paid for.** When a level
  of summarising produced nothing, because every request in it failed,
  the reader answered from nothing. It now answers from the previous
  level’s summaries, cut to fit if they must be.

- **`rerank` could answer with no document in front of it.** The
  relevance score was coerced with
  [`as.numeric()`](https://rdrr.io/r/base/numeric.html), and
  `as.numeric("high")` is `NA`. An `NA` comparison used as a subscript
  selects rather than drops, so the “nothing scored high enough” guard
  did not fire, `[chunk NA]` reached the prompt, and the model answered
  from its own prior with `partial = FALSE`. A failed scoring call, or a
  reply with no score, counted as a score of 0, which passes
  `rerank_min_score = 0` as if a model had judged the chunk. Now a
  failed call, or a score that is missing or unusable, judges nothing:
  if no candidate was judged, the reader falls back to the BM25 ranking
  and warns `gr_rerank_degraded`, as it already did when every call
  failed; if only some were, the answer (`NOT_IN_DOCUMENT` included) is
  marked partial.

- **A ceiling that cannot be compared no longer means no ceiling.**
  [`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)
  checked names only, and each consumer made up its own mind about a bad
  value. `is.finite(max_cost_usd)` as a guard meant that `NA`, or `"5"`
  read from a config file, removed the cost cap entirely, and
  `max_calls = "400"` was enforced by the pre-flight estimate and
  ignored by the per-call check. Values are now checked where they are
  set. `max_cost_usd` and `max_calls` refuse anything that is not a
  single non-negative number (`NULL` and `Inf` still mean no limit). The
  tuning settings (`safety_margin`, `min_output_tokens`, `max_retries`,
  `retry_pause_base`, `request_timeout`, `workers` and `temperature`)
  read a number written as text as that number, warn and keep their
  current value for one they cannot read, and clamp one outside the
  range the package can use, with a warning. The Shiny app checks its
  cost-cap field before setting it.

- **A missing setting falls back to its default, not to the end of its
  range that does harm.** `clamp()` maps `NA` to the bottom of a range,
  and for several settings the bottom is what the setting exists to
  prevent: `gr_budget(safety_margin = NA)` budgeted with no headroom at
  all, an `overhead` of `NA` counted the system prompt as free and
  pushed the input budget up, and `semantic_percentile`,
  `semantic_window` and `proposition_batch_tokens` fell to their
  minimums. A missing percentile cut at the median boundary rather than
  the 90th, multiplying the chunks and the calls, and
  `gr_budget(reserve_output = NA)` left the answer one token. These now
  take their default, with a warning;
  [`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)
  takes the session’s `safety_margin`, and refuses an overhead it cannot
  read as a single number, since no default is safe there.
  `gr_segment_spec(max_tokens = NA)` and
  `gr_synthesise(max_section_tokens = NA)` fall back to their documented
  default of 1200, rather than 800 and 1500.
  `gr_ingest_spec(min_chars = NA)` failed every document with “missing
  value where TRUE/FALSE needed”. And `as_int1()` tested its range after
  the coercion that creates the `NA`, so
  `gr_screen(screen_tokens = 3e9)` marked every document failed.

- **A model’s reply is read by its exact keys.** The readers read parsed
  replies with `$`, which partial-matches: a reply carrying `decisions`
  satisfied a read of `decision` and the screener recorded an “include”
  from a key the schema never defined, `can_answer_now` ended the
  `iterative` loop with its answer taken as final, and `choices`
  answered for the conflict resolver’s `choice`. Replies are now read
  through one accessor that matches exactly and copes with the shapes
  `jsonlite` gives the same JSON on different days: a two-element score
  crashed `rerank`, and an array of arrays of propositions was flattened
  column by column and so written into the document transposed.

- **Proposition segmentation no longer loses text.** A batch whose call
  failed, or whose reply held nothing usable, was dropped: one bad batch
  in ten silently removed a tenth of the document from everything
  downstream, and only a run in which every batch failed noticed. Such a
  batch is now kept as written, with a `gr_segment_fallback` warning,
  and counted in `$extra$batches_kept_as_written`. A reply shaped as a
  list of objects was written into the document as the literal R
  expression `c("A.", "B.")`. Each object is now read through its text
  key when it has one and through every string when it has none, so an
  `id` or a page label beside a proposition does not become one.

- **Two values that differ past the seventh digit are two values.**
  Conflicts between parts of one document were found by comparing
  [`format()`](https://rdrr.io/r/base/format.html)ed values, which keep
  seven significant digits, so 3000000001 and 3000000002, or 0.123456789
  and 0.123456781, were one value and the conflict went unreported.
  Values are compared, and shown to the model that adjudicates a
  conflict, in full.

- **An integer above 2^31 is stored, not lost.** An `integer` field
  holding a value above `.Machine$integer.max` (a count of person-days,
  say) became `NA`, so the table said the document had not reported a
  figure it stated plainly. Such a value is stored as a double.

- **A `NULL` override is refused rather than silently replaced.**
  `answer_document(f, q, "fast", max_tokens = NULL)` deleted the
  recipe’s value, so the constructor’s default replaced it (segmenting
  at 1200 tokens instead of the recipe’s 4000) with no warning, and the
  trace’s `settings` no longer recorded which value was used. `NULL` is
  accepted only for a setting whose default is `NULL` (`model`,
  `temperature`, `skim_model` and the like) and is otherwise refused
  with `gr_bad_override` before anything is read.

- **The citation check did not recognise the form the prompt asks for.**
  The synthesis prompt asks the model to “cite more than one where more
  than one supports it”, and the check matched only `[study 1]`. So a
  section citing studies as `[studies 1 and 2]` was reported as citing
  nothing (an audit report printed its prose above the line “This
  section cites nothing.”), and `[studies 1 and 99]` over three studies
  passed as clean, where `[study 99]` is correctly flagged. A fabricated
  citation slipping past the fabrication check is the exact failure this
  pipeline exists to prevent. The check and the new citation renderer
  share one grammar, `cite_pattern()`, and a drift test asserts that
  anything the renderer rewrites, the check has seen.

- **A number that appears in no paper could pass the evidence check.**
  Coercing a value to `integer` or `number` stripped every character
  that was not a digit, which deleted the separators along with the
  words: “120 (60 per arm)” became **12060**, “482 (Table 1)” became
  4821, and “1,204 randomised; 1,180 analysed” became 12041180. The
  quote these arrived with was verbatim, so the evidence check passed,
  `n_unverified` stayed 0, and the audit report certified the figure. A
  value carrying more than one number is now a miss, which is counted
  and reported; there is no reading of “120 (60 per arm)” under which
  12060 is better than nothing.

- **Three cache keys could return another request’s answer.**
  `gr_hash()` was blind to any list name nested below the third level:
  `str(max.level = 3L)` stops printing there, and
  [`as.character()`](https://rdrr.io/r/base/character.html) drops names,
  so `use.names = TRUE` was inert. Two JSON schemas differing only in a
  leaf’s name hashed identically, and the schema is part of the
  response-cache key: asking for `{result:{headcount}}` after
  `{result:{revenue}}` returned the revenue figure, marked
  `cached = TRUE`. `temperature` was hashed before it was resolved from
  [`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md),
  so a temperature sweep through one cache directory explored nothing
  and reported the first sample’s answer for every setting. And
  `embed_cache_key()` omitted the endpoint, so two clients with
  different `base_url` and the same model id shared one vector space,
  the failure the embedder name was added to prevent, one field over.
  The corpus store gained the tokenizer, which is what turns
  `max_tokens` into an actual chunk boundary. All four keys are
  versioned, so old entries are not mixed with new ones.

- **A replay reports the `finish_reason` the live call returned.** The
  trace did not record it, and
  [`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
  hard-coded it to `NA`, so anything that decides on it decided
  differently on replay: `gr_synthesise(coherence = TRUE)`, which
  discards a revision that stopped for “length”, would keep it on
  replay, produce a different review, and report 0 misses, certifying
  itself as an exact reproduction of a run it had not reproduced.

- **The “running sequentially instead” fallback aborted one line
  later.** `gr_lapply()` called `fn(item)` without the trace on that
  branch, and lazy evaluation hid it until the worker’s first line, so
  `parallel = TRUE` without the `future` packages, which is a default
  install, died after ingestion, segmentation and any calls already paid
  for. Workers also now receive the registries: `future.packages`
  re-runs `.onLoad()`, which registers the built-ins only, so a model
  registered with
  [`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md)
  was unknown in the worker and the parallel run used a different output
  ceiling from the sequential one.

- **A failed batch was dropped from a section that then reported itself
  complete.** With more studies than fit one prompt,
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
  drafts in batches and merges; `tree_merge()` strips empty pieces, so a
  merge over the survivors read exactly like a merge over everything,
  and with one survivor it returned that piece unchanged with
  `ok = TRUE`. The studies in the failed batch were simply absent. The
  section is marked partial now and says so.

- **Three constructs that did not mean what they said.**
  `sprintf("%d", median(tokens))`:
  [`median()`](https://rdrr.io/r/stats/median.html) returns a double on
  an even-length vector, and
  [`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
  auto-prints, so the canonical interactive call failed on about one
  document in four. `clamp()`’s `length(x) == 0L || is.na(x)` reached a
  vectorised [`is.na()`](https://rdrr.io/r/base/NA.html) for any setting
  that was not one number, stopping every read in the session with a
  message naming neither the setting nor the function. And
  [`gr_ingest()`](https://elkronos.github.io/readgpt/reference/gr_ingest.md)
  gated [`file.exists()`](https://rdrr.io/r/base/files.html) behind a
  no-newlines test, so a file whose name contains a newline was never
  looked for and the path string itself became the document: status
  “ok”, `partial = FALSE`, the model answering about a filename.

- **Six tests were passing without testing anything.** Each is now
  written so that deleting what it names makes it fail:
  `on_error = "stop"` asserted only that *an* error was raised, against
  a fixture that always fails, and survived the feature being replaced
  by an unconditional [`stop()`](https://rdrr.io/r/base/stop.html); the
  ingest cache-key test used inputs with different content, so it would
  have passed with the key collision restored;
  `expect_error(class = "gr_error")` passes on every condition this
  package raises, because that is the base class; the `as_json`
  round-trip survived both methods being replaced by the string `"{}"`;
  “writing leaves no temporary files” was satisfied by an empty
  directory; and the v1 compatibility shims were checked only for
  returning a string, so pointing all four at one reader left the suite
  green.

- **A provider that omits its usage block no longer makes the call
  free.** `parse_response()` collapsed a missing, null or non-numeric
  `usage` (several OpenAI-compatible local servers omit it, and a
  gateway can strip it) to 0 tokens in and 0 out, so a real call was
  recorded and costed as nothing. That is the same fault 0.5.0 fixed in
  `ellmer_usage()` and
  [`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md),
  still alive on the path every HTTP user takes. The parser now reports
  NA and the call settles NA against the local count, as the ellmer
  adapter already did: the prompt was measured before it was sent and
  the reply is in hand, and the local tokenizer is biased to over-count,
  so the substitute errs towards charging too much. A figure the
  provider did report is kept as reported, including a genuine zero.

  Two neighbours of the same shape: a handler-built `gr_result` that
  left `usage` out took `list(0, 0)` where a bare-string reply from the
  same handler was counted locally, and
  [`gr_trace_cost()`](https://elkronos.github.io/readgpt/reference/gr_trace_cost.md)
  turned an unknown step count into 0 before summing, so a trace read
  back from a file with one unknown step reported a confident, too-small
  dollar figure beside an NA token total. Both now say unknown; the
  report renders that as a dash, where zero rendered as a price.

- **The two readers whose distinctness is a loop were never tested
  looping.** Line coverage over the whole suite found every line after
  `iterative`’s retrieve-assess loop dead, and `hierarchical`’s
  reduction body never executed once. The cause was the shared test
  client: it answers the iterative prompt with `can_answer: true` on
  round one, so the loop always stopped immediately, and every fixture
  document was small enough that one summarise pass always fit. Under
  those conditions the two readers claiming the most in their registered
  signatures (`topk|rounds*2|forward` and `all|N+tree+1|tree`) behaved
  as `retrieve` and `map_reduce`. The existing call-count assertion for
  `hierarchical` was `n + 1`, which is the arithmetic of the
  non-recursive case: the expectation had been written around what the
  fixtures happened to produce.

  Driven by hand both paths were correct, so nothing shipped broken. But
  a signature is a claim about behaviour, and nothing was checking that
  the behaviour happened. Two steering clients now do: one that refuses
  to answer so the loop runs, one whose summaries stay over a small
  context window so the tree has to fan in. The new tests assert rounds,
  distinct queries, accumulated chunks, tree depth, the `max_levels` cap
  and its warning. Each was confirmed to fail against a deliberately
  broken build before being kept.

  Sweeping the same way found three settings that no test ever set to a
  non-default value: `skim_model` and `summary_model` (the routing that
  sends the cheap per-chunk pass to a cheaper model, which is a headline
  cost feature) and `rerank_min_score`. All three are now checked, and
  reader coverage rose from 75.1% to 82.5%.

## readgpt 0.5.0

### New

- **[`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md):
  the run, written out so somebody else can check it.** One
  self-contained HTML file: the protocol as fixed in advance, what
  happened to every document, every extracted value with the sentence
  and page it came from and whether that sentence is really there, what
  was written and which rows each claim rests on, and what the whole
  thing cost. No new dependencies; pass whichever stages you ran.

  Everything in it was already recorded, across four objects and half a
  dozen data frames, which is why in practice nobody looked at it. This
  is not for the person who ran the review, who can index into
  `$evidence`. It is for the reviewer, co-author or regulator who did
  not, and whose question is “how do you know?”

  **It does not flatter the run.** Unverified quotes, documents that
  could not be read, screening calls the model declined to make, fields
  nothing supported and citations pointing at rows that do not exist are
  all counted near the top. An audit that showed only what worked would
  look like diligence and be the opposite of it.

  **And it says what the checking does not establish.** That a quoted
  sentence occurs in the chunk it was credited to is not evidence that
  it supports the value taken from it, nor that the value is right. What
  the check rules out is the quote having been invented, which is the
  failure that is otherwise invisible. A verification column a reader
  over-reads is worse than no column.

### Fixed

- **An unknown token count is no longer costed as a free one.**
  [`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md)
  summed with `na.rm = TRUE`, so `gr_estimate_cost(model, NA, NA)`
  returned `0`: a run whose size nobody knew, reported as having cost
  nothing. That is the defect
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
  was fixed for in 0.3.0, in a different function. Unknown now comes
  back `NA`, so a total cannot quietly omit it; `NULL` still means none,
  because a length-zero sum really is zero.

- **A provider that reports no usable token count falls back instead of
  reporting zero.**
  [`gr_ellmer_client()`](https://elkronos.github.io/readgpt/reference/gr_ellmer_client.md)
  reads `chat$get_tokens()`, whose shape varies by provider. A column
  that was present but held `NA` (or text, which some providers give)
  summed with `na.rm = TRUE` to `0`, which is finite, so the fallback to
  the local estimate never fired and a real call went into the trace as
  having spent no tokens.

  Its two tests were also the only ones in the suite that always
  skipped, because CI never installed `ellmer`. It does now, and the
  first time they ran, one failed: its stub carried three of the five
  methods the adapter requires and its own comment said “the three
  methods”, a drift nothing could catch while the test skipped
  everywhere. The stub is now built against the requirement list, and a
  new test asks the real
  [`ellmer::Chat`](https://ellmer.tidyverse.org/reference/Chat.html)
  whether it has those methods rather than asking a stub written to
  match.

  Running it then surfaced a second fault in the same stub: its
  `clone()` copied the bindings but not the closures, so the copied
  methods still wrote to the *original*. `clone$set_turns()` emptied the
  caller’s turns and left the clone’s untouched. That is exactly
  backwards, and it made the adapter look as though it mutates a chat it
  does not. The stub is a factory now, so a clone is a new object whose
  methods close over itself, and it checks that isolation as it is built
  rather than leaving it to surface as a confusing expectation later.

- **Two more fixtures that could drift from the source, now guarded.**
  Sweeping for the same shape found the shared test mock branching on
  phrases lifted from three real prompts. Reword one and the mock
  silently stops matching, returns its generic answer, and the `rerank`
  and `iterative` tests keep passing against the *degraded* path,
  because both fall back gracefully on output they cannot parse. A test
  that quietly changes what it tests is worse than one that fails. The
  guard reads the phrases out of the fixture rather than repeating them,
  so it cannot fall behind what it guards.

  And `.gr_evidence_kind`, which decides whether a reader’s quotes are
  checked at all: a reader missing from it falls back to “verbatim”,
  meaning text copied out of the document and so never verified. Every
  reader was present, but nothing said so; now something does. A stale
  entry for `page`, which is a segmenter rather than a reader, is gone.

- **[`gr_flow()`](https://elkronos.github.io/readgpt/reference/gr_flow.md)**
  returns the same counts as a data frame: sources given, duplicates
  removed, screened, included, excluded, unclear, unreadable, extracted,
  and values with no verbatim span. Every source is accounted for at
  every stage it reached, so the arithmetic closes.

## readgpt 0.4.2

### Fixed

- **A misspelt reader or segmenter setting is no longer swallowed in
  silence.**
  [`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md)
  and
  [`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md)
  keep a `...` so a custom reader has somewhere for its own settings:
  `fields`, `include`, `screen_tokens` and the rest all arrive that way.
  A typo landed there too: `gr_read_spec("retrieve", topk = 8)` was
  accepted in full, stored a `topk` nothing reads, and left `top_k` at
  its default of 6. The run then worked perfectly and answered a
  different question.

  A `...` name one edit from a real setting, or the same name in
  different case or punctuation, now warns and says which setting it
  looks like. It still goes through, because the escape hatch has to
  stay open for a field that is new, and
  [`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)
  continues to refuse an unknown name outright, since it has no such
  escape hatch to keep. R’s own partial matching already resolved the
  prefix cases (`cit` finds `cite`); this covers the rest.

## readgpt 0.4.1

### Fixed

- **`parallel = TRUE` no longer under-reports the run it speeds up.** A
  worker is a separate process, so the trace it was handed was a copy
  and every call it recorded was thrown away with it. A six-chunk
  `map_reduce` reported one call instead of seven;
  [`gr_trace_cost()`](https://elkronos.github.io/readgpt/reference/gr_trace_cost.md)
  under-reported the bill in proportion to how parallel the run was. The
  answers were correct throughout, which is what made it hard to notice.

  Each worker now keeps its own trace and the parent absorbs them, in
  input order rather than completion order, so a parallel run reports
  exactly what the same run made sequentially reports: the same calls,
  steps, tokens and cost.

  Two things still do not cross the process boundary, and both are now
  documented: `gr_options(max_calls =)` is checked per worker while a
  fan-out is in flight, so the pre-flight estimate is what bounds a
  parallel run; and a client that keeps its own log in a closure, such
  as
  [`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md),
  only sees the calls made in this process. Ask the trace.

  There were no tests for `parallel = TRUE` in the suite at all, and CI
  did not install `future`, so any that had existed would have skipped.
  Both fixed.

## readgpt 0.4.0

Reading a corpus for an answer and reading it for a *table* are
different jobs. This release is the second one.

### New

- **Extraction schemas.**
  [`gr_fields()`](https://elkronos.github.io/readgpt/reference/gr_fields.md)
  describes what you want out of a document as typed fields rather than
  as a sentence
  (`gr_field("Number of participants randomised, not the number analysed", type = "integer")`),
  and
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  applies one schema to a whole corpus, returning one tidy row per
  document and one column per field, of that field’s type.

  The point is joinability. A paragraph about one paper cannot be
  compared with a paragraph about two hundred others; a table can be
  sorted, counted, filtered and published.

- **A new reader, `extract`.** Traversal signature
  `all|N+conflicts|none`: every chunk is asked to fill what it can and
  to leave the rest null, and the per-chunk answers are then reconciled.
  Reconciliation is arithmetic when the chunks agree, so it costs a call
  only where a document contradicts itself. `resolve = "model"`
  adjudicates those, `resolve = "first"` (the default) takes the earlier
  value and records the disagreement.

- **Every filled cell carries its provenance.** Each value is asked for
  the sentence it came from, and that sentence is checked against the
  chunk it was attributed to. `$evidence` is the long form of that (one
  row per supported cell, with `verified` and `match`), and
  `n_unverified` in the table counts the values that could not be tied
  to a verbatim span, whether because no quote was given or because the
  quote is not in the document. Nothing is discarded for failing;
  `require_quote = TRUE` makes it a policy for a protocol that needs
  one.

- **Protocols.**
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md)
  writes down the three decisions a review must not make while it reads:
  which documents count (`include`/`exclude`), what to collect from them
  (a
  [`gr_fields()`](https://elkronos.github.io/readgpt/reference/gr_fields.md)
  schema), and what the write-up has to cover (`outline`). Not because a
  model cannot infer them, but because a criterion invented while
  reading is a criterion fitted to what was found.

  It is the seventh registry:
  [`gr_protocols()`](https://elkronos.github.io/readgpt/reference/gr_protocols.md)
  lists what is available,
  [`gr_register_protocol()`](https://elkronos.github.io/readgpt/reference/gr_register_protocol.md)
  adds your own, and
  [`gr_protocol_save()`](https://elkronos.github.io/readgpt/reference/gr_protocol_save.md)
  /
  [`gr_protocol_read()`](https://elkronos.github.io/readgpt/reference/gr_protocol_save.md)
  round-trip one through a JSON file so it can be shared, diffed and
  cited alongside the results. Three templates ship (`bibliography`,
  `evidence_table` and `systematic_review`) as starting points, not
  standards: the package knows the shape a protocol has, not what your
  criteria should be.

  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
  takes a protocol wherever it takes a schema, using its question and
  recipe unless you say otherwise.

- **Screening.**
  [`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)
  is the stage before extraction: one model call per document, a
  decision and a reason for every one, and nothing dropped on the way.
  Two hundred candidate papers extracted in full is two hundred times
  twenty calls; screened, it is two hundred calls, and most of them end
  the document’s involvement.

  Three properties it is built around. Every document gets a decision:
  there is no retrieval step or relevance prefilter that could quietly
  remove a source before one is recorded, and a document that could not
  be read is `status = "failed"` with no decision rather than a silent
  absence. `"unclear"` is an answer, not a failure: forcing a binary
  decision out of an excerpt that does not settle the question is how
  automated screening loses studies, and those documents are for a
  person. And every decision names the criterion that produced it,
  because `table(x$table$criterion)` is what a flow diagram asks for.

  `screen_tokens =` caps what the model is shown, counted from the start
  of the document, so title-and-abstract screening is available
  deliberately rather than by accident; `truncated` and `seen_tokens`
  always say what was actually read. `x$included` is the argument to
  hand to
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md).

  The reader behind it is `screen`, traversal `head|1|none`.

- **Synthesis.**
  [`gr_synthesise()`](https://elkronos.github.io/readgpt/reference/gr_synthesise.md)
  writes the review up from the extraction table, one section per call,
  against the outline the protocol fixed in advance, so the structure is
  not shaped by whatever happened to be found, and a section can be
  rewritten without redoing the rest.

  Every section is written with `[study 3]` markers, and the markers are
  parsed back out and checked against the rows that exist. A citation to
  a row that is not there is reported and marks the section partial. It
  is the same check
  [`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)
  runs on `[chunk 3]`, for the same reason. `$citations` resolves every
  marker to its `document` and `document_id`.

  Duplicates and unread rows never reach the write-up: a study counted
  twice is the error the rest of this release exists to prevent, and it
  is easiest to make here, where the rows all look alike. A table too
  large for one prompt is written in batches and merged rather than
  truncated.

  That completes the chain: a sentence cites a study, the study’s row
  cites a quote, and the quote was checked against the page it is
  attributed to. None of it proves the sentence is true. It makes every
  step of the way back to the document short enough to walk.

- **The same document is read once.** A source whose cleaned text
  repeats one already read this run is not read again: its row is filled
  in from the first copy, `status` is `"duplicate"` and the new
  `duplicate_of` column names the row it repeats. Nothing is dropped
  (every source you passed still has a row), so
  `subset(x$summary, is.na(duplicate_of))` is the deduplicated set and
  `sum(!is.na(x$summary$duplicate_of))` is the number to report as
  removed.

  A response cache already made the second copy’s calls free. What it
  could not do was stop the duplicate appearing in the results as a
  second, independent document, which is how one study gets counted
  twice in a synthesis. The hash travels in the `store`, so a resumed
  run does not pay to rediscover it.

- **A citable document id.** `document_id` is the hash of a document’s
  cleaned text, in the corpus summary, the extraction table and the
  evidence table. `document` is a filename: it changes when the file is
  renamed, collides between folders, and does not exist for a document
  passed as text. The id is the same string for the same document in
  every run and on every machine, and identical for two copies of it,
  which is the same fact as the duplicate detection above.

### Fixed

- **A quote now cites the page it is on.** A chunk is packed from
  several units, and a chunk packed from two pages reported the *first*
  one: right for its opening sentence, wrong for everything after it,
  and wrong in the most expensive way, because a citation that names a
  page is checked by turning to that page. Two changes: a chunk whose
  units disagree about the page (or the section) now reports `NA` rather
  than picking one, and an evidence span is located in the document’s
  own blocks, so it gets the page of the block that contains it rather
  than the page of the chunk that carried it. A span found on several
  pages, or not found at all, is left alone, since the first hit would
  be a guess dressed as a fact.

  Same for a runt paragraph absorbed into the previous chunk, which used
  to keep only the host’s page.

### Behaviour

- `gr_call_json()` gains `allow_empty`, used only by `extract`, where a
  JSON object with no keys is a real answer (this excerpt supports none
  of the fields) rather than a broken one.

- A field cannot be named `document`, `document_id`, `status`,
  `duplicate_of`, `error`, `n_filled`, `n_unverified`, `conflicts`,
  `field`, `chunk_id`, `page`, `section`, `quote`, `verified` or
  `match`: those are the extraction table’s own columns, and a field
  with one of those names would be silently overwritten. Nor may a name
  end in `__quote`, which collides with the companion span every field
  gets.

- [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
  returns `$sources`: the sources as they were read, aligned row for row
  with `summary`. `summary$document` is a display label (a basename,
  made unique with a suffix when two folders hold the same filename),
  and there is no way back from it to a file, which is what any caller
  feeding part of a corpus into the next stage needs.

- [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)’s
  summary gains `document_id` and `duplicate_of`. A `store` written by
  0.3.0 still resumes; its rows have neither, and both are filled with
  `NA` rather than being invented.

- A field a document does not report comes back `NA` with `status`
  `"ok"`, not `"failed"`. “Not reported” is a finding; the `status`
  column is what separates it from a document that was never read.

## readgpt 0.3.0

Two additions, one theme: the model call is the only part of this
package that costs money or fails to repeat itself, and neither of those
had to be true twice.

### New

- **A response cache.**
  [`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
  stores each successful model response, keyed on the exact request, and
  [`gr_cache_client()`](https://elkronos.github.io/readgpt/reference/gr_cache_client.md)
  attaches it to any client. A repeated request is free and
  byte-identical, so iterating on a prompt, resuming after a crash, or
  re-running an analysis whose last step changed no longer re-bills
  every earlier call. The key covers the messages, model, output cap,
  temperature, JSON schema, API shape and base URL, and nothing else,
  because nothing else reaches the model. Failures are never cached: a
  rate limit or a refusal is a property of the moment, and storing one
  would make a blip permanent.
  [`gr_cache_stats()`](https://elkronos.github.io/readgpt/reference/gr_cache_stats.md)
  and
  [`gr_cache_clear()`](https://elkronos.github.io/readgpt/reference/gr_cache_clear.md)
  report on and empty a cache.

  The default directory is under
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html), so a cache costs
  nothing and disappears with the session; set
  `gr_options(cache_dir = ...)` to keep entries across sessions and make
  a long run resumable.

- **Quoted evidence is checked against the document.** `ans$evidence` is
  what an answer rests on, and for most readers those spans are verbatim
  chunk text, true because the package put them there. For `skim` they
  are what the model wrote when asked to extract the relevant passages:
  *presented* as quotations, with nothing checking that they were.

  Now they are checked, on every run. A span that is not in the chunk it
  is attributed to sets `notes$unverified_evidence` and makes the answer
  `partial`, like any other degradation.
  [`gr_verify_evidence()`](https://elkronos.github.io/readgpt/reference/gr_verify_evidence.md)
  reports the detail: `chunk_id`, `kind`, `verified`, `match` and the
  span.

  The comparison is forgiving about typography and unforgiving about
  content. Whitespace, curly quotes, dashes, case and the punctuation a
  model wraps a quotation in are folded away, because none of that is
  fabrication and flagging it would make `partial` mean nothing. A
  changed number is not folded away. Below an exact match, `match` is
  the fraction of the span carried by its longest consecutive run in the
  source. It is a run measure rather than word overlap, because overlap
  cannot tell a quotation from a paraphrase built out of the same
  vocabulary, which is the whole distinction.

  Citations are checked too, for every reader: an answer citing a chunk
  that was never sent to it sets `notes$cited_unknown` and is `partial`.
  A fabricated citation is more convincing than a fabricated answer,
  because it looks like the thing that would let you check.

  Both checks are local string operations on text already in hand. They
  cost nothing, so there is no option to turn them off.

- **A vignette**,
  [`vignette("readgpt")`](https://elkronos.github.io/readgpt/articles/readgpt.md).
  It walks through the three axes and the decision each one represents,
  then through comparing recipes, the cost rails, caching, replay and
  reading a corpus. It builds against
  [`gr_mock_client()`](https://elkronos.github.io/readgpt/reference/gr_mock_client.md)
  with `gr_options(embedder = "lexical")`, so it compiles offline,
  deterministically and with no API key, on your machine and on CRAN’s
  alike.

- **`mmr` and `context_order` on the read spec**, both off by default.

  `mmr` below 1 selects chunks by maximal marginal relevance (relevance
  traded against redundancy with what is already selected), so three
  paragraphs saying the same thing do not take all three top-k slots and
  pay for each other. It costs nothing, the vectors are already
  computed, and it applies to `retrieve` and `iterative`. `mmr = 1` is
  exactly top-k, bit for bit.

  `context_order` decides where the selected chunks sit in the prompt:
  `"relevance"` (default), `"document"`, or `"edges"`, which puts the
  strongest first and second-strongest last and buries the weakest in
  the middle, because transformers attend measurably better to the
  beginning and end of a long context than to its middle. Selection is
  unaffected: this is placement only, for `retrieve` and `rerank`. It is
  not the primacy/recency effect it resembles: those come from rehearsal
  and interference in human memory, which a transformer does not have.

- **An embedder registry.**
  [`gr_register_embedder()`](https://elkronos.github.io/readgpt/reference/gr_register_embedder.md)
  and
  [`gr_embedders()`](https://elkronos.github.io/readgpt/reference/gr_embedders.md)
  make embedding the sixth registry, alongside extractors, cleaners,
  segmenters, readers and models. It was the one axis that was a chain
  of [`inherits()`](https://rdrr.io/r/base/class.html) branches, so
  adding a local model meant editing the package and there was no way to
  ask what was available. `gr_options(embedder = )` switches every part
  of the package that embeds; two built-ins are registered, `"api"` and
  `"lexical"`.

  The registry carries something a branch cannot: whether an embedder is
  **deterministic**. That closes the replay gap. A replay now reproduces
  a run’s chunk ranking exactly when the recording used a deterministic
  embedder *and* the replay uses the same one. Both conditions are
  checked against the embedder the trace recorded. Determinism alone is
  not enough: replaying an API-embedded run with a deterministic local
  embedder would compute vectors the original never saw while reporting
  itself exact.

  The embedding cache key now includes the embedder. Without it,
  switching embedders returned the previous one’s vectors for the same
  text and model: two vector spaces silently mixed in one matrix, and a
  cosine similarity across them means nothing.

- **One question, many documents.**
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
  runs one recipe over a vector of files, or a directory expanded by the
  extractor registry, and returns one row per document: `answer`,
  `not_found`, `partial`, `chunks_used`, `calls`, `cached`, tokens,
  `cost_usd`, `seconds`, `status` and `error`. It is the counterpart to
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md),
  which runs several recipes over one document.

  It does four things a loop does not. One unreadable document costs one
  row rather than the run. Budgets are per document (each gets its own
  trace, so `max_calls` applies as if it had been read alone and one
  enormous file cannot starve the rest), under an optional corpus-wide
  `max_total_usd` ceiling, past which documents are marked `"skipped"`
  rather than quietly dropped. A `store =` directory makes a run
  resumable: each result is written as it completes and restored later,
  keyed on the document’s path, size and mtime, the question, the whole
  pipeline and the model, so an edited document is a new job and not a
  stale hit. And every document’s cost is recorded.

- **[`gr_trace_cost()`](https://elkronos.github.io/readgpt/reference/gr_trace_cost.md)**
  prices a run using each step’s own model and counts only the calls
  that were issued. A call served from a cache or a replay spent nothing
  however large its prompt was. One row per model; an unpriced model
  contributes `NA` rather than zero, so a total cannot quietly omit it.
  This is the missing half of the `cached` accounting added alongside
  the cache: token totals describe a run’s shape, and this describes its
  bill.

- **A swappable transport.**
  [`gr_backend_client()`](https://elkronos.github.io/readgpt/reference/gr_backend_client.md)
  makes any function of `(messages, params)` the model transport, and
  [`gr_ellmer_client()`](https://elkronos.github.io/readgpt/reference/gr_ellmer_client.md)
  plugs in an `ellmer` chat, so Anthropic, Google, Bedrock, Azure,
  Ollama and Hugging Face work with every reading strategy here. Nothing
  about ingesting, segmenting or reading a document depends on one HTTP
  dialect, and the package should not be reimplementing a transport
  layer R already has. Everything built around the call is unchanged:
  context budgeting, cost and call rails, provenance, traces, caching,
  replay and
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md).

  Backends may also supply an embedding function; without one, readers
  that embed fall back to lexical vectors and warn
  (`gr_backend_no_embeddings`). A supplied embedder is checked for one
  row per input before its output reaches the ranking maths. A
  wrong-shaped matrix would associate every chunk with another chunk’s
  vector and the answer would look fine.

  Two ellmer limits are documented rather than papered over: sampling
  parameters belong to the chat object, so a per-call `temperature` is
  ignored and warned about once (`gr_ellmer_temperature`); and each call
  runs against a fresh deep clone with its turns cleared, so no history
  leaks between chunks and the caller’s chat is never mutated.

- **Reproducible replay.**
  [`gr_trace_save()`](https://elkronos.github.io/readgpt/reference/gr_trace_save.md)
  writes a run’s trace to a file and
  [`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
  answers from it, so a recorded run can be re-run exactly by someone
  with no API key and no budget. A trace already held every prompt and
  every response; it was write-only. Now a published result is something
  a reader can check rather than trust, and a bug report can be a
  re-runnable recording instead of a description.

  A prompt with no recorded response raises `gr_replay_miss` rather than
  inventing an answer. Silent divergence would produce a result that
  looks like the original and is not. Embeddings are not model calls and
  are not recorded, so `retrieve` and the `semantic` segmenter fall back
  to lexical vectors under replay and warn (`gr_replay_no_embeddings`).

- **Cached calls are accounted separately.** `gr_trace` gains a `cached`
  counter, and
  [`gr_trace_summary()`](https://elkronos.github.io/readgpt/reference/gr_trace_summary.md)
  a `cached` column, so `calls - cached` is what a run actually paid
  for. `gr_result` gains `$cached`. Without this a fully cached run
  reported the same token totals as a fully paid one and
  [`gr_estimate_cost()`](https://elkronos.github.io/readgpt/reference/gr_estimate_cost.md)
  quietly overstated the bill.

### Fixes

- [`gr_cache_clear()`](https://elkronos.github.io/readgpt/reference/gr_cache_clear.md)
  was two different functions. An internal helper of that name in
  `R/core-state.R` cleared the in-memory document and embedding caches,
  and R collates that file after `R/core-cache.R`, so the internal
  definition silently replaced the exported one. The package would have
  shipped the wrong implementation under the right documentation. The
  internal helper is now `gr_flush_caches()`, and a test asserts the
  shadowing has not returned.

- **A trace could not survive being written to a file.** `jsonlite`
  escapes bytes it cannot interpret in the *current locale*, so an
  unmarked string holding UTF-8 bytes came out of
  [`as_json()`](https://elkronos.github.io/readgpt/reference/as_json.md)
  as the literal text `"caf<c3><a9>"` on any machine whose locale is not
  UTF-8. The bytes were always right; nothing had told R what they were.
  `trace_record()` now labels prompt, response and error text with
  `mark_utf8()`, which labels and converts nothing, never
  [`enc2utf8()`](https://rdrr.io/r/base/Encoding.html), which corrupts
  valid UTF-8 in exactly this situation. This was present in 0.2.0 and
  is the same defect class as the locale-dependent tokenizer fixed
  there: correct bytes, absent label, one locale in the test matrix.

- **`gr_result$text` is now always labelled.** An unlabelled response
  was at the mercy of the session locale the moment anything serialised,
  compared or counted characters in it, so two runs that produced
  identical bytes could compare unequal. Both cache and replay keys
  normalise text the same way, which is what lets a saved trace replay
  on a different machine.

- **A missing or unusable setting falls back to its documented
  default**, rather than to whichever end of its range happens to be the
  lower bound. `gr_read_spec(max_answer_tokens = NA)` used to give 16,
  truncating every answer, and `top_k = NA` gave 1, because `clamp()`
  maps an unusable value to `lo`. That is right for a bound and wrong
  for a setting. Applies to `mmr`, `top_k`, the three token caps,
  `rerank_candidates`, `rerank_min_score`, `fan_in`, `max_levels`,
  `max_rounds` and the segmenter’s `max_tokens`, and to anything
  unusable, not only `NA`.

- **An embedding that *fell back* now sets `partial`.** The
  documentation says to check `partial` before trusting an answer and
  lists a lexical fallback as a degradation; it was recorded in `$notes`
  but not in the one flag readers are told to look at. A fallback is a
  degradation, and `gr_options(embedder = "lexical")` is a choice. The
  two are now distinguished, and only the first sets the flag.

- **A citation of a chunk that was sent but did not contribute is no
  longer reported as a fabrication.** `notes$cited_unknown` compared
  citations against `chunks_used`, which for a per-chunk reader holds
  only the chunks that *answered*, so a model faithfully citing a chunk
  that had replied “not in this excerpt” was flagged as inventing it,
  and the answer was silently downgraded to `partial`. A false positive
  in a hallucination check is the one place a false positive is least
  affordable.

- **`context_order = "document"` was a silent no-op below three
  chunks**, the common case for a top-k reader. Only `"edges"` needs a
  middle to bury the weakest chunk in.

- **Warnings raised while evaluating a setting were silently
  swallowed.** `clamp()` did `x <- suppressWarnings(as.numeric(x))`, and
  `x` arrives as a promise, so forcing it inside
  [`suppressWarnings()`](https://rdrr.io/r/base/warning.html) discarded
  everything raised while *evaluating the argument*, not merely the
  coercion warning that call exists to quiet. Every `clamp(f(...))` in
  the package lost `f()`’s warnings, across
  [`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md),
  [`gr_segment_spec()`](https://elkronos.github.io/readgpt/reference/gr_segment_spec.md)
  and
  [`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md).
  The argument is now forced first. Present in 0.2.0.

- **[`gr_options()`](https://elkronos.github.io/readgpt/reference/gr_options.md)
  did not compose with
  [`on.exit()`](https://rdrr.io/r/base/on.exit.html), which is the one
  thing its documentation promised.** The “old” value it returns was
  built with [`modifyList()`](https://rdrr.io/r/utils/modifyList.html),
  which deletes a key whose value is `NULL`, so once an option had been
  *stored* as `NULL`, which is exactly what restoring a saved list does
  for `temperature`, `max_cost_usd`, `cache_dir` and `embedder`, the
  returned name came back as `NA` and the next `gr_options(old)` failed
  with “Unknown option(s): NA”. The second use of the documented
  pattern, in a function already fixed once for this same `NULL` trap in
  its setter.

- `ensemble` combined its members’ evidence with a plain
  [`rbind()`](https://rdrr.io/r/base/cbind.html), which requires every
  member to produce the same columns. It does not (only readers whose
  evidence is model-written carry verification columns), so an ensemble
  of `skim` and `map_reduce` failed with “numbers of columns of
  arguments do not match” the moment verification was added. Evidence
  tables are now unioned.

- [`gr_call()`](https://elkronos.github.io/readgpt/reference/gr_call.md)
  had two exits, each recording its own trace entry. Anything that
  needed to sit between a request and its response had to be written
  twice and kept in step by hand. It now dispatches once and records
  once, and the normalisation that enforces the `gr_result` invariants
  on whatever a handler returned is shared by the mock and backend paths
  rather than duplicated.

### Behaviour changes

- [`gr_trace_summary()`](https://elkronos.github.io/readgpt/reference/gr_trace_summary.md)
  returns an extra `cached` column, between `calls` and `steps`. Code
  that indexes its result by position rather than by name will need
  updating.

- `min_score` is applied to `retrieve` *before* selection rather than
  after. Applied after, a chunk below the floor could displace one above
  it in the top-k and then be dropped, quietly returning fewer chunks
  than `top_k` asked for and giving no way to see why. A run with a
  finite `min_score` may now use more chunks than it did.

## readgpt 0.2.0

A rewrite. The package is now a proper R package with three independent,
registry-based axes (**ingest**, **segment**, **read**) instead of five
entangled “modes”. Old entry points still work and warn once.

### Why the rewrite

The five reading modes were not five methodologies. `Chunked` and
`Semantic` called the same function with the same arguments on the same
chunk object and returned byte-identical answers while billing twice;
`MultiPass` re-ran two of the others verbatim; `Hierarchical` was
single-level map-reduce that overflowed the context window past roughly
32 chunks. The “semantic” ordering came from
`set.seed(nchar(text)); runif(768)`, so the embedding was a function of
string length alone and two 48-character strings scored a cosine
similarity of 1.0.

Distinctness is now a property the package can **check**. Every reader
declares a traversal signature (`select|calls|state`), and
[`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
refuses to bill you twice for two recipes that resolve to the same work.

### New

- **Three axes, each a registry.** 6 extractors, 14 individually
  toggleable cleaners, 9 segmenters and 9 readers, in any combination.
  [`gr_recipe()`](https://elkronos.github.io/readgpt/reference/gr_recipe.md)
  binds one of each;
  [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
  runs one;
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  runs several over one document and reports how they differ.
- **A public extension API.**
  [`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md),
  [`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
  [`gr_register_segmenter()`](https://elkronos.github.io/readgpt/reference/gr_register_segmenter.md),
  [`gr_register_reader()`](https://elkronos.github.io/readgpt/reference/gr_register_reader.md)
  and
  [`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md),
  with
  [`new_chunks()`](https://elkronos.github.io/readgpt/reference/new_chunks.md)
  and
  [`new_answer()`](https://elkronos.github.io/readgpt/reference/new_answer.md)
  to build what a custom segmenter or reader must return. Additions get
  the same token-cap enforcement, provenance handling, cost caps and
  reporting as the built-ins.
- **Overlap and minimum chunk size**, on every segmenter. The previous
  release had neither, so an answer straddling a boundary was lost by
  both chunks.
- **Provenance.** Extraction returns blocks carrying page, section and
  block id, and that survives cleaning and chunking, so `ans$evidence`
  can point back at where an answer came from.
- **A run trace.** One trace per run records every prompt, response,
  token count and local step, so the trace always explains the answer
  next to it. The previous Shiny app ran the pipeline twice per question
  and displayed the reasoning of a *different* generation.
- **Pre-flight cost and call caps**, checked before the first request:
  `gr_options(max_cost_usd =, max_calls =)`.
- **A pluggable, script-aware tokenizer**, biased to over-count, with an
  exact `tiktoken` backend when reticulate and Python `tiktoken` are
  available.
- **A data-driven model registry** with explicit match precedence and an
  `as_of` stamp, extensible with
  [`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md)
  for compatible endpoints.
- **[`gr_chunk_stats()`](https://elkronos.github.io/readgpt/reference/gr_chunk_stats.md)**,
  so you can compare chunkings for free before spending anything on
  reading.

### Behaviour changes that will alter results

Three are deliberate. If you need the old behaviour for comparison, the
`"legacy"` recipe reproduces it.

- `mode` no longer defaults to running all five modes.
  `answer_question(f, q)` ran 41 API calls; it now runs one pipeline.
- Modes no longer share a chunk object, so selecting a second one cannot
  change the first one’s answer.
- **Digits are no longer stripped from documents by default.**
  `remove_numbers` was `TRUE` and unreachable from the public entry
  point, so every figure, date and percentage was deleted before the
  model saw the document.

### Deprecated

[`answer_question()`](https://elkronos.github.io/readgpt/reference/answer_question.md),
[`parse_text()`](https://elkronos.github.io/readgpt/reference/parse_text.md),
[`gpt_read_chunked()`](https://elkronos.github.io/readgpt/reference/gpt_read_chunked.md),
[`gpt_read_retrieval()`](https://elkronos.github.io/readgpt/reference/gpt_read_retrieval.md),
[`gpt_read_hierarchical()`](https://elkronos.github.io/readgpt/reference/gpt_read_hierarchical.md)
and
[`gpt_read_multipass()`](https://elkronos.github.io/readgpt/reference/gpt_read_multipass.md)
still work and warn once per session. Each help page names its
replacement.

### Fixes

Too many to list individually; `REVIEW.md` documents each with a
reproduction. The ones most likely to have affected real runs:

- An unrecognised model id produced a **negative** token budget, which
  turned a 10,000-word document into roughly 10,001 API calls and fed it
  to the model backwards.
  [`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)
  is now the single arithmetic chokepoint and cannot return a
  non-positive value; it raises an actionable error instead.
- `grepl("gpt-4", model)` matched `gpt-4o`, treating a 128k-context
  model as 8k and over-chunking by ~26x.
- The boilerplate filters were dead code: digit removal ran before the
  page/figure patterns that need digits to match. Cleaning steps are now
  staged so structure-aware steps always precede destructive ones.
- The reference-section cleaner deleted a body sentence and kept the
  whole bibliography.
- The ingestion cache key was the file path alone, so the first settings
  used in a session won for the rest of it, which made fine-grained
  control, the point of the package, unreachable.
- A failed API call was spliced into the next prompt as the literal
  string `"NULL"`; an empty completion crashed every mode.
- `refine = TRUE` could never work: it called a function that was not
  defined anywhere in the repository.
- The Shiny app stored the API key process-wide, so two browser sessions
  in one R process billed each other, and exposed the entire filesystem
  to the browser.
- **The same document produced different results on different
  machines.** [`enc2utf8()`](https://rdrr.io/r/base/Encoding.html)
  treats an *unmarked* string as native, so in a non-UTF-8 locale it
  re-encoded bytes that were already valid UTF-8. Downstream, the
  ligature and smart-quote cleaners stopped matching and
  [`utf8ToInt()`](https://rdrr.io/r/base/utf8Conversion.html) fell back
  to counting bytes, charging a one-codepoint character as three, a 50%
  token swing on the affected line. Token counts, chunk boundaries,
  budgets and cost estimates all varied with the locale R happened to
  start in. Text is now labelled before any conversion, and a test
  asserts that ingestion and segmentation are byte-identical under a C
  locale.
- [`answer_document()`](https://elkronos.github.io/readgpt/reference/answer_document.md)
  and
  [`gr_compare()`](https://elkronos.github.io/readgpt/reference/gr_compare.md)
  called [`basename()`](https://rdrr.io/r/base/basename.html) on
  whatever was passed as `source`. Raw document text is a length-1
  character vector, so the whole document went to
  [`basename()`](https://rdrr.io/r/base/basename.html), which is bounded
  by `PATH_MAX` and warns past it on macOS (1024 bytes, so most
  documents), and which put a mangled fragment of the document into the
  trace where the filename belongs.

### Testing

627 tests, all offline against a recording mock client, plus GitHub
Actions running `R CMD check` on three platforms and a fast suite that
also executes every README code block and diffs its documented output
against real output.
