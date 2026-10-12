# Write a review from an extraction table

The last stage. Takes what
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
found and writes it up section by section, against an outline fixed in
advance, with every section citing the rows it rests on.

## Usage

``` r
gr_synthesise(
  extraction,
  protocol = NULL,
  outline = NULL,
  question = NULL,
  client = NULL,
  model = NULL,
  max_section_tokens = 1200L,
  temperature = NULL,
  include_unclear = FALSE,
  cite_style = c("auto", "marker", "author-year", "numeric"),
  bib = NULL,
  style = NULL,
  coherence = FALSE,
  references = TRUE,
  claims = NULL,
  gaps = NULL,
  trace = NULL,
  effort = NULL
)
```

## Arguments

- extraction:

  A `gr_extraction` from
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
  or a data frame shaped like its `$table`.

- protocol:

  A
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md);
  its `outline` and `question` are used unless you give them directly.

- outline:

  The sections, as a named character vector: names are headings, values
  say what that section has to cover. As
  [`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md).
  A heading with a blank brief is dropped, unless the claim assignment
  (see `claims`) places claims in it: then its heading is its brief.

- question:

  The review question, for framing.

- client:

  A `gr_client`.

- model, max_section_tokens, temperature:

  Overrides for the writing calls.

- include_unclear:

  Write from rows whose extraction was incomplete. Off by default: a row
  with nothing in it contributes nothing but its own absence, and the
  count of skipped rows is reported either way.

- cite_style:

  How citations appear in the finished prose. `"auto"` (the default)
  names the studies when the table can name all of them and uses markers
  when it cannot. `"author-year"` asks for names and warns if they
  cannot be produced; `"numeric"` gives `(1, 2)`; `"marker"` leaves
  `[study 1]` as written.

  The model always writes `[study N]`, whatever this is set to, and the
  rendering happens afterwards from the table. That is deliberate: a
  marker can be checked exactly against the rows that exist, whereas
  verifying an author-year string would mean matching a name the model
  wrote against a name in the table, and near-misses (Smith for Smyth,
  2019 for 2018) are both the errors that matter and the ones fuzzy
  matching forgives. A rendered citation is therefore a fact about the
  extraction rather than something the model asserted.
  `$sections$text_marked` and `$text_marked` keep the marker form so the
  check can be re-run on the published prose.

- bib:

  Which columns carry bibliographic identity, as a named list of
  `citation`, `authors`, `year`, `title`, `venue`, `doi`. Omitted, the
  conventional names are looked for, which is why
  `gr_protocols("bibliography")` works without configuration: `citation`
  (or `cite`, `citation_key`, `cite_key`), `authors` (or `author`),
  `year`, `title`, `venue` (or `journal`) and `doi`, in any case. Names
  that are as often findings – `date`, `source`, `url`, `published`,
  `publication` – are not taken for bibliographic ones unless named
  here. A column found this way is withheld from the writing prompts and
  used for the citations and references; give a role as `NA` to keep a
  column of that name as a finding (`bib = list(venue = NA)`). The most
  reliable route is a `citation` field asked for during extraction:
  parsing an arbitrary author list is a heuristic, and where it cannot
  be done confidently the run falls back to markers rather than printing
  a name that may be wrong.

  Pass the same `bib` to
  [`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
  and
  [`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md).
  The claims model is shown every column its own `bib` did not withhold,
  so a claim, and the moderator it names, can carry a column this
  write-up withholds, and the claims reach the writing prompts as they
  are. With `claims` drawn under a `bib` that withheld less than this
  one, the run warns (`gr_claims_bib_mismatch`) and names the columns.

- style:

  A register instruction appended to the writing prompts, such as
  `"formal academic; hedge claims; past tense for findings"`. It governs
  how sections are written, never what they may say: the rules about
  citing every claim and inventing nothing hold whatever voice is asked
  for.

- coherence:

  Revision passes over the finished draft: `TRUE` for all three, `FALSE`
  for none, or any of `"structure"` (reorder and merge), `"cut"` (remove
  repetition) and `"register"` (polish sentences) by name. Each pass is
  forbidden from doing the others' job, and each is discarded (with
  `$draft` kept, and the reason in `$coherence`) if it changed which
  studies are cited, wrote a citation the check cannot read, cited a
  study more often than the draft did, or took the citation marker off a
  claim sentence that stays (each a `gr_coherence_rejected` warning); if
  it arrived truncated (`gr_coherence_truncated`); if it strengthened a
  claim (`gr_revision_escalated`); or if it introduced a figure the
  draft did not have (`gr_revision_new_figure`): one the draft gives in
  no form, so a figure changed or rounded counts, and 0.61 written as
  .61 does not. Off by default: each is a call, and each is a chance for
  a model to touch finished prose.

  The `"cut"` pass is also discarded when the cut pass did not shorten
  the review (`gr_revision_uncut`): its revision must have strictly
  fewer words of prose than the text it was given, headings and citation
  markers left out of the count. Fewer by any amount will do, since how
  much a draft can lose depends on how much it repeats itself. Only the
  cut is held to a length: a polish that keeps every sentence can remove
  half the words of padded prose, so no bound on the other two passes
  would tell their job from cutting. Their counts are in `$coherence`
  all the same.

  The check that refuses a revision which strengthens a claim reads
  English hedges and boosters, so a draft it judges not to be in English
  is not revised at all: a warning (`gr_revision_unguarded`) says so, no
  pass is sent, and every pass is reported with `ran = FALSE` and that
  reason. The judgment counts letters outside the Latin alphabet and
  common function words, so a very short draft can be misjudged: an
  English one naming an accented author left unrevised, or a sentence or
  two of a language that has no accents taken for English.

- references:

  Append a `## References` section built from the studies the finished
  text actually cites. Alphabetical under `"author-year"`, numbered by
  study otherwise: the list is labelled by whatever the prose uses to
  point into it. The alphabetical order follows a fixed rule rather than
  the session's locale, so it is the same on every machine: it ignores
  case, accents and apostrophes, filing an accented name with its base
  letter. Two papers by the same authors in the same year are lettered
  (2019a, 2019b) in title order; a key that does not end in a year is
  lettered after a hyphen ("in press-a"). Under `"author-year"`, an
  entry whose key came from a `citation` field leads with that key, so
  the prose can be followed to it.

- claims:

  A
  [`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
  result. With it each section argues that section's claims and sees
  only the studies those claims rest on, rather than being handed every
  study and writing a paragraph per row. Needs an `outline` from
  [`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md),
  which carries the assignment, and it must be an outline derived from
  these same claims: the run stops (`gr_claims_mismatch`) when the
  outline says it came from other claims, and (`gr_claims_unplaced`)
  when the assignment places a claim in a heading the outline does not
  have – a heading renamed after
  [`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md),
  say; rename it in `attr(outline, "claims")$section` too. A section
  given no claims is written from the rows, as without `claims`, except
  the closing section, which is written from `gaps` alone and is left
  out when there are none. A section too large for one prompt is written
  a few claims at a time, each batch sent only its claims' studies, and
  merged.

- gaps:

  A
  [`gr_gaps()`](https://elkronos.github.io/readgpt/reference/gr_gaps.md)
  result, or lines of text. Given to the closing section
  [`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
  named, `attr(outline, "closing")`, with an instruction to state those
  gaps and no others. When the outline has no such section they reach
  none, and a warning (`gr_gaps_unused`) says so. They are cut, at a
  line, to a quarter of the model's input room, with a warning
  (`gr_gaps_truncated`), and the closing section is then partial.

- trace:

  A
  [`gr_trace()`](https://elkronos.github.io/readgpt/reference/gr_trace.md)
  to fold this write-up's accounting into, as in
  [`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md).
  It is a parent, not this stage's counter: `$trace` is still the
  write-up's own, so `gr_options(max_calls =)` bounds the write-up
  rather than being spent by the screening that came before it.

- effort:

  Reasoning effort, or `NULL` (the default) to send none: one string, or
  a character vector named by stage, as `effort` in
  [`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md).
  The calls here are labelled `synthesise.section`, `synthesise.batch`,
  `synthesise.merge` and, with `coherence`,
  `synthesise.revise.structure`, `.cut` and `.register`;
  `c(synthesise = "medium", "synthesise.section" = "high")` writes the
  sections at high and does the rest at medium. The reasoning counts
  against each call's reply limit (`max_section_tokens` for a section),
  and a section cut off at it is reported as one.

## Value

An object of class `gr_synthesis`:

- `text`:

  The whole write-up, as markdown, citations rendered and the reference
  list appended.

- `text_marked`:

  The same document with `[study N]` markers intact: what the citation
  check ran on.

- `draft`:

  The write-up before the coherence pass, for comparison.

- `references`:

  The reference list, or `NULL`.

- `cite_style`:

  The style actually used, which is not always the one asked for.

- `coherence`:

  One row per revision pass, or `NULL` if none ran: `pass` (which of
  `"structure"`, `"cut"`, `"register"`), `ran`, `kept`, `lost` and
  `added` (citations, if the revision changed them), `reason` for
  anything discarded, and `words_before` and `words_after` (integer:
  words of prose, headings and citation markers left out, in the text
  the pass was given and in its revision; `NA` for a pass that was not
  sent, or was refused by a check made before the length check, which
  comes last).

- `unrendered`:

  Studies a section cited honestly that `text` still shows as
  `[study N]` somewhere, because a kept revision moved or reworded a
  citation of the same study that another section made without being
  given it, and the two could no longer be told apart. Leaving both is
  the safe side of that doubt; `$draft` renders each section's own
  citations. Empty when nothing was in doubt.

- `sections`:

  One row per section: `section`, `brief`, `text`, `n_cited`,
  `n_unknown` (citations to a row that does not exist), `n_unsupplied`
  (citations to a study that exists but was not given to the call that
  wrote them: with `claims`, a study not behind that section's claims;
  for a section written in batches, a study outside that batch, or one
  the merge of the batch drafts cited that no draft did. They are left
  as markers and kept out of the reference list, with one exception:
  once batch drafts are merged, a study that another batch of the same
  section was given and cited cannot be told apart, so it is rendered,
  and still counted), `n_unparsed` (brackets that open like a citation,
  such as `[studies 1 to 7]`, but cannot be read as one, so the studies
  they name were not checked), `n_truncated` (replies, including batch
  drafts and merges, that stopped at the reply limit), `n_claims` and
  `claims_missed` (the claims it was given, and those it did not write
  up), `lost_batches` (batches of studies whose call failed),
  `capped_batches` (batches not sent because the run reached its call or
  cost ceiling), `merge_failed` (its batch drafts could not be merged,
  so it is those drafts joined end to end), and `partial`. A section is
  partial when any of those counts is non-zero (`n_cited` and `n_claims`
  aside), when it came back empty, when a batch of studies was lost or
  not read, when its batch drafts could not be merged or one had to be
  cut to fit the merge, when (without `claims`) none of the studies of a
  batch it was drafted from is cited in it, when it could not be sent at
  all (its brief and gaps leave no room in the window), when the gaps
  given to it were cut, or, with `claims`, when it did not write up a
  claim it was given. Last, `n_unbacked`: the figures in the section
  that are not in the rows their sentence cites (see "Figures" below).
  It does not make a section partial, which says that a citation cannot
  be trusted, and a figure's citation may be sound.

- `citations`:

  Every citation, resolved to the row it points at, in long form:
  `section`, `study`, `document`, `document_id`.

- `figures`:

  Every figure in a sentence of the write-up, checked against the rows
  it cites: `stage` (`"draft"` for the sections as written, `"final"`
  for the revised text when a kept revision pass changed it), `section`
  (the heading it stands under), `sentence`, `number` (as written),
  `studies` (the studies the sentence cites, comma-joined, or NA),
  `backed`, and `basis`, what it was found in (NA when it was not). See
  "Figures" below.

- `studies`:

  The rows that were written from, with the `study` number each was
  cited by.

- `claims`:

  The
  [`gr_claims()`](https://elkronos.github.io/readgpt/reference/gr_claims.md)
  result written from, or `NULL`. When its `$lost` names studies whose
  claims batch contributed nothing, the run warns (`gr_claims_partial`),
  and print() and the audit report say the review was written without
  them.

- `claim_sections`:

  With `claims`, which section each claim was given to: a data frame of
  `section` and `claim_id`, the outline's assignment. `NULL` without
  `claims`.

- `outline_alternative`:

  Present only when the outline is one of several
  [`gr_outline()`](https://elkronos.github.io/readgpt/reference/gr_outline.md)
  compared (`alternatives` above 1): which one it was, as `alternative`,
  `of` (how many were asked for), `distinct` (how many came back
  distinct) and `distance` from the first proposal. The trace's meta
  records `outline_alternative` and `outline_of` too.

- `trace`:

  As
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md).

## Which rows are used

Rows that were never read (`status` `"failed"` or `"skipped"`) are left
out, and so are duplicates: a study counted twice is the error this
whole pipeline exists to avoid, and
[`gr_read_many()`](https://elkronos.github.io/readgpt/reference/gr_read_many.md)
has already marked them. The number left out is reported by
[`print()`](https://rdrr.io/r/base/print.html) and is in `$skipped`.

## What it costs

One call per section when the table fits one prompt, which is the usual
case: a hundred rows of a ten-field schema is a few thousand tokens. A
table too large for one prompt is written in batches and merged, so a
section costs batches + merges instead. Either way the cost is per
*section*, not per document: the expensive reading has already happened.

The merge folds every batch draft into one reply of at most
`max_section_tokens`, the same limit each draft had, so a section
drafted from a very large table covers only what that one reply can
hold. Nothing checks that it cites every study – a section need not –
but without `claims` a batch none of whose studies the merged section
cites is reported (`gr_synth_batch_dropped`) and marks it partial. Raise
`max_section_tokens`, or divide the table between narrower sections.

## Figures

The citation check says which rows a sentence rests on, not what it says
they found, and the numbers are what a reader copies out of a review. So
every number in every sentence is looked for in what the writer was
shown for that sentence: the rows it cites, as the writing prompt showed
them; the question, the section's heading and brief, `style`, and the
gaps a closing section was given; and with `claims`, the claims given to
the section that rest on a cited study. A sentence that cites nothing is
checked against every row its section was shown. This runs on the draft,
and again on the revised text when a kept revision pass changed it; it
sends nothing, so it is always on, and it changes no request.

A figure counts as found in any form a faithful copy may give it: `.61`
for 0.61, `45.20` for 45.2, `1204` for 1,204, `12 450` for 12,450
(grouped by a space, where that is the row's figure), a minus sign or
none, either side of a dash in a range, `45.2 million` for 45,200,000,
`4 x 10^-4` for 0.0004, `100,000` for a numeric field shown as `1e+05`,
`45%` for 0.45, `12` for "twelve", and rounded to the places it is
written to (0.61 or 0.6 for 0.613, never 0.62). A row's percentage read
as a proportion, or its scaled figure at full value, must match exactly:
rounded as well, a pooled estimate no study reported matched a row's
unrelated percentage. A figure is also allowed when it is the total of a
numeric field over the rows the sentence cites; worked out from two
other figures of the same sentence ("120 of 1,020 (11.8%)"), or from a
ratio in it read as a change from 1 ("HR 0.78, a 22% lower hazard"); a
difference in points between two figures of one field of a cited row, or
two percentages of one cited row; a year a cited row carries in any
column; the number of studies the sentence cites, its section was shown
or the review was written from, or how many of those rows share a
field's value ("10 were randomised trials"); or, after "about", "more
than", "fewer than" and their like, within a tenth (about) or a quarter
(a bound) of a value it could have come from. Not read as figures: a
citation's ids, written bare as well ("studies 12 to 15"), a whole
number below ten that is not a percentage or scaled (a review writes "2
of the trials" as it likes), a number joined to a name ("COVID-19",
"IL-6"), an ordinal, a decade ("1990s", "in their 60s"), an interval's
level (`95% CI`), a significance threshold (`p < .001`, "p value below
0.05") and a significance level ("at the 5% level"). Numbers written in
words are not checked.

A figure found nowhere is reported in `$figures` and counted in
`$sections$n_unbacked`, and the run warns (`gr_synth_unbacked_figures`)
about the text as published. It is arithmetic the sentence does not
show, a pooled estimate no study reported, or a number the writer made
up, and nothing here can tell which. Found means copied, not right: a
figure the cited row carries can still be the wrong one for the
sentence. The section is not marked partial, because `partial` is about
whether its citations can be trusted, and those are checked apart.

## See also

[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md),
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)

## Examples

``` r
fields <- gr_fields(design = "The study design",
                    n = gr_field("Participants", type = "integer"))
cl <- gr_mock_client(function(messages, params) {
  seen <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                collapse = " ")
  if (grepl("<studies>", seen, fixed = TRUE)) "One randomised trial of 120 people [study 1]."
  else '{"design":"randomised trial","n":120,
         "design__quote":"We ran a randomised trial.",
         "n__quote":"We enrolled 120 people."}'
})

f <- tempfile(fileext = ".txt")
writeLines("We ran a randomised trial. We enrolled 120 people.", f)
x <- gr_extract(f, fields, client = cl)
#> [1/1] file1d6c1e868862.txt
#> Extracting 'file1d6c1e868862.txt' with the 'txt' extractor.
#> Ingested 1 block(s), ~18 tokens (0 chars removed by cleaning).
#> Segmenting with 'structural' (cap 900 tokens, overlap 90).
#> Reading with 'extract' (all|N+conflicts|none) over 1 chunk(s).

s <- gr_synthesise(x, question = "Does it work?",
                   outline = c("Included studies" = "How many, of what design"),
                   client = cl)
#> [1/1] Included studies
s$sections[, c("section", "n_cited", "n_unknown", "n_unbacked")]
#>            section n_cited n_unknown n_unbacked
#> 1 Included studies       1         0          0
# The 120 in the section, found in the row it cites.
s$figures[, c("number", "studies", "backed", "basis")]
#>   number studies backed     basis
#> 1    120       1   TRUE cited row
```
