# Measure the screener against a hand-screened sample

Compares
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md)'s
decisions with a person's on the same documents, and reports what that
sample supports: how much of the eligible literature the screener kept,
how much of the irrelevant literature it removed, and how much reading
it saved. Each comes with an interval, and each is refused when the
sample is too small to say.

## Usage

``` r
gr_calibrate(
  screening,
  reference,
  positive = "include",
  min_positives = 10L,
  of = NULL
)
```

## Arguments

- screening:

  A `gr_screening` from
  [`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md).

- reference:

  The completed frame from
  [`gr_reference()`](https://elkronos.github.io/readgpt/reference/gr_reference.md),
  a path to the filled-in CSV, or any data frame with `document` and
  `human_decision`.

- positive:

  Which human decision counts as eligible.

- min_positives:

  Below this many eligible studies in the sample, the sensitivity
  estimate is reported but marked inadequate.

- of:

  Which part of the screening run the reference was drawn from:
  `"excluded"`, `"kept"` or `"all"`. Normally recovered from the file
  [`gr_reference()`](https://elkronos.github.io/readgpt/reference/gr_reference.md)
  wrote; give it explicitly for a reference built by hand.

## Value

An object of class `gr_calibration`:

- `counts`:

  The confusion matrix, as kept/excluded by eligible/not.

- `metrics`:

  One row per statistic: `estimate`, `lower`, `upper`, `n`.

- `missed`:

  The eligible studies the screener excluded: the rows themselves,
  because a list of the misses says more than a rate.

- `disagreements`:

  Every row where the two differ, in either direction.

- `adequate`:

  Whether the sample supports a sensitivity claim.

- `frame`:

  Which rows the sample was drawn from, and how many.

## Two sensitivities, and the gap between them

`"unclear"` is a deferral, not a miss. A record the screener could not
settle goes to a person, so it is not lost, and counting it as a failure
would punish the screener for the one behaviour that makes it safe.

So two figures are reported. **Sensitivity as deployed** asks what
fraction of the eligible studies survived: kept, whether by `"include"`
or by `"unclear"`. That is the number that matters, because it is the
one where a shortfall is permanent. **Strict sensitivity** asks what
fraction were actively included. The gap between them is the reading a
person still has to do, and reporting only the first would flatter a
screener that defers everything.

## Why accuracy is not reported

Inclusion rates run at a few per cent, so a screener that excluded every
record would score around 95% accurate and find nothing. Cohen's kappa
is reported instead, because it is the statistic that notices.

## What a small sample cannot do

Sensitivity is estimated from the eligible studies in the sample and
from nothing else. Twelve hand-screened records containing two eligible
studies estimate it from two observations, and "1.00" from two
observations is not a finding. The intervals are Wilson score intervals,
which stay sensible at zero and one where the textbook interval
collapses to a point, and `$adequate` says whether there was enough to
support a claim at all.

## See also

[`gr_reference()`](https://elkronos.github.io/readgpt/reference/gr_reference.md),
[`gr_screen()`](https://elkronos.github.io/readgpt/reference/gr_screen.md),
[`gr_audit_report()`](https://elkronos.github.io/readgpt/reference/gr_audit_report.md)

Other corpus functions:
[`gr_inventory()`](https://elkronos.github.io/readgpt/reference/gr_inventory.md),
[`gr_records()`](https://elkronos.github.io/readgpt/reference/gr_records.md),
[`gr_reference()`](https://elkronos.github.io/readgpt/reference/gr_reference.md),
[`gr_search()`](https://elkronos.github.io/readgpt/reference/gr_search.md)

## Examples

``` r
tab <- data.frame(document = paste0("d", 1:8, ".pdf"),
                  decision = c("include", "include", "unclear", "exclude",
                               "exclude", "exclude", "include", "exclude"),
                  stringsAsFactors = FALSE)
ref <- data.frame(document = paste0("d", 1:8, ".pdf"),
                  human_decision = c("include", "exclude", "include", "exclude",
                                     "exclude", "include", "include", "exclude"),
                  stringsAsFactors = FALSE)
gr_calibrate(structure(list(table = tab), class = "gr_screening"), ref)
#> Warning: This reference does not say which part of the screening run it came from, so the figures below assume a sample of EVERYTHING screened. If it is a sample of one stratum (the exclusions, say), sensitivity and specificity are artifacts of that frame rather than facts about the screener. Pass `of = "excluded"`, `"kept"` or `"all"` to say which.
#> <gr_calibration> 8 hand-screened row(s), 4 eligible
#>   sampled from: unknown (NA of 8 screened)
#>   sensitivity (as deployed)       75.0%  [30.1%, 95.4%]  n=4
#>   sensitivity (strict include)    50.0%  [15.0%, 85.0%]  n=4
#>   specificity                     75.0%  [30.1%, 95.4%]  n=4
#>   deferred to a person            12.5%  [2.2%, 47.1%]  n=8
#>   reading avoided                 50.0%  [21.5%, 78.5%]  n=8
#>   Cohen's kappa                   0.33
#>   ! 1 eligible study was excluded by the screener:
#>       d6.pdf
#>   ! only 4 eligible studies in the sample. Every rate above rests on
#>     those 4 observations, which is why the intervals are as wide as they are.
#>     Hand-screen more before quoting a figure.
```
