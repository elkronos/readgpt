# Write down what a review is looking for

A protocol fixes the three decisions that must not be made while
reading: which documents count, what to collect from them, and what the
write-up has to cover. Pass one to
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)
in place of a schema.

## Usage

``` r
gr_protocol(
  name,
  question = NULL,
  include = NULL,
  exclude = NULL,
  fields = NULL,
  outline = NULL,
  recipe = "research",
  description = ""
)
```

## Arguments

- name:

  A short name. Becomes the registry key if you register it.

- question:

  The review question, in one sentence. Required. It has a default only
  so that omitting it produces the explanation below rather than R's
  "argument is missing".

- include, exclude:

  Criteria, one per element, each a statement a document either meets or
  does not. Write them so that a careful reader with no knowledge of
  your field could apply them: "reports a randomised comparison" is
  checkable, "is high quality" is not.

- fields:

  A
  [`gr_fields()`](https://elkronos.github.io/readgpt/reference/gr_fields.md)
  schema: what to extract from an included document.

- outline:

  The write-up, as a named character vector: names are section headings,
  values say what that section has to cover.

- recipe:

  The reading pipeline to default to, as in
  [`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md).

- description:

  One line, for
  [`gr_protocols()`](https://elkronos.github.io/readgpt/reference/gr_protocols.md).

## Value

A `gr_protocol`.

## Criteria are not free text

`include` and `exclude` are separate, and both are kept, because a
document can meet an inclusion criterion and still be excluded, and a
review has to be able to say which. Collapsing them into one list of
"criteria" loses the reason, which is the part anyone auditing the
review will ask for.

## See also

[`gr_protocols()`](https://elkronos.github.io/readgpt/reference/gr_protocols.md)
for the built-ins,
[`gr_register_protocol()`](https://elkronos.github.io/readgpt/reference/gr_register_protocol.md),
[`gr_protocol_save()`](https://elkronos.github.io/readgpt/reference/gr_protocol_save.md),
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md),
[`gr_fields()`](https://elkronos.github.io/readgpt/reference/gr_fields.md)

## Examples

``` r
p <- gr_protocol(
  "statins-primary",
  question = "Do statins reduce cardiovascular events in primary prevention?",
  include = c("Reports a randomised comparison",
              "Participants have no prior cardiovascular event",
              "Reports at least one cardiovascular outcome"),
  exclude = c("Secondary prevention only", "Not a primary research report"),
  fields = gr_fields(
    n = gr_field("Number randomised", type = "integer"),
    drug = "The statin studied, and the dose"
  ),
  outline = c(
    "Included studies" = "How many, of what design, over what period",
    "Findings" = "Effect on each outcome, with the range across studies"
  )
)
p
#> <gr_protocol 'statins-primary'>
#>   question : Do statins reduce cardiovascular events in primary prevention?
#>   include  : Reports a randomised comparison
#>              Participants have no prior cardiovascular event
#>              Reports at least one cardiovascular outcome
#>   exclude  : Secondary prevention only
#>              Not a primary research report
#>   fields   : n, drug
#>   outline  : Included studies / Findings
#>   recipe   : research
```
