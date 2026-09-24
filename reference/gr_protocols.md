# Protocols that ship with the package, and any you have registered

Four starting points. They are **templates, not standards**: the package
knows the shape a protocol has, not what your criteria should be, and
every field of a built-in is meant to be edited before it is used.

## Usage

``` r
gr_protocols(name = NULL)
```

## Arguments

- name:

  A protocol name, or omit to list them all.

## Value

With `name`, a
[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md).
Without, a data frame of `name`, `question` and `description`.

## Details

- `bibliography`:

  Who wrote it, what it is called, where it appeared. No screening
  (everything is included), so it is the cheapest way to turn a folder
  into a reference list you can check.

- `evidence_table`:

  One row per study: design, population, comparison, outcome, effect. No
  synthesis outline; the table is the output.

- `claims`:

  The same ground as `evidence_table`, coded so studies can be compared
  mechanically rather than only read: `design` and `finding` are enums,
  each paired with the paper's own wording or figures so the coding can
  be checked. `finding` is relative to *your* question, so this template
  means nothing until you replace it.

- `systematic_review`:

  Criteria, a schema and a write-up outline, in the shape a report
  following a standard like PRISMA expects. Filling it in is your work,
  not the package's.

## See also

[`gr_protocol()`](https://elkronos.github.io/readgpt/reference/gr_protocol.md),
[`gr_register_protocol()`](https://elkronos.github.io/readgpt/reference/gr_register_protocol.md),
[`gr_extract()`](https://elkronos.github.io/readgpt/reference/gr_extract.md)

## Examples

``` r
gr_protocols()
#>                name                                                 question
#> 1      bibliography       What is each of these documents, and who wrote it?
#> 2            claims REPLACE THIS with your review question, in one sentence.
#> 3    evidence_table   What did each study do, to whom, and what did it find?
#> 4 systematic_review REPLACE THIS with your review question, in one sentence.
#>                                                                                                           description
#> 1                                                        Turn a folder into a checkable reference list. No screening.
#> 2 Coded where studies must be compared, verbatim where they must be quoted. The input a claims-level synthesis needs.
#> 3                                                   One row per study. The table is the output; there is no write-up.
#> 4                                            A template in the shape a PRISMA-style report expects. Edit every field.
gr_protocols("bibliography")$fields
#> <gr_fields> 5 field(s)
#>   title          string    The document's own title, exactly as it is printed on it
#>   authors        string    The authors, in the order they are listed, as printed
#>   year           integer   Year of publication
#>   venue          string    The journal, publisher, conference or issuing body
#>   doi            string    The DOI or other permanent identifier, if one is printed
```
