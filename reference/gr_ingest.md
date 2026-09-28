# Ingest a document into cleaned, provenance-bearing text blocks

The first axis. Extraction happens once and is cached, so one file read
can feed any number of segmentations: comparing chunking strategies
costs one extraction, not one per strategy. Page and section provenance
survives cleaning, which is what lets `ans$evidence` point back at where
an answer came from.

## Usage

``` r
gr_ingest(source, spec = NULL, cache = NULL, trace = NULL)
```

## Arguments

- source:

  A file path, a web address, or a character vector / single string of
  raw text. An address starting `http://` or `https://` is downloaded
  and read with the extractor for what came back: a specific type the
  server declares, else the extension in the address, else the file's
  first bytes. The charset the server declares is used for HTML and
  text. A download that fails is an error (`gr_url_error`), one past 512
  MB is refused (`gr_too_large`, which is also a `gr_url_error`), and
  one no extractor reads is refused (`gr_unsupported_format`). An
  address on this machine or a private network (localhost, 10.x,
  172.16/12, 192.168.x, 169.254.x, the IPv6 local ranges), and a host
  name that resolves to one, is refused (`gr_url_error`), after a
  redirect too, unless `options(readgpt.allow_local_urls = TRUE)`;
  behind a proxy that looks names up itself, a name that cannot be
  looked up here is not checked. The request uses the address in full,
  but the document's `source`, the messages and the errors show it
  without a user name and password and with its query replaced by a
  short fingerprint (`[query hidden 1a2b3c]`), since a presigned link
  carries its credentials there; two addresses that differ only in the
  query keep different fingerprints. A Word file, fetched or on disk,
  whose text parts would unpack to more than 512 MB is refused
  (`gr_too_large`). A one-line string ending in an extension some
  extractor claims is taken as a path, and is an error
  (`gr_file_not_found`) when no such file exists. Any other string is
  read as text; one that looks like a path (a directory separator and an
  extension no extractor claims) also raises a `gr_path_as_text`
  warning.

- spec:

  A `gr_ingest_spec`, a bare preset name, or `NULL` for defaults.

- cache:

  Use the session document cache. The cache key includes the file's size
  and mtime plus every ingestion option; for a web address, the address,
  so it is downloaded once a session. Registering a cleaner or extractor
  the ingestion uses again (as when fixing it) starts a new cache entry.
  The cache lives in memory for the session, is shared by everything
  running in the R process (every Shiny session of one app, say), and
  holds about 256 MB of documents; past that the documents used least
  recently are dropped, and read again when next asked for.

- trace:

  Optional `gr_trace`.

## Value

A `gr_document`: a list with `blocks` (data frame), `text`, `source`,
`spec` and `stats`.

## See also

[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md)
for the options,
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md)
for the step names,
[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md)
for the formats,
[gr_document](https://elkronos.github.io/readgpt/reference/gr_document.md)
for what comes back,
[`gr_segment()`](https://elkronos.github.io/readgpt/reference/gr_segment.md)
for the next axis

Other ingest functions:
[`gr_clean()`](https://elkronos.github.io/readgpt/reference/gr_clean.md),
[`gr_cleaners()`](https://elkronos.github.io/readgpt/reference/gr_cleaners.md),
[`gr_document`](https://elkronos.github.io/readgpt/reference/gr_document.md),
[`gr_extractors()`](https://elkronos.github.io/readgpt/reference/gr_extractors.md),
[`gr_ingest_spec()`](https://elkronos.github.io/readgpt/reference/gr_ingest_spec.md),
[`gr_register_cleaner()`](https://elkronos.github.io/readgpt/reference/gr_register_cleaner.md),
[`gr_register_extractor()`](https://elkronos.github.io/readgpt/reference/gr_register_extractor.md)

## Examples

``` r
doc <- gr_ingest(readgpt_example())
#> Extracting 'annual_report.md' with the 'md' extractor.
#> Ingested 17 block(s), ~574 tokens (12 chars removed by cleaning).

# Provenance survives cleaning; this is what `ans$evidence` points back at.
head(doc$blocks[, c("block_id", "page", "section", "kind")], 4)
#>   block_id page                                    section    kind
#> 1        1   NA Northwind Instruments — Annual Report 2024 heading
#> 2        2   NA                                    Summary heading
#> 3        3   NA                                    Summary    body
#> 4        4   NA                                    Summary    body

# What each cleaning step actually removed, in characters.
vapply(doc$stats$clean_log, function(s) s$chars_removed, integer(1))
#>        page_numbers         hyphenation       control_chars           ligatures 
#>                  12                   0                   0                   0 
#> collapse_whitespace 
#>                   0 

# Cleaning is a choice, not a default. Compare before committing to it.
raw <- gr_ingest(readgpt_example(), gr_ingest_spec(clean = "none"))
#> Extracting 'annual_report.md' with the 'md' extractor.
#> Ingested 19 block(s), ~586 tokens (0 chars removed by cleaning).
c(standard = doc$stats$chars, none = raw$stats$chars)
#> standard     none 
#>     1846     1858 
```
