# Get or set package options

`gr_options()` with no arguments returns the full option list. Called
with a single string it returns that option. Called with `name = value`
pairs it sets them and invisibly returns the previous values, so it
composes with [`on.exit()`](https://rdrr.io/r/base/on.exit.html).

## Usage

``` r
gr_options(...)
```

## Arguments

- ...:

  Nothing, a single option name, `name = value` pairs, or a named list
  (the form `gr_options()` itself returns, so a saved value can be
  restored directly).

## Value

The option list, a single option value, or (when setting) the old values
invisibly.

## Details

Options are read at *call* time, never captured at load time, so
changing an option mid-session affects subsequent runs.

## Options

- `verbose` (TRUE):

  Print a line for each ingest, segment and read stage. In an
  interactive session, also keep one line up to date with how many
  chunks a long read has done and what the run has spent.

- `model` ("gpt-5.6-terra"):

  Default chat model. Note the default is a reasoning model, which does
  not accept `temperature`.

- `embedding_model` ("text-embedding-3-small"):

  Default embedding model.

- `tokenizer` ("heuristic"):

  Token counter: `"heuristic"` (conservative, no dependencies),
  `"words"`, `"chars"`, `"tiktoken"` (needs reticulate), or a name
  registered via
  [`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md).

- `api` ("responses"):

  `"responses"` or `"chat"` request shape.

- `api_base` ("https://api.openai.com/v1"):

  API root; point this at a proxy or a compatible endpoint.

- `api_headers` (none):

  Named character vector of extra HTTP headers sent with every request,
  for gateways that do not authenticate with a bearer token. Inherited
  by every
  [`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md)
  that does not name its own; see that function's `headers` argument for
  the rules.

- `temperature` (NULL):

  Default sampling temperature. `NULL` omits the field. Dropped
  automatically for models that reject it.

- `max_retries` (4):

  Retries for transient failures. HTTP 400 is never retried: a malformed
  request stays malformed.

- `retry_pause_base` (2):

  Seconds; exponential backoff base.

- `request_timeout` (120):

  Per-request timeout, seconds.

- `safety_margin` (0.10):

  Fraction of the context window left unused to absorb tokenizer error.
  Not a spending cap (see `max_cost_usd`).

- `min_output_tokens` (256):

  Floor on the completion room
  [`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md)
  reserves *when `reserve_output` is not given explicitly*. An explicit
  `reserve_output` is honoured down to 1.

- `cache_documents` (TRUE):

  Cache ingestion per file + settings. The key covers file size, mtime
  and every option that changes the output.

- `cache_embeddings` (TRUE):

  Cache embeddings per text + model.

- `embedder` (NULL):

  Which registered embedder to use. `NULL` means the one the client
  carries, if any, and otherwise `"api"`. Naming one here overrides
  both. See
  [`gr_embedders()`](https://elkronos.github.io/readgpt/reference/gr_embedders.md);
  recording a run with a *deterministic* embedder is what lets a
  [`gr_replay_client()`](https://elkronos.github.io/readgpt/reference/gr_replay_client.md)
  reproduce its chunk ranking.

- `cache_dir` (NULL):

  Directory
  [`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)
  stores model responses in. `NULL` means a per-session directory under
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html), which costs
  nothing and disappears with the session. Set it to a real path, such
  as `tools::R_user_dir("readgpt", "cache")`, to keep responses across
  sessions and make a long run resumable.

- `parallel` (FALSE):

  Run per-chunk calls concurrently. Needs the future and future.apply
  packages; without them it warns and runs sequentially.

- `workers` (4):

  Worker processes when `parallel` is TRUE.

- `max_cost_usd` (5):

  Spending limit per run, in USD. A run whose reader sends every chunk
  is refused before it starts when sending them would cost more than
  this. Every run is checked again before each request, and stops with a
  `partial` answer once what it has spent reaches the limit. The cost of
  a request is known only once it is made, so a run can pass the limit
  by one request. With `parallel = TRUE` requests go out in batches that
  cannot be stopped part way, so a reader that sends batches is also
  refused before it starts when its worst case, every reply at its token
  cap and the price of the dearest model it uses, would pass the limit.
  Needs a model with a registered price (see
  [`gr_models()`](https://elkronos.github.io/readgpt/reference/gr_models.md)).
  Under a limit of 0 a model registered at no cost runs and one with a
  price is refused. `NULL` removes the limit.

- `max_calls` (400):

  Hard cap on model calls per run, checked before the first call and
  again before every subsequent one. `NULL` removes the cap.

- `unknown_model_action` ("warn"):

  `"warn"` or `"error"` when a model id is not in the registry.

## Checked values

Numeric options are checked when they are set, and the two kinds are
treated differently because a wrong value costs different things.

The ceilings, `max_cost_usd` and `max_calls`, refuse anything that is
not a single number of zero or more, with an error of class
`gr_bad_option`: a limit that cannot be compared is not a limit, and
ignoring it spends money. `NULL` and `Inf` both mean "no limit".

The tuning settings (`safety_margin` \[0, 0.5\], `min_output_tokens`
\[0, 1e6\], `max_retries` \[0, 10\], `retry_pause_base` \[0, 60\],
`request_timeout` \[1, 3600\], `workers` \[1, 32\] and `temperature`
\[0, 2\]) read a number written as text as that number. A value they
cannot read, such as `NA` or `"x"`, warns (`gr_bad_option`) and leaves
the current setting unchanged; a value outside the range is clamped into
it, with the same warning. Whole-number settings are rounded down.
`temperature` also takes `NULL`, its default.

## See also

[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md)
to correct a model's limits,
[`gr_set_tokenizer()`](https://elkronos.github.io/readgpt/reference/gr_set_tokenizer.md),
[`gr_budget()`](https://elkronos.github.io/readgpt/reference/gr_budget.md),
[`gr_cache()`](https://elkronos.github.io/readgpt/reference/gr_cache.md)

## Examples

``` r
old <- gr_options(verbose = FALSE)
gr_options("verbose")
#> [1] FALSE
gr_options(old)
```
