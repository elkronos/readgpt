# Read documents through an ellmer chat

Uses an `ellmer` `Chat` object as the transport, so every provider
ellmer supports (Anthropic, Google, Bedrock, Azure, Ollama, Hugging
Face, and the rest) becomes available to every reading strategy here,
with this package's context budgeting, cost rails, traces, caching and
replay unchanged around it.

## Usage

``` r
gr_ellmer_client(chat, embed = NULL, model = NULL)
```

## Arguments

- chat:

  An ellmer `Chat`, e.g. from
  [`ellmer::chat_anthropic()`](https://ellmer.tidyverse.org/reference/chat_anthropic.html)
  or
  [`ellmer::chat_ollama()`](https://ellmer.tidyverse.org/reference/chat_ollama.html).

- embed:

  Optional function of `(texts, params)` returning one row per input,
  for example a thin wrapper around `ragnar::embed_ollama()`. Without
  it, `retrieve` and the `semantic` segmenter fall back to lexical
  vectors and warn.

- model:

  Model id reported to this package. Defaults to the chat's own model.
  Register it with
  [`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md)
  if it is not already known. The context window is what sizes your
  chunks, so a wrong one is not cosmetic.

## Value

A
[`gr_backend_client()`](https://elkronos.github.io/readgpt/reference/gr_backend_client.md).

## Details

ellmer is a suggested dependency: this function is the only thing in the
package that needs it.

## Requirements on the chat

The adapter calls `$chat()`, `$chat_structured()`, `$clone()`,
`$set_turns()` and `$set_system_prompt()`, and refuses a chat missing
any of them (`gr_bad_backend`). `$chat_structured()` is used by every
schema-bearing call (each `rerank` score and each `iterative` round), so
a chat without it would have failed mid-run rather than at construction.
The last two are not conveniences: this package puts its instructions in
the system prompt, and it clears turns so that one chunk's call cannot
leak into the next. A chat that silently dropped either would produce
unconstrained answers with nothing to show for it.

## What does not carry over

Two things, both worth knowing before you rely on them.

`temperature` belongs to the chat object, not to the call. ellmer fixes
sampling parameters when the chat is constructed, so a `temperature` in
a
[`gr_read_spec()`](https://elkronos.github.io/readgpt/reference/gr_read_spec.md)
cannot be honoured per-call; it is ignored and warned about once
(`gr_ellmer_temperature`). Build a second chat if you need a second
temperature.

Each call is independent. An ellmer chat accumulates turns, and this
package issues many unrelated calls per run, so every call runs against
a fresh deep clone with its turns cleared. Your chat object is never
mutated, and no conversation history leaks from one chunk's call into
the next.

## See also

[`gr_backend_client()`](https://elkronos.github.io/readgpt/reference/gr_backend_client.md),
[`gr_client()`](https://elkronos.github.io/readgpt/reference/gr_client.md),
[`gr_register_model()`](https://elkronos.github.io/readgpt/reference/gr_register_model.md)

## Examples

``` r
if (FALSE) { # \dontrun{
library(ellmer)

# Any provider ellmer speaks to, with any reading strategy here.
cl <- gr_ellmer_client(chat_anthropic(model = "claude-sonnet-4-5"))
ans <- answer_document("report.pdf", "What was revenue?", "thorough", client = cl)

# Locally, for nothing:
local <- gr_ellmer_client(chat_ollama(model = "llama3.1"))
gr_compare("report.pdf", "What was revenue?",
           c("fast", "thorough"), client = local)$summary
} # }
```
