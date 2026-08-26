# core-models.R -- model registry and context-budget arithmetic.
#
# WHY THIS FILE EXISTS
# The old `get_model_limits()` was an if/else ladder with three failure modes:
#
#   1. `grepl("gpt-4", model)` swallowed every 128k-context 4-series model into
#      an 8192-token window, over-chunking documents by ~26x.
#   2. Any unrecognised id fell through to `context_window = 4096,
#      output_tokens = 4096`. Since budgets were computed as
#      `context - output - overhead`, unknown models produced a NEGATIVE budget.
#      Downstream, `split(tokens, ceiling(seq_along(tokens) / -54))` reversed the
#      document and `chunk_text_minimal(text, -2000)` emitted one chunk -- and
#      therefore one API call -- PER WORD.
#   3. The list was hard-coded, so it went stale the day it was written and
#      there was no way for a user to correct it.
#
# The replacement is a data-driven registry with explicit match precedence
# (exact id > alias > family regex > default), user extension via
# `gr_register_model()`, and a budget function that is mathematically incapable
# of returning a non-positive value -- it errors with an actionable message
# instead.

# Seed data. `as_of` is stamped so a stale entry is visible rather than implied.
# Prices are USD per 1M tokens and are used only for pre-flight cost estimates.
.gr_seed_models <- function() {
  m <- function(id, context, max_output, input_usd = NA_real_, output_usd = NA_real_,
                reasoning = FALSE, temperature = TRUE, kind = "chat", dims = NA_integer_) {
    list(id = id, context_window = as.integer(context), max_output = as.integer(max_output),
         input_usd = input_usd, output_usd = output_usd, reasoning = reasoning,
         supports_temperature = temperature, kind = kind, dimensions = dims,
         as_of = "2026-08")
  }
  list(
    # ---- GPT-5.6 family --------------------------------------------------
    m("gpt-5.6-sol",   1050000, 128000,  4.00, 20.00, reasoning = TRUE,  temperature = FALSE),
    m("gpt-5.6-terra", 1050000, 128000,  2.00, 12.00, reasoning = TRUE,  temperature = FALSE),
    m("gpt-5.6-luna",  1050000, 128000,  1.00,  6.00, reasoning = TRUE,  temperature = FALSE),
    # ---- Older GPT-5 line -------------------------------------------------
    m("gpt-5.5",        400000, 128000,  1.75, 14.00, reasoning = TRUE,  temperature = FALSE),
    m("gpt-5.4",        400000, 128000,  1.25, 10.00, reasoning = TRUE,  temperature = FALSE),
    m("gpt-5.4-mini",   400000, 128000,  0.75,  4.00, reasoning = TRUE,  temperature = FALSE),
    m("gpt-5",          400000, 128000,  1.25, 10.00, reasoning = TRUE,  temperature = FALSE),
    m("gpt-5-mini",     400000, 128000,  0.25,  2.00, reasoning = TRUE,  temperature = FALSE),
    m("gpt-5-nano",     400000, 128000,  0.05,  0.40, reasoning = TRUE,  temperature = FALSE),
    # ---- GPT-4 line -------------------------------------------------------
    m("gpt-4.1",       1047576, 32768,   2.00,  8.00),
    m("gpt-4.1-mini",  1047576, 32768,   0.40,  1.60),
    m("gpt-4.1-nano",  1047576, 32768,   0.10,  0.40),
    m("gpt-4o",         128000, 16384,   2.50, 10.00),
    m("gpt-4o-mini",    128000, 16384,   0.15,  0.60),
    m("gpt-4-turbo",    128000,  4096,  10.00, 30.00),
    m("gpt-4",            8192,  8192,  30.00, 60.00),
    m("gpt-3.5-turbo",   16385,  4096,   0.50,  1.50),
    # ---- Embeddings -------------------------------------------------------
    m("text-embedding-3-small", 8191, 0, 0.02, 0, kind = "embedding", dims = 1536L),
    m("text-embedding-3-large", 8191, 0, 0.13, 0, kind = "embedding", dims = 3072L)
  )
}

# Family patterns, tried in order, only after exact and alias lookup fail.
# Each carries `certain = FALSE` so callers can tell a guess from a fact.
.gr_seed_patterns <- function() {
  list(
    list(pattern = "^gpt-5\\.6",       context_window = 1050000L, max_output = 128000L,
         reasoning = TRUE,  supports_temperature = FALSE),
    list(pattern = "^gpt-5",           context_window =  400000L, max_output = 128000L,
         reasoning = TRUE,  supports_temperature = FALSE),
    list(pattern = "^o[1-9]",          context_window =  200000L, max_output = 100000L,
         reasoning = TRUE,  supports_temperature = FALSE),
    list(pattern = "^gpt-4\\.1",       context_window = 1047576L, max_output =  32768L),
    list(pattern = "^gpt-4o",          context_window =  128000L, max_output =  16384L),
    list(pattern = "^gpt-4-turbo",     context_window =  128000L, max_output =   4096L),
    list(pattern = "^gpt-4",           context_window =    8192L, max_output =   4096L),
    list(pattern = "^gpt-3\\.5",       context_window =   16385L, max_output =   4096L),
    list(pattern = "^text-embedding",  context_window =    8191L, max_output =      0L,
         kind = "embedding", dimensions = 1536L)
  )
}

.gr_aliases <- list(
  "gpt-5.6"     = "gpt-5.6-sol",
  "gpt-4o-2024-08-06"      = "gpt-4o",
  "chatgpt-4o-latest"      = "gpt-4o",
  "gpt-4o-mini-2024-07-18" = "gpt-4o-mini",
  "gpt-3.5-turbo-1106"     = "gpt-3.5-turbo",
  "gpt-3.5-turbo-0125"     = "gpt-3.5-turbo",
  "gpt-3.5-turbo-16k"      = "gpt-3.5-turbo"
)

# Conservative last resort. Deliberately small context but a *proportionate*
# output reserve, so `gr_budget()` can never go negative on this path.
.gr_unknown_model <- list(
  context_window = 128000L, max_output = 4096L, input_usd = NA_real_,
  output_usd = NA_real_, reasoning = FALSE, supports_temperature = TRUE,
  kind = "chat", dimensions = NA_integer_, certain = FALSE, as_of = NA_character_
)

#' Register a model (or override a built-in entry)
#'
#' Use this whenever the shipped registry is stale or you are pointing the
#' client at a compatible non-OpenAI endpoint. Registered entries take
#' precedence over everything built in.
#'
#' @param id Model id string, exactly as the API expects it.
#' @param context_window Total context window in tokens.
#' @param max_output Maximum tokens the model will emit in one response.
#' @param input_usd,output_usd Price per 1M tokens; used for cost estimates.
#' @param reasoning Whether this is a reasoning model (affects prompt shape).
#' @param supports_temperature Whether the API accepts `temperature`.
#' @param kind `"chat"` or `"embedding"`.
#' @param dimensions Embedding dimensionality, for `kind = "embedding"`.
#' @return Invisibly, `id`.
#' @seealso [gr_models()] to see the registry, [gr_model_info()] for lookup and
#'   its `certain` flag, [gr_budget()], [gr_estimate_cost()]
#' @family cost and token functions
#' @export
#' @examples
#' gr_register_model("my-local-llama", context_window = 32768, max_output = 4096)
#' gr_model_info("my-local-llama")[c("context_window", "source", "certain")]
#'
#' # Without prices, the cost cap cannot be checked -- gptread says so rather
#' # than assuming the run is free.
#' is.na(gr_estimate_cost("my-local-llama", 1000, 500))
gr_register_model <- function(id, context_window, max_output,
                              input_usd = NA_real_, output_usd = NA_real_,
                              reasoning = FALSE, supports_temperature = TRUE,
                              kind = c("chat", "embedding"), dimensions = NA_integer_) {
  kind <- match.arg(kind)
  if (!is_nonblank(id)) gr_abort("`id` must be a non-empty string.")
  context_window <- as.integer(context_window)
  max_output <- as.integer(max_output)
  if (is.na(context_window) || context_window <= 0L) {
    gr_abort("`context_window` must be a positive integer.")
  }
  if (is.na(max_output) || max_output < 0L) gr_abort("`max_output` must be >= 0.")
  if (max_output >= context_window && kind == "chat") {
    gr_abort(sprintf(paste0("`max_output` (%d) must be smaller than `context_window` (%d): a model ",
                            "cannot emit its entire context. This exact inversion is what made the ",
                            "previous implementation compute negative input budgets."),
                     max_output, context_window))
  }
  # Every field below becomes a column in gr_models(). NULL or a zero-length
  # value here does not store "unknown" -- modifyList() deletes the key and
  # data.frame() drops the column, so one such registration made gr_models()
  # fail for the whole session. Coerce to a scalar, or to NA.
  registry_set("models", id, list(
    id = id, context_window = context_window, max_output = max_output,
    input_usd = as_num1(input_usd), output_usd = as_num1(output_usd),
    reasoning = isTRUE(reasoning),
    supports_temperature = isTRUE(supports_temperature), kind = kind,
    dimensions = as_int1(dimensions), certain = TRUE, as_of = format(Sys.Date(), "%Y-%m")
  ))
  invisible(id)
}

#' Look up a model's capabilities
#'
#' Resolution order: user-registered exact id, built-in exact id, alias, family
#' regex, then a conservative default. The returned list always carries
#' `certain`, which is `FALSE` when the answer came from a regex or the default
#' -- treat that as "verify before trusting for cost control".
#'
#' @param model Model id.
#' @return A named list: `id`, `context_window`, `max_output`, `input_usd` and
#'   `output_usd` (per 1M tokens, used by [gr_estimate_cost()]), `reasoning`,
#'   `supports_temperature`, `kind`, `dimensions`, `as_of`, `certain`, and
#'   `source` (`"registered"`, `"builtin"`, `"alias"`, `"pattern:..."` or
#'   `"default"`). `certain` is `FALSE` when the answer came from a family regex
#'   or the fallback -- verify before relying on it for cost control.
#' @seealso [gr_register_model()], [gr_models()], [gr_budget()]
#' @family cost and token functions
#' @export
#' @examples
#' gr_model_info("gpt-4o")[c("context_window", "max_output", "certain")]
#'
#' # An unrecognised id warns and falls back to conservative limits.
#' suppressWarnings(gr_model_info("some-model-from-next-year")$certain)
gr_model_info <- function(model = NULL) {
  model <- as_chr1(model %||% gr_options("model"))
  if (!nzchar(model)) gr_abort("`model` must be a non-empty string.")

  if (!is.null(gr_state$models[[model]])) {
    return(utils::modifyList(.gr_unknown_model,
                             c(gr_state$models[[model]], list(certain = TRUE, source = "registered"))))
  }
  seeded <- gr_state$seed_models %||% list()
  if (!is.null(seeded[[model]])) {
    return(utils::modifyList(.gr_unknown_model,
                             c(seeded[[model]], list(certain = TRUE, source = "builtin"))))
  }
  alias <- .gr_aliases[[model]]
  if (!is.null(alias) && !is.null(seeded[[alias]])) {
    return(utils::modifyList(.gr_unknown_model,
                             c(seeded[[alias]], list(id = model, certain = TRUE, source = "alias"))))
  }
  for (p in gr_state$model_patterns) {
    if (grepl(p$pattern, model, perl = TRUE)) {
      out <- utils::modifyList(.gr_unknown_model, p[setdiff(names(p), "pattern")])
      out$id <- model; out$certain <- FALSE; out$source <- paste0("pattern:", p$pattern)
      # A family match is a GUESS. Say so: the gpt-4 catch-all assigns an
      # 8192-token window, and silently applying that to a 128k model
      # over-chunks the document by ~20x and multiplies the API calls to match.
      gr_warn(sprintf(paste0("Model '%s' is not in the registry; matched family pattern '%s' ",
                             "-> %d-token context, %d-token output. Verify with ",
                             "gr_model_info('%s'), and correct it with gr_register_model() ",
                             "if that is wrong."),
                      model, p$pattern, out$context_window, out$max_output, model),
              class = "gr_unknown_model")
      return(out)
    }
  }
  action <- gr_options("unknown_model_action")
  msg <- sprintf(paste0("Model '%s' is not in the registry. Falling back to a conservative ",
                        "%d-token context with a %d-token output reserve. Register the real ",
                        "limits with gr_register_model('%s', context_window = ..., max_output = ...) ",
                        "so budgets and cost estimates are correct."),
                 model, .gr_unknown_model$context_window, .gr_unknown_model$max_output, model)
  if (identical(action, "error")) gr_abort(msg, class = "gr_unknown_model")
  gr_warn(msg, class = "gr_unknown_model")
  out <- .gr_unknown_model
  out$id <- model; out$source <- "default"
  out
}

#' Context and output limits for a model
#'
#' A two-field convenience wrapper over [gr_model_info()].
#'
#' @param model Model id.
#' @return A list with exactly two elements: `context_window` and
#'   `output_tokens`. (Note the second name differs from [gr_model_info()]'s
#'   `max_output`.)
#' @seealso [gr_model_info()], [gr_budget()]
#' @family cost and token functions
#' @export
#' @examples
#' gr_model_limits("gpt-4o")
gr_model_limits <- function(model = NULL) {
  info <- gr_model_info(model)
  list(context_window = info$context_window, output_tokens = info$max_output)
}

#' List every known model
#' @return A data frame with `id`, `kind`, `context_window`, `max_output`,
#'   `reasoning`, `input_usd_per_1m`, `output_usd_per_1m` and `as_of` (the month
#'   the entry was recorded -- a stale date is a prompt to verify).
#' @seealso [gr_model_info()], [gr_register_model()]
#' @family cost and token functions
#' @export
#' @examples
#' subset(gr_models(), kind == "chat")[, c("id", "context_window", "as_of")]
gr_models <- function() {
  all <- utils::modifyList(gr_state$seed_models %||% list(), gr_state$models %||% list())
  if (!length(all)) return(data.frame())
  df <- do.call(rbind, lapply(names(all), function(nm) {
    e <- utils::modifyList(.gr_unknown_model, all[[nm]])
    data.frame(id = nm, kind = as_chr1(e$kind, NA_character_),
               context_window = as_int1(e$context_window),
               max_output = as_int1(e$max_output), reasoning = isTRUE(e$reasoning),
               input_usd_per_1m = as_num1(e$input_usd),
               output_usd_per_1m = as_num1(e$output_usd),
               as_of = as_chr1(e$as_of, NA_character_), stringsAsFactors = FALSE)
  }))
  df <- df[order(df$kind, df$id), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Compute a usable input-token budget for one model call
#'
#' This is the single arithmetic chokepoint that every prompt-building path in
#' the package must go through. It has three guarantees:
#'
#' * the returned `input` budget is always **strictly positive**;
#' * `input + output <= context_window * (1 - safety_margin)`;
#' * if no positive budget exists, it raises a `gr_budget_error` naming the
#'   parameter to change, rather than returning a negative number that silently
#'   corrupts every downstream `split()` and `while` loop.
#'
#' @param model Model id.
#' @param reserve_output Tokens to reserve for the completion. An explicit value
#'   is clamped to the model's `max_output`. `NULL` reserves
#'   `min(max_output, max(min_output_tokens, usable / 4))`, where `usable` is the
#'   context window after `safety_margin` and `min_output_tokens` is a
#'   [gr_options()] setting (default 256).
#' @param overhead Tokens consumed by system prompts, question text and message
#'   framing that are not part of the document payload.
#' @param safety_margin Fraction of the context window left unused to absorb
#'   tokenizer error. Defaults to the `safety_margin` option; clamped to
#'   \[0, 0.5\].
#' @return A list with `input`, `output`, `context_window`, `overhead`, `margin`
#'   and `certain` (`FALSE` when the model's limits were guessed rather than
#'   known -- see [gr_model_info()]).
#' @seealso [gr_model_info()], [gr_estimate_cost()], [gr_count_tokens()]
#' @export
#' @family cost and token functions
#' @examples
#' unlist(gr_budget("gpt-4o", reserve_output = 1024, overhead = 200))
#'
#' # The same question leaves very different room on different models.
#' vapply(c("gpt-4", "gpt-4o", "gpt-5.6-terra"),
#'        function(m) gr_budget(m)$input, numeric(1))
#'
#' # No positive budget is an error naming what to change, never a negative
#' # number that silently reverses the document downstream.
#' tryCatch(gr_budget("gpt-4", overhead = 9000),
#'          gr_budget_error = function(e) conditionMessage(e))
gr_budget <- function(model = NULL, reserve_output = NULL, overhead = 0,
                      safety_margin = NULL) {
  info <- gr_model_info(model)
  ctx <- info$context_window
  safety_margin <- clamp(safety_margin %||% gr_options("safety_margin"), 0, 0.5)
  usable <- floor(ctx * (1 - safety_margin))

  min_out <- as.integer(gr_options("min_output_tokens"))
  reserve_output <- if (is.null(reserve_output)) {
    min(info$max_output, max(min_out, floor(usable / 4)))
  } else {
    as.integer(clamp(reserve_output, 1, info$max_output))
  }
  overhead <- as.integer(clamp(overhead, 0, Inf))

  input <- usable - reserve_output - overhead
  if (input <= 0) {
    # Try shrinking the reserve to the floor before giving up.
    reserve_output <- min(reserve_output, max(min_out, 1L))
    input <- usable - reserve_output - overhead
  }
  if (input <= 0) {
    gr_abort(sprintf(paste0("No positive input budget for model '%s': context %d, usable after a ",
                            "%.0f%% safety margin %d, output reserve %d, fixed overhead %d. ",
                            "Reduce `reserve_output`, shorten the question/system prompt, or use a ",
                            "model with a larger context window."),
                     info$id, ctx, safety_margin * 100, usable, reserve_output, overhead),
             class = "gr_budget_error")
  }
  list(input = as.integer(input), output = as.integer(reserve_output),
       context_window = as.integer(ctx), overhead = overhead, margin = safety_margin,
       certain = isTRUE(info$certain))
}

#' Estimate the USD cost of a set of calls
#'
#' @param model Model id.
#' @param input_tokens,output_tokens Token counts (scalars or vectors; vectors
#'   are summed).
#' @return A single numeric USD figure, or `NA_real_` when the model has no
#'   pricing in the registry. Pricing is seeded from the registry's `as_of`
#'   snapshot -- treat it as an estimate, and use [gr_register_model()] to
#'   correct it.
#' @seealso [gr_model_info()], [gr_register_model()], [gr_trace_summary()]
#' @family cost and token functions
#' @export
#' @examples
#' gr_estimate_cost("gpt-4o", input_tokens = 120000, output_tokens = 4000)
#' gr_estimate_cost("a-model-with-no-pricing", 1000, 100)
gr_estimate_cost <- function(model, input_tokens, output_tokens = 0) {
  info <- gr_model_info(model)
  if (is.na(info$input_usd)) return(NA_real_)
  sum(input_tokens, na.rm = TRUE) / 1e6 * info$input_usd +
    sum(output_tokens, na.rm = TRUE) / 1e6 * (info$output_usd %|z|% 0)
}
