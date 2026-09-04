# helper-readgpt.R
#
# The v1 test suite mocked by writing over globals -- `process_api_call <<- fake`
# and `assign(..., envir = .GlobalEnv)` -- and never restored them. testthat runs
# every file in one session, so a fake installed in test_chunked_mode.R was still
# installed when test_main_api.R ran, and the "real API" tests silently asserted
# against fakes. One file even set OPENAI_API_KEY="dummy" and left it set for the
# next file, which was a live-API file.
#
# Here, every fake is a locally-scoped mock client passed as an argument. Nothing
# is assigned into the global environment, so nothing can leak between files.

# Deterministic client that echoes which prompt it saw, so tests can assert on
# the exact call pattern a strategy produced.
mock_echo <- function(answer = "MOCK ANSWER") {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("Rate how useful", sys, fixed = TRUE)) {
      return('{"score": 8, "reason": "relevant"}')
    }
    if (grepl("reading iteratively", sys, fixed = TRUE)) {
      return(sprintf('{"can_answer": true, "answer": %s, "next_query": ""}',
                     jsonlite::toJSON(answer, auto_unbox = TRUE)))
    }
    if (grepl("Decompose text into standalone propositions", sys, fixed = TRUE)) {
      return('{"propositions": ["Alpha is one.", "Beta is two."]}')
    }
    answer
  })
}

# Client that fails every call, for exercising error paths.
mock_dead <- function(msg = "simulated transport failure") {
  gr_mock_client(function(messages, params) stop(msg))
}

# Client that returns an empty completion -- the `content: null` case that made
# v1 crash with "missing value where TRUE/FALSE needed".
mock_empty <- function() {
  gr_mock_client(function(messages, params) {
    gr_result_for_test(ok = FALSE, text = "", error = "no content")
  })
}

# Exposed so the helper can build a failure result without reaching into internals.
gr_result_for_test <- function(ok, text = "", error = NULL) {
  structure(list(ok = ok, text = text, error = error, status = 200L,
                 usage = list(input = 0L, output = 0L), model = "mock",
                 finish_reason = NA_character_, retryable = FALSE, raw = NULL),
            class = "gr_result")
}

# A document big enough that segmenters actually diverge and readers make more
# than one call. Roughly 55 tokens per paragraph, so sample_doc(4, 4) is about
# 900 tokens across 16 body paragraphs in 4 sections.
sample_doc <- function(sections = 4, paras = 4) {
  lines <- c("The cohort comprised 482 participants recruited across nine clinical sites.",
             "Adherence exceeded 91 percent in the treatment arm throughout the study.",
             "We fitted a mixed-effects model with study site as a random intercept.",
             "The primary endpoint was reached at week 24 in both randomised groups.",
             "Costs per averted event were estimated at 3,140 dollars in the base case.",
             "Baseline characteristics were balanced between the two randomised arms.",
             "Sensitivity analyses excluded every participant lost to follow-up early.",
             "Adverse events were mild and self-limiting in the large majority of cases.")
  paste(unlist(lapply(seq_len(sections), function(s) {
    c(sprintf("## Section %d", s),
      vapply(seq_len(paras), function(p) {
        idx <- ((s * 3L + p * 2L) %% length(lines)) + seq_len(4L)
        paste(lines[((idx - 1L) %% length(lines)) + 1L], collapse = " ")
      }, character(1)))
  })), collapse = "\n\n")
}

call_labels <- function(client) {
  vapply(client$calls(), function(x) x$label, character(1))
}

quiet <- function(expr) suppressWarnings(suppressMessages(force(expr)))

# ---------------------------------------------------------------------------
# Registry and option isolation.
#
# The registries live in one package-level environment, so a test that registers
# a cleaner, a segmenter or a model changes what every later test sees --
# including counts (`nrow(gr_cleaners())`) that other tests assert on. That makes
# the suite order-dependent: it passes when run as a whole and fails when a
# single file is run, or vice versa. Anything that mutates global state calls
# this first.
# ---------------------------------------------------------------------------
.gr_registry_slots <- c("extractors", "cleaners", "segmenters", "readers",
                        "models", "model_patterns")

local_registries <- function(env = parent.frame()) {
  # `readgpt:::gr_state[[s]] <- v` is a REPLACEMENT call on `readgpt`, not on
  # the state environment, and fails with "object 'readgpt' not found". Bind the
  # environment to a local name and assign into that.
  st <- readgpt:::gr_state
  snap <- stats::setNames(lapply(.gr_registry_slots, function(s) st[[s]]),
                          .gr_registry_slots)
  opts <- gr_options()
  withr::defer({
    for (s in names(snap)) st[[s]] <- snap[[s]]
    gr_options(opts)
    readgpt:::.warn_once_reset()
  }, envir = env)
  invisible(snap)
}

# Caches are global too: a document cached under one ingest spec must not decide
# what a later test sees.
local_clean_cache <- function(env = parent.frame()) {
  readgpt:::gr_flush_caches()
  withr::defer(readgpt:::gr_flush_caches(), envir = env)
}

# Strip a string's encoding label without touching its bytes.
#
# Source literals written with \u escapes are labelled UTF-8 by the parser, so a
# fixture built that way is already in the state an encoding fix produces and
# proves nothing. Text that reaches a user's session from readLines() without an
# `encoding=`, from a command-line argument, or from a connection carries no
# label at all -- that is the string that breaks serialisation and cache keys,
# and this is how a test gets one deterministically in any locale.
unmarked <- function(x) {
  x <- as.character(x)
  Encoding(x) <- "unknown"
  x
}
