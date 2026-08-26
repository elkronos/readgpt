# core-tokenize.R -- pluggable token counting.
#
# WHY THIS FILE EXISTS
# The old `estimate_token_count()` counted whitespace-separated words. For
# English that undercounts real BPE tokens by roughly 1.3x, and far more for
# code, URLs, numbers, and non-Latin scripts. Every context-budget calculation
# in the package was built on top of it, so requests that the code believed fit
# comfortably were rejected by the API with HTTP 400 -- after the retries had
# been paid for.
#
# Token counting is now:
#   * pluggable       -- swap in a real BPE tokenizer when one is available;
#   * conservative    -- the default heuristic is deliberately biased to
#                        OVERCOUNT, because overcounting wastes a little context
#                        while undercounting produces a hard API failure;
#   * script-aware    -- CJK and other dense scripts are counted per-character.

#' Register or inspect the active tokenizer
#'
#' @param name One of the built-in tokenizers (`"heuristic"`, `"words"`,
#'   `"chars"`, `"tiktoken"`) or a name previously registered with a custom
#'   function.
#' @param fn Optional. A function taking a character vector and returning an
#'   integer vector of token counts. Supplying it registers a custom tokenizer
#'   under `name`.
#' @return Invisibly, the name of the newly active tokenizer.
#' @seealso [gr_tokenizer()], [gr_count_tokens()], [gr_budget()]
#' @family cost and token functions
#' @export
#' @examples
#' old <- gr_tokenizer()
#' gr_set_tokenizer("heuristic")
#' gr_count_tokens("the quick brown fox")
#'
#' # Register your own; budgets recompute against it immediately.
#' gr_set_tokenizer("naive_words",
#'                  function(x) lengths(strsplit(trimws(x), "\\s+")))
#' gr_count_tokens("the quick brown fox")
#' gr_set_tokenizer(old)
gr_set_tokenizer <- function(name, fn = NULL) {
  if (!is.null(fn)) {
    if (!is.function(fn)) gr_abort("`fn` must be a function.")
    gr_state$tokenizers <- utils::modifyList(gr_state$tokenizers %||% list(),
                                             stats::setNames(list(fn), name))
  }
  known <- c("heuristic", "words", "chars", "tiktoken", names(gr_state$tokenizers %||% list()))
  if (!name %in% known) {
    gr_abort(sprintf("Unknown tokenizer '%s'. Available: %s.", name,
                     paste(unique(known), collapse = ", ")))
  }
  if (identical(name, "tiktoken") && !tiktoken_available()) {
    gr_abort(paste0("Tokenizer 'tiktoken' needs the reticulate package and a Python ",
                    "'tiktoken' install. Falling back is not automatic here because a ",
                    "silent fallback would change every budget calculation."))
  }
  gr_options(tokenizer = name)
  invisible(name)
}

#' The active tokenizer
#'
#' @return The name of the tokenizer currently in use, visibly.
#' @seealso [gr_set_tokenizer()], [gr_count_tokens()]
#' @family cost and token functions
#' @export
#' @examples
#' gr_tokenizer()
gr_tokenizer <- function() gr_options("tokenizer")

#' Count tokens in text
#'
#' Vectorised over `text`. Always returns a non-negative integer vector of the
#' same length; blank and `NA` entries count as 0.
#'
#' The default `"heuristic"` tokenizer is a deliberate **over-estimate**, not an
#' exact count: it classifies each word, sums the per-script contributions, and
#' adds a small per-message framing allowance. Overcounting wastes a little
#' context; undercounting produces a hard API failure after you have paid for the
#' request. For exact counts install reticulate plus Python `tiktoken` and call
#' `gr_set_tokenizer("tiktoken")`.
#'
#' @param text Character vector.
#' @param model Optional model id; used only by tokenizers that are
#'   encoding-specific (e.g. `"tiktoken"`).
#' @return Integer vector of token counts -- a conservative upper bound under the
#'   default tokenizer.
#' @seealso [gr_set_tokenizer()], [gr_truncate_tokens()], [gr_budget()]
#' @family cost and token functions
#' @export
#' @examples
#' gr_count_tokens(c("the quick brown fox", "", "a much longer sentence than that one"))
#'
#' # Compare tokenizers on the same text.
#' old <- gr_tokenizer()
#' vapply(c("heuristic", "words", "chars"), function(t) {
#'   gr_set_tokenizer(t); gr_count_tokens("the quick brown fox jumps")
#' }, integer(1))
#' gr_set_tokenizer(old)
gr_count_tokens <- function(text, model = NULL) {
  if (is.null(text) || length(text) == 0L) return(integer(0))
  text <- vapply(text, as_chr1, character(1), USE.NAMES = FALSE)
  which <- gr_options("tokenizer")
  custom <- (gr_state$tokenizers %||% list())[[which]]
  out <- if (!is.null(custom)) {
    as.integer(custom(text))
  } else {
    switch(which,
      heuristic = tok_heuristic(text),
      words     = vapply(text, function(x) length(words_of(x)), integer(1), USE.NAMES = FALSE),
      chars     = as.integer(ceiling(nchar(text, type = "chars") / 4)),
      tiktoken  = tok_tiktoken(text, model),
      gr_abort(sprintf("Unknown tokenizer '%s'.", which))
    )
  }
  if (length(out) != length(text)) {
    gr_abort("Tokenizer returned the wrong number of counts; it must be vectorised.")
  }
  out[is.na(out) | out < 0L] <- 0L
  as.integer(out)
}

#' Conservative script-aware token estimate.
#'
#' Blends three signals and sums their contributions, then adds a small constant for
#' per-message chat overhead. Calibration targets (cl100k/o200k-family):
#'   * English prose:        ~1.30 tokens/word, ~0.25 tokens/char
#'   * Code and identifiers: ~0.77 tokens/char (L / 1.3)
#'   * CJK:                  ~1.0 tokens/char
#' @noRd
tok_heuristic <- function(text) {
  vapply(text, function(x) {
    x <- enc2utf8(x)
    if (is.na(x) || !nzchar(trimws(x))) return(0L)
    # Work on code points, so multibyte text is measured rather than its byte
    # length, and so a non-UTF-8 locale cannot make this throw.
    cp <- tryCatch(utf8ToInt(x), error = function(e) NULL)
    if (is.null(cp) || anyNA(cp)) cp <- as.integer(charToRaw(x))
    n <- length(cp); if (!n) return(0L)

    # Classify by how densely each script tokenizes. The previous version
    # treated only U+1100-U+FFEF as dense, excluding Cyrillic, Greek, Hebrew,
    # Arabic, Devanagari, Thai and every astral character including all emoji.
    # Those under-counted by up to 5.5x, and since every budget is built on this
    # number, a prompt believed to fit could be 1.5x the whole context window.
    is_ascii  <- cp < 128L
    is_astral <- cp >= 0x10000L
    is_cjk    <- (cp >= 0x2E80L & cp <= 0xA4CFL) | (cp >= 0xAC00L & cp <= 0xD7AFL) |
                 (cp >= 0xF900L & cp <= 0xFAFFL) | (cp >= 0xFE30L & cp <= 0xFFEFL) |
                 (cp >= 0x3040L & cp <= 0x30FFL)
    n_astral <- sum(is_astral); n_cjk <- sum(is_cjk)
    n_other  <- sum(!is_ascii & !is_astral & !is_cjk)

    # ASCII is estimated per whitespace-delimited word, because ordinary prose
    # (~4 chars/token) and dense strings such as base64, hex, ids and minified
    # JSON (~1.6 chars/token) cannot share one divisor without either
    # under-counting the dense case or wasting a third of the context on prose.
    ascii_est <- 0
    if (any(is_ascii)) {
      w <- words_of(intToUtf8(cp[is_ascii]))
      if (length(w)) {
        L <- nchar(w, type = "chars")
        # "Normal" = a plain word, optionally capitalised, optionally with
        # attached punctuation. Everything else is treated as dense.
        normal <- grepl("^[[:punct:]]*[A-Za-z][a-z]*[[:punct:]]*$", w, perl = TRUE)
        ascii_est <- sum(ifelse(normal, pmax(1, L / 4.0), L / 1.3))
      }
    }
    est <- ascii_est + n_cjk * 1.15 + n_other * 1.15 + n_astral * 6.5
    as.integer(ceiling(est) + 3L)   # +3 for per-message framing overhead
  }, integer(1), USE.NAMES = FALSE)
}

#' @noRd
tiktoken_available <- function() {
  requireNamespace("reticulate", quietly = TRUE) &&
    isTRUE(try(reticulate::py_module_available("tiktoken"), silent = TRUE))
}

#' @noRd
tok_tiktoken <- function(text, model = NULL) {
  if (!tiktoken_available()) gr_abort("tiktoken backend unavailable.")
  tk <- reticulate::import("tiktoken", delay_load = TRUE)
  enc <- tryCatch(tk$encoding_for_model(model %||% "gpt-4o"),
                  error = function(e) tk$get_encoding("o200k_base"))
  vapply(text, function(x) {
    if (!nzchar(x)) return(0L)
    as.integer(length(enc$encode(x)))
  }, integer(1), USE.NAMES = FALSE)
}

#' Truncate text to at most `n` tokens
#'
#' Truncation happens at a word boundary where the text has whitespace, and at a
#' character boundary where it does not (base64, a data URI, a CJK run, a minified
#' line) -- otherwise the cap would be unenforceable for exactly the inputs that
#' most need it. Returns `""` for
#' empty input and never returns `NA`.
#'
#' @param text A single string.
#' @param n Maximum token count.
#' @param marker Appended when truncation occurred; set `""` to suppress.
#' @return A single string. `""` when `n <= 0` or the input is blank -- this is
#'   not treated as an error.
#' @seealso [gr_count_tokens()]
#' @family cost and token functions
#' @export
#' @examples
#' gr_truncate_tokens(paste(rep("alpha beta gamma", 40), collapse = " "), 20)
#' gr_truncate_tokens("short enough already", 100)
gr_truncate_tokens <- function(text, n, marker = " ...[truncated]") {
  text <- as_chr1(text)
  n <- suppressWarnings(as.numeric(n)[1])
  if (is.na(n)) n <- 0
  if (is.infinite(n) && n > 0) return(text)          # Inf used to crash
  n <- as.integer(clamp(n, 0, .Machine$integer.max))
  if (n <= 0L || !nzchar(text)) return("")
  if (gr_count_tokens(text) <= n) return(text)

  # Drop the marker when it would eat the whole budget. The old code always
  # subtracted its cost, so every n <= 10 returned "" -- total content loss.
  mk <- if (gr_count_tokens(marker) < n / 2) marker else ""
  budget <- max(n - gr_count_tokens(mk), 1L)

  # Cut on whitespace where there is any, and on characters otherwise, so
  # space-free text (base64, CJK, minified JSON) truncates instead of vanishing.
  units <- words_of(text)
  joiner <- " "
  if (length(units) <= 1L) {
    units <- strsplit(text, "", fixed = TRUE)[[1]]
    joiner <- ""
  }
  if (!length(units)) return("")
  lo <- 0L; hi <- length(units)
  while (lo < hi) {
    mid <- as.integer((lo + hi + 1L) %/% 2L)
    if (gr_count_tokens(paste(units[seq_len(mid)], collapse = joiner)) <= budget) lo <- mid else hi <- mid - 1L
  }
  if (lo == 0L) {
    # Even one unit does not fit: return the largest character prefix that does.
    ch <- strsplit(text, "", fixed = TRUE)[[1]]
    k <- 0L; hi2 <- length(ch)
    while (k < hi2) {
      mid <- as.integer((k + hi2 + 1L) %/% 2L)
      if (gr_count_tokens(paste(ch[seq_len(mid)], collapse = "")) <= n) k <- mid else hi2 <- mid - 1L
    }
    return(if (k == 0L) "" else paste(ch[seq_len(k)], collapse = ""))
  }
  paste0(paste(units[seq_len(lo)], collapse = joiner), mk)
}
