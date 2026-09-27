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
#' adds a small per-message framing allowance. For numbers, tables, code and
#' short lines it also counts the pieces a BPE tokenizer splits text into first
#' (digit groups, punctuation runs, line breaks, indentation) and keeps the
#' larger figure, line by line. Overcounting wastes a little context;
#' undercounting produces a hard API failure after you have paid for the
#' request. Rare long words (technical and medical vocabulary) can still come in
#' under the real count, by about a tenth for a table of medical terms; that is
#' what the `safety_margin` of [gr_budget()] is for.
#'
#' For counts from OpenAI's own tokenizer install reticulate plus Python
#' `tiktoken` and call `gr_set_tokenizer("tiktoken")`. Given `model`, it counts
#' in that model's encoding. Without one, which is how the package's own budgets
#' call it, it counts in both encodings current OpenAI models use (`cl100k_base`
#' and `o200k_base`) and keeps the larger, so the count holds for any of them.
#' Models from other providers tokenize differently, and for them it is an
#' estimate without the heuristic's padding. Text that spells a special token
#' (`<|endoftext|>`, in a paper about language models) is counted as the plain
#' text it is.
#'
#' @param text Character vector.
#' @param model Optional model id; used only by tokenizers that are
#'   encoding-specific (e.g. `"tiktoken"`).
#' @return Integer vector of token counts: under the default tokenizer, an
#'   estimate built to err high (see above for the one kind of text where it
#'   can fall short).
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
  # Label valid UTF-8 as UTF-8 BEFORE decoding. `enc2utf8()` on an unlabelled
  # string is a no-op in a non-UTF-8 locale, so `utf8ToInt()` failed and the
  # byte fallback below charged a 1-codepoint em dash as 3 characters. The same
  # document then produced different token counts on different machines -- and
  # therefore different budgets, different chunk boundaries and different cost
  # estimates -- purely from the locale R happened to start in. Measured: 16
  # tokens labelled versus 24 unlabelled, for identical bytes.
  text <- mark_utf8(text)
  vapply(text, function(x) {
    if (is.na(x) || !nzchar(trimws(x))) return(0L)
    # Work on code points, so multibyte text is measured rather than its byte
    # length. The byte fallback now fires only for genuinely undecodable input.
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
      acp <- cp[is_ascii]
      a <- intToUtf8(acp)
      g <- gregexpr("[^[:space:]]+", a)[[1]]
      if (g[1] > 0L) {
        L <- attr(g, "match.length")
        w <- substring(a, g, g + L - 1L)
        # "Normal" = a plain word, optionally capitalised, optionally with
        # attached punctuation. Everything else is treated as dense.
        normal <- grepl("^[[:punct:]]*[A-Za-z][a-z]*[[:punct:]]*$", w, perl = TRUE)
        words <- list(start = as.integer(g), cost = ifelse(normal, pmax(1, L / 4.0), L / 1.3))
        # The larger of the per-word figure and the pieces floor (see
        # tok_pieces()), line by line: taken over the whole text, the prose in
        # a chunk over-counted enough to hide the table under-counted beside
        # it.
        ascii_est <- per_line_max(acp, words, tok_pieces(a, acp))
      }
    }
    est <- ascii_est + n_cjk * 1.15 + n_other * 1.15 + n_astral * 6.5
    as.integer(ceiling(est) + 3L)   # +3 for per-message framing overhead
  }, integer(1), USE.NAMES = FALSE)
}

#' A floor under the ASCII estimate, from the pieces a BPE tokenizer cuts
#' text into before it merges anything.
#'
#' cl100k and o200k split text into pieces first, and every piece is at least
#' one token: digits never join the space before them and go in groups of at
#' most three, and every run of punctuation, every line break and every run of
#' indentation is a piece of its own. The per-word estimate charges a digit
#' 0.77 of a token and a line break nothing, so against real cl100k counts it
#' came in under on exactly the text that fills a budget fastest: 0.83x on an
#' aligned table, 0.71x on tab-separated values, 0.39x on a list of single
#' digits, 0.90x on prose broken into short punctuated lines. That is the
#' direction that overruns the window. Pieces are found with the cl100k
#' pattern (letters and digits being ASCII here) and charged: a digit group or
#' a whitespace run 1; a punctuation run one per three characters, at least 1;
#' a letter run 1 up to seven letters and one more per three letters past that
#' (common words are one token, long technical ones several), plus 1 when a
#' character other than a space leads it ("\tyes", "(x"), which the vocabulary
#' seldom joins. Ordinary prose, where the per-word figure is the larger, is
#' unchanged. Measured against cl100k and o200k this lands at or above the
#' real count on tables, digit lists, tab-separated values and short
#' punctuated lines. Rare long words are what neither figure covers: a table
#' of medical terms still came in about 10% under, which is what the budget's
#' safety margin is for.
#' @noRd
tok_pieces <- function(a, cp = utf8ToInt(a)) {
  g <- gregexpr(.gr_piece_pattern, a, perl = TRUE)[[1]]
  if (g[1] < 0L) return(list(start = integer(0), cost = numeric(0)))
  # Classified by code point rather than by extracting each piece as a string:
  # this runs on every count, and the string version made counting five times
  # slower.
  n <- attr(g, "match.length")
  start <- as.integer(g)
  first <- cp[start]
  last <- cp[start + n - 1L]
  second <- cp[pmin(start + 1L, length(cp))]
  is_alpha <- function(x) (x >= 65L & x <= 90L) | (x >= 97L & x <= 122L)
  is_space <- function(x) x == 32L | (x >= 9L & x <= 13L)
  alpha <- is_alpha(last)                        # a letter run, or 's 't 're ...
  digit <- !alpha & last >= 48L & last <= 57L    # a group of one to three digits
  punct <- !alpha & !digit & (!is_space(first) | (n > 1L & !is_space(second)))
  led <- alpha & !is_alpha(first)                # " word", "(word", "'s"
  cost <- rep(1, length(n))                      # digit groups and whitespace runs
  cost[punct] <- pmax(1, n[punct] / 3)
  cost[alpha] <- pmax(1, (n[alpha] - led[alpha] - 4) / 3) +
    (led[alpha] & first[alpha] != 32L & first[alpha] != 39L)
  list(start = start, cost = cost)
}

#' Sum, over the lines of `cp`, the larger of two estimates of each line.
#'
#' `a` and `b` are lists of `start` (a position in `cp`) and `cost`.
#' @noRd
per_line_max <- function(cp, a, b) {
  nl <- which(cp == 10L)
  if (!length(nl)) return(max(sum(a$cost), sum(b$cost)))
  k <- length(nl) + 1L
  by_line <- function(x) {
    v <- numeric(k)
    if (!length(x$cost)) return(v)
    s <- rowsum(x$cost, findInterval(x$start, nl) + 1L)
    v[as.integer(rownames(s))] <- s[, 1L]
    v
  }
  sum(pmax(by_line(a), by_line(b)))
}

#' cl100k's pre-tokenizer, with its letter and digit classes written for the
#' ASCII text tok_pieces() is given.
#' @noRd
.gr_piece_pattern <- paste0("'(?i:[sdmt]|ll|ve|re)|[^\\r\\nA-Za-z0-9]?[A-Za-z]+|[0-9]{1,3}|",
                            " ?[^\\sA-Za-z0-9]+[\\r\\n]*|\\s*[\\r\\n]+|\\s+(?!\\S)|\\s+")

#' @noRd
tiktoken_available <- function() {
  requireNamespace("reticulate", quietly = TRUE) &&
    isTRUE(try(reticulate::py_module_available("tiktoken"), silent = TRUE))
}

#' @noRd
tok_tiktoken <- function(text, model = NULL) {
  if (!tiktoken_available()) gr_abort("tiktoken backend unavailable.")
  counts <- lapply(tiktoken_encodings(model), function(enc) {
    vapply(text, function(x) {
      if (!nzchar(x)) return(0L)
      # disallowed_special = (): tiktoken's default raises a ValueError on any
      # text that spells a special token, so one ML paper quoting
      # "<|endoftext|>" aborted the whole pipeline at its first count. Document
      # text is text; it is encoded as such.
      as.integer(length(enc$encode(x, disallowed_special = list())))
    }, integer(1), USE.NAMES = FALSE)
  })
  do.call(pmax, counts)
}

#' The encodings a tiktoken count is taken in.
#'
#' Every call site in the package counts without a model, so this always used
#' gpt-4o's o200k_base -- wrong for the cl100k models still in the registry
#' (gpt-4, gpt-4-turbo, gpt-3.5), which need more tokens for the same
#' non-English text, and with none of the heuristic's padding to absorb it.
#' Without a model, or for one tiktoken does not know, both encodings current
#' OpenAI models use are returned and the larger count kept.
#' @noRd
tiktoken_encodings <- function(model = NULL) {
  tk <- reticulate::import("tiktoken", delay_load = TRUE)
  if (!is.null(model)) {
    enc <- tryCatch(tk$encoding_for_model(as_chr1(model)), error = function(e) NULL)
    if (!is.null(enc)) return(list(enc))
  }
  list(tk$get_encoding("cl100k_base"), tk$get_encoding("o200k_base"))
}

#' Truncate text to at most `n` tokens
#'
#' The result is the start of `text` as written -- line breaks, indentation and
#' paragraph breaks kept -- followed by `marker`. The cut falls at the end of a
#' word where the text has whitespace. When the next unit is too long to be a
#' word (a CJK run, base64, a data URI, minified JSON, a long URL: anything that
#' alone would cost more than 16 tokens) the cut goes inside it at a character,
#' so a long unbroken run fills the budget instead of being dropped whole;
#' otherwise the cap would be unenforceable, or nearly all of it wasted, for
#' exactly the inputs that most need it. Returns `""` for empty input and never
#' returns `NA`.
#'
#' @param text A single string.
#' @param n Maximum token count.
#' @param marker Appended when truncation occurred; set `""` to suppress. It is
#'   dropped when it would cost half the budget or more.
#' @return A single string. `""` when `n <= 0` or the input is blank. This is
#'   not treated as an error.
#' @seealso [gr_count_tokens()]
#' @family cost and token functions
#' @export
#' @examples
#' gr_truncate_tokens(paste(rep("alpha beta gamma", 40), collapse = " "), 20)
#' gr_truncate_tokens("short enough already", 100)
#'
#' # A table keeps its rows.
#' cat(gr_truncate_tokens("Arm | N | Events\nA | 482 | 31\nB | 479 | 44\nC | 470 | 50", 24))
gr_truncate_tokens <- function(text, n, marker = " ...[truncated]") {
  text <- mark_utf8(as_chr1(text))
  force(n)   # see clamp(): forcing inside suppressWarnings() eats the caller's warnings
  n <- suppressWarnings(as.numeric(n)[1])
  if (is.na(n)) n <- 0
  if (is.infinite(n) && n > 0) return(text)          # Inf used to crash
  n <- as.integer(clamp(n, 0, .Machine$integer.max))
  if (n <= 0L || !nzchar(text)) return("")
  if (gr_count_tokens(text) <= n) return(text)
  # Positions are characters, and the cuts below are substr() of the text
  # itself; that needs text R can index by character.
  if (!validUTF8(text)) text <- to_utf8(text)

  # Drop the marker when it would eat the whole budget. The old code always
  # subtracted its cost, so every n <= 10 returned "" -- total content loss.
  mk <- if (gr_count_tokens(marker) < n / 2) marker else ""
  budget <- max(n - gr_count_tokens(mk), 1L)
  fits <- function(k) gr_count_tokens(substr(text, 1L, k)) <= budget
  # The largest k in [lo, hi] that fits, given that lo does (or is 0).
  widest <- function(lo, hi, at = identity) {
    while (lo < hi) {
      mid <- as.integer((lo + hi + 1L) %/% 2L)
      if (fits(at(mid))) lo <- mid else hi <- mid - 1L
    }
    lo
  }

  # Where each whitespace-delimited unit ends, in the text as written. The old
  # code re-joined the units with " ", so any truncation turned a table, an
  # outline or merge_giveup's "---"-separated findings into a single line.
  m <- gregexpr("[^[:space:]]+", text)[[1]]
  ends <- if (m[1] > 0L) as.integer(m + attr(m, "match.length") - 1L) else integer(0)
  if (!length(ends)) return("")
  whole <- widest(0L, length(ends), function(k) ends[k])
  cut <- if (whole > 0L) ends[whole] else 0L
  # Into the next unit when it is too long to be a word. Stopping before it
  # kept only what came first: a "[chunk 7 p.3]" header and none of the long
  # CJK chunk after it, 12 tokens of a 3000-token budget. A short word is not
  # cut, so prose still ends on a whole word.
  if (whole < length(ends)) {
    from <- as.integer(m[whole + 1L])
    unit <- substr(text, from, ends[whole + 1L])
    if (whole == 0L || gr_count_tokens(unit) > 16L) {
      inner <- widest(from - 1L, ends[whole + 1L] - 1L)
      if (inner >= from) cut <- inner
    }
  }
  if (cut <= 0L) return("")
  out <- paste0(substr(text, 1L, cut), mk)
  # Counts are not strictly additive; never hand back more than was asked for.
  if (gr_count_tokens(out) > n) out <- substr(text, 1L, cut)
  out
}
