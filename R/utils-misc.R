# utils-misc.R -- tiny helpers shared across the package.
#
# Design rule for this file: nothing here may make a network call, mutate global
# state outside `gr_state`, or return NULL where a caller is going to index into
# the result. The original codebase's most common crash class was a function
# returning NULL / character(0) into a caller that assumed character(1); the
# helpers here exist to make that impossible by construction.

#' Null-coalescing operator.
#' @noRd
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Empty-coalescing: falls back when `x` is NULL, length 0, all-NA, or all-"".
#' @noRd
`%|z|%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) return(y)
  if (is.character(x) && all(is.na(x) | !nzchar(x))) return(y)
  if (all(is.na(x))) return(y)
  x
}

#' Coerce anything to a single non-NA character string.
#'
#' Guards against the `NULL` / `character(0)` / `NA` / length > 1 cases that
#' repeatedly crashed `if (!nzchar(x))` style checks in the previous codebase.
#' @noRd
as_chr1 <- function(x, default = "") {
  if (is.null(x)) return(default)
  if (length(x) == 0L) return(default)
  x <- as.character(x)
  x <- x[!is.na(x)]
  if (length(x) == 0L) return(default)
  if (length(x) > 1L) x <- paste(x, collapse = "\n")
  x
}

#' Coerce to a length-one numeric, never to NULL or a zero-length vector.
#'
#' `utils::modifyList()` DELETES a key whose value is NULL, and `data.frame()`
#' silently drops a zero-length column. A registry entry holding NULL or
#' `numeric(0)` therefore produced rows with different column sets, and the
#' `rbind` that builds `gr_models()` failed. Every registry field that becomes a
#' data-frame column goes through this.
#' @noRd
as_num1 <- function(x, default = NA_real_) {
  force(x)   # see clamp(): forcing inside suppressWarnings() eats the caller's warnings
  if (is.null(x) || length(x) == 0L) return(default)
  x <- suppressWarnings(as.numeric(x)[1])
  if (is.na(x)) default else x
}

#' @noRd
as_int1 <- function(x, default = NA_integer_) {
  x <- as_num1(x, NA_real_)
  if (is.na(x) || is.infinite(x)) default else as.integer(x)
}

#' A short, safe label for whatever the user passed as `source`.
#'
#' `basename()` is bounded by PATH_MAX -- 1024 bytes on macOS, and R warns or
#' errors past it. A document passed as raw text is a length-1 character vector,
#' so the obvious `is.character(x) && length(x) == 1L` guard handed the WHOLE
#' DOCUMENT to `basename()`. On macOS that warned on every run over ~1KB (which
#' is most documents) and put a mangled fragment of the document into the trace
#' where the filename belongs; on Linux it silently did the same thing without
#' warning. Test for a path the way `gr_ingest()` does, and label anything else
#' inline.
#' @noRd
source_label <- function(source, inline = "<inline text>") {
  if (!is.character(source) || length(source) != 1L || is.na(source)) return(inline)
  if (grepl("\n", source, fixed = TRUE)) return(inline)
  if (nchar(source, type = "bytes") >= 1000L) return(inline)
  if (!file.exists(source)) return(inline)
  basename(source)
}

#' Is `x` a usable, non-blank single string?
#' @noRd
is_nonblank <- function(x) {
  !is.null(x) && length(x) == 1L && is.character(x) && !is.na(x) && nzchar(trimws(x))
}

#' Fall back to a default when a setting is missing, and say so.
#'
#' `clamp()` maps NA to the low end of the range, which is right for a bound and
#' wrong for a setting: it silently picks whichever extreme happens to be `lo`.
#' @noRd
na_default <- function(x, default, name) {
  # Not just NA: anything that cannot become a number lands on `lo` too, and
  # `gr_read_spec(mmr = "abc")` selecting pure diversity is the same defect as
  # `mmr = NA` doing it.
  usable <- length(x) == 1L && !is.na(x) &&
    (is.numeric(x) || is.finite(suppressWarnings(as.numeric(x))))
  if (usable) return(x)
  if (!identical(x, default)) {
    gr_warn(sprintf("`%s` is missing or not a single value; using the default (%s).",
                    name, format(default)), class = "gr_bad_setting")
  }
  default
}

#' Vectorised, NA-safe isTRUE.
#' @noRd
isTRUE_vec <- function(x) !is.na(x) & as.logical(x)

#' Vectorised, NA-safe "has visible content".
#' @noRd
has_content <- function(x) {
  if (is.null(x) || length(x) == 0L) return(logical(0))
  x <- vapply(x, as_chr1, character(1), USE.NAMES = FALSE)
  nzchar(trimws(x))
}

#' Safe integer sequence: `seq_len2(from, to)` is empty when `to < from`.
#'
#' Replaces the `2:length(x)` idiom, which silently counts *down* when
#' `length(x) < 2` and was a live crash in `chunk_text_semantic()`.
#' @noRd
seq_between <- function(from, to) {
  if (!is.finite(from) || !is.finite(to) || to < from) return(integer(0))
  seq.int(from, to)
}

#' Clamp a numeric into [lo, hi], announcing the change.
#'
#' Silently rewriting a user's parameter is how v1 fed documents to the model
#' backwards. Every spec constructor clamps through this, so an out-of-range
#' setting is always reported rather than quietly replaced.
#' @noRd
clamp_warn <- function(x, lo, hi, what, integer = TRUE) {
  out <- clamp(x, lo, hi)
  if (integer) out <- as.integer(out)
  same <- tryCatch(isTRUE(all.equal(as.numeric(x), as.numeric(out))), error = function(e) FALSE)
  if (!same) {
    gr_warn(sprintf("`%s` %s is outside [%s, %s]; using %s.",
                    what, format(x), format(lo), format(hi), format(out)),
            class = "gr_clamped")
  }
  out
}

#' Clamp a numeric into [lo, hi].
#' @noRd
clamp <- function(x, lo = -Inf, hi = Inf) {
  # force() FIRST, outside suppressWarnings. `x` arrives as a promise, and
  # forcing it inside suppressWarnings() swallows every warning raised while
  # EVALUATING the argument, not merely the coercion warning this is meant to
  # quiet -- so `clamp_warn(na_default(mmr, 1, "mmr"), 0, 1, "mmr")` silently
  # ate na_default()'s "using the default" warning, and any other
  # clamp(f(...)) would have eaten f()'s warnings too. Same shape as
  # `expect_warning(suppressWarnings(...))` never firing: the inner handler is
  # established first and wins.
  force(x)
  x <- suppressWarnings(as.numeric(x))
  if (length(x) == 0L || is.na(x)) x <- lo
  min(max(x, lo), hi)
}

#' Stop with a class so callers can catch package errors specifically.
#' @noRd
gr_abort <- function(msg, class = "gr_error", ...) {
  stop(structure(
    class = c(class, "gr_error", "error", "condition"),
    list(message = msg, call = sys.call(-1), ...)
  ))
}

#' Emit a warning with a package class.
#' @noRd
gr_warn <- function(msg, class = "gr_warning", ...) {
  warning(structure(
    class = c(class, "gr_warning", "warning", "condition"),
    list(message = msg, call = sys.call(-1), ...)
  ))
  invisible(NULL)
}

#' Verbosity-aware message.
#' @noRd
gr_msg <- function(...) {
  if (isTRUE(gr_options("verbose"))) message(...)
  invisible(NULL)
}

#' Deterministic content hash, with a pure-R fallback when digest is absent.
#'
#' Used for cache keys. Never returns NA.
#' @noRd
gr_hash <- function(x) {
  s <- paste(vapply(list(x), function(e) paste(utils::capture.output(utils::str(e, max.level = 3L)), collapse = "|"),
                    character(1)), collapse = "|")
  s <- paste0(s, "|", paste(as.character(unlist(x, use.names = TRUE)), collapse = "\u0001"))
  if (requireNamespace("digest", quietly = TRUE)) {
    return(digest::digest(s, algo = "xxhash64"))
  }
  hash_fallback(s)
}

#' @noRd
hash_fallback <- function(s) {
  # Deliberately not a hand-rolled hash. The previous fallback was an FNV-1a
  # loop seeded above .Machine$integer.max, so bitwXor() returned NA on the
  # first byte and EVERY input hashed identically -- every cache key collided
  # and gr_ingest() returned the first document it had ever seen, whatever file
  # or settings you asked for. A weak replacement risks the same class of
  # silent wrong-document bug, so cache-key hashing has one implementation and
  # `digest` is a hard dependency.
  gr_abort(paste0("The 'digest' package is required for cache-key hashing but is not ",
                  "installed. Install it (install.packages('digest')), or disable ",
                  "caching with gr_options(cache_documents = FALSE, cache_embeddings = FALSE)."),
           class = "gr_missing_dep")
}

#' Label bytes that ARE valid UTF-8 as UTF-8, without converting anything.
#'
#' `enc2utf8()` is a no-op on an unlabelled string in a non-UTF-8 locale, and
#' `validUTF8()` then trusts the absent label -- the same trap that made
#' `to_utf8()`'s transcoding path dead code. Anything that decodes code points
#' (`utf8ToInt`, a `(*UCP)` regex) needs the label to be right, or it silently
#' falls back to counting bytes and gives a different answer on a different
#' machine. Cheap: it touches only the strings that need it.
#' @noRd
mark_utf8 <- function(x) {
  if (!length(x)) return(x)
  x <- as.character(x)
  need <- !is.na(x) & Encoding(x) != "UTF-8" & validUTF8(x)
  if (any(need)) { tmp <- x[need]; Encoding(tmp) <- "UTF-8"; x[need] <- tmp }
  x
}

#' Coerce text to valid UTF-8, whatever it arrived as.
#'
#' Latin-1 and CP1252 exports are common in the documents this package targets,
#' and a non-UTF-8 locale marks even ASCII-adjacent strings "unknown". Either
#' made the perl regexes throw "input string 1 is invalid UTF-8" from deep
#' inside cleaning, with no class and no guidance.
#' @noRd
to_utf8 <- function(x) {
  if (!length(x)) return(character(0))
  x <- as.character(x)

  # LABEL FIRST. `enc2utf8()` treats an UNMARKED string as *native*, so in a
  # non-UTF-8 locale it re-encodes bytes that were already perfectly valid
  # UTF-8 -- corrupting them, and leaving the result neither valid UTF-8 nor
  # labelled. Everything downstream then diverged by locale: the ligature and
  # smart-quote cleaners stopped matching, `utf8ToInt()` fell back to counting
  # bytes, and the SAME document produced a different token count, different
  # chunk boundaries and a different cost estimate on a different machine.
  out <- mark_utf8(x)

  # Anything R has explicitly labelled latin1 is converted, not just relabelled.
  lat <- which(Encoding(out) == "latin1")
  if (length(lat)) out <- mark_utf8(replace(out, lat, enc2utf8(out[lat])))

  # Whatever is still not valid UTF-8 is genuinely mis-encoded bytes.
  bad <- which(is.na(out) | !validUTF8(out))
  if (length(bad)) {
    pending <- x[bad]
    fixed <- rep(NA_character_, length(bad))
    fixed[is.na(pending)] <- ""
    # CP1252 before latin1: the two agree except on 0x80-0x9F, where Windows
    # text means smart quotes, dashes and ellipses and latin1 means C1 control
    # characters. latin1 maps all 256 bytes, so it is the catch-all.
    for (from in c("CP1252", "latin1")) {
      todo <- which(is.na(fixed))
      if (!length(todo)) break
      cand <- suppressWarnings(iconv(pending[todo], from = from, to = "UTF-8"))
      good <- !is.na(cand) & validUTF8(cand)
      if (any(good)) fixed[todo[good]] <- cand[good]
    }
    # Last resort: drop the bytes that cannot be interpreted at all. Lossy, but
    # a document with three mangled characters beats an aborted run.
    todo <- which(is.na(fixed))
    if (length(todo)) {
      cand <- suppressWarnings(iconv(pending[todo], from = "UTF-8", to = "UTF-8", sub = ""))
      fixed[todo] <- ifelse(is.na(cand), "", cand)
    }
    out <- replace(out, bad, fixed)
  }
  out[is.na(out)] <- ""
  # Label again, then normalise newlines, then label once more: every base-R
  # string operation between here and the caller can drop the mark in a
  # non-UTF-8 locale.
  mark_utf8(normalise_newlines(mark_utf8(out)))
}

#' Normalise CRLF and lone CR to LF.
#'
#' Every structural regex in this package -- paragraph splitting, heading
#' detection, page-number and running-head removal -- is anchored on `\n`. A file
#' saved on Windows arrives as `\r\n`, so `\n[ \t]*\n` never matched and the
#' whole document became a single block: one chunk, one enormous prompt, and
#' every structure-aware segmenter silently degraded to `stuff`.
#' @noRd
normalise_newlines <- function(x) {
  if (!length(x)) return(x)
  # useBytes: CR and LF are ASCII, so this substitution is byte-safe, and it is
  # reached from paths that have not been transcoded yet -- a regex engine given
  # not-yet-valid UTF-8 aborts the whole ingest with "input string 1 is invalid
  # UTF-8". Bail out early when there is nothing to do, which is the common case.
  if (!any(grepl("\r", x, fixed = TRUE, useBytes = TRUE))) return(x)
  enc <- Encoding(x)
  out <- gsub("\r\n?", "\n", x, perl = TRUE, useBytes = TRUE)
  Encoding(out) <- enc
  out
}

#' Split text into words, dropping the empty leading token that
#' `strsplit(" a", "\\s+")` produces.
#' @noRd
words_of <- function(text) {
  text <- as_chr1(text)
  if (!nzchar(trimws(text))) return(character(0))
  w <- unlist(strsplit(trimws(text), "[[:space:]]+"), use.names = FALSE)
  w[nzchar(w)]
}

#' Split text into paragraphs on blank lines. Always returns a character vector
#' (possibly length 0) -- never NULL.
#' @noRd
paragraphs_of <- function(text) {
  text <- normalise_newlines(as_chr1(text))
  if (!nzchar(trimws(text))) return(character(0))
  p <- unlist(strsplit(text, "\n[ \t]*\n[ \t\n]*", perl = TRUE), use.names = FALSE)
  p <- trimws(p, which = "right")
  # Re-label. In a non-UTF-8 locale `paste`, `strsplit` and `trimws` all return
  # unmarked strings even when the input was marked UTF-8, and every downstream
  # UTF-8 pattern then silently stops matching.
  mark_utf8(p[nzchar(trimws(p))])
}

#' Split text into sentences using a conservative abbreviation-aware regex.
#' @noRd
sentences_of <- function(text) {
  text <- as_chr1(text)
  if (!nzchar(trimws(text))) return(character(0))
  # Protect common abbreviations and decimals from being treated as boundaries.
  guarded <- text
  abbrevs <- c("Mr", "Mrs", "Ms", "Dr", "Prof", "Sr", "Jr", "St", "vs", "etc",
               "e.g", "i.e", "cf", "al", "Fig", "Eq", "No", "Inc", "Ltd", "Co")
  for (a in abbrevs) {
    guarded <- gsub(paste0("\\b", gsub("\\.", "\\\\.", a), "\\."), paste0(a, "\u0001"),
                    guarded, perl = TRUE)
  }
  guarded <- gsub("(\\d)\\.(\\d)", "\\1\u0001\\2", guarded, perl = TRUE)
  parts <- unlist(strsplit(guarded, "(?<=[.!?])[ \t]+(?=[\"'(\\[]?[A-Z0-9])|\n{2,}", perl = TRUE),
                  use.names = FALSE)
  parts <- gsub("\u0001", ".", parts, fixed = TRUE)
  parts <- trimws(parts)
  parts[nzchar(parts)]
}

#' Cosine similarity between two numeric vectors. Returns 0 (not NaN) for
#' zero-norm inputs.
#' @noRd
cosine_similarity <- function(a, b) {
  if (is.null(a) || is.null(b)) return(0)
  n <- min(length(a), length(b))
  if (n == 0L) return(0)
  a <- as.numeric(a[seq_len(n)]); b <- as.numeric(b[seq_len(n)])
  da <- sqrt(sum(a * a)); db <- sqrt(sum(b * b))
  if (!is.finite(da) || !is.finite(db) || da == 0 || db == 0) return(0)
  as.numeric(sum(a * b) / (da * db))
}

#' Generate a run identifier without touching the global RNG stream.
#'
#' The previous code called `set.seed()` inside `compute_embedding()`, silently
#' destroying reproducibility of anything else in the user's session. Nothing in
#' this package may call `set.seed()`; where randomness is needed, use a
#' local, restored RNG (see `with_private_rng()`).
#' @noRd
gr_new_id <- function(prefix = "run") {
  paste0(prefix, "_", format(Sys.time(), "%Y%m%d%H%M%OS3"), "_",
         substr(gr_hash(list(Sys.time(), Sys.getpid(), gr_state$counter)), 1, 6))
}

#' Run an expression with a private RNG stream, restoring the caller's state.
#' @noRd
with_private_rng <- function(seed, expr) {
  had <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
  old <- if (had) get(".Random.seed", envir = globalenv(), inherits = FALSE) else NULL
  on.exit({
    if (had) assign(".Random.seed", old, envir = globalenv())
    else if (exists(".Random.seed", envir = globalenv(), inherits = FALSE)) {
      rm(".Random.seed", envir = globalenv())
    }
  }, add = TRUE)
  set.seed(seed)
  force(expr)
}

#' Normalise a list of named specs coming from a user (recipe fields).
#' @noRd
as_spec_list <- function(x, what = "spec") {
  if (is.null(x)) return(list())
  if (is.character(x) && length(x) == 1L) return(list(method = x))
  if (!is.list(x)) gr_abort(sprintf("`%s` must be a string or a named list, got <%s>.", what, class(x)[1]))
  x
}

# NOTE: there is deliberately no `str()` shim here. Defining one would shadow
# `utils::str` for every function in this package to save one qualified call in
# `gr_hash()`, which calls `utils::str()` directly instead.
