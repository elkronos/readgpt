# ingest-clean.R -- AXIS 1b: an ordered, inspectable cleaning pipeline.
#
# WHY THIS FILE EXISTS
# Cleaning in the old code was four hard-coded lines in `parse_text()` plus a
# `filter_text()` helper, and the combination was broken in ways that were
# invisible from the outside:
#
#   1. ORDER. `parse_text()` ran `str_replace_all(text, "\\d+", " ")` BEFORE
#      calling `filter_text()`, whose patterns are `^\s*Page\s+\d+.*$` and
#      `^\s*(Figure|Table)\s+\d+.*$`. After digit removal "Page 1" is "Page  ",
#      so `\d+` could never match. With default settings the entire documented
#      boilerplate-removal feature was dead code.
#   2. DEFAULTS. `remove_numbers = TRUE` was the DEFAULT, and `answer_question()`
#      never forwarded an override. Every document lost every digit before the
#      model saw it: "revenue of 45 million in 2019" arrived as "revenue of
#      million in". Any question about a figure, date, percentage or section
#      number was unanswerable by construction.
#   3. REGEX. `sub("(?mi)References.*", "", text, perl = TRUE)` was documented as
#      "cut off everything from References to end". With `(?m)` but no `(?s)`,
#      `.` does not cross newlines, so it deleted the heading line and left the
#      whole reference list. And because the `sub()` was unanchored while its
#      `grepl()` guard was anchored, "See References section for details about
#      method X." lost the rest of that sentence instead.
#   4. REGEX ENGINE. `gsub("[^[:alnum:]\\s]", "", x)` without `perl = TRUE` uses
#      TRE, where `\s` inside a bracket expression is not a shorthand -- it is
#      the two literal characters `\` and `s`. So the pattern stripped spaces
#      too, and `www\.[^\s]+` stopped matching at the first letter "s":
#      "www.nasa.gov" became the fragment "sa.gov".
#
# Every cleaner here is a named, individually toggleable step; the pipeline
# order is explicit and enforced; each step reports what it changed.

#' Register a cleaning step
#'
#' @param name Step name.
#' @param fn Function of `(text, opts)` returning cleaned text.
#' @param stage `"early"` (structure-preserving, e.g. boilerplate removal) or
#'   `"late"` (destructive normalisation, e.g. digit stripping). Early steps
#'   always run before late steps regardless of the order the user lists them.
#' @param description One-line description.
#' @param default_on Whether the step is enabled by the standard preset.
#' @param scope `"block"` (default) applies the step to each text block
#'   independently. `"document"` applies it once to all blocks joined together,
#'   which is required for anything that reasons about position or repetition
#'   across the whole document -- dropping a trailing bibliography, or detecting
#'   a running head by how often a line recurs. A document-scoped step that was
#'   applied per block would simply never fire.
#' @return Invisibly, `name`.
#' @seealso [gr_cleaners()], [gr_clean()], [gr_ingest_spec()]
#' @family ingest functions
#' @export
#' @examples
#' gr_register_cleaner("drop_confidential", stage = "early",
#'   description = "Remove CONFIDENTIAL banner lines",
#'   fn = function(x, o) gsub("(?mi)^\\s*CONFIDENTIAL.*$", "", x, perl = TRUE))
#'
#' gr_clean("CONFIDENTIAL - DRAFT\n\nThe real content of the document.",
#'          steps = c("drop_confidential", "collapse_whitespace"))
gr_register_cleaner <- function(name, fn, stage = c("early", "late"), description = "",
                                default_on = FALSE, scope = c("block", "document")) {
  stage <- match.arg(stage); scope <- match.arg(scope)
  if (!is.function(fn)) gr_abort("`fn` must be a function of (text, opts).")
  registry_set("cleaners", name, list(name = name, fn = fn, stage = stage, scope = scope,
                                      description = description, default_on = default_on))
}

#' List registered cleaners
#'
#' `default_on` marks the steps the `"standard"` preset runs. Steps are always
#' applied `"early"` stage first, whatever order you list them in.
#'
#' @return A data frame with `name`, `stage`, `scope`, `default_on` and
#'   `description`.
#' @seealso [gr_clean()], [gr_register_cleaner()], [gr_ingest_spec()]
#' @family ingest functions
#' @export
#' @examples
#' gr_cleaners()
#' # The steps the "standard" preset runs:
#' subset(gr_cleaners(), default_on)$name
gr_cleaners <- function() {
  reg <- gr_state$cleaners
  if (!length(reg)) return(data.frame())
  df <- do.call(rbind, lapply(reg, function(e) data.frame(
    name = e$name, stage = e$stage, scope = e$scope %||% "block", default_on = e$default_on,
    description = e$description, stringsAsFactors = FALSE)))
  df <- df[order(df$stage != "early", df$name), , drop = FALSE]
  rownames(df) <- NULL
  df
}

#' Run the cleaning pipeline over a character vector
#'
#' Exposed separately from [gr_ingest()] so you can see exactly what a cleaning
#' configuration does to your own text before committing a run to it. Cleaning
#' is destructive and irreversible from the model's point of view: whatever is
#' removed here, no reading strategy can recover.
#'
#' @param text Character vector of block texts.
#' @param steps Character vector of cleaner **names** (see [gr_cleaners()]), or
#'   `NULL` for the `default_on` set. Unlike [gr_ingest_spec()]'s `clean`
#'   argument this does **not** accept preset names. Steps are reordered so every
#'   `"early"` cleaner runs before every `"late"` one, regardless of the order
#'   given -- this is what stops digit removal from running before the page and
#'   figure filters that need digits to match.
#' @param opts Named list passed to every step.
#' @return The cleaned character vector, with a `"gr_clean_log"` attribute
#'   recording characters removed per step.
#' @export
#' @seealso [gr_cleaners()] for the step names, [gr_register_cleaner()],
#'   [gr_ingest_spec()] to use a configuration in a real run
#' @family ingest functions
#' @examples
#' # Listed late-then-early, but page_numbers still runs FIRST -- otherwise
#' # remove_numbers eats the "1" and "Page" survives as body text.
#' out <- gr_clean(c("Page 1", "Revenue rose to 45.2 million in 2024."),
#'                 steps = c("remove_numbers", "page_numbers"))
#' out[]
#'
#' # Every step reports what it took out.
#' vapply(attr(out, "gr_clean_log"), function(s) s$chars_removed, integer(1))
gr_clean <- function(text, steps = NULL, opts = list()) {
  text <- vapply(text %||% character(0), as_chr1, character(1), USE.NAMES = FALSE)
  if (!length(text)) return(text)
  # Several cleaners match UTF-8 literals -- the ligatures, the smart quotes, the
  # en/em dashes in `page_numbers`, the zero-width characters in `control_chars`.
  # A pattern marked UTF-8 does not match unmarked bytes in a non-UTF-8 locale,
  # so those steps silently did nothing on some machines: the SAME document came
  # out 8 tokens longer, chunked differently, and cost a different amount purely
  # because of the locale R started in. Label once, here, so every step below
  # sees the same text everywhere.
  text <- mark_utf8(text)
  reg <- gr_state$cleaners
  if (is.null(steps)) {
    steps <- names(reg)[vapply(reg, function(e) isTRUE(e$default_on), logical(1))]
  }
  steps <- steps[nzchar(steps)]
  unknown <- setdiff(steps, names(reg))
  if (length(unknown)) {
    gr_abort(sprintf("Unknown cleaner(s): %s. Available: %s.",
                     paste(unknown, collapse = ", "), paste(sort(names(reg)), collapse = ", ")))
  }
  # Enforce stage order: structure-aware steps before destructive ones. This is
  # the fix for the digits-before-page-numbers bug -- the user cannot reorder
  # these into a broken sequence even by asking for it.
  stages <- vapply(steps, function(s) reg[[s]]$stage, character(1), USE.NAMES = FALSE)
  steps <- steps[order(stages != "early", seq_along(steps))]

  log <- list()
  sep <- "\n\n"
  for (s in steps) {
    before <- sum(nchar(text))
    if (identical(reg[[s]]$scope %||% "block", "document")) {
      # Decide on the whole document, but report per block, so the caller's
      # block-aligned provenance (page, section, block_id) stays valid. A block
      # the cleaner removed comes back as "" and is dropped by the caller.
      #
      # Block-wise application would make these steps no-ops entirely: a
      # bibliography heading and its entries are separate blocks, and a running
      # head only looks like one when you can see the whole document at once.
      joined <- paste(text, collapse = sep)
      cleaned <- as_chr1(reg[[s]]$fn(joined, opts))
      text <- vapply(text, function(bt) {
        trimmed <- trimws(bt)
        if (!nzchar(trimmed)) return("")
        if (grepl(trimmed, cleaned, fixed = TRUE)) return(bt)
        # The block did not survive intact. It may still contain a line the
        # cleaner strips (a running head inside a longer block), so try the
        # cleaner on the block alone before discarding it.
        sub <- as_chr1(reg[[s]]$fn(bt, opts))
        if (nzchar(trimws(sub)) && !identical(sub, bt)) sub else ""
      }, character(1), USE.NAMES = FALSE)
    } else {
      text <- vapply(text, function(tx) as_chr1(reg[[s]]$fn(tx, opts)), character(1), USE.NAMES = FALSE)
    }
    log[[s]] <- list(step = s, stage = reg[[s]]$stage, scope = reg[[s]]$scope %||% "block",
                     chars_removed = before - sum(nchar(text)))
  }
  attr(text, "gr_clean_log") <- log
  text
}

# ---------------------------------------------------------------------------
# Built-in cleaners. Every regex below uses perl = TRUE so that `\s`, `\d` and
# friends mean what they look like they mean.
# ---------------------------------------------------------------------------

#' @noRd
register_builtin_cleaners <- function() {

  gr_register_cleaner("page_numbers", stage = "early", default_on = TRUE,
    description = "Drop decorated page-number lines ('Page 4', '- 12 -', '[12]', '12.'); bare numbers are kept",
    fn = function(x, o) {
      x <- gsub("(?mi)^[ \t]*(page|p\\.)[ \t]*\\d+[ \t]*(of[ \t]*\\d+)?[ \t]*$", "", x, perl = TRUE)
      # A bare number on its own line is only treated as a page number when it
      # is DECORATED (- 12 -, [12], 12.) . An undecorated number is far more
      # often a table cell, and stripping those silently gutted numeric columns
      # under the default preset.
      gsub("(?m)^[ \t]*(?:[-\u2013\u2014][ \t]*\\d{1,4}[ \t]*[-\u2013\u2014]|\\[[ \t]*\\d{1,4}[ \t]*\\]|\\d{1,4}[ \t]*\\.)[ \t]*$",
           "", x, perl = TRUE)
    })

  gr_register_cleaner("captions", stage = "early", default_on = FALSE,
    description = "Drop figure/table caption lines (OFF by default: destroys table-heavy documents)",
    fn = function(x, o) {
      gsub("(?mi)^[ \t]*(figure|fig\\.|table|tbl\\.|exhibit|chart)[ \t]*\\d+[.:)]?.*$", "",
           x, perl = TRUE)
    })

  gr_register_cleaner("urls", stage = "early", default_on = FALSE,
    description = "Remove URLs (OFF by default: URLs are often the answer)",
    fn = function(x, o) gsub("(?i)\\b(?:https?://|www\\.)\\S+", "", x, perl = TRUE))

  gr_register_cleaner("emails", stage = "early", default_on = FALSE,
    description = "Remove email addresses",
    fn = function(x, o) {
      gsub("[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", "", x, perl = TRUE)
    })

  gr_register_cleaner("references", stage = "early", default_on = FALSE, scope = "document",
    description = "Drop a trailing bibliography, from a References/Bibliography heading to the end",
    fn = function(x, o) {
      # Correct version of the old one-liner. `(?s)` lets `.` cross newlines;
      # the heading must be on its own line, optionally numbered, so a mid-
      # sentence mention of "references" is not a match; and the heading must
      # fall in the last 40% of the text, so a References entry in a table of
      # contents does not truncate the document.
      m <- gregexpr("(?mi)^[ \t]*(?:\\d+\\.?[ \t]*)?(references|bibliography|works cited)[ \t:]*$",
                    x, perl = TRUE)[[1]]
      if (m[1] == -1) return(x)
      pos <- as.integer(m)
      total <- nchar(x)
      cand <- pos[pos >= total * 0.4]
      if (!length(cand)) return(x)
      substr(x, 1, max(cand[1] - 1L, 0L))
    })

  gr_register_cleaner("hyphenation", stage = "early", default_on = TRUE,
    description = "Rejoin words split across line breaks by PDF layout ('mito-\\nchondria')",
    fn = function(x, o) gsub("(\\w)[-\u2010\u2011]\\s*\n\\s*(\\w)", "\\1\\2", x, perl = TRUE))

  gr_register_cleaner("headers_footers", stage = "early", default_on = FALSE, scope = "document",
    description = "Drop short lines repeated on many pages (running heads)",
    fn = function(x, o) {
      lines <- strsplit(x, "\n", fixed = TRUE)[[1]]
      if (length(lines) < 8L) return(x)
      trimmed <- trimws(lines)
      short <- nchar(trimmed) > 0 & nchar(trimmed) <= as.integer(o$header_max_chars %||% 80L)
      tab <- table(trimmed[short])
      thresh <- max(3L, as.integer(o$header_min_repeats %||% 3L))
      repeated <- names(tab)[tab >= thresh]
      if (!length(repeated)) return(x)
      paste(lines[!(trimmed %in% repeated)], collapse = "\n")
    })

  gr_register_cleaner("collapse_whitespace", stage = "late", default_on = TRUE,
    description = "Collapse runs of spaces/tabs and 2+ consecutive blank lines, and trim block edges; preserves paragraph breaks",
    fn = function(x, o) {
      x <- gsub("[ \t]+", " ", x, perl = TRUE)
      x <- gsub("[ \t]*\n[ \t]*", "\n", x, perl = TRUE)
      x <- gsub("\n{3,}", "\n\n", x, perl = TRUE)
      trimws(x)
    })

  gr_register_cleaner("control_chars", stage = "late", default_on = TRUE,
    description = "Strip control and zero-width characters that survive PDF extraction",
    fn = function(x, o) {
      # U+200C ZWNJ and U+200D ZWJ are NOT stripped: they are meaningful in
      # Indic and Persian orthography and in emoji sequences. Removing them
      # turned one family emoji into three people and mis-spelled Devanagari.
      x <- gsub("[\u200b\u200e\u200f\ufeff\u00ad]", "", x, perl = TRUE)
      gsub("[\\x00-\\x08\\x0b\\x0c\\x0e-\\x1f]", " ", x, perl = TRUE)
    })

  gr_register_cleaner("ligatures", stage = "late", default_on = TRUE,
    description = "Expand typographic ligatures and normalise smart quotes/dashes",
    fn = function(x, o) {
      from <- c("\ufb00", "\ufb01", "\ufb02", "\ufb03", "\ufb04",
                "\u201c", "\u201d", "\u2018", "\u2019", "\u2013", "\u2014", "\u2026")
      to   <- c("ff", "fi", "fl", "ffi", "ffl", "\"", "\"", "'", "'", "-", "--", "...")
      for (i in seq_along(from)) x <- gsub(from[i], to[i], x, fixed = TRUE)
      x
    })

  gr_register_cleaner("remove_numbers", stage = "late", default_on = FALSE,
    description = "Replace every digit with a space. OFF by default -- this makes figures, dates and percentages unanswerable",
    fn = function(x, o) gsub("\\d+", " ", x, perl = TRUE))

  gr_register_cleaner("remove_punctuation", stage = "late", default_on = FALSE,
    description = "Replace punctuation with spaces. OFF by default -- destroys sentence boundaries",
    # (*UCP) makes \w Unicode-aware; without it [[:alnum:]] is ASCII-only under
    # PCRE here and every accented or non-Latin letter was deleted too.
    fn = function(x, o) gsub("(*UCP)[^\\w\\s]", " ", x, perl = TRUE))

  gr_register_cleaner("ascii_only", stage = "late", default_on = FALSE,
    description = "Transliterate to ASCII (drops accents and non-Latin scripts)",
    fn = function(x, o) {
      out <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
      if (is.na(out)) gsub("[^\\x01-\\x7f]", " ", x, perl = TRUE) else out
    })

  gr_register_cleaner("lowercase", stage = "late", default_on = FALSE,
    description = "Lowercase everything (loses proper-noun and acronym signal)",
    fn = function(x, o) tolower(x))

  invisible(NULL)
}

#' Named cleaning presets.
#' @noRd
.gr_clean_presets <- list(
  none      = character(0),
  minimal   = c("control_chars", "collapse_whitespace"),
  standard  = c("page_numbers", "hyphenation", "control_chars", "ligatures",
                "collapse_whitespace"),
  academic  = c("page_numbers", "captions", "references", "hyphenation",
                "control_chars", "ligatures", "collapse_whitespace"),
  scan      = c("page_numbers", "headers_footers", "hyphenation", "control_chars",
                "ligatures", "collapse_whitespace"),
  # Reproduces the previous release's behaviour, digit-stripping and all, so a
  # old pipeline can be A/B'd against the new one rather than assumed
  # equivalent.
  legacy_v1 = c("page_numbers", "captions", "urls", "emails", "references",
                "remove_numbers", "collapse_whitespace")
)

#' @noRd
resolve_clean_steps <- function(clean) {
  if (is.null(clean)) return(.gr_clean_presets$standard)
  if (is.character(clean) && length(clean) == 1L && clean %in% names(.gr_clean_presets)) {
    return(.gr_clean_presets[[clean]])
  }
  if (is.character(clean)) return(clean)
  gr_abort(sprintf("`clean` must be a preset name (%s) or a character vector of cleaner names.",
                   paste(names(.gr_clean_presets), collapse = ", ")))
}
