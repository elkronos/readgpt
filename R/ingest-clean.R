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
#'   across the whole document (dropping a trailing bibliography, or detecting
#'   a running head by how often a line recurs). A document-scoped step that was
#'   applied per block would simply never fire. Its result is matched back to
#'   the blocks line by line, so such a step should remove whole lines, or cut
#'   the text short (at the start of a line or inside one), and leave the
#'   lines it keeps as they were: each block then keeps whichever of its lines
#'   survived. A step that rewrites text inside lines is matched line for line
#'   when it keeps every line; otherwise it is applied to each block on its
#'   own, with a `gr_clean_unmapped` warning.
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
                                      description = description, default_on = default_on,
                                      registered = registration_stamp()))
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
#'   given. This is what stops digit removal from running before the page and
#'   figure filters that need digits to match.
#' @param opts Named list passed to every step.
#' @return The cleaned character vector, with a `"gr_clean_log"` attribute
#'   recording characters removed per step.
#' @export
#' @seealso [gr_cleaners()] for the step names, [gr_register_cleaner()],
#'   [gr_ingest_spec()] to use a configuration in a real run
#' @family ingest functions
#' @examples
#' # Listed late-then-early, but page_numbers still runs FIRST; otherwise
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
      mapped <- map_document_step(text, joined, cleaned)
      if (is.null(mapped)) {
        # Re-running a document-level decision on one block cannot repeat it
        # (a running head needs the other pages to be seen as one), so this is
        # a fallback, and it says so rather than passing for the real thing.
        gr_warn(sprintf(paste0("The document-scoped cleaner '%s' rewrote text inside lines, so its ",
                               "result could not be matched back to the blocks it came from. It ",
                               "was applied to each block on its own instead."), s),
                class = "gr_clean_unmapped")
        mapped <- vapply(text, function(bt) as_chr1(reg[[s]]$fn(bt, opts)), character(1),
                         USE.NAMES = FALSE)
      }
      text <- mapped
    } else {
      text <- vapply(text, function(tx) as_chr1(reg[[s]]$fn(tx, opts)), character(1), USE.NAMES = FALSE)
    }
    log[[s]] <- list(step = s, stage = reg[[s]]$stage, scope = reg[[s]]$scope %||% "block",
                     chars_removed = before - sum(nchar(text)))
  }
  attr(text, "gr_clean_log") <- log
  text
}

#' Map a document-scoped step's result back onto the blocks it was given.
#'
#' The step saw `joined`, the blocks separated by blank lines, and returned
#' `cleaned`. Keeping a block only when its WHOLE text survived deleted every
#' block the step edited in part: a paragraph that began with a running head,
#' or a conclusion with the References heading on its next line, went with the
#' line the step removed. So the result is matched line by line instead: each
#' non-blank line of `cleaned` is found, in order, among the lines of `joined`,
#' and every block keeps the lines of it that survived. A block none of whose
#' lines survived comes back as "".
#'
#' That covers a step that removes whole lines, which is what the built-in
#' ones do. A line of the result may also be the start of a line of the
#' input: the step cut the text short inside that line, and took out what
#' followed, to the end ("6 References" and the bibliography after it) or up
#' to a later line it kept (an acknowledgements section). The block the cut
#' fell in keeps what the step left of that line. So a step that removes
#' running heads AND cuts the bibliography off inside a line is mapped too;
#' only a cut that left the input a prefix of itself used to be, so that one
#' fell back and kept the whole bibliography. A line is taken as a copy of
#' the next line of the input that is the same, when there is one, and only
#' otherwise as the start of the nearest line that begins with it; a heading
#' "Results" is not mistaken for a cut running head "Results of the trial"
#' that the step dropped.
#'
#' A step that rewrites text inside lines but keeps every line is mapped line
#' for line. Anything else returns NULL, and the caller falls back.
#' @noRd
map_document_step <- function(text, joined, cleaned) {
  if (identical(cleaned, joined)) return(ifelse(has_content(text), text, ""))
  # A trailing "\n" is appended so strsplit() keeps a block's own trailing
  # empty line. With it, the lines of `joined` are exactly each block's lines
  # with one empty separator line between blocks.
  lines_of <- function(s) strsplit(paste0(s, "\n"), "\n", fixed = TRUE)[[1]]
  orig <- lines_of(joined)
  n <- vapply(text, function(t) length(lines_of(t)), integer(1), USE.NAMES = FALSE)
  owner <- rep(rbind(seq_along(text), 0L), rbind(n, 1L))
  owner <- owner[seq_len(length(owner) - 1L)]     # no separator after the last block
  if (length(owner) != length(orig)) return(NULL)
  out_lines <- lines_of(cleaned)
  blank <- !nzchar(trimws(orig))

  by_block <- function(x) split(x, factor(owner, levels = c(0L, seq_along(text))))[-1L]

  # Blank lines carry nothing to match; a step may add or drop them freely.
  # The copies of each distinct input line, in order, with a pointer to the
  # first one not yet passed: finding the next copy of a line never rescans.
  wanted <- out_lines[nzchar(trimws(out_lines))]
  first <- match(orig, orig)
  groups <- unique(first)
  copies <- split(seq_along(orig), factor(first, levels = groups))
  slot <- integer(length(orig))
  slot[groups] <- seq_along(groups)
  next_copy <- rep(1L, length(groups))
  wanted_slot <- slot[match(wanted, orig)]         # NA: no input line is the same

  kept <- rep(FALSE, length(orig))
  now <- orig                                      # what each input line keeps
  subsequence <- TRUE
  i <- 1L
  for (k in seq_along(wanted)) {
    j <- NA_integer_
    g <- wanted_slot[k]
    if (!is.na(g)) {
      at <- copies[[g]]
      while (next_copy[g] <= length(at) && at[next_copy[g]] < i) next_copy[g] <- next_copy[g] + 1L
      if (next_copy[g] <= length(at)) j <- at[next_copy[g]]
    }
    if (is.na(j)) {
      # No copy of the line left: the start of the nearest line, cut inside it.
      while (i <= length(orig) && !startsWith(orig[i], wanted[k])) i <- i + 1L
      if (i > length(orig)) { subsequence <- FALSE; break }
      j <- i
      now[j] <- wanted[k]
    }
    kept[j] <- TRUE
    i <- j + 1L
  }
  if (!subsequence) {
    # Not the input with lines taken out. The same number of lines means text
    # was rewritten in place, and each block takes back its own lines.
    if (length(out_lines) != length(orig)) return(NULL)
    return(vapply(by_block(out_lines), function(l) {
      t <- paste(l, collapse = "\n")
      if (nzchar(trimws(t))) t else ""
    }, character(1), USE.NAMES = FALSE))
  }

  idx <- by_block(seq_along(orig))
  cut <- now != orig
  vapply(seq_along(text), function(b) {
    own <- idx[[b]]
    content <- own[!blank[own]]
    if (!length(content) || !any(kept[content])) return("")
    if (all(kept[content]) && !any(cut[own])) return(text[b])
    # Not blank: a line of the result it kept (or the start of one) never is.
    paste(now[own[kept[own] | blank[own]]], collapse = "\n")
  }, character(1), USE.NAMES = FALSE)
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
    description = paste0("Rejoin words split across line breaks by PDF layout ('mito-\\nchondria', ",
                         "'LIA-\\nBLE' in capitals); number ranges ('18-\\n65') and compounds ",
                         "('Anglo-\\nSaxon') are left alone"),
    fn = function(x, o) rejoin_hyphenated(x))

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
    description = "Replace every digit with a space. OFF by default: this makes figures, dates and percentages unanswerable",
    fn = function(x, o) gsub("\\d+", " ", x, perl = TRUE))

  gr_register_cleaner("remove_punctuation", stage = "late", default_on = FALSE,
    description = "Replace punctuation with spaces. OFF by default: destroys sentence boundaries",
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

#' Rejoin words that PDF layout split at a line break with a hyphen.
#'
#' A letter before the hyphen and a lower-case letter after it. `\w` also
#' matched digits, so a range that wrapped at its hyphen ("aged 18-" / "65
#' years") became one wrong number, "aged 1865 years", and a quote of that
#' number then checked out against the document. In mixed-case text an
#' upper-case letter after the break is a compound ("Anglo-" / "Saxon"), not a
#' split word. `[ \t]` rather than `\s`, so the match cannot run across a
#' blank line; `\r?`, so text with Windows line ends is rejoined too.
#'
#' Text set in capitals (a disclaimer, a contract's liability clause) has
#' nothing but capitals after its breaks, so that rule never fired there:
#' "LIA-" / "BLE" stayed split, and a quote of "LIABLE" failed to check out. A
#' word of capitals broken before more capitals is rejoined as well when the
#' text around it is set in capitals, and left alone in mixed-case text, where
#' the pair is two abbreviations ("the HIV-" / "AIDS epidemic", "analysed by
#' LC-" / "MS/MS").
#'
#' What counts as the text around it: the nearest 12 letters before the pair
#' on its first line, and the nearest 12 after it on its second. A side is set
#' in capitals when those letters have at least five capitals and at most one
#' lower-case letter ("Le CONTRAT DE LI-", "(a) FOR ANY INDI-"), and mixed case
#' when they have a lower-case letter and are not set in capitals. The pair is
#' joined when either side is set in capitals, or when neither is mixed case (a
#' heading on lines of its own, "8. INDEMNI-" / "FICATION"). Judging by the one
#' nearest word, as the rule first did, joined pairs in ordinary prose whenever
#' an abbreviation stood beside them: "A PET-" / "CT scan", "Phase II HIV-" /
#' "AIDS", "LC-" / "MS/MS". A lone word of capitals broken inside mixed-case
#' text ("Please read CARE-" / "FULLY") looks exactly like "the HIV-" / "AIDS
#' epidemic" and is left split too; that is the price of keeping those apart.
#' Lines above or below the pair are not looked at: a heading in capitals on
#' the line before ("RESULTS" / "US-" / "UK trade grew") says nothing about the
#' prose that follows it.
#'
#' The block is cut into lines and only the ends of lines are looked at, so the
#' time grows with the length of the block. Matching the pairs across the whole
#' block and slicing it by character position once per pair took time in
#' proportion to the length times the number of pairs: a 2 MB block in capitals
#' took a minute.
#' @noRd
rejoin_hyphenated <- function(x) {
  if (length(x) != 1L) return(vapply(x, rejoin_hyphenated, character(1), USE.NAMES = FALSE))
  if (is.na(x)) return(x)
  x <- gsub("(\\p{L})[-\u2010\u2011][ \t]*\r?\n[ \t]*(\\p{Ll})", "\\1\\2", x, perl = TRUE)
  if (!grepl("\\p{Lu}[-\u2010\u2011][ \t]*\r?\n[ \t]*\\p{Lu}", x, perl = TRUE)) return(x)
  # A trailing "\n" makes strsplit() keep a final empty line, so pasting the
  # lines back with "\n" gives `x` again exactly.
  lines <- strsplit(paste0(x, "\n"), "\n", fixed = TRUE)[[1]]
  n <- length(lines)
  # A line that ends in a word of two or more capitals and a hyphen, and a line
  # that starts with a word of capitals.
  tail_at <- regexpr("(?<!\\p{L})\\p{Lu}{2,}[-\u2010\u2011][ \t]*\r?$", lines, perl = TRUE)
  head_at <- regexpr("^[ \t]*\\p{Lu}+(?!\\p{L})", lines, perl = TRUE)
  i <- which(tail_at[-n] > 0L & head_at[-1L] > 0L)
  if (!length(i)) return(x)
  before <- gsub("\\P{L}+", "", substr(lines[i], 1L, tail_at[i] - 1L), perl = TRUE)
  before <- substring(before, pmax(1L, nchar(before) - 11L))
  after <- substring(lines[i + 1L], head_at[i + 1L] + attr(head_at, "match.length")[i + 1L])
  after <- substr(gsub("\\P{L}+", "", after, perl = TRUE), 1L, 12L)
  count <- function(s, class) nchar(gsub(sprintf("[^%s]+", class), "", s, perl = TRUE))
  low_b <- count(before, "\\p{Ll}"); low_a <- count(after, "\\p{Ll}")
  capitals <- (low_b <= 1L & count(before, "\\p{Lu}") >= 5L) |
    (low_a <= 1L & count(after, "\\p{Lu}") >= 5L)
  j <- i[capitals | (low_b == 0L & low_a == 0L)]
  if (!length(j)) return(x)
  lines[j] <- sub("[-\u2010\u2011][ \t]*\r?$", "", lines[j], perl = TRUE)
  lines[j + 1L] <- sub("^[ \t]+", "", lines[j + 1L], perl = TRUE)
  sep <- rep("\n", n - 1L)
  sep[j] <- ""
  paste0(lines, c(sep, ""), collapse = "")
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
  legacy = c("page_numbers", "captions", "urls", "emails", "references",
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
