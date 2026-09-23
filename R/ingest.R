# ingest.R -- AXIS 1: the public ingestion entry point.
#
# WHY THIS FILE EXISTS
# `parse_text()` cached its result under `normalizePath(file_path)` and nothing
# else. Not the chunk method, not the token limit, not the cleaning flags, not
# the file's own mtime or content hash. So:
#
#     a <- parse_text(f, chunk_method = "naive",    chunk_token_limit = 3000)
#     b <- parse_text(f, chunk_method = "semantic", chunk_token_limit = 5)
#     identical(a, b)   # TRUE -- every argument of `b` was silently discarded
#
# and editing the file on disk did not invalidate anything. In a Shiny process
# the cache lived in `.GlobalEnv`, shared across every user session, with no way
# to clear it. That single defect made the repository's stated goal -- fine
# control over how information is broken up -- unreachable: whichever settings
# ran first won for the rest of the session.
#
# The cache key here covers the file identity (path + size + mtime) AND every
# option that can change the output. Ingestion also now stops at clean text:
# segmentation is a separate axis with its own cache, so re-chunking a document
# a dozen different ways costs one extraction, not twelve.

#' Describe an ingestion configuration
#'
#' @param clean A preset name (`"none"`, `"minimal"`, `"standard"`,
#'   `"academic"`, `"scan"`, `"legacy"`) or a character vector of cleaner
#'   names from `gr_cleaners()`.
#' @param ocr `"auto"` (OCR pages with no text layer), `"always"`, or `"never"`.
#' @param ocr_lang Tesseract language code.
#' @param ocr_dpi Render density for PDF OCR.
#' @param ocr_min_chars A page with fewer characters than this is treated as
#'   needing OCR under `ocr = "auto"`. Zero or more; `Inf` marks every page.
#'   Anything else warns and uses 40.
#' @param min_chars Refuse the document if less than this much text survives.
#' @param extractor Force a specific extractor name instead of dispatching on
#'   the file extension.
#' @param parallel Parallelise page-level OCR.
#' @param cleaner_opts Extra options passed to individual cleaners.
#' @param layout How a PDF page's text is put in reading order. `"auto"` drops
#'   running heads and feet (short lines repeated at the top or bottom of many
#'   pages), reads a page set in two columns one column after the other, and
#'   marks headings, taken from the PDF's bookmarks or recognised as a standard
#'   section name ("Introduction", "Methods" and so on), so blocks carry
#'   sections. `"raw"` keeps each page as pdftools lays it out. Other formats
#'   are not affected. Stored in the spec only when it is not `"auto"`, so a
#'   default spec keeps the cache and store keys it had before the setting
#'   existed.
#' @return A list of class `gr_ingest_spec`.
#' @export
#' @seealso [gr_ingest()], [gr_cleaners()] for the individual step names,
#'   [gr_clean()] to preview a configuration on your own text
#' @family ingest functions
#' @examples
#' # What each preset costs you, on a real document, before any model call.
#' vapply(c("none", "minimal", "standard", "academic", "scan"),
#'        function(p) gr_ingest(readgpt_example(), gr_ingest_spec(clean = p))$stats$chars,
#'        integer(1))
#'
#' # Or name the steps yourself; see gr_cleaners() for what is available.
#' gr_ingest_spec(clean = c("page_numbers", "references"), ocr = "never")$clean
gr_ingest_spec <- function(clean = "standard", ocr = c("auto", "always", "never"),
                           ocr_lang = "eng", ocr_dpi = 300, ocr_min_chars = 40L,
                           min_chars = 20L, extractor = NULL, parallel = NULL,
                           cleaner_opts = list(), layout = c("auto", "raw")) {
  ocr <- match.arg(ocr)
  layout <- match.arg(layout)
  spec <- structure(list(clean = clean, ocr = ocr, ocr_lang = ocr_lang, ocr_dpi = ocr_dpi,
                 # Not bare as.integer(): NA and 3e9 both became NA_integer_, and
                 # `sum(nchar(text)) < NA` then failed EVERY document with "missing
                 # value where TRUE/FALSE needed". The OCR threshold is read by
                 # ocr_threshold(), which gr_inventory() uses too, so the survey
                 # predicts what ingestion then does.
                 ocr_min_chars = ocr_threshold(ocr_min_chars),
                 min_chars = as_int1(na_default(min_chars, 20L, "min_chars"), 20L),
                 extractor = extractor, parallel = parallel, cleaner_opts = cleaner_opts),
            class = "gr_ingest_spec")
  if (!identical(layout, "auto")) spec$layout <- layout
  spec
}

#' Ingest a document into cleaned, provenance-bearing text blocks
#'
#' The first axis. Extraction happens once and is cached, so one file read can
#' feed any number of segmentations -- comparing chunking strategies costs one
#' extraction, not one per strategy. Page and section provenance survives
#' cleaning, which is what lets `ans$evidence` point back at where an answer
#' came from.
#'
#' @param source A file path, a web address, or a character vector / single
#'   string of raw text. An address starting `http://` or `https://` is
#'   downloaded and read with the extractor for what came back: a specific type
#'   the server declares, else the extension in the address, else the file's
#'   first bytes. A download that fails is an error (`gr_url_error`), one no
#'   extractor reads is refused (`gr_unsupported_format`), and the document's
#'   `source` is the address. A one-line string ending in an
#'   extension some extractor claims is taken as a path, and is an error
#'   (`gr_file_not_found`) when no such file exists. Any other string is read as
#'   text; one that looks like a path (a directory separator and an extension no
#'   extractor claims) also raises a `gr_path_as_text` warning.
#' @param spec A `gr_ingest_spec`, a bare preset name, or `NULL` for defaults.
#' @param cache Use the session document cache. The cache key includes the file's
#'   size and mtime plus every ingestion option; for a web address, the address,
#'   so it is downloaded once a session.
#' @param trace Optional `gr_trace`.
#' @return A `gr_document`: a list with `blocks` (data frame), `text`, `source`,
#'   `spec` and `stats`.
#' @export
#' @seealso [gr_ingest_spec()] for the options, [gr_cleaners()] for the step
#'   names, [gr_extractors()] for the formats, [gr_document] for what comes
#'   back, [gr_segment()] for the next axis
#' @family ingest functions
#' @examples
#' doc <- gr_ingest(readgpt_example())
#'
#' # Provenance survives cleaning; this is what `ans$evidence` points back at.
#' head(doc$blocks[, c("block_id", "page", "section", "kind")], 4)
#'
#' # What each cleaning step actually removed, in characters.
#' vapply(doc$stats$clean_log, function(s) s$chars_removed, integer(1))
#'
#' # Cleaning is a choice, not a default -- compare before committing to it.
#' raw <- gr_ingest(readgpt_example(), gr_ingest_spec(clean = "none"))
#' c(standard = doc$stats$chars, none = raw$stats$chars)
gr_ingest <- function(source, spec = NULL, cache = NULL, trace = NULL) {
  spec <- as_ingest_spec(spec)
  cache <- isTRUE(cache %||% gr_options("cache_documents"))

  # A single-line string that carries a file extension is a PATH, not prose. If
  # it does not exist, say so. Falling through to the inline-text branch means a
  # mistyped filename becomes the document: the model is handed
  # "~/reports/q3_final.pdf" as its source text and answers about the filename,
  # with no warning and partial = FALSE.
  # `scalar` and `one_line` are different questions, and conflating them meant a
  # file whose NAME contains a newline was never looked for: file.exists() was
  # never consulted, the guard below could not fire either, and the path string
  # itself became the document text -- status "ok", partial FALSE, the model
  # answering about a filename. nchar(type = "bytes") because a string holding
  # invalid UTF-8 makes the default nchar() raise.
  scalar <- is.character(source) && length(source) == 1L && !is.na(source) &&
    nchar(source, type = "bytes") < 4096
  one_line <- scalar && !grepl("\n", source)
  # A web address is fetched, not read as text; see ingest-url.R. Space around
  # it is dropped, a trailing newline included.
  url <- scalar && is_url(source)
  if (url) source <- trimws(source)
  # Only treat a string as a path when its extension is one an extractor
  # actually claims. Matching any 1-6 character suffix rejected ordinary prose
  # ending in a decimal ("...revenue of 45.2") while still swallowing missing
  # paths whose extension was absent or longer, which then became the document.
  known_ext <- unique(tolower(unlist(lapply(gr_state$extractors, `[[`, "extensions"))))
  ext_of <- function(s) tolower(sub(".*\\.([A-Za-z0-9]+)$", "\\1", s))
  looks_like_path <- one_line && !url && grepl("\\.[A-Za-z0-9]+$", source) &&
    ext_of(source) %in% known_ext &&
    !grepl("[ \t]{2,}", source) && !grepl("\\.[0-9]+$", source)
  is_path <- scalar && file.exists(source)
  if (!is_path && looks_like_path) {
    gr_abort(sprintf(paste0("File not found: '%s'. If you meant to pass document text rather than ",
                            "a path, it must not end in something that looks like a file extension."),
                     source), class = "gr_file_not_found")
  }
  # Everything below is recorded on the document, so an answer drawn from it
  # carries the warnings too. That includes a cache hit, when extraction does
  # not run again and nothing would be raised a second time.
  rec <- warning_recorder()
  # The other half of the rule above. A mistyped or unsupported extension is not
  # one an extractor claims, so the string is read as text. That is correct,
  # since prose can end in a word with a dot in it, but a one-line string with a
  # directory separator in it is almost certainly a path, and the answer that
  # follows would be about the file name.
  if (!is_path && !looks_like_path && !url && one_line && grepl("[/\\\\]", source) &&
      grepl("\\.[A-Za-z][A-Za-z0-9]{0,5}$", source) && !grepl("[ \t]{2,}", source)) {
    withCallingHandlers(
      gr_warn(sprintf(paste0("'%s' is not an existing file, and '.%s' is not an extension readgpt ",
                             "reads, so the string itself is being read as the document. If it is ",
                             "a path, check it; readgpt reads %s."),
                      source, ext_of(source),
                      paste0(".", sort(known_ext), collapse = ", ")),
              class = "gr_path_as_text"),
      gr_warning = rec$record)
  }

  if (is_path) {
    fi <- file.info(source)
    key <- gr_hash(list(normalizePath(source, winslash = "/", mustWork = FALSE),
                        fi$size, as.numeric(fi$mtime), unclass(spec)))
  } else if (url) {
    # The address, not what it serves today: a page that changes during a
    # session is read as it was the first time, as a file is until it is saved.
    key <- gr_hash(list("url", source, unclass(spec)))
  } else {
    # Hash the SAME string the pipeline goes on to ingest. Joining with "\n"
    # here while ingestion joined with "\n\n" made a two-element vector hash
    # identically to a one-element string containing a single newline, and the
    # wrong cached document came back.
    key <- gr_hash(list("inline",
                        paste(vapply(source, as_chr1, character(1), USE.NAMES = FALSE),
                              collapse = "\n\n"),
                        unclass(spec)))
  }

  if (cache && !is.null(gr_state$doc_cache[[key]])) {
    gr_msg("Using cached ingestion for this document + settings.")
    doc <- gr_state$doc_cache[[key]]
    trace_note(trace, "ingest", list(cached = TRUE, blocks = nrow(doc$blocks)))
    return(doc)
  }

  if (is_path || url) {
    path <- source
    if (url) {
      gr_msg(sprintf("Fetching '%s'.", source))
      path <- fetch_url(source)
      on.exit(unlink(path), add = TRUE)
    }
    ext <- tolower(tools::file_ext(path))
    ex <- if (!is.null(spec$extractor)) registry_get("extractors", spec$extractor)
          else extractor_for(ext)
    if (is.null(ex)) {
      gr_abort(sprintf("No extractor for '.%s'. Registered extensions: %s. Add one with gr_register_extractor().",
                       ext, paste(sort(unique(unlist(lapply(gr_state$extractors, `[[`, "extensions")))),
                                  collapse = ", ")),
               class = "gr_unsupported_format")
    }
    gr_msg(sprintf("Extracting '%s' with the '%s' extractor.",
                   if (url) source else basename(source), ex$name))
    raw <- withCallingHandlers(ex$fn(path, spec), gr_warning = rec$record)
    unread <- attr(raw, "gr_unread_pages", exact = TRUE)
    blocks <- as_blocks(raw)
    blocks$text <- to_utf8(blocks$text)
    src <- if (url) source else normalizePath(source, winslash = "/", mustWork = FALSE)
  } else {
    txt <- paste(vapply(source, as_chr1, character(1), USE.NAMES = FALSE), collapse = "\n\n")
    txt <- to_utf8(txt)
    blocks <- as_blocks(data.frame(text = paragraphs_of(txt), stringsAsFactors = FALSE))
    unread <- NULL
    src <- "<inline text>"
  }
  unread <- sort(unique(as.integer(unread[!is.na(unread)])))

  raw_chars <- sum(nchar(blocks$text))
  steps <- resolve_clean_steps(spec$clean)
  cleaned <- withCallingHandlers(
    gr_clean(blocks$text, steps = steps,
             opts = utils::modifyList(as.list(spec), spec$cleaner_opts %||% list())),
    gr_warning = rec$record)
  clean_log <- attr(cleaned, "gr_clean_log")
  blocks$text <- mark_utf8(as.character(cleaned))
  blocks <- blocks[has_content(blocks$text), , drop = FALSE]
  rownames(blocks) <- NULL

  if (!nrow(blocks) || sum(nchar(blocks$text)) < spec$min_chars) {
    gr_abort(sprintf(paste0("Only %d characters survived ingestion of '%s' (minimum %d). ",
                            "The file may be empty, image-only with OCR disabled, or your ",
                            "cleaning steps may be too aggressive -- %d characters were removed ",
                            "by cleaners: %s."),
                     sum(nchar(blocks$text)), basename(src), spec$min_chars,
                     raw_chars - sum(nchar(blocks$text)),
                     paste(vapply(clean_log, function(l) sprintf("%s(-%d)", l$step, l$chars_removed),
                                  character(1)), collapse = " ")),
             class = "gr_empty_document")
  }

  blocks$block_id <- seq_len(nrow(blocks))
  doc <- structure(list(
    blocks = blocks,
    text = mark_utf8(paste(blocks$text, collapse = "\n\n")),
    source = src,
    spec = spec,
    stats = list(blocks = nrow(blocks), chars = sum(nchar(blocks$text)),
                 chars_removed = raw_chars - sum(nchar(blocks$text)),
                 tokens = sum(gr_count_tokens(blocks$text)),
                 # A trailing page that never became text is still a page of the
                 # document; counting only pages with blocks under-reported it.
                 pages = if (all(is.na(blocks$page)) && !length(unread)) NA_integer_
                         else as.integer(max(c(blocks$page, unread), na.rm = TRUE)),
                 clean_steps = steps, clean_log = clean_log,
                 unread_pages = unread),
    warnings = rec$get()
  ), class = "gr_document")

  if (cache) gr_state$doc_cache[[key]] <- doc
  trace_note(trace, "ingest", list(cached = FALSE, source = basename(src),
                                   blocks = nrow(blocks), tokens = doc$stats$tokens,
                                   clean_steps = steps))
  gr_msg(sprintf("Ingested %d block(s), ~%d tokens (%d chars removed by cleaning).",
                 doc$stats$blocks, doc$stats$tokens, doc$stats$chars_removed))
  doc
}

#' @noRd
as_ingest_spec <- function(spec) {
  if (inherits(spec, "gr_ingest_spec")) return(spec)
  if (is.null(spec)) return(gr_ingest_spec())
  if (is.character(spec) && length(spec) == 1L) return(gr_ingest_spec(clean = spec))
  if (is.list(spec)) return(do.call(gr_ingest_spec, spec))
  gr_abort("`ingest` must be a gr_ingest_spec, a preset name, or a named list.")
}

#' @export
print.gr_document <- function(x, ...) {
  cat(sprintf("<gr_document> %s\n", x$source))
  cat(sprintf("  %d blocks, ~%d tokens, %d chars (%d removed by cleaning)\n",
              x$stats$blocks, x$stats$tokens, x$stats$chars, x$stats$chars_removed))
  if (!is.na(x$stats$pages)) cat(sprintf("  %d page(s)\n", x$stats$pages))
  cat(sprintf("  cleaners: %s\n", paste(x$stats$clean_steps, collapse = ", ") %|z|% "none"))
  unread <- x$stats$unread_pages %||% integer(0)
  if (length(unread)) {
    cat(sprintf("  pages that never became text: %s\n",
                paste(utils::head(unread, 20), collapse = ", ")))
  }
  w <- x[["warnings", exact = TRUE]] %||% character(0)
  if (length(w)) cat(sprintf("  %d warning(s), in $warnings\n", length(w)))
  cat(sprintf("  first block: %s\n", substr(x$blocks$text[1], 1, 120)))
  invisible(x)
}

#' @export
as_json.gr_document <- function(x, pretty = TRUE, ...) {
  as_json.default(list(source = x$source, stats = x$stats,
                       blocks = x$blocks), pretty = pretty, ...)
}
