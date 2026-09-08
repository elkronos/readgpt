# inventory.R -- what is in this folder, before anything is read.
#
# Every other pre-run check in this package is per document and happens once the
# run is already going: `preflight()` estimates a document's calls and cost at
# the moment `gr_read()` is called, and `extract_pdf()` decides a page needs OCR
# while it is extracting that page. Both are the right checks in the wrong
# place for a corpus. Point `gr_extract()` at four hundred PDFs and you find out
# that a hundred and eighty of them are scans, and that tesseract is not
# installed, after paying for the two hundred and twenty that were not.
#
# So this is the same discipline hoisted one level up, and it is deliberately
# NOT an LLM step. Whether a PDF page carries a text layer is a character count.
# Whether a file has an extractor is a lookup. How big it is is `file.size()`.
# Asking a model any of that would be slower, cost money, and be less accurate
# than the exact answer sitting on disk. What a model would be needed for --
# what a document is ABOUT -- is not what stops a corpus run.
#
# The output is one row per file INCLUDING the files that will not be read,
# because "we skipped 180 of your files" is the finding, and a table listing
# only the survivors cannot report it.

#' What is in a folder, before you read any of it
#'
#' A deterministic survey of a directory or a set of paths: what is there, which
#' files a registered extractor can read, which PDFs are scans that need OCR,
#' how big the whole thing is, and roughly what one pass would cost. It makes no
#' model calls and needs no API key.
#'
#' Run it before [gr_read_many()] or [gr_extract()] on anything you have not
#' read before. The three things it exists to catch are the three that turn a
#' corpus run into a confident wrong answer: files no extractor claims (which
#' would otherwise be dropped without appearing anywhere), PDFs with no text
#' layer (which extract to nothing and then answer `NOT_IN_DOCUMENT`,
#' indistinguishable from a document that genuinely does not say), and a folder
#' whose contents are one level further down than you scanned.
#'
#' @section What it does not do:
#' It does not decide anything for you. There is no automatic routing of files
#' to recipes here, and that is deliberate: a router that quietly reads one
#' document with `retrieve` and another with `stuff` gives you a plausible
#' answer built on part of a file with nothing saying so, and it makes
#' `gr_compare()` meaningless because the corpus no longer had *a*
#' configuration. Group the rows yourself -- `split(inv$files, inv$files$folder)`
#' is usually all it takes -- and pass each group to the recipe you chose.
#'
#' @param sources A directory, or a character vector of paths. A directory is
#'   walked; anything else is taken as given.
#' @param recursive Descend into subdirectories. `TRUE` here, unlike
#'   [gr_read_many()], because the point of the function is to show you
#'   everything.
#' @param model Model id used for the cost floor. Defaults to
#'   `gr_options("model")`.
#' @param ocr_min_chars A PDF page with fewer than this many characters of
#'   extractable text is counted as needing OCR. The same default
#'   [gr_ingest_spec()] uses, so what this predicts is what ingestion will do.
#' @param max_pdf_pages Pages sampled per PDF for the text-layer probe. The
#'   whole point is to be fast on a big folder; a scan is obvious from a few
#'   pages. `Inf` reads every page.
#' @return An object of class `gr_inventory`:
#'   \describe{
#'     \item{`files`}{One row per file found, readable or not: `file` (the path
#'       relative to `sources`, so the folder it came from survives), `folder`,
#'       `ext`, `bytes`, `extractor` (`NA` when none claims it), `status`,
#'       `pages`, `ocr_pages`, `tokens` and `note`.}
#'     \item{`by_status`}{Counts and sizes per status.}
#'     \item{`totals`}{Files, readable files, bytes, known `tokens`,
#'       `tokens_unknown` (readable files whose size cannot be counted yet), and
#'       `cost_floor_usd`.}
#'     \item{`root`}{The directory surveyed, or `NA`.}
#'   }
#'
#'   `status` is one of `"ready"` (an extractor claims it and there is text),
#'   `"needs_ocr"` (a PDF whose pages have no text layer), `"needs_package"` (an
#'   extractor claims it, but that extractor's package is not installed, so
#'   reading it would abort), `"no_extractor"`,
#'   `"empty"` (zero bytes, or nothing that reads as text -- `note` says which),
#'   or `"unreadable"` (it exists but could not be opened, or probing it raised
#'   an error; `note` carries the reason).
#'
#'   No file can stop the survey. A folder is surveyed because nobody knows what
#'   is in it yet, so a corrupt PDF, a binary file wearing a `.txt` extension, a
#'   broken symlink or anything else unforeseen becomes a row saying so, not an
#'   error. That is the same contract [gr_read_many()] gives a corpus run.
#'
#' @section Tokens, and what is left unknown:
#' `tokens` is counted exactly where counting is cheap -- plain text, markdown,
#' HTML, CSV, and PDFs from the pages actually probed, scaled by page count. For
#' formats needing an optional package that is not installed, and for scans
#' whose text does not exist until OCR runs, it is `NA` -- not a guess. Those
#' files are counted in `totals$tokens_unknown` rather than folded into the sum
#' as zeroes, so the total is always a floor and always says how far from
#' complete it is.
#'
#' `cost_floor_usd` is a floor: what a single call per document over that much
#' input would cost. Every per-chunk reader costs more, most of them by a factor
#' of the chunk count. It is there to catch the order of magnitude -- to tell
#' four dollars from four hundred -- not to be a quote.
#'
#' @seealso [gr_read_many()], [gr_extract()], [gr_extractors()],
#'   [gr_ingest_spec()] for the OCR settings this predicts
#' @family corpus functions
#' @export
#' @examples
#' d <- tempfile(); dir.create(file.path(d, "2019"), recursive = TRUE)
#' writeLines("The 2019 cohort had 482 participants.", file.path(d, "2019", "report.txt"))
#' writeLines("notes", file.path(d, "notes.doc"))   # no extractor claims .doc
#'
#' inv <- gr_inventory(d)
#' inv
#' inv$files[, c("file", "folder", "ext", "status", "tokens")]
gr_inventory <- function(sources, recursive = TRUE, model = NULL,
                         ocr_min_chars = 40L, max_pdf_pages = 3L) {
  is_dir <- is.character(sources) && length(sources) == 1L && !is.na(sources) &&
    dir.exists(sources)
  root <- if (is_dir) sources else NA_character_
  if (is_dir) {
    paths <- list.files(sources, full.names = TRUE, recursive = recursive, no.. = TRUE)
    paths <- paths[!dir.exists(paths)]
  } else {
    paths <- as.character(unlist(sources, use.names = FALSE))
    paths <- paths[!is.na(paths)]
  }
  paths <- sort(paths)
  # A symlinked directory -- a `latest -> v3` beside the versions it points at,
  # or an outright loop -- makes `list.files(recursive = TRUE)` return the same
  # file many times over. One real file became 42 rows. Deduplicate on the
  # RESOLVED path: two entries that resolve to one file are one file, however
  # many ways there are to walk to it.
  if (length(paths) > 1L) {
    real <- vapply(paths, function(x)
      tryCatch(normalizePath(x, winslash = "/", mustWork = FALSE),
               error = function(e) x), character(1), USE.NAMES = FALSE)
    paths <- paths[!duplicated(real)]
  }

  if (!length(paths)) {
    return(structure(list(files = inventory_frame(), by_status = inventory_status_frame(),
                          totals = list(files = 0L, readable = 0L, bytes = 0,
                                        tokens = 0, tokens_unknown = 0L,
                                        cost_floor_usd = NA_real_),
                          root = root, model = as_chr1(model %||% gr_options("model"))),
                     class = "gr_inventory"))
  }

  ext <- tolower(tools::file_ext(paths))
  claims <- extractor_for_ext(ext)
  # One unreadable file must not cost the survey, for the same reason one
  # unreadable document does not cost a `gr_read_many()` run: a folder is
  # surveyed precisely because nobody knows yet what is in it, so the one thing
  # this must not do is fall over on the first surprise. The probes below guard
  # what is foreseeable -- a corrupt PDF, a file that cannot be read as text --
  # and this guards what is not.
  rows <- lapply(seq_along(paths), function(i) {
    tryCatch(inventory_row(paths[i], ext[i], claims[i], root, ocr_min_chars, max_pdf_pages),
             error = function(e) {
               rel <- if (!is.na(root)) relative_path(paths[i], root) else NA_character_
               if (is.na(rel)) rel <- basename(paths[i])
               data.frame(file = rel, folder = dirname(rel), ext = ext[i],
                          # Keep what is already known. Throwing the size away
                          # because a later step failed reports less than was
                          # measured, and makes the row look unmeasurable when
                          # only the probe went wrong.
                          bytes = tryCatch(as.numeric(file.size(paths[i])),
                                           error = function(e) NA_real_),
                          extractor = if (is.na(claims[i])) NA_character_ else claims[i],
                          status = "unreadable", pages = NA_integer_,
                          ocr_pages = NA_integer_, tokens = NA_real_,
                          note = substr(conditionMessage(e), 1, 200),
                          stringsAsFactors = FALSE)
             })
  })
  files <- do.call(rbind, rows)
  rownames(files) <- NULL

  # `needs_package` is not readable: the extractor aborts on it. Counting it as
  # readable would put its size into a cost estimate for a run that cannot
  # happen.
  readable <- files$status %in% c("ready", "needs_ocr")
  # A scan's size is genuinely unknown until it is OCR'd, and on a real corpus
  # that is common enough that collapsing the whole total to NA would throw away
  # the useful answer. So: sum what is known, COUNT what is not, and let both
  # travel together. What must not happen is an unknown file counting as zero
  # and the total being reported as if it were complete -- every figure below
  # is a floor, and `tokens_unknown` is what says so.
  tok <- sum(files$tokens[readable], na.rm = TRUE)
  unknown <- sum(readable & is.na(files$tokens))
  model <- as_chr1(model %||% gr_options("model"))
  cost <- suppressWarnings(gr_estimate_cost(model, tok, sum(readable) * 500))

  by <- as.data.frame(table(factor(files$status,
                                   levels = c("ready", "needs_ocr", "needs_package",
                                              "no_extractor", "empty", "unreadable"))),
                      stringsAsFactors = FALSE)
  names(by) <- c("status", "files")
  by$bytes <- vapply(by$status, function(s) sum(files$bytes[files$status == s]), numeric(1))
  by <- by[by$files > 0L, , drop = FALSE]
  rownames(by) <- NULL

  structure(list(files = files, by_status = by,
                 totals = list(files = nrow(files), readable = sum(readable),
                               bytes = sum(files$bytes, na.rm = TRUE),
                               tokens = tok, tokens_unknown = unknown,
                               cost_floor_usd = cost),
                 root = root, model = model),
            class = "gr_inventory")
}

#' @noRd
inventory_frame <- function() {
  data.frame(file = character(0), folder = character(0), ext = character(0),
             bytes = numeric(0), extractor = character(0), status = character(0),
             pages = integer(0), ocr_pages = integer(0), tokens = numeric(0),
             note = character(0), stringsAsFactors = FALSE)
}

#' @noRd
inventory_status_frame <- function() {
  data.frame(status = character(0), files = integer(0), bytes = numeric(0),
             stringsAsFactors = FALSE)
}

#' Packages a built-in extractor needs before it can read anything at all.
#'
#' Only the hard requirements -- the ones whose absence makes the extractor
#' abort. OCR packages are not here: a PDF with a text layer reads perfectly
#' without them, and `probe_pdf()` reports separately on the ones that need OCR.
#' A third-party extractor is not listed and is assumed to be able to run, which
#' is the safe assumption in the direction that matters: it may still fail, and
#' will then be a row saying so.
#' @noRd
.gr_extractor_deps <- list(pdf = "pdftools", html = "xml2", docx = "xml2",
                           image = "tesseract")

#' @noRd
missing_extractor_deps <- function(extractor) {
  need <- .gr_extractor_deps[[as_chr1(extractor)]]
  if (!length(need)) return(character(0))
  need[!vapply(need, requireNamespace, logical(1), quietly = TRUE)]
}

#' Which registered extractor claims each extension.
#' @noRd
extractor_for_ext <- function(ext) {
  reg <- gr_extractors()
  map <- character(0)
  for (i in seq_len(nrow(reg))) {
    e <- tolower(trimws(strsplit(reg$extensions[i], ",\\s*")[[1]]))
    map[e[nzchar(e)]] <- reg$name[i]
  }
  unname(map[ext])
}

#' @noRd
inventory_row <- function(path, ext, extractor, root, ocr_min_chars, max_pdf_pages) {
  rel <- if (!is.na(root)) relative_path(path, root) else NA_character_
  if (is.na(rel)) rel <- basename(path)
  folder <- dirname(rel)
  size <- tryCatch(as.numeric(file.size(path)), error = function(e) NA_real_)

  out <- data.frame(file = rel, folder = if (identical(folder, ".")) "." else folder,
                    ext = ext, bytes = size,
                    extractor = if (is.na(extractor)) NA_character_ else extractor,
                    status = NA_character_, pages = NA_integer_, ocr_pages = NA_integer_,
                    tokens = NA_real_, note = NA_character_, stringsAsFactors = FALSE)

  if (is.na(size)) {
    out$status <- "unreadable"; out$note <- "could not be measured"; return(out)
  }
  if (size == 0) { out$status <- "empty"; out$note <- "zero bytes"; return(out) }
  if (is.na(extractor)) {
    out$status <- "no_extractor"
    out$note <- sprintf("no extractor claims '.%s'", ext)
    return(out)
  }
  # An extractor whose package is absent will ABORT on this file the moment
  # anything reads it. Reporting the file as ready would be the exact failure
  # this function exists to prevent: a survey saying a corpus is fine, and the
  # run dying on the first PDF. Caught here it costs nothing to fix.
  missing <- missing_extractor_deps(extractor)
  if (length(missing)) {
    out$status <- "needs_package"
    out$note <- sprintf("the '%s' extractor needs %s, which %s not installed",
                        extractor, paste(sprintf("'%s'", missing), collapse = " and "),
                        if (length(missing) > 1L) "are" else "is")
    return(out)
  }

  probe <- switch(extractor,
                  pdf = probe_pdf(path, ocr_min_chars, max_pdf_pages),
                  txt = , md = , html = probe_text(path),
                  list(status = "ready", tokens = NA_real_,
                       note = sprintf("size not counted for '%s' without extracting", extractor)))
  out$status <- probe$status
  out$tokens <- probe$tokens
  out$note <- probe$note %||% NA_character_
  out$pages <- probe$pages %||% NA_integer_
  out$ocr_pages <- probe$ocr_pages %||% NA_integer_
  out
}

#' @noRd
probe_text <- function(path) {
  txt <- tryCatch(readLines(path, warn = FALSE), error = function(e) NULL)
  if (is.null(txt)) return(list(status = "unreadable", tokens = NA_real_,
                                note = "could not be read as text"))
  body <- paste(txt, collapse = "\n")
  if (!nzchar(trimws(body))) {
    return(list(status = "empty", tokens = 0, note = "no text content"))
  }
  list(status = "ready", tokens = as.numeric(sum(gr_count_tokens(body))), note = NA_character_)
}

#' Does this PDF have a text layer, or is it a scan?
#'
#' The same test `extract_pdf()` applies per page, run over a sample before the
#' money is spent rather than during. Sampling matters: a scan is obvious from
#' three pages, and reading every page of four hundred PDFs to find that out
#' would defeat the purpose.
#' @noRd
probe_pdf <- function(path, ocr_min_chars, max_pdf_pages) {
  if (!requireNamespace("pdftools", quietly = TRUE)) {
    # Unreachable through gr_inventory(), which settles this earlier. Kept
    # honest anyway: "ready" here would have been a claim that the file can be
    # read, and it cannot.
    return(list(status = "needs_package", tokens = NA_real_,
                note = "the 'pdf' extractor needs 'pdftools', which is not installed"))
  }
  n <- tryCatch(pdftools::pdf_info(path)$pages, error = function(e) NA_integer_)
  if (is.na(n)) {
    return(list(status = "unreadable", tokens = NA_real_, note = "could not open the PDF"))
  }
  take <- if (is.finite(max_pdf_pages)) seq_len(min(n, max(1L, as.integer(max_pdf_pages))))
          else seq_len(n)
  pg <- tryCatch(pdftools::pdf_text(path)[take], error = function(e) NULL)
  if (is.null(pg)) {
    return(list(status = "unreadable", tokens = NA_real_, pages = n,
                note = "the PDF opened but no text could be read"))
  }
  chars <- nchar(trimws(pg))
  thin <- chars < ocr_min_chars
  # Tokens from the sampled pages, scaled to the whole document. An estimate,
  # and said to be one, rather than reading four hundred pages to be exact.
  seen <- as.numeric(sum(gr_count_tokens(paste(pg, collapse = "\n"))))
  tokens <- if (!length(take)) NA_real_ else seen * (n / length(take))
  if (all(thin)) {
    # Name what is actually missing. Listing both packages when one of them is
    # installed sends people to fix the wrong thing.
    missing <- c("tesseract", "magick")[!vapply(c("tesseract", "magick"),
                                                requireNamespace, logical(1), quietly = TRUE)]
    return(list(status = "needs_ocr", tokens = NA_real_, pages = n, ocr_pages = n,
                note = if (!length(missing)) "no text layer; will be OCR'd"
                       else sprintf("no text layer, and %s %s not installed",
                                    paste(sprintf("'%s'", missing), collapse = " and "),
                                    if (length(missing) > 1L) "are" else "is")))
  }
  list(status = "ready", tokens = tokens, pages = n,
       ocr_pages = as.integer(round(sum(thin) * (n / length(take)))),
       note = if (any(thin)) "some pages have no text layer" else NA_character_)
}

#' @export
print.gr_inventory <- function(x, ...) {
  t <- x$totals
  cat(sprintf("<gr_inventory> %s\n",
              if (is.na(x$root)) sprintf("%d path(s)", t$files) else x$root))
  cat(sprintf("  %d file(s), %s; %d readable\n", t$files, format_bytes(t$bytes), t$readable))
  if (nrow(x$by_status)) {
    cat(sprintf("  %s\n", paste(sprintf("%d %s", x$by_status$files, x$by_status$status),
                                collapse = ", ")))
  }
  cat(sprintf("  tokens: %s%s   cost floor: %s (%s, one call per document)\n",
              format(round(t$tokens), big.mark = ","),
              if (isTRUE(t$tokens_unknown > 0L))
                sprintf(" + %d file(s) not yet countable", t$tokens_unknown) else "",
              if (is.na(t$cost_floor_usd)) "unknown" else sprintf("$%.2f", t$cost_floor_usd),
              x$model))

  # The two lines somebody actually has to act on.
  ocr <- x$files[x$files$status == "needs_ocr", , drop = FALSE]
  if (nrow(ocr)) {
    stuck <- grepl("not installed", ocr$note, fixed = TRUE)
    cat(sprintf("  ! %d file(s) have no text layer%s\n", nrow(ocr),
                if (any(stuck)) " and 'tesseract'/'magick' are not installed, so they will read as empty"
                else "; they will be OCR'd, which is slower"))
  }
  dep <- x$files[x$files$status == "needs_package", , drop = FALSE]
  if (nrow(dep)) {
    pk <- sort(unique(unlist(regmatches(dep$note, gregexpr("'[^']+'", dep$note)))))
    cat(sprintf("  ! %d file(s) cannot be read at all until you install %s\n",
                nrow(dep), paste(setdiff(pk, sprintf("'%s'", unique(dep$extractor))),
                                 collapse = ", ")))
  }
  none <- x$files[x$files$status == "no_extractor", , drop = FALSE]
  if (nrow(none)) {
    e <- sort(table(none$ext), decreasing = TRUE)
    nm <- names(e); nm[!nzchar(nm)] <- "(none)"
    cat(sprintf("  ! %d file(s) would be skipped: %s. gr_register_extractor() adds a format.\n",
                nrow(none), paste(sprintf(".%s (%d)", nm, as.integer(e)), collapse = ", ")))
  }
  invisible(x)
}

#' @noRd
format_bytes <- function(b) {
  if (!length(b) || is.na(b)) return("unknown size")
  u <- c("B", "KB", "MB", "GB", "TB"); i <- 1L
  while (b >= 1024 && i < length(u)) { b <- b / 1024; i <- i + 1L }
  sprintf("%.1f %s", b, u[i])
}
