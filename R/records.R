# records.R -- the search, as the reference manager exported it.
#
# WHY THIS FILE EXISTS
# Everything else in this package starts from a folder of documents, which is
# already past the step that decides whether a review can be reproduced. A
# folder cannot say which databases were searched, with what query, on what
# date, how many records came back, or which of them were never obtained. Those
# are PRISMA items 6, 7 and 16, and no amount of care downstream substitutes for
# them: a review that cannot state its search is not a systematic review however
# good its extraction is.
#
# A reference-manager export says all of it, and is the file every researcher
# already has. Reading it also fixes something subtler. `gr_synthesise()` cites
# by author and year, and until now those came from asking a model to read a
# title page -- so the one part of a citation that must be exactly right was the
# part with the loosest guarantee. From an export they are data. The model is
# never shown them and never asserts them; they are joined on afterwards.
#
# WHY NOT synthesisr OR revtools
# Both are good and both are heavier than this needs to be. RIS is one tag per
# line and BibTeX is braces; the parsing is not the hard part. The hard part is
# the dialect drift between what Scopus, Web of Science, PubMed, Scholar and
# EndNote each call the same field, and a dependency does not remove that -- it
# just moves it somewhere this package cannot see. Everything here is base R.

#' RIS tags, mapped to the fields this package uses.
#'
#' A record type may use several tags for one idea and databases disagree about
#' which. Order matters: the first tag present wins, so the more specific tag is
#' listed first.
#' @noRd
.gr_ris_map <- list(
  type     = "TY",
  authors  = c("AU", "A1", "A2", "A3", "A4"),
  # DP is the drift this map exists for: it is "database provider" in the RIS
  # specification and "date of publication" in what PubMed exports. Listed last
  # among the year aliases, so a file carrying a real PY is unaffected, and the
  # four-digit rule below rejects it if it really was a provider name.
  year     = c("PY", "Y1", "DA", "DP"),
  title    = c("TI", "T1", "BT"),
  # T2 is the journal for an article and the book for a chapter; JO/JF/JA are
  # what the older exporters use, and Scopus emits both.
  venue    = c("JO", "JF", "JA", "T2", "T3", "SO"),
  volume   = "VL",
  issue    = c("IS", "CP"),
  pages    = c("SP", "BP"),
  doi      = c("DO", "DI"),
  abstract = c("AB", "N2"),
  keywords = "KW",
  url      = c("UR", "L1", "L2"),
  # Where the PDF is, when the manager wrote one out. EndNote and Zotero both
  # use L1; Mendeley uses a "file" tag in BibTeX.
  file     = c("L1", "LK"),
  database = "DB",
  accession = c("AN", "ID")
)

#' BibTeX fields, likewise.
#' @noRd
.gr_bib_map <- list(
  authors  = "author",
  year     = c("year", "date"),
  title    = "title",
  venue    = c("journal", "booktitle", "journaltitle", "publisher"),
  volume   = "volume",
  issue    = c("number", "issue"),
  pages    = "pages",
  doi      = "doi",
  abstract = "abstract",
  keywords = c("keywords", "keyword"),
  url      = "url",
  file     = "file",
  database = c("database", "source")
)

#' The columns every record set has, whatever it was read from.
#' @noRd
.gr_record_cols <- c("record_id", "type", "authors", "year", "title", "venue",
                     "volume", "issue", "pages", "doi", "abstract", "keywords",
                     "url", "file", "database", "accession", "source_file")

#' @noRd
empty_records <- function() {
  out <- as.data.frame(stats::setNames(
    replicate(length(.gr_record_cols), character(0), simplify = FALSE), .gr_record_cols),
    stringsAsFactors = FALSE)
  out$record_id <- integer(0)
  out
}

#' Split a RIS file into records.
#'
#' A record starts at `TY  -` and ends at `ER  -`. Both are required by the
#' format and both are routinely mangled: some exporters omit the trailing `ER`
#' on the last record, and Scholar writes `TY  - ` with no type. Splitting on
#' `ER` and tolerating a missing final one is what survives real files.
#' @noRd
ris_records <- function(lines) {
  # Tag lines look like "AU  - Smith, J." -- two to four characters, then spaces
  # and a hyphen. A continuation line has no tag and belongs to the tag above.
  tag_re <- "^([A-Z][A-Z0-9]{1,3})[[:space:]]{0,2}-[[:space:]]?(.*)$"
  ends <- grep("^ER[[:space:]]{0,2}-", lines)
  starts <- grep("^TY[[:space:]]{0,2}-", lines)
  if (!length(starts)) return(list())
  # Pair each start with the first end after it; a start with no end runs to the
  # next start, or to the end of the file.
  out <- vector("list", length(starts))
  for (i in seq_along(starts)) {
    s <- starts[i]
    nxt <- if (i < length(starts)) starts[i + 1L] - 1L else length(lines)
    e <- ends[ends > s]
    stop_at <- if (length(e)) min(e[1] - 1L, nxt) else nxt
    block <- lines[s:max(s, stop_at)]
    m <- regmatches(block, regexec(tag_re, block))
    tags <- list(); last <- NULL
    for (k in seq_along(block)) {
      if (length(m[[k]]) == 3L) {
        last <- m[[k]][2]
        tags[[last]] <- c(tags[[last]], trimws(m[[k]][3]))
      } else if (!is.null(last) && nzchar(trimws(block[k]))) {
        # A wrapped abstract. Append to the tag it continues rather than
        # dropping it, which is how half an abstract goes missing.
        n <- length(tags[[last]])
        tags[[last]][n] <- paste(tags[[last]][n], trimws(block[k]))
      }
    }
    out[[i]] <- tags
  }
  out
}

#' Pull one field out of a tag list, following the alias order.
#' @noRd
tag_value <- function(tags, aliases, collapse = "; ") {
  for (a in aliases) {
    v <- tags[[a]]
    v <- v[nzchar(trimws(as.character(v)))]
    if (length(v)) return(paste(trimws(v), collapse = collapse))
  }
  NA_character_
}

#' @noRd
records_from_ris <- function(lines, source_file) {
  recs <- ris_records(lines)
  if (!length(recs)) return(empty_records())
  rows <- lapply(recs, function(tags) {
    vals <- lapply(names(.gr_ris_map), function(f) tag_value(tags, .gr_ris_map[[f]]))
    names(vals) <- names(.gr_ris_map)
    as.data.frame(c(vals, list(source_file = source_file)), stringsAsFactors = FALSE)
  })
  finish_records(do.call(rbind, rows))
}

#' Split a BibTeX file into entries.
#'
#' Brace-counting rather than a regex: a title containing braces -- which is how
#' BibTeX protects capitalisation, so `{DNA}` is common -- breaks any regex that
#' assumes the first `}` ends the field.
#' @noRd
bib_entries <- function(txt) {
  txt <- paste(txt, collapse = "\n")
  starts <- gregexpr("@[[:alpha:]]+[[:space:]]*\\{", txt, perl = TRUE)[[1]]
  if (starts[1] == -1L) return(list())
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]
  out <- list()
  for (s in starts) {
    open <- s + attr(starts, "match.length")[which(starts == s)[1]] - 1L
    depth <- 0L; i <- open; n <- length(chars); close <- NA_integer_
    while (i <= n) {
      if (chars[i] == "{") depth <- depth + 1L
      else if (chars[i] == "}") {
        depth <- depth - 1L
        if (depth == 0L) { close <- i; break }
      }
      i <- i + 1L
    }
    if (is.na(close)) next
    out[[length(out) + 1L]] <- substr(txt, open + 1L, close - 1L)
  }
  out
}

#' @noRd
records_from_bib <- function(txt, source_file) {
  entries <- bib_entries(txt)
  if (!length(entries)) return(empty_records())
  rows <- lapply(entries, function(body) {
    # field = {value} or field = "value", one per comma at depth zero.
    fld <- regmatches(body, gregexpr(
      "[[:alpha:]_-]+[[:space:]]*=[[:space:]]*(\\{(?:[^{}]|\\{[^{}]*\\})*\\}|\"[^\"]*\"|[^,\\n]+)",
      body, perl = TRUE))[[1]]
    tags <- list()
    for (f in fld) {
      nm <- tolower(trimws(sub("[[:space:]]*=.*$", "", f)))
      v <- trimws(sub("^[^=]*=[[:space:]]*", "", f))
      v <- sub("^\\{(.*)\\}$", "\\1", v); v <- sub('^"(.*)"$', "\\1", v)
      # Capitalisation-protecting braces are not part of the value, and the
      # characters BibTeX makes you escape are not meant to keep their
      # backslash: "Memory {\\&} Cognition" is "Memory & Cognition".
      v <- gsub("\\\\([&%$#_{}])", "\\1", trimws(v), perl = TRUE)
      tags[[nm]] <- trimws(gsub("[{}]", "", v))
    }
    vals <- lapply(names(.gr_bib_map), function(f) tag_value(tags, .gr_bib_map[[f]]))
    names(vals) <- names(.gr_bib_map)
    # BibTeX joins authors with " and "; RIS gives one per line. Normalise to
    # the RIS shape so downstream sees one convention.
    if (!is.na(vals$authors)) {
      vals$authors <- paste(trimws(strsplit(vals$authors, "[[:space:]]+and[[:space:]]+")[[1]]),
                            collapse = "; ")
    }
    as.data.frame(c(vals, list(type = NA_character_, accession = NA_character_,
                               source_file = source_file)), stringsAsFactors = FALSE)
  })
  finish_records(do.call(rbind, rows))
}

#' Normalise whatever the parsers produced into the standard frame.
#' @noRd
finish_records <- function(df) {
  for (nm in .gr_record_cols) if (is.null(df[[nm]])) df[[nm]] <- NA_character_
  df <- df[, .gr_record_cols, drop = FALSE]
  for (nm in setdiff(.gr_record_cols, "record_id")) {
    v <- mark_utf8(as.character(df[[nm]]))
    v[!nzchar(trimws(v))] <- NA_character_
    df[[nm]] <- v
  }
  # A year is whatever four digits the field contains: exporters write "2019",
  # "2019/03/12", "2019 Mar" and "c2019" for the same year.
  df$year <- sub(".*?((?:1[6-9]|20)[0-9]{2}).*", "\\1", df$year, perl = TRUE)
  df$year[!grepl("^[0-9]{4}$", df$year)] <- NA_character_
  # DOIs arrive as bare, as a URL, and with a "doi:" prefix. One shape, so two
  # records for one paper can be recognised as one paper.
  df$doi <- tolower(trimws(df$doi))
  df$doi <- sub("^(https?://)?(dx\\.)?doi\\.org/", "", df$doi)
  df$doi <- sub("^doi:[[:space:]]*", "", df$doi)
  df$doi[!grepl("^10\\.[0-9]{4,9}/", df$doi)] <- NA_character_
  df$record_id <- seq_len(nrow(df))
  rownames(df) <- NULL
  df
}

#' A title reduced to what two exports of one paper have in common.
#'
#' Case, punctuation, accents and the trailing full stop all differ between
#' databases for the same article. What survives is the letters and digits.
#' @noRd
title_key <- function(x) {
  v <- tolower(as.character(x))
  v <- iconv(v, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  v <- gsub("[^a-z0-9]+", "", v)
  v[is.na(v) | !nzchar(v)] <- NA_character_
  v
}

#' Read a search export, and say what the search found
#'
#' Reads RIS or BibTeX exports from one or more databases, removes the records
#' that are the same work, and matches what is left to the documents you have on
#' disk. It is the step before [gr_screen()], and it is where a review's numbers
#' come from: how many records were identified, how many duplicates went, how
#' many reports were sought and how many were never obtained.
#'
#' Nothing here calls a model. Which paper a record is, and whether two records
#' are one paper, are questions a DOI answers exactly.
#'
#' @section Why start here rather than at a folder:
#' A folder of PDFs cannot say which databases were searched, with what query,
#' on what date, or how many records came back — PRISMA items 6, 7 and 16 — and
#' no care further down substitutes for them. It also cannot say what is
#' *missing*: a record with no PDF is "report not retrieved", which is a finding
#' about the review, and a folder represents it as nothing at all.
#'
#' It fixes something quieter too. [gr_synthesise()] cites by author and year,
#' and without an export those come from asking a model to read a title page —
#' the one part of a citation that must be exactly right, resting on the loosest
#' guarantee in the pipeline. From an export they are data.
#'
#' @param exports Paths to `.ris`, `.txt`, `.bib` or `.bibtex` files, or a
#'   directory containing them. Several exports from several databases is the
#'   normal case and is what the duplicate counts are for.
#' @param files A directory of documents, or a character vector of paths, to
#'   match records against. Optional: a record set is useful before anything has
#'   been downloaded.
#' @param search A [gr_search()] describing how the export was produced. Not
#'   required, and the reason to supply it is that a review has to report it.
#' @param dedupe `"doi"` matches on DOI alone; `"doi+title"` (the default) falls
#'   back to normalised title and year when a DOI is missing, which is what
#'   catches the same conference paper indexed twice; `"none"` keeps everything.
#' @return An object of class `gr_records`:
#'   \describe{
#'     \item{`records`}{One row per distinct work, with `duplicate_of` naming the
#'       row a dropped record repeats, `file` the document matched to it, and
#'       `retrieved` whether one was found.}
#'     \item{`counts`}{Identified, per database, duplicates removed, distinct
#'       records, reports sought, reports retrieved, reports not retrieved.}
#'     \item{`search`}{The `gr_search()`, or `NULL`.}
#'     \item{`unmatched_files`}{Documents on disk that no record claims — usually
#'       a sign the export and the folder are out of step.}
#'   }
#' @seealso [gr_search()], [gr_screen()], [gr_flow()], [gr_inventory()]
#' @family corpus functions
#' @export
#' @examples
#' ris <- tempfile(fileext = ".ris")
#' writeLines(c("TY  - JOUR", "AU  - Smith, J.", "TI  - A trial of spacing",
#'              "PY  - 2019", "DO  - 10.1000/abc", "ER  - "), ris)
#' recs <- gr_records(ris)
#' recs
#' recs$records[, c("authors", "year", "title", "doi", "retrieved")]
gr_records <- function(exports, files = NULL, search = NULL,
                       dedupe = c("doi+title", "doi", "none")) {
  dedupe <- match.arg(dedupe)
  if (!is.null(search) && !inherits(search, "gr_search")) {
    gr_abort("`search` must come from gr_search().", class = "gr_bad_search")
  }
  paths <- export_paths(exports)
  if (!length(paths)) {
    gr_abort(paste0("No export files found. Pass .ris, .bib or .txt files from a reference ",
                    "manager, or a directory containing them."), class = "gr_no_exports")
  }
  parsed <- lapply(paths, read_export)
  recs <- do.call(rbind, parsed)
  recs$record_id <- seq_len(nrow(recs))
  rownames(recs) <- NULL
  if (!nrow(recs)) gr_abort("Those exports contain no records.", class = "gr_no_exports")

  identified <- nrow(recs)
  by_db <- table(ifelse(is.na(recs$database), basename(recs$source_file), recs$database))

  recs$duplicate_of <- dedupe_records(recs, dedupe)
  distinct <- is.na(recs$duplicate_of)

  matched <- match_files(recs, files)
  recs$file <- matched$file
  recs$retrieved <- !is.na(recs$file)
  # A duplicate is not separately "not retrieved": it is the same work as a row
  # that either was or was not. Counting it again would inflate both numbers.
  recs$retrieved[!distinct] <- NA

  counts <- data.frame(
    stage = c("records identified", "duplicates removed", "records screened",
              "reports sought", "reports retrieved", "reports not retrieved"),
    n = c(identified, sum(!distinct), sum(distinct), sum(distinct),
          sum(distinct & recs$retrieved %in% TRUE),
          sum(distinct & recs$retrieved %in% FALSE)),
    stringsAsFactors = FALSE)

  structure(list(records = recs, counts = counts, by_database = by_db,
                 search = search, exports = paths,
                 unmatched_files = matched$unmatched, dedupe = dedupe),
            class = "gr_records")
}

#' @noRd
export_paths <- function(exports) {
  ext <- c("ris", "bib", "bibtex", "txt", "nbib")
  if (is.character(exports) && length(exports) == 1L && !is.na(exports) && dir.exists(exports)) {
    f <- list.files(exports, full.names = TRUE, no.. = TRUE)
    f <- f[!dir.exists(f)]
    return(sort(f[tolower(tools::file_ext(f)) %in% ext]))
  }
  p <- as.character(unlist(exports, use.names = FALSE))
  p <- p[!is.na(p)]
  missing <- p[!file.exists(p)]
  if (length(missing)) {
    gr_abort(sprintf("Export file(s) not found: %s.",
                     paste(sprintf("'%s'", missing), collapse = ", ")),
             class = "gr_no_exports")
  }
  p
}

#' Read one export, choosing the parser by content rather than by extension.
#'
#' Exporters are careless with extensions -- Web of Science writes RIS into a
#' `.txt`, and a `.txt` from Scholar is BibTeX. Looking at the first line is
#' more reliable than trusting the name.
#' @noRd
read_export <- function(path) {
  lines <- tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"),
                    error = function(e) character(0))
  if (!length(lines)) return(empty_records())
  head_txt <- paste(utils::head(lines, 60), collapse = "\n")
  if (grepl("^[[:space:]]*@[[:alpha:]]+[[:space:]]*\\{", head_txt) ||
      grepl("\n[[:space:]]*@[[:alpha:]]+[[:space:]]*\\{", head_txt)) {
    return(records_from_bib(lines, basename(path)))
  }
  if (grepl("(^|\n)TY[[:space:]]{0,2}-", head_txt)) {
    return(records_from_ris(lines, basename(path)))
  }
  gr_warn(sprintf(paste0("'%s' does not look like RIS or BibTeX -- no 'TY  -' and no '@article{'. ",
                         "It contributed no records."), basename(path)),
          class = "gr_unknown_export")
  empty_records()
}

#' Which rows repeat an earlier one.
#'
#' Returns the `record_id` of the first row a duplicate repeats, or NA. A DOI is
#' exact and is tried first; the title fallback exists because conference papers
#' and preprints are routinely indexed without one.
#' @noRd
dedupe_records <- function(recs, how) {
  out <- rep(NA_integer_, nrow(recs))
  if (identical(how, "none") || nrow(recs) < 2L) return(out)
  first_of <- function(key) {
    idx <- rep(NA_integer_, length(key))
    seen <- new.env(parent = emptyenv())
    for (i in seq_along(key)) {
      if (is.na(key[i])) next
      prev <- seen[[key[i]]]
      if (is.null(prev)) seen[[key[i]]] <- i else idx[i] <- prev
    }
    idx
  }
  hit <- first_of(recs$doi)
  if (identical(how, "doi+title")) {
    # Only for rows a DOI could not settle. A title match between two rows that
    # both have DOIs, and different ones, is two papers with similar titles --
    # which happens, and merging them would lose a study.
    tk <- title_key(recs$title)
    tk[!is.na(recs$doi)] <- NA_character_
    key <- ifelse(is.na(tk) | is.na(recs$year), NA_character_, paste0(tk, "|", recs$year))
    fallback <- first_of(key)
    hit[is.na(hit)] <- fallback[is.na(hit)]
  }
  out[!is.na(hit)] <- recs$record_id[hit[!is.na(hit)]]
  out
}

#' Match records to documents on disk.
#'
#' Three routes, in order of how much they prove: the path the export itself
#' recorded, a filename containing the DOI's suffix, and a filename whose letters
#' match the title's. Nothing fuzzier -- a wrong match attributes one paper's
#' findings to another, which is worse than reporting the report as not
#' retrieved.
#' @noRd
match_files <- function(recs, files) {
  n <- nrow(recs)
  if (is.null(files)) return(list(file = rep(NA_character_, n), unmatched = character(0)))
  paths <- if (is.character(files) && length(files) == 1L && !is.na(files) && dir.exists(files)) {
    corpus_sources(files, recursive = TRUE, quiet = TRUE)
  } else as.character(unlist(files, use.names = FALSE))
  paths <- unname(paths[!is.na(paths)])
  if (!length(paths)) return(list(file = rep(NA_character_, n), unmatched = character(0)))

  base <- basename(paths)
  stem <- title_key(tools::file_path_sans_ext(base))
  out <- rep(NA_character_, n)
  taken <- rep(FALSE, length(paths))

  # Route 4 below matches on first-author surname plus year, which is ambiguous
  # the moment two records share both. Checking only that ONE FILE matched the
  # record let the first of two Smith-2019 papers claim smith2019.pdf and the
  # second find nothing -- a coin flip presented as a match, and the way one
  # paper's findings end up attributed to another. Ambiguity is a property of
  # the record set, so it is settled here, before anything claims anything.
  ay <- vapply(seq_len(n), function(i) {
    sur <- bib_surnames(recs$authors[i])
    if (!length(sur) || is.na(recs$year[i])) return(NA_character_)
    k <- title_key(sur[1])
    if (is.na(k) || nchar(k) < 3L) NA_character_ else paste0(k, "|", recs$year[i])
  }, character(1))
  # Duplicates of one another are one work, so they do not make a key ambiguous.
  live <- is.na(recs$duplicate_of %||% rep(NA_integer_, n))
  # Subset first, THEN test -- `duplicated(ay[live])` is shorter than `ay`, and
  # combining the two with `&` recycles it silently against the wrong rows.
  live_keys <- ay
  live_keys[!live] <- NA_character_
  ambiguous <- unique(live_keys[!is.na(live_keys) & duplicated(live_keys)])
  ay[ay %in% ambiguous] <- NA_character_

  claim <- function(i, j) { out[i] <<- paths[j]; taken[j] <<- TRUE }

  for (i in seq_len(n)) {
    # 1. The export said where the file is.
    f <- recs$file[i]
    if (!is.na(f)) {
      # Mendeley writes ":path:pdf"; EndNote writes "internal-pdf://...".
      cand <- sub("^:+", "", sub(":[[:alpha:]]+$", "", f))
      j <- which(!taken & (paths == cand | base == basename(cand)))
      if (length(j)) { claim(i, j[1]); next }
    }
    # 2. The DOI suffix appears in the filename -- how most managers name files.
    if (!is.na(recs$doi[i])) {
      suffix <- title_key(sub("^10\\.[0-9]{4,9}/", "", recs$doi[i]))
      if (!is.na(suffix) && nchar(suffix) >= 6L) {
        j <- which(!taken & grepl(suffix, stem, fixed = TRUE))
        if (length(j) == 1L) { claim(i, j); next }
      }
    }
    # 3. The title, reduced to letters, is a prefix of the filename or contains
    # it. Requires a long enough overlap that a coincidence is implausible.
    tk <- title_key(recs$title[i])
    if (!is.na(tk) && nchar(tk) >= 12L) {
      j <- which(!taken & !is.na(stem) &
                   (startsWith(stem, substr(tk, 1, 24)) | startsWith(tk, substr(stem, 1, 24))))
      if (length(j) == 1L) { claim(i, j); next }
    }
    # 4. First author's surname and the year both appear in the filename. This
    # is how people actually name downloaded PDFs -- "smith2019.pdf",
    # "Smith 2019 - Cognitive Load.pdf" -- and without it the routes above match
    # almost nothing in a real folder. Both parts are required and the match
    # must be unique, so two Smith papers from 2019 match neither rather than
    # attributing one paper's findings to the other.
    if (!is.na(ay[i])) {
      k <- sub("\\|.*$", "", ay[i])
      j <- which(!taken & !is.na(stem) &
                   grepl(k, stem, fixed = TRUE) &
                   grepl(recs$year[i], base, fixed = TRUE))
      if (length(j) == 1L) { claim(i, j); next }
    }
  }
  list(file = out, unmatched = paths[!taken])
}

#' @export
print.gr_records <- function(x, ...) {
  cat(sprintf("<gr_records> %d record(s) from %d export(s)\n",
              nrow(x$records), length(x$exports)))
  if (length(x$by_database)) {
    cat(sprintf("  %s\n", paste(sprintf("%s %d", names(x$by_database),
                                        as.integer(x$by_database)), collapse = ", ")))
  }
  for (i in seq_len(nrow(x$counts))) {
    cat(sprintf("  %-24s %d\n", x$counts$stage[i], x$counts$n[i]))
  }
  gone <- x$counts$n[x$counts$stage == "reports not retrieved"]
  if (length(gone) && gone > 0L) {
    cat(sprintf("  ! %d record(s) have no document. They are part of the review and are\n", gone))
    cat("    reported as sought-but-not-retrieved, not quietly dropped.\n")
  }
  if (length(x$unmatched_files)) {
    cat(sprintf("  ! %d file(s) on disk match no record: %s\n", length(x$unmatched_files),
                paste(utils::head(basename(x$unmatched_files), 3), collapse = ", ")))
  }
  if (is.null(x$search)) {
    cat("  no gr_search() attached: the review cannot report what was searched\n")
  }
  invisible(x)
}

#' Record how the search was run
#'
#' The part of a review that no tool can infer and every review must state:
#' which databases were searched, with what query, on what date, and what limits
#' were applied. PRISMA items 6 and 7 ask for it, and item 7 asks for the full
#' strategy for at least one database *so that it could be repeated*.
#'
#' This function computes nothing. It exists so the answer travels with the run
#' instead of living in a lab notebook — it reaches [gr_flow()] and the audit
#' report, and it round-trips through [gr_protocol_save()] alongside the
#' criteria, so what was searched and what was eligible are one artifact.
#'
#' @param databases Named character vector or list: name of each source, value
#'   the query string run against it. The query is the point; a review reporting
#'   "we searched PubMed" without it cannot be repeated.
#' @param dates When each search was run, as `YYYY-MM-DD`. One value, or one per
#'   database. Searches drift: a review is a statement about a date.
#' @param limits Limits applied — language, publication years, study design
#'   filters. Free text; a review states them whatever they were.
#' @param registration Registration identifier (a PROSPERO number, say), or
#'   `NA` and say so. PRISMA item 24 asks either way.
#' @param other Any further sources: reference lists checked, experts contacted,
#'   registers, preprint servers, hand-searched journals.
#' @param notes Anything else the methods section has to say.
#' @return An object of class `gr_search`.
#' @seealso [gr_records()], [gr_protocol()], [gr_flow()]
#' @family corpus functions
#' @export
#' @examples
#' gr_search(
#'   databases = c(PubMed = "(spaced practice[tiab]) AND (retention[tiab])",
#'                 Scopus = "TITLE-ABS-KEY(\"spaced practice\" AND retention)"),
#'   dates = "2026-02-14",
#'   limits = "English; 2000 onwards; primary studies only",
#'   registration = "PROSPERO CRD42026000000",
#'   other = "Reference lists of included studies hand-searched"
#' )
gr_search <- function(databases, dates = NULL, limits = NULL, registration = NA_character_,
                      other = NULL, notes = NULL) {
  db <- unlist(databases, use.names = TRUE)
  if (!length(db) || is.null(names(db)) || any(!nzchar(names(db)))) {
    gr_abort(paste0("`databases` must be named: the name is the source and the value is the ",
                    "query run against it. A review that reports which databases it searched ",
                    "but not what it asked them cannot be repeated, which is what PRISMA ",
                    "item 7 is for."), class = "gr_bad_search")
  }
  db <- vapply(db, as_chr1, character(1))
  if (any(!nzchar(trimws(db)))) {
    gr_abort(sprintf("No query given for %s.",
                     paste(sprintf("'%s'", names(db)[!nzchar(trimws(db))]), collapse = ", ")),
             class = "gr_bad_search")
  }
  dates <- if (is.null(dates)) NA_character_ else as.character(dates)
  if (length(dates) > 1L && length(dates) != length(db)) {
    gr_abort("`dates` must be one value, or one per database.", class = "gr_bad_search")
  }
  bad <- dates[!is.na(dates) & !grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", dates)]
  if (length(bad)) {
    gr_warn(sprintf("Search date(s) %s are not YYYY-MM-DD. They are recorded as given.",
                    paste(sprintf("'%s'", bad), collapse = ", ")), class = "gr_bad_search_date")
  }
  structure(list(
    databases = db,
    dates = rep(dates, length.out = length(db)),
    limits = if (is.null(limits)) NA_character_ else as.character(limits),
    registration = as_chr1(registration, NA_character_),
    other = if (is.null(other)) character(0) else as.character(other),
    notes = if (is.null(notes)) NA_character_ else as_chr1(notes)
  ), class = "gr_search")
}

#' @export
print.gr_search <- function(x, ...) {
  cat(sprintf("<gr_search> %d source(s)\n", length(x$databases)))
  for (i in seq_along(x$databases)) {
    cat(sprintf("  %s%s\n    %s\n", names(x$databases)[i],
                if (is.na(x$dates[i])) "" else sprintf("  (searched %s)", x$dates[i]),
                substr(x$databases[i], 1, 100)))
  }
  if (!is.na(x$limits[1])) cat(sprintf("  limits : %s\n", paste(x$limits, collapse = "; ")))
  for (o in x$other) cat(sprintf("  also   : %s\n", o))
  cat(sprintf("  registration: %s\n",
              if (is.na(x$registration)) "NOT REGISTERED -- say so in the report"
              else x$registration))
  invisible(x)
}
