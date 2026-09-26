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

#' `@string`, `@preamble` and `@comment` are not records. They match the same
#' `@word{` opener, and each one became a row with all seventeen fields NA --
#' which dedupe_records() cannot collapse, because an NA key matches nothing,
#' and nothing else filters. JabRef, Mendeley and publisher exports all emit
#' them, so "records identified" -- the first number of a PRISMA flow diagram --
#' came out too high by however many the file happened to carry.
#' @noRd
.gr_bib_skip <- c("string", "preamble", "comment")

#' Split a BibTeX file into entries.
#'
#' Brace-counting rather than a regex: a title containing braces -- which is how
#' BibTeX protects capitalisation, so `{DNA}` is common -- breaks any regex that
#' assumes the first `}` ends the field.
#' @noRd
bib_entries <- function(txt) {
  txt <- paste(txt, collapse = "\n")
  m <- gregexpr("@[[:alpha:]]+[[:space:]]*\\{", txt, perl = TRUE)
  starts <- m[[1]]
  if (starts[1] == -1L) return(list())
  lens <- attr(starts, "match.length")
  types <- tolower(sub("[[:space:]]*\\{$", "", sub("^@", "", regmatches(txt, m)[[1]])))
  chars <- strsplit(txt, "", fixed = TRUE)[[1]]
  nchars <- length(chars)
  out <- list()
  for (idx in seq_along(starts)) {
    if (types[idx] %in% .gr_bib_skip) next
    open <- starts[idx] + lens[idx] - 1L
    depth <- 0L; i <- open; close <- NA_integer_
    while (i <= nchars) {
      if (chars[i] == "{") depth <- depth + 1L
      else if (chars[i] == "}") {
        depth <- depth - 1L
        if (depth == 0L) { close <- i; break }
      }
      i <- i + 1L
    }
    if (is.na(close)) {
      # Silently dropping it removed a study from the review -- from the counts,
      # from screening and from the flow diagram -- with nothing to notice.
      gr_warn(sprintf(paste0("A BibTeX @%s entry beginning at character %d has no closing ",
                             "brace and was skipped; check the file for an unbalanced '{'."),
                      types[idx], starts[idx]), class = "gr_bib_unterminated")
      next
    }
    out[[length(out) + 1L]] <- substr(txt, open + 1L, close - 1L)
  }
  out
}

#' Split one entry's body into its `name = value` fields.
#'
#' The same depth counting bib_entries() uses, not a regex. The regex this
#' replaced allowed one level of braces inside a value, and real exports nest
#' deeper: `M{\"{u}}ller` (how Web of Science and BibDesk write an umlaut) and
#' `{{DNA}}` are two. The value then fell back to "up to the first comma", the
#' rest of it was rescanned for fields, and the last one seen won -- so an
#' author list lost everyone after the first accented name, a title stopped at
#' its comma, and "year = 1999" in an abstract replaced the real year.
#'
#' A separator counts only at brace depth zero and outside a quoted value; a
#' quote inside braces (`{\"u}`) does not end one. Values joined with `#` are
#' concatenated. The first occurrence of a field wins, as it does in BibTeX. A
#' value ends where its braces or quotes close, so a hand-edited file that
#' left out the comma before the next field still reads that field.
#' @noRd
bib_fields <- function(body) {
  ch <- strsplit(body, "", fixed = TRUE)[[1]]
  n <- length(ch)
  if (!n) return(list())
  step <- (ch == "{") - (ch == "}")
  depth <- cumsum(step) - step                  # depth before each character
  q <- ch == "\"" & depth == 0L                 # quotes that open or close a value
  inq <- (cumsum(q) - q) %% 2L == 1L
  top <- depth == 0L & !inq & !q
  ws <- grepl("^[[:space:]]$", ch)
  # The first of the sorted positions `x` that comes after `v`, or NA.
  after <- function(x, v) {
    k <- findInterval(v, x) + 1L
    if (k > length(x)) NA_integer_ else x[k]
  }
  eq_or_comma <- which(top & (ch == "=" | ch == ","))
  closes <- which(ch == "}" & depth == 1L)       # a brace that returns to depth zero
  quotes <- which(q)
  stops <- which(ws | (top & (ch == "," | ch == "#")))
  solid <- which(!ws)
  text <- function(a, b) if (a > b) "" else paste(ch[a:b], collapse = "")
  tags <- list()
  i <- 1L
  while (i <= n) {
    s <- after(eq_or_comma, i - 1L)
    if (is.na(s)) break
    # A comma before any "=": the citation key, or stray text.
    if (ch[s] == ",") { i <- s + 1L; next }
    nm <- tolower(sub("^.*[[:space:]]", "", trimws(text(i, s - 1L))))
    parts <- character(0)
    v <- after(solid, s)
    e <- n
    while (!is.na(v)) {
      if ((ch[v] == "{" && top[v]) || q[v]) {
        # An unclosed quote runs to the end of the entry rather than losing it.
        e <- after(if (q[v]) quotes else closes, v)
        parts <- c(parts, if (is.na(e)) text(v + 1L, n) else text(v + 1L, e - 1L))
        if (is.na(e)) e <- n
      } else if (top[v] && ch[v] %in% c(",", "#")) {
        e <- v - 1L                             # "name = ," is an empty value
      } else {
        e <- after(stops, v); e <- if (is.na(e)) n else e - 1L
        parts <- c(parts, text(v, e))
      }
      nx <- after(solid, e)
      v <- if (!is.na(nx) && ch[nx] == "#" && top[nx]) after(solid, nx) else NA_integer_
    }
    if (grepl("^[[:alnum:]_:.+-]+$", nm) && is.null(tags[[nm]])) {
      tags[[nm]] <- paste(parts, collapse = "")
    }
    nx <- after(solid, e)
    i <- if (is.na(nx)) n + 1L else if (ch[nx] == "," && top[nx]) nx + 1L else nx
  }
  tags
}

#' @noRd
records_from_bib <- function(txt, source_file) {
  entries <- bib_entries(txt)
  if (!length(entries)) return(empty_records())
  rows <- lapply(entries, function(body) {
    tags <- lapply(bib_fields(body), function(v) {
      # Capitalisation-protecting braces are not part of the value, and the
      # characters BibTeX makes you escape are not meant to keep their
      # backslash: "Memory {\\&} Cognition" is "Memory & Cognition".
      v <- gsub("\\\\([&%$#_{}])", "\\1", trimws(v), perl = TRUE)
      trimws(gsub("[{}]", "", v))
    })
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

#' Latin letters that carry a diacritic, and the plain letter each folds to.
#'
#' A table rather than `iconv(to = "ASCII//TRANSLIT")`, which folded an
#' e-acute to "e" but DELETED every letter it had no ASCII spelling for: Greek,
#' Cyrillic and CJK titles came out as their digits and Latin fragments, so
#' "PPAR-alpha" and "PPAR-gamma" (the Greek letters, not the words) were one
#' title, and two different Chinese titles about type 2 diabetes were both "2".
#' What it keeps also depends on the platform's iconv and the locale -- glibc
#' in the C locale writes "?" for an e-acute -- so a table is the only way two
#' machines agree on which records are one work.
#' @noRd
.gr_latin_fold <- local({
  one <- c(
    a = "\u00e0\u00e1\u00e2\u00e3\u00e4\u00e5\u0101\u0103\u0105",
    A = "\u00c0\u00c1\u00c2\u00c3\u00c4\u00c5\u0100\u0102\u0104",
    c = "\u00e7\u0107\u0109\u010b\u010d", C = "\u00c7\u0106\u0108\u010a\u010c",
    d = "\u010f\u0111\u00f0", D = "\u010e\u0110\u00d0",
    e = "\u00e8\u00e9\u00ea\u00eb\u0113\u0115\u0117\u0119\u011b",
    E = "\u00c8\u00c9\u00ca\u00cb\u0112\u0114\u0116\u0118\u011a",
    g = "\u011d\u011f\u0121\u0123", G = "\u011c\u011e\u0120\u0122",
    h = "\u0125\u0127", H = "\u0124\u0126",
    i = "\u00ec\u00ed\u00ee\u00ef\u0129\u012b\u012d\u012f\u0131",
    I = "\u00cc\u00cd\u00ce\u00cf\u0128\u012a\u012c\u012e\u0130",
    j = "\u0135", J = "\u0134", k = "\u0137", K = "\u0136",
    l = "\u013a\u013c\u013e\u0140\u0142", L = "\u0139\u013b\u013d\u013f\u0141",
    n = "\u00f1\u0144\u0146\u0148", N = "\u00d1\u0143\u0145\u0147",
    o = "\u00f2\u00f3\u00f4\u00f5\u00f6\u00f8\u014d\u014f\u0151",
    O = "\u00d2\u00d3\u00d4\u00d5\u00d6\u00d8\u014c\u014e\u0150",
    r = "\u0155\u0157\u0159", R = "\u0154\u0156\u0158",
    s = "\u015b\u015d\u015f\u0161\u0219", S = "\u015a\u015c\u015e\u0160\u0218",
    t = "\u0163\u0165\u0167\u021b", T = "\u0162\u0164\u0166\u021a",
    u = "\u00f9\u00fa\u00fb\u00fc\u0169\u016b\u016d\u016f\u0171\u0173",
    U = "\u00d9\u00da\u00db\u00dc\u0168\u016a\u016c\u016e\u0170\u0172",
    w = "\u0175", W = "\u0174",
    y = "\u00fd\u00ff\u0177", Y = "\u00dd\u0178\u0176",
    z = "\u017a\u017c\u017e", Z = "\u0179\u017b\u017d")
  list(from = paste(one, collapse = ""),
       to = paste(rep(names(one), nchar(one)), collapse = ""),
       # Two vectors, not a named one: a name is a symbol, and a symbol is
       # translated to the native encoding, which in the C locale turned "\u00df"
       # into "<U+00DF>" and folded nothing.
       multi_from = c("\u00df", "\u00e6", "\u00c6", "\u0153", "\u0152",
                      "\u00fe", "\u00de", "\u0133", "\u0132"),
       multi_to = c("ss", "ae", "AE", "oe", "OE", "th", "TH", "ij", "IJ"))
})

#' Fold Latin diacritics to plain letters, leaving every other letter alone.
#'
#' Case is kept (route 4 of match_files() needs it). Combining marks are
#' dropped, so a decomposed "e" + U+0301 -- how macOS spells a filename --
#' folds the same way as the precomposed U+00E9.
#' @noRd
fold_latin <- function(x) {
  x <- mark_utf8(as.character(x))
  x <- chartr(.gr_latin_fold$from, .gr_latin_fold$to, x)
  for (k in seq_along(.gr_latin_fold$multi_from)) {
    x <- gsub(.gr_latin_fold$multi_from[k], .gr_latin_fold$multi_to[k], x, fixed = TRUE)
  }
  gsub("\\p{M}+", "", x, perl = TRUE)
}

#' A title reduced to what two exports of one paper have in common.
#'
#' Case, punctuation, accents and the trailing full stop all differ between
#' databases for the same article. What survives is the letters and digits --
#' all of them, in any script: a letter with no ASCII spelling is still part of
#' the title, and deleting it made different studies one key.
#' @noRd
title_key <- function(x) {
  v <- fold_latin(tolower(mark_utf8(as.character(x))))
  v <- gsub("[^\\p{L}\\p{N}]+", "", v, perl = TRUE)
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
#' on what date, or how many records came back (PRISMA items 6, 7 and 16), and
#' no care further down substitutes for them. It also cannot say what is
#' *missing*: a record with no PDF is "report not retrieved", which is a finding
#' about the review, and a folder represents it as nothing at all.
#'
#' It fixes something quieter too. [gr_synthesise()] cites by author and year,
#' and without an export those come from asking a model to read a title page.
#' That is the one part of a citation that must be exactly right, resting on the
#' loosest guarantee in the pipeline. From an export they are data.
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
#'   back to normalised title and year when a DOI cannot settle it, which is
#'   what catches the same conference paper indexed twice, or indexed once with
#'   its DOI and once without. The title fallback also needs the first author to
#'   agree (or, when a record has no authors, the venue), ignores titles shorter
#'   than 12 letters ("Reply", "Editorial"), and never merges records carrying
#'   two different DOIs; `"none"` keeps everything.
#' @return An object of class `gr_records`:
#'   \describe{
#'     \item{`records`}{One row per distinct work, with `duplicate_of` naming the
#'       row a dropped record repeats, `file` the document matched to it, and
#'       `retrieved` whether one was found. A duplicate's file path counts
#'       towards the row it repeats, which is where the match is reported.}
#'     \item{`counts`}{Identified, per database, duplicates removed, distinct
#'       records, reports sought, reports retrieved, reports not retrieved.}
#'     \item{`search`}{The `gr_search()`, or `NULL`.}
#'     \item{`unmatched_files`}{Documents on disk that no record claims, usually
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
    f <- f[tolower(tools::file_ext(f)) %in% ext]
    # Byte order, not sort(): sort() follows LC_COLLATE, so "adams" came before
    # "Baker" on a laptop and after it under cron or R CMD check. The export
    # order decides record_id and which copy of a duplicated work is the one
    # kept, and a record set should not depend on the machine that read it.
    return(f[order(enc2utf8(f), method = "radix")])
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
  gr_warn(sprintf(paste0("'%s' does not look like RIS or BibTeX: no 'TY  -' and no '@article{'. ",
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
  n <- nrow(recs)
  out <- rep(NA_integer_, n)
  if (identical(how, "none") || n < 2L) return(out)
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
  doi_hit <- first_of(recs$doi)
  by_title <- identical(how, "doi+title")
  key <- rep(NA_character_, n)
  if (by_title) {
    # A title key is built for EVERY row, DOI or not. Blanking it on rows with a
    # DOI -- meant to keep two different DOIs apart -- also meant a DOI-less
    # record (Scholar, a conference index, grey literature) could never match
    # the same paper exported from Scopus with its DOI, which is the commonest
    # duplicate there is. What keeps two DOIs apart is the check below.
    tk <- title_key(recs$title)
    # Short titles are not identities: "Reply", "Editorial", "Erratum" recur in
    # every journal every year. The same floor match_files() uses for a title.
    tk[!is.na(tk) & nchar(tk) < 12L] <- NA_character_
    key <- ifelse(is.na(tk) | is.na(recs$year), NA_character_, paste0(tk, "|", recs$year))
    # A title and year that rows with two different DOIs share is two papers
    # with one title -- which happens, and merging them would lose a study. A
    # DOI-less row with that title could be either, so it is merged with
    # neither rather than with whichever came first.
    ok <- !is.na(key) & !is.na(recs$doi)
    if (any(ok)) {
      n_doi <- tapply(recs$doi[ok], key[ok], function(d) length(unique(d)))
      key[key %in% names(n_doi)[n_doi > 1L]] <- NA_character_
    }
    # A title and year is not enough on its own either: two letters titled
    # "Response to the commentary on ..." in one year are not one letter. The
    # first author has to agree as well, loosely -- "Smith, J." and "Smith JA"
    # are one person -- or, when a record has no authors, the venue.
    first_au <- title_key(sub("[[:space:]]*;.*$", "", recs$authors))
    surname <- vapply(recs$authors, function(a) {
      s <- bib_surnames(a)
      if (length(s)) title_key(s[1]) else NA_character_
    }, character(1), USE.NAMES = FALSE)
    venue <- title_key(recs$venue)
    either_in <- function(a, b) grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE)
    agree <- function(i, j) {
      if (!is.na(surname[i]) && !is.na(surname[j])) {
        return(grepl(surname[i], first_au[j], fixed = TRUE) ||
                 grepl(surname[j], first_au[i], fixed = TRUE))
      }
      if (!is.na(venue[i]) && !is.na(venue[j])) return(either_in(venue[i], venue[j]))
      TRUE
    }
  }
  # `root` is the kept row each row belongs to. Every row is resolved to a KEPT
  # row as it is reached, so `duplicate_of` never points at another duplicate.
  root <- seq_len(n)
  holders <- new.env(parent = emptyenv())
  for (i in seq_len(n)) {
    if (!is.na(doi_hit[i])) {
      root[i] <- root[doi_hit[i]]
    } else if (by_title && !is.na(key[i])) {
      h <- holders[[key[i]]]
      if (length(h)) {
        g <- unique(root[h[vapply(h, function(j) agree(i, j), logical(1))]])
        # Two different works it could equally be is no match at all.
        if (length(g) == 1L) root[i] <- g
      }
    }
    if (by_title && !is.na(key[i])) holders[[key[i]]] <- c(holders[[key[i]]], i)
  }
  dup <- root != seq_len(n)
  out[dup] <- recs$record_id[root[dup]]
  out
}

#' Match records to documents on disk.
#'
#' Four routes, in order of how much they prove: the path the export itself
#' recorded, a filename containing the DOI's suffix, a filename whose letters
#' match the title's, and a filename naming the first author and the year.
#' Nothing fuzzier -- a wrong match attributes one paper's findings to another,
#' which is worse than reporting the report as not retrieved.
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
  # Case kept, for route 4: "SmithEtAl2019" is three words and "Parkinson" one.
  fstem <- fold_latin(tools::file_path_sans_ext(base))
  out <- rep(NA_character_, n)
  taken <- rep(FALSE, length(paths))

  # A duplicate is matched AS the row it repeats. Letting it claim a file on its
  # own gave the document to a row every later stage ignores: when only the
  # second copy of a paper carried the PDF path (an EndNote export beside a
  # Scopus one, or one library holding the item twice), the kept record was
  # "not retrieved", the file was taken so it was not unmatched either, and the
  # full text never reached screening or the flow counts. So each kept row
  # matches with everything its duplicates know, and duplicates claim nothing.
  dup <- recs$duplicate_of %||% rep(NA_integer_, n)
  owner <- ifelse(is.na(dup), seq_len(n), match(dup, recs$record_id))
  members <- split(which(!is.na(owner)), owner[!is.na(owner)])

  # Route 4 below matches on first-author surname plus year, which is ambiguous
  # the moment two records share both. Checking only that ONE FILE matched the
  # record let the first of two Smith-2019 papers claim smith2019.pdf and the
  # second find nothing -- a coin flip presented as a match, and the way one
  # paper's findings end up attributed to another. Ambiguity is a property of
  # the record set, so it is settled here, before anything claims anything.
  #
  # Every route needs that settlement. A key that two works share identifies
  # neither of them, and without this the FIRST record listed took the file
  # while the second went unretrieved -- a coin flip decided by export order.
  # Two folders each holding a `report.txt`, which is what a `year/report.pdf`
  # archive looks like, put the 2019 file on the 2020 record and the 2020 file
  # on the 2019 record, both marked retrieved. That is one paper's findings
  # published under another paper's authors, year and DOI. A key the rows of
  # ONE work share (a record and its duplicate) is not ambiguous.
  settle <- function(k) {
    k[is.na(owner)] <- NA_character_
    ok <- !is.na(k)
    if (any(ok)) {
      works <- tapply(owner[ok], k[ok], function(g) length(unique(g)))
      k[k %in% names(works)[works > 1L]] <- NA_character_
    }
    k
  }
  sur <- vapply(seq_len(n), function(i) as_chr1(bib_surnames(recs$authors[i])[1], NA_character_),
                character(1))
  ay <- vapply(seq_len(n), function(i) {
    if (is.na(sur[i]) || is.na(recs$year[i])) return(NA_character_)
    k <- title_key(sur[i])
    if (is.na(k) || nchar(k) < 3L) NA_character_ else paste0(k, "|", recs$year[i])
  }, character(1))
  ay <- settle(ay)
  # The surname as a whole word of the filename, not as any substring of it.
  # `grepl("chen", "cheng2020")` gave Chen's record Cheng's paper, "park" found
  # "Parkinson disease and exercise", and "lee" found "Sleep quality". A word
  # ends where letters stop, or where a lower-case letter meets a capital, so
  # "SmithEtAl2019" and "SmithJ_2019" still name Smith; an all-lower
  # "smithj2019" does not, and is left unmatched rather than guessed at.
  sur_re <- vapply(seq_len(n), function(i) {
    if (is.na(ay[i])) return(NA_character_)
    w <- tolower(regmatches(fold_latin(sur[i]),
                            gregexpr("[\\p{L}\\p{N}]+", fold_latin(sur[i]), perl = TRUE))[[1]])
    if (!length(w)) return(NA_character_)
    edge <- "(?:(?<!\\p{L})|(?<=\\p{Ll})(?=\\p{Lu}))"
    after <- "(?:(?!\\p{L})|(?<=\\p{Ll})(?=\\p{Lu}))"
    paste0(edge, "(?i:", paste(w, collapse = "[^\\p{L}\\p{N}]*"), ")", after)
  }, character(1))

  cand_all <- ifelse(is.na(recs$file), NA_character_,
                     sub("^:+", "", sub(":[[:alpha:]]+$", "", recs$file)))
  base_key <- settle(ifelse(is.na(cand_all), NA_character_, basename(cand_all)))
  doi_key <- settle(vapply(seq_len(n), function(i) {
    if (is.na(recs$doi[i])) return(NA_character_)
    sfx <- title_key(sub("^10\\.[0-9]{4,9}/", "", recs$doi[i]))
    if (is.na(sfx) || nchar(sfx) < 6L) NA_character_ else sfx
  }, character(1)))
  tk_all <- title_key(recs$title)
  ttl_key <- settle(ifelse(is.na(tk_all) | nchar(tk_all) < 12L, NA_character_,
                           substr(tk_all, 1, 24)))

  # Each route returns the untaken files one work's rows point to.
  any_hit <- function(rows, test) {
    hit <- rep(FALSE, length(paths))
    for (i in rows) hit <- hit | test(i)
    which(!taken & hit)
  }
  routes <- list(
    # 1. The export said where the file is. Mendeley writes ":path:pdf";
    # EndNote writes "internal-pdf://...". The full path first, and the
    # basename only when no full path matches.
    function(rows) {
      j <- which(!taken & paths %in% cand_all[rows][!is.na(cand_all[rows])])
      if (!length(j)) j <- which(!taken & base %in% base_key[rows][!is.na(base_key[rows])])
      j
    },
    # 2. The DOI suffix appears in the filename -- how most managers name files.
    function(rows) any_hit(rows[!is.na(doi_key[rows])],
                           function(i) grepl(doi_key[i], stem, fixed = TRUE)),
    # 3. The title, reduced to letters, is a prefix of the filename or contains
    # it. Requires a long enough overlap that a coincidence is implausible.
    function(rows) any_hit(rows[!is.na(ttl_key[rows])], function(i)
      !is.na(stem) & (startsWith(stem, ttl_key[i]) | startsWith(tk_all[i], substr(stem, 1, 24)))),
    # 4. First author's surname and the year both appear in the filename. This
    # is how people actually name downloaded PDFs -- "smith2019.pdf",
    # "Smith 2019 - Cognitive Load.pdf" -- and without it the routes above match
    # almost nothing in a real folder. Both parts are required and the match
    # must be unique, so two Smith papers from 2019 match neither rather than
    # attributing one paper's findings to the other.
    function(rows) any_hit(rows[!is.na(sur_re[rows])], function(i)
      grepl(sur_re[i], fstem, perl = TRUE) & grepl(recs$year[i], base, fixed = TRUE))
  )

  # One route at a time across ALL records, strongest first -- not every route
  # for one record before the next record is looked at. Record by record, a
  # weak surname match for an early record took a file that was an exact title
  # match for a later one, and which paper got the file depended on export
  # order. A claim needs the file to be the work's only candidate at that route
  # (two files of one name on disk is the same ambiguity as two records
  # claiming one file, seen from the other end) and the work to be the file's
  # only claimant. A file two works both want goes to neither at that route; it
  # stays on the table, as a key two works share does, for a later route that
  # only one work passes -- or for nobody, and then unmatched_files says so.
  pending <- which(is.na(dup))
  for (route in routes) {
    if (!length(pending) || all(taken)) break
    cands <- lapply(pending, function(g) route(members[[as.character(g)]]))
    wanted <- tabulate(unlist(cands), nbins = length(paths))
    win <- vapply(cands, function(j) length(j) == 1L && wanted[j] == 1L, logical(1))
    for (w in which(win)) {
      out[pending[w]] <- paths[cands[[w]]]
      taken[cands[[w]]] <- TRUE
    }
    pending <- pending[!win]
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
#' instead of living in a lab notebook. It reaches [gr_flow()] and the audit
#' report, and it round-trips through [gr_protocol_save()] alongside the
#' criteria, so what was searched and what was eligible are one artifact.
#'
#' @param databases Named character vector or list: name of each source, value
#'   the query string run against it. The query is the point; a review reporting
#'   "we searched PubMed" without it cannot be repeated.
#' @param dates When each search was run, as `YYYY-MM-DD`. One value, or one per
#'   database. Searches drift: a review is a statement about a date.
#' @param limits Limits applied: language, publication years, study design
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
              if (is.na(x$registration)) "NOT REGISTERED; say so in the report"
              else x$registration))
  invisible(x)
}
