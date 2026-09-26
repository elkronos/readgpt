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
#' quote inside braces (`{\"u}`) does not end one, and neither does one written
#' `\"`, which is not valid BibTeX but is how a hand-typed umlaut looks. Values
#' joined with `#` are concatenated. The first occurrence of a field wins, as it
#' does in BibTeX. A value ends where its braces or quotes close, so a
#' hand-edited file that left out the comma before the next field still reads
#' that field.
#' @noRd
bib_fields <- function(body) {
  ch <- strsplit(body, "", fixed = TRUE)[[1]]
  n <- length(ch)
  if (!n) return(list())
  step <- (ch == "{") - (ch == "}")
  depth <- cumsum(step) - step                  # depth before each character
  # Quotes that open or close a value. A backslash before one makes it text:
  # counting `author="M\"uller, J."` as three quotes flipped the parity for the
  # rest of the entry, and its title and year were lost.
  q <- ch == "\"" & depth == 0L & c("", ch[-n]) != "\\"
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
  # A bare (unbraced, unquoted) value runs to the next comma, `#` or line end,
  # as the regex this replaced read it: `journal = Nature Medicine,` is two
  # words, not "Nature". It stops early only where the next word is the name of
  # a field (`year = 2022 month = jan`, a missing comma).
  stops <- which(ch == "\n" | ch == "\r" | (top & (ch == "," | ch == "#")))
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
        nxt <- regexpr("[[:space:]]+[[:alnum:]_:.+-]+[[:space:]]*=", text(v, e))
        if (nxt > 0L) e <- v + nxt - 2L
        while (e > v && ws[e]) e <- e - 1L
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

#' LaTeX accent commands, each with the letters it composes with.
#'
#' `from` and `to` are the plain letters and what each becomes under that
#' accent, from Unicode's canonical compositions. An accent on a letter that
#' has no composed form keeps the combining mark after it.
#' @noRd
.gr_tex_accents <- local({
  acc <- list(
    list(cmd = "'", mark = "\u0301", from = "acegiklmnoprsuwyzACEGIKLMNOPRSUWYZ",
         to = paste0("\u00e1\u0107\u00e9\u01f5\u00ed\u1e31\u013a\u1e3f\u0144\u00f3\u1e55\u0155",
                     "\u015b\u00fa\u1e83\u00fd\u017a\u00c1\u0106\u00c9\u01f4\u00cd\u1e30\u0139",
                     "\u1e3e\u0143\u00d3\u1e54\u0154\u015a\u00da\u1e82\u00dd\u0179")),
    list(cmd = "`", mark = "\u0300", from = "aeinouwyAEINOUWY",
         to = paste0("\u00e0\u00e8\u00ec\u01f9\u00f2\u00f9\u1e81\u1ef3\u00c0\u00c8\u00cc\u01f8",
                     "\u00d2\u00d9\u1e80\u1ef2")),
    list(cmd = "^", mark = "\u0302", from = "aceghijosuwyzACEGHIJOSUWYZ",
         to = paste0("\u00e2\u0109\u00ea\u011d\u0125\u00ee\u0135\u00f4\u015d\u00fb\u0175\u0177",
                     "\u1e91\u00c2\u0108\u00ca\u011c\u0124\u00ce\u0134\u00d4\u015c\u00db\u0174",
                     "\u0176\u1e90")),
    list(cmd = "\"", mark = "\u0308", from = "aehiotuwxyAEHIOUWXY",
         to = paste0("\u00e4\u00eb\u1e27\u00ef\u00f6\u1e97\u00fc\u1e85\u1e8d\u00ff\u00c4\u00cb",
                     "\u1e26\u00cf\u00d6\u00dc\u1e84\u1e8c\u0178")),
    list(cmd = "~", mark = "\u0303", from = "aeinouvyAEINOUVY",
         to = paste0("\u00e3\u1ebd\u0129\u00f1\u00f5\u0169\u1e7d\u1ef9\u00c3\u1ebc\u0128\u00d1",
                     "\u00d5\u0168\u1e7c\u1ef8")),
    list(cmd = "=", mark = "\u0304", from = "aegiouyAEGIOUY",
         to = paste0("\u0101\u0113\u1e21\u012b\u014d\u016b\u0233\u0100\u0112\u1e20\u012a\u014c",
                     "\u016a\u0232")),
    list(cmd = ".", mark = "\u0307", from = "cegzCEGIZ",
         to = "\u010b\u0117\u0121\u017c\u010a\u0116\u0120\u0130\u017b"),
    list(cmd = "u", mark = "\u0306", from = "aegiouAEGIOU",
         to = "\u0103\u0115\u011f\u012d\u014f\u016d\u0102\u0114\u011e\u012c\u014e\u016c"),
    list(cmd = "v", mark = "\u030c", from = "acdeghijklnorstuzACDEGHIKLNORSTUZ",
         to = paste0("\u01ce\u010d\u010f\u011b\u01e7\u021f\u01d0\u01f0\u01e9\u013e\u0148\u01d2",
                     "\u0159\u0161\u0165\u01d4\u017e\u01cd\u010c\u010e\u011a\u01e6\u021e\u01cf",
                     "\u01e8\u013d\u0147\u01d1\u0158\u0160\u0164\u01d3\u017d")),
    list(cmd = "H", mark = "\u030b", from = "ouOU", to = "\u0151\u0171\u0150\u0170"),
    list(cmd = "c", mark = "\u0327", from = "cegklnrstCEGKLNRST",
         to = paste0("\u00e7\u0229\u0123\u0137\u013c\u0146\u0157\u015f\u0163\u00c7\u0228\u0122",
                     "\u0136\u013b\u0145\u0156\u015e\u0162")),
    list(cmd = "k", mark = "\u0328", from = "aeiouAEIOU",
         to = "\u0105\u0119\u012f\u01eb\u0173\u0104\u0118\u012e\u01ea\u0172"),
    list(cmd = "r", mark = "\u030a", from = "auAU", to = "\u00e5\u016f\u00c5\u016e"),
    list(cmd = "d", mark = "\u0323", from = "adehiklmnorstuyzADEHIKLMNORSTUYZ",
         to = paste0("\u1ea1\u1e0d\u1eb9\u1e25\u1ecb\u1e33\u1e37\u1e43\u1e47\u1ecd\u1e5b\u1e63",
                     "\u1e6d\u1ee5\u1ef5\u1e93\u1ea0\u1e0c\u1eb8\u1e24\u1eca\u1e32\u1e36\u1e42",
                     "\u1e46\u1ecc\u1e5a\u1e62\u1e6c\u1ee4\u1ef4\u1e92"))
  )
  base <- "(\\\\[ij](?![A-Za-z])|[A-Za-z])"
  lapply(acc, function(a) {
    cmd <- a$cmd
    # A symbol accent may be followed by its letter directly (\'a); a letter
    # accent needs a brace or a space, or it is a different command (\vspace).
    re <- if (grepl("^[A-Za-z]$", cmd)) {
      sprintf("\\\\%s(?:[[:space:]]*\\{[[:space:]]*%s[[:space:]]*\\}|[[:space:]]+%s)", cmd, base, base)
    } else {
      sprintf("\\\\\\%s[[:space:]]*(?:\\{[[:space:]]*%s[[:space:]]*\\}|%s)", cmd, base, base)
    }
    list(re = re, mark = a$mark, from = strsplit(a$from, "")[[1]],
         to = strsplit(a$to, "")[[1]])
  })
})

#' LaTeX commands that are letters in their own right.
#' @noRd
.gr_tex_letters <- list(
  cmd = c("ss", "ae", "AE", "oe", "OE", "aa", "AA", "o", "O", "l", "L", "i", "j"),
  to = c("\u00df", "\u00e6", "\u00c6", "\u0153", "\u0152", "\u00e5", "\u00c5", "\u00f8",
         "\u00d8", "\u0142", "\u0141", "\u0131", "\u0237"))

#' Turn LaTeX accents in a BibTeX value into the letters they spell.
#'
#' Scholar, JabRef and BibDesk write "Dvo{\\v{r}}{\\'a}k"; a database writes
#' "Dvo\u0159\u00e1k". Stripping the braces and keeping the rest gave
#' "Dvo\\vr\\'ak", which is nobody's name: it went into citations as that, and
#' its key "dvovrak" matched neither the database's record nor a filename. The
#' symbol accents (\\" and \\') vanished from keys by luck; the letter ones
#' (\\v, \\c, \\H, \\k, \\u) left a stray letter inside the surname.
#' @noRd
bib_unlatex <- function(v) {
  if (is.na(v) || !grepl("\\", v, fixed = TRUE)) return(v)
  for (a in .gr_tex_accents) {
    m <- gregexpr(a$re, v, perl = TRUE)
    if (m[[1]][1] == -1L) next
    regmatches(v, m) <- list(vapply(regmatches(v, m)[[1]], function(s) {
      b <- sub(a$re, "\\1\\2", s, perl = TRUE)
      b <- sub("^\\\\", "", b)                  # \i and \j are the dotless letters
      k <- match(b, a$from)
      if (is.na(k)) paste0(b, a$mark) else a$to[k]
    }, character(1), USE.NAMES = FALSE))
  }
  for (k in seq_along(.gr_tex_letters$cmd)) {
    v <- gsub(sprintf("\\\\%s(?![A-Za-z])(?:\\{\\})?[[:space:]]*", .gr_tex_letters$cmd[k]),
              .gr_tex_letters$to[k], v, perl = TRUE)
  }
  mark_utf8(v)
}

#' @noRd
records_from_bib <- function(txt, source_file) {
  entries <- bib_entries(txt)
  if (!length(entries)) return(empty_records())
  rows <- lapply(entries, function(body) {
    fields <- bib_fields(body)
    tags <- lapply(names(fields), function(nm) {
      v <- trimws(fields[[nm]])
      # Not in a path or a link, where "\o" is a folder, not a letter.
      if (!nm %in% c("file", "url", "doi")) v <- bib_unlatex(v)
      # Capitalisation-protecting braces are not part of the value, and the
      # characters BibTeX makes you escape are not meant to keep their
      # backslash: "Memory {\\&} Cognition" is "Memory & Cognition".
      v <- gsub("\\\\([&%$#_{}])", "\\1", v, perl = TRUE)
      trimws(gsub("[{}]", "", v))
    })
    names(tags) <- names(fields)
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
  # Every other letter in Latin Extended-B and Latin Extended Additional whose
  # canonical decomposition is one plain letter and marks: Vietnamese above all
  # ("Nguy\u1ec5n" was a different surname from "Nguyen", where the old iconv step
  # made them one), and what a BibTeX accent command composes to.
  ext <- c(
    a = paste0("\u01ce\u01df\u01e1\u01fb\u0201\u0203\u0227\u1e01\u1ea1\u1ea3\u1ea5\u1ea7",
               "\u1ea9\u1eab\u1ead\u1eaf\u1eb1\u1eb3\u1eb5\u1eb7"),
    A = paste0("\u01cd\u01de\u01e0\u01fa\u0200\u0202\u0226\u1e00\u1ea0\u1ea2\u1ea4\u1ea6",
               "\u1ea8\u1eaa\u1eac\u1eae\u1eb0\u1eb2\u1eb4\u1eb6"),
    b = "\u1e03\u1e05\u1e07", B = "\u1e02\u1e04\u1e06", c = "\u1e09", C = "\u1e08",
    d = "\u1e0b\u1e0d\u1e0f\u1e11\u1e13", D = "\u1e0a\u1e0c\u1e0e\u1e10\u1e12",
    e = paste0("\u0205\u0207\u0229\u1e15\u1e17\u1e19\u1e1b\u1e1d\u1eb9\u1ebb\u1ebd\u1ebf",
               "\u1ec1\u1ec3\u1ec5\u1ec7"),
    E = paste0("\u0204\u0206\u0228\u1e14\u1e16\u1e18\u1e1a\u1e1c\u1eb8\u1eba\u1ebc\u1ebe",
               "\u1ec0\u1ec2\u1ec4\u1ec6"),
    f = "\u1e1f", F = "\u1e1e", g = "\u01e7\u01f5\u1e21", G = "\u01e6\u01f4\u1e20",
    h = "\u021f\u1e23\u1e25\u1e27\u1e29\u1e2b\u1e96", H = "\u021e\u1e22\u1e24\u1e26\u1e28\u1e2a",
    i = "\u01d0\u0209\u020b\u1e2d\u1e2f\u1ec9\u1ecb", I = "\u01cf\u0208\u020a\u1e2c\u1e2e\u1ec8\u1eca",
    j = "\u01f0\u0237", k = "\u01e9\u1e31\u1e33\u1e35", K = "\u01e8\u1e30\u1e32\u1e34",
    l = "\u1e37\u1e39\u1e3b\u1e3d", L = "\u1e36\u1e38\u1e3a\u1e3c",
    m = "\u1e3f\u1e41\u1e43", M = "\u1e3e\u1e40\u1e42",
    n = "\u01f9\u1e45\u1e47\u1e49\u1e4b", N = "\u01f8\u1e44\u1e46\u1e48\u1e4a",
    o = paste0("\u01a1\u01d2\u01eb\u01ed\u020d\u020f\u022b\u022d\u022f\u0231\u1e4d\u1e4f",
               "\u1e51\u1e53\u1ecd\u1ecf\u1ed1\u1ed3\u1ed5\u1ed7\u1ed9\u1edb\u1edd\u1edf",
               "\u1ee1\u1ee3"),
    O = paste0("\u01a0\u01d1\u01ea\u01ec\u020c\u020e\u022a\u022c\u022e\u0230\u1e4c\u1e4e",
               "\u1e50\u1e52\u1ecc\u1ece\u1ed0\u1ed2\u1ed4\u1ed6\u1ed8\u1eda\u1edc\u1ede",
               "\u1ee0\u1ee2"),
    p = "\u1e55\u1e57", P = "\u1e54\u1e56",
    r = "\u0211\u0213\u1e59\u1e5b\u1e5d\u1e5f", R = "\u0210\u0212\u1e58\u1e5a\u1e5c\u1e5e",
    s = "\u1e61\u1e63\u1e65\u1e67\u1e69\u017f", S = "\u1e60\u1e62\u1e64\u1e66\u1e68",
    t = "\u1e6b\u1e6d\u1e6f\u1e71\u1e97", T = "\u1e6a\u1e6c\u1e6e\u1e70",
    u = paste0("\u01b0\u01d4\u01d6\u01d8\u01da\u01dc\u0215\u0217\u1e73\u1e75\u1e77\u1e79",
               "\u1e7b\u1ee5\u1ee7\u1ee9\u1eeb\u1eed\u1eef\u1ef1"),
    U = paste0("\u01af\u01d3\u01d5\u01d7\u01d9\u01db\u0214\u0216\u1e72\u1e74\u1e76\u1e78",
               "\u1e7a\u1ee4\u1ee6\u1ee8\u1eea\u1eec\u1eee\u1ef0"),
    v = "\u1e7d\u1e7f", V = "\u1e7c\u1e7e",
    w = "\u1e81\u1e83\u1e85\u1e87\u1e89\u1e98", W = "\u1e80\u1e82\u1e84\u1e86\u1e88",
    x = "\u1e8b\u1e8d", X = "\u1e8a\u1e8c",
    y = "\u0233\u1e8f\u1e99\u1ef3\u1ef5\u1ef7\u1ef9", Y = "\u0232\u1e8e\u1ef2\u1ef4\u1ef6\u1ef8",
    z = "\u1e91\u1e93\u1e95", Z = "\u1e90\u1e92\u1e94")
  # Compatibility characters, which are one character spelled another way: a
  # superscript or subscript digit, a full-width letter from a CJK input
  # method, the micro sign (which NFKC makes the Greek letter). The iconv step
  # folded most of these, so "m\u00b2" and "m2" were one title; the table has to
  # say so itself. (No "-" anywhere in these strings: chartr() reads it as a range.)
  compat_from <- paste0("\u2070\u00b9\u00b2\u00b3\u2074\u2075\u2076\u2077\u2078\u2079",
                        "\u2080\u2081\u2082\u2083\u2084\u2085\u2086\u2087\u2088\u2089",
                        intToUtf8(c(0xFF10:0xFF19, 0xFF21:0xFF3A, 0xFF41:0xFF5A)), "\u00b5")
  compat_to <- paste0("0123456789", "0123456789", "0123456789",
                      paste(LETTERS, collapse = ""), paste(letters, collapse = ""), "\u03bc")
  list(from = paste0(paste(one, collapse = ""), paste(ext, collapse = ""), compat_from),
       to = paste0(paste(rep(names(one), nchar(one)), collapse = ""),
                   paste(rep(names(ext), nchar(ext)), collapse = ""), compat_to),
       # Two vectors, not a named one: a name is a symbol, and a symbol is
       # translated to the native encoding, which in the C locale turned "\u00df"
       # into "<U+00DF>" and folded nothing. Ligatures come from PDF metadata
       # ("E\ufb03cacy"), and a vulgar fraction was "1/2" to the iconv step.
       multi_from = c("\u00df", "\u1e9e", "\u00e6", "\u00c6", "\u0153", "\u0152",
                      "\u00fe", "\u00de", "\u0133", "\u0132",
                      "\ufb00", "\ufb01", "\ufb02", "\ufb03", "\ufb04", "\ufb05", "\ufb06",
                      "\u00bc", "\u00bd", "\u00be", "\u2153", "\u2154", "\u2155", "\u215b"),
       multi_to = c("ss", "SS", "ae", "AE", "oe", "OE", "th", "TH", "ij", "IJ",
                    "ff", "fi", "fl", "ffi", "ffl", "st", "st",
                    "1/4", "1/2", "3/4", "1/3", "2/3", "1/5", "1/8"))
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
  # Lower-cased again after folding: tolower() may leave a capital it has no
  # table for in the C locale, and the fold turns it into an ASCII one.
  v <- tolower(fold_latin(tolower(mark_utf8(as.character(x)))))
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
#'   its DOI and once without. The title fallback also needs the first author's
#'   surname to agree as a whole name ("Smith, J." and "Smith JA" agree, "Li" and
#'   "Lin" do not), or, when a record has no authors, the venue. It ignores
#'   titles shorter than 12 letters ("Reply", "Editorial") and never merges
#'   records carrying two different DOIs. A record without a DOI is merged with
#'   one that has a DOI, and a title with fewer than 12 Latin letters or digits
#'   is merged at all, only when the venues do not disagree, the first authors'
#'   initials do not differ, and the author or the venue confirms it. The kept row takes
#'   any field it lacks (the DOI, the journal) from the rows merged into it.
#'   `"none"` keeps everything.
#' @return An object of class `gr_records`:
#'   \describe{
#'     \item{`records`}{One row per distinct work, with `duplicate_of` naming the
#'       row a dropped record repeats, `file` the document matched to it, and
#'       `retrieved` whether one was found. A duplicate's file path counts
#'       towards the row it repeats, which is where the match is reported; a
#'       second copy the duplicate's own path points to is shown on the
#'       duplicate's row.}
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
  # `source_file` is already a basename. basename() again translated it to the
  # native encoding, and in a C locale an export named "\u00c9vora.ris" aborted
  # the whole call.
  by_db <- table(ifelse(is.na(recs$database), recs$source_file, recs$database))

  recs$duplicate_of <- dedupe_records(recs, dedupe)
  distinct <- is.na(recs$duplicate_of)
  recs <- merge_duplicate_fields(recs)

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
    # mark_utf8(), not enc2utf8(): in a C locale enc2utf8() rewrites the
    # unlabelled UTF-8 bytes of "\u00c9vora.ris" as the text "<c3><89>vora.ris",
    # which sorts before "Baker".
    return(f[order(mark_utf8(f), method = "radix")])
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

#' Words that make an author an organisation rather than a person.
#'
#' This file's own list, not the citation code's: which records are one work
#' and which file is whose must not change when the rules for rendering a
#' citation do.
#' @noRd
.gr_rec_corporate <- paste0(
  "\\b(group|collaborat\\w*|consortium|investigators?|committee|organi[sz]ations?|",
  "associations?|institutes?|society|council|agency|department|ministry|foundation|",
  "universit(y|ies)|cent(re|er)s?|commission|federation|bureau|administration|network|",
  "academy|alliance|coalition|initiative|task ?force|working party|trialists|panel)\\b")

#' Particles that are part of a surname but not what identifies it.
#' @noRd
.gr_rec_particles <- c("van", "von", "der", "den", "de", "del", "della", "di", "da", "dos",
                       "das", "du", "la", "le", "ten", "ter", "bin", "ibn", "al", "el", "st",
                       "zu", "af", "av")

#' The first author of each record: surname, first initial, and whether it is
#' an organisation.
#'
#' Only the first author, read on its own. Reading the whole list and giving up
#' when any entry was doubtful -- which is right for a rendered citation --
#' lost the first author whenever a later one was a group ("Horby P; Lim WS;
#' RECOVERY Collaborative Group") or an acronym, and with it the author check
#' on duplicates and the author-year route to a file.
#'
#' The shapes: "Smith, J." and "van der Berg, P." (surname before the comma);
#' "Smith JA" and "SMITH J" (Vancouver: surname, then initials); "John Smith"
#' (the last word); "WHO" (one word is the name); and an organisation, kept
#' whole.
#' @noRd
record_first_author <- function(authors) {
  a <- mark_utf8(as.character(authors))
  n <- length(a)
  out <- list(surname = rep(NA_character_, n), initial = rep(NA_character_, n),
              corporate = rep(FALSE, n), entry = rep(NA_character_, n))
  is_ini <- function(w) grepl("^(?:\\p{Lu}[.\\-]*){1,4}$", w, perl = TRUE)
  for (k in seq_len(n)) {
    e <- a[k]
    if (is.na(e)) next
    e <- trimws(gsub("[{}\"]", "", sub(";.*$", "", e)))
    e <- trimws(sub("[,[:space:]]*(et[[:space:]]+al\\.?|and others)[.]?$", "", e,
                    ignore.case = TRUE, perl = TRUE))
    # One author per slot is the norm; a single field holding the whole list
    # ("Smith, J., Okafor, A. and Lee, K.") gives up its first name here.
    e <- trimws(strsplit(e, "[[:space:]]*(?:\\band\\b|&)[[:space:]]*", perl = TRUE)[[1]][1])
    if (is.na(e) || !grepl("\\p{L}", e, perl = TRUE)) next
    out$entry[k] <- e
    if (!grepl(",", e, fixed = TRUE) && grepl("[[:space:]]", e) &&
        grepl(.gr_rec_corporate, e, ignore.case = TRUE, perl = TRUE)) {
      out$surname[k] <- e
      out$corporate[k] <- TRUE
      next
    }
    comma <- grepl(",", e, fixed = TRUE)
    head <- if (comma) trimws(sub(",.*$", "", e)) else e
    rest <- if (comma) trimws(sub("^[^,]*,", "", e)) else ""
    w <- strsplit(head, "[[:space:]]+")[[1]]
    w <- w[nzchar(w) & !grepl("^(Jr|Sr|II|III|IV)\\.?$", w)]
    if (!length(w)) next
    ini <- is_ini(w)
    if (length(w) == 1L) {
      s <- w; g <- rest
    } else if (all(ini)) {
      # "LI WS", "KIM S": all capitals, so every word looks like initials. The
      # longest is the surname, and the first on a tie, as Vancouver writes it.
      len <- nchar(gsub("[^[:alpha:]]", "", w))
      s <- w[which.max(len)]; g <- paste(w[-which.max(len)], collapse = " ")
    } else if (ini[length(w)]) {
      last <- max(which(!ini))
      s <- paste(w[seq_len(last)], collapse = " ")
      g <- paste(w[-seq_len(last)], collapse = " ")
    } else if (comma) {
      s <- paste(w, collapse = " "); g <- rest
    } else {
      s <- w[length(w)]; g <- w[1]
    }
    s <- gsub("^[^\\p{L}\\p{N}]+|[^\\p{L}\\p{N}]+$", "", s, perl = TRUE)
    if (!grepl("\\p{L}", s, perl = TRUE)) next
    out$surname[k] <- s
    g1 <- regmatches(g, regexpr("\\p{L}", g, perl = TRUE))
    if (length(g1)) out$initial[k] <- g1
  }
  # Folded once for the whole vector: the fold table is long, and chartr()
  # builds it afresh on every call.
  out$initial <- toupper(fold_latin(out$initial))
  out
}

#' A name reduced to the ways two exports can spell it.
#'
#' The folded key, and a looser one that also reads "ue" as "u", "oe" as "o",
#' "ae" as "a" and "aa" as "a" -- the German and Scandinavian transliterations,
#' so "M\u00fcller", "Mueller" and "Muller" are one name, and "S\u00f8rensen" and
#' "Soerensen".
#' @noRd
record_name_forms <- function(x) {
  lapply(title_key(x), function(k) {
    if (is.na(k)) character(0) else unique(c(k, gsub("aa", "a", gsub("([aou])e", "\\1", k))))
  })
}

#' The words of each name, less its particles, in both spellings.
#' @noRd
record_name_words <- function(x) {
  lapply(tolower(fold_latin(x)), function(v) {
    if (is.na(v)) return(character(0))
    w <- strsplit(v, "[^\\p{L}\\p{N}]+", perl = TRUE)[[1]]
    w <- w[nchar(w) >= 2L]
    core <- w[!w %in% .gr_rec_particles]
    if (length(core)) w <- core
    unique(c(w, gsub("aa", "a", gsub("([aou])e", "\\1", w))))
  })
}

#' The significant words of each venue name, for record_venue_match().
#' @noRd
record_venue_words <- function(v) {
  v <- gsub("\\([^)]*\\)", " ", mark_utf8(as.character(v)))
  lapply(tolower(fold_latin(v)), function(x) {
    if (is.na(x)) return(character(0))
    w <- strsplit(x, "[^\\p{L}\\p{N}]+", perl = TRUE)[[1]]
    w[nzchar(w) & !w %in% c("the", "of", "and", "for", "in", "on", "an", "de", "la", "le",
                            "der", "und", "des", "du", "et", "y", "e", "at", "to")]
  })
}

#' Whether two venue names, as record_venue_words() gives them, are one journal.
#'
#' `loose` is the test for records that have no authors to compare: one name
#' contained in the other, as before, or one an abbreviation of the other. The
#' strict test, for a merge the title key alone never made (see
#' dedupe_records()), drops containment -- "BMJ" is in "BMJ Open", and "Sleep"
#' in "Sleep Medicine", and neither is the same journal -- and keeps the same
#' spelling, a word-for-word abbreviation ("J Educ Psychol" of "Journal of
#' Educational Psychology"), or an acronym ("BMJ", "JAMA").
#' @noRd
record_venue_match <- function(wa, wb, loose = TRUE) {
  if (!length(wa) || !length(wb)) return(NA)
  ka <- paste(wa, collapse = ""); kb <- paste(wb, collapse = "")
  if (identical(ka, kb)) return(TRUE)
  if (loose && (grepl(ka, kb, fixed = TRUE) || grepl(kb, ka, fixed = TRUE))) return(TRUE)
  # "educ" abbreviates "educational": same first letter, then its letters in order.
  abbrev <- function(s, l) {
    substr(s, 1, 1) == substr(l, 1, 1) &&
      grepl(paste0("^", paste(strsplit(s, "")[[1]], collapse = ".*")), l)
  }
  if (length(wa) == length(wb) &&
      all(mapply(function(x, y) abbrev(x, y) || abbrev(y, x), wa, wb))) return(TRUE)
  acronym <- function(s, l) length(s) == 1L && length(l) >= 2L &&
    identical(s, paste(substr(l, 1, 1), collapse = ""))
  acronym(wa, wb) || acronym(wb, wa)
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
  # The first earlier row with the same key, or NA. match(), not an environment
  # keyed on the value: an environment's names are symbols, translated to the
  # native encoding, so in the C locale every non-Latin title warned.
  first_of <- function(key) {
    m <- match(key, key)
    m[is.na(key) | m == seq_along(key)] <- NA_integer_
    m
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
    # neither rather than with whichever came first. Two DOI-less rows of it
    # can still be one another's copy, as they always could: blanking the key
    # for them too kept the same Scholar letter twice whenever two other
    # letters of that title had DOIs.
    ok <- !is.na(key) & !is.na(recs$doi)
    kid <- match(key, key)
    multi_doi <- rep(FALSE, n)
    if (any(ok)) {
      n_doi <- vapply(split(recs$doi[ok], kid[ok]), function(d) length(unique(d)), integer(1))
      multi_doi <- !is.na(key) & kid %in% as.integer(names(n_doi)[n_doi > 1L])
    }
    # A title and year is not enough on its own either: two letters titled
    # "Response to the commentary on ..." in one year are not one letter. The
    # first author's surname has to agree as a whole name -- "Smith, J." and
    # "Smith JA" are one person, "Li" and "Lin" are not -- or, when a record has
    # no authors, the venue.
    fa <- record_first_author(recs$authors)
    forms <- record_name_forms(fa$surname)
    words <- record_name_words(fa$surname)
    entry_words <- record_name_words(fa$entry)
    all_key <- title_key(recs$authors)
    all_words <- record_name_words(recs$authors)
    # Merges the old title key could never make are held to more: a DOI-less
    # record with one that has a DOI (it used to blank the key of any row with
    # a DOI), and a title with fewer than 12 Latin letters or digits (its key
    # was the digits and nothing else, so a Chinese title could never match).
    # Identical generic titles are common in both, and there a surname alone is
    # thin: "Wang, L." and "Wang, H." wrote different papers, and so did two
    # authors of one name in two journals.
    latin <- !is.na(tk) & nchar(gsub("[^a-z0-9]", "", tk)) >= 12L
    authors_agree <- function(i, j, strict) {
      if (is.na(fa$surname[i]) || is.na(fa$surname[j])) return(NA)
      if (fa$corporate[i] || fa$corporate[j]) {
        if (fa$corporate[i] && fa$corporate[j]) {
          a <- forms[[i]][1]; b <- forms[[j]][1]
          return(if (grepl(a, b, fixed = TRUE) || grepl(b, a, fixed = TRUE)) "yes" else "no")
        }
        # A group first in one export and a person first in the other is one
        # work when either names the other somewhere in its author list.
        g <- if (fa$corporate[i]) i else j
        p <- if (fa$corporate[i]) j else i
        hit <- (!is.na(all_key[p]) && grepl(forms[[g]][1], all_key[p], fixed = TRUE)) ||
          any(words[[p]] %in% all_words[[g]])
        return(if (hit) "yes" else "no")
      }
      if (any(forms[[i]] %in% forms[[j]]) || any(words[[i]] %in% words[[j]])) {
        if (strict && !is.na(fa$initial[i]) && !is.na(fa$initial[j]) &&
            fa$initial[i] != fa$initial[j]) return("no")
        return("yes")
      }
      # "Li Wei" in one export and "Li, W." in the other: a given name first.
      if (any(words[[i]] %in% entry_words[[j]]) || any(words[[j]] %in% entry_words[[i]])) {
        return("order")
      }
      "no"
    }
    venue_words <- record_venue_words(recs$venue)
    same_work <- function(i, j, strict) {
      a <- authors_agree(i, j, strict)
      if (identical(a, "no")) return(FALSE)
      if (!strict && !is.na(a)) return(TRUE)
      v <- record_venue_match(venue_words[[i]], venue_words[[j]], loose = !strict)
      if (!strict) return(!isFALSE(v))
      # Nothing may contradict it, and something beyond the title must confirm it.
      !isFALSE(v) && (identical(a, "yes") || isTRUE(v))
    }
  }
  # `root` is the kept row each row belongs to. Every row is resolved to a KEPT
  # row as it is reached, so `duplicate_of` never points at another duplicate.
  root <- seq_len(n)
  has_doi <- !is.na(recs$doi)
  group_doi <- has_doi
  kid <- match(key, key)
  holders <- vector("list", n)
  for (i in seq_len(n)) {
    if (!is.na(doi_hit[i])) {
      root[i] <- root[doi_hit[i]]
    } else if (by_title && !is.na(key[i])) {
      h <- holders[[kid[i]]]
      if (length(h)) {
        # Every row of a work that carries this title has to agree, not just
        # one: a group holding Smith's record and an authorless one is still
        # Smith's paper, and Jones's letter in the same journal is not it.
        gs <- unique(root[h])
        fits <- vapply(gs, function(g) {
          # A DOI of its own and one in the group are two DOIs (the same one
          # would have matched above).
          if (has_doi[i] && group_doi[g]) return(FALSE)
          if (multi_doi[i] && (has_doi[i] || group_doi[g])) return(FALSE)
          strict <- has_doi[i] || group_doi[g] || !latin[i]
          all(vapply(h[root[h] == g], function(j) same_work(i, j, strict), logical(1)))
        }, logical(1))
        # Two different works it could equally be is no match at all.
        if (sum(fits) == 1L) root[i] <- gs[fits]
      }
    }
    group_doi[root[i]] <- group_doi[root[i]] || has_doi[i]
    if (by_title && !is.na(key[i])) holders[[kid[i]]] <- c(holders[[kid[i]]], i)
  }
  dup <- root != seq_len(n)
  out[dup] <- recs$record_id[root[dup]]
  out
}

#' Fill the kept row's empty fields from the rows it absorbed.
#'
#' The kept row is the one that came first, which is often the export with the
#' least in it: "scholar.bib" sorts before "scopus.ris", and Scholar gives no
#' DOI. Its duplicate had the DOI and the journal, the file was found through
#' that DOI, and then the extraction table and every citation built from it
#' said DOI NA and venue NA -- the document identified by a DOI the record set
#' then denied it had. Only empty fields are filled, in record order; where the
#' search came from (database, accession, source file) and the file path stay
#' each row's own.
#' @noRd
merge_duplicate_fields <- function(recs) {
  dup <- recs$duplicate_of
  if (is.null(dup) || all(is.na(dup))) return(recs)
  fill <- c("type", "authors", "year", "title", "venue", "volume", "issue", "pages", "doi",
            "abstract", "keywords")
  kept <- match(dup, recs$record_id)
  for (f in fill) {
    v <- recs[[f]]
    src <- which(!is.na(kept) & !is.na(v))       # duplicates that have it, in record order
    src <- src[!duplicated(kept[src])]            # the first of them for each kept row
    gap <- is.na(v[kept[src]])
    v[kept[src][gap]] <- v[src[gap]]
    recs[[f]] <- v
  }
  recs
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
  # matches with everything its duplicates know, the kept row's own evidence
  # first and then each duplicate's in record order.
  dup <- recs$duplicate_of %||% rep(NA_integer_, n)
  owner <- ifelse(is.na(dup), seq_len(n), match(dup, recs$record_id))
  kept <- is.na(dup)
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
  #
  # The author-year key is the exception (`own_first`). There a kept row is
  # settled against kept rows only, and a duplicate's key is extra evidence
  # for its work, used only when no other work has it at all: the same paper
  # routinely carries its e-pub year in one database and its print year in
  # another, and its PubMed copy's "jones|2019" made a different Jones's 2019
  # paper unmatchable even after the first Jones had claimed its own file.
  settle <- function(k, own_first = FALSE) {
    k[is.na(owner)] <- NA_character_
    kid <- match(k, k)
    shared <- function(rows) {
      cnt <- vapply(split(owner[rows], kid[rows]), function(g) length(unique(g)), integer(1))
      as.integer(names(cnt)[cnt > 1L])
    }
    ok <- !is.na(k)
    if (!own_first) {
      if (any(ok)) k[kid %in% shared(which(ok))] <- NA_character_
      return(k)
    }
    if (any(ok & kept)) k[kept & kid %in% shared(which(ok & kept))] <- NA_character_
    if (any(ok & !kept)) k[!kept & kid %in% shared(which(ok))] <- NA_character_
    k
  }
  sur <- record_first_author(recs$authors)$surname
  sur_k <- title_key(sur)
  ay <- ifelse(is.na(sur_k) | nchar(sur_k) < 3L | is.na(recs$year), NA_character_,
               paste0(sur_k, "|", recs$year))
  ay <- settle(ay, own_first = TRUE)
  # The surname as a whole word of the filename, not as any substring of it.
  # `grepl("chen", "cheng2020")` gave Chen's record Cheng's paper, "park" found
  # "Parkinson disease and exercise", and "lee" found "Sleep quality". A word
  # starts where letters start, and ends where they stop or where a lower-case
  # letter meets a capital, so "SmithEtAl2019" and "SmithJ_2019" still name
  # Smith, and so does "smithetal2019". A capital inside a word does not start
  # one: "McKay2019" is not Kay's, "DeWitt" not Witt's. An all-lower
  # "smithj2019" is left unmatched rather than guessed at, since "chenl" cannot
  # be told from "cheng".
  sur_f <- fold_latin(sur)
  sur_re <- vapply(seq_len(n), function(i) {
    if (is.na(ay[i])) return(NA_character_)
    w <- tolower(regmatches(sur_f[i], gregexpr("[\\p{L}\\p{N}]+", sur_f[i], perl = TRUE))[[1]])
    if (!length(w)) return(NA_character_)
    after <- "(?:(?i:et[^\\p{L}\\p{N}]*al)?(?!\\p{L})|(?<=\\p{Ll})(?=\\p{Lu}))"
    paste0("(?<!\\p{L})(?i:", paste(w, collapse = "[^\\p{L}\\p{N}]*"), ")", after)
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

  # Each route gives, for one row, the untaken files it points to, or NULL when
  # the row has nothing to offer that route.
  hits <- list(
    # 1. The export said where the file is. Mendeley writes ":path:pdf";
    # EndNote writes "internal-pdf://...". The full path first, and the
    # basename only when no full path matches.
    function(i) {
      if (is.na(cand_all[i])) return(NULL)
      j <- which(!taken & paths == cand_all[i])
      if (!length(j) && !is.na(base_key[i])) j <- which(!taken & base == base_key[i])
      j
    },
    # 2. The DOI suffix appears in the filename -- how most managers name files.
    function(i) {
      if (is.na(doi_key[i])) return(NULL)
      which(!taken & grepl(doi_key[i], stem, fixed = TRUE))
    },
    # 3. The title, reduced to letters, is a prefix of the filename or contains
    # it. Requires a long enough overlap that a coincidence is implausible.
    function(i) {
      if (is.na(ttl_key[i])) return(NULL)
      which(!taken & !is.na(stem) &
              (startsWith(stem, ttl_key[i]) | startsWith(tk_all[i], substr(stem, 1, 24))))
    },
    # 4. First author's surname and the year both appear in the filename. This
    # is how people actually name downloaded PDFs -- "smith2019.pdf",
    # "Smith 2019 - Cognitive Load.pdf" -- and without it the routes above match
    # almost nothing in a real folder. Both parts are required and the match
    # must be unique, so two Smith papers from 2019 match neither rather than
    # attributing one paper's findings to the other. `pos` is where the surname
    # starts in the filename, which says whose file "Kim and Lee 2020" is.
    function(i) {
      if (is.na(sur_re[i])) return(NULL)
      at <- regexpr(sur_re[i], fstem, perl = TRUE)
      j <- which(!taken & at > 0L & grepl(recs$year[i], base, fixed = TRUE))
      structure(j, pos = as.integer(at[j]))
    }
  )
  # What one work offers a route: `pick`, the files of the first of its rows
  # that points anywhere (the kept row's own recorded path beats a duplicate's,
  # so two copies of one paper, each with its own PDF, are not an ambiguity);
  # `want`, everything any of its rows points to, which is what it contests;
  # and each row's own files, for the copies a route-1 claim also accounts for.
  offer <- function(g, route) {
    pick <- integer(0); want <- integer(0); pos <- integer(0); by_row <- list()
    for (i in members[[as.character(g)]]) {
      j <- hits[[route]](i)
      if (is.null(j)) next
      by_row[[as.character(i)]] <- as.integer(j)
      if (!length(pick)) pick <- as.integer(j)
      p <- attr(j, "pos") %||% rep(0L, length(j))
      for (k in seq_along(j)) {
        m <- match(j[k], want)
        if (is.na(m)) { want <- c(want, j[k]); pos <- c(pos, p[k]) }
        else pos[m] <- min(pos[m], p[k])
      }
    }
    list(pick = pick, want = want, pos = pos, by_row = by_row)
  }

  # One route at a time across ALL records, strongest first -- not every route
  # for one record before the next record is looked at. Record by record, a
  # weak surname match for an early record took a file that was an exact title
  # match for a later one, and which paper got the file depended on export
  # order. A claim needs the file to be the work's only candidate at that route
  # (two files of one name on disk is the same ambiguity as two records
  # claiming one file, seen from the other end) and the work to be the file's
  # only claimant. A file two works both want goes to neither; it stays on the
  # table, as a key two works share does, for a later route that only one work
  # passes -- or for nobody, and then unmatched_files says so.
  #
  # A route is run again while it still settles something: once a work has
  # claimed its file, a work that wanted that file and one other now wants only
  # the other. And at route 4 a file naming two surnames is claimed by the one
  # it names first, as a filename names its first author first: "Kim and Lee
  # 2020" is Kim's, and it no longer stops Lee's record taking "Lee 2020".
  pending <- which(kept & !is.na(owner))
  for (route in seq_along(hits)) {
    repeat {
      if (!length(pending) || all(taken)) break
      offers <- lapply(pending, offer, route = route)
      who <- rep(seq_along(offers), vapply(offers, function(o) length(o$want), integer(1)))
      what <- unlist(lapply(offers, `[[`, "want"))
      if (!length(what)) break
      pos <- unlist(lapply(offers, `[[`, "pos"))
      front <- pos == stats::ave(pos, what, FUN = min)
      wanted <- tabulate(what[front], nbins = length(paths))
      win <- vapply(seq_along(offers), function(w) {
        p <- offers[[w]]$pick
        length(p) == 1L && wanted[p] == 1L && any(who == w & what == p & front)
      }, logical(1))
      if (!any(win)) break
      for (w in which(win)) {
        g <- pending[w]
        f <- offers[[w]]$pick
        out[g] <- paths[f]
        taken[f] <- TRUE
        if (route == 1L) {
          # The other rows' own recorded files are that work's other copies --
          # one library holding the item twice, each with its PDF attached --
          # and each is reported on the row that recorded it, as it was before
          # duplicates were pooled. Only a file no other work points to.
          for (nm in names(offers[[w]]$by_row)) {
            i <- as.integer(nm)
            j <- offers[[w]]$by_row[[nm]]
            if (i != g && length(j) == 1L && !taken[j] && wanted[j] == 1L) {
              out[i] <- paths[j]
              taken[j] <- TRUE
            }
          }
        }
      }
      pending <- pending[!win]
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
