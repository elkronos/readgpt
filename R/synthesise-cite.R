# synthesise-cite.R -- turning study numbers into citations, and a reference list.
#
# WHY THE MODEL IS NOT ASKED TO WRITE "Smith and Okafor (2019)"
# Because then nothing could check it. `[study 3]` is checkable exactly: parse
# the markers, compare against the rows that exist, and a citation to a row that
# was never supplied is caught every time. An author-year string is not
# checkable that way -- to verify "Smith and Okafor (2019)" you would have to
# match a name the model wrote against a name extracted from a document, and
# near-misses (Smith vs Smyth, 2019 vs 2018) are exactly the errors that matter
# and exactly the ones fuzzy matching forgives.
#
# So the model keeps writing markers, the markers are checked as they always
# were, and rendering happens AFTER the check, from the table. A rendered
# citation is therefore a fact about the extraction rather than something the
# model asserted, and `$sections$text_marked` keeps the pre-rendering form so
# the check can be re-run on the published prose at any time.
#
# WHY PARSING AUTHORS IS THE LAST RESORT
# "Smith, J., Okafor, A." and "John Smith and Aisha Okafor" and "Smith J,
# Okafor A" all name the same two people, and any rule that handles all three
# will mangle a fourth. The reliable way to get a citation key is to ask for one
# during extraction -- a `citation` field whose description says what shape it
# should take -- and that is the first thing looked for. Parsing is what happens
# when nobody asked, and when it cannot be done confidently the run falls back
# to markers rather than printing a name that might be wrong.

#' The conventional names for the bibliographic roles, in preference order.
#' @noRd
.gr_bib_aliases <- list(
  citation = c("citation", "cite", "citation_key", "cite_key"),
  authors  = c("authors", "author"),
  year     = c("year", "date", "published"),
  title    = c("title"),
  venue    = c("venue", "journal", "publication", "source"),
  doi      = c("doi", "url")
)

#' Resolve which columns carry bibliographic identity.
#'
#' `bib` names them outright. Otherwise the conventional names are looked for,
#' which is what makes `gr_protocols("bibliography")` work without configuration.
#' Nothing is guessed from content -- a column called `n` holding "2019" is not
#' a year.
#' @noRd
bib_columns <- function(tab, bib = NULL) {
  out <- list()
  for (role in names(.gr_bib_aliases)) {
    col <- if (!is.null(bib) && !is.null(bib[[role]])) as_chr1(bib[[role]]) else {
      hit <- intersect(tolower(.gr_bib_aliases[[role]]), tolower(names(tab)))
      if (length(hit)) names(tab)[match(hit[1], tolower(names(tab)))] else NULL
    }
    if (!is.null(col) && nzchar(col)) {
      if (!col %in% names(tab)) {
        gr_abort(sprintf("`bib$%s` names the column '%s', which is not in the table.", role, col),
                 class = "gr_bad_bib")
      }
      out[[role]] <- col
    }
  }
  out
}

#' Surnames from a free-text author list, or NULL when it cannot be done safely.
#'
#' Handles the three shapes that actually turn up. Anything else returns the
#' string whole, which renders as one "author" and is visibly odd rather than
#' quietly wrong.
#' @noRd
bib_surnames <- function(x) {
  s <- trimws(as_chr1(x))
  if (!nzchar(s)) return(character(0))
  s <- sub("[,;[:space:]]*(et al\\.?|and others)[.]?$", "", s, ignore.case = TRUE)
  parts <- if (grepl(";", s, fixed = TRUE)) {
    strsplit(s, "[[:space:]]*;[[:space:]]*")[[1]]
  } else if (grepl("^[^,]+,[[:space:]]*[[:alpha:].]{1,4}\\b", s)) {
    # "Smith, J., Okafor, A." -- surname-comma-initials, repeated. Split before
    # each surname rather than on every comma, which would separate the initials
    # from the name they belong to.
    #
    # The lookahead allows a lowercase particle and a multi-word surname, so
    # "van der Berg" and "de la Cruz" survive. Requiring an initial capital
    # dropped them silently, which is the worst way to get a citation wrong:
    # the reference list is short by one author and nothing says so.
    # "Smith, J. and Okafor, A." mixes separators: the last author is joined with
    # "and" rather than a comma, so a comma-only split found one author and
    # dropped the rest. Normalise first, then split once.
    s2 <- gsub("[[:space:]]+(and|&)[[:space:]]+", ", ", s, ignore.case = TRUE, perl = TRUE)
    trimws(strsplit(s2,
      "(?<=[.[:alpha:]]),[[:space:]]+(?=[[:alpha:]][[:alpha:]'\u2019-]*(?:[[:space:]]+[[:alpha:]'\u2019-]+)*,)",
      perl = TRUE)[[1]])
  } else {
    trimws(strsplit(s, "[[:space:]]*(,|&|[[:space:]]and[[:space:]])[[:space:]]*",
                    perl = TRUE)[[1]])
  }
  parts <- trimws(parts)
  parts <- parts[nzchar(parts)]
  if (!length(parts)) return(character(0))
  out <- vapply(parts, function(p) {
    p <- trimws(p)
    if (grepl(",", p, fixed = TRUE)) return(trimws(sub(",.*$", "", p)))   # "Smith, J."
    # "John Smith" -- the surname is the last word that is not an initial.
    w <- strsplit(p, "[[:space:]]+")[[1]]
    w <- w[!grepl("^[[:upper:]]\\.?$", w)]
    if (!length(w)) p else w[length(w)]
  }, character(1), USE.NAMES = FALSE)
  # A string of punctuation parses to an empty "surname", and an empty surname
  # renders as a citation with nobody in it -- "( , 2019)". Nothing is better
  # than that: with no surnames the caller gets no key, and the whole run falls
  # back to markers.
  out[nzchar(trimws(out))]
}

#' The citation key for one row.
#'
#' Two forms, because English prose uses two. A trailing citation is
#' parenthetical -- "(Smith & Okafor, 2019)" -- and one that is the subject of
#' the sentence is narrative -- "Smith and Okafor (2019)". The model writes
#' trailing markers, so parenthetical is what gets rendered; narrative is here
#' because a caller assembling prose by hand needs it and getting it wrong is
#' the most visible way to look like a machine wrote the review.
#'
#' Returns NA when there is not enough to say, which makes the caller fall back
#' to markers for the whole run. A review citing some studies by name and others
#' by number reads as a mistake, so this is all-or-nothing.
#' @noRd
bib_key <- function(row, cols, form = c("parenthetical", "narrative")) {
  form <- match.arg(form)
  if (!is.null(cols$citation)) {
    v <- trimws(as_chr1(row[[cols$citation]]))
    if (nzchar(v) && !identical(v, "NA")) return(v)
  }
  if (is.null(cols$authors)) return(NA_character_)
  sur <- bib_surnames(row[[cols$authors]])
  if (!length(sur)) return(NA_character_)
  join <- if (identical(form, "narrative")) "and" else "&"
  who <- if (length(sur) == 1L) sur[1]
         else if (length(sur) == 2L) paste(sur[1], join, sur[2])
         else paste0(sur[1], " et al.")
  yr <- if (is.null(cols$year)) NA_character_ else trimws(as_chr1(row[[cols$year]]))
  # A year is not optional. "(Smith)" is not a citation anybody can follow up,
  # and inventing "n.d." would assert something the extraction never found.
  if (!nzchar(yr) || identical(yr, "NA")) return(NA_character_)
  if (identical(form, "narrative")) sprintf("%s (%s)", who, yr) else sprintf("%s, %s", who, yr)
}

#' Keys for every row, or NULL when any row lacks one.
#' @noRd
bib_keys <- function(used, cols, form = "parenthetical") {
  if (!length(cols)) return(NULL)
  keys <- vapply(seq_len(nrow(used)),
                 function(i) bib_key(used[i, , drop = FALSE], cols, form), character(1))
  if (anyNA(keys)) return(NULL)
  # Two studies by the same authors in the same year are 2019a and 2019b, which
  # is the convention and also the only way a reader can tell them apart.
  dup <- keys %in% keys[duplicated(keys)]
  if (any(dup)) {
    for (k in unique(keys[dup])) {
      i <- which(keys == k)
      # `sub()` is not vectorised over `replacement` -- it takes the first and
      # warns -- so the obvious one-liner gave every duplicate the suffix "a"
      # and left them identical, which is the fault the suffix exists to fix.
      for (j in seq_along(i)) {
        keys[i[j]] <- sub("([0-9]{3,4})([)]?)$", paste0("\\1", .gr_bib_suffix(j), "\\2"), keys[i[j]])
      }
    }
  }
  keys
}

#' Replace `[study N]` with the rendered citation.
#'
#' Runs AFTER the citation check, so a marker pointing at a row that does not
#' exist has already been reported and is left as it is -- rendering it would
#' hide the fault this pipeline exists to surface.
#' @noRd
render_citations <- function(text, used, keys, style) {
  if (identical(style, "marker") || !nzchar(trimws(as_chr1(text)))) return(text)
  one <- "\\[stud(?:y|ies)[[:space:]]+[0-9]+(?:[[:space:]]*(?:,|and|&)[[:space:]]*[0-9]+)*\\]"
  # Runs of ADJACENT markers collapse into one citation. A model asked for three
  # supporting studies writes "[study 1] [study 2] [study 3]", and rendering each
  # separately gives "(Garcia, 2022) (Lee & Petrov, 2021) (Smith & Okafor, 2019)"
  # -- which no journal would print and which reads as three separate assertions
  # rather than one claim resting on three studies.
  run <- sprintf("%s(?:[[:space:],;]*%s)*", one, one)
  m <- gregexpr(run, text, perl = TRUE, ignore.case = TRUE)
  toks <- regmatches(text, m)[[1]]
  if (!length(toks)) return(text)
  regmatches(text, m) <- list(vapply(toks, function(tok) {
    ids <- as.integer(regmatches(tok, gregexpr("[0-9]+", tok))[[1]])
    hit <- match(ids, used$study)
    # An unknown row was already reported by the citation check. Leaving the
    # marker visible is the point: rendering it would hide the fault.
    if (anyNA(hit)) return(tok)
    if (identical(style, "numeric")) return(paste0("(", paste(ids, collapse = ", "), ")"))
    # Ordered as a reference list is, not as the model happened to write them.
    paste0("(", paste(sort(unique(keys[hit])), collapse = "; "), ")")
  }, character(1), USE.NAMES = FALSE))
  text
}

#' The reference list, from the rows actually cited.
#'
#' Only cited rows. A reference list carrying studies the text never mentions
#' claims a breadth the write-up does not have, and is the easiest kind of
#' padding to produce by accident here, where every row is right there.
#' @noRd
reference_list <- function(used, keys, cited, cols, style) {
  if (!length(cols) || !length(cited)) return(NULL)
  hit <- sort(unique(match(cited, used$study)))
  hit <- hit[!is.na(hit)]
  if (!length(hit)) return(NULL)
  fld <- function(i, role) {
    if (is.null(cols[[role]])) return("")
    v <- trimws(as_chr1(used[[cols[[role]]]][i]))
    if (identical(v, "NA")) "" else v
  }
  entries <- vapply(hit, function(i) {
    # Each part is trimmed of its own trailing punctuation before the parts are
    # joined with ". ". Without it an authors field ending in an initial --
    # "Garcia, R." -- came out as "Garcia, R.. (2022)".
    tidy <- function(x) sub("[[:space:].,;]+$", "", x)
    bits <- c(tidy(fld(i, "authors")),
              if (nzchar(fld(i, "year"))) sprintf("(%s)", tidy(fld(i, "year"))) else "",
              tidy(fld(i, "title")), tidy(fld(i, "venue")), tidy(fld(i, "doi")))
    bits <- bits[nzchar(bits)]
    line <- paste(bits, collapse = ". ")
    if (!nzchar(line)) line <- as_chr1(used$document[i])
    paste0(line, ".")
  }, character(1), USE.NAMES = FALSE)

  # The list is labelled by whatever the prose uses to point into it. Author-year
  # text is followed up alphabetically; text citing `[study 2]` or `(2)` needs a
  # list numbered by study, or the reader has no way to get from the citation to
  # the entry and the list is decoration.
  if (identical(style, "author-year")) {
    entries <- sprintf("- %s", entries[order(tolower(entries))])
  } else {
    entries <- sprintf("%d. %s", used$study[hit], entries)
  }
  entries
}


#' a, b, ... z, aa, ab -- so a twenty-seventh same-year study still gets a label.
#' @noRd
.gr_bib_suffix <- function(j) {
  out <- ""
  while (j > 0L) {
    r <- (j - 1L) %% 26L
    out <- paste0(letters[r + 1L], out)
    j <- (j - 1L) %/% 26L
  }
  out
}
