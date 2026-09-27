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
#' Reads the shapes that actually turn up: "Smith, J., Okafor, A., & Lee, K."
#' and its relatives, "Smith JA, Okafor AB" (Vancouver, which is how PubMed
#' exports), "John Smith and Aisha Okafor", any of those separated by
#' semicolons, and a corporate author such as "World Health Organization", which
#' is one name and is kept whole.
#'
#' Anything it cannot read confidently comes back NULL, never as a best guess:
#' bib_key() then has no key and the whole run falls back to markers. The best
#' guesses were the fault. "Smith JA, Okafor AB" gave the surnames "JA" and
#' "AB"; the ", &" before an APA list's last author left an empty slot that
#' swallowed that author; "John Smith, Mary Jones" was read as surname-comma-
#' initials because "Mary" is four letters. Each came out as a rendered
#' citation, which this file promises is a fact about the extraction.
#'
#' A list ending "et al." names fewer authors than the paper has, so the result
#' carries `attr(, "et_al") = TRUE` and bib_key() cites it as "Smith et al."
#' however many names are left. Counting only the names given cited "Smith et
#' al." as "(Smith, 2020)" and "Smith J, Okafor A, et al." as a two-author paper.
#' @noRd
bib_surnames <- function(x) {
  # to_utf8() first, and Unicode classes rather than [[:upper:]] below. An
  # unlabelled string in a non-UTF-8 session was matched byte by byte, so "Å"
  # was no capital: "Åberg Å, Lindqvist P" came out as "(Åberg Å, 2019)", one
  # author dropped and an initial printed as a surname.
  s <- trimws(to_utf8(as_chr1(x)))
  if (!nzchar(s)) return(character(0))
  named <- sub(.gr_bib_et_al_re, "", s, ignore.case = TRUE)
  sur <- bib_surnames_read(named)
  if (length(sur) && !identical(named, s)) attr(sur, "et_al") <- TRUE
  sur
}

#' bib_surnames() once any "et al." is off the end. `byline` carries a
#' byline's reading into the people before a group author.
#' @noRd
bib_surnames_read <- function(s, byline = FALSE) {
  s <- sub("[,;&[:space:]]+$", "", s)
  # Degrees follow a name in a byline ("John Smith, MD; Aisha Okafor, PhD") and
  # were read as its given names, so the whole name was printed as a surname.
  # Removed only once a degree nothing else could be says this is a byline:
  # "MD" alone is also Vancouver for the initials M. D., as in "Garcia Lopez MD".
  # A byline writes given names first, so what is left is read that way too:
  # "Surname, Given" is not a byline's form, and reading "Smith MS, Okafor PhD"
  # by it made one author, Smith, with the given name Okafor.
  if (grepl(.gr_bib_degree_re[["anchor"]], s, perl = TRUE)) {
    byline <- TRUE
    s <- bib_strip_degrees(s)
  }
  # A string of punctuation has nobody in it, and an empty surname renders as
  # "( , 2019)". With no surnames the caller gets no key, and the run falls back
  # to markers.
  if (!grepl("\\p{L}", s, perl = TRUE)) return(character(0))
  # One organisation is one author, and the "and" in "Centers for Disease
  # Control and Prevention" is not a separator. Mixed into a list of people it
  # cannot be told apart from them, so that is left unread -- except as the
  # last author after two or more people ("Smith, J., Okafor, A., Lee, K., &
  # the ABC Study Group"), where the key needs only the first surname and
  # "et al.", whatever the group is called.
  if (grepl(.gr_bib_corporate, s, perl = TRUE)) {
    if (!grepl("[,;]", s)) return(s)
    m <- regmatches(s, regexec("^(.*)(?:;|,?[[:space:]]*(?:&|\\band\\b)|,)[[:space:]]*([^,;&]+)$",
                               s, perl = TRUE))[[1]]
    # "for the EPIC Investigators" is on whose behalf the people wrote, not
    # another author, so it does not make two people "et al.".
    if (length(m) != 3L || !grepl(.gr_bib_corporate, m[3], perl = TRUE) ||
        grepl("^(for|on behalf of)\\b", m[3], ignore.case = TRUE, perl = TRUE) ||
        grepl(.gr_bib_corporate, m[2], perl = TRUE)) return(NULL)
    people <- bib_surnames_read(sub(.gr_bib_et_al_re, "", m[2], ignore.case = TRUE), byline)
    if (length(people) < 2L) return(NULL)
    attr(people, "et_al") <- TRUE
    return(people)
  }
  # So is a field that is one run of capitals and nothing else: WHO, CDC, NICE,
  # OECD. The initials guard below exists for the "JA" of "Smith JA", and read
  # a lone acronym as initials too, which sent the whole review to markers.
  if (grepl("^\\p{Lu}{2,}\\.?$", s, perl = TRUE)) return(sub(".", "", s, fixed = TRUE))

  # ", &" and ", and" are ONE separator. Turning "&" into ", " and then
  # splitting on commas left an empty slot before the last author, and the
  # surname-comma-initials split then lost that author.
  and_sep <- "[[:space:]]*,?[[:space:]]*(?:&|\\band\\b)[[:space:]]*"
  pieces <- function(v) {
    v <- trimws(v)
    v[grepl("\\p{L}", v, perl = TRUE) & !grepl(.gr_bib_suffix_re, v)]
  }
  sur <- if (grepl(";", s, fixed = TRUE)) {
    entries <- pieces(unlist(strsplit(strsplit(s, ";", fixed = TRUE)[[1]], and_sep, perl = TRUE)))
    vapply(entries, function(e) {
      bits <- pieces(strsplit(e, ",", fixed = TRUE)[[1]])
      if (length(bits) == 1L) return(bib_one_name(bits))
      # "Smith, J." is one surname and its given names. More commas than that
      # is more than one author in one slot, which is not a list anyone meant.
      if (length(bits) == 2L && !bib_has_initial(bits[1]) &&
          (!byline || bib_is_initials(bits[2]))) bits[1] else NA_character_
    }, character(1), USE.NAMES = FALSE)
  } else {
    chunks <- pieces(strsplit(s, and_sep, perl = TRUE)[[1]])
    per <- lapply(chunks, function(ch) pieces(strsplit(ch, ",", fixed = TRUE)[[1]]))
    toks <- unlist(per, use.names = FALSE)
    n <- length(toks)
    if (!n) return(character(0))
    ini <- vapply(toks, bib_is_initials, logical(1), USE.NAMES = FALSE)
    odd <- toks[seq(1L, n, by = 2L)]
    even <- if (n > 1L) toks[seq(2L, n, by = 2L)] else character(0)
    # Surname, given names, surname, given names. Two things make that reading
    # safe: the surnames carry no initials of their own ("Smith J" is a whole
    # Vancouver name, not a surname), and either some given name is plainly
    # initials or every author sits alone between the "and"s, as in "Smith,
    # John and Okafor, Aisha". "Smith, Jones, Lee" has neither.
    paired <- n >= 2L && n %% 2L == 0L &&
      !any(vapply(odd, bib_has_initial, logical(1))) &&
      all(vapply(even, if (byline) bib_is_initials else bib_is_given, logical(1))) &&
      (any(ini) || all(lengths(per) == 2L))
    if (paired) {
      odd
    } else if (any(ini) ||
               (!byline && n > length(chunks) && any(!grepl("[[:space:]]", toks)))) {
      # Initials that do not pair with a surname mean the separators and the
      # authors disagree. Bare single words between commas are either surnames
      # or surname-and-given-name pairs, and nothing here says which.
      NA_character_
    } else {
      vapply(toks, bib_one_name, character(1), USE.NAMES = FALSE)
    }
  }
  if (!length(sur) || anyNA(sur)) return(NULL)
  sur <- trimws(sur)
  # A surname that is itself initials, or still carries one, was read from the
  # wrong end of a name -- the "JA" of "Smith JA", the "Lee K" of a list split
  # in the wrong places.
  if (any(vapply(sur, function(w) bib_is_initials(w) || bib_has_initial(w), logical(1)))) {
    return(NULL)
  }
  unname(sur)
}

#' Words that make an author field an organisation rather than a person.
#' @noRd
.gr_bib_corporate <- paste0(
  "\\b(Organi[sz]ations?|Associations?|Institutes?|Society|Committee|Council|Agency|",
  "Department|Ministry|Foundation|Collaborat(ion|ive)|Consortium|University|",
  "Cent(re|er)s?|Commission|Federation|Bureau|Administration|Investigators|Group|",
  "Network|Academy|Alliance|Coalition|Initiative|Task Force|Taskforce)\\b")

#' "et al." or "and others" closing an author list.
#' @noRd
.gr_bib_et_al_re <- "[,;[:space:]]*(et al\\.?|and others)[.]?$"

#' A generational suffix, which is part of no surname and names no author.
#' @noRd
.gr_bib_suffix_re <- "^(Jr|Sr|II|III|IV)\\.?$"

#' Degrees and post-nominals, which follow a name in a byline and are no name.
#'
#' `anchor` holds the ones nothing else could be: a lowercase letter in the
#' middle, or more capitals than a list of initials has. One of them anywhere
#' says the field is a byline, with every name written given names first.
#' `initials` holds the ones that are also plausible initials -- "Smith MD" is
#' Vancouver for M. D. Smith -- and they are removed only from a field an anchor
#' has already shown to be a byline. Matched with or without their full stops
#' ("Ph.D."), and case-sensitively, so the surname "Ma" is not the degree "MA".
#' @noRd
.gr_bib_degrees <- list(
  anchor = c("PhD", "DPhil", "MPhil", "MSc", "BSc", "MBChB", "MBBCh", "BChir", "ScD",
             "DSc", "DrPH", "PharmD", "PsyD", "EdD", "MEd", "MRes", "MSt", "MMed",
             "MMedSci", "BMedSci", "FMedSci", "FRCPath", "FRCPsych", "MRCPsych",
             "FRCOphth", "DClinPsy", "FRCPC", "FRCPE", "FRCPI", "FRCSC", "FRCSE", "FRACP",
             "FRCOG", "MRCOG", "MRCGP", "FRCGP", "FACEP", "FAAFP", "FACOG", "FANZCA",
             "FRANZCP"),
  initials = c("MD", "DO", "RN", "BN", "MA", "MS", "BA", "BS", "BM", "PA", "NP", "PT",
               "OT", "RD", "PHD", "MPH", "MHS", "MPA", "MBA", "MSN", "BSN", "DNP", "DDS",
               "DMD", "DVM", "CPH", "LPN", "CNM", "FNP", "MSW", "MSPH", "MBBS", "FRCP",
               "FRCS", "FRCR", "FACP", "FACS", "FAAP", "FACC", "FAHA", "FESC", "MRCP",
               "MRCS", "FFPH", "APRN", "CRNA", "LCSW"))

#' A regex for a whole degree, full stops optional, as a word of its own.
#' @noRd
bib_degree_pattern <- function(tokens) {
  alt <- vapply(strsplit(tokens, ""), function(ch) paste0(ch, "\\.?", collapse = ""),
                character(1))
  sprintf("(?<![\\p{L}.'-])(?:%s)(?![\\p{L}'-])", paste(alt, collapse = "|"))
}

#' @noRd
.gr_bib_degree_re <- list(
  anchor = bib_degree_pattern(.gr_bib_degrees$anchor),
  any = bib_degree_pattern(unlist(.gr_bib_degrees, use.names = FALSE)))

#' A byline with its degrees taken out, and the separators they leave tidied,
#' so "John Smith, MD, and Aisha Okafor, PhD" reads as "John Smith, and Aisha
#' Okafor".
#' @noRd
bib_strip_degrees <- function(s) {
  s <- gsub(.gr_bib_degree_re[["any"]], "", s, perl = TRUE)
  s <- gsub("[[:space:]]+", " ", s)
  s <- gsub("[[:space:]]*,([[:space:]]*,)+", ",", s)
  s <- gsub("[[:space:]]*,[[:space:]]*(;|$)", "\\1", s)
  s <- gsub("[[:space:]]+([,;])", "\\1", s)
  sub("[,;&[:space:]]+$", "", trimws(s))
}

#' "J.", "JA", "J. A.", "M.-C.": initials and nothing else.
#'
#' Up to four letters, all capitals. A capitalised surname always has a
#' lowercase letter in it, so this is what tells "Smith JA" from "Wu Li".
#' Unicode classes, so "Å" is a capital whatever the session's locale.
#' @noRd
bib_is_initials <- function(w) {
  w <- trimws(w)
  grepl("^[\\p{Lu}.[:space:]-]+$", w, perl = TRUE) &&
    nchar(gsub("[^\\p{Lu}]", "", w, perl = TRUE)) %in% 1:4
}

#' Whether a name has an initial among its words.
#' @noRd
bib_has_initial <- function(x) {
  any(vapply(strsplit(trimws(x), "[[:space:]]+")[[1]], bib_is_initials, logical(1)))
}

#' Given names in "Surname, Given" -- initials, or one capitalised name that
#' may be followed by initials ("John", "John A.", "Mary-Kate").
#' @noRd
bib_is_given <- function(x) {
  bib_is_initials(x) || grepl(.gr_bib_given_re, trimws(x), perl = TRUE)
}

#' The typographic apostrophe by code point, not as a "\\u" escape, which is
#' left unlabelled when parsed in a non-UTF-8 locale (see .gr_bib_fold).
#' @noRd
.gr_bib_given_re <- paste0("^\\p{Lu}[\\p{L}'", intToUtf8(0x2019),
                           "-]*([[:space:]]+\\p{Lu}{1,3}\\.?)*$")

#' The surname in one author's name written without a comma.
#'
#' Vancouver puts the initials after the surname ("Smith JA", "van der Berg P"),
#' and everything before them is the surname. Otherwise the given names and
#' initials come first ("John Smith", "J. A. Smith") and the surname is the last
#' word. Initials on both sides is neither, and gives NA.
#' @noRd
bib_one_name <- function(e) {
  w <- strsplit(trimws(e), "[[:space:]]+")[[1]]
  w <- w[nzchar(w) & !grepl(.gr_bib_suffix_re, w)]
  if (!length(w)) return(NA_character_)
  ini <- vapply(w, bib_is_initials, logical(1), USE.NAMES = FALSE)
  if (all(ini)) return(NA_character_)
  if (length(w) > 1L && ini[length(w)]) {
    k <- max(which(!ini))
    if (any(ini[seq_len(k)])) return(NA_character_)
    return(paste(w[seq_len(k)], collapse = " "))
  }
  w[length(w)]
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
  # A list that ended "et al." has more authors than it names.
  who <- if (isTRUE(attr(sur, "et_al"))) paste0(sur[1], " et al.")
         else if (length(sur) == 1L) sur[1]
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
      # Lettered in title order, as the convention has it, and row order only
      # between equal titles. By row alone, the letter a paper got followed
      # whatever order its table's rows arrived in -- which was the locale's
      # collation of the file names -- so "2019a" named a different paper on
      # another machine.
      if (!is.null(cols$title)) i <- i[bib_order(as.character(used[[cols$title]][i]))]
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
#' hide the fault this pipeline exists to surface. `leave` names rows that do
#' exist but that the check reported all the same (a study the section was not
#' given), and their markers are left for the same reason.
#' @noRd
render_citations <- function(text, used, keys, style, leave = integer(0)) {
  if (identical(style, "marker") || !nzchar(trimws(as_chr1(text)))) return(text)
  # cite_grammar(), not a second copy: the checker and the renderer disagreeing
  # about what a citation looks like is how a marker got rendered into published
  # prose that the check had reported as citing nothing. Lists and ranges, read
  # marker by marker with cite_marker_ids(), so "[studies 1-3]" renders as the
  # three studies the check counted rather than staying a bare marker beside
  # rendered prose. Not a marker with a locator ("[study 3 p. 4]"): rendering
  # it would drop the page. It stays as written, and is checked all the same.
  one <- paste0(cite_grammar("study")$ids, "\\]")
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
    # Per marker, never every number in the run: read that way "[studies 1-7]"
    # was studies 1 and 7.
    markers <- regmatches(tok, gregexpr(one, tok, perl = TRUE, ignore.case = TRUE))[[1]]
    ids <- unlist(lapply(markers, cite_marker_ids, word = "study"), use.names = FALSE)
    hit <- match(ids, used$study)
    # An unknown row was already reported by the citation check. Leaving the
    # marker visible is the point: rendering it would hide the fault.
    if (!length(ids) || anyNA(hit) || any(ids %in% leave)) return(tok)
    if (identical(style, "numeric")) return(paste0("(", paste(ids, collapse = ", "), ")"))
    # Ordered as a reference list is, not as the model happened to write them,
    # and by the same key, so the two agree on every machine.
    k <- unique(keys[hit])
    paste0("(", paste(k[bib_order(k)], collapse = "; "), ")")
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
  # The a/b suffix bib_keys() assigned. `keys` was in this signature and unused,
  # so the prose said "(Smith & Okafor, 2019a)" and "(Smith & Okafor, 2019b)"
  # against two identical reference entries -- neither citation resolvable,
  # which is the one thing a reference list has to do.
  suffix <- function(i) {
    if (is.null(keys) || i > length(keys) || is.na(keys[i])) return("")
    m <- regmatches(keys[i], regexpr("[0-9]{3,4}[a-z]+", keys[i]))
    if (!length(m)) "" else sub("^[0-9]{3,4}", "", m)
  }
  entries <- vapply(hit, function(i) {
    # Each part is trimmed of its own trailing punctuation before the parts are
    # joined with ". ". Without it an authors field ending in an initial --
    # "Garcia, R." -- came out as "Garcia, R.. (2022)".
    tidy <- function(x) sub("[[:space:].,;]+$", "", x)
    # "Smith, J.; Okafor, A." is how records.R normalises an author list
    # internally, because RIS gives one author per tag. A reference list is not
    # the place for that convention -- it is an artifact of the parser, and
    # printing it makes the output look machine-made.
    authors <- gsub(";[[:space:]]*", ", ", tidy(fld(i, "authors")))
    bits <- c(authors,
              if (nzchar(fld(i, "year"))) sprintf("(%s%s)", tidy(fld(i, "year")), suffix(i)) else "",
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
    entries <- sprintf("- %s", entries[bib_order(entries)])
  } else {
    entries <- sprintf("%d. %s", used$study[hit], entries)
  }
  entries
}


#' Accented Latin letters, by the letters they sort as.
#'
#' Code points, turned into text by intToUtf8(), rather than literals or
#' "\\u" escapes: an escape parsed in a non-UTF-8 locale is left unlabelled,
#' and a pattern built from it then fails to match, or fails to compile.
#'
#' Every precomposed Latin letter that is a base letter with a mark: Latin-1,
#' Latin Extended-A and -B (pinyin tones, the horn letters of Vietnamese, the
#' comma-below S and T of Romanian), Latin Extended Additional (the rest of
#' Vietnamese, and the dot-below letters of Yoruba and transliterated Arabic),
#' and the few IPA letters that African and Azerbaijani alphabets write names
#' with. Covering only Latin-1 and Extended-A filed "Tran" and "Trinh" before
#' the Vietnamese "Tran" with a circumflex and grave, which en_US collation
#' files between them. `drop` is what the key ignores: an accent typed as a
#' separate combining mark, and the typographic apostrophe.
#' @noRd
.gr_bib_fold <- list(
  a = c(0xC0:0xC5, 0xE0:0xE5, 0x100:0x105, 0x1CD:0x1CE, 0x1DE:0x1E1, 0x1FA:0x1FB,
        0x200:0x203, 0x226:0x227, 0x23A, 0x1E00:0x1E01, 0x1E9A, 0x1EA0:0x1EB7),
  ae = c(0xC6, 0xE6, 0x1E2:0x1E3, 0x1FC:0x1FD),
  b = c(0x180:0x183, 0x243, 0x253, 0x1E02:0x1E07),
  c = c(0xC7, 0xE7, 0x106:0x10D, 0x187:0x188, 0x23B:0x23C, 0x1E08:0x1E09),
  d = c(0xD0, 0xF0, 0x10E:0x111, 0x189:0x18C, 0x221, 0x256:0x257, 0x1E0A:0x1E13),
  dz = c(0x1C4:0x1C6, 0x1F1:0x1F3),
  e = c(0xC8:0xCB, 0xE8:0xEB, 0x112:0x11B, 0x18E:0x190, 0x1DD, 0x204:0x207, 0x228:0x229,
        0x246:0x247, 0x259, 0x25B, 0x1E14:0x1E1D, 0x1EB8:0x1EC7),
  f = c(0x191:0x192, 0x1E1E:0x1E1F),
  g = c(0x11C:0x123, 0x193, 0x1E4:0x1E7, 0x1F4:0x1F5, 0x260, 0x1E20:0x1E21),
  h = c(0x124:0x127, 0x21E:0x21F, 0x1E22:0x1E2B, 0x1E96),
  hv = c(0x195, 0x1F6),
  i = c(0xCC:0xCF, 0xEC:0xEF, 0x128:0x131, 0x197, 0x1CF:0x1D0, 0x208:0x20B, 0x268,
        0x1E2C:0x1E2F, 0x1EC8:0x1ECB),
  ij = 0x132:0x133,
  j = c(0x134:0x135, 0x1F0, 0x237, 0x248:0x249),
  k = c(0x136:0x138, 0x198:0x199, 0x1E8:0x1E9, 0x1E30:0x1E35),
  l = c(0x139:0x142, 0x19A, 0x234, 0x23D, 0x1E36:0x1E3D),
  lj = 0x1C7:0x1C9, ll = 0x1EFA:0x1EFB,
  m = c(0x19C, 0x1E3E:0x1E43),
  n = c(0xD1, 0xF1, 0x143:0x14B, 0x19D:0x19E, 0x1F8:0x1F9, 0x220, 0x235, 0x272,
        0x1E44:0x1E4B),
  nj = 0x1CA:0x1CC,
  o = c(0xD2:0xD6, 0xD8, 0xF2:0xF6, 0xF8, 0x14C:0x151, 0x186, 0x19F:0x1A1, 0x1D1:0x1D2,
        0x1EA:0x1ED, 0x1FE:0x1FF, 0x20C:0x20F, 0x22A:0x231, 0x254, 0x1E4C:0x1E53,
        0x1ECC:0x1EE3),
  oe = 0x152:0x153, oi = 0x1A2:0x1A3, ou = 0x222:0x223,
  p = c(0x1A4:0x1A5, 0x1E54:0x1E57),
  q = 0x24A:0x24B,
  r = c(0x154:0x159, 0x210:0x213, 0x24C:0x24D, 0x1E58:0x1E5F),
  s = c(0x15A:0x161, 0x17F, 0x218:0x219, 0x23F, 0x1E60:0x1E69, 0x1E9B:0x1E9D),
  ss = c(0xDF, 0x1E9E),
  t = c(0x162:0x167, 0x1AB:0x1AE, 0x21A:0x21B, 0x236, 0x23E, 0x1E6A:0x1E71, 0x1E97),
  th = c(0xDE, 0xFE),
  u = c(0xD9:0xDC, 0xF9:0xFC, 0x168:0x173, 0x1AF:0x1B0, 0x1D3:0x1DC, 0x214:0x217, 0x244,
        0x289, 0x1E72:0x1E7B, 0x1EE4:0x1EF1),
  v = c(0x1B2, 0x245, 0x28B, 0x1E7C:0x1E7F, 0x1EFC:0x1EFD),
  w = c(0x174:0x175, 0x1F7, 0x1E80:0x1E89, 0x1E98),
  x = 0x1E8A:0x1E8D,
  y = c(0xDD, 0xFD, 0xFF, 0x176:0x178, 0x1B3:0x1B4, 0x232:0x233, 0x24E:0x24F,
        0x1E8E:0x1E8F, 0x1E99, 0x1EF2:0x1EF9, 0x1EFE:0x1EFF),
  z = c(0x179:0x17E, 0x1B5:0x1B6, 0x224:0x225, 0x240, 0x1E90:0x1E95),
  drop = c(0x300:0x36F, 0x27, 0x2019))

#' The key a reference list is alphabetised by.
#'
#' The same on every machine. order(tolower()) and sort() collate by the
#' session's locale, so one table printed its references in one order under
#' en_US and another under C -- where an accented name went to one end of the
#' list and "de la Cruz" came after "Zhou" -- and its in-text citations too.
#'
#' The choice made here: an accented letter files with its base letter
#' (Ozturk with an umlaut under O, Lukasz with a stroke under L), case is
#' ignored ("de la Cruz" under D), and an apostrophe is ignored ("O'Brien" as
#' "OBrien"), which is how a reader looking a name up expects to find it and
#' how en_US collation files it. Only ASCII letters are case-folded, by rule
#' rather than by the locale; anything left after the folding (another
#' script) sorts after the Latin letters, by its UTF-8 bytes.
#' @noRd
bib_sort_key <- function(x) {
  # to_utf8() first: the regexes below run in UTF-8, and an unlabelled string
  # in a non-UTF-8 locale would make them throw rather than match.
  k <- to_utf8(x)
  for (to in names(.gr_bib_fold)) {
    k <- gsub(sprintf("[%s]", intToUtf8(.gr_bib_fold[[to]])),
              if (identical(to, "drop")) "" else to, k, perl = TRUE)
  }
  gsub("([A-Z]+)", "\\L\\1", k, perl = TRUE)
}

#' A locale-independent order for bibliographic strings.
#'
#' By bib_sort_key(), then by the strings' own UTF-8 bytes so that two names
#' equal under the key ("Muller" and the umlauted one) still come out in one
#' fixed order. Radix, because it is the one method that does not collate by
#' the locale.
#' @noRd
bib_order <- function(x) {
  order(bib_sort_key(x), to_utf8(x), method = "radix")
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
