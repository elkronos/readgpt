# extract-fields.R -- a goal expressed as fields rather than as a sentence.
#
# WHY THIS FILE EXISTS
# "Read this paper and tell me about it" is a worse instruction than "extract the
# design, the number randomised, the primary outcome and the effect size", and
# not because the model minds vagueness. The difference is in the OUTPUT: a
# paragraph about one paper cannot be joined to a paragraph about two hundred
# others, and a table can.
#
# So the useful version of "make it goal-driven" is not a fuzzier prompt. It is
# a typed schema: a goal is a set of fields, extraction fills them, and the
# result is a data frame you can sort, count, filter and publish. Everything a
# corpus-scale reading task actually needs -- screening, evidence tables,
# synthesis -- is built on that shape.
#
# Two things the types buy beyond tidiness. A field that was LOOKED FOR AND NOT
# REPORTED is distinguishable from one the run failed to reach, which is the
# same distinction `is_not_found()` draws for answers and matters just as much
# here: "not reported" is a finding. And each filled field carries the chunk and
# the quoted span it came from, so `gr_verify_evidence()` applies to an
# extraction table exactly as it does to an answer.

.gr_field_types <- c("string", "integer", "number", "boolean", "enum")

#' Describe one field of an extraction schema
#'
#' @param description What to look for, in the words you would use to a research
#'   assistant. This is the whole instruction the model gets for this field, so
#'   "Number of participants randomised, not the number analysed" earns its
#'   length.
#' @param type One of `"string"`, `"integer"`, `"number"`, `"boolean"` or
#'   `"enum"`. An `"integer"` value above `.Machine$integer.max` is kept as a
#'   whole double, so that column is a double whenever one such value is
#'   present. A value that carries more than one number, such as "120 (60 per
#'   arm)", is recorded as missing rather than run together, and so is one
#'   that reads more than one way: a scale word or suffix ("3 million",
#'   "12k"), a comma that is neither between groups of three digits nor after
#'   a lone zero ("1,5"; "1,204" and "0,85" are read), a fraction in an
#'   integer field, and a boolean that is not a plain yes or no ("unclear").
#'   [gr_extract()] counts such a value in `n_unverified` rather than reporting
#'   the field as not reported. Digits of other scripts (Arabic-Indic,
#'   Persian, full-width) and a typeset minus sign are read as they are meant.
#' @param values For `type = "enum"`, the permitted values. A declared value
#'   is kept as written, including one such as `"null"` or `"none"` that would
#'   otherwise read as missing.
#' @return A `gr_field`.
#' @seealso [gr_fields()], [gr_extract()]
#' @export
#' @examples
#' gr_field("Number of participants randomised", type = "integer")
#' gr_field("Overall direction of the result", type = "enum",
#'          values = c("positive", "null", "mixed", "negative"))
gr_field <- function(description, type = "string", values = NULL) {
  type <- match.arg(as_chr1(type), .gr_field_types)
  if (identical(type, "enum") && !length(values)) {
    gr_abort("A field of type 'enum' needs `values`.", class = "gr_bad_field")
  }
  if (!is_nonblank(description)) {
    gr_abort(paste0("Every field needs a `description`. It is the entire instruction the model ",
                    "gets for that field, so a bare name is not enough."),
             class = "gr_bad_field")
  }
  # Labelled UTF-8, as a message is. Under a non-UTF-8 locale a description or
  # value typed in a script is UTF-8 bytes marked "unknown": the schema then
  # went out with "r<c3><a9>duit" as the permitted value, and a reply that
  # gave the value as declared did not match it and was recorded as unreadable.
  structure(list(description = mark_utf8(as_chr1(description)), type = type,
                 values = if (length(values)) mark_utf8(as.character(values)) else NULL),
            class = "gr_field")
}

#' @export
print.gr_field <- function(x, ...) {
  cat(sprintf("<gr_field %s%s> %s\n", x$type,
              if (length(x$values)) sprintf("(%s)", paste(x$values, collapse = "/")) else "",
              substr(x$description, 1, 90)))
  invisible(x)
}

#' Build an extraction schema
#'
#' A goal, expressed as the fields you want filled. Each argument is either a
#' description (a string field) or a [gr_field()].
#'
#' @param ... Named fields. A bare string is shorthand for
#'   `gr_field(string, type = "string")`.
#' @return A `gr_fields` object.
#'
#' @section Naming the fields:
#' The names become column names, so keep them short and syntactic. The
#' *descriptions* carry the instruction, and they are worth writing carefully:
#' most extraction disagreements come from an ambiguous field description rather
#' than from the model.
#'
#' @seealso [gr_field()], [gr_extract()], [gr_protocol()]
#' @export
#' @examples
#' fields <- gr_fields(
#'   design  = "The study design, e.g. randomised controlled trial, cohort, case series",
#'   n       = gr_field("Number of participants randomised, not the number analysed",
#'                      type = "integer"),
#'   outcome = gr_field("Direction of the primary result",
#'                      type = "enum", values = c("positive", "null", "mixed")),
#'   funded  = gr_field("Whether industry funding is declared", type = "boolean")
#' )
#' fields
#' names(fields)
gr_fields <- function(...) {
  spec <- list(...)
  if (!length(spec)) gr_abort("`gr_fields()` needs at least one field.", class = "gr_bad_field")
  if (is.null(names(spec)) || any(!nzchar(names(spec)))) {
    gr_abort("Every field must be named; the names become column names.", class = "gr_bad_field")
  }
  if (anyDuplicated(names(spec))) {
    gr_abort(sprintf("Duplicate field name(s): %s.",
                     paste(unique(names(spec)[duplicated(names(spec))]), collapse = ", ")),
             class = "gr_bad_field")
  }
  check_field_names(names(spec))
  out <- lapply(spec, function(f) if (inherits(f, "gr_field")) f else gr_field(f))
  structure(out, class = "gr_fields")
}

# Names the extraction table uses for its own bookkeeping. A field called
# `status` would be silently overwritten by the run status, which is the sort of
# thing you discover after the run rather than before it.
.gr_reserved_fields <- c("document", "document_id", "status", "duplicate_of",
                         "error", "n_filled", "n_unverified", "conflicts",
                         "field", "chunk_id", "page", "section", "quote",
                         "verified", "match")

#' Reject a field name before it becomes a broken column.
#'
#' Three separate collisions, all silent if unchecked. A name that is not a
#' syntactic R name comes back from `data.frame()` renamed, so the column the
#' user asked for is not the column they get. A name ending `__quote` collides
#' with the companion span `fields_schema()` adds for every field. And a name the
#' extraction table already uses for bookkeeping is simply overwritten.
#' @noRd
check_field_names <- function(nms) {
  bad <- nms[!grepl("^[A-Za-z][A-Za-z0-9_.]*$", nms)]
  if (length(bad)) {
    gr_abort(sprintf(paste0("Field name(s) %s cannot be used: a name becomes a JSON key and a ",
                            "column name, so it must start with a letter and contain only ",
                            "letters, digits, '.' and '_'."),
                     paste(sQuote(bad, q = FALSE), collapse = ", ")), class = "gr_bad_field")
  }
  q <- nms[grepl("__quote$", nms)]
  if (length(q)) {
    gr_abort(sprintf(paste0("Field name(s) %s cannot end in '__quote': every field gets a ",
                            "'<name>__quote' companion holding the sentence it came from, and ",
                            "the two would collide."),
                     paste(sQuote(q, q = FALSE), collapse = ", ")), class = "gr_bad_field")
  }
  r <- intersect(nms, .gr_reserved_fields)
  if (length(r)) {
    gr_abort(sprintf(paste0("Field name(s) %s are reserved: the extraction table uses them for ",
                            "%s. Rename the field."),
                     paste(sQuote(r, q = FALSE), collapse = ", "),
                     paste(.gr_reserved_fields, collapse = ", ")), class = "gr_bad_field")
  }
  invisible(nms)
}

#' @export
print.gr_fields <- function(x, ...) {
  cat(sprintf("<gr_fields> %d field(s)\n", length(x)))
  for (nm in names(x)) {
    cat(sprintf("  %-14s %-9s %s\n", nm, x[[nm]]$type, substr(x[[nm]]$description, 1, 60)))
  }
  invisible(x)
}

#' The JSON schema for one extraction pass.
#'
#' Every field is nullable, and that is the point: a chunk that does not mention
#' the sample size must be able to say so, rather than being pushed into
#' inventing one. Reconciliation then treats null as "this chunk had nothing",
#' which is a different thing from "the document does not report it" -- the
#' second is only known once every chunk has been asked.
#' @noRd
fields_schema <- function(fields) {
  props <- lapply(fields, function(f) {
    base <- switch(f$type,
                   string  = list(type = c("string", "null")),
                   integer = list(type = c("integer", "null")),
                   number  = list(type = c("number", "null")),
                   boolean = list(type = c("boolean", "null")),
                   enum    = list(type = c("string", "null"), enum = as.list(c(f$values, NA))))
    base$description <- f$description
    base
  })
  # A quoted span per field, so a filled value can be checked against the chunk
  # it came from -- the same guarantee gr_verify_evidence() gives an answer.
  quotes <- lapply(fields, function(f) list(type = c("string", "null"),
                                            description = "The exact sentence this value came from, copied verbatim, or null."))
  names(quotes) <- paste0(names(fields), "__quote")
  list(type = "object", additionalProperties = FALSE,
       required = as.list(c(names(props), names(quotes))),
       properties = c(props, quotes))
}

#' @noRd
fields_prompt <- function(fields) {
  lines <- vapply(names(fields), function(nm) {
    f <- fields[[nm]]
    sprintf("- %s (%s%s): %s", nm, f$type,
            if (length(f$values)) sprintf("; one of %s", paste(f$values, collapse = ", ")) else "",
            f$description)
  }, character(1), USE.NAMES = FALSE)
  paste(lines, collapse = "\n")
}

#' @noRd
empty_record <- function(fields) {
  stats::setNames(vector("list", length(fields)), names(fields))
}

#' Words a model writes in place of a value when it has none.
#'
#' "not reported" is not among them because it is never a value, whatever
#' quote comes with it; coerce_field() drops it for every type.
#' @noRd
.gr_placeholder_words <- c("null", "na", "n/a", "none")

#' Ways of saying the document does not report a field, which a model writes
#' where a boolean or a number should be. Not a value, and not a value that
#' could not be read either (see unreadable_value()).
#' @noRd
.gr_unreported_words <- c("not reported", "not stated", "not specified", "not mentioned",
                          "not given", "not provided", "not available", "not applicable")

#' The words a boolean field reads as TRUE and as FALSE. Anything else is not
#' an answer to a yes-or-no question.
#' @noRd
.gr_true_words <- c("true", "yes", "y", "1")
.gr_false_words <- c("false", "no", "n", "0")

#' Is `v` one string that is one of `words`, whatever its case and the space
#' around it?
#'
#' Matched without case rather than lower-cased: this runs for every value
#' every chunk gives and again for every stored value a table is built from,
#' and lower_text() reads its whole table of letters on each call. The words
#' are plain ASCII (no pattern characters), which a caseless match treats
#' alike in every locale.
#' @noRd
is_one_of_words <- function(v, words) {
  is.character(v) && length(v) == 1L && !is.na(v) &&
    grepl(paste0("^\\s*(?:", paste(words, collapse = "|"), ")\\s*$"), v,
          ignore.case = TRUE, perl = TRUE)
}

#' @noRd
is_placeholder_value <- function(v) is_one_of_words(v, .gr_placeholder_words)

#' Coerce one extracted value to the type its field declares.
#'
#' A model asked for an integer will sometimes return "1,204" or "about 1200".
#' Coercing here rather than at the point of use means the table has the type it
#' promises; a value that cannot be coerced becomes NA, and reconcile_fields()
#' reports it as one that could not be read (unreadable_value()) rather than
#' silently poisoning a numeric column with text.
#' @noRd
coerce_field <- function(value, field) {
  # A field sent as [] or {} parses to a zero-length list, and value[[1]] of it
  # threw "subscript out of bounds" after every chunk had been paid for: the
  # whole document failed, with every value its other chunks gave, and failed
  # the same way on every retry. One level deeper ([[]], [{}]) is no value
  # either.
  if (!length(value) || (length(value) == 1L && is.na(value))) return(NULL)
  v <- value[[1]]
  if (is.list(v) || length(v) != 1L || is.na(v)) return(NULL)
  if (is.character(v) && !nzchar(trimws(v))) return(NULL)
  # A declared enum value is a value, whatever it spells, so membership is
  # decided before any word is read as "missing". The documented schemas use
  # values = c("positive", "null", "mixed"), and the null-word filter running
  # first recorded every null result as not reported -- dropping exactly the
  # studies a synthesis most needs to count. Anything undeclared is still NULL.
  if (identical(field$type, "enum")) {
    s <- as_chr1(v)
    return(if (s %in% field$values) s else NULL)
  }
  if (is_one_of_words(v, "not reported")) return(NULL)
  # For a number or a boolean these words cannot be the value, and a boolean
  # would otherwise read "none" as FALSE. For a string they can be: "None" is
  # the answer for declared conflicts of interest or serious adverse events.
  # Whether a string "None" is that answer or the model's way of saying it
  # found nothing depends on the sentence behind it, which only
  # reconcile_fields() can see, so it decides there.
  if (!identical(field$type, "string") && is_placeholder_value(v)) return(NULL)
  # Above .Machine$integer.max an integer field is stored as a DOUBLE, not
  # thrown away. as.integer(3e9) is NA, so a person-days or population count
  # came out as "not reported" and n_filled dropped to 0 -- the extraction
  # table said the document was silent about a value it had stated plainly.
  # A fraction is not a whole number, and rounding one invents it: "0,85" is
  # not 1, and "1.204" (a thousand and more, written the European way) is not
  # 1.
  whole <- function(z) if (z != round(z)) NULL else
    if (abs(z) > .Machine$integer.max) z else as.integer(z)
  switch(field$type,
    string  = as_chr1(v),
    # Only the words that answer yes or no. `%in%` against the yes-words alone
    # made every other string a confident FALSE -- "unclear", "not stated",
    # and "Yes - Pfizer funded it" -- with a verbatim quote beside it, so
    # the evidence check passed and an industry-funded trial was counted as
    # unfunded.
    boolean = { if (is.logical(v)) return(v)
                s <- sub("[.!]+\\s*$", "", as_chr1(v), perl = TRUE)
                if (is_one_of_words(s, .gr_true_words)) TRUE
                else if (is_one_of_words(s, .gr_false_words)) FALSE else NULL },
    # Already the right type: take it as it is. Round-tripping a double through
    # as.character() to strip punctuation it does not contain costs precision
    # (as.character(1/3) is fifteen digits), and coercion runs again on values
    # that have already been coerced once.
    integer = { if (is.numeric(v) && is.finite(v)) return(whole(v))
                n <- numeric_token(as_chr1(v))
                if (is.null(n) || !is.finite(n)) NULL else whole(n) },
    number  = { if (is.numeric(v) && is.finite(v)) return(as.numeric(v))
                n <- numeric_token(as_chr1(v), exponent = TRUE)
                if (is.null(n) || !is.finite(n)) NULL else n },
    NULL)
}

#' Is `raw`, which coerce_field() made nothing of, a value the model gave all
#' the same?
#'
#' TRUE for "120 (60 per arm)" in an integer field, "unclear" in a boolean one
#' or an undeclared enum value; FALSE for nothing at all, a placeholder
#' ("null", "N/A") or a way of saying the field is not reported. A field left
#' empty for such a value is not a field the document does not report: the
#' model found something there, and nothing here could read it.
#' @noRd
unreadable_value <- function(raw) {
  if (!length(raw)) return(FALSE)
  v <- raw[[1]]
  if (is.list(v) || length(v) != 1L || is.na(v)) return(FALSE)
  if (!is.character(v)) return(TRUE)
  nzchar(trimws(v)) && !is_one_of_words(v, c(.gr_placeholder_words, .gr_unreported_words))
}

#' The one number in a string, or nothing.
#'
#' `gsub("[^0-9.+-]", "", x)` deleted the separators along with the words, so
#' every digit in the value was glued into a single number: "120 (60 per arm)"
#' became 12060, "482 (Table 1)" became 4821, and "1,204 randomised; 1,180
#' analysed" became 12041180. Those are fabricated figures, and they were the
#' worst kind, because the QUOTE they came with was verbatim -- so the evidence
#' check passed, `n_unverified` stayed 0, and the audit report certified a
#' number that appears nowhere in the paper.
#'
#' A value carrying more than one number is a value this field did not get.
#' Returning nothing makes it a miss, which is counted and reported. There is no
#' reading of "120 (60 per arm)" under which 12060 is better than a miss.
#'
#' The same holds for a number written in a way that reads two ways, and one
#' that reads one way only once its spelling is folded:
#'
#' * digits of other scripts (Arabic-Indic, Persian, full-width, Devanagari,
#'   Bengali) are read as the digits they are, and a minus sign (U+2212), an
#'   en dash or a full-width hyphen before a number as a minus: "HR" U+2212
#'   "0.2" lost its sign, and the direction of the effect with it;
#' * ".03" is 0.03, as APA style writes it, not 3;
#' * a comma is a thousands separator only between groups of three digits
#'   ("1,204"), and a decimal comma only after a lone zero ("0,85"); any other
#'   ("1,5", "12,50") could be either, and is a miss rather than 15 or 1250;
#' * a number followed by a scale word or suffix ("3 million", "12k", "2.5
#'   bn", 3 followed by U+4E07) is a miss: 3 is not the value, and which unit
#'   the field wants (millions, or people) is not something this can know.
#' @noRd
numeric_token <- function(x, exponent = FALSE) {
  x <- fold_number_marks(as_chr1(x, ""))
  # Every run of digits and the marks that sit inside a number, taken whole,
  # so the grammar below judges the run as written: a pattern that stopped
  # where the grammar did cut "1,5" into a plausible 1 and 5, or glued it into
  # 15.
  pat <- paste0("[-+]?(?:[0-9]|\\.[0-9])[0-9.,]*", if (exponent) "(?:[eE][-+]?[0-9]+)?")
  m <- gregexpr(pat, x, perl = TRUE)[[1]]
  if (m[1] < 0L || length(m) != 1L) return(NULL)
  body <- substr(x, m, m + attr(m, "match.length") - 1L)
  # A full stop or comma after the number ends the sentence or the clause.
  while (endsWith(body, ".") || endsWith(body, ",")) body <- substr(body, 1L, nchar(body) - 1L)
  end <- m + nchar(body) - 1L
  # The run holds an "e" only as the start of its exponent.
  exp <- ""
  if (exponent && grepl("[eE]", body)) {
    at <- regexpr("[eE]", body)
    exp <- substring(body, at)
    body <- substr(body, 1L, at - 1L)
  }
  sign <- ""
  if (startsWith(body, "-") || startsWith(body, "+")) {
    sign <- substr(body, 1L, 1L)
    body <- substring(body, 2L)
  }
  if (grepl(",", body, fixed = TRUE)) {
    body <- if (grepl("^[1-9][0-9]{0,2}(?:,[0-9]{3})+(?:\\.[0-9]+)?$", body, perl = TRUE)) {
      gsub(",", "", body, fixed = TRUE)
    } else if (grepl("^0,[0-9]+$", body, perl = TRUE)) {
      sub(",", ".", body, fixed = TRUE)
    } else return(NULL)
  } else if (!grepl("^(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)$", body, perl = TRUE)) {
    return(NULL)
  }
  if (scaled_after(substring(x, end + 1L))) return(NULL)
  as.numeric(paste0(sign, body, exp))
}

#' Does `tail`, the text straight after a number, open with a scale word or
#' suffix: "million", "k", "bn", "Mio.", U+4E07 and the like?
#'
#' A space before a word and before "bn" and "mn", and none before "k" and
#' "m", which after a space are as often a unit ("2.5 K" of potassium, "1.2 m"
#' of height), as digit_numbers() reads them.
#' @noRd
scaled_after <- function(tail) {
  # Every scale word read here starts with one of these letters, and the one
  # pattern test is cheap where the full one is not: this runs for every
  # number every chunk gives.
  if (!nzchar(tail) ||
      !grepl("^\\s*[hHtTmMbBlLcCkK\u5343\u4e07\u842c\u4ebf\u5104]", tail, perl = TRUE)) {
    return(FALSE)
  }
  tail <- substr(tail, 1L, 24L)
  # Accents folded as the scale words are spelled ("millon" for the Spanish
  # word with its accent), and only where a Latin word follows: fold_numerals()
  # is the slow part, and no word of another script is a scale word here.
  if (grepl("[^\\x01-\\x7f]", tail, perl = TRUE, useBytes = TRUE) &&
      grepl("^\\s*\\p{Latin}", tail, perl = TRUE)) {
    tail <- fold_numerals(tail)
  }
  grepl(scale_pattern(), tail, ignore.case = TRUE, perl = TRUE)
}

#' The pattern scaled_after() matches, built once: .gr_foreign_scale is
#' defined in a file collated after this one.
#' @noRd
scale_pattern <- function() {
  if (is.null(.gr_scale_cache$pattern)) {
    words <- c("hundreds?", "thousands?", "millions?", "billions?", "trillions?", "lakhs?",
               "crores?", names(.gr_foreign_scale))
    .gr_scale_cache$pattern <- paste0(
      "^\\s*(?:", paste(words, collapse = "|"), ")(?![a-z])|^[km](?![a-z])|",
      "^\\s?(?:bn|mn)(?![a-z])|^\\s*[\u5343\u4e07\u842c\u4ebf\u5104]")
  }
  .gr_scale_cache$pattern
}

#' @noRd
.gr_scale_cache <- new.env(parent = emptyenv())

#' Digits of other scripts as ASCII, and the marks a typeset number carries
#' in place of ASCII ones folded to them: full-width and Arabic decimal and
#' thousands separators, and a minus sign (U+2212), a hyphen, figure or en
#' dash or a full-width or small hyphen-minus to "-". The one map of digits,
#' shared with fold_numerals().
#' @noRd
fold_number_marks <- function(x) {
  if (!grepl("[^\\x01-\\x7f]", x, perl = TRUE, useBytes = TRUE)) return(x)
  if (!identical(Encoding(x), "UTF-8")) x <- to_utf8(x)
  x <- chartr(.gr_digit_fold$from, .gr_digit_fold$to, x)
  gsub("[\u2010\u2011\u2012\u2013\u2212\ufe63\uff0d]", "-", x, perl = TRUE)
}

#' Full-width, Arabic-Indic (both), Devanagari and Bengali digits, then the
#' full-width and Arabic decimal and thousands separators and percent sign,
#' and what each is in ASCII.
#' @noRd
.gr_digit_fold <- list(
  from = intToUtf8(c(0xFF10:0xFF19, 0x0660:0x0669, 0x06F0:0x06F9, 0x0966:0x096F,
                     0x09E6:0x09EF, 0xFF0E, 0xFF0C, 0x066B, 0x066C, 0xFF05)),
  to = paste0(strrep("0123456789", 5L), ".,.,%"))

#' A sample-size column read the way [numeric_token()] reads a value.
#'
#' A stored `n` is whatever the extraction put there, and that is often
#' "900 participants" rather than 900. as.numeric() makes that NA, and a weight
#' built on it then treats a large study as an unreported one.
#'
#' Vectorised, and deliberately as strict as numeric_token(): a cell holding two
#' numbers is a cell this column did not get, not an invitation to guess which
#' one is the sample size.
#' @noRd
n_column <- function(x) {
  if (is.numeric(x)) return(as.numeric(x))
  vapply(as.character(x), function(v) {
    if (is.na(v) || !nzchar(trimws(v))) return(NA_real_)
    t <- numeric_token(v)
    if (is.null(t) || !length(t) || !is.finite(t[1])) NA_real_ else as.numeric(t[1])
  }, numeric(1), USE.NAMES = FALSE)
}
