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
#'   `"enum"`.
#' @param values For `type = "enum"`, the permitted values.
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
  structure(list(description = as_chr1(description), type = type,
                 values = if (length(values)) as.character(values) else NULL),
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
#' @seealso [gr_field()], [gr_extract()]
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

#' Coerce one extracted value to the type its field declares.
#'
#' A model asked for an integer will sometimes return "1,204" or "about 1200".
#' Coercing here rather than at the point of use means the table has the type it
#' promises; a value that cannot be coerced becomes NA and is reported as a
#' conflict-free miss rather than silently poisoning a numeric column with text.
#' @noRd
coerce_field <- function(value, field) {
  if (is.null(value) || (length(value) == 1L && is.na(value))) return(NULL)
  v <- value[[1]]
  if (is.character(v) && !nzchar(trimws(v))) return(NULL)
  if (is.character(v) && tolower(trimws(v)) %in% c("null", "na", "n/a", "none", "not reported")) {
    return(NULL)
  }
  switch(field$type,
    string  = as_chr1(v),
    boolean = { b <- if (is.logical(v)) v else tolower(trimws(as_chr1(v))) %in% c("true", "yes", "y", "1")
                if (is.na(b)) NULL else b },
    enum    = { s <- as_chr1(v); if (s %in% field$values) s else NULL },
    # Already the right type: take it as it is. Round-tripping a double through
    # as.character() to strip punctuation it does not contain costs precision
    # (as.character(1/3) is fifteen digits), and coercion runs again on values
    # that have already been coerced once.
    integer = { if (is.numeric(v) && is.finite(v)) return(as.integer(round(v)))
                n <- suppressWarnings(as.numeric(gsub("[^0-9.+-]", "", as_chr1(v))))
                if (!is.finite(n)) NULL else as.integer(round(n)) },
    number  = { if (is.numeric(v) && is.finite(v)) return(as.numeric(v))
                n <- suppressWarnings(as.numeric(gsub("[^0-9.eE+-]", "", as_chr1(v))))
                if (!is.finite(n)) NULL else n },
    NULL)
}
