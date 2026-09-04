# protocol.R -- what a review is looking for, written down before it starts.
#
# WHY THIS FILE EXISTS
# `gr_fields()` says what to collect. A review needs three more things said out
# loud, and needs them said BEFORE any document is read:
#
#   * which documents count (inclusion and exclusion criteria)
#   * what to collect from the ones that do (the schema)
#   * what the write-up has to cover (the outline)
#
# Not because a model cannot infer them, but because a criterion invented while
# reading is a criterion fitted to what was found. That is the difference between
# a review and a summary of whatever turned up, and it is why every reporting
# standard for evidence synthesis asks for the protocol to be registered in
# advance. A protocol object makes the three fixed, inspectable and shareable --
# it can be written to a file, diffed, and cited alongside the results.
#
# THESE TEMPLATES ARE STARTING POINTS, NOT STANDARDS. `gr_protocols()` ships
# three because a blank page is the wrong place to begin, not because the
# package is qualified to write your criteria. Every field of a built-in is meant
# to be edited.

#' Write down what a review is looking for
#'
#' A protocol fixes the three decisions that must not be made while reading:
#' which documents count, what to collect from them, and what the write-up has to
#' cover. Pass one to [gr_extract()] in place of a schema.
#'
#' @param name A short name. Becomes the registry key if you register it.
#' @param question The review question, in one sentence. Required -- it has a
#'   default only so that omitting it produces the explanation below rather than
#'   R's "argument is missing".
#' @param include,exclude Criteria, one per element, each a statement a document
#'   either meets or does not. Write them so that a careful reader with no
#'   knowledge of your field could apply them: "reports a randomised comparison"
#'   is checkable, "is high quality" is not.
#' @param fields A [gr_fields()] schema: what to extract from an included
#'   document.
#' @param outline The write-up, as a named character vector: names are section
#'   headings, values say what that section has to cover.
#' @param recipe The reading pipeline to default to, as in [gr_extract()].
#' @param description One line, for [gr_protocols()].
#' @return A `gr_protocol`.
#'
#' @section Criteria are not free text:
#' `include` and `exclude` are separate, and both are kept, because a document
#' can meet an inclusion criterion and still be excluded -- and a review has to
#' be able to say which. Collapsing them into one list of "criteria" loses the
#' reason, which is the part anyone auditing the review will ask for.
#'
#' @seealso [gr_protocols()] for the built-ins, [gr_register_protocol()],
#'   [gr_protocol_save()], [gr_extract()], [gr_fields()]
#' @export
#' @examples
#' p <- gr_protocol(
#'   "statins-primary",
#'   question = "Do statins reduce cardiovascular events in primary prevention?",
#'   include = c("Reports a randomised comparison",
#'               "Participants have no prior cardiovascular event",
#'               "Reports at least one cardiovascular outcome"),
#'   exclude = c("Secondary prevention only", "Not a primary research report"),
#'   fields = gr_fields(
#'     n = gr_field("Number randomised", type = "integer"),
#'     drug = "The statin studied, and the dose"
#'   ),
#'   outline = c(
#'     "Included studies" = "How many, of what design, over what period",
#'     "Findings" = "Effect on each outcome, with the range across studies"
#'   )
#' )
#' p
gr_protocol <- function(name, question = NULL, include = NULL, exclude = NULL,
                        fields = NULL, outline = NULL, recipe = "research",
                        description = "") {
  if (!is_nonblank(name)) gr_abort("A protocol needs a `name`.", class = "gr_bad_protocol")
  if (!is_nonblank(question)) {
    gr_abort(paste0("A protocol needs a `question`. It is what the criteria and the schema are ",
                    "for, and a review whose question is implicit is one whose question moved."),
             class = "gr_bad_protocol")
  }
  if (!is.null(fields) && !inherits(fields, "gr_fields")) {
    fields <- if (is.list(fields)) do.call(gr_fields, fields) else
      gr_abort("`fields` must come from gr_fields().", class = "gr_bad_protocol")
  }
  structure(list(
    name = as_chr1(name),
    question = as_chr1(question),
    include = criteria_vector(include, "include"),
    exclude = criteria_vector(exclude, "exclude"),
    fields = fields,
    outline = outline_vector(outline),
    recipe = recipe,
    description = as_chr1(description)
  ), class = "gr_protocol")
}

#' @export
print.gr_protocol <- function(x, ...) {
  cat(sprintf("<gr_protocol '%s'>\n", x$name))
  cat(sprintf("  question : %s\n", x$question))
  show <- function(label, v) {
    if (!length(v)) return(invisible(NULL))
    cat(sprintf("  %-9s: %s\n", label, v[[1]]))
    for (s in v[-1]) cat(sprintf("  %-9s  %s\n", "", s))
  }
  show("include", x$include)
  show("exclude", x$exclude)
  if (!is.null(x$fields)) {
    cat(sprintf("  fields   : %s\n", paste(names(x$fields), collapse = ", ")))
  }
  if (length(x$outline)) {
    cat(sprintf("  outline  : %s\n", paste(names(x$outline), collapse = " / ")))
  }
  cat(sprintf("  recipe   : %s\n", as_chr1(x$recipe, "research")))
  invisible(x)
}

#' Register a protocol
#'
#' @param name Registry key.
#' @param protocol A [gr_protocol()].
#' @return The name, invisibly.
#' @seealso [gr_protocols()], [gr_protocol()]
#' @export
#' @examples
#' p <- gr_protocol("mine", question = "What did each report conclude?",
#'                  fields = gr_fields(conclusion = "The report's own conclusion"))
#' gr_register_protocol("mine", p)
#' gr_protocols("mine")$question
gr_register_protocol <- function(name, protocol) {
  if (!inherits(protocol, "gr_protocol")) {
    gr_abort("`protocol` must come from gr_protocol().", class = "gr_bad_protocol")
  }
  protocol$name <- as_chr1(name)
  registry_set("protocols", name, protocol)
}

#' Protocols that ship with the package, and any you have registered
#'
#' Three starting points. They are **templates, not standards**: the package
#' knows the shape a protocol has, not what your criteria should be, and every
#' field of a built-in is meant to be edited before it is used.
#'
#' \describe{
#'   \item{`bibliography`}{Who wrote it, what it is called, where it appeared.
#'     No screening -- everything is included -- so it is the cheapest way to
#'     turn a folder into a reference list you can check.}
#'   \item{`evidence_table`}{One row per study: design, population, comparison,
#'     outcome, effect. No synthesis outline; the table is the output.}
#'   \item{`systematic_review`}{Criteria, a schema and a write-up outline, in
#'     the shape a report following a standard like PRISMA expects. Filling it in
#'     is your work, not the package's.}
#' }
#'
#' @param name A protocol name, or omit to list them all.
#' @return With `name`, a [gr_protocol()]. Without, a data frame of `name`,
#'   `question` and `description`.
#' @seealso [gr_protocol()], [gr_register_protocol()], [gr_extract()]
#' @export
#' @examples
#' gr_protocols()
#' gr_protocols("bibliography")$fields
gr_protocols <- function(name = NULL) {
  if (is.null(name)) return(registry_table("protocols", c("question", "description")))
  registry_get("protocols", name, "protocols")
}

#' Save a protocol to a file, and read one back
#'
#' A protocol is meant to be written down before the reading starts, shared with
#' whoever is checking the work, and cited alongside the results. That means a
#' file, and JSON because it is exact and needs nothing installed.
#'
#' The round trip is lossless for everything a protocol *is*. `recipe` is the one
#' thing it may not be: a `gr_recipe` object is written as its name, because a
#' file that pinned every clean and segmentation setting would silently pin them
#' for a reader on a different version.
#'
#' @param protocol A [gr_protocol()].
#' @param path File path. `gr_protocol_read()` also accepts a JSON string.
#' @return `gr_protocol_save()` returns `path` invisibly;
#'   `gr_protocol_read()` returns a `gr_protocol`.
#' @seealso [gr_protocol()], [gr_protocols()]
#' @export
#' @examples
#' p <- gr_protocols("bibliography")
#' f <- tempfile(fileext = ".json")
#' gr_protocol_save(p, f)
#' identical(gr_protocol_read(f)$question, p$question)
gr_protocol_save <- function(protocol, path) {
  if (!inherits(protocol, "gr_protocol")) {
    gr_abort("`protocol` must come from gr_protocol().", class = "gr_bad_protocol")
  }
  writeLines(as.character(as_json(protocol_as_list(protocol), pretty = TRUE)), path)
  invisible(path)
}

#' @rdname gr_protocol_save
#' @export
gr_protocol_read <- function(path) {
  txt <- if (length(path) == 1L && !grepl("[{}]", path) && file.exists(path)) {
    paste(readLines(path, warn = FALSE), collapse = "\n")
  } else {
    as_chr1(path)
  }
  raw <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE),
                  error = function(e) NULL)
  if (!is.list(raw)) {
    gr_abort("Could not read a protocol from that path or string: it is not JSON.",
             class = "gr_bad_protocol")
  }
  as_protocol(raw)
}

# --- internals -------------------------------------------------------------

#' @noRd
criteria_vector <- function(x, what) {
  if (is.null(x) || !length(x)) return(character(0))
  v <- vapply(x, as_chr1, character(1), USE.NAMES = FALSE)
  v <- trimws(v)
  v <- v[nzchar(v)]
  if (!length(v)) return(character(0))
  unname(v)
}

#' The outline, as headings pointing at what each heading must cover.
#'
#' A bare character vector is accepted and each element becomes its own heading
#' with itself as the brief, because "Methods", "Findings", "Limitations" is a
#' perfectly good outline and demanding names for it is bureaucracy.
#' @noRd
outline_vector <- function(x) {
  if (is.null(x) || !length(x)) return(character(0))
  v <- vapply(x, as_chr1, character(1), USE.NAMES = FALSE)
  nms <- names(x)
  if (is.null(nms)) nms <- rep("", length(v))
  nms[!nzchar(trimws(nms))] <- v[!nzchar(trimws(nms))]
  keep <- nzchar(trimws(v))
  stats::setNames(unname(v[keep]), nms[keep])
}

#' A protocol as a plain list, for JSON.
#'
#' `gr_fields` is a list of `gr_field` objects, and a class attribute does not
#' survive JSON. Written out field by field so the file says what it means.
#' @noRd
protocol_as_list <- function(p) {
  list(
    name = p$name,
    question = p$question,
    description = p$description,
    include = as.list(p$include),
    exclude = as.list(p$exclude),
    fields = if (is.null(p$fields)) NULL else lapply(p$fields, function(f) {
      out <- list(description = f$description, type = f$type)
      if (length(f$values)) out$values <- as.list(f$values)
      out
    }),
    # A `gr_recipe` is written as its NAME. Pinning every clean and segmentation
    # setting into a shared file would silently pin them for whoever reads it,
    # on whatever version they have -- a protocol says what to look for, not how
    # this machine happened to chunk.
    outline = as.list(p$outline),
    recipe = if (inherits(p$recipe, "gr_recipe")) p$recipe$name else as_chr1(p$recipe, "research")
  )
}

#' @noRd
as_protocol <- function(x) {
  if (inherits(x, "gr_protocol")) return(x)
  if (!is.list(x)) gr_abort("Expected a gr_protocol or a named list.", class = "gr_bad_protocol")
  flat <- function(v) if (is.null(v)) NULL else vapply(v, as_chr1, character(1))
  fields <- if (!length(x$fields)) NULL else {
    do.call(gr_fields, lapply(x$fields, function(f) {
      gr_field(as_chr1(f$description), type = as_chr1(f$type, "string"),
               values = if (length(f$values)) unlist(f$values, use.names = FALSE) else NULL)
    }))
  }
  outline <- if (!length(x$outline)) NULL else {
    stats::setNames(vapply(x$outline, as_chr1, character(1), USE.NAMES = FALSE),
                    names(x$outline))
  }
  gr_protocol(name = as_chr1(x$name, "protocol"), question = as_chr1(x$question),
              include = flat(x$include), exclude = flat(x$exclude),
              fields = fields, outline = outline,
              recipe = as_chr1(x$recipe, "research"),
              description = as_chr1(x$description))
}

#' @noRd
register_builtin_protocols <- function() {
  gr_register_protocol("bibliography", gr_protocol(
    "bibliography",
    question = "What is each of these documents, and who wrote it?",
    description = "Turn a folder into a checkable reference list. No screening.",
    fields = gr_fields(
      title   = "The document's own title, exactly as it is printed on it",
      authors = "The authors, in the order they are listed, as printed",
      year    = gr_field("Year of publication", type = "integer"),
      venue   = "The journal, publisher, conference or issuing body",
      doi     = "The DOI or other permanent identifier, if one is printed"
    ),
    recipe = "research"))

  gr_register_protocol("evidence_table", gr_protocol(
    "evidence_table",
    question = "What did each study do, to whom, and what did it find?",
    description = "One row per study. The table is the output; there is no write-up.",
    include = c("Reports original empirical research",
                "Reports at least one quantitative result"),
    exclude = c("Commentary, editorial or letter with no original data",
                "Protocol or registration only, with no results"),
    fields = gr_fields(
      design     = "The study design, in the paper's own words",
      population = "Who was studied: who they were, where, and how many",
      n          = gr_field("Number of participants analysed", type = "integer"),
      comparison = "What was compared with what, or NA if there is no comparison",
      outcome    = "The primary outcome, as the paper defines it",
      effect     = "The primary result, with its interval, as reported",
      direction  = gr_field("Direction of the primary result",
                            type = "enum",
                            values = c("favours intervention", "favours control",
                                       "no difference", "unclear"))
    ),
    recipe = "research"))

  gr_register_protocol("systematic_review", gr_protocol(
    "systematic_review",
    question = "REPLACE THIS with your review question, in one sentence.",
    description = "A template in the shape a PRISMA-style report expects. Edit every field.",
    include = c("REPLACE: the population the review is about",
                "REPLACE: the intervention or exposure",
                "REPLACE: the comparison, if the question has one",
                "REPLACE: the outcomes that make a study relevant",
                "REPLACE: the study designs that count"),
    exclude = c("Not a primary research report",
                "REPLACE: any population, setting or design ruled out in advance"),
    fields = gr_fields(
      design      = "The study design, in the paper's own words",
      setting     = "Country and setting",
      population  = "Who was studied, and how many",
      n           = gr_field("Number of participants analysed", type = "integer"),
      intervention = "What was done, to whom, for how long",
      comparison  = "What it was compared with",
      outcome     = "The primary outcome, as the paper defines it",
      effect      = "The primary result, with its interval, as reported",
      funding     = "Who funded the study, as declared",
      # Not `conflicts`: the extraction table uses that name for the fields a
      # document disagreed with itself about, and check_field_names() refuses
      # it -- which is how this template found out.
      coi         = "Declared conflicts of interest"
    ),
    outline = c(
      "Included studies" =
        "How many studies, of what designs, from where, over what period",
      "Participants" =
        "Who was studied across the included studies, and how they differ",
      "Findings" =
        "The effect on each outcome, with the range across studies and where they disagree",
      "Certainty" =
        "What limits confidence: study limitations, inconsistency, imprecision",
      "Conclusions" =
        "What the body of evidence supports, and what it does not"
    ),
    recipe = "research"))
  invisible(NULL)
}
