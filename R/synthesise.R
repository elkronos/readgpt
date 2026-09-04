# synthesise.R -- the write-up, one section at a time, from the table.
#
# WHY THIS FILE EXISTS
# An extraction table is the finding; a review is the account of it. Asking a
# model for that account in one call over two hundred studies is the version that
# does not work: it overflows, and long before it overflows it starts writing
# fluently about studies it has stopped attending to.
#
# So the unit is the SECTION, and it comes from the protocol's outline rather
# than from the model's sense of how a review is structured. Each section is one
# call, over the table, against a brief the author wrote in advance. That makes
# the write-up reproducible section by section, revisable a section at a time,
# and -- because the outline is fixed before the reading -- not shaped by what
# happened to be found.
#
# EVERY CLAIM CITES A ROW. Sections are written with `[study 3]` markers, the
# markers are parsed back out, and any pointing at a row that does not exist is
# reported and marks the section partial. This is the same check `new_answer()`
# runs on `[chunk 3]`, for the same reason: a citation to something that was
# never supplied is a fabrication, and the most convincing kind there is.
#
# The chain it completes: a sentence cites a study, the study's row cites a
# quote, the quote was checked against the page it is attributed to. Nothing here
# proves the SENTENCE is true. It makes every step of the way back to the
# document short enough to walk.

#' Write a review from an extraction table
#'
#' The last stage. Takes what [gr_extract()] found and writes it up section by
#' section, against an outline fixed in advance, with every section citing the
#' rows it rests on.
#'
#' @param extraction A `gr_extraction` from [gr_extract()], or a data frame
#'   shaped like its `$table`.
#' @param protocol A [gr_protocol()]; its `outline` and `question` are used
#'   unless you give them directly.
#' @param outline The sections, as a named character vector: names are headings,
#'   values say what that section has to cover. As [gr_protocol()].
#' @param question The review question, for framing.
#' @param client A `gr_client`.
#' @param model,max_section_tokens,temperature Overrides for the writing calls.
#' @param include_unclear Write from rows whose extraction was incomplete. Off by
#'   default: a row with nothing in it contributes nothing but its own absence,
#'   and the count of skipped rows is reported either way.
#'
#' @return An object of class `gr_synthesis`:
#'   \describe{
#'     \item{`text`}{The whole write-up, as markdown.}
#'     \item{`sections`}{One row per section: `section`, `brief`, `text`,
#'       `n_cited`, `n_unknown`, `partial`.}
#'     \item{`citations`}{Long form: `section`, `study`, `document`,
#'       `document_id` -- every citation, resolved to the row it points at.}
#'     \item{`studies`}{The rows that were written from, with the `study` number
#'       each was cited by.}
#'     \item{`trace`}{As [gr_extract()].}
#'   }
#'
#' @section Which rows are used:
#' Rows that were never read (`status` `"failed"` or `"skipped"`) are left out,
#' and so are duplicates -- a study counted twice is the error this whole
#' pipeline exists to avoid, and `gr_read_many()` has already marked them. The
#' number left out is reported by `print()` and is in `$skipped`.
#'
#' @section What it costs:
#' One call per section when the table fits one prompt, which is the usual case:
#' a hundred rows of a ten-field schema is a few thousand tokens. A table too
#' large for one prompt is written in batches and merged, so a section costs
#' batches + merges instead. Either way the cost is per *section*, not per
#' document -- the expensive reading has already happened.
#'
#' @seealso [gr_extract()], [gr_protocol()], [gr_screen()]
#' @export
#' @examples
#' fields <- gr_fields(design = "The study design",
#'                     n = gr_field("Participants", type = "integer"))
#' cl <- gr_mock_client(function(messages, params) {
#'   seen <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
#'                 collapse = " ")
#'   if (grepl("<studies>", seen, fixed = TRUE)) "One randomised trial of 120 people [study 1]."
#'   else '{"design":"randomised trial","n":120,
#'          "design__quote":"We ran a randomised trial.",
#'          "n__quote":"We enrolled 120 people."}'
#' })
#'
#' f <- tempfile(fileext = ".txt")
#' writeLines("We ran a randomised trial. We enrolled 120 people.", f)
#' x <- gr_extract(f, fields, client = cl)
#'
#' s <- gr_synthesise(x, question = "Does it work?",
#'                    outline = c("Included studies" = "How many, of what design"),
#'                    client = cl)
#' s$sections[, c("section", "n_cited", "n_unknown")]
gr_synthesise <- function(extraction, protocol = NULL, outline = NULL, question = NULL,
                          client = NULL, model = NULL, max_section_tokens = 1200L,
                          temperature = NULL, include_unclear = FALSE) {
  tab <- if (inherits(extraction, "gr_extraction")) extraction$table else extraction
  if (!is.data.frame(tab) || !nrow(tab)) {
    gr_abort("`extraction` must be a gr_extraction, or a data frame shaped like its $table.",
             class = "gr_no_studies")
  }
  if (!is.null(protocol)) {
    if (!inherits(protocol, "gr_protocol")) {
      gr_abort("`protocol` must come from gr_protocol().", class = "gr_bad_protocol")
    }
    if (is.null(outline)) outline <- protocol$outline
    if (is.null(question)) question <- protocol$question
  }
  outline <- outline_vector(outline)
  if (!length(outline)) {
    gr_abort(paste0("`outline` is empty. Give the sections to write, as a named character ",
                    "vector of heading = what it must cover, or a gr_protocol() carrying one."),
             class = "gr_no_outline")
  }
  if (!is_nonblank(question)) gr_abort("`question` must be a non-empty string.")

  keep <- synth_usable(tab, include_unclear)
  used <- tab[keep, , drop = FALSE]
  if (!nrow(used)) {
    gr_abort(paste0("No usable rows: every document either failed, was a duplicate of another, ",
                    "or had nothing extracted. There is nothing to write from."),
             class = "gr_no_studies")
  }
  used$study <- seq_len(nrow(used))

  client <- client %||% gr_client(model = model %||% gr_options("model"))
  spec <- gr_read_spec("stuff", model = model, temperature = temperature,
                       max_answer_tokens = max_section_tokens)
  trace <- gr_trace(meta = list(stage = "synthesise", question = question,
                                sections = length(outline), studies = nrow(used)))

  rendered <- render_studies(used)
  rows <- lapply(seq_along(outline), function(i) {
    heading <- names(outline)[[i]]
    gr_msg(sprintf("[%d/%d] %s", i, length(outline), heading))
    synth_section(heading, outline[[i]], question, rendered, used, client, spec, trace)
  })

  sections <- do.call(rbind, lapply(rows, `[[`, "row"))
  citations <- rbind_evidence(lapply(rows, `[[`, "citations"))
  structure(list(
    text = synth_document(sections),
    sections = sections,
    citations = citations %||% synth_empty_citations(),
    studies = used,
    skipped = sum(!keep),
    question = question,
    outline = outline,
    trace = trace
  ), class = "gr_synthesis")
}

#' @export
print.gr_synthesis <- function(x, ...) {
  s <- x$sections
  cat(sprintf("<gr_synthesis> %d section(s) from %d stud%s\n", nrow(s), nrow(x$studies),
              if (nrow(x$studies) == 1L) "y" else "ies"))
  if (x$skipped) {
    cat(sprintf("  %d row(s) left out: failed, duplicate, or nothing extracted\n", x$skipped))
  }
  for (i in seq_len(nrow(s))) {
    cat(sprintf("  %-22s %5d words, %d citation(s)%s\n",
                substr(s$section[i], 1, 22),
                lengths(strsplit(trimws(s$text[i]), "\\s+"))[1],
                s$n_cited[i],
                if (s$n_unknown[i]) sprintf(", %d TO A ROW THAT DOES NOT EXIST", s$n_unknown[i])
                else if (!s$n_cited[i]) ", NONE" else ""))
  }
  if (any(s$n_unknown > 0L)) {
    cat("  a citation to a row that does not exist is a fabrication; those sections are partial\n")
  }
  cost <- gr_trace_cost(x$trace)
  total <- if (nrow(cost)) sum(cost$usd) else 0
  cat(sprintf("  this run: %d model call(s), %s\n", x$trace$calls,
              if (!nrow(cost)) "no cost recorded"
              else if (is.na(total)) "cost unknown (unpriced model)"
              else sprintf("$%.4f", total)))
  invisible(x)
}

# --- internals -------------------------------------------------------------

#' Which rows a write-up may draw on.
#'
#' A duplicate is excluded even though it has perfectly good values, because it
#' is the SAME study: counting it twice is the error the whole pipeline exists to
#' avoid, and it is the easiest one to make here, where the rows all look alike.
#' @noRd
synth_usable <- function(tab, include_unclear) {
  ok <- if (is.null(tab$status)) rep(TRUE, nrow(tab)) else
    tab$status %in% c("ok", "restored", "duplicate")
  distinct <- if (is.null(tab$duplicate_of)) rep(TRUE, nrow(tab)) else is.na(tab$duplicate_of)
  filled <- if (isTRUE(include_unclear) || is.null(tab$n_filled)) rep(TRUE, nrow(tab)) else
    !is.na(tab$n_filled) & tab$n_filled > 0L
  ok & distinct & filled
}

#' The table as text the model can cite.
#'
#' One block per row, numbered, with the field values and the document it came
#' from. The number is what a section cites, and it is positional within THIS
#' synthesis -- `$studies` carries it alongside `document_id` so a citation can
#' always be resolved back to a document.
#' @noRd
render_studies <- function(used) {
  meta <- c("document", "document_id", "status", "duplicate_of", "error",
            "n_filled", "n_unverified", "conflicts", "study")
  fields <- setdiff(names(used), meta)
  vapply(seq_len(nrow(used)), function(i) {
    vals <- vapply(fields, function(f) {
      v <- used[[f]][i]
      if (is.na(v)) sprintf("%s: not reported", f) else sprintf("%s: %s", f, as_chr1(v))
    }, character(1), USE.NAMES = FALSE)
    paste0("[study ", used$study[i], "] (", used$document[i], ")\n",
           paste(vals, collapse = "\n"))
  }, character(1), USE.NAMES = FALSE)
}

#' @noRd
synth_section <- function(heading, brief, question, rendered, used, client, spec, trace) {
  system_prompt <- sprintf(.gr_prompts$synthesise_system, heading)
  ask <- paste0("Review question: ", question,
                "\n\nSection: ", heading, "\nThis section must cover: ", brief)
  overhead <- prompt_overhead(ask, system_prompt)
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)

  body <- paste(rendered, collapse = "\n\n")
  text <- if (gr_count_tokens(body) <= bud$input) {
    res <- gr_call(client, list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = ask),
      list(role = "user", content = paste0("<studies>\n", body, "\n</studies>"))
    ), model = spec$model, max_output = spec$max_answer_tokens,
       temperature = spec$temperature, trace = trace, label = "synthesise.section")
    if (usable_text(res)) res$text else ""
  } else {
    # Too many studies for one prompt: draft the section from batches and merge.
    # Cheaper alternatives -- take the first N rows, or summarise the table first
    # -- both drop studies without saying which, which is the one thing a review
    # may not do.
    groups <- synth_batches(rendered, bud$input)
    parts <- vapply(groups, function(g) {
      res <- gr_call(client, list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = ask),
        list(role = "user", content = paste0("<studies>\n", paste(g, collapse = "\n\n"),
                                             "\n</studies>"))
      ), model = spec$model, max_output = spec$max_answer_tokens,
         temperature = spec$temperature, trace = trace, label = "synthesise.batch")
      if (usable_text(res)) res$text else ""
    }, character(1), USE.NAMES = FALSE)
    m <- tree_merge(client, ask, parts, spec, trace, label = "synthesise.merge",
                    system_prompt = system_prompt, kind = "draft")
    if (isTRUE(m$ok)) m$text else paste(parts[nzchar(parts)], collapse = "\n\n")
  }

  cited <- cited_ids(text, "study")
  known <- cited[cited %in% used$study]
  unknown <- setdiff(cited, used$study)
  hits <- match(known, used$study)
  list(
    row = data.frame(section = heading, brief = as_chr1(brief), text = as_chr1(text),
                     n_cited = length(known), n_unknown = length(unknown),
                     # A section citing a row that is not in the table, or citing
                     # nothing at all, is not a section anyone should paste into a
                     # manuscript unread.
                     partial = length(unknown) > 0L || !nzchar(trimws(text)),
                     stringsAsFactors = FALSE),
    citations = if (!length(known)) NULL else
      data.frame(section = heading, study = known,
                 document = used$document[hits],
                 # NOT as_chr1(): it collapses a vector to ONE string with
                 # newlines between the elements, so every citation row got the
                 # same value -- all the ids, glued together. It is for scalars.
                 document_id = if (is.null(used$document_id)) NA_character_ else
                   as.character(used$document_id[hits]),
                 stringsAsFactors = FALSE))
}

#' @noRd
synth_batches <- function(rendered, budget) {
  groups <- list(); buf <- character(0); tks <- 0L
  for (p in rendered) {
    pt <- gr_count_tokens(p)
    if (length(buf) && tks + pt > budget) {
      groups[[length(groups) + 1L]] <- buf; buf <- character(0); tks <- 0L
    }
    buf <- c(buf, p); tks <- tks + pt
  }
  if (length(buf)) groups[[length(groups) + 1L]] <- buf
  groups
}

#' @noRd
synth_document <- function(sections) {
  paste(sprintf("## %s\n\n%s", sections$section, trimws(sections$text)), collapse = "\n\n")
}

#' @noRd
synth_empty_citations <- function() {
  data.frame(section = character(0), study = integer(0), document = character(0),
             document_id = character(0), stringsAsFactors = FALSE)
}
