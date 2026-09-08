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
#' @param cite_style How citations appear in the finished prose. `"auto"` (the
#'   default) names the studies when the table can name all of them and uses
#'   markers when it cannot. `"author-year"` asks for names and warns if they
#'   cannot be produced; `"numeric"` gives `(1, 2)`; `"marker"` leaves
#'   `[study 1]` as written.
#'
#'   The model always writes `[study N]`, whatever this is set to, and the
#'   rendering happens afterwards from the table. That is deliberate: a marker
#'   can be checked exactly against the rows that exist, whereas verifying an
#'   author-year string would mean matching a name the model wrote against a
#'   name in the table, and near-misses -- Smith for Smyth, 2019 for 2018 -- are
#'   both the errors that matter and the ones fuzzy matching forgives. A
#'   rendered citation is therefore a fact about the extraction rather than
#'   something the model asserted. `$sections$text_marked` and `$text_marked`
#'   keep the marker form so the check can be re-run on the published prose.
#' @param bib Which columns carry bibliographic identity, as a named list of
#'   `citation`, `authors`, `year`, `title`, `venue`, `doi`. Omitted, the
#'   conventional names are looked for, which is why
#'   `gr_protocols("bibliography")` works without configuration. The most
#'   reliable route is a `citation` field asked for during extraction: parsing
#'   an arbitrary author list is a heuristic, and where it cannot be done
#'   confidently the run falls back to markers rather than printing a name that
#'   may be wrong.
#' @param style A register instruction, appended to the writing prompts --
#'   `"formal academic; hedge claims; past tense for findings"`. It governs how
#'   sections are written, never what they may say: the rules about citing every
#'   claim and inventing nothing hold whatever voice is asked for.
#' @param coherence Run one further call over the assembled draft to make the
#'   independently-written sections read as one argument: repetition removed,
#'   transitions added, terminology made consistent. The revision is checked, not
#'   trusted -- one that added a citation or dropped one is discarded with a
#'   warning, and `$draft` is what you get. Off by default, because it is an
#'   extra call and an extra chance for the model to touch finished prose.
#' @param references Append a `## References` section built from the studies the
#'   finished text actually cites. Alphabetical under `"author-year"`, numbered
#'   by study otherwise -- the list is labelled by whatever the prose uses to
#'   point into it.
#'
#' @return An object of class `gr_synthesis`:
#'   \describe{
#'     \item{`text`}{The whole write-up, as markdown, citations rendered and the
#'       reference list appended.}
#'     \item{`text_marked`}{The same document with `[study N]` markers intact --
#'       what the citation check ran on.}
#'     \item{`draft`}{The write-up before the coherence pass, for comparison.}
#'     \item{`references`}{The reference list, or `NULL`.}
#'     \item{`cite_style`}{The style actually used, which is not always the one
#'       asked for.}
#'     \item{`coherence`}{What the coherence pass did, or `NULL` if it did not
#'       run: `ran`, `kept`, `reason`, and any citations `added` or `lost`.}
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
                          temperature = NULL, include_unclear = FALSE,
                          cite_style = c("auto", "marker", "author-year", "numeric"),
                          bib = NULL, style = NULL, coherence = FALSE,
                          references = TRUE) {
  cite_style <- match.arg(cite_style)
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

  # The writing model is NOT shown who wrote each study. Adding bibliographic
  # fields to a schema put "authors: Smith, J., Okafor, A." in front of a model
  # asked to cite `[study 1]`, and a model that can see a name will sooner or
  # later write "Smith and Okafor (2019) found..." instead of the marker. That
  # citation is checked by nothing and rendered by nothing: it is the model
  # asserting an attribution, which is the one thing this design exists to
  # prevent. Identity is applied afterwards, from the table. A model cannot
  # misattribute a study whose authors it was never told.
  #
  # If a bibliographic value is also a FINDING -- publication year as
  # chronology, say -- extract it a second time under a name of its own.
  rendered <- render_studies(used, hide = unlist(bib_columns(used, bib), use.names = FALSE))
  rows <- lapply(seq_along(outline), function(i) {
    heading <- names(outline)[[i]]
    gr_msg(sprintf("[%d/%d] %s", i, length(outline), heading))
    synth_section(heading, outline[[i]], question, rendered, used, client, spec, trace, style)
  })

  sections <- do.call(rbind, lapply(rows, `[[`, "row"))
  citations <- rbind_evidence(lapply(rows, `[[`, "citations"))

  # ORDER MATTERS, and getting it wrong is silent. Everything downstream works
  # on the MARKER form -- the notation the model actually wrote and the citation
  # check actually ran on -- and rendering to "(Smith & Okafor, 2019)" happens
  # once, at the very end. Rendering first made the coherence pass compare a
  # rendered draft against a marked revision, so every citation in the revision
  # looked newly added and every honest revision was thrown away.
  cols <- bib_columns(used, bib)   # already resolved above; cheap and pure
  keys <- bib_keys(used, cols)
  resolved <- resolve_cite_style(cite_style, keys, cols)
  if (!identical(resolved, cite_style) && !identical(cite_style, "auto")) {
    gr_warn(paste0("`cite_style = \"", cite_style, "\"` needs a citation key for every study, and ",
                   "the table does not carry one for all of them. Falling back to markers rather ",
                   "than printing a name that may be wrong. Extract a `citation` field, or an ",
                   "`authors` and a `year` field, or point `bib` at the columns that hold them."),
            class = "gr_cite_unresolvable")
  }

  marked <- synth_document(sections)
  revised <- if (isTRUE(coherence)) {
    synth_coherence(marked, question, client, spec, trace, style)
  } else NULL
  final_marked <- revised$text %||% marked

  # The reference list follows what the FINISHED text cites, not what the
  # sections cited before revision. A reference list carrying a study the final
  # prose never mentions claims a breadth the review does not have.
  cited <- cited_ids(final_marked, "study")
  refs <- if (isTRUE(references)) reference_list(used, keys, cited, cols, resolved) else NULL

  render <- function(x) render_citations(x, used, keys, resolved)
  # Kept because it is what the citation check ran on, so the check can be
  # re-run on the published prose at any point.
  sections$text_marked <- sections$text
  sections$text <- vapply(sections$text, render, character(1), USE.NAMES = FALSE)

  structure(list(
    text = append_references(render(final_marked), refs),
    text_marked = final_marked,
    draft = append_references(render(marked), refs),
    sections = sections,
    references = refs,
    citations = citations %||% synth_empty_citations(),
    studies = used,
    skipped = sum(!keep),
    question = question,
    outline = outline,
    cite_style = resolved,
    coherence = revised$report,
    trace = trace
  ), class = "gr_synthesis")
}

#' Which citation style can actually be honoured.
#'
#' "auto" is the only one that silently changes: name the studies when the table
#' can name all of them, and use markers when it cannot. Asking for author-year
#' outright and not getting it is worth a warning, because the caller expected
#' something the data cannot support.
#' @noRd
resolve_cite_style <- function(want, keys, cols) {
  if (identical(want, "marker")) return("marker")
  if (identical(want, "numeric")) return(if (length(cols)) "numeric" else "marker")
  if (is.null(keys)) return("marker")
  if (identical(want, "auto")) "author-year" else want
}

#' One pass over the assembled draft, to make it read as one argument.
#'
#' Sections are written independently and cannot see each other, which is what
#' keeps each one answerable to its own brief and to the table. The cost is that
#' nothing joins them: terms drift, the same study is introduced twice, and
#' there are no transitions. This fixes that and nothing else.
#'
#' What comes back is checked, not trusted. A revision that added a citation, or
#' lost one, is discarded -- those are the two ways this step could quietly
#' undo the guarantee the rest of the pipeline exists to give.
#' @noRd
synth_coherence <- function(drafted, question, client, spec, trace, style = NULL) {
  before <- cited_ids(drafted, "study")
  sys <- .gr_prompts$coherence_system
  if (is_nonblank(style)) sys <- paste0(sys, "\n\nRegister: ", as_chr1(style))
  overhead <- prompt_overhead(question, sys)

  # This step returns the WHOLE document, not a section, so it must be budgeted
  # for the whole document. Sizing it by `max_section_tokens` -- which is what a
  # section writer needs -- asked a model revising a 4800-token review for 300
  # tokens of output. The reply is then truncated mid-review, and the citation
  # check below rejects it for "dropping" citations that were never written,
  # reporting a budgeting mistake as a model failure.
  need <- as.integer(ceiling(gr_count_tokens(drafted) * 1.15) + 64L)
  info <- gr_model_info(spec$model)
  room <- as.integer(info$max_output)
  if (need > room) {
    gr_warn(sprintf(paste0("The draft is about %d tokens and '%s' can emit at most %d, so the ",
                           "coherence pass was skipped rather than returning a review cut off ",
                           "part-way. The sections are as written; a shorter outline or a model ",
                           "with a larger output limit would let it run."),
                    gr_count_tokens(drafted), spec$model, room),
            class = "gr_coherence_skipped")
    return(list(text = NULL, report = list(ran = FALSE, reason = "draft exceeds the output limit",
                                           kept = FALSE)))
  }
  bud <- gr_budget(spec$model, reserve_output = need, overhead = overhead)
  if (gr_count_tokens(drafted) > bud$input) {
    gr_warn(paste0("The draft does not fit one prompt alongside room to rewrite it, so the ",
                   "coherence pass was skipped. The sections are as written; revise by hand, or ",
                   "use a model with a larger context window."),
            class = "gr_coherence_skipped")
    return(list(text = NULL, report = list(ran = FALSE, reason = "draft exceeds the context window",
                                           kept = FALSE)))
  }
  if (!trace_can_call(trace)) {
    return(list(text = NULL, report = list(ran = FALSE, reason = "call cap reached", kept = FALSE)))
  }
  res <- gr_call(client, list(
    list(role = "system", content = sys),
    list(role = "user", content = paste0("Review question: ", question)),
    list(role = "user", content = paste0("<draft>\n", drafted, "\n</draft>"))
  ), model = spec$model, max_output = need,
     temperature = spec$temperature, trace = trace, label = "synthesise.coherence")

  if (!usable_text(res)) {
    return(list(text = NULL, report = list(ran = TRUE, reason = "the revision call failed",
                                           kept = FALSE)))
  }
  # A revision that hit the output limit is a review with its ending cut off.
  # The citation check below would usually catch it -- the lost citations are
  # the ones in the missing tail -- but not when the truncation happens to land
  # after the last marker, and a review silently missing its conclusion is worse
  # than one that was never revised.
  if (identical(as_chr1(res$finish_reason), "length")) {
    gr_warn(paste0("The coherence pass was cut off by the model's output limit, so its revision ",
                   "was discarded and the section-by-section draft is what you have."),
            class = "gr_coherence_rejected")
    return(list(text = NULL, report = list(ran = TRUE, reason = "revision truncated", kept = FALSE,
                                           added = integer(0), lost = integer(0))))
  }
  after <- cited_ids(res$text, "study")
  added <- setdiff(after, before); lost <- setdiff(before, after)
  if (length(added) || length(lost)) {
    gr_warn(sprintf(paste0("The coherence pass %s, so its revision was discarded and the ",
                           "section-by-section draft is what you have. This step may reorganise ",
                           "prose; it may not change what the review cites."),
                    paste(c(if (length(added)) sprintf("added citation(s) to stud%s %s",
                                                       if (length(added) == 1L) "y" else "ies",
                                                       paste(added, collapse = ", ")),
                            if (length(lost)) sprintf("dropped citation(s) to stud%s %s",
                                                      if (length(lost) == 1L) "y" else "ies",
                                                      paste(lost, collapse = ", "))),
                          collapse = " and ")),
            class = "gr_coherence_rejected")
    return(list(text = NULL, report = list(ran = TRUE, reason = "citations changed", kept = FALSE,
                                           added = added, lost = lost)))
  }
  list(text = res$text, report = list(ran = TRUE, reason = NA_character_, kept = TRUE,
                                      added = integer(0), lost = integer(0)))
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
render_studies <- function(used, hide = character(0)) {
  meta <- c("document", "document_id", "status", "duplicate_of", "error",
            "n_filled", "n_unverified", "conflicts", "study")
  fields <- setdiff(names(used), c(meta, hide))
  vapply(seq_len(nrow(used)), function(i) {
    vals <- vapply(fields, function(f) {
      v <- used[[f]][i]
      if (is.na(v)) sprintf("%s: not reported", f) else sprintf("%s: %s", f, as_chr1(v))
    }, character(1), USE.NAMES = FALSE)
    # The study NUMBER and nothing else. The filename used to be here, and
    # academic PDFs are routinely called "Smith2019_CognitiveLoad.pdf" -- which
    # hands a model asked to cite `[study 1]` an author and a year anyway,
    # through the one field nobody thought of as bibliographic. `$citations`
    # resolves the number back to its document for the audit; the writing model
    # has no use for it.
    paste0("[study ", used$study[i], "]\n", paste(vals, collapse = "\n"))
  }, character(1), USE.NAMES = FALSE)
}

#' @noRd
synth_section <- function(heading, brief, question, rendered, used, client, spec, trace,
                          style = NULL) {
  system_prompt <- sprintf(.gr_prompts$synthesise_system, heading)
  # Appended rather than replacing: the register is how it is written, not what
  # it may say, and the rules above about citing and not inventing hold whatever
  # voice is asked for.
  if (is_nonblank(style)) system_prompt <- paste0(system_prompt, " Register: ", as_chr1(style))
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
append_references <- function(body, references) {
  if (!length(references)) return(body)
  paste0(body, "\n\n## References\n\n", paste(references, collapse = "\n"))
}

#' @noRd
synth_empty_citations <- function() {
  data.frame(section = character(0), study = integer(0), document = character(0),
             document_id = character(0), stringsAsFactors = FALSE)
}
