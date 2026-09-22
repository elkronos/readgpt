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
#' @param coherence Revision passes over the finished draft: `TRUE` for all
#'   three, `FALSE` for none, or any of `"structure"` (reorder and merge),
#'   `"cut"` (remove repetition) and `"register"` (polish sentences) by name.
#'   Each pass is forbidden from doing the others' job, and each is discarded
#'   -- with `$draft` kept -- if it changed the citations, arrived truncated,
#'   or strengthened a claim. Off by default: each is a call, and each is a
#'   chance for a model to touch finished prose.
#' @param claims A [gr_claims()] result. With it each section argues that
#'   section's claims and sees only the studies those claims rest on, rather
#'   than being handed every study and writing a paragraph per row. Needs an
#'   `outline` from [gr_outline()], which carries the assignment.
#' @param gaps A [gr_gaps()] result, or lines of text. Given to the closing
#'   section [gr_outline()] named, with an instruction to state those gaps and
#'   no others.
#' @param trace A [gr_trace()] to record into, so the write-up joins the trace
#'   the screening and extraction used instead of starting a fourth one.
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
#'     \item{`coherence`}{One row per revision pass, or `NULL` if none ran:
#'       `pass` (which of `"structure"`, `"cut"`, `"register"`), `ran`, `kept`,
#'       `lost` and `added` (citations, if the revision changed them), and
#'       `reason` for anything discarded.}
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
                          references = TRUE, claims = NULL, gaps = NULL,
                          trace = NULL) {
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
    check_protocol_edited(protocol, "write against it")
    if (is.null(outline)) outline <- protocol$outline
    if (is.null(question)) question <- protocol$question
  }
  # Read BEFORE outline_vector(), which rebuilds the vector with setNames() and
  # drops every attribute -- including the claim assignment gr_outline() put
  # there. Losing it silently would take every section back to writing from rows.
  assign_map <- attr(outline, "claims")
  closing <- as_chr1(attr(outline, "closing"), NA_character_)
  outline <- outline_vector(outline)
  if (!length(outline)) {
    gr_abort(paste0("`outline` is empty. Give the sections to write, as a named character ",
                    "vector of heading = what it must cover, or a gr_protocol() carrying one."),
             class = "gr_no_outline")
  }
  if (!is_nonblank(question)) gr_abort("`question` must be a non-empty string.")

  used <- synth_studies(tab, include_unclear)
  keep <- attr(used, "keep")

  if (!is.null(claims)) {
    if (!inherits(claims, "gr_claims")) {
      gr_abort("`claims` must come from gr_claims().", class = "gr_bad_claims")
    }
    # THE guard for this whole layer. A claim number and a `[study N]` marker are
    # the same identifier, and they agree only because both sides derive it from
    # synth_studies() over the same table. Hand gr_claims() one extraction and
    # gr_synthesise() another -- or the same one with a different
    # `include_unclear` -- and every claim silently points at a different row.
    # Nothing downstream could detect it: the numbers are all valid, they just
    # mean other studies, and the review attributes findings to papers that do
    # not contain them.
    ident <- function(d) as.character(d$document_id %||% d$document)
    if (!identical(ident(claims$studies), ident(used))) {
      gr_abort(paste0("These claims were drawn from a different set of studies than this ",
                      "synthesis is writing from, so their study numbers point at different ",
                      "rows. Give gr_claims() and gr_synthesise() the same `extraction` and the ",
                      "same `include_unclear`."),
               class = "gr_claims_mismatch")
    }
    if (is.null(assign_map)) {
      gr_abort(paste0("`claims` was given but the outline does not say which claims belong to ",
                      "which section, so every section would fall back to writing from rows. ",
                      "Use gr_outline(claims) for the outline, or attach the assignment ",
                      "yourself with attr(outline, \"claims\") <- data.frame(section = , ",
                      "claim_id = )."),
               class = "gr_no_claim_assignment")
    }
  }

  client <- client %||% gr_client(model = model %||% gr_options("model"))
  spec <- gr_read_spec("stuff", model = model, temperature = temperature,
                       max_answer_tokens = max_section_tokens)
  # Given one, use it. A review is one run, and it used to produce three or four
  # unrelated traces -- so `gr_audit_report()` printed three cost rows, no single
  # figure for the review, and `gr_trace_save()` could only ever save a stage.
  trace <- trace %||% gr_trace(meta = list(stage = "synthesise", question = question,
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
  # gr_gaps() returns a table; a section wants lines. Accepting both means the
  # caller never has to know which.
  if (inherits(gaps, "gr_gaps") || is.data.frame(gaps)) gaps <- render_gaps(gaps)
  weights <- if (is.null(claims)) NULL else study_weight(used)
  rows <- lapply(seq_along(outline), function(i) {
    heading <- names(outline)[[i]]
    gr_msg(sprintf("[%d/%d] %s", i, length(outline), heading))
    ids <- if (is.null(assign_map)) integer(0) else
      as.integer(assign_map$claim_id[as.character(assign_map$section) == heading])
    synth_section(heading, outline[[i]], question, rendered, used, client, spec, trace, style,
                  claims = claims, claim_ids = ids, weights = weights,
                  # Gaps reach exactly one section, the one gr_outline() marked.
                  # Handing them to every section makes every section recite them.
                  gaps = if (!is.na(closing) && identical(heading, closing)) gaps else NULL)
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
  passes <- revise_passes(coherence)
  revised <- if (length(passes)) {
    synth_revise(marked, question, client, spec, trace, style, passes)
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
    claims = claims,
    gaps = gaps,
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

#' The studies a synthesis works from, numbered.
#'
#' Shared by [gr_synthesise()] and [gr_claims()] because the number IS the
#' identifier: a claim saying it rests on study 3 and a section citing
#' `[study 3]` have to mean the same row. Two copies of this filter would drift
#' the moment one of them learned about a new status value, and the symptom
#' would be a claims table that silently points at the wrong studies -- not an
#' error, just a review attributing findings to papers that do not contain them.
#' @noRd
synth_studies <- function(tab, include_unclear = FALSE) {
  keep <- synth_usable(tab, include_unclear)
  used <- tab[keep, , drop = FALSE]
  if (!nrow(used)) {
    gr_abort(paste0("No usable rows: every document either failed, was a duplicate of another, ",
                    "or had nothing extracted. There is nothing to write from."),
             class = "gr_no_studies")
  }
  used$study <- seq_len(nrow(used))
  attr(used, "keep") <- keep
  used
}

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

#' The fields `render_studies()` actually shows the model.
#'
#' Shared so that a check on what the model may name cannot drift from what the
#' model was shown. `claims_verify()` used the reserved-field list instead, which
#' differs from this in both directions, and accepted a moderator naming a column
#' that had been withheld from the prompt.
#' @noRd
study_fields <- function(used, hide = character(0)) {
  meta <- c("document", "document_id", "status", "duplicate_of", "error",
            "n_filled", "n_unverified", "conflicts", "study")
  setdiff(names(used), c(meta, hide))
}

#' The table as text the model can cite.
#'
#' One block per row, numbered, with the field values and the document it came
#' from. The number is what a section cites, and it is positional within THIS
#' synthesis -- `$studies` carries it alongside `document_id` so a citation can
#' always be resolved back to a document.
#' @noRd
render_studies <- function(used, hide = character(0)) {
  fields <- study_fields(used, hide)
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
                          style = NULL, claims = NULL, claim_ids = integer(0),
                          weights = NULL, gaps = NULL) {
  # With claims, the section argues a list of claims and sees only the studies
  # those claims rest on. Without, it sees every study and writes from rows --
  # which is what produces "Smith (2019) found X. Garcia (2022) found Y."
  by_claims <- !is.null(claims) && length(claim_ids)
  if (by_claims) {
    cw <- claims$claims[claims$claims$claim_id %in% claim_ids, , drop = FALSE]
    want <- unique(claims$support$study[claims$support$claim_id %in% claim_ids])
    # Indexing the ALREADY-rendered blocks rather than re-rendering, so the
    # bibliographic columns stay hidden by exactly the same rule.
    rendered <- rendered[used$study %in% want]
    claim_block <- render_claims(cw, claims$support, weights)
  }
  system_prompt <- if (by_claims) .gr_prompts$claims_section_system else
    sprintf(.gr_prompts$synthesise_system, heading)
  # Appended rather than replacing: the register is how it is written, not what
  # it may say, and the rules above about citing and not inventing hold whatever
  # voice is asked for.
  if (is_nonblank(style)) system_prompt <- paste0(system_prompt, " Register: ", as_chr1(style))
  ask <- paste0("Review question: ", question,
                "\n\nSection: ", heading, "\nThis section must cover: ", brief)
  if (by_claims) ask <- paste0(ask, "\n\n<claims>\n", claim_block, "\n</claims>")
  if (is_nonblank(gaps)) {
    # The gaps are COMPUTED from the table, so the section may state these and
    # nothing else as a gap. That is the whole reason they are computed.
    ask <- paste0(ask, "\n\n<gaps>\n", as_chr1(gaps), "\n</gaps>\n",
                  "State only the gaps listed above. Do not add others.")
  }
  # The brief is repeated after the studies (see below), and a restatement that
  # is not in the overhead is a prompt that overruns the window by exactly its
  # length. `ask` itself is sent once, hence "never" there; the tail is counted
  # on its own because it restates the section brief, not the whole of `ask`.
  again <- paste0("write the '", heading, "' section, which must cover: ", brief)
  overhead <- prompt_overhead(ask, system_prompt, "never") +
    if (identical(as_chr1(spec$restate, "auto"), "never")) 0L else
      gr_count_tokens(paste0("\n\nAgain, the question: ", again))
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)

  body <- paste(rendered, collapse = "\n\n")
  lost_batches <- 0L
  text <- if (!trace_can_call(trace)) {
    # Every other stage checks the run's ceiling before it spends -- gr_claims(),
    # gr_outline(), revise_once() and every reader do. Synthesis did not, so a
    # run that had already hit `max_calls` kept writing sections, one call each.
    # An empty section is already marked partial below, which is the right
    # outcome: the ceiling was the user's instruction.
    gr_warn(sprintf(paste0("Section '%s' was not written: the run reached its call or cost ",
                           "ceiling first. The section is marked partial."), heading),
            class = "gr_synth_capped")
    ""
  } else if (gr_count_tokens(body) <= bud$input) {
    res <- gr_call(client, list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = ask),
      # The section's job again after the studies, for the same reason
      # answer_messages() asks last: over several thousand tokens of table, an
      # instruction given once at the top is a long way from where the writing
      # happens.
      list(role = "user", content = paste0("<studies>\n", body, "\n</studies>",
                                           restate_tail(body, again, spec$restate)))
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
      # A batch that the ceiling stops is an empty part, which `lost_batches`
      # counts and warns about below -- the same visible degradation as a batch
      # whose call failed.
      if (!trace_can_call(trace)) return("")
      res <- gr_call(client, list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = ask),
        list(role = "user", content = paste0("<studies>\n", paste(g, collapse = "\n\n"),
                                             "\n</studies>"))
      ), model = spec$model, max_output = spec$max_answer_tokens,
         temperature = spec$temperature, trace = trace, label = "synthesise.batch")
      if (usable_text(res)) res$text else ""
    }, character(1), USE.NAMES = FALSE)
    # tree_merge() strips empty pieces, so a merge over the survivors reads
    # exactly like a merge over everything -- and with one survivor it returns
    # that piece unchanged, ok = TRUE, without making a call. The studies in the
    # failed batch were then simply absent from the write-up, with nothing on
    # the section, on `partial` or in print() to say so.
    # `<-`, not `<<-`: an if/else block shares the enclosing frame, so `<<-`
    # here would have written to the global environment and left this one at 0.
    lost_batches <- sum(!nzchar(parts))
    m <- tree_merge(client, ask, parts, spec, trace, label = "synthesise.merge",
                    system_prompt = system_prompt, kind = "draft")
    # m$text on failure carries tree_merge()'s own "[merge failed; findings
    # above are truncated]" marker. Throwing it away for a raw paste() of the
    # parts discarded the one thing that said the section was incomplete.
    if (isTRUE(m$ok)) m$text else as_chr1(m$text, paste(parts[nzchar(parts)], collapse = "\n\n"))
  }

  cited <- cited_ids(text, "study")
  known <- cited[cited %in% used$study]
  unknown <- setdiff(cited, used$study)
  hits <- match(known, used$study)
  # The check in the other direction, which the citation check cannot make: a
  # section handed four claims and citing none of the studies behind one of them
  # did not write that claim up. The outline promised it would.
  missed <- if (!by_claims) integer(0) else {
    claim_ids[!vapply(claim_ids, function(id) {
      any(claims$support$study[claims$support$claim_id == id] %in% known)
    }, logical(1))]
  }
  if (length(missed)) {
    gr_warn(sprintf(paste0("Section '%s' was given %d claim(s) and did not write up %d of them ",
                           "(claim %s). The section is marked partial."),
                    heading, length(claim_ids), length(missed),
                    paste(missed, collapse = ", ")),
            class = "gr_claims_missed")
  }
  if (lost_batches > 0L) {
    gr_warn(sprintf(paste0("Section '%s': %d batch(es) of studies failed, so the studies in ",
                           "them are missing from it. The section is marked partial."),
                    heading, lost_batches), class = "gr_synth_batch_failed")
  }
  list(
    row = data.frame(section = heading, brief = as_chr1(brief), text = as_chr1(text),
                     n_cited = length(known), n_unknown = length(unknown),
                     # A section citing a row that is not in the table, or citing
                     # nothing at all, is not a section anyone should paste into a
                     # manuscript unread.
                     n_claims = length(claim_ids), claims_missed = length(missed),
                     partial = length(unknown) > 0L || !nzchar(trimws(text)) ||
                       lost_batches > 0L || length(missed) > 0L,
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

#' The claims a section must argue, in the order they earned.
#'
#' The tier is a sentence budget, and it is the whole of step "emphasis": a claim
#' resting on nine studies and one resting on a single pilot were getting the
#' same space, which is a claim about the literature that the literature does not
#' support.
#' @noRd
render_claims <- function(cw, support, weights) {
  ord <- claim_order(cw, support, weights)
  cw <- cw[ord, , drop = FALSE]
  n <- nrow(cw)
  third <- max(1L, n %/% 3L)
  tier <- rep("one sentence", n)
  if (n <= 3L) {
    tier[] <- "two or three sentences"
  } else {
    tier[seq_len(third)] <- "two or three sentences"
    tier[seq(third + 1L, min(n, 2L * third))] <- "one or two sentences"
  }
  ids <- function(id, role) {
    v <- sort(support$study[support$claim_id == id & support$role == role])
    if (!length(v)) return(NULL)
    paste(sprintf("[study %d]", v), collapse = " ")
  }
  vapply(seq_len(n), function(i) {
    id <- cw$claim_id[i]
    paste(c(sprintf("[claim %d] (%s)", id, tier[i]),
            cw$claim[i],
            sprintf("supported by: %s", ids(id, "supports")),
            if (cw$n_contradict[i]) sprintf("contradicted by: %s", ids(id, "contradicts")),
            if (!is.na(cw$moderator[i])) sprintf("distinguished by: %s", cw$moderator[i]),
            if (cw$n_contradict[i] && is.na(cw$moderator[i]))
              "the disagreement is not explained by anything in the table",
            if (!is.na(cw$scope[i])) sprintf("scope: %s", cw$scope[i])),
          collapse = "\n")
  }, character(1), USE.NAMES = FALSE) -> blocks
  paste(blocks, collapse = "\n\n")
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
