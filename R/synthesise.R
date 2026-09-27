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
# markers are parsed back out, and any pointing at a row that does not exist --
# or at a row the call that wrote it was not shown (with claims, or a section
# drafted in batches) -- is reported and marks the section partial. This is the
# same check `new_answer()`
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
#'   values say what that section has to cover. As [gr_protocol()]. A heading
#'   with a blank brief is dropped, unless the claim assignment (see `claims`)
#'   places claims in it: then its heading is its brief.
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
#'   name in the table, and near-misses (Smith for Smyth, 2019 for 2018) are
#'   both the errors that matter and the ones fuzzy matching forgives. A
#'   rendered citation is therefore a fact about the extraction rather than
#'   something the model asserted. `$sections$text_marked` and `$text_marked`
#'   keep the marker form so the check can be re-run on the published prose.
#' @param bib Which columns carry bibliographic identity, as a named list of
#'   `citation`, `authors`, `year`, `title`, `venue`, `doi`. Omitted, the
#'   conventional names are looked for, which is why
#'   `gr_protocols("bibliography")` works without configuration: `citation`
#'   (or `cite`, `citation_key`, `cite_key`), `authors` (or `author`), `year`,
#'   `title`, `venue` (or `journal`) and `doi`, in any case. Names that are as
#'   often findings -- `date`, `source`, `url`, `published`, `publication` --
#'   are not taken for bibliographic ones unless named here. A column found
#'   this way is withheld from the writing prompts and used for the citations
#'   and references; give a role as `NA` to keep a column of that name as a
#'   finding (`bib = list(venue = NA)`). The most
#'   reliable route is a `citation` field asked for during extraction: parsing
#'   an arbitrary author list is a heuristic, and where it cannot be done
#'   confidently the run falls back to markers rather than printing a name that
#'   may be wrong.
#' @param style A register instruction appended to the writing prompts, such as
#'   `"formal academic; hedge claims; past tense for findings"`. It governs how
#'   sections are written, never what they may say: the rules about citing every
#'   claim and inventing nothing hold whatever voice is asked for.
#' @param coherence Revision passes over the finished draft: `TRUE` for all
#'   three, `FALSE` for none, or any of `"structure"` (reorder and merge),
#'   `"cut"` (remove repetition) and `"register"` (polish sentences) by name.
#'   Each pass is forbidden from doing the others' job, and each is discarded
#'   (with `$draft` kept) if it changed the citations, arrived truncated,
#'   or strengthened a claim. Off by default: each is a call, and each is a
#'   chance for a model to touch finished prose.
#' @param claims A [gr_claims()] result. With it each section argues that
#'   section's claims and sees only the studies those claims rest on, rather
#'   than being handed every study and writing a paragraph per row. Needs an
#'   `outline` from [gr_outline()], which carries the assignment, and it must be
#'   an outline derived from these same claims: the run stops
#'   (`gr_claims_mismatch`) when the outline says it came from other claims,
#'   and (`gr_claims_unplaced`) when the assignment places a claim in a heading
#'   the outline does not have -- a heading renamed after [gr_outline()], say;
#'   rename it in `attr(outline, "claims")$section` too. A section given no
#'   claims is written from the rows, as without `claims`, except the closing
#'   section, which is written from `gaps` alone and is left out when there
#'   are none. A section too large for one prompt is written a few claims at a
#'   time, each batch sent only its claims' studies, and merged.
#' @param gaps A [gr_gaps()] result, or lines of text. Given to the closing
#'   section [gr_outline()] named, `attr(outline, "closing")`, with an
#'   instruction to state those gaps and no others. When the outline has no
#'   such section they reach none, and a warning (`gr_gaps_unused`) says so.
#'   They are cut, at a line, to a quarter of the model's input room, with a
#'   warning (`gr_gaps_truncated`), and the closing section is then partial.
#' @param trace A [gr_trace()] to fold this write-up's accounting into, as in
#'   [gr_read_many()]. It is a parent, not this stage's counter: `$trace` is
#'   still the write-up's own, so `gr_options(max_calls =)` bounds the write-up
#'   rather than being spent by the screening that came before it.
#' @param references Append a `## References` section built from the studies the
#'   finished text actually cites. Alphabetical under `"author-year"`, numbered
#'   by study otherwise: the list is labelled by whatever the prose uses to
#'   point into it. The alphabetical order follows a fixed rule rather than the
#'   session's locale, so it is the same on every machine: it ignores case,
#'   accents and apostrophes, filing an accented name with its base letter.
#'   Two papers by the same authors in the same year are lettered (2019a,
#'   2019b) in title order; a key that does not end in a year is lettered
#'   after a hyphen ("in press-a"). Under `"author-year"`, an entry whose key
#'   came from a `citation` field leads with that key, so the prose can be
#'   followed to it.
#'
#' @return An object of class `gr_synthesis`:
#'   \describe{
#'     \item{`text`}{The whole write-up, as markdown, citations rendered and the
#'       reference list appended.}
#'     \item{`text_marked`}{The same document with `[study N]` markers intact:
#'       what the citation check ran on.}
#'     \item{`draft`}{The write-up before the coherence pass, for comparison.}
#'     \item{`references`}{The reference list, or `NULL`.}
#'     \item{`cite_style`}{The style actually used, which is not always the one
#'       asked for.}
#'     \item{`coherence`}{One row per revision pass, or `NULL` if none ran:
#'       `pass` (which of `"structure"`, `"cut"`, `"register"`), `ran`, `kept`,
#'       `lost` and `added` (citations, if the revision changed them), and
#'       `reason` for anything discarded.}
#'     \item{`unrendered`}{Studies a section cited honestly that `text` still
#'       shows as `[study N]` somewhere, because a kept revision moved or
#'       reworded a citation of the same study that another section made
#'       without being given it, and the two could no longer be told apart.
#'       Leaving both is the safe side of that doubt; `$draft` renders each
#'       section's own citations. Empty when nothing was in doubt.}
#'     \item{`sections`}{One row per section: `section`, `brief`, `text`,
#'       `n_cited`, `n_unknown` (citations to a row that does not exist),
#'       `n_unsupplied` (citations to a study that exists but was not given to
#'       the call that wrote them: with `claims`, a study not behind that
#'       section's claims; for a section written in batches, a study outside
#'       that batch, or one the merge of the batch drafts cited that no draft
#'       did. They are left as markers and kept out of the reference
#'       list, with one exception: once batch drafts are merged, a study that
#'       another batch of the same section was given and cited cannot be told
#'       apart, so it is rendered, and still counted), `n_unparsed` (brackets
#'       that open like a citation, such as `[studies 1 to 7]`, but cannot be
#'       read as one, so the studies they name were not checked), `n_truncated` (replies,
#'       including batch drafts and merges, that stopped at the reply limit),
#'       `partial`. A section is partial when any of those counts is non-zero
#'       (`n_cited` aside), when it came back empty, when a batch of studies
#'       was lost or not read, when its batch drafts could not be merged or
#'       one had to be cut to fit the merge, when (without `claims`) none of
#'       the studies of a batch it was drafted from is cited in it, when it
#'       could not be sent at all (its brief and gaps leave no room in the
#'       window), when the gaps given to it were cut, or, with `claims`, when
#'       it did not write up a claim it was given.}
#'     \item{`citations`}{Every citation, resolved to the row it points at, in
#'       long form: `section`, `study`, `document`, `document_id`.}
#'     \item{`studies`}{The rows that were written from, with the `study` number
#'       each was cited by.}
#'     \item{`claims`}{The [gr_claims()] result written from, or `NULL`. When
#'       its `$lost` names studies whose claims batch contributed nothing, the
#'       run warns (`gr_claims_partial`), and print() and the audit report say
#'       the review was written without them.}
#'     \item{`trace`}{As [gr_extract()].}
#'   }
#'
#' @section Which rows are used:
#' Rows that were never read (`status` `"failed"` or `"skipped"`) are left out,
#' and so are duplicates: a study counted twice is the error this whole
#' pipeline exists to avoid, and `gr_read_many()` has already marked them. The
#' number left out is reported by `print()` and is in `$skipped`.
#'
#' @section What it costs:
#' One call per section when the table fits one prompt, which is the usual case:
#' a hundred rows of a ten-field schema is a few thousand tokens. A table too
#' large for one prompt is written in batches and merged, so a section costs
#' batches + merges instead. Either way the cost is per *section*, not per
#' document: the expensive reading has already happened.
#'
#' The merge folds every batch draft into one reply of at most
#' `max_section_tokens`, the same limit each draft had, so a section drafted
#' from a very large table covers only what that one reply can hold. Nothing
#' checks that it cites every study -- a section need not -- but without
#' `claims` a batch none of whose studies the merged section cites is
#' reported (`gr_synth_batch_dropped`) and marks it partial. Raise
#' `max_section_tokens`, or divide the table between narrower sections.
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
  fingerprint <- attr(outline, "claims_fingerprint")
  # A section the claim assignment names is a section to write, whatever its
  # brief says. outline_vector() drops a heading with a blank brief, and
  # gr_outline() keeps one when the model left the brief empty, so the section
  # vanished here and took its claims with it.
  outline <- fill_assigned_briefs(outline, assign_map)
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
    if (!identical(study_identity(claims$studies), study_identity(used))) {
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
    if (is.null(assign_map$section) || is.null(assign_map$claim_id)) {
      gr_abort(paste0("attr(outline, \"claims\") must be a data frame with columns `section` and ",
                      "`claim_id`, as gr_outline() makes it. Without them no section finds its ",
                      "claims, and every section falls back to writing from rows."),
               class = "gr_no_claim_assignment")
    }
    # Claim numbers are positions in ONE gr_claims() result. An outline made
    # from another run -- a re-run, or a reconcile that ordered the claims
    # differently -- names the same numbers for different claims, and each
    # section argued someone else's. gr_outline() stamps the claims it was
    # given; nothing to compare against is an outline made some other way.
    if (!is.null(fingerprint) &&
        !identical(as_chr1(fingerprint), claims_fingerprint(claims$claims))) {
      gr_abort(paste0("This outline's claim assignment was made from different claims than the ",
                      "`claims` given, so its claim numbers name different claims. Derive the ",
                      "outline from these claims with gr_outline(claims)."),
               class = "gr_claims_mismatch")
    }
    # Every claim the assignment places has to land in a section that is
    # written. A claim assigned to a heading the outline no longer has -- one
    # renamed after gr_outline(), which `names<-` allows and keeps the
    # assignment through -- was never argued, the renamed section fell back to
    # writing from rows, and nothing said so.
    orphan <- !as.character(assign_map$section) %in% names(outline)
    if (any(orphan)) {
      gr_abort(sprintf(paste0("The outline assigns claim(s) %s to section(s) %s, which it does not ",
                              "contain, so those claims would never be written. A heading renamed ",
                              "after gr_outline() does this: rename it in ",
                              "attr(outline, \"claims\")$section too, or move the claims to a ",
                              "section the outline has."),
                       paste(sort(unique(assign_map$claim_id[orphan])), collapse = ", "),
                       paste0("'", unique(as.character(assign_map$section[orphan])), "'",
                              collapse = ", ")),
               class = "gr_claims_unplaced")
    }
    # gr_claims() warned when a batch came back with nothing, but that warning
    # is long gone by the time the review is written, and every section is
    # complete against claims that are not. Said again here, and by print()
    # and the audit report, because the review covers only the rest.
    lost <- claims$lost %||% integer(0)
    if (length(lost)) {
      gr_warn(sprintf(paste0("These claims were drawn with %d of %d studies contributing nothing: ",
                             "their batch was cut off, failed or was not sent (see `claims$lost`). ",
                             "The review is written from the rest. Run gr_claims() again to ",
                             "include them."),
                      length(lost), nrow(claims$studies)),
              class = "gr_claims_partial")
    }
  }

  client <- client %||% gr_client(model = model %||% gr_options("model"))
  # This function's own default, range check and name, not gr_read_spec()'s:
  # passed through raw, an NA fell back to max_answer_tokens' 1500, and any bad
  # value warned about a setting the caller never used.
  max_section_tokens <- clamp_warn(na_default(max_section_tokens, 1200L, "max_section_tokens"),
                                   16, 1e6, "max_section_tokens")
  spec <- gr_read_spec("stuff", model = model, temperature = temperature,
                       max_answer_tokens = max_section_tokens)
  # gr_read_spec() leaves the model NULL when none is named, and gr_budget() and
  # gr_call() fall back to different models on NULL: sections were sized for
  # gr_options("model") and sent to the client's, so a small model got a prompt
  # sized for a large one. Settled once here, so the sections, the merges and
  # the revision passes all size for, and ask, the same model.
  spec <- resolve_read_model(spec, client)
  # The parent, not the counter -- see as_parent_trace(). Running the write-up on
  # a trace that screening had already spent meant `trace_can_call()` refused
  # every section and the review came back as a list of empty headings.
  parent <- as_parent_trace(trace)
  trace <- gr_trace(meta = list(stage = "synthesise", question = question,
                                sections = length(outline), studies = nrow(used)))
  if (!is.null(parent)) on.exit(trace_absorb(parent, trace), add = TRUE)

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
  # caller never has to know which. Lines of text are one block: as several
  # strings they were not "nonblank" and reached no section.
  if (inherits(gaps, "gr_gaps") || is.data.frame(gaps)) gaps <- render_gaps(gaps)
  if (is.character(gaps) && length(gaps) > 1L) gaps <- as_chr1(gaps[nzchar(trimws(gaps))])
  if (!is_nonblank(gaps)) gaps <- NULL
  # Gaps reach exactly one section, the closing one gr_outline() marked: handing
  # them to every section makes every section recite them. An outline with no
  # such heading -- written by hand, from a protocol, or with the closing
  # heading renamed -- sent them nowhere while `$gaps` reported them.
  if (!is.null(gaps) && (is.na(closing) || !closing %in% names(outline))) {
    gr_warn(sprintf(paste0("`gaps` were given, but the outline has no closing section to state them ",
                           "(%s), so no section does. Name the heading that should with ",
                           "attr(outline, \"closing\") <- \"<heading>\"."),
                    if (is.na(closing)) "it names none" else sprintf("it names '%s'", closing)),
            class = "gr_gaps_unused")
    gaps <- NULL
  }
  # Bounded, because they are the closing section's fixed text and every batch
  # of it resends them. Cut at a line, and said: that section states only the
  # gaps it was given.
  gaps_cut <- FALSE
  if (!is.null(gaps)) {
    room <- tryCatch(gr_budget(spec$model, reserve_output = spec$max_answer_tokens)$input,
                     gr_budget_error = function(e) NA_integer_)
    fit <- if (is.na(room)) gaps else cap_gap_lines(gaps, max(1L, room %/% 4L))
    if (!identical(fit, gaps)) {
      gr_warn(sprintf(paste0("The gaps were cut to %d of %d line(s) to fit the model's window, so ",
                             "section '%s' states only those. It is marked partial."),
                      attr(fit, "kept"), attr(fit, "of"), closing),
              class = "gr_gaps_truncated")
      gaps <- as.character(fit)
      gaps_cut <- TRUE
    }
  }
  weights <- if (is.null(claims)) NULL else study_weight(used)
  # One latch for the whole write-up, so the ceiling is reported once rather
  # than once per section.
  capped <- new.env(parent = emptyenv())
  rows <- lapply(seq_along(outline), function(i) {
    heading <- names(outline)[[i]]
    ids <- if (is.null(assign_map)) integer(0) else
      as.integer(assign_map$claim_id[as.character(assign_map$section) == heading])
    is_closing <- !is.na(closing) && identical(heading, closing)
    # The closing section of a write-up from claims, with no claim placed in
    # it and no gaps to state, has nothing to say, and was written from every
    # row instead. Left out, unless it is all there is.
    if (!is.null(claims) && is_closing && !length(ids) && is.null(gaps) && length(outline) > 1L) {
      gr_msg(sprintf(paste0("Section '%s' was left out: no claim was placed in it and no gaps were ",
                            "given. Pass `gaps = gr_gaps(claims)` to write it."), heading))
      return(NULL)
    }
    gr_msg(sprintf("[%d/%d] %s", i, length(outline), heading))
    # A section that cannot be sized is that section's failure. Uncaught, a
    # budget error ended the write-up and threw away every section already
    # written and paid for.
    tryCatch(
      synth_section(heading, outline[[i]], question, rendered, used, client, spec, trace, style,
                    claims = claims, claim_ids = ids, weights = weights,
                    gaps = if (is_closing) gaps else NULL, capped = capped,
                    gaps_cut = is_closing && gaps_cut),
      gr_budget_error = function(e) synth_unwritten(heading, outline[[i]], ids, conditionMessage(e)))
  })
  rows <- rows[!vapply(rows, is.null, logical(1))]

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

  # A study a section cited without being shown it stays a visible marker, as a
  # row that does not exist does: rendering it would print the fabrication as a
  # fact about the extraction. Per section, because the same study may be cited
  # honestly by another section that WAS shown it.
  unsupplied <- lapply(rows, `[[`, "unsupplied")
  render <- function(x, leave) render_citations(x, used, keys, resolved, leave = leave)
  # Kept because it is what the citation check ran on, so the check can be
  # re-run on the published prose at any point.
  sections$text_marked <- sections$text
  sections$text <- unlist(Map(render, sections$text, unsupplied), use.names = FALSE)
  # The draft is its sections, so it is rendered from them, each with its own
  # exceptions; rendering the joined document is the same text otherwise.
  drafted <- synth_document(sections)
  # A kept revision is one text rather than sections, so the markers to leave
  # are found in it again by heading and by sentence, pass by pass (see
  # render_revised()). `left` is the studies no marker was rendered for
  # anywhere.
  unrendered <- integer(0)
  if (is.null(revised$text)) {
    published <- drafted
    left <- setdiff(unlist(unsupplied), citations$study)
  } else {
    # Each kept pass's text, followed one at a time; the last is `final_marked`.
    steps <- if (length(revised$steps)) revised$steps else final_marked
    rr <- render_revised(steps, sections$section, sections$text_marked, unsupplied, render)
    published <- rr$text
    left <- rr$left
    # An honest citation left as a marker is the safe side of a doubt, and
    # still a citation the reader loses: said here, by print() and in
    # `$unrendered`, since no section's own counts describe the revision.
    unrendered <- rr$unrendered
    if (length(unrendered) && !identical(resolved, "marker")) {
      gr_warn(sprintf(paste0("The kept revision moved or reworded a citation of study %s that a ",
                             "section made without being given it, so where an honest citation ",
                             "of it could no longer be told from that one it is left as a marker ",
                             "(see `$unrendered`; `$draft` renders each section's own)."),
                      paste(unrendered, collapse = ", ")),
              class = "gr_synth_unrendered")
    }
  }

  # The reference list follows what the FINISHED text cites, not what the
  # sections cited before revision. A reference list carrying a study the final
  # prose never mentions claims a breadth the review does not have. Nor does it
  # carry a study the text cites only by a marker left as written: that marker
  # is a fault the check reported, not a citation to follow up.
  cited <- setdiff(cited_ids(final_marked, "study"), left)
  refs <- if (isTRUE(references)) reference_list(used, keys, cited, cols, resolved) else NULL

  structure(list(
    text = append_references(published, refs),
    text_marked = final_marked,
    draft = append_references(drafted, refs),
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
    unrendered = if (identical(resolved, "marker")) integer(0) else unrendered,
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
    cat(sprintf("  %d row(s) left out: failed or read in part, duplicate, or nothing extracted\n", x$skipped))
  }
  # `%||%` for a synthesis saved before the column existed.
  unsup <- s$n_unsupplied %||% integer(nrow(s))
  unparsed <- s$n_unparsed %||% integer(nrow(s))
  cut <- s$n_truncated %||% integer(nrow(s))
  for (i in seq_len(nrow(s))) {
    flags <- c(if (s$n_unknown[i]) sprintf("%d TO A ROW THAT DOES NOT EXIST", s$n_unknown[i]),
               if (unsup[i]) sprintf("%d TO A STUDY IT WAS NOT GIVEN", unsup[i]),
               if (unparsed[i]) sprintf("%d THE CHECK CANNOT READ", unparsed[i]),
               if (cut[i]) "CUT OFF AT THE REPLY LIMIT")
    cat(sprintf("  %-22s %5d words, %d citation(s)%s\n",
                substr(s$section[i], 1, 22),
                lengths(strsplit(trimws(s$text[i]), "\\s+"))[1],
                s$n_cited[i],
                if (length(flags)) paste0(", ", paste(flags, collapse = ", "))
                else if (!s$n_cited[i]) ", NONE" else ""))
  }
  if (any(s$n_unknown > 0L)) {
    cat("  a citation to a row that does not exist is a fabrication; those sections are partial\n")
  }
  if (any(unsup > 0L)) {
    # "wherever it can be told apart": a merged batch draft citing a study that
    # another batch of the same section cited honestly is rendered; see
    # synth_section().
    cat(paste0("  so is a citation to a study the section was not given; those sections are ",
               "partial, and such a marker is left unrendered wherever it can be told apart ",
               "from an honest citation\n"))
  }
  if (any(unparsed > 0L)) {
    cat(paste0("  a citation the check cannot read names studies nobody checked; those sections ",
               "are partial\n"))
  }
  if (any(cut > 0L)) {
    cat("  a section cut off at the reply limit stops mid-thought; those sections are partial\n")
  }
  unr <- x$unrendered %||% integer(0)
  if (length(unr)) {
    cat(sprintf(paste0("  study %s: cited honestly, but left as a marker in the revised text where ",
                       "the revision made that citation impossible to tell from the one the check ",
                       "reported; see $draft\n"), paste(unr, collapse = ", ")))
  }
  lost <- x$claims$lost %||% integer(0)
  if (length(lost)) {
    cat(sprintf(paste0("  PARTIAL: written from claims that %d of %d studies contributed nothing to, ",
                       "their batch cut off, failed or not sent; see $claims$lost\n"),
                length(lost), nrow(x$studies)))
  }
  cost <- gr_trace_cost(x$trace)
  total <- if (nrow(cost)) sum(cost$usd) else 0
  # format_call_counts(), not `calls`: that counts embeddings requests too, and
  # a run that made any showed them as model calls, disagreeing with the trace.
  cat(sprintf("  this run: %s, %s\n", format_call_counts(x$trace),
              if (!nrow(cost)) "no cost recorded"
              else if (is.na(total)) "cost unknown (unpriced model)"
              else sprintf("$%.4f", total)))
  invisible(x)
}

# --- internals -------------------------------------------------------------

#' The system prompt for a closing section written from the gaps alone: the
#' closing section of a write-up from claims, when no claim was placed in it.
#' It is given no study, so it is told to name none.
#' @noRd
.gr_gaps_section_system <- paste0(
  "You write the '%s' section of a review: what the body of work it reviews does not cover. ",
  "You are given the gaps, computed by counting the studies' records, and no study records. ",
  "State those gaps in prose, grouping the ones that belong together, and say what each leaves ",
  "unanswered. Do not add a gap that is not listed, do not name or cite a study, and do not ",
  "guess at findings. Write prose for the section only -- no heading, no preamble, no closing ",
  "summary of what you just wrote.")

#' Give a blank brief to the heading it belongs to, for each section the
#' claim assignment names, so outline_vector() keeps the section. A heading
#' is its own brief already when an outline is a bare vector.
#' @noRd
fill_assigned_briefs <- function(outline, assign_map) {
  nms <- names(outline)
  if (is.null(assign_map) || !length(outline) || is.null(nms) || is.null(assign_map$section)) {
    return(outline)
  }
  v <- vapply(outline, as_chr1, character(1), USE.NAMES = FALSE)
  blank <- !nzchar(trimws(v)) & nzchar(trimws(nms)) & nms %in% as.character(assign_map$section)
  if (any(blank)) outline[blank] <- nms[blank]
  outline
}

#' What identifies a synthesis's rows, for comparing two sets of them.
#'
#' The document ids, else the document names. Else the rows themselves, as the
#' writing model is shown them: with neither column, or with every id NA, the
#' comparison was of character(0) with character(0), or of NAs with NAs, and
#' claims from any other table of the same length passed it.
#' @noRd
study_identity <- function(d) {
  for (col in c("document_id", "document")) {
    v <- d[[col]]
    if (!is.null(v) && !all(is.na(v))) return(as.character(v))
  }
  render_studies(d)
}

#' A fingerprint of a claims table: which claim each number is.
#'
#' gr_outline() stamps it on the outline as `attr(, "claims_fingerprint")`, and
#' gr_synthesise() compares it with the claims it is given, because an
#' assignment by claim number means nothing against another set of claims.
#' @noRd
claims_fingerprint <- function(cw) {
  gr_hash(list(claim_id = as.integer(cw$claim_id), claim = as.character(cw$claim)))
}

#' The first lines of `gaps` that fit `n` tokens, or `gaps` itself when all do;
#' a first line too long on its own is cut. `attr(, "kept")` and `attr(, "of")`
#' count the lines.
#' @noRd
cap_gap_lines <- function(gaps, n) {
  if (gr_count_tokens(gaps) <= n) return(gaps)
  lines <- strsplit(as_chr1(gaps), "\n", fixed = TRUE)[[1]]
  keep <- cumsum(gr_count_tokens(lines) + 1L) <= n
  out <- if (any(keep)) paste(lines[keep], collapse = "\n") else gr_truncate_tokens(lines[1], n)
  structure(out, kept = max(1L, sum(keep)), of = length(lines))
}

#' A section that could not be written at all, with the warning that says so.
#' @noRd
synth_unwritten <- function(heading, brief, claim_ids, why) {
  gr_warn(sprintf("Section '%s' was not written (%s). It is marked partial.", heading, why),
          class = "gr_synth_too_large")
  list(row = data.frame(section = heading, brief = as_chr1(brief), text = "",
                        n_cited = 0L, n_unknown = 0L, n_unsupplied = 0L, n_unparsed = 0L,
                        n_truncated = 0L, n_claims = length(claim_ids),
                        claims_missed = length(claim_ids), partial = TRUE,
                        stringsAsFactors = FALSE),
       unsupplied = integer(0), citations = NULL)
}

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
    gr_abort(paste0("No usable rows: every document either failed or was read only in part, ",
                    "was a duplicate of another, or had nothing extracted. There is nothing to ",
                    "write from."),
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
                          weights = NULL, gaps = NULL, capped = NULL, gaps_cut = FALSE) {
  # With claims, the section argues a list of claims and sees only the studies
  # those claims rest on. Without, it sees every study and writes from rows --
  # which is what produces "Smith (2019) found X. Garcia (2022) found Y."
  by_claims <- !is.null(claims) && length(claim_ids)
  # The closing section of a write-up from claims, holding no claim, states the
  # gaps and reads no study. Written from rows, it was handed every study under
  # the row-by-row prompt: prose about each study under a heading meant for
  # what the studies leave out, and at scale nearly half the write-up's prompt
  # tokens. gr_synthesise() gives gaps to that section alone, and leaves it out
  # when there are none.
  gaps_only <- !is.null(claims) && !length(claim_ids) && is_nonblank(gaps)
  # Every study, rendered: a section written a few claims at a time sends each
  # batch its own rows from here.
  all_rows <- rendered
  if (by_claims) {
    cw <- claims$claims[claims$claims$claim_id %in% claim_ids, , drop = FALSE]
    want <- unique(claims$support$study[claims$support$claim_id %in% claim_ids])
    # Indexing the ALREADY-rendered blocks rather than re-rendering, so the
    # bibliographic columns stay hidden by exactly the same rule.
    rendered <- rendered[used$study %in% want]
    blocks <- render_claims(cw, claims$support, weights, collapse = FALSE)
    claim_block <- paste(blocks, collapse = "\n\n")
  } else if (gaps_only) {
    rendered <- character(0)
  }
  system_prompt <- if (by_claims) .gr_prompts$claims_section_system else
    if (gaps_only) sprintf(.gr_gaps_section_system, heading) else
      sprintf(.gr_prompts$synthesise_system, heading)
  # Appended rather than replacing: the register is how it is written, not what
  # it may say, and the rules above about citing and not inventing hold whatever
  # voice is asked for.
  if (is_nonblank(style)) system_prompt <- paste0(system_prompt, " Register: ", as_chr1(style))
  base <- paste0("Review question: ", question,
                 "\n\nSection: ", heading, "\nThis section must cover: ", brief)
  # The gaps are COMPUTED from the table, so the section may state these and
  # nothing else as a gap. That is the whole reason they are computed.
  gap_block <- if (!is_nonblank(gaps)) "" else
    paste0("\n\n<gaps>\n", as_chr1(gaps), "\n</gaps>\n",
           "State only the gaps listed above. Do not add others.")
  with_claims <- function(block) paste0(base, "\n\n<claims>\n", block, "\n</claims>", gap_block)
  ask <- if (by_claims) with_claims(claim_block) else paste0(base, gap_block)
  # The brief is repeated after the studies (see below), and a restatement that
  # is not in the overhead is a prompt that overruns the window by exactly its
  # length. `ask` itself is sent once, hence "never" there; the tail is counted
  # on its own because it restates the section brief, not the whole of `ask`.
  again <- paste0("write the '", heading, "' section, which must cover: ", brief)
  overhead <- prompt_overhead(ask, system_prompt, "never") +
    if (identical(as_chr1(spec$restate, "auto"), "never")) 0L else
      gr_count_tokens(paste0("\n\nAgain, the question: ", again))
  # No room for the section's own text is this section's problem, not the
  # run's. gr_budget() raised and nothing caught it, so a claims block larger
  # than the window aborted the write-up with every section before it already
  # written, and paid for, thrown away. NULL here is a section that cannot be
  # sent whole; with claims it is written a few claims at a time instead.
  bud <- tryCatch(gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead),
                  gr_budget_error = function(e) NULL)

  body <- paste(rendered, collapse = "\n\n")
  lost_batches <- 0L
  # A latch of this function's own, not `trace$budget_stop`. tree_merge() calls
  # trace_can_call() too, and sets budget_stop while still returning usable text
  # -- so a run whose merge tripped the ceiling suppressed the warning for every
  # section after it, and those sections came back empty in silence.
  capped <- capped %||% new.env(parent = emptyenv())
  first_stop <- !isTRUE(capped$warned)
  capped_batches <- 0L
  # Replies that stopped at the reply limit. gr_call() keeps them ok = TRUE --
  # the text is what the model wrote -- so usable_text() accepts them, and a
  # section ending "However, the trial" came back complete.
  cut_off <- 0L
  merge_failed <- FALSE
  # Batch drafts the merge had to cut to fit before merging them.
  merge_cut <- 0L
  # The section's own text leaves no room for a study, and claims whose own
  # block of studies does not fit a prompt: neither can be written.
  too_large <- FALSE
  unwritable <- integer(0)
  # Studies a batch draft cited without being in its batch (or the merge cited
  # without any draft citing them), and studies a batch cited that it was
  # given. Only written in the batch path.
  batch_stray <- integer(0)
  batch_honest <- integer(0)
  # The studies each batch was sent, and whether its draft came back: what the
  # merged section is checked against below.
  batch_rows <- list()
  batch_read <- logical(0)
  text <- if (!trace_can_call(trace)) {
    # Every other stage checks the run's ceiling before it spends -- gr_claims(),
    # gr_outline(), revise_once() and every reader do. Synthesis did not, so a
    # run that had already hit `max_calls` kept writing sections, one call each.
    # An empty section is already marked partial below, which is the right
    # outcome: the ceiling was the user's instruction.
    if (first_stop) {
      capped$warned <- TRUE
      gr_warn(sprintf(paste0("Section '%s' was not written, nor is any section after it: the run ",
                             "reached its call or cost ceiling first. They are marked partial."),
                      heading),
              class = "gr_synth_capped")
    }
    ""
  } else if (!is.null(bud) && gr_count_tokens(body) <= bud$input) {
    msgs <- list(
      list(role = "system", content = system_prompt),
      list(role = "user", content = ask))
    # The section's job again after the studies, for the same reason
    # answer_messages() asks last: over several thousand tokens of table, an
    # instruction given once at the top is a long way from where the writing
    # happens. A section written from the gaps alone has no studies to send.
    if (!gaps_only) {
      msgs[[3L]] <- list(role = "user", content = paste0("<studies>\n", body, "\n</studies>",
                                                         restate_tail(body, again, spec$restate)))
    }
    res <- gr_call(client, msgs, model = spec$model, max_output = spec$max_answer_tokens,
                   temperature = spec$temperature, trace = trace, label = "synthesise.section")
    if (usable_text(res) && reply_cut_off(res)) cut_off <- cut_off + 1L
    if (usable_text(res)) res$text else ""
  } else {
    # Too many studies for one prompt: draft the section from batches and merge.
    # Cheaper alternatives -- take the first N rows, or summarise the table first
    # -- both drop studies without saying which, which is the one thing a review
    # may not do.
    #
    # With claims, a batch is some of the section's claims and the studies
    # behind them, not some of its studies under the whole claims block. The
    # block went into every batch as fixed overhead: batches shrank to a few
    # studies that each resent every claim, and past the window the section
    # could not be written at all.
    jobs <- if (by_claims) {
      room <- tryCatch(gr_budget(spec$model, reserve_output = spec$max_answer_tokens,
                                 overhead = prompt_overhead(with_claims(""), system_prompt,
                                                            "never"))$input,
                       gr_budget_error = function(e) NULL)
      if (is.null(room)) NULL else {
        cb <- claim_batches(blocks, claims$support, all_rows, room)
        unwritable <- attr(cb, "unwritable")
        lapply(cb, function(j) list(ask = with_claims(j$block), rows = j$rows))
      }
    } else if (!is.null(bud)) {
      groups <- synth_batches(rendered, bud$input)
      # Without claims `rendered` is every study in order, so a position in it
      # is a row of `used`.
      lapply(attr(groups, "index"), function(ix) list(ask = ask, rows = ix))
    }
    too_large <- is.null(jobs) || (!length(jobs) && !length(unwritable))
    parts <- vapply(jobs, function(j) {
      # Counted apart from a batch whose CALL failed. Both leave an empty part,
      # but "2 batches of studies failed" points at the model or the network,
      # and the cause here is the ceiling the caller set. `<<-` is right in this
      # nested function: it targets synth_section()'s frame, not the global one.
      if (!trace_can_call(trace)) { capped_batches <<- capped_batches + 1L; return("") }
      res <- gr_call(client, list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = j$ask),
        list(role = "user", content = paste0("<studies>\n", paste(all_rows[j$rows], collapse = "\n\n"),
                                             "\n</studies>"))
      ), model = spec$model, max_output = spec$max_answer_tokens,
         temperature = spec$temperature, trace = trace, label = "synthesise.batch")
      # Kept for the merge, as the one-call section keeps its text: part of a
      # batch's draft beats none of it. Counted, so the section says it.
      if (usable_text(res) && reply_cut_off(res)) cut_off <<- cut_off + 1L
      if (usable_text(res)) res$text else ""
    }, character(1), USE.NAMES = FALSE)
    # tree_merge() strips empty pieces, so a merge over the survivors reads
    # exactly like a merge over everything -- and with one survivor it returns
    # that piece unchanged, ok = TRUE, without making a call. The studies in the
    # failed batch were then simply absent from the write-up, with nothing on
    # the section, on `partial` or in print() to say so.
    # `<-`, not `<<-`: an if/else block shares the enclosing frame, so `<<-`
    # here would have written to the global environment and left this one at 0.
    lost_batches <- sum(!nzchar(parts)) - capped_batches
    batch_rows <- lapply(jobs, function(j) used$study[j$rows])
    batch_read <- nzchar(parts)
    # Each batch draft checked against what IT was given -- its rows, and any
    # study its prompt names (the brief, the gaps, its claims' own block) --
    # before the merge makes one text of them. Checked against the whole
    # section, a draft citing a study it never saw passed as known, was
    # rendered and went into the reference list.
    for (b in seq_along(parts)) {
      ids <- cited_ids(parts[[b]], "study")
      given <- union(batch_rows[[b]], cited_ids(jobs[[b]]$ask, "study"))
      batch_stray <- c(batch_stray, setdiff(intersect(ids, used$study), given))
      batch_honest <- c(batch_honest, intersect(ids, given))
    }
    # The merge is shown the section's brief and the drafts. With claims, not
    # the claims block: that is what did not fit.
    merge_ask <- if (by_claims) paste0(base, gap_block) else ask
    from <- length(trace$steps) + 1L
    m <- if (!length(parts)) list(text = "", ok = FALSE) else
      tryCatch(tree_merge(client, merge_ask, parts, spec, trace, label = "synthesise.merge",
                          system_prompt = system_prompt, kind = "draft"),
               # The merge sizes its prompt as the section did. No room is a
               # merge that did not happen, not a write-up that stops.
               gr_budget_error = function(e) list(text = merge_giveup(parts[nzchar(parts)], spec),
                                                  ok = FALSE, error = conditionMessage(e)))
    # tree_merge() accepts a merge reply cut off at the limit as it accepts any
    # other, and returns only the text, so the steps it recorded are the one
    # place that says whether a merge -- the last one, or one at a level below
    # it that the last one merged from -- stopped short.
    cut_off <- cut_off + trace_cut_off(trace, from, "synthesise.merge")
    # A merge that failed hands back the batch drafts joined and capped. The
    # section is not the merged draft it stands in for, whether or not every
    # batch was read; when none was, the lost batches already say so.
    merge_failed <- length(parts) > 0L && !isTRUE(m$ok) &&
      lost_batches + capped_batches < length(parts)
    # A draft too long for the merge's prompt, or a merge group that failed and
    # was passed up whole, is cut to fit before the next merge, and what it said
    # past the cut is gone from a merge that otherwise succeeded.
    merge_cut <- if (isTRUE(m$ok)) as_int1(m$truncated, 0L) else 0L
    # m$text on failure carries tree_merge()'s own "[merge failed; findings
    # above are truncated]" marker. Throwing it away for a raw paste() of the
    # parts discarded the one thing that said the section was incomplete.
    merged <- if (isTRUE(m$ok)) m$text else
      as_chr1(m$text, paste(parts[nzchar(parts)], collapse = "\n\n"))
    # The merge is a call too, and it is shown the brief and the drafts and no
    # row of the table. A study it cites that no draft cited and the brief does
    # not name is one it never saw, by the same rule as a batch draft's.
    # Unchecked, "Also seen in [study 7]" added by the merge passed as known,
    # was rendered and went into the reference list, with the section complete.
    in_drafts <- unique(unlist(lapply(parts, cited_ids, word = "study"), use.names = FALSE))
    batch_stray <- c(batch_stray, setdiff(intersect(cited_ids(merged, "study"), used$study),
                                          union(in_drafts, cited_ids(merge_ask, "study"))))
    merged
  }

  cited <- cited_ids(text, "study")
  # A bracket that opens like a citation and does not parse as one --
  # "[studies 1 to 7]", "[study one]" -- names ids nothing here can check.
  # Counting it as citing nothing is how a fabricated study passed as clean.
  unparsed <- unparsed_citations(text, "study")
  # Checked against what this section was SHOWN. With claims it sees only the
  # studies behind its claims, and a number it could not have seen is, by this
  # file's own definition, a citation to something never supplied. Checked
  # against the whole table it passed: it counted as known, was rendered as an
  # author-year citation and went into the reference list. `unsupplied` is kept
  # apart from `unknown` because the row does exist, and "a row that does not
  # exist" would misdescribe it. A section written from the gaps is shown no
  # study but one the gaps name.
  shown <- if (by_claims) want else if (gaps_only) cited_ids(ask, "study") else used$study
  unknown <- setdiff(cited, used$study)
  unsupplied <- setdiff(intersect(cited, used$study), shown)
  # The same check per batch, for a section drafted in batches (see above),
  # counting only what survived the merge into the section's text. A study
  # another batch of this section cited honestly cannot be told apart once the
  # drafts are merged, so it is rendered, but counted and marked partial; one
  # no batch was given, or one the merge cited and no draft did, stays a
  # marker, as `unsupplied` does.
  stray <- intersect(unique(batch_stray), cited)
  leave <- union(unsupplied, setdiff(stray, batch_honest))
  unsupplied <- union(unsupplied, stray)
  known <- cited[cited %in% shown & !cited %in% leave]
  hits <- match(known, used$study)
  # The check in the other direction, which the citation check cannot make: a
  # section handed four claims and citing none of the studies behind one of them
  # did not write that claim up. The outline promised it would. A claim that
  # could not be sent at all is not written up whatever else cites its studies.
  missed <- if (!by_claims) integer(0) else {
    claim_ids[claim_ids %in% unwritable | !vapply(claim_ids, function(id) {
      any(claims$support$study[claims$support$claim_id == id] %in% known)
    }, logical(1))]
  }
  # And for a section drafted in batches without claims: a batch whose draft
  # came back and not one of whose studies the section cites. The merge folds
  # several drafts into one reply of the same length, and a merge that
  # finished normally but kept only the first draft's studies -- 69 of 1000 --
  # read as a complete section. Not with claims, where a claim is what must be
  # covered, and `missed` checks that.
  dropped <- if (by_claims || !length(batch_rows)) 0L else
    sum(batch_read & !vapply(batch_rows, function(r) any(r %in% cited), logical(1)))
  if (too_large) {
    gr_warn(sprintf(paste0("Section '%s' was not written: its brief%s alone leave%s no room in ",
                           "the model's window for a single study. It is marked partial; shorten ",
                           "the brief, or use a model with a larger window."),
                    heading, if (nzchar(gap_block)) " and gaps" else "",
                    if (nzchar(gap_block)) "" else "s"),
            class = "gr_synth_too_large")
  }
  if (length(unwritable)) {
    gr_warn(sprintf(paste0("Section '%s': claim %s could not be sent, because the list of studies ",
                           "behind %s alone does not fit the model's window. %s marked missed and ",
                           "the section partial."),
                    heading, paste(unwritable, collapse = ", "),
                    if (length(unwritable) == 1L) "it" else "each",
                    if (length(unwritable) == 1L) "It is" else "They are"),
            class = "gr_synth_too_large")
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
  if (capped_batches > 0L && first_stop) {
    capped$warned <- TRUE
    gr_warn(sprintf(paste0("Section '%s': %d batch(es) of studies were not read, nor is any ",
                           "section after this one: the run reached its call or cost ceiling. ",
                           "They are marked partial."),
                    heading, capped_batches), class = "gr_synth_capped")
  }
  if (cut_off > 0L) {
    gr_warn(sprintf(paste0("Section '%s': %d repl%s stopped at the %d-token reply limit before ",
                           "finishing, so the section is incomplete. It is marked partial; raise ",
                           "`max_section_tokens`."),
                    heading, cut_off, if (cut_off == 1L) "y" else "ies", spec$max_answer_tokens),
            class = "gr_synth_truncated")
  }
  if (merge_failed) {
    gr_warn(sprintf(paste0("Section '%s': the batches of studies were drafted but not merged (%s), ",
                           "so the section is those drafts joined end to end, capped in length, ",
                           "rather than one account. It is marked partial."),
                    heading, as_chr1(m$error, "the merge failed")),
            class = "gr_synth_merge_failed")
  }
  if (merge_cut > 0L) {
    gr_warn(sprintf(paste0("Section '%s': %d batch draft(s) were too long for the merge and were ",
                           "cut to fit, so what they said past the cut is missing. It is marked ",
                           "partial."), heading, merge_cut),
            class = "gr_synth_merge_truncated")
  }
  if (dropped > 0L) {
    gr_warn(sprintf(paste0("Section '%s' was drafted in %d batches of studies and cites none of the ",
                           "studies in %d of them: merging the drafts into one reply of at most %d ",
                           "tokens left them out. It is marked partial; raise ",
                           "`max_section_tokens`, or narrow what the section covers."),
                    heading, length(batch_rows), dropped, spec$max_answer_tokens),
            class = "gr_synth_batch_dropped")
  }
  if (length(unparsed)) {
    gr_warn(sprintf(paste0("Section '%s' has %d citation(s) the check cannot read (%s), so the ",
                           "studies they name were not checked. The section is marked partial."),
                    heading, length(unparsed), paste(unparsed, collapse = ", ")),
            class = "gr_synth_unparsed")
  }
  list(
    row = data.frame(section = heading, brief = as_chr1(brief), text = as_chr1(text),
                     n_cited = length(known), n_unknown = length(unknown),
                     n_unsupplied = length(unsupplied),
                     n_unparsed = length(unparsed), n_truncated = cut_off,
                     # A section citing a row that is not in the table, or citing
                     # nothing at all, is not a section anyone should paste into a
                     # manuscript unread.
                     n_claims = length(claim_ids), claims_missed = length(missed),
                     # `capped_batches` counts here as well as `lost_batches`.
                     # Separating the two for the MESSAGE removed the only thing
                     # that marked the section partial, so a section that
                     # silently dropped a quarter of the corpus read as complete
                     # while the warning said it had been marked partial.
                     partial = length(unknown) > 0L || length(unsupplied) > 0L ||
                       !nzchar(trimws(text)) || lost_batches > 0L || capped_batches > 0L ||
                       length(missed) > 0L || length(unparsed) > 0L || cut_off > 0L ||
                       merge_failed || merge_cut > 0L || dropped > 0L || too_large ||
                       isTRUE(gaps_cut),
                     stringsAsFactors = FALSE),
    # Which markers in THIS section must stay markers when the prose is rendered.
    unsupplied = leave,
    citations = if (!length(known)) NULL else
      data.frame(section = heading, study = known,
                 # A table with no `document` column is one gr_synthesise()
                 # accepts; indexing NULL gave a zero-length column, and the
                 # frame, and the run, failed.
                 document = if (is.null(used$document)) NA_character_ else
                   as.character(used$document[hits]),
                 # NOT as_chr1(): it collapses a vector to ONE string with
                 # newlines between the elements, so every citation row got the
                 # same value -- all the ids, glued together. It is for scalars.
                 document_id = if (is.null(used$document_id)) NA_character_ else
                   as.character(used$document_id[hits]),
                 stringsAsFactors = FALSE))
}

#' How many replies recorded from step `from` on, under a label starting with
#' `prefix`, stopped at the reply limit.
#'
#' For calls made inside a helper that returns only text, such as tree_merge():
#' the trace records every reply's finish reason, normalised by gr_result(), and
#' worker traces are folded in, so this counts the same with or without
#' parallel merge levels.
#' @noRd
trace_cut_off <- function(trace, from, prefix) {
  if (!inherits(trace, "gr_trace") || from > length(trace$steps)) return(0L)
  steps <- trace$steps[seq.int(from, length(trace$steps))]
  sum(vapply(steps, function(st) {
    startsWith(as_chr1(st$label), prefix) && identical(as_chr1(st$finish_reason, ""), "length")
  }, logical(1)))
}

#' The claims a section must argue, in the order they earned.
#'
#' The tier is a sentence budget, and it is the whole of step "emphasis": a claim
#' resting on nine studies and one resting on a single pilot were getting the
#' same space, which is a claim about the literature that the literature does not
#' support.
#'
#' `collapse = FALSE` gives one block per claim, in that order, with the ids in
#' `attr(, "claim_id")`: the tiers are set across the whole section first, so a
#' section written a few claims at a time gives each claim the room it earned
#' among all of them.
#' @noRd
render_claims <- function(cw, support, weights, collapse = TRUE) {
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
  if (!collapse) return(structure(blocks, claim_id = cw$claim_id))
  paste(blocks, collapse = "\n\n")
}

#' Pack rendered studies into batches that fit `budget` input tokens.
#'
#' `max_n` caps the studies per batch, for a caller whose REPLY grows with the
#' batch (gr_claims()). `attr(, "index")` gives each batch's positions in
#' `rendered`, so a caller can say which studies a batch held -- which is what a
#' claim drawn from it may cite.
#' @noRd
synth_batches <- function(rendered, budget, max_n = Inf) {
  groups <- list(); index <- list(); buf <- character(0); at <- integer(0); tks <- 0L
  for (i in seq_along(rendered)) {
    p <- rendered[[i]]
    pt <- gr_count_tokens(p)
    if (length(buf) && (tks + pt > budget || length(buf) >= max_n)) {
      groups[[length(groups) + 1L]] <- buf; index[[length(index) + 1L]] <- at
      buf <- character(0); at <- integer(0); tks <- 0L
    }
    buf <- c(buf, p); at <- c(at, i); tks <- tks + pt
  }
  if (length(buf)) {
    groups[[length(groups) + 1L]] <- buf; index[[length(index) + 1L]] <- at
  }
  attr(groups, "index") <- index
  groups
}

#' Pack a section's claims into batches that each fit `budget` input tokens
#' together with the studies behind them.
#'
#' `blocks` is render_claims(collapse = FALSE): one block per claim, ranked.
#' In that order a claim joins the current batch while its block and the rows
#' it adds still fit, so a study behind several claims of one batch is sent
#' once, and each batch resends only its own claims. A claim too large for a
#' batch of its own goes out with its studies split over several batches,
#' each carrying its block; one whose block alone leaves no room comes back in
#' `attr(, "unwritable")`. Each batch is `block`, the text of its claims, and
#' `rows`, its study numbers, which are positions in `rendered`.
#' @noRd
claim_batches <- function(blocks, support, rendered, budget) {
  ids <- attr(blocks, "claim_id")
  # The blank line between blocks, and between rows.
  tb <- gr_count_tokens(blocks) + 2L
  tr <- gr_count_tokens(rendered) + 2L
  behind <- lapply(ids, function(id) sort(unique(support$study[support$claim_id == id])))
  out <- list()
  unwritable <- integer(0)
  cur <- integer(0); rows <- integer(0); cost <- 0
  flush <- function() {
    if (!length(cur)) return(invisible(NULL))
    block <- paste(blocks[cur], collapse = "\n\n")
    if (cost <= budget) {
      out[[length(out) + 1L]] <<- list(block = block, rows = rows)
    } else {
      # Only ever one claim: a second joins a batch only when it fits.
      room <- budget - tb[cur]
      if (room <= 0) {
        unwritable <<- c(unwritable, ids[cur])
      } else {
        g <- synth_batches(rendered[rows], room)
        for (ix in attr(g, "index")) out[[length(out) + 1L]] <<- list(block = block, rows = rows[ix])
      }
    }
    cur <<- integer(0); rows <<- integer(0); cost <<- 0
  }
  for (k in seq_along(ids)) {
    add <- function() tb[k] + sum(tr[setdiff(behind[[k]], rows)])
    if (length(cur) && cost + add() > budget) flush()
    cost <- cost + add()
    cur <- c(cur, k)
    rows <- sort(union(rows, behind[[k]]))
  }
  flush()
  attr(out, "unwritable") <- unwritable
  out
}

#' @noRd
synth_document <- function(sections) {
  paste(sprintf("## %s\n\n%s", sections$section, trimws(sections$text)), collapse = "\n\n")
}

#' Render a kept revision, leaving the markers the sections' checks reported.
#'
#' A revision is one text, and the check that caught a section citing a study
#' it was never given ran on the sections. Leaving only the studies that NO
#' section was given, as this once did, rendered the fabricated citation as an
#' author-year fact wherever another section had cited the same study honestly
#' -- which, with claims, is nearly every study -- while print() said the
#' marker had been left.
#'
#' So every sentence citing a reported study is followed from the draft
#' through each kept pass in turn. In the draft it is honest if its section was
#' given the study and reported if not. In each pass's text the sections are
#' found again by the `## ` headings the draft was assembled with, and a
#' sentence citing the study is
#'
#'   * honest if it is, word for word, a sentence only an honest one was,
#'     wherever it now stands;
#'   * reported under the heading of a section that reported the study, or
#'     under no heading of the draft (a preamble, a renamed section);
#'   * under the heading of a section given the study: honest or reported as
#'     the sentence it repeats word for word, and a reworded or new sentence is
#'     honest only if nothing says a reported marker could be in it -- the
#'     pass kept every reported marker where it can be seen (none deleted, none
#'     reworded away, no honest sentence taken into a reporting section), the
#'     section has no more such sentences than it lost honest ones, and the
#'     sentence has no more words in common with the reported sentences than
#'     with the honest ones it may replace.
#'
#' A marker is rendered only in an honest sentence. Following the passes one at
#' a time is what tells a cut that deleted the reported clause, then a
#' register pass that polished the honest one, from a pass that wrote the
#' reported clause into the honest sentence. Counting markers per section, as
#' this did before, took the deletion for a move and left the honest citation
#' as a marker with its reference dropped; and counts that did not change let
#' two sections trade claims unseen. Where this cannot tell, it leaves the
#' marker and says so in `unrendered`.
#'
#' `texts` is each kept pass's text in order, the last one published (one text
#' will do). `headings`, `marked` and `unsupplied` are per section: its
#' heading, its marked text, and the studies its check reported. `render(x,
#' leave)` renders one piece of text. Returns the rendered `text`; `left`, the
#' reported studies no marker was rendered for anywhere, which the reference
#' list omits; and `unrendered`, the studies a section cited honestly that are
#' left somewhere for want of telling that citation from the reported one.
#' @noRd
render_revised <- function(texts, headings, marked, unsupplied, render) {
  texts <- as.character(texts)
  text <- texts[length(texts)]
  reported <- unique(as.integer(unlist(unsupplied)))
  if (!length(reported)) {
    return(list(text = render(text, integer(0)), left = integer(0), unrendered = integer(0)))
  }
  secs <- unique(headings)
  own <- lapply(secs, function(h) unique(as.integer(unlist(unsupplied[headings == h]))))
  reports <- function(i, j) !is.na(i) && j %in% own[[i]]

  # Per sentence: the reported studies it cites, once per marker as the check
  # reads them (read once per distinct marker: a long review repeats a few
  # hundred); the sentence with its spacing evened out, which is what "word for
  # word" compares; and its words, for comparing a reworded one.
  grammar <- cite_grammar("study")$marker
  seen <- new.env(parent = emptyenv())
  marker_ids <- function(k) seen[[k]] %||% (seen[[k]] <- unique(cite_marker_ids(k, "study")))
  ids_of <- function(spans) lapply(spans, function(x) {
    if (!grepl("[", x, fixed = TRUE)) return(integer(0))
    mk <- regmatches(x, gregexpr(grammar, x, perl = TRUE, ignore.case = TRUE))[[1]]
    ids <- unlist(lapply(mk, marker_ids), use.names = FALSE)
    ids[ids %in% reported]
  })
  norm <- function(x) gsub("[[:space:]]+", " ", trimws(x))
  said <- new.env(parent = emptyenv())
  words <- function(x) {
    x <- unique(x[nzchar(x)])
    new <- x[!vapply(x, exists, logical(1), envir = said, inherits = FALSE)]
    if (length(new)) {
      low <- lower_text(to_utf8(gsub(grammar, " ", new, perl = TRUE, ignore.case = TRUE)))
      for (q in seq_along(new)) {
        w <- strsplit(low[q], "[^\\p{L}\\p{N}]+", perl = TRUE)[[1]]
        assign(new[q], unique(w[(nchar(w) >= 3L | grepl("[0-9]", w)) &
                                  !w %in% .gr_revise_stopwords]), envir = said)
      }
    }
    unique(unlist(mget(x, envir = said), use.names = FALSE))
  }
  # How many of `a` are not matched in `b`, counting repeats.
  missing_from <- function(a, b) {
    if (!length(a)) return(0L)
    ta <- table(a)
    sum(pmax(0L, as.integer(ta) - as.integer(table(factor(b, levels = names(ta))))))
  }
  # Which sentences cite each study, once per marker: by study, the positions.
  by_study <- function(ids) split(rep(seq_along(ids), lengths(ids)), unlist(ids))
  sentences <- function(span, sec) {
    out <- list(span = span, sec = sec, key = norm(span), ids = ids_of(span))
    # Every sentence that may need comparing, in one call rather than many.
    words(out$key[lengths(out$ids) > 0L])
    out
  }
  # A pass's text, cut before every heading line and then into sentences, so
  # pasting the spans back gives the text exactly.
  read_text <- function(x) {
    at <- gregexpr("(?m)^##[ \t]+[^\n]*", x, perl = TRUE)[[1]]
    at <- if (at[1] > 0L) as.integer(at) else integer(0)
    starts <- unique(c(1L, at))
    pieces <- substring(x, starts, c(starts[-1] - 1L, nchar(x)))
    first <- sub("(?s)\n.*$", "", pieces, perl = TRUE)
    head_of <- ifelse(starts %in% at,
                      trimws(sub("[ \t]+#+[ \t]*$", "", sub("^##[ \t]+", "", first))), NA_character_)
    sp <- lapply(pieces, sentence_spans)
    sentences(unlist(sp, use.names = FALSE), rep(match(head_of, trimws(secs)), lengths(sp)))
  }

  # The draft: every sentence citing a reported study, honest or reported.
  d_span <- lapply(secs, function(h) sentence_spans(paste(trimws(marked[headings == h]),
                                                          collapse = "\n\n")))
  state <- sentences(unlist(d_span, use.names = FALSE), rep(seq_along(secs), lengths(d_span)))
  state$status <- lapply(seq_along(state$key), function(s) {
    u <- unique(state$ids[[s]])
    stats::setNames(ifelse(vapply(u, reports, logical(1), i = state$sec[s]), "reported", "honest"),
                    u)
  })
  cited_honestly <- unique(unlist(lapply(state$status, function(st) names(st)[st == "honest"])))
  # The words of every sentence citing a study that has not been honest, in
  # the draft or any pass since. A pass sees only the text before it, but what
  # it rewords may have been reported a pass or two earlier.
  suspect_words <- list()
  remember <- function(st) {
    for (s in seq_along(st$key)) {
      for (jc in names(st$status[[s]])[st$status[[s]] != "honest"]) {
        suspect_words[[jc]] <<- union(suspect_words[[jc]], words(st$key[s]))
      }
    }
  }
  remember(state)

  # One kept pass: the statuses of `new`'s sentences, from `prev`'s.
  follow <- function(prev, new) {
    status <- lapply(new$ids, function(v) {
      u <- unique(v)
      stats::setNames(rep("honest", length(u)), u)
    })
    at_prev <- by_study(prev$ids)
    at_new <- by_study(new$ids)
    for (jc in intersect(names(at_new), as.character(reported))) {
      j <- as.integer(jc)
      pi <- at_prev[[jc]] %||% integer(0)
      pu <- unique(pi)
      pst <- vapply(pu, function(s) prev$status[[s]][[jc]], character(1))
      hon <- prev$key[pu][pst == "honest"]
      sus <- prev$key[pu][pst != "honest"]
      hon_only <- setdiff(hon, sus)
      # A sentence repeated word for word keeps its status; "unsure" wins over
      # "reported", so a doubt already declared is not forgotten.
      as_before <- function(key) {
        st <- pst[pst != "honest" & prev$key[pu] == key]
        if ("unsure" %in% st) "unsure" else "reported"
      }
      ni <- at_new[[jc]]
      nu <- unique(ni)
      cls <- vapply(nu, function(s) {
        key <- new$key[s]; i <- new$sec[s]
        if (key %in% hon_only) return("honest")
        if (is.na(i)) return(if (key %in% sus) as_before(key) else "unsure")
        if (j %in% own[[i]]) {
          if (key %in% sus) return(as_before(key))
          # Reworded under a reporting heading is the reported sentence --
          # unless an honest one had been standing there to be reworded.
          return(if (any(prev$sec[pu][pst == "honest"] %in% i)) "unsure" else "reported")
        }
        if (key %in% hon) return("honest")
        if (key %in% sus) return(as_before(key))
        "pending"
      }, character(1))
      # Whether a reported marker may have gone into a reworded sentence: fewer
      # markers are accounted for than the pass was given, or a reporting
      # section took in a sentence only an honest one wrote, which is what a
      # trade of claims leaves.
      kept <- sum(ni %in% nu[cls %in% c("reported", "unsure")])
      moved <- kept < sum(pi %in% pu[pst != "honest"]) ||
        any(vapply(nu[cls == "honest"], function(s) reports(new$sec[s], j), logical(1)))
      if (any(cls == "pending")) w_sus <- union(suspect_words[[jc]], words(sus))
      for (i in unique(new$sec[nu[cls == "pending"]])) {
        mine <- nu[cls == "pending" & new$sec[nu] %in% i]
        was <- prev$key[pu][pst == "honest" & prev$sec[pu] %in% i]
        now <- new$key[nu[new$sec[nu] %in% i]]
        ok <- !moved && length(mine) <= missing_from(was, now)
        w_was <- words(was)
        cls[match(mine, nu)] <- vapply(mine, function(s) {
          if (!ok) return("unsure")
          # Reworded: honest only if it reads no more like the reported
          # sentences than like the honest ones it may replace.
          w <- words(new$key[s])
          k <- sum(w %in% w_sus)
          if (k > 0L && k >= sum(w %in% w_was)) "unsure" else "honest"
        }, character(1))
      }
      for (q in seq_along(nu)) status[[nu[q]]][[jc]] <- cls[q]
    }
    new$status <- status
    new
  }
  for (x in texts) {
    state <- follow(state, read_text(x))
    remember(state)
  }

  leave <- lapply(state$status, function(st) as.integer(names(st)[st != "honest"]))
  rendered <- unique(unlist(lapply(state$status, function(st) names(st)[st == "honest"])))
  unsure <- unique(unlist(lapply(state$status, function(st) names(st)[st == "unsure"])))
  spans <- state$span
  if (!length(spans)) return(list(text = text, left = reported, unrendered = integer(0)))
  # Consecutive sentences leaving the same studies are rendered together, which
  # is every sentence of a text with nothing to leave.
  sig <- vapply(leave, function(v) paste(sort(v), collapse = ","), character(1))
  run <- cumsum(c(TRUE, sig[-1L] != sig[-length(sig)]))
  out <- vapply(split(seq_along(spans), run), function(ix) {
    render(paste(spans[ix], collapse = ""), leave[[ix[1L]]])
  }, character(1), USE.NAMES = FALSE)
  list(text = paste(out, collapse = ""),
       left = setdiff(reported, as.integer(rendered)),
       unrendered = sort(as.integer(intersect(unsure, cited_honestly))))
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
