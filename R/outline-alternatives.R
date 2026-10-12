# outline-alternatives.R -- more than one way to organise the same claims.
#
# WHY THIS FILE EXISTS
# gr_outline() turns a review's structure from the author's hypothesis into a
# finding, but it found one structure: a single call, a single grouping, handed
# back to accept or edit. The grouping IS the review's argument, and one
# proposal gives the author nothing to weigh it against. Asking again does not
# help by itself. The default model is a reasoning model that takes no
# temperature, and the response cache keys on the prompt, so the same request
# sent twice comes back as one cached sample -- the second "alternative" is the
# first, byte for byte.
#
# So each further outline is its own request, shown the groupings proposed
# before it and asked for a different organising principle. Its prompt differs
# from every earlier one by construction, which is what makes it a new sample
# rather than a cache hit, and each goes through the same verification the
# first does. Whether it really differs is then measured here, by counting,
# rather than taken from the model: an alternative that groups the claims
# exactly as an earlier one did is dropped, and the comparison says so. Which
# outline to write from is the author's choice; nothing here ranks them.
#
# Why k requests rather than one request for k groupings: the first request is
# then byte for byte the one `alternatives = 1` sends, so the first outline is
# the same object and the same cached reply whatever `alternatives` says, and a
# recorded run still replays. One reply holding k groupings would need k times
# the reply limit under one cap -- with the reasoning allowance on top -- and a
# reply cut off there loses every grouping, the first included, where a
# separate request cut off loses only itself. And each request is shown the
# groupings it must differ from as they were verified, not as a model wrote
# them a moment earlier in the same reply. The price is the claims list sent
# again with each alternative, and on a reasoning model the thinking done
# again: input is the cheap side of a call, and the thinking is spent under
# each request's own limit, not one shared one.

#' The request for one further outline, added after the messages the first
#' outline was asked with.
#'
#' Kept here rather than in `.gr_prompts`: it is only ever sent as an addition
#' to the outline request, so it is part of the prompt gr_outline() builds and
#' never something gr_call() adds.
#' @noRd
.gr_outline_alt_prompt <- paste0(
  "These groupings of the same claims have been proposed already. Each lists its sections ",
  "and the claim numbers it put in them.\n\n",
  "<earlier_outlines>\n%s\n</earlier_outlines>\n\n",
  "Write proposal %d: group the claims by a DIFFERENT organising principle from each of these. ",
  "If they group by population, group by measure, by design or by the disagreement the claims ",
  "are about; whatever they group by, claims they put together should mostly be apart here. ",
  "Renaming an earlier proposal's sections, or moving a claim or two between them, is not a ",
  "different grouping. Every rule above still holds, and each `rationale` names what puts that ",
  "section's claims together under the new principle.")

#' The most outlines one gr_outline() call compares. Each is a request whose
#' prompt carries every earlier proposal, so the tenth is the longest of ten.
#' @noRd
.gr_outline_max_alternatives <- 10L

#' The earlier proposals, as the lines `.gr_outline_alt_prompt` shows them.
#'
#' The closing section is left out. Its heading is not the model's -- R adds it
#' after the reply -- and shown among the model's sections it invites the next
#' proposal to copy it, which finish_outline() would merge with the real one
#' under a message about duplicate headings.
#' @noRd
render_earlier_outlines <- function(outlines, numbers) {
  blocks <- vapply(seq_along(outlines), function(i) {
    o <- outlines[[i]]
    map <- attr(o, "claims")
    closing <- as_chr1(attr(o, "closing"), NA_character_)
    heads <- names(o)
    heads <- heads[is.na(closing) | heads != closing]
    lines <- vapply(heads, function(h) {
      ids <- sort(as.integer(map$claim_id[as.character(map$section) == h]))
      sprintf("- %s: claims %s", mark_utf8(h), paste(ids, collapse = ", "))
    }, character(1), USE.NAMES = FALSE)
    paste(c(sprintf("Proposal %d:", numbers[i]), lines), collapse = "\n")
  }, character(1))
  paste(blocks, collapse = "\n\n")
}

#' How far apart two groupings of the same claims are.
#'
#' The fraction of pairs of claims that one outline puts in the same section and
#' the other puts in different ones (one minus the Rand index). 0 is the same
#' grouping whatever its headings say; 1 is two groupings that agree about no
#' pair. Headings are only labels here: two outlines that put the same claims
#' together are one argument, renamed. A claim one map does not place counts as
#' a section of its own, so it agrees with no other claim.
#'
#' Counted from section sizes rather than by listing the pairs: a pair is
#' together in both outlines exactly when both claims share a section in each,
#' so the pairs together in one and not the other are those together in A,
#' plus those together in B, less twice those together in both. Listing them
#' is an n-by-n matrix per outline compared.
#' @noRd
outline_distance <- function(a, b) {
  ids <- sort(unique(c(as.integer(a$claim_id), as.integer(b$claim_id))))
  n <- length(ids)
  if (n < 2L) return(0)
  # Each claim's section as a number, a claim not placed getting one of its own.
  group <- function(map) {
    g <- as.character(map$section)[match(ids, as.integer(map$claim_id))]
    code <- match(g, unique(g[!is.na(g)]))
    miss <- is.na(code)
    code[miss] <- -seq_len(sum(miss))
    code
  }
  pairs_within <- function(g) {
    size <- tabulate(match(g, unique(g)))
    sum(size * (size - 1) / 2)
  }
  ga <- group(a)
  gb <- group(b)
  apart <- pairs_within(ga) + pairs_within(gb) - 2 * pairs_within(paste(ga, gb))
  apart / (n * (n - 1) / 2)
}

#' One row of the comparison: the outline's shape, and how far it lies from
#' the first proposal and from the nearest earlier one that was kept.
#'
#' Counts per section are comma-joined rather than a list column, because the
#' comparison is printed and written out, and a list column survives neither.
#' @noRd
outline_comparison_row <- function(alternative, o = NULL, cw = NULL, distance = NA_real_,
                                   nearest = NA_real_, kept = FALSE, note = NA_character_) {
  if (is.null(o)) {
    return(data.frame(alternative = as.integer(alternative), sections = NA_integer_,
                      claims_per_section = NA_character_, contested_per_section = NA_character_,
                      distance = NA_real_, nearest = NA_real_, kept = FALSE,
                      note = as_chr1(note, NA_character_), headings = NA_character_,
                      stringsAsFactors = FALSE))
  }
  map <- attr(o, "claims")
  heads <- names(o)
  contested <- cw$claim_id[cw$n_contradict > 0L]
  in_sec <- function(h) as.integer(map$claim_id[as.character(map$section) == h])
  per <- vapply(heads, function(h) length(in_sec(h)), integer(1), USE.NAMES = FALSE)
  con <- vapply(heads, function(h) sum(in_sec(h) %in% contested), integer(1), USE.NAMES = FALSE)
  data.frame(alternative = as.integer(alternative), sections = length(heads),
             claims_per_section = paste(per, collapse = ", "),
             contested_per_section = paste(con, collapse = ", "),
             distance = round(distance, 3), nearest = round(nearest, 3), kept = kept,
             note = as_chr1(note, NA_character_),
             # " | ", not a comma: a heading names what a section argues, and
             # "Dose, not design, explains the split" is one heading.
             headings = paste(mark_utf8(heads), collapse = " | "),
             stringsAsFactors = FALSE)
}

#' Ask for the further outlines, verify each as the first was, and attach the
#' distinct ones to the first.
#'
#' `first` is gr_outline()'s result, `derived` whether it came from the model
#' (a call that failed leaves a one-section stand-in, which is nothing to
#' differ from), and `msgs` the messages it was asked with.
#' @noRd
outline_alternatives <- function(first, k, derived, msgs, cw, closing, max_sections,
                                 client, spec, trace) {
  kept <- list(first)
  kept_no <- 1L
  rows <- list(outline_comparison_row(1L, first, cw, distance = 0, kept = TRUE))
  skip <- function(from, why) {
    for (j in seq.int(from, k)) rows[[length(rows) + 1L]] <<- outline_comparison_row(j, note = why)
  }
  capped <- function() sprintf("not requested, as the run had reached its %s", cap_name(trace))
  if (nrow(cw) < 2L) {
    # Counted rather than asked: every grouping of one claim is the same one.
    skip(2L, "not requested, as one claim can be grouped only one way")
  } else if (!derived) {
    # The first is then the one-section stand-in, which is no proposal to
    # differ from. And what most often stops it -- the cap, or a reply limit
    # the claims overrun -- would stop a longer request just the same.
    skip(2L, if (!trace_can_call(trace)) capped() else
      "not requested, as the first outline did not come back and there was nothing to differ from")
  } else {
    for (j in seq.int(2L, k)) {
      if (!trace_can_call(trace)) {
        skip(j, capped())
        break
      }
      ask <- c(msgs, list(list(role = "user", content = sprintf(
        .gr_outline_alt_prompt, render_earlier_outlines(kept, kept_no), j))))
      res <- gr_call_json(client, ask, schema = .gr_outline_schema, schema_name = "outline",
                          model = spec$model, max_output = spec$max_answer_tokens,
                          temperature = spec$temperature, trace = trace,
                          label = sprintf("outline.derive.alt%d", j), effort = spec$effort)
      secs <- if (isTRUE(res$ok)) outline_rows(json_field(res$value, "sections", scalar = FALSE))
      if (is.null(secs) || !nrow(secs)) {
        # No stand-in, unlike the first: one section holding every claim is
        # the fallback for having no outline, not an alternative to one.
        rows[[length(rows) + 1L]] <- outline_comparison_row(j, note = if (isTRUE(res$ok))
          "dropped, as the reply held no usable sections" else
            sprintf("dropped, as the call %s",
                    call_failure(res$result, ask, spec$model, spec$max_answer_tokens,
                                 "`max_outline_tokens`")))
        next
      }
      # finish_outline()'s messages name no outline, and after the first's they
      # read as a second report on it.
      o <- withCallingHandlers(
        finish_outline(secs, cw, closing, max_sections),
        message = function(m) {
          gr_msg(sprintf("Outline alternative %d: %s", j, sub("\n$", "", conditionMessage(m))))
          invokeRestart("muffleMessage")
        })
      d <- vapply(kept, function(e) outline_distance(attr(e, "claims"), attr(o, "claims")),
                  numeric(1))
      same <- which(d == 0)
      if (length(same)) {
        rows[[length(rows) + 1L]] <- outline_comparison_row(
          j, o, cw, distance = d[1], nearest = 0, note = sprintf(
            "dropped, as it grouped the claims exactly as alternative %d did", kept_no[same[1]]))
        next
      }
      kept[[length(kept) + 1L]] <- o
      kept_no <- c(kept_no, j)
      rows[[length(rows) + 1L]] <- outline_comparison_row(j, o, cw, distance = d[1],
                                                          nearest = min(d), kept = TRUE)
    }
  }
  comparison <- do.call(rbind, rows)
  rownames(comparison) <- NULL
  short <- comparison[!comparison$kept, , drop = FALSE]
  if (nrow(short)) {
    gr_warn(sprintf(paste0("%d of the %d outlines asked for came back distinct. %s. ",
                           "attr(outline, \"comparison\") records each."),
                    length(kept_no), k,
                    paste(sprintf("Alternative %d: %s", short$alternative, short$note),
                          collapse = ". ")),
            class = "gr_outline_alternatives_short")
  }
  trace_note(trace, "outline.compare", list(
    asked = as.integer(k),
    kept = paste(kept_no, collapse = ", "),
    dropped = paste(short$alternative, collapse = ", "),
    distance = paste(sprintf("%.3f", comparison$distance[comparison$kept]), collapse = ", ")))

  marker <- function(j, distance) {
    data.frame(alternative = as.integer(j), of = as.integer(k), distinct = length(kept_no),
               distance = round(as.numeric(distance), 3), stringsAsFactors = FALSE)
  }
  alts <- lapply(seq_along(kept)[-1L], function(i) {
    o <- kept[[i]]
    attr(o, "alternative") <- marker(kept_no[i],
                                     comparison$distance[comparison$alternative == kept_no[i]])
    o
  })
  out <- first
  attr(out, "alternative") <- marker(1L, 0)
  attr(out, "comparison") <- comparison
  attr(out, "alternatives") <- alts
  out
}

#' Which of gr_outline()'s outlines this is, as gr_synthesise() records it, or
#' NULL for an outline that does not say (one alternative asked for, or one
#' written by hand).
#' @noRd
outline_choice <- function(outline) {
  a <- attr(outline, "alternative")
  if (!is.data.frame(a) || nrow(a) != 1L) return(NULL)
  j <- as_int1(a$alternative)
  if (is.na(j)) return(NULL)
  data.frame(alternative = j, of = as_int1(a$of), distinct = as_int1(a$distinct),
             distance = as_num1(a$distance), stringsAsFactors = FALSE)
}

#' Choose one of the outlines gr_outline() compared
#'
#' [gr_outline()] with `alternatives` above 1 returns its first proposal and
#' attaches the others it kept. This hands back the one you choose, with its own
#' claim assignment, rationale and closing section, so the headings, the claims
#' placed under each and the claims they were derived from stay one consistent
#' object. Copying `attr(, "claims")` from one outline onto another's headings
#' is what this avoids: [gr_synthesise()] refuses a claim placed under a
#' heading the outline does not have.
#'
#' Nothing here ranks the outlines. `attr(outline, "comparison")` lays them
#' side by side: how many sections each has, how many claims and how many
#' contested claims each section holds, and how far each lies from the first
#' proposal. Which grouping makes the review's argument is the author's call.
#'
#' The outline returned carries `attr(, "alternative")`, which says which one it
#' is. [gr_synthesise()] records it in `$outline_alternative` and in its trace,
#' and [gr_audit_report()] says which was used, so a review written from the
#' second proposal says so.
#'
#' @param outline A [gr_outline()] result made with `alternatives` above 1.
#' @param alternative Which outline, by its number in
#'   `attr(outline, "comparison")$alternative`: 1 is the first proposal.
#' @return The outline chosen, shaped like any [gr_outline()] result, without
#'   the alternatives and comparison attached.
#' @seealso [gr_outline()], [gr_synthesise()]
#' @export
#' @examples
#' tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok",
#'                   duplicate_of = NA_character_, n_filled = 1L, n_unverified = 0L,
#'                   conflicts = NA_character_, design = c("trial", "survey"),
#'                   stringsAsFactors = FALSE)
#' cl <- gr_mock_client(function(messages, params) {
#'   seen <- paste(unlist(lapply(messages, `[[`, "content")), collapse = " ")
#'   if (grepl("<earlier_outlines>", seen, fixed = TRUE)) {
#'     return(paste0('{"sections":[{"heading":"Trials find it","brief":"the trials",',
#'                   '"claims":[1],"rationale":"design"},{"heading":"Surveys do not",',
#'                   '"brief":"the surveys","claims":[2],"rationale":"design"}]}'))
#'   }
#'   if (grepl("<claims>", seen, fixed = TRUE)) {
#'     return(paste0('{"sections":[{"heading":"What it finds","brief":"the claims",',
#'                   '"claims":[1,2],"rationale":"one topic"}]}'))
#'   }
#'   paste0('{"claims":[{"claim":"It works in trials.","kind":"finding",',
#'          '"supported_by":[1],"contradicted_by":[],"moderator":null,"scope":null},',
#'          '{"claim":"Surveys find no effect.","kind":"finding",',
#'          '"supported_by":[2],"contradicted_by":[],"moderator":null,"scope":null}]}')
#' })
#' cm <- gr_claims(tab, question = "Does it work?", client = cl)
#' o <- gr_outline(cm, client = cl, alternatives = 2)
#' attr(o, "comparison")[, c("alternative", "sections", "claims_per_section", "distance")]
#' o2 <- gr_outline_pick(o, 2)
#' names(o2)
#' attr(o2, "claims")
gr_outline_pick <- function(outline, alternative) {
  cmp <- attr(outline, "comparison")
  if (!is.data.frame(cmp) || is.null(cmp$alternative) || is.null(cmp$kept)) {
    gr_abort(paste0("This outline carries no alternatives to choose from. gr_outline(claims, ",
                    "alternatives = 3) asks for them and attaches them to the outline it returns."),
             class = "gr_outline_no_alternative")
  }
  j <- if (is.numeric(alternative) && length(alternative) == 1L && !is.na(alternative) &&
           alternative == round(alternative)) as.integer(alternative) else NA_integer_
  if (is.na(j) || !j %in% cmp$alternative) {
    gr_abort(sprintf(paste0("`alternative` must be one of the numbers in ",
                            "attr(outline, \"comparison\")$alternative: %s."),
                     paste(cmp$alternative, collapse = ", ")),
             class = "gr_outline_no_alternative")
  }
  row <- cmp[cmp$alternative == j, , drop = FALSE]
  if (!isTRUE(row$kept[1])) {
    gr_abort(sprintf("Alternative %d was not kept, so there is no outline to choose: %s.",
                     j, as_chr1(row$note[1], "no reason recorded")),
             class = "gr_outline_no_alternative")
  }
  if (j == 1L) {
    out <- outline
    attr(out, "alternatives") <- NULL
    attr(out, "comparison") <- NULL
    return(out)
  }
  hit <- Filter(function(a) identical(outline_choice(a)$alternative, j),
                attr(outline, "alternatives") %||% list())
  if (!length(hit)) {
    gr_abort(sprintf(paste0("The comparison lists alternative %d as kept, but no outline numbered ",
                            "%d is attached to this one: its attributes were edited since ",
                            "gr_outline() made it."), j, j),
             class = "gr_outline_no_alternative")
  }
  hit[[1]]
}
