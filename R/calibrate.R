# calibrate.R -- how often the screener is wrong, measured rather than assumed.
#
# WHY THIS FILE EXISTS
# Everything else here makes a run auditable: what was read, what was cited,
# what could not be verified. None of it says whether the screening DECISIONS
# were any good, and no amount of provenance substitutes for that. A screener
# that excludes a fifth of the eligible studies produces a beautifully audited
# review of the wrong corpus.
#
# The tempting substitute is agreement between two model passes. It measures the
# wrong thing. Two passes of one model share weights, priors and blind spots, so
# their errors are correlated: they agree most confidently exactly where they are
# both wrong, and a high figure would read as reliability while meaning
# self-consistency. Repeated sampling does help with careless reasoning, and it
# does nothing for a systematic misreading -- and which of those you have is not
# knowable from the passes themselves.
#
# Only a reference standard settles it. Screen a sample by hand, compare, and
# report what that sample can support. That is also the number a methods section
# needs, and the one a reader can argue with.
#
# WHY PREVALENCE MAKES ACCURACY USELESS HERE
# Inclusion rates in a real search run at a few per cent. A screener that
# excluded everything would be about 95% accurate and would find nothing. So
# accuracy is not reported. Sensitivity is, because a missed study is invisible
# and permanent; specificity is, because it is what the saved effort is made of;
# and Cohen's kappa is, because it is the one that notices the do-nothing
# screener.

#' The decisions a person can record, and what they mean here.
#' @noRd
.gr_reference_decisions <- c("include", "exclude")

#' The widest interval a one-stratum sample's headline rate may have and still
#' be called adequate: ten percentage points, a margin of five either way.
#' @noRd
.gr_frame_max_width <- 0.10

#' Draw a sample to screen by hand
#'
#' Picks documents out of a [gr_screen()] run for a person to judge without
#' seeing what the model said, and writes them to a CSV to fill in. Feeding the
#' completed file back to [gr_calibrate()] is what turns "we used an LLM" into a
#' claim with a number attached.
#'
#' @section Which rows to sample:
#' `of = "excluded"` is usually the right answer and is not the obvious one.
#' Sensitivity failures hide among the exclusions: a study the screener threw
#' away is gone, and nothing downstream will ever mention it, while the records
#' it kept are going to be read by a person anyway. Sampling everything at a
#' realistic inclusion rate spends most of the sample confirming exclusions that
#' were never in doubt, and leaves two or three positives to estimate
#' sensitivity from, which is no estimate at all.
#'
#' The design that matches how people actually work is to judge *every* record
#' the screener kept, plus a sample of what it discarded: `of = "kept"` with
#' `n = Inf`, and `of = "excluded"` with whatever hand-screening effort you have.
#' [gr_calibrate()] knows which frame it was given and will not compute a
#' corpus-wide figure from a sample that cannot support one.
#'
#' @section The file:
#' The CSV is written as UTF-8 with a byte-order mark, which is what makes
#' Excel open accented names and titles correctly; save it back as "CSV UTF-8".
#' [gr_calibrate()] reads it the same way in any locale, reads a sheet a
#' spreadsheet re-saved as Windows-1252 as that, and matches a row whose name
#' still does not agree by its `document_id`.
#'
#' @param screening A `gr_screening` from [gr_screen()].
#' @param n How many to draw: a whole number of at least 1. `Inf` takes the
#'   whole frame, as does any `n` larger than it.
#' @param of Which rows to sample from: `"excluded"`, `"kept"` (include and
#'   unclear), or `"all"`.
#' @param seed A seed for the draw, so it is reproducible: the sample is drawn
#'   after `set.seed(seed)` and your session's random number stream is put back
#'   afterwards. A calibration sample is part of the method and has to be
#'   re-drawable.
#' @param path Where to write the CSV. `NULL` returns the frame without writing.
#' @param blind Leave the model's decision and reason out of the file. On by
#'   default: a person shown the answer agrees with it, and the resulting figure
#'   measures nothing. A blind sheet does not name its frame either, since
#'   "excluded" on every row says what the model decided: `sampled_from` holds
#'   an opaque key that [gr_calibrate()] reads back as the frame and its size.
#'   With `blind = FALSE` the file has `model_decision`, `model_reason`, and the
#'   frame in plain words (`sampled_from`, `frame_n`, `screened_n`).
#' @return A data frame of class `gr_reference_frame`, invisibly when written.
#'   The `human_decision` column is empty and is yours to fill with `include` or
#'   `exclude`.
#' @seealso [gr_calibrate()], [gr_screen()]
#' @family corpus functions
#' @export
#' @examples
#' tab <- data.frame(document = paste0("d", 1:6, ".pdf"),
#'                   decision = c("include", "exclude", "exclude",
#'                                "unclear", "exclude", "include"),
#'                   reason = "because", stringsAsFactors = FALSE)
#' gr_reference(structure(list(table = tab), class = "gr_screening"),
#'              n = 3, of = "excluded", seed = 1)
gr_reference <- function(screening, n = 50, of = c("excluded", "kept", "all"),
                         seed = NULL, path = NULL, blind = TRUE) {
  of <- match.arg(of)
  # A number, compared as a number, for the reason min_positives is below:
  # `"5" >= 30` is TRUE because R compares as strings, so n = "5" from a config
  # file took the whole frame, and NA stopped with "missing value where
  # TRUE/FALSE needed".
  n_num <- suppressWarnings(as.numeric(n))
  if (length(n_num) != 1L || is.na(n_num) || n_num < 1 ||
      (is.finite(n_num) && n_num != floor(n_num))) {
    gr_abort(sprintf("`n` must be a single whole number of at least 1, or Inf; got %s.",
                     paste(format(n), collapse = ", ")), class = "gr_bad_setting")
  }
  n <- n_num
  tab <- as_screening_table(screening)
  # A row with no decision was never read. It is not an exclusion and cannot be
  # compared with a human judgement of the document's eligibility.
  judged <- !is.na(tab$decision)
  frame <- switch(of,
                  excluded = judged & tab$decision == "exclude",
                  kept     = judged & tab$decision %in% c("include", "unclear"),
                  all      = judged)
  pool <- tab[frame, , drop = FALSE]
  if (!nrow(pool)) {
    gr_abort(sprintf("No rows to sample: nothing in this run was %s.",
                     switch(of, excluded = "excluded", kept = "kept", "screened")),
             class = "gr_no_reference_frame")
  }
  take <- if (is.infinite(n) || n >= nrow(pool)) seq_len(nrow(pool)) else {
    idx <- seq_len(nrow(pool))
    # with_private_rng(), not withr::with_seed(): withr is only suggested, so a
    # default install stopped here with "there is no package called 'withr'".
    # It is the same draw -- set.seed() under the session's RNG kind, the
    # caller's stream restored afterwards -- so a seed keeps its sample.
    if (is.null(seed)) sample(idx, n) else with_private_rng(seed, sample(idx, n))
  }
  out <- pool[sort(take), , drop = FALSE]

  keep <- intersect(c("document", "document_id", "title", "authors", "year"), names(out))
  ref <- out[, keep, drop = FALSE]
  if (!blind) {
    ref$model_decision <- out$decision
    ref$model_reason <- out$reason
  }
  ref$human_decision <- NA_character_
  ref$human_note <- NA_character_
  # As a COLUMN, not only an attribute, because the file is the thing that gets
  # emailed, opened in Excel and read back a fortnight later, and an attribute
  # does not survive any of that. Without it a sample of exclusions comes back
  # looking like a sample of everything, and gr_calibrate() would then report a
  # sensitivity of 0% and a specificity of 100% -- both artifacts of the frame.
  # screened_n as a column for the same reason, which it was missing: without
  # it gr_calibrate() fell back to nrow(tab), which counts rows the screener
  # never decided, and the printed denominator quietly changed meaning between
  # the in-memory frame and the file read back.
  #
  # In a blind sheet the three travel as one opaque key. Written out plainly,
  # sampled_from = "excluded" on every row told the person filling it in that
  # the model had rejected each record -- the answer `blind` exists to keep from
  # them -- and frame_n against screened_n said the same by its ratio.
  if (blind) {
    ref$sampled_from <- frame_key(of, nrow(pool), sum(judged), out$document)
  } else {
    ref$sampled_from <- of
    ref$frame_n <- nrow(pool)
    ref$screened_n <- sum(judged)
  }
  rownames(ref) <- NULL
  attr(ref, "of") <- of
  attr(ref, "frame_n") <- nrow(pool)
  attr(ref, "screened_n") <- sum(judged)
  attr(ref, "seed") <- seed
  class(ref) <- c("gr_reference_frame", "data.frame")
  # See `[.gr_reference_frame` below: subsetting a classed data frame keeps the
  # class and drops every other attribute, so a column subset used to hand
  # gr_calibrate() a frame that still claimed to know where it came from.

  if (!is.null(path)) {
    write_reference_csv(ref, path)
    gr_msg(sprintf(paste0("Wrote %d row(s) to '%s'. Fill in `human_decision` with 'include' or ",
                          "'exclude', then read it back with gr_calibrate()."),
                   nrow(ref), path))
    return(invisible(ref))
  }
  ref
}

#' @noRd
as_screening_table <- function(screening) {
  tab <- if (inherits(screening, "gr_screening")) screening$table else screening
  if (!is.data.frame(tab) || !nrow(tab)) {
    gr_abort("`screening` must be a gr_screening, or a data frame shaped like its $table.",
             class = "gr_bad_screening")
  }
  if (is.null(tab$document) || is.null(tab$decision)) {
    gr_abort("`screening` needs `document` and `decision` columns.", class = "gr_bad_screening")
  }
  # Read back the way the human decisions are. A table saved with
  # write.csv(na = "") -- the convention gr_reference() uses -- comes back with
  # "" where a document was never read, and a spreadsheet capitalises; kept
  # as they were, every value other than "include" and "unclear" counted as an
  # exclusion, so unread documents a person judged eligible were listed in
  # `missed` as studies the screener threw away.
  # to_utf8() first: lower_text() needs text it can decode, and it turns NA
  # into "", which is then NA again below.
  dec <- lower_text(trimws(to_utf8(tab$decision)))
  dec[!nzchar(dec)] <- NA_character_
  bad <- !is.na(dec) & !dec %in% c("include", "exclude", "unclear")
  if (any(bad)) {
    gr_abort(sprintf(paste0("`decision` must be 'include', 'exclude', 'unclear' or empty (never ",
                            "read); found %s."),
                     paste(sprintf("'%s'", utils::head(unique(dec[bad]), 5)), collapse = ", ")),
             class = "gr_bad_screening")
  }
  # A row the run could not read has no decision whatever the column says.
  if (!is.null(tab$status)) {
    st <- lower_text(trimws(to_utf8(tab$status)))
    dec[nzchar(st) & !st %in% c("ok", "restored", "duplicate")] <- NA_character_
  }
  tab$decision <- dec
  # Duplicates were decided once, under the row they repeat. Blank is not a
  # duplicate: read back from a CSV, a non-duplicate's NA is "".
  if (!is.null(tab$duplicate_of)) {
    dup <- as.character(tab$duplicate_of)
    tab <- tab[is.na(dup) | !nzchar(trimws(dup)), , drop = FALSE]
  }
  tab
}

#' Measure the screener against a hand-screened sample
#'
#' Compares [gr_screen()]'s decisions with a person's on the same documents, and
#' reports what that sample supports: how much of the eligible literature the
#' screener kept, how much of the irrelevant literature it removed, and how much
#' reading it saved. Each comes with an interval, and each is refused when the
#' sample is too small to say.
#'
#' @section Two sensitivities, and the gap between them:
#' `"unclear"` is a deferral, not a miss. A record the screener could not settle
#' goes to a person, so it is not lost, and counting it as a failure would
#' punish the screener for the one behaviour that makes it safe.
#'
#' So two figures are reported. **Sensitivity as deployed** asks what fraction of
#' the eligible studies survived: kept, whether by `"include"` or by
#' `"unclear"`. That is the number that matters, because it is the one where a
#' shortfall is permanent. **Strict sensitivity** asks what fraction were
#' actively included. The gap between them is the reading a person still has to
#' do, and reporting only the first would flatter a screener that defers
#' everything.
#'
#' @section Why accuracy is not reported:
#' Inclusion rates run at a few per cent, so a screener that excluded every
#' record would score around 95% accurate and find nothing. Cohen's kappa is
#' reported instead, because it is the statistic that notices.
#'
#' @section What a small sample cannot do:
#' Sensitivity is estimated from the eligible studies in the sample and from
#' nothing else. Twelve hand-screened records containing two eligible studies
#' estimate it from two observations, and "1.00" from two observations is not a
#' finding. The intervals are Wilson score intervals, which stay sensible at
#' zero and one where the textbook interval collapses to a point, and
#' `$adequate` says whether there was enough to support a claim at all.
#'
#' What "enough" means depends on the frame, because what the headline rate
#' rests on does. In an `"all"` sample it is sensitivity, estimated from the
#' eligible studies alone, so the sample needs `min_positives` of them. In an
#' `"excluded"` or `"kept"` sample the headline rate ("eligible among the
#' excluded", "eligible among those kept") is taken over every row sampled, so
#' a sample of 500 clean exclusions is a good one, not one resting on no
#' observations. There the sample is adequate when that rate's interval is at
#' most 10 percentage points wide, or when every record in the stratum was
#' judged (nothing is left to sample, and the rate is a count for this run).
#' The bar is a floor, not a verdict: 10 points is wide for an omission rate
#' near zero across a large pile of exclusions, and the projected count of lost
#' studies, printed with its interval, is the figure to judge that by.
#' `$adequacy$note` says in a sentence why a sample fell short.
#'
#' @section One row per document:
#' The reference is matched to the screening by document name, compared as
#' UTF-8 whatever the session's locale, and by `document_id` for a row whose
#' name still does not match. A document judged twice (two reviewers' sheets
#' stacked, say) is counted once when the copies agree, with a warning, and is
#' refused when they disagree: reconcile dual screening into one consensus
#' decision per document first.
#'
#' @param screening A `gr_screening` from [gr_screen()], or a data frame shaped
#'   like its `$table`, for instance read back from a CSV. Its `decision` must
#'   be `"include"`, `"exclude"`, `"unclear"` or empty (never read), in any
#'   case; a row whose `status` says it was not read counts as never read.
#' @param reference The completed frame from [gr_reference()], a path to the
#'   filled-in CSV, or any data frame with `document` and `human_decision`.
#' @param positive Which human decision counts as eligible: `"include"` or
#'   `"exclude"`.
#' @param of Which part of the screening run the reference was drawn from:
#'   `"excluded"`, `"kept"` or `"all"`; anything else is an error. Normally
#'   recovered from the file [gr_reference()] wrote; give it explicitly for a
#'   reference built by hand. Given over a file that records a different
#'   frame, it wins with a warning, and the file's frame size is not used.
#' @param min_positives For an `"all"` sample: below this many eligible studies
#'   the sensitivity estimate is reported but marked inadequate. A sample of
#'   one stratum is judged by its interval instead; see "What a small sample
#'   cannot do".
#' @return An object of class `gr_calibration`:
#'   \describe{
#'     \item{`counts`}{The confusion matrix, as kept/excluded by eligible/not.}
#'     \item{`metrics`}{One row per statistic: `estimate`, `lower`, `upper`, `n`.}
#'     \item{`missed`}{The eligible studies the screener excluded: the rows
#'       themselves, because a list of the misses says more than a rate.}
#'     \item{`disagreements`}{Every row where the two differ, in either
#'       direction.}
#'     \item{`adequate`}{Whether the sample supports a claim from its frame's
#'       headline rate: sensitivity in an `"all"` sample, the frame's own rate
#'       in an `"excluded"` or `"kept"` one.}
#'     \item{`adequacy`}{How that was judged: `rule` (`"positives"`,
#'       `"interval"` or `"whole frame"`), `bar`, `width`, and `note`, a
#'       sentence saying why the sample fell short (`NA` when it did not).}
#'     \item{`frame`}{Which rows the sample was drawn from, and how many.}
#'   }
#' @seealso [gr_reference()], [gr_screen()], [gr_audit_report()]
#' @family corpus functions
#' @export
#' @examples
#' tab <- data.frame(document = paste0("d", 1:8, ".pdf"),
#'                   decision = c("include", "include", "unclear", "exclude",
#'                                "exclude", "exclude", "include", "exclude"),
#'                   stringsAsFactors = FALSE)
#' ref <- data.frame(document = paste0("d", 1:8, ".pdf"),
#'                   human_decision = c("include", "exclude", "include", "exclude",
#'                                      "exclude", "include", "include", "exclude"),
#'                   stringsAsFactors = FALSE)
#' gr_calibrate(structure(list(table = tab), class = "gr_screening"), ref)
gr_calibrate <- function(screening, reference, positive = "include", min_positives = 10L,
                         of = NULL) {
  tab <- as_screening_table(screening)
  ref <- read_reference(reference)
  # Normalised the way the human decisions are. A positive the decisions can
  # never equal ("Include") made every study ineligible without a word.
  positive <- lower_text(trimws(as_chr1(positive, "")))
  if (!positive %in% .gr_reference_decisions) {
    gr_abort(sprintf("`positive` must be 'include' or 'exclude', not '%s'.", positive),
             class = "gr_bad_setting")
  }

  hit <- reference_rows(ref, tab)
  if (anyNA(hit)) {
    gr_abort(sprintf(paste0("%d row(s) in the reference name documents this screening run does ",
                            "not contain, starting with '%s'. A calibration compares the two on ",
                            "the same documents; a name that is in one and not the other means ",
                            "they are not the same run, or that the names were changed on the ",
                            "way (retyped, or re-saved by a spreadsheet in another encoding)."),
                     sum(is.na(hit)), ref$document[which(is.na(hit))[1]]),
             class = "gr_reference_mismatch")
  }
  model <- tab$decision[hit]
  human <- lower_text(trimws(to_utf8(ref$human_decision)))

  blank <- !nzchar(human)
  if (any(blank)) {
    gr_warn(sprintf(paste0("%d of %d reference row(s) have no `human_decision` and are left out. ",
                           "An unfilled row is not an exclusion."), sum(blank), length(human)),
            class = "gr_reference_incomplete")
  }
  bad <- !blank & !human %in% .gr_reference_decisions
  if (any(bad)) {
    gr_abort(sprintf("`human_decision` must be 'include' or 'exclude'; found %s.",
                     paste(sprintf("'%s'", unique(human[bad])), collapse = ", ")),
             class = "gr_bad_reference")
  }
  # One judgement per document. Two reviewers' sheets stacked with rbind() --
  # dual screening is standard practice -- doubled n, counted a disagreement
  # once each way, and narrowed every interval as if the sample were twice the
  # size. Copies that agree say nothing more and are counted once; copies that
  # disagree are a conflict for the reviewers to settle, not for a rate to
  # average.
  key <- ifelse(blank, -seq_along(hit), hit)
  again <- duplicated(key)
  if (any(again)) {
    n_views <- tapply(human[!blank], key[!blank], function(h) length(unique(h)))
    split_on <- as.integer(names(n_views)[n_views > 1L])
    if (length(split_on)) {
      gr_abort(sprintf(paste0("%d document(s) have more than one `human_decision` and the copies ",
                              "disagree, starting with '%s'. Reconcile them first (the consensus ",
                              "decision, one row per document); a calibration against both ",
                              "counts the document as eligible and as not."),
                       length(split_on), tab$document[split_on[1]]),
               class = c("gr_reference_conflict", "gr_bad_reference"))
    }
    gr_warn(sprintf(paste0("%d reference row(s) repeat a document already judged the same way, ",
                           "and are counted once."), sum(again)),
            class = "gr_reference_duplicate")
  }
  use <- !blank & !again & !is.na(model)
  model <- model[use]; human <- human[use]; rows <- ref[use, , drop = FALSE]
  if (!length(model)) {
    gr_abort("Nothing to compare: no reference row has both a human decision and a model one.",
             class = "gr_bad_reference")
  }

  eligible <- human == positive
  kept <- model %in% c("include", "unclear")
  strict <- model == "include"

  counts <- c(tp = sum(eligible & kept), fn = sum(eligible & !kept),
              fp = sum(!eligible & kept), tn = sum(!eligible & !kept))

  # WHICH STATISTICS THE SAMPLE CAN SUPPORT depends entirely on where it was
  # drawn from, and computing all of them regardless is worse than computing
  # none. A sample of exclusions contains no kept records by construction, so
  # sensitivity comes out 0%, specificity 100% and "reading avoided" 100% -- all
  # three artifacts of the frame, all three alarming or flattering, and none of
  # them a fact about the screener. What that frame DOES estimate is the false
  # omission rate, which is the most useful number here anyway: of everything
  # thrown away, how much should not have been.
  #
  # So `of` is checked, not taken as given. Any value but the two strata fell
  # through to the corpus-wide branch: of = "exclude" (the decision's label)
  # printed specificity 100% and "reading avoided" 100% from a sample of
  # exclusions, with no warning at all.
  recorded <- attr(ref, "of")
  frame_n <- attr(ref, "frame_n") %||% NA_integer_
  overridden <- NULL
  if (!is.null(of)) {
    of <- lower_text(trimws(as_chr1(of, "")))
    if (!of %in% c("excluded", "kept", "all")) {
      gr_abort(sprintf("`of` must be \"excluded\", \"kept\" or \"all\", not '%s'.", of),
               class = "gr_bad_setting")
    }
    # Named over a file that records otherwise, the file is what gr_reference()
    # did and `of` is a claim about it. Said out loud (below, once the rows are
    # known to fit the claim), and the file's frame size dropped: it counts the
    # frame the file names, not this one, and a projection multiplied by it
    # would be the wrong pile.
    if (!is.null(recorded) && !identical(recorded, of)) {
      overridden <- paste(attr(ref, "frames") %||% recorded, collapse = " and ")
      frame_n <- NA_integer_
    }
  }
  of <- as_chr1(of %||% recorded %||% "unknown", "unknown")
  if (identical(of, "mixed")) {
    # Two strata stacked into one file. This is not a corner case: it is what
    # following this package's own advice produces -- judge everything kept AND
    # a sample of what was discarded -- and gr_calibrate() takes one reference,
    # so people rbind() them. The kept stratum is then sampled at 100% and the
    # excluded stratum at a few percent, and the unweighted figures are not
    # merely imprecise, they are wrong in a flattering direction: on a screener
    # that really missed 14% of eligible studies, this reported sensitivity
    # 100% with a 95% interval that excluded the truth.
    gr_abort(paste0("This reference stacks more than one sampling frame (",
                    paste(attr(ref, "frames") %||% sort(unique(as.character(ref$sampled_from))),
                          collapse = " and "),
                    "). An unweighted figure over stratified samples is wrong, not just ",
                    "imprecise. Calibrate each frame separately (one gr_calibrate() call ",
                    "per file), or pass `of =` if the rows are one frame."),
             class = "gr_mixed_frame")
  }
  if (identical(of, "unknown")) {
    gr_warn(paste0("This reference does not say which part of the screening run it came ",
                   "from, so the figures below assume a sample of EVERYTHING screened. If ",
                   "it is a sample of one stratum (the exclusions, say), sensitivity ",
                   "and specificity are artifacts of that frame rather than facts about ",
                   "the screener. Pass `of = \"excluded\"`, `\"kept\"` or `\"all\"` to say which."),
            class = "gr_unknown_frame")
  }
  # A frame is a claim about the model's decisions, so check it against them.
  # The per-frame rates count every row as belonging to the frame: "eligible
  # among the excluded" is eligible rows over ALL rows, and the projection
  # multiplies that by the number of exclusions. A kept row in an exclusions
  # sample -- stacked frames with `of =` given, or a reference drawn from an
  # earlier run whose decisions have since changed -- was therefore scored as a
  # study thrown away, and "projected lost" reported studies the screener kept
  # while `missed` listed none of them.
  fits <- switch(of, excluded = model == "exclude",
                 kept = model %in% c("include", "unclear"), rep(TRUE, length(model)))
  if (!all(fits)) {
    gr_abort(sprintf(paste0("%d of %d reference row(s) are said to come from the %s records, but ",
                            "this screening run did not %s them, starting with '%s' (now '%s'). ",
                            "Either the reference was drawn from a different run, or it stacks ",
                            "frames; scoring those rows as %s would misstate what was %s. ",
                            "Calibrate against the run the sample was drawn from, one frame at ",
                            "a time."),
                     sum(!fits), length(fits), of, if (of == "excluded") "exclude" else "keep",
                     rows$document[which(!fits)[1]], model[which(!fits)[1]],
                     if (of == "excluded") "exclusions" else "kept records",
                     if (of == "excluded") "lost" else "kept"),
             class = "gr_reference_mismatch")
  }
  if (!is.null(overridden)) {
    gr_warn(sprintf(paste0("`of = \"%s\"` overrides the reference, which records that it was ",
                           "drawn from %s. The figures below are computed as if it were not; ",
                           "if the file is right, leave `of` out."), of, overridden),
            class = "gr_frame_override")
  }
  metrics <- switch(
    of,
    excluded = rbind(
      prop_row("eligible among the excluded", sum(eligible), length(model)),
      prop_row("correctly excluded", sum(!eligible), length(model))),
    kept = rbind(
      prop_row("eligible among those kept", sum(eligible), length(model)),
      prop_row("deferred to a person", sum(model == "unclear"), length(model))),
    rbind(
      prop_row("sensitivity (as deployed)", counts[["tp"]], counts[["tp"]] + counts[["fn"]]),
      prop_row("sensitivity (strict include)", sum(eligible & strict), sum(eligible)),
      prop_row("specificity", counts[["tn"]], counts[["tn"]] + counts[["fp"]]),
      prop_row("deferred to a person", sum(model == "unclear"), length(model)),
      prop_row("reading avoided", sum(!kept), length(model))))
  # Kept before rounding, because the projection multiplies by the frame size:
  # 1/30 rounded to 0.0333 across 30,000 exclusions reports 999 studies lost
  # where the arithmetic says 1000, and the gap grows with the corpus.
  exact <- metrics
  metrics$estimate <- round(metrics$estimate, 4)
  metrics$lower <- round(metrics$lower, 4); metrics$upper <- round(metrics$upper, 4)

  n_pos <- sum(eligible)
  # Kappa needs both kinds of decision from both raters to mean anything. On a
  # single-stratum sample the model said one thing throughout, so it is not
  # chance-corrected agreement, it is arithmetic on a constant.
  kappa <- if (of %in% c("excluded", "kept")) NA_real_ else
    cohen_kappa(model, ifelse(eligible, "include", "exclude"))
  # A number, compared as a number. `4 >= "10"` is TRUE because R compares as
  # STRINGS, and as_int1() then turned Inf and anything above 2^31 into the
  # default of 10 -- so asking for more eligible studies than the sample could
  # have declared the calibration adequate. A bar that cannot be read falls
  # back to the default, and says so.
  min_pos <- suppressWarnings(as.numeric(min_positives))
  if (length(min_pos) != 1L || is.na(min_pos)) {
    gr_warn("`min_positives` must be a single number; using the default (10).",
            class = "gr_bad_setting")
    min_pos <- 10
  }
  # ADEQUACY IS PER FRAME, because what the headline rate rests on is. In an
  # "all" sample sensitivity rests on the eligible studies alone, so it needs
  # min_positives of them. In a one-stratum sample the rate is taken over every
  # row sampled: judged by the eligible count instead, a clean sample of 500
  # exclusions -- 0.0% [0.0%, 0.8%] -- was "inadequate", said to rest on "those
  # 0 observations", and the better the screener the surer that verdict. There
  # the question is whether the interval is narrow enough to say something, or
  # whether the whole frame was judged, when nothing is left to sample and the
  # rate is a count for this run.
  headline <- switch(of, excluded = "eligible among the excluded",
                     kept = "eligible among those kept", NA_character_)
  adequacy <- if (is.na(headline)) {
    list(rule = "positives", bar = min_pos, width = NA_real_,
         note = if (n_pos >= min_pos) NA_character_ else
           sprintf("Only %d eligible stud%s in the sample, below the %s this calibration asks for.",
                   n_pos, if (n_pos == 1L) "y" else "ies", format(min_pos)))
  } else {
    h <- exact[exact$metric == headline, ]
    width <- h$upper - h$lower
    in_frame <- which(!is.na(tab$decision) &
                        switch(of, excluded = tab$decision == "exclude",
                               tab$decision %in% c("include", "unclear")))
    whole <- length(in_frame) > 0L && all(in_frame %in% hit[use])
    list(rule = if (whole) "whole frame" else "interval", bar = .gr_frame_max_width,
         width = width,
         note = if (whole || width <= .gr_frame_max_width) NA_character_ else
           sprintf(paste0("The interval on '%s' is %.1f points wide, from %d hand-screened ",
                          "row(s); a sample of one stratum needs it within %.0f points (or the ",
                          "whole stratum judged) before the figure says much."),
                   headline, 100 * width, length(model), 100 * .gr_frame_max_width))
  }
  structure(list(
    counts = counts,
    metrics = metrics,
    kappa = kappa,
    missed = rows[eligible & !kept, , drop = FALSE],
    disagreements = rows[(eligible & !kept) | (!eligible & strict), , drop = FALSE],
    n = length(model),
    n_positives = n_pos,
    # as_int1() first. `4 >= "10"` is TRUE -- R compares as STRINGS -- so a
    # character min_positives declared an inadequate calibration adequate, and
    # the "too few eligible studies to quote a figure" warning vanished from
    # print() while the object still displayed 10.
    adequate = is.na(adequacy$note),
    adequacy = adequacy,
    min_positives = min_pos,
    frame = list(of = of,
                 frame_n = frame_n,
                 screened_n = attr(ref, "screened_n") %||% nrow(tab)),
    # What the rate implies for the part of the frame nobody checked. The whole
    # reason to sample exclusions is to find out how much was lost, and a rate
    # without that multiplication leaves the reader to do it.
    projected = if (identical(of, "excluded") && !is.na(frame_n)) {
      fr <- frame_n
      m <- exact[exact$metric == "eligible among the excluded", ]
      list(frame_n = fr, lost = fr * m$estimate,
           lower = fr * m$lower, upper = fr * m$upper)
    } else NULL
  ), class = "gr_calibration")
}

#' @noRd
read_reference <- function(reference) {
  ref <- if (is.character(reference) && length(reference) == 1L && file.exists(reference)) {
    read_reference_csv(reference)
  } else reference
  if (!is.data.frame(ref) || !nrow(ref)) {
    gr_abort("`reference` must be a data frame, or a path to the CSV gr_reference() wrote.",
             class = "gr_bad_reference")
  }
  if (is.null(ref$document) || is.null(ref$human_decision)) {
    gr_abort(paste0("`reference` needs `document` and `human_decision` columns. ",
                    "gr_reference() writes a file with both."), class = "gr_bad_reference")
  }
  # A blind sheet's frame is an opaque key (see frame_key()); read it back as
  # the frame's name and sizes, which is what everything below works with.
  # Names in plain words -- a sheet written with blind = FALSE, or before the
  # key -- are read as they always were.
  keys <- list()
  if (!is.null(ref$sampled_from)) {
    raw <- as.character(ref$sampled_from)
    seen <- unique(raw[!is.na(raw) & nzchar(trimws(raw))])
    decoded <- lapply(seen, frame_unkey)
    named <- vapply(seq_along(seen), function(i) {
      if (is.null(decoded[[i]])) lower_text(trimws(to_utf8(seen[i]))) else decoded[[i]]$of
    }, character(1))
    ref$sampled_from <- named[match(raw, seen)]
    keys <- Filter(Negate(is.null), decoded)
  }
  # The COLUMNS say where each row came from; the attributes only say where the
  # first row did. rbind() keeps its first argument's attributes, so a sample
  # of the kept records stacked under a sample of the exclusions -- the design
  # gr_reference() recommends, combined the obvious way -- still claimed to be
  # one sample of exclusions, and was scored as one: every kept eligible study
  # counted as an eligible study thrown away, projected across the whole
  # discarded pile. The same rows read back from a CSV were refused. So the
  # columns are read whenever they are there, and the attributes are what is
  # left when they are not.
  if (!is.null(ref$sampled_from)) {
    v <- unique(as.character(ref$sampled_from))
    v <- v[!is.na(v) & nzchar(v)]
    a <- attr(ref, "of")
    frames <- unique(c(v, a))
    # More than one stratum in one file is reported as such rather than left to
    # fall through to "unknown", which took the corpus-wide branch and computed
    # sensitivity and specificity from a stratified sample without a word.
    if (length(frames) > 1L && length(v)) {
      attr(ref, "of") <- "mixed"
      attr(ref, "frames") <- sort(frames)
    } else if (length(v) == 1L && v %in% c("excluded", "kept", "all")) attr(ref, "of") <- v
  }
  for (col in c("frame_n", "screened_n")) {
    from_keys <- vapply(keys, function(k) k[[col]], integer(1))
    if (is.null(ref[[col]]) && !length(from_keys)) next
    v <- unique(c(suppressWarnings(as.integer(ref[[col]])), from_keys))
    v <- v[!is.na(v)]
    # Two sizes means rows from two frames or two runs, and there is no one
    # frame to project onto; nothing is better than the first argument's.
    if (length(v) == 1L) attr(ref, col) <- v
    else if (length(v) > 1L) attr(ref, col) <- NULL
  }
  ref
}

#' Which screening row each reference row is about, NA where none is.
#'
#' By name, compared as UTF-8 on both sides: the same name read in another
#' locale, or marked differently, is otherwise a different string to match().
#' A name that still does not match falls back to the row's `document_id`,
#' which the sheet carries and which no spreadsheet re-encodes.
#' @noRd
reference_rows <- function(ref, tab) {
  hit <- match(to_utf8(ref$document), to_utf8(tab$document))
  if (anyNA(hit) && !is.null(ref$document_id) && !is.null(tab$document_id)) {
    id_of <- function(x) { x <- trimws(to_utf8(x)); x[!nzchar(x)] <- NA_character_; x }
    miss <- which(is.na(hit))
    hit[miss] <- match(id_of(ref$document_id)[miss], id_of(tab$document_id), incomparables = NA)
  }
  hit
}

#' An opaque key for the frame a blind sample was drawn from.
#'
#' The frame, its size and the run's screened count, as `"gr"`, a nonce, and
#' the three XORed with a pad the nonce derives. Not secret, only unreadable at
#' a glance, which is what blinding a person needs; the nonce, taken from the
#' sampled documents, keeps one frame from having the same key in every file.
#' @noRd
frame_key <- function(of, frame_n, screened_n, salt) {
  payload <- as.integer(charToRaw(sprintf("%s:%s:%s", of, frame_n, screened_n)))
  nonce <- substr(gr_hash(list("readgpt-frame-key", as.character(salt))), 1, 8)
  body <- bitwXor(payload, frame_pad(nonce, length(payload)))
  paste0("gr", nonce, paste(sprintf("%02x", body), collapse = ""))
}

#' The frame a key stands for, as list(of, frame_n, screened_n); NULL for
#' anything that is not a key this package wrote.
#' @noRd
frame_unkey <- function(x) {
  x <- lower_text(trimws(as_chr1(x, "")))
  if (!grepl("^gr[0-9a-f]{8}([0-9a-f]{2})+$", x)) return(NULL)
  hex <- substring(x, 11L)
  at <- seq(1L, nchar(hex) - 1L, by = 2L)
  body <- strtoi(substring(hex, at, at + 1L), 16L)
  plain <- bitwXor(body, frame_pad(substr(x, 3L, 10L), length(body)))
  if (any(plain < 32L | plain > 126L)) return(NULL)
  txt <- rawToChar(as.raw(plain))
  m <- regmatches(txt, regexec("^(excluded|kept|all):([0-9]+|NA):([0-9]+|NA)$", txt))[[1]]
  if (!length(m)) return(NULL)
  list(of = m[2], frame_n = suppressWarnings(as.integer(m[3])),
       screened_n = suppressWarnings(as.integer(m[4])))
}

#' The pad behind a frame key: bytes from a 32-bit linear congruential
#' generator seeded by the nonce. Plain arithmetic on doubles (every value stays
#' below 2^53), so a key written on one machine reads back on any other.
#' @noRd
frame_pad <- function(nonce, n) {
  state <- as.numeric(strtoi(substr(nonce, 1L, 4L), 16L)) * 65536 +
    strtoi(substr(nonce, 5L, 8L), 16L)
  out <- integer(n)
  for (i in seq_len(n)) {
    state <- (state * 69069 + 1) %% 4294967296
    out[i] <- as.integer(state %/% 16777216)
  }
  out
}

#' Write a reference sheet as UTF-8 with a byte-order mark, in any locale.
#'
#' write.csv() translates to the session's encoding before it writes, so in a C
#' locale an accented name went out as "<U+00C9>vora.txt" -- irreversibly, and
#' gr_calibrate() then refused its own file -- and with fileEncoding = "UTF-8"
#' it went out as nothing at all. So the cells are quoted here, as write.csv()
#' quotes them, and written as bytes. The mark is what makes Excel read the
#' file as UTF-8 rather than as its own code page.
#' @noRd
write_reference_csv <- function(df, path) {
  cell <- function(v) {
    if (is.factor(v)) v <- as.character(v)
    if (is.character(v)) {
      out <- paste0("\"", gsub("\"", "\"\"", to_utf8(v), fixed = TRUE), "\"")
    } else {
      out <- as.character(v)
    }
    out[is.na(v)] <- ""
    out
  }
  body <- if (ncol(df)) do.call(paste, c(lapply(unclass(df), cell), sep = ",")) else character(0)
  head <- paste0("\"", gsub("\"", "\"\"", to_utf8(names(df)), fixed = TRUE), "\"",
                 collapse = ",")
  text <- to_utf8(paste0(paste(c(head, body), collapse = "\n"), "\n"))
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeBin(c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(text)), con)
  invisible(path)
}

#' Read a reference sheet back as UTF-8, whatever the locale and whatever a
#' spreadsheet did to it.
#'
#' read.csv() reads bytes in the session's encoding: under a C locale a UTF-8
#' name came back as bytes match() did not equate with the marked name in the
#' screening, a sheet Excel saved as Windows-1252 stopped with "invalid
#' multibyte string", and a byte-order mark became part of the first column's
#' name ("X...document"). So the bytes are read here: the mark dropped, each
#' line kept as UTF-8 where it is valid and read as Windows-1252 where it is not
#' (to_utf8()), and the text parsed from memory, which read.csv() marks UTF-8.
#' @noRd
read_reference_csv <- function(path) {
  bytes <- readBin(path, "raw", n = max(file.size(path), 0))
  if (length(bytes) >= 3L && identical(bytes[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) {
    bytes <- bytes[-(1:3)]
  }
  bytes <- bytes[bytes != as.raw(0L)]
  lines <- strsplit(rawToChar(bytes), "\n", fixed = TRUE, useBytes = TRUE)[[1]]
  lines <- to_utf8(sub("\r$", "", lines, useBytes = TRUE))
  if (!length(lines)) return(data.frame())
  utils::read.csv(text = lines, stringsAsFactors = FALSE, na.strings = c("", "NA"))
}

#' A proportion with a Wilson score interval.
#'
#' Wilson rather than the textbook normal interval, because screening
#' proportions sit at the ends of the scale where the textbook one is worst: at
#' 5 of 5 it gives [1, 1], which asserts certainty from five observations.
#' @noRd
prop_row <- function(name, hits, total, conf = 0.95) {
  if (!total) {
    return(data.frame(metric = name, estimate = NA_real_, lower = NA_real_,
                      upper = NA_real_, n = 0L, stringsAsFactors = FALSE))
  }
  z <- stats::qnorm(1 - (1 - conf) / 2)
  p <- hits / total
  d <- 1 + z^2 / total
  centre <- (p + z^2 / (2 * total)) / d
  half <- z * sqrt(p * (1 - p) / total + z^2 / (4 * total^2)) / d
  data.frame(metric = name, estimate = p, lower = max(0, centre - half),
             upper = min(1, centre + half), n = as.integer(total),
             stringsAsFactors = FALSE)
}

#' Cohen's kappa between two labellings.
#'
#' Chance-corrected, which is the point: at a 5% inclusion rate a screener that
#' excluded everything agrees with a person 95% of the time, and only kappa
#' reports that as the nothing it is.
#' @noRd
cohen_kappa <- function(a, b) {
  lv <- union(unique(a), unique(b))
  m <- table(factor(a, levels = lv), factor(b, levels = lv))
  n <- sum(m)
  if (!n) return(NA_real_)
  po <- sum(diag(m)) / n
  pe <- sum(rowSums(m) * colSums(m)) / n^2
  if (isTRUE(all.equal(pe, 1))) return(NA_real_)
  as.numeric((po - pe) / (1 - pe))
}

#' Subsetting a reference frame gives a plain data frame.
#'
#' `[` on an object whose class extends `data.frame` keeps the CLASS and drops
#' every other attribute, so `ref[, cols]` came back still claiming to be a
#' `gr_reference_frame` while `of`, `frame_n`, `screened_n` and `seed` were gone,
#' and gr_calibrate() then computed the corpus-wide metric set from a stratified
#' sample. The same trap, and the same fix, as `[.gr_gaps`.
#' @param x A `gr_reference_frame`.
#' @param ... Passed to the data frame method.
#' @return A plain data frame.
#' @export
`[.gr_reference_frame` <- function(x, ...) {
  out <- NextMethod()
  if (is.data.frame(out)) {
    class(out) <- "data.frame"
    for (a in c("of", "frame_n", "screened_n", "seed")) attr(out, a) <- NULL
  }
  out
}

#' @export
print.gr_calibration <- function(x, ...) {
  cat(sprintf("<gr_calibration> %d hand-screened row(s), %d eligible\n", x$n, x$n_positives))
  cat(sprintf("  sampled from: %s (%s of %s screened)\n", x$frame$of,
              format(x$frame$frame_n), format(x$frame$screened_n)))
  for (i in seq_len(nrow(x$metrics))) {
    m <- x$metrics[i, ]
    cat(sprintf("  %-30s %s  [%s, %s]  n=%d\n", m$metric,
                if (is.na(m$estimate)) "  --" else sprintf("%5.1f%%", 100 * m$estimate),
                if (is.na(m$lower)) "--" else sprintf("%.1f%%", 100 * m$lower),
                if (is.na(m$upper)) "--" else sprintf("%.1f%%", 100 * m$upper), m$n))
  }
  if (!is.na(x$kappa)) cat(sprintf("  %-30s %5.2f\n", "Cohen's kappa", x$kappa))
  if (!is.null(x$projected)) {
    p <- x$projected
    cat(sprintf(paste0("  -> across all %d excluded record(s) that rate implies about %.0f\n",
                       "     eligible stud%s lost (%.0f to %.0f on the interval above).\n"),
                p$frame_n, p$lost, if (round(p$lost) == 1) "y" else "ies", p$lower, p$upper))
  }
  if (identical(x$frame$of, "excluded")) {
    cat("  (sampled from exclusions only: this frame estimates what was lost, not\n")
    cat("   sensitivity or specificity: it contains no kept records to compute them from)\n")
  } else if (identical(x$frame$of, "kept")) {
    cat("  (sampled from kept records only: this frame estimates how much of what was\n")
    cat("   kept is worth keeping, not sensitivity: the misses are not in it)\n")
  }
  if (nrow(x$missed)) {
    cat(sprintf("  ! %d eligible stud%s excluded by the screener:\n", nrow(x$missed),
                if (nrow(x$missed) == 1L) "y was" else "ies were"))
    for (d in utils::head(x$missed$document, 5)) cat(sprintf("      %s\n", d))
    if (nrow(x$missed) > 5L) cat(sprintf("      ... and %d more\n", nrow(x$missed) - 5L))
  }
  rule <- x$adequacy$rule %||% "positives"
  if (!x$adequate && !identical(rule, "positives")) {
    # One stratum: the rate is over every row, so the eligible count is not
    # what it rests on, and saying so told the owner of a clean 500-row sample
    # that it rested on nothing.
    cat(sprintf(paste0("  ! the interval on '%s' is %.1f points wide\n",
                       "    (n=%d). A sample of one stratum needs it within %.0f points, or the\n",
                       "    whole stratum judged. Hand-screen more before quoting a figure.\n"),
                if (identical(x$frame$of, "kept")) "eligible among those kept"
                else "eligible among the excluded",
                100 * x$adequacy$width, x$n, 100 * x$adequacy$bar))
  } else if (!x$adequate) {
    cat(sprintf(paste0("  ! only %d eligible stud%s in the sample. Every rate above rests on\n",
                       "    %s, which is why the intervals are as wide as they are.\n",
                       "    Hand-screen more before quoting a figure.\n"),
                x$n_positives, if (x$n_positives == 1L) "y" else "ies",
                if (x$n_positives == 1L) "that one observation"
                else sprintf("those %d observations", x$n_positives)))
  }
  invisible(x)
}
