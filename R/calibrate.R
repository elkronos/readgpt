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

#' Draw a sample to screen by hand
#'
#' Picks documents out of a [gr_screen()] run for a person to judge without
#' seeing what the model said, and writes them to a CSV to fill in. Feeding the
#' completed file back to [gr_calibrate()] is what turns "we used an LLM" into a
#' claim with a number attached.
#'
#' @section Which rows to sample:
#' `of = "excluded"` is usually the right answer and is not the obvious one.
#' Sensitivity failures hide among the exclusions — a study the screener threw
#' away is gone, and nothing downstream will ever mention it — while the records
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
#' @param screening A `gr_screening` from [gr_screen()].
#' @param n How many to draw. `Inf` takes the whole frame.
#' @param of Which rows to sample from: `"excluded"`, `"kept"` (include and
#'   unclear), or `"all"`.
#' @param seed Passed to [withr::with_seed()] so the draw is reproducible. A
#'   calibration sample is part of the method and has to be re-drawable.
#' @param path Where to write the CSV. `NULL` returns the frame without writing.
#' @param blind Leave the model's decision and reason out of the file. On by
#'   default: a person shown the answer agrees with it, and the resulting figure
#'   measures nothing.
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
    if (is.null(seed)) sample(idx, n) else withr::with_seed(seed, sample(idx, n))
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
  ref$sampled_from <- of
  ref$frame_n <- nrow(pool)
  rownames(ref) <- NULL
  attr(ref, "of") <- of
  attr(ref, "frame_n") <- nrow(pool)
  attr(ref, "screened_n") <- sum(judged)
  attr(ref, "seed") <- seed
  class(ref) <- c("gr_reference_frame", "data.frame")

  if (!is.null(path)) {
    utils::write.csv(ref, path, row.names = FALSE, na = "")
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
  # Duplicates were decided once, under the row they repeat.
  if (!is.null(tab$duplicate_of)) tab <- tab[is.na(tab$duplicate_of), , drop = FALSE]
  tab
}

#' Measure the screener against a hand-screened sample
#'
#' Compares [gr_screen()]'s decisions with a person's on the same documents, and
#' reports what that sample supports: how much of the eligible literature the
#' screener kept, how much of the irrelevant literature it removed, and how much
#' reading it saved — each with an interval, and each refused when the sample is
#' too small to say.
#'
#' @section Two sensitivities, and the gap between them:
#' `"unclear"` is a deferral, not a miss. A record the screener could not settle
#' goes to a person, so it is not lost — and counting it as a failure would
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
#' @param screening A `gr_screening` from [gr_screen()].
#' @param reference The completed frame from [gr_reference()], a path to the
#'   filled-in CSV, or any data frame with `document` and `human_decision`.
#' @param positive Which human decision counts as eligible.
#' @param min_positives Below this many eligible studies in the sample, the
#'   sensitivity estimate is reported but marked inadequate.
#' @return An object of class `gr_calibration`:
#'   \describe{
#'     \item{`counts`}{The confusion matrix, as kept/excluded by eligible/not.}
#'     \item{`metrics`}{One row per statistic: `estimate`, `lower`, `upper`, `n`.}
#'     \item{`missed`}{The eligible studies the screener excluded — the rows
#'       themselves, because a list of the misses says more than a rate.}
#'     \item{`disagreements`}{Every row where the two differ, in either
#'       direction.}
#'     \item{`adequate`}{Whether the sample supports a sensitivity claim.}
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
gr_calibrate <- function(screening, reference, positive = "include", min_positives = 10L) {
  tab <- as_screening_table(screening)
  ref <- read_reference(reference)

  hit <- match(ref$document, tab$document)
  if (anyNA(hit)) {
    gr_abort(sprintf(paste0("%d row(s) in the reference name documents this screening run does ",
                            "not contain, starting with '%s'. A calibration compares the two on ",
                            "the same documents; a name that is in one and not the other means ",
                            "they are not the same run."),
                     sum(is.na(hit)), ref$document[which(is.na(hit))[1]]),
             class = "gr_reference_mismatch")
  }
  model <- tab$decision[hit]
  human <- tolower(trimws(as.character(ref$human_decision)))

  blank <- is.na(human) | !nzchar(human)
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
  use <- !blank & !is.na(model)
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
  of <- attr(ref, "of") %||% "unknown"
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
  metrics$estimate <- round(metrics$estimate, 4)
  metrics$lower <- round(metrics$lower, 4); metrics$upper <- round(metrics$upper, 4)

  n_pos <- sum(eligible)
  # Kappa needs both kinds of decision from both raters to mean anything. On a
  # single-stratum sample the model said one thing throughout, so it is not
  # chance-corrected agreement, it is arithmetic on a constant.
  kappa <- if (of %in% c("excluded", "kept")) NA_real_ else
    cohen_kappa(model, ifelse(eligible, "include", "exclude"))
  structure(list(
    counts = counts,
    metrics = metrics,
    kappa = kappa,
    missed = rows[eligible & !kept, , drop = FALSE],
    disagreements = rows[(eligible & !kept) | (!eligible & strict), , drop = FALSE],
    n = length(model),
    n_positives = n_pos,
    adequate = n_pos >= min_positives,
    min_positives = as.integer(min_positives),
    frame = list(of = of,
                 frame_n = attr(ref, "frame_n") %||% NA_integer_,
                 screened_n = attr(ref, "screened_n") %||% nrow(tab)),
    # What the rate implies for the part of the frame nobody checked. The whole
    # reason to sample exclusions is to find out how much was lost, and a rate
    # without that multiplication leaves the reader to do it.
    projected = if (identical(of, "excluded") && !is.na(attr(ref, "frame_n") %||% NA)) {
      fr <- attr(ref, "frame_n")
      m <- metrics[metrics$metric == "eligible among the excluded", ]
      list(frame_n = fr, lost = fr * m$estimate,
           lower = fr * m$lower, upper = fr * m$upper)
    } else NULL
  ), class = "gr_calibration")
}

#' @noRd
read_reference <- function(reference) {
  ref <- if (is.character(reference) && length(reference) == 1L && file.exists(reference)) {
    utils::read.csv(reference, stringsAsFactors = FALSE, na.strings = c("", "NA"))
  } else reference
  if (!is.data.frame(ref) || !nrow(ref)) {
    gr_abort("`reference` must be a data frame, or a path to the CSV gr_reference() wrote.",
             class = "gr_bad_reference")
  }
  if (is.null(ref$document) || is.null(ref$human_decision)) {
    gr_abort(paste0("`reference` needs `document` and `human_decision` columns. ",
                    "gr_reference() writes a file with both."), class = "gr_bad_reference")
  }
  # Recover what the attributes cannot carry through a CSV.
  if (is.null(attr(ref, "of")) && !is.null(ref$sampled_from)) {
    v <- unique(as.character(ref$sampled_from))
    v <- v[!is.na(v)]
    if (length(v) == 1L && v %in% c("excluded", "kept", "all")) attr(ref, "of") <- v
  }
  if (is.null(attr(ref, "frame_n")) && !is.null(ref$frame_n)) {
    v <- unique(suppressWarnings(as.integer(ref$frame_n)))
    v <- v[!is.na(v)]
    if (length(v) == 1L) attr(ref, "frame_n") <- v
  }
  ref
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
    cat("   sensitivity or specificity -- it contains no kept records to compute them from)\n")
  } else if (identical(x$frame$of, "kept")) {
    cat("  (sampled from kept records only: this frame estimates how much of what was\n")
    cat("   kept is worth keeping, not sensitivity -- the misses are not in it)\n")
  }
  if (nrow(x$missed)) {
    cat(sprintf("  ! %d eligible stud%s excluded by the screener:\n", nrow(x$missed),
                if (nrow(x$missed) == 1L) "y was" else "ies were"))
    for (d in utils::head(x$missed$document, 5)) cat(sprintf("      %s\n", d))
    if (nrow(x$missed) > 5L) cat(sprintf("      ... and %d more\n", nrow(x$missed) - 5L))
  }
  if (!x$adequate) {
    cat(sprintf(paste0("  ! only %d eligible stud%s in the sample. Every rate above rests on\n",
                       "    %s, which is why the intervals are as wide as they are.\n",
                       "    Hand-screen more before quoting a figure.\n"),
                x$n_positives, if (x$n_positives == 1L) "y" else "ies",
                if (x$n_positives == 1L) "that one observation"
                else sprintf("those %d observations", x$n_positives)))
  }
  invisible(x)
}
