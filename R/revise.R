# revise.R -- editing a finished draft without strengthening what it says.
#
# WHY THIS FILE EXISTS
# There was one pass, `coherence = TRUE`, doing three jobs at once: reorder,
# trim, and polish. It kept two guards -- the revision must not lose citations,
# and it must not arrive truncated -- and both are necessary. Neither is
# sufficient, because the most damaging thing an editing pass can do leaves the
# citations exactly where they were.
#
# Editing for impact means deleting hedges, and the hedges are where the
# uncertainty lives. "Three small trials suggest a modest benefit" becomes
# "trials show a benefit": same markers, same studies, a claim the evidence does
# not carry. The citation check cannot see it. So the passes are separated, each
# forbidden from doing the others' job, and each measured for escalation.

#' The passes, in the order they run.
#' @noRd
.gr_revise_passes <- c("structure", "cut", "register")

#' Words that weaken a claim, and words that strengthen one.
#'
#' Stems, so inflection does not matter: an editing pass substituting "shows" for
#' "showed" is not introducing a booster, while one substituting "shows" for
#' "suggests" is.
#' @noRd
.gr_hedges <- c("may", "might", "could", "suggest", "appear", "seem", "indicat",
                "tend", "possibl", "potential", "prelimin", "tentativ", "unclear",
                "uncertain", "limited evidence", "some evidence", "mixed evidence",
                "small", "few", "single study", "one study", "two studies",
                "cannot", "not clear", "no firm", "caution")

#' @noRd
.gr_boosters <- c("demonstrat", "prove", "proven", "establish", "confirm", "conclusiv",
                  "definitiv", "robustly", "unequivocal", "clearly show", "clear evidence",
                  "strong evidence", "shows that", "show that", "well established")

#' Universal quantifiers, which are escalation on their own.
#' @noRd
.gr_universals <- c("\\ball\\b", "\\bevery\\b", "\\balways\\b", "\\bnever\\b",
                    "\\bnone\\b", "\\bno studies\\b", "\\bconsistently\\b",
                    "\\binvariably\\b", "\\buniversally\\b", "\\bwithout exception\\b")

#' The sentences in a draft that make a claim.
#'
#' A claim-bearing sentence is one carrying a citation marker. Prose between them
#' -- headings, framing, transitions -- is what an editing pass is supposed to be
#' free to rewrite, so measuring it would make the guard fire on the work it is
#' meant to permit.
#' @noRd
claim_sentences <- function(text, word = "study") {
  txt <- as_chr1(text)
  if (!nzchar(trimws(txt))) return(character(0))
  # Split on sentence enders followed by whitespace. Not perfect, and it does not
  # need to be: a mis-split sentence is measured on both halves, which changes
  # the denominator identically before and after.
  parts <- unlist(strsplit(txt, "(?<=[.!?])\\s+", perl = TRUE), use.names = FALSE)
  pat <- cite_pattern(word)
  parts[grepl(pat, parts, perl = TRUE, ignore.case = TRUE)]
}

#' Count hedges, boosters and universals in the claim-bearing prose.
#' @noRd
claim_strength <- function(text, word = "study") {
  sents <- claim_sentences(text, word)
  low <- tolower(paste(sents, collapse = " "))
  count <- function(stems) {
    if (!nzchar(low)) return(0L)
    sum(vapply(stems, function(p) {
      m <- gregexpr(p, low, fixed = TRUE)[[1]]
      if (m[1] == -1L) 0L else length(m)
    }, integer(1)))
  }
  universals <- if (!nzchar(low)) character(0) else {
    Filter(function(p) grepl(p, low, perl = TRUE), .gr_universals)
  }
  boosters <- if (!nzchar(low)) character(0) else {
    Filter(function(p) grepl(p, low, fixed = TRUE), .gr_boosters)
  }
  list(sentences = length(sents), hedges = count(.gr_hedges),
       boosters = boosters, universals = universals)
}

#' Refuse a revision that made the review claim more than the draft did.
#'
#' Three rules, no tolerances, each answering a way an editing pass escalates:
#'
#'   * A universal quantifier that was not there before. "All trials" out of
#'     "the trials" is the largest possible strengthening and the least likely to
#'     be a legitimate edit.
#'   * A booster stem that was not there before. An editing pass has no business
#'     introducing "demonstrates" where the draft said "suggests".
#'   * Fewer hedges per claim-bearing sentence than the draft had. Measured as a
#'     rate, not a total, so removing a whole redundant sentence -- which is the
#'     `cut` pass doing its job -- carries its hedges away with it and passes,
#'     while stripping the hedges off the sentences that remain does not.
#' @noRd
strength_guard <- function(before, after, word = "study") {
  b <- claim_strength(before, word)
  a <- claim_strength(after, word)
  new_u <- setdiff(a$universals, b$universals)
  if (length(new_u)) {
    return(list(ok = FALSE, reason = sprintf(
      "the revision introduced %s into a claim", paste(gsub("\\\\b", "", new_u), collapse = ", "))))
  }
  new_b <- setdiff(a$boosters, b$boosters)
  if (length(new_b)) {
    return(list(ok = FALSE, reason = sprintf(
      "the revision introduced '%s' into a claim", paste(new_b, collapse = "', '"))))
  }
  if (b$sentences > 0L && a$sentences > 0L) {
    # floor(), so a legitimate cut is never rejected for arithmetic: the after
    # text need only carry the hedges its own sentence count implies.
    need <- floor(b$hedges / b$sentences * a$sentences)
    if (a$hedges < need) {
      return(list(ok = FALSE, reason = sprintf(
        "the revision dropped hedging: %d hedge(s) across %d claim sentence(s), where the draft's rate implies at least %d",
        a$hedges, a$sentences, need)))
    }
  }
  list(ok = TRUE, reason = NA_character_)
}

#' Which passes a `coherence` argument asks for.
#'
#' `TRUE` means all three, for the callers written before there were three.
#' @noRd
revise_passes <- function(coherence) {
  if (is.null(coherence) || isFALSE(coherence)) return(character(0))
  if (isTRUE(coherence)) return(.gr_revise_passes)
  want <- as.character(coherence)
  bad <- setdiff(want, .gr_revise_passes)
  if (length(bad)) {
    gr_abort(sprintf("Unknown revision pass(es): %s. Available: %s.",
                     paste(sQuote(bad), collapse = ", "),
                     paste(.gr_revise_passes, collapse = ", ")),
             class = "gr_unknown_method")
  }
  .gr_revise_passes[.gr_revise_passes %in% want]
}

#' @noRd
.gr_revise_prompts <- c(
  structure = paste0(
    "You reorder a finished review so it reads as one argument. ",
    "Move and merge paragraphs; add or rewrite only the transitions between them. ",
    "DO NOT reword any sentence that makes a claim, and do not change, add or remove a single ",
    "citation marker. Return the whole review."),
  cut = paste0(
    "You cut a finished review to length. Remove repetition, filler and anything the review says ",
    "twice. ",
    "DO NOT change what any surviving claim says: keep its hedging, its qualifiers and its ",
    "citation markers exactly. If a claim is only stated once, it stays. Return the whole review."),
  register = paste0(
    "You polish the prose of a finished review sentence by sentence. ",
    "DO NOT reorder anything, do not cut anything, and do not strengthen any claim: a sentence ",
    "that said 'three small trials suggest' must not come back as 'trials show'. Keep every ",
    "citation marker where it is. Return the whole review."))

#' Run the revision passes over a marked draft.
#'
#' Each pass is independent: a pass that fails its guards is discarded and the
#' next runs on the text that survived, so one bad revision costs that pass
#' rather than the document.
#' @noRd
synth_revise <- function(drafted, question, client, spec, trace, style = NULL,
                         passes = .gr_revise_passes) {
  text <- as_chr1(drafted)
  report <- list()
  for (pass in passes) {
    step <- revise_once(text, question, client, spec, trace, style, pass)
    report[[length(report) + 1L]] <- data.frame(
      pass = pass, ran = isTRUE(step$ran), kept = isTRUE(step$kept),
      # Comma-joined rather than a list column: this frame is printed, written to
      # an audit report and read back out of a CSV, and a list column survives
      # none of that.
      lost = as_chr1(step$lost, NA_character_), added = as_chr1(step$added, NA_character_),
      reason = as_chr1(step$reason, NA_character_), stringsAsFactors = FALSE)
    if (isTRUE(step$kept)) text <- step$text
  }
  rep_df <- if (length(report)) do.call(rbind, report) else
    data.frame(pass = character(0), ran = logical(0), kept = logical(0),
               lost = character(0), added = character(0), reason = character(0),
               stringsAsFactors = FALSE)
  rownames(rep_df) <- NULL
  list(text = if (identical(text, as_chr1(drafted))) NULL else text, report = rep_df)
}

#' @noRd
revise_once <- function(drafted, question, client, spec, trace, style, pass) {
  sys <- .gr_revise_prompts[[pass]]
  if (is_nonblank(style)) sys <- paste0(sys, "\n\nRegister: ", as_chr1(style))
  overhead <- prompt_overhead(question, sys)

  # Budgeted for the WHOLE document, because that is what comes back. Sizing a
  # whole-draft rewrite by a section's allowance asked a model revising 4800
  # tokens for 300, and the citation check then blamed the model for the
  # truncation that budgeting caused.
  need <- as.integer(ceiling(gr_count_tokens(drafted) * 1.15) + 64L)
  info <- gr_model_info(spec$model)
  room <- as.integer(info$max_output)
  if (need > room) {
    gr_warn(sprintf(paste0("The draft is about %d tokens and '%s' can emit at most %d, so the ",
                           "'%s' pass was skipped rather than returning a review cut off ",
                           "part-way."),
                    gr_count_tokens(drafted), spec$model, room, pass),
            class = "gr_coherence_skipped")
    return(list(ran = FALSE, kept = FALSE, reason = "draft exceeds the output limit"))
  }
  bud <- gr_budget(spec$model, reserve_output = need, overhead = overhead)
  if (gr_count_tokens(drafted) > bud$input) {
    gr_warn(sprintf(paste0("The draft does not fit one prompt alongside room to rewrite it, so ",
                           "the '%s' pass was skipped."), pass),
            class = "gr_coherence_skipped")
    return(list(ran = FALSE, kept = FALSE, reason = "draft exceeds the context window"))
  }
  if (!trace_can_call(trace)) {
    return(list(ran = FALSE, kept = FALSE, reason = "call cap reached"))
  }
  res <- gr_call(client, list(
    list(role = "system", content = sys),
    list(role = "user", content = paste0("Review question: ", question)),
    list(role = "user", content = paste0("<draft>\n", drafted, "\n</draft>"))
  ), model = spec$model, max_output = need, temperature = spec$temperature,
     trace = trace, label = paste0("synthesise.revise.", pass))

  if (!usable_text(res)) {
    return(list(ran = TRUE, kept = FALSE, reason = "the revision call failed"))
  }
  if (identical(as_chr1(res$finish_reason), "length")) {
    return(list(ran = TRUE, kept = FALSE, reason = "revision truncated"))
  }
  before <- cited_ids(drafted, "study")
  after <- cited_ids(res$text, "study")
  if (!setequal(before, after)) {
    lost <- sort(setdiff(before, after)); gained <- sort(setdiff(after, before))
    reason <- sprintf("the revision changed the citations (%s)",
                      paste(c(if (length(lost)) sprintf("dropped %s",
                                                        paste(lost, collapse = ", ")),
                              if (length(gained)) sprintf("added %s",
                                                          paste(gained, collapse = ", "))),
                            collapse = "; "))
    gr_warn(sprintf("The '%s' pass was discarded: %s.", pass, reason),
            class = "gr_coherence_rejected")
    return(list(ran = TRUE, kept = FALSE, reason = reason,
                lost = if (length(lost)) paste(lost, collapse = ", ") else NA_character_,
                added = if (length(gained)) paste(gained, collapse = ", ") else NA_character_))
  }
  # The guard the citation check cannot make. Same markers, stronger claim.
  st <- strength_guard(drafted, res$text)
  if (!isTRUE(st$ok)) {
    gr_warn(sprintf("The '%s' pass was discarded: %s.", pass, st$reason),
            class = "gr_revision_escalated")
    return(list(ran = TRUE, kept = FALSE, reason = st$reason))
  }
  list(ran = TRUE, kept = TRUE, text = res$text, reason = NA_character_)
}
