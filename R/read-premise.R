# read-premise.R -- the third outcome: a question the document contradicts.
#
# WHY THIS FILE EXISTS
# The question-answering readers had two exits: an answer, or exactly
# NOT_IN_DOCUMENT. A loaded question has neither. Asked "Why did revenue fall
# in 2023?" of a report in which revenue rose 12%, a model either answers it on
# its own terms (inventing a cause for a fall that did not happen, or offering
# whatever the report says about costs) or replies NOT_IN_DOCUMENT, because the
# report gives no reason for a fall. Either way the one thing the document does
# say about the question, that revenue rose, is lost, and the second exit tells
# the reader the document is silent when it contradicts them.
#
# `premise = TRUE` in gr_read_spec() adds a third exit. The answering prompts
# (answer_system(), and the merge, refine and adjudication prompts the readers
# send) tell the model that when the excerpts contradict an assumption in the
# question it is to reply FALSE_PREMISE: and one sentence saying what they
# state instead. That sentence becomes `$answer` and `$premise`; the answer is
# not a not-found. The instruction is part of each system prompt at the place
# it is built, so it is in the string the budget is computed from, never added
# inside gr_call().
#
# Off by default, because it changes what is sent: a cache, a recorded replay
# and a corpus store all key on the requests and the spec, and a run without it
# has to send exactly what it sent before. gr_read_spec() leaves the field out
# of the spec altogether unless it is TRUE, so a default spec, and every key
# built from one, is the one it was.
#
# The verdict is the model's reading of the excerpts. Nothing here checks it
# against the document, and nothing calls it verified: print(), the audit
# report and the summaries say the model reports it.

#' The sentinel a reader that checks premises is told to open its reply with.
#' @noRd
.FALSE_PREMISE <- "FALSE_PREMISE"

#' What each prompt gains when the premise is checked: appended to the system
#' prompt it extends, at the place that prompt is built.
#' @noRd
.gr_premise_prompts <- list(
  # answer_system(), and through it every reader's answering request. The last
  # sentence is what keeps the two sentinels apart: a chunk of a long document
  # that does not mention revenue is silent about it, not a contradiction.
  answer = paste0(
    " If the excerpts contradict an assumption the question makes, do not answer it as asked: ",
    "reply with FALSE_PREMISE: followed by one sentence saying what the excerpts actually ",
    "state. Use it only when the excerpts state the contrary; when they are merely silent, ",
    "the reply is NOT_IN_DOCUMENT."),
  # merge_system_prompt(): map_reduce's merge and ensemble's adjudication. A
  # chunk or member that flagged the premise reaches it as a finding that opens
  # with the sentinel, and the merge may return the sentinel itself.
  merge = paste0(
    " A finding that begins with FALSE_PREMISE: reports that its excerpt contradicts an ",
    "assumption in the question, and its sentence says what the excerpt states instead. If ",
    "the findings, taken together, contradict an assumption in the question, reply with ",
    "FALSE_PREMISE: followed by one sentence saying what they actually state, instead of ",
    "answering. A finding that merely lacks the answer contradicts nothing."),
  # refine_system_prompt(): the draft carries the verdict forward, so a later
  # excerpt can add to the correction or, where it shows the assumption holds
  # after all, overturn it.
  refine = paste0(
    " A draft that begins with FALSE_PREMISE: says the excerpts so far contradict an ",
    "assumption in the question. Keep that verdict, revising its sentence with what this ",
    "excerpt adds, unless the excerpt shows the assumption holds after all. If this excerpt ",
    "contradicts an assumption in the question, reply with FALSE_PREMISE: followed by one ",
    "sentence saying what the excerpts actually state."),
  # read_iterative()'s step, whose reply is JSON: the verdict has a field of
  # its own in the schema, which only a spec that checks the premise asks for.
  iterative = paste0(
    " If the excerpts contradict an assumption in the question, set false_premise true, set ",
    "can_answer true, and put in answer the one sentence saying what they actually state.")
)

#' The answer a premise verdict gets when the model gave the sentinel and
#' nothing after it: still a verdict, and said to be one, but the sentence
#' that was asked for is missing and this says so rather than leaving `$answer`
#' blank, which is_not_found() would read as nothing found.
#' @noRd
.gr_premise_unstated <- paste0(
  "The model reports that the excerpts contradict an assumption in the question, ",
  "but did not say what they state instead.")

#' The readers that check a premise when asked. gr_read() records the check on
#' their answers (`$premise` NA when nothing was flagged) whichever way they
#' returned, including before any request was made. `screen` and `extract`
#' answer no question, and a custom reader says for itself, through
#' new_answer(premise =), whether it checked.
#' @noRd
.gr_premise_readers <- c("stuff", "map_reduce", "refine", "skim", "retrieve", "rerank",
                         "hierarchical", "iterative", "preview", "ensemble")

#' The merge prompt map_reduce and ensemble send, with the premise rule when
#' the spec checks it. Without, the string tree_merge() has always defaulted
#' to, so passing it changes nothing.
#' @noRd
merge_system_prompt <- function(premise = FALSE) {
  if (isTRUE(premise)) paste0(.gr_prompts$merge_system, .gr_premise_prompts$merge)
  else .gr_prompts$merge_system
}

#' refine's revision prompt, with the premise rule when the spec checks it.
#' @noRd
refine_system_prompt <- function(premise = FALSE) {
  if (isTRUE(premise)) paste0(.gr_prompts$refine_system, .gr_premise_prompts$refine)
  else .gr_prompts$refine_system
}

#' The sentinel opening a reply: decoration (.gr_reply_decoration), an
#' optional label ("Answer:", "**Verdict:**"), then the sentinel as its own
#' word. Not a word that goes on ("FALSE_PREMISES"), nor a file name or a
#' hyphenated name ("FALSE_PREMISE.txt", "FALSE_PREMISE-2"). The first group
#' is the sentinel as written.
#' @noRd
.gr_premise_opens <- paste0(
  "^", .gr_reply_decoration,
  "(?:(?:final[ \\t]+)?(?:answer|response|reply|verdict|result|outcome|status)",
  .gr_reply_decoration, ")?",
  "(FALSE[ _]PREMISE)(?![\\p{L}\\p{N}_])(?![.\\-][\\p{L}\\p{N}])")

#' Is `x` a premise verdict, and if so what does it say the document states?
#'
#' In the style of is_not_found(), because models decorate this sentinel the
#' way they decorate that one: bold (`"**FALSE_PREMISE:** Revenue rose."`),
#' quotes (`"\"FALSE_PREMISE\": Revenue rose."`), a label before it
#' (`"Answer: FALSE_PREMISE - Revenue rose."`), a dash or a line break where
#' the colon was asked for, the punctuation of the language they write in
#' (`"「FALSE_PREMISE」：..."`), or the whole reply in bold. A
#' reply that OPENS with the sentinel is the verdict, and the rest of it,
#' stripped of that decoration, is the correction. Written as the token (with
#' its underscore, or in capitals), it counts before any break; written as
#' words in another case ("False premise"), only before a colon, since "False
#' premise reasoning is ..." is an answer that happens to start that way. The
#' sentinel anywhere else is part of a real answer.
#'
#' @return NULL when `x` is not a premise verdict; otherwise the correction,
#'   which is .gr_premise_unstated when the model gave the sentinel alone.
#' @noRd
premise_correction <- function(x) {
  x <- trimws(to_utf8(as_chr1(x)))
  if (!nzchar(x)) return(NULL)
  m <- regmatches(x, regexec(.gr_premise_opens, x, perl = TRUE, ignore.case = TRUE))[[1]]
  if (!length(m)) return(NULL)
  rest <- substring(x, nchar(m[1]) + 1L)
  word <- m[2]
  # The closing quote mark or bold of the sentinel itself: "FALSE_PREMISE"**.
  # When a quote mark opened it, its partner may stand after a space, as French
  # sets it (« FALSE_PREMISE »), and may be a mark Unicode classes as opening,
  # as German uses („FALSE_PREMISE“, »FALSE_PREMISE«). Only then: after a
  # sentinel no quote mark opened, a quote mark after a space, or an opening
  # one, begins the sentence's own quotation (FALSE_PREMISE “Revenue rose.”),
  # and stays.
  lead <- substr(m[1], 1L, nchar(m[1]) - nchar(word))
  quoted <- grepl("[\"'`\\p{Ps}\\p{Pi}\\p{Pf}][\\s\\p{Z}*_]*$", lead, perl = TRUE)
  closer <- if (quoted) {
    "^(?:[\\s\\p{Z}]*[\"'`\\p{Pi}\\p{Pe}\\p{Pf}])?[\"'`*_\\p{Pi}\\p{Pe}\\p{Pf}]*"
  } else "^[\"'`*_\\p{Pe}\\p{Pf}]*"
  rest <- sub(closer, "", rest, perl = TRUE)
  token <- grepl("_", word, fixed = TRUE) || identical(word, toupper(word))
  if (!token && !grepl("^[ \\t\\p{Zs}]*[:\uff1a]", rest, perl = TRUE)) return(NULL)
  # Then the break the prompt asked for (a colon) or the one the model used
  # instead, and bold closed after it: "**FALSE_PREMISE:** Revenue rose".
  # Only bold after the break, not quote marks: "FALSE_PREMISE: \"Revenue
  # rose\"" keeps the sentence's own quotes.
  rest <- sub(paste0("^[\\s\\p{Z}]*[.!?:;,\\p{Pd}\u3001\u3002\uff01\uff0c\uff0e\uff1a\uff1b\uff1f]*",
                     "[*_`]*[\\s\\p{Z}]*"), "", rest, perl = TRUE)
  rest <- trimws(rest)
  # A reply bolded whole leaves the closing marker at the end of the sentence,
  # and one marker with no partner is decoration, not text.
  for (mk in c("**", "__")) {
    n <- lengths(regmatches(rest, gregexpr(mk, rest, fixed = TRUE)))
    if (n == 1L && endsWith(rest, mk)) rest <- trimws(substr(rest, 1L, nchar(rest) - 2L))
  }
  if (!has_visible(rest)) .gr_premise_unstated else rest
}

#' A reply classified for a reader that may have been told to check the
#' premise.
#'
#' `check` FALSE (the default spec): the text as it is and no `premise`, so a
#' reader that never asked returns exactly what it always returned. Checked:
#' a premise verdict becomes its correction, as `text` and as `premise`;
#' anything else is the text as it is, with `premise` NA. A verdict whose
#' "correction" is the not-found sentinel (`"FALSE_PREMISE: NOT_IN_DOCUMENT"`)
#' is a model that found nothing and used both exits at once, and reads as
#' not found, since reported as an answer it would say the document holds the
#' sentinel.
#' @return `list(text, premise)`; `premise` is NULL, NA or the correction.
#' @noRd
premise_reply <- function(text, check) {
  if (!isTRUE(check)) return(list(text = text, premise = NULL))
  corr <- premise_correction(text)
  if (is.null(corr)) return(list(text = text, premise = NA_character_))
  if (is_not_found(corr)) return(list(text = .NOT_FOUND, premise = NA_character_))
  list(text = corr, premise = corr)
}

#' read_iterative()'s step, whose verdict may be the schema's `false_premise`
#' field, the sentinel inside `answer`, or both.
#'
#' The field set with a blank answer is a verdict without its sentence; with
#' the not-found sentinel as the answer it is the model using both exits, and
#' reads as not found, as premise_reply() reads the same thing in prose.
#' @noRd
premise_field_reply <- function(text, flagged) {
  pr <- premise_reply(text, TRUE)
  # An answer that opens with the sentinel has been read by premise_reply()
  # already, as a correction, a verdict without one, or both exits at once.
  # Tested on its NA instead, "FALSE_PREMISE: NOT_IN_DOCUMENT" with the field
  # set fell through to the branch below and became a verdict whose sentence
  # was the two sentinels.
  if (!isTRUE(flagged) || !is.null(premise_correction(text))) return(pr)
  t <- trimws(as_chr1(text))
  if (!has_visible(t)) return(list(text = .gr_premise_unstated, premise = .gr_premise_unstated))
  if (is_not_found(t)) return(list(text = .NOT_FOUND, premise = NA_character_))
  list(text = t, premise = t)
}

#' A correction as a finding for a merge or an adjudication: with the sentinel
#' in front, which is what the premise rule in merge_system_prompt() tells the
#' model to look for. A finding carried up a merge level unmerged, or as the
#' only one, keeps it, and premise_reply() reads it as the verdict it was.
#' @noRd
premise_finding <- function(correction) paste0(.FALSE_PREMISE, ": ", correction)

#' The correction an answer carries, or NA: for a summary column, from an
#' answer that may be NULL, may not have checked, or may be from before the
#' field existed.
#' @noRd
answer_premise <- function(x) {
  p <- if (is.list(x)) x[["premise", exact = TRUE]] else NULL
  if (is_nonblank(p)) p else NA_character_
}

#' Did the model report that the document contradicts the question?
#' @noRd
premise_flagged <- function(x) !is.na(answer_premise(x))

#' After a reader returns: record on the answer and in the trace that the
#' premise was checked, and what came of it.
#'
#' A built-in reader that returned before any reply could flag anything (no
#' chunk fitted, a limit stopped it) did check, in the sense that matters, and
#' its `$premise` is NA rather than absent. A reader that does not check,
#' `screen`, `extract` or a custom one that did not set it, is left as it is:
#' an absent field is the record that nothing was asked.
#' @noRd
premise_record <- function(out, spec, trace) {
  if (!isTRUE(spec$premise)) return(out)
  if (is.null(out[["premise", exact = TRUE]])) {
    if (!as_chr1(spec$reader, "") %in% .gr_premise_readers) return(out)
    out$premise <- NA_character_
  }
  flagged <- premise_flagged(out)
  trace_note(trace, "read.premise",
             c(list(reader = as_chr1(spec$reader, ""), flagged = flagged),
               if (flagged) list(correction = out$premise)))
  out
}
