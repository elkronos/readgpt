# revise-length.R -- whether a `cut` pass cut anything.
#
# WHY THIS FILE EXISTS
# Every check revise_once() makes on a revision asks whether it is SAFE: the
# same studies cited, as often, on the same sentences, written so the check
# can read them, claiming no more than the draft did. None asked whether the
# pass did its JOB. A `cut` pass that came back polished instead, or handed the
# draft back as it was, or longer, passed every one of them and was kept: the
# review was reported as cut to length when nothing had been cut, and
# `$coherence` gave the reader no number to see it by.
#
# So each revision that gets this far is measured, in words of prose, against
# the text its pass was given, and a `cut` that is not shorter is refused.
#
# WHY ONLY THE CUT
# The other two passes were told not to cut, and refusing one that shrank the
# review would be just as cheap. It would not be well-founded. Counted on a
# padded two-section draft of 232 words, the hardest polish that could be
# written sentence by sentence, keeping every sentence, claim, hedge and
# citation marker, came back at 118 words, 49% removed: "due to the fact that"
# becomes "because", and that is the `register` pass doing its job. The same
# draft with its closing section dropped -- a pass that returned only part of
# the review -- came back at 162 words, 30% removed. A bound loose enough never
# to refuse the first cannot catch the second: words cannot tell cutting
# wordiness from cutting content. The counts are reported instead, kept passes
# included, so a `register` pass that halved the review can be seen.

#' Words of prose in a review, the unit the length check counts in.
#'
#' Words, not tokens. gr_count_tokens() answers in whichever tokenizer the
#' session has set -- the heuristic, tiktoken, "words", a custom one -- so the
#' same revision could be shorter under one setting and not under another. A
#' token count also moves when nothing a reader reads does: "[study 1] [study
#' 2] [study 3]" written as "[studies 1-3]" is fewer tokens and the same
#' sentence. And the reason is read by a person, who counts length in words.
#'
#' Headings and citation markers are left out, as guard_reads() leaves them
#' out, and so are list markers: deleting a heading, writing three markers as
#' one range, or turning a numbered list into bullets, is not cutting the
#' review. A word is a run of non-space characters holding a letter or a
#' digit, so "12%" and "all-cause" are one word each and a dash standing
#' between spaces is none. `\\p{L}\\p{N}` rather than `[[:alnum:]]`,
#' whose reach depends on the locale.
#'
#' A language written without spaces would count as very few words. No such
#' text gets here: a draft the escalation guard cannot read is never revised,
#' and a revision it cannot read is refused before this is asked.
#' @noRd
prose_words <- function(text, word = "study") {
  txt <- gsub(cite_grammar(word)$marker, " ", as_chr1(text), perl = TRUE, ignore.case = TRUE)
  txt <- gsub("(^|\n)[ \t]*#{1,6}[^\n]*", "\\1", txt, perl = TRUE)
  # "1." holds a digit and "-" does not, so a numbered list rewritten as
  # bullets lost a word per item, and a cut that removed nothing was kept.
  # Space after the marker is required, so a line opening on "1.5 mg" keeps it.
  txt <- gsub("(^|\n)[ \t]*(?:[-*+]|[0-9]+[.)])[ \t]+", "\\1", txt, perl = TRUE)
  as.integer(sum(grepl("[\\p{L}\\p{N}]", words_of(txt), perl = TRUE)))
}

#' Measure a revision against the text its pass was given, and say why it is
#' refused, or NA.
#'
#' Only the `cut` pass is judged, and only by direction: strictly fewer words,
#' with no minimum share. How much a cut should remove depends on how much the
#' draft repeats itself, which nothing here knows. A tight draft written
#' section by section from its claims may have nothing to lose but filler, and
#' "It is important to note that" is six words of an 800-word review, under
#' 1%: any floor above that refuses the cut the prompt asked for. Nor would a
#' floor buy what it seems to. A polish shortens too, by half in the count at
#' the top of this file, so no share of words tells a cut from a polish; what
#' the length can show is a cut that removed nothing, and that is what is
#' refused.
#'
#' The measurement goes into the trace as a local step whichever way the
#' check goes, so the trace says what was measured as well as what was sent.
#' @noRd
revision_length <- function(before, after, pass, trace = NULL, word = "study") {
  b <- prose_words(before, word)
  a <- prose_words(after, word)
  reason <- if (identical(pass, "cut") && a >= b) {
    sprintf("the cut pass did not shorten the review: %d words before, %d after", b, a)
  } else NA_character_
  trace_note(trace, paste0("synthesise.revise.", pass, ".length"),
             list(words_before = b, words_after = a, ok = is.na(reason)))
  list(words_before = b, words_after = a, reason = reason)
}
