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
#' Stems, matched at a word boundary and open at the end, so inflection does not
#' matter: `\\bsuggest` catches "suggests" and "suggested". The boundary is the
#' point. These were matched as bare substrings, which made "improved" an
#' instance of the booster "prove" -- so a draft saying "outcomes improved"
#' licensed a revision saying "proves the drug works", and a revision that ADDED
#' the hedge "unproven" was rejected for introducing a booster. Both directions
#' wrong, from one missing `\\b`.
#' @noRd
.gr_hedges <- c("may", "might", "could", "suggest", "appear", "seem", "indicat",
                "tend", "possibl", "potential", "prelimin", "tentativ", "unclear",
                "uncertain", "limited evidence", "some evidence", "mixed evidence",
                "small", "few", "single study", "one study", "two studies",
                "cannot", "not clear", "no firm", "caution",
                # Weakenings the list did not recognise, so a revision that
                # replaced an over-hedged sentence with a plainly weaker one
                # counted as having dropped all its hedging.
                "unproven", "unproved", "inconclusiv", "insufficient",
                "not significant", "no significant", "remains limited",
                "further research", "further work")

#' "proven" is not listed: `\\bprove` already matches it, and listing both counted
#' the same word twice.
#' @noRd
.gr_boosters <- c("demonstrat", "prove", "establish", "confirm", "conclusiv",
                  "definitiv", "robustly", "unequivocal", "clearly show", "clear evidence",
                  "strong evidence", "shows that", "show that", "well established")

#' Universal quantifiers, which are escalation on their own.
#' @noRd
.gr_universals <- c("\\ball\\b", "\\bevery\\b", "\\balways\\b", "\\bnever\\b",
                    "\\bnone\\b", "\\bno studies\\b", "\\bconsistently\\b",
                    "\\binvariably\\b", "\\buniversally\\b", "\\bwithout exception\\b")

#' Abbreviations that end in a period and do not end a sentence.
#'
#' "(e.g. those over 65)" was split after "e.g.", so the half that carried the
#' hedge was thrown away and the guard measured "those over 65) [study 1]."
#' Matched at a word boundary, because masking a bare "p." would also mask the
#' full stop at the end of "group."
#' @noRd
.gr_abbrev <- c("e.g.", "i.e.", "cf.", "vs.", "et al.", "ca.", "approx.",
                "Fig.", "fig.", "Dr.", "Prof.", "Mr.", "Mrs.", "Ms.", "St.",
                "Inc.", "Ltd.")

#' Split prose into sentences, minus the markdown headings.
#'
#' Headings go first because they are not claims and an editing pass is meant to
#' be free to rewrite them -- and because a heading with no terminator glued
#' itself to the first sentence of its section, which then measured as one unit.
#'
#' The ideographic full stop, question and exclamation marks end a sentence as
#' well, with or without a space after them: Chinese and Japanese put none, so a
#' whole review came back as one sentence.
#' @noRd
split_sentences <- function(txt) {
  txt <- as_chr1(txt)
  if (!nzchar(trimws(txt))) return(character(0))
  txt <- gsub("(^|\n)[ \t]*#{1,6}[^\n]*", "\\1", txt, perl = TRUE)
  for (a in .gr_abbrev) {
    txt <- gsub(paste0("\\b", gsub(".", "\\.", a, fixed = TRUE)),
                gsub(".", "\u0001", a, fixed = TRUE), txt, perl = TRUE)
  }
  txt <- gsub("([\u3002\uff01\uff1f])(?![[:space:]])", "\\1 ", txt, perl = TRUE)
  parts <- unlist(strsplit(txt, "(?<=[.!?\u3002\uff01\uff1f])\\s+", perl = TRUE),
                  use.names = FALSE)
  parts <- trimws(gsub("\u0001", ".", parts, fixed = TRUE))
  parts[nzchar(parts)]
}

#' Sentences as the guard measures them: a run of citation markers standing on
#' its own belongs to the sentence before it.
#'
#' "... a benefit in every adult. [study 1] [study 2]" splits after the full
#' stop, and the markers became a "sentence" of their own -- the only claim
#' sentence, with no words in it -- so the universal in the sentence they cite
#' was never counted. Moving a marker past the full stop was enough to pass.
#' @noRd
measured_sentences <- function(text, word = "study") {
  parts <- split_sentences(text)
  if (length(parts) < 2L) return(parts)
  marker <- cite_grammar(word)$marker
  bare <- gsub(marker, "", parts, perl = TRUE, ignore.case = TRUE)
  # \p{L}\p{N} rather than [[:alnum:]], whose reach depends on the locale.
  alone <- !grepl("[\\p{L}\\p{N}]", bare, perl = TRUE) &
    grepl(marker, parts, perl = TRUE, ignore.case = TRUE)
  alone[1] <- FALSE
  if (!any(alone)) return(parts)
  unname(vapply(split(parts, cumsum(!alone)), paste, character(1), collapse = " "))
}

#' Common English words the guard's word lists presuppose.
#'
#' Function words, which every English paragraph is full of and no other
#' language shares often. The short words English has in common with its
#' neighbours ("a", "in", "no", "on", "to", "is", "was") are kept apart in
#' `.gr_guard_english_shared`: they count as English against another
#' language's words, but cannot vouch for accented text on their own, since
#' "a" is also Czech for "and".
#' @noRd
.gr_guard_english <- c("the", "and", "of", "that", "with", "for", "this", "are", "were", "been",
                       "be", "by", "from", "which", "have", "has", "had", "not", "than", "these",
                       "those", "their", "its", "it", "they", "there", "also", "more", "into",
                       "between", "both", "only", "such", "other", "most", "may", "might",
                       "would", "could", "should", "when", "while", "who", "whether", "however",
                       "although", "because")

#' @noRd
.gr_guard_english_shared <- c("a", "an", "in", "on", "to", "is", "was", "as", "at", "or", "no",
                              "so", "we", "can", "but", "all", "one", "two", "three")

#' Common function words of other languages written in the Latin alphabet.
#'
#' French, Spanish, Portuguese, Italian, German, Dutch, the Scandinavian
#' languages, Czech, Polish, Romanian, Hungarian, Turkish, Indonesian. Words
#' that are also English, or that lower-case to a common abbreviation in a
#' clinical review ("il" in IL-6, "los" for length of stay, "von" in von
#' Willebrand, "de" in de novo), are left out: one of those in an English
#' draft would count against it.
#' @noRd
.gr_guard_foreign <- c(
  "le", "les", "des", "du", "une", "est", "sont", "dans", "pour", "qui", "que", "avec", "sur",
  "ont", "aux", "ces", "cette", "\u00e9t\u00e9", "mais", "leur", "leurs", "elle", "ils",
  "el", "las", "en", "por", "una", "fue", "este", "esta", "pero", "como", "m\u00e1s",
  "uma", "n\u00e3o", "foi", "pelo", "pela", "dos",
  "che", "gli", "della", "delle", "sono", "nel", "alla", "anche",
  "der", "das", "den", "dem", "und", "ist", "sind", "zu", "auf", "f\u00fcr", "nicht", "ein",
  "eine", "einer", "wurde", "wurden", "bei", "aus", "auch", "sich", "wir", "oder",
  "het", "een", "zijn", "werd", "niet", "voor", "ook", "bij", "naar",
  "och", "att", "det", "som", "\u00e4r", "og", "ikke",
  "je", "\u017ee", "jsou", "byl", "jako", "si\u0119", "nie", "jest", "przez",
  "\u0219i", "\u00een", "\u00e9s", "egy", "hogy", "nem",
  "ve", "bir", "i\u00e7in", "olarak",
  "yang", "dan", "dengan", "untuk", "ini", "dari", "pada", "tidak", "adalah")

#' Whether the escalation guard can read a text at all.
#'
#' Its hedges, boosters and universals are English stems. On a French draft
#' there were no hedges to keep, "demontrent clairement" (demonstrate clearly)
#' was no booster and "tous les essais" (all the trials) no universal, so every
#' escalation passed and the revision replaced the draft. A guard that cannot
#' read the text must not approve a revision of it.
#'
#' The test is a count, not language identification: a text is unreadable when
#' letters outside the Latin alphabet are a large share of it (Chinese,
#' Russian, Greek, Arabic ...), when common function words of another
#' language outnumber English ones, when accented letters appear with no
#' English function word to vouch for the text, or when twenty or more words
#' include no English function word at all. It can be wrong both ways on a
#' text of a few words: a short English draft with an accented name is not
#' revised, which costs the revision and never the draft; and a sentence or
#' two in a language with no accents and none of the listed words -- short
#' Indonesian, say -- is read as English, and measured by English words.
#' @noRd
guard_reads <- function(text, word = "study") {
  txt <- gsub(cite_grammar(word)$marker, " ", as_chr1(text), perl = TRUE, ignore.case = TRUE)
  txt <- gsub("(^|\n)[ \t]*#{1,6}[^\n]*", "\\1", txt, perl = TRUE)
  # \p{L}, not [[:alpha:]]: under perl = TRUE the POSIX class is ASCII only,
  # so it could not see the letters this exists to count.
  n_letters <- nchar(gsub("[^\\p{L}]", "", txt, perl = TRUE))
  if (!n_letters) return(TRUE)
  other <- nchar(gsub("[^\\p{L}]|[A-Za-z]", "", txt, perl = TRUE)) / n_letters
  if (other > 0.3) return(FALSE)
  low <- lower_text(txt)
  words <- regmatches(low, gregexpr("\\p{L}+", low, perl = TRUE))[[1]]
  en <- sum(words %in% .gr_guard_english)
  en_any <- en + sum(words %in% .gr_guard_english_shared)
  fo <- sum(words %in% .gr_guard_foreign)
  if (fo > 0L && fo >= en_any) return(FALSE)
  if (en == 0L && other > 0.02) return(FALSE)
  if (en_any == 0L && length(words) >= 20L) return(FALSE)
  TRUE
}

#' Words too common to say which sentence a reworded one came from.
#'
#' For render_revised(), which compares the words of a reworded sentence with
#' the sentences it may have come from. Words under three letters are ignored
#' there already.
#' @noRd
.gr_revise_stopwords <- c("the", "and", "with", "was", "were", "for", "that", "this",
                          "from", "but", "not", "are", "its", "than", "also", "which",
                          "has", "had", "have", "been", "their", "these", "those", "there",
                          "into", "who", "whom", "all", "one", "more", "less", "our")

#' Sentences with nothing dropped between them: pasting them back gives `txt`.
#'
#' For render_revised(), which decides sentence by sentence and has to hand
#' back every character it was given. Cut as split_sentences() cuts, after a
#' full stop, question or exclamation mark that is not part of an abbreviation
#' or inside a bracket ("[study 3 p. 4]" is one citation), and also at every
#' line break, so a heading is a piece of its own. The space after a sentence
#' stays with it.
#' @noRd
sentence_spans <- function(txt) {
  txt <- as_chr1(txt)
  n <- nchar(txt)
  if (!n) return(character(0))
  # Masked character for character, so a position in `masked` is the same
  # position in `txt`.
  masked <- txt
  for (a in .gr_abbrev) {
    masked <- gsub(paste0("\\b", gsub(".", "\\.", a, fixed = TRUE)),
                   gsub(".", "\u0001", a, fixed = TRUE), masked, perl = TRUE)
  }
  br <- gregexpr("\\[[^][\n]*\\]", masked, perl = TRUE)
  regmatches(masked, br) <- lapply(regmatches(masked, br),
                                   function(v) gsub("[.!?]", "\u0001", v))
  m <- gregexpr("(?<=[.!?])[[:space:]]+|[ \t]*\n[[:space:]]*", masked, perl = TRUE)[[1]]
  if (m[1] < 0L) return(txt)
  ends <- as.integer(m) + attr(m, "match.length") - 1L
  ends <- unique(ends[ends < n])
  substring(txt, c(1L, ends + 1L), c(ends, n))
}

#' The sentences in a draft that make a claim.
#'
#' A claim-bearing sentence is one carrying a citation marker. The HEDGE rate is
#' measured over these only: prose between them is what an editing pass is
#' supposed to be free to rewrite, so holding it to a hedging rate would make the
#' guard fire on the work it is meant to permit.
#'
#' Any marker the citation check reads, ranges and locators included. With the
#' listed form alone, a draft citing "[studies 1-3]" had no claim sentences at
#' all, and "Three small trials may suggest" could come back "All trials
#' suggest" with neither the hedge nor the universal rule looking at it.
#' @noRd
claim_sentences <- function(text, word = "study") {
  parts <- measured_sentences(text, word)
  if (!length(parts)) return(character(0))
  parts[grepl(cite_grammar(word)$marker, parts, perl = TRUE, ignore.case = TRUE)]
}

#' Occurrences of each pattern, named, so a revision can be compared to a draft
#' by COUNT and not by which patterns are present.
#'
#' `setdiff()` on the pattern sets let a revision say "demonstrates" five times
#' where the draft said it once: the set of boosters was unchanged, so nothing
#' fired.
#' @noRd
count_marks <- function(low, pats, prefix = TRUE) {
  vapply(pats, function(p) {
    if (!nzchar(low)) return(0L)
    m <- gregexpr(if (prefix) paste0("\\b", p) else p, low, perl = TRUE)[[1]]
    if (m[1] == -1L) 0L else length(m)
  }, integer(1))
}

#' Count hedges, boosters and universals.
#'
#' Hedges and universals over the claim-bearing sentences; BOOSTERS over all
#' prose except headings. The asymmetry is deliberate. A universal has ordinary
#' non-claim uses in framing -- "the evidence, all of which is relevant" -- so
#' measuring it outside a claim would refuse the transitions an editing pass
#' exists to rewrite. A booster does not: an editing pass has no business
#' introducing "demonstrates", "confirms" or "conclusive" anywhere in a review,
#' and restricting the booster check to cited sentences left the one place such a
#' sentence actually gets written -- uncited framing prose between the claims --
#' entirely unguarded.
#'
#' `universals_all` is the universal count over all prose too, for the
#' revision that moved words out of the claim sentences; see strength_guard().
#' @noRd
claim_strength <- function(text, word = "study") {
  sents <- measured_sentences(text, word)
  cited <- sents[grepl(cite_grammar(word)$marker, sents, perl = TRUE, ignore.case = TRUE)]
  low_cited <- lower_text(paste(cited, collapse = " "))
  low_all <- lower_text(paste(sents, collapse = " "))
  list(sentences = length(cited),
       uncited = length(sents) - length(cited),
       hedges = sum(count_marks(low_cited, .gr_hedges)),
       boosters = count_marks(low_all, .gr_boosters),
       universals = count_marks(low_cited, .gr_universals, prefix = FALSE),
       universals_all = count_marks(low_all, .gr_universals, prefix = FALSE))
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
#'   * Fewer hedges than the draft's rate implies, scaled by how much
#'     claim-bearing prose survived. Shorter is allowed to carry fewer hedges --
#'     that is the `cut` pass doing its job -- but never NONE if the draft had
#'     any: `floor()` made the implied minimum 0 whenever the revision had fewer
#'     claim sentences than the draft, so merging two hedged sentences into one
#'     unhedged sentence passed, which is the exact edit this guard exists to
#'     catch. What it does NOT promise is that every legitimate cut passes: a
#'     pass that removes the most heavily hedged sentence lowers the rate, and is
#'     refused. The cost of that is a discarded pass and a warning; the draft
#'     stands.
#'
#' Universals are counted over the claim sentences, so that framing prose can
#' say "all of which" -- unless the revision has FEWER claim sentences or MORE
#' uncited ones than the draft. Then a claim may be sitting in uncited prose,
#' its marker dropped or moved, and universals are counted over everything:
#' "Participants over 65 had more adverse events [study 1]" came back as "All
#' patients over 65 always suffer more adverse events", uncited, and no rule
#' looked at it.
#'
#' And a text the guard cannot read (guard_reads()) is refused outright, the
#' draft or the revision: counting English stems in French finds nothing to
#' refuse.
#' @noRd
strength_guard <- function(before, after, word = "study") {
  for (t in list(list(before, "draft"), list(after, "revision"))) {
    if (!guard_reads(t[[1]], word)) {
      return(list(ok = FALSE, reason = sprintf(
        "the %s is not in English, and the check against strengthening a claim reads English only",
        t[[2]])))
    }
  }
  b <- claim_strength(before, word)
  a <- claim_strength(after, word)
  if (a$sentences < b$sentences || a$uncited > b$uncited) {
    b$universals <- b$universals_all
    a$universals <- a$universals_all
  }
  new_u <- names(a$universals)[a$universals > b$universals]
  if (length(new_u)) {
    return(list(ok = FALSE, reason = sprintf(
      "the revision introduced %s into a claim", paste(gsub("\\\\b", "", new_u), collapse = ", "))))
  }
  new_b <- names(a$boosters)[a$boosters > b$boosters]
  if (length(new_b)) {
    # "into the review", not "into a claim": boosters are counted over the
    # framing prose too, so the sentence that introduced one may carry no marker.
    return(list(ok = FALSE, reason = sprintf(
      "the revision introduced '%s' into the review", paste(new_b, collapse = "', '"))))
  }
  if (b$sentences > 0L && a$sentences > 0L) {
    need <- if (b$hedges == 0L) 0L else
      max(1L, as.integer(ceiling(b$hedges * min(1, a$sentences / b$sentences))))
    if (a$hedges < need) {
      return(list(ok = FALSE, reason = sprintf(
        "the revision dropped hedging: %d hedge(s) across %d claim sentence(s), where the draft's rate implies at least %d",
        a$hedges, a$sentences, need)))
    }
  }
  list(ok = TRUE, reason = NA_character_)
}

#' How many times the text cites each study, named by id.
#'
#' A range counts once for every id in it, so "[studies 1-3]" and "[study 1]
#' [study 2] [study 3]" cite the same.
#' @noRd
cite_counts <- function(text, word = "study") {
  txt <- as_chr1(text)
  hits <- regmatches(txt, gregexpr(cite_grammar(word)$marker, txt, perl = TRUE,
                                   ignore.case = TRUE))[[1]]
  # Each distinct marker read once: a review cites the same few studies many times.
  u <- unique(hits)
  ids <- unlist(lapply(u, cite_marker_ids, word = word)[match(hits, u)], use.names = FALSE)
  if (!length(ids)) return(stats::setNames(integer(0), character(0)))
  tab <- table(as.integer(ids))
  stats::setNames(as.integer(tab), names(tab))
}

#' Why a revision moved citations in a way the set comparison cannot see, or NA.
#'
#' revise_once() compares the SET of studies cited, which a revision could keep
#' while doing two things the documentation says discard a pass:
#'
#'   * citing a study more often than the draft did. Each extra marker
#'     attributes one more statement to that study; with the set unchanged,
#'     "The drug cures the disease [study 1]" could be added to a draft that
#'     cited study 1 once, for something else.
#'   * taking the marker off a claim sentence that stays, which is what fewer
#'     claim sentences alongside more uncited ones means. The claim is then
#'     the review's own assertion, with the study still cited somewhere else.
#'     A cut that removes a claim sentence whole is not this: it leaves fewer
#'     claim sentences and no more uncited ones.
#'
#' A citation dropped as a sentence is cut, or merged into its neighbour's, is
#' allowed: that is what the `cut` pass is for.
#' @noRd
citation_shift <- function(before, after, word = "study") {
  b <- cite_counts(before, word)
  a <- cite_counts(after, word)
  was <- b[names(a)]
  was[is.na(was)] <- 0L
  more <- names(a)[a > was]
  if (length(more)) {
    return(sprintf("the revision cited %s more often than the draft did",
                   paste(sprintf("study %s (%d times, not %d)", more, a[more], was[more]),
                         collapse = ", ")))
  }
  count <- function(txt) {
    sents <- measured_sentences(txt, word)
    cited <- sum(grepl(cite_grammar(word)$marker, sents, perl = TRUE, ignore.case = TRUE))
    list(sentences = cited, uncited = length(sents) - cited)
  }
  sb <- count(before)
  sa <- count(after)
  lost <- min(sb$sentences - sa$sentences, sa$uncited - sb$uncited)
  if (lost > 0L) {
    return(sprintf(paste0("the revision took the citation off %d claim sentence(s): %d fewer ",
                          "carry a marker, and %d more carry none"),
                   lost, sb$sentences - sa$sentences, sa$uncited - sb$uncited))
  }
  NA_character_
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
#' rather than the document. `steps` is each kept pass's text in order, which
#' render_revised() follows one pass at a time.
#' @noRd
synth_revise <- function(drafted, question, client, spec, trace, style = NULL,
                         passes = .gr_revise_passes) {
  text <- as_chr1(drafted)
  report <- list()
  steps <- character(0)
  # Every pass is measured by strength_guard(), whose words are English. On a
  # draft in another language it could refuse nothing, so a pass that turned
  # "three small trials suggest" into "trials prove" was kept and published.
  # Not sent at all, rather than sent and then refused: the call costs money
  # and its result could not be kept.
  unread <- length(passes) > 0L && !guard_reads(text)
  if (unread) {
    gr_warn(paste0("The revision pass(es) were not run: the check that refuses a revision which ",
                   "strengthens a claim reads English, and this draft is not in English. The draft ",
                   "is kept as written."),
            class = "gr_revision_unguarded")
  }
  for (pass in passes) {
    step <- if (unread) {
      list(ran = FALSE, kept = FALSE,
           reason = "the draft is not in English, which the escalation guard reads")
    } else revise_once(text, question, client, spec, trace, style, pass)
    report[[length(report) + 1L]] <- data.frame(
      pass = pass, ran = isTRUE(step$ran), kept = isTRUE(step$kept),
      # Comma-joined rather than a list column: this frame is printed, written to
      # an audit report and read back out of a CSV, and a list column survives
      # none of that.
      lost = as_chr1(step$lost, NA_character_), added = as_chr1(step$added, NA_character_),
      reason = as_chr1(step$reason, NA_character_), stringsAsFactors = FALSE)
    if (isTRUE(step$kept)) {
      text <- step$text
      steps <- c(steps, text)
    }
  }
  rep_df <- if (length(report)) do.call(rbind, report) else
    data.frame(pass = character(0), ran = logical(0), kept = logical(0),
               lost = character(0), added = character(0), reason = character(0),
               stringsAsFactors = FALSE)
  rownames(rep_df) <- NULL
  list(text = if (identical(text, as_chr1(drafted))) NULL else text, report = rep_df,
       steps = steps)
}

#' Every provider's spelling of "stopped at the output limit".
#'
#' Chat Completions says "length", Anthropic "max_tokens", Gemini "MAX_TOKENS",
#' and the Responses API -- this package's default -- reports the reply's
#' status, "incomplete", with "max_output_tokens" as the reason. Only "length"
#' was recognised, so on the default path a revision cut off after its last
#' unique citation passed every other check and replaced the draft, and the
#' published review ended mid-sentence. Compared case-insensitively, and
#' whether or not the client has already normalised the value.
#' @noRd
.gr_cut_off_reasons <- c("length", "max_tokens", "max_output_tokens", "incomplete")

#' Whether a model reply stopped at its output limit rather than finishing.
#' @noRd
reply_cut_off <- function(res) {
  lower_text(as_chr1(res$finish_reason, "")) %in% .gr_cut_off_reasons
}

#' @noRd
revise_once <- function(drafted, question, client, spec, trace, style, pass) {
  sys <- .gr_revise_prompts[[pass]]
  if (is_nonblank(style)) sys <- paste0(sys, "\n\nRegister: ", as_chr1(style))
  # "never": the messages built below carry the question once.
  overhead <- prompt_overhead(question, sys, "never")

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
  # `bud$output < need` as well as the input test. gr_budget() shrinks the output
  # reserve to the floor rather than failing when the window is tight, and a
  # smaller reserve makes bud$input LARGER -- so a draft with no room for its own
  # rewrite passed this check, and the call then went out with max_output = need
  # anyway. Bigger drafts were being sent where smaller ones were skipped.
  if (bud$output < need || gr_count_tokens(drafted) > bud$input) {
    gr_warn(sprintf(paste0("The draft does not fit one prompt alongside room to rewrite it, so ",
                           "the '%s' pass was skipped."), pass),
            class = "gr_coherence_skipped")
    return(list(ran = FALSE, kept = FALSE, reason = "draft exceeds the context window"))
  }
  if (!trace_can_call(trace)) {
    return(list(ran = FALSE, kept = FALSE, reason = paste(cap_name(trace), "reached")))
  }
  res <- gr_call(client, list(
    list(role = "system", content = sys),
    list(role = "user", content = paste0("Review question: ", question)),
    list(role = "user", content = paste0("<draft>\n", drafted, "\n</draft>"))
  ), model = spec$model, max_output = need, temperature = spec$temperature,
     trace = trace, label = paste0("synthesise.revise.", pass))

  # Before usable_text(): a reply cut off at the limit is truncated whether or
  # not the client counts it as a success, and saying "the call failed" about it
  # would send the reader looking at the network instead of at the limit.
  if (reply_cut_off(res)) {
    gr_warn(sprintf(paste0("The '%s' pass was discarded: the revision arrived cut off before it ",
                           "finished (finish reason '%s')."),
                    pass, as_chr1(res$finish_reason)),
            class = "gr_coherence_truncated")
    return(list(ran = TRUE, kept = FALSE, reason = "revision truncated"))
  }
  if (!usable_text(res)) {
    return(list(ran = TRUE, kept = FALSE, reason = "the revision call failed"))
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
  # The comparison above sees only what cited_ids() can read. A revision that
  # wrote "[studies 2 to 9]" matched the draft's citations exactly and replaced
  # it, and the published review carried a citation no check had looked at --
  # while every section's own count of unreadable citations described the
  # draft, not this.
  unread <- setdiff(unparsed_citations(res$text, "study"), unparsed_citations(drafted, "study"))
  if (length(unread)) {
    reason <- sprintf("the revision wrote citation(s) the check cannot read (%s)",
                      paste(unread, collapse = ", "))
    gr_warn(sprintf("The '%s' pass was discarded: %s.", pass, reason),
            class = "gr_coherence_rejected")
    return(list(ran = TRUE, kept = FALSE, reason = reason))
  }
  # Same studies, cited differently: a marker reused for a new sentence, or
  # taken off a claim that stays.
  moved <- citation_shift(drafted, res$text, "study")
  if (!is.na(moved)) {
    gr_warn(sprintf("The '%s' pass was discarded: %s.", pass, moved),
            class = "gr_coherence_rejected")
    return(list(ran = TRUE, kept = FALSE, reason = moved))
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
