# read-core.R -- AXIS 3 machinery: the answer object, prompt building, evidence.
#
# WHY THIS FILE EXISTS
# In the old repository the five "modes" were not five methodologies. Traced
# through `main.R`:
#
#   * "Chunked" and "Semantic" called `gpt_read_chunked(chunks, question,
#     use_parallel = use_parallel, ...)` -- the same function with the same
#     arguments on the same already-sorted `chunks` object. Running both issued
#     14 API calls where 7 would do, and `identical(res$Chunked, res$Semantic)`
#     was TRUE.
#   * "MultiPass" re-executed Retrieval and Chunked verbatim, so selecting
#     Retrieval + Chunked + MultiPass ran each of them twice.
#   * "Hierarchical" was map-then-single-reduce, i.e. `map_reduce` with a
#     summarise prompt instead of an answer prompt -- one level, never
#     recursive, and it blew the context window the moment N x 512 summary
#     tokens exceeded the model's limit.
#   * `chunk_method` was decided once for the whole run, so ticking "Semantic"
#     silently changed the chunking -- and therefore the answer -- of every
#     other mode in the same run.
#
# Distinctness is now a property the package can *check*. Every reader declares
# a `signature`: the shape of its traversal (how chunks are selected, how many
# calls it makes, whether state flows forward). `gr_compare()` refuses to bill
# you twice for two recipes that resolve to the same ingestion, the same
# segmentation AND the same read spec -- the signature alone is not the test,
# because two recipes can share a signature and still differ in `top_k`,
# `min_score` or a token cap, all of which change what the model sees.

#' Build a `gr_answer`, the object every reader must return
#'
#' Exported because it is part of the extension API: [gr_read()] rejects
#' anything that does not inherit `"gr_answer"`, so a custom reader registered
#' with [gr_register_reader()] cannot be written without this. Using it also
#' means your reader reports `partial`, `evidence` and `notes` the same way the
#' built-ins do, so [gr_compare()] can put it in the same table.
#'
#' @param text The answer string. Use `"NOT_IN_DOCUMENT"` when the chunks did
#'   not contain the answer; [gr_compare()] counts that separately from a
#'   failure.
#' @param reader Your reader's name, as registered.
#' @param question The question, carried through for the record.
#' @param chunks_used Integer `chunk_id`s that CONTRIBUTED to the answer -- not
#'   every chunk you sent.
#' @param trace The `gr_trace` passed to your reader. Pass it through; do not
#'   create a new one, or your calls will not appear in the run's totals.
#' @param evidence Optional data frame of supporting spans; build it with the
#'   columns `chunk_id`, `text`, `page`, `section`, `score`.
#' @param partial `TRUE` if anything degraded -- a failed call, a dropped chunk,
#'   a truncated prompt. Callers are told to check this before trusting
#'   `$answer`, so setting it honestly matters more than it looks.
#' @param chunks_sent Chunk ids the reader actually put in front of the model,
#'   when that is more than `chunks_used`. Used only to tell a fabricated
#'   citation from a faithful one: a per-chunk reader drops the chunks that
#'   answered "not in this excerpt", and citing one of those is not an invention.
#'   Defaults to `chunks_used`.
#' @param notes Named list of whatever your reader wants to report.
#' @return A [gr_answer].
#' @seealso [gr_register_reader()], [gr_answer], [gr_read()], [new_chunks()]
#' @family reading functions
#' @export
#' @examples
#' # The minimum a custom reader has to return.
#' tr <- gr_trace()
#' a <- new_answer("Revenue was 45.2 million.", "my_reader", "What was revenue?",
#'                 chunks_used = 1L, trace = tr, notes = list(strategy = "first chunk"))
#' a$partial
#' a$notes$strategy
new_answer <- function(text, reader, question, chunks_used, trace, evidence = NULL,
                       partial = FALSE, notes = list(), chunks_sent = NULL) {
  text <- as_chr1(text)

  # A citation pointing at a chunk that was never sent is a fabrication, and the
  # most convincing kind there is: it looks like the thing that would let you
  # check. Parsing for it costs nothing and runs on every answer, including the
  # ones that never asked for citations, where it finds nothing.
  #
  # Compared against what was SENT, not against `chunks_used`. For the per-chunk
  # readers `chunks_used` is only the chunks that CONTRIBUTED, so a model that
  # faithfully cited a chunk which had answered "not in this excerpt" was
  # reported as having fabricated the citation -- a false positive in a
  # hallucination check, which is the one place a false positive is least
  # affordable.
  cited <- cited_chunks(text)
  unknown <- setdiff(cited, as.integer(chunks_sent %||% chunks_used))
  if (length(unknown)) {
    notes$cited_unknown <- unknown
    partial <- TRUE
  }

  # Evidence a reader could not verify against its own source. Only readers
  # whose evidence is model-written carry the columns to check, so this is a
  # no-op for the rest.
  if (is.data.frame(evidence) && !is.null(evidence$verified)) {
    bad <- sum(!is.na(evidence$verified) & !evidence$verified)
    if (bad > 0L) {
      notes$unverified_evidence <- bad
      partial <- TRUE
    }
  }

  structure(list(
    answer = text,
    reader = as_chr1(reader),
    question = as_chr1(question),
    evidence = evidence,
    chunks_used = chunks_used,
    partial = isTRUE(partial),
    notes = notes,
    trace = trace
  ), class = "gr_answer")
}

#' @export
print.gr_answer <- function(x, ...) {
  cat(sprintf("<gr_answer> reader=%s%s\n", x$reader, if (x$partial) " (PARTIAL)" else ""))
  cat(sprintf("  Q: %s\n", substr(x$question, 1, 160)))
  if (!is.null(x$trace)) {
    s <- gr_trace_summary(x$trace)
    cat(sprintf("  %d model call(s)%s, %d in / %d out tokens, %d error(s)\n",
                s$calls, if (s$cached > 0L) sprintf(" (%d cached)", s$cached) else "",
                s$tokens_in, s$tokens_out, s$errors))
  }
  cat("  ---\n")
  cat(x$answer, "\n")
  if (!is.null(x$evidence) && nrow(x$evidence)) {
    cat(sprintf("  ---\n  %d evidence span(s) from chunk(s): %s\n",
                nrow(x$evidence), paste(utils::head(unique(x$evidence$chunk_id), 12), collapse = ", ")))
  }
  invisible(x)
}

#' @export
as_json.gr_answer <- function(x, pretty = TRUE, ...) {
  as_json.default(list(
    answer = x$answer, reader = x$reader, question = x$question,
    partial = x$partial, chunks_used = x$chunks_used, notes = x$notes,
    evidence = x$evidence,
    trace = trace_as_list(x$trace)
  ), pretty = pretty, ...)
}

# ---------------------------------------------------------------------------
# Prompt construction
# ---------------------------------------------------------------------------

.gr_prompts <- list(
  answer_system = paste0(
    "You answer questions using only the supplied document excerpts. ",
    "If the excerpts do not contain the answer, say exactly: NOT_IN_DOCUMENT. ",
    "Never use outside knowledge. Quote figures, dates and names exactly as written."),
  answer_system_cited = paste0(
    "You answer questions using only the supplied document excerpts. ",
    "If the excerpts do not contain the answer, say exactly: NOT_IN_DOCUMENT. ",
    "Never use outside knowledge. Quote figures, dates and names exactly as written. ",
    "Cite the excerpt you relied on for each claim using its bracketed id, e.g. [chunk 3]."),
  extract_system = paste0(
    "You extract evidence, not answers. Copy out the passages of the excerpt that bear on ",
    "the question, verbatim and without commentary. If nothing in the excerpt is relevant, ",
    "reply with exactly: NONE."),
  summarise_system = paste0(
    "You compress text while preserving everything that could bear on the question. ",
    "Keep all specific figures, dates, names and qualifications. Drop only material that ",
    "cannot possibly relate to the question."),
  merge_system = paste0(
    "You consolidate partial findings into one answer. Resolve contradictions by preferring ",
    "the finding with concrete supporting detail, and say so when findings genuinely conflict. ",
    "Do not introduce anything absent from the findings. If none of them answer the question, ",
    "say exactly: NOT_IN_DOCUMENT."),
  refine_system = paste0(
    "You revise a draft answer using a new excerpt. Add what the excerpt supports, correct what ",
    "it contradicts, and leave the rest of the draft alone. Return the complete revised answer, ",
    "not a description of your edits."),
  synthesise_system = paste0(
    "You write the '%s' section of a review, using only the study records supplied. Cite the ",
    "record behind every claim with its bracketed id, e.g. [study 3], and cite more than one ",
    "where more than one supports it. Say where the records disagree and where they are silent; ",
    "'not reported' is a finding and worth stating. Do not add studies, figures or conclusions ",
    "that are not in the records, and do not cite an id that is not there. Write prose for the ",
    "section only -- no heading, no preamble, no closing summary of what you just wrote."),
  preview_system = paste0(
    "You plan how to read a document, before reading it. You are given the question and an ",
    "outline: one line per section, with its size and a short excerpt. Decide, for each ",
    "section, whether it must be read in full ('read'), is worth scanning for stray relevant ",
    "detail ('skim'), or cannot bear on the question and should be skipped ('skip'). Reading ",
    "everything in full is the expensive default you are here to improve on, so mark 'read' ",
    "only where the section is likely to contain the answer itself. When the outline is too ",
    "thin to tell, prefer 'skim' over 'skip': a skipped section is never looked at again. Give ",
    "a short reason for each decision."),
  claims_system = paste0(
    "You turn a table of studies into a list of CLAIMS about the literature.\n\n",
    "A claim is a statement about the body of work, not about one study. ",
    "\"Trials in clinical samples found a benefit and the two community samples did not\" is a ",
    "claim. \"Study 4 found a benefit\" is not -- that is a row, and the table already has it.\n\n",
    "Rules:\n",
    "- Every claim names the studies it rests on, by the [study N] numbers shown. ",
    "Use only numbers that appear. Never invent one.\n",
    "- Where studies disagree, put the ones supporting the claim in supported_by and the ones ",
    "contradicting it in contradicted_by, and name in `moderator` the FIELD NAME from the table ",
    "that distinguishes them, copied exactly. Leave `moderator` null if nothing in the table ",
    "explains the split -- an unexplained disagreement is a finding, and guessing at a reason ",
    "is worse than reporting it as unexplained.\n",
    "- A claim may rest on one study, but say so in `scope`.\n",
    "- Do not write a claim you cannot attach to at least one study.\n",
    "- Write the claim itself without citations; the numbers go in the fields."),
  reconcile_system = paste0(
    "You are given numbered claims drawn from different batches of ONE literature. ",
    "Group the ones that make the same assertion.\n\n",
    "- Return every number exactly once, in exactly one group.\n",
    "- Two claims belong together only if they assert the same thing. Claims about different ",
    "populations, different measures, or opposite directions are DIFFERENT claims even when ",
    "they are worded alike.\n",
    "- A claim that matches nothing else is a group of one.\n",
    "- Do not write new claims and do not use numbers that were not listed."),
  outline_system = paste0(
    "You are given the CLAIMS a body of literature supports. Group them into the sections of a ",
    "review.\n\n",
    "The grouping is the review's argument, so group by what the claims are ABOUT -- a shared ",
    "population, a shared measure, a shared disagreement -- not by the order they are listed ",
    "in.\n\n",
    "Rules:\n",
    "- Every claim number goes in exactly one section. Do not leave one out and do not repeat ",
    "one.\n",
    "- Use only the numbers listed.\n",
    "- A heading names what the section argues, not a topic label. \"Measurement explains the ",
    "disagreement\" is a heading; \"Measurement\" is a label.\n",
    "- Order the sections so the review reads as an argument: what was studied before what was ",
    "found, and disagreements after the claims they are about.\n",
    "- `rationale` says in one line why those claims belong together."),
  claims_section_system = paste0(
    "You write one section of a literature review from a list of CLAIMS and the studies behind ",
    "them.\n\n",
    "Argue the claims. Do not walk through the studies one at a time -- the claims are the ",
    "structure, and a paragraph per study is what this replaces.\n\n",
    "Rules:\n",
    "- Every sentence that makes a claim carries the [study N] markers of the studies it rests ",
    "on. Cite more than one where more than one supports it.\n",
    "- Use only the numbers shown.\n",
    "- Where a claim is contested, say so in the same sentence or the next, and give the ",
    "distinguishing field if one is named. Do not resolve a disagreement the evidence leaves ",
    "open.\n",
    "- Keep every claim's hedging. If a claim rests on one small study, the sentence must say ",
    "something a reader can check that against.\n",
    "- Cover every claim you are given. Do not add claims that are not in the list."),
  coherence_system = paste0(
    "You are given the complete draft of a review, written section by section by ",
    "someone who could not see the other sections while writing. Your job is to make it read as ",
    "one argument. Fix repetition between sections, add the connective sentences that carry a ",
    "reader from one section to the next, and make terminology and tense consistent throughout. ",
    "Keep every heading, in the order given.\n\n",
    "You may not add anything. No new studies, no new figures, no new claims, no conclusion the ",
    "draft does not already reach. Every bracketed citation marker must survive exactly as it is ",
    "written, attached to the same claim -- do not add one, remove one, renumber one, or move one ",
    "to a different sentence. If a section is thin, leave it thin: that is a finding about the ",
    "evidence, and padding it would be inventing evidence. Return the full revised document and ",
    "nothing else."),
  screen_system = paste0(
    "You screen one document against a review's criteria, using only the excerpt supplied. ",
    "Answer 'include' only if the excerpt shows the document meets every inclusion criterion ",
    "and no exclusion criterion. Answer 'exclude' only if the excerpt clearly shows it fails ",
    "one; name that criterion. If the excerpt does not settle the decision, answer 'unclear' -- ",
    "that is a correct and expected answer, and it is much better than a guess, because an ",
    "'unclear' document is looked at by a person while a wrong 'exclude' is never seen again. ",
    "Do not use anything you know about the document from outside the excerpt.")
)

#' Sentinel the readers use to mark "this chunk had nothing".
#' @noRd
.NOT_FOUND <- "NOT_IN_DOCUMENT"

#' Did the model report that the document does not contain the answer?
#'
#' Every reader is instructed to reply with exactly `"NOT_IN_DOCUMENT"` when the
#' excerpts do not answer the question. Use this rather than
#' `grepl("NOT_IN_DOCUMENT", ans$answer)`: a real answer can quote the sentinel
#' ("the log said NOT_IN_DOCUMENT, but revenue was 45.2 million"), and models do
#' not reproduce the token byte-exactly -- they wrap it in quotes, bold it, or
#' add a full stop. This matches the sentinel *alone*, modulo that decoration,
#' and treats a blank answer as not-found too.
#'
#' The v1 test was `grepl("not found|no information|not applicable", ...)` over
#' the whole response, which threw away every answer that happened to contain
#' one of those phrases.
#'
#' @param x An answer string, or `ans$answer`.
#' @return `TRUE` if the string is the not-found sentinel (or blank).
#' @seealso [gr_answer], [gr_compare()], whose `summary$not_found` column is
#'   this predicate applied per recipe
#' @family reading functions
#' @export
#' @examples
#' is_not_found("NOT_IN_DOCUMENT")
#' is_not_found("**NOT_IN_DOCUMENT.**")     # models decorate it
#' is_not_found("")                          # nothing came back
#'
#' # A real answer that merely mentions the sentinel is NOT not-found.
#' is_not_found("The log said NOT_IN_DOCUMENT, but revenue was 45.2 million.")
is_not_found <- function(x) {
  x <- trimws(as_chr1(x))
  if (!nzchar(x)) return(TRUE)
  # The sentinel alone, modulo surrounding punctuation and formatting. Models do
  # not reproduce it byte-exactly, so accept the common trailing punctuation and
  # a space instead of the underscores; anything longer is a real answer.
  grepl("^[\"'`*_ ]*NOT[ _]IN[ _]DOCUMENT[\"'`*_.!:;, ]*$", x, ignore.case = TRUE)
}

#' A model call that succeeded but returned nothing usable.
#'
#' A blank completion is not an answer and must not be reported as one, nor
#' spliced into a downstream prompt as evidence.
#' @noRd
usable_text <- function(res) {
  isTRUE(res$ok) && nzchar(trimws(as_chr1(res$text)))
}

#' Render chunks into a labelled block for a prompt.
#' @noRd
render_chunks <- function(df, ids = NULL) {
  if (!nrow(df)) return("")
  ids <- ids %||% df$chunk_id
  paste(vapply(seq_len(nrow(df)), function(i) {
    loc <- c(if (!is.na(df$page[i])) sprintf("p.%d", df$page[i]),
             if (!is.na(df$section[i])) sprintf("\u00a7 %s", df$section[i]))
    hdr <- sprintf("[chunk %s%s]", ids[i], if (length(loc)) paste0(" ", paste(loc, collapse = ", ")) else "")
    paste0(hdr, "\n", df$text[i])
  }, character(1)), collapse = "\n\n")
}

#' How long a body has to be before the question is worth repeating.
#'
#' A number, and an arbitrary one: what it should be is a question about a
#' particular corpus and a particular model, which is why `restate` is a read
#' setting -- gr_compare() can take two recipes differing only in it and measure
#' the difference, rather than the package asserting one.
#' @noRd
.gr_restate_tokens <- 1200L

#' @noRd
restate_now <- function(body, how = "auto") {
  how <- as_chr1(how, "auto")
  if (identical(how, "always")) return(TRUE)
  if (identical(how, "never")) return(FALSE)
  gr_count_tokens(as_chr1(body)) > .gr_restate_tokens
}

#' The question again, after a body long enough to bury the first ask.
#' @noRd
restate_tail <- function(body, question, how = "auto") {
  if (!restate_now(body, how)) return("")
  paste0("\n\nAgain, the question: ", as_chr1(question))
}

#' The system prompt `answer_messages()` will actually send.
#'
#' Named rather than inlined so that the budget can be computed from the same
#' string the call sends. Budgeting with `answer_system` and then sending
#' `answer_system_cited` understates the overhead by 22 tokens whenever
#' `cite = TRUE`, and the excerpts are then sized to fill the gap.
#' @noRd
answer_system <- function(cite = FALSE) {
  if (isTRUE(cite)) .gr_prompts$answer_system_cited else .gr_prompts$answer_system
}

#' Build the standard question-answering message list.
#' @noRd
answer_messages <- function(question, body, cite = FALSE, label = "Excerpts",
                            restate = "auto") {
  # The question is already LAST here, which is the position that gets followed.
  # What was missing is the other end: over a few thousand tokens of excerpt, an
  # instruction that appears once at the bottom is easy to lose track of on the
  # way down. Added INSIDE the existing message rather than as a fourth one, so
  # nothing that indexes these positionally shifts.
  lead <- if (restate_now(body, restate)) paste0("Question: ", question, "\n\n") else ""
  list(
    list(role = "system", content = answer_system(cite)),
    list(role = "user", content = paste0(lead, "<", tolower(label), ">\n", body,
                                         "\n</", tolower(label), ">")),
    list(role = "user", content = paste0("Question: ", question))
  )
}

#' Greedily fit as many chunks as the budget allows, in the order given. A chunk
#' that would overflow is skipped, not truncated. Returns the indices used and
#' the rendered body.
#'
#' The multi-chunk readers (`stuff`, `retrieve`, `rerank`) go through this; the
#' rest bound their prompts with `gr_truncate_tokens()` or `tree_merge()`. Every
#' path is bounded one way or the other -- that is the failure mode that produced
#' unbounded merge prompts and HTTP 400s in `gpt_read_chunked()` and
#' `gpt_read_hierarchical()`.
#' @noRd
fit_chunks <- function(df, budget_tokens, order = NULL, prefix = FALSE) {
  idx <- order %||% seq_len(nrow(df))
  used <- integer(0); total <- 0L
  for (i in idx) {
    t <- gr_count_tokens(render_chunks(df[i, , drop = FALSE]))
    if (total + t > budget_tokens) {
      # Skipping over an oversized chunk and carrying on is right for a RANKED
      # order -- take as much of the good stuff as fits. It is wrong for a
      # contiguous read: `screen` shows the model the opening of a document, and
      # an opening assembled from chunks 1, 2 and 47 is not an opening. `prefix`
      # stops at the first thing that does not fit.
      if (prefix) break else next
    }
    used <- c(used, i); total <- total + t
  }
  list(idx = used, tokens = total, dropped = setdiff(idx, used))
}

#' Standard overhead accounting for a reader's prompt.
#' @noRd
prompt_overhead <- function(question, system_prompt, restate = "auto") {
  # The question is counted TWICE unless restatement is off, because that is how
  # many times it appears. `answer_messages()` adds a second copy before the body
  # once the body is long, and the body was sized against a budget computed here
  # -- so the prompt overran the window by exactly the length of the question.
  # With a long question and a model whose window the run nearly fills,
  # gr_call() refused to dispatch at all and the reader returned NOT_IN_DOCUMENT
  # with nothing in it but `notes$error`. The default is the safe direction:
  # reserving room that goes unused costs a little context, under-reserving
  # costs the answer.
  again <- if (identical(as_chr1(restate, "auto"), "never")) 0L else
    gr_count_tokens(as_chr1(question))
  sum(gr_count_tokens(c(as_chr1(question), as_chr1(system_prompt)))) + again + 64L
}

#' Turn per-chunk extraction results into an evidence table.
#' Stack evidence tables that need not have the same columns.
#'
#' `ensemble` combines its members' evidence, and members are different readers.
#' Only readers whose evidence is model-written carry the verification columns,
#' so a plain `rbind()` of a `skim` table and a `map_reduce` table fails on
#' "numbers of columns of arguments do not match" -- which is what happened the
#' moment verification was added, and what the v1 shim tests caught. Union the
#' columns and fill what is absent, so a reader may add a column without
#' breaking every reader it can be ensembled with.
#' @noRd
rbind_evidence <- function(tables) {
  tables <- Filter(function(d) is.data.frame(d) && nrow(d), tables)
  if (!length(tables)) return(NULL)
  cols <- unique(unlist(lapply(tables, names), use.names = FALSE))
  filled <- lapply(tables, function(d) {
    for (nm in setdiff(cols, names(d))) d[[nm]] <- NA
    d[, cols, drop = FALSE]
  })
  out <- do.call(rbind, filled)
  rownames(out) <- NULL
  out
}

#' @noRd
evidence_table <- function(chunk_ids, texts, pages = NA_integer_, sections = NA_character_,
                           scores = NA_real_, source_text = NULL,
                           kind = c("verbatim", "extracted", "answer"), extra = NULL) {
  kind <- match.arg(kind)
  n <- length(texts)
  if (!n) {
    out <- data.frame(chunk_id = integer(0), text = character(0), page = integer(0),
                      section = character(0), score = numeric(0), kind = character(0),
                      stringsAsFactors = FALSE)
    for (nm in names(extra)) out[[nm]] <- extra[[nm]][0]
    return(out)
  }
  df <- data.frame(chunk_id = rep(chunk_ids, length.out = n),
                   text = vapply(texts, as_chr1, character(1), USE.NAMES = FALSE),
                   page = rep(pages, length.out = n),
                   section = rep(sections, length.out = n),
                   score = rep(scores, length.out = n),
                   # PER ROW, not per answer. `ensemble` combines evidence from
                   # several readers, so one kind for the whole table said
                   # "mixed" and then string-matched a map_reduce ANSWER against
                   # its chunk -- reporting a correct run as fabricated evidence.
                   kind = kind,
                   stringsAsFactors = FALSE)
  # Only readers whose evidence is MODEL-WRITTEN carry their sources. For the
  # readers that put verbatim chunk text here, source and span are the same
  # string and storing it twice would double an answer's size to prove that a
  # thing equals itself.
  if (!is.null(source_text)) {
    df$source_text <- vapply(rep(source_text, length.out = n), as_chr1, character(1),
                             USE.NAMES = FALSE)
    v <- verify_spans(df$text, df$source_text)
    df$verified <- v$verified
    df$match <- v$match
  }
  # Per-row labels that have to survive the filter below. `extract` puts the
  # FIELD each span supports here; attaching it after the fact would line the
  # labels up against the wrong rows, because the filter drops empty spans and
  # renumbers nothing.
  for (nm in names(extra)) df[[nm]] <- rep(extra[[nm]], length.out = n)
  df[has_content(df$text), , drop = FALSE]
}

#' Merge a set of partial findings into one answer, tree-wise so the merge
#' prompt itself can never exceed the context window.
#' @noRd
tree_merge <- function(client, question, pieces, spec, trace, label = "merge",
                       system_prompt = NULL, kind = "findings") {
  system_prompt <- system_prompt %||% .gr_prompts$merge_system
  pieces <- pieces[has_content(pieces)]
  if (!length(pieces)) return(list(text = .NOT_FOUND, ok = FALSE, levels = 0L))
  if (length(pieces) == 1L) return(list(text = pieces[[1]], ok = TRUE, levels = 0L))

  level <- 0L
  truncated <- 0L
  prev_n <- length(pieces) + 1L
  repeat {
    level <- level + 1L
    # "never": tree_merge() builds its own messages below and does not restate,
    # so the question appears once. Left at the default it would reserve room
    # for a second copy that is never sent and shrink every merge group.
    overhead <- prompt_overhead(question, system_prompt, "never")
    bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)

    # A single finding larger than the whole merge budget cannot be reduced by
    # grouping: it lands alone in its group, a one-element group is returned
    # unchanged, and the loop spins through every level doing nothing before
    # concatenating the lot. Truncate it once, and say so, so each level makes
    # real progress.
    over <- gr_count_tokens(pieces) > bud$input
    if (any(over)) {
      gr_msg(sprintf("Merge: %d finding(s) exceed the %d-token merge budget; truncating them.",
                     sum(over), bud$input))
      pieces[over] <- vapply(pieces[over], gr_truncate_tokens, character(1),
                             n = bud$input, USE.NAMES = FALSE)
      truncated <- truncated + sum(over)
    }

    groups <- list(); buf <- character(0); tks <- 0L
    for (p in pieces) {
      pt <- gr_count_tokens(p)
      if (length(buf) && tks + pt > bud$input) { groups[[length(groups) + 1L]] <- buf; buf <- character(0); tks <- 0L }
      buf <- c(buf, p); tks <- tks + pt
    }
    if (length(buf)) groups[[length(groups) + 1L]] <- buf

    if (length(groups) == 1L && length(groups[[1]]) == length(pieces)) {
      body <- paste(sprintf("<%s %d>\n%s\n</%s %d>", kind, seq_along(pieces), pieces,
                            kind, seq_along(pieces)), collapse = "\n\n")
      if (!trace_can_call(trace)) {
        # The call cap stopped us before the final merge. This path used to
        # return unbounded concatenation -- an "answer" the size of every
        # finding put together -- while the failure path two lines down was
        # carefully capped. Same degradation, same bound.
        return(list(text = merge_giveup(pieces, spec), ok = FALSE, levels = level,
                    truncated = truncated, error = "call cap reached before merging"))
      }
      res <- gr_call(client, list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = body),
        list(role = "user", content = paste0("Question: ", question))
      ), model = spec$model, max_output = spec$max_answer_tokens, temperature = spec$temperature,
         trace = trace, label = label)
      if (!res$ok) {
        # Concatenation is a documented, visible degradation -- not a silent one.
        return(list(text = merge_giveup(pieces, spec), ok = FALSE, levels = level,
                    truncated = truncated, error = res$error))
      }
      return(list(text = res$text, ok = TRUE, levels = level, truncated = truncated))
    }

    gr_msg(sprintf("Merge level %d: %d finding(s) -> %d group(s).", level, length(pieces), length(groups)))
    pieces <- unlist(gr_lapply(groups, function(g, trace) {
      if (length(g) == 1L) return(g)
      if (!trace_can_call(trace)) return(merge_giveup(g, spec))
      body <- paste(sprintf("<%s>\n%s\n</%s>", kind, g, kind), collapse = "\n\n")
      res <- gr_call(client, list(
        list(role = "system", content = system_prompt),
        list(role = "user", content = body),
        list(role = "user", content = paste0("Question: ", question))
      ), model = spec$model, max_output = spec$max_answer_tokens, temperature = spec$temperature,
         trace = trace, label = paste0(label, ".level", level))
      if (res$ok) res$text else paste(g, collapse = "\n\n")
    }, parallel = spec$parallel, label = "merge group", trace = trace),
      use.names = FALSE)
    pieces <- pieces[has_content(pieces)]

    # Progress guard. If a level did not shrink the pile, another level will not
    # either -- every input to it is the same. Stop now with a bounded answer
    # rather than burning six more rounds of calls to reach the same place.
    if (!length(pieces)) return(list(text = .NOT_FOUND, ok = FALSE, levels = level,
                                     truncated = truncated))
    if (length(pieces) >= prev_n || level > 6L) {
      return(list(text = merge_giveup(pieces, spec), ok = FALSE, levels = level,
                  truncated = truncated,
                  error = if (level > 6L) "merge depth cap reached" else "merge made no progress"))
    }
    prev_n <- length(pieces)
  }
}

#' The bounded fallback when merging fails.
#'
#' Returning `paste(pieces, collapse=)` unbounded handed the caller an "answer"
#' that could be the size of the whole document. Cap it at a generous multiple of
#' the answer budget and mark the cut.
#' @noRd
merge_giveup <- function(pieces, spec) {
  cap <- as.integer(clamp((spec$max_answer_tokens %||% 800L) * 3L, 200L, 8000L))
  gr_truncate_tokens(paste(pieces, collapse = "\n\n---\n\n"), cap,
                     "\n\n...[merge failed; findings above are truncated]")
}
