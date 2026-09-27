# screen.R -- decide which documents count, one call each, before reading any of
# them properly.
#
# WHY THIS FILE EXISTS
# Screening is the step a review cannot skip and cannot afford to do the
# expensive way. Two hundred candidate papers extracted in full is two hundred
# times twenty calls; screened, it is two hundred calls, and most of them end the
# document's involvement.
#
# Three things make this a separate stage rather than a cheap `gr_extract()`:
#
#   * EVERY DOCUMENT GETS A DECISION. There is no retrieval step, no relevance
#     prefilter, nothing that can quietly drop a source before a decision is
#     recorded. A document that could not be read is `status = "failed"` with no
#     decision -- a job for a person, not a silent omission. A review whose
#     denominator is unknown is not a review.
#
#   * "UNCLEAR" IS AN ANSWER. Forcing a binary decision out of an excerpt that
#     does not settle the question is how automated screening loses studies. The
#     model is told to say so, and those go to a human. Every published
#     evaluation of LLM screening says the same thing: good at the easy calls,
#     not yet trustworthy alone on the hard ones.
#
#   * THE REASON IS RECORDED. An exclusion without a stated criterion cannot be
#     checked, appealed, or reported, and every reporting standard asks for the
#     count excluded at each criterion.

#' @noRd
read_screen <- function(chunks, question, client, spec, trace) {
  include <- as.character(spec[["include"]] %||% character(0))
  exclude <- as.character(spec[["exclude"]] %||% character(0))
  if (!length(include) && !length(exclude)) {
    gr_abort(paste0("The 'screen' reader needs `include` and/or `exclude` criteria. Build them ",
                    "with gr_protocol(), or use gr_screen(), which does this for you."),
             class = "gr_no_criteria")
  }
  d <- chunks$chunks
  # Nothing to show the model is not a judgement on an excerpt, so not
  # "unclear": the document fails and is outstanding, as an unreadable one is.
  if (!NROW(d)) {
    gr_abort("There is nothing to screen: the document has no chunks.",
             class = "gr_empty_chunks")
  }
  listing <- criteria_prompt(include, exclude)
  # "never": the message list below carries the question once.
  overhead <- prompt_overhead(paste(question, listing), .gr_prompts$screen_system, "never")
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)
  cap <- min(bud$input, as_int1(spec[["screen_tokens"]], bud$input))

  # A CONTIGUOUS opening, not the chunks that happen to fit. What a screener
  # reads is the front of the paper -- title, abstract, opening -- and a decision
  # made from paragraphs 1, 2 and 47 is not that, however well it fits.
  fit <- fit_chunks(d, cap, prefix = TRUE)
  sub <- d[fit$idx, , drop = FALSE]
  if (!length(fit$idx)) {
    # The first chunk alone is over the cap -- a structural chunk of title,
    # abstract and introduction runs to 900 tokens, and screen_tokens of "a few
    # hundred" is what the docs suggest. What was asked for is the opening, so
    # the first chunk is cut down to the cap and that is screened. Returning
    # "unclear" instead made no call at all, and a whole corpus came back as
    # model deferrals on openings nobody had read.
    sub <- screen_opening(d[1, , drop = FALSE], cap)
    if (is.null(sub)) {
      given <- as_int1(spec[["screen_tokens"]], NA_integer_)
      gr_abort(if (!is.na(given) && given <= bud$input) {
        sprintf(paste0("`screen_tokens = %d` leaves no room to show the model any of the ",
                       "document once the chunk's heading is counted. Raise it."), given)
      } else {
        paste0("The question and criteria leave no room in the model's context for any of ",
               "the document.")
      }, class = "gr_bad_setting")
    }
  }
  truncated <- nrow(sub) < nrow(d) || !identical(sub$text[1], d$text[1])
  # The chunks' own token counts, not `fit$tokens`. That one measures the
  # RENDERED prompt, which carries a "[chunk N]" header per chunk, so a document
  # that was read whole reported seeing more tokens than it contains -- a table
  # saying 26 of 19 is worse than one saying nothing.
  seen_tokens <- as.integer(sum(sub$tokens))

  # Checked, as every request is. A request a limit stopped was not sent, so it
  # is not a failed call; gr_read() names the limit.
  capped <- !trace_can_call(trace)
  out <- if (capped) list(ok = FALSE, value = NULL) else gr_call_json(client, list(
    list(role = "system", content = .gr_prompts$screen_system),
    list(role = "user", content = paste0("Review question: ", question)),
    list(role = "user", content = listing),
    list(role = "user", content = paste0(
      "<excerpt>\n", render_chunks(sub), "\n</excerpt>",
      if (truncated) paste0("\n\n(This is the opening ", seen_tokens, " of about ",
                            sum(d$tokens), " tokens. If the excerpt does not settle the ",
                            "decision, answer 'unclear'.)") else ""))
  ), schema = .gr_screen_schema, schema_name = "screening",
     model = spec$model, max_output = spec$max_answer_tokens,
     temperature = spec$temperature, trace = trace, label = "screen.decide")

  v <- if (isTRUE(out$ok)) out$value else list()
  failed <- !isTRUE(out$ok) && !capped
  # No reply that could be read, no decision: NA, not "unclear". "Unclear" is a
  # judgement the model made about an excerpt it read, and gr_flow() and
  # gr_calibrate() count it as one. A provider outage recorded that way showed
  # every document as a model deferral and none as unread.
  # json_field(): `$` partial-matches, so a reply carrying `decisions` satisfied
  # a read of `decision` and a real "include" was recorded from a key the schema
  # never defined.
  decision <- if (isTRUE(out$ok)) screen_decision(json_field(v, "decision")) else NA_character_
  # Held to the protocol. The criterion used to be recorded as whatever the
  # model wrote, so an adult trial excluded as "Conducted outside Europe" --
  # a criterion no protocol listed -- left the review with nothing to say it
  # happened, and the flow diagram gained a category nobody fixed in advance.
  # An exclusion has to name one of the protocol's criteria; one that names
  # another, or none, goes to a person as "unclear".
  crit <- screen_criterion(decision, as_chr1(json_text(v, "criterion"), NA_character_),
                           include, exclude)
  reason <- as_chr1(json_text(v, "reason"), NA_character_)
  if (crit$downgraded) reason <- downgrade_reason(crit$criterion, reason)
  decision <- crit$decision
  # Said in the notes, where partial_reasons() and gr_read_many() look. A reply
  # that arrived but was not the JSON asked for has no transport error of its own.
  error <- if (!failed) NA_character_ else {
    res <- out$result
    if (!is.null(res) && !isTRUE(res$ok)) as_chr1(res$error, "the request failed")
    else "the reply was not the JSON object the screening schema asks for"
  }
  quote <- as_chr1(json_text(v, "quote"), "")
  # Checked against the document's words, not a model's. A segmenter that puts
  # model-written text into `text` (a contextual header, propositions it
  # rewrote) keeps the original in `source_text`; NA, or no such column, means
  # `text` is itself the source. A quote copied out of a header would otherwise
  # verify as a sentence of the paper.
  said <- sub$text
  if ("source_text" %in% names(sub)) {
    orig <- as.character(sub[["source_text"]])
    said <- ifelse(is.na(orig), said, orig)
  }
  ev <- if (nzchar(trimws(quote))) screen_evidence(quote, sub, said) else NULL

  new_answer(if (is.na(decision)) "" else decision, "screen", question,
             if (is.null(ev)) integer(0) else ev$chunk_id[!is.na(ev$chunk_id)],
             trace, chunks_sent = if (capped) integer(0) else sub$chunk_id, evidence = ev,
             # A failed call is partial. "unclear" is NOT: it is a correct answer
             # meaning a person has to look, and marking it partial would put a
             # right answer and a broken one in the same bucket. Nor is
             # truncation, which is recorded in its own columns because
             # title-and-abstract screening is a method, not a defect.
             partial = !isTRUE(out$ok),
             notes = list(decision = decision,
                          reason = reason,
                          criterion = crit$criterion,
                          criterion_valid = crit$valid,
                          seen_tokens = seen_tokens,
                          document_tokens = as.integer(sum(d$tokens)),
                          truncated = truncated,
                          failed_call = failed, error = error))
}

#' Decide which documents a review should read
#'
#' The stage before [gr_extract()]. One model call per document, a decision and a
#' reason for every one, and nothing dropped on the way.
#'
#' @param sources As [gr_read_many()]: file paths, a directory, or raw text.
#' @param max_total_calls,trace As [gr_read_many()]. Pass the same `trace` to
#'   [gr_extract()] and [gr_synthesise()] and it accumulates the whole review,
#'   while each stage keeps its own for its own cost and its own ceiling.
#' @param protocol A [gr_protocol()] carrying the criteria and the review
#'   question. Give this, or `include`/`exclude` directly.
#' @param question,include,exclude The review question and the criteria, if you
#'   are not passing a protocol. Each criterion is one statement a document
#'   either meets or does not.
#' @param recipe,client,store,on_error,max_total_usd,recursive,keep_answers As
#'   [gr_extract()].
#' @param screen_tokens Cap what the model is shown, in tokens, counted from the
#'   start of the document. Leaving it unset shows as much as the model's context
#'   allows. Setting it to a few hundred is title-and-abstract screening, done
#'   deliberately: cheaper, and closer to what a human screener sees at this
#'   stage. The opening is whole chunks while they fit; when the first chunk is
#'   longer than the cap on its own (a structural chunk of title, abstract and
#'   introduction can be), it is cut to the cap and that is what is screened. A
#'   cap too small to show any of the document at all fails the document, with
#'   an `error` naming `screen_tokens`. Either way the `truncated` and
#'   `seen_tokens` columns say what was actually read.
#' @param ... Recipe overrides, as in [gr_extract()].
#'
#' @return An object of class `gr_screening`:
#'   \describe{
#'     \item{`table`}{One row per document: `document`, `document_id`,
#'       `decision`, `reason`, `criterion`, `criterion_valid`, `quote`,
#'       `verified`, `seen_tokens`, `document_tokens`, `truncated`, `status`,
#'       `duplicate_of`, `error`.}
#'     \item{`included`}{The distinct sources whose decision was `"include"`
#'       (the paths, not the display labels). Duplicates are left out; they are
#'       the same study, and `table` still has their rows. `gr_extract()` takes
#'       this, but prefer handing it the whole screening object: a character
#'       vector of paths cannot carry the search, so `gr_extract(screened)`
#'       keeps it and `gr_extract(screened$included)` does not.}
#'     \item{`records`}{The [gr_records()] the run was made over, or `NULL`.}
#'     \item{`summary`,`answers`,`trace`,`store`}{As [gr_extract()].}
#'   }
#'
#' @section Three decisions, not two:
#' `decision` is `"include"`, `"exclude"` or `"unclear"`. The third is not a
#' failure mode; it is the answer when the excerpt does not settle the
#' question, and it is what stops an uncertain call from being recorded as a
#' confident one. Those documents are for a person to look at. Every published
#' evaluation of automated screening reaches the same conclusion: reliable on the
#' easy calls, not yet trustworthy alone on the hard ones.
#'
#' A document that could not be read at all gets `status = "failed"` and no
#' decision. It is not excluded, and it is not silently absent: it is an
#' outstanding job. A review whose denominator is unknown is not a review. The
#' same goes for a document whose screening request failed, or whose reply was
#' not the JSON asked for: no model judged it, so it is not "unclear" either.
#' `error` says what happened, and with a `store` the next run screens it again.
#'
#' @section The criterion is checked:
#' `criterion` is the criterion the model said decided it, in the protocol's
#' own words when it matches one of `include` or `exclude` (case, spacing, a
#' leading list marker, surrounding quotes and closing punctuation aside);
#' `criterion_valid` says whether it did, and is `NA` when the model named
#' none. An exclusion has to name one of the protocol's criteria: an exclusion
#' criterion it meets, or an inclusion criterion it fails. One that names
#' another criterion, or none, is recorded as `"unclear"` so that a person
#' decides, with `criterion_valid = FALSE` (or `NA`) and a `reason` saying
#' so. The match is on the wording, so a paraphrased criterion is held back
#' too; that costs a person a look, where accepting an invented one would cost
#' a study.
#'
#' The quote is checked against the excerpt the model was shown and attributed
#' to the chunk that contains it; a quote spanning two chunks, or one that does
#' not verify, has no chunk.
#'
#' @section Reporting it:
#' `table(x$table$decision)` is the screening result and
#' `table(x$table$criterion[x$table$decision %in% "exclude"])` is the
#' breakdown of exclusions by criterion, which is what a flow diagram asks for;
#' every one names a criterion of the protocol. Duplicates were removed before
#' screening, so `sum(!is.na(x$table$duplicate_of))` is the "duplicates
#' removed" count and every one of them still has a row; see [gr_read_many()].
#'
#' @seealso [gr_protocol()], [gr_extract()], [gr_read_many()]
#' @export
#' @examples
#' cl <- gr_mock_client(function(messages, params) {
#'   '{"decision":"include","reason":"Reports a randomised comparison.",
#'     "criterion":"Reports a randomised comparison",
#'     "quote":"We randomly assigned participants to two groups."}'
#' })
#'
#' f <- tempfile(fileext = ".txt")
#' writeLines("We randomly assigned participants to two groups.", f)
#'
#' s <- gr_screen(f, question = "Does the treatment work?",
#'                include = "Reports a randomised comparison", client = cl)
#' s$table[, c("document", "decision", "reason")]
gr_screen <- function(sources, protocol = NULL, question = NULL, include = NULL,
                      exclude = NULL, recipe = "research", client = NULL, store = NULL,
                      screen_tokens = NULL, on_error = c("continue", "stop"),
                      max_total_usd = NULL, max_total_calls = NULL,
                      keep_answers = FALSE, recursive = FALSE, trace = NULL,
                      ...) {
  if (!is.null(protocol)) {
    if (!inherits(protocol, "gr_protocol")) {
      gr_abort("`protocol` must come from gr_protocol().", class = "gr_bad_protocol")
    }
    check_protocol_edited(protocol, "screen against it")
    if (is.null(question)) question <- protocol$question
    if (is.null(include)) include <- protocol$include
    if (is.null(exclude)) exclude <- protocol$exclude
    if (missing(recipe)) recipe <- protocol$recipe %||% recipe
  }
  include <- criteria_vector(include, "include")
  exclude <- criteria_vector(exclude, "exclude")
  if (!length(include) && !length(exclude)) {
    gr_abort(paste0("Screening needs criteria. Pass a gr_protocol(), or `include` and/or ",
                    "`exclude` as character vectors: one statement per element, each one a ",
                    "document either meets or does not."),
             class = "gr_no_criteria")
  }
  if (!is_nonblank(question)) {
    gr_abort("Screening needs a `question`: the criteria are read in light of it.")
  }

  base <- as_recipe(recipe)
  rd <- unclass(base$read)
  rd$reader <- "screen"
  rd$include <- include
  rd$exclude <- exclude
  rd$screen_tokens <- screen_tokens
  rec <- gr_recipe(paste0(base$name, "+screen"), ingest = base$ingest,
                   segment = base$segment, read = rd)

  max_total_calls <- as_call_ceiling(max_total_calls)
  out <- gr_read_many(sources, question, rec, client = client, store = store,
                      on_error = on_error, max_total_usd = max_total_usd,
                      max_total_calls = max_total_calls, keep_answers = TRUE,
                      recursive = recursive, trace = trace, ...)

  docs <- out$summary$document
  answers <- out$answers[docs]
  names(answers) <- docs
  tab <- screening_table(docs, answers, out$summary, include, exclude)

  structure(list(
    table    = tab,
    # The SOURCES, not summary$document, which is a display label with no way
    # back to a file -- gr_extract(screened$included) then failed with "file not
    # found" on every row. And distinct: a duplicate is the same study, so
    # extracting it again buys nothing but the chance of counting it twice.
    included = out$sources[!is.na(tab$decision) & tab$decision == "include" &
                             is.na(tab$duplicate_of)],
    include  = include,
    exclude  = exclude,
    summary  = out$summary,
    answers  = if (isTRUE(keep_answers)) out$answers else list(),
    # Carried so the audit can show the search without being handed the record
    # set a second time at the end of a run.
    records  = out$records,
    trace    = out$trace,
    store    = out$store
  ), class = "gr_screening")
}

#' @export
print.gr_screening <- function(x, ...) {
  tab <- x$table
  cat(sprintf("<gr_screening> %d document(s) screened against %d criteri%s\n",
              nrow(tab), length(x$include) + length(x$exclude),
              if (length(x$include) + length(x$exclude) == 1L) "on" else "a"))
  dec <- table(factor(tab$decision, levels = c("include", "exclude", "unclear")))
  cat(sprintf("  %s\n", paste(sprintf("%d %s", as.integer(dec), names(dec)), collapse = ", ")))
  undecided <- sum(is.na(tab$decision))
  if (undecided) {
    cat(sprintf("  %d could not be read and have NO decision; these are outstanding\n",
                undecided))
  }
  if (sum(tab$decision == "unclear", na.rm = TRUE)) {
    cat("  'unclear' means the excerpt did not settle it; those are for a person\n")
  }
  dup <- sum(!is.na(tab$duplicate_of))
  if (dup) cat(sprintf("  %d were duplicates of a document already screened\n", dup))
  trunc <- sum(tab$truncated, na.rm = TRUE)
  if (trunc) {
    cat(sprintf("  %d decided on the opening of the document, not all of it\n", trunc))
  }
  cost <- gr_trace_cost(x$trace)
  total <- if (nrow(cost)) sum(cost$usd) else 0
  # format_call_counts(), not trace$calls: `calls` counts embeddings requests
  # too, so a run with 2 model calls and 4 embeddings requests printed "6 model
  # call(s)" above a trace that said 2.
  cat(sprintf("  this run: %s, %s\n", format_call_counts(x$trace),
              if (!nrow(cost)) "no cost recorded"
              else if (is.na(total)) "cost unknown (unpriced model)"
              else sprintf("$%.4f", total)))
  invisible(x)
}

# --- internals -------------------------------------------------------------

.gr_screen_schema <- list(
  type = "object", additionalProperties = FALSE,
  required = list("decision", "reason", "criterion", "quote"),
  properties = list(
    decision = list(type = "string", enum = list("include", "exclude", "unclear"),
                    description = paste0("'include' if the excerpt meets every inclusion ",
                                         "criterion and no exclusion criterion; 'exclude' if it ",
                                         "clearly fails one; 'unclear' if the excerpt does not ",
                                         "settle it.")),
    reason = list(type = "string",
                  description = "One sentence saying why, referring to what the excerpt says."),
    criterion = list(type = c("string", "null"),
                     description = paste0("The criterion that decided it, copied from the list ",
                                          "above, or null if no single one did.")),
    quote = list(type = c("string", "null"),
                 description = paste0("The sentence in the excerpt that decided it, copied ",
                                      "verbatim, or null if none does."))))

#' @noRd
criteria_prompt <- function(include, exclude) {
  part <- function(label, v) {
    if (!length(v)) return(NULL)
    paste0(label, "\n", paste(sprintf("- %s", v), collapse = "\n"))
  }
  paste(c(part("Include a document only if it meets ALL of:", include),
          part("Exclude a document if ANY of these is true:", exclude)),
        collapse = "\n\n")
}

#' The opening of a document whose first chunk is over the cap: that chunk cut
#' down to fit, heading and all, or NULL when not even a word of it does.
#'
#' Measured on the rendered chunk, as fit_chunks() measures, and cut again by
#' the overshoot if the heading and the text count to more together than apart.
#' @noRd
screen_opening <- function(row, cap) {
  bare <- row
  bare$text <- ""
  room <- cap - gr_count_tokens(render_chunks(bare))
  for (attempt in 1:3) {
    if (room <= 0) return(NULL)
    # A generous character prefix first: gr_truncate_tokens() searches the whole
    # text for the boundary, and a chunk of 100,000 words cut to 300 tokens took
    # a second. Sixteen characters a token is more than any text needs to hold
    # `room` tokens; where it does not, the whole text is searched as before.
    cut <- ""
    if (nchar(row$text) > room * 16) {
      pre <- substr(row$text, 1L, room * 16)
      if (gr_count_tokens(pre) > room) cut <- gr_truncate_tokens(pre, room, marker = "")
    }
    if (!nzchar(cut)) cut <- gr_truncate_tokens(row$text, room, marker = "")
    if (!nzchar(trimws(cut))) return(NULL)
    try_row <- row
    try_row$text <- cut
    over <- gr_count_tokens(render_chunks(try_row)) - cap
    if (over <= 0) {
      try_row$tokens <- as.integer(gr_count_tokens(cut))
      if (!is.null(try_row$chars)) try_row$chars <- nchar(cut)
      return(try_row)
    }
    room <- room - over
  }
  NULL
}

#' The screening quote as evidence, attributed to the chunk it is in.
#'
#' Verified against the whole excerpt, since that is what the model read, but
#' placed in the chunk that holds it. Pinned to the first chunk sent instead,
#' a deciding sentence on page 3 was reported on page 1, verified, and a person
#' checking it looked in the wrong place. A quote found in no single chunk --
#' one spanning two, or one that did not verify -- has no chunk, page or
#' section rather than the first chunk's.
#' @noRd
screen_evidence <- function(quote, sub, said) {
  ev <- evidence_table(NA_integer_, quote, NA_integer_, NA_character_,
                       source_text = paste(said, collapse = "\n\n"), kind = "extracted")
  if (!nrow(ev) || !isTRUE(ev$verified[1])) return(ev)
  at <- if (length(said) == 1L) 1L else {
    found <- NA_integer_
    for (i in seq_along(said)) {
      if (isTRUE(span_match(quote, said[i])$verified)) { found <- i; break }
    }
    found
  }
  if (!is.na(at)) {
    ev$chunk_id[1] <- sub$chunk_id[at]
    ev$page[1] <- sub$page[at]
    ev$section[1] <- sub$section[at]
    ev$source_text[1] <- said[at]
  }
  ev
}

#' Hold screening decisions to the protocol's criteria.
#'
#' Vectorised over `decision` and `criterion`. A criterion that matches one of
#' `include` or `exclude` once case, spacing, a list marker, surrounding quotes
#' and closing punctuation are set aside is recorded in the protocol's own
#' words, so the per-criterion count groups as the protocol does; `valid` says
#' whether it matched (NA when none was named). An "exclude" whose criterion is
#' not the protocol's -- or that names none -- becomes "unclear", which is
#' what sends a document to a person, and `downgraded` marks it.
#' @noRd
screen_criterion <- function(decision, criterion, include, exclude) {
  listed <- c(as.character(include), as.character(exclude))
  key <- criterion_key(criterion)
  named <- !is.na(criterion) & nzchar(key)
  at <- ifelse(named, match(key, criterion_key(listed)), NA_integer_)
  valid <- ifelse(named, !is.na(at), NA)
  down <- !is.na(decision) & decision == "exclude" & !(valid %in% TRUE)
  list(decision = ifelse(down, "unclear", decision),
       criterion = ifelse(is.na(at), criterion, listed[at]),
       valid = valid, downgraded = down)
}

#' A criterion folded for comparison. Lower case by lower_text(), per the
#' package's rule, so the comparison does not depend on the locale.
#' @noRd
criterion_key <- function(x) {
  if (!length(x)) return(character(0))
  x <- gsub("[[:space:]]+", " ", to_utf8(x), perl = TRUE)
  x <- sub("^ *[-*\u2022]+ *", "", x, perl = TRUE)
  q <- "[\"'\u2018\u2019\u201c\u201d]"
  x <- gsub(sprintf("^ *%s+|%s+ *$", q, q), "", x, perl = TRUE)
  x <- sub("[.;:,]+ *$", "", x, perl = TRUE)
  lower_text(trimws(x))
}

#' The reason recorded for an exclusion held back as "unclear".
#' @noRd
downgrade_reason <- function(criterion, reason) {
  why <- ifelse(is.na(criterion) | !nzchar(trimws(to_utf8(criterion))),
                "without naming a criterion",
                sprintf("under \"%s\", which is not one of the protocol's criteria", criterion))
  sprintf("Not excluded: the model excluded it %s, so a person should decide. Its reason: %s",
          why, ifelse(is.na(reason), "none given", reason))
}

#' Read a decision back, defaulting to the one that is never wrong.
#'
#' For a reply that was read: anything unrecognised in it -- a missing field, a
#' new label -- becomes "unclear", which routes the document to a person. The
#' two alternatives are both worse: defaulting to "exclude" loses studies
#' silently, and defaulting to "include" quietly buys a full extraction for
#' every document the screener could not read. A call that failed, or a reply
#' that was not JSON at all, never reaches this: `read_screen()` records no
#' decision for it, and the corpus marks the document "failed".
#' @noRd
screen_decision <- function(x) {
  v <- lower_text(trimws(as_chr1(x, "")))
  if (v %in% c("include", "exclude", "unclear")) v else "unclear"
}

#' @noRd
screening_table <- function(docs, answers, summary, include = character(0),
                            exclude = character(0)) {
  note <- function(d, field, default) {
    a <- answers[[d]]
    if (is.null(a)) default else {
      v <- a$notes[[field]]
      if (is.null(v)) default else v
    }
  }
  ev_of <- function(d) {
    a <- answers[[d]]
    if (is.null(a) || !is.data.frame(a$evidence) || !nrow(a$evidence)) NULL else a$evidence[1, ]
  }
  tab <- data.frame(
    document    = as.character(docs),
    document_id = as.character(summary$document_id %||% NA_character_),
    # NA, not "unclear", when the document was never read. "Unclear" is a
    # judgement about a document someone looked at; this is the absence of one.
    decision    = vapply(docs, function(d) as_chr1(note(d, "decision", NA_character_),
                                                   NA_character_),
                         character(1), USE.NAMES = FALSE),
    reason      = vapply(docs, function(d) as_chr1(note(d, "reason", NA_character_),
                                                   NA_character_),
                         character(1), USE.NAMES = FALSE),
    criterion   = vapply(docs, function(d) as_chr1(note(d, "criterion", NA_character_),
                                                   NA_character_),
                         character(1), USE.NAMES = FALSE),
    criterion_valid = rep(NA, length(docs)),   # filled in below
    quote       = vapply(docs, function(d) {
                    e <- ev_of(d); if (is.null(e)) NA_character_ else as_chr1(e$text)
                  }, character(1), USE.NAMES = FALSE),
    verified    = vapply(docs, function(d) {
                    e <- ev_of(d)
                    if (is.null(e) || is.null(e$verified)) NA else as.logical(e$verified)
                  }, logical(1), USE.NAMES = FALSE),
    seen_tokens = vapply(docs, function(d) as_int1(note(d, "seen_tokens", NA_integer_)),
                         integer(1), USE.NAMES = FALSE),
    document_tokens = vapply(docs, function(d) as_int1(note(d, "document_tokens", NA_integer_)),
                             integer(1), USE.NAMES = FALSE),
    truncated   = vapply(docs, function(d) {
                    v <- note(d, "truncated", NA); if (is.null(v)) NA else isTRUE(v)
                  }, logical(1), USE.NAMES = FALSE),
    status      = as.character(summary$status),
    duplicate_of = as.character(summary$duplicate_of %||% NA_character_),
    error       = as.character(summary$error),
    stringsAsFactors = FALSE
  )
  unread <- !summary$status %in% c("ok", "restored", "duplicate")
  tab$decision[unread] <- NA_character_
  tab$truncated[unread] <- NA
  # Checked again here, for answers a store restored from a run made before
  # read_screen() checked them. On an answer it did check this changes nothing.
  crit <- screen_criterion(tab$decision, tab$criterion, include, exclude)
  if (any(crit$downgraded)) {
    tab$reason[crit$downgraded] <- downgrade_reason(tab$criterion[crit$downgraded],
                                                    tab$reason[crit$downgraded])
  }
  tab$decision <- crit$decision
  tab$criterion <- crit$criterion
  tab$criterion_valid <- crit$valid
  rownames(tab) <- NULL
  tab
}
