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
  listing <- criteria_prompt(include, exclude)
  overhead <- prompt_overhead(paste(question, listing), .gr_prompts$screen_system)
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)
  cap <- min(bud$input, as_int1(spec[["screen_tokens"]], bud$input))

  # A CONTIGUOUS opening, not the chunks that happen to fit. What a screener
  # reads is the front of the paper -- title, abstract, opening -- and a decision
  # made from paragraphs 1, 2 and 47 is not that, however well it fits.
  fit <- fit_chunks(d, cap, prefix = TRUE)
  if (!length(fit$idx)) {
    return(new_answer("unclear", "screen", question, integer(0), trace,
                      chunks_sent = integer(0), partial = TRUE,
                      notes = list(decision = "unclear",
                                   reason = "not even the first chunk fits one prompt",
                                   criterion = NA_character_, seen_tokens = 0L,
                                   document_tokens = sum(d$tokens), truncated = TRUE)))
  }
  sub <- d[fit$idx, , drop = FALSE]
  truncated <- length(fit$idx) < nrow(d)
  # The chunks' own token counts, not `fit$tokens`. That one measures the
  # RENDERED prompt, which carries a "[chunk N]" header per chunk, so a document
  # that was read whole reported seeing more tokens than it contains -- a table
  # saying 26 of 19 is worse than one saying nothing.
  seen_tokens <- as.integer(sum(sub$tokens))

  out <- gr_call_json(client, list(
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
  decision <- screen_decision(v$decision)
  quote <- as_chr1(v$quote, "")
  ev <- if (nzchar(trimws(quote))) {
    evidence_table(sub$chunk_id[1], quote, sub$page[1], sub$section[1],
                   source_text = paste(sub$text, collapse = "\n\n"), kind = "extracted")
  } else NULL

  new_answer(decision, "screen", question, if (is.null(ev)) integer(0) else ev$chunk_id,
             trace, chunks_sent = sub$chunk_id, evidence = ev,
             # A failed call is partial. "unclear" is NOT: it is a correct answer
             # meaning a person has to look, and marking it partial would put a
             # right answer and a broken one in the same bucket. Nor is
             # truncation, which is recorded in its own columns because
             # title-and-abstract screening is a method, not a defect.
             partial = !isTRUE(out$ok),
             notes = list(decision = decision,
                          reason = as_chr1(v$reason, NA_character_),
                          criterion = as_chr1(v$criterion, NA_character_),
                          seen_tokens = seen_tokens,
                          document_tokens = as.integer(sum(d$tokens)),
                          truncated = truncated,
                          failed_call = !isTRUE(out$ok)))
}

#' Decide which documents a review should read
#'
#' The stage before [gr_extract()]. One model call per document, a decision and a
#' reason for every one, and nothing dropped on the way.
#'
#' @param sources As [gr_read_many()]: file paths, a directory, or raw text.
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
#'   stage. Either way the `truncated` and `seen_tokens` columns say what was
#'   actually read.
#' @param ... Recipe overrides, as in [gr_extract()].
#'
#' @return An object of class `gr_screening`:
#'   \describe{
#'     \item{`table`}{One row per document: `document`, `document_id`,
#'       `decision`, `reason`, `criterion`, `quote`, `verified`, `seen_tokens`,
#'       `document_tokens`, `truncated`, `status`, `duplicate_of`, `error`.}
#'     \item{`included`}{The sources whose decision was `"include"`, ready to
#'       hand to [gr_extract()].}
#'     \item{`summary`,`answers`,`trace`,`store`}{As [gr_extract()].}
#'   }
#'
#' @section Three decisions, not two:
#' `decision` is `"include"`, `"exclude"` or `"unclear"`. The third is not a
#' failure mode -- it is the answer when the excerpt does not settle the
#' question, and it is what stops an uncertain call from being recorded as a
#' confident one. Those documents are for a person to look at. Every published
#' evaluation of automated screening reaches the same conclusion: reliable on the
#' easy calls, not yet trustworthy alone on the hard ones.
#'
#' A document that could not be read at all gets `status = "failed"` and no
#' decision. It is not excluded, and it is not silently absent: it is an
#' outstanding job. A review whose denominator is unknown is not a review.
#'
#' @section Reporting it:
#' `table(x$table$decision)` is the screening result and
#' `table(x$table$criterion)` is the breakdown by exclusion criterion, which is
#' what a flow diagram asks for. Duplicates were removed before screening, so
#' `sum(!is.na(x$table$duplicate_of))` is the "duplicates removed" count and
#' every one of them still has a row -- see [gr_read_many()].
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
                      max_total_usd = NULL, keep_answers = FALSE, recursive = FALSE,
                      ...) {
  if (!is.null(protocol)) {
    if (!inherits(protocol, "gr_protocol")) {
      gr_abort("`protocol` must come from gr_protocol().", class = "gr_bad_protocol")
    }
    if (is.null(question)) question <- protocol$question
    if (is.null(include)) include <- protocol$include
    if (is.null(exclude)) exclude <- protocol$exclude
    if (missing(recipe)) recipe <- protocol$recipe %||% recipe
  }
  include <- criteria_vector(include, "include")
  exclude <- criteria_vector(exclude, "exclude")
  if (!length(include) && !length(exclude)) {
    gr_abort(paste0("Screening needs criteria. Pass a gr_protocol(), or `include` and/or ",
                    "`exclude` as character vectors -- one statement per element, each one a ",
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

  out <- gr_read_many(sources, question, rec, client = client, store = store,
                      on_error = on_error, max_total_usd = max_total_usd,
                      keep_answers = TRUE, recursive = recursive, ...)

  docs <- out$summary$document
  answers <- out$answers[docs]
  names(answers) <- docs
  tab <- screening_table(docs, answers, out$summary)

  structure(list(
    table    = tab,
    included = out$summary$document[!is.na(tab$decision) & tab$decision == "include"],
    include  = include,
    exclude  = exclude,
    summary  = out$summary,
    answers  = if (isTRUE(keep_answers)) out$answers else list(),
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
    cat(sprintf("  %d could not be read and have NO decision -- these are outstanding\n",
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
  cat(sprintf("  this run: %d model call(s), %s\n", x$trace$calls,
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

#' Read a decision back, defaulting to the one that is never wrong.
#'
#' Anything unrecognised -- a failed call, a model that answered in prose, a new
#' label -- becomes "unclear", which routes the document to a person. The two
#' alternatives are both worse: defaulting to "exclude" loses studies silently,
#' and defaulting to "include" quietly buys a full extraction for every document
#' the screener could not read.
#' @noRd
screen_decision <- function(x) {
  v <- tolower(trimws(as_chr1(x, "")))
  if (v %in% c("include", "exclude", "unclear")) v else "unclear"
}

#' @noRd
screening_table <- function(docs, answers, summary) {
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
  rownames(tab) <- NULL
  tab
}
