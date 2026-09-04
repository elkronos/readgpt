# extract-corpus.R -- a schema, a folder, and one table out.
#
# WHY THIS FILE EXISTS
# `gr_read_many()` answers one question of many documents and gives you a column
# of prose. That is the right shape for "what does each of these say about X"
# and the wrong shape for every downstream thing a review actually does, which
# is counting, sorting, filtering and joining.
#
# `gr_extract()` is the same loop with a schema in the middle: one row per
# document, one column per field, typed. It deliberately owns almost no
# machinery of its own -- error isolation, the resumable store, per-document
# budgets, the corpus cost ceiling and the trace all come from `gr_read_many()`,
# because a second copy of that loop would drift from the first and the drift
# would only show up four hours into someone's run.
#
# What it adds is the two tables: the record, and the provenance. Every filled
# cell can be traced to the chunk and the quoted sentence it came from, and the
# quote has already been checked against that chunk's text. An extraction table
# without that column is a claim; with it, it is a claim you can audit.

#' Extract a typed schema from many documents
#'
#' The corpus counterpart to [gr_fields()]: applies one extraction schema to
#' every document and returns a tidy table, one row per document and one column
#' per field, with a separate long table saying where every value came from.
#'
#' @param sources As [gr_read_many()]: file paths, a directory, or raw text.
#' @param fields A [gr_fields()] schema. This is the goal; the argument below is
#'   only framing.
#' @param goal One sentence of context for the extraction -- "screening trials
#'   for a review of statins in primary prevention". It sharpens judgement calls
#'   about what counts as the primary outcome; it does not decide what is
#'   collected, because `fields` does that. Defaults to a neutral instruction.
#' @param recipe The ingest and segmentation to use. The reader is always
#'   `extract`, whatever the recipe says.
#' @param client,store,on_error,max_total_usd,recursive As [gr_read_many()].
#'   `store` is worth setting for anything longer than a coffee break: an
#'   interrupted extraction resumes instead of restarting.
#' @param resolve What to do when two parts of one document give different values
#'   for the same field. `"first"` (default) takes the earlier one and records
#'   the disagreement in the `conflicts` column, costing nothing. `"model"`
#'   spends one extra call per disagreeing field to adjudicate.
#' @param require_quote Discard any value the model could not tie to a verbatim
#'   span in the chunk it cited. Off by default: an extracted value is never
#'   thrown away without being asked for, and `n_unverified` makes the same
#'   problem visible without destroying anything. Turn it on for a protocol that
#'   says no quote, no datum.
#' @param keep_answers Keep the underlying [gr_answer] objects in `$answers`.
#'   They hold every chunk's source text, so for a large corpus this is what
#'   runs you out of memory; the tables do not need them.
#' @param ... Recipe overrides, as in [gr_read_many()] -- `max_tokens =` is the
#'   one that matters here, because it decides how many chunks each document is
#'   cut into and therefore how many calls it costs.
#'
#' @return An object of class `gr_extraction`:
#'   \describe{
#'     \item{`table`}{One row per document: `document`, one column per field in
#'       the schema and of that field's type, then `n_filled`, `n_unverified`,
#'       `conflicts`, `status`, `error`.}
#'     \item{`evidence`}{Long form, one row per filled cell: `document`,
#'       `field`, `chunk_id`, `page`, `section`, `quote`, `verified`, `match`.}
#'     \item{`fields`}{The schema, so the table can be read without it.}
#'     \item{`summary`}{The per-document run summary from [gr_read_many()],
#'       including calls, tokens, cost and seconds.}
#'     \item{`answers`}{The `gr_answer` objects, when `keep_answers = TRUE`.}
#'     \item{`trace`,`store`}{As [gr_read_many()].}
#'   }
#'
#' @section NA means two different things:
#' A cell is `NA` either because the document was looked at and does not report
#' that field, or because the document was never successfully read. The `status`
#' column tells them apart, and it matters: "not reported" is a finding you can
#' publish, and "failed" is a job you have to redo. Filtering an extraction table
#' without checking `status` silently turns the second into the first.
#'
#' @section What it costs:
#' One call per chunk per document -- every chunk is read, because a schema field
#' can be answered by a sentence anywhere in the paper and a retrieval step that
#' looked at the top eight chunks would miss it silently. Reconciliation is free
#' unless a document contradicts itself. Cut the cost by segmenting more coarsely
#' (`max_tokens =`), not by looking at fewer chunks.
#'
#' @section Verifying it:
#' Every filled cell is asked for the sentence it came from, and that sentence is
#' checked against the text of the chunk it was attributed to. Nothing is ever
#' discarded for failing: a paraphrase stays in `$evidence` with
#' `verified = FALSE` and the fraction of it that did match in `match`.
#'
#' `n_unverified` counts the cells in that row whose value could not be tied to a
#' verbatim span -- either because no quote was given, or because the quote is
#' not in the chunk. That column is the one to look at before believing a table:
#' `n_unverified` of zero means every value in the row can be pointed at in the
#' document. A row where it is not zero is not wrong, but it is unaudited, and
#' the answer is marked `partial` to say so. `require_quote = TRUE` turns the
#' count into a policy and drops those values instead.
#'
#' See [gr_verify_evidence()] for the same check on a single answer.
#'
#' @seealso [gr_fields()], [gr_read_many()], [gr_verify_evidence()],
#'   [gr_trace_cost()]
#' @export
#' @examples
#' fields <- gr_fields(
#'   design = "The study design",
#'   n      = gr_field("Number of participants", type = "integer")
#' )
#'
#' # A mock that fills the form, so the example runs offline.
#' cl <- gr_mock_client(function(messages, params) {
#'   '{"design":"randomised controlled trial","n":120,
#'     "design__quote":"We ran a randomised controlled trial.",
#'     "n__quote":"We enrolled 120 participants."}'
#' })
#'
#' f <- tempfile(fileext = ".txt")
#' writeLines("We ran a randomised controlled trial. We enrolled 120 participants.", f)
#'
#' x <- gr_extract(f, fields, client = cl)
#' x$table[, c("document", "design", "n", "n_unverified", "status")]
#' x$evidence[, c("field", "quote", "verified")]
gr_extract <- function(sources, fields, goal = NULL, recipe = "research",
                       client = NULL, store = NULL, resolve = c("first", "model"),
                       require_quote = FALSE, on_error = c("continue", "stop"),
                       max_total_usd = NULL, keep_answers = FALSE,
                       recursive = FALSE, ...) {
  if (!inherits(fields, "gr_fields")) {
    gr_abort("`fields` must come from gr_fields().", class = "gr_no_fields")
  }
  resolve <- match.arg(resolve)
  goal <- if (is.null(goal)) {
    paste0("Extract the fields below from this document, exactly as it reports them.")
  } else {
    if (!is_nonblank(goal)) gr_abort("`goal` must be a non-empty string.")
    as_chr1(goal)
  }

  # The reader is not negotiable: `recipe` chooses how the document is cleaned
  # and cut up, and this function is defined by what it does afterwards. Letting
  # a recipe's own reader through would make gr_extract() return an answer
  # object with no record in it, which fails much later and much less clearly.
  base <- as_recipe(recipe)
  rd <- unclass(base$read)
  rd$reader <- "extract"
  rd$fields <- fields
  rd$resolve <- resolve
  rd$require_quote <- isTRUE(require_quote)
  rec <- gr_recipe(paste0(base$name, "+extract"), ingest = base$ingest,
                   segment = base$segment, read = rd)

  # keep_answers is TRUE here whatever the caller asked for, and dropped below.
  # It is not a preference: a document restored from `store` hands its answer
  # back or nothing at all, so a resumed run with keep_answers = FALSE would
  # produce a table with empty rows for everything it had already done.
  out <- gr_read_many(sources, goal, rec, client = client, store = store,
                      on_error = on_error, max_total_usd = max_total_usd,
                      keep_answers = TRUE, recursive = recursive, ...)

  docs <- out$summary$document
  answers <- out$answers[docs]          # NULL for failed and skipped rows
  names(answers) <- docs

  structure(list(
    table    = extraction_table(docs, answers, fields, out$summary),
    evidence = extraction_evidence(docs, answers),
    fields   = fields,
    summary  = out$summary,
    answers  = if (isTRUE(keep_answers)) out$answers else list(),
    trace    = out$trace,
    store    = out$store
  ), class = "gr_extraction")
}

#' @export
print.gr_extraction <- function(x, ...) {
  tab <- x$table
  nf <- length(x$fields)
  cat(sprintf("<gr_extraction> %d document(s) x %d field(s)\n", nrow(tab), nf))
  st <- table(factor(tab$status, levels = c("ok", "restored", "failed", "skipped")))
  cat(sprintf("  %s\n", paste(sprintf("%d %s", as.integer(st), names(st))[st > 0L],
                              collapse = ", ")))
  done <- tab$status %in% c("ok", "restored")
  if (any(done) && nf > 0L) {
    cat(sprintf("  cells filled: %d of %d (%.0f%%)\n",
                sum(tab$n_filled[done]), sum(done) * nf,
                100 * sum(tab$n_filled[done]) / (sum(done) * nf)))
  }
  unver <- sum(tab$n_unverified, na.rm = TRUE)
  if (unver) {
    cat(sprintf("  %d value(s) with no verbatim span in the chunk cited\n", unver))
  }
  cnf <- sum(!is.na(tab$conflicts))
  if (cnf) cat(sprintf("  %d document(s) contradicted themselves on at least one field\n", cnf))
  ev <- x$evidence
  if (!is.null(ev) && nrow(ev)) {
    v <- ev$verified[!is.na(ev$verified)]
    cat(sprintf("  evidence: %d span(s)%s\n", nrow(ev),
                if (length(v)) sprintf(", %.0f%% verbatim in the cited chunk",
                                       100 * mean(v)) else ""))
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

#' The record an answer carries, however it got here.
#'
#' `notes$record` is the live path. The JSON in `$answer` is the fallback, for an
#' answer restored from a `store` written by a build whose notes were shaped
#' differently -- a store outlives the session that wrote it, and a resumed run
#' that silently produced empty rows would be worse than one that failed.
#' Everything is put back through `coerce_field()` either way, so both paths
#' produce the same types.
#' @noRd
answer_record <- function(answer, fields) {
  if (is.null(answer)) return(empty_record(fields))
  raw <- answer$notes$record
  if (!is.list(raw)) {
    raw <- tryCatch(jsonlite::fromJSON(as_chr1(answer$answer), simplifyVector = TRUE),
                    error = function(e) NULL)
  }
  if (!is.list(raw)) return(empty_record(fields))
  out <- empty_record(fields)
  # `out[[nm]] <- NULL` would DELETE the entry, leaving a record shorter than the
  # schema; `out[nm] <- list(NULL)` empties it and keeps the name. Everything
  # downstream indexes the record BY NAME for the same reason.
  for (nm in names(fields)) out[nm] <- list(coerce_field(raw[[nm]], fields[[nm]]))
  out
}

#' One typed column, across documents.
#'
#' Built from a typed NA rather than by `unlist()`ing the values: a field that
#' every document left empty must still come back as an integer column of NAs,
#' not a logical one, or a later `rbind()` or join against it changes type
#' depending on how much of the corpus happened to be readable.
#' @noRd
extraction_column <- function(field, values) {
  out <- rep(switch(field$type,
                    string  = NA_character_,
                    enum    = NA_character_,
                    integer = NA_integer_,
                    number  = NA_real_,
                    boolean = NA), length(values))
  for (i in seq_along(values)) {
    if (!is.null(values[[i]])) out[[i]] <- values[[i]]
  }
  out
}

#' @noRd
extraction_table <- function(docs, answers, fields, summary) {
  records <- lapply(docs, function(d) answer_record(answers[[d]], fields))
  tab <- data.frame(document = as.character(docs), stringsAsFactors = FALSE)
  for (nm in names(fields)) {
    tab[[nm]] <- extraction_column(fields[[nm]], lapply(records, function(r) r[[nm]]))
  }
  # By name, never by position. `answer_record()` guarantees one entry per field,
  # so the positional form would give the same answer today -- which is exactly
  # why this is written the safe way rather than the clever one: the day that
  # invariant slips, a length-1 logical index recycles against a length-2 name
  # vector and the WRONG fields are reported as filled, with no error anywhere.
  filled_in <- function(r) names(fields)[vapply(names(fields),
                                                function(nm) !is.null(r[[nm]]),
                                                logical(1))]
  tab$n_filled <- vapply(records, function(r) length(filled_in(r)), integer(1))
  # Values in this row that nothing in the document verifiably supports -- or,
  # under `require_quote = TRUE`, values that were discarded for that reason.
  # Same number either way; the policy decides what happened to them.
  #
  # Recomputed from the record and the evidence rather than read out of `notes`,
  # for the same reason answer_record() re-coerces: a `store` outlives the build
  # that wrote it, and a count taken from a note that a restored answer happens
  # not to carry would silently read zero -- "every value is supported" is the
  # one wrong answer this column must never give by default.
  tab$n_unverified <- vapply(seq_along(docs), function(i) {
    a <- answers[[docs[[i]]]]
    if (is.null(a)) return(NA_integer_)
    here <- filled_in(records[[i]])
    ev <- a$evidence
    supported <- if (!is.data.frame(ev) || !nrow(ev) ||
                     is.null(ev$field) || is.null(ev$verified)) {
      character(0)
    } else {
      unique(as.character(ev$field[isTRUE_vec(ev$verified)]))
    }
    length(setdiff(here, supported)) + length(a$notes$dropped_unverified)
  }, integer(1), USE.NAMES = FALSE)
  tab$conflicts <- vapply(docs, function(d) {
    a <- answers[[d]]
    cf <- if (is.null(a)) NULL else a$notes$conflicts
    if (!length(cf)) NA_character_ else paste(names(cf), collapse = ", ")
  }, character(1), USE.NAMES = FALSE)
  # A document that was never read has no record, so its n_filled of zero would
  # read as "reports none of these fields" -- exactly the confusion the status
  # column exists to prevent. Make it NA and let the status say why.
  unread <- !summary$status %in% c("ok", "restored")
  tab$n_filled[unread] <- NA_integer_
  tab$n_unverified[unread] <- NA_integer_
  tab$status <- as.character(summary$status)
  tab$error <- as.character(summary$error)
  rownames(tab) <- NULL
  tab
}

#' @noRd
extraction_evidence <- function(docs, answers) {
  cols <- c("document", "field", "chunk_id", "page", "section", "quote",
            "verified", "match")
  empty <- data.frame(document = character(0), field = character(0),
                      chunk_id = integer(0), page = integer(0),
                      section = character(0), quote = character(0),
                      verified = logical(0), match = numeric(0),
                      stringsAsFactors = FALSE)
  parts <- lapply(docs, function(d) {
    ev <- answers[[d]]$evidence
    if (!is.data.frame(ev) || !nrow(ev)) return(NULL)
    data.frame(document = d,
               field    = if (is.null(ev$field)) NA_character_ else as.character(ev$field),
               chunk_id = as.integer(ev$chunk_id),
               page     = as.integer(ev$page),
               section  = as.character(ev$section),
               quote    = as.character(ev$text),
               # `source_text` is deliberately not carried: it is the full chunk,
               # once per cell, and the question it answers -- does this quote
               # really appear there -- is already answered by `verified`.
               verified = if (is.null(ev$verified)) NA else as.logical(ev$verified),
               match    = if (is.null(ev$match)) NA_real_ else as.numeric(ev$match),
               stringsAsFactors = FALSE)
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(empty)
  out <- do.call(rbind, parts)
  rownames(out) <- NULL
  out[, cols, drop = FALSE]
}
