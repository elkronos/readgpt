# corpus.R -- one question, many documents.
#
# WHY THIS FILE EXISTS
# `answer_document()` reads one document and `gr_compare()` runs several recipes
# over one document. Neither is the shape of the work people actually have,
# which is a folder of two hundred PDFs and one question -- or ten questions,
# asked one at a time.
#
# Writing that loop yourself is easy and wrong in four specific ways, all of
# which only show up an hour in:
#
#   1. One unreadable file kills the run. `lapply()` propagates the error and
#      the previous hundred and ninety-nine answers go with it.
#   2. There is no way to resume. Restarting re-reads everything, and with a
#      response cache that is cheap but not free -- it still re-extracts,
#      re-cleans and re-chunks every document.
#   3. Budgets do not compose. `max_calls` is per run, so a naive loop either
#      shares one budget (and the first long document starves the rest) or has
#      no total cap at all.
#   4. You cannot see what anything cost. A trace per document, discarded.
#
# So this is a loop with the four things a loop needs: isolation, a store,
# per-document budgets under a corpus-wide ceiling, and accounting.

#' What a run actually cost
#'
#' Costs a trace using each step's own model and counting only the calls that
#' were really issued -- a call served from a [gr_cache()] or a
#' [gr_replay_client()] spent nothing, however many tokens its prompt contained.
#'
#' This is why the token totals on [gr_trace_summary()] are not a bill. They
#' report how large the prompts and replies were, which is the right measure of
#' a run's *shape*; a fully cached re-run has the same shape as the original and
#' cost nothing at all.
#'
#' @param trace A `gr_trace`.
#' @return A data frame with one row per model: `model`, `calls`, `paid_calls`,
#'   `paid_in`, `paid_out`, `usd`. `sum(x$usd)` is the run's cost. A model with
#'   no registered price contributes `NA`, so a total that silently omitted an
#'   unpriced model is impossible.
#' @seealso [gr_trace_summary()], [gr_estimate_cost()], [gr_cache()],
#'   [gr_read_many()]
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
#' ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl)
#' gr_trace_cost(ans$trace)
#'
#' # Priced by the model on the STEP -- the recipe's model -- not by the mock
#' # that answered. A cached re-run costs nothing for a different reason:
#' # paid_calls falls to zero while calls does not.
#' cache <- gr_cache(file.path(tempdir(), "readgpt-cost-example"))
#' again <- answer_document(readgpt_example(), "What was revenue?", "fast",
#'                          client = gr_cache_client(cl, cache))
#' twice <- answer_document(readgpt_example(), "What was revenue?", "fast",
#'                          client = gr_cache_client(cl, cache))
#' gr_trace_cost(twice$trace)[c("calls", "paid_calls", "usd")]
gr_trace_cost <- function(trace) {
  stopifnot(inherits(trace, "gr_trace"))
  steps <- Filter(function(s) !identical(s$kind, "local") && !is.null(s$tokens), trace$steps)
  if (!length(steps)) {
    return(data.frame(model = character(0), calls = integer(0), paid_calls = integer(0),
                      paid_in = integer(0), paid_out = integer(0), usd = numeric(0),
                      stringsAsFactors = FALSE))
  }
  model <- vapply(steps, function(s) as_chr1(s$model, "unknown"), character(1))
  paid <- !vapply(steps, function(s) isTRUE(s$cached), logical(1))
  # NA, not 0, for a step whose count is unknown -- a trace read back from a
  # file can carry one. Summed, it makes that model's total unknown and its cost
  # NA, which the report renders as a dash. Zero would render as free.
  tin  <- vapply(steps, function(s) as_int1(s$tokens$input, NA_integer_), integer(1))
  tout <- vapply(steps, function(s) as_int1(s$tokens$output, NA_integer_), integer(1))

  do.call(rbind, lapply(sort(unique(model)), function(m) {
    i <- model == m
    pin <- sum(tin[i & paid]); pout <- sum(tout[i & paid])
    data.frame(
      model = m, calls = sum(i), paid_calls = sum(i & paid),
      paid_in = as.integer(pin), paid_out = as.integer(pout),
      usd = tryCatch(as.numeric(gr_estimate_cost(m, pin, pout)),
                     error = function(e) NA_real_,
                     warning = function(w) NA_real_),
      stringsAsFactors = FALSE)
  }))
}

#' Which limit stopped the read behind an answer, in words, or NULL.
#' @noRd
limit_note <- function(ans) {
  n <- as.list(ans$notes %||% list())
  cost <- n[["cost_cap_reached", exact = TRUE]]
  calls <- n[["call_cap_reached", exact = TRUE]]
  if (!is.null(cost)) {
    return(list(limit = sprintf("$%s spending limit", format(cost, scientific = FALSE)),
                option = "gr_options(max_cost_usd =)"))
  }
  if (!is.null(calls)) {
    return(list(limit = sprintf("%s-request limit", format(calls, scientific = FALSE)),
                option = "gr_options(max_calls =)"))
  }
  NULL
}

#' A trace's cost in words, for the print methods.
#'
#' One wording everywhere a cost is shown: a model with no registered price
#' makes the total unknown, never zero.
#' @noRd
format_trace_cost <- function(trace) {
  cost <- gr_trace_cost(trace)
  if (!nrow(cost)) return("no cost recorded")
  total <- sum(cost$usd)
  if (is.na(total)) {
    return(sprintf("cost unknown (no registered price for %s)",
                   paste(cost$model[is.na(cost$usd)], collapse = ", ")))
  }
  sprintf("$%.4f across %s", total, paste(cost$model, collapse = ", "))
}

#' Ask one question of many documents
#'
#' The counterpart to [gr_compare()]: that runs several recipes over one
#' document, this runs one recipe over many. Returns one tidy row per document,
#' so the result goes straight into a data frame you can write out, join, or
#' code against.
#'
#' @param sources A character vector of file paths, or a single directory, or
#'   raw text -- or a [gr_records()] or a [gr_screen()] result, either of which
#'   also carries the search forward to the audit. A directory, and a vector of
#'   paths that all exist, are both filtered to the extensions some registered
#'   extractor claims -- so which files are picked up follows [gr_extractors()],
#'   including any you registered yourself. Raw text, a mixed vector and a
#'   `list()` of sources are passed through untouched.
#' @param question The question, asked of every document.
#' @param recipe One recipe, applied to every document.
#' @param client A `gr_client`. Wrap it in [gr_cache_client()] for a long run:
#'   with a durable cache directory, a restart pays for nothing it has already
#'   answered. A closure-backed client -- [gr_backend_client()] or
#'   [gr_mock_client()] -- reuses a cache or a `store` across sessions only if it
#'   was given a stable `id`; see [gr_backend_client()] for why.
#' @param store Optional directory. Each document's result is written there as
#'   it completes and restored on a later run instead of being read again. This
#'   is what makes a four-hour run survive being interrupted.
#' @param on_error `"continue"` (default) records the failure and moves on;
#'   `"stop"` aborts. One unreadable file in two hundred should not cost you the
#'   other hundred and ninety-nine.
#' @param max_total_usd Stop once the run has spent this much, marking the
#'   remaining documents `"skipped"`. This is a *corpus* ceiling and is separate
#'   from `gr_options(max_cost_usd =)`, which is a limit per document.
#'   It needs a model with a registered price: against one without, cost is
#'   *unknown* rather than zero, the ceiling cannot be enforced, and you get a
#'   `gr_corpus_cost_unknown` warning instead of a silent free pass.
#' @param max_total_calls Stop *before* a document once the run has made this
#'   many model calls, marking the rest `"skipped"`. The counterpart to
#'   `max_total_usd` for runs whose model has no registered price, and the only
#'   ceiling that bounds the run rather than each document:
#'   `gr_options(max_calls =)` is per document, so a corpus can make
#'   `length(sources)` times that many. Checked before each document, because a
#'   call ceiling noticed after the calls is not a ceiling -- which means the run
#'   can overshoot by at most one document's worth, exactly as `max_total_usd`
#'   does.
#' @param keep_answers Keep every [gr_answer] in the result. Set `FALSE` for a
#'   large corpus, where holding every trace and evidence table is the thing that
#'   runs you out of memory.
#' @param recursive Descend into subdirectories when `sources` is a directory.
#' @param trace A [gr_trace()] to fold this run's accounting into, so several
#'   stages of one review add up to one figure. It is a *parent*: this run still
#'   gets its own trace, which is what `$trace` returns and what
#'   `gr_options(max_calls =)` is measured against. Running a stage directly on a
#'   shared trace would charge the previous stage's calls against this one's
#'   ceiling. Omit it and there is no parent.
#' @param ... Overrides applied to the recipe, as in [answer_document()].
#' @return An object of class `gr_corpus`: `summary` (one row per document),
#'   `answers` (named list, empty when `keep_answers = FALSE`), `sources` (the
#'   sources as read, aligned row for row with `summary` -- `summary$document` is
#'   a display label and cannot be turned back into a path), `records` (the
#'   [gr_records()] the corpus came from, or `NULL`), `trace` (every call
#'   made *this run*) and `store`.
#'
#' @section The summary:
#' `document`, `document_id`, `answer`, `not_found`, `partial`, `reader`,
#' `chunks`, `chunks_used`, `calls`, `cached`, `tokens_in`, `tokens_out`,
#' `cost_usd`, `seconds`, `status`, `duplicate_of`, `error`, `warnings`.
#'
#' `warnings` holds what readgpt warned about while reading that document,
#' joined with `" | "`, or `NA` when it raised nothing. The warnings still print
#' as they happen; this column is what lets you tie them to a document afterwards.
#'
#' `document` is a filename and `document_id` is the hash of the cleaned text
#' (and of the pages that never became text, when there are any). Cite with the
#' second: a filename changes when the file is renamed, collides between
#' folders, and does not exist at all for a document passed as text, while the
#' id is the same string for the same document in every run and on every machine
#' with the same OCR setup. Two copies of one paper share an id, which is the
#' same fact as the duplicate detection below.
#'
#' `status` is `"ok"`, `"failed"`, `"skipped"` (the corpus ceiling was reached
#' first), `"restored"` (read from `store`, not re-read now) or `"duplicate"`
#' (see below). A document that `max_calls` or `max_cost_usd` stopped before it
#' was read in full is `"failed"` too, with the limit in `error` and its partial
#' answer in `answers`; it is not written to `store`, so a resumed run with a
#' higher limit reads it again. A restored row keeps the numbers from when that document was
#' first read, so its `cost_usd` is what it cost then, not what this run spent --
#' which is why the run's own spend comes from `gr_trace_cost(x$trace)` and not
#' from summing the column.
#'
#' @section Documents that are the same document:
#' The same paper reaches you from three databases under three filenames. Each
#' source is extracted and cleaned, and a document whose cleaned text (and set
#' of unread pages) is identical to one already read this run is **not read
#' again**: its row is filled in from the first copy, except for `warnings`,
#' which are its own; `status` is `"duplicate"` and `duplicate_of`
#' names the row it repeats. Nothing is dropped -- every source you passed still
#' has a row -- so `subset(x$summary, is.na(duplicate_of))` is the deduplicated
#' set and `sum(!is.na(x$summary$duplicate_of))` is the number to report as
#' removed.
#'
#' This is about the table, not the bill: a [gr_cache_client()] already makes the
#' second copy's calls free. What it could not do is stop the duplicate from
#' appearing in the results as a second, independent document, which is how one
#' study gets counted twice in a synthesis.
#'
#' The comparison is exact, on cleaned text. Two typesettings of one paper are
#' two documents here; matching those needs bibliographic metadata, not text.
#'
#' @section Budgets:
#' Every document gets its own trace, so `gr_options(max_calls =)` and
#' `gr_options(max_cost_usd =)` apply per document exactly as they would if you
#' read it alone. One enormous document therefore cannot starve the rest.
#'
#' That is a deliberate design and it leaves the run itself unbounded: two
#' hundred documents under a 400-call ceiling is a *corpus* ceiling of eighty
#' thousand calls. The run-level ceilings are `max_total_calls`, checked before
#' each document, and `max_total_usd`, checked after each one because what a
#' document costs is not knowable until it has been read. With neither set, the
#' run says once what its worst case is rather than leaving you to multiply.
#'
#' @section What this does not do:
#' It reads documents one at a time. Per-document work is embarrassingly
#' parallel, and the per-worker traces the parallel helper already builds would
#' carry the accounting across, so the obstacle is not the trace: it is that
#' duplicate detection, the resume store and both run-level ceilings are all
#' order-dependent, and a parallel loop would have to serialise on each of them. Within a document,
#' `gr_options(parallel = TRUE)` already applies.
#'
#' @seealso [answer_document()], [gr_compare()], [gr_cache_client()],
#'   [gr_trace_cost()]
#' @export
#' @examples
#' cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
#'
#' a <- tempfile(fileext = ".txt"); writeLines("Revenue was 45.2 million.", a)
#' b <- tempfile(fileext = ".txt"); writeLines("Revenue was 51.8 million.", b)
#'
#' out <- gr_read_many(c(a, b), "What was revenue?", "fast", client = cl)
#' out$summary[, c("document", "answer", "not_found", "status")]
#'
#' # A missing file is one bad row, not a failed run.
#' bad <- gr_read_many(c(a, "no-such-file.txt"), "What was revenue?", "fast", client = cl)
#' bad$summary[, c("document", "status", "error")]
#'
#' # What the run actually cost, counting only calls that were really issued.
#' gr_trace_cost(out$trace)
gr_read_many <- function(sources, question, recipe = "thorough", client = NULL,
                         store = NULL, on_error = c("continue", "stop"),
                         max_total_usd = NULL, max_total_calls = NULL,
                         keep_answers = TRUE, recursive = FALSE, trace = NULL, ...) {
  on_error <- match.arg(on_error)
  if (!is_nonblank(question)) gr_abort("`question` must be a non-empty string.")
  sources <- corpus_sources(sources, recursive = recursive)
  if (!length(sources)) {
    below <- attr(sources, "below") %||% character(0)
    gone <- attr(sources, "skipped") %||% character(0)
    if (isTRUE(attr(sources, "screened_out"))) {
      gr_abort(paste0("Screening kept no documents, so there is nothing to read. `$table` says ",
                      "why each one was excluded, and `$summary` says which could not be read ",
                      "at all."), class = "gr_no_sources")
    }
    gr_abort(paste0(
      # "Pass file paths" is not useful advice to somebody who just passed file
      # paths. When every one of them was filtered, say that instead.
      if (length(gone)) sprintf(
        "Every one of the %d file(s) given was skipped: no registered extractor claims %s. ",
        length(gone),
        paste(sprintf("%s (%d)", ifelse(nzchar(names(sort(table(tolower(tools::file_ext(gone))),
                                                           decreasing = TRUE))),
                                        names(sort(table(tolower(tools::file_ext(gone))),
                                                   decreasing = TRUE)), "(no extension)"),
                      as.integer(sort(table(tolower(tools::file_ext(gone))), decreasing = TRUE))),
              collapse = ", "))
      else "`sources` is empty. ",
      "Pass file paths, a directory containing files ",
                    "some extractor handles (see gr_extractors()), or raw text.",
                    # The commonest cause by far, and the old message did not
                    # mention the argument that fixes it.
                    if (length(below)) sprintf(
                      " %d readable file(s) are in subdirectories of that folder; pass recursive = TRUE to include them.",
                      length(below)) else "",
                    " gr_inventory() reports what is there and why each file was or was not taken."),
             class = "gr_no_sources")
  }
  rec <- apply_overrides(as_recipe(recipe), list(...))
  client <- client %||% gr_client(model = rec$read$model)
  # Checked at the first document that has to be READ, not here: a run whose
  # every document is already in `store` sends no request and needs no key.
  credentials_checked <- FALSE
  root <- attr(sources, "root")
  labels <- make.unique(vapply(sources, corpus_label, character(1),
                               root = root, USE.NAMES = FALSE), sep = "#")
  # Carried through so gr_extract()'s table can join to bibliographic fields the
  # export supplied, rather than to ones a model read off a title page.
  from_records <- attr(sources, "record_set")

  # A given trace is the PARENT, and this run still gets its own -- the same
  # arrangement this function already uses for each document below. Running
  # directly on a shared trace would charge the previous stage's calls against
  # this one's per-document `max_calls`, and would leave every stage's `$trace`
  # reporting the whole review's cost instead of its own.
  parent <- as_parent_trace(trace)
  max_total_calls <- as_call_ceiling(max_total_calls)
  # Same treatment: gated on is.finite() at the point of use, this failed open
  # for NA and for a value read from a config file as text -- and once the notice
  # below was gated on it as well, a silently unenforced ceiling also silenced
  # the line that would have said the run was uncapped.
  max_total_usd <- as_usd_ceiling(max_total_usd)
  trace <- gr_trace(meta = list(recipe = rec$name, question = question,
                                documents = length(sources)))
  # on.exit, not a line at the end: a run that aborts part-way -- a cost cap, an
  # unreadable file under on_error = "stop" -- has still spent whatever it spent,
  # and a parent that loses it reports a review as cheaper than it was.
  if (!is.null(parent)) on.exit(trace_absorb(parent, trace), add = TRUE)
  # What the per-document ceiling does NOT bound, said once, before the money is
  # spent. `max_calls` is per document by design -- one enormous document must
  # not starve the rest -- but that makes the run's own exposure the product of
  # the two, and nothing said so. Only when NEITHER run-level ceiling is set: a
  # caller who set one has already thought about this.
  if (is.null(max_total_calls) && is.null(max_total_usd) && length(sources) > 1L) {
    # Doubles, not integers: 3L * 1000000000L overflows to NA, and the notice
    # then announced a worst case of "NA call(s)".
    cap <- gr_options("max_calls")
    cap <- if (length(cap) != 1L || !is.numeric(cap) || !is.finite(cap)) NA_real_ else as.numeric(cap)
    gr_msg(if (is.na(cap)) sprintf(
      paste0("%d document(s), and gr_options(max_calls) is not set, so nothing bounds how many ",
             "calls this run makes. Pass max_total_calls = to cap it."), length(sources))
      else sprintf(
      paste0("%d document(s), and gr_options(max_calls) is %s PER DOCUMENT, so this run may make ",
             "up to %s call(s). Pass max_total_calls = to cap the run."),
      length(sources), format(cap, scientific = FALSE, trim = TRUE),
      format(length(sources) * cap, scientific = FALSE, trim = TRUE)))
  }
  if (!is.null(store)) {
    store <- as_chr1(store)
    if (!dir.exists(store) && !dir.create(store, recursive = TRUE, showWarnings = FALSE)) {
      gr_abort(sprintf("Could not create the store directory '%s'.", store))
    }
  }

  rows <- vector("list", length(sources))
  answers <- list()
  spent <- 0
  stopped <- FALSE
  warned_unpriced <- NULL
  # Cleaned-text hash -> index of the first source that had it.
  seen <- new.env(parent = emptyenv())

  for (i in seq_along(sources)) {
    src <- sources[[i]]
    lab <- labels[[i]]

    # Checked BEFORE the document, not after it: a call ceiling that is only
    # noticed once the calls are made is not a ceiling. (The cost one below can
    # only be checked afterwards -- what a document costs is not knowable until
    # it has been read -- which is why they are enforced in different places.)
    if (!stopped && !is.null(max_total_calls) && trace$calls >= max_total_calls) {
      stopped <- TRUE
      gr_warn(sprintf(paste0("Stopped before document %d of %d: the run has made %d call(s), at ",
                             "or above the %s `max_total_calls` ceiling. The remaining documents ",
                             "are marked 'skipped'."),
                      i, length(sources), trace$calls,
                      format(max_total_calls, scientific = FALSE, trim = TRUE)),
              class = "gr_corpus_call_cap")
    }
    if (stopped) {
      rows[[i]] <- corpus_row(lab, status = "skipped",
                              error = "corpus ceiling reached before this document")
      next
    }

    key <- if (is.null(store)) NULL else corpus_key(src, question, rec, client)
    restored <- if (is.null(key)) NULL else corpus_restore(store, key)
    if (!is.null(restored)) {
      gr_msg(sprintf("[%d/%d] %s -- restored from store", i, length(sources), lab))
      restored$row$document <- lab
      restored$row$status <- "restored"
      if (is.null(restored$row$document_id)) {
        restored$row$document_id <- as_chr1(restored$doc_hash, NA_character_)
      }
      if (is.null(restored$row$duplicate_of)) restored$row$duplicate_of <- NA_character_
      if (is.null(restored$row[["warnings", exact = TRUE]])) {
        restored$row$warnings <- NA_character_
      }
      rows[[i]] <- restored$row
      if (keep_answers && !is.null(restored$answer)) answers[[lab]] <- restored$answer
      # A restored document is never ingested, so its text is not available to
      # hash here. The hash travels in the store entry instead -- otherwise a
      # resumed run would restore the first copy and then pay to read the second,
      # reporting the pair as two independent documents. Entries written before
      # this existed carry no hash and simply do not seed the set.
      # First occurrence wins, and a row that is itself a duplicate never
      # becomes the anchor -- otherwise a third copy would be reported as a
      # duplicate of a duplicate and the chain would have to be followed to find
      # the document actually read.
      if (!is.null(restored$doc_hash) && is.na(restored$row$duplicate_of) &&
          !exists(restored$doc_hash, envir = seen, inherits = FALSE)) {
        assign(restored$doc_hash, i, envir = seen)
      }
      next
    }

    # Before this document is ingested: without a key it, and every document
    # after it, would be read and then fail at its first request, and the run
    # would report a folder of failures instead of the one thing that is wrong.
    if (!credentials_checked) {
      stop_if_no_credentials(client)
      credentials_checked <- TRUE
    }
    gr_msg(sprintf("[%d/%d] %s", i, length(sources), lab))
    started <- Sys.time()
    # One trace per document, folded into the parent afterwards. Sharing the
    # parent outright would make `max_calls` count earlier documents against
    # later ones, so the same document would answer differently depending on
    # its position in the corpus -- the bug gr_compare() had between recipes.
    sub <- gr_trace(meta = list(recipe = rec$name, question = question, source = lab))

    # What a document raised before it failed has no answer to travel on, so it
    # is recorded here for its row.
    doc_rec <- warning_recorder()
    # What this document's ingestion recorded. Reset for every document: `doc`
    # below still holds the previous one when this ingestion fails.
    doc_w <- NULL
    out <- withCallingHandlers(tryCatch({
      doc <- gr_ingest(src, rec$ingest, trace = sub)
      # From the document, not from what was raised just now: a document served
      # from the ingestion cache raises nothing a second time.
      doc_w <- doc$warnings
      # Identity is the CLEANED text, not the bytes: the same paper exported by
      # two databases differs in metadata and whitespace and is the same
      # document. Hashing before segmentation also means the check costs one
      # local ingest rather than one read.
      h <- gr_hash(list("readgpt-doc-v1", key_text(doc$text)))
      # A document with pages that never became text is not a copy of one that
      # was read in full, even when the text that did come out is the same.
      # Documents read in full keep the hash they always had.
      if (length(doc$stats$unread_pages)) h <- gr_hash(list(h, doc$stats$unread_pages))
      prior <- mget(h, envir = seen, ifnotfound = list(NULL))[[1]]
      if (!is.null(prior)) {
        structure(list(of = prior, hash = h, warnings = doc$warnings),
                  class = "gr_corpus_duplicate")
      } else {
        ch <- gr_segment(doc, rec$segment, client = client, trace = sub)
        a <- finish_answer(gr_read(ch, question, client, rec$read, trace = sub),
                           doc, ch, rec$name)
        attr(a, "doc_hash") <- h
        a
      }
    }, error = function(e) {
      # Not this document's failure: every document after it would fail the
      # same way, so the run stops here rather than recording each one.
      if (inherits(e, "gr_auth_error")) stop(e)
      if (identical(on_error, "stop")) stop(e)
      gr_warn(sprintf("Document '%s' failed: %s", lab, conditionMessage(e)),
              class = "gr_document_failed")
      e
    }), gr_warning = doc_rec$record)

    trace_absorb(trace, sub)
    secs <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2)
    # NOT na.rm = TRUE. gr_trace_cost() returns NA for a model with no registered
    # price precisely so that a total cannot quietly omit it; dropping the NA here
    # turned "we do not know what this cost" into "$0.0000", which then made
    # `max_total_usd` unenforceable while the run reported itself free.
    cost <- sum(gr_trace_cost(sub)$usd)
    spent <- spent + cost

    if (inherits(out, "condition")) {
      raised <- doc_rec$get()
      # The failure is the row's `status` and `error`; repeating it as a warning
      # would say it twice.
      raised <- raised[names(raised) != "gr_document_failed"]
      rows[[i]] <- corpus_row(lab, status = "failed", error = conditionMessage(out),
                              trace = sub, seconds = secs, cost = cost,
                              warnings = c(doc_w, raised))
    } else if (inherits(out, "gr_corpus_duplicate")) {
      first <- labels[[out$of]]
      gr_msg(sprintf("[%d/%d] %s -- same text as '%s', not read again",
                     i, length(sources), lab, first))
      # The first copy's row, relabelled. Its answer, reader and chunk counts
      # describe this content too; its call counts and cost do not, because this
      # run made no calls for it.
      r <- rows[[out$of]]
      r$document <- lab
      r$status <- "duplicate"
      r$duplicate_of <- first
      # Its own warnings, not the first copy's: two files with the same text can
      # still have been read with different problems.
      r$warnings <- corpus_warnings(out$warnings)
      r[c("calls", "cached", "tokens_in", "tokens_out")] <- 0L
      r$cost_usd <- cost
      r$seconds <- secs
      rows[[i]] <- r
      dup_answer <- answers[[first]]
      if (keep_answers && !is.null(dup_answer)) answers[[lab]] <- dup_answer
      # Saved under its OWN key, so a resumed run restores it instead of paying
      # to rediscover that it is a duplicate.
      if (!is.null(key)) corpus_save(store, key, rows[[i]], dup_answer, out$hash)
    } else if (!is.null(stopped_by <- limit_note(out))) {
      # Not read in full, so not a result: reported and handled as a failure,
      # kept out of the store so a resumed run with a higher limit reads it
      # again, and left out of the duplicate check for the same reason. The
      # partial answer stays in `answers`.
      why <- sprintf("stopped at the %s before the document was read in full; raise %s",
                     stopped_by$limit, stopped_by$option)
      gr_warn(sprintf("Document '%s' failed: %s.", lab, why), class = "gr_document_failed")
      rows[[i]] <- corpus_row(lab, status = "failed", error = why, trace = sub,
                              seconds = secs, cost = cost, warnings = out$warnings)
      if (keep_answers) answers[[lab]] <- out
    } else {
      rows[[i]] <- corpus_row(lab, status = "ok", answer = out, trace = sub,
                              seconds = secs, cost = cost,
                              document_id = attr(out, "doc_hash"))
      if (keep_answers) answers[[lab]] <- out
      assign(attr(out, "doc_hash"), i, envir = seen)
      if (!is.null(key)) corpus_save(store, key, rows[[i]], out, attr(out, "doc_hash"))
    }

    # A ceiling on a cost nobody can compute is not a ceiling. Say so once,
    # rather than letting an unpriced model run past a limit the user set.
    if (!is.null(max_total_usd) && is.na(spent) && is.null(warned_unpriced)) {
      warned_unpriced <- TRUE
      gr_warn(paste0("`max_total_usd` cannot be enforced: at least one model in this run has no ",
                     "registered price, so what it costs is unknown rather than zero. Register ",
                     "the price with gr_register_model(input_usd =, output_usd =), or drop the ",
                     "ceiling. The run continues, uncapped."),
              class = "gr_corpus_cost_unknown")
    }
    if (!is.null(max_total_usd) && !is.na(spent) && spent >= max_total_usd) {
      stopped <- TRUE
      if (i < length(sources)) {
        gr_warn(sprintf(paste0("Stopped after %d of %d documents: the run has spent about ",
                               "$%.4f, at or above the $%.4f `max_total_usd` ceiling. The ",
                               "remaining documents are marked 'skipped'."),
                        i, length(sources), spent, max_total_usd),
                class = "gr_corpus_cost_cap")
      }
    }
  }

  structure(list(summary = do.call(rbind, rows), answers = answers,
                 # The sources as they were actually read, aligned row for row
                 # with `summary`. `summary$document` is a LABEL -- a basename,
                 # made unique with a suffix when two folders hold the same
                 # filename -- so there is no way back from it to the file. Any
                 # caller that wants to feed a subset of a corpus into the next
                 # stage needs the paths, not the labels.
                 sources = simplify_sources(sources),
                 # The search that produced the corpus, carried rather than
                 # discarded. gr_audit_report() asks for it at the very end, and
                 # a user who does not remember to re-supply the same object
                 # gets a report whose search section reads "Not recorded" and a
                 # flow diagram that begins at "sources given".
                 records = from_records,
                 trace = trace, store = store), class = "gr_corpus")
}

#' @export
print.gr_corpus <- function(x, ...) {
  s <- x$summary
  tab <- table(factor(s$status,
                      levels = c("ok", "restored", "duplicate", "failed", "skipped")))
  cat(sprintf("<gr_corpus> %d document(s): %s\n", nrow(s),
              paste(sprintf("%d %s", as.integer(tab), names(tab))[tab > 0L], collapse = ", ")))
  dup <- sum(!is.na(s$duplicate_of))
  if (dup) cat(sprintf("  %d repeated a document already read (see duplicate_of)\n", dup))
  done <- s$status %in% c("ok", "restored", "duplicate")
  if (any(done)) {
    cat(sprintf("  %d answered, %d found nothing, %d partial\n",
                sum(done), sum(s$not_found[done], na.rm = TRUE),
                sum(s$partial[done], na.rm = TRUE)))
  }
  cat(sprintf("  this run: %d model call(s), %s\n", x$trace$calls,
              format_trace_cost(x$trace)))
  warned <- if (is.null(s$warnings)) 0L else sum(!is.na(s$warnings))
  if (warned) {
    cat(sprintf("  %d document(s) raised warnings (see the `warnings` column)\n", warned))
  }
  if (!is.null(x$store)) cat(sprintf("  store: %s\n", x$store))
  invisible(x)
}

# --- internals -------------------------------------------------------------

#' Name a document for the summary.
#'
#' `source_label()` calls anything that is not an existing file "<inline text>",
#' which is right for a trace and useless here: a corpus with three missing
#' files would show three rows called "<inline text>" and no way to tell which
#' path failed. A source that *looks* like a path is named by its basename even
#' when it does not exist, because a row saying a file is missing has to say
#' which file.
#'
#' The length guard is not decoration. `basename()` is bounded by PATH_MAX, and
#' raw document text passed as a source is a single long string -- handing that
#' to `basename()` warns about an expanded path of 1200 characters, which is how
#' this was found in the first place.
#' @noRd
corpus_label <- function(source, inline = "<inline text>", root = NULL) {
  if (!is.character(source) || length(source) != 1L || is.na(source)) return(inline)
  if (grepl("\n", source, fixed = TRUE)) return(inline)
  if (nchar(source, type = "bytes") >= 1000L) return(inline)
  if (file.exists(source)) {
    # Keep the folder a file came from. `basename()` turned 2019/report.txt and
    # 2020/report.txt into one name, and make.unique() then separated them as
    # "report.txt" and "report.txt#1" -- discarding the meaningful half and
    # replacing it with an index that depends on sort order. A reviewer who
    # filed by year had that year thrown away and could not tell the rows apart.
    if (!is.null(root) && nzchar(root)) {
      rel <- relative_path(source, root)
      if (!is.na(rel)) return(rel)
    }
    return(basename(source))
  }
  looks_like_path <- grepl("[/\\\\]", source) || nzchar(tools::file_ext(source))
  if (looks_like_path) basename(source) else inline
}

#' The file extensions some registered extractor claims.
#' @noRd
known_extensions <- function() {
  ext <- unlist(strsplit(gr_extractors()$extensions, ",\\s*"), use.names = FALSE)
  tolower(unique(trimws(ext[nzchar(ext)])))
}

#' A run-level call ceiling, or nothing.
#'
#' `is.finite()` alone failed OPEN: NA, Inf, and a character value read from a
#' config file all made it FALSE, so the ceiling was skipped silently and the
#' notice that would have said the run was uncapped was suppressed at the same
#' time, because that was gated on `is.null()`.
#' @noRd
as_ceiling <- function(x, arg, whole = FALSE) {
  if (is.null(x)) return(NULL)
  if (length(x) != 1L || !is.numeric(x) || is.na(x)) {
    gr_abort(sprintf(paste0("`%s` must be a single number, or NULL for no ceiling. A ceiling that ",
                            "cannot be compared is not a ceiling."), arg),
             class = "gr_bad_ceiling")
  }
  # The sign test comes FIRST, so -Inf is refused rather than falling into the
  # "no ceiling" branch below. max(integer(0)) is -Inf, so a ceiling computed
  # from an empty set meant "unlimited".
  if (x < 0) gr_abort(sprintf("`%s` cannot be negative.", arg), class = "gr_bad_ceiling")
  # Inf is a legitimate way to say "no ceiling", and saying it that way should
  # not cost the notice that the run is uncapped.
  if (!is.finite(x)) return(NULL)
  # A DOUBLE, floored -- not as.integer(). as.integer(1e10) is NA with a warning,
  # and the loop guard then compared against NA, so `if` threw a bare
  # simpleError: the exact failure this function exists to prevent, reintroduced
  # by the coercion meant to fix it. Whole, so the message and the comparison
  # agree: 1.5 compared as 1.5 and printed as 1 said a run had stopped at a
  # ceiling it had not reached.
  if (whole) floor(x) else x
}

#' @noRd
as_call_ceiling <- function(x, arg = "max_total_calls") as_ceiling(x, arg, whole = TRUE)

#' @noRd
as_usd_ceiling <- function(x, arg = "max_total_usd") as_ceiling(x, arg, whole = FALSE)

#' Expand a directory to the files some extractor actually handles.
#'
#' Carries what it decided on the result: `root` (so labels can keep the folder
#' a file came from), `skipped` (files no extractor claims) and `below` (files
#' that exist further down and were not scanned). Silently returning only the
#' survivors is how a directory of 200 `.doc` files reads as an empty corpus.
#' @noRd
corpus_sources <- function(sources, recursive = FALSE, quiet = FALSE) {
  # A screening result names the documents that survived screening AND the search
  # that found them. Hanging the record set off `included` instead would have
  # made print(screened$included) dump the whole record set under a list of file
  # paths, which is the thing a user looks at most.
  if (inherits(sources, "gr_screening")) {
    rs <- sources$records
    inc <- as.character(sources$included %||% character(0))
    # Explicitly, not by falling through: the file-vector branch below requires
    # every path to exist, so one moved or deleted file dropped the record set
    # and the search vanished from the audit.
    out <- if (!length(inc)) structure(character(0), screened_out = TRUE) else
      corpus_sources(inc, recursive = recursive, quiet = quiet)
    if (inherits(rs, "gr_records")) attr(out, "record_set") <- rs
    return(out)
  }
  # A record set names the documents a search actually retrieved, which is a
  # better answer to "what is the corpus" than a folder listing: it excludes
  # duplicates, and it knows which records have no document rather than being
  # unable to represent them.
  if (inherits(sources, "gr_records")) {
    r <- sources$records
    keep <- is.na(r$duplicate_of) & !is.na(r$file)
    out <- r$file[keep]
    attr(out, "root") <- NULL
    # The whole record set, not just its rows: the search strategy lives on the
    # object, and the audit's "The search" section is built from it.
    attr(out, "record_set") <- sources
    return(out)
  }
  if (is.character(sources) && length(sources) == 1L && !is.na(sources) &&
      dir.exists(sources)) {
    ext <- known_extensions()
    files <- list.files(sources, full.names = TRUE, recursive = recursive, no.. = TRUE)
    files <- files[!dir.exists(files)]
    keep <- tolower(tools::file_ext(files)) %in% ext
    skipped <- sort(files[!keep])
    # Files sitting further down that a non-recursive scan never looked at. The
    # single most likely reason a directory looks empty.
    below <- if (recursive) character(0) else {
      deep <- list.files(sources, full.names = TRUE, recursive = TRUE, no.. = TRUE)
      deep <- deep[!dir.exists(deep)]
      deep <- setdiff(deep, files)
      sort(deep[tolower(tools::file_ext(deep)) %in% ext])
    }
    if (!quiet && length(skipped)) {
      by_ext <- sort(table(tolower(tools::file_ext(skipped))), decreasing = TRUE)
      nm <- names(by_ext); nm[!nzchar(nm)] <- "(no extension)"
      gr_warn(sprintf(paste0("%d file(s) in '%s' were skipped: no registered extractor claims ",
                             "%s. See gr_extractors(), gr_inventory() for the full picture, or ",
                             "gr_register_extractor() to add one."),
                      length(skipped), sources,
                      paste(sprintf("%s (%d)", nm, as.integer(by_ext)), collapse = ", ")),
              class = "gr_sources_skipped")
    }
    out <- sort(files[keep])
    attr(out, "root") <- sources
    attr(out, "skipped") <- skipped
    attr(out, "below") <- below
    return(out)
  }
  # A vector of paths that all exist gets the SAME filter a directory gets.
  # Without this, one corpus behaved two ways: gr_read_many(dir) skipped the
  # files no extractor claims and said so, while gr_read_many(list.files(dir))
  # handed each of them to an extractor and recorded a failed row. The second
  # form is what every pipeline uses -- gr_extract(screened$included) is a
  # character vector -- so the stage that reads the most documents was the one
  # getting the worse behaviour.
  if (is.character(sources) && length(sources) && !anyNA(sources) &&
      all(nzchar(sources)) && all(file.exists(sources)) && !any(dir.exists(sources))) {
    ext <- known_extensions()
    keep <- tolower(tools::file_ext(sources)) %in% ext
    skipped <- sort(sources[!keep])
    if (!quiet && length(skipped)) {
      by_ext <- sort(table(tolower(tools::file_ext(skipped))), decreasing = TRUE)
      nm <- names(by_ext); nm[!nzchar(nm)] <- "(no extension)"
      gr_warn(sprintf(paste0("%d of the %d file(s) given were skipped: no registered extractor ",
                             "claims %s. See gr_extractors(), gr_inventory() for the full ",
                             "picture, or gr_register_extractor() to add one."),
                      length(skipped), length(sources),
                      paste(sprintf("%s (%d)", nm, as.integer(by_ext)), collapse = ", ")),
              class = "gr_sources_skipped")
    }
    out <- sources[keep]
    attr(out, "skipped") <- skipped
    # gr_screen() puts the record set on `included`, so the search survives the
    # hand-off to gr_extract() -- which takes a character vector, not the
    # gr_records, and so could never have carried it otherwise.
    rs <- attr(sources, "record_set")
    if (inherits(rs, "gr_records")) attr(out, "record_set") <- rs
    return(out)
  }
  if (is.list(sources)) return(sources)
  as.list(as.character(sources))
}

#' One row of the summary, from an answer or from a failure.
#' @noRd
corpus_row <- function(document, status, answer = NULL, trace = NULL, error = NA_character_,
                       seconds = NA_real_, cost = NA_real_, document_id = NA_character_,
                       warnings = NULL) {
  seg <- if (is.null(answer)) list() else (answer$segmentation %||% list())
  s <- if (is.null(trace)) NULL else gr_trace_summary(trace)
  data.frame(
    document    = as_chr1(document),
    # The stable half of a citation. `document` is a filename: it changes when
    # the file is renamed, it collides between folders (make.unique() then
    # appends "#1", which depends on the order you passed the sources in), and
    # it says nothing about a document passed as text. This is the hash of the
    # cleaned text, so it is the same string for the same document in every run,
    # on every machine, under any name -- and identical for two copies of it,
    # which is what makes duplicate detection and this column the same fact.
    document_id = as_chr1(document_id, NA_character_),
    answer      = if (is.null(answer)) NA_character_ else as_chr1(answer$answer),
    not_found   = if (is.null(answer)) NA else is_not_found(answer$answer),
    partial     = if (is.null(answer)) NA else isTRUE(answer$partial),
    reader      = if (is.null(answer)) NA_character_ else as_chr1(answer$reader, NA_character_),
    chunks      = as_int1(seg$n, NA_integer_),
    chunks_used = if (is.null(answer)) NA_integer_ else length(answer$chunks_used),
    calls       = if (is.null(s)) NA_integer_ else as.integer(s$calls),
    cached      = if (is.null(s)) NA_integer_ else as.integer(s$cached),
    tokens_in   = if (is.null(s)) NA_integer_ else as.integer(s$tokens_in),
    tokens_out  = if (is.null(s)) NA_integer_ else as.integer(s$tokens_out),
    cost_usd    = as.numeric(cost),
    seconds     = as.numeric(seconds),
    status      = as_chr1(status),
    # The durable marker. `status` is overwritten with "restored" when a row
    # comes back from a store, so filtering duplicates out has to key on this
    # column, which survives the round trip.
    duplicate_of = NA_character_,
    error       = as_chr1(error, NA_character_),
    # Last, so code that reads the columns by position still finds them where
    # they were. What the document raised while it was read: a warning printed
    # during a run over two hundred files cannot be tied to any of them.
    warnings    = corpus_warnings(warnings %||% answer[["warnings", exact = TRUE]]),
    stringsAsFactors = FALSE
  )
}

#' One document's warnings as one summary cell, or NA when there were none.
#' @noRd
corpus_warnings <- function(w) {
  w <- unique(as.character(w %||% character(0)))
  w <- w[!is.na(w) & nzchar(w)]
  if (!length(w)) NA_character_ else paste(w, collapse = " | ")
}

#' Identity of one (document, question, pipeline, model) job.
#'
#' A file is identified by path, size and mtime, so an edited document is a new
#' job rather than a stale hit -- the same rule the ingest cache uses. Anything
#' that is not an existing file is identified by its own text.
#' @noRd
corpus_key <- function(src, question, rec, client) {
  ident <- if (is.character(src) && length(src) == 1L && !is.na(src) &&
               nchar(src, type = "bytes") < 1000L && file.exists(src)) {
    info <- file.info(src)
    list("file", normalizePath(src, winslash = "/", mustWork = FALSE),
         info$size, format(info$mtime))
  } else {
    list("text", key_text(as.character(src)))
  }
  gr_hash(list("readgpt-corpus-v2", ident, key_text(question),
               # The tokenizer, because it is what turns `max_tokens = 300` into
               # an actual chunk boundary: the same document under "chars" and
               # under "words" segments differently and is answered differently.
               # Without it the store handed back an answer built from a
               # segmentation this run would never have produced.
               as_chr1(gr_options("tokenizer"), "?"),
               unclass(rec$ingest), unclass(rec$segment), unclass(rec$read),
               as_chr1(client$model, "?"), as_chr1(client$api, "?"),
               as_chr1(client$base_url, "?"),
               # As in cache_key(): for a closure-backed client the transport
               # fields are identical constants, so without this a store restored
               # one client's answers for a different client's run.
               as_chr1(client$.client_id, "<url-addressed>")))
}

#' Give the sources back in the shape they arrived in.
#'
#' `corpus_sources()` works in a list so that a mixed set -- paths and raw text
#' together -- survives. Handing that back to a caller who passed a plain
#' character vector is a small surprise with a long tail: `basename(x$sources)`
#' and `file.exists(x$sources)` both do the wrong thing on a list of strings.
#' @noRd
simplify_sources <- function(x) {
  # Attributes stripped: `$sources` is a list of paths a user prints and
  # subsets, and carrying `record_set` on it made print() dump the whole record
  # set underneath -- the objection that moved the record set off `$included` in
  # the first place.
  if (!is.list(x)) return(`attributes<-`(x, NULL))
  if (all(vapply(x, function(e) is.character(e) && length(e) == 1L, logical(1)))) {
    unlist(x, use.names = FALSE)
  } else x
}

#' @noRd
corpus_store_path <- function(store, key) file.path(store, paste0(key, ".rds"))

#' @noRd
corpus_restore <- function(store, key) {
  path <- corpus_store_path(store, key)
  if (!file.exists(path)) return(NULL)
  entry <- tryCatch(readRDS(path), error = function(e) NULL, warning = function(w) NULL)
  if (!is.list(entry) || !identical(entry$format, 1L) || !is.data.frame(entry$row)) return(NULL)
  entry
}

#' Written to a temporary name and renamed, so an interrupt cannot leave a half
#' entry that a later run would have to tell from a real one.
#' @noRd
corpus_save <- function(store, key, row, answer, doc_hash = NULL) {
  path <- corpus_store_path(store, key)
  tmp <- paste0(path, ".tmp-", Sys.getpid())
  ok <- tryCatch({
    # `doc_hash` is additive and the format number does not move: an entry
    # written before it existed is still a perfectly good answer, it just cannot
    # seed duplicate detection.
    saveRDS(list(format = 1L, key = key, created = Sys.time(), row = row,
                 answer = answer, doc_hash = doc_hash),
            tmp, compress = TRUE)
    file.rename(tmp, path)
  }, error = function(e) FALSE, warning = function(w) FALSE)
  if (!isTRUE(ok) && file.exists(tmp)) unlink(tmp)
  invisible(isTRUE(ok))
}
