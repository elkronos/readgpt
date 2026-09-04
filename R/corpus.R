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
  tin  <- vapply(steps, function(s) as_int1(s$tokens$input, 0L), integer(1))
  tout <- vapply(steps, function(s) as_int1(s$tokens$output, 0L), integer(1))

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

#' Ask one question of many documents
#'
#' The counterpart to [gr_compare()]: that runs several recipes over one
#' document, this runs one recipe over many. Returns one tidy row per document,
#' so the result goes straight into a data frame you can write out, join, or
#' code against.
#'
#' @param sources A character vector of file paths, or a single directory, or
#'   raw text. A directory is expanded to the files in it whose extensions any
#'   registered extractor claims -- so which files are picked up follows
#'   [gr_extractors()], including any you registered yourself.
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
#'   from `gr_options(max_cost_usd =)`, which is a per-document pre-flight check.
#'   It needs a model with a registered price: against one without, cost is
#'   *unknown* rather than zero, the ceiling cannot be enforced, and you get a
#'   `gr_corpus_cost_unknown` warning instead of a silent free pass.
#' @param keep_answers Keep every [gr_answer] in the result. Set `FALSE` for a
#'   large corpus, where holding every trace and evidence table is the thing that
#'   runs you out of memory.
#' @param recursive Descend into subdirectories when `sources` is a directory.
#' @param ... Overrides applied to the recipe, as in [answer_document()].
#' @return An object of class `gr_corpus`: `summary` (one row per document),
#'   `answers` (named list, empty when `keep_answers = FALSE`), `trace` (every
#'   call made *this run*) and `store`.
#'
#' @section The summary:
#' `document`, `document_id`, `answer`, `not_found`, `partial`, `reader`,
#' `chunks`, `chunks_used`, `calls`, `cached`, `tokens_in`, `tokens_out`,
#' `cost_usd`, `seconds`, `status`, `duplicate_of`, `error`.
#'
#' `document` is a filename and `document_id` is the hash of the cleaned text.
#' Cite with the second: a filename changes when the file is renamed, collides
#' between folders, and does not exist at all for a document passed as text,
#' while the id is the same string for the same document in every run and on
#' every machine. Two copies of one paper share an id, which is the same fact as
#' the duplicate detection below.
#'
#' `status` is `"ok"`, `"failed"`, `"skipped"` (the corpus ceiling was reached
#' first), `"restored"` (read from `store`, not re-read now) or `"duplicate"`
#' (see below). A restored row keeps the numbers from when that document was
#' first read, so its `cost_usd` is what it cost then, not what this run spent --
#' which is why the run's own spend comes from `gr_trace_cost(x$trace)` and not
#' from summing the column.
#'
#' @section Documents that are the same document:
#' The same paper reaches you from three databases under three filenames. Each
#' source is extracted and cleaned, and a document whose cleaned text is
#' identical to one already read this run is **not read again**: its row is
#' filled in from the first copy, `status` is `"duplicate"` and `duplicate_of`
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
#' read it alone. One enormous document therefore cannot starve the rest. The
#' corpus-wide ceiling is `max_total_usd`.
#'
#' @section What this does not do:
#' It reads documents one at a time. Per-document work is embarrassingly
#' parallel, but a `gr_trace` accumulates by reference and does not survive
#' being sent to a worker process, so parallelising it would silently lose the
#' accounting that is half the point.
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
                         max_total_usd = NULL, keep_answers = TRUE,
                         recursive = FALSE, ...) {
  on_error <- match.arg(on_error)
  if (!is_nonblank(question)) gr_abort("`question` must be a non-empty string.")
  sources <- corpus_sources(sources, recursive = recursive)
  if (!length(sources)) {
    gr_abort(paste0("`sources` is empty. Pass file paths, a directory containing files ",
                    "some extractor handles (see gr_extractors()), or raw text."),
             class = "gr_no_sources")
  }
  rec <- apply_overrides(as_recipe(recipe), list(...))
  client <- client %||% gr_client(model = rec$read$model)
  labels <- make.unique(vapply(sources, corpus_label, character(1), USE.NAMES = FALSE), sep = "#")

  trace <- gr_trace(meta = list(recipe = rec$name, question = question,
                                documents = length(sources)))
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

    if (stopped) {
      rows[[i]] <- corpus_row(lab, status = "skipped",
                              error = "corpus cost ceiling reached before this document")
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

    gr_msg(sprintf("[%d/%d] %s", i, length(sources), lab))
    started <- Sys.time()
    # One trace per document, folded into the parent afterwards. Sharing the
    # parent outright would make `max_calls` count earlier documents against
    # later ones, so the same document would answer differently depending on
    # its position in the corpus -- the bug gr_compare() had between recipes.
    sub <- gr_trace(meta = list(recipe = rec$name, question = question, source = lab))

    out <- tryCatch({
      doc <- gr_ingest(src, rec$ingest, trace = sub)
      # Identity is the CLEANED text, not the bytes: the same paper exported by
      # two databases differs in metadata and whitespace and is the same
      # document. Hashing before segmentation also means the check costs one
      # local ingest rather than one read.
      h <- gr_hash(list("readgpt-doc-v1", key_text(doc$text)))
      prior <- mget(h, envir = seen, ifnotfound = list(NULL))[[1]]
      if (!is.null(prior)) {
        structure(list(of = prior, hash = h), class = "gr_corpus_duplicate")
      } else {
        ch <- gr_segment(doc, rec$segment, client = client, trace = sub)
        a <- gr_read(ch, question, client, rec$read, trace = sub)
        a$recipe <- rec$name
        a$document <- list(source = doc$source, stats = doc$stats)
        a$segmentation <- as.list(gr_chunk_stats(ch))
        attr(a, "doc_hash") <- h
        a
      }
    }, error = function(e) {
      if (identical(on_error, "stop")) stop(e)
      gr_warn(sprintf("Document '%s' failed: %s", lab, conditionMessage(e)),
              class = "gr_document_failed")
      e
    })

    trace_absorb(trace, sub)
    secs <- round(as.numeric(difftime(Sys.time(), started, units = "secs")), 2)
    # NOT na.rm = TRUE. gr_trace_cost() returns NA for a model with no registered
    # price precisely so that a total cannot quietly omit it; dropping the NA here
    # turned "we do not know what this cost" into "$0.0000", which then made
    # `max_total_usd` unenforceable while the run reported itself free.
    cost <- sum(gr_trace_cost(sub)$usd)
    spent <- spent + cost

    if (inherits(out, "condition")) {
      rows[[i]] <- corpus_row(lab, status = "failed", error = conditionMessage(out),
                              trace = sub, seconds = secs, cost = cost)
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
      r[c("calls", "cached", "tokens_in", "tokens_out")] <- 0L
      r$cost_usd <- cost
      r$seconds <- secs
      rows[[i]] <- r
      dup_answer <- answers[[first]]
      if (keep_answers && !is.null(dup_answer)) answers[[lab]] <- dup_answer
      # Saved under its OWN key, so a resumed run restores it instead of paying
      # to rediscover that it is a duplicate.
      if (!is.null(key)) corpus_save(store, key, rows[[i]], dup_answer, out$hash)
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
    if (!is.null(max_total_usd) && is.finite(max_total_usd) && is.na(spent) &&
        is.null(warned_unpriced)) {
      warned_unpriced <- TRUE
      gr_warn(paste0("`max_total_usd` cannot be enforced: at least one model in this run has no ",
                     "registered price, so what it costs is unknown rather than zero. Register ",
                     "the price with gr_register_model(input_usd =, output_usd =), or drop the ",
                     "ceiling. The run continues, uncapped."),
              class = "gr_corpus_cost_unknown")
    }
    if (!is.null(max_total_usd) && is.finite(max_total_usd) && !is.na(spent) &&
        spent >= max_total_usd) {
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
  cost <- gr_trace_cost(x$trace)
  total <- if (nrow(cost)) sum(cost$usd) else 0
  cat(sprintf("  this run: %d model call(s), %s\n", x$trace$calls,
              if (!nrow(cost)) "no cost recorded"
              else if (is.na(total))
                sprintf("cost unknown (no registered price for %s)",
                        paste(cost$model[is.na(cost$usd)], collapse = ", "))
              else sprintf("$%.4f across %s", total, paste(cost$model, collapse = ", "))))
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
corpus_label <- function(source, inline = "<inline text>") {
  if (!is.character(source) || length(source) != 1L || is.na(source)) return(inline)
  if (grepl("\n", source, fixed = TRUE)) return(inline)
  if (nchar(source, type = "bytes") >= 1000L) return(inline)
  if (file.exists(source)) return(basename(source))
  looks_like_path <- grepl("[/\\\\]", source) || nzchar(tools::file_ext(source))
  if (looks_like_path) basename(source) else inline
}

#' Expand a directory to the files some extractor actually handles.
#' @noRd
corpus_sources <- function(sources, recursive = FALSE) {
  if (is.character(sources) && length(sources) == 1L && !is.na(sources) &&
      dir.exists(sources)) {
    ext <- unlist(strsplit(gr_extractors()$extensions, ",\\s*"), use.names = FALSE)
    ext <- unique(trimws(ext[nzchar(ext)]))
    files <- list.files(sources, full.names = TRUE, recursive = recursive,
                        no.. = TRUE)
    files <- files[!dir.exists(files)]
    keep <- tolower(tools::file_ext(files)) %in% tolower(ext)
    return(sort(files[keep]))
  }
  if (is.list(sources)) return(sources)
  as.list(as.character(sources))
}

#' One row of the summary, from an answer or from a failure.
#' @noRd
corpus_row <- function(document, status, answer = NULL, trace = NULL, error = NA_character_,
                       seconds = NA_real_, cost = NA_real_, document_id = NA_character_) {
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
    stringsAsFactors = FALSE
  )
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
  gr_hash(list("readgpt-corpus-v1", ident, key_text(question),
               unclass(rec$ingest), unclass(rec$segment), unclass(rec$read),
               as_chr1(client$model, "?"), as_chr1(client$api, "?"),
               as_chr1(client$base_url, "?"),
               # As in cache_key(): for a closure-backed client the transport
               # fields are identical constants, so without this a store restored
               # one client's answers for a different client's run.
               as_chr1(client$.client_id, "<url-addressed>")))
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
