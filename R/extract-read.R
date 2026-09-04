# extract-read.R -- the `extract` reader: fill a schema from a document.
#
# Traversal `all|N+conflicts|none`. Every chunk is asked to fill whatever fields
# it can and to leave the rest null; the per-field answers are then reconciled.
#
# The call count is N plus one per DISAGREEMENT, not N + 1. Most fields are
# stated once in a paper, so most reconciliations are arithmetic rather than
# judgement, and paying a model to agree with itself is the kind of cost this
# package exists to avoid. A field that genuinely appears twice with different
# values is the interesting case, and that is where the extra call goes.
#
# Nothing here invents a value. A field no chunk reported comes back NA with the
# status "not reported", which is a FINDING -- the same distinction
# `is_not_found()` draws for answers. A review that cannot tell "the paper does
# not say" from "we failed to look" is not a review.

#' @noRd
read_extract <- function(chunks, question, client, spec, trace) {
  # `[[` not `$`: partial matching on a list is how `.cache_id` silently found
  # `client$.cache` once already. A read spec carries user-supplied `...` fields,
  # so the set of names is open and a prefix collision is a live possibility.
  fields <- spec[["fields"]]
  if (!inherits(fields, "gr_fields")) {
    gr_abort(paste0("The 'extract' reader needs `fields`. Build them with gr_fields(), and pass ",
                    "them through the read spec: gr_read_spec('extract', fields = gr_fields(...)), ",
                    "or use gr_extract(), which does this for you."),
             class = "gr_no_fields")
  }
  d <- chunks$chunks
  schema <- fields_schema(fields)
  listing <- fields_prompt(fields)

  if (!trace_can_call(trace, nrow(d))) {
    gr_warn(sprintf(paste0("extract needs %d calls but the run cap is %s; segment more coarsely ",
                           "or raise gr_options(max_calls =)."),
                    nrow(d), format(gr_options("max_calls"))), class = "gr_call_cap")
  }

  got <- gr_lapply(seq_len(nrow(d)), function(i) {
    if (!trace_can_call(trace)) return(list(ok = FALSE, value = NULL))
    out <- gr_call_json(client, list(
      list(role = "system", content = paste0(
        "You fill a data-extraction form from one excerpt of a document. Fill only the fields ",
        "this excerpt actually supports; leave every other field null. Do not infer, do not ",
        "estimate, and do not carry over knowledge from outside the excerpt. For each field you ",
        "fill, copy the exact sentence it came from into the matching __quote field.")),
      list(role = "user", content = paste0("Goal: ", question)),
      list(role = "user", content = paste0("Fields:\n", listing)),
      list(role = "user", content = paste0("<excerpt>\n",
                                           render_chunks(d[i, , drop = FALSE]), "\n</excerpt>"))
    ), schema = schema, schema_name = "extraction", allow_empty = TRUE,
       model = spec[["skim_model"]] %||% spec[["model"]], max_output = spec$max_chunk_tokens,
       temperature = spec$temperature, trace = trace, label = "extract.chunk")
    if (spec$delay_between_calls > 0) Sys.sleep(spec$delay_between_calls)
    list(ok = isTRUE(out$ok), value = out$value, chunk = i)
  }, parallel = spec$parallel, label = "extract chunk")

  failed <- sum(!vapply(got, function(g) isTRUE(g$ok), logical(1)))
  rec <- reconcile_fields(got, fields, d, client, spec, trace)

  ev <- if (length(rec$evidence_chunk)) {
    evidence_table(rec$evidence_chunk, rec$evidence_quote,
                   d$page[match(rec$evidence_chunk, d$chunk_id)],
                   d$section[match(rec$evidence_chunk, d$chunk_id)],
                   source_text = d$text[match(rec$evidence_chunk, d$chunk_id)],
                   kind = "extracted", extra = list(field = rec$evidence_field))
  } else NULL

  # A value is SUPPORTED when a span was quoted for it and that span really
  # occurs in the chunk it was attributed to. Two distinct ways to fail: no quote
  # at all, and a quote that is a paraphrase. The second is loud already --
  # `verified = FALSE` sits in the evidence table and new_answer() marks the
  # answer partial. The first was silent, because a field with no quote produced
  # no evidence row at all, so a value nothing supports looked exactly like a
  # value everything supported. That is the failure mode worth naming.
  record <- rec$record
  filled <- names(fields)[!vapply(record, is.null, logical(1))]
  supported <- if (is.null(ev) || !nrow(ev)) character(0)
               else unique(as.character(ev$field[isTRUE_vec(ev$verified)]))
  unsupported <- setdiff(filled, supported)

  # `require_quote` is the strict policy a review protocol needs: no verbatim
  # span, no datum. It is off by default because discarding an extracted value is
  # destructive and the caller should choose it, and because the count below
  # makes the same problem visible without discarding anything.
  if (isTRUE(spec[["require_quote"]]) && length(unsupported)) {
    # `record[[nm]] <- NULL` DELETES the key -- the same trap as modifyList().
    # The record has to stay one entry per field, or the JSON stops saying that
    # the field was looked for, and anything downstream that lines the record up
    # against the schema silently shifts.
    for (nm in unsupported) record[nm] <- list(NULL)
    # Evidence for a value that is no longer in the record would cite a cell that
    # does not exist.
    if (!is.null(ev) && nrow(ev)) ev <- ev[!ev$field %in% unsupported, , drop = FALSE]
    filled <- setdiff(filled, unsupported)
  }

  new_answer(as.character(as_json(record, pretty = FALSE)), "extract", question,
             unique(if (is.null(ev)) integer(0) else ev$chunk_id), trace,
             chunks_sent = d$chunk_id, evidence = ev,
             # An extraction that filled NOTHING is not a partial answer, it is a
             # complete negative one -- the document was read and does not report
             # these fields. Marking it partial would contradict the distinction
             # this reader exists to preserve, and would flag every off-topic
             # paper in a screening run as a broken read. An UNSUPPORTED value is
             # a different matter: something is in the table that nothing in the
             # document backs, and that is exactly what `partial` is for.
             partial = failed > 0 || length(unsupported) > 0,
             notes = list(chunks = nrow(d), fields = length(fields),
                          filled = length(filled),
                          not_reported = setdiff(names(fields), filled),
                          unsupported = unsupported,
                          dropped_unverified = if (isTRUE(spec[["require_quote"]]))
                            unsupported else character(0),
                          conflicts = rec$conflicts, failed_calls = failed,
                          record = record))
}

#' Turn per-chunk partial records into one record.
#'
#' Agreement is free; only disagreement costs a call. `resolve = "first"` (the
#' default) takes the earliest chunk's value and records the conflict, so a run
#' never silently doubles in price because a document repeated itself
#' inconsistently. `resolve = "model"` spends one call per conflicted field.
#' @noRd
reconcile_fields <- function(got, fields, d, client, spec, trace) {
  record <- empty_record(fields)
  conflicts <- list()
  ev_chunk <- integer(0); ev_quote <- character(0); ev_field <- character(0)
  resolve <- match.arg(as_chr1(spec[["resolve"]] %||% "first"), c("first", "model"))

  for (nm in names(fields)) {
    hits <- list()
    for (g in got) {
      if (!isTRUE(g$ok) || is.null(g$value)) next
      v <- coerce_field(g$value[[nm]], fields[[nm]])
      if (is.null(v)) next
      hits[[length(hits) + 1L]] <- list(
        value = v, chunk = d$chunk_id[g$chunk],
        quote = as_chr1(g$value[[paste0(nm, "__quote")]], ""))
    }
    if (!length(hits)) next

    vals <- lapply(hits, function(h) h$value)
    distinct <- !duplicated(vapply(vals, function(v) as_chr1(format(v)), character(1)))
    chosen <- hits[[1]]

    if (sum(distinct) > 1L) {
      alt <- vapply(vals[distinct], function(v) as_chr1(format(v)), character(1))
      conflicts[[nm]] <- alt
      if (identical(resolve, "model") && trace_can_call(trace)) {
        pick <- resolve_conflict(nm, fields[[nm]], hits[distinct], client, spec, trace)
        if (!is.null(pick)) chosen <- pick
      }
    }
    record[[nm]] <- chosen$value
    if (nzchar(trimws(chosen$quote))) {
      ev_chunk <- c(ev_chunk, chosen$chunk)
      ev_quote <- c(ev_quote, chosen$quote)
      # Which field this span supports. Without it the evidence table says only
      # that SOMETHING in chunk 7 was quoted, and an extraction table whose
      # provenance cannot be traced back to the cell it justifies is decoration.
      ev_field <- c(ev_field, nm)
    }
  }
  list(record = record, conflicts = conflicts, evidence_chunk = ev_chunk,
       evidence_quote = ev_quote, evidence_field = ev_field)
}

#' @noRd
resolve_conflict <- function(nm, field, hits, client, spec, trace) {
  opts <- vapply(seq_along(hits), function(i)
    sprintf("%d. %s   (from chunk %s: \"%s\")", i, as_chr1(format(hits[[i]]$value)),
            hits[[i]]$chunk, substr(hits[[i]]$quote, 1, 200)),
    character(1))
  out <- gr_call_json(client, list(
    list(role = "system", content = paste0(
      "Two or more parts of one document give different values for the same field. Choose the ",
      "one the document actually supports for the field as described, by its number. If none is ",
      "supportable, choose 0.")),
    list(role = "user", content = paste0("Field: ", nm, " -- ", field$description)),
    list(role = "user", content = paste(opts, collapse = "\n"))
  ), schema = list(type = "object", additionalProperties = FALSE,
                   required = list("choice"),
                   properties = list(choice = list(type = "integer", minimum = 0,
                                                   maximum = length(hits)))),
     schema_name = "conflict", model = spec$model, max_output = 100L,
     temperature = spec$temperature, trace = trace, label = "extract.resolve")
  if (!isTRUE(out$ok)) return(NULL)
  i <- as_int1(out$value$choice, 0L)
  if (i >= 1L && i <= length(hits)) hits[[i]] else NULL
}
