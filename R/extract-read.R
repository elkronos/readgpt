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
    warn_capped_batch(trace, "extract", nrow(d), "segment more coarsely")
  }

  model <- spec[["skim_model"]] %||% spec[["model"]]
  fixed <- list(
    list(role = "system", content = paste0(
      "You fill a data-extraction form from one excerpt of a document. Fill only the fields ",
      "this excerpt actually supports; leave every other field null. Do not infer, do not ",
      "estimate, and do not carry over knowledge from outside the excerpt. For each field you ",
      "fill, copy the exact sentence it came from into the matching __quote field.")),
    list(role = "user", content = paste0("Goal: ", question)),
    list(role = "user", content = paste0("Fields:\n", listing)))
  excerpts <- vapply(seq_len(nrow(d)), function(i)
    paste0("<excerpt>\n", render_chunks(d[i, , drop = FALSE]), "\n</excerpt>"), character(1))
  # The largest request with its reply at the cap: what gr_lapply() holds a
  # parallel batch to, since the workers cannot see what the run spends.
  # Counted from the messages themselves: the field listing goes out with
  # every excerpt, and a long one is most of each prompt. Held only to
  # preflight's flat allowance for the prompt around a chunk, a parallel
  # extraction of 40 fields spent two thirds past max_cost_usd.
  worst <- extract_item_usd(client, model,
                            sum(gr_count_tokens(vapply(fixed, `[[`, character(1), "content"))) +
                              max(c(0L, gr_count_tokens(excerpts))),
                            spec$max_chunk_tokens)

  got <- gr_lapply(seq_len(nrow(d)), function(i, trace) {
    if (!trace_can_call(trace)) return(list(ok = FALSE, value = NULL, capped = TRUE))
    out <- gr_call_json(client, c(fixed, list(list(role = "user", content = excerpts[i]))),
                        schema = schema, schema_name = "extraction", allow_empty = TRUE,
                        model = model, max_output = spec$max_chunk_tokens,
                        temperature = spec$temperature, trace = trace, label = "extract.chunk")
    if (spec$delay_between_calls > 0) Sys.sleep(spec$delay_between_calls)
    list(ok = isTRUE(out$ok), value = out$value, chunk = i)
  }, parallel = spec$parallel, label = "extract chunk", trace = trace,
     client = client, item_usd = worst)

  # A request a limit stopped was not sent, so it did not fail: gr_read() names
  # the limit.
  capped <- vapply(got, function(g) isTRUE(g$capped), logical(1))
  failed <- sum(!vapply(got, function(g) isTRUE(g$ok), logical(1)) & !capped)
  rec <- reconcile_fields(got, fields, d, client, spec, trace)

  src <- extract_verbatim_source(d)
  ev <- if (length(rec$evidence_chunk)) {
    at <- match(rec$evidence_chunk, d$chunk_id)
    evidence_table(rec$evidence_chunk, rec$evidence_quote, d$page[at], d$section[at],
                   source_text = src[at],
                   kind = "extracted", extra = list(field = rec$evidence_field))
  } else NULL
  # `verified` from evidence_table() says only that the span occurs in the
  # chunk. For an extracted value that is not enough: "We enrolled 120 people."
  # verified n = 5000, and so did the quote "2". The span also has to carry the
  # value it is cited for, or it is not evidence for it. `match` keeps the span
  # score, so verified = FALSE with match = 1 reads as "the sentence is there,
  # and it does not say this".
  if (!is.null(ev) && nrow(ev)) {
    backs <- vapply(seq_len(nrow(ev)), function(i) {
      nm <- as.character(ev$field[i])
      quote_backs_value(rec$record[[nm]], ev$text[i], ev$source_text[i], fields[[nm]])
    }, logical(1))
    ev$verified <- ev$verified & backs
  }

  # A value is SUPPORTED when a span was quoted for it, that span really occurs
  # in the chunk it was attributed to, and it carries the value. Two distinct
  # ways to fail: no quote at all, and a quote that is a paraphrase or does not
  # say this. The second is loud already -- `verified = FALSE` sits in the
  # evidence table and new_answer() marks the answer partial. The first was
  # silent, because a field with no quote produced no evidence row at all, so a
  # value nothing supports looked exactly like a value everything supported.
  # That is the failure mode worth naming.
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

  # digits = NA: jsonlite rounds to four decimal places by default, and this
  # text is the answer -- summary$answer, the audit report, as_json() and the
  # store fallback in answer_record() all read it. p = 0.00003 came out as 0.
  new_answer(as.character(as_json(record, pretty = FALSE, digits = NA)), "extract", question,
             unique(if (is.null(ev)) integer(0) else ev$chunk_id), trace,
             chunks_sent = d$chunk_id, evidence = ev,
             # An extraction that filled NOTHING is not a partial answer, it is a
             # complete negative one -- the document was read and does not report
             # these fields. Marking it partial would contradict the distinction
             # this reader exists to preserve, and would flag every off-topic
             # paper in a screening run as a broken read. An UNSUPPORTED value is
             # a different matter: something is in the table that nothing in the
             # document backs, and that is exactly what `partial` is for.
             partial = failed > 0 || any(capped) || length(unsupported) > 0,
             # With a request failed, an empty field may be in the excerpt that
             # was never read, so it is unknown rather than not reported.
             notes = list(chunks = nrow(d), fields = length(fields),
                          filled = length(filled),
                          not_reported = if (failed > 0) character(0)
                                         else setdiff(names(fields), filled),
                          unknown = if (failed > 0) setdiff(names(fields), filled)
                                    else character(0),
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
#' Chunks that agree on a value pool their quotes: the value is cited with the
#' best one any of them gave (best_supported_hit()).
#' @noRd
reconcile_fields <- function(got, fields, d, client, spec, trace) {
  record <- empty_record(fields)
  conflicts <- list()
  ev_chunk <- integer(0); ev_quote <- character(0); ev_field <- character(0)
  resolve <- match.arg(as_chr1(spec[["resolve"]] %||% "first"), c("first", "model"))
  src <- extract_verbatim_source(d)

  for (nm in names(fields)) {
    hits <- list()
    for (g in got) {
      if (!isTRUE(g$ok) || is.null(g$value)) next
      v <- coerce_field(g$value[[nm]], fields[[nm]])
      if (is.null(v)) next
      q <- as_chr1(g$value[[paste0(nm, "__quote")]], "")
      # "None" or "N/A" in a string field with no sentence behind it is the
      # model saying it found nothing -- often Python's None, spelled out. With
      # a sentence behind it, it is the answer ("Conflicts of interest: None."),
      # and dropping it recorded a stated fact as not reported.
      if (identical(fields[[nm]]$type, "string") && is_placeholder_value(v) &&
          !nzchar(trimws(q))) next
      # quote_backs_value() finds the span in the chunk before anything else,
      # so TRUE here also means it verifies.
      hits[[length(hits) + 1L]] <- list(
        value = v, chunk = d$chunk_id[g$chunk], quote = q,
        supported = quote_backs_value(v, q, src[g$chunk], fields[[nm]]))
    }
    if (!length(hits)) next

    # value_key(), not format(). format() keeps seven significant digits, so
    # 0.123456789 and 0.123456781 -- or two counts above 2^31 -- were the same
    # value and a real conflict between two parts of one paper was not reported.
    keys <- vapply(hits, function(h) value_key(h$value), character(1))
    # One hit per distinct value, in order of first appearance -- so "first"
    # still means the earliest chunk's VALUE -- but carrying the best quote any
    # chunk gave for it. Taking the first hit's quote made the abstract's
    # unquoted n = 120 hide the methods section's verbatim sentence for the same
    # 120: the value counted as unsupported, and require_quote deleted a datum
    # the document states word for word.
    reps <- lapply(unique(keys), function(k) best_supported_hit(hits[keys == k]))
    chosen <- reps[[1]]

    if (length(reps) > 1L) {
      conflicts[[nm]] <- unique(keys)
      if (identical(resolve, "model") && trace_can_call(trace)) {
        pick <- resolve_conflict(nm, fields[[nm]], reps, client, spec, trace)
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

#' Of several hits giving the same value, the one whose quote best backs it.
#'
#' A quote that verifies and carries the value first, then any quote at all (so
#' a paraphrase is still shown and flagged rather than replaced by nothing),
#' then the first hit.
#' @noRd
best_supported_hit <- function(hits) {
  for (h in hits) if (isTRUE(h$supported)) return(h)
  for (h in hits) if (nzchar(trimws(h$quote))) return(h)
  hits[[1]]
}

#' The most one extraction request can cost: `input_tokens` of prompt and a
#' reply at `max_output`, priced at the model that is billed for it.
#'
#' That is the model the request names, except through a client that bills
#' every call as its own model whatever the request names (an ellmer chat;
#' see client_billed_model()). NA when that model has no price, which a
#' spending limit cannot count in this process either.
#' @noRd
extract_item_usd <- function(client, model, input_tokens, max_output) {
  billed <- client_billed_model(client) %||% model %||%
    as_chr1(client[["model", exact = TRUE]], gr_options("model"))
  # The request itself warns about an unknown model; once is enough.
  quiet <- function(expr) tryCatch(suppressWarnings(expr, classes = "gr_unknown_model"),
                                   error = function(e) NULL)
  info <- quiet(gr_model_info(billed))
  if (is.null(info)) return(NA_real_)
  out <- min(as_num1(max_output, 0), as_num1(info$max_output, Inf))
  as_num1(quiet(gr_estimate_cost(billed, input_tokens, out)), NA_real_)
}

#' The document text each chunk was cut from, with nothing a model wrote.
#'
#' The shared chunk contract: a `source_text` column, where present and not NA,
#' is the text the chunk was derived from, without a contextual header or
#' propositions a segmenter had a model write into `text`. NA, or no column,
#' means `text` is itself source. A quote is evidence only if it is in the
#' document, so it is checked against this and never against model-written text.
#' `[[`, not `$`: `$` on a data frame partial-matches.
#' @noRd
extract_verbatim_source <- function(d) {
  src <- as.character(d[["text"]])
  own <- d[["source_text"]]
  if (!is.null(own)) {
    own <- as.character(own)
    use <- !is.na(own)
    src[use] <- own[use]
  }
  src
}

#' The numbers a quotation states, as absolute values.
#'
#' Wider than numeric_token(), because this looks for a value among many rather
#' than reading one: every number in the span counts, and ".05" is 0.05, as APA
#' style writes p-values. Signs are dropped because a hyphen before a number is
#' as often a range ("20-30") or a name ("COVID-19") as a minus.
#' @noRd
quote_numbers <- function(s) {
  pat <- "(?:[0-9][0-9,]*(?:\\.[0-9]+)?|\\.[0-9]+)(?:[eE][-+]?[0-9]+)?"
  hits <- regmatches(s, gregexpr(pat, s, perl = TRUE))[[1]]
  out <- suppressWarnings(as.numeric(gsub(",", "", hits, fixed = TRUE)))
  out[is.finite(out)]
}

#' Does a quotation carry the value it is cited for?
#'
#' span_match() answers whether the span is in the chunk. This answers whether,
#' being there, it backs THIS value, and it fails the ways a quote can be real
#' and still prove nothing:
#'
#' * It must sit on word boundaries in the chunk. "120 participants" occurs
#'   inside "1120 participants", and "2" inside almost anything.
#' * For an integer or a number, the value must be one of the numbers the span
#'   states. A real sentence paired with an invented number was the worst case
#'   numeric_token() describes -- a figure certified that the paper never gives.
#' * Anything else cannot be looked for in its quote (TRUE is not written in
#'   "funded by Pfizer"), so the quote has to be a passage rather than a word
#'   that occurs anywhere: two words at least, or long enough to be specific in
#'   a script written without spaces, unless the one word is the value itself.
#'
#' A value this rejects is kept and counted in `n_unverified`, as a paraphrase
#' is; only `require_quote = TRUE` drops it.
#' @noRd
quote_backs_value <- function(value, quote, source, field) {
  if (is.null(value) || is.null(field)) return(FALSE)
  s <- trim_quote_edges(normalise_for_match(as_chr1(quote, "")))
  src <- normalise_for_match(as_chr1(source, ""))
  if (!nzchar(s) || !nzchar(src)) return(FALSE)
  if (!on_word_boundaries(s, src)) return(FALSE)
  if (field$type %in% c("integer", "number")) {
    x <- abs(as.numeric(value))
    got <- quote_numbers(s)
    return(length(got) > 0L && any(abs(got - x) <= 1e-9 * max(1, x)))
  }
  words <- regmatches(s, gregexpr("[\\p{L}\\p{N}]+", s, perl = TRUE))[[1]]
  if (length(words) >= 2L || nchar(s) >= 12L) return(TRUE)
  v <- normalise_for_match(as_chr1(value, ""))
  nzchar(v) && on_word_boundaries(v, s)
}

#' Does `s` occur in `src` without starting or ending inside a word or number?
#' Both already normalised. Han and kana are not word characters here: those
#' scripts put no boundary between words, so any cut between two of them is one.
#' @noRd
on_word_boundaries <- function(s, src) {
  at <- gregexpr(s, src, fixed = TRUE)[[1]]
  if (at[1] < 0L) return(FALSE)
  len <- nchar(s)
  alnum <- function(ch) grepl("^[\\p{L}\\p{N}]$", ch, perl = TRUE) &
                        !grepl("^[\\p{Han}\\p{Hiragana}\\p{Katakana}]$", ch, perl = TRUE)
  before <- substr(rep(src, length(at)), at - 1L, at - 1L)
  after <- substr(rep(src, length(at)), at + len, at + len)
  open_ok <- !alnum(substr(s, 1L, 1L)) | !alnum(before)
  close_ok <- !alnum(substr(s, len, len)) | !alnum(after)
  any(open_ok & close_ok)
}

#' A value as a string that distinguishes every distinct value.
#'
#' Full precision for numbers, because the point is to tell two values apart:
#' `format()` rounds to seven significant digits and made different values
#' identical. `%.15g` keeps every digit a value written in a paper can have, so
#' two such values differ here exactly when they differ on the page; it also
#' prints a whole number without a decimal point or an exponent.
#' @noRd
value_key <- function(v) {
  if (is.numeric(v)) return(sprintf("%.15g", v[1]))
  as_chr1(v, "")
}

#' @noRd
resolve_conflict <- function(nm, field, hits, client, spec, trace) {
  # value_key(), for the reason reconcile_fields() uses it: format() showed
  # 3000000001 and 3000000002 both as "3e+09", and the model was asked to choose
  # between two options it could not tell apart.
  opts <- vapply(seq_along(hits), function(i)
    sprintf("%d. %s   (from chunk %s: \"%s\")", i, value_key(hits[[i]]$value),
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
  # json_field(): `$` let a reply keyed `choices` answer a read of `choice`.
  i <- as_int1(json_field(out$value, "choice"), 0L)
  if (i >= 1L && i <= length(hits)) hits[[i]] else NULL
}
