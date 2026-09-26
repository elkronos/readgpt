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

  # `require_quote` is the strict policy a review protocol needs: no verified
  # quote, no datum. It is off by default because discarding an extracted value is
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
      # What the evidence row will say: the span verifies in the chunk and
      # carries the value (see read_extract()).
      # quote_backs_value() first: it is the cheaper test, and when it passes
      # every passage is in the chunk, so span_match() takes its fast path.
      backs <- quote_backs_value(v, q, src[g$chunk], fields[[nm]]) &&
        isTRUE(span_match(q, src[g$chunk])$verified)
      # "None" or "N/A" in a string field is usually the model saying it found
      # nothing -- often Python's None, spelled out -- and it often fills the
      # quote the same way. It is the answer only when the document itself
      # says it ("Conflicts of interest: None."): kept with a sentence that
      # verifies and spells it, and dropped otherwise, as it always was. Kept
      # on any quote at all, "N/A" quoting "N/A" filled a document that reports
      # nothing and beat a later chunk's verbatim value.
      filler <- identical(fields[[nm]]$type, "string") && is_placeholder_value(v)
      if (filler && !(backs && placeholder_stated(v, q))) next
      hits[[length(hits) + 1L]] <- list(
        value = v, chunk = d$chunk_id[g$chunk], quote = q, supported = backs, filler = filler)
    }
    # A real value anywhere in the document beats a placeholder, rather than
    # contradicting it: that is what the old reading gave, by dropping every
    # placeholder.
    real <- !vapply(hits, function(h) isTRUE(h$filler), logical(1))
    if (any(real)) hits <- hits[real]
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

#' The pieces of a quotation, in the order they are quoted.
#'
#' quote_passages() gives passages, each split into the pieces an elision
#' separates; the checks here take them one after another.
#' @noRd
quote_pieces <- function(quote) unlist(quote_passages(quote)$raw, use.names = FALSE)

#' Does a quotation state a placeholder value itself?
#'
#' "Conflicts of interest: None." does; "None" alone is the model echoing its
#' own filler, and "No country data were given." is the document not
#' reporting the field, which NA already says.
#' @noRd
placeholder_stated <- function(value, quote) {
  pieces <- quote_pieces(quote)
  if (!length(pieces) || all(vapply(pieces, is_placeholder_value, logical(1)))) return(FALSE)
  v <- normalise_for_match(as_chr1(value, ""))
  any(vapply(pieces, function(p) on_word_boundaries(v, p), logical(1)))
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
#' than reading one: every number in the span counts, however the paper wrote
#' it. Reading digit strings alone rejected honest values the old span check
#' had verified, and require_quote then deleted them: "Twenty-four patients",
#' "three arms", "No participants died", "1 204" in the Lancet's thin-space
#' style, "0,45", "0.84" with the Lancet's middle dot, "3.2 x 10-5" and "1.2
#' million" each state the value
#' they were quoted for. So every reading below is added to the others:
#'
#' * digits with comma thousands, a decimal point and an exponent; ".05" is
#'   0.05, as APA style writes p-values;
#' * thousands grouped by a space (thin and no-break spaces included), an
#'   apostrophe or, with a decimal comma, a dot: "1 204", "1'204", "1.204,5";
#' * a decimal comma ("0,45") and the Lancet's middle-dot decimal point
#'   (U+00B7);
#' * scientific notation: "3.2 x 10^-5", with a multiplication sign (U+00D7)
#'   and a superscript exponent (U+207B U+2075), and "3.2 x 10-5" as a PDF
#'   text layer flattens the superscript;
#' * a scale word or suffix ("1.2 million", "$3bn", 3 followed by U+4E07) and a
#'   percentage as
#'   a proportion ("54%" is also 0.54);
#' * English number words (word_numbers()), Chinese numerals (han_numbers())
#'   and a Roman numeral after a word such as "phase" (roman_numbers()).
#'
#' Signs are dropped because a hyphen before a number is as often a range
#' ("20-30") or a name ("COVID-19") as a minus.
#' @noRd
quote_numbers <- function(s) {
  s <- fold_numerals(as_chr1(s, ""))
  if (!nzchar(s)) return(numeric(0))
  # A middle dot (U+00B7) is a decimal point in Lancet style, between two
  # digits, and a multiplication sign before a power of ten.
  times <- gsub("\u00b7(?=\\s*10)", " x ", s, perl = TRUE)
  forms <- c(gsub("(?<=[0-9])\u00b7(?=[0-9])", ".", s, perl = TRUE),
             if (!identical(times, s)) times)
  out <- unlist(lapply(forms, function(f) c(digit_numbers(f), word_numbers(f), han_numbers(f),
                                             roman_numbers(f))),
                use.names = FALSE)
  unique(abs(out[is.finite(out)]))
}

#' Digits of other scripts as ASCII, superscripts as a caret, and the spaces
#' and minus signs normalise_for_match() folds, folded here too, so the
#' readers below see one spelling. Lower case, as a normalised quote is.
#' @noRd
fold_numerals <- function(s) {
  s <- to_utf8(s)
  # Full-width, Arabic-Indic (both), Devanagari and Bengali digits, then the
  # full-width and Arabic decimal and thousands separators and percent sign.
  from <- intToUtf8(c(0xFF10:0xFF19, 0x0660:0x0669, 0x06F0:0x06F9, 0x0966:0x096F,
                      0x09E6:0x09EF, 0xFF0E, 0xFF0C, 0x066B, 0x066C, 0xFF05))
  s <- chartr(from, paste0(strrep("0123456789", 5L), ".,.,%"), s)
  sup <- "\u2070\u00b9\u00b2\u00b3\u2074\u2075\u2076\u2077\u2078\u2079\u207a\u207b"
  m <- gregexpr(paste0("[", sup, "]+"), s, perl = TRUE)
  regmatches(s, m) <- lapply(regmatches(s, m), function(x)
    if (length(x)) paste0("^", chartr(sup, "0123456789+-", x)) else x)
  s <- gsub("[\u00a0\u2007\u2009\u202f]", " ", s, perl = TRUE)
  tolower(gsub("\u2212", "-", s, fixed = TRUE))
}

#' Every number written in digits, in each of the forms quote_numbers() lists.
#' @noRd
digit_numbers <- function(s) {
  num <- function(x) suppressWarnings(as.numeric(x))
  grab <- function(pat) regmatches(s, gregexpr(pat, s, perl = TRUE))[[1]]
  groups <- function(pat) lapply(grab(pat), function(x)
    regmatches(x, regexec(pat, x, perl = TRUE))[[1]][-1L])
  # A figure before a scale word may use either comma: "1,200 million" is
  # 1.2e9 in English and "1,2 millions" 1.2e6 in French.
  figure <- function(x) c(if (grepl("^[0-9]{1,3}(?:,[0-9]{3})+(?:\\.[0-9]+)?$", x, perl = TRUE))
                            num(gsub(",", "", x, fixed = TRUE)),
                          num(chartr(",", ".", x)))
  fig <- "([0-9]+(?:[.,][0-9]+)?)"

  # As written: digits, comma thousands, a decimal point, an exponent.
  out <- num(gsub(",", "", grab("(?:[0-9][0-9,]*(?:\\.[0-9]+)?|\\.[0-9]+)(?:e[-+]?[0-9]+)?"),
                  fixed = TRUE))
  # Thousands grouped by spaces or apostrophes: every run of two groups or
  # more, since a table row "120 118" is two numbers as well as a grouping.
  for (g in grab("(?<![0-9.,])[1-9][0-9]{0,2}(?:[ '][0-9]{3})+(?:[.,][0-9]+)?(?![0-9])")) {
    dec <- regmatches(g, regexpr("[.,][0-9]+$", g, perl = TRUE))
    parts <- strsplit(if (length(dec)) substr(g, 1L, nchar(g) - nchar(dec)) else g,
                      "[ ']", perl = TRUE)[[1]]
    k <- length(parts)
    for (i in seq_len(k - 1L)) for (j in (i + 1L):k) {
      out <- c(out, num(paste0(paste(parts[i:j], collapse = ""),
                               if (j == k && length(dec)) paste0(".", substring(dec, 2L)))))
    }
  }
  # Dots for thousands with a decimal comma, and a decimal comma alone.
  eur <- grab("(?<![0-9.,])[1-9][0-9]{0,2}(?:\\.[0-9]{3})+(?:,[0-9]+)?(?![0-9]|\\.[0-9])")
  out <- c(out, num(chartr(",", ".", gsub(".", "", eur, fixed = TRUE))),
           num(chartr(",", ".", grab("(?<![0-9.,])[0-9]+,[0-9]+(?![0-9]|[.,][0-9])"))))
  # a x 10^b, and 10^b alone. The caret is optional straight after "10" once
  # an "x" has said this is a power, because a PDF's text layer writes the
  # superscript exponent of 3.2 x 10^-5 as "10-5".
  for (g in groups(paste0(fig, "\\s*[x\u00d7*]\\s*10(?:\\s*(?:\\^|\\*\\*)\\s*|(?=[-+]?[0-9]))",
                          "([-+]?[0-9]{1,3})(?![0-9])"))) {
    out <- c(out, num(chartr(",", ".", g[1])) * 10^num(g[2]))
  }
  out <- c(out, 10^num(unlist(groups("(?<![0-9.,])10\\s*(?:\\^|\\*\\*)\\s*([-+]?[0-9]{1,3})"))))
  # Scale words, suffixes and percentages.
  # setNames(), not `"\u5343" = 1e3`: a name written in a call is made a
  # symbol in the native encoding, and under LC_ALL=C that mangles it.
  scale <- stats::setNames(c(1e2, 1e3, 1e6, 1e9, 1e12, 1e5, 1e7, 1e3, 1e6, 1e6, 1e9,
                             1e3, 1e4, 1e4, 1e8, 1e8),
                           c("hundred", "thousand", "million", "billion", "trillion", "lakh",
                             "crore", "k", "m", "mn", "bn",
                             "\u5343", "\u4e07", "\u842c", "\u4ebf", "\u5104"))
  for (g in c(groups(paste0(fig, "\\s*(hundred|thousand|million|billion|trillion|lakh|crore)",
                            "(?![a-z])")),
              groups(paste0(fig, "(k|mn|m|bn)(?![a-z])")),
              groups(paste0(fig, "\\s*([\u5343\u4e07\u842c\u4ebf\u5104])")))) {
    out <- c(out, figure(g[1]) * scale[[g[2]]])
  }
  for (g in groups(paste0(fig, "\\s*(?:%|per ?cent(?![a-z]))"))) out <- c(out, figure(g[1]) / 100)
  out
}

#' English number words.
#'
#' APA and AMA style spell out a number that starts a sentence and, in APA,
#' most numbers below ten, so "Twenty-four patients were enrolled" and
#' "randomised to three arms" are how a count is often quoted. Cardinals and
#' ordinals to the trillions, joined by spaces, hyphens and "and" ("one
#' hundred and twenty"), and the words that state a count without a numeral:
#' "no" and "none" for zero, "both" and "twice" for two.
#' @noRd
word_numbers <- function(s) {
  m <- gregexpr("[a-z]+", s, perl = TRUE)[[1]]
  if (m[1] < 0L) return(numeric(0))
  toks <- regmatches(s, list(m))[[1]]
  from <- as.integer(m)
  to <- from + attr(m, "match.length") - 1L
  out <- unname(.gr_count_words[toks[toks %in% names(.gr_count_words)]])
  small <- c(.gr_number_words, .gr_ordinal_words)
  total <- 0; cur <- 0; last <- ""
  close <- function() {
    if (nzchar(last)) out <<- c(out, total + cur)
    total <<- 0; cur <<- 0; last <<- ""
  }
  # A number runs on across a space or hyphen, never across punctuation.
  joined <- function(i) grepl("^[ -]+$", substr(s, to[i - 1L] + 1L, from[i] - 1L))
  then <- function(i, set) i < length(toks) && toks[i + 1L] %in% set && joined(i + 1L)
  for (i in seq_along(toks)) {
    t <- toks[i]
    if (i > 1L && !joined(i)) close()
    if (t %in% names(small)) {
      v <- small[[t]]
      kind <- if (v < 10) "unit" else if (v < 20) "teen" else "tens"
      # "twenty four" is one number; "two three" and "twenty thirty" are two.
      if (last %in% c("unit", "teen") || (last == "tens" && kind != "unit")) close()
      cur <- cur + v
      last <- kind
      if (t %in% names(.gr_ordinal_words)) close()
    } else if (t %in% c("hundred", "dozen") && nzchar(last) && last != "hundred") {
      cur <- (if (cur > 0) cur else 1) * (if (t == "hundred") 100 else 12)
      last <- "hundred"
    } else if (t %in% names(.gr_scale_words) && nzchar(last)) {
      # Only after a number: the "million" of "1.2 million" is not 1e6 on its
      # own (digit_numbers() reads that one), and "a million" set `last`.
      total <- total + (if (cur > 0) cur else 1) * .gr_scale_words[[t]]
      cur <- 0
      last <- "scale"
    } else if (t == "and" && last %in% c("hundred", "scale") && then(i, names(small))) {
      next
    } else if (t == "a" && then(i, c("hundred", "dozen", names(.gr_scale_words)))) {
      close()
      cur <- 1
      last <- "a"
    } else {
      close()
    }
  }
  close()
  out
}

#' @noRd
.gr_number_words <- c(zero = 0, one = 1, two = 2, three = 3, four = 4, five = 5, six = 6,
                      seven = 7, eight = 8, nine = 9, ten = 10, eleven = 11, twelve = 12,
                      thirteen = 13, fourteen = 14, fifteen = 15, sixteen = 16,
                      seventeen = 17, eighteen = 18, nineteen = 19, twenty = 20,
                      thirty = 30, forty = 40, fifty = 50, sixty = 60, seventy = 70,
                      eighty = 80, ninety = 90)

#' @noRd
.gr_ordinal_words <- c(first = 1, second = 2, third = 3, fourth = 4, fifth = 5, sixth = 6,
                       seventh = 7, eighth = 8, ninth = 9, tenth = 10, eleventh = 11,
                       twelfth = 12, thirteenth = 13, fourteenth = 14, fifteenth = 15,
                       sixteenth = 16, seventeenth = 17, eighteenth = 18, nineteenth = 19,
                       twentieth = 20, thirtieth = 30, fortieth = 40, fiftieth = 50,
                       sixtieth = 60, seventieth = 70, eightieth = 80, ninetieth = 90)

#' @noRd
.gr_scale_words <- c(thousand = 1e3, million = 1e6, billion = 1e9, trillion = 1e12,
                     lakh = 1e5, crore = 1e7)

#' Words that state a count without being a numeral.
#' @noRd
.gr_count_words <- c(no = 0, none = 0, nil = 0, nobody = 0, nought = 0, once = 1,
                     single = 1, twice = 2, both = 2, thrice = 3, half = 0.5)

#' Chinese (and Japanese) numerals: U+4E09 U+7EC4 is "three groups", U+4E00
#' U+767E U+4E8C U+5341 is 120, and a year may be written digit by digit.
#' @noRd
han_numbers <- function(s) {
  # Named with setNames() for the reason digit_numbers() gives.
  digit <- stats::setNames(c(0, 0, 1, 2, 2, 2, 3, 4, 5, 6, 7, 8, 9),
                           c("\u96f6", "\u3007", "\u4e00", "\u4e8c", "\u4e24", "\u5169",
                             "\u4e09", "\u56db", "\u4e94", "\u516d", "\u4e03", "\u516b",
                             "\u4e5d"))
  unit <- stats::setNames(c(10, 100, 1000), c("\u5341", "\u767e", "\u5343"))
  big <- stats::setNames(c(1e4, 1e4, 1e8, 1e8), c("\u4e07", "\u842c", "\u4ebf", "\u5104"))
  runs <- regmatches(s, gregexpr(paste0("[", paste(c(names(digit), names(unit), names(big)),
                                                   collapse = ""), "]+"), s, perl = TRUE))[[1]]
  # A run with no digit and no ten states no number: the hundred (U+767E) of
  # "per cent" or a lone ten thousand (U+4E07) is not 0.
  runs <- runs[grepl(paste0("[", paste(c(names(digit), "\u5341"), collapse = ""), "]"), runs,
                     perl = TRUE)]
  vapply(runs, function(r) {
    total <- 0; section <- 0; n <- 0; prev_digit <- FALSE
    for (ch in strsplit(r, "", fixed = TRUE)[[1]]) {
      if (ch %in% names(digit)) {
        n <- if (prev_digit) n * 10 + digit[[ch]] else digit[[ch]]
        prev_digit <- TRUE
      } else if (ch %in% names(unit)) {
        # Ten then two (U+5341 U+4E8C) is 12: a bare ten counts one ten.
        section <- section + (if (n == 0 && !prev_digit) 1 else n) * unit[[ch]]
        n <- 0; prev_digit <- FALSE
      } else {
        total <- if (big[[ch]] >= 1e8) (total + section + n) * big[[ch]]
                 else total + (section + n) * big[[ch]]
        section <- 0; n <- 0; prev_digit <- FALSE
      }
    }
    total + section + n
  }, numeric(1), USE.NAMES = FALSE)
}

#' Roman numerals where a paper uses them for a number: "phase III", "stage
#' IIb", "grade II/III". Only after such a word, because on its own "iv" is
#' intravenous and "vi" a sixth of nothing.
#' @noRd
roman_numbers <- function(s) {
  pat <- paste0("(?<![a-z])(?:phase|stage|grade|type|class|level|tier|category|group|arm|",
                "part|wave|cycle|step)s?\\s+([ivxl]+)[abc]?(?:\\s*[/-]\\s*([ivxl]+)[abc]?)?",
                "(?![a-z])")
  hits <- regmatches(s, gregexpr(pat, s, perl = TRUE))[[1]]
  romans <- unlist(lapply(hits, function(x) regmatches(x, regexec(pat, x, perl = TRUE))[[1]][-1L]),
                   use.names = FALSE)
  romans <- romans[nzchar(romans)]
  # utils::as.roman() reads "iiii" and "vx" too; it is the value that counts.
  out <- suppressWarnings(as.integer(utils::as.roman(toupper(romans))))
  as.numeric(out[!is.na(out)])
}

#' Could a quotation state its number in words quote_numbers() cannot read?
#'
#' English number words and Chinese numerals are read; "veinticuatro" or
#' Russian's "dvadtsat' chetyre" are not. A quote in such a language whose number is
#' spelled out would fail as if it stated no number, where the span check
#' this replaced verified it, so for such a quote -- and only when no number
#' at all can be read from it -- the span check stands. Letters outside
#' English's alphabet (other than Han, kana and the ligatures a PDF text layer
#' writes) or a common function word of another European language mark one.
#' @noRd
numerals_unreadable <- function(s) {
  if (grepl("(?=\\p{L})[^\\p{Latin}\\p{Han}\\p{Hiragana}\\p{Katakana}]", s, perl = TRUE)) return(TRUE)
  if (grepl("(?=\\p{Latin})[^a-z\ufb00-\ufb06]", s, perl = TRUE)) return(TRUE)
  any(regmatches(s, gregexpr("[a-z]+", s, perl = TRUE))[[1]] %in% .gr_other_language_words)
}

#' Common function words of Spanish, French, German, Portuguese, Italian,
#' Dutch and the Scandinavian languages. Not ones English quotes use too
#' ("et" of "et al.", "die", "a", "e" of "e.g.", "un" of "UN"): a word here
#' only ever loosens the check back to what it was, but it should not do that
#' for English.
#' @noRd
.gr_other_language_words <- c(
  "de", "la", "el", "los", "las", "del", "en", "se", "que", "con", "por", "para",
  "fueron", "una",
  "le", "les", "des", "du", "dans", "avec", "sont", "une", "ont", "qui",
  "der", "das", "und", "mit", "wurden", "eine", "einer", "von", "zu", "bei", "den", "dem",
  "os", "com", "foram", "uma", "dos", "em",
  "il", "di", "gli", "della", "sono", "stati", "nel", "dei", "che",
  "het", "een", "werden", "zijn",
  "och", "og", "blev", "ble")

#' Is `x` among the numbers a quotation states?
#' @noRd
value_among <- function(x, got) {
  length(got) > 0L && is.finite(x) && any(abs(got - x) <= 1e-9 * max(1, x))
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
#'   states, in whatever form it states them (quote_numbers()). A real
#'   sentence paired with an invented number was the worst case
#'   numeric_token() describes -- a figure certified that the paper never gives.
#' * Anything else cannot be looked for in its quote (TRUE is not written in
#'   "funded by Pfizer"), so the quote has to be a passage rather than a word
#'   that occurs anywhere: two words at least, or long enough to be specific in
#'   a script written without spaces, unless the one word is the value itself.
#'
#' A quotation made of several passages (separate lines, bullets, "[...]",
#' **bold**) is split as span_match() splits it (quote_passages()): each
#' passage must sit on word boundaries, and the value may be in any of them.
#' Checked as one string, such a quote never verified here although
#' span_match() accepted it, so require_quote deleted a faithfully quoted
#' value. What the split leaves out is checked too (passage_gaps_ok()), so an
#' elision cannot drop a "not" or join one sentence's subject to another's
#' claim. The value is still ANDed with span_match().
#'
#' A value this rejects is kept and counted in `n_unverified`, as a paraphrase
#' is; only `require_quote = TRUE` drops it.
#' @noRd
quote_backs_value <- function(value, quote, source, field) {
  if (is.null(value) || is.null(field)) return(FALSE)
  q <- quote_passages(quote)
  txt <- as_chr1(source, "")
  src <- normalise_for_match(txt)
  if (!length(q$raw) || !nzchar(src)) return(FALSE)
  # Bold is emphasis, in the quotation or the source: try the quotation as
  # written and, failing that, with bold taken out of both sides, as
  # span_match() does -- never out of one side only.
  if (pieces_back_value(value, unlist(q$raw, use.names = FALSE), src, field)) return(TRUE)
  if (is.null(q$plain) && !grepl("**", txt, fixed = TRUE)) return(FALSE)
  pieces_back_value(value, unlist(q$plain %||% q$raw, use.names = FALSE),
                    normalise_for_match(strip_bold(txt)), field)
}

#' quote_backs_value() for one reading of the quotation: `pieces` in the
#' order they are quoted and `src`, both normalised.
#' @noRd
pieces_back_value <- function(value, pieces, src, field) {
  if (!length(pieces)) return(FALSE)
  if (!all(vapply(pieces, on_word_boundaries, logical(1), src = src))) return(FALSE)
  if (!passage_gaps_ok(pieces, src)) return(FALSE)
  passage <- any(vapply(pieces, function(p) {
    length(regmatches(p, gregexpr("[\\p{L}\\p{N}]+", p, perl = TRUE))[[1]]) >= 2L ||
      nchar(p) >= 12L
  }, logical(1)))
  if (field$type %in% c("integer", "number")) {
    got <- unlist(lapply(pieces, quote_numbers), use.names = FALSE)
    if (value_among(abs(as.numeric(value)), got)) return(TRUE)
    # A passage that states no number this can read, in a language whose
    # number words it does not know, is checked as it was before: by span.
    return(!length(got) && passage && numerals_unreadable(paste(pieces, collapse = " ")))
  }
  if (passage) return(TRUE)
  v <- normalise_for_match(as_chr1(value, ""))
  nzchar(v) && any(vapply(pieces, function(p) on_word_boundaries(v, p), logical(1)))
}

#' Do the passages of a quotation leave out only what a faithful elision may?
#'
#' Each passage verifies on its own, but the quotation also asserts that they
#' belong together, and two ways of joining them change what the document
#' says. Within one sentence, the words left out must not include a negation:
#' "the drug did ... reduce mortality" against "the drug did not reduce
#' mortality" reverses it. Across a sentence boundary, or out of the
#' document's order, the passage after the join must start a sentence, or
#' "Revenue ... rose 30%" is quoted from "Revenue fell 12%. Costs rose 30%.".
#' A pair that fails is treated as the quote was before it was split: not
#' verified. `pieces` and `src` are normalised.
#' @noRd
passage_gaps_ok <- function(pieces, src) {
  if (length(pieces) < 2L) return(TRUE)
  where <- function(p) {
    at <- gregexpr(p, src, fixed = TRUE)[[1]]
    if (at[1] < 0L) integer(0) else as.integer(at)
  }
  boundary <- "[.!?;:][\"')\\]]*(?:\\s|$)"
  starts_sentence <- function(at) {
    before <- sub("[\\s\"'(\\[]*$", "", substr(rep(src, length(at)), 1L, at - 1L), perl = TRUE)
    !nzchar(before) | grepl("[.!?;:][\"')\\]]*$", before, perl = TRUE)
  }
  negated <- function(gap) {
    w <- regmatches(gap, gregexpr("[\\p{L}']+", gap, perl = TRUE))[[1]]
    any(w %in% .gr_negation_words | grepl("n't$", w))
  }
  for (i in seq_len(length(pieces) - 1L)) {
    a <- where(pieces[i]); b <- where(pieces[i + 1L])
    a_end <- a + nchar(pieces[i]) - 1L
    ok <- FALSE
    for (j in seq_along(a)) {
      nxt <- b[b > a_end[j]]
      if (!length(nxt)) next
      gap <- substr(src, a_end[j] + 1L, nxt[1] - 1L)
      ok <- if (!grepl(boundary, gap, perl = TRUE)) !negated(gap) else starts_sentence(nxt[1])
      if (ok) break
    }
    # Out of the document's order, the later passage has to stand as its own
    # sentence.
    if (!ok) ok <- any(starts_sentence(b))
    if (!ok) return(FALSE)
  }
  TRUE
}

#' Words that negate what follows them, for passage_gaps_ok().
#' @noRd
.gr_negation_words <- c("not", "no", "never", "neither", "nor", "none", "nobody", "nothing",
                        "without", "cannot", "non", "failed", "fail", "fails", "unable",
                        "lack", "lacked", "lacking", "absent", "absence")

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

#' Mark the errors a trace recorded after the first `before` as recovered.
#'
#' The shared contract for a request whose failure the pipeline recovers from
#' without losing input: its entry in `trace$errors` carries `recovered =
#' TRUE`, and failed_note() does not count it as a failed read.
#' @noRd
extract_mark_recovered <- function(trace, before) {
  if (!inherits(trace, "gr_trace")) return(invisible(NULL))
  n <- length(trace$errors)
  if (n > before) {
    for (i in seq.int(before + 1L, n)) trace$errors[[i]]$recovered <- TRUE
  }
  invisible(NULL)
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
  before <- if (inherits(trace, "gr_trace")) length(trace$errors) else 0L
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
  if (!isTRUE(out$ok)) {
    # The field falls back to the first value, as with resolve = "first", and
    # every excerpt was still read: the failure is recovered. Counted as a
    # failed read, it made a fully extracted document "failed", unstored and
    # left out of synthesis, and re-read on every run while the call kept
    # failing. The conflict itself stays in `conflicts`.
    extract_mark_recovered(trace, before)
    return(NULL)
  }
  # json_field(): `$` let a reply keyed `choices` answer a read of `choice`.
  i <- as_int1(json_field(out$value, "choice"), 0L)
  if (i >= 1L && i <= length(hits)) hits[[i]] else NULL
}
