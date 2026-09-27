# claims.R -- the layer between a table of studies and a write-up.
#
# WHY THIS FILE EXISTS
# `gr_synthesise()`'s unit is the SECTION, and sections come from an outline
# fixed before the reading. Each one is then drafted independently from the whole
# study table. Three things follow, and none of them can be repaired downstream:
#
#   1. The review's structure is the author's hypothesis rather than a finding.
#      Often the strongest sentence a review contains is structural -- "the
#      disagreement about effect size is really a disagreement about
#      measurement" -- and that can only come from the evidence.
#   2. Studies arrive as rows, so the model writes row by row: "Smith (2019)
#      found X. Garcia (2022) found Y." That enumeration is the clearest marker
#      of a weak review and it is a direct consequence of what the prompt holds.
#   3. Nothing computes relations between studies. Synthesis IS the claim about
#      relations, so without this layer "synthesise" concatenates.
#
# A claim here is checkable in exactly the way a citation already is. It names
# the studies it rests on, those numbers are verified against the table rather
# than trusted, and each study's values are backed by a quote that was checked
# against the page it came from. That extends the audit chain by one link --
# claim -> studies -> quotes -> pages -- instead of stepping around it.

#' What a claim is about.
#' @noRd
.gr_claim_kinds <- c("finding", "method", "measurement", "gap")

#' Every property is in `required`, the optional ones written as "string or
#' null" or as an array that may be empty. That is not style: the schema is sent
#' with `strict = TRUE`, and OpenAI refuses a strict schema whose `required`
#' leaves a property out, with a 400 that is not retried. Leaving out
#' `contradicted_by`, `moderator` and `scope` made every claims call on the
#' default client fail, and gr_claims() report that no claims came back.
#' @noRd
.gr_claims_schema <- list(
  type = "object", additionalProperties = FALSE,
  required = list("claims"),
  properties = list(claims = list(
    type = "array",
    items = list(
      type = "object", additionalProperties = FALSE,
      required = list("claim", "kind", "supported_by", "contradicted_by", "moderator", "scope"),
      properties = list(
        claim = list(type = "string",
                     description = paste0("One sentence about the LITERATURE, not about one ",
                                          "study. State it plainly, without citations.")),
        kind = list(type = "string", enum = as.list(.gr_claim_kinds),
                    description = paste0("'finding' for what was found, 'method' for how it was ",
                                         "studied, 'measurement' for how the construct was ",
                                         "defined, 'gap' for what is missing.")),
        supported_by = list(type = "array", items = list(type = "integer"),
                            description = paste0("The [study N] numbers that support it. Use only ",
                                                 "numbers shown. At least one.")),
        contradicted_by = list(type = "array", items = list(type = "integer"),
                               description = paste0("The [study N] numbers that contradict it, ",
                                                    "or an empty array if none do.")),
        moderator = list(type = c("string", "null"),
                         description = paste0("The FIELD NAME from the table that distinguishes ",
                                              "the supporting studies from the contradicting ",
                                              "ones, copied exactly, or null if nothing in the ",
                                              "table explains the split.")),
        scope = list(type = c("string", "null"),
                     description = paste0("How far the claim reaches: the populations, designs ",
                                          "or measures it holds for. Say so if it rests on one ",
                                          "study.")))))))

#' @noRd
.gr_reconcile_schema <- list(
  type = "object", additionalProperties = FALSE,
  required = list("groups"),
  properties = list(groups = list(
    type = "array",
    items = list(type = "array", items = list(type = "integer")),
    description = paste0("One array per distinct claim, holding the numbers of the listed ",
                         "claims that say the same thing. Every number exactly once."))))

#' The claims a table of studies supports
#'
#' Turns an extraction table into statements about the *literature*, each naming
#' the studies it rests on. It is the step that makes a write-up a synthesis
#' rather than an ordered recitation, and the step that lets a section be written
#' from an argument instead of from rows.
#'
#' @section What makes a claim checkable:
#' Every study number a claim names is verified against the table, exactly as a
#' `[study N]` marker in finished prose already is, and against the batch the
#' claim was drawn from: a claim may only name studies its call was shown. A
#' number that fails either is dropped and counted rather than trusted, a claim
#' left with no supporting study is dropped entirely, and a `moderator` naming a
#' column the model was not shown is cleared: it is an invented explanation for
#' a real disagreement. So is a moderator that does not tell the two sides of a
#' contested claim apart by counting: a column the table does not report for
#' the studies on one side, a value reported on both sides, or numbers whose
#' ranges overlap. `$dropped` records all of it, so a claims table that looks
#' thin can be told apart from a literature that is.
#'
#' The study numbers are the same ones [gr_synthesise()] cites, because both
#' derive them from one function. A claim resting on study 3 and a sentence
#' citing `[study 3]` point at the same row by construction.
#'
#' @section Corpora too large for one prompt:
#' The studies are batched, and claims are then reconciled across batches in one
#' further call that sees only the claim TEXTS. Without it a claim holding across
#' the whole corpus comes back once per batch with disjoint support, which reads
#' as several narrow claims instead of one broad one. The reconcile pass may only
#' group claims that already exist: every claim it fails to place stays on its
#' own rather than disappearing. When it cannot run -- a call or cost limit, a
#' failed or cut-off reply, a reply that is not a list of groups, or more claims
#' than fit one prompt -- the claims are kept unmerged, with a warning, and
#' `$unmerged` is `TRUE`.
#'
#' What the reconcile pass cannot check is MEANING. A merged claim keeps one
#' member's wording and the union of every member's studies, so if the model
#' groups two claims that say different things, the studies behind one are
#' listed as supporting the other's wording. Such a merge is not detectable by
#' counting; the merged claim's `note` names every other wording it absorbed so
#' that it can be seen, and a group mixing claims of different `kind` is warned
#' about.
#'
#' A batch is limited by the reply as well as by the context window, because the
#' reply names every study it uses: with the default `max_claim_tokens` a batch
#' holds about 30 studies. A batch whose reply is cut off, or whose call fails,
#' is reported with the number of studies it held rather than passed over.
#'
#' @param extraction A [gr_extract()] result, or a data frame shaped like its
#'   `$table`.
#' @param question The review question. Taken from `protocol` if omitted.
#' @param protocol A [gr_protocol()]; its `question` is used when `question` is
#'   not given.
#' @param client A [gr_client()]. One is built from `model` if omitted.
#' @param model,temperature,max_claim_tokens Passed to the model call.
#'   `max_claim_tokens` is the reply limit for each call, and it also sets how
#'   many studies go into one: about `(max_claim_tokens - 400) / 40`. Raise it
#'   for fewer, larger batches.
#' @param include_unclear Keep rows with nothing extracted. Off by default, the
#'   same as [gr_synthesise()].
#' @param trace A [gr_trace()] to record into.
#' @param bib Which columns carry bibliographic identity, as in
#'   [gr_synthesise()]: a named list of `citation`, `authors`, `year`, `title`,
#'   `venue`, `doi`. Those columns are withheld from the model, so a claim
#'   cannot attribute a finding to a name, and cannot be moderated by one.
#'   Omitted, only the conventional names are withheld. Pass the same `bib` to
#'   [gr_synthesise()] and [gr_gaps()].
#' @return An object of class `gr_claims`:
#'   \describe{
#'     \item{`claims`}{One row per claim: `claim_id`, `claim`, `kind`,
#'       `moderator`, `scope`, `n_support`, `n_contradict`, `note`.}
#'     \item{`support`}{Long: `claim_id`, `study`, `role` (`"supports"` or
#'       `"contradicts"`). Join it to `$studies` to reach documents and quotes.}
#'     \item{`studies`}{The rows the claims were drawn from, numbered.}
#'     \item{`dropped`}{What verification removed, and why.}
#'     \item{`partial`}{`TRUE` when some studies contributed nothing because
#'       their batch was cut off at the reply limit, failed, or was not sent
#'       for a call or cost limit.}
#'     \item{`lost`}{The study numbers of those studies.}
#'     \item{`unmerged`}{`TRUE` when claims from several batches could not be
#'       reconciled, so one finding may appear as more than one claim.}
#'     \item{`hidden`}{The columns withheld from the model as bibliographic.}
#'   }
#' @seealso [gr_outline()] to derive sections from these claims,
#'   [gr_synthesise()] to write from them, [gr_gaps()] for what they do not
#'   cover, [gr_protocols()] for the `claims` schema they want as input
#' @export
#' @examples
#' tab <- data.frame(
#'   document = c("a.pdf", "b.pdf"), status = "ok", duplicate_of = NA_character_,
#'   n_filled = 2L, n_unverified = 0L, conflicts = NA_character_,
#'   design = c("randomised trial", "cross-sectional"),
#'   finding = c("supports", "contradicts"), stringsAsFactors = FALSE)
#'
#' cl <- gr_mock_client(function(messages, params) paste0(
#'   '{"claims":[{"claim":"The effect appears in trials but not in surveys.",',
#'   '"kind":"finding","supported_by":[1],"contradicted_by":[2],',
#'   '"moderator":"design","scope":"one trial, one survey"}]}'))
#'
#' cm <- gr_claims(tab, question = "Does it work?", client = cl)
#' cm$claims[, c("claim", "moderator", "n_support", "n_contradict")]
#' cm$support
gr_claims <- function(extraction, question = NULL, protocol = NULL, client = NULL,
                      model = NULL, temperature = NULL, max_claim_tokens = 1600L,
                      include_unclear = FALSE, trace = NULL, bib = NULL) {
  tab <- if (inherits(extraction, "gr_extraction")) extraction$table else extraction
  if (!is.data.frame(tab) || !nrow(tab)) {
    gr_abort("`extraction` must be a gr_extraction, or a data frame shaped like its $table.",
             class = "gr_no_studies")
  }
  if (!is.null(protocol)) {
    if (!inherits(protocol, "gr_protocol")) {
      gr_abort("`protocol` must come from gr_protocol().", class = "gr_bad_protocol")
    }
    check_protocol_edited(protocol, "draw claims against it")
    if (is.null(question)) question <- protocol$question
  }
  if (!is_nonblank(question)) {
    gr_abort(paste0("`question` must be a non-empty string. A claim is a statement about the ",
                    "literature RELATIVE TO a question, and without one there is nothing for a ",
                    "study to support or contradict."),
             class = "gr_no_question")
  }

  used <- synth_studies(tab, include_unclear)
  client <- client %||% gr_client(model = model %||% gr_options("model"))
  # This function's own default, range check and name, not gr_read_spec()'s;
  # and the budget reserves what the spec settled on. Passed through raw, an NA
  # reached gr_budget() as the output reserve while the spec said 1500.
  max_claim_tokens <- clamp_warn(na_default(max_claim_tokens, 1600L, "max_claim_tokens"),
                                 16, 1e6, "max_claim_tokens")
  spec <- gr_read_spec("stuff", model = model, temperature = temperature,
                       max_answer_tokens = max_claim_tokens)
  # One model for the batch sizing and the requests. Left NULL, gr_budget()
  # sized batches for gr_options("model") while gr_call() asked the client's.
  spec <- resolve_read_model(spec, client)
  trace <- trace %||% gr_trace(meta = list(stage = "claims", question = question,
                                           studies = nrow(used)))

  # Same withholding as the write-up: a model that can see who wrote a study
  # will attribute to the name rather than to the number, and a claim attributed
  # to a name is checked by nothing. With the write-up's `bib` too: without it a
  # `first_author` column reached this model, came back inside claim text and as
  # a moderator, and gr_synthesise(bib =) then pasted both into a prompt whose
  # studies block had withheld that column.
  hidden <- unlist(bib_columns(used, bib), use.names = FALSE)
  rendered <- render_studies(used, hide = hidden)
  # "never": claims_batch() sends the question once.
  overhead <- prompt_overhead(question, .gr_prompts$claims_system, "never")
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)
  # Sized by what comes BACK as well as by what goes in. The reply names every
  # study it uses and carries a claim, a scope and a moderator per claim, so it
  # grows with the batch while `max_claim_tokens` stays where it is. Sized by the
  # input window alone, a 128k model put 300 studies in one call with a
  # 1600-token reply; the JSON was cut off, did not parse, and every study in
  # the batch contributed nothing.
  groups <- synth_batches(rendered, bud$input, max_n = claims_per_batch(bud$output))
  index <- attr(groups, "index")

  raw <- list()
  not_sent <- 0L
  unsent <- integer(0)
  failed <- integer(0)
  cut_off <- integer(0)
  for (g in seq_along(groups)) {
    if (length(groups) > 1L) gr_msg(sprintf("Claims from batch %d of %d.", g, length(groups)))
    # A batch a limit stopped is not a batch the model found nothing in.
    if (!trace_can_call(trace)) {
      not_sent <- not_sent + 1L
      unsent <- c(unsent, g)
      next
    }
    b <- claims_batch(groups[[g]], question, client, spec, trace, shown = used$study[index[[g]]])
    if (identical(b$status, "cut_off")) cut_off <- c(cut_off, g)
    if (identical(b$status, "failed")) failed <- c(failed, g)
    raw[[g]] <- b$rows
  }
  if (not_sent > 0L) {
    gr_warn(sprintf(paste0("%d of %d batch(es) of studies were not sent: the run reached its %s. ",
                           "The studies in them contribute no claims. Raise the limit and run ",
                           "gr_claims() again."),
                    not_sent, length(groups), cap_name(trace)),
            class = "gr_claims_capped")
  }
  # A batch that was sent and came back with nothing usable. With one batch this
  # was only "every call failed"; with several, the survivors' claims hid the
  # loss entirely, and a review was written from the leftover batch.
  lost <- c(cut_off, failed)
  # The studies that contributed nothing, on the result as well as in the
  # warnings: a claims table missing a third of the corpus otherwise looks
  # exactly like a complete one to anything that reads it later.
  lost_studies <- sort(used$study[unlist(index[c(unsent, lost)], use.names = FALSE)])
  lost_msg <- if (!length(lost)) NULL else sprintf(paste0(
    "%d of %d batch(es) of studies, holding %d of %d studies, returned nothing usable (%s), ",
    "so those studies contribute no claims."),
    length(lost), length(groups), length(unlist(index[lost])), nrow(used),
    paste(c(if (length(cut_off)) sprintf(paste0(
              "%d cut off at the %d-token reply limit before the JSON closed; raise ",
              "`max_claim_tokens`"), length(cut_off), spec$max_answer_tokens),
            if (length(failed)) sprintf("%d call(s) failed", length(failed))),
          collapse = "; "))
  got <- do.call(rbind, raw[!vapply(raw, is.null, logical(1))])
  if (is.null(got) || !nrow(got)) {
    gr_warn(paste0("No claims came back. ",
                   if (not_sent == length(groups)) sprintf("No batch was sent: the run had reached its %s. ",
                                                           cap_name(trace))
                   else if (length(lost)) paste0(lost_msg, " ")
                   else "The model returned none. ",
                   "There is nothing for gr_outline() or gr_synthesise(claims = ) to work from."),
            class = "gr_no_claims")
    return(new_claims(empty_claim_rows(), used, question, empty_dropped(), trace, lost_studies,
                      hidden = hidden))
  }
  if (length(lost)) {
    gr_warn(lost_msg, class = if (length(cut_off)) c("gr_claims_truncated", "gr_claims_batch_failed")
                              else "gr_claims_batch_failed")
  }

  # The columns the model was SHOWN, not every column in the table. The moderator
  # check used the reserved-field list, which leaves the bibliographic columns in
  # -- so a claim could name `year` as the moderator of a disagreement, be
  # accepted, and carry an explanation drawn from a column render_studies()
  # withheld precisely so it could not be used. An invented explanation for a
  # real disagreement is the most convincing error this layer can make.
  checked <- claims_verify(got, used, cols = study_fields(used, hide = hidden))
  if (!nrow(checked$claims)) {
    gr_warn(paste0("Every claim was dropped in verification; see `$dropped`. The usual cause is ",
                   "a model citing study numbers that are not in the table."),
            class = "gr_no_claims")
    return(new_claims(empty_claim_rows(), used, question, checked$dropped, trace, lost_studies,
                      hidden = hidden))
  }
  # Claims can repeat only across batches that returned some.
  final <- if (sum(vapply(raw, function(r) NROW(r) > 0L, logical(1))) > 1L) {
    claims_reconcile(checked$claims, question, client, spec, trace)
  } else checked$claims
  # A merge changes which studies sit on each side of a claim, so a moderator
  # that told a member's two sides apart may not tell the merged claim's apart.
  # Checked again on what is actually returned.
  again <- claims_moderators(final, used)
  new_claims(again$claims, used, question, rbind(checked$dropped, again$dropped), trace,
             lost_studies, hidden = hidden, unmerged = isTRUE(attr(final, "unmerged")))
}

#' What a claims reply costs, for sizing batches by it.
#'
#' A base for the JSON around the claims plus a share per study: each study is
#' named at least once, and every few studies carry a claim sentence, a scope
#' and a moderator. Deliberately generous. A batch too large for its reply is
#' lost whole, while a batch smaller than it needed to be costs one more call.
#' @noRd
.gr_claims_reply_tokens <- c(base = 400L, per_study = 40L)

#' How many studies one claims call can take and still answer in `reply` tokens.
#' @noRd
claims_per_batch <- function(reply) {
  max(1L, as.integer(floor((reply - .gr_claims_reply_tokens[["base"]]) /
                             .gr_claims_reply_tokens[["per_study"]])))
}

#' What a reconcile reply for `n` claims can take: every claim number once, with
#' its comma and the brackets of its group. About two tokens a claim; six, to be
#' generous, since a reply cut off here leaves every claim unmerged.
#' @noRd
reconcile_reply_tokens <- function(n) {
  as.integer(200L + 6L * n)
}

#' What went wrong with a call that returned nothing usable, as a phrase
#' following "the ... call".
#'
#' Three causes that need three different remedies. gr_call() shrinks the reply
#' to whatever the context window leaves after the prompt, so a long prompt is
#' cut off BELOW the limit it asked for, and "raise the limit" was advice that
#' could not help. `knob` names the argument that raises `cap`.
#' @noRd
call_failure <- function(result, messages, model, cap, knob) {
  if (reply_cut_off(result)) {
    info <- gr_model_info(model)
    prompt <- sum(gr_count_tokens(vapply(messages, function(m) as_chr1(m$content), character(1))))
    room <- as.integer(info$context_window - prompt - 32L)
    if (room < min(cap, info$max_output)) {
      return(sprintf(paste0("stopped short: its %d-token prompt left about %d tokens for the ",
                            "reply in '%s''s %d-token context window"),
                     prompt, max(room, 0L), as_chr1(model), as.integer(info$context_window)))
    }
    return(sprintf("stopped at the %d-token reply limit (raise %s)",
                   as.integer(min(cap, info$max_output)), knob))
  }
  err <- as_chr1(result$error, "")
  if (nzchar(err)) sprintf("failed (%s)", substr(err, 1L, 200L)) else "failed"
}

#' One claims call over one batch of studies.
#'
#' Returns the rows and how the call went: "ok" (which may be no claims at
#' all), "cut_off" (the reply stopped at the limit and the JSON never closed) or
#' "failed". The caller has to tell those apart, because a batch the model
#' found nothing in and a batch whose answer was lost look identical as rows.
#' `shown` is the study numbers in the batch, which is what its claims may cite.
#' @noRd
claims_batch <- function(block, question, client, spec, trace, shown = NULL) {
  if (!trace_can_call(trace)) return(list(rows = NULL, status = "not_sent"))
  res <- gr_call_json(client, list(
    list(role = "system", content = .gr_prompts$claims_system),
    list(role = "user", content = paste0("Review question: ", question)),
    list(role = "user", content = paste0("<studies>\n", paste(block, collapse = "\n\n"),
                                         "\n</studies>"))
  ), schema = .gr_claims_schema, schema_name = "claims", model = spec$model,
     max_output = spec$max_answer_tokens, temperature = spec$temperature,
     trace = trace, label = "claims.draw")
  if (!isTRUE(res$ok)) {
    return(list(rows = NULL, status = if (reply_cut_off(res$result)) "cut_off" else "failed"))
  }
  rows <- claim_rows(json_field(res$value, "claims", scalar = FALSE))
  if (!is.null(rows) && !is.null(shown)) rows$.shown <- rep(list(as.integer(shown)), nrow(rows))
  list(rows = rows, status = "ok")
}

#' Normalise whatever jsonlite made of the reply into a flat frame.
#'
#' `simplifyVector = TRUE` is not type-stable for an array of arrays: with equal
#' lengths it produces a MATRIX, with unequal lengths a list, and with a single
#' element a bare vector. A caller that assumes any one of those is broken by the
#' other two, and the failure is silent -- the ids come out transposed or
#' recycled rather than absent.
#' @noRd
as_id_list <- function(x, n) {
  out <- rep(list(integer(0)), n)
  if (is.null(x) || !n) return(out)
  pick <- function(v) {
    v <- suppressWarnings(as.integer(unlist(v, use.names = FALSE)))
    v <- v[!is.na(v)]
    unique(v)
  }
  if (is.matrix(x)) {
    for (i in seq_len(min(n, nrow(x)))) out[[i]] <- pick(x[i, ])
    return(out)
  }
  if (is.list(x)) {
    for (i in seq_len(min(n, length(x)))) out[[i]] <- pick(x[[i]])
    return(out)
  }
  # One id per claim, arriving as a plain vector.
  v <- suppressWarnings(as.integer(x))
  for (i in seq_len(min(n, length(v)))) out[[i]] <- if (is.na(v[i])) integer(0) else v[i]
  out
}

#' @noRd
claim_rows <- function(x) {
  # Exact reads throughout: this is a model's reply, and `$` let `claim_draft`
  # answer for `claim` and `headings` answer for `heading` -- a key the schema
  # never defined producing a real row.
  fx <- function(o, nm) o[[nm, exact = TRUE]]
  if (is.null(x)) return(NULL)
  # A single claim can arrive as a bare named list rather than a one-row frame.
  if (!is.data.frame(x) && is.list(x) && !is.null(names(x))) x <- list(x)
  # Anything else the reply might be. A JSON array of strings parses to a list
  # of CHARACTERS, and `e$claim` on a character vector is an error, not a miss:
  # `$ operator is invalid for atomic vectors` came out of gr_claims() as a crash
  # rather than as "no claims came back". A bare vector reached the frame branch
  # and died in rep(NA, nrow(x)) on a NULL nrow.
  if (!is.data.frame(x) && !is.list(x)) return(NULL)
  if (is.list(x) && !is.data.frame(x)) {
    x <- x[vapply(x, function(e) is.list(e) && !is.null(names(e)), logical(1))]
    if (!length(x)) return(NULL)
    flat <- lapply(x, function(e) list(
      claim = as_chr1(fx(e, "claim")), kind = as_chr1(fx(e, "kind"), "finding"),
      moderator = as_chr1(fx(e, "moderator"), NA_character_), scope = as_chr1(fx(e, "scope"), NA_character_),
      supported_by = list(as_id_list(list(fx(e, "supported_by")), 1L)[[1]]),
      contradicted_by = list(as_id_list(list(fx(e, "contradicted_by")), 1L)[[1]])))
    x <- do.call(rbind, lapply(flat, function(e) data.frame(
      claim = fx(e, "claim"), kind = fx(e, "kind"), moderator = fx(e, "moderator"), scope = fx(e, "scope"),
      stringsAsFactors = FALSE)))
    sup <- lapply(flat, function(e) fx(e, "supported_by")[[1]])
    con <- lapply(flat, function(e) fx(e, "contradicted_by")[[1]])
  } else {
    n <- nrow(x)
    sup <- as_id_list(fx(x, "supported_by"), n)
    con <- as_id_list(fx(x, "contradicted_by"), n)
    # `%||%` on `claim` as well as the rest: a reply in which NO object carries
    # it gave vapply() a NULL, which is character(0), and data.frame() then died
    # with "arguments imply differing number of rows: 0, 2" instead of warning
    # that no claims came back.
    x <- data.frame(claim = vapply(fx(x, "claim") %||% rep(NA, n), as_chr1, character(1),
                                   USE.NAMES = FALSE),
                    kind = vapply(fx(x, "kind") %||% rep("finding", n), as_chr1, character(1),
                                  USE.NAMES = FALSE),
                    moderator = vapply(fx(x, "moderator") %||% rep(NA, n), as_chr1, character(1),
                                       USE.NAMES = FALSE),
                    scope = vapply(fx(x, "scope") %||% rep(NA, n), as_chr1, character(1),
                                   USE.NAMES = FALSE),
                    stringsAsFactors = FALSE)
  }
  if (is.null(x) || !nrow(x)) return(NULL)
  x$moderator[!nzchar(x$moderator)] <- NA_character_
  x$scope[!nzchar(x$scope)] <- NA_character_
  x$.support <- sup
  x$.contradict <- con
  # A claim with no text is not a claim. It used to survive to claims_verify(),
  # which drops it only if it also has no supporting study -- so an entry that
  # was all ids and no sentence became a row in `$claims` with an empty claim,
  # and gr_outline() then wrote a section arguing it. outline_rows() has always
  # dropped blank headings; this is the same rule one file over.
  x <- x[nzchar(trimws(fx(x, "claim"))), , drop = FALSE]
  if (!nrow(x)) return(NULL)
  x
}

#' Verify a claim against the table it says it rests on.
#'
#' Four rules, each dropping or clearing rather than repairing:
#'
#'   * A study number that is not in the table is removed. This is the same check
#'     `cited_ids()` makes on finished prose, one link earlier -- and it is the
#'     one that matters most, because everything downstream trusts these numbers.
#'     So is one that IS in the table but was not in the batch the claim came
#'     from (`.shown`, when the rows carry it): the model never saw that study,
#'     so naming it is the same fabrication with a number that happens to exist.
#'   * A claim left with no supporting study is dropped. A claim attached to
#'     nothing is an opinion.
#'   * A study cannot both support and contradict one claim, so it is removed
#'     from the contradicting side and noted.
#'   * A `moderator` naming a column the table does not have is cleared. The
#'     claim may still be sound; the EXPLANATION was invented, and an invented
#'     explanation for a real disagreement is the most convincing kind of error
#'     this layer can make. So is a moderator that names a real column which
#'     does not tell the claim's two sides apart; see moderator_split().
#' @noRd
claims_verify <- function(got, used, cols = study_fields(used)) {
  ids <- used$study
  drops <- list()
  note <- rep(NA_character_, nrow(got))
  add <- function(claim, reason, detail) {
    drops[[length(drops) + 1L]] <<- data.frame(claim = claim, reason = reason, detail = detail,
                                               stringsAsFactors = FALSE)
  }
  keep <- rep(TRUE, nrow(got))
  for (i in seq_len(nrow(got))) {
    sup <- got$.support[[i]]; con <- got$.contradict[[i]]
    bad <- setdiff(c(sup, con), ids)
    if (length(bad)) {
      add(got$claim[i], "study number not in the table", paste(sort(bad), collapse = ", "))
      note[i] <- sprintf("dropped study number(s) %s", paste(sort(bad), collapse = ", "))
    }
    sup <- intersect(sup, ids); con <- intersect(con, ids)
    # Checked against the whole table alone, a claim from batch 2 naming a study
    # only batch 1 was shown was kept as support, with nothing in `$dropped`.
    shown <- if (is.null(got[[".shown"]])) NULL else got[[".shown"]][[i]]
    unseen <- if (is.null(shown)) integer(0) else setdiff(c(sup, con), shown)
    if (length(unseen)) {
      add(got$claim[i], "study number not shown to the batch that wrote the claim",
          paste(sort(unseen), collapse = ", "))
      note[i] <- paste(stats::na.omit(c(note[i], sprintf(
        "dropped study number(s) %s, not shown to its batch", paste(sort(unseen), collapse = ", ")))),
        collapse = "; ")
      sup <- setdiff(sup, unseen); con <- setdiff(con, unseen)
    }
    both <- intersect(sup, con)
    if (length(both)) {
      con <- setdiff(con, both)
      note[i] <- paste(stats::na.omit(c(note[i], sprintf(
        "study %s listed on both sides; kept as supporting", paste(both, collapse = ", ")))),
        collapse = "; ")
    }
    if (!length(sup)) {
      add(got$claim[i], "no supporting study left after verification", "")
      keep[i] <- FALSE
      next
    }
    if (!is.na(got$moderator[i]) && !got$moderator[i] %in% cols) {
      add(got$claim[i], "moderator is not a column in the table", got$moderator[i])
      note[i] <- paste(stats::na.omit(c(note[i], sprintf("cleared moderator '%s'", got$moderator[i]))),
                       collapse = "; ")
      got$moderator[i] <- NA_character_
    }
    if (!got$kind[i] %in% .gr_claim_kinds) got$kind[i] <- "finding"
    got$.support[[i]] <- sort(sup); got$.contradict[[i]] <- sort(con)
  }
  got$note <- note
  got[[".shown"]] <- NULL
  # Existing is not explaining. A moderator naming `design` when every study is
  # an RCT was kept, printed to the writer as "distinguished by: design", and
  # took the claim out of gr_gaps()' unexplained disagreements -- while the same
  # gr_gaps() reported that design never varies.
  split <- claims_moderators(got[keep, , drop = FALSE], used)
  list(claims = split$claims,
       dropped = rbind(if (length(drops)) do.call(rbind, drops) else empty_dropped(),
                       split$dropped))
}

#' Values that say a study did not report the field.
#' @noRd
.gr_unreported <- c("", "na", "n/a", "not reported")

#' Why a moderator column does NOT tell a claim's two sides apart, or NA if it does.
#'
#' The schema asks for "the field that distinguishes the supporting studies from
#' the contradicting ones", and that is a count: the values on one side must not
#' turn up on the other. Numbers must be separable by a threshold -- every
#' supporting value above every contradicting one, or below -- because two sets
#' of sample sizes are always different sets and that explains nothing. Checked
#' only on a contested claim; with nothing on the other side there is nothing
#' to separate, and nothing to count.
#'
#' Strict on purpose. A column that separates most of the studies is cleared,
#' and recorded in `$dropped`: a claim shown as an unexplained disagreement
#' that was mostly explained costs a sentence, while an explanation the table
#' does not bear out is exactly the error this layer exists to stop.
#' @noRd
moderator_split <- function(used, col, sup, con, vals = moderator_values(used, col)) {
  if (!length(con) || !length(sup)) return(NA_character_)
  if (is.null(vals)) return("it is not a column in the table")
  side <- function(ids) {
    x <- vals[match(ids, used$study)]
    x[!is.na(x)]
  }
  a <- side(sup); b <- side(con)
  if (!length(a) || !length(b)) {
    return(sprintf("the table does not report it for any %s study",
                   if (!length(a)) "supporting" else "contradicting"))
  }
  na <- suppressWarnings(as.numeric(a)); nb <- suppressWarnings(as.numeric(b))
  if (!anyNA(na) && !anyNA(nb)) {
    if (max(na) < min(nb) || max(nb) < min(na)) return(NA_character_)
    return(sprintf("its values overlap (%s to %s supporting, %s to %s contradicting)",
                   format(min(na)), format(max(na)), format(min(nb)), format(max(nb))))
  }
  both <- intersect(a, b)
  if (length(both)) {
    return(sprintf("studies on both sides report '%s'", both[1]))
  }
  NA_character_
}

#' A moderator column as moderator_split() compares it: numbers as they are,
#' text trimmed and lower-cased, and "not reported" in any spelling as NA.
#' NULL for a column the table does not have.
#' @noRd
moderator_values <- function(used, col) {
  if (!col %in% names(used)) return(NULL)
  v <- used[[col]]
  if (is.numeric(v)) return(v)
  v <- lower_text(trimws(as.character(v)))
  v[v %in% .gr_unreported] <- NA_character_
  v
}

#' Clear every moderator that does not separate its claim's two sides.
#'
#' For claims whose sides changed after claims_verify() looked at them, which
#' is what a reconcile merge does. Returns the claims and the `$dropped` rows.
#' @noRd
claims_moderators <- function(claims, used) {
  drops <- list()
  # Each column normalised once, not once per claim naming it.
  seen <- list()
  for (i in seq_len(nrow(claims))) {
    m <- claims$moderator[i]
    if (is.na(m)) next
    if (!m %in% names(seen)) seen[m] <- list(moderator_values(used, m))
    why <- moderator_split(used, m, claims$.support[[i]], claims$.contradict[[i]],
                           vals = seen[[m]])
    if (is.na(why)) next
    drops[[length(drops) + 1L]] <- data.frame(
      claim = claims$claim[i],
      reason = "moderator does not separate the supporting from the contradicting studies",
      detail = sprintf("%s: %s", m, why), stringsAsFactors = FALSE)
    claims$note[i] <- paste(stats::na.omit(c(claims$note[i], sprintf(
      "cleared moderator '%s': %s", m, why))), collapse = "; ")
    claims$moderator[i] <- NA_character_
  }
  list(claims = claims, dropped = if (length(drops)) do.call(rbind, drops) else empty_dropped())
}

#' Group claims that say the same thing, across batches.
#'
#' The model is shown the claim TEXTS only -- no studies, no numbers to cite --
#' so it cannot invent a claim here, and every study a merged claim names was
#' verified before this ran. What it CAN do is group badly: a group's support
#' is the union of its members', under one member's wording, so two claims
#' that say different things grouped together list each other's studies as
#' support. Nothing here can check meaning, so the merged claim's `note` names
#' every other wording it absorbed, and a group mixing kinds is warned about.
#' A claim the reply fails to place keeps its own group, because dropping one
#' silently is the failure this whole file is built to avoid.
#'
#' Whenever the claims come back unmerged for a reason other than "nothing to
#' merge", the result carries `attr(, "unmerged") = TRUE`, which gr_claims()
#' records on the object.
#' @noRd
claims_reconcile <- function(claims, question, client, spec, trace) {
  n <- nrow(claims)
  if (n < 2L) return(reindex_claims(claims))
  unmerged <- function(why, class = "gr_claims_unmerged") {
    gr_warn(sprintf(paste0("Claims from different batches were not merged: %s, so one finding can ",
                           "appear as more than one claim."), why),
            class = class)
    out <- reindex_claims(claims)
    attr(out, "unmerged") <- TRUE
    out
  }
  if (!trace_can_call(trace)) {
    return(unmerged(sprintf("the run reached its %s", cap_name(trace)), class = "gr_claims_capped"))
  }
  listed <- paste(sprintf("%d. %s", seq_len(n), claims$claim), collapse = "\n")
  # Sized by the claims, as the draw is sized by its studies: the reply names
  # every claim once, so a fixed limit cut off the reply for a large corpus and
  # every claim came back unmerged. gr_call() clamps it to what the model can emit.
  cap <- max(as.integer(spec$max_answer_tokens), reconcile_reply_tokens(n))
  msgs <- list(
    list(role = "system", content = .gr_prompts$reconcile_system),
    list(role = "user", content = paste0("Review question: ", question)),
    list(role = "user", content = paste0("<claims>\n", listed, "\n</claims>")))
  res <- gr_call_json(client, msgs, schema = .gr_reconcile_schema, schema_name = "claim_groups",
                      model = spec$model, max_output = cap, temperature = spec$temperature,
                      trace = trace, label = "claims.reconcile")
  if (!isTRUE(res$ok)) {
    # Said, as the capped case above is. Batches sized by their reply make this
    # pass the normal route for a large corpus, and its reply names every claim.
    return(unmerged(sprintf("the reconcile call %s", call_failure(res$result, msgs, spec$model, cap,
                                                                  "`max_claim_tokens`"))))
  }

  raw <- json_field(res$value, "groups", scalar = FALSE)
  # A flat array, `{"groups":[1,2,3]}`, is not one group of three: it is a reply
  # that ignored the schema (gr_call_json() does not enforce it). Read as one
  # group it merged every claim into the longest one's wording and handed it
  # every study, opposite findings included. Equal-length groups arrive as a
  # matrix and ragged ones as a list, so a bare vector is only ever this.
  if (!is.null(raw) && !is.list(raw) && !is.matrix(raw)) {
    return(unmerged("the reconcile reply was a flat list of numbers, not a list of groups"))
  }
  g <- as_id_list(raw, if (is.matrix(raw)) nrow(raw) else length(raw))
  # Defence in depth, not load-bearing: `claims[c(1, 99), ]` yields a row of NAs
  # rather than an error, and the merge below happens to absorb it -- nchar(NA)
  # loses to any real claim text and NULL support unions to nothing. Mutating
  # this line away changes no output today. It stays because the next change to
  # the merge would not be so lucky, and because a reply naming a claim that was
  # never listed should not reach that code at all.
  g <- lapply(g, function(v) v[v >= 1L & v <= n])
  g <- g[vapply(g, length, integer(1)) > 0L]
  # A claim in two groups would be merged twice and counted twice.
  seen <- integer(0)
  clean <- list()
  for (v in g) {
    v <- setdiff(v, seen)
    if (!length(v)) next
    seen <- c(seen, v)
    clean[[length(clean) + 1L]] <- v
  }
  missed <- setdiff(seq_len(n), seen)
  if (length(missed)) {
    for (i in missed) clean[[length(clean) + 1L]] <- i
    gr_msg(sprintf("%d claim(s) were not placed by the reconcile pass and kept their own group.",
                   length(missed)))
  }
  # A finding, a method and a gap cannot "say the same thing"; a group mixing
  # them is the model grouping by topic, and the merged claim keeps one kind.
  mixed <- vapply(clean, function(v) length(unique(claims$kind[v])) > 1L, logical(1))
  if (any(mixed)) {
    gr_warn(sprintf(paste0("The reconcile pass merged claims of different kinds into %d claim(s); ",
                           "a merged claim keeps one wording and every member's studies. Their ",
                           "`note` names the wordings merged in."), sum(mixed)),
            class = "gr_claims_merged")
  }
  merged <- lapply(clean, function(v) {
    first <- claims[v[1], , drop = FALSE]
    # The longest text, because a claim stated over more studies is usually
    # stated more fully, and the support is the union either way.
    keep <- which.max(nchar(claims$claim[v]))
    first$claim <- claims$claim[v][keep]
    # Every other wording, so a merge can be read back. Without it one claim
    # absorbing an opposite one ("did not improve symptoms" taking in "improved
    # symptoms") left the first claim's studies supporting the second's words,
    # with nothing anywhere to show that a merge had happened.
    norm <- function(s) lower_text(gsub("[[:space:]]+", " ", trimws(s)))
    others <- unique(claims$claim[v][-keep])
    others <- others[norm(others) != norm(first$claim)]
    absorbed <- if (length(others)) sprintf("merged with: %s", paste(sprintf(
      "'%s'", others), collapse = "; ")) else NULL
    mods <- stats::na.omit(claims$moderator[v])
    first$moderator <- if (length(mods)) mods[1] else NA_character_
    sc <- stats::na.omit(claims$scope[v])
    first$scope <- if (length(sc)) sc[which.max(nchar(sc))] else NA_character_
    sup <- sort(unique(unlist(claims$.support[v], use.names = FALSE)))
    con <- sort(unique(unlist(claims$.contradict[v], use.names = FALSE)))
    # A study that supported one member of this group and contradicted another
    # is a DISAGREEMENT, and the setdiff below files it on the supporting side.
    # Silently: `n_contradict` went 1 -> 0 with nothing in `note`, so a merge
    # manufactured consensus that no study agreed to. The study still belongs on
    # the supporting side -- it does support the merged wording -- but the reader
    # has to be told, because this is exactly the kind of loss the claims layer
    # exists to make visible.
    flipped <- intersect(con, sup)
    nts <- c(stats::na.omit(claims$note[v]), absorbed)
    if (length(flipped)) {
      nts <- c(nts, sprintf(paste0("study %s contradicted a claim merged into this one; ",
                                   "kept as supporting"), paste(flipped, collapse = ", ")))
    }
    first$note <- if (length(nts)) paste(unique(nts), collapse = "; ") else NA_character_
    first$.support <- list(sup)
    first$.contradict <- list(setdiff(con, sup))
    first
  })
  reindex_claims(do.call(rbind, merged))
}

#' @noRd
reindex_claims <- function(claims) {
  claims$claim_id <- seq_len(nrow(claims))
  rownames(claims) <- NULL
  claims
}

#' @noRd
empty_claim_rows <- function() {
  out <- data.frame(claim = character(0), kind = character(0), moderator = character(0),
                    scope = character(0), note = character(0), claim_id = integer(0),
                    stringsAsFactors = FALSE)
  out$.support <- list(); out$.contradict <- list()
  out
}

#' @noRd
empty_dropped <- function() {
  data.frame(claim = character(0), reason = character(0), detail = character(0),
             stringsAsFactors = FALSE)
}

#' `lost` is the study numbers whose batch contributed nothing -- cut off at
#' the reply limit, failed, or not sent -- and `partial` says whether there
#' are any. `hidden` is the columns withheld from the model as bibliographic,
#' which gr_gaps() withholds too; `unmerged` says the reconcile pass could not
#' run over claims that needed it.
#' @noRd
new_claims <- function(claims, used, question, dropped, trace, lost = integer(0),
                       hidden = character(0), unmerged = FALSE) {
  claims <- reindex_claims(claims)
  support <- claim_support_table(claims)
  wide <- data.frame(
    claim_id = claims$claim_id, claim = claims$claim, kind = claims$kind,
    moderator = claims$moderator, scope = claims$scope,
    n_support = vapply(claims$.support, length, integer(1)),
    n_contradict = vapply(claims$.contradict, length, integer(1)),
    note = claims$note, stringsAsFactors = FALSE)
  rownames(wide) <- NULL
  structure(list(claims = wide, support = support, studies = used, question = question,
                 dropped = dropped, partial = length(lost) > 0L, lost = as.integer(lost),
                 unmerged = isTRUE(unmerged), hidden = as.character(hidden),
                 trace = trace), class = "gr_claims")
}

#' Long form, the shape `$citations` already uses: one row per claim per study.
#' @noRd
claim_support_table <- function(claims) {
  if (!nrow(claims)) {
    return(data.frame(claim_id = integer(0), study = integer(0), role = character(0),
                      stringsAsFactors = FALSE))
  }
  parts <- list()
  for (i in seq_len(nrow(claims))) {
    for (role in c("supports", "contradicts")) {
      v <- if (identical(role, "supports")) claims$.support[[i]] else claims$.contradict[[i]]
      if (!length(v)) next
      parts[[length(parts) + 1L]] <- data.frame(claim_id = claims$claim_id[i], study = v,
                                                role = role, stringsAsFactors = FALSE)
    }
  }
  out <- if (length(parts)) do.call(rbind, parts) else
    data.frame(claim_id = integer(0), study = integer(0), role = character(0),
               stringsAsFactors = FALSE)
  rownames(out) <- NULL
  out
}

#' @export
print.gr_claims <- function(x, ...) {
  cat(sprintf("<gr_claims> %d claim(s) over %d study/studies\n",
              nrow(x$claims), nrow(x$studies)))
  if (nrow(x$claims)) {
    kinds <- table(x$claims$kind)
    cat(sprintf("  kinds: %s\n", paste(sprintf("%s %d", names(kinds), as.integer(kinds)),
                                       collapse = ", ")))
    dis <- sum(x$claims$n_contradict > 0L)
    cat(sprintf("  %d contested, %d of those with a moderator named\n", dis,
                sum(x$claims$n_contradict > 0L & !is.na(x$claims$moderator))))
    single <- sum(x$claims$n_support == 1L)
    if (single) cat(sprintf("  %d resting on a single study\n", single))
    for (i in seq_len(min(5L, nrow(x$claims)))) {
      cat(sprintf("  [%d] %s\n", x$claims$claim_id[i],
                  substr(x$claims$claim[i], 1, 96)))
    }
    if (nrow(x$claims) > 5L) cat(sprintf("  ... and %d more\n", nrow(x$claims) - 5L))
  }
  if (nrow(x$dropped)) {
    cat(sprintf("  %d dropped in verification; see $dropped\n", nrow(x$dropped)))
  }
  # `%||%` for a claims table saved before the field existed.
  if (length(x$lost %||% integer(0))) {
    cat(sprintf(paste0("  PARTIAL: %d of %d studies contributed nothing, their batch cut off, ",
                       "failed or not sent; see $lost\n"),
                length(x$lost), nrow(x$studies)))
  }
  if (isTRUE(x$unmerged)) {
    cat("  UNMERGED: claims from different batches were not reconciled, so one finding may\n")
    cat("  appear as more than one claim\n")
  }
  invisible(x)
}

# ---------------------------------------------------------------------------
# Ordering for emphasis, and structure from the claims.
# ---------------------------------------------------------------------------

#' How much of a section a study is worth, for emphasis only.
#'
#' NOT a quality score, and deliberately NOT a design hierarchy. Ranking designs
#' would mean asserting that a cohort study beats a qualitative one, which is a
#' methodological claim this package has no standing to make on its own -- and
#' adding up per-item scores to get a total is the thing Cochrane says plainly is
#' discouraged. Design earns its place as a GROUPING variable instead: it is what
#' distinguishes the studies on each side of a disagreement, and what
#' [gr_gaps()] crosstabs.
#'
#' What is left is what the table can honestly support: how many units were
#' analysed, and how completely the row was filled with evidence that verified.
#' A principled weighting waits on `gr_appraise()` and a named instrument.
#' @noRd
study_weight <- function(used) {
  # numeric_token(), not as.numeric(): an `n` column reading "900 participants"
  # is not missing data, and as.numeric() made it NA. NA then scored 0 -- the
  # bottom of the scale -- so a 900-participant study weighed LESS than one with
  # 25, and claim_order() put the small study's claim first.
  n <- n_column(used$n %||% rep(NA, nrow(used)))
  # log1p, because the difference between 20 and 200 participants matters far
  # more than the difference between 2000 and 2180.
  size <- log1p(ifelse(is.na(n) | n < 0, NA_real_, n))
  mx <- suppressWarnings(max(size, na.rm = TRUE))
  size <- if (is.finite(mx) && mx > 0) size / mx else rep(NA_real_, length(size))
  # An `n` nobody reported is UNKNOWN, not zero -- the same distinction the
  # `filled` term below already makes by scoring NA at 0.5 rather than 0. Zero
  # punished a study for a cell the extraction could not fill.
  mid <- if (any(!is.na(size))) mean(size, na.rm = TRUE) else 0
  size[is.na(size)] <- mid
  filled <- suppressWarnings(as.numeric(used$n_filled %||% rep(NA, nrow(used))))
  unver <- suppressWarnings(as.numeric(used$n_unverified %||% rep(0, nrow(used))))
  unver[is.na(unver)] <- 0
  # A row whose values are there and whose quotes were found is worth more than
  # one held together by misses. NA filled counts as neither good nor bad.
  ok <- ifelse(is.na(filled) | filled <= 0, 0.5,
               pmax(0, (filled - unver)) / pmax(filled, 1))
  stats::setNames(0.5 + 0.5 * size + 0.5 * ok, as.character(used$study))
}

#' Order claims by how much of the section they have earned.
#'
#' Breadth first -- a claim resting on nine studies says more about a literature
#' than one resting on one -- then the weight of the studies behind it. Contested
#' claims count their contradicting studies too: a disagreement across six
#' studies is more of the story than an agreement across two.
#' @noRd
claim_order <- function(claims, support, weights) {
  if (!nrow(claims)) return(integer(0))
  breadth <- claims$n_support + claims$n_contradict
  # A study with no weight is UNKNOWN, not weightless: it takes the mean of the
  # weights that ARE known across the table -- or 1, the middle of
  # study_weight()'s 0.5-1.5 scale, if none is. Imputing per claim instead gave a
  # claim whose every weight was missing the mean of nothing, NaN, which
  # sum(na.rm = TRUE) then made 0: the unknown-becomes-zero pattern this file
  # documents elsewhere. Unreachable today, because claims_verify() drops claims
  # citing studies outside the table first; kept honest because that is one
  # refactor away from being live.
  known <- weights[!is.na(weights)]
  neutral <- if (length(known)) mean(known) else 1
  mass <- vapply(claims$claim_id, function(id) {
    s <- support$study[support$claim_id == id]
    if (!length(s)) return(0)
    w <- unname(weights[as.character(s)])
    w[is.na(w)] <- neutral
    sum(w)
  }, numeric(1))
  order(-breadth, -mass, claims$claim_id)
}

#' `rationale` is required and nullable rather than optional, for the reason
#' given at .gr_claims_schema: strict mode refuses anything else.
#' @noRd
.gr_outline_schema <- list(
  type = "object", additionalProperties = FALSE,
  required = list("sections"),
  properties = list(sections = list(
    type = "array",
    items = list(
      type = "object", additionalProperties = FALSE,
      required = list("heading", "brief", "claims", "rationale"),
      properties = list(
        heading = list(type = "string",
                       description = "The section heading, as it will be printed."),
        brief = list(type = "string",
                     description = "One line saying what this section must cover."),
        claims = list(type = "array", items = list(type = "integer"),
                      description = paste0("The claim numbers this section covers. Every claim ",
                                           "goes in exactly one section.")),
        rationale = list(type = c("string", "null"),
                         description = "One line on why these claims belong together."))))))

#' Derive a review's sections from its claims
#'
#' [gr_synthesise()] takes an `outline` fixed before the reading, which makes the
#' review's structure the author's hypothesis rather than a finding. It is often
#' the strongest thing a review has to say: that a literature splits into three
#' incompatible operationalisations, say, and that the argument about effect size
#' is really an argument about measurement. This derives the structure from the
#' claims instead, and hands it back for you to accept or replace.
#'
#' @section What is verified:
#' Every claim is assigned to exactly one section. A claim number that does not
#' exist is dropped; a claim assigned twice keeps its first section; a claim the
#' reply never placed is put in the closing section rather than lost, with a
#' message saying so. A section left with no claims is removed, except the
#' closing one, which is allowed to be empty because it is where [gr_gaps()]
#' writes and gaps are not claims.
#'
#' @param claims A [gr_claims()] result.
#' @param question The review question. Taken from `claims` if omitted.
#' @param client A [gr_client()]. One is built from `model` if omitted.
#' @param model,temperature Passed to the model call.
#' @param max_sections Upper bound on sections, before the closing one.
#' @param closing The heading of the closing section, which receives gap claims
#'   and anything unplaced. `NULL` for no closing section.
#' @param trace A [gr_trace()] to record into.
#' @param max_outline_tokens The reply limit for the outline call. The reply
#'   names every claim and writes a heading, brief and rationale per section,
#'   so by default it grows with both: `400 + 8` per claim `+ 60` per section,
#'   never below 1200, plus 4000 on a model registered as a reasoning model,
#'   which spends part of the same limit before it writes. Clamped to what the
#'   model can emit. A reply cut off at it puts every claim in one section,
#'   with a warning.
#' @return A named character vector shaped exactly like the `outline` argument of
#'   [gr_synthesise()] (headings as names, briefs as values), carrying
#'   `attr(, "claims")` (a `section`/`claim_id` frame), `attr(, "rationale")` and
#'   `attr(, "closing")`.
#' @seealso [gr_claims()], [gr_synthesise()], [gr_gaps()]
#' @export
#' @examples
#' tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok",
#'                   duplicate_of = NA_character_, n_filled = 1L, n_unverified = 0L,
#'                   conflicts = NA_character_, finding = c("supports", "contradicts"),
#'                   stringsAsFactors = FALSE)
#' cl <- gr_mock_client(function(messages, params) {
#'   if (grepl("<claims>", paste(unlist(messages), collapse = " "), fixed = TRUE)) {
#'     return(paste0('{"sections":[{"heading":"What it finds","brief":"the claims",',
#'                   '"claims":[1],"rationale":"one topic"}]}'))
#'   }
#'   paste0('{"claims":[{"claim":"It works in trials.","kind":"finding",',
#'          '"supported_by":[1],"contradicted_by":[2],"moderator":null,"scope":null}]}')
#' })
#' cm <- gr_claims(tab, question = "Does it work?", client = cl)
#' o <- gr_outline(cm, client = cl)
#' o
#' attr(o, "claims")
gr_outline <- function(claims, question = NULL, client = NULL, model = NULL,
                       temperature = NULL, max_sections = 6L,
                       closing = "What is missing", trace = NULL,
                       max_outline_tokens = NULL) {
  if (!inherits(claims, "gr_claims")) {
    gr_abort("`claims` must come from gr_claims().", class = "gr_bad_claims")
  }
  if (!nrow(claims$claims)) {
    gr_abort("That gr_claims has no claims, so there is no structure to derive from it.",
             class = "gr_no_claims")
  }
  question <- as_chr1(question %||% claims$question)
  if (!is_nonblank(question)) gr_abort("`question` must be a non-empty string.")
  client <- client %||% gr_client(model = model %||% gr_options("model"))
  # The model first, because the default reply limit depends on it.
  spec <- resolve_read_model(gr_read_spec("stuff", model = model, temperature = temperature),
                             client)
  cw <- claims$claims
  # A fixed 1200 tokens, with no way to raise it, cut off the outline from about
  # 300 claims on an ordinary model and from 20 on a reasoning one, which spends
  # the same limit thinking first. Every claim then went to one section, and the
  # warning's advice could not help: the same claims ask for the same reply.
  info <- gr_model_info(spec$model)
  auto <- outline_reply_tokens(nrow(cw), max_sections, info)
  max_outline_tokens <- if (is.null(max_outline_tokens)) auto else
    clamp_warn(na_default(max_outline_tokens, auto, "max_outline_tokens"), 16, 1e6,
               "max_outline_tokens")
  # Clamped to what the model can emit, as gr_call() would, so that the limit a
  # warning names is the limit the request carried.
  spec$max_answer_tokens <- min(as.integer(max_outline_tokens), as.integer(info$max_output))
  trace <- trace %||% claims$trace %||% gr_trace(meta = list(stage = "outline"))

  listed <- paste(sprintf("%d. [%s] %s%s", cw$claim_id, cw$kind, cw$claim,
                          ifelse(is.na(cw$moderator), "",
                                 sprintf(" (contested; distinguished by %s)", cw$moderator))),
                  collapse = "\n")
  msgs <- list(
    list(role = "system", content = .gr_prompts$outline_system),
    list(role = "user", content = paste0("Review question: ", question)),
    list(role = "user", content = paste0("<claims>\n", listed, "\n</claims>")),
    list(role = "user", content = sprintf("Use at most %d sections.", as.integer(max_sections))))
  capped <- !trace_can_call(trace)
  res <- if (!capped) {
    gr_call_json(client, msgs, schema = .gr_outline_schema, schema_name = "outline",
                 model = spec$model, max_output = spec$max_answer_tokens,
                 temperature = spec$temperature, trace = trace, label = "outline.derive")
  } else list(ok = FALSE, value = NULL)

  secs <- if (isTRUE(res$ok)) outline_rows(json_field(res$value, "sections", scalar = FALSE)) else NULL
  if (is.null(secs) || !nrow(secs)) {
    # A reply cut off at the limit never closes its JSON, so it arrives here
    # like any failed call. Said apart, because "try again" cannot help: the
    # same claims ask for the same reply, and it stops in the same place.
    cut <- !capped && reply_cut_off(res$result)
    gr_warn(if (capped) sprintf(paste0("The outline was not requested: the run had reached its %s. ",
                                       "Every claim was put in one section. Raise the limit, or ",
                                       "pass an `outline` to gr_synthesise() yourself."),
                                cap_name(trace))
            else if (cut) sprintf(paste0("The outline reply %s before it finished, so every claim was ",
                                         "put in one section. Lower `max_sections`, or pass an ",
                                         "`outline` to gr_synthesise() yourself."),
                                  call_failure(res$result, msgs, spec$model, spec$max_answer_tokens,
                                               "`max_outline_tokens`"))
            else paste0("The outline call did not return usable sections, so every claim was put ",
                        "in one section. Pass an `outline` to gr_synthesise() yourself, or try again."),
            class = if (cut) c("gr_outline_truncated", "gr_outline_failed") else "gr_outline_failed")
    secs <- data.frame(heading = "Findings", brief = "What the evidence supports",
                       rationale = NA_character_, stringsAsFactors = FALSE)
    secs$.claims <- list(cw$claim_id)
  }
  finish_outline(secs, cw, closing, max_sections)
}

#' The default reply limit for an outline of `n` claims, on the model `info`
#' describes.
#'
#' The reply lists every claim number and writes a heading, a brief and a
#' rationale per section. Never below the 1200 it used to be fixed at. A
#' reasoning model spends part of the same limit before it writes a word, and
#' how much is not known in advance; a limit is a ceiling rather than a charge,
#' so the allowance is generous, while a reply cut off costs the whole outline.
#' @noRd
outline_reply_tokens <- function(n, max_sections, info) {
  sections <- suppressWarnings(as.numeric(max_sections))
  if (length(sections) != 1L || !is.finite(sections)) sections <- 6
  base <- max(1200, 400 + 8 * n + 60 * sections)
  as.integer(min(1e6, base + if (isTRUE(info$reasoning)) 4000 else 0))
}

#' @noRd
outline_rows <- function(x) {
  # Exact reads throughout: this is a model's reply, and `$` let `claim_draft`
  # answer for `claim` and `headings` answer for `heading` -- a key the schema
  # never defined producing a real row.
  fx <- function(o, nm) o[[nm, exact = TRUE]]
  if (is.null(x)) return(NULL)
  if (!is.data.frame(x) && is.list(x) && !is.null(names(x))) x <- list(x)
  # Same two shapes claim_rows() guards against: a JSON array of strings, and a
  # bare vector. Both crashed rather than returning "no outline came back".
  if (!is.data.frame(x) && !is.list(x)) return(NULL)
  if (is.list(x) && !is.data.frame(x)) {
    x <- x[vapply(x, function(e) is.list(e) && !is.null(names(e)), logical(1))]
    if (!length(x)) return(NULL)
    ids <- lapply(x, function(e) as_id_list(list(fx(e, "claims")), 1L)[[1]])
    out <- do.call(rbind, lapply(x, function(e) data.frame(
      heading = as_chr1(fx(e, "heading")), brief = as_chr1(fx(e, "brief")),
      rationale = as_chr1(fx(e, "rationale"), NA_character_), stringsAsFactors = FALSE)))
  } else {
    n <- nrow(x)
    ids <- as_id_list(fx(x, "claims"), n)
    out <- data.frame(heading = vapply(fx(x, "heading") %||% rep(NA, n), as_chr1, character(1),
                                       USE.NAMES = FALSE),
                      brief = vapply(fx(x, "brief") %||% rep(NA, n), as_chr1, character(1),
                                     USE.NAMES = FALSE),
                      rationale = vapply(fx(x, "rationale") %||% rep(NA, n), as_chr1, character(1),
                                         USE.NAMES = FALSE),
                      stringsAsFactors = FALSE)
  }
  if (is.null(out) || !nrow(out)) return(NULL)
  out$rationale[!nzchar(out$rationale)] <- NA_character_
  out$.claims <- ids
  out[nzchar(trimws(out$heading)), , drop = FALSE]
}

#' Assign every claim exactly once, and keep the ones nobody placed.
#' @noRd
finish_outline <- function(secs, cw, closing, max_sections) {
  ids <- cw$claim_id
  if (nrow(secs) > max_sections) secs <- secs[seq_len(max_sections), , drop = FALSE]
  seen <- integer(0)
  for (i in seq_len(nrow(secs))) {
    v <- intersect(secs$.claims[[i]], ids)
    # First assignment wins. A claim in two sections is written up twice, and
    # the reader has no way to know it is one claim.
    v <- setdiff(v, seen)
    seen <- c(seen, v)
    secs$.claims[[i]] <- sort(v)
  }
  unplaced <- setdiff(ids, seen)
  gaps_kind <- cw$claim_id[cw$kind == "gap"]

  keep <- vapply(secs$.claims, length, integer(1)) > 0L
  secs <- secs[keep, , drop = FALSE]
  if (!nrow(secs) && !is_nonblank(closing)) {
    # Every section the reply proposed lost all its claims to verification, and
    # there is no closing section to put them in. One section beats none: an
    # empty outline aborts gr_synthesise(), which loses the claims entirely.
    secs <- data.frame(heading = "Findings", brief = "What the evidence supports",
                       rationale = NA_character_, stringsAsFactors = FALSE)
    secs$.claims <- list(sort(ids))
    unplaced <- integer(0)
  }
  if (is_nonblank(closing)) {
    # Gap claims belong here whatever the reply said: a gap is what the
    # literature does not cover, and reading it under "What it finds" inverts it.
    move <- unique(c(unplaced, gaps_kind))
    for (i in seq_len(nrow(secs))) secs$.claims[[i]] <- setdiff(secs$.claims[[i]], gaps_kind)
    tail_row <- data.frame(heading = as_chr1(closing),
                           brief = "The questions this body of work cannot answer, and why",
                           rationale = "Gaps, and anything the structure could not place",
                           stringsAsFactors = FALSE)
    tail_row$.claims <- list(sort(move))
    secs <- secs[vapply(secs$.claims, length, integer(1)) > 0L, , drop = FALSE]
    secs <- rbind(secs, tail_row)
  } else if (length(unplaced)) {
    secs$.claims[[nrow(secs)]] <- sort(c(secs$.claims[[nrow(secs)]], unplaced))
  }
  if (length(unplaced)) {
    gr_msg(sprintf("%d claim(s) were not placed by the outline and went to the closing section.",
                   length(unplaced)))
  }

  # Everything downstream keys the outline by HEADING -- gr_synthesise() writes
  # one section per name and looks its claims up by that name -- so two sections
  # called the same thing wrote the same heading twice and each copy took the
  # union of both claim sets. Three claims came out as five section slots. Merge
  # rather than rename: a reply that proposed "Findings" twice meant one section.
  if (anyDuplicated(secs$heading)) {
    dup <- duplicated(secs$heading)
    for (h in unique(secs$heading[dup])) {
      j <- which(secs$heading == h)
      secs$.claims[[j[1]]] <- sort(unique(unlist(secs$.claims[j], use.names = FALSE)))
    }
    gr_msg(sprintf("%d duplicate section heading(s) in the outline were merged into one.",
                   sum(dup)))
    secs <- secs[!dup, , drop = FALSE]
  }

  map <- do.call(rbind, lapply(seq_len(nrow(secs)), function(i) {
    v <- secs$.claims[[i]]
    if (!length(v)) return(NULL)
    data.frame(section = secs$heading[i], claim_id = v, stringsAsFactors = FALSE)
  }))
  if (is.null(map)) {
    map <- data.frame(section = character(0), claim_id = integer(0), stringsAsFactors = FALSE)
  }
  out <- stats::setNames(secs$brief, secs$heading)
  attr(out, "claims") <- map
  attr(out, "rationale") <- stats::setNames(secs$rationale, secs$heading)
  attr(out, "closing") <- if (is_nonblank(closing)) as_chr1(closing) else NA_character_
  out
}

# ---------------------------------------------------------------------------
# What the corpus does not contain.
# ---------------------------------------------------------------------------

#' What a body of work does not cover
#'
#' Reviews are read for the gap, and a gap a model was asked to notice is an
#' impression. This computes them instead, in R, from the extraction table and
#' the claims: a declared category nobody studied, a dimension with no variation,
#' an empty cell in a crosstab, a claim resting on one study that nobody has
#' tried to replicate, a disagreement nothing in the table explains. Every row is
#' a fact about the corpus that can be checked by counting.
#'
#' No model is called. [gr_synthesise()] takes the result as `gaps =` and hands
#' it to the closing section with an instruction to state these and no others,
#' which is what keeps the written gap list attached to the table.
#'
#' @param claims A [gr_claims()] result.
#' @param extraction The [gr_extract()] result the claims came from. Only its
#'   `$fields` is used, and only to learn which categories were *declared*;
#'   without it a category nobody studied is indistinguishable from one nobody
#'   thought of.
#' @param max_cells Cap on reported empty combinations, which grow as the product
#'   of two fields' sizes.
#' @param min_reported A field missing for more than this fraction of studies is
#'   reported as not reported.
#' @param bib Which columns carry bibliographic identity, as in [gr_claims()]
#'   and [gr_synthesise()]. They are not dimensions of the evidence -- "every
#'   study reports 'Lancet'" is not a gap in a literature -- and a gap line goes
#'   to the writing model, which is never told who wrote a study. The columns
#'   `claims` withheld are left out whatever this says; `bib` adds to them.
#' @return A data frame of class `gr_gaps`: `kind`, `dimension`, `detail`, `n`.
#' @seealso [gr_claims()], [gr_outline()], [gr_synthesise()]
#' @export
#' @examples
#' tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok",
#'                   duplicate_of = NA_character_, n_filled = 1L, n_unverified = 0L,
#'                   conflicts = NA_character_, design = c("cohort", "cohort"),
#'                   stringsAsFactors = FALSE)
#' cl <- gr_mock_client(function(messages, params) paste0(
#'   '{"claims":[{"claim":"One cohort found it.","kind":"finding","supported_by":[1],',
#'   '"contradicted_by":[],"moderator":null,"scope":"one study"}]}'))
#' cm <- gr_claims(tab, question = "Does it work?", client = cl)
#' gr_gaps(cm)
gr_gaps <- function(claims, extraction = NULL, max_cells = 40L, min_reported = 0.5,
                    bib = NULL) {
  if (!inherits(claims, "gr_claims")) {
    gr_abort("`claims` must come from gr_claims().", class = "gr_bad_claims")
  }
  st <- claims$studies
  cw <- claims$claims
  fields <- if (inherits(extraction, "gr_extraction")) extraction$fields else
    if (inherits(extraction, "gr_fields")) extraction else NULL
  # The fields the claims model was shown, by the rule render_studies() uses.
  # Every column but the reserved ones made authors, year and venue dimensions,
  # and "no variation (authors): ... all say 'Smith, J.; Okafor, A.'" went to
  # the closing section as a gap the writer was told to state. `%||%` for a
  # claims table saved before it recorded what it withheld.
  hidden <- union(claims$hidden %||% unlist(bib_columns(st), use.names = FALSE),
                  unlist(bib_columns(st, bib), use.names = FALSE))
  cols <- setdiff(study_fields(st, hide = hidden), .gr_reserved_fields)
  out <- list()
  add <- function(kind, dimension, detail, n) {
    out[[length(out) + 1L]] <<- data.frame(kind = kind, dimension = dimension,
                                          detail = detail, n = as.integer(n),
                                          stringsAsFactors = FALSE)
  }
  vals <- function(col) {
    v <- trimws(as.character(st[[col]]))
    v[!is.na(v) & nzchar(v) & v != "NA"]
  }

  # 1. A category that was declared and nobody studied. Only knowable from the
  #    schema: without it, "no qualitative studies" and "we never asked about
  #    design" look identical.
  enums <- character(0)
  if (!is.null(fields)) {
    for (nm in intersect(names(fields), cols)) {
      f <- fields[[nm]]
      if (!identical(f$type, "enum") || !length(f$values)) next
      enums <- c(enums, nm)
      absent <- setdiff(f$values, vals(nm))
      if (length(absent)) {
        add("declared but unstudied", nm, paste(absent, collapse = "; "), length(absent))
      }
    }
  }

  for (nm in cols) {
    v <- vals(nm)
    # 2. A field every study answers the same way cannot explain a disagreement,
    #    and cannot bound a claim's scope either.
    if (length(v) > 1L && length(unique(v)) == 1L) {
      # "every study reports 'cohort'" was printed when 2 of 10 studies reported
      # it and the other 8 left the cell empty -- `v` drops the missing ones, so
      # the sentence described the reporters as the whole table. A gap report
      # that overstates what the literature says is worse than none.
      add("no variation", nm,
          if (length(v) == nrow(st)) sprintf("every study reports '%s'", unique(v))
          else sprintf("the %d of %d studies that report it all say '%s'",
                       length(v), nrow(st), unique(v)),
          length(v))
    }
    # 3. A field most studies do not report is a gap in the literature's
    #    reporting, which is a finding about the literature.
    miss <- nrow(st) - length(v)
    if (nrow(st) && miss / nrow(st) > min_reported) {
      add("mostly not reported", nm, sprintf("%d of %d studies do not report it",
                                             miss, nrow(st)), miss)
    }
  }

  # 4. Empty cells between two declared dimensions: the combination nobody has
  #    looked at, which is the most actionable kind of gap there is.
  if (length(enums) >= 2L) {
    cells <- 0L
    pairs <- utils::combn(sort(enums), 2L, simplify = FALSE)
    for (pr in pairs) {
      a <- fields[[pr[1]]]$values; b <- fields[[pr[2]]]$values
      # trimws(), because vals() trims and this did not: a cell holding " cohort"
      # counted as studied for rule 2 and as UNSTUDIED here, so the same table
      # produced "every study reports 'cohort'" and "design = 'cohort' with ...
      # unstudied" in one report.
      seen <- unique(paste(trimws(as.character(st[[pr[1]]])),
                           trimws(as.character(st[[pr[2]]])), sep = "\u0001"))
      for (x in a) for (y in b) {
        if (cells >= max_cells) break
        if (!paste(x, y, sep = "\u0001") %in% seen) {
          add("combination unstudied", paste(pr, collapse = " x "),
              sprintf("%s = '%s' with %s = '%s'", pr[1], x, pr[2], y), 0L)
          cells <- cells + 1L
        }
      }
      if (cells >= max_cells) break
    }
  }

  # 5 and 6. Gaps the CLAIMS carry rather than the table.
  if (nrow(cw)) {
    lone <- cw$claim_id[cw$n_support == 1L & cw$n_contradict == 0L]
    for (id in lone) {
      add("unreplicated", sprintf("claim %d", id),
          substr(cw$claim[cw$claim_id == id], 1, 160), 1L)
    }
    open <- cw$claim_id[cw$n_contradict > 0L & is.na(cw$moderator)]
    for (id in open) {
      add("disagreement unexplained", sprintf("claim %d", id),
          substr(cw$claim[cw$claim_id == id], 1, 160),
          cw$n_support[cw$claim_id == id] + cw$n_contradict[cw$claim_id == id])
    }
    for (id in cw$claim_id[cw$kind == "gap"]) {
      add("stated as a gap", sprintf("claim %d", id),
          substr(cw$claim[cw$claim_id == id], 1, 160), cw$n_support[cw$claim_id == id])
    }
  }

  res <- if (length(out)) do.call(rbind, out) else
    data.frame(kind = character(0), dimension = character(0), detail = character(0),
               n = integer(0), stringsAsFactors = FALSE)
  rownames(res) <- NULL
  structure(res, class = c("gr_gaps", "data.frame"),
            studies = nrow(st), had_schema = !is.null(fields))
}

# `[` on a classed data frame keeps the class and loses every other attribute, so
# `g[, c("kind", "dimension")]` was still dispatched to print.gr_gaps() -- which
# then reported "NA study/studies" and "no schema given" about a perfectly good
# object, because the attributes carrying both had been dropped by the subset.
# Selecting columns from a table should give a table.
#' @export
`[.gr_gaps` <- function(x, ...) {
  out <- NextMethod()
  if (is.data.frame(out)) {
    class(out) <- "data.frame"
    attr(out, "studies") <- NULL
    attr(out, "had_schema") <- NULL
  }
  out
}

#' @export
print.gr_gaps <- function(x, ...) {
  cat(sprintf("<gr_gaps> %d gap(s) over %s study/studies\n", nrow(x),
              format(attr(x, "studies") %||% NA)))
  if (!isTRUE(attr(x, "had_schema"))) {
    cat("  no schema given, so a category nobody studied cannot be told from one\n")
    cat("  nobody asked about; pass `extraction =` for those\n")
  }
  if (nrow(x)) {
    k <- table(x$kind)
    for (nm in names(k)) cat(sprintf("  %-26s %d\n", nm, as.integer(k[[nm]])))
  }
  invisible(x)
}

#' The gaps as the lines a section may state.
#' @noRd
render_gaps <- function(gaps) {
  if (!is.data.frame(gaps) || !nrow(gaps)) return(NULL)
  paste(sprintf("- %s (%s): %s", gaps$kind, gaps$dimension, gaps$detail), collapse = "\n")
}
