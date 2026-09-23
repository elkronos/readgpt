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

#' @noRd
.gr_claims_schema <- list(
  type = "object", additionalProperties = FALSE,
  required = list("claims"),
  properties = list(claims = list(
    type = "array",
    items = list(
      type = "object", additionalProperties = FALSE,
      required = list("claim", "kind", "supported_by"),
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
                               description = "The [study N] numbers that contradict it, if any."),
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
#' `[study N]` marker in finished prose already is. A number that is not there is
#' dropped and counted rather than trusted, a claim left with no supporting study
#' is dropped entirely, and a `moderator` naming a column the table does not have
#' is cleared -- an invented explanation for a real disagreement. `$dropped`
#' records all of it, so a claims table that looks thin can be told apart from a
#' literature that is.
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
#' own rather than disappearing.
#'
#' @param extraction A [gr_extract()] result, or a data frame shaped like its
#'   `$table`.
#' @param question The review question. Taken from `protocol` if omitted.
#' @param protocol A [gr_protocol()]; its `question` is used when `question` is
#'   not given.
#' @param client A [gr_client()]. One is built from `model` if omitted.
#' @param model,temperature,max_claim_tokens Passed to the model call.
#' @param include_unclear Keep rows with nothing extracted. Off by default, the
#'   same as [gr_synthesise()].
#' @param trace A [gr_trace()] to record into.
#' @return An object of class `gr_claims`:
#'   \describe{
#'     \item{`claims`}{One row per claim: `claim_id`, `claim`, `kind`,
#'       `moderator`, `scope`, `n_support`, `n_contradict`, `note`.}
#'     \item{`support`}{Long: `claim_id`, `study`, `role` (`"supports"` or
#'       `"contradicts"`). Join it to `$studies` to reach documents and quotes.}
#'     \item{`studies`}{The rows the claims were drawn from, numbered.}
#'     \item{`dropped`}{What verification removed, and why.}
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
                      include_unclear = FALSE, trace = NULL) {
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
  trace <- trace %||% gr_trace(meta = list(stage = "claims", question = question,
                                           studies = nrow(used)))

  # Same withholding as the write-up: a model that can see who wrote a study
  # will attribute to the name rather than to the number, and a claim attributed
  # to a name is checked by nothing.
  hidden <- unlist(bib_columns(used), use.names = FALSE)
  rendered <- render_studies(used, hide = hidden)
  # "never": claims_batch() sends the question once.
  overhead <- prompt_overhead(question, .gr_prompts$claims_system, "never")
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)
  groups <- synth_batches(rendered, bud$input)

  raw <- list()
  not_sent <- 0L
  for (g in seq_along(groups)) {
    if (length(groups) > 1L) gr_msg(sprintf("Claims from batch %d of %d.", g, length(groups)))
    # A batch a limit stopped is not a batch the model found nothing in.
    if (!trace_can_call(trace)) {
      not_sent <- not_sent + 1L
      next
    }
    raw[[g]] <- claims_batch(groups[[g]], question, client, spec, trace)
  }
  if (not_sent > 0L) {
    gr_warn(sprintf(paste0("%d of %d batch(es) of studies were not sent: the run reached its %s. ",
                           "The studies in them contribute no claims. Raise the limit and run ",
                           "gr_claims() again."),
                    not_sent, length(groups), cap_name(trace)),
            class = "gr_claims_capped")
  }
  got <- do.call(rbind, raw[!vapply(raw, is.null, logical(1))])
  if (is.null(got) || !nrow(got)) {
    gr_warn(paste0("No claims came back. ",
                   if (not_sent == length(groups)) sprintf("No batch was sent: the run had reached its %s. ",
                                                           cap_name(trace))
                   else "Every call failed, or the model returned none. ",
                   "There is nothing for gr_outline() or gr_synthesise(claims = ) to work from."),
            class = "gr_no_claims")
    return(new_claims(empty_claim_rows(), used, question, empty_dropped(), trace))
  }

  # The columns the model was SHOWN, not every column in the table. The moderator
  # check used the reserved-field list, which leaves the bibliographic columns in
  # -- so a claim could name `year` as the moderator of a disagreement, be
  # accepted, and carry an explanation drawn from a column render_studies()
  # withheld precisely so it could not be used. An invented explanation for a
  # real disagreement is the most convincing error this layer can make.
  checked <- claims_verify(got, used, cols = study_fields(used, hide = hidden))
  if (!nrow(checked$claims)) {
    gr_warn(paste0("Every claim was dropped in verification -- see `$dropped`. The usual cause is ",
                   "a model citing study numbers that are not in the table."),
            class = "gr_no_claims")
    return(new_claims(empty_claim_rows(), used, question, checked$dropped, trace))
  }
  # Claims can repeat only across batches that returned some.
  final <- if (sum(vapply(raw, function(r) NROW(r) > 0L, logical(1))) > 1L) {
    claims_reconcile(checked$claims, question, client, spec, trace)
  } else checked$claims
  new_claims(final, used, question, checked$dropped, trace)
}

#' One claims call over one batch of studies.
#' @noRd
claims_batch <- function(block, question, client, spec, trace) {
  if (!trace_can_call(trace)) return(NULL)
  res <- gr_call_json(client, list(
    list(role = "system", content = .gr_prompts$claims_system),
    list(role = "user", content = paste0("Review question: ", question)),
    list(role = "user", content = paste0("<studies>\n", paste(block, collapse = "\n\n"),
                                         "\n</studies>"))
  ), schema = .gr_claims_schema, schema_name = "claims", model = spec$model,
     max_output = spec$max_answer_tokens, temperature = spec$temperature,
     trace = trace, label = "claims.draw")
  if (!isTRUE(res$ok)) return(NULL)
  claim_rows(json_field(res$value, "claims", scalar = FALSE))
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
#'   * A claim left with no supporting study is dropped. A claim attached to
#'     nothing is an opinion.
#'   * A study cannot both support and contradict one claim, so it is removed
#'     from the contradicting side and noted.
#'   * A `moderator` naming a column the table does not have is cleared. The
#'     claim may still be sound; the EXPLANATION was invented, and an invented
#'     explanation for a real disagreement is the most convincing kind of error
#'     this layer can make.
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
  list(claims = got[keep, , drop = FALSE],
       dropped = if (length(drops)) do.call(rbind, drops) else empty_dropped())
}

#' Group claims that say the same thing, across batches.
#'
#' The model is shown the claim TEXTS only -- no studies, no numbers to cite --
#' so the worst it can do is group badly. It cannot invent a claim here, and it
#' cannot invent support: a group's support is the union of its members', all of
#' which were verified before this ran. A claim the reply fails to place keeps
#' its own group, because dropping one silently is the failure this whole file is
#' built to avoid.
#' @noRd
claims_reconcile <- function(claims, question, client, spec, trace) {
  n <- nrow(claims)
  if (n < 2L) return(reindex_claims(claims))
  if (!trace_can_call(trace)) {
    gr_warn(sprintf(paste0("Claims from different batches were not merged: the run reached its %s, ",
                           "so one finding can appear as more than one claim."),
                    cap_name(trace)),
            class = "gr_claims_capped")
    return(reindex_claims(claims))
  }
  listed <- paste(sprintf("%d. %s", seq_len(n), claims$claim), collapse = "\n")
  res <- gr_call_json(client, list(
    list(role = "system", content = .gr_prompts$reconcile_system),
    list(role = "user", content = paste0("Review question: ", question)),
    list(role = "user", content = paste0("<claims>\n", listed, "\n</claims>"))
  ), schema = .gr_reconcile_schema, schema_name = "claim_groups", model = spec$model,
     max_output = spec$max_answer_tokens, temperature = spec$temperature,
     trace = trace, label = "claims.reconcile")
  if (!isTRUE(res$ok)) return(reindex_claims(claims))

  raw <- json_field(res$value, "groups", scalar = FALSE)
  g <- as_id_list(if (is.list(raw) || is.matrix(raw)) raw else list(raw),
                  if (is.matrix(raw)) nrow(raw) else length(raw))
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
  merged <- lapply(clean, function(v) {
    first <- claims[v[1], , drop = FALSE]
    # The longest text, because a claim stated over more studies is usually
    # stated more fully, and the support is the union either way.
    first$claim <- claims$claim[v][which.max(nchar(claims$claim[v]))]
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
    nts <- stats::na.omit(claims$note[v])
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

#' @noRd
new_claims <- function(claims, used, question, dropped, trace) {
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
                 dropped = dropped, trace = trace), class = "gr_claims")
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

#' @noRd
.gr_outline_schema <- list(
  type = "object", additionalProperties = FALSE,
  required = list("sections"),
  properties = list(sections = list(
    type = "array",
    items = list(
      type = "object", additionalProperties = FALSE,
      required = list("heading", "brief", "claims"),
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
#' the strongest thing a review has to say -- that a literature splits into three
#' incompatible operationalisations, say, and that the argument about effect size
#' is really an argument about measurement. This derives the structure from the
#' claims instead, and hands it back for you to accept or replace.
#'
#' @section What is verified:
#' Every claim is assigned to exactly one section. A claim number that does not
#' exist is dropped; a claim assigned twice keeps its first section; a claim the
#' reply never placed is put in the closing section rather than lost, with a
#' message saying so. A section left with no claims is removed -- except the
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
#' @return A named character vector shaped exactly like the `outline` argument of
#'   [gr_synthesise()] -- headings as names, briefs as values -- carrying
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
                       closing = "What is missing", trace = NULL) {
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
  spec <- gr_read_spec("stuff", model = model, temperature = temperature,
                       max_answer_tokens = 1200L)
  trace <- trace %||% claims$trace %||% gr_trace(meta = list(stage = "outline"))

  cw <- claims$claims
  listed <- paste(sprintf("%d. [%s] %s%s", cw$claim_id, cw$kind, cw$claim,
                          ifelse(is.na(cw$moderator), "",
                                 sprintf(" (contested; distinguished by %s)", cw$moderator))),
                  collapse = "\n")
  capped <- !trace_can_call(trace)
  res <- if (!capped) {
    gr_call_json(client, list(
      list(role = "system", content = .gr_prompts$outline_system),
      list(role = "user", content = paste0("Review question: ", question)),
      list(role = "user", content = paste0("<claims>\n", listed, "\n</claims>")),
      list(role = "user", content = sprintf("Use at most %d sections.",
                                            as.integer(max_sections)))
    ), schema = .gr_outline_schema, schema_name = "outline", model = spec$model,
       max_output = spec$max_answer_tokens, temperature = spec$temperature,
       trace = trace, label = "outline.derive")
  } else list(ok = FALSE, value = NULL)

  secs <- if (isTRUE(res$ok)) outline_rows(json_field(res$value, "sections", scalar = FALSE)) else NULL
  if (is.null(secs) || !nrow(secs)) {
    gr_warn(if (capped) sprintf(paste0("The outline was not requested: the run had reached its %s. ",
                                       "Every claim was put in one section. Raise the limit, or ",
                                       "pass an `outline` to gr_synthesise() yourself."),
                                cap_name(trace))
            else paste0("The outline call did not return usable sections, so every claim was put ",
                        "in one section. Pass an `outline` to gr_synthesise() yourself, or try again."),
            class = "gr_outline_failed")
    secs <- data.frame(heading = "Findings", brief = "What the evidence supports",
                       rationale = NA_character_, stringsAsFactors = FALSE)
    secs$.claims <- list(cw$claim_id)
  }
  finish_outline(secs, cw, closing, max_sections)
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
#'   `$fields` is used, and only to learn which categories were *declared* --
#'   without it a category nobody studied is indistinguishable from one nobody
#'   thought of.
#' @param max_cells Cap on reported empty combinations, which grow as the product
#'   of two fields' sizes.
#' @param min_reported A field missing for more than this fraction of studies is
#'   reported as not reported.
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
gr_gaps <- function(claims, extraction = NULL, max_cells = 40L, min_reported = 0.5) {
  if (!inherits(claims, "gr_claims")) {
    gr_abort("`claims` must come from gr_claims().", class = "gr_bad_claims")
  }
  st <- claims$studies
  cw <- claims$claims
  fields <- if (inherits(extraction, "gr_extraction")) extraction$fields else
    if (inherits(extraction, "gr_fields")) extraction else NULL
  cols <- setdiff(names(st), c("study", .gr_reserved_fields))
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
    cat("  nobody asked about -- pass `extraction =` for those\n")
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
