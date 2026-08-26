# read-methods.R -- AXIS 3: the reading strategies.
#
# Each reader below has a distinct TRAVERSAL SIGNATURE, declared in its
# registration. The signature is `select|calls|state`; the values in use are
# exactly those in `gr_readers()$signature`, and nothing else is accepted:
#
#   select : all | topk | ensemble          -- which chunks reach the model
#   calls  : 1 | N | N+1 | N+logN | m+1 | N+tree+1 | rounds*2 | sum+1
#   state  : none | forward | tree          -- how information flows between calls
#
# Run `gr_readers()` for the authoritative table; the enumeration above is a
# summary of it and the registry is what is checked.
#
# Two readers that would resolve to the same signature on the same inputs are
# the same methodology wearing two names -- exactly the "Chunked" vs "Semantic"
# situation in the previous release. `gr_compare()` checks this and refuses to
# bill you twice for it.

# ---------------------------------------------------------------------------
# stuff: all | 1 | none
# The whole document in one prompt. The right answer when it fits, and the
# baseline every other strategy should be measured against.
# ---------------------------------------------------------------------------
#' @noRd
read_stuff <- function(chunks, question, client, spec, trace) {
  d <- chunks$chunks
  overhead <- prompt_overhead(question, .gr_prompts$answer_system)
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)
  fit <- fit_chunks(d, bud$input)

  if (length(fit$dropped)) {
    msg <- sprintf(paste0("The document is ~%d tokens but only ~%d fit in one '%s' prompt; ",
                          "%d of %d chunks were dropped."),
                   sum(d$tokens), bud$input, spec$model, length(fit$dropped), nrow(d))
    if (identical(spec$on_overflow, "error")) {
      gr_abort(paste0(msg, " Use reader = 'map_reduce', 'retrieve' or 'hierarchical', or set ",
                      "on_overflow = 'warn' to answer from the part that fits."),
               class = "gr_overflow")
    }
    gr_warn(paste0(msg, " This answer is based on a truncated document."), class = "gr_overflow")
  }
  if (!length(fit$idx)) {
    return(new_answer(.NOT_FOUND, "stuff", question, integer(0), trace, partial = TRUE,
                      notes = list(reason = "no chunk fits the context window")))
  }
  sub <- d[fit$idx, , drop = FALSE]
  res <- gr_call(client, answer_messages(question, render_chunks(sub), cite = spec$cite),
                 model = spec$model, max_output = spec$max_answer_tokens,
                 temperature = spec$temperature, trace = trace, label = "stuff.answer")
  ok <- usable_text(res)
  new_answer(if (ok) res$text else .NOT_FOUND, "stuff", question, sub$chunk_id, trace,
             evidence = evidence_table(sub$chunk_id, sub$text, sub$page, sub$section),
             partial = !ok || length(fit$dropped) > 0,
             notes = list(dropped_chunks = length(fit$dropped), error = res$error))
}

# ---------------------------------------------------------------------------
# map_reduce: all | N + log(N) | tree
# Ask every chunk the question independently, then consolidate. Parallelisable,
# order-independent, and the tree reduce means the merge prompt cannot overflow.
# ---------------------------------------------------------------------------
#' @noRd
read_map_reduce <- function(chunks, question, client, spec, trace) {
  d <- chunks$chunks
  if (!trace_can_call(trace, nrow(d))) {
    gr_warn(sprintf("map_reduce needs %d calls but the run cap is %d; reduce the chunk count or raise gr_options(max_calls=).",
                    nrow(d), gr_options("max_calls")), class = "gr_call_cap")
  }
  res <- gr_lapply(seq_len(nrow(d)), function(i) {
    if (!trace_can_call(trace)) return(list(ok = FALSE, text = "", capped = TRUE))
    r <- gr_call(client, answer_messages(question, render_chunks(d[i, , drop = FALSE]),
                                         cite = spec$cite),
                 model = spec$model, max_output = spec$max_chunk_tokens,
                 temperature = spec$temperature, trace = trace, label = "map.answer")
    if (spec$delay_between_calls > 0) Sys.sleep(spec$delay_between_calls)
    list(ok = usable_text(r), text = r$text, capped = FALSE)
  }, parallel = spec$parallel, label = "chunk")

  ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
  texts <- vapply(res, function(r) as_chr1(r$text), character(1))
  # A failed call contributes NOTHING. The old code let `sapply` collapse a NULL
  # into the literal string "NULL" and spliced it into the merge prompt as if it
  # were a finding.
  useful <- ok & !vapply(texts, is_not_found, logical(1))
  n_failed <- sum(!ok)

  if (!any(useful)) {
    return(new_answer(.NOT_FOUND, "map_reduce", question, d$chunk_id, trace,
                      partial = n_failed > 0,
                      notes = list(chunks = nrow(d), failed_calls = n_failed,
                                   reason = "no chunk yielded an answer")))
  }
  merged <- tree_merge(client, question, texts[useful], spec, trace, label = "reduce")
  new_answer(merged$text, "map_reduce", question, d$chunk_id[useful], trace,
             evidence = evidence_table(d$chunk_id[useful], texts[useful],
                                       d$page[useful], d$section[useful]),
             partial = n_failed > 0 || !merged$ok,
             notes = list(chunks = nrow(d), answered = sum(useful), failed_calls = n_failed,
                          merge_levels = merged$levels, merge_ok = merged$ok))
}

# ---------------------------------------------------------------------------
# refine: all | N | forward
# Strictly sequential: build a draft from the first chunk, then revise it
# against each subsequent chunk. Order matters, cannot be parallelised, and it
# is the only strategy where late evidence can overturn an early conclusion.
# ---------------------------------------------------------------------------
#' @noRd
read_refine <- function(chunks, question, client, spec, trace) {
  d <- chunks$chunks
  draft <- NULL; used <- integer(0); revisions <- 0L; failures <- 0L; truncated <- 0L
  # The draft grows with every revision, so unlike the other readers this one
  # can outgrow the context window mid-run. Budget for it explicitly.
  overhead <- prompt_overhead(question, .gr_prompts$refine_system)
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)
  for (i in seq_len(nrow(d))) {
    if (!trace_can_call(trace)) break
    excerpt <- render_chunks(d[i, , drop = FALSE])
    # Halve the budget between the running draft and the incoming excerpt.
    half <- max(as.integer(bud$input / 2), 64L)
    if (gr_count_tokens(excerpt) > half) {
      excerpt <- gr_truncate_tokens(excerpt, half); truncated <- truncated + 1L
    }
    if (!is.null(draft) && gr_count_tokens(draft) > half) {
      draft <- gr_truncate_tokens(draft, half); truncated <- truncated + 1L
    }
    msgs <- if (is.null(draft)) {
      answer_messages(question, excerpt, cite = spec$cite)
    } else {
      list(
        list(role = "system", content = .gr_prompts$refine_system),
        list(role = "user", content = paste0("Question: ", question)),
        list(role = "user", content = paste0("<draft>\n", draft, "\n</draft>")),
        list(role = "user", content = paste0("<new_excerpt>\n", excerpt, "\n</new_excerpt>")),
        list(role = "user", content = paste0(
          "Return the revised answer. If the excerpt adds nothing, return the draft unchanged. ",
          "If neither the draft nor the excerpt answers the question, return exactly ", .NOT_FOUND, "."))
      )
    }
    res <- gr_call(client, msgs, model = spec$model, max_output = spec$max_answer_tokens,
                   temperature = spec$temperature, trace = trace,
                   label = if (is.null(draft)) "refine.draft" else "refine.revise")
    if (!usable_text(res)) { failures <- failures + 1L; next }
    used <- c(used, d$chunk_id[i])
    if (is.null(draft)) { draft <- res$text } else {
      if (!identical(trimws(res$text), trimws(draft))) revisions <- revisions + 1L
      draft <- res$text
    }
    if (spec$delay_between_calls > 0) Sys.sleep(spec$delay_between_calls)
  }
  new_answer(draft %||% .NOT_FOUND, "refine", question, used, trace,
             partial = failures > 0 || length(used) < nrow(d),
             notes = list(chunks = nrow(d), visited = length(used),
                          revisions = revisions, failed_calls = failures,
                          truncations = truncated))
}

# ---------------------------------------------------------------------------
# skim: all | N + 1 | none
# The map step extracts EVIDENCE, not answers, and the single synthesis step
# sees the verbatim source text rather than N paraphrases. This is what the old
# "Retrieval" mode was trying to be.
# ---------------------------------------------------------------------------
#' @noRd
read_skim <- function(chunks, question, client, spec, trace) {
  d <- chunks$chunks
  res <- gr_lapply(seq_len(nrow(d)), function(i) {
    if (!trace_can_call(trace)) return(list(ok = FALSE, text = ""))
    r <- gr_call(client, list(
      list(role = "system", content = .gr_prompts$extract_system),
      list(role = "user", content = paste0("Question: ", question)),
      list(role = "user", content = paste0("<excerpt>\n", render_chunks(d[i, , drop = FALSE]),
                                           "\n</excerpt>"))
    ), model = spec$skim_model %||% spec$model, max_output = spec$max_chunk_tokens,
       temperature = spec$temperature, trace = trace, label = "skim.extract")
    if (spec$delay_between_calls > 0) Sys.sleep(spec$delay_between_calls)
    list(ok = usable_text(r), text = r$text)
  }, parallel = spec$parallel, label = "skim chunk")

  ok <- vapply(res, function(r) isTRUE(r$ok), logical(1))
  txt <- vapply(res, function(r) as_chr1(r$text), character(1))
  keep <- ok & !grepl("^[\"'`*_ ]*NONE[\"'`*_. ]*$", trimws(txt), ignore.case = TRUE) & has_content(txt)
  if (!any(keep)) {
    return(new_answer(.NOT_FOUND, "skim", question, integer(0), trace,
                      partial = any(!ok),
                      notes = list(chunks = nrow(d), failed_calls = sum(!ok),
                                   reason = "no chunk contained relevant evidence")))
  }
  ev <- evidence_table(d$chunk_id[keep], txt[keep], d$page[keep], d$section[keep])
  overhead <- prompt_overhead(question, .gr_prompts$answer_system)
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)
  body <- paste(sprintf("[chunk %d]\n%s", ev$chunk_id, ev$text), collapse = "\n\n")
  dropped <- 0L
  if (gr_count_tokens(body) > bud$input) {
    # Evidence itself can exceed the window on a large document. Consolidate it
    # tree-wise rather than truncating blind.
    m <- tree_merge(client, question, ev$text, spec, trace, label = "skim.consolidate",
                    system_prompt = .gr_prompts$summarise_system, kind = "evidence")
    body <- m$text
    dropped <- nrow(ev)
  }
  res2 <- if (trace_can_call(trace)) {
    gr_call(client, answer_messages(question, body, cite = spec$cite, label = "Evidence"),
            model = spec$model, max_output = spec$max_answer_tokens,
            temperature = spec$temperature, trace = trace, label = "skim.answer")
  } else gr_result(FALSE, error = "call cap reached before the synthesis step")
  new_answer(if (usable_text(res2)) res2$text else .NOT_FOUND, "skim", question, ev$chunk_id, trace,
             evidence = ev, partial = any(!ok) || !res2$ok,
             notes = list(chunks = nrow(d), with_evidence = nrow(ev),
                          failed_calls = sum(!ok), evidence_consolidated = dropped > 0))
}

# ---------------------------------------------------------------------------
# retrieve: topk | 1 | none
# Embed once, rank by cosine similarity, answer from the top k only. The cheapest
# strategy on a large corpus, and the only one whose cost does not scale with
# document length.
# ---------------------------------------------------------------------------
#' @noRd
read_retrieve <- function(chunks, question, client, spec, trace) {
  d <- chunks$chunks
  emb <- gr_embed(client, c(question, d$text), trace = trace)
  src <- attr(emb, "embedding_source") %||% "api"
  scores <- if (nrow(emb) >= 2L) cosine_against(emb[-1, , drop = FALSE], emb[1, ]) else rep(0, nrow(d))
  if (identical(src, "lexical")) {
    # Blend in BM25 so the offline path is at least a real lexical retriever.
    scores <- 0.5 * scores + 0.5 * scale01(bm25_scores(d$text, question))
  }
  k <- as.integer(clamp(spec$top_k, 1, nrow(d)))
  ord <- order(scores, decreasing = TRUE)
  keep <- ord[seq_len(k)]
  min_score <- as_num1(spec$min_score, -Inf)
  if (is.finite(min_score)) keep <- keep[scores[keep] >= min_score]
  if (!length(keep)) keep <- ord[1]

  overhead <- prompt_overhead(question, .gr_prompts$answer_system)
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)
  fit <- fit_chunks(d, bud$input, order = keep)
  sub <- d[fit$idx, , drop = FALSE]
  if (!nrow(sub)) {
    return(new_answer(.NOT_FOUND, "retrieve", question, integer(0), trace, partial = TRUE,
                      notes = list(reason = "top-ranked chunk does not fit the context window")))
  }
  trace_note(trace, "retrieve.rank",
             list(k = k, kept = nrow(sub), embedding_source = src,
                  top_scores = round(utils::head(sort(scores, decreasing = TRUE), 5), 4)))
  res <- gr_call(client, answer_messages(question, render_chunks(sub), cite = spec$cite),
                 model = spec$model, max_output = spec$max_answer_tokens,
                 temperature = spec$temperature, trace = trace, label = "retrieve.answer")
  new_answer(if (res$ok) res$text else .NOT_FOUND, "retrieve", question, sub$chunk_id, trace,
             evidence = evidence_table(sub$chunk_id, sub$text, sub$page, sub$section,
                                       scores[fit$idx]),
             partial = !res$ok || length(fit$dropped) > 0,
             notes = list(chunks = nrow(d), top_k = k, used = nrow(sub),
                          embedding_source = src, error = res$error))
}

#' @noRd
scale01 <- function(x) {
  if (!length(x)) return(x)
  r <- range(x, na.rm = TRUE)
  if (!is.finite(r[1]) || diff(r) == 0) return(rep(0.5, length(x)))
  (x - r[1]) / diff(r)
}

# ---------------------------------------------------------------------------
# rerank: topk | m + 1 | none
# Cheap lexical prefilter (free), then the model scores each candidate for
# relevance (m small calls), then one answer call. Distinct from `retrieve`
# because relevance is judged by the model, not by vector distance.
# ---------------------------------------------------------------------------
#' @noRd
read_rerank <- function(chunks, question, client, spec, trace) {
  d <- chunks$chunks
  pre <- bm25_scores(d$text, question)
  m <- as.integer(clamp(spec$rerank_candidates, 1, nrow(d)))
  cand <- order(pre, decreasing = TRUE)[seq_len(m)]

  schema <- list(type = "object", additionalProperties = FALSE,
                 required = list("score", "reason"),
                 properties = list(
                   score = list(type = "integer", minimum = 0, maximum = 10),
                   reason = list(type = "string")))
  scored <- gr_lapply(cand, function(i) {
    if (!trace_can_call(trace)) return(list(i = i, score = 0, reason = "call cap reached"))
    out <- gr_call_json(client, list(
      list(role = "system", content = paste0(
        "Rate how useful an excerpt is for answering a question. 0 = irrelevant, ",
        "10 = contains the answer directly. Judge only what is written in the excerpt.")),
      list(role = "user", content = paste0("Question: ", question)),
      list(role = "user", content = paste0("<excerpt>\n", d$text[i], "\n</excerpt>"))
    ), schema = schema, schema_name = "relevance",
       model = spec$skim_model %||% spec$model, max_output = 200L,
       temperature = spec$temperature, trace = trace, label = "rerank.score")
    if (!out$ok) return(list(i = i, score = 0, reason = "scoring failed"))
    list(i = i, score = as.numeric(out$value$score %||% 0),
         reason = as_chr1(out$value$reason))
  }, parallel = spec$parallel, label = "rerank candidate")

  sc <- vapply(scored, function(s) s$score, numeric(1))
  ii <- vapply(scored, function(s) s$i, numeric(1))
  n_failed <- sum(vapply(scored, function(s) isTRUE(s$reason == "scoring failed"), logical(1)))
  degraded <- FALSE
  if (n_failed == length(scored)) {
    # Every scoring call failed -- typically an endpoint without structured
    # output support. Degrade to the BM25 prefilter order and SAY SO, rather
    # than reporting "nothing relevant" for a document that was never
    # actually scored.
    gr_warn(paste0("Every rerank scoring call failed (does this endpoint support JSON schema ",
                   "output?). Falling back to the BM25 prefilter ranking, which is lexical, ",
                   "not model-judged."), class = "gr_rerank_degraded")
    sc <- pre[cand]
    degraded <- TRUE
  }
  ord <- order(sc, decreasing = TRUE)
  keep_ord <- ii[ord]
  keep_sc <- sc[ord]
  thresh <- if (degraded) -Inf else clamp(spec$rerank_min_score, 0, 10)
  keep_ord <- keep_ord[keep_sc >= thresh]
  if (!length(keep_ord)) {
    return(new_answer(.NOT_FOUND, "rerank", question, integer(0), trace,
                      notes = list(candidates = m, scoring_failures = n_failed,
                                   reason = sprintf("no candidate scored >= %g", thresh))))
  }
  keep_ord <- utils::head(keep_ord, as.integer(clamp(spec$top_k, 1, length(keep_ord))))

  overhead <- prompt_overhead(question, .gr_prompts$answer_system)
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)
  fit <- fit_chunks(d, bud$input, order = as.integer(keep_ord))
  sub <- d[fit$idx, , drop = FALSE]
  trace_note(trace, "rerank.select", list(candidates = m, kept = nrow(sub),
                                          degraded_to_bm25 = degraded,
                                          scores = round(utils::head(keep_sc, 10), 2)))
  res <- if (trace_can_call(trace)) {
    gr_call(client, answer_messages(question, render_chunks(sub), cite = spec$cite),
            model = spec$model, max_output = spec$max_answer_tokens,
            temperature = spec$temperature, trace = trace, label = "rerank.answer")
  } else gr_result(FALSE, error = "call cap reached before the answer step")
  new_answer(if (res$ok) res$text else .NOT_FOUND, "rerank", question, sub$chunk_id, trace,
             evidence = evidence_table(sub$chunk_id, sub$text, sub$page, sub$section,
                                       sc[match(fit$idx, ii)]),
             partial = !res$ok || degraded,
             notes = list(chunks = nrow(d), candidates = m, used = nrow(sub),
                          scoring_failures = n_failed, degraded_to_bm25 = degraded))
}

# ---------------------------------------------------------------------------
# hierarchical: all | N + tree + 1 | tree
# Genuinely recursive: summarise, then summarise the summaries, until what
# remains fits, then answer. The old "Hierarchical" mode did exactly one
# summarisation level and then concatenated -- so on a 40-chunk document its
# final prompt was 40 x 512 = 20,480 tokens against a 16,385-token window.
# ---------------------------------------------------------------------------
#' @noRd
read_hierarchical <- function(chunks, question, client, spec, trace) {
  d <- chunks$chunks
  summary_failures <- 0L
  summarise <- function(texts, level) {
    unlist(gr_lapply(seq_along(texts), function(i) {
      if (!trace_can_call(trace)) return("")
      r <- gr_call(client, list(
        list(role = "system", content = .gr_prompts$summarise_system),
        list(role = "user", content = paste0("Question: ", question)),
        list(role = "user", content = paste0("<text>\n", texts[i], "\n</text>"))
      ), model = spec$summary_model %||% spec$model, max_output = spec$max_summary_tokens,
         temperature = spec$temperature, trace = trace,
         label = sprintf("hier.summarise.L%d", level))
      if (usable_text(r)) r$text else ""
    }, parallel = spec$parallel, label = sprintf("summary L%d", level)), use.names = FALSE)
  }
  count_failures <- function(texts, got) {
    summary_failures <<- summary_failures + sum(!has_content(got))
    got
  }

  overhead <- prompt_overhead(question, .gr_prompts$answer_system)
  bud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens, overhead = overhead)

  level <- 1L
  current <- count_failures(d$text, summarise(d$text, level))
  current <- current[has_content(current)]
  if (!length(current)) {
    return(new_answer(.NOT_FOUND, "hierarchical", question, integer(0), trace, partial = TRUE,
                      notes = list(chunks = nrow(d), levels = 1L,
                                   reason = "every summarisation call failed")))
  }
  # Recurse until the combined summary fits, capping the depth so a model that
  # refuses to compress cannot spin forever.
  while (gr_count_tokens(paste(current, collapse = "\n\n")) > bud$input && level < spec$max_levels) {
    level <- level + 1L
    fan <- as.integer(spec$fan_in)
    groups <- split(current, ceiling(seq_along(current) / fan))
    prev_n <- length(current)
    current <- summarise(vapply(groups, paste, character(1), collapse = "\n\n"), level)
    current <- current[has_content(current)]
    gr_msg(sprintf("Hierarchical level %d: %d -> %d summaries.", level, prev_n, length(current)))
    if (!length(current) || length(current) >= prev_n) break
  }

  body <- paste(current, collapse = "\n\n")
  if (gr_count_tokens(body) > bud$input) {
    body <- gr_truncate_tokens(body, bud$input)
    gr_warn(sprintf("Hierarchical summaries still exceed the budget after %d level(s); truncating.", level),
            class = "gr_overflow")
  }
  res <- if (trace_can_call(trace)) {
    # cite = FALSE, always. This reader answers from SUMMARIES, which carry no
    # [chunk N] ids -- asking for citations here asks the model to invent ids
    # that are not in front of it, which is how a citing reader starts citing
    # chunks it never saw.
    gr_call(client, answer_messages(question, body, cite = FALSE, label = "Summaries"),
            model = spec$model, max_output = spec$max_answer_tokens,
            temperature = spec$temperature, trace = trace, label = "hier.answer")
  } else gr_result(FALSE, error = "call cap reached before the answer step")
  new_answer(if (usable_text(res)) res$text else body, "hierarchical", question, d$chunk_id, trace,
             partial = !usable_text(res) || summary_failures > 0,
             notes = list(chunks = nrow(d), levels = level, final_summaries = length(current),
                          failed_summaries = summary_failures))
}

# ---------------------------------------------------------------------------
# iterative: topk | rounds x (1 + 1) | forward
# Agentic. The model states what it still needs, that becomes the next query,
# and the loop stops when it says it can answer or the budget runs out. The only
# strategy where the retrieval query is not the user's question.
# ---------------------------------------------------------------------------
#' @noRd
read_iterative <- function(chunks, question, client, spec, trace) {
  d <- chunks$chunks
  emb <- gr_embed(client, d$text, trace = trace)
  seen <- integer(0); gathered <- character(0); queries <- as_chr1(question)
  rounds <- 0L; done_reason <- "max rounds"

  schema <- list(type = "object", additionalProperties = FALSE,
                 required = list("can_answer", "answer", "next_query"),
                 properties = list(
                   can_answer = list(type = "boolean"),
                   answer = list(type = "string"),
                   next_query = list(type = "string")))

  while (rounds < spec$max_rounds) {
    rounds <- rounds + 1L
    q_emb <- gr_embed(client, queries[length(queries)], trace = trace)
    sc <- if (nrow(emb)) cosine_against(emb, q_emb[1, ]) else rep(0, nrow(d))
    sc[seen] <- -Inf
    take <- utils::head(order(sc, decreasing = TRUE), as.integer(clamp(spec$top_k, 1, nrow(d))))
    take <- take[is.finite(sc[take])]
    if (!length(take)) { done_reason <- "no unseen chunks"; break }
    seen <- c(seen, take)
    gathered <- c(gathered, render_chunks(d[take, , drop = FALSE]))

    if (!trace_can_call(trace)) { done_reason <- "call cap"; break }
    body <- paste(gathered, collapse = "\n\n")
    obud <- gr_budget(spec$model, reserve_output = spec$max_answer_tokens,
                      overhead = prompt_overhead(question, .gr_prompts$answer_system))
    if (gr_count_tokens(body) > obud$input) body <- gr_truncate_tokens(body, obud$input)

    out <- gr_call_json(client, list(
      list(role = "system", content = paste0(
        .gr_prompts$answer_system,
        " You are reading iteratively. If the excerpts so far are sufficient, set can_answer ",
        "true and give the answer. If not, set can_answer false and put in next_query the ",
        "specific missing information to search for -- a phrase you would expect to appear in ",
        "the document, not a restatement of the question.")),
      list(role = "user", content = paste0("Question: ", question)),
      list(role = "user", content = paste0("<excerpts>\n", body, "\n</excerpts>"))
    ), schema = schema, schema_name = "iterative_step", model = spec$model,
       max_output = spec$max_answer_tokens, temperature = spec$temperature,
       trace = trace, label = "iterative.step")

    if (!out$ok) {
      done_reason <- "step failed"
      if (rounds == 1L) {
        gr_warn(paste0("The first iterative step returned no parsable structured output, so the ",
                       "retrieve-assess loop cannot run (does this endpoint support JSON schema ",
                       "output?). Answering from the first retrieval only -- this is effectively ",
                       "'retrieve', not 'iterative'."), class = "gr_iterative_degraded")
      }
      break
    }
    if (isTRUE(out$value$can_answer)) {
      trace_note(trace, "iterative.stop", list(rounds = rounds, reason = "model satisfied"))
      return(new_answer(as_chr1(out$value$answer, .NOT_FOUND), "iterative", question,
                        d$chunk_id[seen], trace,
                        evidence = evidence_table(d$chunk_id[seen], d$text[seen],
                                                  d$page[seen], d$section[seen]),
                        notes = list(rounds = rounds, chunks_seen = length(seen),
                                     queries = queries, stop_reason = "model satisfied")))
    }
    nq <- as_chr1(out$value$next_query)
    if (!nzchar(nq) || nq %in% queries) { done_reason <- "query loop"; break }
    queries <- c(queries, nq)
    gr_msg(sprintf("Iterative round %d -> searching for: %s", rounds, substr(nq, 1, 90)))
  }

  # Budget exhausted: answer from everything gathered rather than returning
  # nothing for work already paid for.
  if (!length(seen)) {
    return(new_answer(.NOT_FOUND, "iterative", question, integer(0), trace, partial = TRUE,
                      notes = list(rounds = rounds, stop_reason = done_reason)))
  }
  sub <- d[seen, , drop = FALSE]
  res <- if (trace_can_call(trace)) {
    gr_call(client, answer_messages(question, render_chunks(sub), cite = spec$cite),
            model = spec$model, max_output = spec$max_answer_tokens,
            temperature = spec$temperature, trace = trace, label = "iterative.final")
  } else gr_result(FALSE, error = "call cap reached before the answer step")
  new_answer(if (res$ok) res$text else .NOT_FOUND, "iterative", question, sub$chunk_id, trace,
             evidence = evidence_table(sub$chunk_id, sub$text, sub$page, sub$section),
             partial = TRUE,
             notes = list(rounds = rounds, chunks_seen = length(seen), queries = queries,
                          stop_reason = done_reason))
}

# ---------------------------------------------------------------------------
# ensemble: ensemble | sum+1 | none
# Runs several DIFFERENT readers and adjudicates. Distinct only if its members
# are distinct -- which is enforced, because the old "MultiPass" was just
# Retrieval + Chunked re-run verbatim and merged.
# ---------------------------------------------------------------------------
#' @noRd
read_ensemble <- function(chunks, question, client, spec, trace) {
  members <- spec$members %||% c("retrieve", "map_reduce")
  members <- unique(members[nzchar(members)])
  if ("ensemble" %in% members) {
    gr_abort("An ensemble cannot contain 'ensemble'.", class = "gr_bad_ensemble")
  }
  if (length(members) < 2L) {
    gr_abort(paste0("An ensemble needs at least two DISTINCT members; got ",
                    if (!length(members)) "none." else sprintf("only '%s'.", members[1]),
                    " See gr_readers() for the available strategies and their signatures."),
             class = "gr_bad_ensemble")
  }
  sigs <- vapply(members, function(m) gr_reader_signature(m), character(1))
  if (anyDuplicated(sigs)) {
    dup <- members[duplicated(sigs) | duplicated(sigs, fromLast = TRUE)]
    gr_abort(sprintf(paste0("Ensemble members %s share the traversal signature '%s', so they would ",
                            "issue the same calls and produce the same answer at temperature 0. ",
                            "Pick members with different signatures -- see gr_readers()."),
                     paste(sprintf("'%s'", dup), collapse = " and "), sigs[duplicated(sigs)][1]),
             class = "gr_bad_ensemble")
  }

  # Distinct signatures guarantee the members traverse differently *when there is
  # something to traverse*. On a one-chunk document every selection strategy
  # selects the same single chunk and every reader issues the same prompt, so
  # 'stuff' and 'rerank' return byte-identical answers however different their
  # signatures are. Say so rather than presenting agreement as corroboration.
  if (nrow(chunks$chunks) <= 1L) {
    gr_warn(paste0("This document produced ", nrow(chunks$chunks), " chunk(s). Every ensemble ",
                   "member will read the same text and issue the same prompt, so their ",
                   "agreement carries no independent evidence. Lower `max_tokens` in the ",
                   "segment spec, or use a single reader."),
            class = "gr_ensemble_degenerate")
  }

  results <- lapply(members, function(m) {
    sub <- spec; sub$reader <- m; sub$members <- NULL
    r <- registry_get("readers", m, "readers")
    tryCatch(r$fn(chunks, question, client, sub, trace),
             error = function(e) new_answer(.NOT_FOUND, m, question, integer(0), trace,
                                            partial = TRUE,
                                            notes = list(error = conditionMessage(e))))
  })
  names(results) <- members
  usable <- vapply(results, function(r) !is_not_found(r$answer), logical(1))
  if (!any(usable)) {
    return(new_answer(.NOT_FOUND, "ensemble", question, integer(0), trace, partial = TRUE,
                      notes = list(members = members, reason = "no member produced an answer")))
  }
  if (sum(usable) == 1L) {
    only <- results[[which(usable)]]
    return(new_answer(only$answer, "ensemble", question, only$chunks_used, trace,
                      evidence = only$evidence, partial = TRUE,
                      notes = list(members = members, adjudication = "single member answered",
                                   answered_by = members[usable])))
  }
  body <- paste(sprintf("<finding source=\"%s\">\n%s\n</finding>", members[usable],
                        vapply(results[usable], function(r) r$answer, character(1))),
                collapse = "\n\n")
  res <- if (trace_can_call(trace)) {
    gr_call(client, list(
      list(role = "system", content = paste0(
        .gr_prompts$merge_system,
        " These findings come from different reading strategies over the same document. Where they ",
        "agree, state it once. Where they conflict, say which strategy found what and which claim ",
        "has the more specific support.")),
      list(role = "user", content = body),
      list(role = "user", content = paste0("Question: ", question))
    ), model = spec$model, max_output = spec$max_answer_tokens, temperature = spec$temperature,
       trace = trace, label = "ensemble.adjudicate")
  } else gr_result(FALSE, error = "call cap reached before adjudication")

  ev <- do.call(rbind, lapply(results[usable], function(r) r$evidence))

  # Observed, not assumed, distinctness. Two members that read the same chunks
  # and returned the same text did not corroborate each other -- they were the
  # same run twice. Record which, so `gr_compare()` and the report can say so.
  ans <- vapply(results[usable], function(r) trimws(as_chr1(r$answer)), character(1))
  seen <- vapply(results[usable], function(r) paste(sort(unique(r$chunks_used)), collapse = ","),
                 character(1))
  fp <- paste(ans, seen, sep = "")
  collapsed <- if (anyDuplicated(fp)) {
    unname(lapply(split(members[usable], factor(fp, levels = unique(fp))),
                  function(g) g)[vapply(split(fp, factor(fp, levels = unique(fp))), length, integer(1)) > 1L])
  } else list()
  if (length(collapsed)) {
    gr_warn(sprintf(paste0("Ensemble members %s read the same chunks and returned the same answer. ",
                           "Their agreement is not independent corroboration."),
                    paste(vapply(collapsed, function(g) paste(sprintf("'%s'", g), collapse = " and "),
                                 character(1)), collapse = "; ")),
            class = "gr_ensemble_degenerate")
  }

  new_answer(if (res$ok) res$text else paste(ans, collapse = "\n\n---\n\n"),
             "ensemble", question,
             unique(unlist(lapply(results[usable], function(r) r$chunks_used))), trace,
             evidence = ev, partial = !res$ok || any(!usable),
             notes = list(members = members, signatures = unname(sigs),
                          answered = members[usable], adjudication = if (res$ok) "llm" else "concatenated",
                          collapsed_members = collapsed,
                          member_notes = lapply(results, function(r) r$notes)))
}

#' @noRd
register_builtin_readers <- function() {
  gr_register_reader("stuff", read_stuff, signature = "all|1|none", cost_calls = "1",
    description = "Whole document in one prompt. The baseline; errors or truncates if it does not fit.")
  gr_register_reader("map_reduce", read_map_reduce, signature = "all|N+logN|tree", cost_calls = "N + merges",
    description = "Answer each chunk independently, then tree-reduce. Parallel, order-independent.")
  gr_register_reader("refine", read_refine, signature = "all|N|forward", cost_calls = "N",
    description = "Sequential draft-and-revise down the document. Order matters; not parallelisable.")
  gr_register_reader("skim", read_skim, signature = "all|N+1|none", cost_calls = "N + 1",
    description = "Extract verbatim evidence per chunk, then answer once from the evidence.")
  gr_register_reader("retrieve", read_retrieve, signature = "topk|1|none", cost_calls = "1 + embeddings",
    description = "Embed, rank by cosine similarity, answer from the top k. Cost independent of length.")
  gr_register_reader("rerank", read_rerank, signature = "topk|m+1|none", cost_calls = "m + 1",
    description = "BM25 prefilter, then the model scores candidates, then answer from the winners.")
  gr_register_reader("hierarchical", read_hierarchical, signature = "all|N+tree+1|tree",
    cost_calls = "N + fan-in levels + 1",
    description = "Recursively summarise until the summaries fit, then answer. Truly recursive.")
  gr_register_reader("iterative", read_iterative, signature = "topk|rounds*2|forward",
    cost_calls = "up to 2 x max_rounds",
    description = "Agentic loop: the model names what it still needs and that drives the next retrieval.")
  gr_register_reader("ensemble", read_ensemble, signature = "ensemble|sum+1|none",
    cost_calls = "sum of members + 1",
    description = "Run several distinct readers and adjudicate. Members must have different signatures.")
  invisible(NULL)
}
