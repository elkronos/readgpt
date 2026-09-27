# core-embed.R -- real embeddings.
#
# WHY THIS FILE EXISTS
# The old `compute_embedding()` was:
#
#     set.seed(nchar(text)); runif(768)
#
# Three consequences. First, the "embedding" was a function of *string length
# only*, so "The mitochondrion is the powerhouse of the cell." and "Quarterly
# revenue fell by twelve percent in Q3!!" (both 48 characters) had cosine
# similarity exactly 1.0. `sort_chunks_by_semantic()` therefore ranked chunks by
# how close their character count was to the question's -- a 21-character run of
# "aaaa..." outranked the paragraph that actually answered the question. Second,
# because "Semantic" mode fed those chunks straight into the same chunked reader
# as "Chunked" mode, the mode had no distinct behaviour at all. Third,
# `set.seed()` permanently clobbered the caller's RNG stream, silently breaking
# any simulation or bootstrap running in the same session.
#
# This file provides real API embeddings, cached and batched, with a documented
# offline fallback (hashed bag-of-words) that is honest about being a fallback.

#' Embed texts
#'
#' @param client A `gr_client`.
#' @param texts Character vector.
#' @param model Embedding model id; defaults to the client's.
#' @param batch_size Texts per request.
#' @param cache Use the session embedding cache.
#' @param trace Optional trace. With the built-in `"api"` embedder, each
#'   request to the embeddings endpoint is checked against `max_calls` and
#'   `max_cost_usd` (see [gr_options()]) before it is sent and recorded in the
#'   trace, priced, once it is made; its tokens are counted in the trace's
#'   `embed_tokens`, not `tokens_in`. A request the limits refuse is a failure,
#'   handled by `fallback`. A failed request that the lexical fallback replaces
#'   is marked `recovered = TRUE` in the trace's `errors`: the text was still
#'   embedded, on word overlap, so nothing was left unread.
#' @param embedder A registered embedder name (see [gr_embedders()]), or a
#'   function of `(texts, params)`. When it is `NULL` the first of these is
#'   used: `gr_options("embedder")` when it names one, then the embed function
#'   supplied with the client (a [gr_mock_client()]'s `embed_handler`, or the
#'   `embed` given to [gr_backend_client()] or [gr_ellmer_client()]), then the
#'   built-in `"api"`. So an embedder named in [gr_options()] wins over the
#'   client's own embed function.
#' @param fallback What to do when the embedding request fails. **Defaults to
#'   `"lexical"`**: hashed bag-of-words vectors that measure word overlap, not
#'   meaning, so `semantic` segmentation and `retrieve` ranking become markedly
#'   less accurate. The substitution warns and is recorded, but the run
#'   continues. Use `"error"` to fail fast, or `"none"` to get an empty matrix.
#'   An embedder fails when it raises, or returns the wrong number of rows,
#'   vectors with no dimensions, or missing or non-finite values. The built-in
#'   `"api"` embedder also fails on a reply that leaves a text without a usable
#'   vector (missing, empty or all zeros), gives vectors of different lengths,
#'   or labels them with `index` fields that are not one for each text sent
#'   (vectors are matched to texts by `index` when the reply gives it). It
#'   retries a rate limit (HTTP 429), a server error or a dropped connection up
#'   to the client's `max_retries`, as [gr_call()] does, and the warning names
#'   the HTTP status and the provider's message. A refused key (HTTP 401 or
#'   403) is a failure like the others rather than a stop, since a key can be
#'   allowed to chat and not to embed; a missing key stops the run.
#' @return A numeric matrix, one row per input, carrying an `"embedding_source"`
#'   attribute naming the embedder that produced it (`"api"` or `"lexical"` for
#'   the built-ins). Always check it before treating the rows as semantic.
#'   Rows from the API and lexical paths are L2-normalised.
#'   With `fallback = "none"` and a failed request the result is a 0 x 0 matrix.
#' @export
#' @seealso [gr_embedders()] for what is registered, [gr_register_embedder()]
#'   to add one, [gr_client()], [gr_segment_spec()] for `method = "semantic"`,
#'   [gr_read_spec()] for `reader = "retrieve"`, [gr_models()] for the
#'   embedding models in the registry
#' @examples
#' cl <- gr_mock_client()
#' e <- gr_embed(cl, c("cats sleep all day", "dogs bark all night",
#'                     "revenue rose to 45.2 million"))
#'
#' # Always check this before treating the rows as semantic: on the lexical
#' # fallback they reflect word overlap, not meaning.
#' attr(e, "embedding_source")
#'
#' # Rows are L2-normalised, so the cross-product is cosine similarity.
#' round(e %*% t(e), 3)
#'
#' # The semantic segmenter records the same thing, so a degraded run stays
#' # visible after the fact.
#' gr_segment(readgpt_example(), list(method = "semantic", max_tokens = 200),
#'            client = cl)$extra$embedding_source
gr_embed <- function(client, texts, model = NULL, batch_size = 64L, cache = NULL,
                     trace = NULL, fallback = c("lexical", "error", "none"),
                     embedder = NULL) {
  fallback <- match.arg(fallback)
  texts <- vapply(texts %||% character(0), as_chr1, character(1), USE.NAMES = FALSE)
  if (!length(texts)) return(matrix(numeric(0), nrow = 0, ncol = 0))
  cache <- isTRUE(cache %||% gr_options("cache_embeddings"))
  model <- as_chr1(model %||% client$embedding_model)
  emb <- resolve_embedder(client, embedder)
  # Where this call's failed requests start in the trace, so the ones the
  # lexical fallback recovers from can be marked as such.
  first_error <- if (inherits(trace, "gr_trace")) length(trace$errors) + 1L else 1L

  degrade <- function(msg, class) {
    if (identical(fallback, "error")) gr_abort(msg, class = "gr_embed_error")
    if (identical(fallback, "none")) return(matrix(numeric(0), nrow = 0, ncol = 0))
    gr_warn(msg, class = class)
    # Recovered: every text is still embedded, on word overlap, and the reader
    # marks its answer partial. Left as a plain failure, a request the old code
    # never recorded made gr_read_many() call the document "failed ... not read
    # in full" and keep it out of the store, on every run, on any endpoint
    # without embeddings (a gateway, a local server with no embedding model).
    trace_mark_recovered(trace, first_error)
    # Marked as a FALLBACK, not merely as lexical. Choosing
    # gr_options(embedder = "lexical") is a decision; being dropped onto it
    # because the real embedder failed is a degradation, and the readers OR that
    # into `partial` -- which the documentation tells people to check first.
    out <- finish_embedding(lexical_embed(texts), "lexical", trace, length(texts))
    attr(out, "embedding_fallback") <- TRUE
    out
  }

  # A trace records each embeddings request, but not the vectors it returned.
  # So a replay can only reproduce a run's ranking if the embedder is a pure
  # function of the text -- in which case the vectors are simply computed again
  # and the replay is exact. Anything else has to degrade, and say so.
  if (inherits(client, "gr_replay_client")) {
    recorded <- as_chr1(client$embed_source, NA_character_)
    # Two conditions, not one. The embedder must be deterministic AND it must be
    # the one the recording used: replaying an API-embedded run with a local
    # deterministic embedder would reproduce nothing while looking exact.
    reproducible <- isTRUE(emb$deterministic) && identical(recorded, emb$name)
    if (!reproducible) {
      why <- if (is.na(recorded))
        "the recording does not say which embedder produced its vectors"
      else if (!identical(recorded, emb$name))
        sprintf("the recording embedded with '%s' and this run would use '%s'",
                recorded, emb$name)
      else sprintf("'%s' is not deterministic, so re-running it need not give the same vectors",
                   emb$name)
      # Noted on the client, so a miss this causes says so: the prompts built
      # from these vectors are not the recorded ones, and a bare "the replay
      # has diverged" sent people looking for a different document or question.
      if (is.environment(client$.idx)) client$.idx$embed_degraded <- TRUE
      return(degrade(paste0(
        "Replaying a run cannot reproduce its embeddings: ", why, ". A trace records ",
        "embeddings requests, not the vectors they returned. Falling back to hashed lexical ",
        "vectors, which place semantic cuts and rank chunks differently from the original run, ",
        "so the replay can send prompts the recording does not hold (a gr_replay_miss, or ",
        "failed calls with strict = FALSE) and not give the recorded answer. Record the run ",
        "with a deterministic embedder (gr_options(embedder = 'lexical'), or one registered ",
        "with gr_register_embedder(deterministic = TRUE)) and replay it with the same one, ",
        "and the replay is exact."),
        "gr_replay_no_embeddings"))
    }
  }

  # The built-in "api" embedder posts to the client's base URL. A backend or
  # replay client has no such endpoint, so say that plainly rather than letting
  # a request to "backend://" fail and be reported as a network problem.
  if (identical(emb$name, "api") && !is.function(client$embed_handler) &&
      as_chr1(client$api, "") %in% c("backend", "replay")) {
    return(degrade(paste0(
      "This client has no embeddings endpoint and no embed function, so there is no way to ",
      "embed text. Falling back to hashed lexical vectors: these approximate word overlap, ",
      "not meaning, so semantic segmentation and top-k retrieval will be markedly less ",
      "accurate. Pass `embed =` to gr_backend_client() or gr_ellmer_client(), or set ",
      "gr_options(embedder = ) to a registered embedder; see gr_embedders()."),
      "gr_backend_no_embeddings"))
  }

  m <- tryCatch(emb$fn(texts, list(client = client, model = model, batch_size = batch_size,
                                   cache = cache, trace = trace, embedder = emb$name)),
                error = function(e) e)
  # A missing key is not an embedder failing: falling back to lexical vectors
  # would hide it behind a quality warning, and every later request would fail
  # the same way. It stops the run.
  if (inherits(m, "gr_auth_error")) stop(m)
  bad <- if (inherits(m, "condition")) conditionMessage(m)
         else if (!is.numeric(m)) "it did not return a numeric matrix"
         else if (NROW(m) != length(texts))
           sprintf("it returned %d row(s) for %d text(s)", NROW(m), length(texts))
         # Vectors with no dimensions, or with holes, rank nothing: every
         # cosine came out 0 (or NA), and a reader took the first chunk as the
         # best one while the matrix passed for a good one.
         else if (length(m) == 0L || (is.matrix(m) && ncol(m) == 0L))
           "it returned vectors with no dimensions"
         else if (!all(is.finite(m))) "it returned missing or non-finite values"
         else NULL
  if (is.null(bad)) return(finish_embedding(m, emb$name, trace, length(texts)))

  degrade(sprintf(paste0("Embedder '%s' failed: %s. Falling back to hashed lexical vectors: ",
                         "these approximate word overlap, not meaning, so semantic ",
                         "segmentation and top-k retrieval will be markedly less accurate."),
                  emb$name, bad),
          "gr_embed_fallback")
}

#' The built-in embeddings-endpoint backend.
#'
#' Batched and cached. Raises on failure: what to do about a failed embedding is
#' the caller's `fallback` policy, not this function's business, and the branch
#' that used to decide it here had to be duplicated for every other path.
#' @noRd
embed_api <- function(texts, params) {
  client <- params$client
  model <- params$model
  cache <- isTRUE(params$cache)
  endpoint <- paste0(as_chr1(client$base_url, "?"), "|",
                     as_chr1(client$.client_id, "<url-addressed>"))
  keys <- vapply(texts, function(t) embed_cache_key(params$embedder, model, t, endpoint),
                 character(1), USE.NAMES = FALSE)
  out <- vector("list", length(texts))
  todo <- seq_along(texts)
  if (cache) {
    hit <- vapply(keys, function(k) !is.null(gr_state$embed_cache[[k]]), logical(1),
                  USE.NAMES = FALSE)
    for (i in which(hit)) out[[i]] <- gr_state$embed_cache[[keys[i]]]
    todo <- which(!hit)
  }

  if (length(todo)) {
    limit <- gr_model_info(model)$context_window
    payload <- vapply(texts[todo], function(t) gr_truncate_tokens(t, max(limit - 16L, 16L), ""),
                      character(1), USE.NAMES = FALSE)
    batch <- as.integer(clamp(params$batch_size %||% 64L, 1, 2048))
    starts <- seq(1, length(todo), by = batch)
    trace <- params$trace
    for (b in seq_along(starts)) {
      start <- starts[b]
      idx <- todo[start:min(start + batch - 1L, length(todo))]
      texts_b <- payload[match(idx, todo)]
      body <- list(model = model, input = as.list(texts_b))
      # Same auth path as gr_call(), deliberately: a gateway that needs an
      # `api-key` header for chat needs it for embeddings too, and two copies of
      # the header logic is how one of them ends up a release behind.
      headers <- request_headers(client)
      if (is.null(headers)) no_credentials_error()
      # An embeddings request is billed like any other request, so it answers
      # to the same limits and goes in the same ledger. It used to do neither:
      # semantic segmentation, retrieve and iterative spent outside
      # max_calls and max_cost_usd (even `max_calls = 0` sent them), and the
      # trace and gr_trace_cost() said the run cost nothing. A refusal raises
      # like any other failure here, so gr_embed()'s `fallback` decides what
      # happens next; cache hits never get this far and stay free.
      if (!trace_can_call(trace)) {
        gr_abort(embed_cap_message(trace, b, length(starts)),
                 class = c(if (identical(cap_name(trace), "spending limit")) "gr_cost_cap"
                           else "gr_call_cap", "gr_embed_error"))
      }
      started <- Sys.time()
      resp <- embed_post(client, paste0(client$base_url, "/embeddings"), headers, body)
      parsed <- if (inherits(resp, "condition") || httr::status_code(resp) >= 300) NULL else
        tryCatch(httr::content(resp, as = "parsed", type = "application/json"),
                 error = function(e) NULL)
      got <- embed_vectors(parsed, length(idx))
      problem <- if (is.null(parsed)) embed_http_problem(resp) else got$problem
      embed_record(trace, model, texts_b, resp, parsed, problem,
                   seconds = as.numeric(difftime(Sys.time(), started, units = "secs")))
      if (!is.null(problem)) {
        gr_abort(sprintf("the request to '%s' failed: %s", model, problem),
                 class = "gr_embed_error")
      }
      for (j in seq_along(idx)) {
        v <- got$vectors[[j]]
        v <- v / sqrt(sum(v^2))
        out[[idx[j]]] <- v
        if (cache) gr_state$embed_cache[[keys[idx[j]]]] <- v
      }
    }
  }
  # One vector space. Rows of different lengths (a cached vector from before
  # the endpoint changed its model, say) used to be padded with zeros to the
  # longest and ranked against each other, which means nothing.
  d <- unique(lengths(out))
  if (length(d) != 1L) {
    gr_abort(sprintf("the embeddings for '%s' have different lengths (%s)", model,
                     paste(sort(d), collapse = ", ")),
             class = "gr_embed_error")
  }
  matrix(unlist(out, use.names = FALSE), nrow = length(out), byrow = TRUE)
}

#' POST one embeddings request, retrying a transient failure as gr_call() does.
#'
#' It used to be one attempt: a single 429 on any batch of a long document sent
#' every row of the matrix to the lexical fallback, although the client's
#' `max_retries` promised otherwise, and the next call, made a moment later,
#' got real vectors. The statuses retried, the backoff and a server's
#' Retry-After are http_call()'s. Returns the last response, or the transport
#' error once the retries are spent.
#' @noRd
embed_post <- function(client, url, headers, body) {
  retries <- as_int1(client$max_retries, 0L)
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    resp <- tryCatch(
      httr::POST(url,
                 httr::content_type_json(),
                 httr::add_headers(.headers = headers),
                 httr::timeout(client$timeout),
                 body = body, encode = "json"),
      error = function(e) e)
    if (inherits(resp, "condition")) {
      if (attempt > retries) return(resp)
      Sys.sleep(backoff_delay(as_num1(client$retry_pause_base, 0), attempt))
      next
    }
    status <- httr::status_code(resp)
    if (!(status %in% .retryable_status) || attempt > retries) return(resp)
    wait <- retry_after(resp) %||% backoff_delay(as_num1(client$retry_pause_base, 0), attempt)
    gr_msg(sprintf("Embeddings request: HTTP %d; retrying in %.1fs (attempt %d/%d).",
                   status, wait, attempt, retries + 1L))
    Sys.sleep(wait)
  }
}

#' Why an embeddings request that got no usable body failed: the transport
#' error, or the HTTP status with the provider's own message, so a rate limit,
#' a bad request and a bad key no longer read alike.
#' @noRd
embed_http_problem <- function(resp) {
  if (inherits(resp, "condition")) return(paste0("Transport error: ", conditionMessage(resp)))
  status <- as_int1(httr::status_code(resp), NA_integer_)
  if (is.na(status) || status < 300L) return("the response was not readable JSON")
  body <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
  msg <- tryCatch({
    js <- jsonlite::parse_json(body)
    err <- if (is.list(js)) js[["error"]] else NULL
    if (is.list(err)) as_chr1(err[["message"]], "") else as_chr1(err, "")
  }, error = function(e) "")
  if (!nzchar(msg)) msg <- trimws(substr(as_chr1(body, ""), 1, 300))
  if (nzchar(msg)) sprintf("HTTP %d: %s", status, msg) else sprintf("HTTP %d", status)
}

#' The vectors in an embeddings response, in the order of the texts sent, or
#' why the response cannot be used.
#'
#' Checked rather than trusted. Each element was taken to be the text at its
#' position, but the API says which text a vector is for in its `index`, and
#' a gateway that gathers a batch out of order gave every text another text's
#' vector with nothing to show for it. An element with no embedding (`null`,
#' `[]`) became an empty vector, padded with zeros: a whole batch of them made
#' an n x 0 matrix that gr_embed() returned as good "api" vectors, and one of
#' them made a text no query could ever reach. Either is a failed request now,
#' and gr_embed()'s `fallback` decides what happens next.
#' @return `list(vectors =)` or `list(problem =)`.
#' @noRd
embed_vectors <- function(parsed, n) {
  data <- if (is.list(parsed)) parsed[["data"]] else NULL
  if (!is.list(data) || !length(data)) return(list(problem = "the response held no embeddings"))
  if (length(data) != n) {
    return(list(problem = sprintf("the response held %d embedding(s) for %d text(s)",
                                  length(data), n)))
  }
  index <- lapply(data, function(el) if (is.list(el)) el[["index"]] else NULL)
  given <- !vapply(index, is.null, logical(1))
  if (any(given)) {
    pos <- vapply(index, function(i) as_num1(i, NA_real_), numeric(1))
    if (!all(given) || anyNA(pos) || !setequal(pos, seq_len(n) - 1L) || anyDuplicated(pos)) {
      return(list(problem = sprintf(paste0("the response's `index` fields are not the numbers ",
                                           "0 to %d, one for each text sent"), n - 1L)))
    }
    data <- data[order(pos)]
  }
  vectors <- lapply(data, function(el) {
    e <- if (is.list(el)) el[["embedding"]] else NULL
    v <- unlist(e, use.names = FALSE)
    # A null inside the array is dropped by unlist(), so the lengths disagree.
    if (!is.numeric(v) || length(v) != length(e)) numeric(0) else as.numeric(v)
  })
  len <- lengths(vectors)
  bad <- len == 0L | !vapply(vectors, function(v) all(is.finite(v)) && any(v != 0), logical(1))
  if (any(bad)) {
    return(list(problem = sprintf(paste0("the response held no usable embedding (missing, empty, ",
                                         "non-numeric or all zeros) for %d of %d text(s)"),
                                  sum(bad), n)))
  }
  if (length(unique(len)) != 1L) {
    return(list(problem = sprintf("the response held embeddings of different lengths (%s)",
                                  paste(sort(unique(len)), collapse = ", "))))
  }
  list(vectors = vectors)
}

#' Record one embeddings request in the trace, priced like a model call.
#'
#' It has no prompt messages, so a replay passes over it (a trace does not
#' record vectors; see gr_embed()), while gr_trace_cost(), `spent_usd` and
#' `as.data.frame()` count it like any other request. Tokens are the provider's
#' `usage.prompt_tokens`, or a local count when it reports none (see
#' settle_usage()), and go to the trace's `embed_tokens`. A failed request is
#' recorded as failed with no tokens, as http_call() records one; gr_embed()
#' marks it recovered when its fallback replaces it.
#' @noRd
embed_record <- function(trace, model, texts, resp, parsed, problem = NULL, seconds = NA_real_) {
  if (!inherits(trace, "gr_trace")) return(invisible(NULL))
  status <- if (inherits(resp, "condition")) 0L
            else if (inherits(resp, "response")) as_int1(httr::status_code(resp), NA_integer_)
            else NA_integer_
  res <- if (is.list(parsed) && !is.null(parsed[["data"]])) {
    ug <- if (is.list(parsed[["usage"]])) parsed[["usage"]] else list()
    usage <- settle_usage(list(input = ug[["prompt_tokens"]] %||% ug[["input_tokens"]],
                               output = 0L),
                          sum(gr_count_tokens(texts)), "")
    # A reply that held embeddings the checks refused was still answered, and
    # billed, so its tokens stay on the step; only its vectors are unusable.
    if (is.null(problem)) gr_result(TRUE, model = model, usage = usage)
    else gr_result(FALSE, model = model, status = status, usage = usage,
                   error = sprintf("Embeddings request to '%s' failed: %s", model, problem))
  } else {
    gr_result(FALSE, model = model, status = status, error = sprintf(
      "Embeddings request to '%s' failed: %s", model,
      problem %||% embed_http_problem(resp)))
  }
  trace_record(trace, "embed.request", list(), res,
               params = list(model = model, texts = length(texts)), seconds = seconds,
               embedding = TRUE)
}

#' Why an embeddings request was not sent, naming the limit and its setting.
#' @noRd
embed_cap_message <- function(trace, i, n) {
  what <- if (n > 1L) sprintf("embeddings request %d of %d", i, n) else "the embeddings request"
  if (identical(cap_name(trace), "spending limit")) {
    sprintf(paste0("the run has spent $%s, which reaches the $%s spending limit, so %s was ",
                   "not sent; raise gr_options(max_cost_usd =)"),
            fmt_usd(budget_spent(trace)), format(gr_options("max_cost_usd"), scientific = FALSE), what)
  } else {
    sprintf("%s would pass the run's %s-call cap, so it was not sent; raise gr_options(max_calls =)",
            what, format(gr_options("max_calls"), scientific = FALSE))
  }
}

#' Key for one cached embedding.
#'
#' The embedder name is part of it. Without that, switching embedders returned
#' the previous one's vectors for the same text and model: two different vector
#' spaces silently mixed in one matrix, and a cosine similarity computed across
#' them means nothing at all.
#' @noRd
embed_cache_key <- function(embedder, model, text, endpoint = "") {
  # The endpoint, because `embedder` is the literal "api" for every
  # URL-addressed client: two clients with different `base_url` and the same
  # model id shared one entry, and the second one's vectors were the first
  # one's. That is the failure the embedder name was added to prevent -- "two
  # different vector spaces silently mixed in one matrix" -- reintroduced one
  # field over, where a cosine similarity is meaningless and looks fine.
  gr_hash(list("readgpt-embed-v2", as_chr1(embedder, "?"), as_chr1(model, "?"),
               as_chr1(endpoint, ""), key_text(as_chr1(text))))
}

#' @noRd
finish_embedding <- function(m, source, trace, n) {
  if (!is.matrix(m)) m <- matrix(m, nrow = n)
  attr(m, "embedding_source") <- source
  trace_note(trace, "embed", list(n = n, dim = ncol(m), source = source))
  m
}

#' Deterministic hashed bag-of-words vectors.
#'
#' An honest fallback: it measures lexical overlap, not meaning. Unlike the code
#' it replaces, it is at least a function of the text's *content*, is
#' deterministic without touching the RNG, and is labelled as non-semantic
#' wherever it is used.
#' @noRd
lexical_embed <- function(texts, dim = 512L) {
  m <- t(vapply(texts, function(tx) {
    w <- lexical_terms(tx, min_chars = 3L)
    v <- numeric(dim)
    if (!length(w)) return(v)
    tf <- table(w)
    for (i in seq_along(tf)) {
      h <- strtoi(substr(gr_hash(names(tf)[i]), 1, 6), 16L)
      slot <- (h %% dim) + 1L
      v[slot] <- v[slot] + (1 + log(as.numeric(tf[i])))
    }
    n <- sqrt(sum(v^2)); if (n > 0) v / n else v
  }, numeric(dim), USE.NAMES = FALSE))
  m
}

#' Cosine similarity of one vector against every row of a matrix.
#' @noRd
cosine_against <- function(mat, vec) {
  if (!is.matrix(mat) || nrow(mat) == 0L) return(numeric(0))
  vec <- as.numeric(vec)
  n <- min(ncol(mat), length(vec))
  if (n == 0L) return(rep(0, nrow(mat)))
  mat <- mat[, seq_len(n), drop = FALSE]; vec <- vec[seq_len(n)]
  dv <- sqrt(sum(vec^2))
  dm <- sqrt(rowSums(mat^2))
  out <- as.numeric(mat %*% vec)
  denom <- dm * dv
  out[denom == 0 | !is.finite(denom)] <- 0
  ok <- denom > 0 & is.finite(denom)
  out[ok] <- out[ok] / denom[ok]
  out
}

#' Okapi BM25 scores for a query against a set of documents.
#'
#' Pure R, no API cost. Used by the `rerank` reader as a cheap prefilter and as
#' the offline path for `retrieve` when embeddings are unavailable.
#' @noRd
bm25_scores <- function(docs, query, k1 = 1.5, b = 0.75) {
  norm <- function(x) lexical_terms(x, min_chars = 2L)
  dt <- lapply(docs, norm)
  q <- unique(norm(query))
  if (!length(q) || !length(dt)) return(rep(0, length(docs)))
  lens <- vapply(dt, length, integer(1))
  avg <- mean(lens[lens > 0]); if (!is.finite(avg) || avg == 0) avg <- 1
  N <- length(dt)
  vapply(seq_along(dt), function(i) {
    d <- dt[[i]]
    if (!length(d)) return(0)
    tf <- table(d)
    sum(vapply(q, function(term) {
      f <- as.numeric(tf[term]); if (is.na(f)) return(0)
      n_q <- sum(vapply(dt, function(dd) term %in% dd, logical(1)))
      idf <- log(1 + (N - n_q + 0.5) / (n_q + 0.5))
      idf * (f * (k1 + 1)) / (f + k1 * (1 - b + b * lens[i] / avg))
    }, numeric(1)))
  }, numeric(1))
}

#' The terms of a text for word matching: lowercased runs of letters, marks and
#' digits, in any script.
#'
#' Unicode classes, not POSIX ones. Without `(*UCP)`, PCRE's `[:alnum:]`
#' matches ASCII only, so the old `[^[:alnum:][:space:]]` blanked every letter
#' outside A-Z: a Russian, Greek, Arabic or Chinese document had no terms at
#' all, BM25 scored every chunk 0 and lexical vectors were all zero, so rerank
#' and retrieve picked chunks in document order without saying so, and the
#' German word for "size", with its umlaut and sharp s, became "Gr" and "e".
#' `\p{M}` is in the class because Devanagari, Thai, Arabic and Hebrew write
#' vowels and diacritics as combining marks, and blanking those broke every
#' word apart. For ASCII text nothing changes.
#'
#' Scripts written without spaces between words (Chinese, Japanese, Thai, Lao,
#' Khmer, Myanmar) give one "word" per clause, which matches nothing, so a run
#' of those characters is indexed as overlapping character pairs, the usual
#' unit for matching such text without a dictionary. The pairs are kept
#' whatever `min_chars` says: the floor is there to drop short function words
#' in space-separated scripts.
#' @noRd
lexical_terms <- function(x, min_chars = 2L) {
  x <- gsub("[^\\p{L}\\p{M}\\p{N}\\s]", " ", to_utf8(as_chr1(x)), perl = TRUE)
  w <- lower_text(words_of(x))
  if (!length(w)) return(character(0))
  dense_script <- "\\p{Han}\\p{Hiragana}\\p{Katakana}\\p{Thai}\\p{Lao}\\p{Khmer}\\p{Myanmar}"
  # A run starts with a character of such a script and carries on through
  # modifier letters and marks (the Japanese long-vowel mark, Thai tone marks).
  run_re <- sprintf("[%s][%s\\p{Lm}\\p{M}]*", dense_script, dense_script)
  dense <- grepl(sprintf("[%s]", dense_script), w, perl = TRUE)
  out <- w[!dense & nchar(w) >= min_chars]
  if (any(dense)) {
    d <- w[dense]
    runs <- unlist(regmatches(d, gregexpr(run_re, d, perl = TRUE)), use.names = FALSE)
    rest <- unlist(lapply(gsub(run_re, " ", d, perl = TRUE), words_of), use.names = FALSE)
    pairs <- unlist(lapply(strsplit(runs, "", fixed = TRUE), function(ch) {
      if (length(ch) < 2L) ch else paste0(ch[-length(ch)], ch[-1L])
    }), use.names = FALSE)
    out <- c(out, rest[nchar(rest) >= min_chars], pairs)
  }
  # Labelled consistently: lexical_embed() hashes each term, and a hash of the
  # same characters differs by encoding label (see key_text()).
  mark_utf8(out)
}
