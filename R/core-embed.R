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
#' @param trace Optional trace.
#' @param fallback What to do when the embedding request fails. **Defaults to
#'   `"lexical"`**: hashed bag-of-words vectors that measure word overlap, not
#'   meaning, so `semantic` segmentation and `retrieve` ranking become markedly
#'   less accurate. The substitution warns and is recorded, but the run
#'   continues. Use `"error"` to fail fast, or `"none"` to get an empty matrix.
#' @return A numeric matrix, one row per input, carrying an `"embedding_source"`
#'   attribute of `"api"` or `"lexical"` -- always check it before treating the
#'   rows as semantic. Rows from the API and lexical paths are L2-normalised.
#'   With `fallback = "none"` and a failed request the result is a 0 x 0 matrix.
#' @export
#' @seealso [gr_client()], [gr_segment_spec()] for `method = "semantic"`,
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
#' gr_segment(gptread_example(), list(method = "semantic", max_tokens = 200),
#'            client = cl)$extra$embedding_source
gr_embed <- function(client, texts, model = NULL, batch_size = 64L, cache = NULL,
                     trace = NULL, fallback = c("lexical", "error", "none")) {
  fallback <- match.arg(fallback)
  texts <- vapply(texts %||% character(0), as_chr1, character(1), USE.NAMES = FALSE)
  if (!length(texts)) return(matrix(numeric(0), nrow = 0, ncol = 0))
  cache <- isTRUE(cache %||% gr_options("cache_embeddings"))
  model <- as_chr1(model %||% client$embedding_model)

  if (inherits(client, "gr_mock_client")) {
    m <- client$embed_handler(texts, list(model = model))
    client$.log$embeds <- c(client$.log$embeds, list(list(texts = texts, model = model)))
    return(finish_embedding(m, "api", trace, length(texts)))
  }

  keys <- vapply(texts, function(t) gr_hash(list(model, t)), character(1), USE.NAMES = FALSE)
  out <- vector("list", length(texts))
  todo <- seq_along(texts)
  if (cache) {
    hit <- vapply(keys, function(k) !is.null(gr_state$embed_cache[[k]]), logical(1), USE.NAMES = FALSE)
    for (i in which(hit)) out[[i]] <- gr_state$embed_cache[[keys[i]]]
    todo <- which(!hit)
  }

  if (length(todo)) {
    limit <- gr_model_info(model)$context_window
    payload <- vapply(texts[todo], function(t) gr_truncate_tokens(t, max(limit - 16L, 16L), ""),
                      character(1), USE.NAMES = FALSE)
    ok <- TRUE
    for (start in seq(1, length(todo), by = batch_size)) {
      idx <- todo[start:min(start + batch_size - 1L, length(todo))]
      body <- list(model = model, input = as.list(payload[match(idx, todo)]))
      key <- tryCatch(gr_api_key(client$api_key), error = function(e) NULL)
      resp <- if (is.null(key)) NULL else tryCatch(
        httr::POST(paste0(client$base_url, "/embeddings"),
                   httr::add_headers(Authorization = paste("Bearer", key)),
                   httr::content_type_json(), httr::timeout(client$timeout),
                   body = body, encode = "json"),
        error = function(e) e)
      parsed <- if (is.null(resp) || inherits(resp, "condition") ||
                    httr::status_code(resp) >= 300) NULL else
        tryCatch(httr::content(resp, as = "parsed", type = "application/json"),
                 error = function(e) NULL)
      if (is.null(parsed$data)) { ok <- FALSE; break }
      for (j in seq_along(idx)) {
        v <- as.numeric(unlist(parsed$data[[j]]$embedding))
        n <- sqrt(sum(v^2)); if (n > 0) v <- v / n
        out[[idx[j]]] <- v
        if (cache) gr_state$embed_cache[[keys[idx[j]]]] <- v
      }
    }
    if (!ok) {
      msg <- sprintf("Embedding request to '%s' failed.", model)
      if (identical(fallback, "error")) gr_abort(msg, class = "gr_embed_error")
      if (identical(fallback, "none")) return(matrix(numeric(0), nrow = 0, ncol = 0))
      gr_warn(paste0(msg, " Falling back to hashed lexical vectors: these approximate ",
                     "word overlap, not meaning, so semantic segmentation and top-k ",
                     "retrieval will be markedly less accurate."), class = "gr_embed_fallback")
      m <- lexical_embed(texts)
      return(finish_embedding(m, "lexical", trace, length(texts)))
    }
  }
  d <- max(vapply(out, function(v) length(v %||% numeric(0)), integer(1)))
  m <- t(vapply(out, function(v) {
    v <- as.numeric(v %||% numeric(0))
    if (length(v) < d) v <- c(v, rep(0, d - length(v)))
    v[seq_len(d)]
  }, numeric(d), USE.NAMES = FALSE))
  finish_embedding(m, "api", trace, length(texts))
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
    w <- tolower(words_of(gsub("[^[:alnum:][:space:]]", " ", as_chr1(tx), perl = TRUE)))
    w <- w[nchar(w) > 2L]
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
  norm <- function(x) {
    w <- tolower(words_of(gsub("[^[:alnum:][:space:]]", " ", as_chr1(x), perl = TRUE)))
    w[nchar(w) > 1L]
  }
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
