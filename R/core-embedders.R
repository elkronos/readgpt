# core-embedders.R -- the sixth registry.
#
# WHY THIS FILE EXISTS
# Extractors, cleaners, segmenters, readers and models are all registries: named
# entries you can list, inspect and add to without editing the package.
# Embedding was the one axis that was not. `gr_embed()` had a chain of
# `inherits()` branches -- mock here, replay there, backend somewhere else, HTTP
# at the end -- which meant adding a local model was a change to this package
# rather than a call in your own script, and there was no way to ask what was
# available.
#
# The registry also carries something a branch cannot: whether an embedder is
# DETERMINISTIC. That single flag is what closes the replay gap. A trace records
# model calls, not embedding vectors, so replaying a run that embedded through
# an API cannot reproduce its ranking. Replaying a run that embedded with a
# deterministic embedder reproduces it exactly -- the vectors are a pure
# function of the text, so they can simply be computed again. Before this, every
# replay of a `retrieve` run warned and degraded, including the ones that did
# not need to.

#' Register an embedding backend
#'
#' Embedding is the sixth registry, alongside extractors, cleaners, segmenters,
#' readers and models. Register a function and every part of the package that
#' embeds -- the `semantic` segmenter, the `retrieve` and `iterative` readers --
#' uses it, with no change to any of them.
#'
#' @param name Short id, used as the value of `gr_options(embedder =)` and as
#'   the `"embedding_source"` recorded on the result.
#' @param fn Function of `(texts, params)` returning a numeric matrix with one
#'   row per input. `params` carries `client`, `model`, `batch_size`, `cache`
#'   and `trace`. Rows should be L2-normalised: everything downstream treats the
#'   cross-product as cosine similarity. Signal failure by raising -- the caller
#'   applies its own `fallback` policy, which is not the embedder's business.
#' @param description One line, shown by [gr_embedders()].
#' @param deterministic `TRUE` if the same text always gives the same vector in
#'   any session on any machine. **Say so only if it is true.** It is what lets a
#'   [gr_replay_client()] reproduce a run's chunk ranking instead of degrading to
#'   lexical vectors, and claiming it wrongly turns a replay from a recording
#'   into a plausible-looking fiction.
#' @return The name, invisibly.
#' @seealso [gr_embedders()], [gr_embed()], [gr_options()] for `embedder`,
#'   [gr_backend_client()] to supply an embedder alongside a transport
#' @export
#' @examples
#' # A local model, a company service, anything: it is a function of texts.
#' gr_register_embedder("first-letters",
#'   fn = function(texts, params) {
#'     m <- t(vapply(texts, function(tx) {
#'       v <- numeric(26)
#'       ltr <- utf8ToInt(tolower(substr(tx, 1, 40))) - 96L
#'       for (i in ltr[ltr >= 1 & ltr <= 26]) v[i] <- v[i] + 1
#'       n <- sqrt(sum(v^2)); if (n == 0) v else v / n
#'     }, numeric(26), USE.NAMES = FALSE))
#'     m
#'   },
#'   description = "Toy: letter counts", deterministic = TRUE)
#'
#' gr_embedders()
#'
#' old <- gr_options(embedder = "first-letters")
#' attr(gr_embed(gr_client(), c("alpha", "beta")), "embedding_source")
#' gr_options(old)
gr_register_embedder <- function(name, fn, description = "", deterministic = FALSE) {
  if (!is.function(fn)) gr_abort("`fn` must be a function of (texts, params).")
  registry_set("embedders", name, list(name = name, fn = fn,
                                       description = description,
                                       deterministic = isTRUE(deterministic)))
}

#' List registered embedding backends
#'
#' @return A data frame with one row per embedder: `name`, `deterministic`
#'   (`"TRUE"`/`"FALSE"`) and `description`. `deterministic` is the column that
#'   matters for reproducibility: only a deterministic embedder lets a
#'   [gr_replay_client()] reproduce a run's chunk ranking.
#' @seealso [gr_register_embedder()], [gr_embed()], [gr_replay_client()]
#' @export
#' @examples
#' gr_embedders()
gr_embedders <- function() registry_table("embedders", c("deterministic", "description"))

#' Which embedder a call should use.
#'
#' Order: an explicit argument, then the `embedder` option, then an embed
#' function carried by the client, then `"api"`.
#'
#' The option's default is `NULL`, meaning "whatever the client brought", and
#' that is deliberate. An earlier version instead asked whether the option had
#' been *set*, so that a user-set value could beat a handler `gr_mock_client()`
#' manufactures whether or not you asked for one. That distinction cannot
#' survive `gr_options(gr_options())` -- restoring a saved option list, which is
#' exactly what the test helpers and `on.exit()` handlers do -- because the
#' restore sets every option explicitly. The meaning has to live in the value,
#' not in whether someone assigned it.
#' @noRd
resolve_embedder <- function(client, embedder = NULL) {
  if (!is.null(embedder)) {
    if (is.function(embedder)) {
      return(list(name = "custom", fn = embedder, deterministic = FALSE))
    }
    return(registry_get("embedders", as_chr1(embedder), "embedder"))
  }
  opt <- gr_options("embedder")
  if (!is.null(opt) && is_nonblank(as_chr1(opt))) {
    return(registry_get("embedders", as_chr1(opt), "embedder"))
  }
  if (is.function(client$embed_handler)) {
    # Reported as "api" whatever supplied it: the distinction that matters
    # downstream is semantic vectors versus the lexical fallback, and a
    # client-supplied embedder is on the semantic side of it.
    return(list(name = "api", deterministic = FALSE,
                fn = function(texts, params) {
                  m <- client$embed_handler(texts, list(model = params$model))
                  if (!is.null(client$.log)) {
                    client$.log$embeds <- c(client$.log$embeds,
                                            list(list(texts = texts, model = params$model)))
                  }
                  m
                }))
  }
  registry_get("embedders", "api", "embedder")
}

#' @noRd
register_builtin_embedders <- function() {
  gr_register_embedder(
    "api", fn = embed_api, deterministic = FALSE,
    description = "Embeddings endpoint on the client's base URL")
  gr_register_embedder(
    "lexical", fn = function(texts, params) lexical_embed(texts), deterministic = TRUE,
    description = "Hashed bag-of-words; free, offline, word overlap not meaning")
  invisible(NULL)
}
