# test-embedders.R -- the sixth registry, and the reproducibility claim it buys.
#
# The registry itself is ordinary. What is worth testing carefully is the claim
# it makes possible: that a replay can reproduce a run's chunk ranking. That
# claim has TWO conditions -- the embedder must be deterministic AND it must be
# the one the recording used -- and getting only the first right would produce
# the worst kind of bug: a replay that reports itself exact while ranking chunks
# by vectors the original run never saw.

toy_embedder <- function(dim = 8L) {
  function(texts, params) {
    t(vapply(texts, function(tx) {
      v <- numeric(dim)
      b <- utf8ToInt(as_chr1_test(substr(tx, 1, 60)))
      for (i in b) v[(i %% dim) + 1L] <- v[(i %% dim) + 1L] + 1
      n <- sqrt(sum(v^2)); if (n == 0) v else v / n
    }, numeric(dim), USE.NAMES = FALSE))
  }
}
as_chr1_test <- function(x) { x <- as.character(x)[1]; if (is.na(x)) " " else x }

test_that("the built-ins are registered and listed", {
  reg <- gr_embedders()
  expect_true(all(c("api", "lexical") %in% reg$name))
  expect_identical(reg$deterministic[reg$name == "lexical"], "TRUE")
  expect_identical(reg$deterministic[reg$name == "api"], "FALSE")
  expect_true(all(nzchar(reg$description)))
})

test_that("a registered embedder is used and names itself on the result", {
  local_registries()
  gr_register_embedder("toy", fn = toy_embedder(), description = "toy",
                       deterministic = TRUE)
  expect_true("toy" %in% gr_embedders()$name)

  e <- gr_embed(gr_client(api_key = "sk-x"), c("alpha text", "beta text"), embedder = "toy")
  expect_identical(nrow(e), 2L)
  expect_identical(ncol(e), 8L)
  expect_identical(attr(e, "embedding_source"), "toy")
  # Rows are unit vectors, so the cross-product is cosine similarity.
  expect_equal(sqrt(rowSums(e^2)), c(1, 1), tolerance = 1e-8)
})

test_that("an embedder can be chosen by option, argument, or function", {
  local_registries()
  gr_register_embedder("toy", fn = toy_embedder(), deterministic = TRUE)
  cl <- gr_client(api_key = "sk-x")

  gr_options(embedder = "toy")
  expect_identical(attr(gr_embed(cl, "a"), "embedding_source"), "toy")

  # An explicit argument beats the option.
  expect_identical(attr(gr_embed(cl, "a", embedder = "lexical"), "embedding_source"),
                   "lexical")
  # And a bare function works without registering anything.
  e <- gr_embed(cl, c("a", "b"), embedder = toy_embedder(4L))
  expect_identical(ncol(e), 4L)
  expect_identical(attr(e, "embedding_source"), "custom")
})

test_that("an unregistered embedder is an error naming what is available", {
  local_registries()
  expect_error(gr_embed(gr_client(api_key = "sk-x"), "a", embedder = "nope"),
               class = "gr_unknown_method")
  expect_error(gr_register_embedder("bad", fn = "not a function"), "must be a function")
})

test_that("naming an embedder beats an embed handler nobody asked for", {
  # gr_mock_client() manufactures an embed handler whether or not you asked for
  # one, so if a client handler outranked the option then gr_options(embedder =)
  # would be silently ignored for every mock client -- which is exactly the
  # client you use when you want a deterministic offline run. The option's
  # default is NULL, meaning "whatever the client brought", so naming one is
  # always an override and the precedence does not depend on whether anyone
  # happened to assign the option.
  local_registries()
  cl <- gr_mock_client()
  expect_identical(attr(gr_embed(cl, c("a", "b")), "embedding_source"), "api")

  gr_options(embedder = "lexical")
  expect_identical(attr(gr_embed(cl, c("a", "b")), "embedding_source"), "lexical")
})

test_that("a client's own embedder is still used when no option is set", {
  local_registries()
  seen <- 0L
  cl <- gr_backend_client(function(m, p) "x",
                          embed = function(texts, params) { seen <<- seen + 1L
                            matrix(1 / sqrt(4), nrow = length(texts), ncol = 4L) })
  e <- gr_embed(cl, c("a", "b"))
  expect_identical(seen, 1L)
  expect_identical(attr(e, "embedding_source"), "api")
  expect_length(cl$embeds(), 1L)
})

test_that("a failing or wrong-shaped embedder degrades, and says which one", {
  local_registries()
  gr_register_embedder("raiser", fn = function(texts, params) stop("service down"))
  gr_register_embedder("wrong", fn = function(texts, params) matrix(1, nrow = 99L, ncol = 4L))
  cl <- gr_client(api_key = "sk-x")

  expect_warning(e <- gr_embed(cl, c("a", "b"), embedder = "raiser"),
                 class = "gr_embed_fallback")
  expect_identical(attr(e, "embedding_source"), "lexical")
  expect_identical(nrow(e), 2L)

  expect_warning(w <- gr_embed(cl, c("a", "b"), embedder = "wrong"),
                 class = "gr_embed_fallback")
  expect_identical(nrow(w), 2L)

  expect_error(gr_embed(cl, "a", embedder = "raiser", fallback = "error"),
               class = "gr_embed_error")
  expect_identical(nrow(gr_embed(cl, "a", embedder = "raiser", fallback = "none")), 0L)
})

test_that("the embedding cache is keyed on the embedder, not only the model", {
  # Without the embedder in the key, switching embedders returned the previous
  # one's vectors for the same text and model: two different vector spaces
  # silently mixed in one matrix, and a cosine similarity across them means
  # nothing.
  #
  # This asserts on the key itself. Only the built-in "api" embedder caches, and
  # exercising that needs an embeddings endpoint, so the round trip is not
  # reachable offline -- but the property that matters is that the key separates
  # embedders, and that is checkable here. The first version of this test called
  # gr_embed() with two toy embedders and passed whatever the key did, because
  # toy embedders never touch the cache at all.
  key <- readgpt:::embed_cache_key
  expect_false(identical(key("eight", "m", "the same text"),
                         key("four",  "m", "the same text")))
  expect_false(identical(key("e", "model-a", "t"), key("e", "model-b", "t")))
  expect_false(identical(key("e", "m", "text one"), key("e", "m", "text two")))
  expect_identical(key("e", "m", "same"), key("e", "m", "same"))
  # And it does not depend on how the text happens to be labelled, for the same
  # reason the response-cache key does not.
  bare <- unmarked("caf\u00e9 r\u00e9sum\u00e9")
  marked <- "caf\u00e9 r\u00e9sum\u00e9"; Encoding(marked) <- "UTF-8"
  expect_identical(key("e", "m", bare), key("e", "m", marked))
})

# ---------------------------------------------------------------------------
# What the registry buys: an exact replay
# ---------------------------------------------------------------------------

# Record a retrieve run, then replay it, reporting what happened. `NULL` means
# "leave the option alone", which is how you get a recording that used the mock
# client's own embedder (reported as "api").
#
# suppressMessages, never quiet(): quiet() calls suppressWarnings, whose handler
# is established INSIDE this one and therefore muffles every warning before the
# withCallingHandlers below can see it -- so the warnings under test would all
# come back empty and every assertion here would pass vacuously.
replay_round_trip <- function(recorded_with, replayed_with) {
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  if (!is.null(recorded_with)) gr_options(embedder = recorded_with)
  original <- suppressMessages(suppressWarnings(
    answer_document(readgpt_example(), "What was revenue?", "needle", client = cl)))
  if (!is.null(replayed_with)) gr_options(embedder = replayed_with)
  warned <- character(0)
  out <- withCallingHandlers(
    tryCatch(suppressMessages(
               answer_document(readgpt_example(), "What was revenue?", "needle",
                               client = gr_replay_client(original$trace))),
             gr_replay_miss = function(e) "MISS"),
    warning = function(w) { warned <<- c(warned, class(w)[1]); invokeRestart("muffleWarning") })
  list(original = original, replayed = out, warned = unique(warned),
       recorded_source = readgpt:::replay_embed_source(original$trace))
}

test_that("a run recorded and replayed with the same deterministic embedder is exact", {
  local_registries()
  r <- replay_round_trip("lexical", "lexical")
  expect_identical(r$recorded_source, "lexical")
  expect_false("gr_replay_no_embeddings" %in% r$warned)
  expect_identical(r$replayed$answer, r$original$answer)
  expect_identical(r$replayed$chunks_used, r$original$chunks_used)
})

test_that("a run replayed with a DIFFERENT embedder does not claim to be exact", {
  # The dangerous case. Determinism alone is not enough: replaying an
  # API-embedded run with a deterministic local embedder would reproduce nothing
  # while reporting itself reproducible.
  local_registries()
  # No option set, so the mock client's own embedder records as "api".
  r <- replay_round_trip(NULL, "lexical")
  expect_identical(r$recorded_source, "api")
  expect_true("gr_replay_no_embeddings" %in% r$warned)
})

test_that("a non-deterministic embedder never counts as reproducible", {
  local_registries()
  gr_register_embedder("shifty", fn = toy_embedder(), deterministic = FALSE,
                       description = "claims nothing")
  r <- replay_round_trip("shifty", "shifty")
  expect_identical(r$recorded_source, "shifty")
  expect_true("gr_replay_no_embeddings" %in% r$warned)
})

test_that("changing the embedder between record and replay is caught, not papered over", {
  # Different vectors rank different chunks, so the reader sends a prompt the
  # recording never saw. Strict replay refuses rather than inventing an answer:
  # a wrong answer that looks like the original is the worst possible outcome.
  local_registries()
  gr_register_embedder("toy", fn = toy_embedder(), deterministic = TRUE)
  r <- replay_round_trip("toy", "lexical")
  expect_true("gr_replay_no_embeddings" %in% r$warned)
  expect_identical(r$replayed, "MISS")
})

test_that("a client with no way to embed says so specifically", {
  local_registries()
  cl <- gr_backend_client(function(m, p) "x")
  expect_warning(e <- gr_embed(cl, c("a", "b")), class = "gr_backend_no_embeddings")
  expect_identical(attr(e, "embedding_source"), "lexical")

  # But not when an embedder has been chosen: then there is nothing to warn about.
  gr_options(embedder = "lexical")
  expect_no_warning(e2 <- gr_embed(cl, c("a", "b")))
  expect_identical(attr(e2, "embedding_source"), "lexical")
})

test_that("registering an embedder does not disturb the other registries", {
  local_registries()
  before <- vapply(list(gr_extractors(), gr_cleaners(), gr_segmenters(), gr_readers()),
                   nrow, integer(1))
  gr_register_embedder("toy", fn = toy_embedder())
  after <- vapply(list(gr_extractors(), gr_cleaners(), gr_segmenters(), gr_readers()),
                  nrow, integer(1))
  expect_identical(before, after)
})
