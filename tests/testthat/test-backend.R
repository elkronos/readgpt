# test-backend.R -- the transport seam, and the ellmer adapter built on it.
#
# The point of gr_backend_client() is that a stranger's function can be the
# transport and nothing downstream notices. So the tests that matter are not
# "does the handler get called" -- they are: does everything the package builds
# on top of a call still hold when the call came from somewhere else? Budgeting,
# caps, traces, caching, replay, comparison, and above all the gr_result
# invariants, which every reader relies on and no third-party handler has ever
# heard of.

# Every test here talks to a model id of its own invention, and an unregistered
# id warns (correctly -- a guessed context window is what mis-sizes chunks). Give
# them a registered one so the suite stays warning-free and the warnings that do
# appear mean something.
local_backend_model <- function(id = "bk", env = parent.frame()) {
  local_registries(env)
  gr_register_model(id, context_window = 100000L, max_output = 4096L,
                    input_usd = 1, output_usd = 2)
  id
}

test_that("a handler receives the request and its resolved parameters", {
  local_backend_model()
  seen <- NULL
  cl <- gr_backend_client(function(messages, params) { seen <<- list(messages, params); "ok" },
                          model = "bk")
  invisible(gr_call(cl, list(list(role = "system", content = "be terse"),
                             list(role = "user", content = "the question")),
                    max_output = 128L, temperature = 0.4))

  expect_length(seen[[1]], 2L)
  expect_identical(seen[[1]][[1]]$role, "system")
  expect_identical(seen[[2]]$model, "bk")
  expect_identical(seen[[2]]$max_output, 128L)
  expect_identical(seen[[2]]$temperature, 0.4)
  expect_gt(seen[[2]]$prompt_tokens, 0L)
  # The schema reaches the handler: a backend cannot honour structured output
  # without it, and the built-in client is the only other thing that sees it.
  expect_true("schema" %in% names(seen[[2]]))
})

test_that("a backend handler is held to the same gr_result invariants as everything else", {
  local_backend_model()
  mk <- function(f) gr_backend_client(f, model = "bk")

  # A raiser is data, not an exception: one bad chunk must not abort a run.
  bad <- gr_call(mk(function(m, p) stop("provider exploded")), "q")
  expect_false(bad$ok)
  expect_identical(bad$text, "")
  expect_match(bad$error, "provider exploded")

  # An empty completion is a failure, so "" is never spliced downstream as
  # evidence -- the same rule the real client and the mock follow.
  expect_false(gr_call(mk(function(m, p) ""), "q")$ok)
  expect_false(gr_call(mk(function(m, p) "   "), "q")$ok)

  # A handler returning a malformed gr_result is re-normalised, not trusted.
  loose <- gr_call(mk(function(m, p) readgpt:::gr_result(TRUE, text = "fine")), "q")
  expect_true(loose$ok)
  expect_identical(loose$text, "fine")
  expect_length(loose$text, 1L)

  # Text is always character(1), whatever the handler returned.
  expect_length(gr_call(mk(function(m, p) c("two", "strings")), "q")$text, 1L)
})

test_that("everything built on top of a call still works when the call is a backend", {
  local_backend_model()
  cl <- gr_backend_client(function(m, p) "Revenue was 45.2 million dollars.", model = "bk")

  ans <- quiet(answer_document(readgpt_example(), "What was revenue?", "thorough", client = cl))
  expect_false(ans$partial)
  expect_match(ans$answer, "45.2")

  s <- gr_trace_summary(ans$trace)
  expect_gt(s$calls, 0L)
  expect_gt(s$tokens_in, 0L)                    # budgeting saw real numbers
  expect_identical(s$cached, 0L)
  expect_length(cl$calls(), s$calls)            # and the backend saw the same calls
})

test_that("a backend composes with the cache and with replay", {
  local_backend_model()
  n <- 0L
  cache <- gr_cache(withr::local_tempdir())
  cl <- gr_cache_client(gr_backend_client(function(m, p) { n <<- n + 1L; "42" },
                                          model = "bk"), cache)
  expect_false(gr_call(cl, "q")$cached)
  expect_true(gr_call(cl, "q")$cached)
  expect_identical(n, 1L)

  tr <- gr_trace()
  invisible(gr_call(gr_backend_client(function(m, p) "recorded", model = "bk"),
                    "q2", trace = tr))
  expect_identical(gr_call(gr_replay_client(tr), "q2")$text, "recorded")
})

test_that("the cost and call rails apply to a backend as they do to anything else", {
  local_backend_model()
  n <- 0L
  cl <- gr_backend_client(function(m, p) { n <<- n + 1L; "x" }, model = "bk")
  ch <- gr_segment(gr_ingest(sample_doc(4, 4)), list(method = "paragraph", max_tokens = 120))
  expect_gt(nrow(ch$chunks), 3L)

  # The call cap is a PRE-FLIGHT refusal, not a mid-run degradation: the run is
  # refused before any money is spent, and the error names the option to change.
  gr_options(max_calls = 2L)
  expect_error(quiet(gr_read(ch, "Q?", cl, "map_reduce")), class = "gr_call_cap")
  expect_identical(n, 0L)

  # And the cost rail, which needs the registered per-token prices above.
  gr_options(max_calls = 400L, max_cost_usd = 1e-9)
  expect_error(quiet(gr_read(ch, "Q?", cl, "map_reduce")), class = "gr_cost_cap")
  expect_identical(n, 0L)
})

test_that("gr_backend_client validates what it is given", {
  local_registries()
  expect_error(gr_backend_client("not a function"), "must be a function")
  expect_error(gr_backend_client(function(m, p) "x", embed = "not a function"),
               "must be a function")
  cl <- gr_backend_client(function(m, p) "x")
  expect_s3_class(cl, "gr_client")
  expect_output(print(cl), "gr_backend_client")
})

# ---------------------------------------------------------------------------
# Embeddings. A backend that cannot embed must say so; a backend that embeds
# badly must be caught before the ranking maths, not after.
# ---------------------------------------------------------------------------

test_that("a backend without an embedder degrades loudly, not silently", {
  local_registries()
  cl <- gr_backend_client(function(m, p) "x")
  expect_warning(e <- gr_embed(cl, c("alpha", "beta")), class = "gr_backend_no_embeddings")
  expect_identical(nrow(e), 2L)
  expect_identical(attr(e, "embedding_source"), "lexical")
  expect_error(gr_embed(cl, "alpha", fallback = "error"), class = "gr_embed_error")
})

test_that("a supplied embedder is used, and recorded", {
  local_registries()
  cl <- gr_backend_client(function(m, p) "x",
                          embed = function(texts, params)
                            matrix(seq_len(length(texts) * 4L), nrow = length(texts)))
  e <- gr_embed(cl, c("alpha", "beta", "gamma"))
  expect_identical(nrow(e), 3L)
  expect_identical(attr(e, "embedding_source"), "api")
  expect_length(cl$embeds(), 1L)
})

test_that("an embedder returning the wrong shape is caught before it corrupts ranking", {
  # One row per text, or the ranking silently associates each chunk with another
  # chunk's vector and the answer looks perfectly fine.
  local_registries()
  wrong <- gr_backend_client(function(m, p) "x",
                             embed = function(texts, params) matrix(1, nrow = 99L, ncol = 4L))
  expect_warning(e <- gr_embed(wrong, c("a", "b")), class = "gr_embed_fallback")
  expect_identical(nrow(e), 2L)
  expect_identical(attr(e, "embedding_source"), "lexical")

  raiser <- gr_backend_client(function(m, p) "x",
                              embed = function(texts, params) stop("embedding service down"))
  expect_warning(gr_embed(raiser, c("a", "b")), class = "gr_embed_fallback")
  expect_error(gr_embed(raiser, "a", fallback = "error"), class = "gr_embed_error")
})

# ---------------------------------------------------------------------------
# The ellmer adapter.
#
# ellmer is a Suggests, and the parts of the adapter that call into it can only
# be tested where it is installed. The part most likely to be WRONG, though, is
# the mapping from this package's list of roles onto ellmer's single system
# prompt plus single turn -- and that is pure, so it is tested unconditionally.
# ---------------------------------------------------------------------------

test_that("messages map onto ellmer's system prompt and turn", {
  split <- readgpt:::ellmer_split_messages

  one <- split(list(list(role = "user", content = "just a question")))
  expect_identical(one$system, "")
  expect_identical(one$user, "just a question")

  two <- split(list(list(role = "system", content = "be terse"),
                    list(role = "user", content = "the question")))
  expect_identical(two$system, "be terse")
  expect_identical(two$user, "the question")

  # "developer" is a system message as far as any provider is concerned.
  dev <- split(list(list(role = "developer", content = "rules"),
                    list(role = "user", content = "q")))
  expect_identical(dev$system, "rules")

  # Several user messages -- which is what the rerank and iterative readers send
  # -- become one turn, in order, not the first or the last.
  many <- split(list(list(role = "system", content = "S"),
                     list(role = "user", content = "A"),
                     list(role = "user", content = "B")))
  expect_identical(many$user, "A\n\nB")

  # A prompt of nothing but system messages must still send something, or the
  # provider gets an empty request and answers a question nobody asked.
  only <- split(list(list(role = "system", content = "S1"),
                     list(role = "system", content = "S2")))
  expect_identical(only$user, "S1\n\nS2")
  expect_identical(only$system, "")
})

test_that("token usage is read from ellmer when it can be, and counted locally when not", {
  usage <- readgpt:::ellmer_usage
  params <- list(prompt_tokens = 100L)

  from_chat <- usage(list(get_tokens = function() data.frame(input = 7, output = 11)),
                     params, "some text")
  expect_identical(from_chat$input, 7L)
  expect_identical(from_chat$output, 11L)

  # ellmer's column names are ellmer's to change. An unrecognised frame must
  # fall back to local counting, never to zero -- a zero would silently zero out
  # every cost estimate and budget decision downstream.
  odd <- usage(list(get_tokens = function() data.frame(whatever = 1)), params, "some text")
  expect_identical(odd$input, 100L)
  expect_gt(odd$output, 0L)

  broken <- usage(list(get_tokens = function() stop("no")), params, "some text")
  expect_identical(broken$input, 100L)
  expect_gt(broken$output, 0L)
})

test_that("gr_ellmer_client reports a missing ellmer clearly", {
  skip_if(requireNamespace("ellmer", quietly = TRUE),
          "ellmer is installed; this asserts the message when it is not")
  expect_error(gr_ellmer_client(list()), class = "gr_missing_dep")
})

test_that("gr_ellmer_client rejects something that is not a chat", {
  skip_if_not_installed("ellmer")
  expect_error(gr_ellmer_client(list(a = 1)), class = "gr_bad_backend")
})

test_that("gr_ellmer_client drives a chat and never mutates the caller's", {
  skip_if_not_installed("ellmer")
  # A stub with the three methods the adapter documents as its requirements.
  turns_seen <- list()
  stub <- local({
    self <- new.env(parent = emptyenv())
    self$turns <- list("PRE-EXISTING")
    self$system <- NULL
    self$chat <- function(user, echo = "none") {
      turns_seen[[length(turns_seen) + 1L]] <<- list(user = user, system = self$system,
                                                     turns = self$turns)
      "an answer"
    }
    self$clone <- function(deep = FALSE) {
      c2 <- as.environment(as.list(self, all.names = TRUE))
      parent.env(c2) <- emptyenv()
      c2
    }
    self$set_turns <- function(value) self$turns <- value
    self$set_system_prompt <- function(value) self$system <- value
    self$get_model <- function() "stub-model"
    self$get_tokens <- function() data.frame(input = 5, output = 3)
    self
  })
  cl <- gr_ellmer_client(stub)
  res <- gr_call(cl, list(list(role = "system", content = "be terse"),
                          list(role = "user", content = "q")))
  expect_true(res$ok)
  expect_identical(res$text, "an answer")
  expect_identical(res$usage$input, 5L)
  # The caller's chat keeps its own turns: each call ran against a clone.
  expect_identical(stub$turns, list("PRE-EXISTING"))
})
