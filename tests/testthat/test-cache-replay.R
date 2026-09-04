# test-cache-replay.R -- the response cache and the replay client.
#
# Two claims are under test here, and both are the kind that are easy to assert
# loosely and get wrong.
#
#   1. A cache must return a stored response ONLY for a request that is
#      identical in every respect that can change the answer. A key that is too
#      coarse does not make a run cheap, it makes it wrong -- and wrong in the
#      worst way, because the answer looks plausible and the cost report says
#      you saved money. Most of the cache tests below are therefore about
#      MISSES: proof that changing the model, the temperature, the schema, the
#      output cap, the endpoint or a single character of the prompt is not
#      served from the entry for something else.
#
#   2. A replay must reproduce the recorded run, and must fail loudly when it
#      cannot. A replay that silently invents an answer for an unrecorded
#      prompt is worse than no replay at all: it produces a result that looks
#      like the original and is not.

# ---------------------------------------------------------------------------
# The cache: hits
# ---------------------------------------------------------------------------

test_that("an identical request is served from the cache, and the handler is not called", {
  local_registries()
  n <- 0L
  cl <- gr_cache_client(
    gr_mock_client(function(m, p) { n <<- n + 1L; "42" }),
    gr_cache(withr::local_tempdir())
  )

  first  <- gr_call(cl, "What is the answer?")
  second <- gr_call(cl, "What is the answer?")

  expect_identical(first$text, "42")
  expect_identical(second$text, "42")
  expect_identical(n, 1L)
  # The mock's own log is the independent witness: a cache hit never reaches it.
  expect_length(cl$calls(), 1L)
})

test_that("a cache hit is marked, so spending can be told from replay", {
  local_registries()
  cl <- gr_cache_client(gr_mock_client(function(m, p) "x"),
                        gr_cache(withr::local_tempdir()))
  expect_false(gr_call(cl, "q")$cached)
  expect_true(gr_call(cl, "q")$cached)
})

test_that("the trace counts cached calls separately from paid ones", {
  local_registries()
  cache <- gr_cache(withr::local_tempdir())
  cl <- gr_cache_client(gr_mock_client(function(m, p) "x"), cache)

  paid <- gr_trace()
  invisible(gr_call(cl, "q", trace = paid))
  expect_identical(gr_trace_summary(paid)$calls, 1L)
  expect_identical(gr_trace_summary(paid)$cached, 0L)

  free <- gr_trace()
  invisible(gr_call(cl, "q", trace = free))
  s <- gr_trace_summary(free)
  expect_identical(s$calls, 1L)
  expect_identical(s$cached, 1L)
  # Tokens are still reported -- the prompt was that big -- but nothing was paid.
  expect_gt(s$tokens_in, 0L)
})

test_that("a whole cached run reports itself as having cost nothing", {
  local_registries()
  cache <- gr_cache(withr::local_tempdir())
  cl <- gr_cache_client(gr_mock_client(function(m, p) "Revenue was 45.2 million."), cache)

  a <- quiet(answer_document(readgpt_example(), "What was revenue?", "thorough", client = cl))
  b <- quiet(answer_document(readgpt_example(), "What was revenue?", "thorough", client = cl))

  expect_identical(a$answer, b$answer)
  sb <- gr_trace_summary(b$trace)
  expect_gt(sb$calls, 0L)
  expect_identical(sb$cached, sb$calls)
})

# ---------------------------------------------------------------------------
# The cache: misses. Each of these is a request that MUST NOT reuse another's
# entry, because each changes what the model would return.
# ---------------------------------------------------------------------------

test_that("the key separates every request property that can change the answer", {
  local_registries()
  dir <- withr::local_tempdir()
  n <- 0L
  base <- gr_mock_client(function(m, p) { n <<- n + 1L; "answer" })
  cl <- gr_cache_client(base, gr_cache(dir))
  gr_register_model("other-mock", context_window = 128000L, max_output = 4096L,
                    input_usd = 0, output_usd = 0)

  variants <- list(
    baseline    = function() gr_call(cl, "the prompt"),
    repeated    = function() gr_call(cl, "the prompt"),                       # hit
    other_text  = function() gr_call(cl, "the prompt."),                      # one character
    other_model = function() gr_call(cl, "the prompt", model = "other-mock"),
    temp_zero   = function() gr_call(cl, "the prompt", temperature = 0),
    temp_one    = function() gr_call(cl, "the prompt", temperature = 1),
    capped      = function() gr_call(cl, "the prompt", max_output = 16L),
    schema      = function() gr_call(cl, "the prompt",
                                     schema = list(type = "object",
                                                   properties = list(a = list(type = "string")))),
    schema_name = function() gr_call(cl, "the prompt",
                                     schema = list(type = "object",
                                                   properties = list(a = list(type = "string"))),
                                     schema_name = "different"),
    two_msgs    = function() gr_call(cl, list(list(role = "system", content = "be terse"),
                                              list(role = "user", content = "the prompt"))),
    roles_swap  = function() gr_call(cl, list(list(role = "user", content = "be terse"),
                                              list(role = "user", content = "the prompt")))
  )
  for (f in variants) invisible(f())

  # Eleven requests, one of which repeats the first: ten distinct keys.
  expect_identical(n, 10L)
  expect_identical(gr_cache_stats(gr_cache(dir))$entries, 10L)
})

test_that("a different endpoint is a different cache entry", {
  local_registries()
  dir <- withr::local_tempdir()
  n <- 0L
  mk <- function(base_url) {
    cl <- gr_mock_client(function(m, p) { n <<- n + 1L; "answer" })
    cl$base_url <- base_url
    gr_cache_client(cl, gr_cache(dir))
  }
  invisible(gr_call(mk("https://a.example/v1"), "same prompt"))
  invisible(gr_call(mk("https://b.example/v1"), "same prompt"))
  expect_identical(n, 2L)
})

test_that("failures are never cached", {
  local_registries()
  dir <- withr::local_tempdir()

  n <- 0L
  dead <- gr_cache_client(gr_mock_client(function(m, p) { n <<- n + 1L; stop("transport") }),
                          gr_cache(dir))
  invisible(gr_call(dead, "q")); invisible(gr_call(dead, "q"))
  expect_identical(n, 2L)

  # An empty completion is a failure too -- caching it would make a one-off
  # content filter permanent.
  m <- 0L
  empty <- gr_cache_client(gr_mock_client(function(m_, p) { m <<- m + 1L; "" }), gr_cache(dir))
  invisible(gr_call(empty, "q2")); invisible(gr_call(empty, "q2"))
  expect_identical(m, 2L)

  expect_identical(gr_cache_stats(gr_cache(dir))$entries, 0L)
})

# ---------------------------------------------------------------------------
# The cache: robustness. A cache is an optimisation and must never be a new way
# for a run to fail.
# ---------------------------------------------------------------------------

test_that("a corrupt entry is a miss, not an error", {
  local_registries()
  dir <- withr::local_tempdir()
  n <- 0L
  cl <- gr_cache_client(gr_mock_client(function(m, p) { n <<- n + 1L; "ok" }), gr_cache(dir))
  invisible(gr_call(cl, "q"))

  f <- list.files(dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  expect_length(f, 1L)
  writeBin(as.raw(c(0x00, 0x01, 0x02, 0x03)), f)          # truncated / garbage

  expect_no_error(res <- gr_call(cl, "q"))
  expect_true(res$ok)
  expect_identical(n, 2L)
})

test_that("an RDS holding the wrong object is a miss, not an error", {
  local_registries()
  dir <- withr::local_tempdir()
  cl <- gr_cache_client(gr_mock_client(function(m, p) "ok"), gr_cache(dir))
  invisible(gr_call(cl, "q"))
  f <- list.files(dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  saveRDS(list(format = 99L, result = "not a gr_result"), f)
  expect_no_error(res <- gr_call(cl, "q"))
  expect_true(res$ok)
})

test_that("an unwritable cache directory does not stop the run", {
  local_registries()
  # A path whose parent is a regular file can never be created.
  blocker <- withr::local_tempfile()
  writeLines("not a directory", blocker)
  cl <- gr_cache_client(gr_mock_client(function(m, p) "ok"),
                        gr_cache(file.path(blocker, "cache")))
  expect_no_error(res <- gr_call(cl, "q"))
  expect_true(res$ok)
})

test_that("writing leaves no temporary files behind", {
  local_registries()
  dir <- withr::local_tempdir()
  cl <- gr_cache_client(gr_mock_client(function(m, p) "ok"), gr_cache(dir))
  for (i in 1:5) invisible(gr_call(cl, paste("q", i)))
  expect_length(list.files(dir, pattern = "tmp", recursive = TRUE), 0L)
})

test_that("read and write can each be switched off", {
  local_registries()
  dir <- withr::local_tempdir()

  n <- 0L
  handler <- function(m, p) { n <<- n + 1L; "ok" }

  no_write <- gr_cache_client(gr_mock_client(handler), gr_cache(dir, write = FALSE))
  invisible(gr_call(no_write, "q")); invisible(gr_call(no_write, "q"))
  expect_identical(n, 2L)
  expect_identical(gr_cache_stats(gr_cache(dir))$entries, 0L)

  n <- 0L
  writing <- gr_cache_client(gr_mock_client(handler), gr_cache(dir))
  invisible(gr_call(writing, "q"))
  no_read <- gr_cache_client(gr_mock_client(handler), gr_cache(dir, read = FALSE))
  invisible(gr_call(no_read, "q"))
  expect_identical(n, 2L)
})

test_that("the cache stores the response but not the prompt", {
  local_registries()
  dir <- withr::local_tempdir()
  secret <- "PATIENT-IDENTIFIER-90210"
  cl <- gr_cache_client(gr_mock_client(function(m, p) "a harmless answer"), gr_cache(dir))
  invisible(gr_call(cl, paste("Summarise this:", secret)))

  f <- list.files(dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  entry <- readRDS(f)
  flat <- paste(unlist(lapply(entry, function(x) as.character(unlist(x)))), collapse = " ")
  expect_false(grepl(secret, flat, fixed = TRUE))
  expect_true(grepl("a harmless answer", flat, fixed = TRUE))
})

test_that("the raw parsed API response is dropped before an entry is written", {
  # The mock's own results carry no `raw`, so asserting on them proves nothing --
  # the first version of this check passed just as happily with the drop removed.
  # This handler returns a result that DOES carry one, the shape the real client
  # produces, and the stored entry must not have it: nothing downstream reads
  # `raw`, and it is both the largest part of a response and a second copy of
  # whatever document text the model was quoting.
  local_registries()
  dir <- withr::local_tempdir()
  bulky <- list(id = "resp_1", output = list(list(content = list(list(
    type = "output_text", text = "SENSITIVE-RAW-PAYLOAD")))))
  cl <- gr_cache_client(
    gr_mock_client(function(m, p) readgpt:::gr_result(TRUE, text = "clean answer", raw = bulky)),
    gr_cache(dir))

  first <- gr_call(cl, "q")
  expect_false(is.null(first$raw))                    # the live result keeps it

  entry <- readRDS(list.files(dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE))
  expect_null(entry$result$raw)                       # the stored copy does not
  flat <- paste(unlist(lapply(entry, function(x) as.character(unlist(x)))), collapse = " ")
  expect_false(grepl("SENSITIVE-RAW-PAYLOAD", flat, fixed = TRUE))

  hit <- gr_call(cl, "q")                             # and a hit is still usable
  expect_true(hit$cached)
  expect_identical(hit$text, "clean answer")
  expect_null(hit$raw)
})

test_that("stats and clear report and empty the cache", {
  local_registries()
  dir <- withr::local_tempdir()
  cache <- gr_cache(dir)
  cl <- gr_cache_client(gr_mock_client(function(m, p) "ok"), cache)
  invisible(gr_call(cl, "one")); invisible(gr_call(cl, "two")); invisible(gr_call(cl, "one"))

  s <- gr_cache_stats(cache)
  expect_identical(s$entries, 2L)
  expect_identical(s$writes, 2L)
  expect_identical(s$hits, 1L)
  expect_identical(s$misses, 2L)
  expect_gt(s$bytes, 0)

  expect_identical(gr_cache_clear(cache), 2L)
  expect_identical(gr_cache_stats(cache)$entries, 0L)
  expect_identical(gr_cache_stats(cache)$hits, 0L)
})

test_that("gr_cache_client validates its arguments and can detach", {
  local_registries()
  expect_error(gr_cache_client("not a client"), "gr_client")
  expect_error(gr_cache_client(gr_mock_client(), cache = "not a cache"), "gr_cache")
  cl <- gr_cache_client(gr_mock_client(), gr_cache(withr::local_tempdir()))
  expect_true(inherits(cl$.cache, "gr_cache"))
  expect_null(gr_cache_client(cl, NULL)$.cache)
})

test_that("a cached mock client is still a mock client", {
  local_registries()
  cl <- gr_cache_client(gr_mock_client(function(m, p) "ok"), gr_cache(withr::local_tempdir()))
  expect_s3_class(cl, "gr_mock_client")
  expect_s3_class(cl, "gr_client")
  expect_true(is.function(cl$calls))
})

test_that("the exported gr_cache_clear is the response-cache one, not the internal flusher", {
  # R collates R/core-state.R after R/core-cache.R, so an internal function with
  # the same name silently replaced the exported one and the package shipped the
  # wrong implementation under the right documentation. The internal one is now
  # gr_flush_caches(); this asserts the shadowing has not come back.
  expect_true("cache" %in% names(formals(gr_cache_clear)))
  expect_false("what" %in% names(formals(gr_cache_clear)))
  expect_error(gr_cache_clear("documents"), "gr_cache")
  expect_identical(readgpt:::gr_flush_caches("documents"), "documents")
})

# ---------------------------------------------------------------------------
# Replay
# ---------------------------------------------------------------------------

test_that("a recorded run replays to the same answer with no client", {
  local_registries()
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  original <- quiet(answer_document(readgpt_example(), "What was revenue?", "thorough",
                                    client = cl))

  rp <- gr_replay_client(original$trace)
  replayed <- quiet(answer_document(readgpt_example(), "What was revenue?", "thorough",
                                    client = rp))

  expect_identical(replayed$answer, original$answer)
  expect_identical(replayed$partial, original$partial)
  expect_gt(rp$stats()$hits, 0L)
  expect_identical(rp$stats()$misses, 0L)
  # Nothing was paid for: every call came from the recording.
  s <- gr_trace_summary(replayed$trace)
  expect_identical(s$cached, s$calls)
})

test_that("a trace survives the round trip through a file", {
  local_registries()
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  original <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = cl))

  f <- withr::local_tempfile(fileext = ".json")
  expect_identical(gr_trace_save(original$trace, f), f)
  expect_true(file.exists(f))
  expect_true(jsonlite::validate(paste(readLines(f, warn = FALSE), collapse = "\n")))

  replayed <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast",
                                    client = gr_replay_client(f)))
  expect_identical(replayed$answer, original$answer)
})

test_that("repeated identical prompts replay in the order they were recorded", {
  local_registries()
  tr <- gr_trace()
  n <- 0L
  cl <- gr_mock_client(function(m, p) { n <<- n + 1L; paste("answer", n) })
  invisible(gr_call(cl, "same prompt", trace = tr))
  invisible(gr_call(cl, "same prompt", trace = tr))
  invisible(gr_call(cl, "same prompt", trace = tr))

  rp <- gr_replay_client(tr)
  expect_identical(gr_call(rp, "same prompt")$text, "answer 1")
  expect_identical(gr_call(rp, "same prompt")$text, "answer 2")
  expect_identical(gr_call(rp, "same prompt")$text, "answer 3")

  # A fourth call has nothing left. The last response repeats, and `$stats()`
  # says so rather than pretending the replay was exact.
  expect_identical(gr_call(rp, "same prompt")$text, "answer 3")
  expect_identical(rp$stats()$repeats, 1L)
})

test_that("an unrecorded prompt is a loud failure by default", {
  local_registries()
  tr <- gr_trace()
  cl <- gr_mock_client(function(m, p) "recorded")
  invisible(gr_call(cl, "the recorded prompt", trace = tr))

  strict <- gr_replay_client(tr)
  expect_error(gr_call(strict, "a prompt nobody recorded"), class = "gr_replay_miss")
  expect_length(strict$missed(), 1L)

  lenient <- gr_replay_client(tr, strict = FALSE)
  res <- gr_call(lenient, "a prompt nobody recorded")
  expect_false(res$ok)
  expect_true(res$cached)
  expect_match(res$error, "No recorded response")
})

test_that("a miss caused only by the model says so", {
  local_registries()
  tr <- gr_trace()
  cl <- gr_mock_client(function(m, p) "recorded")
  invisible(gr_call(cl, "shared prompt", trace = tr))
  gr_register_model("another-mock", context_window = 128000L, max_output = 4096L,
                    input_usd = 0, output_usd = 0)

  rp <- gr_replay_client(tr, strict = FALSE)
  res <- gr_call(rp, "shared prompt", model = "another-mock")
  expect_false(res$ok)
  expect_match(res$error, "IS recorded under model")
})

test_that("a trace with no model calls cannot be replayed", {
  expect_error(gr_replay_client(gr_trace()), class = "gr_replay_empty")
})

test_that("an unreadable or missing trace file is reported clearly", {
  expect_error(gr_replay_client(file.path(tempdir(), "no-such-trace.json")),
               class = "gr_file_not_found")
  bad <- withr::local_tempfile(fileext = ".json")
  writeLines("{ this is not json", bad)
  expect_error(gr_replay_client(bad), class = "gr_replay_unreadable")
  expect_error(gr_replay_client(42), class = "gr_replay_unreadable")
})

test_that("replay says plainly that it cannot reproduce embeddings", {
  local_registries()
  tr <- gr_trace()
  invisible(gr_call(gr_mock_client(function(m, p) "x"), "q", trace = tr))
  rp <- gr_replay_client(tr)

  expect_warning(e <- gr_embed(rp, c("alpha text", "beta text")),
                 class = "gr_replay_no_embeddings")
  expect_identical(nrow(e), 2L)
  expect_identical(attr(e, "embedding_source"), "lexical")
  # And it never pretends to have succeeded when told not to fall back.
  expect_error(gr_embed(rp, "alpha", fallback = "error"), class = "gr_embed_error")
})

test_that("a replay client is accepted anywhere a client is", {
  local_registries()
  tr <- gr_trace()
  invisible(gr_call(gr_mock_client(function(m, p) "x"), "q", trace = tr))
  rp <- gr_replay_client(tr)
  expect_s3_class(rp, "gr_client")
  expect_output(print(rp), "gr_replay_client")
  expect_identical(rp$stats()$recorded, 1L)
})

test_that("local trace steps are not mistaken for model calls", {
  local_registries()
  cl <- gr_mock_client(function(m, p) "an answer")
  ans <- quiet(answer_document(readgpt_example(), "What was revenue?", "thorough", client = cl))
  n_local <- sum(vapply(ans$trace$steps,
                        function(s) identical(s$kind, "local"), logical(1)))
  expect_gt(n_local, 0L)
  expect_identical(gr_replay_client(ans$trace)$n_recorded,
                   length(ans$trace$steps) - n_local)
})

# ---------------------------------------------------------------------------
# Encoding. This is the defect class that got through four adversarial passes
# and 613 tests in 0.2.0, because every test ran in one locale on one machine
# with ASCII fixtures. Everything below uses \u escapes so the fixtures are
# non-ASCII while the source file stays ASCII, and every assertion is on bytes
# rather than on characters, whose count depends on the very labelling under
# test.
# ---------------------------------------------------------------------------

test_that("a saved trace preserves non-ASCII text", {
  # jsonlite escapes bytes it cannot interpret in the CURRENT LOCALE, so an
  # unmarked string holding UTF-8 bytes serialised as the literal text
  # "caf<c3><a9>" on any machine whose locale is not UTF-8. The bytes were always
  # right; nothing had told R what they were. A trace that cannot survive being
  # written to a file is not a record of anything.
  local_registries()
  # Unmarked on purpose. A \u escape in source is UTF-8-labelled by the parser,
  # so a fixture written that way tests nothing -- it is already in the state the
  # fix produces. Text that arrives from readLines() without an `encoding=`, from
  # a command-line argument, or from a connection is not labelled, and that is
  # the string that used to serialise as "caf<c3><a9>".
  q <- unmarked("Quel \u00e9tait le chiffre d\u2019affaires ? caf\u00e9 na\u00efve")
  a <- unmarked("r\u00e9ponse enregistr\u00e9e \u2014 45,2 M\u20ac")

  tr <- gr_trace()
  invisible(gr_call(gr_mock_client(function(m, p) a), q, trace = tr))

  f <- withr::local_tempfile(fileext = ".json")
  gr_trace_save(tr, f)
  raw <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")

  expect_true(jsonlite::validate(raw))
  expect_false(grepl("<c3>", raw, fixed = TRUE))          # the mangled form
  back <- jsonlite::fromJSON(f, simplifyVector = FALSE)
  expect_identical(charToRaw(back$steps[[1]]$response), charToRaw(a))
  expect_identical(charToRaw(back$steps[[1]]$prompt[[1]]$content), charToRaw(q))
})

test_that("a run with a non-ASCII question and answer replays from a file", {
  local_registries()
  q <- unmarked("Quel \u00e9tait le chiffre d\u2019affaires ? caf\u00e9 na\u00efve")
  a <- unmarked("r\u00e9ponse enregistr\u00e9e \u2014 45,2 M\u20ac")

  original <- quiet(answer_document(readgpt_example(), q, "thorough",
                                    client = gr_mock_client(function(m, p) a)))
  f <- withr::local_tempfile(fileext = ".json")
  gr_trace_save(original$trace, f)

  rp <- gr_replay_client(f)
  replayed <- quiet(answer_document(readgpt_example(), q, "thorough", client = rp))

  expect_length(rp$missed(), 0L)
  expect_identical(charToRaw(replayed$answer), charToRaw(original$answer))
  expect_identical(replayed$answer, original$answer)
})

test_that("the cache key depends on characters, not on how they are labelled", {
  # digest() hashes the serialised R object, and a string's declared encoding is
  # part of that object -- so the same bytes marked "UTF-8" and marked "unknown"
  # hashed differently and the same prompt was billed twice.
  local_registries()
  n <- 0L
  cl <- gr_cache_client(gr_mock_client(function(m, p) { n <<- n + 1L; "ok" }),
                        gr_cache(withr::local_tempdir()))
  txt <- "caf\u00e9 r\u00e9sum\u00e9 na\u00efve \u2014 45,2 M\u20ac"
  labelled <- txt; Encoding(labelled) <- "UTF-8"
  bare <- unmarked(txt)
  expect_identical(charToRaw(labelled), charToRaw(bare))     # same bytes

  invisible(gr_call(cl, labelled))
  invisible(gr_call(cl, bare))
  expect_identical(n, 1L)
})

test_that("a result labels valid UTF-8 text without converting it", {
  local_registries()
  a <- "r\u00e9ponse \u2014 45,2 M\u20ac"
  res <- gr_call(gr_mock_client(function(m, p) unmarked(a)), "q")
  expect_identical(Encoding(res$text), "UTF-8")            # labelled
  expect_identical(charToRaw(res$text), charToRaw(a))      # and unchanged
})
