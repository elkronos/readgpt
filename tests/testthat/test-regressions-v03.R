# test-regressions-v03.R -- defects found reviewing the 0.3.0 work itself.
#
# Every test here corresponds to a bug that was written, shipped into the branch,
# and found afterwards by an adversarial pass over the new code rather than by
# the suite that was supposed to cover it. Two of them -- the hang and the shared
# cache -- would have been very hard to diagnose in the field, and neither was
# anywhere near the paths the feature tests exercised.

test_that("mmr_select() cannot fail to terminate", {
  # A non-finite embedding row poisons every candidate through pmax(), every
  # score becomes NaN, which.max() returns integer(0), `sel` stops growing and
  # the loop never ends. Not an error and not a crash: a hang, which is the
  # worst thing a library can do. An all-zero vector L2-normalised is 0/0, so
  # any chunk an embedder finds no words in produces one.
  mmr <- readgpt:::mmr_select
  emb <- rbind(c(NaN, NaN, NaN), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  rel <- c(0.99, 0.8, 0.7, 0.6)          # the poisoned row ranks FIRST

  out <- withr::with_options(list(timeout = 15), {
    setTimeLimit(elapsed = 10, transient = TRUE)
    on.exit(setTimeLimit(), add = TRUE)
    mmr(rel, emb, 3, 0.5)
  })
  expect_length(out, 3L)
  expect_false(anyNA(out))

  # Every row unusable: still terminates, still returns k.
  allbad <- matrix(NaN, nrow = 3L, ncol = 3L)
  expect_length(mmr(c(0.5, 0.9, 0.1), allbad, 2, 0.5), 2L)
})

test_that("an unusable embedding row is neutralised, not dropped from the answer", {
  # The first fix EXCLUDED such rows from selection, which stopped the hang and
  # introduced a quieter bug: cosine_against() maps a non-finite row to a
  # relevance of 0, so the chunk is a legitimate if unpromising candidate that
  # top-k will take. Excluding it meant `mmr < 1` silently returned FEWER chunks
  # than top_k asked for while `mmr = 1` returned the full set -- a diversity
  # setting shrinking the evidence with nothing in `notes` to say so.
  mmr <- readgpt:::mmr_select
  emb <- rbind(c(NaN, NaN, NaN), c(1, 0, 0), c(0, 1, 0))
  rel <- c(0, 0.9, 0.8)                  # all finite, as cosine_against() delivers

  expect_length(mmr(rel, emb, 3, 1), 3L)
  expect_length(mmr(rel, emb, 3, 0.5), 3L)          # the same count, both ways
  expect_setequal(mmr(rel, emb, 3, 0.5), 1:3)
  expect_identical(mmr(rel, emb, 3, 0.5)[1], 2L)    # still starts with the best
})

test_that("two closure-backed clients do not share cache entries", {
  # Every mock and backend client is api="mock"/"backend", the same base_url and
  # the same default model, so a key built from those three could not tell them
  # apart -- and gr_cache() twice gives the same default directory. Two clients
  # with genuinely different handlers silently traded answers.
  local_registries()
  dir <- withr::local_tempdir()
  one <- gr_cache_client(gr_mock_client(function(m, p) "ANSWER ONE"), gr_cache(dir))
  two <- gr_cache_client(gr_mock_client(function(m, p) "ANSWER TWO"), gr_cache(dir))
  expect_identical(gr_call(one, "same question")$text, "ANSWER ONE")
  expect_identical(gr_call(two, "same question")$text, "ANSWER TWO")
  expect_length(two$calls(), 1L)         # the second handler really ran

  b1 <- gr_cache_client(gr_backend_client(function(m, p) "BACKEND ONE", model = "mock-model"),
                        gr_cache(dir))
  b2 <- gr_cache_client(gr_backend_client(function(m, p) "BACKEND TWO", model = "mock-model"),
                        gr_cache(dir))
  expect_identical(gr_call(b1, "q")$text, "BACKEND ONE")
  expect_identical(gr_call(b2, "q")$text, "BACKEND TWO")

  # And the fix must not have cost the feature: one client still hits its cache.
  same <- gr_cache_client(gr_mock_client(function(m, p) "X"), gr_cache(dir))
  invisible(gr_call(same, "repeated"))
  expect_true(gr_call(same, "repeated")$cached)
})

test_that("a corpus store does not restore one client's answers for another", {
  local_registries()
  store <- withr::local_tempdir()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("Revenue was 45.2 million dollars.", f)

  a <- quiet(gr_read_many(f, "Q?", "fast", store = store,
                          client = gr_mock_client(function(m, p) "FIRST CLIENT")))
  b <- quiet(gr_read_many(f, "Q?", "fast", store = store,
                          client = gr_mock_client(function(m, p) "SECOND CLIENT")))
  expect_identical(a$summary$answer, "FIRST CLIENT")
  expect_identical(b$summary$answer, "SECOND CLIENT")
  expect_identical(b$summary$status, "ok")

  # The same client still restores, which is the whole point of the store.
  cl <- gr_mock_client(function(m, p) "STABLE CLIENT")
  quiet(gr_read_many(f, "Q?", "fast", client = cl, store = store))
  again <- quiet(gr_read_many(f, "Q?", "fast", client = cl, store = store))
  expect_identical(again$summary$status, "restored")
})

test_that("a client with no cache attached does not partial-match its own id", {
  # `$` on a list partial-matches. The per-client id was first called
  # `.cache_id`, so `client$.cache` -- how every caller asks whether a cache is
  # attached -- returned that id string for any client without one. The same
  # trap that made `parsed$output` return `output_text` in the previous release.
  local_registries()
  for (cl in list(gr_mock_client(), gr_backend_client(function(m, p) "x"), gr_client())) {
    expect_null(cl$.cache)
  }
  attached <- gr_cache_client(gr_mock_client(), gr_cache(withr::local_tempdir()))
  expect_s3_class(attached$.cache, "gr_cache")
  expect_null(gr_cache_client(attached, NULL)$.cache)
})

test_that("a cost nobody can compute is not reported as zero", {
  # gr_trace_cost() returns NA for an unpriced model precisely so a total cannot
  # quietly omit it. gr_read_many() then summed with na.rm = TRUE, turning "we
  # do not know" into "$0.0000" -- which also made max_total_usd unenforceable
  # while the run reported itself free.
  local_registries()
  # The per-run cap warns separately about the same unpriced model; this test is
  # about the CORPUS ceiling, so take that one out of the picture.
  gr_options(max_cost_usd = NULL)
  gr_register_model("unpriced", context_window = 100000L, max_output = 4096L)
  cl <- gr_backend_client(function(m, p) "an answer", model = "unpriced")
  files <- vapply(1:3, function(i) {
    p <- withr::local_tempfile(fileext = ".txt", .local_envir = parent.frame(3))
    writeLines("Revenue was 45.2 million dollars.", p); p
  }, character(1))

  expect_warning(
    out <- suppressMessages(gr_read_many(files, "Q?", "fast", client = cl,
                                         model = "unpriced", max_total_usd = 1e-6)),
    class = "gr_corpus_cost_unknown")
  expect_true(all(is.na(out$summary$cost_usd)))
  expect_true(all(out$summary$status == "ok"))      # uncapped, and said so
  expect_true(any(grepl("cost unknown", capture.output(print(out)))))

  # With a price, the ceiling works exactly as before.
  gr_register_model("priced", context_window = 100000L, max_output = 4096L,
                    input_usd = 1000, output_usd = 1000)
  cl2 <- gr_backend_client(function(m, p) "an answer", model = "priced")
  expect_warning(
    capped <- suppressMessages(gr_read_many(files, "Q?", "fast", client = cl2,
                                            model = "priced", max_total_usd = 1e-6)),
    class = "gr_corpus_cost_cap")
  expect_identical(capped$summary$status[1], "ok")
  expect_true(all(capped$summary$status[-1] == "skipped"))
})

test_that("an ensemble verifies each evidence row by its own kind", {
  # `kind` was derived from the ANSWER's reader, which for an ensemble is
  # "mixed" -- so per-chunk answers contributed by a map_reduce member were
  # string-matched against their chunks and reported as fabricated quotations.
  local_registries()
  gr_options(embedder = "lexical")
  doc <- gr_ingest(paste(c("Revenue rose to 45.2 million dollars in fiscal 2024.",
                           "Headcount grew to 1,204 employees worldwide.",
                           "Operating margin fell 5 percent year over year."), collapse = "\n\n"))
  ch <- gr_segment(doc, list(method = "paragraph", max_tokens = 40))
  a <- quiet(gr_read(ch, "What was revenue?", mock_echo("Revenue was 45.2 million dollars."),
                     list(reader = "ensemble", members = c("retrieve", "map_reduce"))))

  v <- gr_verify_evidence(a, ch)
  expect_true(any(v$kind == "answer"))
  expect_true(any(v$kind == "verbatim"))
  expect_true(all(is.na(v$verified[v$kind == "answer"])))
  expect_true(all(v$verified[v$kind == "verbatim"]))
  expect_false(a$partial)
})

test_that("a minus sign is content, not typography", {
  # The edge-trimming class included `-` so a normalised em dash could be
  # stripped, which also deleted a leading minus: "-5%" verified as an exact
  # quotation of "fell 5%". A flipped sign is the fabrication this check exists
  # to catch.
  sm <- readgpt:::span_match
  src <- "Operating margin fell 5% year over year, before tax."
  expect_false(sm("-5%", src)$verified)
  expect_false(sm("-5% year over year", src)$verified)
  expect_true(sm("fell 5% year over year", src)$verified)

  # Trailing dashes are still typography, and so is a leading dash on a word.
  expect_true(sm("Operating margin fell 5%—", src)$verified)
  expect_true(sm("- Operating margin fell 5%", src)$verified)
})

test_that("the documented extraction examples key off the prompt that exists", {
  # The roxygen example, the README and the vignette all branch on recognising
  # skim's extraction prompt. When that marker does not match, the "honest"
  # model in the example returns the same non-quotation as the "liar" and the
  # feature's only demonstration shows it failing. R CMD check cannot catch it:
  # the example runs fine, it just proves the opposite of what it says.
  marker <- "You extract evidence"
  expect_true(grepl(marker, readgpt:::.gr_prompts$extract_system, fixed = TRUE))

  rd <- file.path(system.file("help", package = "readgpt"))
  ex <- readgpt:::.gr_prompts$extract_system
  expect_false(grepl("^Extract", ex))     # the marker that used to be assumed
})

test_that("clamp() does not eat warnings raised while its argument is evaluated", {
  # `x` arrives as a promise. Forcing it inside suppressWarnings() swallows
  # everything raised while EVALUATING the argument, not just the coercion
  # warning that call exists to quiet -- so clamp_warn(na_default(...)) silently
  # ate the "using the default" warning, and every other clamp(f(...)) in the
  # package would have eaten f()'s warnings too. Exactly the shape that makes
  # expect_warning(suppressWarnings(...)) never fire: the inner handler is
  # established last and wins.
  noisy <- function() { warning("raised while the promise was forced"); 1 }
  expect_warning(readgpt:::clamp_warn(noisy(), 0, 1, "x", integer = FALSE),
                 "raised while the promise was forced")
  expect_warning(readgpt:::clamp(noisy(), 0, 1), "raised while the promise was forced")
  expect_warning(readgpt:::as_num1(noisy()), "raised while the promise was forced")

  # And the coercion warning it IS meant to quiet is still quiet.
  expect_no_warning(readgpt:::clamp("not a number", 0, 1))
  expect_identical(readgpt:::clamp("not a number", 0, 1), 0)
})

test_that("a missing setting falls back to the default, not to the bottom of the range", {
  # clamp() maps NA to `lo`, which for mmr is 0 -- documented as selecting "for
  # novelty alone and will happily pick irrelevant chunks". A missing value must
  # not choose the most destructive end of a scale.
  expect_warning(s <- gr_read_spec(mmr = NA), class = "gr_bad_setting")
  expect_identical(s$mmr, 1)
  expect_warning(s2 <- gr_read_spec(mmr = c(0.2, 0.4)), class = "gr_bad_setting")
  expect_identical(s2$mmr, 1)
  expect_identical(gr_read_spec(mmr = 0)$mmr, 0)     # an explicit 0 is still honoured
})

# ---------------------------------------------------------------------------
# The ellmer adapter's guarantees, testable without ellmer installed
# ---------------------------------------------------------------------------

test_that("the adapter requires every method its guarantees depend on", {
  check <- readgpt:::check_chat_methods
  full <- list(chat = function(...) "x", chat_structured = function(...) NULL,
               clone = function(...) NULL, set_turns = function(...) NULL,
               set_system_prompt = function(...) NULL)
  expect_true(check(full))

  # set_system_prompt is the one that matters most: this package puts its
  # instructions there, so a chat that silently dropped it would return
  # unconstrained answers reporting ok = TRUE.
  for (drop in names(full)) {
    expect_error(check(full[setdiff(names(full), drop)]), class = "gr_bad_backend")
  }
  expect_error(check(list(a = 1)), class = "gr_bad_backend")
  # And the most likely wrong argument of all gets this function's own message
  # rather than "object of type 'closure' is not subsettable".
  expect_error(check(function(x) x), class = "gr_bad_backend")
  expect_error(check("a string"), class = "gr_bad_backend")
})

test_that("an empty ellmer reply is a failure, as it is for every other handler", {
  res <- readgpt:::ellmer_result("", "m", list(input = 5L, output = 0L))
  expect_false(res$ok)
  expect_identical(res$text, "")
  expect_match(res$error, "empty completion")
  expect_identical(res$usage$input, 5L)          # usage is still reported

  expect_false(readgpt:::ellmer_result("   \n ", "m", list(input = 1L, output = 0L))$ok)
  good <- readgpt:::ellmer_result("an answer", "m", list(input = 5L, output = 2L))
  expect_true(good$ok)
  expect_identical(good$text, "an answer")
})

# ---------------------------------------------------------------------------
# The sixth pass: defects introduced or missed by the fifth
# ---------------------------------------------------------------------------

test_that("a stable client id makes a cache and a store reusable across sessions", {
  # The fix that stopped two closure-backed clients trading answers gave every
  # one of them a fresh per-session identity -- which also broke the durable
  # cache and the resumable store the documentation promises. A caller who knows
  # their handler is stable must be able to say so.
  local_registries()
  gr_register_model("mock-model", context_window = 128000L, max_output = 4096L,
                    input_usd = 0, output_usd = 0)
  dir <- withr::local_tempdir()
  h <- function(m, p) "Revenue was 45.2 million dollars."

  anon1 <- gr_cache_client(gr_backend_client(h, model = "mock-model"), gr_cache(dir))
  anon2 <- gr_cache_client(gr_backend_client(h, model = "mock-model"), gr_cache(dir))
  invisible(gr_call(anon1, "q"))
  expect_false(gr_call(anon2, "q")$cached)          # safe default: no sharing

  named1 <- gr_cache_client(gr_backend_client(h, model = "mock-model", id = "my-provider"),
                            gr_cache(dir))
  named2 <- gr_cache_client(gr_backend_client(h, model = "mock-model", id = "my-provider"),
                            gr_cache(dir))
  invisible(gr_call(named1, "q2"))
  expect_true(gr_call(named2, "q2")$cached)         # opted in: reuse works
  expect_false(gr_call(
    gr_cache_client(gr_backend_client(h, model = "mock-model", id = "other"), gr_cache(dir)),
    "q2")$cached)                                   # a different id is a different client
})

test_that("a replay client is identified by its recording, not by its object", {
  # gr_replay_client() was left out of the identity fix entirely, so a corpus
  # store served one recording's answers while replaying a different one --
  # exactly the failure the identity was introduced to prevent.
  local_registries()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("Revenue was 45.2 million dollars.", f)
  runA <- quiet(gr_read_many(f, "Q?", "fast", client = gr_mock_client(function(m, p) "ANSWER-A")))
  runB <- quiet(gr_read_many(f, "Q?", "fast", client = gr_mock_client(function(m, p) "ANSWER-B")))

  store <- withr::local_tempdir()
  quiet(gr_read_many(f, "Q?", "fast", client = gr_replay_client(runA$trace), store = store))
  b <- quiet(gr_read_many(f, "Q?", "fast", client = gr_replay_client(runB$trace), store = store))
  expect_identical(b$summary$answer, "ANSWER-B")
  expect_identical(b$summary$status, "ok")

  # Two replays of the SAME recording are the same thing, and do share.
  again <- quiet(gr_read_many(f, "Q?", "fast", client = gr_replay_client(runA$trace),
                              store = store))
  expect_identical(again$summary$status, "restored")
})

test_that("a citation of a chunk that was sent but did not contribute is not a fabrication", {
  # `chunks_used` holds only the chunks that CONTRIBUTED, so comparing citations
  # against it reported a faithful citation of a chunk which had answered "not in
  # this excerpt" as an invention -- a false positive in a hallucination check,
  # which is the one place a false positive is least affordable.
  local_registries()
  # Each paragraph must be its own chunk, so size them near the cap: the
  # paragraph segmenter packs short runs together, and three short lines under a
  # 40-token cap come back as two chunks.
  doc <- paste(c(
    paste("Revenue was 45.2 million dollars in fiscal 2024 across every region the",
          "company serves, before tax and before the restatement described later."),
    paste("This paragraph concerns an entirely unrelated administrative matter of no",
          "interest to the question and mentions no figures whatsoever anywhere."),
    paste("Headcount grew to 1,204 employees across nine clinical sites in total,",
          "with the largest increase recorded in the northern operating region.")),
    collapse = "\n\n")
  ch <- gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 40))
  expect_identical(nrow(ch$chunks), 3L)

  n <- 0L
  faithful <- gr_mock_client(function(m, p) {
    n <<- n + 1L
    if (n <= nrow(ch$chunks)) (if (n == 2L) "NOT_IN_DOCUMENT" else "a finding")
    else "Merged, citing [chunk 2]."
  })
  a <- quiet(gr_read(ch, "What was revenue?", faithful, gr_read_spec("map_reduce", cite = TRUE)))
  expect_false(2L %in% a$chunks_used)          # it did not contribute
  expect_null(a$notes$cited_unknown)           # but it WAS sent, so this is honest
  expect_false(a$partial)

  n <- 0L
  inventing <- gr_mock_client(function(m, p) {
    n <<- n + 1L
    if (n <= nrow(ch$chunks)) "a finding" else "Merged, citing [chunk 99]."
  })
  b <- quiet(gr_read(ch, "What was revenue?", inventing, gr_read_spec("map_reduce", cite = TRUE)))
  expect_identical(b$notes$cited_unknown, 99L)
  expect_true(b$partial)
})

test_that("an embedding that fell back sets partial; one that was chosen does not", {
  # The vignette tells readers `partial` is TRUE "whenever anything degraded --
  # ... an embedding fell back to lexical vectors". It was not. The distinction
  # that matters is fallback versus choice: being dropped onto lexical vectors
  # because the real embedder failed is a degradation; asking for them is not.
  local_registries()
  doc <- paste(c("Revenue was 45.2 million dollars in fiscal 2024 across all regions.",
                 "Headcount grew to 1,204 employees across nine clinical sites."),
               collapse = "\n\n")
  ch <- gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 40))

  gr_register_model("mock-model", context_window = 128000L, max_output = 4096L,
                    input_usd = 0, output_usd = 0)
  no_embedder <- gr_backend_client(function(m, p) "Revenue was 45.2 million.",
                                   model = "mock-model")
  fell <- suppressWarnings(quiet(gr_read(ch, "What was revenue?", no_embedder,
                                         gr_read_spec("retrieve", top_k = 2))))
  expect_true(fell$notes$embedding_fallback)
  expect_true(fell$partial)

  gr_options(embedder = "lexical")
  chose <- quiet(gr_read(ch, "What was revenue?", mock_echo("Revenue was 45.2 million."),
                         gr_read_spec("retrieve", top_k = 2)))
  expect_identical(chose$notes$embedding_source, "lexical")
  expect_false(chose$notes$embedding_fallback)
  expect_false(chose$partial)
})

test_that("context_order = 'document' applies however few chunks were selected", {
  # The length guard sat above the "document" branch, so it was a silent no-op
  # below three chunks -- which is the common case for a top-k reader.
  arr <- readgpt:::arrange_context
  expect_identical(arr(c(5L, 1L), "document"), c(1L, 5L))
  expect_identical(arr(c(9L, 2L, 7L), "document"), c(2L, 7L, 9L))
  expect_identical(arr(c(5L, 1L), "edges"), c(5L, 1L))    # nothing to bury in a pair
  expect_identical(arr(1:6, "edges"), c(1L, 3L, 5L, 6L, 4L, 2L))
})

test_that("an ensemble's verbatim rows verify when the chunks are supplied", {
  # `kind` was made per row but `sources` was not, so rbind_evidence()'s NA fill
  # in the source_text column made every non-skim row unverifiable even with
  # `chunks` in hand.
  local_registries()
  gr_options(embedder = "lexical")
  doc <- paste(c("Revenue rose to 45.2 million dollars in fiscal 2024.",
                 "Headcount grew to 1,204 employees worldwide this year.",
                 "Operating margin fell 5 percent year over year overall."), collapse = "\n\n")
  ch <- gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 40))
  a <- quiet(gr_read(ch, "What was revenue?",
                     mock_echo("Revenue rose to 45.2 million dollars in fiscal 2024."),
                     list(reader = "ensemble", members = c("skim", "retrieve"))))

  v <- gr_verify_evidence(a, ch)
  expect_true(any(v$kind == "verbatim"))
  expect_true(all(v$verified[v$kind == "verbatim"]))    # not NA

  # An id the supplied chunks do not contain reports NA rather than FALSE: there
  # was nothing to compare against, which is not evidence of fabrication.
  # (Ids are positional, so a chunk set that HAS the id compares against
  # whatever text sits there -- see the note on `chunks` in ?gr_verify_evidence.)
  one <- gr_segment(gr_ingest("A single short paragraph and nothing else at all."),
                    list(method = "paragraph", max_tokens = 400))
  expect_identical(nrow(one$chunks), 1L)
  w <- gr_verify_evidence(a, one)
  missing_ids <- w$kind == "verbatim" & !(w$chunk_id %in% one$chunks$chunk_id)
  expect_true(any(missing_ids))
  expect_true(all(is.na(w$verified[missing_ids])))
})

test_that("an unusable setting falls back to its default, whatever kind of unusable", {
  # na_default() covered NA only, so `mmr = "abc"` still clamped to 0 -- pure
  # diversity -- and every other setting still clamped to its own destructive
  # `lo`: max_answer_tokens to 16, top_k to 1, fan_in to 2.
  for (bad in list(NA, "abc", "", c(1, 2), character(0))) {
    s <- suppressWarnings(gr_read_spec(mmr = bad, top_k = bad, max_answer_tokens = bad,
                                       fan_in = bad, rerank_candidates = bad))
    expect_identical(s$mmr, 1, info = paste(format(bad), collapse = ","))
    expect_identical(s$top_k, 6L)
    expect_identical(s$max_answer_tokens, 1500L)
    expect_identical(s$fan_in, 5L)
    expect_identical(s$rerank_candidates, 20L)
  }
  expect_identical(suppressWarnings(gr_segment_spec("paragraph", max_tokens = NA))$max_tokens, 800L)
  # A usable value is still honoured, including one at the destructive extreme.
  expect_identical(gr_read_spec(mmr = 0)$mmr, 0)
  expect_identical(suppressWarnings(gr_read_spec(top_k = 1))$top_k, 1L)
})
