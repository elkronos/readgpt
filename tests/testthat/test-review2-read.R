# test-review2-read.R -- the reading axis, second pass: replies cut off at the
# output cap, what a parallel batch is told it may spend, a prefilter that
# cannot rank, embedding requests in the pre-flight count, and an extracted
# value's quote.

# Four short parts, one chunk each, every one ending "million dollars.", so a
# quotation of that phrase verifies against any of them.
review2_chunks <- function() {
  txt <- paste(sprintf("## Part %d\n\nRevenue in region %d was %d million dollars.", 1:4, 1:4, 1:4),
               collapse = "\n\n")
  quiet(gr_segment(gr_ingest(txt), list(method = "paragraph", max_tokens = 20)))
}

# A reply the model stopped writing at its output cap: ok, carrying the text it
# managed. gr_result() spells every provider's reason "length". Built without a
# model or usage, so the mock fills in the model asked for and counts tokens.
cut_reply <- function(text) {
  structure(list(ok = TRUE, text = text, finish_reason = "max_tokens"), class = "gr_result")
}

# Messages kept quiet, warnings left for the test to see.
quiet_msgs <- function(expr) suppressMessages(expr)

# Answers every prompt the readers send, and cuts off the replies to requests
# made with an output cap in `cut_caps`. Each stage of a read has its own cap
# in review2_spec(), so a test can cut one stage and leave the rest whole.
review2_client <- function(cut_caps = integer(0), text = "million dollars") {
  gr_mock_client(function(m, p) {
    sys <- m[[1]]$content
    last <- m[[length(m)]]$content
    reply <- if (grepl("Rate how useful", sys, fixed = TRUE)) {
      '{"score": 8, "reason": "relevant"}'
    } else if (grepl("reading iteratively", sys, fixed = TRUE)) {
      sprintf('{"can_answer": true, "answer": "%s", "next_query": ""}', text)
    } else if (grepl("You plan how to read a document", sys, fixed = TRUE)) {
      n <- suppressWarnings(as.integer(sub(".*each of the ([0-9]+) sections.*", "\\1", last)))
      preview_plan_json(rep("skim", if (is.na(n)) 1L else n))
    } else text
    if (as.integer(p$max_output) %in% cut_caps) cut_reply(reply) else reply
  })
}

.chunk_cap <- 111L
.summary_cap <- 222L
.answer_cap <- 333L
review2_spec <- function(reader, ...) {
  c(list(reader = reader, max_chunk_tokens = .chunk_cap, max_summary_tokens = .summary_cap,
         max_answer_tokens = .answer_cap), list(...))
}

.review2_readers <- c("stuff", "map_reduce", "refine", "skim", "retrieve", "rerank",
                      "hierarchical", "iterative", "preview", "ensemble")

# ---------------------------------------------------------------------------
# model-output-07: a reply cut off at the output cap makes the answer partial
# ---------------------------------------------------------------------------

test_that("an answer cut off at the output cap is partial, counted and explained, in every reader", {
  ch <- review2_chunks()
  for (r in .review2_readers) {
    a <- quiet(gr_read(ch, "What was revenue?", review2_client(c(.answer_cap, .chunk_cap,
                                                                 .summary_cap)),
                       review2_spec(r)))
    expect_true(a$partial, info = r)
    expect_gte(a$notes$truncated_calls %||% 0, 1, label = r)
    expect_true(any(grepl("cut off at the output cap", readgpt:::partial_reasons(a))), info = r)
  }
  # And the words reach the printed answer.
  a <- quiet(gr_read(ch, "What was revenue?", review2_client(.answer_cap), review2_spec("stuff")))
  expect_output(print(a), "Partial because: 1 response(s) cut off at the output cap", fixed = TRUE)
})

test_that("whole replies are counted as none", {
  ch <- review2_chunks()
  for (r in .review2_readers) {
    a <- quiet(gr_read(ch, "What was revenue?", review2_client(), review2_spec(r)))
    expect_identical(as.integer(a$notes$truncated_calls), 0L, info = r)
    expect_false(any(grepl("cut off", readgpt:::partial_reasons(a))), info = r)
  }
  a <- quiet(gr_read(ch, "What was revenue?", review2_client(), review2_spec("stuff")))
  expect_false(a$partial)
})

test_that("an intermediate reply cut off at the cap makes a whole final answer partial", {
  ch <- review2_chunks()
  q <- "What was revenue?"
  # map answers, hierarchical summaries, skim and preview extractions, and the
  # first refine draft: each is cut, and the reply the answer is taken from is
  # whole.
  cases <- list(map_reduce = .chunk_cap, hierarchical = .summary_cap, skim = .chunk_cap,
                preview = .chunk_cap)
  for (r in names(cases)) {
    a <- quiet(gr_read(ch, q, review2_client(cases[[r]]), review2_spec(r)))
    expect_true(a$partial, info = r)
    expect_gte(a$notes$truncated_calls, 1, label = r)
    whole <- quiet(gr_read(ch, q, review2_client(), review2_spec(r)))
    expect_identical(whole$answer, a$answer, info = r)
  }
  # refine: only the first draft is cut; each revision after it is whole.
  first <- TRUE
  cl <- gr_mock_client(function(m, p) {
    if (first) { first <<- FALSE; return(cut_reply("Revenue in region 1 was")) }
    "million dollars"
  })
  a <- quiet(gr_read(ch, q, cl, review2_spec("refine")))
  expect_true(a$partial)
  expect_identical(a$notes$truncated_calls, 1L)

  # ensemble: one member's answer is cut, the adjudication is whole.
  cl <- gr_mock_client(function(m, p) {
    if (grepl("different reading strategies", m[[1]]$content, fixed = TRUE)) return("both agree")
    if (as.integer(p$max_output) == .chunk_cap) return(cut_reply("Revenue in region"))
    "million dollars"
  })
  a <- quiet(gr_read(ch, q, cl, review2_spec("ensemble", members = c("stuff", "map_reduce"))))
  expect_identical(a$answer, "both agree")
  expect_true(a$partial)
  expect_gte(a$notes$truncated_calls, 1)
})

test_that("tree_merge() counts merge replies cut off at every level", {
  local_registries()
  # Small enough that eight findings take a level of merges before the last.
  gr_register_model("merge-small", context_window = 4000, max_output = 800,
                    input_usd = 0, output_usd = 0)
  spec <- gr_read_spec("map_reduce", model = "merge-small", max_answer_tokens = 300)
  pieces <- rep(paste(rep("finding text words", 200), collapse = " "), 8)
  final <- function(m) grepl("<findings 1>", m[[2]]$content, fixed = TRUE)
  # The levels cut, the final merge whole.
  cl <- gr_mock_client(function(m, p) if (final(m)) "merged" else cut_reply("merged part"))
  out <- quiet(readgpt:::tree_merge(cl, "q", pieces, spec, gr_trace()))
  expect_true(out$ok)
  expect_gte(out$levels, 2L)
  expect_gte(out$truncated_calls, 2L)
  # Only the final merge cut.
  cl <- gr_mock_client(function(m, p) if (final(m)) cut_reply("merged") else "merged part")
  out <- quiet(readgpt:::tree_merge(cl, "q", pieces, spec, gr_trace()))
  expect_identical(out$truncated_calls, 1L)
  # Nothing to merge reports none.
  expect_identical(readgpt:::tree_merge(cl, "q", "one", spec, gr_trace())$truncated_calls, 0L)
})

test_that("an iterative step cut off before its JSON closes says so", {
  ch <- review2_chunks()
  cl <- gr_mock_client(function(m, p) {
    if (grepl("reading iteratively", m[[1]]$content, fixed = TRUE)) {
      return(cut_reply('{"can_answer": true, "answer": "Revenue in reg'))
    }
    "million dollars"
  })
  expect_warning(a <- quiet_msgs(gr_read(ch, "What was revenue?", cl, "iterative")),
                 class = "gr_iterative_degraded", regexp = "cut off at the output cap")
  expect_identical(a$notes$stop_reason, "step cut off at the output cap")
  expect_true(a$partial)
})

# ---------------------------------------------------------------------------
# money-06: a reader's parallel batch is told the most one item can cost
# ---------------------------------------------------------------------------

# Records what each gr_lapply() call is told, and runs it.
record_batches <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$calls <- list()
  orig <- readgpt:::gr_lapply
  testthat::local_mocked_bindings(
    gr_lapply = function(x, fn, parallel = NULL, workers = NULL, key = NULL, label = "task",
                         trace = NULL, client = NULL, item_usd = NULL) {
      seen$calls[[length(seen$calls) + 1L]] <- list(label = label, client = client,
                                                    item_usd = item_usd)
      orig(x, fn, parallel = parallel, workers = workers, key = key, label = label,
           trace = trace, client = client, item_usd = item_usd)
    }, .package = "readgpt", .env = env)
  seen
}

# The dearest single request a trace recorded under one of `labels`, priced as
# the trace prices it.
dearest_step <- function(trace, labels) {
  st <- Filter(function(s) any(startsWith(as.character(s$label %||% ""), labels)), trace$steps)
  max(vapply(st, function(s) gr_estimate_cost(s$model, s$tokens$input, s$tokens$output),
             numeric(1)))
}

test_that("every batch a reader sends carries its client and the most one item can cost", {
  local_registries()
  gr_register_model("priced-batch", context_window = 4000, max_output = 800,
                    input_usd = 2, output_usd = 8)
  seen <- record_batches()
  ch <- review2_chunks()
  # Replies at the full cap, so the recorded requests are as dear as they get.
  cl <- gr_mock_client(function(m, p) {
    if (grepl("Rate how useful", m[[1]]$content, fixed = TRUE))
      return('{"score": 8, "reason": "relevant"}')
    if (grepl("You plan how to read", m[[1]]$content, fixed = TRUE))
      return(preview_plan_json(rep("skim", 4)))
    structure(list(ok = TRUE, text = "million dollars",
                   usage = list(input = NA, output = as.integer(p$max_output))),
              class = "gr_result")
  })
  stages <- list(map_reduce = "map.answer", skim = "skim.extract", rerank = "rerank.score",
                 hierarchical = "hier.summarise", preview = "preview.skim")
  for (r in names(stages)) {
    seen$calls <- list()
    tr <- gr_trace()
    quiet(gr_read(ch, "What was revenue?", cl, review2_spec(r, model = "priced-batch"), trace = tr))
    b <- seen$calls[[1]]
    expect_identical(b$client, cl, info = r)
    expect_true(is.finite(b$item_usd) && b$item_usd > 0, info = r)
    # An upper bound on what one of the batch's requests really cost.
    expect_gte(b$item_usd, dearest_step(tr, stages[[r]]), label = r)
  }
  # A merge level, which is a batch too.
  seen$calls <- list()
  spec <- gr_read_spec("map_reduce", model = "priced-batch", max_answer_tokens = 300)
  tr <- gr_trace()
  quiet(readgpt:::tree_merge(cl, "q", rep(paste(rep("finding text words", 200), collapse = " "), 8),
                             spec, tr))
  b <- Filter(function(x) identical(x$label, "merge group"), seen$calls)[[1]]
  expect_identical(b$client, cl)
  expect_gte(b$item_usd, dearest_step(tr, "merge.level"))

  # A model with no price has no worst case to hold a batch to.
  gr_register_model("free-batch", context_window = 4000, max_output = 800)
  seen$calls <- list()
  quiet(gr_read(ch, "What was revenue?", cl, review2_spec("map_reduce", model = "free-batch")))
  expect_true(is.na(seen$calls[[1]]$item_usd))
})

test_that("a parallel merge level runs where each request is checked when it could pass the limit", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  local_registries()
  gr_register_model("priced-merge", context_window = 4000, max_output = 800,
                    input_usd = 2, output_usd = 8)
  # verbose, so the fallback's message is shown whatever the suite was run with;
  # local_registries() restores the options afterwards.
  gr_options(max_calls = 1000, max_cost_usd = 0.05, workers = 2, verbose = TRUE)
  cl <- gr_mock_client(function(m, p) "merged finding")
  spec <- gr_read_spec("map_reduce", model = "priced-merge", max_answer_tokens = 300,
                       parallel = TRUE)
  pieces <- rep(paste(rep("finding text words", 200), collapse = " "), 8)
  # Close enough to the limit that one merge request reaches it. Sent to
  # workers, the whole level went out: three requests past the limit.
  tr <- gr_trace()
  tr$spent_usd <- 0.045
  msgs <- testthat::capture_messages(out <- readgpt:::tree_merge(cl, "q", pieces, spec, tr))
  expect_true(any(grepl("one at a time", msgs)))
  expect_false(out$ok)
  expect_identical(tr$calls, 1L)
  expect_true(tr$budget_stop)
  expect_identical(tr$stop_reason, "cost")
})

test_that("a parallel map step near the spending limit stops where a sequential one would", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  local_registries()
  gr_register_model("priced-map", context_window = 4000, max_output = 800,
                    input_usd = 2, output_usd = 8)
  gr_options(max_calls = 1000, max_cost_usd = 0.05, workers = 2)
  ch <- review2_chunks()
  cl <- gr_mock_client(function(m, p) "million dollars")
  spec <- gr_read_spec("map_reduce", model = "priced-map", parallel = TRUE)
  run <- function(parallel) {
    spec$parallel <- parallel
    tr <- gr_trace()
    tr$spent_usd <- 0.04995
    quiet(readgpt:::read_map_reduce(ch, "What was revenue?", cl, spec, tr))
    tr
  }
  s <- run(FALSE)
  p <- run(TRUE)
  expect_identical(p$calls, s$calls)
  expect_lt(p$calls, nrow(ch$chunks))
  expect_true(p$budget_stop)
})

# ---------------------------------------------------------------------------
# tokenize-embed-02: a word-matching prefilter that cannot rank says so
# ---------------------------------------------------------------------------

# Twenty paragraphs, the answer in the tenth, none sharing a word with the
# paraphrased question below.
blind_chunks <- function() {
  paras <- c(sprintf("Paragraph %d describes the weather at the coastal site on day %d.", 1:9, 1:9),
             "Turnover for the year reached 45.2 million dollars, up from 39.8 million.",
             sprintf("Paragraph %d describes the weather at the coastal site on day %d.", 11:20,
                     11:20))
  quiet(gr_segment(gr_ingest(paste(paras, collapse = "\n\n")),
                   list(method = "paragraph", max_tokens = 20)))
}
blind_q <- "How much money did this firm bring in?"

# Scores 9 for the turnover paragraph and 1 for the rest; embeds the question
# and that paragraph close together when asked to.
blind_client <- function() {
  gr_mock_client(function(m, p) {
    if (grepl("Rate how useful", m[[1]]$content, fixed = TRUE)) {
      hit <- grepl("Turnover", m[[3]]$content, fixed = TRUE)
      return(sprintf('{"score": %d, "reason": "r"}', if (hit) 9L else 1L))
    }
    if (grepl("Turnover", m[[2]]$content, fixed = TRUE)) "45.2 million dollars." else "NOT_IN_DOCUMENT"
  }, embed_handler = function(texts, params) {
    near <- grepl("Turnover|much money", texts)
    t(vapply(near, function(x) if (x) c(1, 0.1) else c(0.1, 1), numeric(2)))
  })
}

scored_ids <- function(cl, ch) {
  labs <- vapply(cl$calls(), function(x) x$label, character(1))
  ex <- vapply(cl$calls()[labs == "rerank.score"], function(x) x$messages[[3]]$content,
               character(1))
  vapply(ex, function(e) which(vapply(ch$chunks$text, function(t) grepl(t, e, fixed = TRUE),
                                      logical(1)))[1], integer(1), USE.NAMES = FALSE)
}

test_that("a question sharing no word with any chunk is ranked by embeddings, and says so", {
  ch <- blind_chunks()
  expect_true(all(readgpt:::bm25_scores(ch$chunks$text, blind_q) == 0))
  cl <- blind_client()
  expect_warning(a <- quiet_msgs(gr_read(ch, blind_q, cl, list(reader = "rerank",
                                                                 rerank_candidates = 4))),
                 class = "gr_rerank_prefilter", regexp = "embedding")
  # The first four chunks in document order used to be the candidates.
  expect_false(identical(sort(scored_ids(cl, ch)), 1:4))
  expect_true(10L %in% scored_ids(cl, ch))
  expect_identical(a$answer, "45.2 million dollars.")
  expect_identical(a$notes$prefilter, "embedding")
})

test_that("when embeddings cannot rank either, the candidates are an evenly spread sample", {
  local_registries()
  gr_options(embedder = "lexical")
  ch <- blind_chunks()
  cl <- blind_client()
  expect_warning(a <- quiet_msgs(gr_read(ch, blind_q, cl, list(reader = "rerank",
                                                                 rerank_candidates = 4))),
                 class = "gr_rerank_prefilter", regexp = "spread evenly")
  expect_identical(scored_ids(cl, ch), c(3L, 8L, 13L, 18L))
  # A negative from a sample is not a negative about the document.
  expect_true(is_not_found(a$answer))
  expect_true(a$partial)
  expect_identical(a$notes$prefilter, "spread")
  expect_true(any(grepl("evenly spread sample", readgpt:::partial_reasons(a))))
})

test_that("a prefilter that can rank, or a candidate list that is every chunk, is left alone", {
  ch <- blind_chunks()
  cl <- blind_client()
  a <- quiet(gr_read(ch, "What was the turnover for the year?", cl,
                     list(reader = "rerank", rerank_candidates = 4)))
  expect_identical(a$notes$prefilter, "bm25")
  expect_length(cl$embeds(), 0L)
  cl <- blind_client()
  expect_no_warning(a <- quiet_msgs(gr_read(ch, blind_q, cl,
                                            list(reader = "rerank", rerank_candidates = 50))))
  expect_identical(a$notes$prefilter, "bm25")
  expect_length(cl$embeds(), 0L)
  expect_identical(a$answer, "45.2 million dollars.")
})

# ---------------------------------------------------------------------------
# money-05: embedding requests are in the pre-flight count
# ---------------------------------------------------------------------------

# Stands in for an OpenAI-shaped server: embeddings, and chat completions that
# answer an iterative step with JSON and anything else with text.
fake_openai <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$embed <- 0L
  seen$chat <- 0L
  testthat::local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      out <- if (grepl("/embeddings$", url)) {
        seen$embed <- seen$embed + 1L
        k <- length(body$input)
        list(data = lapply(seq_len(k), function(i) list(embedding = as.list(c(i, 1, 0.5)))),
             usage = list(prompt_tokens = 10L * k))
      } else {
        seen$chat <- seen$chat + 1L
        sys <- as.character(body$messages[[1]]$content)
        txt <- if (grepl("reading iteratively", sys, fixed = TRUE))
          '{"can_answer": false, "answer": "", "next_query": "region seven"}'
        else "45.2 million"
        list(choices = list(list(message = list(content = txt), finish_reason = "stop")),
             usage = list(prompt_tokens = 100L, completion_tokens = 5L))
      }
      structure(list(url = url, status_code = 200L,
                     headers = structure(list(`content-type` = "application/json"),
                                         class = c("insensitive", "list")),
                     content = charToRaw(as.character(jsonlite::toJSON(out, auto_unbox = TRUE)))),
                class = "response")
    }, .package = "httr", .env = env)
  seen
}

api_embed_client <- function() {
  gr_register_model("priced-embed2", context_window = 8191L, max_output = 0L,
                    input_usd = 0.02, kind = "embedding", dimensions = 3L)
  gr_client(api_key = "sk-test", base_url = "https://embed.invalid/v1", api = "chat",
            model = "gpt-4o-mini", embedding_model = "priced-embed2")
}

many_chunks <- function(n = 200) {
  txt <- paste(sprintf("Paragraph %d reports that revenue in region %d was %d million.",
                       seq_len(n), seq_len(n), seq_len(n)), collapse = "\n\n")
  quiet(gr_segment(gr_ingest(txt), list(method = "paragraph", max_tokens = 20)))
}

preflight_detail <- function(tr) {
  Filter(function(s) identical(s$label, "preflight"), tr$steps)[[1]]$detail
}

test_that("a cap too small for retrieve's embedding requests refuses the read up front", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE, max_calls = 3)
  seen <- fake_openai()
  cl <- api_embed_client()
  ch <- many_chunks()
  expect_identical(nrow(ch$chunks), 200L)
  tr <- gr_trace()
  # 201 texts at 64 a request is four requests, and one for the answer.
  expect_error(quiet(gr_read(ch, "What was revenue in region 7?", cl, "retrieve", trace = tr)),
               class = "gr_call_cap", regexp = "5 more requests (4 of them for embeddings;", fixed = TRUE)
  # It used to send three, fall back to lexical vectors, and never ask.
  expect_identical(seen$embed, 0L)
  expect_identical(seen$chat, 0L)

  gr_options(max_calls = 5)
  tr <- gr_trace()
  a <- quiet(gr_read(ch, "What was revenue in region 7?", cl, "retrieve", trace = tr))
  expect_false(a$partial)
  pf <- preflight_detail(tr)
  expect_identical(pf$est_calls, 5L)
  expect_identical(pf$embed_calls, 4L)
  expect_identical(tr$calls, 5L)
  expect_identical(c(seen$embed, seen$chat), c(4L, 1L))
})

test_that("embeddings already cached are not counted against the cap", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = TRUE, max_calls = 100)
  seen <- fake_openai()
  cl <- api_embed_client()
  ch <- many_chunks()
  q <- "What was revenue in region 7?"
  quiet(gr_read(ch, q, cl, "retrieve"))
  gr_options(max_calls = 1)
  tr <- gr_trace()
  a <- quiet(gr_read(ch, q, cl, "retrieve", trace = tr))
  expect_false(a$partial)
  expect_identical(preflight_detail(tr)$embed_calls, 0L)
  expect_identical(tr$calls, 1L)
})

test_that("iterative counts its chunk and query embeddings, and a step a round and the answer", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE, max_calls = 100)
  seen <- fake_openai()
  cl <- api_embed_client()
  ch <- many_chunks(100)
  tr <- gr_trace()
  quiet(gr_read(ch, "What was revenue?", cl, list(reader = "iterative", max_rounds = 3,
                                                  top_k = 2), trace = tr))
  pf <- preflight_detail(tr)
  # Two requests for the 100 chunks, one query a round; three steps and the
  # answer.
  expect_identical(pf$embed_calls, 2L + 3L)
  expect_identical(pf$est_calls, 2L + 3L + 3L + 1L)
  expect_lte(tr$calls, pf$est_calls)

  # A client that brings its own embed function sends nothing to count.
  tr <- gr_trace()
  quiet(gr_read(ch, "What was revenue?", mock_iterative_loop(refuse = 5L),
                list(reader = "iterative", max_rounds = 3, top_k = 2), trace = tr))
  expect_identical(preflight_detail(tr)$embed_calls, 0L)
  expect_identical(preflight_detail(tr)$est_calls, 4L)
})

test_that("rerank counts the embedding requests of a prefilter that cannot rank", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE, max_calls = 100)
  seen <- fake_openai()
  cl <- api_embed_client()
  ch <- blind_chunks()
  tr <- gr_trace()
  quiet(gr_read(ch, blind_q, cl, list(reader = "rerank", rerank_candidates = 4), trace = tr))
  expect_identical(preflight_detail(tr)$embed_calls, 1L)
  expect_identical(seen$embed, 1L)
  tr <- gr_trace()
  quiet(gr_read(ch, "What was the turnover?", cl, list(reader = "rerank", rerank_candidates = 4),
                trace = tr))
  expect_identical(preflight_detail(tr)$embed_calls, 0L)
})

# ---------------------------------------------------------------------------
# model-output-04: gr_verify_evidence() keeps the extract reader's verdict
# ---------------------------------------------------------------------------

test_that("a quote that does not carry its value is unverified in gr_verify_evidence() too", {
  doc <- "We ran a randomised controlled trial. We enrolled 120 people. The trial was funded by Pfizer."
  fields <- gr_fields(n = gr_field("participants enrolled", "integer"),
                      design = gr_field("study design"))
  cl <- gr_mock_client(function(m, p) paste0(
    '{"n": 5000, "n__quote": "We enrolled 120 people.", "design": "randomised controlled trial",',
    ' "design__quote": "We ran a randomised controlled trial."}'))
  ch <- quiet(gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 200)))
  a <- quiet(gr_read(ch, "Extract the study.", cl, gr_read_spec("extract", fields = fields)))
  expect_identical(a$evidence$verified[a$evidence$field == "n"], FALSE)
  for (v in list(gr_verify_evidence(a), gr_verify_evidence(a, ch))) {
    # The sentence is in the document, and it does not say 5000.
    expect_identical(v$verified, c(FALSE, TRUE))
    expect_identical(v$match, c(1, 1))
  }
})
