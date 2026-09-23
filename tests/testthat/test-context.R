# test-context.R -- what reaches the context window, and in what order.
#
# The package does not hold a conversation, so there is no dialogue to compress.
# What it does instead is fit: a budget computed before the prompt is built,
# selection by relevance, and position used as a resource. These are the two
# places that were doing it wrong.

ctx_doc <- function(paras = 12L, per = 40L) {
  txt <- vapply(seq_len(paras), function(i) {
    paste(c(sprintf("Paragraph %d about topic %d.", i, i),
            rep(sprintf("filler sentence for paragraph %d", i), per)), collapse = " ")
  }, character(1))
  gr_ingest(paste(txt, collapse = "\n\n"))
}

# ---------------------------------------------------------------------------
# iterative: whole chunks, chosen by score, and only what was actually shown.
# ---------------------------------------------------------------------------

test_that("iterative_fit keeps whole chunks and drops the weakest, not the newest", {
  d <- data.frame(chunk_id = 1:4, text = c("alpha alpha", "beta beta", "gamma gamma",
                                           "delta delta"),
                  page = NA_integer_, section = NA_character_, stringsAsFactors = FALSE)
  # Round one took chunks 1 and 2, round two took 3 and 4; chunk 2 scored worst.
  seen <- c(1L, 2L, 3L, 4L)
  score <- c(0.9, 0.1, 0.8, 0.7)
  big <- readgpt:::iterative_fit(d, seen, score, 1e6, "relevance")
  expect_setequal(big$rows, seen)
  expect_length(big$dropped, 0L)

  # A budget that fits three of the four. The old code truncated the tail, which
  # dropped the material the loop had just gone back for; the weakest chunk is
  # the one that should go.
  one <- readgpt:::gr_count_tokens(readgpt:::render_chunks(d[1, , drop = FALSE]))
  tight <- readgpt:::iterative_fit(d, seen, score, one * 3L, "relevance")
  expect_length(tight$rows, 3L)
  expect_identical(tight$dropped, 2L)
  expect_false(2L %in% tight$rows)

  # Whole chunks: every surviving chunk's text is present in full, so a quote
  # from it can still be found. A mid-chunk cut breaks that for a reason that has
  # nothing to do with the document.
  for (i in tight$rows) expect_true(grepl(d$text[i], tight$body, fixed = TRUE))
})

test_that("iterative_fit orders the prompt by context_order", {
  d <- data.frame(chunk_id = 1:3, text = c("aa", "bb", "cc"), page = NA_integer_,
                  section = NA_character_, stringsAsFactors = FALSE)
  seen <- c(3L, 1L, 2L)                       # accumulation order
  score <- c(0.9, 0.5, 0.1)                   # chunk 3 best, chunk 2 worst
  rel <- readgpt:::iterative_fit(d, seen, score, 1e6, "relevance")
  expect_identical(rel$rows, c(3L, 1L, 2L))
  # Document order has to come from the chunk ids, not from where the chunk
  # happens to sit in the accumulation.
  doc <- readgpt:::iterative_fit(d, seen, score, 1e6, "document")
  expect_identical(doc$rows, c(1L, 2L, 3L))
})

test_that("a chunk that did not fit the prompt is not reported as read", {
  # The provenance fault: chunks_used and the evidence table were built from
  # everything SEEN, including chunks truncation had removed from the prompt --
  # claiming support the answer does not have.
  local_registries()
  gr_register_model("tiny-ctx", context_window = 900L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  doc <- ctx_doc(paras = 10L)
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 120)))
  ans <- quiet(gr_read(ch, "What is topic 7?", mock_iterative_loop(refuse = 4L),
                       list(reader = "iterative", model = "tiny-ctx", max_rounds = 4L,
                            top_k = 3L)))
  expect_gt(ans$notes$chunks_seen, length(ans$chunks_used))
  expect_gt(ans$notes$chunks_dropped, 0L)
  expect_equal(nrow(ans$evidence), length(ans$chunks_used))
  expect_true(all(ans$evidence$chunk_id %in% ans$chunks_used))
  expect_true(ans$partial)
})

test_that("the same holds when the model answers mid-loop", {
  # The provenance test above leaves through max_rounds, so the FINAL fit decides
  # what is reported. The `can_answer = TRUE` return is a separate exit with its
  # own accounting, and restoring the old mid-loop truncation there changes
  # nothing the other test can see.
  local_registries()
  gr_register_model("tiny-ctx", context_window = 900L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  round <- 0L
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("reading iteratively", messages[[1]]$content, fixed = TRUE)) {
      round <<- round + 1L
      if (round == 1L) {
        return('{"can_answer": false, "answer": "", "next_query": "topic 7 detail"}')
      }
      return('{"can_answer": true, "answer": "FROM ROUND TWO", "next_query": ""}')
    }
    "x"
  })
  doc <- ctx_doc(paras = 10L)
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 120)))
  ans <- quiet(gr_read(ch, "What is topic 7?", cl,
                       list(reader = "iterative", model = "tiny-ctx", max_rounds = 4L,
                            top_k = 3L)))
  expect_identical(ans$answer, "FROM ROUND TWO")
  expect_identical(ans$notes$stop_reason, "model satisfied")
  expect_gt(ans$notes$chunks_seen, length(ans$chunks_used))
  expect_gt(ans$notes$chunks_dropped, 0L)
  expect_true(all(ans$evidence$chunk_id %in% ans$chunks_used))
  expect_true(ans$partial)
})

test_that("iterative's final call is budgeted, which it never used to be", {
  # Every other reader sizes its final prompt; this one rendered everything
  # gathered, so several rounds of top_k chunks went out over the window and the
  # provider rejected the call after the whole loop had been paid for.
  local_registries()
  gr_register_model("tiny-ctx", context_window = 900L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  doc <- ctx_doc(paras = 10L)
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 120)))
  cl <- mock_iterative_loop(refuse = 4L)
  quiet(gr_read(ch, "What is topic 7?", cl,
                list(reader = "iterative", model = "tiny-ctx", max_rounds = 4L, top_k = 3L)))
  final <- Filter(function(x) identical(x$label, "iterative.final"), cl$calls())
  expect_length(final, 1L)
  body <- paste(vapply(final[[1]]$messages, function(m) as.character(m$content), character(1)),
                collapse = "\n")
  expect_lte(gr_count_tokens(body), as.integer(gr_model_info("tiny-ctx")$context_window))
})

# ---------------------------------------------------------------------------
# restate: the question at both ends of a long prompt.
# ---------------------------------------------------------------------------

test_that("a long body gets the question before the excerpts as well as after", {
  short <- "One short excerpt."
  long <- paste(rep("a sentence with several words in it", 400L), collapse = " ")
  expect_false(readgpt:::restate_now(short))
  expect_true(readgpt:::restate_now(long))

  m_short <- readgpt:::answer_messages("What was revenue?", short)
  m_long <- readgpt:::answer_messages("What was revenue?", long)
  # The question is already last in both -- that was never the gap.
  expect_match(m_short[[3]]$content, "What was revenue?", fixed = TRUE)
  expect_match(m_long[[3]]$content, "What was revenue?", fixed = TRUE)
  # The other end is what was missing.
  expect_false(grepl("What was revenue?", m_short[[2]]$content, fixed = TRUE))
  expect_match(m_long[[2]]$content, "^Question: What was revenue\\?")
  # And the message COUNT does not change, so nothing that indexes these
  # positionally shifts.
  expect_length(m_short, 3L)
  expect_length(m_long, 3L)
})

test_that("restate can be forced on or off", {
  short <- "One short excerpt."
  long <- paste(rep("a sentence with several words in it", 400L), collapse = " ")
  expect_true(readgpt:::restate_now(short, "always"))
  expect_false(readgpt:::restate_now(long, "never"))
  expect_match(readgpt:::answer_messages("Q?", short, restate = "always")[[2]]$content,
               "^Question: Q\\?")
  expect_false(grepl("Question:", readgpt:::answer_messages("Q?", long, restate = "never")[[2]]$content,
                     fixed = TRUE))
})

test_that("restate is a read setting, so gr_compare() can measure it", {
  # The point of it being a setting rather than a rule: whether repeating the
  # question helps is a question about a corpus and a model, and this is the
  # machinery that can answer it instead of the package asserting one.
  expect_identical(gr_read_spec("stuff")$restate, "auto")
  expect_identical(gr_read_spec("stuff", restate = "never")$restate, "never")
  expect_error(gr_read_spec("stuff", restate = "sometimes"))

  cl <- mock_echo()
  doc <- ctx_doc(paras = 6L)
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 400)))
  quiet(gr_read(ch, "What is topic 3?", cl, list(reader = "stuff", restate = "always")))
  body <- cl$calls()[[1]]$messages[[2]]$content
  expect_match(body, "^Question: What is topic 3\\?")
})

test_that("the question reaches the far end of the iterative step prompt too", {
  # That prompt asked first and then handed over several thousand tokens, which
  # is the opposite of what answer_messages() does everywhere else.
  local_registries()
  gr_register_model("wide-ctx", context_window = 128000L, max_output = 4096L,
                    input_usd = 0, output_usd = 0)
  doc <- ctx_doc(paras = 14L, per = 60L)
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 900)))
  cl <- mock_iterative_loop(refuse = 3L)
  quiet(gr_read(ch, "What is topic 9?", cl,
                list(reader = "iterative", model = "wide-ctx", max_rounds = 3L, top_k = 4L)))
  steps <- Filter(function(x) identical(x$label, "iterative.step"), cl$calls())
  expect_gt(length(steps), 0L)
  last <- steps[[length(steps)]]
  excerpts <- last$messages[[length(last$messages)]]$content
  expect_match(excerpts, "Again, the question: What is topic 9\\?")
})
