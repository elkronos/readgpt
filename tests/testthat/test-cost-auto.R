# test-cost-auto.R
#
# answer_document()'s default recipe, "auto", which picks "fast" or "thorough"
# from the document's length; and the spending limit, which is checked against
# what a run spends as it goes rather than against an estimate made once.

# A model with round prices, so a test can say what a run costs. A reply token
# costs a cent, so a few requests with short replies reach a limit of cents,
# while the input the pre-flight check prices stays near zero.
local_priced_model <- function(env = parent.frame()) {
  local_registries(env)
  gr_register_model("price-test", context_window = 200000, max_output = 8000,
                    input_usd = 1, output_usd = 10000)
  "price-test"
}

priced_spec <- function(reader, ...) list(reader = reader, model = "price-test", ...)

test_chunks <- function() {
  ch <- quiet(gr_segment(sample_doc(6, 6), list(method = "paragraph", max_tokens = 120)))
  stopifnot(nrow(ch$chunks) >= 10L)
  ch
}

last_call_usd <- function(tr, model = "price-test") {
  calls <- Filter(function(s) !identical(s$kind, "local") && !is.null(s$tokens), tr$steps)
  last <- calls[[length(calls)]]
  gr_estimate_cost(model, last$tokens$input, last$tokens$output)
}

# ---------------------------------------------------------------------------
# "auto"
# ---------------------------------------------------------------------------

test_that("answer_document() reads a short document in one request by default", {
  local_clean_cache()
  cl <- mock_echo("Revenue was 45.2 million dollars.")
  a <- quiet(answer_document(readgpt_example(), "What was revenue?", client = cl))
  expect_identical(a$recipe, "fast")
  expect_identical(a$reader, "stuff")
  expect_identical(a$notes$auto_recipe, "fast")
  expect_length(cl$calls(), 1L)
  expect_identical(a$trace$meta$recipe, "auto")
  step <- Filter(function(s) identical(s$label, "auto_recipe"), a$trace$steps)
  expect_length(step, 1L)
  expect_identical(step[[1]]$detail$chose, "fast")
  expect_equal(step[[1]]$detail$tokens, gr_count_tokens(quiet(gr_ingest(readgpt_example()))$text))
  expect_equal(step[[1]]$detail$limit, 50000)

  # A recipe named outright is used as given, and "auto" leaves no mark.
  b <- quiet(answer_document(readgpt_example(), "What was revenue?", "thorough",
                             client = mock_echo()))
  expect_identical(b$reader, "map_reduce")
  expect_null(b$notes[["auto_recipe", exact = TRUE]])
  expect_identical(b$trace$meta$recipe, "thorough")
})

long_doc <- function(n = 2100L) {
  paste(rep(paste("The committee reviewed the annual budget and approved the spending",
                  "plan for the coming year after a long debate."), n), collapse = "\n\n")
}

test_that("answer_document() reads a long document chunk by chunk by default", {
  local_clean_cache()
  long <- long_doc()
  expect_gt(gr_count_tokens(long), 50000)
  cl <- mock_echo("The plan was approved.")
  a <- quiet(answer_document(long, "What was approved?", client = cl))
  expect_identical(a$recipe, "thorough")
  expect_identical(a$reader, "map_reduce")
  expect_identical(a$notes$auto_recipe, "thorough")
  # thorough's segmentation, not fast's: chunks of at most 1,200 tokens.
  expect_lte(a$segmentation$max, 1200L)
  expect_gt(length(cl$calls()), a$segmentation$n)
  expect_false(a$partial)
})

test_that("'auto' sends in one request only what sits well inside one request", {
  local_registries()
  fast <- gr_recipes("fast")
  pick <- function(tokens, model = NULL) {
    f <- if (is.null(model)) fast else readgpt:::apply_overrides(fast, list(model = model))
    readgpt:::pick_auto_recipe(tokens, f)
  }
  expect_identical(pick(50000), "fast")
  expect_identical(pick(50001), "thorough")
  # A document whose size is unknown is read the way that works at any size.
  expect_identical(pick(NA), "thorough")
  expect_identical(pick(NULL), "thorough")

  # A small window lowers the threshold to half of what one request holds.
  gr_register_model("small-window", context_window = 20000, max_output = 4000,
                    input_usd = 1, output_usd = 1)
  room <- gr_budget("small-window", reserve_output = fast$read$max_answer_tokens)$input
  expect_lt(room / 2, 50000)
  expect_identical(pick(floor(room / 2), "small-window"), "fast")
  expect_identical(pick(floor(room / 2) + 1, "small-window"), "thorough")
  # Every model named must have room: the smallest decides.
  expect_identical(readgpt:::pick_auto_recipe(floor(room / 2) + 1, fast,
                                              c(fast$read$model, "small-window")), "thorough")
  expect_identical(readgpt:::pick_auto_recipe(floor(room / 2), fast,
                                              c(fast$read$model, "small-window")), "fast")
  # A model whose limits are a guess is no basis for sending everything at once.
  expect_identical(suppressWarnings(pick(1000, "a-model-from-next-year")), "thorough")
})

test_that("'auto' measures the client's model as well, unless a model is named", {
  local_registries()
  local_clean_cache()
  gr_register_model("my-local-4k", context_window = 4000, max_output = 1000,
                    input_usd = 0, output_usd = 0)
  asked <- character(0)
  cl <- gr_backend_client(function(messages, params) {
    asked <<- c(asked, params$model)
    "The plan was approved."
  }, model = "my-local-4k")
  doc <- sample_doc(6, 6)
  expect_gt(gr_count_tokens(doc), 1500)
  # Short for the recipe's model, long for the one behind the client.
  a <- quiet(answer_document(doc, "Q?", client = cl))
  expect_identical(a$recipe, "thorough")
  step <- Filter(function(s) identical(s$label, "auto_recipe"), a$trace$steps)[[1]]$detail
  expect_true("my-local-4k" %in% step$models)
  # Naming the model says which one answers.
  b <- quiet(answer_document(doc, "Q?", client = cl, model = "gpt-5.6-terra"))
  expect_identical(b$recipe, "fast")
})

test_that("the choice counts the text as one request carries it", {
  local_clean_cache()
  lines <- sprintf("Item %d: the committee approved the plan.", 1:3700)
  joined <- paste(lines, collapse = "\n")
  # One block per element makes the sum of the blocks' counts larger than the
  # count of the text they make.
  expect_gt(sum(gr_count_tokens(lines)), 50000)
  expect_lt(gr_count_tokens(joined), 50000)
  a <- quiet(answer_document(lines, "Q?", client = mock_echo()))
  b <- quiet(answer_document(joined, "Q?", client = mock_echo()))
  expect_identical(a$recipe, "fast")
  expect_identical(b$recipe, "fast")
})

test_that("a replay repeats the recorded choice", {
  local_registries()
  local_clean_cache()
  gr_register_model("my-local-4k", context_window = 4000, max_output = 1000,
                    input_usd = 0, output_usd = 0)
  cl <- gr_backend_client(function(messages, params) "The plan was approved.",
                          model = "my-local-4k")
  doc <- sample_doc(6, 6)
  a <- quiet(answer_document(doc, "Q?", client = cl))
  expect_identical(a$recipe, "thorough")
  # The replay client carries the recorded requests' model, which has room for
  # the whole document; made again, the choice would be "fast" and every
  # request a miss.
  rp <- gr_replay_client(a$trace)
  again <- quiet(answer_document(doc, "Q?", client = rp))
  expect_identical(again$recipe, "thorough")
  expect_identical(again$answer, a$answer)
  expect_identical(rp$stats()$misses, 0L)
  step <- Filter(function(s) identical(s$label, "auto_recipe"), again$trace$steps)[[1]]$detail
  expect_true(isTRUE(step$replayed))
})

test_that("overrides apply to the recipe 'auto' picks, and warn only for that one", {
  local_clean_cache()
  # fast's own cap would make this one chunk; the override makes it several.
  a <- quiet(answer_document(readgpt_example(), "Q?", client = mock_echo(), max_tokens = 100))
  expect_identical(a$recipe, "fast")
  expect_gt(a$segmentation$n, 1L)
  expect_lte(a$segmentation$max, 100L)

  # max_tokens = 100 puts thorough's 120-token overlap out of range. A run that
  # reads with fast has nothing to warn about.
  expect_no_warning(suppressMessages(
    answer_document(readgpt_example(), "Q?", client = mock_echo(), max_tokens = 100)))
  # One that reads with thorough does. A small window makes this document long.
  local_registries()
  readgpt:::gr_flush_caches()
  gr_register_model("small-window", context_window = 4000, max_output = 1000,
                    input_usd = 0, output_usd = 0)
  w <- NULL
  b <- withCallingHandlers(
    suppressMessages(answer_document(sample_doc(6, 6), "Q?", client = mock_echo(),
                                     model = "small-window", max_tokens = 100)),
    gr_clamped = function(c) {
      w <<- c(w, conditionMessage(c))
      invokeRestart("muffleWarning")
    })
  expect_identical(b$recipe, "thorough")
  expect_length(w, 1L)
  expect_match(w, "overlap_tokens")
})

test_that("an override that applies to neither recipe fails before the document is read", {
  local_clean_cache()
  cl <- mock_echo()
  expect_error(answer_document(readgpt_example(), "Q?", client = cl, not_a_setting = 1),
               class = "gr_unknown_override")
  expect_length(ls(readgpt:::gr_state$doc_cache), 0L)
  expect_length(cl$calls(), 0L)
})

test_that("a warning both recipes raise is shown before the document is read", {
  local_clean_cache()
  seen <- character(0)
  expect_error(withCallingHandlers(
    answer_document("no/such/report.pdf", "Q?", client = mock_echo(), max_answer_tokens = 5),
    gr_clamped = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    }), class = "gr_file_not_found")
  expect_length(seen, 1L)
  expect_match(seen, "max_answer_tokens")
  # Shown once, not again when the chosen recipe's own warnings are replayed.
  seen <- character(0)
  withCallingHandlers(
    suppressMessages(answer_document(readgpt_example(), "Q?", client = mock_echo(),
                                     max_answer_tokens = 5)),
    gr_clamped = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_length(seen, 1L)
})

test_that("choosing a recipe adds no warning of its own", {
  local_registries()
  n_unknown <- function(...) {
    readgpt:::gr_flush_caches()
    n <- 0L
    suppressWarnings(withCallingHandlers(
      suppressMessages(answer_document(readgpt_example(), "Q?", ..., client = mock_echo(),
                                       model = "a-model-from-next-year")),
      gr_unknown_model = function(w) {
        n <<- n + 1L
        invokeRestart("muffleWarning")
      }))
    n
  }
  # A model readgpt does not know has guessed limits, so "auto" reads with
  # thorough; it should warn exactly as often as thorough named outright.
  expect_gt(n_unknown("thorough"), 0L)
  expect_identical(n_unknown(), n_unknown("thorough"))
})

test_that("functions that need one fixed recipe refuse 'auto'", {
  cl <- mock_echo()
  expect_error(quiet(gr_read_many(readgpt_example(), "Q?", "auto", client = cl)),
               class = "gr_bad_recipe")
  expect_error(quiet(gr_compare(readgpt_example(), "Q?", c("auto", "fast"), client = cl)),
               class = "gr_bad_recipe")
  expect_error(readgpt:::as_recipe("auto"), class = "gr_bad_recipe")
  expect_error(gr_recipes("auto"), "not a recipe")
  expect_length(cl$calls(), 0L)
  # Nor can a reader take the name: answer_document() would never reach it.
  local_registries()
  expect_error(gr_register_reader("auto", function(chunks, question, client, spec, trace) NULL,
                                  signature = "all|1|none"),
               class = "gr_reserved_name")
  # The two candidates ingest alike, which is what lets "auto" ingest once.
  expect_identical(gr_recipes("fast")$ingest, gr_recipes("thorough")$ingest)
})

# ---------------------------------------------------------------------------
# The spending limit
# ---------------------------------------------------------------------------

test_that("a run stops once what it has spent reaches the spending limit", {
  model <- local_priced_model()
  local_clean_cache()
  gr_options(max_cost_usd = 0.2, max_calls = 1000)
  ch <- test_chunks()
  cl <- mock_echo("Some answer.")
  tr <- gr_trace()
  a <- quiet(gr_read(ch, "Q?", cl, priced_spec("map_reduce"), trace = tr))

  expect_true(tr$budget_stop)
  expect_identical(tr$stop_reason, "cost")
  # It stopped before reading everything...
  expect_lt(length(cl$calls()), nrow(ch$chunks))
  # ...once it had reached the limit, and it passed it by one request at most.
  expect_gte(tr$spent_usd, 0.2)
  expect_lt(tr$spent_usd - last_call_usd(tr), 0.2)
  expect_equal(tr$spent_usd, sum(gr_trace_cost(tr)$usd))

  expect_true(a$partial)
  expect_identical(a$notes$cost_cap_reached, 0.2)
  expect_null(a$notes[["call_cap_reached", exact = TRUE]])
  out <- paste(utils::capture.output(print(a)), collapse = "\n")
  expect_match(out, "stopped at the $0.2 spending limit", fixed = TRUE)
})

test_that("requests a limit stopped are not counted as failed", {
  model <- local_priced_model()
  local_clean_cache()
  gr_options(max_cost_usd = 0.2, max_calls = 1000)
  ch <- test_chunks()

  mr <- quiet(gr_read(ch, "Q?", mock_echo("Some answer."), priced_spec("map_reduce")))
  expect_identical(mr$notes$failed_calls, 0L)
  expect_false(grepl("failed", paste(utils::capture.output(print(mr)), collapse = "\n")))

  sk <- quiet(gr_read(ch, "Q?", mock_echo("The cohort comprised 482 participants."),
                      priced_spec("skim")))
  expect_true(sk$partial)
  expect_identical(sk$notes$failed_calls, 0L)
  # And when no chunk it reached held any evidence.
  sk0 <- quiet(gr_read(ch, "Q?", mock_echo("NONE"), priced_spec("skim")))
  expect_true(sk0$partial)
  expect_identical(sk0$notes$failed_calls, 0L)

  hi <- quiet(gr_read(ch, "Q?", mock_echo("A summary."), priced_spec("hierarchical")))
  expect_true(hi$partial)
  expect_identical(hi$notes$failed_summaries, 0L)

  fields <- gr_fields(design = "The study design")
  ex <- quiet(gr_read(ch, "Q?", gr_mock_client(function(m, p) '{"design": null}'),
                      priced_spec("extract", fields = fields)))
  expect_true(ex$partial)
  expect_identical(ex$notes$failed_calls, 0L)

  # preview: the plan's reply alone passes the limit, so no section is skimmed.
  pv_ch <- quiet(gr_segment(sample_doc(4, 2), list(method = "structural", max_tokens = 80)))
  pv <- quiet(gr_read(pv_ch, "Q?", mock_planner(rep("skim", 4)), priced_spec("preview")))
  expect_true(pv$partial)
  expect_identical(pv$notes$failed_calls, 0L)

  # A request that did fail still counts.
  n <- 0L
  flaky <- gr_mock_client(function(m, p) {
    n <<- n + 1L
    if (n == 1L) stop("simulated transport failure")
    "Some answer."
  })
  mr2 <- quiet(gr_read(ch, "Q?", flaky, priced_spec("map_reduce")))
  expect_identical(mr2$notes$failed_calls, 1L)
})

test_that("a run is not refused for what its replies might cost", {
  model <- local_priced_model()
  local_clean_cache()
  # Every reply at its cap would cost far more than this; the input does not,
  # and neither do the replies this run gets.
  gr_options(max_cost_usd = 5, max_calls = 1000)
  ch <- test_chunks()
  tr <- gr_trace()
  a <- quiet(gr_read(ch, "Q?", mock_echo("x"), priced_spec("map_reduce"), trace = tr))
  pf <- Filter(function(s) identical(s$label, "preflight"), tr$steps)[[1]]$detail
  expect_gt(pf$est_cost_usd, 5)
  expect_lt(pf$est_input_usd, 5)
  expect_lt(tr$spent_usd, 5)
  expect_false(a$partial)
  expect_false(tr$budget_stop)
  expect_true(is.na(tr$stop_reason))
})

test_that("a run that cannot finish under the limit is refused before its first request", {
  model <- local_priced_model()
  local_clean_cache()
  ch <- test_chunks()
  input <- gr_estimate_cost(model, sum(ch$chunks$tokens), 0)
  gr_options(max_cost_usd = input / 2, max_calls = 1000)
  cl <- mock_echo()
  expect_error(quiet(gr_read(ch, "Q?", cl, priced_spec("map_reduce"))),
               class = "gr_cost_cap", regexp = "before any reply")
  # An ensemble with a member that sends every chunk sends every chunk.
  expect_error(quiet(gr_read(ch, "Q?", cl, priced_spec("ensemble",
                                                       members = c("retrieve", "map_reduce")))),
               class = "gr_cost_cap")
  expect_length(cl$calls(), 0L)

  # A reader that picks chunks is not refused on the size of the whole document:
  # it sends a few, and its spending is checked as it goes.
  a <- quiet(gr_read(ch, "Q?", cl, priced_spec("retrieve", top_k = 2)))
  expect_length(cl$calls(), 1L)
  expect_false(a$partial)
})

test_that("what a run has already spent counts against the limit", {
  model <- local_priced_model()
  local_clean_cache()
  ch <- test_chunks()
  input <- gr_estimate_cost(model, sum(ch$chunks$tokens), 0)
  gr_options(max_cost_usd = 1, max_calls = 1000)
  cl <- mock_echo()

  tr <- gr_trace()
  tr$spent_usd <- 1
  expect_error(quiet(gr_read(ch, "Q?", cl, priced_spec("retrieve"), trace = tr)),
               class = "gr_cost_cap", regexp = "already spent")

  # Under the limit, but not with this document's input on top.
  tr2 <- gr_trace()
  tr2$spent_usd <- 1 - input / 2
  expect_error(quiet(gr_read(ch, "Q?", cl, priced_spec("map_reduce"), trace = tr2)),
               class = "gr_cost_cap", regexp = "already spent")
  expect_length(cl$calls(), 0L)

  # And what one read spends, the next on the same trace inherits.
  tr3 <- gr_trace()
  quiet(gr_read(ch, "Q?", mock_echo("x"), priced_spec("stuff"), trace = tr3))
  expect_gt(tr3$spent_usd, 0)
  gr_options(max_cost_usd = tr3$spent_usd + input / 2)
  expect_error(quiet(gr_read(ch, "Q?", cl, priced_spec("map_reduce"), trace = tr3)),
               class = "gr_cost_cap")
})

test_that("a request answered from the cache spends nothing", {
  model <- local_priced_model()
  local_clean_cache()
  ch <- test_chunks()
  cached <- gr_cache_client(mock_echo("Some answer."), gr_cache(withr::local_tempdir()))
  gr_options(max_cost_usd = 100, max_calls = 1000)
  first <- gr_trace()
  quiet(gr_read(ch, "Q?", cached, priced_spec("map_reduce"), trace = first))
  expect_gt(first$spent_usd, 0.2)

  gr_options(max_cost_usd = 0.2)
  again <- gr_trace()
  a <- quiet(gr_read(ch, "Q?", cached, priced_spec("map_reduce"), trace = again))
  expect_identical(again$spent_usd, 0)
  expect_false(again$budget_stop)
  expect_false(a$partial)
})

test_that("each request is priced by its own model, and one with no price adds nothing", {
  local_registries()
  gr_register_model("priced", 100000, 1000, input_usd = 1, output_usd = 100)
  gr_register_model("unpriced", 100000, 1000)
  tr <- gr_trace()
  record <- function(model, cached = FALSE) {
    readgpt:::trace_record(tr, "x", list(list(role = "user", content = "hi")),
                           gr_result(TRUE, text = "a", usage = list(input = 1000L, output = 1000L),
                                     model = model, cached = cached))
  }
  record("priced")
  expect_equal(tr$spent_usd, 0.101)
  record("unpriced")
  expect_equal(tr$spent_usd, 0.101)
  # The full account says it does not know, which the running figure cannot.
  expect_true(is.na(sum(gr_trace_cost(tr)$usd)))
  record("priced", cached = TRUE)
  expect_equal(tr$spent_usd, 0.101)
})

test_that("a parent trace adds up what its children spent and why one stopped", {
  parent <- gr_trace()
  parent$spent_usd <- 0.25
  child <- gr_trace()
  child$spent_usd <- 0.5
  child$budget_stop <- TRUE
  child$stop_reason <- "cost"
  readgpt:::trace_absorb(parent, child)
  expect_equal(parent$spent_usd, 0.75)
  expect_true(parent$budget_stop)
  expect_identical(parent$stop_reason, "cost")
})

test_that("each limit stops a run for its own reason", {
  local_registries()
  gr_options(max_calls = 2, max_cost_usd = 1)
  tr <- gr_trace()
  expect_true(readgpt:::trace_can_call(tr))
  tr$calls <- 2L
  expect_false(readgpt:::trace_can_call(tr))
  expect_identical(tr$stop_reason, "calls")

  spent <- gr_trace()
  spent$spent_usd <- 0.99
  expect_true(readgpt:::trace_can_call(spent))
  spent$spent_usd <- 1
  expect_false(readgpt:::trace_can_call(spent))
  expect_identical(spent$stop_reason, "cost")
  # NULL and Inf both mean no limit.
  gr_options(max_cost_usd = NULL)
  expect_true(readgpt:::trace_can_call(spent))
  gr_options(max_cost_usd = Inf)
  expect_true(readgpt:::trace_can_call(spent))
})

test_that("the answer names the limit that stopped the run", {
  model <- local_priced_model()
  local_clean_cache()
  # Asks for more until a limit says no. Its signature does not claim every
  # chunk, so nothing is refused before the run.
  gr_register_reader("greedy", signature = "some|N|none", cost_calls = "N",
    fn = function(chunks, question, client, spec, trace) {
      n <- 0L
      while (readgpt:::trace_can_call(trace) && n < 50L) {
        gr_call(client, list(list(role = "user", content = "more")), model = spec$model,
                trace = trace, label = "greedy")
        n <- n + 1L
      }
      new_answer("done", "greedy", question, chunks$chunks$chunk_id, trace)
    })
  ch <- quiet(gr_segment("One short paragraph.", list(method = "paragraph")))

  gr_options(max_calls = 3, max_cost_usd = 1000)
  by_calls <- quiet(gr_read(ch, "Q?", mock_echo(), priced_spec("greedy")))
  expect_true(by_calls$partial)
  expect_equal(by_calls$notes$call_cap_reached, 3)
  expect_null(by_calls$notes[["cost_cap_reached", exact = TRUE]])

  gr_options(max_calls = 1000, max_cost_usd = 0.05)
  by_cost <- quiet(gr_read(ch, "Q?", mock_echo(), priced_spec("greedy")))
  expect_true(by_cost$partial)
  expect_equal(by_cost$notes$cost_cap_reached, 0.05)
  expect_null(by_cost$notes[["call_cap_reached", exact = TRUE]])
  expect_match(paste(utils::capture.output(print(by_cost)), collapse = "\n"),
               "stopped at the $0.05 spending limit", fixed = TRUE)
})

test_that("messages left by a stopped reader name the limit", {
  model <- local_priced_model()
  local_clean_cache()
  gr_options(max_cost_usd = 0.5, max_calls = 1000)
  ch <- test_chunks()
  it <- quiet(gr_read(ch, "Q?", mock_iterative_loop(refuse = 20L),
                      priced_spec("iterative", max_rounds = 10, top_k = 2)))
  expect_identical(it$notes$stop_reason, "spending limit")

  tr <- gr_trace()
  expect_identical(readgpt:::cap_name(tr), "call cap")
  tr$stop_reason <- "cost"
  tr$spent_usd <- 2
  expect_identical(readgpt:::cap_name(tr), "spending limit")
  expect_warning(readgpt:::warn_capped_batch(tr, "map_reduce", 10L, "reduce the chunk count"),
                 class = "gr_cost_cap", regexp = "spending limit")
  tr$stop_reason <- "calls"
  expect_warning(readgpt:::warn_capped_batch(tr, "map_reduce", 10L, "reduce the chunk count"),
                 class = "gr_call_cap", regexp = "run cap is")
})

test_that("a parallel batch is not sent once the run has reached a limit", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  local_registries()
  gr_options(max_cost_usd = 1)
  tr <- gr_trace()
  tr$spent_usd <- 2
  ran_here <- new.env(parent = emptyenv())
  ran_here$n <- 0L
  out <- readgpt:::gr_lapply(1:4, function(i, trace) {
    ran_here$n <- ran_here$n + 1L
    readgpt:::trace_can_call(trace)
  }, parallel = TRUE, workers = 2, trace = tr)
  # Run in this process against the run's own trace, where every item finds the
  # limit reached, rather than in workers that each start a count of their own.
  expect_identical(unlist(out), rep(FALSE, 4))
  expect_identical(ran_here$n, 4L)
  expect_identical(tr$stop_reason, "cost")
})

test_that("a parallel read is held to its worst case before it starts", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  model <- local_priced_model()
  local_clean_cache()
  ch <- test_chunks()
  gr_options(max_cost_usd = 5, max_calls = 1000, workers = 2)
  cl <- mock_echo("x")
  # Every reply at its cap is far above $5; the chunks and the real replies are
  # not, so the same read made in sequence runs.
  expect_error(quiet(gr_read(ch, "Q?", cl, priced_spec("map_reduce", parallel = TRUE))),
               class = "gr_cost_cap", regexp = "parallel")
  a <- quiet(gr_read(ch, "Q?", cl, priced_spec("map_reduce", parallel = FALSE)))
  expect_false(a$partial)

  # Priced at the dearest model the run uses: here the per-chunk one.
  gr_register_model("free-main", context_window = 200000, max_output = 8000,
                    input_usd = 0, output_usd = 0)
  expect_error(quiet(gr_read(ch, "Q?", cl, list(reader = "skim", model = "free-main",
                                                skim_model = model, parallel = TRUE))),
               class = "gr_cost_cap", regexp = "parallel")

  # A reader that makes one request sends no batch, so it is not held to it.
  one <- quiet(gr_read(ch, "Q?", cl, priced_spec("stuff", parallel = TRUE)))
  expect_false(one$partial)
})

test_that("an ensemble member that makes one request checks the limit too", {
  model <- local_priced_model()
  local_clean_cache()
  gr_options(max_cost_usd = 0.2, max_calls = 1000)
  ch <- test_chunks()
  for (second in c("stuff", "retrieve")) {
    tr <- gr_trace()
    a <- quiet(gr_read(ch, "Q?", mock_echo("Some answer."),
                       priced_spec("ensemble", members = c("map_reduce", second)), trace = tr))
    labels <- vapply(Filter(function(s) !identical(s$kind, "local"), tr$steps),
                     function(s) s$label, character(1))
    expect_false(paste0(second, ".answer") %in% labels)
    expect_true(a$partial)
  }
})

test_that("stuff and retrieve give no evidence for a request that failed", {
  local_clean_cache()
  ch <- quiet(gr_segment(sample_doc(2, 2), list(method = "paragraph", max_tokens = 200)))
  for (reader in c("stuff", "retrieve")) {
    a <- quiet(gr_read(ch, "Q?", mock_dead(), reader))
    expect_true(a$partial)
    expect_true(is.null(a$evidence) || !nrow(a$evidence))
    ok <- quiet(gr_read(ch, "Q?", mock_echo(), reader))
    expect_gt(nrow(ok$evidence), 0L)
  }
})

test_that("hierarchical answers from the summaries it has when a level produces none", {
  local_registries()
  local_clean_cache()
  gr_register_model("price-small", context_window = 3000, max_output = 1000,
                    input_usd = 1, output_usd = 10000)
  ch <- quiet(gr_segment(sample_doc(8, 8), list(method = "paragraph", max_tokens = 120)))
  spec <- list(reader = "hierarchical", model = "price-small", max_answer_tokens = 200,
               max_summary_tokens = 300, fan_in = 3)
  # Stopped by the limit part way through the first level, with too much left
  # to answer from without a second.
  gr_options(max_calls = 1000, max_cost_usd = 60)
  a <- quiet(gr_read(ch, "Q?", mock_bulky(), spec))
  expect_true(a$partial)
  expect_gt(a$notes$final_summaries, 0L)
  expect_true(nzchar(a$answer))
  expect_identical(a$notes$cost_cap_reached, 60)

  # Every request of the second level failing.
  gr_options(max_cost_usd = NULL)
  body <- paste(rep("Summary sentence about the cohort and the primary endpoint.", 12),
                collapse = " ")
  cl <- gr_mock_client(function(messages, params) {
    txt <- paste(vapply(messages, function(m) as.character(m$content), ""), collapse = " ")
    if (grepl("Summary sentence", txt, fixed = TRUE)) stop("simulated failure")
    body
  })
  b <- quiet(gr_read(ch, "Q?", cl, spec))
  expect_gt(b$notes$final_summaries, 0L)
})

test_that("a stop belongs to the read it happened in", {
  model <- local_priced_model()
  local_clean_cache()
  ch <- test_chunks()
  tr <- gr_trace()
  gr_options(max_cost_usd = 0.2, max_calls = 1000)
  first <- quiet(gr_read(ch, "Q?", mock_echo("Some answer."), priced_spec("map_reduce"), trace = tr))
  expect_true(first$partial)
  expect_identical(tr$stop_reason, "cost")
  # Raised, as the message says to, and read again on the same trace.
  gr_options(max_cost_usd = 100)
  second <- quiet(gr_read(ch, "Q?", mock_echo("Some answer."), priced_spec("stuff"), trace = tr))
  expect_false(second$partial)
  expect_null(second$notes[["cost_cap_reached", exact = TRUE]])
  # The trace still records that a read on it was stopped.
  expect_true(tr$budget_stop)
  expect_identical(tr$stop_reason, "cost")
})

claims_studies <- function() {
  data.frame(document = paste0(letters[1:4], ".pdf"), document_id = paste0("h", 1:4),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_, n = c(400L, 60L, 900L, 25L),
             design = c("randomised trial", "cross-sectional", "randomised trial", "qualitative"),
             finding = c("supports", "contradicts", "supports", "mixed"),
             stringsAsFactors = FALSE)
}

# Two claims per batch, resting on the studies that batch was shown. Two, so
# that one batch alone has claims a merge could be attempted on.
claims_by_batch <- function() {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return('{"sections":[{"heading":"A","brief":"b","claims":[1],"rationale":null}]}')
    }
    txt <- paste(vapply(messages, function(m) m$content, ""), collapse = "\n")
    ids <- paste(regmatches(txt, gregexpr("(?<=\\[study )[0-9]+", txt, perl = TRUE))[[1]],
                 collapse = ",")
    one <- paste0('{"claim":"%s studies %s.","kind":"finding","supported_by":[%s],',
                  '"contradicted_by":[],"moderator":null,"scope":null}')
    sprintf('{"claims":[%s,%s]}', sprintf(one, "From", ids, ids), sprintf(one, "Also from", ids, ids))
  })
}

test_that("the review stages say when a limit stopped them", {
  local_registries()
  gr_register_model("claims-small", context_window = 1000, max_output = 500,
                    input_usd = 1, output_usd = 10000)
  cl <- claims_by_batch()
  gr_options(max_cost_usd = 0.5)
  seen <- character(0)
  cm <- withCallingHandlers(
    suppressMessages(gr_claims(claims_studies(), question = "Does it work?", client = cl,
                               model = "claims-small", max_claim_tokens = 500)),
    gr_claims_capped = function(w) {
      seen <<- c(seen, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_identical(cm$trace$stop_reason, "cost")
  expect_true(any(grepl("were not sent: the run reached its spending limit", seen, fixed = TRUE)))
  # One batch's claims have nothing to be merged with.
  expect_false(any(grepl("were not merged", seen, fixed = TRUE)))

  # Both batches sent, and the limit reached before their claims are merged.
  gr_options(max_cost_usd = NULL)
  free_run <- suppressWarnings(suppressMessages(
    gr_claims(claims_studies(), question = "Does it work?", client = cl,
              model = "claims-small", max_claim_tokens = 500)))
  usd <- vapply(Filter(function(s) identical(s$label, "claims.draw"), free_run$trace$steps),
                function(s) gr_estimate_cost("claims-small", s$tokens$input, s$tokens$output),
                numeric(1))
  expect_length(usd, 2L)
  gr_options(max_cost_usd = usd[1] + usd[2] / 2)
  merged <- character(0)
  withCallingHandlers(
    suppressMessages(gr_claims(claims_studies(), question = "Does it work?", client = cl,
                               model = "claims-small", max_claim_tokens = 500)),
    gr_claims_capped = function(w) {
      merged <<- c(merged, conditionMessage(w))
      invokeRestart("muffleWarning")
    })
  expect_true(any(grepl("were not merged", merged, fixed = TRUE)))
  gr_options(max_cost_usd = 0.5)

  said <- NULL
  withCallingHandlers(
    suppressMessages(gr_outline(cm, client = cl, model = "claims-small")),
    gr_outline_failed = function(w) {
      said <<- conditionMessage(w)
      invokeRestart("muffleWarning")
    })
  expect_match(said, "not requested: the run had reached its spending limit", fixed = TRUE)
})

test_that("the up-front check prices chunks at the model that receives them", {
  local_registries()
  local_clean_cache()
  gr_register_model("pricey", context_window = 200000, max_output = 8000,
                    input_usd = 30, output_usd = 60)
  gr_register_model("cheap", context_window = 200000, max_output = 8000,
                    input_usd = 0.1, output_usd = 0.4)
  ch <- test_chunks()
  at_pricey <- gr_estimate_cost("pricey", sum(ch$chunks$tokens), 0)
  gr_options(max_cost_usd = at_pricey / 2, max_calls = 1000)
  cl <- mock_echo("The cohort comprised 482 participants.")
  # The main model's price would refuse all three; the per-chunk model's does not.
  expect_error(quiet(gr_read(ch, "Q?", cl, list(reader = "skim", model = "pricey"))),
               class = "gr_cost_cap")
  tr <- gr_trace()
  quiet(gr_read(ch, "Q?", cl, list(reader = "skim", model = "pricey", skim_model = "cheap"),
                trace = tr))
  expect_gt(tr$calls, 0L)
  tr2 <- gr_trace()
  quiet(gr_read(ch, "Q?", cl, list(reader = "hierarchical", model = "pricey",
                                   summary_model = "cheap"), trace = tr2))
  expect_gt(tr2$calls, 0L)
  # stuff sends what fits in one request, not the whole document.
  gr_register_model("pricey-small", context_window = 2000, max_output = 500,
                    input_usd = 30, output_usd = 60)
  room <- gr_budget("pricey-small", reserve_output = 1500)$input
  expect_lt(room, sum(ch$chunks$tokens))
  gr_options(max_cost_usd = gr_estimate_cost("pricey-small", room, 0) * 1.5)
  tr3 <- gr_trace()
  quiet(gr_read(ch, "Q?", cl, list(reader = "stuff", model = "pricey-small",
                                   on_overflow = "warn"), trace = tr3))
  expect_identical(tr3$calls, 1L)
})

test_that("every model without a price is named, and a free one runs under a $0 limit", {
  local_registries()
  local_clean_cache()
  gr_register_model("free", context_window = 200000, max_output = 8000,
                    input_usd = 0, output_usd = 0)
  gr_register_model("no-price", context_window = 200000, max_output = 8000)
  ch <- test_chunks()
  gr_options(max_cost_usd = 5, max_calls = 1000)
  expect_warning(suppressMessages(
    gr_read(ch, "Q?", mock_echo(), list(reader = "skim", model = "free", skim_model = "no-price"))),
    class = "gr_cost_uncheckable", regexp = "'no-price'")

  gr_options(max_cost_usd = 0)
  a <- quiet(gr_read(ch, "Q?", mock_echo(), list(reader = "map_reduce", model = "free")))
  expect_false(a$partial)
  cl <- mock_echo()
  for (reader in c("map_reduce", "retrieve")) {
    expect_error(quiet(gr_read(ch, "Q?", cl, list(reader = reader, model = "gpt-4o"))),
                 class = "gr_cost_cap")
  }
  expect_length(cl$calls(), 0L)
})

test_that("a refusal states a small amount as it is", {
  model <- local_priced_model()
  local_clean_cache()
  ch <- test_chunks()
  input <- gr_estimate_cost(model, sum(ch$chunks$tokens), 0)
  expect_lt(input, 0.01)
  gr_options(max_cost_usd = input / 2, max_calls = 1000)
  msg <- tryCatch(quiet(gr_read(ch, "Q?", mock_echo(), priced_spec("map_reduce"))),
                  gr_cost_cap = function(e) conditionMessage(e))
  expect_false(grepl("$0.00 ", msg, fixed = TRUE))
  expect_match(msg, sprintf("$%s", format(signif(input, 2), scientific = FALSE)), fixed = TRUE)
})

test_that("a document a limit stopped is not a result", {
  model <- local_priced_model()
  local_clean_cache()
  d <- list(sample_doc(4, 4))
  store <- withr::local_tempdir()
  gr_options(max_cost_usd = 0.05, max_calls = 1000)
  first <- quiet(gr_read_many(d, "Q?", "thorough", client = mock_echo(), store = store,
                              model = model))
  expect_identical(first$summary$status, "failed")
  expect_match(first$summary$error, "spending limit")
  # The partial answer is kept for inspection.
  expect_true(first$answers[[1]]$partial)
  # Not stored, so raising the limit and resuming reads it again.
  gr_options(max_cost_usd = 100)
  again <- quiet(gr_read_many(d, "Q?", "thorough", client = mock_echo(), store = store,
                              model = model))
  expect_identical(again$summary$status, "ok")
  expect_false(again$summary$partial)

  # An extraction table does not report a field in the unread part as absent.
  gr_options(max_cost_usd = 0.05)
  x <- quiet(gr_extract(d, fields = gr_fields(n = "sample size"), recipe = "thorough",
                        client = gr_mock_client(function(m, p) '{"n": null}'), model = model))
  expect_false(identical(x$table$status, "ok"))
})

test_that("a replay finds the choice made for its own document", {
  local_clean_cache()
  short <- sample_doc(2, 2)
  long <- long_doc()
  tr <- gr_trace()
  cl <- mock_echo()
  r1 <- quiet(answer_document(short, "Q?", client = cl, trace = tr))
  r2 <- quiet(answer_document(long, "Q?", client = cl, trace = tr))
  expect_identical(c(r1$recipe, r2$recipe), c("fast", "thorough"))
  f <- tempfile(fileext = ".json")
  gr_trace_save(tr, f)
  # Only the second run, replayed: it must not be handed the first run's choice.
  rp <- gr_replay_client(f, strict = TRUE)
  again <- quiet(answer_document(long, "Q?", client = rp))
  expect_identical(again$recipe, "thorough")
  expect_identical(rp$stats()$misses, 0L)
})

test_that("a reader whose answer request was not sent gives no evidence", {
  model <- local_priced_model()
  local_clean_cache()
  ch <- quiet(gr_segment(sample_doc(4, 4), list(method = "paragraph", max_tokens = 120)))
  gr_options(max_cost_usd = 0.1, max_calls = 1000)
  cases <- list(rerank = mock_echo(), iterative = mock_iterative_loop(),
                preview = mock_echo(), skim = mock_echo("The cohort comprised 482 participants."))
  for (reader in names(cases)) {
    a <- quiet(gr_read(ch, "Q?", cases[[reader]], priced_spec(reader)))
    labels <- vapply(a$trace$steps, function(s) as.character(s$label)[1], character(1))
    expect_false(any(grepl("[.](answer|final)$", labels)), info = reader)
    expect_true(is_not_found(a$answer), info = reader)
    expect_true(is.null(a$evidence) || !nrow(a$evidence), info = reader)
  }
})
