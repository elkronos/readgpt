# test-review-read.R -- regressions for the reading axis found in review:
# which model a read uses and is billed as, the limits a run is held to, and
# whether a quotation or a citation really checks out.

# A backend that records the model and output cap each request was sent with.
recording_backend <- function(model, reply = "Revenue was 45.2 million dollars.") {
  seen <- new.env(parent = emptyenv())
  seen$params <- list()
  cl <- gr_backend_client(function(messages, params) {
    seen$params[[length(seen$params) + 1L]] <- params[c("model", "max_output")]
    reply
  }, model = model)
  list(client = cl, models = function() vapply(seen$params, `[[`, "", "model"),
       max_output = function() vapply(seen$params, function(p) as.numeric(p$max_output), 0))
}

# Chunks as a segmenter that writes model text into `text` leaves them: the
# document text it worked from in `source_text`.
chunks_with_source <- function(text, source_text) {
  ch <- new_chunks(text, "precomputed", gr_segment_spec(max_tokens = 400))
  ch$chunks$source_text <- source_text
  ch
}

# A duck-typed ellmer Chat, the shape test-backend.R uses.
fake_chat <- function(model_id) {
  self <- new.env(parent = emptyenv())
  self$turns <- list()
  self$system <- NULL
  self$chat <- function(user, echo = "none") "Revenue was 45.2 million dollars."
  self$chat_structured <- function(user, type = NULL, echo = "none") list(score = 5)
  self$clone <- function(deep = FALSE) fake_chat(model_id)
  self$set_turns <- function(value) self$turns <- value
  self$set_system_prompt <- function(value) self$system <- value
  self$get_model <- function() model_id
  self
}

# ---------------------------------------------------------------------------
# client-01: the client's model is the one a read uses
# ---------------------------------------------------------------------------

test_that("a read without a model uses, budgets for and is billed as the client's model", {
  local_registries()
  local_clean_cache()
  gr_register_model("tiny-local", context_window = 4096, max_output = 512,
                    input_usd = 0, output_usd = 0)
  gr_register_model("house-a", context_window = 64000, max_output = 4000,
                    input_usd = 0.15, output_usd = 0.6)
  ch <- quiet(gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 200)))

  rb <- recording_backend("tiny-local")
  a <- quiet(gr_read(ch, "What was revenue?", rb$client, "stuff"))
  expect_identical(unique(rb$models()), "tiny-local")
  # Sized for the client's model: its 512-token output ceiling, not the
  # default model's.
  expect_lte(max(rb$max_output()), 512)

  rb <- recording_backend("house-a")
  tr <- gr_trace()
  a <- quiet(gr_read(ch, "What was revenue?", rb$client, "map_reduce", trace = tr))
  expect_identical(unique(rb$models()), "house-a")
  expect_identical(unique(gr_trace_cost(tr)$model), "house-a")

  # The same through answer_document() and gr_compare().
  rb <- recording_backend("house-a")
  a <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = rb$client))
  cmp <- quiet(gr_compare(readgpt_example(), "What was revenue?", c("fast", "thorough"),
                          client = rb$client))
  expect_identical(unique(rb$models()), "house-a")
})

test_that("a model named in the spec, the recipe or the overrides still wins over the client's", {
  local_registries()
  local_clean_cache()
  gr_register_model("tiny-local", context_window = 4096, max_output = 512,
                    input_usd = 0, output_usd = 0)
  gr_register_model("house-a", context_window = 64000, max_output = 4000,
                    input_usd = 0.15, output_usd = 0.6)
  ch <- quiet(gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 200)))
  rb <- recording_backend("house-a")
  a <- quiet(gr_read(ch, "What was revenue?", rb$client, list(reader = "stuff", model = "tiny-local")))
  b <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = rb$client,
                             model = "tiny-local"))
  rec <- gr_recipe("mine", read = list(reader = "stuff", model = "tiny-local"))
  d <- quiet(answer_document(readgpt_example(), "What was revenue?", rec, client = rb$client))
  expect_identical(unique(rb$models()), "tiny-local")
})

test_that("a spec keeps no model of its own until it is read", {
  expect_null(gr_read_spec("stuff")$model)
  expect_identical(gr_read_spec("stuff", model = "gpt-4o")$model, "gpt-4o")
  # A recipe's read line still prints when it names no model.
  out <- capture.output(print(gr_recipes("fast")))
  expect_true(any(grepl("read    : stuff", out, fixed = TRUE)))
  # A model that follows the client is not recorded as a setting.
  tr <- gr_trace()
  ch <- quiet(gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 200)))
  a <- quiet(gr_read(ch, "Q?", mock_echo(), "stuff", trace = tr))
  pf <- Filter(function(s) identical(s$label, "preflight"), tr$steps)[[1]]$detail
  expect_null(pf$settings$model)
})

# ---------------------------------------------------------------------------
# read-core-04: one unpriced model must not waive the parallel worst case
# ---------------------------------------------------------------------------

test_that("an unpriced model does not waive the parallel worst case for a priced one", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  local_registries()
  gr_register_model("pricey", context_window = 128000, max_output = 16000,
                    input_usd = 10, output_usd = 40)
  gr_register_model("house-model", context_window = 128000, max_output = 16000)
  gr_options(max_cost_usd = 0.5, max_calls = 10000)
  ch <- quiet(gr_segment(sample_doc(6, 6), list(method = "paragraph", max_tokens = 120)))
  cl <- mock_echo()
  spec <- function(main) gr_read_spec("rerank", model = main, skim_model = "pricey",
                                      parallel = TRUE, rerank_candidates = 200)
  expect_error(quiet(gr_read(ch, "Q?", cl, spec("pricey"))), class = "gr_cost_cap")
  # The answer model has no price; the batch of scoring calls still goes to
  # the priced one.
  expect_error(quiet(gr_read(ch, "Q?", cl, spec("house-model"))), class = "gr_cost_cap")
  expect_length(cl$calls(), 0L)
})

test_that("an ensemble's known floor is not waived by a member read by an unpriced model", {
  local_registries()
  gr_register_model("pricey", context_window = 128000, max_output = 16000,
                    input_usd = 10, output_usd = 40)
  gr_register_model("house-model", context_window = 128000, max_output = 16000)
  ch <- quiet(gr_segment(sample_doc(6, 6), list(method = "paragraph", max_tokens = 120)))
  floor_priced <- gr_estimate_cost("pricey", sum(ch$chunks$tokens), 0)
  gr_options(max_cost_usd = floor_priced / 2, max_calls = 10000)
  cl <- mock_echo()
  expect_error(quiet(gr_read(ch, "Q?", cl, gr_read_spec("ensemble", model = "pricey",
                                                          skim_model = "house-model",
                                                          members = c("map_reduce", "skim")))),
               class = "gr_cost_cap", regexp = "before any reply")
  expect_length(cl$calls(), 0L)
})

# ---------------------------------------------------------------------------
# money-04: an ellmer chat is priced as the model it bills
# ---------------------------------------------------------------------------

test_that("an ellmer chat's own unpriced model is named as uncheckable, override or not", {
  skip_if_not_installed("ellmer")
  local_registries()
  gr_register_model("claude-test", context_window = 200000, max_output = 8000)
  gr_options(max_cost_usd = 1)
  ch <- quiet(gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 200)))
  cl <- gr_ellmer_client(fake_chat("claude-test"))
  expect_identical(readgpt:::client_billed_model(cl), "claude-test")
  # The fake chat cannot take a per-call output cap either; that warning is
  # tested in test-backend.R and is not the one this test is about.
  expect_warning(suppressWarnings(suppressMessages(gr_read(ch, "Q?", cl, "stuff")),
                                  classes = "gr_ellmer_max_output"),
                 class = "gr_cost_uncheckable", regexp = "claude-test")
  # Naming a priced model does not change what the chat bills as.
  ws <- character(0)
  withCallingHandlers(suppressMessages(gr_read(ch, "Q?", cl, list(reader = "stuff",
                                                                 model = "gpt-5.6-terra"))),
                      warning = function(w) {
                        ws <<- c(ws, paste(class(w)[1], conditionMessage(w)))
                        invokeRestart("muffleWarning")
                      })
  expect_true(any(grepl("^gr_cost_uncheckable .*claude-test", ws)))
  # Other clients bill as the model each request names.
  expect_null(readgpt:::client_billed_model(mock_echo()))
  expect_null(readgpt:::client_billed_model(gr_backend_client(function(m, p) "x", model = "bk")))
})

# ---------------------------------------------------------------------------
# money-06: the worst case counts every level
# ---------------------------------------------------------------------------

test_that("the worst case counts every merge and summary level", {
  # 64 findings of 400 tokens into a merge room of 3,000: 7 per merge, so nine
  # merges and one finding passed on alone, then two merges, then the last.
  w <- readgpt:::merge_tree_worst(64, 400, 400, 3000)
  expect_identical(w$calls, 9 + 2 + 1)
  expect_null(readgpt:::merge_tree_worst(64, 400, 400, NA_real_))
  expect_identical(readgpt:::merge_tree_worst(1, 400, 400, 3000)$calls, 0)
  # 64 summaries halving until 400-token summaries fit 3,000 tokens, within 5
  # levels: 64 + 32 + 16 + 8 + 4, then the answer.
  h <- readgpt:::summary_levels_worst(64, 400, 3000, 2, 5)
  expect_identical(h$calls, 64 + 32 + 16 + 8 + 4 + 1)
  # max_levels still caps it.
  expect_identical(readgpt:::summary_levels_worst(64, 400, 3000, 2, 2)$calls, 64 + 32 + 1)
})

test_that("a hierarchical read's recorded worst case is not below what it makes", {
  local_registries()
  gr_register_model("small-ctx", 4000, 500, input_usd = 2, output_usd = 8)
  gr_options(max_calls = 100000, max_cost_usd = NULL)
  paras <- vapply(1:40, function(i) paste(sprintf("Paragraph %d.", i),
    paste(rep("The cohort comprised participants across clinical sites and endpoints.", 5),
          collapse = " ")), "")
  ch <- quiet(gr_segment(paste(paras, collapse = "\n\n"), list(method = "paragraph", max_tokens = 120)))
  # Replies just under the 400-token caps.
  cl <- mock_bulky(28)
  spec <- list(reader = "hierarchical", model = "small-ctx", fan_in = 2,
               max_answer_tokens = 400, max_summary_tokens = 400, max_chunk_tokens = 400)
  tr <- gr_trace()
  a <- quiet(gr_read(ch, "Q?", cl, spec, trace = tr))
  pf <- Filter(function(s) identical(s$label, "preflight"), tr$steps)[[1]]$detail
  expect_gte(pf$worst_calls, tr$calls)
  expect_gt(pf$worst_calls, pf$est_calls)
  expect_gte(pf$est_cost_usd, tr$spent_usd)

  # skim: consolidating the evidence is part of its worst case too.
  tr <- gr_trace()
  s <- quiet(gr_read(ch, "Q?", cl, list(reader = "skim", model = "small-ctx",
                                        max_answer_tokens = 400, max_chunk_tokens = 400),
                     trace = tr))
  pf <- Filter(function(s) identical(s$label, "preflight"), tr$steps)[[1]]$detail
  expect_gte(pf$worst_calls, tr$calls)
  expect_gte(pf$est_cost_usd, tr$spent_usd)
})

test_that("a parallel read is refused when its worst case passes a limit", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  local_registries()
  gr_register_model("small-ctx", 4000, 500, input_usd = 2, output_usd = 8)
  paras <- vapply(1:64, function(i) paste(sprintf("Paragraph %d.", i),
    paste(rep("The cohort comprised participants across clinical sites and endpoints.", 5),
          collapse = " ")), "")
  ch <- quiet(gr_segment(paste(paras, collapse = "\n\n"), list(method = "paragraph", max_tokens = 120)))
  n <- nrow(ch$chunks)
  spec <- list(reader = "hierarchical", model = "small-ctx", fan_in = 2,
               max_answer_tokens = 400, max_summary_tokens = 400, parallel = TRUE)
  cl <- mock_bulky(28)
  # Room for the old estimate (n + n/2 + 1) but not for every level.
  gr_options(max_calls = n + ceiling(n / 2) + 3, max_cost_usd = NULL)
  expect_error(quiet(gr_read(ch, "Q?", cl, spec)), class = "gr_call_cap", regexp = "worst case")
  expect_length(cl$calls(), 0L)
  # Sequential, the same cap stops the run part way instead.
  a <- quiet(gr_read(ch, "Q?", cl, utils::modifyList(spec, list(parallel = FALSE))))
  expect_true(a$partial)
})

# ---------------------------------------------------------------------------
# money-02: a comparison is one run for the spending limit
# ---------------------------------------------------------------------------

test_that("gr_compare holds all its recipes to one spending limit", {
  local_registries()
  local_clean_cache()
  gr_register_model("price-test", context_window = 200000, max_output = 8000,
                    input_usd = 1, output_usd = 10000)
  txt <- sample_doc(6, 6)
  q <- "What was the cohort size?"
  cl <- mock_echo("The cohort comprised 482 participants recruited across nine sites.")
  seg <- list(method = "paragraph", max_tokens = 120)
  mk <- function(reader) gr_recipe(name = reader, segment = seg,
                                   read = gr_read_spec(reader = reader, model = "price-test"))
  gr_options(max_calls = 1000, max_cost_usd = NULL)
  one <- quiet(gr_compare(txt, q, list(mk("map_reduce")), client = cl))
  lim <- one$trace$spent_usd * 1.2
  gr_options(max_cost_usd = lim)
  cmp <- quiet(gr_compare(txt, q, list(mk("map_reduce"), mk("refine"), mk("skim"),
                                       mk("hierarchical")), client = cl))
  # Past the limit by at most the request that reached it, not by three more
  # recipes' worth.
  expect_lt(cmp$trace$spent_usd, lim * 1.2)
  expect_true(cmp$trace$budget_stop)
  expect_true(all(cmp$summary$partial[-1]))
  # The recipes refused outright say why.
  expect_true(any(grepl("already spent", cmp$summary$error)))
  # The shared trace still adds up to its steps.
  expect_equal(cmp$trace$spent_usd, sum(gr_trace_cost(cmp$trace)$usd))
})

# ---------------------------------------------------------------------------
# read-core-01: a quotation matches whole words and whole numbers
# ---------------------------------------------------------------------------

test_that("a quotation with a changed figure does not verify", {
  src <- paste("Overall, 25% of patients had nausea. Change in revenue: -12% year on year.",
               "The trial enrolled 200 patients. Revenue rose to 45.2 million dollars.",
               "The dose was 0.5 mg twice daily. In all, 15 patients died.",
               "Headcount grew to 1,204. COVID-19 patients were excluded.")
  bad <- c("5% of patients had nausea.", "12% year on year", "The trial enrolled 20.",
           "Revenue rose to 45.", "5 mg twice daily.", "5 patients died.",
           "Headcount grew to 1,20", "grew to 204", "19 patients were excluded")
  for (s in bad) {
    m <- readgpt:::span_match(s, src)
    expect_false(m$verified, label = s)
    expect_lt(m$match, 1, label = s)
  }
  good <- c("25% of patients had nausea.", "-12% year on year", "The trial enrolled 200 patients.",
            "Revenue rose to 45.2 million dollars", "0.5 mg twice daily", "headcount grew to 1,204",
            "\u201cOverall, 25% of patients had nausea\u2026\u201d", "enrolled 200", "In all, 15")
  for (s in good) expect_true(readgpt:::span_match(s, src)$verified, label = s)
})

test_that("a skim quotation that drops a digit makes the answer partial", {
  doc <- "Overall, 25% of patients had nausea during the first week of treatment."
  ch <- quiet(gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 200)))
  cl <- gr_mock_client(function(m, p) {
    if (grepl("You extract evidence", m[[1]]$content, fixed = TRUE))
      return("5% of patients had nausea during the first week of treatment.")
    "5% of patients had nausea."
  })
  a <- quiet(gr_read(ch, "How common was nausea?", cl, "skim"))
  expect_true(a$partial)
  expect_false(gr_verify_evidence(a)$verified)
})

# ---------------------------------------------------------------------------
# read-core-05: a quotation of several passages is checked passage by passage
# ---------------------------------------------------------------------------

test_that("several verbatim passages, listed or elided, verify", {
  src <- paste("Overall, 25% of patients had nausea during the trial.",
               "The study was funded by the national research council.",
               "Headache was reported by 12 patients.")
  good <- c(
    "Overall, 25% of patients had nausea during the trial.\n\nHeadache was reported by 12 patients.",
    "- Overall, 25% of patients had nausea during the trial.\n- Headache was reported by 12 patients.",
    "* Headache was reported by 12 patients.",
    "\u2022 Headache was reported by 12 patients.",
    "1. Overall, 25% of patients had nausea during the trial.\n2. Headache was reported by 12 patients.",
    "**Headache was reported by 12 patients.**",
    "Overall, 25% of patients had nausea ... Headache was reported by 12 patients.",
    "Overall, 25% of patients had nausea [...] reported by 12 patients.",
    "\"Overall, 25% of patients had nausea during the trial.\" \"Headache was reported by 12 patients.\"")
  for (s in good) {
    m <- readgpt:::span_match(s, src)
    expect_true(m$verified, label = s)
    expect_identical(m$match, 1, label = s)
  }
  # Every passage has to be there: one invented line fails the span.
  bad <- c("Overall, 25% of patients had nausea during the trial.\nMortality fell by 40%.",
           "- Headache was reported by 21 patients.",
           "Overall, 5% of patients ... reported by 12 patients.")
  for (s in bad) expect_false(readgpt:::span_match(s, src)$verified, label = s)
  # Nothing to check is still NA.
  expect_true(is.na(readgpt:::span_match("...", src)$verified))
})

test_that("a faithful multi-passage extraction does not make a skim answer partial", {
  src <- paste("Overall, 25% of patients had nausea during the trial.",
               "The study was funded by the national research council.",
               "Headache was reported by 12 patients.")
  ch <- quiet(gr_segment(gr_ingest(src), list(method = "paragraph", max_tokens = 200)))
  cl <- gr_mock_client(function(m, p) {
    if (grepl("You extract evidence", m[[1]]$content, fixed = TRUE))
      return(paste0("Overall, 25% of patients had nausea during the trial.\n\n",
                    "Headache was reported by 12 patients."))
    "25% had nausea and 12 had headache."
  })
  a <- quiet(gr_read(ch, "Side effects?", cl, "skim"))
  expect_false(a$partial)
  expect_true(gr_verify_evidence(a)$verified)
})

# ---------------------------------------------------------------------------
# model-output-02: citation lists, ranges and unreadable markers
# ---------------------------------------------------------------------------

test_that("every common way of writing a citation is read", {
  ids <- function(x, w = "study") sort(readgpt:::cited_ids(x, w))
  expect_identical(ids("[studies 1, 2, and 7]"), c(1L, 2L, 7L))
  expect_identical(ids("[studies 1-4]"), 1:4)
  expect_identical(ids("[studies 1\u20134]"), 1:4)
  expect_identical(ids("[study 1; study 7]"), c(1L, 7L))
  expect_identical(ids("[study 1, study 7]"), c(1L, 7L))
  expect_identical(ids("[studies 1; 7]"), c(1L, 7L))
  expect_identical(ids("[study\u00a07]"), 7L)
  expect_identical(ids("[studies 1 and 7]"), c(1L, 7L))
  expect_identical(ids("[study 3] [study 4]"), c(3L, 4L))
  # A locator is not an id: the page and section render_chunks() prints.
  expect_identical(ids("[chunk 3 p.12, \u00a7 Study population]", "chunk"), 3L)
  expect_identical(ids("[study 3, p. 4]"), 3L)
  expect_length(readgpt:::unparsed_citations("[chunk 3 p.12, \u00a7 Study population]", "chunk"), 0L)
  # ...but it cannot hide another id.
  expect_identical(readgpt:::unparsed_citations("[chunk 1 p.7, chunk 9]", "chunk"),
                   "[chunk 1 p.7, chunk 9]")
  expect_identical(readgpt:::unparsed_citations("see [chunk nine] and [chunk 2]", "chunk"),
                   "[chunk nine]")
  # A range too long to be a citation is kept as its two ends.
  expect_identical(ids("[chunks 1-1000000]", "chunk"), c(1L, 1000000L))
  # The listed form, which the renderer reads every number of, takes the lists
  # but never a range or a locator: those it leaves as written.
  listed <- readgpt:::cite_pattern("study")
  expect_true(grepl(listed, "[studies 1, 2, and 7]", perl = TRUE))
  expect_true(grepl(listed, "[study 1; study 7]", perl = TRUE))
  expect_false(grepl(listed, "[studies 1-7]", perl = TRUE))
  expect_false(grepl(listed, "[study 3, p. 4]", perl = TRUE))
})

test_that("a synthesised section citing a missing study in a list is not clean", {
  local_clean_cache()
  make_client <- function(section_text) {
    gr_mock_client(function(messages, params) {
      seen <- paste(vapply(messages, function(m) paste(as.character(m$content), collapse = ""),
                           character(1)), collapse = "\n")
      if (grepl("<studies>", seen, fixed = TRUE)) return(section_text)
      if (grepl("randomised trial", seen, fixed = TRUE))
        return(paste0('{"design":"randomised trial","n":120,',
                      '"design__quote":"We ran a randomised trial.","n__quote":"We enrolled 120 people."}'))
      if (grepl("cohort study", seen, fixed = TRUE))
        return(paste0('{"design":"cohort study","n":900,',
                      '"design__quote":"We ran a cohort study.","n__quote":"We followed 900 people."}'))
      '{"design":null,"n":null,"design__quote":null,"n__quote":null}'
    })
  }
  d <- withr::local_tempdir()
  f <- function(name, txt) { p <- file.path(d, name); writeLines(txt, p); p }
  srcs <- c(f("a.txt", "We ran a randomised trial. We enrolled 120 people."),
            f("b.txt", "We ran a cohort study. We followed 900 people."))
  x <- quiet(gr_extract(srcs, gr_fields(design = "The study design",
                                         n = gr_field("Participants", type = "integer")),
                        client = make_client(""), recipe = "thorough", max_tokens = 40))
  for (t in c("Three trials found a benefit [studies 1, 2, and 7].",
              "Three trials found a benefit [studies 1-7].",
              "Three trials found a benefit [study 1; study 7].")) {
    s <- quiet(gr_synthesise(x, question = "Does it work?", outline = c(Findings = "What they found"),
                             client = make_client(t), references = FALSE))
    expect_gte(s$sections$n_unknown, 1L, label = t)
    expect_true(s$sections$partial, label = t)
  }
})

test_that("a fabricated id in a list or range makes the answer partial", {
  ch <- new_chunks(c("Revenue rose to 45.2 million dollars.", "Headcount grew to 1,204."),
                   "precomputed", gr_segment_spec(max_tokens = 400))
  read_with <- function(txt) quiet(gr_read(ch, "Q?", gr_mock_client(function(m, p) txt),
                                           list(reader = "stuff", cite = TRUE)))
  a <- read_with("Revenue rose [chunks 1, 2, and 9].")
  expect_true(a$partial)
  expect_identical(a$notes$cited_unknown, 9L)
  b <- read_with("Revenue rose [chunks 1-4].")
  expect_identical(sort(b$notes$cited_unknown), 3:4)
  c2 <- read_with("Revenue rose [chunk nine].")
  expect_true(c2$partial)
  expect_identical(c2$notes$cited_unparsed, "[chunk nine]")
  expect_true(any(grepl("could not be checked", readgpt:::partial_reasons(c2))))
  # Faithful lists and ranges are clean.
  expect_false(read_with("Revenue rose [chunks 1-2] and [chunk 1 p.3].")$partial)
})

# ---------------------------------------------------------------------------
# read-core-02, segment-03: quotations are checked against the document text
# ---------------------------------------------------------------------------

test_that("a quotation of a model-written context header does not verify", {
  body <- "Annual report for the year.\n\nRevenue for the year was 45.2 million dollars."
  hdr <- "[This excerpt reports that revenue rose to 52 million dollars.]\n\n"
  ch <- chunks_with_source(paste0(hdr, body), body)
  liar <- gr_mock_client(function(m, p) {
    if (grepl("You extract evidence", m[[1]]$content, fixed = TRUE))
      return("revenue rose to 52 million dollars.")
    "Revenue was 52 million dollars."
  })
  a <- quiet(gr_read(ch, "What was revenue?", liar, "skim"))
  expect_true(a$partial)
  expect_false(a$evidence$verified)
  expect_false(gr_verify_evidence(a, ch)$verified)
  # Checked against the chunks too, when the answer carries no source of its own.
  a2 <- a
  a2$evidence$source_text <- NA_character_
  expect_false(gr_verify_evidence(a2, ch)$verified)

  honest <- gr_mock_client(function(m, p) {
    if (grepl("You extract evidence", m[[1]]$content, fixed = TRUE))
      return("Revenue for the year was 45.2 million dollars.")
    "Revenue was 45.2 million dollars."
  })
  h <- quiet(gr_read(ch, "What was revenue?", honest, "skim"))
  expect_false(h$partial)

  # Verbatim evidence is the document's text, not the header.
  s <- quiet(gr_read(ch, "What was revenue?", honest, "stuff"))
  expect_identical(s$evidence$text, body)
  expect_true(gr_verify_evidence(s, ch)$verified)
})

test_that("a proposition the segmenting model invented does not verify", {
  props <- "The trial enrolled 120 patients.\nMortality fell by 40% in the treatment arm."
  ch <- chunks_with_source(props, "The trial enrolled 120 patients and outcomes improved.")
  cl <- gr_mock_client(function(m, p) {
    if (grepl("You extract evidence", m[[1]]$content, fixed = TRUE))
      return("Mortality fell by 40% in the treatment arm.")
    "Mortality fell by 40% [chunk 1]."
  })
  a <- quiet(gr_read(ch, "What happened to mortality?", cl, "skim"))
  expect_true(a$partial)
  expect_identical(a$notes$unverified_evidence, 1L)
  # A chunk with no source_text, or an NA one, is its own source.
  plain <- new_chunks(props, "precomputed", gr_segment_spec(max_tokens = 400))
  expect_true(quiet(gr_read(plain, "Q?", cl, "skim"))$evidence$verified)
  na_src <- chunks_with_source(props, NA_character_)
  expect_true(quiet(gr_read(na_src, "Q?", cl, "skim"))$evidence$verified)
  # preview's skimmed sections follow the same rule.
  skimmer <- gr_mock_client(function(m, p) {
    sys <- m[[1]]$content
    if (grepl("You plan how to read a document", sys, fixed = TRUE)) return(preview_plan_json("skim"))
    if (grepl("extract evidence, not answers", sys, fixed = TRUE))
      return("Mortality fell by 40% in the treatment arm.")
    "Mortality fell by 40%."
  })
  ev <- function(chunks) {
    pv <- quiet(gr_read(chunks, "Q?", skimmer, "preview"))
    pv$evidence$verified[pv$evidence$kind == "extracted"]
  }
  expect_false(ev(ch))
  expect_true(ev(plain))
})
