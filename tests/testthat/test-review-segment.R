# test-review-segment.R -- segmentation and parallel batches, after review.
#
# Each block names the defect it guards against. The parallel ones need the
# future packages and start worker processes, so they are skipped without them.

skip_if_no_future <- function() {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
}

# A mock client at a price, so a run can reach a spending limit. The mock
# registers "mock-model" at no cost when it is first made, so the price is set
# after it.
priced_mock <- function(handler, env = parent.frame()) {
  local_registries(env)
  cl <- gr_mock_client(handler)
  gr_register_model("mock-model", context_window = 128000, max_output = 16384,
                    input_usd = 4, output_usd = 20)
  cl
}

numbered_paragraphs <- function(n) {
  paste(sprintf("Paragraph %d states that the trial enrolled %d patients and measured outcome %d.",
                seq_len(n), 100 + seq_len(n), seq_len(n)), collapse = "\n\n")
}

bulky_decompose <- function(m, p) {
  if (grepl("Decompose", m[[1]]$content, fixed = TRUE)) {
    sprintf('{"propositions":["%s"]}',
            paste(rep("A fact about the trial that is stated verbatim.", 40), collapse = " "))
  } else {
    paste(rep("This excerpt describes trial enrolment in the methods section.", 5), collapse = " ")
  }
}

# ---------------------------------------------------------------------------
# money-01 / money-06: a parallel batch is held to the run's limits.
# ---------------------------------------------------------------------------

test_that("a parallel LLM segmentation stops at the limits where the sequential one does", {
  skip_if_no_future()
  cl <- priced_mock(bulky_decompose)
  doc <- quiet(gr_ingest(numbered_paragraphs(40)))
  seg <- function(method, par) {
    tr <- gr_trace()
    spec <- if (method == "proposition") {
      gr_segment_spec("proposition", max_tokens = 400, proposition_batch_tokens = 200, parallel = par)
    } else {
      gr_segment_spec("contextual", context_source = "llm", max_tokens = 200, parallel = par)
    }
    ch <- quiet(gr_segment(doc, spec, client = cl, trace = tr))
    list(calls = tr$calls, stop = tr$budget_stop, reason = tr$stop_reason, n = nrow(ch$chunks))
  }
  gr_options(workers = 2)
  for (method in c("proposition", "contextual")) {
    # The spending limit. Parallel used to send every batch: ten calls and
    # five times the limit, with budget_stop FALSE.
    gr_options(max_calls = 1000, max_cost_usd = 0.02)
    s <- seg(method, FALSE); p <- seg(method, TRUE)
    expect_identical(p, s, info = method)
    expect_true(p$stop, info = method)
    expect_identical(p$reason, "cost", info = method)
    # The call cap.
    gr_options(max_calls = 3, max_cost_usd = NULL)
    s <- seg(method, FALSE); p <- seg(method, TRUE)
    expect_identical(p, s, info = method)
    expect_identical(p$calls, 3L, info = method)
    expect_identical(p$reason, "calls", info = method)
  }
})

test_that("a batch that could pass the call cap runs where each item is checked", {
  skip_if_no_future()
  local_registries()
  gr_options(max_calls = 10, max_cost_usd = NULL)
  tr <- gr_trace()
  tr$calls <- 8L
  out <- readgpt:::gr_lapply(1:4, function(i, trace) {
    if (!readgpt:::trace_can_call(trace)) return(NA_integer_)
    trace$calls <- trace$calls + 1L
    Sys.getpid()
  }, parallel = TRUE, workers = 2, trace = tr)
  # Room for one call was enough to send all four: 12 calls under a cap of 10.
  expect_identical(tr$calls, 10L)
  expect_identical(unlist(out), c(Sys.getpid(), Sys.getpid(), NA_integer_, NA_integer_))
  expect_true(tr$budget_stop)
  expect_identical(tr$stop_reason, "calls")
})

test_that("a batch that could pass the spending limit at its worst runs here", {
  skip_if_no_future()
  local_registries()
  gr_options(max_calls = 100, max_cost_usd = 0.05)
  spend <- function(each) function(i, trace) {
    if (!readgpt:::trace_can_call(trace)) return(NA_integer_)
    trace$calls <- trace$calls + 1L
    trace$spent_usd <- trace$spent_usd + each
    Sys.getpid()
  }
  tr <- gr_trace()
  out <- readgpt:::gr_lapply(1:4, spend(0.02), parallel = TRUE, workers = 2, trace = tr,
                             item_usd = 0.02)
  expect_identical(unlist(out), c(rep(Sys.getpid(), 3), NA_integer_))
  expect_true(tr$budget_stop)
  expect_identical(tr$stop_reason, "cost")

  # Running here is not itself a stop: items that turn out cheap all run, and
  # the run does not claim a limit stopped it.
  tr2 <- gr_trace()
  out2 <- readgpt:::gr_lapply(1:4, spend(0.001), parallel = TRUE, workers = 2, trace = tr2,
                              item_usd = 0.02)
  expect_false(anyNA(unlist(out2)))
  expect_false(tr2$budget_stop)
})

test_that("a batch that fits every limit still goes to workers", {
  skip_if_no_future()
  local_registries()
  gr_options(max_calls = 100, max_cost_usd = 1)
  tr <- gr_trace()
  pids <- unlist(readgpt:::gr_lapply(1:4, function(i, trace) Sys.getpid(),
                                     parallel = TRUE, workers = 2, trace = tr, item_usd = 0.01))
  expect_true(all(pids != Sys.getpid()))
  expect_false(tr$budget_stop)
})

test_that("a batch that passed a limit anyway is recorded as having done so", {
  skip_if_no_future()
  local_registries()
  gr_options(max_calls = 100, max_cost_usd = 0.05)
  tr <- gr_trace()
  # No item price: the caller vouched for the batch, and was wrong.
  readgpt:::gr_lapply(1:4, function(i, trace) {
    trace$calls <- trace$calls + 1L
    trace$spent_usd <- trace$spent_usd + 0.02
    i
  }, parallel = TRUE, workers = 2, trace = tr)
  expect_equal(tr$spent_usd, 0.08)
  expect_true(tr$budget_stop)
  expect_identical(tr$stop_reason, "cost")
})

test_that("a parallel hierarchical read stays under max_calls at every level", {
  # preflight() counted two levels of a hierarchical read; each later level is a
  # batch of its own, and went out whole with any headroom left at all. Two
  # things now hold it: preflight() counts every level, and gr_lapply() checks
  # room for a whole batch before it sends one.
  skip_if_no_future()
  local_registries()
  gr_options(workers = 2)
  gr_register_model("small-ctx", context_window = 4000, max_output = 500,
                    input_usd = 2, output_usd = 8)
  cl <- gr_mock_client(function(m, p)
    paste(rep("The section reports regional revenue and costs in detail.", 40), collapse = " "))
  txt <- paste(sprintf("Section %d. Revenue in region %d was %d million dollars, and costs rose.",
                       1:16, 1:16, 1:16), collapse = "\n\n")
  ch <- quiet(gr_segment(gr_ingest(txt), list(method = "paragraph", max_tokens = 32)))
  n <- nrow(ch$chunks)
  read_with <- function(par, tr) {
    gr_read(ch, "What was revenue?", cl,
            list(reader = "hierarchical", model = "small-ctx", fan_in = 2,
                 max_answer_tokens = 400, max_summary_tokens = 400, parallel = par),
            trace = tr)
  }
  # The old two-level estimate: enough for the first two levels, not the rest.
  gr_options(max_calls = n + ceiling(n / 2) + 3)
  tr <- gr_trace()
  quiet(read_with(FALSE, tr))
  expect_lte(tr$calls, as.integer(gr_options("max_calls")))
  # In parallel the worst case is over the cap, so nothing is sent at all.
  tr <- gr_trace()
  expect_error(quiet(read_with(TRUE, tr)), class = "gr_call_cap")
  expect_identical(tr$calls, 0L)

  # With room for every level, parallel and sequential make the same calls.
  gr_options(max_calls = 4L * n)
  calls <- vapply(c(FALSE, TRUE), function(par) {
    tr <- gr_trace()
    quiet(read_with(par, tr))
    tr$calls
  }, integer(1))
  expect_identical(calls[2], calls[1])
  expect_lte(calls[2], as.integer(gr_options("max_calls")))
})

# ---------------------------------------------------------------------------
# money-03: gr_segment() without a trace still has limits, and says what it did.
# ---------------------------------------------------------------------------

test_that("an LLM segmenter run without a trace is held to the run's limits", {
  cl <- priced_mock(function(m, p) '{"propositions":["A is B.","C is D."]}')
  gr_options(max_calls = 2, max_cost_usd = NULL)
  doc <- quiet(gr_ingest(numbered_paragraphs(40)))
  ch <- quiet(gr_segment(doc, list(method = "proposition", max_tokens = 400,
                                   proposition_batch_tokens = 200), client = cl))
  # Nine calls were made under a cap of two, and none was recorded.
  expect_identical(length(cl$calls()), 2L)
  expect_s3_class(ch$trace, "gr_trace")
  expect_identical(ch$trace$calls, 2L)
  expect_true(ch$trace$budget_stop)
  # What was not decomposed is kept, not lost.
  expect_gt(ch$extra$batches_kept_as_written, 0L)

  # The spending limit too.
  gr_options(max_calls = 1000, max_cost_usd = 0.001)
  cl$reset()
  ch <- quiet(gr_segment(doc, list(method = "proposition", max_tokens = 400,
                                   proposition_batch_tokens = 200), client = cl))
  expect_identical(length(cl$calls()), 1L)
  expect_identical(ch$trace$stop_reason, "cost")

  # A trace the caller passed is the one used, and is not copied onto the chunks.
  tr <- gr_trace()
  ch <- quiet(gr_segment(doc, list(method = "paragraph"), trace = tr))
  expect_null(ch$trace)
  expect_true(any(vapply(tr$steps, function(s) identical(s$label, "segment"), logical(1))))
})

test_that("the Shiny chunking preview applies the cost cap and does not claim to be free", {
  skip_if_not_installed("shiny")
  app <- system.file("shiny", "app.R", package = "readgpt")
  skip_if(!nzchar(app), "the Shiny app is not available")
  dir <- withr::local_tempdir()
  writeLines(numbered_paragraphs(60), file.path(dir, "long.txt"))
  withr::local_envvar(GPTREAD_DOC_ROOTS = normalizePath(dir))
  cl <- priced_mock(function(m, p) '{"propositions":["A is B.","C is D."]}')
  gr_options(max_calls = 400, verbose = FALSE)
  cap_before <- gr_options("max_cost_usd")
  env <- new.env(parent = globalenv())
  # The app's own client needs a key and the network; the mock stands in.
  env$gr_client <- function(model = NULL, api_key = NULL, ...) cl
  env$shinyApp <- function(ui, server, ...) server
  # The app attaches shiny; leave the search path as it was found.
  if (!"package:shiny" %in% search()) {
    withr::defer(try(detach("package:shiny", character.only = TRUE), silent = TRUE))
  }
  quiet(sys.source(app, envir = env, keep.source = FALSE))
  shiny::testServer(env$server, {
    session$setInputs(api_key = "not-a-real-key", model = "mock-model",
                      file = normalizePath(file.path(dir, "long.txt")),
                      clean_preset = "standard", ocr = "auto",
                      segmenter = "proposition", max_tokens = 400, overlap = 0,
                      min_tokens = 0, parallel = FALSE, max_cost = 0.001)
    expect_match(output$chunk_help, "makes model calls")
    # The cap stops the run after one request, and what was not decomposed is
    # kept as written, which the segmenter says.
    expect_warning(session$setInputs(preview = 1), "kept as written")
    # It sent one paid request per batch of the document, the cost field
    # ignored, under a button that said it was free.
    expect_identical(length(cl$calls()), 1L)
    session$setInputs(segmenter = "paragraph")
    expect_match(output$chunk_help, "No model calls are made")
  })
  # The field applied to the preview only.
  expect_identical(gr_options("max_cost_usd"), cap_before)
})

# ---------------------------------------------------------------------------
# segment-01 / segment-02: structural keeps document order and every heading.
# ---------------------------------------------------------------------------

test_that("structural never gathers same-named sections into one chunk", {
  md <- withr::local_tempfile(fileext = ".md")
  writeLines(c("# Study 1", "", "## Methods", "", "We recruited 40 adults in Ohio.", "",
               "## Results", "", "Study 1 found a 12% gain.", "",
               "# Study 2", "", "## Methods", "", "We recruited 300 children in Kenya.", "",
               "## Results", "", "Study 2 found no effect."), md)
  ch <- quiet(gr_segment(quiet(gr_ingest(md)), "structural"))
  txt <- ch$chunks$text
  # Both studies' methods were one chunk, placed before the second study.
  expect_false(any(grepl("Ohio", txt) & grepl("Kenya", txt)))
  expect_false(any(grepl("12% gain", txt) & grepl("no effect", txt)))
  where <- function(p) which(grepl(p, txt, fixed = TRUE))
  expect_true(where("Ohio") < where("# Study 2"))
  expect_true(where("# Study 2") < where("Kenya"))
  expect_identical(ch$chunks$section,
                   c("Study 1", "Methods", "Results", "Study 2", "Methods", "Results"))
  expect_false(anyNA(ch$chunks$block_id))

  # A preamble and a trailing endnote have no section; they are not one chunk.
  d <- quiet(gr_ingest(md))
  d$blocks <- data.frame(block_id = 1:6, page = NA_integer_,
                         section = c(NA, "Introduction", "Introduction", "Outlook", "Outlook", NA),
                         text = c("Quarterly Report", "## Introduction", "Sales rose in Q3.",
                                  "## Outlook", "We expect growth.",
                                  "[Endnote 1] Figures are unaudited."),
                         stringsAsFactors = FALSE)
  d$text <- paste(d$blocks$text, collapse = "\n\n")
  ch2 <- quiet(gr_segment(d, "structural"))
  expect_identical(ch2$chunks$text[1], "Quarterly Report")
  expect_identical(ch2$chunks$text[nrow(ch2$chunks)], "[Endnote 1] Figures are unaudited.")
  expect_identical(ch2$chunks$block_id, c(1L, 3L, 5L, 6L))
})

test_that("a heading found inline keeps its leading number", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("Five-year review.", "", "2022 Results", "",
               "Revenue was 3.1 million and the plant ran at 70% capacity.", "",
               "2023 Results", "",
               "Revenue was 4.8 million and the plant ran at 95% capacity."), f)
  ch <- quiet(gr_segment(quiet(gr_ingest(f)), "structural"))
  txt <- ch$chunks$text
  # Both headings became "## Results", neither year reached any chunk, and
  # the two years' figures were one chunk.
  expect_true(any(grepl("2022 Results", txt, fixed = TRUE) & grepl("3.1 million", txt, fixed = TRUE)))
  expect_true(any(grepl("2023 Results", txt, fixed = TRUE) & grepl("4.8 million", txt, fixed = TRUE)))
  expect_false(any(grepl("3.1 million", txt, fixed = TRUE) & grepl("4.8 million", txt, fixed = TRUE)))
  expect_true("2023 Results" %in% ch$chunks$section)

  # A sentence led by a year, taken for a heading, keeps its year too.
  inline <- paste("2019 Revenue rose 8% to 3.4 million.", "Growth came from new customers in Spain.",
                  "2020 Revenue fell 5% to 3.1 million.", "The decline followed a lost contract.",
                  sep = "\n\n")
  txt2 <- quiet(gr_segment(quiet(gr_ingest(inline)), "structural"))$chunks$text
  expect_true(any(grepl("2019 Revenue rose", txt2, fixed = TRUE)))
  expect_true(any(grepl("2020 Revenue fell", txt2, fixed = TRUE)))
  # Section numbering is kept as written, with no heading repeated.
  txt3 <- quiet(gr_segment(quiet(gr_ingest("1. Introduction\n\nBody one.\n\n2. Methods\n\nBody two.")),
                           "structural"))$chunks$text
  expect_identical(txt3, c("## 1. Introduction\n\nBody one.", "## 2. Methods\n\nBody two."))
})

# ---------------------------------------------------------------------------
# segment-03 / read-core-02: model-written chunk text keeps its source beside it.
# ---------------------------------------------------------------------------

test_that("proposition chunks carry the document text they were written from", {
  src <- "The trial enrolled 120 patients.\n\nOutcomes improved in the treatment group."
  cl <- gr_mock_client(function(m, p)
    '{"propositions": ["The trial enrolled 120 patients.", "Mortality fell by 40% in the treatment arm."]}')
  ch <- quiet(gr_segment(quiet(gr_ingest(src)), list(method = "proposition", max_tokens = 200),
                         client = cl))
  expect_true("source_text" %in% names(ch$chunks))
  # The invented proposition is in the chunk and not in its source, which is
  # what a quote of it has to be found in.
  expect_match(ch$chunks$text[1], "40%", fixed = TRUE)
  expect_identical(ch$chunks$source_text[1], src)

  # A chunk packed from several batches names all of them, in order; one
  # packed from a single batch names only that one.
  paras <- sprintf("Paragraph %d reports that site %d enrolled %d adults over the year.",
                   1:12, 1:12, 10 * (1:12))
  k <- new.env(); k$n <- 0L
  cl2 <- gr_mock_client(function(m, p) {
    k$n <- k$n + 1L
    sprintf('{"propositions": ["Batch %d holds a fact."]}', k$n)
  })
  ch2 <- quiet(gr_segment(quiet(gr_ingest(paste(paras, collapse = "\n\n"))),
                          list(method = "proposition", max_tokens = 400,
                               proposition_batch_tokens = 200), client = cl2))
  expect_gt(k$n, 1L)
  st <- ch2$chunks$source_text
  expect_false(anyNA(st))
  for (p in paras) expect_true(any(grepl(p, st, fixed = TRUE)), label = p)
  # Every source is document text, never a proposition.
  expect_false(any(grepl("Batch [0-9]+ holds", st)))

  # Cutting an oversized chunk to the cap keeps each piece's source.
  long <- paste(rep("The cohort was followed for five years at nine sites.", 30), collapse = " ")
  cl3 <- gr_mock_client(function(m, p) sprintf('{"propositions": ["%s"]}', long))
  ch3 <- quiet(gr_segment(quiet(gr_ingest(src)), list(method = "proposition", max_tokens = 60),
                          client = cl3))
  expect_gt(nrow(ch3$chunks), 1L)
  expect_true(all(ch3$chunks$source_text == src))
})

test_that("contextual chunks keep the context line out of their source text", {
  doc <- quiet(gr_ingest(paste("Annual report for the year.",
                               "Revenue for the year was 45.2 million dollars, up from 40.1 million.",
                               sep = "\n\n")))
  cl <- gr_mock_client(function(m, p) "This excerpt reports that revenue rose to 52 million dollars.")
  ch <- quiet(gr_segment(doc, list(method = "contextual", context_source = "llm", max_tokens = 400),
                         client = cl))
  expect_match(ch$chunks$text[1], "52 million", fixed = TRUE)
  expect_false(grepl("52 million", ch$chunks$source_text[1], fixed = TRUE))
  expect_identical(ch$chunks$source_text[1], doc$text)

  # The free header is not the document's text either.
  ch2 <- quiet(gr_segment(doc, list(method = "contextual", max_tokens = 400)))
  expect_match(ch2$chunks$text[1], "^\\[Source: ")
  expect_identical(ch2$chunks$source_text[1], doc$text)

  # A header the model did not write leaves the chunk as the document's own.
  dead <- gr_mock_client(function(m, p) stop("offline"))
  ch3 <- quiet(gr_segment(doc, list(method = "contextual", context_source = "llm", max_tokens = 400),
                          client = dead))
  expect_identical(ch3$chunks$text[1], doc$text)
  expect_true(is.na(ch3$chunks$source_text[1]))

  # Segmenters that write nothing of their own add no column.
  expect_false("source_text" %in% names(quiet(gr_segment(doc, "paragraph"))$chunks))
})

# ---------------------------------------------------------------------------
# state-concurrency-05: the user's own functions work in workers.
# ---------------------------------------------------------------------------

# Defines `fns` in the global environment for the length of a test, the way a
# script would, and removes them afterwards.
local_workspace <- function(fns, env = parent.frame()) {
  for (nm in names(fns)) {
    f <- fns[[nm]]
    if (is.function(f)) environment(f) <- globalenv()
    assign(nm, f, envir = globalenv())
  }
  withr::defer(rm(list = names(fns), envir = globalenv()), envir = env)
}

test_that("a handler that calls a workspace helper works in parallel", {
  skip_if_no_future()
  local_registries()
  local_workspace(list(
    review_seg_reply = "Revenue was 45.2 million.",
    review_seg_proxy = function(messages) review_seg_reply
  ))
  handler <- function(messages, params) review_seg_proxy(messages)
  environment(handler) <- globalenv()
  cl <- gr_mock_client(handler)
  gr_options(workers = 2)
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(numbered_paragraphs(6), f)
  a <- quiet(answer_document(f, "What was revenue?", "thorough", client = cl,
                             max_tokens = 40, parallel = TRUE))
  # Every chunk's call failed with 'could not find function "review_seg_proxy"'.
  expect_length(a$trace$errors, 0L)
  expect_identical(a$answer, "Revenue was 45.2 million.")
  expect_false(a$partial)
})

test_that("a tokenizer that calls a workspace helper works in parallel", {
  skip_if_no_future()
  local_registries()
  local_workspace(list(review_seg_pieces = function(x) as.integer(ceiling(nchar(x) / 3))))
  tok <- function(x) review_seg_pieces(x)
  environment(tok) <- globalenv()
  gr_set_tokenizer("review_seg_tok", tok)
  gr_options(workers = 2)
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(numbered_paragraphs(6), f)
  # The parallel run aborted with 'could not find function "review_seg_pieces"'.
  a <- quiet(answer_document(f, "What was revenue?", "thorough", client = mock_echo("Found."),
                             max_tokens = 40, parallel = TRUE))
  expect_identical(a$answer, "Found.")
})

# ---------------------------------------------------------------------------
# state-concurrency-03: what a client keeps survives a parallel batch.
# ---------------------------------------------------------------------------

test_that("a replay under parallel = TRUE hands out the recording in order", {
  skip_if_no_future()
  local_registries()
  txt <- paste(sprintf("Paragraph %d of the annual report. In year %d revenue was %d million dollars.",
                       1:6, 2000 + 1:6, 10 * 1:6), collapse = "\n\n")
  k <- new.env(); k$n <- 0L
  cl <- gr_mock_client(function(m, p) { k$n <- k$n + 1L; sprintf("Draw %d: revenue was %d.", k$n, k$n) })
  gr_options(parallel = FALSE)
  rec <- quiet(gr_compare(txt, "What was revenue?", list("thorough", "thorough"), client = cl,
                          allow_duplicates = TRUE, max_tokens = 40, overlap_tokens = 0))
  f <- withr::local_tempfile(fileext = ".json")
  gr_trace_save(rec$trace, f)

  gr_options(parallel = TRUE, workers = 2)
  rp <- gr_replay_client(f)
  again <- quiet(gr_compare(txt, "What was revenue?", list("thorough", "thorough"), client = rp,
                            allow_duplicates = TRUE, max_tokens = 40, overlap_tokens = 0))
  answers <- function(x) vapply(x$answers, function(a) a$answer, character(1))
  # The second recipe got the first recipe's draws, and stats() said 2 hits.
  expect_identical(answers(again), answers(rec))
  s <- rp$stats()
  expect_identical(s$hits, k$n)
  expect_identical(s$repeats, 0L)

  # A replay that diverges says so under parallel = TRUE too.
  rp2 <- gr_replay_client(f, strict = FALSE)
  quiet(gr_compare(gsub("revenue", "turnover", txt), "What was revenue?",
                   list("thorough", "thorough"), client = rp2,
                   allow_duplicates = TRUE, max_tokens = 40, overlap_tokens = 0))
  expect_gt(length(rp2$missed()), 0L)
})

test_that("a mock's calls and a cache's counts include what workers did", {
  skip_if_no_future()
  local_registries()
  gr_options(workers = 2)
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(numbered_paragraphs(6), f)
  cache <- gr_cache(withr::local_tempdir())
  cl <- gr_cache_client(gr_mock_client(function(m, p) "Revenue was 10."), cache)
  a <- quiet(answer_document(f, "What was revenue?", "thorough", client = cl,
                             max_tokens = 40, parallel = TRUE))
  requests <- sum(vapply(a$trace$steps, function(s) !identical(s$kind, "local"), logical(1)))
  expect_gte(requests, 6L)
  # calls() held only the merge, and the cache counted one miss in seven.
  expect_identical(length(cl$calls()), requests)
  expect_identical(gr_cache_stats(cache)$misses, requests)
  expect_identical(gr_cache_stats(cache)$writes, requests)
  labels <- vapply(cl$calls(), function(x) x$label, character(1))
  expect_identical(labels, vapply(Filter(function(s) !identical(s$kind, "local"), a$trace$steps),
                                  function(s) s$label, character(1)))

  # The segmenters say which client they use.
  cl2 <- gr_mock_client(function(m, p) '{"propositions": ["A is one.", "B is two."]}')
  quiet(gr_segment(quiet(gr_ingest(numbered_paragraphs(20))),
                   list(method = "proposition", proposition_batch_tokens = 200, parallel = TRUE),
                   client = cl2))
  expect_gt(length(cl2$calls()), 1L)
})
