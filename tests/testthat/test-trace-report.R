# Requests one row each, a progress line for long runs, and the evidence page.

# --- one row per request ---------------------------------------------------

req_doc <- function(n = 6L) {
  paste(sprintf("Paragraph %d says revenue was %d.5 million dollars in year %d.", seq_len(n),
                40L + seq_len(n), 2000L + seq_len(n)), collapse = "\n\n")
}

test_that("as.data.frame() on a trace gives one row per request, and no local steps", {
  cl <- gr_mock_client(function(m, p) "Revenue was 41.5 million dollars.")
  ans <- quiet(answer_document(req_doc(), "What was revenue?", "thorough", client = cl,
                               max_tokens = 40, overlap_tokens = 0))
  df <- as.data.frame(ans$trace)
  expect_s3_class(df, "data.frame")
  expect_named(df, c("step", "document", "recipe", "stage", "model", "ok", "cached",
                     "tokens_in", "tokens_out", "usd", "seconds", "error", "prompt", "reply"))
  expect_equal(nrow(df), ans$trace$calls)
  local <- vapply(ans$trace$steps, function(s) identical(s$kind, "local"), logical(1))
  expect_true(any(local))
  expect_setequal(df$step, which(!local))
  expect_equal(df$stage, vapply(ans$trace$steps[df$step], `[[`, "", "label"))
  expect_type(df$step, "integer")
  expect_type(df$tokens_in, "integer")
  expect_type(df$usd, "double")
  expect_type(df$seconds, "double")
  expect_true(all(df$ok))
  expect_true(all(is.na(df$error)))
  expect_equal(unique(df$document), "<inline text>")
  expect_equal(unique(df$recipe), "thorough")
  expect_equal(unique(df$reply), "Revenue was 41.5 million dollars.")
  expect_match(df$prompt[1], "^\\[system\\] ")
  expect_match(df$prompt[1], "\n\n[user] ", fixed = TRUE)
})

test_that("the usd column adds up to gr_trace_cost(), and cached and unpriced rows say so", {
  gr_register_model("rpt-priced", context_window = 8000, max_output = 500,
                    input_usd = 2, output_usd = 8)
  cache <- gr_cache(withr::local_tempdir())
  base <- gr_mock_client(function(m, p) "Revenue was 41.5 million dollars.")
  cl <- gr_cache_client(base, cache)
  first <- quiet(answer_document(req_doc(), "What was revenue?", "thorough", client = cl,
                                 model = "rpt-priced", max_tokens = 40, overlap_tokens = 0))
  df <- as.data.frame(first$trace)
  expect_false(any(df$cached))
  expect_true(all(df$usd > 0))
  expect_equal(sum(df$usd), sum(gr_trace_cost(first$trace)$usd))
  expect_equal(df$usd, (df$tokens_in * 2 + df$tokens_out * 8) / 1e6)

  again <- quiet(answer_document(req_doc(), "What was revenue?", "thorough", client = cl,
                                 model = "rpt-priced", max_tokens = 40, overlap_tokens = 0))
  df2 <- as.data.frame(again$trace)
  expect_true(all(df2$cached))
  expect_equal(df2$usd, rep(0, nrow(df2)))
  expect_equal(sum(gr_trace_cost(again$trace)$usd), 0)

  free <- gr_mock_client(function(m, p) "x")
  tr <- gr_trace()
  quiet(gr_call(free, "hi", model = "rpt-no-such-model", trace = tr))
  expect_true(is.na(as.data.frame(tr)$usd))
})

test_that("each request records how long it took", {
  slow <- gr_mock_client(function(m, p) { Sys.sleep(0.3); "done" })
  tr <- gr_trace()
  gr_call(slow, "hi", trace = tr)
  expect_gte(tr$steps[[1]]$seconds, 0.25)
  expect_gte(as.data.frame(tr)$seconds, 0.25)

  # A request with no room for a reply is refused before it is sent: no time.
  gr_register_model("rpt-tiny", context_window = 60, max_output = 20)
  tr2 <- gr_trace()
  res <- gr_call(slow, paste(rep("word", 200), collapse = " "), model = "rpt-tiny", trace = tr2)
  expect_false(res$ok)
  expect_identical(tr2$steps[[1]]$seconds, 0)
  expect_match(as.data.frame(tr2)$error, "no room|leaving no room")
})

test_that("a failed request has its error and no reply", {
  bad <- gr_mock_client(function(m, p) stop("network down"))
  tr <- gr_trace()
  gr_call(bad, "hi", trace = tr)
  df <- as.data.frame(tr)
  expect_false(df$ok)
  expect_match(df$error, "network down")
  expect_identical(df$reply, "")
})

test_that("an empty trace gives a table with no rows and the same columns", {
  df <- as.data.frame(gr_trace())
  expect_equal(nrow(df), 0L)
  expect_named(df, c("step", "document", "recipe", "stage", "model", "ok", "cached",
                     "tokens_in", "tokens_out", "usd", "seconds", "error", "prompt", "reply"))
  expect_type(df$usd, "double")
  tr <- gr_trace()
  gr_call(gr_mock_client(function(m, p) "x"), "hi", trace = tr)
  expect_equal(rownames(as.data.frame(tr, row.names = "only")), "only")
})

test_that("a corpus trace says which document each request was about", {
  d <- withr::local_tempdir()
  writeLines("Alpha revenue was 10.5 million.", file.path(d, "alpha.txt"))
  writeLines("Beta revenue was 20.5 million.", file.path(d, "beta.txt"))
  cl <- gr_mock_client(function(m, p) "Revenue was stated.")
  co <- quiet(gr_read_many(d, "What was revenue?", "fast", client = cl))
  df <- as.data.frame(co$trace)
  expect_setequal(unique(df$document), c("alpha.txt", "beta.txt"))
  expect_equal(unique(df$recipe), "fast")
  expect_equal(sum(df$document == "alpha.txt"), co$summary$calls[co$summary$document == "alpha.txt"])
  # A document's own trace says the same, from what the run recorded about it.
  own <- as.data.frame(co$answers[["alpha.txt"]]$trace)
  expect_equal(unique(own$document), "alpha.txt")
  expect_equal(unique(own$recipe), "fast")

  # Folded into a parent, the document survives the second fold.
  parent <- gr_trace()
  co2 <- quiet(gr_read_many(d, "What was revenue now?", "fast", client = cl, trace = parent))
  expect_setequal(unique(as.data.frame(parent)$document), c("alpha.txt", "beta.txt"))
})

test_that("each run's requests keep their own document and recipe in a shared trace", {
  a <- tempfile(fileext = ".txt"); writeLines("Revenue was 45.2 million.", a)
  b <- tempfile(fileext = ".txt"); writeLines("Revenue was 51.8 million.", b)
  cl <- gr_mock_client(function(m, p) "Revenue was stated.")
  x1 <- quiet(answer_document(a, "What was revenue?", client = cl))
  expect_equal(x1$recipe, "fast")
  quiet(answer_document(b, "What was revenue?", "thorough", client = cl, trace = x1$trace))
  df <- as.data.frame(x1$trace)
  expect_equal(unique(df$document), c(basename(a), basename(b)))
  # "auto" is not a recipe that read anything: the one it chose is.
  expect_equal(df$recipe[df$document == basename(a)], rep("fast", sum(df$document == basename(a))))
  expect_equal(unique(df$recipe[df$document == basename(b)]), "thorough")

  # A request already in the trace keeps what it had: here, nothing.
  tr <- gr_trace()
  gr_call(cl, "hi", trace = tr)
  quiet(answer_document(a, "What was revenue?", "fast", client = cl, trace = tr))
  got <- as.data.frame(tr)[, c("document", "recipe")]
  expect_equal(got$document, c(NA, basename(a)))
  expect_equal(got$recipe, c(NA, "fast"))
})

test_that("folding a trace in never takes a recipe from a list of recipes", {
  parent <- gr_trace()
  child <- gr_trace(meta = list(recipes = c("fast", "thorough")))
  gr_call(gr_mock_client(function(m, p) "x"), "hi", trace = child)
  readgpt:::trace_absorb(parent, child)
  expect_true(is.na(as.data.frame(parent)$recipe))
})

test_that("a comparison's shared requests belong to no recipe, and each answer to its own", {
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  cmp <- quiet(gr_compare(readgpt_example(), "What was revenue?", c("fast", "thorough"),
                          client = cl, method = "contextual", context_source = "llm"))
  df <- as.data.frame(cmp$trace)
  expect_true(any(df$stage == "segment.context"))
  expect_true(all(is.na(df$recipe[df$stage == "segment.context"])))
  expect_setequal(unique(df$recipe[df$stage != "segment.context"]), c("fast", "thorough"))
  expect_equal(unique(as.data.frame(cmp$answers$fast$trace)$document), "annual_report.md")
})

test_that("a cached request to a model with no price is unknown, as gr_trace_cost() says", {
  gr_register_model("rpt-local", context_window = 32768, max_output = 4096)
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  ans <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = cl,
                               model = "rpt-local"))
  again <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast",
                                 client = gr_replay_client(ans$trace), model = "rpt-local"))
  df <- as.data.frame(again$trace)
  expect_true(all(df$cached))
  expect_true(all(is.na(df$usd)))
  expect_identical(sum(df$usd), sum(gr_trace_cost(again$trace)$usd))
})

# --- progress --------------------------------------------------------------

# Every message a call raised, and the progress lines among them.
catch_msgs <- function(expr) {
  got <- list()
  value <- withCallingHandlers(expr, message = function(m) {
    got[[length(got) + 1L]] <<- m
    invokeRestart("muffleMessage")
  })
  list(value = value, all = vapply(got, conditionMessage, ""),
       progress = vapply(Filter(function(m) inherits(m, "gr_progress"), got),
                         conditionMessage, ""))
}

test_that("a per-chunk reader keeps one line up to date with the count and the spend", {
  local_mocked_bindings(progress_wanted = function() TRUE)
  gr_register_model("rpt-priced", context_window = 8000, max_output = 500,
                    input_usd = 2, output_usd = 8)
  cl <- gr_mock_client(function(m, p) "Revenue was 41.5 million dollars.")
  got <- catch_msgs(answer_document(req_doc(), "What was revenue?", "thorough", client = cl,
                                    model = "rpt-priced", max_tokens = 40,
                                    overlap_tokens = 0))
  n <- got$value$segmentation$n
  expect_gt(n, 2L)
  p <- got$progress
  expect_match(p[1], sprintf("^\r  chunk 0 of %d, \\$0\\.0000 spent", n))
  expect_true(any(grepl(sprintf("chunk %d of %d, \\$0\\.0[0-9]+ spent", n, n), p)))
  # The last one clears the line.
  expect_match(p[length(p)], "^\r +\r$")
  expect_false(isTRUE(readgpt:::gr_state$progress_on))
  # The clearing line is as wide as the widest line drawn.
  expect_equal(nchar(p[length(p)]), max(nchar(p[-length(p)])) + 1L)
})

test_that("nothing is drawn for a script, a single item, or with messages suppressed", {
  cl <- gr_mock_client(function(m, p) "Revenue was 41.5 million dollars.")
  got <- catch_msgs(answer_document(req_doc(), "What was revenue?", "thorough", client = cl,
                                    max_tokens = 40, overlap_tokens = 0))
  expect_length(got$progress, 0L)

  local_mocked_bindings(progress_wanted = function() TRUE)
  one <- catch_msgs(answer_document("Revenue was 41.5 million.", "What was revenue?",
                                    "thorough", client = cl))
  expect_length(one$progress, 0L)

  expect_silent(suppressMessages(answer_document(req_doc(), "What was revenue?", "thorough",
                                                 client = cl, max_tokens = 40,
                                                 overlap_tokens = 0)))
})

test_that("a model with no registered price makes the spend unknown, not zero", {
  local_mocked_bindings(progress_wanted = function() TRUE)
  cl <- gr_mock_client(function(m, p) "Revenue was 41.5 million dollars.")
  got <- catch_msgs(suppressWarnings(answer_document(
    req_doc(), "What was revenue?", "thorough", client = cl, model = "rpt-unpriced-model",
    max_tokens = 40, overlap_tokens = 0)))
  after_first <- got$progress[-1]
  expect_true(any(grepl(", cost unknown", after_first, fixed = TRUE)))
  expect_false(any(grepl("spent", after_first[-length(after_first)])))
})

test_that("a line is never wider than the console", {
  local_mocked_bindings(progress_wanted = function() TRUE)
  withr::local_options(width = 30)
  got <- catch_msgs(readgpt:::gr_lapply(1:3, function(i, trace) i,
                                        label = "a label much longer than thirty"))
  drawn <- got$progress[-length(got$progress)]
  expect_true(length(drawn) > 0L)
  expect_true(all(nchar(drawn) <= 30L))
})

test_that("progress is for a person at the console: not scripts, not knitting, not quiet", {
  old <- gr_options(verbose = TRUE)
  withr::defer(gr_options(old))
  local_mocked_bindings(session_interactive = function() TRUE)
  expect_true(readgpt:::progress_wanted())
  withr::with_options(list(knitr.in.progress = TRUE), expect_false(readgpt:::progress_wanted()))
  gr_options(verbose = FALSE)
  expect_false(readgpt:::progress_wanted())
  gr_options(verbose = TRUE)
  local_mocked_bindings(session_interactive = function() FALSE)
  expect_false(readgpt:::progress_wanted())
})

test_that("another message clears the line first, and an inner loop draws no second line", {
  local_mocked_bindings(progress_wanted = function() TRUE)
  seen <- character(0)
  out <- withCallingHandlers(
    readgpt:::gr_lapply(1:4, function(i, trace) {
      if (i == 2L) message("a note")
      # A nested loop while the outer line is showing.
      readgpt:::gr_lapply(1:3, function(j, trace) j, label = "inner")
      i
    }, label = "item"),
    message = function(m) { seen <<- c(seen, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_equal(unlist(out), 1:4)
  at <- which(seen == "a note\n")
  expect_length(at, 1L)
  expect_match(seen[at - 1L], "^\r +\r$")
  expect_false(any(grepl("inner", seen)))
  expect_true(any(grepl("item 4 of 4", seen)))
  expect_false(isTRUE(readgpt:::gr_state$progress_on))
})

test_that("an error part way clears the line before it is reported, and frees it", {
  local_mocked_bindings(progress_wanted = function() TRUE)
  log <- character(0)
  res <- tryCatch(withCallingHandlers(
    readgpt:::gr_lapply(1:3, function(i, trace) {
      if (i == 2L) stop("boom")
      i
    }, label = "item"),
    message = function(m) {
      log <<- c(log, if (grepl("^\r +\r$", conditionMessage(m))) "clear" else "draw")
      invokeRestart("muffleMessage")
    },
    error = function(e) log <<- c(log, "error")),
    error = function(e) conditionMessage(e))
  expect_equal(res, "boom")
  # Cleared before anything outside saw the error, as R prints it before
  # on.exit() runs.
  expect_equal(log, c("draw", "clear", "error"))
  expect_false(isTRUE(readgpt:::gr_state$progress_on))
  got <- catch_msgs(readgpt:::gr_lapply(1:3, function(i, trace) i, label = "item"))
  expect_true(length(got$progress) > 0L)
})

test_that("a line that is never shown does not stop the next one", {
  local_mocked_bindings(progress_wanted = function() TRUE)
  p <- readgpt:::progress_start(3L, "item")
  expect_false(is.null(p))
  expect_false(isTRUE(readgpt:::gr_state$progress_on))
  got <- catch_msgs(readgpt:::gr_lapply(1:3, function(i, trace) i, label = "item"))
  expect_match(got$progress[1], "^\r  item 0 of 3$")
})

test_that("refine, which reads in a loop of its own, shows the line too", {
  local_mocked_bindings(progress_wanted = function() TRUE)
  cl <- gr_mock_client(function(m, p) "Revenue was 41.5 million dollars.")
  got <- catch_msgs(answer_document(req_doc(), "What was revenue?", "thorough", client = cl,
                                    reader = "refine", max_tokens = 40, overlap_tokens = 0))
  expect_equal(got$value$reader, "refine")
  expect_match(got$progress[1], "^\r  chunk 0 of [0-9]+")
  expect_equal(sum(grepl("chunk 0 of", got$progress, fixed = TRUE)), 1L)
  expect_match(got$progress[length(got$progress)], "^\r +\r$")
})

test_that("in a comparison the line carries on from the recipes before", {
  local_mocked_bindings(progress_wanted = function() TRUE)
  gr_register_model("rpt-priced", context_window = 8000, max_output = 500,
                    input_usd = 2, output_usd = 8)
  cl <- gr_mock_client(function(m, p) "Revenue was 41.5 million dollars.")
  got <- catch_msgs(gr_compare(req_doc(), "What was revenue?", c("thorough", "narrative"),
                               client = cl, model = "rpt-priced", max_tokens = 40,
                               overlap_tokens = 0))
  starts <- grep("chunk 0 of", got$progress, value = TRUE)
  expect_length(starts, 2L)
  first <- sum(gr_trace_cost(got$value$answers$thorough$trace)$usd)
  expect_gt(first, 0)
  expect_match(starts[1], "$0.0000 spent", fixed = TRUE)
  expect_match(starts[2], sprintf("$%.4f spent", first), fixed = TRUE)
})

test_that("a corpus whose spend is unknown says so from the next document on", {
  local_mocked_bindings(progress_wanted = function() TRUE)
  d <- withr::local_tempdir()
  writeLines(req_doc(), file.path(d, "a.txt"))
  writeLines(sub("Paragraph 1", "Paragraph one", req_doc()), file.path(d, "b.txt"))
  cl <- gr_mock_client(function(m, p) "Revenue was 41.5 million dollars.")
  got <- catch_msgs(suppressWarnings(gr_read_many(d, "What was revenue?", "thorough",
                                                  client = cl, model = "rpt-unpriced-model",
                                                  max_tokens = 40, overlap_tokens = 0)))
  starts <- grep("chunk 0 of", got$progress, value = TRUE)
  expect_length(starts, 2L)
  expect_match(starts[2], ", cost unknown$")
})

test_that("gr_read_many() gives what the run has spent before each document", {
  old <- gr_options(verbose = TRUE)
  withr::defer(gr_options(old))
  gr_register_model("rpt-priced", context_window = 8000, max_output = 500,
                    input_usd = 2, output_usd = 8)
  d <- withr::local_tempdir()
  for (nm in c("a", "b", "c")) writeLines(sprintf("Doc %s revenue was 10.5 million.", nm),
                                          file.path(d, paste0(nm, ".txt")))
  cl <- gr_mock_client(function(m, p) "Revenue was 10.5 million.")
  store <- file.path(withr::local_tempdir(), "store")
  got <- catch_msgs(suppressWarnings(gr_read_many(d, "What was revenue?", "fast", client = cl,
                                                  model = "rpt-priced", store = store)))
  lines <- grep("^\\[[0-9]/3\\]", got$all, value = TRUE)
  expect_match(lines[1], "^\\[1/3\\] a\\.txt\n$")
  expect_match(lines[2], "^\\[2/3\\] b\\.txt \\(\\$0\\.[0-9]{4} spent so far\\)\n$")
  expect_match(lines[3], "^\\[3/3\\] c\\.txt \\(\\$0\\.[0-9]{4} spent so far\\)\n$")
  second <- as.numeric(sub(".*\\$([0-9.]+) spent.*", "\\1", lines[3]))
  expect_equal(second, round(sum(got$value$summary$cost_usd[1:2]), 4))

  again <- catch_msgs(gr_read_many(d, "What was revenue?", "fast", client = cl,
                                   model = "rpt-priced", store = store))
  expect_true(any(grepl("^\\[1/3\\] a\\.txt, restored from store\n$", again$all)))
})

test_that("in a corpus the line carries on from what the run has spent", {
  local_mocked_bindings(progress_wanted = function() TRUE)
  gr_register_model("rpt-priced", context_window = 8000, max_output = 500,
                    input_usd = 2, output_usd = 8)
  d <- withr::local_tempdir()
  for (nm in c("a", "b")) writeLines(req_doc(), file.path(d, paste0(nm, ".txt")))
  writeLines(sub("Paragraph 1", "Paragraph one", req_doc()), file.path(d, "b.txt"))
  cl <- gr_mock_client(function(m, p) "Revenue was 41.5 million dollars.")
  got <- catch_msgs(gr_read_many(d, "What was revenue?", "thorough", client = cl,
                                 model = "rpt-priced", max_tokens = 40, overlap_tokens = 0))
  starts <- grep("chunk 0 of", got$progress, value = TRUE)
  expect_length(starts, 2L)
  first_doc <- got$value$summary$cost_usd[1]
  expect_gt(first_doc, 0)
  expect_match(starts[1], "\\$0\\.0000 spent")
  expect_match(starts[2], sprintf("\\$%.4f spent", first_doc), fixed = FALSE)
})

# --- the evidence page -----------------------------------------------------

read_page <- function(...) {
  p <- tempfile(fileext = ".html")
  suppressMessages(gr_audit_report(p, ..., open = FALSE))
  paste(readLines(p, encoding = "UTF-8", warn = FALSE), collapse = "\n")
}

# An answer with hand-made evidence, so the page can be checked row by row.
answer_with <- function(evidence, text = "Revenue was 45.2 million [chunk 7].", partial = FALSE,
                        question = "What was revenue?", notes = list()) {
  a <- new_answer(text, "stuff", question, chunks_used = unique(evidence$chunk_id),
                  trace = gr_trace(), evidence = evidence, partial = partial, notes = notes)
  a$signature <- "all|1|none"
  a
}

test_that("a whole-chunk passage shows the answer's numbers, and a cited chunk says so", {
  ev <- readgpt:::evidence_table(c(7L, 2L), c("Revenue in 2024 was 45.2 million, up from 145.25.",
                                              "Nothing about <money> here, 45.20 aside."),
                                 pages = c(4L, 1L), sections = c("Results", "Intro"),
                                 kind = "verbatim")
  h <- read_page(answer = answer_with(ev))
  expect_match(h, "Revenue was 45.2 million [chunk 7].", fixed = TRUE)
  expect_match(h, "Revenue in 2024 was <mark>45.2</mark> million, up from 145.25.", fixed = TRUE)
  # Not inside a longer number, and markup in the document is escaped.
  expect_false(grepl("<mark>45.2</mark>0", h, fixed = TRUE))
  expect_match(h, "Nothing about &lt;money&gt; here", fixed = TRUE)
  expect_match(h, "Page 4, section &quot;Results&quot;, chunk 7, cited in the answer", fixed = TRUE)
  expect_match(h, "Page 1, section &quot;Intro&quot;, chunk 2</p>", fixed = TRUE)
  # In document order: page 1 before page 4, whatever order the reader gave.
  expect_lt(regexpr("chunk 2</p>", h, fixed = TRUE), regexpr("chunk 7, cited", h, fixed = TRUE))
  expect_match(h, "Not partial:", fixed = TRUE)
  expect_match(h, "Stages run: answered.", fixed = TRUE)
  expect_match(h, "<strong>Question.</strong> What was revenue?", fixed = TRUE)
  # A report on one answer has no search to report, and no quotations to explain.
  expect_false(grepl("<h2>The search</h2>", h, fixed = TRUE))
  expect_false(grepl("A verified quote is", h, fixed = TRUE))
  expect_false(grepl("Screening saw", h, fixed = TRUE))
  expect_match(h, "A passage shows where an answer came from", fixed = TRUE)
})

test_that("rows with no page come after the rest, in chunk order", {
  ev <- readgpt:::evidence_table(c(9L, 3L, 5L), c("Ninth 11.5.", "Third 11.5.", "Fifth 11.5."),
                                 pages = c(NA, 2L, NA), kind = "verbatim")
  h <- read_page(answer = answer_with(ev, text = "It was 11.5."))
  pos <- vapply(c(">Page 2, chunk 3<", ">Chunk 5<", ">Chunk 9<"),
                function(s) as.integer(regexpr(s, h, fixed = TRUE)), integer(1))
  expect_true(all(pos > 0L))
  expect_equal(order(pos), 1:3)
})

test_that("a quotation is marked where it stands, and one that is not there is flagged", {
  chunk <- "The trial enrolled 412 patients.\nMortality was 12.5%   in the \u201ctreated\u201d arm."
  ev <- readgpt:::evidence_table(c(1L, 1L, 1L),
                                 c("mortality was 12.5% in the \"treated\" arm",
                                   "The trial enrolled 900 patients.",
                                   "412 patients"),
                                 source_text = chunk, kind = "extracted")
  expect_equal(ev$verified, c(TRUE, FALSE, TRUE))
  a <- answer_with(ev, text = "Mortality was 12.5%.")
  a$partial <- TRUE
  a$notes$unverified_evidence <- 1L
  h <- read_page(answer = a)
  expect_match(h, "<mark>Mortality was 12.5%   in the \u201ctreated\u201d arm</mark>.", fixed = TRUE)
  expect_match(h, "enrolled <mark>412 patients</mark>.", fixed = TRUE)
  expect_match(h, "Quoted, but not found in this chunk (the longest run of its words that is: 60%)",
               fixed = TRUE)
  expect_match(h, "The trial enrolled 900 patients.", fixed = TRUE)
  expect_match(h, "1 quotation(s) could not be found in the chunk they cite.", fixed = TRUE)
  expect_match(h, "Partial: 1 quotation(s) not found in the document.", fixed = TRUE)
  # Quotations say where to look, so numbers are not marked as well.
  expect_equal(lengths(regmatches(h, gregexpr("<mark>", h, fixed = TRUE))), 2L)
  expect_match(h, "A verified quote is one that is in the document.", fixed = TRUE)
})

test_that("a reader's answer from each chunk is shown as its own words", {
  cl <- gr_mock_client(function(m, p) {
    u <- paste(vapply(m, `[[`, "", "content"), collapse = "\n")
    if (grepl("Paragraph 2 ", u, fixed = TRUE) && !grepl("<findings", u, fixed = TRUE))
      return("In this part, revenue was 42.5 million.")
    if (grepl("<findings", u, fixed = TRUE)) return("Revenue was 42.5 million.")
    "NOT_IN_DOCUMENT"
  })
  ans <- quiet(answer_document(req_doc(), "What was revenue?", "thorough", client = cl,
                               max_tokens = 40, overlap_tokens = 0))
  expect_equal(ans$reader, "map_reduce")
  h <- read_page(answer = ans)
  expect_match(h, "The reader's answer from this chunk. These are the model's words",
               fixed = TRUE)
  expect_match(h, "<blockquote class='passage'>In this part, revenue was 42.5 million.</blockquote>",
               fixed = TRUE)
  expect_false(grepl("<mark>", h, fixed = TRUE))
  expect_match(h, "<h2>Every request</h2>", fixed = TRUE)
  expect_match(h, sprintf("%d request(s)", ans$trace$calls), fixed = TRUE)
  expect_match(h, "<td>map.answer</td>", fixed = TRUE)
})

test_that("not found is worded by how much was read, and a partial answer says why", {
  whole <- answer_with(readgpt:::evidence_table(integer(0), character(0)),
                       text = "NOT_IN_DOCUMENT")
  h <- read_page(answer = whole)
  expect_match(h, "<strong>Not found in the document.</strong>", fixed = TRUE)
  expect_match(h, "The reader recorded no passages for this answer.", fixed = TRUE)

  some <- whole
  some$signature <- "topk|1|none"
  expect_match(read_page(answer = some), "Not found in the part of the document that was read.",
               fixed = TRUE)

  cut <- answer_with(readgpt:::evidence_table(1L, "Revenue 45.2.", kind = "verbatim"),
                     text = "Revenue was 45.2 million.", partial = TRUE,
                     notes = list(failed_calls = 2L))
  expect_match(read_page(answer = cut), "Partial: 2 request(s) failed.", fixed = TRUE)
})

test_that("a long passage is cut to the text around what is marked", {
  filler <- paste(rep("Background text with no figures in it at all.", 400), collapse = " ")
  long <- paste(filler, "Revenue was 45.2 million.", filler)
  ev <- readgpt:::evidence_table(1L, long, kind = "verbatim")
  h <- read_page(answer = answer_with(ev, text = "Revenue was 45.2 million."))
  expect_match(h, "Revenue was <mark>45.2</mark> million.", fixed = TRUE)
  expect_equal(lengths(regmatches(h, gregexpr("[...]", h, fixed = TRUE))), 2L)
  expect_lt(nchar(h), nchar(long) / 2)

  none <- read_page(answer = answer_with(ev, text = "It was large."))
  expect_match(none, sprintf("[... %d more characters]", nchar(long) - 6000L), fixed = TRUE)
})

test_that("the numbers marked are the answer's, whole, and not its citations", {
  expect_equal(readgpt:::answer_numbers("It was 45.2 million in 2024 [chunk 3], [chunks 12 and 14]; 7 sites."),
               c("2024", "45.2"))
  expect_equal(readgpt:::answer_numbers("NOT_IN_DOCUMENT"), character(0))
  sp <- readgpt:::number_spans("45.2 145.2 45.25 45,2 (45.2) 45.2.", c("45.2"))
  starts <- vapply(sp, `[[`, 1L, 1L)
  expect_equal(starts, c(1L, 24L, 30L))
  expect_equal(readgpt:::mark_html("a<b>c", list(c(2L, 4L), c(3L, 5L))),
               "a<mark>&lt;b&gt;c</mark>")
})

test_that("the page marks exactly the quotations the check found", {
  one <- function(quote, chunk) {
    ev <- readgpt:::evidence_table(1L, quote, source_text = chunk, kind = "extracted")
    list(verified = ev$verified, page = read_page(answer = answer_with(ev, text = "x")))
  }
  # A micro sign is not a Greek mu to the check, whatever a regex engine's case
  # folding thinks, so the quotation is flagged and not marked.
  mu <- one("received 5 \u03bcg daily", "Patients received 5 \u00b5g daily.")
  expect_false(mu$verified)
  expect_false(grepl("<mark>", mu$page, fixed = TRUE))
  expect_match(mu$page, "Quoted, but not found in this chunk", fixed = TRUE)

  dotted <- one("\u0130stanbul ofisi", "Yeni \u0130stanbul ofisi a\u00e7\u0131ld\u0131.")
  expect_true(dotted$verified)
  expect_match(dotted$page, "Yeni <mark>\u0130stanbul ofisi</mark> a\u00e7\u0131ld\u0131.",
               fixed = TRUE)

  crlf <- one("caf\u00e9 au lait costs 4.5", "Le caf\u00e9  au\r\nlait costs 4.5 euros.")
  expect_true(crlf$verified)
  expect_match(crlf$page, "Le <mark>caf\u00e9  au\nlait costs 4.5</mark> euros.", fixed = TRUE)

  long <- paste(rep("The trial enrolled 412 patients and measured outcomes at baseline.", 120),
                collapse = " ")
  expect_no_warning(big <- one(long, paste("Methods.", long, "Results follow.")))
  expect_true(big$verified)
  expect_match(big$page, "Methods. <mark>The trial enrolled", fixed = TRUE)
})

test_that("the page follows the check recorded on the evidence", {
  ev <- readgpt:::evidence_table(1L, "412 patients", source_text = "We enrolled 412 patients.",
                                 kind = "extracted")
  expect_true(ev$verified)
  ev$verified <- FALSE
  h <- read_page(answer = answer_with(ev, text = "412 patients."))
  expect_false(grepl("<mark>", h, fixed = TRUE))
  expect_match(h, "Quoted, but not found in this chunk", fixed = TRUE)
})

test_that("a chunk that runs across a page break keeps its place", {
  ev <- readgpt:::evidence_table(1:3, c("From page 1 onto page 2.", "On page 3.", "On page 4."),
                                 pages = c(NA, 3L, 4L), kind = "verbatim")
  h <- read_page(answer = answer_with(ev, text = "x"))
  pos <- vapply(c(">Chunk 1<", ">Page 3, chunk 2<", ">Page 4, chunk 3<"),
                function(s) as.integer(regexpr(s, h, fixed = TRUE)), integer(1))
  expect_true(all(pos > 0L))
  expect_equal(order(pos), 1:3)
})

test_that("numbers are not marked beside a quotation, and text around a mark is escaped", {
  chunk <- "Across 31 sites, <b>mortality</b> was 12.5% in the treated arm."
  ev <- readgpt:::evidence_table(1L, "mortality was 12.5%", source_text = chunk,
                                 kind = "extracted")
  expect_false(ev$verified)
  ev2 <- readgpt:::evidence_table(1L, "was 12.5% in the treated arm", source_text = chunk,
                                  kind = "extracted")
  h <- read_page(answer = answer_with(ev2, text = "Mortality was 12.5% across 31 sites."))
  expect_match(h, paste0("Across 31 sites, &lt;b&gt;mortality&lt;/b&gt; ",
                         "<mark>was 12.5% in the treated arm</mark>."), fixed = TRUE)
  expect_equal(lengths(regmatches(h, gregexpr("<mark>", h, fixed = TRUE))), 1L)

  v <- readgpt:::evidence_table(1L, "<i>Revenue</i> was 45.2 million.", kind = "verbatim")
  hv <- read_page(answer = answer_with(v, text = "Revenue was 45.2 million."))
  expect_match(hv, "&lt;i&gt;Revenue&lt;/i&gt; was <mark>45.2</mark> million.", fixed = TRUE)
})

test_that("a trace from before requests were timed says the times are not recorded", {
  ev <- readgpt:::evidence_table(1L, "Revenue 45.2.", kind = "verbatim")
  a <- answer_with(ev)
  gr_call(gr_mock_client(function(m, p) "x"), "hi", trace = a$trace)
  a$trace$steps <- lapply(a$trace$steps, function(s) { s$seconds <- NULL; s })
  h <- read_page(answer = a)
  expect_match(h, "1 request(s), their times were not recorded.", fixed = TRUE)
})

test_that("a corpus gives one row per document, then each answer and its passages", {
  d <- withr::local_tempdir()
  writeLines("Alpha revenue was 10.5 million.", file.path(d, "alpha.txt"))
  writeLines("Alpha revenue was 10.5 million.", file.path(d, "alpha_copy.txt"))
  writeLines("Beta says nothing on the subject.", file.path(d, "beta.txt"))
  cl <- gr_mock_client(function(m, p) {
    u <- paste(vapply(m, `[[`, "", "content"), collapse = "\n")
    if (grepl("Alpha", u, fixed = TRUE)) "Revenue was 10.5 million." else "NOT_IN_DOCUMENT"
  })
  co <- quiet(gr_read_many(c(file.path(d, c("alpha.txt", "alpha_copy.txt", "beta.txt")),
                             file.path(d, "missing.txt")),
                           "What was revenue?", "fast", client = cl))
  h <- read_page(answer = co)
  expect_match(h, "answered across 4 document(s)", fixed = TRUE)
  expect_match(h, "<h2>Every document</h2>", fixed = TRUE)
  expect_match(h, "<td>not found</td>", fixed = TRUE)
  expect_match(h, "<h3>alpha.txt</h3>", fixed = TRUE)
  expect_match(h, "Alpha revenue was <mark>10.5</mark> million.", fixed = TRUE)
  expect_match(h, "Same text as alpha.txt, which is shown there.", fixed = TRUE)
  expect_match(h, "<h3>missing.txt</h3>\n<p class='sub'>No answer: ", fixed = TRUE)
  expect_match(h, "<strong>Not found in the document.</strong>", fixed = TRUE)
  expect_match(h, "<h2>What the run cost</h2>", fixed = TRUE)

  lean <- quiet(gr_read_many(file.path(d, "alpha.txt"), "What was revenue?", "fast",
                             client = cl, keep_answers = FALSE))
  expect_match(read_page(answer = lean), "The answers were not kept", fixed = TRUE)

  # Every document failed: there were no answers to keep, and each says why.
  gone <- quiet(gr_read_many(file.path(d, c("gone1.txt", "gone2.txt")), "What was revenue?",
                             "fast", client = cl))
  hg <- read_page(answer = gone)
  expect_false(grepl("The answers were not kept", hg, fixed = TRUE))
  expect_equal(lengths(regmatches(hg, gregexpr("No answer: ", hg, fixed = TRUE))), 2L)
})

test_that("a copy found again on a resumed run is still shown as a copy", {
  d <- withr::local_tempdir()
  writeLines("Alpha revenue was 10.5 million.", file.path(d, "alpha.txt"))
  writeLines("Alpha revenue was 10.5 million.", file.path(d, "alpha_copy.txt"))
  cl <- gr_mock_client(function(m, p) "Revenue was 10.5 million.")
  store <- file.path(withr::local_tempdir(), "store")
  quiet(gr_read_many(d, "What was revenue?", "fast", client = cl, store = store))
  co <- quiet(gr_read_many(d, "What was revenue?", "fast", client = cl, store = store))
  expect_equal(co$summary$status, c("restored", "restored"))
  h <- read_page(answer = co)
  expect_match(h, "<th>duplicate_of</th>", fixed = TRUE)
  expect_match(h, "<h3>alpha_copy.txt</h3>\n<p class='sub'>Same text as alpha.txt", fixed = TRUE)
  expect_equal(lengths(regmatches(h, gregexpr("<h2>The answer</h2>|Where it came from</h4>", h))), 1L)
})

test_that("`answer` must be an answer or a corpus, and is enough on its own", {
  expect_error(gr_audit_report(tempfile(), answer = "text"), class = "gr_bad_audit_input")
  expect_error(gr_audit_report(tempfile()), "`answer`", class = "gr_bad_audit_input")
  # Appended, so every existing positional call means what it meant.
  f <- names(formals(gr_audit_report))
  expect_equal(f[1:9], c("path", "screening", "extraction", "synthesis", "protocol", "title",
                         "claims", "records", "calibration"))
  expect_equal(f[10:11], c("answer", "open"))
})

test_that("open = TRUE shows the page in the viewer or, outside tempdir(), the browser", {
  ev <- readgpt:::evidence_table(1L, "Revenue 45.2.", kind = "verbatim")
  a <- answer_with(ev)
  shown <- NULL
  browsed <- NULL
  local_mocked_bindings(open_in_browser = function(path) browsed <<- path)
  withr::local_options(viewer = function(url, ...) shown <<- url)
  p <- file.path(tempdir(), "rpt-open.html")
  suppressMessages(gr_audit_report(p, answer = a, open = TRUE))
  expect_equal(shown, normalizePath(p, winslash = "/"))
  expect_null(browsed)

  # The RStudio viewer shows only files under tempdir(); anything else goes to
  # the browser even when there is a viewer.
  outside <- file.path(getwd(), "rpt-open-outside.html")
  withr::defer(unlink(outside))
  shown <- NULL
  suppressMessages(gr_audit_report(outside, answer = a, open = TRUE))
  expect_null(shown)
  expect_equal(browsed, normalizePath(outside, winslash = "/"))

  browsed <- NULL
  withr::local_options(viewer = NULL)
  suppressMessages(gr_audit_report(p, answer = a, open = TRUE))
  expect_equal(browsed, normalizePath(p, winslash = "/"))

  shown <- NULL; browsed <- NULL
  suppressMessages(gr_audit_report(p, answer = a, open = FALSE))
  expect_null(shown)
  expect_null(browsed)
})

test_that("a review report still has its search and caveats, and can carry an answer", {
  ev <- readgpt:::evidence_table(1L, "Revenue 45.2.", kind = "verbatim")
  fields <- gr_fields(design = "The study design")
  cl <- gr_mock_client(function(messages, params) {
    '{"design":"randomised trial","design__quote":"We ran a randomised trial."}'
  })
  f <- tempfile(fileext = ".txt"); writeLines("We ran a randomised trial.", f)
  x <- quiet(gr_extract(f, fields, client = cl))
  h <- read_page(extraction = x, answer = answer_with(ev))
  expect_match(h, "<h2>The search</h2>", fixed = TRUE)
  expect_match(h, "A verified quote is one that is in the document.", fixed = TRUE)
  expect_match(h, "<h2>The answer</h2>", fixed = TRUE)
  expect_match(h, "Stages run: extracted, answered.", fixed = TRUE)
  expect_match(h, "<td>answer</td>", fixed = TRUE)
})
