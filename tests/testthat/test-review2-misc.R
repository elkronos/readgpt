# test-review2-misc.R -- the second pass on review findings whose fix landed
# in part elsewhere: the audit report's counts and flags, the ellmer adapter's
# model warning, and the extract reader's parallel spending check.
#
# Each block names the finding and says what the old behaviour was.

# ---------------------------------------------------------------------------
# synthesis-02: the audit report shows a citation to a study the section was
# not given.
# ---------------------------------------------------------------------------

m2_synthesis <- function(n_unsupplied = c(1L, 0L)) {
  sections <- data.frame(section = c("Trials", "Surveys"), brief = c("t", "s"),
                         text = c("A benefit [study 1], confirmed elsewhere [study 3].",
                                  "Surveys disagree [study 2]."),
                         n_cited = c(1L, 1L), n_unknown = c(0L, 0L),
                         partial = (n_unsupplied %||% c(0L, 0L)) > 0L, stringsAsFactors = FALSE)
  if (!is.null(n_unsupplied)) sections$n_unsupplied <- n_unsupplied
  structure(list(question = "Does it work?", sections = sections,
                 citations = data.frame(section = c("Trials", "Surveys"), study = 1:2,
                                        document = c("a.pdf", "b.pdf"),
                                        stringsAsFactors = FALSE),
                 skipped = 0L),
            class = "gr_synthesis")
}

m2_report <- function(...) {
  p <- withr::local_tempfile(fileext = ".html", .local_envir = parent.frame())
  quiet(gr_audit_report(p, ..., open = FALSE))
  gsub("[[:space:]]+", " ", paste(readLines(p, encoding = "UTF-8"), collapse = "\n"))
}

test_that("the audit report flags a citation to a study the section was not given", {
  # Before: the section was partial for it, and the report said nothing -- the
  # only flags were for a row that does not exist and for citing nothing.
  h <- m2_report(synthesis = m2_synthesis())
  expect_match(h, "<p class='flag'>1 citation(s) point at a study this section was not given.</p>",
               fixed = TRUE)
  # Only under the section that made it.
  trials <- regmatches(h, regexpr("<h3>Trials</h3>.*?<h3>Surveys</h3>", h, perl = TRUE))
  expect_match(trials, "not given", fixed = TRUE)
  surveys <- sub(".*<h3>Surveys</h3>", "", h)
  expect_false(grepl("not given", surveys, fixed = TRUE))

  # None, no flag; and a synthesis saved before the column existed still reports.
  expect_false(grepl("not given", m2_report(synthesis = m2_synthesis(c(0L, 0L))), fixed = TRUE))
  expect_false(grepl("not given", m2_report(synthesis = m2_synthesis(NULL)), fixed = TRUE))
})

# ---------------------------------------------------------------------------
# contracts-02: a document read in part is not a document that failed.
# ---------------------------------------------------------------------------

test_that("gr_flow() counts a document read in part apart from one that failed", {
  # Before: "incomplete" was counted under "failed to read", whose note says
  # "no values", although its values are real.
  t <- data.frame(document = c("a", "b", "c", "d", "e", "f"),
                  status = c("ok", "ok", "incomplete", "failed", "duplicate", "skipped"),
                  n_filled = c(2L, 0L, 1L, NA, 2L, NA),
                  n_unverified = c(0L, 0L, 0L, NA, 0L, NA),
                  duplicate_of = c(NA, NA, NA, NA, "a", NA), stringsAsFactors = FALSE)
  fl <- gr_flow(extraction = structure(list(table = t), class = "gr_extraction"))
  n <- function(stage) fl$n[fl$stage == stage]
  note <- function(stage) fl$note[fl$stage == stage]
  expect_identical(n("extracted from"), 5L)
  expect_identical(n("  reported nothing"), 1L)
  expect_identical(n("  read in part"), 1L)
  expect_identical(n("  failed to read"), 2L)     # failed and skipped, not incomplete
  expect_match(note("  read in part"), "the values are real", fixed = TRUE)
  expect_match(note("  failed to read"), "no values", fixed = TRUE)
  # The rows are disjoint parts of the distinct documents, so they cannot sum
  # to more than were extracted from; the one left over is "a", which has values.
  expect_identical(n("  reported nothing") + n("  read in part") + n("  failed to read"),
                   n("extracted from") - 1L)
})

test_that("a real extraction with a failed request is read in part in the flow and the report", {
  f <- gr_fields(design = "The study design",
                 n = gr_field("Number of participants", type = "integer"))
  say <- function(s) paste(rep(s, 3), collapse = " ")
  p <- withr::local_tempfile(fileext = ".txt")
  writeLines(c(say("We ran a randomised controlled trial of the new treatment across sites."), "",
               say("We enrolled 120 participants in total over the recruitment window.")), p)
  cl <- gr_mock_client(function(messages, params) {
    ex <- messages[[length(messages)]]$content
    if (grepl("enrolled", ex, fixed = TRUE)) stop("HTTP 429 rate limited")
    paste0('{"design":"randomised controlled trial","n":null,',
           '"design__quote":"We ran a randomised controlled trial of the new treatment across sites.",',
           '"n__quote":null}')
  })
  x <- quiet(gr_extract(p, f, client = cl, recipe = "thorough", max_tokens = 48))
  expect_identical(x$table$status, "incomplete")
  fl <- gr_flow(extraction = x)
  expect_identical(fl$n[fl$stage == "  read in part"], 1L)
  expect_identical(fl$n[fl$stage == "  failed to read"], 0L)
  h <- m2_report(extraction = x)
  expect_match(h, "read in part", fixed = TRUE)
})

test_that("the report says a row left out of the write-up may be one read only in part", {
  syn <- m2_synthesis(c(0L, 0L))
  syn$skipped <- 2L
  expect_match(m2_report(synthesis = syn),
               "2 row(s) were left out of the write-up: a duplicate, a document that could not be read in full, or one with nothing extracted.",
               fixed = TRUE)
})

# ---------------------------------------------------------------------------
# money-04: the ellmer model warning gives advice that is right.
# ---------------------------------------------------------------------------

m2_stub_chat <- function() {
  make <- function() {
    self <- new.env(parent = emptyenv())
    self$chat <- function(user, echo = "none") "Revenue was 45.2 million dollars."
    self$chat_structured <- function(user, type = NULL, echo = "none") list(answer = "x")
    self$clone <- function(deep = FALSE) make()
    self$set_turns <- function(value) invisible(self)
    self$set_system_prompt <- function(value) invisible(self)
    self$get_model <- function() "stub-model"
    self$get_tokens <- function() data.frame()
    self
  }
  make()
}

test_that("the ellmer model warning names the caller's model and does not say recipes carry one", {
  skip_if_not_installed("ellmer")
  local_registries()
  gr_register_model("stub-model", context_window = 128000L, max_output = 4096L,
                    input_usd = 0, output_usd = 0)
  said <- character(0)
  collect <- function(expr) withCallingHandlers(expr,
    gr_ellmer_model = function(w) { said <<- c(said, conditionMessage(w))
                                    invokeRestart("muffleWarning") },
    gr_ellmer_max_output = function(w) invokeRestart("muffleWarning"))

  # Before: "Recipes carry a model, so pass model = 'stub-model' to override
  # it", which told the caller to name a model; naming one is what causes this.
  collect(gr_call(gr_ellmer_client(m2_stub_chat()), "q", model = "gpt-4o"))
  expect_length(said, 1L)
  expect_match(said, "This run named model 'gpt-4o'", fixed = TRUE)
  expect_match(said, "answers, and is billed, as the model it was built with ('stub-model')",
               fixed = TRUE)
  expect_match(said, "Leave `model` unset", fixed = TRUE)
  expect_false(grepl("Recipes carry", said, fixed = TRUE))

  # A built-in recipe names no model, so it reads with the chat's and there is
  # nothing to warn about.
  said <- character(0)
  collect(suppressMessages(answer_document(readgpt_example(), "What was revenue?", "fast",
                                           client = gr_ellmer_client(m2_stub_chat()))))
  expect_length(said, 0L)
})

# ---------------------------------------------------------------------------
# money-06: the extract reader's parallel batch is priced, so it is held to
# what the run has actually spent.
# ---------------------------------------------------------------------------

# Forty fields: the listing goes out with every excerpt, and is most of each
# prompt, far past the flat allowance preflight makes for the prompt around a
# chunk.
m2_fields <- function() {
  do.call(gr_fields, stats::setNames(as.list(sprintf(paste0(
    "Field %d: the value the study reports for measured outcome number %d, as written in ",
    "the methods or results, with units where the paper gives them"), 1:40, 1:40)),
    sprintf("f%02d", 1:40)))
}

m2_chunks <- function() {
  doc <- paste(sprintf("Paragraph %d states that the trial enrolled %d patients and measured outcome %d.",
                       1:30, 100 + 1:30, 1:30), collapse = "\n\n")
  quiet(gr_segment(quiet(gr_ingest(doc)), list(method = "paragraph", max_tokens = 60)))
}

m2_price <- function() {
  gr_register_model("mock-model", context_window = 128000, max_output = 16384,
                    input_usd = 4, output_usd = 20)
  gr_register_model("cheap-model", context_window = 128000, max_output = 16384,
                    input_usd = 1, output_usd = 2)
}

test_that("the extract reader hands gr_lapply its client and the most one request can cost", {
  local_registries()
  cl <- gr_mock_client(function(m, p) '{"f01":null}')
  m2_price()
  ch <- m2_chunks()
  seen <- list()
  real <- readgpt:::gr_lapply
  local_mocked_bindings(gr_lapply = function(x, fn, ..., client = NULL, item_usd = NULL) {
    seen[[length(seen) + 1L]] <<- list(client = client, item_usd = item_usd)
    real(x, fn, ..., client = client, item_usd = item_usd)
  })
  read <- function(spec) {
    seen <<- list()
    tr <- gr_trace()
    quiet(gr_read(ch, "Fill the form", cl, spec, trace = tr))
    steps <- Filter(function(s) identical(s$label, "extract.chunk"), tr$steps)
    list(seen = seen[[1]], largest = max(vapply(steps, function(s) s$tokens$input, numeric(1))))
  }

  # Before: neither was passed, so a parallel batch was checked against the
  # call cap alone and went to workers whatever the run had spent.
  got <- read(gr_read_spec("extract", model = "mock-model", fields = m2_fields(),
                           max_chunk_tokens = 64))
  expect_identical(got$seen$client, cl)
  # The largest prompt the batch sent, with its reply at the cap.
  expect_equal(got$seen$item_usd, gr_estimate_cost("mock-model", got$largest, 64))

  # Priced at the model that receives the chunks.
  got <- read(gr_read_spec("extract", model = "mock-model", skim_model = "cheap-model",
                           fields = m2_fields(), max_chunk_tokens = 64))
  expect_equal(got$seen$item_usd, gr_estimate_cost("cheap-model", got$largest, 64))

  # No price, no figure: NA, which the spending check counts as the trace does.
  gr_register_model("free-for-all", context_window = 128000, max_output = 16384)
  got <- read(gr_read_spec("extract", model = "free-for-all", fields = m2_fields(),
                           max_chunk_tokens = 64))
  expect_true(is.na(got$seen$item_usd))
})

test_that("an extraction through an ellmer chat is priced as the chat's model", {
  local_registries()
  m2_price()
  # client_billed_model() goes by class, so a bare list stands in for the client.
  ellmer_like <- structure(list(model = "mock-model"),
                           class = c("gr_ellmer_client", "gr_backend_client", "gr_client"))
  expect_equal(readgpt:::extract_item_usd(ellmer_like, "cheap-model", 1000, 100),
               gr_estimate_cost("mock-model", 1000, 100))
  # The reply is priced at no more than the model can write.
  gr_register_model("short-model", context_window = 8000, max_output = 50,
                    input_usd = 1, output_usd = 10)
  expect_equal(readgpt:::extract_item_usd(gr_mock_client(function(m, p) "x"), "short-model",
                                          1000, 700),
               gr_estimate_cost("short-model", 1000, 50))
})

test_that("a parallel extraction stops at the spending limit where the sequential one does", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  local_registries()
  cl <- gr_mock_client(function(m, p) '{"f01":null}')
  m2_price()
  ch <- m2_chunks()
  run <- function(par, limit) {
    gr_options(max_calls = 1000, max_cost_usd = limit, workers = 2)
    tr <- gr_trace()
    # Every cap the same, so preflight's worst case prices replies no higher
    # than the reader sends them, and passes the read.
    spec <- gr_read_spec("extract", model = "mock-model", fields = m2_fields(),
                         max_chunk_tokens = 16, max_answer_tokens = 16,
                         max_summary_tokens = 16, parallel = par)
    quiet(gr_read(ch, "Fill the form", cl, spec, trace = tr))
    list(calls = tr$calls, spent = tr$spent_usd, stop = tr$budget_stop, reason = tr$stop_reason)
  }
  all_of_it <- run(FALSE, NULL)
  limit <- all_of_it$spent * 0.6
  s <- run(FALSE, limit)
  # Before: the batch went to workers whole, 15 calls and two thirds past the
  # limit where the sequential read stopped at 10.
  p <- run(TRUE, limit)
  expect_identical(p, s)
  expect_lt(p$calls, all_of_it$calls)
  expect_true(p$stop)
  expect_identical(p$reason, "cost")
})

test_that("the audit report flags unreadable citations and cut-off section replies", {
  # A section can be partial because a citation could not be read, or because a
  # reply stopped at the output cap; the report said nothing about either.
  s <- structure(list(
    sections = data.frame(section = c("Trials", "Cohorts"), brief = c("b1", "b2"),
                          text = c("t1", "t2"), n_unknown = 0L, n_unsupplied = 0L,
                          n_unparsed = c(2L, 0L), n_truncated = c(0L, 1L), n_cited = 1L,
                          stringsAsFactors = FALSE),
    citations = NULL), class = "gr_synthesis")
  html <- paste(readgpt:::audit_synthesis(s), collapse = "\n")
  expect_match(html, "2 citation(s) could not be read", fixed = TRUE)
  expect_match(html, "1 response(s) for this section were cut off at the output cap", fixed = TRUE)
  # A synthesis saved before these columns existed still renders.
  s$sections$n_unparsed <- NULL
  s$sections$n_truncated <- NULL
  expect_no_error(readgpt:::audit_synthesis(s))
})
