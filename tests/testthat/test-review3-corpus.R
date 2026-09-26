# test-review3-corpus.R -- regressions the verification of the corpus fixes
# found: a request the pipeline recovered from failing the whole document, a
# failed row losing its document_id, a path order that still followed LC_CTYPE,
# an unpriced embedding model switching off the corpus ceiling, and a store
# compatibility test that could not fail.

# ---------------------------------------------------------------------------
# corpus-1 / cache-2: a recovered failure is not a failed document
# ---------------------------------------------------------------------------

# A trace holding the given error entries, as trace_record() leaves them.
trace_with_errors <- function(...) {
  tr <- gr_trace()
  tr$errors <- list(...)
  tr
}

test_that("failed_note() leaves out trace errors marked recovered", {
  ans <- list(notes = list())
  embed_404 <- list(step = 1L, label = "embed.request", recovered = TRUE,
                    error = "Embeddings request to 'bad-embed' failed: HTTP 404")
  chat_503 <- list(step = 2L, label = "read", error = "HTTP 503: service unavailable")

  expect_null(failed_note(ans, trace_with_errors(embed_404)))
  expect_null(failed_note(ans, trace_with_errors(embed_404, embed_404)))
  # An unrecovered one still fails the document, and is the error it names.
  why <- failed_note(ans, trace_with_errors(embed_404, chat_503))
  expect_match(why, "^1 request\\(s\\) failed")
  expect_match(why, "HTTP 503", fixed = TRUE)
  expect_false(grepl("HTTP 404", why, fixed = TRUE))
  # Not recovered unless it says so.
  expect_match(failed_note(ans, trace_with_errors(within(embed_404, recovered <- FALSE))),
               "HTTP 404", fixed = TRUE)
  expect_match(failed_note(ans, trace_with_errors(within(embed_404, rm(recovered)))),
               "HTTP 404", fixed = TRUE)
})

# An embedder that fails the way the built-in "api" one does on an endpoint
# with no embeddings: the failed request is in the trace, marked recovered as
# the shared contract says (gr_embed() then falls back to lexical vectors).
register_404_embedder <- function(recovered = TRUE) {
  gr_register_embedder("gone-404", function(texts, params) {
    tr <- params$trace
    if (inherits(tr, "gr_trace")) {
      readgpt:::trace_record(tr, "embed.request", list(),
                             gr_result(FALSE, model = "bad-embed", status = 404L,
                                       error = "Embeddings request to 'bad-embed' failed: HTTP 404"),
                             params = list(model = "bad-embed"))
      if (recovered) tr$errors[[length(tr$errors)]]$recovered <- TRUE
    }
    stop("the request to 'bad-embed' did not return embeddings")
  })
  gr_options(embedder = "gone-404")
}

test_that("an embeddings failure the reader recovered from leaves the document ok and stored", {
  local_registries()
  local_clean_cache()
  register_404_embedder()
  d <- withr::local_tempdir()
  for (i in 1:2) {
    writeLines(c("## Methods",
                 "The cohort comprised 482 participants recruited across nine clinical sites.",
                 "", "## Results", paste("Adherence exceeded 91 percent, document", i)),
               file.path(d, sprintf("d%d.txt", i)))
  }
  st <- withr::local_tempdir()
  cl <- mock_echo("The cohort had 482 participants.")
  failed <- 0L
  first <- withCallingHandlers(
    suppressMessages(gr_read_many(d, "How many participants?", "needle", client = cl,
                                  store = st)),
    gr_document_failed = function(w) { failed <<- failed + 1L; invokeRestart("muffleWarning") },
    warning = function(w) invokeRestart("muffleWarning"))

  expect_identical(failed, 0L)
  expect_identical(first$summary$status, c("ok", "ok"))
  expect_identical(first$summary$answer, rep("The cohort had 482 participants.", 2L))
  expect_false(anyNA(first$summary$document_id))
  # Still degraded, and still said to be: the fallback marks the answer partial.
  expect_identical(first$summary$partial, c(TRUE, TRUE))
  expect_true(isTRUE(first$answers[[1]]$notes$embedding_fallback))
  # The failed request is still in the ledger.
  expect_true(any(vapply(first$trace$errors, function(e) isTRUE(e$recovered), logical(1))))
  expect_length(list.files(st, pattern = "\\.rds$"), 2L)

  again <- quiet(gr_read_many(d, "How many participants?", "needle", client = cl, store = st))
  expect_identical(again$summary$status, c("restored", "restored"))
})

test_that("a chat request that failed still fails the document when embeddings recovered", {
  local_registries()
  local_clean_cache()
  register_404_embedder()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("The cohort comprised 482 participants recruited across nine clinical sites.", f)
  st <- withr::local_tempdir()
  cl <- gr_mock_client(function(m, p) gr_result(FALSE, error = "HTTP 503: service unavailable"))
  out <- quiet(gr_read_many(f, "How many participants?", "needle", client = cl, store = st))
  expect_identical(out$summary$status, "failed")
  expect_match(out$summary$error, "HTTP 503", fixed = TRUE)
  expect_length(list.files(st, pattern = "\\.rds$"), 0L)
})

# ---------------------------------------------------------------------------
# corpus-7: a document read but not in full keeps its document_id
# ---------------------------------------------------------------------------

test_that("an incomplete extraction keeps its document_id in the table, evidence and summary", {
  local_registries()
  local_clean_cache()
  say3 <- function(s) paste(rep(s, 3), collapse = " ")
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(paste(c(say3("We ran a randomised controlled trial of the new treatment across sites."),
                     say3("We enrolled 120 participants in total over the recruitment window.")),
                   collapse = "\n\n"), f)
  fields <- gr_fields(design = "The study design",
                      n = gr_field("Number of participants", type = "integer"))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("enrolled", messages[[length(messages)]]$content, fixed = TRUE)) {
      stop("HTTP 429 rate limited")
    }
    paste0('{"design":"randomised controlled trial","n":null,"design__quote":',
           '"We ran a randomised controlled trial of the new treatment across sites.",',
           '"n__quote":null}')
  })
  st <- withr::local_tempdir()
  x <- quiet(gr_extract(f, fields, client = cl, recipe = "thorough", max_tokens = 48, store = st))
  ok <- quiet(gr_extract(f, fields, client = mock_echo(), recipe = "thorough", max_tokens = 48))
  id <- ok$summary$document_id
  expect_false(is.na(id))

  expect_identical(x$table$status, "incomplete")
  expect_identical(x$table$design, "randomised controlled trial")
  expect_identical(x$table$document_id, id)
  expect_identical(unique(x$evidence$document_id), id)
  expect_identical(x$summary$document_id, id)
  # How far the read got is kept too; the answer itself stays in `answers`.
  expect_identical(x$summary$reader, "extract")
  expect_identical(x$summary$chunks, ok$summary$chunks)
  expect_true(x$summary$partial)
  expect_true(is.na(x$summary$answer))
  # And it is still a failure: not stored.
  expect_identical(x$summary$status, "failed")
  expect_length(list.files(st, pattern = "\\.rds$"), 0L)
})

test_that("a screening call that failed keeps the document_id", {
  local_registries()
  local_clean_cache()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("We ran a randomised controlled trial of adults with asthma, enrolling 120.", f)
  down <- gr_mock_client(function(m, p) stop("HTTP 503 unavailable"))
  up <- gr_mock_client(function(m, p) {
    '{"decision":"include","reason":"An RCT.","criterion":null,"quote":null}'
  })
  s <- quiet(gr_screen(f, question = "Does it work?", include = "Randomised trials", client = down))
  good <- quiet(gr_screen(f, question = "Does it work?", include = "Randomised trials", client = up))
  expect_identical(s$table$status, "failed")
  expect_true(is.na(s$table$decision))
  expect_false(is.na(good$table$document_id))
  expect_identical(s$table$document_id, good$table$document_id)
})

test_that("a document a limit stopped keeps its document_id, and a copy of it is read again", {
  local_registries()
  local_clean_cache()
  gr_register_reader("greedy3", signature = "some|N|none", cost_calls = "N",
    fn = function(chunks, question, client, spec, trace) {
      n <- 0L
      while (readgpt:::trace_can_call(trace) && n < 50L) {
        gr_call(client, list(list(role = "user", content = "more")), model = spec$model,
                trace = trace, label = "greedy")
        n <- n + 1L
      }
      new_answer("done", "greedy3", question, chunks$chunks$chunk_id, trace)
    })
  gr_options(max_calls = 3)
  txt <- "Revenue was 45.2 million dollars in the year under review."
  a <- withr::local_tempfile(fileext = ".txt"); writeLines(txt, a)
  b <- withr::local_tempfile(fileext = ".txt"); writeLines(txt, b)
  st <- withr::local_tempdir()
  out <- quiet(gr_read_many(c(a, b), "Q?", "fast", client = mock_echo(), store = st,
                            reader = "greedy3"))
  expect_identical(out$summary$status, c("failed", "failed"))
  expect_match(out$summary$error[1], "3-request limit", fixed = TRUE)
  expect_false(anyNA(out$summary$document_id))
  expect_identical(out$summary$document_id[1], out$summary$document_id[2])
  # Neither a duplicate of the other: an unfinished read anchors nothing.
  expect_true(all(is.na(out$summary$duplicate_of)))
  expect_length(list.files(st, pattern = "\\.rds$"), 0L)
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-02: one path order under a C LC_CTYPE too
# ---------------------------------------------------------------------------

test_that("a folder's documents come out in the same order under a C character type", {
  names <- c("adams.txt", "Baker.txt", "Évora.txt", "zhou.txt", "x.zzz", "A.zzz")
  d <- withr::local_tempdir()
  made <- vapply(names, function(nm) {
    isTRUE(tryCatch({ writeLines("A document.", file.path(d, nm)); TRUE },
                    error = function(e) FALSE, warning = function(w) FALSE))
  }, logical(1))
  skip_if_not(all(made), "this file system cannot hold the non-ASCII file name")
  listed <- function() {
    s <- quiet(corpus_sources(d))
    # Compared as bytes: printing or matching them depends on the locale too.
    list(read = lapply(basename(s), charToRaw),
         skipped = lapply(basename(attr(s, "skipped")), charToRaw))
  }
  here <- listed()
  expect_identical(here$read, lapply(c("Baker.txt", "adams.txt", "zhou.txt", "Évora.txt"),
                                     charToRaw))

  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!nzchar(old), "no LC_CTYPE to restore")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", old)))
  old_coll <- Sys.getlocale("LC_COLLATE")
  withr::defer(suppressWarnings(Sys.setlocale("LC_COLLATE", old_coll)))
  for (loc in c("C", "POSIX")) {
    ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
    skip_if(!nzchar(ok), "cannot switch to the C locale here")
    suppressWarnings(Sys.setlocale("LC_COLLATE", loc))
    # list.files() now returns the names marked "unknown", and enc2utf8() on
    # them wrote the accented one as "<c3><89>vora.txt", which sorts first.
    expect_identical(listed(), here, info = loc)
  }
})

# ---------------------------------------------------------------------------
# cache-3: an embedding model with no price does not switch off max_total_usd
# ---------------------------------------------------------------------------

# Records each embeddings request as the built-in embedder does, priced by the
# embedding model on the step, and embeds lexically.
register_recording_embedder <- function(model) {
  gr_register_embedder("recorded", function(texts, params) {
    readgpt:::trace_record(params$trace, "embed.request", list(),
                           gr_result(TRUE, model = model,
                                     usage = list(input = 7L * length(texts), output = 0L)),
                           params = list(model = model, texts = length(texts)))
    readgpt:::lexical_embed(texts)
  })
  gr_options(embedder = "recorded")
}

corpus_cost_docs <- function(env = parent.frame()) {
  vapply(1:4, function(i) {
    p <- withr::local_tempfile(fileext = ".txt", .local_envir = env)
    writeLines(c(sprintf("Revenue in region %d was %d million dollars.", i, 40 + i), "",
                 "Costs rose over the year."), p)
    p
  }, character(1))
}

test_that("an unpriced embedding model leaves max_total_usd enforced on the priced spend", {
  local_registries()
  local_clean_cache()
  gr_options(max_cost_usd = NULL)
  gr_register_model("priced-chat", context_window = 100000L, max_output = 4096L,
                    input_usd = 1000, output_usd = 1000)
  gr_register_model("unpriced-embed", context_window = 8191L, max_output = 0L,
                    kind = "embedding")
  register_recording_embedder("unpriced-embed")
  cl <- gr_backend_client(function(m, p) "Revenue was 41 million.", model = "priced-chat")
  docs <- corpus_cost_docs()

  seen <- list()
  out <- withCallingHandlers(
    suppressMessages(gr_read_many(docs, "What was revenue?", "needle", client = cl,
                                  model = "priced-chat", max_total_usd = 1e-6)),
    warning = function(w) {
      seen[[length(seen) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
  classes <- vapply(seen, function(w) class(w)[1], character(1))
  expect_true("gr_corpus_cost_cap" %in% classes)
  expect_false("gr_corpus_cost_unknown" %in% classes)
  floor_w <- seen[classes == "gr_corpus_cost_floor"]
  expect_length(floor_w, 1L)
  msg <- conditionMessage(floor_w[[1]])
  expect_match(msg, "'unpriced-embed'", fixed = TRUE)
  expect_match(msg, "gr_register_model('unpriced-embed'", fixed = TRUE)
  expect_identical(out$summary$status, c("ok", "skipped", "skipped", "skipped"))
  # The row's cost is still unknown, not the priced part passed off as the whole.
  expect_true(is.na(out$summary$cost_usd[1]))
  cost <- gr_trace_cost(out$trace)
  expect_true(is.na(cost$usd[cost$model == "unpriced-embed"]))
  expect_gt(cost$usd[cost$model == "priced-chat"], 0)
})

test_that("an unpriced chat model still makes max_total_usd unenforceable, and says which", {
  local_registries()
  local_clean_cache()
  gr_options(max_cost_usd = NULL)
  gr_register_model("unpriced-chat", context_window = 100000L, max_output = 4096L)
  register_recording_embedder("text-embedding-3-small")
  cl <- gr_backend_client(function(m, p) "Revenue was 41 million.", model = "unpriced-chat")
  docs <- corpus_cost_docs()
  seen <- list()
  out <- withCallingHandlers(
    suppressMessages(gr_read_many(docs, "What was revenue?", "needle", client = cl,
                                  model = "unpriced-chat", max_total_usd = 1e-6)),
    warning = function(w) {
      seen[[length(seen) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
  classes <- vapply(seen, function(w) class(w)[1], character(1))
  expect_identical(sum(classes == "gr_corpus_cost_unknown"), 1L)
  expect_false(any(c("gr_corpus_cost_floor", "gr_corpus_cost_cap") %in% classes))
  expect_match(conditionMessage(seen[[which(classes == "gr_corpus_cost_unknown")]]),
               "'unpriced-chat'", fixed = TRUE)
  expect_identical(out$summary$status, rep("ok", 4L))
  expect_true(all(is.na(out$summary$cost_usd)))
})

test_that("corpus_cost() splits unpriced embedding models from the rest", {
  local_registries()
  gr_register_model("priced-chat", context_window = 100000L, max_output = 4096L,
                    input_usd = 1000, output_usd = 1000)
  gr_register_model("unpriced-embed", context_window = 8191L, max_output = 0L,
                    kind = "embedding")
  tr <- gr_trace()
  rec <- function(label, model, input, output = 0L) {
    readgpt:::trace_record(tr, label, list(),
                           gr_result(TRUE, model = model, usage = list(input = input,
                                                                        output = output)),
                           params = list(model = model))
  }
  rec("read", "priced-chat", 1000L, 100L)
  rec("embed.request", "unpriced-embed", 50L)
  cc <- corpus_cost(tr)
  expect_true(is.na(cc$usd))
  expect_equal(cc$priced, gr_estimate_cost("priced-chat", 1000L, 100L))
  expect_identical(cc$unpriced_embed, "unpriced-embed")
  expect_identical(cc$unpriced, character(0))
  # The same model used for a chat request is not an embedding-only one.
  rec("read", "unpriced-embed", 10L, 10L)
  cc <- corpus_cost(tr)
  expect_identical(cc$unpriced_embed, character(0))
  expect_identical(cc$unpriced, "unpriced-embed")
})

# ---------------------------------------------------------------------------
# corpus-9 / cross-7: a store written by 0.5.0 is read again once, as NEWS says
# ---------------------------------------------------------------------------

test_that("a store entry written by 0.5.0 is read again once, then restored", {
  local_registries()
  local_clean_cache()
  dir <- withr::local_tempdir()
  doc <- file.path(dir, "a.txt")
  writeLines("Revenue was 45.2 million dollars.", doc)
  st <- file.path(dir, "store"); dir.create(st)
  calls <- 0L
  cl <- gr_backend_client(function(m, p) { calls <<- calls + 1L; "Revenue was 45.2 million." },
                          id = "review3-compat")
  rec <- as_recipe("fast")
  # The key exactly as 0.5.0 built it: "readgpt-corpus-v2", without extra_body
  # or the embedding configuration.
  info <- file.info(doc)
  v2 <- gr_hash(list("readgpt-corpus-v2",
                     list("file", normalizePath(doc, winslash = "/", mustWork = FALSE),
                          info$size, format(info$mtime)),
                     key_text("Q?"), as_chr1(gr_options("tokenizer"), "?"),
                     unclass(rec$ingest), unclass(rec$segment), unclass(rec$read),
                     as_chr1(cl$model, "?"), as_chr1(cl$api, "?"), as_chr1(cl$base_url, "?"),
                     as_chr1(cl$.client_id, "<url-addressed>")))
  expect_false(identical(v2, corpus_key(doc, "Q?", rec, cl)))
  # A format-1 entry, as 0.5.0 wrote it, holding an answer this run would not give.
  old_row <- corpus_row("a.txt", status = "ok")
  old_row$answer <- "An answer from 0.5.0."
  saveRDS(list(format = 1L, key = v2, created = Sys.time(), row = old_row, answer = NULL,
               doc_hash = NULL),
          corpus_store_path(st, v2))

  first <- quiet(gr_read_many(doc, "Q?", "fast", client = cl, store = st))
  expect_identical(first$summary$status, "ok")
  expect_identical(first$summary$answer, "Revenue was 45.2 million.")
  expect_gt(calls, 0L)
  expect_length(list.files(st, pattern = "\\.rds$"), 2L)      # beside the old one

  calls <- 0L
  second <- quiet(gr_read_many(doc, "Q?", "fast", client = cl, store = st))
  expect_identical(second$summary$status, "restored")
  expect_identical(calls, 0L)
})
