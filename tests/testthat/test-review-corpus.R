# test-review-corpus.R -- regressions from the review of the corpus loop and the
# screening reader: a failed request recorded as a judgement and stored as a
# result, a store key that ignored part of the configuration, a store that ran
# whatever its files contained, and a corpus order that followed the locale.

review_dir <- function(files, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  for (nm in names(files)) writeLines(files[[nm]], file.path(d, nm))
  d
}

review_docs <- function(env = parent.frame()) {
  review_dir(list(
    a.txt = "A randomised trial of 400 adults with hypertension found lower blood pressure.",
    b.txt = "A survey of 60 teenagers about sleep habits."), env = env)
}

# One client, down for as long as `state$down` is TRUE and answering properly
# after. The same object across runs, so the store key is the same and only the
# outage differs.
outage_client <- function(state) {
  gr_mock_client(function(messages, params) {
    state$calls <- state$calls + 1L
    if (isTRUE(state$down)) stop("HTTP 503: service unavailable")
    sys <- as.character(messages[[1]]$content)
    seen <- paste(vapply(messages, function(m) as.character(m$content), ""), collapse = " ")
    if (grepl("decision", sys, fixed = TRUE)) {
      if (grepl("randomised trial of 400", seen, fixed = TRUE)) {
        return('{"decision":"include","reason":"An RCT in adults.","criterion":null,"quote":null}')
      }
      return('{"decision":"exclude","reason":"Not a trial.","criterion":null,"quote":null}')
    }
    if (identical(params$schema_name, "extraction")) return('{"n":400,"n__quote":null}')
    "The sample size was 400."
  })
}

# ---------------------------------------------------------------------------
# r-semantics-09: a failed screening call is not a decision
# ---------------------------------------------------------------------------

test_that("a screening call that failed leaves no decision, and the run says so", {
  d <- review_docs()
  cl <- gr_mock_client(function(m, p) gr_result(FALSE, error = "HTTP 500 after 3 retries"))
  s <- quiet(gr_screen(d, question = "Does treatment lower blood pressure?",
                       include = "A randomised trial", client = cl, keep_answers = TRUE))
  expect_true(all(is.na(s$table$decision)))
  expect_identical(s$table$status, c("failed", "failed"))
  expect_true(all(grepl("HTTP 500 after 3 retries", s$table$error, fixed = TRUE)))
  expect_length(s$included, 0L)

  # The answer itself records no decision either, and says why it is partial.
  a <- s$answers[[1]]
  expect_true(is.na(a$notes$decision))
  expect_true(isTRUE(a$notes$failed_call))
  expect_true(a$partial)
  expect_match(paste(partial_reasons(a), collapse = "; "), "HTTP 500")

  # The flow diagram counts them as unread, not as the model deferring.
  fl <- gr_flow(s)
  expect_identical(fl$n[fl$stage == "  unclear"], 0L)
  expect_identical(fl$n[fl$stage == "  could not be read"], 2L)
  expect_output(print(s), "could not be read and have NO decision")
})

test_that("a failed screening call is not a model deferral in a calibration", {
  d <- review_docs()
  cl <- gr_mock_client(function(m, p) stop("HTTP 500 server error"))
  s <- quiet(gr_screen(d, question = "Q?", include = "A randomised trial", client = cl))
  ref <- data.frame(document = s$table$document, human_decision = "include",
                    stringsAsFactors = FALSE)
  # Before, both rows were "unclear" and counted as kept: 100% sensitivity, all
  # of it deferred to a person, from a run in which no model judged anything.
  expect_error(quiet(gr_calibrate(s, ref, of = "all")), class = "gr_bad_reference")
})

test_that("a real 'unclear' is still a decision, and a new label still reads as one", {
  d <- review_docs()
  cl <- gr_mock_client(function(m, p) {
    '{"decision":"maybe","reason":"Hard to say.","criterion":null,"quote":null}'
  })
  s <- quiet(gr_screen(d, question = "Q?", include = "A randomised trial", client = cl))
  expect_identical(s$table$decision, c("unclear", "unclear"))
  expect_identical(s$table$status, c("ok", "ok"))
})

test_that("a screening quote is checked against source_text, not a model-written header", {
  # The shared chunk contract: `source_text` holds the document's own words
  # where `text` carries something a model wrote; NA means `text` is the source.
  ch <- new_chunks(c("[This excerpt reports a randomised trial.]\n\nWe surveyed 60 teenagers about sleep.",
                     "Nothing else is reported."), "contextual", list(max_tokens = 500))
  ch$chunks$source_text <- c("We surveyed 60 teenagers about sleep.", NA)
  quoting <- function(q) gr_mock_client(function(m, p) {
    sprintf('{"decision":"include","reason":"r","criterion":null,"quote":"%s"}', q)
  })
  spec <- list(reader = "screen", include = "A randomised trial")
  verified <- function(q) quiet(gr_read(ch, "Q?", quoting(q), spec))$evidence$verified
  expect_false(verified("This excerpt reports a randomised trial."))   # the header
  expect_true(verified("We surveyed 60 teenagers about sleep."))       # the document
  expect_true(verified("Nothing else is reported."))                   # NA: text is source
})

# ---------------------------------------------------------------------------
# tests-02: a document whose requests failed is not stored
# ---------------------------------------------------------------------------

test_that("a document whose calls failed is not stored, so the next run reads it", {
  local_registries()
  local_clean_cache()
  d <- review_docs()
  st <- withr::local_tempdir()
  state <- new.env(); state$down <- TRUE; state$calls <- 0L
  cl <- outage_client(state)
  q <- "What was the sample size?"

  first <- quiet(gr_read_many(d, q, "fast", client = cl, store = st))
  expect_identical(first$summary$status, c("failed", "failed"))
  expect_true(all(grepl("HTTP 503", first$summary$error, fixed = TRUE)))
  expect_true(all(is.na(first$summary$answer)))
  expect_length(list.files(st, pattern = "\\.rds$"), 0L)
  # The partial answer is still there to look at, as for a limit-stopped one.
  expect_true(first$answers[[1]]$partial)
  said <- character(0)
  withCallingHandlers(suppressMessages(gr_read_many(d, q, "fast", client = cl)),
                      gr_document_failed = function(w) {
                        said <<- c(said, conditionMessage(w))
                        invokeRestart("muffleWarning")
                      })
  expect_length(said, 2L)
  expect_match(said, "request\\(s\\) failed")

  state$down <- FALSE; state$calls <- 0L
  second <- quiet(gr_read_many(d, q, "fast", client = cl, store = st))
  expect_identical(second$summary$status, c("ok", "ok"))
  expect_identical(second$summary$answer, rep("The sample size was 400.", 2L))
  expect_gt(state$calls, 0L)
  expect_length(list.files(st, pattern = "\\.rds$"), 2L)

  # And once read, it is restored as usual.
  state$calls <- 0L
  third <- quiet(gr_read_many(d, q, "fast", client = cl, store = st))
  expect_identical(third$summary$status, c("restored", "restored"))
  expect_identical(state$calls, 0L)
})

test_that("one failed document among good ones: only the good ones are stored", {
  local_registries()
  local_clean_cache()
  d <- review_docs()
  st <- withr::local_tempdir()
  flaky <- new.env(); flaky$n <- 0L
  cl <- gr_mock_client(function(m, p) {
    flaky$n <- flaky$n + 1L
    seen <- paste(vapply(m, function(x) as.character(x$content), ""), collapse = " ")
    if (flaky$n == 1L && grepl("randomised trial of 400", seen, fixed = TRUE)) {
      return(gr_result(FALSE, error = "HTTP 429: rate limited"))
    }
    "The sample size was 400."
  })
  first <- quiet(gr_read_many(d, "Q?", "fast", client = cl, store = st))
  expect_identical(first$summary$status, c("failed", "ok"))
  expect_length(list.files(st, pattern = "\\.rds$"), 1L)

  again <- quiet(gr_read_many(d, "Q?", "fast", client = cl, store = st))
  expect_identical(again$summary$status, c("ok", "restored"))
  expect_identical(again$summary$answer[1], "The sample size was 400.")
})

test_that("a screening run during an outage is screened again when it is over", {
  local_registries()
  local_clean_cache()
  d <- review_docs()
  st <- withr::local_tempdir()
  state <- new.env(); state$down <- TRUE; state$calls <- 0L
  cl <- outage_client(state)
  run <- function() quiet(gr_screen(d, question = "Does treatment lower blood pressure?",
                                    include = "A randomised trial in adults",
                                    client = cl, store = st))
  s1 <- run()
  expect_true(all(is.na(s1$table$decision)))
  expect_identical(s1$table$status, c("failed", "failed"))

  state$down <- FALSE; state$calls <- 0L
  s2 <- run()
  expect_identical(s2$table$decision, c("include", "exclude"))
  expect_identical(s2$table$status, c("ok", "ok"))
  expect_identical(state$calls, 2L)
})

test_that("an extraction during an outage is not restored as empty records", {
  local_registries()
  local_clean_cache()
  d <- review_docs()
  st <- withr::local_tempdir()
  state <- new.env(); state$down <- TRUE; state$calls <- 0L
  cl <- outage_client(state)
  f <- gr_fields(n = gr_field("Sample size", type = "integer"))
  e1 <- quiet(gr_extract(d, f, client = cl, store = st))
  expect_identical(e1$table$status, c("failed", "failed"))
  expect_length(list.files(st, pattern = "\\.rds$"), 0L)

  state$down <- FALSE
  e2 <- quiet(gr_extract(d, f, client = cl, store = st))
  expect_identical(e2$table$status, c("ok", "ok"))
  expect_identical(e2$table$n, c(400L, 400L))
})

# ---------------------------------------------------------------------------
# state-concurrency-06: the store key covers the whole configuration
# ---------------------------------------------------------------------------

test_that("the store key changes with extra_body, the embedding model and the embedder", {
  local_registries()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("Revenue in 2023 was 12 million dollars.", f)
  mk <- function(...) gr_client(model = "gpt-4o-mini", api = "chat",
                                base_url = "http://127.0.0.1:9/v1", api_key = "sk-x", ...)
  key <- function(cl, recipe = "fast") corpus_key(f, "q", as_recipe(recipe), cl)

  lo <- mk(extra_body = list(reasoning = list(effort = "low")))
  hi <- mk(extra_body = list(reasoning = list(effort = "high")))
  expect_false(identical(key(lo), key(hi)))
  expect_identical(key(lo), key(mk(extra_body = list(reasoning = list(effort = "low")))))

  small <- mk(embedding_model = "text-embedding-3-small")
  large <- mk(embedding_model = "text-embedding-3-large")
  expect_false(identical(key(small, "needle"), key(large, "needle")))

  before <- key(small, "needle")
  gr_options(embedder = "lexical")
  expect_false(identical(before, key(small, "needle")))

  # An embedder option naming nothing registered is the run's error to raise,
  # not the key's.
  gr_options(embedder = "no-such-embedder")
  expect_no_error(key(small, "needle"))
})

test_that("a rerun with a different extra_body is read again, not restored", {
  local_registries()
  local_clean_cache()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("Revenue in 2023 was 12 million dollars.", f)
  st <- withr::local_tempdir()
  # The effort is read from the request's extra_body, as a provider would.
  cl <- function(effort) {
    b <- gr_backend_client(function(messages, params) {
      sprintf("Answer produced at effort=%s.", effort)
    }, id = "review-stable-backend")
    b$extra_body <- list(reasoning = list(effort = effort))
    b
  }
  lo <- quiet(gr_read_many(f, "What was revenue?", "fast", client = cl("low"), store = st))
  hi <- quiet(gr_read_many(f, "What was revenue?", "fast", client = cl("high"), store = st))
  expect_identical(lo$summary$answer, "Answer produced at effort=low.")
  expect_identical(hi$summary$status, "ok")
  expect_identical(hi$summary$answer, "Answer produced at effort=high.")
  expect_identical(hi$trace$calls, 1L)
})

# ---------------------------------------------------------------------------
# security-01: a store entry is data, never code
# ---------------------------------------------------------------------------

store_fixture <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  doc <- file.path(dir, "a.txt")
  writeLines("Revenue was 45.2 million dollars.", doc)
  st <- file.path(dir, "store"); dir.create(st)
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million.")
  list(doc = doc, store = st, client = cl, marker = file.path(dir, "MARKER"),
       key = corpus_key(doc, "Q?", as_recipe("fast"), cl))
}

plant <- function(fx, entry) saveRDS(entry, corpus_store_path(fx$store, fx$key))

test_that("a planted trace with an active binding is refused, and nothing runs", {
  fx <- store_fixture()
  marker <- fx$marker
  evil <- new.env(parent = emptyenv())
  makeActiveBinding("errors", function(v) {
    cat("ran\n", file = marker, append = TRUE); list()
  }, evil)
  class(evil) <- "gr_trace"
  ans <- structure(list(answer = "Planted.", reader = "stuff", question = "Q?",
                        evidence = NULL, chunks_used = 1L, partial = TRUE, notes = list(),
                        trace = evil, warnings = character(0)), class = "gr_answer")
  for (fmt in c(1L, 2L)) {
    plant(fx, list(format = fmt, key = fx$key, created = Sys.time(),
                   row = corpus_row("a.txt", status = "ok"), answer = ans))
    out <- quiet(gr_read_many(fx$doc, "Q?", "fast", client = fx$client, store = fx$store))
    expect_identical(out$summary$status, "ok")            # read again, not restored
    expect_identical(out$summary$answer, "Revenue was 45.2 million.")
    invisible(partial_reasons(out$answers[[1]]))
    invisible(utils::capture.output(print(out$answers[[1]]$trace)))
    expect_false(file.exists(marker))
  }
})

test_that("a planted row that is not a data frame is a miss, not an error", {
  fx <- store_fixture()
  marker <- fx$marker
  evil <- new.env(parent = emptyenv())
  makeActiveBinding("document", function(v) {
    cat("ran\n", file = marker, append = TRUE); "x"
  }, evil)
  class(evil) <- "data.frame"
  plant(fx, list(format = 1L, key = fx$key, created = Sys.time(), row = evil, answer = NULL))
  expect_no_error(out <- quiet(gr_read_many(fx$doc, "Q?", "fast", client = fx$client,
                                            store = fx$store)))
  expect_identical(out$summary$status, "ok")
  expect_false(file.exists(marker))
})

test_that("a promise planted in a format-1 trace is refused without being forced", {
  fx <- store_fixture()
  marker <- fx$marker
  real <- quiet(gr_read_many(fx$doc, "Q?", "fast", client = fx$client))
  ans <- real$answers[[1]]
  evil <- new.env(parent = emptyenv())
  for (nm in setdiff(ls(ans$trace, all.names = TRUE), "errors")) {
    assign(nm, get(nm, envir = ans$trace), envir = evil)
  }
  delayedAssign("errors", { cat("forced\n", file = marker, append = TRUE); list() },
                assign.env = evil)
  class(evil) <- "gr_trace"
  ans$trace <- evil
  plant(fx, list(format = 1L, key = fx$key, created = Sys.time(), row = real$summary,
                 answer = ans))
  out <- quiet(gr_read_many(fx$doc, "Q?", "fast", client = fx$client, store = fx$store))
  expect_identical(out$summary$status, "ok")
  invisible(partial_reasons(out$answers[[1]]))
  expect_false(file.exists(marker))
})

test_that("a function or a call anywhere in an entry makes it a miss", {
  fx <- store_fixture()
  real <- quiet(gr_read_many(fx$doc, "Q?", "fast", client = fx$client))
  ans <- real$answers[[1]]
  ans$trace <- NULL
  bad <- list(function() "x", quote(system("true")), as.name("x"), globalenv())
  for (fmt in c(1L, 2L)) {
    for (b in bad) {
      a <- ans; a$notes$payload <- b
      plant(fx, list(format = fmt, key = fx$key, created = Sys.time(), row = real$summary,
                     answer = a))
      out <- quiet(gr_read_many(fx$doc, "Q?", "fast", client = fx$client, store = fx$store))
      expect_identical(out$summary$status, "ok")
    }
    # Hidden in an attribute is no better than in a field.
    a <- ans; attr(a$evidence, "hook") <- function() "x"
    plant(fx, list(format = fmt, key = fx$key, created = Sys.time(), row = real$summary,
                   answer = a))
    out <- quiet(gr_read_many(fx$doc, "Q?", "fast", client = fx$client, store = fx$store))
    expect_identical(out$summary$status, "ok")
  }
})

test_that("a store written before this change still restores, trace and all", {
  fx <- store_fixture()
  real <- quiet(gr_read_many(fx$doc, "Q?", "fast", client = fx$client))
  # Format 1 kept the answer's trace as the environment it is in memory.
  expect_true(is.environment(real$answers[[1]]$trace))
  plant(fx, list(format = 1L, key = fx$key, created = Sys.time(), row = real$summary,
                 answer = real$answers[[1]], doc_hash = "a-hash"))
  out <- quiet(gr_read_many(fx$doc, "Q?", "fast", client = fx$client, store = fx$store))
  expect_identical(out$summary$status, "restored")
  expect_identical(out$summary$answer, "Revenue was 45.2 million.")
  tr <- out$answers[[1]]$trace
  expect_s3_class(tr, "gr_trace")
  expect_identical(tr$calls, real$answers[[1]]$trace$calls)
  expect_identical(length(tr$steps), length(real$answers[[1]]$trace$steps))
})

test_that("what the store writes now is plain data, and restores for every recipe", {
  local_registries()
  local_clean_cache()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(sample_doc(2, 2), f)
  st <- withr::local_tempdir()
  cl <- mock_echo("The cohort comprised 482 participants [chunk 1].")
  for (r in names(gr_recipes())) {
    first <- quiet(gr_read_many(f, "How many participants?", r, client = cl, store = st))
    again <- quiet(gr_read_many(f, "How many participants?", r, client = cl, store = st))
    expect_identical(again$summary$status, "restored", info = r)
    expect_identical(again$summary$answer, first$summary$answer, info = r)
    tr <- again$answers[[1]]$trace
    expect_s3_class(tr, "gr_trace")
    expect_identical(tr$calls, first$answers[[1]]$trace$calls, info = r)
    expect_identical(as.data.frame(tr)$stage, as.data.frame(first$answers[[1]]$trace)$stage,
                     info = r)
  }
  for (p in list.files(st, pattern = "\\.rds$", full.names = TRUE)) {
    entry <- readRDS(p)
    expect_identical(entry$format, 2L)
    expect_true(corpus_is_plain(entry), info = basename(p))
  }
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-02: one corpus order on every machine
# ---------------------------------------------------------------------------

test_that("a folder's documents come out in the same order under any collation", {
  names <- c("adams.txt", "Baker.txt", "Évora.txt", "zhou.txt", "x.zzz", "A.zzz")
  d <- withr::local_tempdir()
  made <- vapply(names, function(nm) {
    isTRUE(tryCatch({ writeLines("A document.", file.path(d, nm)); TRUE },
                    error = function(e) FALSE, warning = function(w) FALSE))
  }, logical(1))
  skip_if_not(all(made), "this file system cannot hold the non-ASCII file name")

  under <- function(collate) {
    withr::local_collate(collate)
    s <- quiet(corpus_sources(d))
    list(read = basename(s), skipped = basename(attr(s, "skipped")))
  }
  c_order <- under("C")
  expect_identical(c_order$read, c("Baker.txt", "adams.txt", "zhou.txt", "Évora.txt"))
  expect_identical(c_order$skipped, c("A.zzz", "x.zzz"))
  expect_identical(under(Sys.getlocale("LC_COLLATE")), c_order)
  ok <- suppressWarnings(tryCatch(nzchar(withr::with_collate("en_US.UTF-8",
                                                             Sys.getlocale("LC_COLLATE"))),
                                  error = function(e) FALSE))
  skip_if_not(isTRUE(ok), "no en_US.UTF-8 locale to compare with")
  # Under en_US, sort() gave adams, Baker, Evora, zhou.
  expect_identical(under("en_US.UTF-8"), c_order)
})
