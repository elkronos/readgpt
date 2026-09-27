# test-review7-ingest-corpus.R -- the seventh pass on ingest.R, ingest-url.R
# and corpus.R: handoffs from the fixers of the sixth pass, for changes that
# fell in these files.
#
# Each block names the finding and says what the old behaviour was.

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-01 (client H3): gr_read_many() kept the
# question as it came. One read from a file without an encoding is marked
# "unknown", and in a C locale (cron, a minimal container) the corpus trace
# then wrote it to JSON as "caf<c3><a9>", as did the per-document traces and
# the answers the question was pasted into.
# ---------------------------------------------------------------------------

test_that("an unlabelled UTF-8 question survives a corpus run under a C locale", {
  local_clean_cache()
  q <- unmarked("Quel est le prix du caf\u00e9 ?")
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(enc2utf8("Le caf\u00e9 co\u00fbte trois euros. Revenue was 45 million."), f,
             useBytes = TRUE)
  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!nzchar(old), "no LC_CTYPE to restore")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", old)))
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(ok), "cannot switch to the C locale here")
  out <- quiet(gr_read_many(f, q, "fast", client = gr_mock_client(function(m, p) "Trois euros.")))
  expect_identical(out$summary$status, "ok")
  bytes <- charToRaw(enc2utf8("caf\u00e9"))
  has_cafe <- function(s) grepl(rawToChar(bytes), s, fixed = TRUE, useBytes = TRUE)
  escaped <- function(s) grepl("<c3><a9>", s, fixed = TRUE, useBytes = TRUE)
  j <- as_json(out$trace)
  expect_false(escaped(j))
  expect_true(has_cafe(j))
  expect_identical(Encoding(out$trace$meta$question), "UTF-8")
  a <- out$answers[[1]]
  expect_false(escaped(as_json(a$trace)))
  expect_identical(Encoding(a$question), "UTF-8")
})

# ---------------------------------------------------------------------------
# cache-trace-05 (optional): a corpus stopped by max_total_usd said nothing of
# it on its trace, so a saved corpus trace read as a run that finished, and a
# replay of it counted nothing against the ceiling (replayed calls cost
# nothing now): it went past the document the run stopped at and asked for
# calls the recording never made.
# ---------------------------------------------------------------------------

r7_cost_docs <- function(n = 3L, env = parent.frame()) {
  vapply(seq_len(n), function(i) {
    p <- withr::local_tempfile(fileext = ".txt", .local_envir = env)
    writeLines(c(sprintf("Revenue in region %d was %d million dollars.", i, 40 + i), "",
                 "Costs rose over the year."), p)
    p
  }, character(1))
}

test_that("a corpus the cost ceiling stopped says so, and replays to the same stop", {
  local_registries()
  local_clean_cache()
  gr_options(max_cost_usd = NULL)
  gr_register_model("r7-priced", context_window = 100000L, max_output = 4096L,
                    input_usd = 1000, output_usd = 1000)
  cl <- gr_backend_client(function(m, p) "Revenue was 41 million.", model = "r7-priced",
                          id = "r7-priced-backend")
  docs <- r7_cost_docs()
  out <- quiet(gr_read_many(docs, "What was revenue?", "fast", client = cl,
                            max_total_usd = 1e-6))
  expect_identical(out$summary$status, c("ok", "skipped", "skipped"))
  expect_true(isTRUE(out$trace$budget_stop))
  expect_identical(out$trace$stop_reason, "cost")

  f <- withr::local_tempfile(fileext = ".json")
  gr_trace_save(out$trace, f)
  seen <- list()
  again <- withCallingHandlers(
    suppressMessages(gr_read_many(docs, "What was revenue?", "fast",
                                  client = gr_replay_client(f), max_total_usd = 1e-6)),
    warning = function(w) {
      seen[[length(seen) + 1L]] <<- w
      invokeRestart("muffleWarning")
    })
  classes <- vapply(seen, function(w) class(w)[1], character(1))
  expect_identical(again$summary$status, c("ok", "skipped", "skipped"))
  expect_identical(again$summary$answer[1], out$summary$answer[1])
  expect_false("gr_document_failed" %in% classes)
  expect_true("gr_corpus_cost_cap" %in% classes)
  # Nothing was paid for in the replay, and the run does not say it was.
  expect_identical(sum(gr_trace_cost(again$trace)$usd), 0)
  cap <- conditionMessage(seen[[which(classes == "gr_corpus_cost_cap")]])
  expect_match(cap, "recorded", fixed = TRUE)
})

test_that("a corpus that ends at the ceiling, or never reaches it, is not marked stopped", {
  local_registries()
  local_clean_cache()
  gr_options(max_cost_usd = NULL)
  gr_register_model("r7-priced", context_window = 100000L, max_output = 4096L,
                    input_usd = 1000, output_usd = 1000)
  cl <- gr_backend_client(function(m, p) "Revenue was 41 million.", model = "r7-priced",
                          id = "r7-priced-backend-2")
  one <- r7_cost_docs(1L)
  out <- quiet(gr_read_many(one, "What was revenue?", "fast", client = cl, max_total_usd = 1e-6))
  expect_identical(out$summary$status, "ok")
  expect_false(isTRUE(out$trace$budget_stop))
  roomy <- quiet(gr_read_many(r7_cost_docs(2L), "What was revenue?", "fast", client = cl,
                              max_total_usd = 1e6))
  expect_identical(roomy$summary$status, c("ok", "ok"))
  expect_false(isTRUE(roomy$trace$budget_stop))
})

# ---------------------------------------------------------------------------
# r2-export-serialization-fidelity-06: as_json() of a document with one
# cleaning step or one unread page wrote a string or a number where every
# other document has an array.
# ---------------------------------------------------------------------------

test_that("a document's cleaning steps and unread pages are arrays in JSON, one or many", {
  local_registries()
  local_clean_cache()
  doc <- quiet(gr_ingest("Revenue was 45.2 million dollars in the year.\n\nCosts rose.",
                         gr_ingest_spec(clean = "page_numbers"), cache = FALSE))
  js <- jsonlite::fromJSON(as_json(doc), simplifyVector = FALSE)
  expect_identical(js$stats$clean_steps, list("page_numbers"))
  expect_identical(js$stats$unread_pages, list())

  gr_register_extractor("r7pages", "r7pages", fn = function(path, opts) {
    out <- data.frame(text = c("Revenue was 45.2 million dollars.", "Costs rose over the year."),
                      page = c(1L, 3L), stringsAsFactors = FALSE)
    attr(out, "gr_unread_pages") <- 2L
    out
  })
  f <- withr::local_tempfile(fileext = ".r7pages")
  writeLines("x", f)
  d2 <- quiet(gr_ingest(f, gr_ingest_spec(clean = c("page_numbers", "urls")), cache = FALSE))
  js2 <- jsonlite::fromJSON(as_json(d2), simplifyVector = FALSE)
  expect_identical(js2$stats$unread_pages, list(2L))
  expect_identical(js2$stats$clean_steps, list("page_numbers", "urls"))
})

# ---------------------------------------------------------------------------
# H2-true-page-count (extractors): the page count stopped at the last page any
# block survived cleaning on, so a PDF whose last pages held only references
# (taken out by the cleaner) or page furniture was reported short. extract_pdf
# now says how many pages the file has, and gr_ingest() counts them.
# ---------------------------------------------------------------------------

test_that("the page count includes trailing pages whose blocks were all removed", {
  local_registries()
  local_clean_cache()
  pages <- NULL
  gr_register_extractor("r7count", "r7count", fn = function(path, opts) {
    out <- data.frame(text = c("Revenue was 45.2 million dollars.", "Costs rose over the year.",
                               "Page 5 of 5"),
                      page = c(1L, 2L, 5L), stringsAsFactors = FALSE)
    attr(out, "gr_pages") <- pages
    out
  })
  f <- withr::local_tempfile(fileext = ".r7count")
  writeLines("x", f)
  count <- function(p) {
    pages <<- p
    quiet(gr_ingest(f, gr_ingest_spec(clean = "page_numbers"), cache = FALSE))$stats$pages
  }
  # Page 5 holds only its page number, which is cleaned away.
  expect_identical(count(NULL), 2L)
  expect_identical(count(6L), 6L)
  expect_identical(count(6), 6L)
  # An extractor that says less than its blocks show is outvoted by them, and
  # one that says nothing usable is ignored.
  expect_identical(count(1L), 2L)
  for (bad in list(NA_integer_, "six", c(3L, 9L), -4L, Inf, 1e12, list(9L))) {
    expect_identical(count(bad), 2L, info = paste(format(bad), collapse = " "))
  }
})

test_that("a PDF's trailing references page still counts as a page", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  f <- withr::local_tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 6, height = 8)
  body <- c("Introduction", "Revenue in the year was 45.2 million dollars overall.",
            "Costs rose by four percent over the same period of time.")
  for (pg in 1:2) {
    graphics::plot.new()
    for (k in seq_along(body)) graphics::text(0.05, 0.95 - k * 0.06, body[k], adj = 0)
  }
  graphics::plot.new()
  refs <- c("References", "Smith J. A study of revenue. J Econ. 2019;4:1-9.",
            "Jones K. Costs in practice. Econ Rev. 2020;7:10-19.")
  for (k in seq_along(refs)) graphics::text(0.05, 0.95 - k * 0.06, refs[k], adj = 0)
  grDevices::dev.off()
  raw <- quiet(gr_ingest(f, gr_ingest_spec(clean = "none", ocr = "never"), cache = FALSE))
  skip_if(!identical(max(raw$blocks$page, na.rm = TRUE), 3L),
          "pdftools did not read the generated pages")
  doc <- quiet(gr_ingest(f, gr_ingest_spec(clean = "references", ocr = "never"), cache = FALSE))
  expect_false(3L %in% doc$blocks$page)
  expect_identical(doc$stats$pages, 3L)
})

# ---------------------------------------------------------------------------
# security-04: a presigned or tokenised address was stored and shown whole --
# the document's source, the "Fetching" and "Extracting" lines, the trace, the
# fetch errors, the empty-document error, and a corpus's labels, and from them
# summary$document, names(answers) and every table built on them -- its
# signature, key id and any user name and password included.
# ---------------------------------------------------------------------------

r7_secret_url <- function(path = "reports/p.txt", sig = "SECRETSIG") {
  sprintf("https://user:pw@bucket.example.org/%s?X-Amz-Credential=AKIAKEYID&X-Amz-Signature=%s#frag",
          path, sig)
}
r7_leaks <- function(x) grepl("SECRETSIG|AKIAKEYID|user:pw|pw@|#frag", x)

r7_serve <- function(text = "Revenue was 45.2 million dollars in the year under review.",
                     status = 200L, type = "text/plain", env = parent.frame()) {
  asked <- new.env(parent = emptyenv())
  asked$urls <- character(0)
  local_mocked_bindings(url_download = function(url, dest) {
    asked$urls <- c(asked$urls, url)
    writeLines(text, dest)
    list(status = status, type = type)
  }, .package = "readgpt", .env = env)
  asked
}

test_that("a document fetched from a presigned address does not keep its secrets", {
  local_clean_cache()
  asked <- r7_serve()
  u <- r7_secret_url()
  tr <- gr_trace()
  # The progress lines are what this checks, whatever the suite was run with.
  old_verbose <- gr_options("verbose")
  gr_options(verbose = TRUE)
  withr::defer(gr_options(verbose = old_verbose))
  msgs <- character(0)
  doc <- withCallingHandlers(suppressWarnings(gr_ingest(u, trace = tr)),
                             message = function(m) {
                               msgs <<- c(msgs, conditionMessage(m))
                               invokeRestart("muffleMessage")
                             })
  # The request itself still uses the address in full.
  expect_identical(asked$urls, utils::URLencode(u))
  expect_false(r7_leaks(doc$source))
  expect_match(doc$source, "^https://bucket\\.example\\.org/reports/p\\.txt \\[query hidden [0-9a-f]{6}\\]$")
  # The fingerprint is the audit report's, of the same query.
  expect_identical(sub("^.* \\[query hidden ([0-9a-f]{6})\\]$", "\\1", doc$source),
                   substr(gr_hash("?X-Amz-Credential=AKIAKEYID&X-Amz-Signature=SECRETSIG#frag"),
                          1, 6))
  expect_true(any(grepl("Fetching", msgs, fixed = TRUE)))
  expect_true(any(grepl("Extracting", msgs, fixed = TRUE)))
  expect_false(any(r7_leaks(msgs)))
  expect_false(r7_leaks(as_json(tr)))
  expect_false(r7_leaks(as_json(doc)))
  # Two addresses that differ only in the query stay apart.
  other <- quiet(gr_ingest(r7_secret_url(sig = "OTHERSIG"), cache = FALSE))
  expect_false(identical(other$source, doc$source))
  # An answer drawn from it, and the audit report's name for it, show no more.
  a <- quiet(answer_document(u, "What was revenue?", "fast",
                             client = gr_mock_client(function(m, p) "45.2 million.")))
  expect_false(r7_leaks(a$document$source))
  expect_identical(readgpt:::report_doc_name(a$document$source), a$document$source)
})

test_that("an address with nothing to hide is shown as it is", {
  expect_identical(url_shown(c("https://example.org/reports/annual", "http://h.org/a b.pdf", NA)),
                   c("https://example.org/reports/annual", "http://h.org/a b.pdf", NA))
  shown <- url_shown(r7_secret_url())
  # Shown once or twice, and by the audit report after, it is the same.
  expect_identical(url_shown(shown), shown)
  expect_identical(readgpt:::report_url(shown), shown)
  expect_identical(readgpt:::report_url_text(sprintf("Fetching '%s' failed.", shown)),
                   sprintf("Fetching '%s' failed.", shown))
})

test_that("a long presigned address is read without a path-length warning", {
  local_clean_cache()
  r7_serve()
  u <- sprintf("https://bucket.example.org/p.txt?X-Amz-Signature=%s", strrep("A", 1500))
  expect_no_warning(doc <- suppressMessages(gr_ingest(u, trace = gr_trace(), cache = FALSE)))
  expect_false(grepl("AAAAAAAA", doc$source, fixed = TRUE))
})

test_that("an address whose text is cleaned away is refused without its secrets", {
  local_clean_cache()
  r7_serve(text = "12")
  err <- tryCatch(quiet(gr_ingest(r7_secret_url(), cache = FALSE)), error = function(e) e)
  expect_s3_class(err, "gr_empty_document")
  expect_false(r7_leaks(conditionMessage(err)))
  expect_match(conditionMessage(err), "bucket.example.org/reports/p.txt [query hidden", fixed = TRUE)
})

test_that("the fetch errors do not quote an address's secrets", {
  local_clean_cache()
  r7_serve(status = 403L)
  err <- tryCatch(quiet(gr_ingest(r7_secret_url(), cache = FALSE)), error = function(e) e)
  expect_s3_class(err, "gr_url_error")
  expect_match(conditionMessage(err), "HTTP 403", fixed = TRUE)
  expect_match(conditionMessage(err), "bucket.example.org/reports/p.txt [query hidden", fixed = TRUE)
  expect_false(r7_leaks(conditionMessage(err)))

  # A zip that is not a Word file: no extractor reads it.
  local_mocked_bindings(url_download = function(url, dest) {
    writeBin(as.raw(c(0x50, 0x4b, 0x03, 0x04, 0, 0, 0, 0)), dest)
    list(status = 200L, type = "application/zip")
  }, .package = "readgpt")
  err <- tryCatch(quiet(gr_ingest(r7_secret_url("data"), cache = FALSE)), error = function(e) e)
  expect_s3_class(err, "gr_unsupported_format")
  expect_false(r7_leaks(conditionMessage(err)))

  local_mocked_bindings(url_download = function(url, dest) {
    stop(sprintf("Timeout was reached: [%s] Operation timed out", url))
  }, .package = "readgpt")
  err <- tryCatch(quiet(gr_ingest(r7_secret_url(), cache = FALSE)), error = function(e) e)
  expect_s3_class(err, "gr_url_error")
  expect_match(conditionMessage(err), "Timeout was reached", fixed = TRUE)
  expect_false(r7_leaks(conditionMessage(err)))
})

test_that("a refused or redirected download does not quote an address's secrets", {
  withr::local_options(readgpt.allow_local_urls = NULL)
  err <- tryCatch(readgpt:::url_download("http://user:pw@127.0.0.1/x?token=SECRETSIG",
                                         withr::local_tempfile(), allow_local = FALSE),
                  error = function(e) e)
  expect_s3_class(err, "gr_url_error")
  expect_match(conditionMessage(err), "private network", fixed = TRUE)
  expect_false(r7_leaks(conditionMessage(err)))

  # A redirect to a presigned private address names neither address's secrets.
  local_mocked_bindings(GET = function(url, ...) {
    loc <- if (grepl("start", url)) "http://169.254.169.254/latest/?X-Amz-Signature=SECRETSIG"
           else "ftp://user:pw@files.example.org/p.txt?X-Amz-Signature=SECRETSIG"
    structure(list(url = url, status_code = 302L, headers = list(location = loc)),
              class = "response")
  }, .package = "httr")
  err <- tryCatch(readgpt:::url_download("https://example.org/start?X-Amz-Credential=AKIAKEYID",
                                         withr::local_tempfile(), allow_local = FALSE,
                                         lookup = function(host) NULL),
                  error = function(e) e)
  expect_s3_class(err, "gr_url_error")
  expect_match(conditionMessage(err), "169.254.169.254", fixed = TRUE)
  expect_false(r7_leaks(conditionMessage(err)))
  # One that leaves the web says where it went without its credentials, and
  # names the address that sent it there, not the one it was sent to.
  err <- tryCatch(readgpt:::url_download("https://example.org/go?X-Amz-Credential=AKIAKEYID",
                                         withr::local_tempfile(), allow_local = TRUE),
                  error = function(e) e)
  expect_s3_class(err, "gr_url_error")
  expect_match(conditionMessage(err), "^'https://example\\.org/go \\[query hidden [0-9a-f]{6}\\]' redirected")
  expect_match(conditionMessage(err), "ftp", fixed = TRUE)
  expect_false(r7_leaks(conditionMessage(err)))
})

test_that("a corpus of presigned addresses keeps their secrets out of its labels", {
  local_clean_cache()
  local_mocked_bindings(url_download = function(url, dest) {
    if (grepl("denied", url, fixed = TRUE)) return(list(status = 403L, type = ""))
    writeLines(sprintf("Revenue was %s million dollars.", if (grepl("AAA", url)) "45.2" else "51.8"),
               dest)
    list(status = 200L, type = "text/plain")
  }, .package = "readgpt")
  urls <- c(r7_secret_url("p.txt", "SECRETSIGAAA"), r7_secret_url("p.txt", "SECRETSIGBBB"),
            r7_secret_url("denied.txt"))
  st <- withr::local_tempdir()
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million.")
  co <- quiet(gr_read_many(urls, "What was revenue?", "fast", store = st, client = cl))
  expect_false(any(r7_leaks(co$summary$document)))
  expect_false(any(r7_leaks(names(co$answers))))
  expect_false(any(r7_leaks(co$summary$error), na.rm = TRUE))
  expect_false(r7_leaks(as_json(co$trace)))
  # Two links to one object with different signatures are two rows, apart.
  expect_identical(anyDuplicated(co$summary$document), 0L)
  expect_match(co$summary$document[1], "^bucket\\.example\\.org/p\\.txt \\[query hidden [0-9a-f]{6}\\]$")
  expect_identical(co$summary$status, c("ok", "ok", "failed"))
  # The audit report shows the labels as they are.
  expect_identical(readgpt:::report_url(co$summary$document), co$summary$document)
  # The store is keyed by the address, not the label: a second run restores.
  again <- quiet(gr_read_many(urls, "What was revenue?", "fast", store = st, client = cl))
  expect_identical(again$summary$status, c("restored", "restored", "failed"))
  expect_identical(again$summary$document, co$summary$document)
})

test_that("corpus_label hides an address's credentials and keeps files as they were", {
  lab <- corpus_label(r7_secret_url())
  expect_false(r7_leaks(lab))
  expect_match(lab, "^bucket\\.example\\.org/reports/p\\.txt \\[query hidden [0-9a-f]{6}\\]$")
  expect_identical(corpus_label("  https://Example.org/a/report \n"), "Example.org/a/report")
  expect_identical(corpus_label("http://localhost:8080/x?token=SECRETSIG"),
                   sprintf("localhost:8080/x [query hidden %s]",
                           substr(gr_hash("?token=SECRETSIG"), 1, 6)))
  expect_identical(corpus_label("no-such-dir/notes#2.txt"), "notes#2.txt")
})

# ---------------------------------------------------------------------------
# H3-gr_ingest-docs (extractors): gr_ingest()'s help said nothing of the
# download size limit, the refusal of local and private addresses, the
# declared charset, the Word unpacking limit, or how the source is shown.
# ---------------------------------------------------------------------------

test_that("gr_ingest's documentation states the download limits and refusals", {
  path <- testthat::test_path("..", "..", "R", "ingest.R")
  if (!file.exists(path)) skip("package source not available")
  src <- readLines(path, warn = FALSE)
  from <- grep("^#' @param source", src)
  to <- grep("^#' @param spec", src)
  doc <- paste(sub("^#' ?", "", src[from:(to - 1L)]), collapse = " ")
  for (s in c("512 MB", "`gr_too_large`", "`gr_url_error`", "readgpt.allow_local_urls",
              "169.254", "192.168", "172.16", "redirect", "proxy", "charset",
              "Word file", "query hidden")) {
    expect_match(doc, s, fixed = TRUE, info = s)
  }
})
