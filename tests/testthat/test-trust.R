# test-trust.R
#
# What makes an answer safe to act on without reading the trace: a run that
# cannot send a request stops and says why, print() says why an answer is
# partial and what it cost, the warnings raised while a document was read stay
# with the answer, and pages that never became text mark the answer partial.

no_key <- function(env = parent.frame()) {
  withr::local_envvar(OPENAI_API_KEY = NA, .local_envir = env)
  withr::local_options(readgpt.api_key = NULL, .local_envir = env)
}

# An extractor for a made-up extension that counts how often it runs, so a test
# can tell "stopped before reading" from "read, then stopped".
counting_extractor <- function(env = parent.frame()) {
  hits <- new.env()
  hits$n <- 0L
  gr_register_extractor("counted", "rgx", description = "test extractor", fn = function(path, opts) {
    hits$n <- hits$n + 1L
    data.frame(text = readLines(path, warn = FALSE), stringsAsFactors = FALSE)
  })
  hits
}

write_doc <- function(ext, lines = c("Revenue was 45.2 million dollars in fiscal 2024.",
                                     "Headcount grew to 1,204 employees.")) {
  f <- tempfile(fileext = paste0(".", ext))
  writeLines(lines, f)
  f
}

# A warning of the package's own class, raised the way gr_warn() raises one.
gr_style_warning <- function(msg, class) {
  warning(structure(class = c(class, "gr_warning", "warning", "condition"),
                    list(message = msg, call = NULL)))
}

# ---------------------------------------------------------------------------
# No key
# ---------------------------------------------------------------------------

test_that("a run with no key stops before it reads the document", {
  local_registries()
  local_clean_cache()
  no_key()
  hits <- counting_extractor()
  f <- write_doc("rgx")

  expect_error(answer_document(f, "What was revenue?"), class = "gr_auth_error",
               regexp = "OPENAI_API_KEY")
  expect_error(gr_compare(f, "What was revenue?", c("fast", "needle")), class = "gr_auth_error")
  expect_error(gr_read_many(f, "What was revenue?"), class = "gr_auth_error")
  expect_identical(hits$n, 0L)
})

test_that("a client with no key stops at its first request instead of answering", {
  local_clean_cache()
  no_key()
  ch <- gr_segment(gr_ingest(sample_doc(2, 3)), list(method = "paragraph", max_tokens = 120))
  cl <- gr_client(max_retries = 0L)
  for (reader in c("stuff", "map_reduce", "skim", "ensemble")) {
    expect_error(gr_read(ch, "How many participants?", cl, reader), class = "gr_auth_error",
                 info = reader)
  }
  # The embeddings path stops too, rather than falling back to lexical vectors
  # and reporting a quality problem.
  expect_error(gr_embed(cl, c("one text", "another")), class = "gr_auth_error")
})

test_that("a cached client is let through, and stops at the first request the cache cannot answer", {
  local_clean_cache()
  no_key()
  cc <- gr_cache_client(gr_client(max_retries = 0L), gr_cache(tempfile("cache-")))
  expect_true(readgpt:::stop_if_no_credentials(cc))
  # on_error = "continue" records a failed document and moves on. A missing key
  # is not one document's failure, so it stops the run all the same.
  expect_error(gr_read_many(c(readgpt_example(), write_doc("txt")), "What was revenue?",
                            client = cc, on_error = "continue"),
               class = "gr_auth_error")
  expect_error(gr_compare(readgpt_example(), "What was revenue?", c("fast", "needle"),
                          client = cc, on_error = "continue"),
               class = "gr_auth_error")
})

test_that("clients that carry their credential some other way are not stopped", {
  no_key()
  expect_true(readgpt:::stop_if_no_credentials(gr_client(headers = c("api-key" = "corp"))))
  expect_true(readgpt:::stop_if_no_credentials(mock_echo()))
  expect_true(readgpt:::stop_if_no_credentials(
    gr_backend_client(function(messages, params) "ok", model = "any-model", id = "trust-test")))
  expect_error(readgpt:::stop_if_no_credentials(gr_client()), class = "gr_auth_error")
})

# ---------------------------------------------------------------------------
# print()
# ---------------------------------------------------------------------------

test_that("print() says why an answer is partial and what the first error was", {
  local_clean_cache()
  bad <- quiet(answer_document(sample_doc(1, 2), "How many participants?", "thorough",
                               client = mock_dead("connection refused by host")))
  expect_true(bad$partial)
  out <- capture.output(print(bad))
  expect_true(any(grepl("Partial because: 1 request\\(s\\) failed; first error: connection refused by host",
                        out)), info = paste(out, collapse = "\n"))
  expect_true("Not found in the part of the document that was read." %in% out)
  # The value code tests is unchanged; only the display is in words.
  expect_identical(bad$answer, "NOT_IN_DOCUMENT")
})

test_that("print() words a clean 'not found' differently from a partial one", {
  local_clean_cache()
  nf <- answer_document(sample_doc(1, 2), "What was the dividend?", "fast",
                        client = mock_echo("NOT_IN_DOCUMENT"))
  expect_false(nf$partial)
  out <- capture.output(print(nf))
  expect_true("Not found in the document." %in% out)
  expect_false(any(grepl("Partial because", out)))
})

test_that("print() shows the cost, and the page and section of the evidence", {
  local_registries()
  local_clean_cache()
  gr_register_extractor("paged", "pgx", description = "test extractor with pages",
    fn = function(path, opts) {
      data.frame(text = c(paste("Revenue was 45.2 million dollars in fiscal 2024, up from 41.8",
                                "million a year earlier, driven by the clinical division."),
                          paste("The board expects revenue of about 50 million dollars next year,",
                                "helped by two new sites opening in the spring.")),
                 page = c(3L, 4L), section = c("Results", "Outlook"),
                 stringsAsFactors = FALSE)
    })
  # Paragraph chunks keep the page and section of the block they came from.
  ch <- gr_segment(gr_ingest(write_doc("pgx")), list(method = "paragraph", max_tokens = 32))
  ans <- gr_read(ch, "What was revenue?", mock_echo("Revenue was 45.2 million dollars."), "stuff")
  out <- capture.output(print(ans))
  ev <- grep("^  Evidence: ", out, value = TRUE)
  expect_length(ev, 1L)
  expect_match(ev, "^  Evidence: chunk 1 \\(p\\. 3, \"Results\"\\)")
  expect_match(ev, "\\(p\\. 4, \"Outlook\"\\)")
  expect_true(any(grepl("\\$[0-9]+\\.[0-9]{4} across ", out)))
  expect_true(any(grepl("^  cost: \\$[0-9]", capture.output(print(ans$trace)))))
})

test_that("an unpriced model prints as cost unknown, never as free", {
  local_registries()
  local_clean_cache()
  gr_register_model("unpriced-model", context_window = 100000L, max_output = 2000L)
  ch <- gr_segment(gr_ingest(sample_doc(1, 2)), list(method = "paragraph", max_tokens = 400))
  ans <- quiet(gr_read(ch, "How many participants?", mock_echo("482."),
                       list(reader = "stuff", model = "unpriced-model")))
  out <- capture.output(print(ans))
  expect_true(any(grepl("cost unknown \\(no registered price for unpriced-model\\)", out)))
  expect_false(any(grepl("\\$0\\.0000", out)))
})

test_that("the reasons are read by exact name, not by prefix", {
  # With `$`, notes$degraded would partial-match degraded_to_bm25 and report a
  # fallback the reader never took.
  a <- new_answer("An answer.", "rerank", "q", 1L, gr_trace(), partial = TRUE,
                  notes = list(degraded_to_bm25 = TRUE))
  why <- readgpt:::partial_reasons(a)
  expect_true("relevance scoring fell back to word matching" %in% why)
  expect_false("fell back to a simpler method" %in% why)
})

test_that("gr_extractors() says which packages each one needs and whether they are here", {
  ex <- gr_extractors()
  expect_true(all(c("needs", "available") %in% names(ex)))
  expect_identical(ex$needs[ex$name == "txt"], "")
  expect_true(ex$available[ex$name == "txt"])
  expect_identical(ex$needs[ex$name == "pdf"], "pdftools")
  expect_identical(ex$available[ex$name == "pdf"],
                   requireNamespace("pdftools", quietly = TRUE))
  expect_identical(ex$available[ex$name == "image"],
                   requireNamespace("tesseract", quietly = TRUE))
})

# ---------------------------------------------------------------------------
# Warnings stay with the result
# ---------------------------------------------------------------------------

test_that("warnings raised while a document is read travel with the document and the answer", {
  local_registries()
  local_clean_cache()
  gr_register_extractor("noisy", "nzx", description = "test extractor that warns",
    fn = function(path, opts) {
      gr_style_warning("page 2 had no text layer", "test_extract_warning")
      data.frame(text = readLines(path, warn = FALSE), stringsAsFactors = FALSE)
    })
  f <- write_doc("nzx")
  cl <- mock_echo("Revenue was 45.2 million dollars.")

  # Still raised at the console, as before.
  expect_warning(ans <- answer_document(f, "What was revenue?", "fast", client = cl),
                 class = "test_extract_warning")
  expect_identical(unname(ans$warnings), "page 2 had no text layer")
  expect_identical(names(ans$warnings), "test_extract_warning")

  # The second run reads the document from the ingestion cache. Nothing is
  # raised again, and the answer still carries what the first read found.
  expect_no_warning(again <- answer_document(f, "What was revenue?", "fast", client = cl))
  expect_identical(unname(again$warnings), "page 2 had no text layer")

  # [[exact = TRUE]]: `$` on the parsed list would also find a key that merely
  # starts with "warnings".
  js <- jsonlite::fromJSON(as_json(again), simplifyVector = FALSE)
  w <- js[["warnings", exact = TRUE]]
  expect_identical(w[[1]][["class", exact = TRUE]], "test_extract_warning")
  expect_identical(w[[1]][["message", exact = TRUE]], "page 2 had no text layer")
})

test_that("a folder run ties each warning to its document, including one that failed", {
  local_registries()
  local_clean_cache()
  gr_register_extractor("noisy", "nzx", description = "test extractor that warns",
    fn = function(path, opts) {
      gr_style_warning(sprintf("odd layout in %s", basename(path)), "test_extract_warning")
      if (grepl("broken", path)) stop("cannot parse this one")
      data.frame(text = readLines(path, warn = FALSE), stringsAsFactors = FALSE)
    })
  dir <- tempfile("folder-")
  dir.create(dir)
  writeLines("Revenue was 45.2 million dollars in fiscal 2024.", file.path(dir, "a.nzx"))
  writeLines("Revenue was 45.2 million dollars in fiscal 2024.", file.path(dir, "broken.nzx"))
  writeLines("Headcount grew to 1,204 employees across nine sites.", file.path(dir, "c.txt"))

  out <- quiet(gr_read_many(dir, "What was revenue?", "fast",
                            client = mock_echo("Revenue was 45.2 million dollars.")))
  s <- out$summary
  expect_identical(s$warnings[s$document == "a.nzx"], "odd layout in a.nzx")
  expect_identical(s$status[s$document == "broken.nzx"], "failed")
  # The failure is in `status` and `error`; `warnings` keeps what came before it.
  expect_identical(s$warnings[s$document == "broken.nzx"], "odd layout in broken.nzx")
  expect_true(is.na(s$warnings[s$document == "c.txt"]))
  expect_true(any(grepl("raised warnings", capture.output(print(out)))))
})

test_that("a store written before the warnings column still restores", {
  local_clean_cache()
  store <- tempfile("store-")
  docs <- c(readgpt_example(), write_doc("txt"))
  cl <- mock_echo("Revenue was 45.2 million dollars.")
  quiet(gr_read_many(docs, "What was revenue?", "fast", client = cl, store = store))

  # Take the column back out of every saved row, as an older version wrote them.
  for (p in list.files(store, pattern = "[.]rds$", full.names = TRUE, recursive = TRUE)) {
    entry <- readRDS(p)
    entry$row$warnings <- NULL
    saveRDS(entry, p)
  }
  again <- quiet(gr_read_many(docs, "What was revenue?", "fast", client = cl, store = store))
  expect_identical(again$summary$status, c("restored", "restored"))
  expect_true("warnings" %in% names(again$summary))
  expect_true(all(is.na(again$summary$warnings)))
})

test_that("a string that looks like a path is read as text, with a warning that says so", {
  local_clean_cache()
  expect_warning(doc <- gr_ingest("reports/2024/annual-report.pfd"), class = "gr_path_as_text")
  expect_identical(doc$source, "<inline text>")
  expect_identical(names(doc$warnings), "gr_path_as_text")
  # Prose is not a path, and a missing file with an extension readgpt reads is
  # still an error rather than a document.
  expect_no_warning(gr_ingest("Growth reached 3/4 of the target in fiscal 2024."))
  expect_no_warning(gr_ingest("Revenue rose to 45.2 million dollars in the year."))
  expect_error(gr_ingest("reports/2024/annual-report.pdf"), class = "gr_file_not_found")
})

# ---------------------------------------------------------------------------
# Content that never reached a model
# ---------------------------------------------------------------------------

test_that("pages that never became text mark the answer partial", {
  local_registries()
  local_clean_cache()
  gr_register_extractor("gappy", "gpx", description = "test extractor with an unread page",
    fn = function(path, opts) {
      out <- data.frame(text = "Revenue was 45.2 million dollars in fiscal 2024.",
                        page = 1L, stringsAsFactors = FALSE)
      attr(out, "gr_unread_pages") <- 2L
      out
    })
  f <- write_doc("gpx")
  doc <- gr_ingest(f)
  expect_identical(doc$stats$unread_pages, 2L)
  # A trailing page with no text is still a page of the document.
  expect_identical(doc$stats$pages, 2L)

  ans <- answer_document(f, "What was revenue?", "fast",
                         client = mock_echo("Revenue was 45.2 million dollars."))
  expect_true(ans$partial)
  expect_identical(ans$notes$unread_pages, 2L)
  expect_true(any(grepl("page\\(s\\) never read: 2", capture.output(print(ans)))))

  # The same holds for a reader called directly on the document's chunks.
  ch <- gr_segment(doc, list(method = "paragraph", max_tokens = 200))
  expect_true(gr_read(ch, "What was revenue?", mock_echo("Revenue was 45.2."), "stuff")$partial)

  # A document with every page read is not affected.
  clean <- answer_document(write_doc("txt"), "What was revenue?", "fast",
                           client = mock_echo("Revenue was 45.2 million dollars."))
  expect_false(clean$partial)
  expect_null(clean$notes$unread_pages)
})

test_that("a scanned PDF page read without OCR is recorded as unread", {
  skip_if_not_installed("pdftools")
  skip_if(requireNamespace("tesseract", quietly = TRUE) && requireNamespace("magick", quietly = TRUE),
          "OCR is installed, so the page would be read")
  local_clean_cache()
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  graphics::plot.new()
  graphics::text(0, 0.9, "Revenue was 45.2 million dollars in fiscal 2024.", adj = 0)
  graphics::plot.new()
  invisible(grDevices::dev.off())

  expect_warning(doc <- gr_ingest(f), class = "gr_ocr_unavailable")
  expect_identical(doc$stats$unread_pages, 2L)
  expect_identical(names(doc$warnings), "gr_ocr_unavailable")
  # Declining OCR is a choice, not a loss.
  expect_length(gr_ingest(f, gr_ingest_spec(ocr = "never"))$stats$unread_pages, 0L)
})

test_that("hierarchical summaries cut to fit make the answer partial", {
  local_registries()
  gr_register_model("small-window", context_window = 900L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  ch <- gr_segment(gr_ingest(sample_doc(6, 5)), list(method = "paragraph", max_tokens = 60))
  a <- quiet(gr_read(ch, "Summarise the findings", mock_bulky(),
                     list(reader = "hierarchical", model = "small-window",
                          fan_in = 3L, max_levels = 1L, max_summary_tokens = 150L)))
  expect_true(a$notes$summaries_truncated)
  expect_true(a$partial)
  expect_true(any(grepl("summaries cut to fit", capture.output(print(a)))))
})

test_that("refine marks the answer partial when it cuts an excerpt to fit", {
  local_registries()
  gr_register_model("refine-window", context_window = 900L, max_output = 150L,
                    input_usd = 0, output_usd = 0)
  long <- paste(rep("The cohort comprised 482 participants recruited across nine clinical sites.",
                    30), collapse = " ")
  ch <- new_chunks(c(long, long), "fixed", gr_segment_spec(max_tokens = 4000L))
  a <- quiet(gr_read(ch, "How many participants?", mock_echo("482 participants."),
                     list(reader = "refine", model = "refine-window",
                          max_answer_tokens = 100L)))
  expect_gt(a$notes$truncations, 0L)
  expect_true(a$partial)
})

test_that("gr_compare() answers carry the document and page-resolved evidence too", {
  local_clean_cache()
  cmp <- gr_compare(readgpt_example(), "What was revenue?", c("fast", "needle"),
                    client = mock_echo("Revenue was 45.2 million dollars."))
  for (a in cmp$answers) {
    expect_identical(a$document$source, normalizePath(readgpt_example(), winslash = "/"))
    expect_true(is.character(a$warnings))
  }
})

# ---------------------------------------------------------------------------
# Found in review of the first draft
# ---------------------------------------------------------------------------

test_that("a run whose documents are all in the store needs no key", {
  local_registries()
  local_clean_cache()
  store <- tempfile("store-")
  dead <- function() gr_client(base_url = "http://127.0.0.1:9", max_retries = 0L, timeout = 5)
  # The first run has to SUCCEED to be stored: a document whose requests failed
  # is kept out of the store so that the next run reads it again. So its one
  # request is answered here, and every later request goes to the dead address.
  withr::with_envvar(c(OPENAI_API_KEY = "sk-test-first-run"),
    testthat::with_mocked_bindings(
      first <- quiet(gr_read_many(readgpt_example(), "What was revenue?", "fast", client = dead(),
                                  store = store)),
      http_call = function(client, url, body) {
        gr_result(TRUE, text = "Revenue was 45.2 million dollars.", status = 200L,
                  model = as_chr1(body$model, "gpt-4o-mini"))
      },
      .package = "readgpt"))
  expect_identical(first$summary$status, "ok")
  no_key()
  again <- gr_read_many(readgpt_example(), "What was revenue?", "fast", client = dead(),
                        store = store)
  expect_identical(again$summary$status, "restored")

  # A document that is not in the store still stops the run before it is read.
  hits <- counting_extractor()
  expect_error(gr_read_many(c(readgpt_example(), write_doc("rgx")), "What was revenue?", "fast",
                            client = dead(), store = store),
               class = "gr_auth_error")
  expect_identical(hits$n, 0L)
})

test_that("a PDF page with a little text is read, not listed as unread", {
  skip_if_not_installed("pdftools")
  skip_if(requireNamespace("tesseract", quietly = TRUE) && requireNamespace("magick", quietly = TRUE),
          "OCR is installed, so every page would be read")
  local_clean_cache()
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  graphics::plot.new(); graphics::text(0, 0.9, "Annual Report 2024", adj = 0)
  graphics::plot.new(); graphics::text(0, 0.9, "Revenue was 45.2 million dollars in fiscal 2024.", adj = 0)
  invisible(grDevices::dev.off())
  # The cover is under the 40-character OCR threshold, but its text is there.
  expect_length(suppressWarnings(gr_ingest(f))$stats$unread_pages, 0L)
  expect_length(suppressWarnings(gr_ingest(f, gr_ingest_spec(ocr = "always")))$stats$unread_pages, 0L)
})

test_that("in a folder run, a duplicate keeps its own warnings and unread pages keep a copy apart", {
  local_registries()
  local_clean_cache()
  line <- "Revenue was 45.2 million dollars in fiscal 2024, up from 41.8 million."
  gr_register_extractor("noisy", "nzx", description = "test extractor that warns",
    fn = function(path, opts) {
      gr_style_warning(sprintf("odd layout in %s", basename(path)), "test_extract_warning")
      data.frame(text = readLines(path, warn = FALSE), stringsAsFactors = FALSE)
    })
  gr_register_extractor("gappy", "gpx", description = "test extractor with an unread page",
    fn = function(path, opts) {
      out <- data.frame(text = readLines(path, warn = FALSE), page = 1L, stringsAsFactors = FALSE)
      attr(out, "gr_unread_pages") <- 2L
      out
    })
  dir <- tempfile("folder-")
  dir.create(dir)
  for (f in c("1-first.nzx", "2-copy.txt", "3-copy.nzx", "4-scan.gpx")) writeLines(line, file.path(dir, f))

  s <- quiet(gr_read_many(dir, "What was revenue?", "fast",
                          client = mock_echo("Revenue was 45.2 million dollars.")))$summary
  row <- function(d) s[s$document == d, , drop = FALSE]
  expect_identical(row("2-copy.txt")$status, "duplicate")
  expect_true(is.na(row("2-copy.txt")$warnings))
  expect_identical(row("3-copy.nzx")$warnings, "odd layout in 3-copy.nzx")
  # Same text, but a page of it never became text: read on its own, and partial.
  expect_identical(row("4-scan.gpx")$status, "ok")
  expect_true(row("4-scan.gpx")$partial)
  expect_false(row("1-first.nzx")$partial)
})

test_that("a failed document keeps its warnings when it comes from the ingestion cache", {
  local_registries()
  local_clean_cache()
  gr_register_extractor("noisy", "nzx", description = "test extractor that warns",
    fn = function(path, opts) {
      gr_style_warning("odd layout", "test_extract_warning")
      data.frame(text = sample_doc(3, 4), stringsAsFactors = FALSE)
    })
  f <- write_doc("nzx")
  gr_options(max_calls = 2)
  run <- function() quiet(gr_read_many(f, "How many participants?", "thorough",
                                       client = mock_echo("482."), max_tokens = 60))$summary
  first <- run()
  second <- run()
  expect_identical(first$status, "failed")
  expect_identical(second$status, "failed")
  expect_identical(second$warnings, "odd layout")
})

test_that("gr_compare() gives each recipe the losses of its own ingestion, in any order", {
  local_registries()
  local_clean_cache()
  gr_register_extractor("gappy", "gpx", description = "unread page unless OCR is declined",
    fn = function(path, opts) {
      out <- data.frame(text = "Revenue was 45.2 million dollars in fiscal 2024.", page = 1L,
                        stringsAsFactors = FALSE)
      if (!identical(opts$ocr, "never")) attr(out, "gr_unread_pages") <- 2L
      out
    })
  f <- write_doc("gpx")
  seg <- list(method = "paragraph", max_tokens = 400)
  never <- gr_recipe("ocr_never", ingest = list(ocr = "never"), segment = seg, read = "stuff")
  auto <- gr_recipe("ocr_auto", ingest = list(ocr = "auto"), segment = seg, read = "stuff")
  cl <- mock_echo("Revenue was 45.2 million dollars.")
  for (ord in list(list(never, auto), list(auto, never))) {
    cmp <- quiet(gr_compare(f, "What was revenue?", ord, client = cl))
    expect_false(cmp$answers$ocr_never$partial)
    expect_true(cmp$answers$ocr_auto$partial)
  }
})

test_that("a top-k reader's 'not found' is not printed as the whole document's", {
  local_registries()
  local_clean_cache()
  gr_options(embedder = "lexical")
  ch <- gr_segment(gr_ingest(sample_doc(3, 4)), list(method = "paragraph", max_tokens = 60))
  a <- gr_read(ch, "What was the dividend?", mock_echo("NOT_IN_DOCUMENT"),
               list(reader = "retrieve", top_k = 2))
  expect_false(a$partial)
  expect_true("Not found in the part of the document that was read." %in% capture.output(print(a)))
})

test_that("print() copes with what a custom reader can hand back", {
  tr <- gr_trace()
  a <- new_answer("Revenue was 45.2 million.", "my_reader", "What was revenue?", 1L, tr,
                  partial = TRUE, notes = c(strategy = "first chunk"))
  expect_no_error(capture.output(print(a)))
  ev <- data.frame(chunk_id = 1:2, text = c("45.2 million", "41.8 million"), page = 3:4,
                   section = c("Results", "Outlook"), stringsAsFactors = TRUE)
  b <- new_answer("Revenue was 45.2 million.", "my_reader", "What was revenue?", 1:2, tr,
                  evidence = ev)
  out <- capture.output(print(b))
  expect_true(any(grepl("chunk 1 \\(p\\. 3, \"Results\"\\)", out)))
})

test_that("a warning whose message is not one clean string is still recorded as one", {
  rec <- readgpt:::warning_recorder()
  raise <- function(msg) withCallingHandlers(
    gr_style_warning(msg, "test_odd_message"),
    warning = function(w) { rec$record(w); invokeRestart("muffleWarning") })
  raise(sprintf("page(s) %s have no text layer", integer(0)))
  raise(sprintf("page %d has no text layer", c(2L, 5L)))
  raise("fonts not embedded")
  got <- rec$get()
  expect_length(got, 3L)
  expect_identical(names(got), rep("test_odd_message", 3L))
  expect_identical(unname(got[[3]]), "fonts not embedded")
  latin <- "odd layout in Jos\xe9.pdf"
  Encoding(latin) <- "latin1"
  raise(latin)
  expect_true(validUTF8(rec$get()[[4]]))
})

test_that("a recorded warning still names the function the user called", {
  local_clean_cache()
  w <- tryCatch(gr_ingest("reports/2024/annual-report.pfd"), warning = function(w) w)
  expect_s3_class(w, "gr_path_as_text")
  expect_identical(as.character(conditionCall(w)[[1]]), "gr_ingest")
})
