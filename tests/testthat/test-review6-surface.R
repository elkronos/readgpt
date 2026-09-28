# Regression tests for the medium and low findings in the package's surface:
# the Shiny app, the offline demo, the README, the CI workflow, run-tests.sh
# and the gr_answer help.

# The Shiny app's server, with `cl` standing in for the client it builds from a
# typed key. `fns` replaces functions the app calls (to record them), and the
# folder `dir` is the one root it may read.
shiny_app <- function(dir, cl, fns = list(), env = parent.frame()) {
  skip_if_not_installed("shiny")
  app <- system.file("shiny", "app.R", package = "readgpt")
  skip_if(!nzchar(app), "the Shiny app is not available")
  e <- new.env(parent = globalenv())
  e$gr_client <- function(model = NULL, api_key = NULL, ...) cl
  e$shinyApp <- function(ui, server, ...) server
  for (nm in names(fns)) assign(nm, fns[[nm]], envir = e)
  # The app attaches shiny; leave the search path as it was found.
  if (!"package:shiny" %in% search()) {
    withr::defer(try(detach("package:shiny", character.only = TRUE), silent = TRUE),
                 envir = env)
  }
  withr::with_envvar(c(GPTREAD_DOC_ROOTS = normalizePath(dir)),
                     quiet(sys.source(app, envir = e, keep.source = FALSE)))
  e
}

# The inputs a fresh page sends, for one document.
app_inputs <- function(session, file, ...) {
  args <- utils::modifyList(list(
    api_key = "not-a-real-key", model = "gpt-4o-mini", file = file,
    clean_preset = "standard", clean_steps = NULL, ocr = "auto",
    segmenter = "paragraph", max_tokens = 1200, overlap = 0, min_tokens = 0,
    parallel = FALSE, max_cost = 2, readers = "map_reduce", top_k = 6,
    cite = FALSE, temperature = NA), list(...))
  do.call(session$setInputs, args)
}

# A short document with three paragraphs.
revenue_lines <- function() {
  c("Revenue was 45.2 million dollars in fiscal 2024.", "",
    "Gross margin improved to 41 percent.", "",
    "Headcount at Sheffield was 214 at year end.")
}

# The package source root, for files the built package leaves out (.github,
# run-tests.sh); NULL when the tests run from an installed copy.
source_file <- function(...) {
  f <- testthat::test_path("..", "..", ...)
  if (file.exists(f)) normalizePath(f) else NULL
}

# ---------------------------------------------------------------------------
# surface-06: "custom" with nothing ticked means no cleaning.
# ---------------------------------------------------------------------------

test_that("custom cleaning with every step unticked cleans nothing", {
  local_registries()
  local_clean_cache()
  gr_options(verbose = FALSE)
  dir <- withr::local_tempdir()
  writeLines(c("The gain was attri-", "buted to the new plant.", "", "Page 3", "",
               "Revenue was 45.2 million dollars."), file.path(dir, "report.txt"))
  e <- shiny_app(dir, gr_mock_client(function(m, p) "unused"))
  f <- normalizePath(file.path(dir, "report.txt"))
  shiny::testServer(e$server, {
    # An empty checkbox group reaches the server as NULL.
    app_inputs(session, f, clean_preset = "custom", clean_steps = NULL)
    # NULL was handed on as gr_ingest_spec(clean = NULL): the standard preset.
    expect_identical(ingest_spec()$clean, character(0))
    session$setInputs(preview = 1)
    expect_match(output$chunk_preview, "Page 3", fixed = TRUE)
    expect_match(output$chunk_preview, "attri-", fixed = TRUE)
    # A ticked step still applies, and a preset is still a preset.
    session$setInputs(clean_steps = "page_numbers")
    expect_identical(ingest_spec()$clean, "page_numbers")
    session$setInputs(clean_preset = "minimal")
    expect_identical(ingest_spec()$clean, "minimal")
  })
})

# ---------------------------------------------------------------------------
# surface-04: a click queued during a run does not run it again.
# ---------------------------------------------------------------------------

test_that("a second click on Ask made during a run does not bill the run twice", {
  local_registries()
  local_clean_cache()
  gr_options(verbose = FALSE)
  dir <- withr::local_tempdir()
  writeLines(revenue_lines(), file.path(dir, "report.txt"))
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  e <- shiny_app(dir, cl)
  f <- normalizePath(file.path(dir, "report.txt"))
  shiny::testServer(e$server, {
    app_inputs(session, f)
    # The question and the click arrive together, as a blur and a click do.
    session$setInputs(question = "What was revenue?", go = 1)
    n <- length(cl$calls())
    expect_gt(n, 0L)
    expect_length(history(), 1L)
    # The box is cleared only when the handler returns, so a click queued
    # during the run arrives with the old question still in input$question.
    session$setInputs(go = 2)
    expect_identical(length(cl$calls()), n)
    expect_length(history(), 1L)
    # The clear reaches the server; asking the same question again is a new ask.
    session$setInputs(question = "")
    session$setInputs(question = "What was revenue?", go = 3)
    expect_length(history(), 2L)
    # And a different question asked straight away runs too.
    session$setInputs(question = "What was the margin?", go = 4)
    expect_length(history(), 3L)
  })
})

# ---------------------------------------------------------------------------
# surface-05: a run whose every request failed says so.
# ---------------------------------------------------------------------------

test_that("the app says why an answer is partial when every request failed", {
  local_registries()
  local_clean_cache()
  gr_options(verbose = FALSE)
  dir <- withr::local_tempdir()
  writeLines(revenue_lines(), file.path(dir, "report.txt"))
  # A mistyped key: every request comes back HTTP 401, recorded, not raised.
  cl <- gr_mock_client(function(m, p) {
    structure(list(ok = FALSE, text = "", error = "HTTP 401: Incorrect API key provided",
                   status = 401L, usage = list(input = 0L, output = 0L), model = "mock",
                   finish_reason = NA_character_, retryable = FALSE, raw = NULL),
              class = "gr_result")
  })
  e <- shiny_app(dir, cl)
  f <- normalizePath(file.path(dir, "report.txt"))
  shiny::testServer(e$server, {
    app_inputs(session, f, max_tokens = 100, readers = c("map_reduce", "retrieve"))
    session$setInputs(question = "What was revenue?", go = 1)
    chat <- paste(unlist(output$chat), collapse = " ")
    # It read "NOT_IN_DOCUMENT ... chunk(s) used - PARTIAL" with no reason.
    expect_match(chat, "request(s) failed", fixed = TRUE)
    expect_match(chat, "HTTP 401", fixed = TRUE)
    expect_match(chat, "Not found in the part of the document that was read", fixed = TRUE)
    expect_false(grepl("NOT_IN_DOCUMENT", chat, fixed = TRUE))
    # The comparison table said error = NA beside not_found = TRUE.
    s <- last_cmp()
    expect_true(all(s$partial))
    expect_false(anyNA(s$partial_because))
    expect_match(s$partial_because[s$recipe == "map_reduce"], "request(s) failed", fixed = TRUE)
    expect_match(s$partial_because[s$recipe == "map_reduce"], "HTTP 401", fixed = TRUE)
  })
})

test_that("a complete answer shows no reason and its own text", {
  local_registries()
  local_clean_cache()
  gr_options(verbose = FALSE)
  dir <- withr::local_tempdir()
  writeLines(revenue_lines(), file.path(dir, "report.txt"))
  e <- shiny_app(dir, gr_mock_client(function(m, p) "Revenue was 45.2 million dollars."))
  f <- normalizePath(file.path(dir, "report.txt"))
  shiny::testServer(e$server, {
    app_inputs(session, f, readers = "stuff")
    session$setInputs(question = "What was revenue?", go = 1)
    chat <- paste(unlist(output$chat), collapse = " ")
    expect_match(chat, "Revenue was 45.2 million dollars.", fixed = TRUE)
    expect_false(grepl("PARTIAL", chat, fixed = TRUE))
    expect_true(is.na(last_cmp()$partial_because))
  })
})

test_that("a recipe that threw still shows its whole error", {
  local_registries()
  local_clean_cache()
  gr_options(verbose = FALSE)
  long <- paste("This reader refused to run, and here is a long and specific reason that",
                "runs well past one hundred and twenty characters: set top_k to 3 and retry.")
  gr_register_reader("boom", function(chunks, question, client, spec, trace) stop(long),
                     signature = "boom|0|none")
  dir <- withr::local_tempdir()
  writeLines(revenue_lines(), file.path(dir, "report.txt"))
  e <- shiny_app(dir, gr_mock_client(function(m, p) "unused"))
  f <- normalizePath(file.path(dir, "report.txt"))
  shiny::testServer(e$server, {
    app_inputs(session, f, readers = "boom")
    quiet(session$setInputs(question = "What was revenue?", go = 1))
    chat <- paste(unlist(output$chat), collapse = " ")
    expect_match(chat, "set top_k to 3 and retry.", fixed = TRUE)
    expect_match(last_cmp()$partial_because, "set top_k to 3 and retry.", fixed = TRUE)
  })
})

# ---------------------------------------------------------------------------
# surface-03 / r2-locale-platform-portability-08: only a listed file is read,
# and nothing touches the filesystem with a path before it is checked.
# ---------------------------------------------------------------------------

test_that("the app reads only the files it lists, and never touches a crafted path", {
  local_registries()
  local_clean_cache()
  gr_options(verbose = FALSE)
  dir <- withr::local_tempdir()
  dir.create(file.path(dir, ".private"))
  dir.create(file.path(dir, "sub"))
  writeLines(revenue_lines(), file.path(dir, "report.txt"))
  writeLines("name,salary,ssn\nAlice,182000,123-45-6789", file.path(dir, ".private", "payroll.csv"))
  writeLines("OPENAI_API_KEY=sk-proj-SECRET123456", file.path(dir, ".env.txt"))
  writeLines("login admin password=hunter2", file.path(dir, "server.log"))
  writeLines("private note: hunter2", file.path(dir, "sub", ".notes.txt"))
  # Every filesystem call the app makes on a path, recorded.
  seen <- character(0)
  rec <- function(f) function(x, ...) { seen <<- c(seen, as.character(x)); f(x, ...) }
  e <- shiny_app(dir, gr_mock_client(function(m, p) "unused"),
                 fns = list(normalizePath = rec(base::normalizePath),
                            file.exists = rec(base::file.exists),
                            dir.exists = rec(base::dir.exists)))
  root <- normalizePath(dir, winslash = "/")
  good <- file.path(root, "report.txt")
  crafted <- c(file.path(root, ".private", "payroll.csv"), file.path(root, ".env.txt"),
               file.path(root, "server.log"), file.path(root, "sub", ".notes.txt"),
               file.path(root, "sub", "..", "report.txt"),
               "\\\\203.0.113.5\\s\\a.pdf", "//203.0.113.5/s/a.pdf", "/net/203.0.113.5/s/a.pdf")
  shiny::testServer(e$server, {
    app_inputs(session, good)
    expect_setequal(offered()$files, good)
    session$setInputs(preview = 1)
    expect_match(output$chunk_preview, "45.2 million", fixed = TRUE)
    for (i in seq_along(crafted)) {
      seen <<- character(0)
      session$setInputs(file = crafted[i], preview = 1 + i)
      # The preview is left as the listed file made it.
      expect_false(grepl("123-45|SECRET|hunter2", output$chunk_preview))
      # It was stat()ed, and normalised, before being refused: on Windows a UNC
      # path opens an SMB session with the server's credentials.
      expect_false(crafted[i] %in% seen, label = crafted[i])
    }
    # A crafted root names no folder.
    session$setInputs(root = "../../etc", file = good, preview = 99)
    expect_length(offered()$files, 0L)
  })
  expect_null(e$safe_path("\\\\203.0.113.5\\s\\a.pdf", good))
  expect_null(e$safe_path(c(good, good), good))
  expect_null(e$safe_path(NA_character_, good))
  expect_identical(e$safe_path(good, good), good)
})

test_that("a listed file that is a symlink out of the root is still refused", {
  skip_on_os("windows")
  local_registries()
  gr_options(verbose = FALSE)
  dir <- withr::local_tempdir()
  outside <- withr::local_tempdir()
  writeLines("secret outside the root", file.path(outside, "secret.txt"))
  skip_if_not(file.symlink(file.path(outside, "secret.txt"), file.path(dir, "link.txt")))
  e <- shiny_app(dir, gr_mock_client(function(m, p) "unused"))
  listed <- e$list_documents(e$ALLOWED_ROOTS[[1]])
  expect_length(listed, 1L)
  expect_null(e$safe_path(listed, listed))
})

# ---------------------------------------------------------------------------
# surface-09: the app does not offer readers it cannot run.
# ---------------------------------------------------------------------------

test_that("the app offers every reader except the two that need a protocol", {
  local_registries()
  dir <- withr::local_tempdir()
  e <- shiny_app(dir, gr_mock_client(function(m, p) "unused"))
  expect_false(any(c("extract", "screen") %in% e$read_choices))
  expect_setequal(e$read_choices, setdiff(gr_readers()$name, c("extract", "screen")))
})

# ---------------------------------------------------------------------------
# surface-10: a blank cost field keeps the session's cap.
# ---------------------------------------------------------------------------

test_that("clearing the cost field keeps the R session's cap, and the label says so", {
  local_registries()
  local_clean_cache()
  gr_options(verbose = FALSE, max_cost_usd = 3)
  dir <- withr::local_tempdir()
  writeLines(revenue_lines(), file.path(dir, "report.txt"))
  cap_in_run <- list()
  cl <- gr_mock_client(function(m, p) {
    cap_in_run[[length(cap_in_run) + 1L]] <<- gr_options("max_cost_usd")
    "Revenue was 45.2 million dollars."
  })
  e <- shiny_app(dir, cl)
  expect_match(e$cap_label, "blank = this R session's cap, $3", fixed = TRUE)
  f <- normalizePath(file.path(dir, "report.txt"))
  shiny::testServer(e$server, {
    # A cleared numericInput sends NA; the run went with no limit at all.
    app_inputs(session, f, readers = "stuff", max_cost = NA)
    session$setInputs(question = "What was revenue?", go = 1)
    expect_length(cap_in_run, 1L)
    expect_identical(cap_in_run[[1]], 3)
    # A typed cap still applies, for the run only.
    session$setInputs(max_cost = 0.5, question = "What was the margin?", go = 2)
    expect_identical(cap_in_run[[2]], 0.5)
  })
  expect_identical(gr_options("max_cost_usd"), 3)

  # With no session cap, the label says blank is no limit.
  gr_options(max_cost_usd = NULL)
  e2 <- shiny_app(dir, cl)
  expect_match(e2$cap_label, "blank = no limit", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# surface-08: the shipped demo runs to the end.
# ---------------------------------------------------------------------------

test_that("the offline demo runs every section and every reader", {
  local_registries()
  local_clean_cache()
  demo <- system.file("examples", "demo.R", package = "readgpt")
  skip_if(!nzchar(demo), "the demo is not available")
  out <- quiet(utils::capture.output(
    sys.source(demo, envir = new.env(parent = globalenv()), keep.source = FALSE)))
  # It stopped at 'extract', which needs fields, and nothing after it ran.
  for (r in gr_readers()$name) {
    expect_true(any(grepl(sprintf("^  %s ", r), out)), label = r)
  }
  expect_true(any(grepl("every reader survives a dead API +: TRUE", out)))
  expect_identical(trimws(out[length(out) - 1L]), "Done")
})

# ---------------------------------------------------------------------------
# surface-13: the help says what an answer from gr_compare() carries.
# ---------------------------------------------------------------------------

test_that("an answer from gr_compare carries its recipe's read, and the help says so", {
  local_registries()
  local_clean_cache()
  gr_options(verbose = FALSE)
  cmp <- quiet(gr_compare(readgpt_example(), "What was revenue?",
                          list(gr_recipe("a", segment = "paragraph", read = "stuff"),
                               gr_recipe("b", segment = "sentence", read = "stuff")),
                          client = mock_echo()))
  labels <- function(tr) vapply(tr$steps, function(s) s$label, character(1))
  # Each recipe segments on its own trace (state-concurrency-07), so its
  # answer holds that segmentation and its read, and no other recipe's steps.
  recipes <- function(tr) unique(vapply(tr$steps, function(s) as.character(s$recipe %||% NA), ""))
  expect_true("segment" %in% labels(cmp$answers$a$trace))
  expect_false("b" %in% recipes(cmp$answers$a$trace))
  expect_true("segment" %in% labels(cmp$trace))
  f <- source_file("R", "readgpt-package.R")
  skip_if(is.null(f), "the package source is not available")
  txt <- paste(readLines(f, warn = FALSE), collapse = " ")
  # It said the trace was shared across recipes and recorded every one's calls.
  expect_false(grepl("shared across recipes, so it records every recipe's calls", txt,
                     fixed = TRUE))
  expect_match(txt, "holds only its own recipe's segmentation and", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# contracts-11: the README says what a conflict costs.
# ---------------------------------------------------------------------------

test_that("the README says a conflict keeps the first value unless resolve = 'model'", {
  f <- source_file("README.md")
  skip_if(is.null(f), "the README is not available")
  txt <- gsub("\\s+", " ", paste(readLines(f, warn = FALSE), collapse = " "))
  expect_false(grepl("costs one call where the document contradicts itself", txt, fixed = TRUE))
  expect_match(txt, "the earliest chunk's value is kept", fixed = TRUE)
  expect_match(txt, "`resolve = \"model\"` spends one call per disagreeing field", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# surface-11: CI's vignette step fails a vignette that errors or has no key.
# ---------------------------------------------------------------------------

test_that("the CI vignette step fails a vignette whose chunks error", {
  skip_on_cran()
  skip_if_not_installed("knitr")
  wf <- source_file(".github", "workflows", "tests.yaml")
  skip_if(is.null(wf), "the workflow is not available")
  y <- readLines(wf, warn = FALSE)
  at <- grep("- name: Verify every vignette runs with no key and no network", y, fixed = TRUE)
  expect_length(at, 1L)
  rest <- seq.int(at + 1L, length(y))
  start <- rest[grepl("Rscript -e '", y[rest], fixed = TRUE)][1]
  after <- seq.int(start + 1L, length(y))
  end <- after[grepl("^\\s*'\\s*$", y[after])][1]
  step <- withr::local_tempfile(fileext = ".R")
  writeLines(y[(start + 1L):(end - 1L)], step)

  dir <- withr::local_tempdir()
  dir.create(file.path(dir, "vignettes"))
  rmd <- function(name, ...) writeLines(c("---", paste("title:", name), "---", "", ...),
                                        file.path(dir, "vignettes", paste0(name, ".Rmd")))
  rmd("broken", "```{r}", "ans <- object_that_does_not_exist", "ans$answer", "```")
  rmd("nokey", "```{r}", "message(\"No API key found, so no request can be sent.\")", "```")
  # An error shown on purpose is not a failure.
  rmd("shown", "```{r error = TRUE}", "stop(\"shown on purpose\")", "```", "",
      "```{r}", "1 + 1", "```")
  rscript <- file.path(R.home("bin"), "Rscript")
  out <- withr::with_dir(dir, suppressWarnings(
    system2(rscript, step, stdout = TRUE, stderr = TRUE)))
  # All three "ran offline" and the step exited 0.
  expect_identical(attr(out, "status"), 1L)
  expect_true(any(grepl("broken.Rmd failed", out, fixed = TRUE)))
  expect_true(any(grepl("nokey.Rmd degraded", out, fixed = TRUE)))
  expect_true(any(grepl("shown.Rmd ran offline", out, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# surface-12: run-tests.sh installs what the package imports.
# ---------------------------------------------------------------------------

test_that("run-tests.sh needs every package readgpt imports, and knitr for --check", {
  skip_on_cran()
  skip_on_os("windows")
  sh <- source_file("run-tests.sh")
  skip_if(is.null(sh), "run-tests.sh is not available")
  skip_if(!nzchar(Sys.which("bash")), "bash is not available")
  root <- dirname(sh)
  needs <- function(...) {
    out <- withr::with_dir(root, withr::with_path(R.home("bin"), suppressWarnings(
      system2("bash", c("run-tests.sh", ...), stdout = TRUE, stderr = TRUE))))
    line <- grep("^Required: ", out, value = TRUE)
    expect_length(line, 1L)
    strsplit(sub("^Required: ", "", line), " ", fixed = TRUE)[[1]]
  }
  imports <- trimws(sub("[(].*", "", strsplit(
    read.dcf(file.path(root, "DESCRIPTION"), fields = "Imports")[1, 1], ",")[[1]]))
  imports <- setdiff(imports, rownames(utils::installed.packages(priority = "base")))
  got <- needs("--deps")
  # digest was missing, and a fresh library failed at R CMD INSTALL.
  expect_true(all(c(imports, "testthat", "withr") %in% got))
  expect_true("digest" %in% got)
  expect_false("knitr" %in% got)
  expect_true(all(c("knitr", "rmarkdown") %in% needs("--check", "--deps")))
})
