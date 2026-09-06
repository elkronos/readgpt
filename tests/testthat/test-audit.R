# test-audit.R -- the report a third party reads instead of running the code.
#
# The property under test is not that a file appears. It is that the file does
# not flatter the run: an audit showing only the rows that worked looks like
# diligence and is the opposite of it. So most of these stage a run with a known
# defect in it -- an invented quote, an unreadable file, a citation to a study
# that does not exist -- and check the report says so.

audit_fixture <- function(bad_quote = FALSE, extra = character(0)) {
  d <- withr::local_tempdir(.local_envir = parent.frame())
  writeLines("Revenue was 45.2 million dollars in fiscal 2024.", file.path(d, "north.txt"))
  writeLines("Revenue was 51.8 million dollars in fiscal 2025.", file.path(d, "south.txt"))
  writeLines("This memo is about the office machine and nothing else.", file.path(d, "noise.txt"))
  list(dir = d, sources = c(list.files(d, full.names = TRUE), extra))
}

audit_client <- function(bad_quote = FALSE, bad_citation = FALSE) {
  gr_mock_client(function(messages, params) {
    seen <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                  collapse = " ")
    line <- regmatches(seen, regexpr("Revenue was [0-9.]+ million[^.]*\\.", seen))
    if (grepl("<studies>", seen, fixed = TRUE)) {
      return(if (bad_citation) "North [study 1] and also [study 9]." else "North [study 1].")
    }
    if (grepl("screen", seen)) {
      if (!length(line)) {
        return(paste0('{"decision":"exclude","reason":"No revenue figure.",',
                      '"criterion":"Reports a revenue figure","quote":null}'))
      }
      return(sprintf(paste0('{"decision":"include","reason":"Reports revenue.",',
                            '"criterion":"Reports a revenue figure","quote":"%s"}'), line))
    }
    if (!length(line)) return("{}")
    q <- if (bad_quote) "A sentence that is nowhere in the document." else line
    sprintf('{"region":"%s","revenue":%s,"region__quote":"%s","revenue__quote":"%s"}',
            if (grepl("45.2", seen)) "north" else "south",
            if (grepl("45.2", seen)) "45.2" else "51.8", q, line)
  })
}

audit_protocol <- function() {
  gr_protocol("revenue", question = "How did revenue change across regions?",
              include = "Reports a revenue figure",
              exclude = "Is a forecast rather than a result",
              fields = gr_fields(region = "The region covered",
                                 revenue = gr_field("Revenue in millions", type = "number")),
              outline = c(Findings = "How revenue compares across regions"))
}

read_report <- function(...) {
  p <- quiet(gr_audit_report(withr::local_tempfile(fileext = ".html",
                                                   .local_envir = parent.frame()), ...))
  paste(readLines(p, warn = FALSE), collapse = "\n")
}

# Prose in the report is wrapped for the source, not for a regex. Collapse
# whitespace before matching a sentence, or a test breaks the next time a line
# is rewrapped and says nothing useful when it does.
flat <- function(h) gsub("[[:space:]]+", " ", h)

test_that("gr_flow() accounts for every source at every stage it reached", {
  fx <- audit_fixture()
  cl <- audit_client()
  s <- quiet(gr_screen(c(fx$sources, "no-such-file.txt"), audit_protocol(), client = cl))
  x <- quiet(gr_extract(s$included, audit_protocol(), client = cl, recipe = "fast"))
  fl <- gr_flow(s, x)

  n <- function(stage) fl$n[trimws(fl$stage) == stage]
  expect_identical(n("sources given"), 4L)
  expect_identical(n("screened"), 4L)
  # The arithmetic has to close: every screened source is in exactly one bucket.
  expect_identical(n("include") + n("exclude") + n("unclear") + n("could not be read"),
                   n("screened"))
  expect_identical(n("include"), 2L)
  expect_identical(n("exclude"), 1L)
  expect_identical(n("could not be read"), 1L)   # and it is COUNTED, not dropped
  expect_identical(n("extracted from"), 2L)

  # Either object alone works, and neither is an error.
  expect_equal(nrow(gr_flow(s)), 7L)
  expect_gt(nrow(gr_flow(extraction = x)), 0L)
  expect_equal(nrow(gr_flow()), 0L)
})

test_that("a duplicate leaves the screened count without leaving the report", {
  fx <- audit_fixture()
  dup <- file.path(fx$dir, "north-again.txt")
  writeLines(readLines(file.path(fx$dir, "north.txt")), dup)
  s <- quiet(gr_screen(c(fx$sources, dup), audit_protocol(), client = audit_client()))
  fl <- gr_flow(s)
  n <- function(stage) fl$n[trimws(fl$stage) == stage]
  expect_identical(n("sources given"), 4L)
  expect_identical(n("duplicates removed"), 1L)
  expect_identical(n("screened"), 3L)
})

test_that("the report names what went wrong, near the top", {
  # The whole point. A run with an invented quote, an unreadable file and a
  # citation to a study that does not exist must produce a report that says all
  # three, without the reader comparing columns by eye.
  fx <- audit_fixture()
  cl <- audit_client(bad_quote = TRUE, bad_citation = TRUE)
  p <- audit_protocol()
  s <- quiet(gr_screen(c(fx$sources, "no-such-file.txt"), p, client = cl))
  x <- quiet(gr_extract(s$included, p, client = cl, recipe = "fast"))
  y <- quiet(gr_synthesise(x, p, client = cl))
  h <- read_report(screening = s, extraction = x, synthesis = y, protocol = p)

  expect_match(flat(h), "could not be found in the chunk cited")
  expect_match(flat(h), "point at a row that does not exist")
  expect_match(flat(h), "no decision was recorded; these are outstanding")
  expect_match(flat(h), "values unsupported")
  # The failures are MARKED in the tables, not merely present in them -- a
  # reader should not have to compare columns by eye. Named precisely, because
  # asserting that some flag exists somewhere passes on any one of them.
  expect_true(grepl("<span class='flag'>FALSE</span>", h, fixed = TRUE))   # unverified span
  expect_true(grepl("class=\"num\"><span class='flag'>1</span>", h, fixed = TRUE))  # n_unverified
  expect_match(flat(h), "<p class='flag'>1 citation\\(s\\) point at a row")

  # And what the checking does NOT establish is stated, because a verification
  # column a reader over-reads is worse than no column.
  expect_match(flat(h), "does not confirm that the sentence supports the value")
})

test_that("a clean run does not claim failures it did not have", {
  fx <- audit_fixture()
  cl <- audit_client()
  p <- audit_protocol()
  s <- quiet(gr_screen(fx$sources, p, client = cl))
  x <- quiet(gr_extract(s$included, p, client = cl, recipe = "fast"))
  h <- read_report(screening = s, extraction = x, protocol = p)
  expect_match(flat(h), "every one was found in the chunk cited")
  expect_no_match(flat(h), "could not be found in the chunk cited")
})

test_that("text from the document cannot escape into the markup", {
  # The report is meant to be SHARED. Every quote in it was written by a model
  # or copied out of somebody's document, so interpolating it unescaped both
  # breaks the page on the first angle bracket and turns the file into a way of
  # running whatever the document contained.
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("The rate was <5% & \"unstable\", per <script>alert(1)</script> notes.", f)
  cl <- gr_mock_client(function(messages, params) {
    paste0('{"note":"<5% & unstable","note__quote":',
           '"The rate was <5% & \\"unstable\\", per <script>alert(1)</script> notes."}')
  })
  x <- quiet(gr_extract(f, gr_fields(note = "Any note about the rate"),
                        client = cl, recipe = "fast"))
  h <- read_report(extraction = x)

  expect_false(grepl("<script>alert", h, fixed = TRUE))
  expect_true(grepl("&lt;script&gt;alert", h, fixed = TRUE))
  expect_true(grepl("&lt;5%", h, fixed = TRUE))
  expect_false(grepl("was <5%", h, fixed = TRUE))

  # `&` must be escaped FIRST or the escapes escape each other.
  expect_identical(readgpt:::esc("a & b < c"), "a &amp; b &lt; c")
  expect_identical(readgpt:::esc("&lt;"), "&amp;lt;")
  expect_identical(readgpt:::esc(NA), "")
})

test_that("the file is complete and is UTF-8 whatever the locale", {
  # Two traps, both hit writing this. A connection passed inline to writeLines()
  # is never closed, so the last buffer is not flushed and the file stops mid-tag
  # at 4096 bytes. And file(encoding = "UTF-8") re-encodes FROM the native
  # encoding, so on a machine with no UTF-8 locale it mangles text that is
  # already UTF-8.
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("Le taux était de 45,2 — stable.", f)
  cl <- gr_mock_client(function(messages, params) {
    '{"note":"stable","note__quote":"Le taux était de 45,2 — stable."}'
  })
  x <- quiet(gr_extract(f, gr_fields(note = "The note"), client = cl, recipe = "fast"))
  path <- withr::local_tempfile(fileext = ".html")
  quiet(gr_audit_report(path, extraction = x, title = "Rapport — vérification"))

  lines <- readLines(path, warn = FALSE, encoding = "UTF-8")
  expect_identical(utils::tail(lines, 1), "</html>")     # complete, not truncated
  expect_true(any(grepl("—", lines, useBytes = FALSE)))
  raw <- readBin(path, "raw", file.size(path))
  expect_true(validUTF8(rawToChar(raw)))
})

test_that("the report takes whatever stages were run, and refuses nothing at all", {
  fx <- audit_fixture()
  cl <- audit_client()
  p <- audit_protocol()
  x <- quiet(gr_extract(fx$sources, p, client = cl, recipe = "fast"))

  h <- read_report(extraction = x)
  expect_match(flat(h), "What was extracted")
  expect_false(grepl("Screening decisions", h, fixed = TRUE))
  expect_false(grepl("What was written", h, fixed = TRUE))

  expect_error(gr_audit_report(tempfile()), class = "gr_bad_audit_input")
  expect_error(gr_audit_report(tempfile(), extraction = x$table),
               class = "gr_bad_audit_input")
  expect_error(gr_audit_report(tempfile(), extraction = x, protocol = list(question = "q")),
               class = "gr_bad_audit_input")
})

test_that("the cost table is named for the reader, not after the class", {
  fx <- audit_fixture()
  x <- quiet(gr_extract(fx$sources, audit_protocol(), client = audit_client(),
                        recipe = "fast"))
  h <- read_report(extraction = x)
  expect_match(h, "<td>extraction</td>")
  expect_false(grepl("gr_extraction<", h, fixed = TRUE))
})
