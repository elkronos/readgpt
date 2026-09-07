# test-inventory.R
#
# `gr_inventory()` and the three directory defects it was written alongside.
# All of these are about the same failure: a corpus run that quietly reads less
# than you gave it and reports success. Nothing here needs a model.

# A folder with the shapes that matter: files in subfolders, two files sharing a
# basename, an extension nothing claims, and an empty file.
local_corpus <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  dir.create(file.path(d, "2019")); dir.create(file.path(d, "2020"))
  writeLines(c("The 2019 cohort had 482 participants recruited across nine sites.",
               "", "Adherence exceeded 91 percent in the treatment arm."),
             file.path(d, "2019", "report.txt"))
  writeLines(c("The 2020 cohort had 611 participants.",
               "", "The primary endpoint was reached at week 24."),
             file.path(d, "2020", "report.txt"))
  writeLines(c("# Protocol", "", "Inclusion criteria follow."), file.path(d, "protocol.md"))
  writeLines("legacy notes", file.path(d, "old.doc"))          # nothing claims .doc
  file.create(file.path(d, "empty.txt"))
  d
}

test_that("a directory reports what it skipped instead of dropping it in silence", {
  # Point the package at 200 `.doc` files and it read zero of them and said
  # nothing at all -- the summary simply had no rows for them. An unreadable
  # corpus looked exactly like an empty one, which is the failure this package
  # treats as the serious kind everywhere else.
  d <- local_corpus()
  expect_warning(src <- readgpt:::corpus_sources(d, recursive = TRUE),
                 class = "gr_sources_skipped")
  w <- tryCatch(readgpt:::corpus_sources(d, recursive = TRUE), warning = function(w) w)
  expect_match(conditionMessage(w), "doc (1)", fixed = TRUE)
  expect_match(conditionMessage(w), "gr_inventory()", fixed = TRUE)

  # What it skipped travels with the result, not only in the warning text.
  expect_setequal(basename(attr(src, "skipped")), "old.doc")
  expect_identical(attr(src, "root"), d)
  expect_length(src, 4L)      # two reports, the protocol, and the empty file

  # `quiet` is for the inventory's own use, which reports skips as a table.
  expect_silent(readgpt:::corpus_sources(d, recursive = TRUE, quiet = TRUE))
})

test_that("a document keeps the folder it came from", {
  # `basename()` turned 2019/report.txt and 2020/report.txt into one name, and
  # make.unique() separated them as "report.txt" and "report.txt#1" -- throwing
  # away the meaningful half and replacing it with an index that depends on sort
  # order. Somebody who filed by year could not tell their rows apart.
  d <- local_corpus()
  src <- readgpt:::corpus_sources(d, recursive = TRUE, quiet = TRUE)
  labs <- vapply(src, readgpt:::corpus_label, character(1),
                 root = attr(src, "root"), USE.NAMES = FALSE)
  expect_true("2019/report.txt" %in% labs)
  expect_true("2020/report.txt" %in% labs)
  expect_false(any(grepl("#", labs, fixed = TRUE)))
  # A file at the top level is still named plainly.
  expect_true("protocol.md" %in% labs)
  # And it reaches the corpus summary that way.
  out <- quiet(gr_read_many(d, "How many participants?", "fast",
                            client = mock_echo(), recursive = TRUE))
  expect_true(all(c("2019/report.txt", "2020/report.txt") %in% out$summary$document))
})

test_that("corpus_label falls back to the basename off the root, and does not match a sibling", {
  # A root of /docs must not swallow /docs-old: `startsWith()` alone would.
  d <- local_corpus()
  outside <- withr::local_tempfile(fileext = ".txt")
  writeLines("elsewhere", outside)
  expect_identical(readgpt:::corpus_label(outside, root = d), basename(outside))
  expect_identical(readgpt:::relative_path(paste0(d, "-old/x.txt"), d), NA_character_)
  expect_identical(readgpt:::relative_path(file.path(d, "protocol.md"), paste0(d, "/")),
                   "protocol.md")
})

test_that("an empty directory says to try recursive when the files are one level down", {
  d <- withr::local_tempdir()
  dir.create(file.path(d, "deep"))
  writeLines("The cohort had 482 participants.", file.path(d, "deep", "a.txt"))
  err <- tryCatch(quiet(gr_read_many(d, "Q?", "fast", client = mock_echo())),
                  error = function(e) e)
  expect_s3_class(err, "gr_no_sources")
  expect_match(conditionMessage(err), "recursive = TRUE", fixed = TRUE)
  expect_match(conditionMessage(err), "1 readable file", fixed = TRUE)
  # And a directory that really is empty does not claim otherwise.
  err2 <- tryCatch(quiet(gr_read_many(withr::local_tempdir(), "Q?", "fast",
                                      client = mock_echo())), error = function(e) e)
  expect_false(grepl("recursive = TRUE", conditionMessage(err2), fixed = TRUE))
})

test_that("gr_inventory lists every file, including the ones that will not be read", {
  # A table of only the survivors cannot report "we skipped 180 of your files",
  # which is the finding.
  d <- local_corpus()
  inv <- gr_inventory(d)
  expect_s3_class(inv, "gr_inventory")
  expect_equal(nrow(inv$files), 5L)
  expect_true("old.doc" %in% inv$files$file)
  expect_identical(inv$files$status[inv$files$file == "old.doc"], "no_extractor")
  expect_true(is.na(inv$files$extractor[inv$files$file == "old.doc"]))
  expect_identical(inv$files$status[inv$files$file == "empty.txt"], "empty")

  # The folder survives here too, and is its own column to group on.
  expect_setequal(inv$files$folder, c(".", "2019", "2020"))
  expect_true("2019/report.txt" %in% inv$files$file)

  # Counted exactly for text, and the readable ones add up to the total.
  ready <- inv$files[inv$files$status == "ready", ]
  expect_true(all(ready$tokens > 0))
  expect_equal(inv$totals$tokens, sum(ready$tokens))
  expect_equal(inv$totals$files, 5L)
  expect_equal(inv$totals$readable, sum(inv$files$status %in% c("ready", "needs_ocr")))
  expect_false(is.na(inv$totals$cost_floor_usd))
  expect_output(print(inv), "would be skipped")
})

test_that("gr_inventory tells a scanned PDF from a digital one before anything is read", {
  # This is the one that turns into a confident wrong answer: a PDF with no text
  # layer extracts to nothing, gets read, and answers NOT_IN_DOCUMENT --
  # indistinguishable from a document that genuinely does not say so.
  skip_if_not_installed("pdftools")
  skip_if_not_installed("magick")
  d <- withr::local_tempdir()

  grDevices::pdf(file.path(d, "digital.pdf"))
  graphics::plot.new()
  graphics::text(0.5, 0.5, "Revenue was 45.2 million dollars in fiscal 2024.")
  grDevices::dev.off()

  img <- magick::image_annotate(magick::image_blank(600, 800, "white"),
                                "SCANNED", size = 40, gravity = "center")
  magick::image_write(img, file.path(d, "scan.pdf"), format = "pdf")

  inv <- gr_inventory(d)
  digital <- inv$files[inv$files$file == "digital.pdf", ]
  scan <- inv$files[inv$files$file == "scan.pdf", ]
  expect_identical(digital$status, "ready")
  expect_gt(digital$tokens, 0)
  expect_identical(scan$status, "needs_ocr")
  # Its size is genuinely unknown until OCR runs. NA, never zero -- a zero here
  # would quietly shrink the estimate for the whole corpus.
  expect_true(is.na(scan$tokens))
  expect_equal(inv$totals$tokens_unknown, 1L)
  expect_equal(inv$totals$tokens, digital$tokens)
  expect_match(scan$note, "no text layer")
  expect_output(print(inv), "no text layer")

  # And the note names only what is actually missing, so nobody is sent to
  # install a package they already have.
  if (requireNamespace("magick", quietly = TRUE) &&
      !requireNamespace("tesseract", quietly = TRUE)) {
    expect_match(scan$note, "'tesseract' is not installed", fixed = TRUE)
    expect_false(grepl("magick", scan$note, fixed = TRUE))
  }
})

test_that("gr_inventory handles an empty folder and a plain list of paths", {
  empty <- gr_inventory(withr::local_tempdir())
  expect_equal(nrow(empty$files), 0L)
  expect_equal(empty$totals$files, 0L)
  expect_output(print(empty), "0 file")

  d <- local_corpus()
  some <- gr_inventory(c(file.path(d, "protocol.md"), file.path(d, "old.doc")))
  expect_equal(nrow(some$files), 2L)
  expect_true(is.na(some$root))
  # With no root to be relative to, the name is the basename.
  expect_setequal(some$files$file, c("protocol.md", "old.doc"))
})

test_that("gr_inventory is not recursive-by-accident and honours the flag", {
  d <- local_corpus()
  expect_equal(nrow(gr_inventory(d, recursive = FALSE)$files), 3L)   # top level only
  expect_equal(nrow(gr_inventory(d, recursive = TRUE)$files), 5L)
})


# ---------------------------------------------------------------------------
# Failing gracefully: one bad file costs one row, never the run.
#
# This is the contract `gr_read_many()` has always had for a corpus, asserted
# here for a whole folder of things that go wrong in different ways, and for
# `gr_inventory()` too -- a folder is surveyed precisely because nobody knows
# what is in it yet, so falling over on the first surprise is the one thing it
# must not do.
# ---------------------------------------------------------------------------

hostile_corpus <- function(env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  writeLines("The cohort had 482 participants recruited across nine sites.",
             file.path(d, "good.txt"))
  # Binary bytes wearing a .txt extension.
  writeBin(as.raw(c(0L, 1L, 2L, 255L, 254L, 128L, 129L, 0L, 0L)), file.path(d, "binary.txt"))
  # Text that is not UTF-8 -- the encoding case that breaks naive counting.
  writeBin(as.raw(c(0x63, 0x61, 0x66, 0xe9, 0x20, 0x6e, 0x61, 0xef, 0x76, 0x65, 0x0a)),
           file.path(d, "latin1.txt"))
  writeLines("%PDF-1.4 truncated garbage, not really a PDF", file.path(d, "corrupt.pdf"))
  file.create(file.path(d, "zero.txt"))
  d
}

test_that("gr_inventory gives every broken file a row instead of failing", {
  d <- hostile_corpus()
  inv <- suppressWarnings(gr_inventory(d))
  expect_equal(nrow(inv$files), 5L)
  # Every file is accounted for, and the good one is still read correctly.
  expect_identical(inv$files$status[inv$files$file == "good.txt"], "ready")
  expect_gt(inv$files$tokens[inv$files$file == "good.txt"], 0)
  expect_identical(inv$files$status[inv$files$file == "zero.txt"], "empty")
  expect_identical(inv$files$status[inv$files$file == "corrupt.pdf"], "unreadable")
  expect_match(inv$files$note[inv$files$file == "corrupt.pdf"], "PDF")
  # Nothing broken is silently counted as readable-and-free.
  bad <- inv$files$status %in% c("unreadable", "empty")
  expect_true(all(is.na(inv$files$tokens[bad]) | inv$files$tokens[bad] == 0))
})

test_that("a probe that raises still becomes a row, not an error", {
  # The probes guard what is foreseeable. This is the guard for what is not:
  # anything unanticipated in the per-file work has to land as a row saying so.
  d <- hostile_corpus()
  local_mocked_bindings(
    probe_text = function(path) stop("something nobody predicted"),
    .package = "readgpt")
  inv <- suppressWarnings(gr_inventory(d))
  expect_equal(nrow(inv$files), 5L)
  # A zero-byte file is settled before any probe runs, so it stays "empty" --
  # the ones that would have reached the probe are the ones under test.
  txt <- inv$files[which(inv$files$ext == "txt" & inv$files$bytes > 0), ]
  expect_gt(nrow(txt), 1L)
  expect_true(all(txt$status == "unreadable"))
  expect_match(txt$note[1], "something nobody predicted")
  expect_identical(inv$files$status[inv$files$file == "zero.txt"], "empty")
  # The size was measured before the probe ran, so the row still reports it.
  expect_true(all(txt$bytes > 0))
})

test_that("one unreadable document costs one row of a corpus run, not the run", {
  d <- hostile_corpus()
  out <- quiet(gr_read_many(d, "How many participants?", "fast",
                            client = mock_echo("AN ANSWER")))
  expect_equal(nrow(out$summary), 5L)
  ok <- out$summary$status == "ok"
  expect_true(any(ok))
  expect_true(any(!ok))
  # The good document answered, and nothing was spent on the ones that failed.
  expect_identical(out$summary$answer[out$summary$document == "good.txt"], "AN ANSWER")
  expect_true(all(out$summary$calls[!ok] == 0L))
  # Every failure names itself rather than sharing one generic message.
  errs <- out$summary$error[!ok]
  expect_true(all(!is.na(errs)))
  expect_gt(length(unique(errs)), 1L)

  # The order of the corpus does not matter: a broken FIRST document must not
  # take the rest with it.
  rev <- quiet(gr_read_many(rev(file.path(d, sort(list.files(d)))), "Q?", "fast",
                            client = mock_echo("AN ANSWER")))
  expect_true("ok" %in% rev$summary$status)
})

test_that("a transport failure on one document is reported as partial, not as an answer", {
  # The nastier case, because ingestion succeeded: the document was read, the
  # model call failed, and what comes back must not look like a finding.
  d <- hostile_corpus()
  n <- 0L
  flaky <- gr_mock_client(function(messages, params) {
    n <<- n + 1L
    if (n == 1L) stop("simulated 503 from the provider")
    "AN ANSWER"
  })
  writeLines("The second cohort had 611 participants.", file.path(d, "second.txt"))
  out <- quiet(gr_read_many(file.path(d, c("good.txt", "second.txt")), "Q?", "fast",
                            client = flaky))
  expect_equal(nrow(out$summary), 2L)
  expect_true(out$summary$partial[1])          # the one whose call failed
  expect_true(out$summary$not_found[1])        # and it did not invent an answer
  expect_false(out$summary$partial[2])
  expect_identical(out$summary$answer[2], "AN ANSWER")
})

test_that("on_error = 'stop' is the opt-in, and is not the default", {
  d <- hostile_corpus()
  expect_error(quiet(gr_read_many(d, "Q?", "fast", client = mock_echo(), on_error = "stop")))
  expect_no_error(quiet(gr_read_many(d, "Q?", "fast", client = mock_echo())))
})

test_that("gr_extract gives a failed document an NA row, not a wrong value", {
  d <- hostile_corpus()
  out <- quiet(gr_extract(d, gr_fields(n = gr_field("number of participants",
                                                    type = "integer")),
                          client = mock_echo('{"n": 482}')))
  expect_equal(nrow(out$table), 5L)
  ok <- out$table$status == "ok"
  expect_true(any(ok) && any(!ok))
  expect_equal(out$table$n[out$table$document == "good.txt"], 482L)
  # A document that could not be read reports nothing, rather than a value
  # borrowed from whichever document happened to be read before it.
  expect_true(all(is.na(out$table$n[!ok])))
})
