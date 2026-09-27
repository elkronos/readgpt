# test-review6-screen.R -- the sixth pass on screening and calibration: the
# medium and low findings in R/screen.R and R/calibrate.R.
#
# Each block names the finding and says what the code did before.

r6_screening <- function(dec, docs = sprintf("d%04d.pdf", seq_along(dec)), ...) {
  structure(list(table = data.frame(document = docs, decision = dec, reason = "r", ...,
                                    stringsAsFactors = FALSE)),
            class = "gr_screening")
}

r6_file <- function(x, env = parent.frame()) {
  f <- withr::local_tempfile(fileext = ".txt", .local_envir = env)
  writeLines(x, f)
  f
}

r6_screen_client <- function(json) {
  force(json)
  gr_mock_client(function(messages, params) json)
}

# ---------------------------------------------------------------------------
# model-output-12: the criterion was recorded as whatever the model wrote, so
# an exclusion on a criterion the protocol does not contain was accepted and
# the document was never shown to a person.
# ---------------------------------------------------------------------------

test_that("an exclusion on a criterion the protocol does not list goes to a person", {
  f <- r6_file("We randomly assigned 200 adults in Kenya. The trial was run in Kenya.")
  cl <- r6_screen_client(paste0('{"decision":"exclude","reason":"Run in Kenya.",',
                                '"criterion":"Conducted outside Europe",',
                                '"quote":"The trial was run in Kenya."}'))
  s <- quiet(gr_screen(f, question = "Does it work?",
                       include = "Reports a randomised comparison",
                       exclude = "Participants are children", client = cl))
  # Was: decision "exclude", criterion "Conducted outside Europe", no flag.
  expect_identical(s$table$decision, "unclear")
  expect_identical(s$table$criterion, "Conducted outside Europe")
  expect_false(s$table$criterion_valid)
  expect_match(s$table$reason, "not one of the protocol's criteria", fixed = TRUE)
  expect_match(s$table$reason, "Run in Kenya.", fixed = TRUE)
  expect_true(s$table$verified)
  expect_identical(s$table$status, "ok")
  expect_output(print(s), "1 unclear")
})

test_that("an exclusion naming the protocol's criterion stands, in the protocol's words", {
  f <- r6_file("A survey of 300 children in Leeds.")
  p <- c(inc = "Reports a randomised comparison", exc = "Participants are children")
  run <- function(criterion) {
    cl <- r6_screen_client(sprintf(paste0('{"decision":"exclude","reason":"Children.",',
                                          '"criterion":%s,"quote":null}'), criterion))
    quiet(gr_screen(f, question = "Q?", include = p[["inc"]], exclude = p[["exc"]],
                    client = cl))$table
  }
  # Case, spacing, a list marker, quotes and a closing full stop aside.
  t1 <- run('"- \\"participants  are CHILDREN.\\""')
  expect_identical(t1$decision, "exclude")
  expect_identical(t1$criterion, "Participants are children")
  expect_true(t1$criterion_valid)
  # A failed inclusion criterion is a reason to exclude too.
  t2 <- run('"Reports a randomised comparison"')
  expect_identical(t2$decision, "exclude")
  expect_true(t2$criterion_valid)
  # Naming none is not naming one of the protocol's.
  t3 <- run("null")
  expect_identical(t3$decision, "unclear")
  expect_true(is.na(t3$criterion_valid))
  expect_match(t3$reason, "without naming a criterion", fixed = TRUE)
})

test_that("an include or unclear is not changed by the check, only labelled", {
  f <- r6_file("We randomly assigned participants to two groups.")
  cl <- r6_screen_client(paste0('{"decision":"include","reason":"RCT.",',
                                '"criterion":"Something else entirely","quote":null}'))
  t <- quiet(gr_screen(f, question = "Q?", include = "Reports a randomised comparison",
                       client = cl))$table
  expect_identical(t$decision, "include")
  expect_false(t$criterion_valid)
  expect_identical(t$reason, "RCT.")
})

test_that("an answer restored from before the check is checked when the table is built", {
  # A store written before read_screen() checked the criterion restores the
  # model's exclusion as it was; the table holds it to the protocol anyway.
  ans <- list(notes = list(decision = "exclude", reason = "Outside Europe.",
                           criterion = "Conducted outside Europe", seen_tokens = 10L,
                           document_tokens = 10L, truncated = FALSE))
  tab <- readgpt:::screening_table("a.txt", list(a.txt = ans),
                                   data.frame(document = "a.txt", status = "restored",
                                              error = NA_character_, stringsAsFactors = FALSE),
                                   include = "Reports a randomised comparison",
                                   exclude = "Participants are children")
  expect_identical(tab$decision, "unclear")
  expect_false(tab$criterion_valid)
  expect_match(tab$reason, "Outside Europe.", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# screen-protocol-09: the blind sheet carried sampled_from = "excluded" on
# every row -- the model's decision, the thing `blind` keeps out of it -- and
# frame_n beside screened_n said the same by its ratio.
# ---------------------------------------------------------------------------

test_that("a blind sheet does not say which of the model's decisions it was drawn from", {
  scr <- r6_screening(c("include", "exclude", "exclude", "unclear", "exclude", "include"))
  for (of in c("excluded", "kept", "all")) {
    f <- withr::local_tempfile(fileext = ".csv")
    quiet(gr_reference(scr, n = 3, of = of, seed = 1, path = f))
    txt <- paste(readLines(f, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    # Was: "excluded" on every row, and frame_n 3 of screened_n 6.
    expect_false(grepl("exclu|kept|\"all\"|include|unclear", txt), info = of)
    d <- utils::read.csv(f, stringsAsFactors = FALSE)
    expect_false(any(c("frame_n", "screened_n", "model_decision") %in% names(d)), info = of)
    expect_length(unique(d$sampled_from), 1L)
    # The frame and its sizes still come back.
    back <- readgpt:::read_reference(f)
    expect_identical(attr(back, "of"), of)
    expect_identical(attr(back, "screened_n"), 6L)
  }
  # The in-memory frame shows it no more than the file does.
  ref <- gr_reference(scr, n = 3, of = "excluded", seed = 1)
  expect_false(any(as.matrix(as.data.frame(ref)) %in% c("excluded", "3")))
})

test_that("a blind sheet read back calibrates as the frame it was drawn from", {
  scr <- r6_screening(c(rep("exclude", 50), rep("include", 8), rep("unclear", 2)))
  f <- withr::local_tempfile(fileext = ".csv")
  quiet(gr_reference(scr, n = 10, of = "excluded", seed = 1, path = f))
  d <- utils::read.csv(f, stringsAsFactors = FALSE)
  d$human_decision <- c(rep("exclude", 9), "include")
  utils::write.csv(d, f, row.names = FALSE, na = "")
  cal <- quiet(gr_calibrate(scr, f))
  expect_identical(cal$frame$of, "excluded")
  expect_equal(cal$frame$frame_n, 50L)
  expect_equal(cal$projected$lost, 5)
  # Two blind sheets from different frames stacked are still two frames.
  k <- withr::local_tempfile(fileext = ".csv")
  quiet(gr_reference(scr, n = Inf, of = "kept", path = k))
  dk <- utils::read.csv(k, stringsAsFactors = FALSE)
  dk$human_decision <- "include"
  expect_error(gr_calibrate(scr, rbind(d, dk)), class = "gr_mixed_frame")
  # A key a spreadsheet upper-cased still reads.
  d$sampled_from <- toupper(d$sampled_from)
  expect_identical(quiet(gr_calibrate(scr, d))$frame$of, "excluded")
})

test_that("an unblinded sheet names its frame in plain words, as before", {
  scr <- r6_screening(c(rep("exclude", 20), rep("include", 5)))
  ref <- gr_reference(scr, n = 5, of = "excluded", seed = 1, blind = FALSE)
  expect_identical(unique(ref$sampled_from), "excluded")
  expect_identical(unique(ref$frame_n), 20L)
  expect_true("model_decision" %in% names(ref))
})

test_that("a key that is not one this package wrote is not read as a frame", {
  key <- readgpt:::frame_key("excluded", 40L, 50L, "salt")
  expect_identical(readgpt:::frame_unkey(key), list(of = "excluded", frame_n = 40L,
                                                   screened_n = 50L))
  broken <- paste0(substr(key, 1, 12), if (substr(key, 13, 13) == "0") "1" else "0",
                   substring(key, 14))
  expect_null(readgpt:::frame_unkey(broken))
  expect_null(readgpt:::frame_unkey("excluded"))
  expect_null(readgpt:::frame_unkey(NA))
})

# ---------------------------------------------------------------------------
# screen-protocol-08: model decisions were not normalised. Read back from a CSV
# written with na = "", an unread document's decision was "" and counted as an
# exclusion; "Include" from a spreadsheet counted as one too.
# ---------------------------------------------------------------------------

test_that("a screening table read back from a CSV calibrates as the one in memory", {
  tab <- data.frame(document = sprintf("d%02d.pdf", 1:6),
                    decision = c("include", "include", NA, NA, "exclude", "exclude"),
                    status = c("ok", "ok", "failed", "failed", "ok", "ok"),
                    duplicate_of = NA_character_, stringsAsFactors = FALSE)
  ref <- data.frame(document = tab$document,
                    human_decision = c("include", "exclude", "include", "include",
                                       "exclude", "exclude"), stringsAsFactors = FALSE)
  mem <- gr_calibrate(tab, ref, of = "all")
  f <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(tab, f, row.names = FALSE, na = "")
  back <- utils::read.csv(f, stringsAsFactors = FALSE)
  expect_identical(back$decision[3], "")
  cal <- gr_calibrate(back, ref, of = "all")
  # Was: tp 1, fn 2 -- the two unread documents listed as missed.
  expect_identical(cal$counts, mem$counts)
  expect_identical(nrow(cal$missed), 0L)
  expect_identical(cal$n, 4L)
  # Capitalised and padded, as a spreadsheet leaves them.
  up <- tab
  up$decision <- c(" Include", "INCLUDE", NA, NA, "Exclude ", "exclude")
  expect_identical(gr_calibrate(up, ref, of = "all")$counts, mem$counts)
  # A status that says the row was not read wins over its decision.
  st <- tab
  st$decision[3] <- "exclude"
  expect_identical(gr_calibrate(st, ref, of = "all")$counts, mem$counts)
  # And the sampling frame is drawn from the decided rows only.
  expect_identical(nrow(gr_reference(back, n = Inf, of = "all")), 4L)
})

test_that("a decision the screener cannot give is refused, not counted as an exclusion", {
  tab <- data.frame(document = c("a.pdf", "b.pdf"), decision = c("include", "failed"),
                    stringsAsFactors = FALSE)
  ref <- data.frame(document = c("a.pdf", "b.pdf"), human_decision = "include",
                    stringsAsFactors = FALSE)
  expect_error(gr_calibrate(tab, ref, of = "all"), class = "gr_bad_screening")
  expect_error(gr_reference(tab, of = "all"), class = "gr_bad_screening")
})

# ---------------------------------------------------------------------------
# screen-protocol-04: `of` and `positive` were taken as given. of = "exclude"
# fell through to the corpus-wide metrics with no warning; positive =
# "Include" made every study ineligible.
# ---------------------------------------------------------------------------

test_that("an `of` that names no frame is refused rather than taken as 'all'", {
  scr <- r6_screening(c(rep("exclude", 90), rep("include", 10)))
  ex <- as.data.frame(gr_reference(scr, n = 60, of = "excluded", seed = 1, blind = FALSE))
  ex$human_decision <- "exclude"
  ex$sampled_from <- NULL; ex$frame_n <- NULL; ex$screened_n <- NULL
  # Was: specificity 100% [94.0%, 100.0%], "reading avoided" 100%, no warning.
  expect_error(gr_calibrate(scr, ex, of = "exclude"), class = "gr_bad_setting")
  expect_error(gr_calibrate(scr, ex, of = c("excluded", "kept")), class = "gr_bad_setting")
  # Case and spacing are not a different frame.
  expect_identical(gr_calibrate(scr, ex, of = " Excluded ")$frame$of, "excluded")
})

test_that("`positive` is read the way the human decisions are", {
  scr <- r6_screening(c(rep("exclude", 10), rep("include", 10)))
  ref <- data.frame(document = sprintf("d%04d.pdf", 11:20), human_decision = "include",
                    stringsAsFactors = FALSE)
  # Was: "eligible among those kept" 0 of 10.
  cal <- gr_calibrate(scr, ref, of = "kept", positive = "Include")
  expect_equal(cal$metrics$estimate[cal$metrics$metric == "eligible among those kept"], 1)
  expect_error(gr_calibrate(scr, ref, of = "kept", positive = "eligible"),
               class = "gr_bad_setting")
})

test_that("an `of` that contradicts the file is said out loud, and not projected", {
  scr <- r6_screening(c(rep("exclude", 90), rep("include", 10)))
  ex <- gr_reference(scr, n = 60, of = "excluded", seed = 1)
  ex$human_decision <- "exclude"
  expect_warning(cal <- gr_calibrate(scr, ex, of = "all"), class = "gr_frame_override")
  expect_identical(cal$frame$of, "all")
  expect_true(is.na(cal$frame$frame_n))
  # Agreeing with the file is not an override.
  expect_no_warning(gr_calibrate(scr, ex, of = "excluded"))
  # A reference drawn from everything, claimed as exclusions, is not
  # projected across the exclusions with the size of the whole run.
  scr2 <- r6_screening(rep("exclude", 30))
  all <- gr_reference(scr2, n = 10, of = "all", seed = 2)
  all$human_decision <- "exclude"
  expect_warning(cal2 <- gr_calibrate(scr2, all, of = "excluded"), class = "gr_frame_override")
  expect_null(cal2$projected)
})

# ---------------------------------------------------------------------------
# screen-protocol-05: adequacy was the eligible count in every frame, so a
# large clean sample of exclusions -- 0.0% [0.0%, 0.8%] from 500 rows -- was
# "inadequate", said to rest on "those 0 observations".
# ---------------------------------------------------------------------------

test_that("a large clean sample of exclusions is adequate", {
  scr <- r6_screening(c(rep("exclude", 4800), rep("include", 200)))
  ex <- gr_reference(scr, n = 500, of = "excluded", seed = 1)
  ex$human_decision <- "exclude"
  cal <- gr_calibrate(scr, ex)
  expect_true(cal$adequate)
  expect_identical(cal$adequacy$rule, "interval")
  out <- capture.output(print(cal))
  expect_false(any(grepl("Hand-screen more|0 observations", out)))
})

test_that("a small sample of one stratum is inadequate for its interval, not its positives", {
  scr <- r6_screening(c(rep("exclude", 480), rep("include", 20)))
  ex <- gr_reference(scr, n = 20, of = "excluded", seed = 1)
  ex$human_decision <- "exclude"
  cal <- gr_calibrate(scr, ex)
  expect_false(cal$adequate)
  expect_match(cal$adequacy$note, "points wide", fixed = TRUE)
  out <- paste(capture.output(print(cal)), collapse = "\n")
  expect_match(out, "Hand-screen more", fixed = TRUE)
  expect_false(grepl("0 observations", out, fixed = TRUE))
  # Every record in the stratum judged: nothing is left to sample.
  kept <- gr_reference(scr, n = Inf, of = "kept")
  kept$human_decision <- rep(c("include", "exclude"), 10)
  ck <- gr_calibrate(scr, kept)
  expect_true(ck$adequate)
  expect_identical(ck$adequacy$rule, "whole frame")
  # The "all" frame keeps its rule: eligible studies for sensitivity.
  all <- r6_screening(c("include", "include", rep("exclude", 8)))
  ref <- data.frame(document = all$table$document,
                    human_decision = c("include", "include", rep("exclude", 8)),
                    stringsAsFactors = FALSE)
  ca <- gr_calibrate(all, ref, of = "all")
  expect_false(ca$adequate)
  expect_identical(ca$adequacy$rule, "positives")
  expect_output(print(ca), "only 2 eligible studies")
})

# ---------------------------------------------------------------------------
# screen-protocol-06: with screen_tokens below the first chunk, no call was
# made and the document came back "unclear" (status "ok"), reported as a
# decision on its opening.
# ---------------------------------------------------------------------------

test_that("a first chunk over screen_tokens is cut to the cap and screened", {
  f <- r6_file(c("We randomly assigned participants to two groups.",
                 paste(rep("filler words about the background of the study", 150),
                       collapse = " ")))
  cl <- r6_screen_client(paste0('{"decision":"include","reason":"RCT.",',
                                '"criterion":"Reports a randomised comparison",',
                                '"quote":"We randomly assigned participants to two groups."}'))
  s <- quiet(gr_screen(f, question = "Do statins work?",
                       include = "Reports a randomised comparison", client = cl,
                       screen_tokens = 300))
  # Was: 0 calls, "unclear", "not even the first chunk fits one prompt".
  expect_length(cl$calls(), 1L)
  expect_identical(s$table$decision, "include")
  expect_true(s$table$truncated)
  expect_gt(s$table$seen_tokens, 0L)
  expect_lte(s$table$seen_tokens, 300L)
  expect_true(s$table$verified)
  # The prompt carried only the opening.
  sent <- paste(vapply(cl$calls()[[1]]$messages, function(m) as.character(m$content), ""),
                collapse = "\n")
  expect_lt(gr_count_tokens(sent), 300L + 400L)
})

test_that("a cap too small to show any of the document fails the document, naming it", {
  f <- r6_file(paste(rep("filler words about the background of the study", 150),
                     collapse = " "))
  cl <- r6_screen_client('{"decision":"include","reason":"x","criterion":null,"quote":null}')
  s <- quiet(gr_screen(f, question = "Q?", include = "Reports a randomised comparison",
                       client = cl, screen_tokens = 2))
  expect_length(cl$calls(), 0L)
  expect_true(is.na(s$table$decision))
  expect_identical(s$table$status, "failed")
  expect_match(s$table$error, "screen_tokens", fixed = TRUE)
  # No chunks at all: nothing was judged, so no "unclear" either (was "unclear").
  empty <- new_chunks(character(0), "custom", gr_segment_spec("paragraph"))
  expect_error(quiet(gr_read(empty, "Q?", cl, list(reader = "screen", include = "c"))),
               class = "gr_empty_chunks")
})

# ---------------------------------------------------------------------------
# surface-07: gr_reference(seed =) called withr::with_seed(), and withr is only
# suggested.
# ---------------------------------------------------------------------------

test_that("a seeded draw needs no suggested package, and is the set.seed() draw", {
  expect_false(any(grepl("withr", deparse(gr_reference), fixed = TRUE)))
  scr <- r6_screening(c(rep("exclude", 80), rep("include", 10)))
  ref <- gr_reference(scr, n = 60, of = "excluded", seed = 1)
  set.seed(1)
  expected <- sprintf("d%04d.pdf", sort(sample(seq_len(80), 60)))
  expect_identical(ref$document, expected)
  # The session's stream is put back.
  set.seed(99); before <- .Random.seed
  gr_reference(scr, n = 5, of = "excluded", seed = 3)
  expect_identical(.Random.seed, before)
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-04: the sheet was written and read in the
# session's encoding. Under a C locale an accented name went out as
# "<U+00C9>vora.txt" and gr_calibrate() refused its own file; a sheet Excel
# re-saved as Windows-1252 stopped with "invalid multibyte string".
# ---------------------------------------------------------------------------

r6_accented <- function() {
  r6_screening(c("exclude", "exclude", "include"),
               docs = c("\u00c9vora.txt", "M\u00e1laga.txt", "plain.txt"),
               document_id = c("aa11", "bb22", "cc33"))
}

# The sheet at `path`, filled in, written back as `enc` would write it.
r6_fill <- function(path, enc = c("UTF-8", "BOM", "CP1252")) {
  enc <- match.arg(enc)
  b <- readBin(path, "raw", file.size(path))
  if (identical(b[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))) b <- b[-(1:3)]
  txt <- rawToChar(b)
  Encoding(txt) <- "UTF-8"
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  lines[-1] <- sub(",,", ",\"exclude\",", lines[-1], fixed = TRUE)
  body <- paste0(paste(lines, collapse = "\r\n"), "\r\n")
  out <- switch(enc, "UTF-8" = charToRaw(body),
                BOM = c(as.raw(c(0xEF, 0xBB, 0xBF)), charToRaw(body)),
                CP1252 = iconv(body, "UTF-8", "CP1252", toRaw = TRUE)[[1]])
  f <- tempfile(fileext = ".csv")
  writeBin(out, f)
  f
}

test_that("the sheet is UTF-8 with a byte-order mark, and reads back after Excel", {
  scr <- r6_accented()
  p <- withr::local_tempfile(fileext = ".csv")
  quiet(gr_reference(scr, n = Inf, of = "excluded", path = p))
  b <- readBin(p, "raw", file.size(p))
  expect_identical(b[1:3], as.raw(c(0xEF, 0xBB, 0xBF)))
  expect_gt(length(grepRaw(as.raw(c(0xC3, 0x89)), b)), 0L)
  for (enc in c("UTF-8", "BOM", "CP1252")) {
    cal <- quiet(gr_calibrate(scr, r6_fill(p, enc)))
    expect_identical(cal$n, 2L, info = enc)
  }
  # A name mangled on the way is matched by its document_id.
  ref <- readgpt:::read_reference(r6_fill(p))
  ref$document <- c("?vora.txt", "M?laga.txt")
  expect_identical(quiet(gr_calibrate(scr, ref))$n, 2L)
})

test_that("the sheet round-trips in a C locale", {
  scr <- r6_accented()
  old <- Sys.getlocale("LC_CTYPE")
  if (!nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", "C")))) skip("cannot set a C locale")
  on.exit(Sys.setlocale("LC_CTYPE", old), add = TRUE)
  p <- tempfile(fileext = ".csv")
  quiet(gr_reference(scr, n = Inf, of = "excluded", path = p))
  b <- readBin(p, "raw", file.size(p))
  # Was: "<U+00C9>vora.txt" in the file, and a mismatch on reading it back.
  expect_gt(length(grepRaw(as.raw(c(0xC3, 0x89)), b)), 0L)
  expect_identical(length(grepRaw(charToRaw("<U+"), b, fixed = TRUE)), 0L)
  expect_identical(quiet(gr_calibrate(scr, r6_fill(p)))$n, 2L)
  expect_identical(quiet(gr_calibrate(scr, r6_fill(p, "CP1252")))$n, 2L)
})

# ---------------------------------------------------------------------------
# screen-protocol-13: n was compared as given, so n = "5" took the whole frame
# ("5" >= 30 as strings) and NA stopped with "missing value where TRUE/FALSE
# needed".
# ---------------------------------------------------------------------------

test_that("n is a number, compared as one", {
  scr <- r6_screening(c(rep("exclude", 30), rep("include", 5)))
  expect_identical(nrow(gr_reference(scr, n = "5", of = "excluded", seed = 1)), 5L)
  expect_identical(gr_reference(scr, n = "5", of = "excluded", seed = 1)$document,
                   gr_reference(scr, n = 5, of = "excluded", seed = 1)$document)
  for (bad in list(NA, 0, -1, 2.5, c(3, 4), "five")) {
    expect_error(gr_reference(scr, n = bad, of = "excluded"), class = "gr_bad_setting")
  }
  expect_identical(nrow(gr_reference(scr, n = Inf, of = "excluded")), 30L)
})

# ---------------------------------------------------------------------------
# screen-protocol-11: a reference that repeats documents -- two reviewers'
# sheets stacked -- was counted twice, with no warning.
# ---------------------------------------------------------------------------

test_that("two reviewers' sheets stacked are reconciled first, not double-counted", {
  scr <- r6_screening(c(rep("exclude", 60), rep("include", 40)))
  r1 <- gr_reference(scr, n = 40, of = "all", seed = 2)
  r1$human_decision <- ifelse(r1$document %in% scr$table$document[61:100], "include", "exclude")
  r2 <- r1
  r2$human_decision[1] <- if (r2$human_decision[1] == "include") "exclude" else "include"
  # Was: n = 80, specificity from 73 "observations".
  expect_error(gr_calibrate(scr, rbind(r1, r2)), class = "gr_reference_conflict")
  expect_warning(cal <- gr_calibrate(scr, rbind(r1, r1)), class = "gr_reference_duplicate")
  expect_identical(cal$n, 40L)
  expect_identical(cal$counts, gr_calibrate(scr, r1)$counts)
  # A blank copy is an unfilled row, not a disagreement.
  r3 <- r1
  r3$human_decision[1] <- NA
  expect_warning(expect_warning(cal3 <- gr_calibrate(scr, rbind(r1, r3)),
                                class = "gr_reference_duplicate"),
                 class = "gr_reference_incomplete")
  expect_identical(cal3$n, 40L)
})

# ---------------------------------------------------------------------------
# screen-protocol-10: the screening quote was always attributed to the first
# chunk sent, its page and its section, while it was verified against them all.
# ---------------------------------------------------------------------------

test_that("the screening quote is attributed to the chunk it is in", {
  ch <- new_chunks(c("Title of the paper and background.", "More background on statins.",
                     "We randomly assigned 1,204 participants to two groups."),
                   "custom", gr_segment_spec("paragraph"), page = 1:3,
                   section = c("Intro", "Background", "Methods"))
  spec <- list(reader = "screen", include = "Reports a randomised comparison")
  ask <- function(quote) {
    cl <- r6_screen_client(sprintf(paste0('{"decision":"include","reason":"RCT.",',
                                          '"criterion":"Reports a randomised comparison",',
                                          '"quote":"%s"}'), quote))
    quiet(gr_read(ch, "Do statins work?", cl, spec))
  }
  a <- ask("We randomly assigned 1,204 participants to two groups.")
  # Was: chunk 1, page 1, "Intro".
  expect_identical(a$evidence$chunk_id, 3L)
  expect_identical(a$evidence$page, 3L)
  expect_identical(a$evidence$section, "Methods")
  expect_true(a$evidence$verified)
  expect_identical(a$chunks_used, 3L)
  # Across two chunks: verified, but in no one chunk.
  b <- ask("More background on statins. We randomly assigned 1,204 participants")
  expect_true(b$evidence$verified)
  expect_true(is.na(b$evidence$chunk_id))
  expect_true(is.na(b$evidence$section))
  expect_length(b$chunks_used, 0L)
  # Not in the excerpt: not verified, and not placed in chunk 1 either.
  c3 <- ask("A sentence that is not in the paper.")
  expect_false(c3$evidence$verified)
  expect_true(is.na(c3$evidence$chunk_id))
})

# ---------------------------------------------------------------------------
# corpus-trace-2 (handoff H2): the screening print counted embeddings requests
# as model calls.
# ---------------------------------------------------------------------------

test_that("the screening print counts model calls and embeddings requests apart", {
  tr <- gr_trace()
  ok <- gr_result(TRUE, text = "x", usage = list(input = 5L, output = 1L), model = "gpt-4o")
  readgpt:::trace_record(tr, "screen.decide", list(), ok)
  for (i in 1:3) {
    readgpt:::trace_record(tr, "embed.request", list(),
                           gr_result(TRUE, text = "", usage = list(input = 5L, output = 0L),
                                     model = "text-embedding-3-small"),
                           embedding = TRUE)
  }
  s <- r6_screening("include")
  s$trace <- tr
  s$include <- "c"
  # Was: "this run: 4 model call(s)".
  expect_output(print(s), "this run: 1 model call(s), 3 embeddings request(s)", fixed = TRUE)
})
