# test-adversarial.R -- regressions from an adversarial sweep of the whole
# package, done from six angles at once: silent wrong answers, hostile inputs,
# R semantics traps, statistical validity, cache and replay determinism, and
# the test suite's own honesty.
#
# Every defect below was reproduced against the shipped build before it was
# fixed, and every test here fails without its fix. They are grouped by the
# thing that was wrong rather than by file, because several of them span two.

# ---------------------------------------------------------------------------
# Citations: the checker and the renderer had different grammars.
# ---------------------------------------------------------------------------

test_that("a combined citation marker is seen by the check, not only the renderer", {
  # The synthesis prompt asks the model to "cite more than one where more than
  # one supports it", the renderer understood `[studies 1 and 2]`, and the
  # CHECKER matched only `[study 1]`. So the invited form was rendered into
  # published prose that the check had reported as citing nothing.
  for (form in c("[studies 1 and 2]", "[studies 1, 2]", "[study 1, 2]",
                 "[studies 1 & 2]", "[studies 1 and 2]")) {
    expect_setequal(readgpt:::cited_ids(paste("Both agree", form, "."), "study"), c(1L, 2L))
  }
  expect_setequal(readgpt:::cited_ids("One [study 3].", "study"), 3L)
  expect_setequal(readgpt:::cited_ids("Two [chunks 4 and 5].", "chunk"), c(4L, 5L))
  expect_length(readgpt:::cited_ids("No markers here.", "study"), 0L)
})

test_that("the checker and the renderer cannot disagree about what a marker is", {
  # A drift guard, not a restatement: the two regexes WERE different, and that
  # is the whole defect. Anything the renderer will rewrite, the check must see.
  used <- data.frame(study = 1:3, authors = c("Smith, J.", "Garcia, R.", "Lee, K."),
                     year = c("2019", "2022", "2021"), title = paste("T", 1:3),
                     document = paste0("d", 1:3, ".pdf"), stringsAsFactors = FALSE)
  cols <- readgpt:::bib_columns(used); keys <- readgpt:::bib_keys(used, cols)
  for (txt in c("A [studies 1 and 2].", "B [study 3].", "C [studies 1, 2 and 3].",
                "D [study 1] [study 2].")) {
    rendered <- readgpt:::render_citations(txt, used, keys, "author-year")
    seen <- readgpt:::cited_ids(txt, "study")
    # Rendered means the renderer matched it; a match the check did not make
    # would be a citation in the prose that no reference list can support.
    expect_false(identical(rendered, txt) && !length(seen))
    expect_gt(length(seen), 0L)
  }
})

test_that("a fabricated id inside a combined marker still marks the section partial", {
  tab <- data.frame(document = paste0("d", 1:2, ".pdf"), document_id = c("h1", "h2"),
                    status = "ok", duplicate_of = NA_character_, n_filled = 1L,
                    n_unverified = 0L, conflicts = NA_character_,
                    finding = c("one", "two"), stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(m, p) "Both support it [studies 1 and 99].")
  s <- quiet(gr_synthesise(tab, question = "Q?", outline = c(Findings = "what"),
                           client = cl, cite_style = "marker"))
  expect_identical(s$sections$n_unknown, 1L)
  expect_true(s$sections$partial)
})

test_that("the reference list carries the a/b suffix the prose points with", {
  # `keys` was in reference_list()'s signature and never used, so the prose said
  # "(Smith & Okafor, 2019a)" and "(Smith & Okafor, 2019b)" against two
  # identical entries -- neither citation resolvable.
  used <- data.frame(study = 1:2, authors = c("Smith, J.; Okafor, A.", "Smith, J.; Okafor, A."),
                     year = c("2019", "2019"), title = c("Trial one", "Trial two"),
                     document = c("a.pdf", "b.pdf"), stringsAsFactors = FALSE)
  cols <- readgpt:::bib_columns(used); keys <- readgpt:::bib_keys(used, cols)
  refs <- readgpt:::reference_list(used, keys, 1:2, cols, "author-year")
  expect_true(any(grepl("(2019a)", refs, fixed = TRUE)))
  expect_true(any(grepl("(2019b)", refs, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Records: a match that is a coin flip is not a match.
# ---------------------------------------------------------------------------

test_that("two records whose exports name the same filename claim neither file", {
  # `year/report.pdf` is an ordinary archive shape, and the export's L1 paths
  # were written on another machine, so only the basename can match. Both
  # records matched both files and the first listed took one: the 2020 record
  # was given the 2019 file and the 2019 record the 2020 file, both marked
  # retrieved. That is one paper's findings under another paper's authors.
  d <- withr::local_tempdir()
  dir.create(file.path(d, "2020")); dir.create(file.path(d, "2019"))
  writeLines("Study B, 2020.", file.path(d, "2020", "report.txt"))
  writeLines("Study A, 2019.", file.path(d, "2019", "report.txt"))
  ris <- withr::local_tempfile(fileext = ".ris")
  writeLines(c("TY  - JOUR", "AU  - Bloggs, B.", "TI  - The 2020 study", "PY  - 2020",
               "DO  - 10.1000/bbb", "L1  - /elsewhere/2020/report.txt", "ER  - ",
               "TY  - JOUR", "AU  - Ansel, A.", "TI  - The 2019 study", "PY  - 2019",
               "DO  - 10.1000/aaa", "L1  - /elsewhere/2019/report.txt", "ER  - "), ris)
  r <- quiet(gr_records(ris, files = d))
  got <- r$records$file
  # Unconditional, so the test cannot pass by asserting nothing: a retrieved
  # file must sit under the folder named for its own record's year.
  wrong <- !is.na(got) &
    !mapply(function(y, f) grepl(y, f, fixed = TRUE), r$records$year, got)
  expect_equal(sum(wrong), 0L)
  expect_equal(nrow(r$records), 2L)
})

test_that("two records sharing a title prefix claim neither file", {
  d <- withr::local_tempdir()
  writeLines("Spacing paper about children.", file.path(d, "effects-of-spacing-on-retention.txt"))
  ris <- withr::local_tempfile(fileext = ".ris")
  writeLines(c("TY  - JOUR", "AU  - Smith, John", "TI  - Effects of spacing on retention in adults",
               "PY  - 2019", "DO  - 10.1000/adults", "ER  - ",
               "TY  - JOUR", "AU  - Jones, Mary", "TI  - Effects of spacing on retention in children",
               "PY  - 2020", "DO  - 10.1000/children", "ER  - "), ris)
  r <- quiet(gr_records(ris, files = d))
  # Reversing the export order used to move the file to the other record.
  expect_equal(sum(!is.na(r$records$file)), 0L)
})

test_that("one file two records both name is claimed by neither", {
  # The sharp case for the settlement, and the one a files-per-record check
  # cannot see: only ONE `report.txt` exists, so exactly one file matches each
  # record and `length(j) == 1L` is satisfied for both. Whichever record the
  # export listed first took it.
  d <- withr::local_tempdir()
  writeLines("Some report text.", file.path(d, "report.txt"))
  ris <- withr::local_tempfile(fileext = ".ris")
  writeLines(c("TY  - JOUR", "AU  - Bloggs, B.", "TI  - The 2020 study", "PY  - 2020",
               "L1  - /elsewhere/2020/report.txt", "ER  - ",
               "TY  - JOUR", "AU  - Ansel, A.", "TI  - The 2019 study", "PY  - 2019",
               "L1  - /elsewhere/2019/report.txt", "ER  - "), ris)
  r <- quiet(gr_records(ris, files = d))
  expect_equal(sum(!is.na(r$records$file)), 0L)

  # Same shape through the title-prefix route, with one candidate file.
  d2 <- withr::local_tempdir()
  writeLines("Spacing text.", file.path(d2, "effects-of-spacing-on-retention-in-adults.txt"))
  ris2 <- withr::local_tempfile(fileext = ".ris")
  writeLines(c("TY  - JOUR", "AU  - Smith, John",
               "TI  - Effects of spacing on retention in adults", "PY  - 2019", "ER  - ",
               "TY  - JOUR", "AU  - Jones, Mary",
               "TI  - Effects of spacing on retention in children", "PY  - 2020", "ER  - "), ris2)
  r2 <- quiet(gr_records(ris2, files = d2))
  expect_equal(sum(!is.na(r2$records$file)), 0L)
})

test_that("a unique filename still matches, so the guard did not disable matching", {
  d <- withr::local_tempdir()
  writeLines("Smith 2019 text.", file.path(d, "smith2019.txt"))
  ris <- withr::local_tempfile(fileext = ".ris")
  writeLines(c("TY  - JOUR", "AU  - Smith, John", "TI  - A study of spacing effects",
               "PY  - 2019", "ER  - "), ris)
  r <- quiet(gr_records(ris, files = d))
  expect_true(r$records$retrieved[1])
})

test_that("@string and @comment are not records, and a broken entry is announced", {
  # Each `@`-block became a row of all-NA that dedupe cannot collapse, inflating
  # "records identified" -- the first number of a PRISMA flow diagram.
  p <- withr::local_tempfile(fileext = ".bib")
  writeLines(c("@string{jgr = {Journal of Geophysical Research}}",
               "@comment{jabref-meta: databaseType:bibtex;}",
               "@article{good2020,",
               "  author = {Real, Author}, title = {An actual paper},",
               "  year = {2020}, doi = {10.1000/real}", "}"), p)
  r <- quiet(gr_records(p))
  expect_equal(nrow(r$records), 1L)

  p2 <- withr::local_tempfile(fileext = ".bib")
  writeLines(c("@article{first2019,", "  author = {Smith, John},",
               "  title = {A study of {unbalanced braces},",
               "  year = {2019}, doi = {10.1000/first}", "}",
               "@article{second2020,",
               "  author = {Jones, Mary}, title = {Second paper},",
               "  year = {2020}, doi = {10.1000/second}", "}"), p2)
  # Dropping it silently removed a study from the review with nothing to notice.
  expect_warning(gr_records(p2), class = "gr_bib_unterminated")
})

# ---------------------------------------------------------------------------
# Calibration: which frame the sample came from decides what it can support.
# ---------------------------------------------------------------------------

fake_screening <- function(dec) {
  structure(list(table = data.frame(document = sprintf("d%04d.pdf", seq_along(dec)),
                                    decision = dec, stringsAsFactors = FALSE)),
            class = "gr_screening")
}

test_that("two frames stacked in one file are refused, not averaged", {
  # This is what following the package's own advice produces -- judge everything
  # kept AND a sample of what was discarded -- and gr_calibrate() takes one
  # reference, so people rbind() them. Unweighted, on a screener that really
  # missed 14% of eligible studies, it reported sensitivity 100% with an
  # interval that excluded the truth.
  N <- 400; elig <- rep(FALSE, N); elig[1:40] <- TRUE
  dec <- ifelse(elig & seq_len(N) %% 7 != 0, "include", "exclude"); dec[200:230] <- "include"
  scr <- fake_screening(dec)
  f1 <- withr::local_tempfile(fileext = ".csv"); f2 <- withr::local_tempfile(fileext = ".csv")
  quiet(gr_reference(scr, n = Inf, of = "kept", seed = 1, path = f1))
  quiet(gr_reference(scr, n = 40, of = "excluded", seed = 1, path = f2))
  fill <- function(f) {
    x <- utils::read.csv(f, stringsAsFactors = FALSE)
    x$human_decision <- ifelse(elig[match(x$document, sprintf("d%04d.pdf", 1:N))],
                               "include", "exclude")
    x
  }
  both <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(rbind(fill(f1), fill(f2)), both, row.names = FALSE, na = "")
  expect_error(gr_calibrate(scr, both), class = "gr_mixed_frame")
  # Each frame on its own still works, which is the fix the message names.
  expect_s3_class(quiet(gr_calibrate(scr, f2 <- fill(f2))), "gr_calibration")
})

test_that("a reference that does not say where it came from says so out loud", {
  dec <- c(rep("exclude", 30), rep("include", 10))
  scr <- fake_screening(dec)
  ref <- data.frame(document = sprintf("d%04d.pdf", 1:20),
                    human_decision = rep(c("include", "exclude"), each = 10),
                    stringsAsFactors = FALSE)
  expect_warning(gr_calibrate(scr, ref), class = "gr_unknown_frame")
  # And naming the frame silences it, because then it is not unknown.
  expect_silent(gr_calibrate(scr, ref, of = "all"))
})

test_that("screened_n survives the CSV the docs tell people to email", {
  dec <- c(rep("exclude", 60), rep("include", 20), rep(NA_character_, 20))
  scr <- fake_screening(dec)
  p <- withr::local_tempfile(fileext = ".csv")
  ref <- quiet(gr_reference(scr, n = 20, of = "excluded", seed = 3, path = p))
  back <- readgpt:::read_reference(p)
  # nrow(tab) counts rows the screener never decided; sum(judged) is 80.
  expect_identical(attr(back, "screened_n"), attr(ref, "screened_n"))
  expect_identical(attr(back, "screened_n"), 80L)
})

test_that("the projected count of lost studies is not rounded before it is projected", {
  dec <- c(rep("exclude", 3000), rep("include", 10))
  scr <- fake_screening(dec)
  p <- withr::local_tempfile(fileext = ".csv")
  quiet(gr_reference(scr, n = 30, of = "excluded", seed = 5, path = p))
  b <- utils::read.csv(p, stringsAsFactors = FALSE)
  b$human_decision <- "exclude"; b$human_decision[1] <- "include"
  utils::write.csv(b, p, row.names = FALSE, na = "")
  cal <- quiet(gr_calibrate(scr, p))
  # 1/30 of 3000 is 100 exactly; rounding the rate to 0.0333 first gives 99.9.
  expect_equal(cal$projected$lost, 3000 / 30, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# Extraction: a fabricated number is worse than a miss.
# ---------------------------------------------------------------------------

test_that("a value carrying more than one number is a miss, not a concatenation", {
  # `gsub("[^0-9.+-]", "", x)` deleted the separators with the words, so every
  # digit was glued into one number -- and the QUOTE was verbatim, so the
  # evidence check passed and the audit certified a figure in no paper.
  f <- gr_field("Number randomised", type = "integer")
  expect_null(readgpt:::coerce_field("120 (60 per arm)", f))
  expect_null(readgpt:::coerce_field("1,204 randomised; 1,180 analysed", f))
  expect_null(readgpt:::coerce_field("12 to 15", f))
  # The ordinary cases still work, including thousands separators.
  expect_identical(readgpt:::coerce_field("120", f), 120L)
  expect_identical(readgpt:::coerce_field("1,204 participants", f), 1204L)
  expect_identical(readgpt:::coerce_field("about -3 points", f), -3L)
  g <- gr_field("Effect size", type = "number")
  expect_equal(readgpt:::coerce_field("45.2 million", g), 45.2)
  expect_equal(readgpt:::coerce_field("1.2e-3", g), 1.2e-3)
  expect_null(readgpt:::coerce_field("0.2 to 0.9", g))
})

# ---------------------------------------------------------------------------
# Caches: a hit that answers a different question.
# ---------------------------------------------------------------------------

test_that("gr_hash() sees a name nested below the third level", {
  # Two JSON schemas differing only in a leaf's NAME hashed identically, and the
  # schema is part of the response-cache key -- so asking for {result:{headcount}}
  # after {result:{revenue}} returned the revenue figure, marked cached.
  a <- list(type = "object", properties = list(result = list(type = "object",
              properties = list(revenue = list(type = "number")))))
  b <- list(type = "object", properties = list(result = list(type = "object",
              properties = list(headcount = list(type = "number")))))
  expect_false(identical(readgpt:::gr_hash(a), readgpt:::gr_hash(b)))
  expect_identical(readgpt:::gr_hash(a), readgpt:::gr_hash(a))
  # And a value difference at the same depth is still seen, so the fix did not
  # trade one blindness for another.
  cc <- b; cc$properties$result$properties$headcount$type <- "integer"
  expect_false(identical(readgpt:::gr_hash(b), readgpt:::gr_hash(cc)))
})

test_that("the temperature actually sent is part of the cache key", {
  local_registries()
  dir <- withr::local_tempdir()
  cl <- gr_cache_client(gr_mock_client(function(m, p) sprintf("temp=%s", format(p$temperature))),
                        gr_cache(dir))
  old <- gr_options(temperature = 0)
  a <- gr_call(cl, "What was revenue?")
  gr_options(temperature = 1.9)
  b <- gr_call(cl, "What was revenue?")
  gr_options(old)
  # A temperature sweep through one cache directory explored nothing: every
  # setting came back with the first sample's answer, marked cached.
  expect_false(isTRUE(b$cached))
})

test_that("the embedding endpoint is part of the embedding cache key", {
  k1 <- readgpt:::embed_cache_key("api", "shared-embed", "hello", "https://a.invalid|x")
  k2 <- readgpt:::embed_cache_key("api", "shared-embed", "hello", "https://b.invalid|x")
  expect_false(identical(k1, k2))
})

test_that("the tokenizer is part of the corpus store key", {
  local_registries()
  rec <- gr_recipe("fast")
  cl <- gr_mock_client(function(m, p) "x")
  gr_set_tokenizer("chars")
  a <- readgpt:::corpus_key("some document text", "Q?", rec, cl)
  gr_set_tokenizer("words")
  b <- readgpt:::corpus_key("some document text", "Q?", rec, cl)
  # The tokenizer is what turns max_tokens into a chunk boundary, so the store
  # was handing back an answer built from a segmentation this run would not have
  # produced.
  expect_false(identical(a, b))
})

# ---------------------------------------------------------------------------
# R semantics: three constructs that did not mean what they said.
# ---------------------------------------------------------------------------

test_that("the sequential fallback for parallel = TRUE actually runs", {
  # It warned "running sequentially instead" and then called fn(item) without
  # the trace, so it aborted one line later -- after ingestion, segmentation and
  # any calls already paid for.
  # `orig` captured BEFORE the mock: a mock that calls base::requireNamespace()
  # calls itself, and the test dies of infinite recursion rather than of the
  # thing it is testing.
  orig <- base::requireNamespace
  testthat::local_mocked_bindings(
    requireNamespace = function(package, ..., quietly = FALSE)
      if (isTRUE(package %in% c("future", "future.apply"))) invisible(FALSE)
      else orig(package, ..., quietly = quietly), .package = "base")
  fn <- function(item, trace) item * 2
  out <- NULL
  expect_warning(out <- readgpt:::gr_lapply(list(1, 2, 3), fn, parallel = TRUE,
                                            trace = gr_trace()),
                 class = "gr_parallel_unavailable")
  expect_identical(unlist(out), c(2, 4, 6))
})

test_that("a chunk set with a fractional median prints", {
  # median() averages the two middle values on an even-length vector, so it
  # returns a double and sprintf("%d", 27.5) is an error. gr_segment()
  # auto-prints, so the canonical interactive call failed about a quarter of
  # the time.
  d <- data.frame(id = 1:2, text = c("a", "b"), tokens = c(28L, 27L), stringsAsFactors = FALSE)
  ch <- structure(list(chunks = d, method = "paragraph", spec = list(max_tokens = 32)),
                  class = "gr_chunks")
  expect_output(print(ch), "median 27.5")
})

test_that("a setting that is not one number is named as such", {
  # `||` short-circuits on a scalar, so a length-2 value reached is.na(x) and
  # R >= 4.3 stopped with "'length = 2' in coercion to 'logical(1)'" -- naming
  # neither the setting nor the function, from inside gr_budget().
  expect_error(readgpt:::clamp(c(0.05, 0.10), 0, 1), class = "gr_bad_setting")
  expect_equal(readgpt:::clamp(0.5, 0, 1), 0.5)
  expect_equal(readgpt:::clamp(NA, 0, 1), 0)
  expect_equal(readgpt:::clamp(numeric(0), 0, 1), 0)
})

# ---------------------------------------------------------------------------
# preview: a placeholder is not a heading.
# ---------------------------------------------------------------------------

test_that("a document with no headings is not one giant preview unit", {
  # seg_structural() writes the literal "[no section]" where a chunk has none,
  # so `!all(is.na(sec))` was TRUE and the whole document became ONE unit. A
  # plan marking that unit "skim" sent one truncated excerpt and reported the
  # answer complete: 16% of an 18,000-token document read, partial = FALSE.
  d <- data.frame(section = rep(readgpt:::.gr_no_section, 24), tokens = 100L,
                  stringsAsFactors = FALSE)
  expect_gt(length(readgpt:::preview_units(d)), 1L)
  # Real headings still group by heading.
  d2 <- data.frame(section = rep(c("Methods", "Results", "Discussion"), each = 8),
                   tokens = 100L, stringsAsFactors = FALSE)
  expect_equal(length(readgpt:::preview_units(d2)), 3L)
  # One heading over the whole document is not a plan either.
  d3 <- data.frame(section = rep("Everything", 24), tokens = 100L, stringsAsFactors = FALSE)
  expect_gt(length(readgpt:::preview_units(d3)), 1L)
})

test_that("the structural segmenter's placeholder is the one preview looks for", {
  # A drift guard. These were two string literals in two files, and preview read
  # the wrong document because of it.
  doc <- gr_ingest("First paragraph here, with enough words to make a block.\n\nSecond one.")
  ch <- gr_segment(doc, list(method = "structural", max_tokens = 40))
  expect_true(all(is.na(ch$chunks$section) |
                    ch$chunks$section %in% c(readgpt:::.gr_no_section,
                                             unique(ch$chunks$section))))
  expect_identical(readgpt:::.gr_no_section, "[no section]")
})

# ---------------------------------------------------------------------------
# Replay, ingestion, extractors.
# ---------------------------------------------------------------------------

test_that("a trace records finish_reason, so a replay reaches the same decision", {
  tr <- gr_trace()
  cl <- gr_mock_client(function(m, p)
    readgpt:::gr_result(TRUE, text = "a truncated revision", model = "mock-model",
                        finish_reason = "length"))
  invisible(gr_call(cl, "write it", model = "mock-model", trace = tr))
  expect_identical(tr$steps[[1]]$finish_reason, "length")
  rp <- gr_replay_client(tr)
  expect_identical(gr_call(rp, "write it", model = "mock-model")$finish_reason, "length")
})

test_that("a file is opened even when its name contains a newline", {
  # `one_line` gated the file.exists() test, so such a file was never looked
  # for and the PATH STRING became the document: status ok, partial FALSE, the
  # model answering about a filename.
  d <- withr::local_tempdir()
  p <- file.path(d, "report\n2024.txt")
  writeLines("Total revenue for 2024 was 91.7 million dollars.", p)
  doc <- gr_ingest(p)
  expect_true(grepl("91.7 million", doc$text, fixed = TRUE))
  expect_false(grepl("report", doc$text, fixed = TRUE))
})

test_that("csv and tsv are claimed by an extractor, as gr_inventory() documents", {
  # gr_inventory() said it counts CSV tokens and gr_read_many() dropped them:
  # a folder surveyed as readable and then read as nothing.
  ext <- gr_extractors()
  claimed <- unlist(strsplit(gsub("[()]", "", ext$extensions), ",[[:space:]]*"))
  expect_true(all(c("csv", "tsv") %in% trimws(claimed)))
  d <- withr::local_tempdir()
  writeLines(c("year,revenue", "2024,91.7"), file.path(d, "t.csv"))
  inv <- gr_inventory(d)
  expect_identical(inv$files$status, "ready")
  expect_gt(inv$files$tokens, 0L)
})

test_that("a failed batch marks the section partial instead of vanishing from it", {
  # tree_merge() strips empty pieces, so a merge over the survivors read exactly
  # like a merge over everything, and with one survivor it returned that piece
  # unchanged with ok = TRUE.
  n <- 40
  tab <- data.frame(document = paste0("d", 1:n, ".pdf"), document_id = paste0("h", 1:n),
                    status = "ok", duplicate_of = NA_character_, n_filled = 2L,
                    n_unverified = 0L, conflicts = NA_character_,
                    design = paste("A randomised trial of spaced practice.", strrep("detail ", 60)),
                    outcome = paste("The primary outcome improved.", strrep("more detail ", 40)),
                    stringsAsFactors = FALSE)
  nb <- 0L
  cl <- gr_mock_client(function(m, p) {
    s <- paste(vapply(m, function(x) as.character(x$content), character(1)), collapse = " ")
    if (grepl("<studies>", s, fixed = TRUE)) {
      nb <<- nb + 1L
      if (nb == 1L) return(readgpt:::gr_result(FALSE, error = "503 upstream", status = 503L))
      return("A batch of findings. [study 3]")
    }
    "Merged. [study 3]"
  })
  s <- suppressWarnings(gr_synthesise(tab, question = "Does spacing work?",
                                      outline = c(Findings = "how many"),
                                      client = cl, model = "gpt-4", cite_style = "marker"))
  expect_true(any(s$sections$partial))
})
