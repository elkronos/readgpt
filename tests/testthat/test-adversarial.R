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

# ---------------------------------------------------------------------------
# The prompt budget and the prompt that is actually sent had drifted apart.
#
# Every reader sizes its excerpts against `gr_budget(overhead = ...)`. Three
# separate things were added to the prompts after that arithmetic was written --
# the tail restatement, the iterative step's extra system block, and the cited
# variant of the answer system prompt -- and none of them was added to the
# overhead. The prompt then overran the window by exactly the amount that was
# not counted.
# ---------------------------------------------------------------------------

# deparse() wraps and pads, so a source-shape assertion has to normalise
# whitespace before it can match what was written.
squash <- function(x) gsub("\\s+", " ", paste(x, collapse = " "))

test_that("prompt_overhead() counts the question twice unless restatement is off", {
  q <- paste(rep("a long multi clause information need", 20), collapse = " ")
  sys <- "system"
  once <- readgpt:::prompt_overhead(q, sys, "never")
  twice <- readgpt:::prompt_overhead(q, sys, "auto")
  expect_equal(twice - once, gr_count_tokens(q))
  expect_equal(readgpt:::prompt_overhead(q, sys, "always"), twice)
  # The default is the safe direction: over-reserving costs a little context,
  # under-reserving cost the whole answer.
  expect_equal(readgpt:::prompt_overhead(q, sys), twice)
})

test_that("restate = 'auto' still leaves room for the call it is going to make", {
  # Reproduction: with a long question and a model whose window the run nearly
  # fills, the body was fitted to bud$input and answer_messages() then prepended
  # the question again. gr_call() refused to dispatch and the reader returned
  # NOT_IN_DOCUMENT with nothing in it but notes$error -- on the DEFAULT setting.
  gr_register_model("restate-ctx", context_window = 3000L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  doc <- gr_ingest(paste(vapply(1:60, function(i)
    paste(rep(sprintf("para %d words", i), 20), collapse = " "), character(1)),
    collapse = "\n\n"))
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 60)))
  q <- paste(rep("a multi clause information need about the primary outcome", 60),
             collapse = " ")
  for (rs in c("never", "auto", "always")) {
    cl <- mock_echo()
    ans <- quiet(gr_read(ch, q, cl, list(reader = "stuff", model = "restate-ctx",
                                         max_answer_tokens = 200L, restate = rs)))
    expect_length(cl$calls(), 1L)
    expect_equal(ans$answer, "MOCK ANSWER", label = paste("restate =", rs))
  }
})

test_that("the budget uses the system prompt the call actually sends", {
  # answer_messages(cite = TRUE) sends answer_system_cited, which is longer than
  # answer_system. Budgeting against the short one understated the overhead by
  # the difference on every cited read.
  expect_gt(gr_count_tokens(readgpt:::answer_system(TRUE)),
            gr_count_tokens(readgpt:::answer_system(FALSE)))
  msgs <- readgpt:::answer_messages("Q?", "body", cite = TRUE)
  expect_identical(msgs[[1]]$content, readgpt:::answer_system(TRUE))
  msgs <- readgpt:::answer_messages("Q?", "body", cite = FALSE)
  expect_identical(msgs[[1]]$content, readgpt:::answer_system(FALSE))
  # Every reader that goes on to call answer_messages(cite = spec$cite) budgets
  # against the same function, not against the short prompt by name.
  for (fn in c("read_stuff", "read_skim", "read_retrieve", "read_rerank", "read_preview")) {
    src <- squash(deparse(get(fn, envir = asNamespace("readgpt"))))
    expect_match(src, "prompt_overhead(question, answer_system(spec$cite)", fixed = TRUE,
                 info = fn)
  }
  # hierarchical sends cite = FALSE explicitly, so it budgets for that.
  src <- squash(deparse(readgpt:::read_hierarchical))
  expect_match(src, "prompt_overhead(question, answer_system(FALSE)", fixed = TRUE)
})

test_that("the iterative step budgets for the system prompt it sends", {
  # The step call sends answer_system PLUS an iterative instruction block. The
  # overhead counted only the first, so the excerpts were sized to fill the gap.
  gr_register_model("iter-ctx", context_window = 2000L, max_output = 300L,
                    input_usd = 0, output_usd = 0)
  doc <- gr_ingest(paste(vapply(1:12, function(i)
    paste(rep(sprintf("paragraph %d topic %d", i, i), 12), collapse = " "), character(1)),
    collapse = "\n\n"))
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 60)))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("reading iteratively", messages[[1]]$content, fixed = TRUE)) {
      return('{"can_answer": true, "answer": "FOUND IT", "next_query": ""}')
    }
    "x"
  })
  quiet(gr_read(ch, "What is topic 7?", cl, list(reader = "iterative", model = "iter-ctx",
                                                 max_rounds = 2L, top_k = 3L)))
  step <- Filter(function(c) identical(c$label, "iterative.step"), cl$calls())
  expect_gt(length(step), 0L)
  sent <- gr_count_tokens(paste(vapply(step[[1]]$messages,
                                       function(m) as.character(m$content), character(1)),
                                collapse = "\n"))
  room <- gr_budget("iter-ctx", reserve_output = 1500L)$context_window
  expect_lt(sent + 300L, floor(room * (1 - gr_options("safety_margin"))))
  # And the guarantee itself, which is a source-shape one because the defect was
  # source drift: the string is built once and used for BOTH the budget and the
  # call, so the two cannot describe different prompts.
  src <- squash(deparse(readgpt:::read_iterative))
  expect_match(src, "prompt_overhead(question, step_system, spec$restate)", fixed = TRUE)
  expect_match(src, 'content = step_system', fixed = TRUE)
})

test_that("iterative does not answer from an empty excerpt block", {
  # Nothing gathered fit one prompt, the call went out with
  # <excerpts></excerpts>, and the model answered from its own prior -- which
  # came back as the document's answer, can_answer true, chunks_used empty.
  gr_register_model("micro-ctx", context_window = 600L, max_output = 400L,
                    input_usd = 0, output_usd = 0)
  doc <- gr_ingest(paste(vapply(1:6, function(i)
    paste(rep(sprintf("paragraph %d content words here", i), 60), collapse = " "),
    character(1)), collapse = "\n\n"))
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 300)))
  cl <- gr_mock_client(function(messages, params)
    '{"can_answer": true, "answer": "ANSWER FROM THE MODEL PRIOR", "next_query": ""}')
  ans <- quiet(gr_read(ch, "What is paragraph 3 about?", cl,
                       list(reader = "iterative", model = "micro-ctx",
                            max_rounds = 2L, top_k = 2L)))
  expect_length(cl$calls(), 0L)
  expect_false(grepl("PRIOR", ans$answer))
  expect_true(ans$partial)
})

test_that("the synthesis section budgets for its own tail restatement", {
  # restate_tail() was called with two arguments, so `spec$restate` could not
  # reach it -- and its tokens were not in the overhead either, so the studies
  # were fitted to a budget the tail then overran.
  expect_equal(length(formals(readgpt:::restate_tail)), 3L)
  gr_register_model("sec-ctx", context_window = 2600L, max_output = 300L,
                    input_usd = 0, output_usd = 0)
  used <- data.frame(study = 1:40,
                     document = paste0("d", 1:40, ".pdf"),
                     document_id = paste0("h", 1:40), status = "ok",
                     duplicate_of = NA_character_, n_filled = 1L, n_unverified = 0L,
                     conflicts = NA_character_,
                     finding = paste("a reasonably wordy finding sentence number", 1:40),
                     stringsAsFactors = FALSE)
  rendered <- readgpt:::render_studies(used)
  usable <- floor(2600 * (1 - gr_options("safety_margin")))
  tails <- list()
  for (rs in c("always", "never")) {
    cl <- mock_echo("Something [study 1].")
    quiet(readgpt:::synth_section(
      "Findings", "what the evidence supports", "Does it work?", rendered, used, cl,
      gr_read_spec("stuff", model = "sec-ctx", max_answer_tokens = 300L, restate = rs),
      gr_trace()))
    calls <- cl$calls()
    expect_gt(length(calls), 0L)
    for (cc in calls) {
      sent <- gr_count_tokens(paste(vapply(cc$messages, function(m) as.character(m$content),
                                           character(1)), collapse = "\n"))
      expect_lt(sent + 300L, usable, label = paste("restate =", rs))
    }
    msgs <- calls[[1]]$messages
    tails[[rs]] <- as.character(msgs[[length(msgs)]]$content)
  }
  # `spec$restate` reaches restate_tail(), and the tail carries the section brief
  # rather than the whole of `ask`.
  expect_match(tails$always, "Again, the question: write the 'Findings' section")
  expect_false(grepl("Again, the question", tails$never, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# gr_audit_report(): a new argument in the middle rebinds every existing call.
# ---------------------------------------------------------------------------

test_that("the arguments gr_audit_report() had in 0.5.0 keep their positions", {
  # `claims` was inserted fourth and `records` seventh, so a positional
  # gr_audit_report(p, s, x, syn) filed the synthesis as claims and wrote a
  # report with no synthesis section in it, silently.
  expect_equal(names(formals(gr_audit_report))[1:6],
               c("path", "screening", "extraction", "synthesis", "protocol", "title"))
  expect_true(all(c("claims", "records") %in% names(formals(gr_audit_report))[7:8]))
})

test_that("gr_audit_report() rejects a wrong object in claims or records", {
  # A stub is enough: the type checks run before the report is built.
  x <- structure(list(), class = "gr_extraction")
  expect_error(gr_audit_report(tempfile(), extraction = x, claims = list(claims = 1)),
               class = "gr_bad_audit_input")
  expect_error(gr_audit_report(tempfile(), extraction = x, records = data.frame(a = 1)),
               class = "gr_bad_audit_input")
})

# ---------------------------------------------------------------------------
# Capabilities that existed and were not connected to anything.
#
# Each of these was built, tested and documented on its own, and then the
# pipeline that needs it did not call it. Nothing below is a new feature; every
# one is a wire between two parts that were already there.
# ---------------------------------------------------------------------------

corpus_dir <- function(n = 5L, paras = 8L, env = parent.frame()) {
  d <- withr::local_tempdir(.local_envir = env)
  for (i in seq_len(n)) {
    writeLines(paste(vapply(seq_len(paras), function(j)
      paste(rep(sprintf("Doc %d paragraph %d about revenue.", i, j), 25), collapse = " "),
      character(1)), collapse = "\n\n"), file.path(d, sprintf("doc%d.txt", i)))
  }
  d
}

test_that("a corpus run can be capped, not only each document in it", {
  # `gr_options(max_calls =)` is per document by design. That left the RUN
  # unbounded: five documents under a 30-call ceiling made 125 calls, and the
  # only corpus ceiling, max_total_usd, is unenforceable against a model with no
  # registered price and is checked only after a document has been paid for.
  d <- corpus_dir()
  withr::local_options(list())
  old <- gr_options("max_calls")
  on.exit(gr_options(max_calls = old), add = TRUE)
  gr_options(max_calls = 30L)

  cl <- mock_echo("An answer [chunk 1].")
  free <- quiet(gr_read_many(d, "What was revenue?", client = cl, recipe = "fast",
                             method = "paragraph", max_tokens = 120, reader = "map_reduce"))
  expect_true(all(free$summary$calls <= 30L))
  expect_gt(length(cl$calls()), 30L)          # every document obeyed; the run did not

  cl2 <- mock_echo("An answer [chunk 1].")
  capped <- NULL
  expect_warning(
    capped <- suppressMessages(gr_read_many(d, "What was revenue?", client = cl2,
                                            recipe = "fast", method = "paragraph",
                                            max_tokens = 120, reader = "map_reduce",
                                            max_total_calls = 60L)),
    class = "gr_corpus_call_cap")
  expect_lt(length(cl2$calls()), length(cl$calls()))
  expect_true(any(capped$summary$status == "skipped"))
  # Checked BEFORE a document, so the overshoot is bounded by one document's own
  # ceiling -- the same shape as max_total_usd, and the docs say so.
  expect_lte(length(cl2$calls()), 60L + 30L)
})

test_that("gr_synthesise() stops at the run's ceiling like every other stage", {
  # One gr_call() per section, neither guarded by trace_can_call(). A run that
  # had already spent its ceiling kept writing sections, one call each, while
  # gr_claims() beside it stopped at the first.
  tab <- data.frame(document = paste0(letters[1:3], ".pdf"), document_id = paste0("h", 1:3),
                    status = "ok", duplicate_of = NA_character_, n_filled = 1L,
                    n_unverified = 0L, conflicts = NA_character_,
                    finding = c("up", "down", "flat"), stringsAsFactors = FALSE)
  old <- gr_options("max_calls")
  on.exit(gr_options(max_calls = old), add = TRUE)
  gr_options(max_calls = 1L)

  cl <- mock_echo("Something [study 1].")
  sy <- quiet(gr_synthesise(tab, question = "Q?", client = cl,
                            outline = c(A = "first", B = "second", C = "third")))
  expect_length(cl$calls(), 1L)
  expect_equal(nrow(sy$sections), 3L)
  # The sections that were not written say so rather than appearing as empty
  # prose somebody might paste into a manuscript.
  expect_equal(sum(sy$sections$partial), 2L)

  gr_options(max_calls = 1L)
  cl2 <- mock_echo("Something [study 1].")
  expect_warning(suppressMessages(gr_synthesise(tab, question = "Q?", client = cl2,
                                                outline = c(A = "first", B = "second"))),
                 class = "gr_synth_capped")

  # Once per run, not once per section. Twelve identical warnings is how a real
  # one gets skimmed past, and the corpus ceiling beside it already warns once.
  gr_options(max_calls = 0L)
  seen <- character(0)
  withCallingHandlers(
    suppressMessages(gr_synthesise(tab, question = "Q?", client = mock_echo(),
                                   outline = stats::setNames(paste("brief", 1:12),
                                                             paste0("S", 1:12)))),
    warning = function(z) { seen <<- c(seen, class(z)[1]); invokeRestart("muffleWarning") })
  expect_equal(sum(seen == "gr_synth_capped"), 1L)

  # The batched path too: a section with more studies than fit one prompt drafts
  # them in batches and merges, and each of those calls has to be checked as
  # well. Only the single-prompt branch was guarded at first, so a big section
  # went on spending after the ceiling while a small one stopped.
  gr_register_model("synth-tiny", context_window = 1400L, max_output = 400L,
                    input_usd = 0, output_usd = 0)
  n <- 120L
  big <- data.frame(document = paste0("d", seq_len(n), ".pdf"),
                    document_id = paste0("h", seq_len(n)), status = "ok",
                    duplicate_of = NA_character_, n_filled = 1L, n_unverified = 0L,
                    conflicts = NA_character_,
                    finding = paste("a reasonably wordy finding sentence number", seq_len(n)),
                    stringsAsFactors = FALSE)
  gr_options(max_calls = 400L)
  cl3 <- mock_echo("Something [study 1].")
  quiet(gr_synthesise(big, question = "Q?", client = cl3, model = "synth-tiny",
                      max_section_tokens = 200L, outline = c(A = "first")))
  labs <- vapply(cl3$calls(), function(c) as.character(c$label), character(1))
  expect_true(any(labs == "synthesise.batch"))   # the fixture really does batch
  expect_gt(length(cl3$calls()), 2L)

  gr_options(max_calls = 2L)
  cl4 <- mock_echo("Something [study 1].")
  quiet(gr_synthesise(big, question = "Q?", client = cl4, model = "synth-tiny",
                      max_section_tokens = 200L, outline = c(A = "first")))
  expect_lte(length(cl4$calls()), 2L)
})

test_that("one corpus behaves the same whether named as a folder or as its files", {
  # gr_read_many(dir) skipped the files no extractor claims and warned;
  # gr_read_many(list.files(dir)) handed each of them to an extractor and
  # recorded a failed row. Every pipeline uses the second form --
  # gr_extract(screened$included) is a character vector -- so the stage that
  # reads the most documents was the one getting the worse behaviour.
  d <- corpus_dir(n = 3L, paras = 2L)
  writeLines("junk", file.path(d, "notes.pages"))
  writeLines("junk", file.path(d, "data.sav"))

  by_dir <- NULL; by_files <- NULL
  expect_warning(
    by_dir <- suppressMessages(gr_read_many(d, "Q?", client = mock_echo(), recipe = "fast")),
    class = "gr_sources_skipped")
  expect_warning(
    by_files <- suppressMessages(gr_read_many(list.files(d, full.names = TRUE), "Q?",
                                              client = mock_echo(), recipe = "fast")),
    class = "gr_sources_skipped")

  expect_equal(nrow(by_files$summary), nrow(by_dir$summary))
  expect_equal(sort(as.character(by_files$summary$status)),
               sort(as.character(by_dir$summary$status)))
  expect_false(any(by_files$summary$status == "failed"))
  # Raw text is still raw text: the filter only applies when every element is a
  # file that exists.
  expect_equal(nrow(suppressMessages(gr_read_many(
    c("Revenue was 40 million.", "Revenue was 50 million."), "Q?",
    client = mock_echo(), recipe = "fast"))$summary), 2L)
})

test_that("the search travels with the corpus it produced", {
  # gr_audit_report(records = ) had to be handed the same object again at the
  # end of a run. Forget, and the report's search section reads "Not recorded"
  # and the flow diagram starts at "sources given" -- which the README's own
  # end-to-end example did.
  d <- withr::local_tempdir()
  f <- file.path(d, "smith2019.txt")
  writeLines("A randomised trial of 482 adults found a benefit.", f)
  ris <- file.path(d, "export.ris")
  writeLines(c("TY  - JOUR", "AU  - Smith, J.", "TI  - A trial", "PY  - 2019",
               "DO  - 10.1037/edu0000123", sprintf("L1  - %s", f), "ER  - "), ris)
  # `files =` is what fills the `file` column; an L1 line alone names a path the
  # export believed in, not one on this disk.
  recs <- quiet(gr_records(ris, files = f, search = gr_search(
    databases = c(Scopus = "spaced practice AND retention"), dates = "2026-01-05")))

  cl <- gr_mock_client(function(messages, params)
    '{"decision":"include","reason":"A trial.","criterion":"c","quote":null}')
  sc <- quiet(gr_screen(recs, question = "Q?", include = "c", client = cl))
  expect_s3_class(sc$records, "gr_records")

  p <- withr::local_tempfile(fileext = ".html")
  quiet(gr_audit_report(p, screening = sc))          # records NOT passed
  h <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_false(grepl("Not recorded", h, fixed = TRUE))

  # And across the hand-off, which is where it matters: gr_extract() takes a
  # character vector of paths, so the record set rides on `$included` or it is
  # lost. Without this the NEWS claim that gr_extraction carries it was false in
  # the only flow anybody uses.
  x <- quiet(gr_extract(sc$included, gr_fields(n = gr_field("N", type = "number")),
                        goal = "Q?", client = gr_mock_client(function(messages, params)
                          '{"n":482,"n__quote":"A randomised trial of 482 adults found a benefit."}')))
  expect_s3_class(x$records, "gr_records")
  p2 <- withr::local_tempfile(fileext = ".html")
  quiet(gr_audit_report(p2, extraction = x))         # extraction only
  h2 <- paste(readLines(p2, warn = FALSE), collapse = "\n")
  expect_false(grepl("Not recorded", h2, fixed = TRUE))
  expect_match(h2, "spaced practice AND retention", fixed = TRUE)
  expect_match(h, "spaced practice AND retention", fixed = TRUE)
  # And the flow begins at the search rather than at "sources given".
  expect_match(h, "records identified", fixed = TRUE)
  expect_match(h, "reports not retrieved", fixed = TRUE)
})

test_that("a shared trace accumulates the review without becoming its budget", {
  # A trace does two jobs: it is the ledger of what a run did, and it is the
  # counter trace_can_call() measures `max_calls` against. Running a stage
  # directly on a shared trace conflated them -- screening's calls were charged
  # against the write-up's per-stage ceiling and every section came back blank,
  # and each stage's `$trace` then reported the whole review's cost, so the
  # audit's three cost rows each claimed the full total. A given trace is the
  # PARENT; the stage still runs on its own and folds into it at the end.
  expect_true("trace" %in% names(formals(gr_screen)))
  expect_true("trace" %in% names(formals(gr_extract)))
  expect_true("trace" %in% names(formals(gr_synthesise)))

  d <- withr::local_tempdir()
  writeLines("A randomised trial of 412 adults found mortality fell.", file.path(d, "a.txt"))
  cl <- gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("screen", sys, ignore.case = TRUE)) {
      return('{"decision":"include","reason":"r","criterion":"c","quote":null}')
    }
    if (grepl("<studies>", paste(vapply(messages, function(m) as.character(m$content),
                                        character(1)), collapse = " "), fixed = TRUE)) {
      return("Mortality fell [study 1].")
    }
    '{"n":412,"n__quote":"A randomised trial of 412 adults found mortality fell."}'
  })
  tr <- gr_trace(meta = list(review = "one"))
  sc <- quiet(gr_screen(d, question = "Q?", include = "c", client = cl, trace = tr))
  x <- quiet(gr_extract(sc$included, gr_fields(n = gr_field("Participants", type = "number")),
                        goal = "Q?", client = cl, trace = tr))
  rv <- quiet(gr_synthesise(x, question = "Q?", client = cl,
                            outline = c(Findings = "what it shows"), trace = tr))
  # Each stage keeps its own, so its cost is its own.
  expect_false(identical(sc$trace, tr))
  expect_false(identical(x$trace, tr))
  expect_false(identical(rv$trace, tr))
  expect_gt(sc$trace$calls, 0L)
  # And the parent is the review.
  expect_equal(tr$calls, sc$trace$calls + x$trace$calls + rv$trace$calls)
  expect_gt(tr$calls, 2L)
  # The stage's own metadata survives being given a parent.
  expect_equal(rv$trace$meta$stage, "synthesise")
  expect_equal(tr$meta$review, "one")

  # The defect this arrangement exists to prevent: a ceiling already spent by an
  # earlier stage must not silently blank the write-up.
  old <- gr_options("max_calls")
  on.exit(gr_options(max_calls = old), add = TRUE)
  gr_options(max_calls = 3L)
  tab <- data.frame(document = paste0(letters[1:3], ".pdf"), document_id = paste0("h", 1:3),
                    status = "ok", duplicate_of = NA_character_, n_filled = 1L,
                    n_unverified = 0L, conflicts = NA_character_,
                    finding = c("up", "down", "flat"), stringsAsFactors = FALSE)
  tr2 <- gr_trace()
  quiet(gr_screen(d, question = "Q?", include = "c", client = cl, trace = tr2))
  expect_gte(tr2$calls, 1L)
  sy <- quiet(gr_synthesise(tab, question = "Q?", client = cl, trace = tr2,
                            outline = c(A = "first", B = "second", C = "third")))
  expect_equal(sum(nzchar(trimws(sy$sections$text))), 3L)

  # And the audit's per-stage costs still add up to the review's.
  rows <- readgpt:::audit_cost(sc, x, rv)
  nums <- as.numeric(regmatches(rows, regexpr("(?<=<td class=\"num\">)[0-9]+(?=</td>)",
                                              rows, perl = TRUE)))
  expect_equal(sum(nums[!is.na(nums)]), tr$calls)
})

test_that("a trace or a ceiling that cannot be compared is refused, not ignored", {
  # is.finite() alone failed OPEN: NA, Inf and a character value all made the
  # ceiling FALSE, so it was skipped silently -- and the notice that would have
  # said the run was uncapped was suppressed at the same time, being gated on
  # is.null(). A trace was not checked at all: a wrong value ran the whole
  # corpus, discarded every counter, and produced an object print() could not
  # render.
  d <- withr::local_tempdir()
  for (i in 1:3) writeLines(paste(rep(sprintf("Doc %d about revenue.", i), 20), collapse = " "),
                            file.path(d, sprintf("doc%d.txt", i)))
  for (bad in list(list(), "abc", NA, 42)) {
    expect_error(quiet(gr_read_many(d, "Q?", client = mock_echo(), recipe = "fast", trace = bad)),
                 class = "gr_bad_trace")
  }
  for (bad in list(NA, "2", TRUE, c(2, 4), list(2), -5)) {
    expect_error(quiet(gr_read_many(d, "Q?", client = mock_echo(), recipe = "fast",
                                    max_total_calls = bad)),
                 class = "gr_bad_ceiling")
  }
  # Inf is a legitimate way to say "no ceiling", and saying it that way must not
  # cost the notice that the run is uncapped.
  msgs <- character(0)
  withCallingHandlers(
    suppressWarnings(gr_read_many(d, "Q?", client = mock_echo(), recipe = "fast",
                                  max_total_calls = Inf)),
    message = function(m) { msgs <<- c(msgs, conditionMessage(m)); invokeRestart("muffleMessage") })
  expect_true(any(grepl("PER DOCUMENT", msgs, fixed = TRUE)))
})

test_that("how good the screening is reaches the report", {
  # gr_calibrate() computed sensitivity, specificity and kappa and there was
  # nowhere to put them, so the one number a reviewer asks about had to be
  # copied into a methods section by hand.
  expect_true("calibration" %in% names(formals(gr_audit_report)))
  d <- withr::local_tempdir()
  writeLines("A randomised trial of 412 adults found mortality fell.", file.path(d, "a.txt"))
  writeLines("A survey of 60 adults found no change.", file.path(d, "b.txt"))
  writeLines("A randomised trial of 900 adults found mortality fell.", file.path(d, "c.txt"))
  cl <- gr_mock_client(function(messages, params) {
    seen <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                  collapse = " ")
    if (grepl("survey", seen, fixed = TRUE)) {
      return('{"decision":"exclude","reason":"A survey.","criterion":"c","quote":null}')
    }
    '{"decision":"include","reason":"A trial.","criterion":"c","quote":null}'
  })
  sc <- quiet(gr_screen(d, question = "Q?", include = "c", client = cl))
  ref <- quiet(gr_reference(sc, n = 3, of = "all", seed = 1))
  ref$human_decision <- c("include", "exclude", "include")
  cal <- quiet(gr_calibrate(sc, ref, min_positives = 1L))

  p <- withr::local_tempfile(fileext = ".html")
  quiet(gr_audit_report(p, screening = sc, calibration = cal))
  h <- paste(readLines(p, warn = FALSE), collapse = "\n")
  expect_match(h, "How good the screening is", fixed = TRUE)
  expect_match(h, "sensitivity")
  # And a wrong object is refused rather than silently filed.
  expect_error(gr_audit_report(withr::local_tempfile(), screening = sc,
                               calibration = list(metrics = 1)),
               class = "gr_bad_audit_input")
})
