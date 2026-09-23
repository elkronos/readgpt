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

  # A capped batch still marks its section partial. Counting capped batches apart
  # from failed ones for the MESSAGE removed the only thing that set `partial`,
  # so a section that silently dropped a quarter of the corpus read as complete
  # while the warning said it had been marked partial.
  gr_options(max_calls = 3L)
  sy2 <- quiet(gr_synthesise(big, question = "Q?", client = mock_echo("Something [study 1]."),
                             model = "synth-tiny", max_section_tokens = 200L,
                             outline = c(A = "first", B = "second")))
  expect_true(all(sy2$sections$partial))

  # And the latch is this function's own, not trace$budget_stop: tree_merge()
  # sets that too, while still returning usable text, so a run whose merge
  # tripped the ceiling suppressed the warning for every section after it and
  # those sections came back empty in silence.
  for (mc in 3:5) {
    gr_options(max_calls = mc)
    seen2 <- character(0)
    withCallingHandlers(
      suppressMessages(gr_synthesise(big, question = "Q?", client = mock_echo("Something [study 1]."),
                                     model = "synth-tiny", max_section_tokens = 200L,
                                     outline = c(A = "first", B = "second", C = "third"))),
      warning = function(z) { seen2 <<- c(seen2, class(z)[1]); invokeRestart("muffleWarning") })
    expect_true("gr_synth_capped" %in% seen2, label = sprintf("max_calls = %d", mc))
  }
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

  # And across the hand-off, which is where it matters. gr_extract() takes a
  # character vector of paths, so `screened$included` cannot carry anything;
  # passing the screening object itself is what does. An attribute on `included`
  # would have worked too and was rejected: print(screened$included) would then
  # dump the whole record set under a list of file paths.
  ex <- gr_mock_client(function(messages, params)
    '{"n":482,"n__quote":"A randomised trial of 482 adults found a benefit."}')
  expect_null(quiet(gr_extract(sc$included, gr_fields(n = gr_field("N", type = "number")),
                               goal = "Q?", client = ex))$records)
  x <- quiet(gr_extract(sc, gr_fields(n = gr_field("N", type = "number")),
                        goal = "Q?", client = ex))
  expect_s3_class(x$records, "gr_records")
  # Routing through the screening object also joins the bibliographic fields the
  # export supplied, as extracting from the record set directly does.
  expect_true("year" %in% names(x$table))
  # And `included` is still a plain character vector, printable as one -- as is
  # `$sources` on the corpus, which carried the record set for a while and
  # printed the whole thing under a list of file paths.
  expect_identical(sc$included, as.character(sc$included))
  expect_length(capture.output(print(sc$included)), 1L)
  expect_null(attributes(sc$included))
  corp <- quiet(gr_read_many(recs, "Q?", client = gr_mock_client(function(messages, params) "x"),
                             recipe = "fast"))
  expect_null(attributes(corp$sources))
  expect_length(capture.output(print(corp$sources)), 1L)

  # The record set survives a file that has since moved: relying on the
  # all-paths-exist branch to carry it meant one deleted file lost the search.
  gone <- withr::local_tempdir()
  file.copy(sc$included, file.path(gone, basename(sc$included)))
  sc2 <- sc
  sc2$included <- c(sc$included, file.path(gone, "not-here.txt"))
  expect_s3_class(attr(readgpt:::corpus_sources(sc2), "record_set"), "gr_records")

  # A screening that kept nothing says so, rather than telling somebody who
  # passed a screening result to pass file paths.
  sc3 <- sc; sc3$included <- character(0)
  expect_error(quiet(gr_extract(sc3, gr_fields(n = gr_field("N", type = "number")),
                                goal = "Q?", client = ex)),
               "Screening kept no documents", class = "gr_no_sources")
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

test_that("a parent trace keeps what a run spent before it aborted", {
  # Folding into the parent only on the success path meant a run that aborted
  # part-way -- a cost cap, an unreadable file under on_error = "stop" -- handed
  # the parent nothing, and a review then reported itself cheaper than it was.
  d <- withr::local_tempdir()
  writeLines(paste(rep("Small doc one about revenue.", 20), collapse = " "),
             file.path(d, "a.txt"))
  writeLines(paste(rep("Small doc two about revenue.", 20), collapse = " "),
             file.path(d, "b.txt"))
  writeLines(paste(rep("Huge doc three about revenue and many other things.", 4000),
                   collapse = " "), file.path(d, "c.txt"))
  old <- gr_options("max_cost_usd")
  on.exit(gr_options(max_cost_usd = old), add = TRUE)
  gr_options(max_cost_usd = 0.05)

  cl <- mock_echo("An answer [chunk 1].")
  tr <- gr_trace()
  expect_error(quiet(gr_read_many(sort(list.files(d, full.names = TRUE)), "Q?", client = cl,
                                  recipe = "fast", trace = tr, on_error = "stop")),
               class = "gr_cost_cap")
  expect_gt(length(cl$calls()), 0L)          # the fixture really does spend first
  expect_equal(tr$calls, length(cl$calls()))
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
  for (bad in list(NA, "2", TRUE, c(2, 4), list(2), -5, -Inf, NaN)) {
    expect_error(quiet(gr_read_many(d, "Q?", client = mock_echo(), recipe = "fast",
                                    max_total_calls = bad)),
                 class = "gr_bad_ceiling")
    # The cost ceiling was left unvalidated when the call ceiling was fixed, so
    # it kept the same fail-open -- and once the worst-case notice was gated on
    # it, a silently unenforced ceiling silenced the notice too.
    expect_error(quiet(gr_read_many(d, "Q?", client = mock_echo(), recipe = "fast",
                                    max_total_usd = bad)),
                 class = "gr_bad_ceiling")
  }
  # A ceiling above .Machine$integer.max must stay comparable. as.integer(1e10)
  # is NA, and the loop guard then compared against NA, so `if` threw a bare
  # simpleError -- the exact failure the validator exists to prevent.
  expect_equal(readgpt:::as_call_ceiling(1e10), 1e10)
  expect_silent(readgpt:::as_call_ceiling(1e10))
  expect_error(quiet(gr_read_many(d, "Q?", client = mock_echo(), recipe = "fast",
                                  max_total_calls = 1e10)), NA)
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

# ---------------------------------------------------------------------------
# The recurring patterns, swept across the whole package rather than the last
# diff. Three classes, each with prior occurrences on record: a guard that fails
# OPEN, a missing value that becomes the most destructive number on a scale, and
# a parsed model reply read with `$` or assumed to have one shape.
# ---------------------------------------------------------------------------

test_that("rerank does not answer from a prompt full of [chunk NA]", {
  # as.numeric("high") is NA; `NA >= thresh` is NA; and an NA logical SUBSCRIPT
  # selects an NA element rather than dropping it. So the vector kept its length,
  # the "nothing scored high enough" guard did not fire, d[NA, ] put `[chunk NA]`
  # and `NA` into the excerpts, and the model answered the question with no
  # document in front of it -- returned with partial = FALSE and no evidence.
  doc <- paste(rep(paste("The cohort comprised 482 participants across nine sites.",
                         "Adherence exceeded 91 percent in the treatment arm.",
                         "We fitted a mixed-effects model with site as an intercept."), 120),
               collapse = "\n\n")
  sent <- new.env(parent = emptyenv())
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("Rate how useful", messages[[1]]$content, fixed = TRUE)) {
      return('{"score": "high", "reason": "r"}')
    }
    sent$msgs <- messages
    "The sample size was 482."
  })
  a <- quiet(answer_document(doc, "What was the sample size?", reader = "rerank",
                             rerank_candidates = 5, top_k = 3, client = cl))
  body <- as.character(sent$msgs[[2]]$content)
  expect_false(grepl("[chunk NA]", body, fixed = TRUE))
  expect_false(any(is.na(a$chunks_used)))
  expect_true(a$partial)          # it degraded to the lexical ranking, and says so

  # SOME scores unusable and some fine is the case that needs the !is.na() test:
  # with every score unusable the reader degrades to BM25 and no NA reaches the
  # comparison at all, so a mixed reply is what exercises the guard.
  first <- new.env(parent = emptyenv()); first$n <- 0L
  sent2 <- new.env(parent = emptyenv())
  cl3 <- gr_mock_client(function(messages, params) {
    if (grepl("Rate how useful", messages[[1]]$content, fixed = TRUE)) {
      first$n <- first$n + 1L
      return(if (first$n == 1L) '{"score": 9, "reason": "r"}' else '{"score": "high", "reason": "r"}')
    }
    sent2$msgs <- messages
    "The sample size was 482."
  })
  a3 <- quiet(answer_document(doc, "What was the sample size?", reader = "rerank",
                              rerank_candidates = 5, top_k = 3, client = cl3))
  expect_false(any(is.na(a3$chunks_used)))
  expect_false(grepl("[chunk NA]", as.character(sent2$msgs[[2]]$content), fixed = TRUE))

  # Two more shapes of the same reply that used to crash rather than degrade.
  ch <- quiet(gr_segment(quiet(gr_ingest(doc)), list(method = "paragraph", max_tokens = 200)))
  for (reply in c('{"score":[8,9],"reason":"r"}', '{"score":{"v":8},"reason":"r"}')) {
    cl2 <- gr_mock_client(function(messages, params) {
      if (grepl("Rate how useful", messages[[1]]$content, fixed = TRUE)) return(reply)
      "ANSWER"
    })
    expect_error(quiet(gr_read(ch, "q", client = cl2,
                               spec = gr_read_spec(reader = "rerank",
                                                   rerank_candidates = 3L))), NA)
  }
})

test_that("a model reply is read by exact key, never by prefix", {
  # `$` partial-matches. A reply carrying `decisions` satisfied a read of
  # `decision` and the screener recorded a real "include" from a key the schema
  # never defined; `can_answer_now` ended the iterative loop and its answer came
  # back as final. extract_text() has used [[exact = TRUE]] since the same bug
  # bit the HTTP layer; the readers were never given the same treatment.
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(paste(rep("The cohort comprised 482 participants across nine sites.", 60),
                   collapse = " "), f)
  cl <- gr_mock_client(function(messages, params)
    '{"decisions":"include","reasoning":"r","criterions":"c","quotes":"x"}')
  s <- quiet(gr_screen(f, question = "q?", include = "Any study", client = cl))
  expect_false(identical(as.character(s$table$decision), "include"))
  expect_true(is.na(s$table$reason) || !identical(s$table$reason, "r"))

  ch <- quiet(gr_segment(quiet(gr_ingest(paste(rep("Topic sentence here.", 80), collapse = " "))),
                         list(method = "sentence", max_tokens = 60)))
  cl2 <- gr_mock_client(function(messages, params) {
    if (grepl("reading iteratively", messages[[1]]$content, fixed = TRUE)) {
      return('{"can_answer_now": true, "answers": "Yes it was.", "next_query": ""}')
    }
    "fallback"
  })
  a <- quiet(gr_read(ch, "What was measured?", client = cl2,
                     spec = gr_read_spec(reader = "iterative", max_rounds = 2L)))
  expect_false(grepl("Yes it was", a$answer, fixed = TRUE))
  # And the loop must not have STOPPED on it: `can_answer_now` satisfying a read
  # of `can_answer` ended the retrieve-assess loop after one round and returned
  # that round's answer as final.
  # The mid-loop return skips the final answer call entirely, so its absence is
  # the signature of the loop having been ended by a key that does not exist.
  labs <- vapply(cl2$calls(), function(c) as.character(c$label), character(1))
  expect_true("iterative.final" %in% labs)

  # The accessor itself, on every shape simplifyVector can produce.
  v <- list(score = 8, both = c(1, 2), obj = list(a = 1), txt = "x")
  expect_equal(readgpt:::json_field(v, "score"), 8)
  expect_null(readgpt:::json_field(v, "scor"))        # no prefix match
  expect_null(readgpt:::json_field(v, "both"))        # not a scalar
  expect_null(readgpt:::json_field(v, "obj"))
  expect_equal(readgpt:::json_num(v, "txt", -1), -1)
})

test_that("a proposition list keeps its order whatever shape it arrived in", {
  # An array of arrays simplifies to a MATRIX, which as.character() flattens
  # column-major and so transposes; an array of objects to a data frame, which
  # as.character() deparses, putting the literal text c("A.", "B.") into the
  # document that every downstream reader then treats as content.
  src <- "A1 alpha sentence. A2 second sentence. B1 third one. B2 fourth one here."
  shapes <- c('{"propositions":["A1.","A2.","B1.","B2."]}',
              '{"propositions":[["A1.","A2."],["B1.","B2."]]}')
  flat <- function(cs) paste(trimws(unlist(strsplit(cs$chunks$text, "\n"))), collapse = " ")
  for (j in shapes) {
    cl <- gr_mock_client(function(messages, params) j)
    cs <- quiet(gr_segment(quiet(gr_ingest(src)), list(method = "proposition"), client = cl))
    # The matrix shape used to come out transposed: "A1. B1. A2. B2."
    expect_equal(flat(cs), "A1. A2. B1. B2.", info = j)
  }
  cl <- gr_mock_client(function(messages, params)
    '{"propositions":[{"text":"A1."},{"text":"A2."}]}')
  cs <- quiet(gr_segment(quiet(gr_ingest(src)), list(method = "proposition"), client = cl))
  expect_false(any(grepl("c(\"", cs$chunks$text, fixed = TRUE)))
  expect_equal(readgpt:::prop_strings(matrix(c("a", "b", "c", "d"), nrow = 2, byrow = TRUE)),
               c("a", "b", "c", "d"))
  expect_equal(readgpt:::prop_strings(NULL), character(0))
})

test_that("a plan or a claim list that omits a required key degrades, not crashes", {
  ch <- quiet(gr_segment(quiet(gr_ingest(paste(rep("Topic sentence here.", 80), collapse = " "))),
                         list(method = "sentence", max_tokens = 60)))
  # as.integer() on a LIST column is an error, not a warning, so
  # suppressWarnings() did not catch it and the preview reader crashed instead of
  # taking its documented "read everything" path.
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("You plan how to read", messages[[1]]$content, fixed = TRUE)) {
      return('{"sections":[{"id":[1,2],"treatment":"skip","reason":"r"}]}')
    }
    "A"
  })
  expect_error(quiet(gr_read(ch, "q", client = cl,
                             spec = gr_read_spec(reader = "preview"))), NA)

  # Three of four columns were guarded with %||% and the load-bearing one was
  # not, so a reply in which NO object carried `claim` died with "arguments
  # imply differing number of rows: 0, 2".
  tab <- data.frame(document = c("a.pdf", "b.pdf"), status = "ok",
                    duplicate_of = NA_character_, n_filled = 1L, n_unverified = 0L,
                    conflicts = NA_character_, finding = c("x", "y"),
                    stringsAsFactors = FALSE)
  noclaim <- gr_mock_client(function(messages, params)
    '{"claims":[{"kind":"finding","supported_by":[1]},{"kind":"finding","supported_by":[2]}]}')
  cm <- quiet(gr_claims(tab, question = "Q?", client = noclaim))
  expect_equal(nrow(cm$claims), 0L)
  # And the outline equivalent: a reply with no `heading` anywhere yields no
  # usable sections rather than an error, which is what gr_outline() checks for
  # before falling back to one section.
  expect_error(readgpt:::outline_rows(list(list(brief = "b1"), list(brief = "b2"))), NA)
  expect_equal(nrow(readgpt:::outline_rows(list(list(brief = "b1"),
                                                list(brief = "b2")))), 0L)
})

test_that("a limit that cannot be compared is refused where it is set", {
  # gr_options() validated NAMES only. Every consumer then invented its own
  # opinion of a bad value and the opinions pointed the wrong way:
  # is.finite(max_cost_usd) as a guard meant NA, or a value read from a config
  # file as text, silently removed the cost cap -- $710 against a $5 ceiling.
  # trace_can_call() did the same for max_calls and DISAGREED with preflight(),
  # which parsed a character cap and enforced it: one option, two answers.
  old <- gr_options(max_cost_usd = 5, max_calls = 400L, safety_margin = 0.1,
                    min_output_tokens = 256L, workers = 4L, temperature = NULL)
  on.exit(gr_options(old), add = TRUE)
  for (bad in list(NA, "5", c(1, 2), -1, list(1))) {
    expect_error(gr_options(max_cost_usd = bad), class = "gr_bad_option")
    expect_error(gr_options(max_calls = bad), class = "gr_bad_option")
  }
  # A CEILING refuses what it cannot compare, because ignoring it spends money.
  # A TUNING knob is different: every value it can hold is safe, so an unusable
  # one warns and leaves the setting as it was, and an out-of-range one is
  # clamped into range -- the cost of refusing is a crashed script over a retry
  # count. "As it was", not the package default: from 0.3, a stray NA dropping
  # the margin to 0.1 pushes every input budget up.
  gr_options(safety_margin = 0.3, min_output_tokens = 512L, workers = 8L)
  for (bad in list(NA, "x", c(1, 2))) {
    expect_warning(gr_options(safety_margin = bad), class = "gr_bad_option")
    expect_equal(gr_options("safety_margin"), 0.3)
    expect_warning(gr_options(min_output_tokens = bad), class = "gr_bad_option")
    expect_equal(gr_options("min_output_tokens"), 512)
    expect_warning(gr_options(workers = bad), class = "gr_bad_option")
    expect_equal(gr_options("workers"), 8)
  }
  # A number written as text is that number, as it is for gr_budget().
  expect_no_warning(gr_options(safety_margin = "0.2"))
  expect_equal(gr_options("safety_margin"), 0.2)
  expect_warning(gr_options(safety_margin = -1), class = "gr_bad_option")
  expect_equal(gr_options("safety_margin"), 0)
  expect_warning(gr_options(safety_margin = Inf), class = "gr_bad_option")
  expect_equal(gr_options("safety_margin"), 0.5)
  # The range is the one the code that reads the option clamps to, so the
  # warning's "using N" is the value actually used.
  expect_warning(gr_options(workers = 2000), "using 32", class = "gr_bad_option")
  expect_equal(gr_options("workers"), 32)
  old_r <- gr_options()[c("max_retries", "retry_pause_base", "request_timeout")]
  on.exit(gr_options(old_r), add = TRUE)
  expect_warning(gr_options(max_retries = 50), "using 10", class = "gr_bad_option")
  expect_equal(gr_options("max_retries"), 10)
  expect_identical(gr_client(api_key = "k")$max_retries, 10L)
  expect_warning(gr_options(retry_pause_base = 700), "using 60\\.", class = "gr_bad_option")
  expect_equal(gr_options("retry_pause_base"), 60)
  expect_equal(gr_client(api_key = "k")$retry_pause_base, 60)
  expect_warning(gr_options(request_timeout = 1e5), "using 3600", class = "gr_bad_option")
  expect_equal(gr_client(api_key = "k")$timeout, 3600)
  expect_warning(gr_options(min_output_tokens = -1), "using 0", class = "gr_bad_option")
  expect_equal(gr_options("min_output_tokens"), 0)
  gr_options(old_r)
  # Inf is "no limit" only for a ceiling. For temperature it went on the wire
  # as "temperature":"Inf".
  expect_warning(gr_options(temperature = Inf), class = "gr_bad_option")
  expect_equal(gr_options("temperature"), 2)
  gr_options(temperature = NULL)
  expect_null(gr_options("temperature"))
  # And an in-range whole number is not reported as out of range because it
  # arrived as an integer: max(4L, 1) is a double, and identical(4, 4L) is FALSE.
  expect_no_warning(gr_options(workers = 4L, max_retries = 3L, min_output_tokens = 512L))
  gr_options(safety_margin = 0.1, min_output_tokens = 256L, workers = 4L)
  # NULL and Inf are the two explicit ways to say "no limit", and both survive.
  gr_options(max_cost_usd = NULL)
  expect_null(gr_options("max_cost_usd"))
  expect_equal({ gr_options(max_cost_usd = Inf); gr_options("max_cost_usd") }, Inf)
  expect_equal({ gr_options(max_calls = 0L); gr_options("max_calls") }, 0)
  # A whole number stays a comparable DOUBLE: as.integer(1e10) is NA.
  expect_equal({ gr_options(max_calls = 1e10); gr_options("max_calls") }, 1e10)
  gr_options(max_calls = 400L, max_cost_usd = 5)

  # And the cap is really enforced, which is the point of refusing the value.
  gr_register_model("swept-costly", context_window = 2e5, max_output = 16000,
                    input_usd = 1e5, output_usd = 1e5)
  doc <- paste(rep("The cohort comprised 482 participants across nine sites.", 200),
               collapse = " ")
  expect_error(quiet(answer_document(doc, "n?", reader = "map_reduce",
                                     model = "swept-costly", client = mock_echo())),
               class = "gr_cost_cap")
})

test_that("a missing value never becomes the destructive end of a scale", {
  # clamp() maps NA to `lo`, and for several settings `lo` is the value the
  # setting exists to prevent: zero safety margin, zero overhead, the median
  # semantic boundary. na_default() sends it to the DEFAULT instead, as `mmr`
  # and `max_tokens` already did.
  s <- quiet(gr_segment_spec("semantic", semantic_percentile = NA, semantic_window = NA,
                             overlap_tokens = NA, min_tokens = NA,
                             proposition_batch_tokens = NA))
  expect_equal(s$semantic_percentile, 90)
  expect_equal(s$semantic_window, 2L)
  expect_equal(s$proposition_batch_tokens, 900L)
  expect_equal(quiet(gr_read_spec("stuff", delay_between_calls = NA))$delay_between_calls, 0)
  expect_equal(quiet(gr_read_spec("retrieve", min_score = NA))$min_score, -Inf)
  expect_equal(quiet(gr_ingest_spec(min_chars = NA))$min_chars, 20L)

  # as_int1()'s range test has to come BEFORE the coercion that creates the NA:
  # as.integer(3e9) is NA, so the function returned the NA its own contract says
  # it never returns, and gr_screen(screen_tokens = 3e9) then marked every
  # document "failed" with "missing value where TRUE/FALSE needed".
  expect_equal(readgpt:::as_int1(3e9, 999L), 999L)
  expect_equal(readgpt:::as_int1(5.7, 0L), 5L)

  # An overhead nobody can count would be budgeted as zero, which is the one
  # direction that overruns the window.
  expect_error(gr_budget("gpt-4o", overhead = NA), class = "gr_budget_error")
  expect_error(gr_budget("gpt-4o", overhead = 3e9), class = "gr_budget_error")
  # A safety margin of NA became ZERO -- no headroom at all, which is the one
  # condition the margin exists to prevent -- and the input budget went UP.
  expect_equal(quiet(gr_budget("gpt-4o", overhead = 200, safety_margin = NA))$margin, 0.10)
  expect_equal(quiet(gr_budget("gpt-4o", overhead = 200, safety_margin = NA))$input,
               gr_budget("gpt-4o", overhead = 200, safety_margin = 0.10)$input)

  # `4 >= "10"` is TRUE -- R compares as STRINGS -- so a character min_positives
  # declared an inadequate calibration adequate while still printing 10.
  tab <- data.frame(document = paste0("d", 1:8, ".pdf"),
                    decision = c("include", "include", "unclear", "exclude",
                                 "exclude", "exclude", "include", "exclude"),
                    stringsAsFactors = FALSE)
  ref <- data.frame(document = paste0("d", 1:8, ".pdf"),
                    human_decision = c("include", "exclude", "include", "exclude",
                                       "exclude", "include", "include", "exclude"),
                    stringsAsFactors = FALSE)
  sc <- structure(list(table = tab), class = "gr_screening")
  for (mp in list(10L, "10", NA)) {
    cal <- quiet(gr_calibrate(sc, ref, min_positives = mp))
    expect_false(isTRUE(cal$adequate))
    expect_error(capture.output(print(cal)), NA)
  }

  # An integer field above .Machine$integer.max is stored, not thrown away: it
  # used to become NA, so n_filled dropped to 0 and the table said the document
  # had not reported a value it stated plainly.
  fl <- withr::local_tempfile(fileext = ".txt")
  writeLines("We enrolled 3000000000 person-days of follow-up.", fl)
  cc <- gr_mock_client(function(messages, params)
    '{"n": 3000000000, "n__quote": "We enrolled 3000000000 person-days of follow-up."}')
  x <- quiet(gr_extract(fl, gr_fields(n = gr_field("Person-days", type = "integer")),
                        client = cc))
  expect_equal(x$table$n, 3e9)
  expect_equal(x$table$n_filled, 1L)
})

test_that("subsetting a reference frame gives a plain data frame", {
  # `[` on a class that extends data.frame keeps the CLASS and drops every other
  # attribute, so ref[, cols] still claimed to know which stratum it came from
  # while `of`, `frame_n`, `screened_n` and `seed` were gone -- and gr_calibrate()
  # then computed the corpus-wide metric set from a stratified sample. The same
  # trap, and the same fix, as `[.gr_gaps`.
  rf <- structure(data.frame(document = c("a", "b"), sampled_from = "excluded",
                             stringsAsFactors = FALSE),
                  of = "excluded", frame_n = 30L, screened_n = 100L, seed = 1L,
                  class = c("gr_reference_frame", "data.frame"))
  for (sub in list(rf[, "document", drop = FALSE], rf[1, ], rf[, 1:2])) {
    expect_identical(class(sub), "data.frame")
    expect_null(attr(sub, "of"))
    expect_null(attr(sub, "frame_n"))
  }
})

test_that("a NULL override does not silently reset a recipe field", {
  # `x[[nm]] <- NULL` DELETES the element, and gr_recipe() then rebuilt the spec
  # with do.call(), which supplied the constructor's formal default -- not the
  # recipe's value and not what the constructor would have made of NULL. The
  # segmentation changed with no warning, and the trace's `settings` lost the
  # entry too, so the run record no longer said which cap was used.
  r <- readgpt:::as_recipe("fast")
  for (nm in c("max_tokens", "method", "prefix_section", "reader", "top_k")) {
    expect_error(readgpt:::apply_overrides(r, stats::setNames(list(NULL), nm)),
                 class = "gr_bad_override", info = nm)
  }
  # NULL IS a setting where the constructor's own default is NULL -- "the
  # session's model", "no separate skim model" -- and there an override of NULL
  # replaces the recipe's value with whatever the constructor makes of NULL.
  for (nm in c("model", "skim_model", "summary_model")) {
    set <- readgpt:::apply_overrides(r, stats::setNames(list("gpt-4o"), nm))
    expect_identical(unclass(set$read)[[nm]], "gpt-4o")
    back <- readgpt:::apply_overrides(set, stats::setNames(list(NULL), nm))
    expect_identical(unclass(back$read)[[nm]], unclass(gr_read_spec())[[nm]], info = nm)
  }
  set <- readgpt:::apply_overrides(r, list(temperature = 0.7))
  expect_null(unclass(readgpt:::apply_overrides(set, list(temperature = NULL))$read)$temperature)
  set <- readgpt:::apply_overrides(r, list(parallel = TRUE))
  back <- readgpt:::apply_overrides(set, list(parallel = NULL))
  expect_identical(unclass(back$segment)$parallel, unclass(gr_segment_spec())$parallel)
  expect_identical(unclass(back$read)$parallel, unclass(gr_read_spec())$parallel)
  # Through the public entry point too: refused before anything is read.
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(paste(rep("A paragraph about revenue.", 50), collapse = " "), f)
  expect_error(answer_document(f, "q", "fast", client = mock_echo(), max_tokens = NULL),
               class = "gr_bad_override")
  expect_error(quiet(answer_document(f, "q", "fast", client = mock_echo(), skim_model = NULL)), NA)
})

test_that("a text field that arrives as an array is joined, not discarded", {
  # A scalar-only read of `answer` returned NULL for ["...", "..."], and the
  # iterative reader then reported NOT_IN_DOCUMENT for a document that answered.
  doc <- paste(rep(paste("The cohort comprised 482 participants across nine sites.",
                         "Adherence exceeded 91 percent in the treatment arm."), 20),
               collapse = "\n\n")
  ch <- quiet(gr_segment(quiet(gr_ingest(doc)), list(method = "paragraph", max_tokens = 120)))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("reading iteratively", messages[[1]]$content, fixed = TRUE)) {
      return('{"can_answer": true, "answer": ["The sample size was 482.", "Nine sites."], "next_query": ""}')
    }
    "x"
  })
  a <- quiet(gr_read(ch, "How many?", cl, gr_read_spec("iterative", max_rounds = 3)))
  expect_identical(a$answer, "The sample size was 482.\nNine sites.")
  expect_false(a$partial)
  # The same for the next search: an array of terms is one query, not the end
  # of the loop.
  k <- new.env(); k$n <- 0L
  cl2 <- gr_mock_client(function(messages, params) {
    if (grepl("reading iteratively", messages[[1]]$content, fixed = TRUE)) {
      k$n <- k$n + 1L
      if (k$n == 1L) return('{"can_answer": false, "answer": "", "next_query": ["nine sites", "cohort"]}')
      return('{"can_answer": true, "answer": "482.", "next_query": ""}')
    }
    "x"
  })
  a <- quiet(gr_read(ch, "How many?", cl2, gr_read_spec("iterative", max_rounds = 3)))
  expect_identical(a$notes$queries, c("How many?", "nine sites\ncohort"))

  # json_text() itself: exact key, NAs dropped, nothing usable is the default.
  jt <- readgpt:::json_text
  expect_identical(jt(list(answer = list("a", "b")), "answer"), "a\nb")
  expect_identical(jt(list(answers = "a"), "answer", "none"), "none")
  expect_identical(jt(list(answer = c(NA, "z")), "answer"), "z")
  expect_identical(jt(list(answer = list()), "answer", "none"), "none")
  expect_identical(jt(list(answer = NA), "answer", "none"), "none")
  expect_identical(jt(list(answer = 3), "answer"), "3")
  # Everything in a prose field is kept, numbers included, in the order it was
  # written: an answer of {"participants": 482} is 482, not "no answer".
  f <- jsonlite::fromJSON
  expect_identical(jt(f('{"answer":{"participants":482}}'), "answer"), "482")
  expect_identical(jt(f('{"answer":{"value":482,"unit":"participants"}}'), "answer"),
                   "482\nparticipants")
  expect_identical(jt(f('{"answer":[{"part":"A","n":1},{"part":"B","n":2}]}'), "answer"),
                   "A\n1\nB\n2")
  obj <- gr_mock_client(function(messages, params) {
    if (grepl("reading iteratively", messages[[1]]$content, fixed = TRUE)) {
      return('{"can_answer": true, "answer": {"participants": 482}, "next_query": ""}')
    }
    "x"
  })
  expect_identical(quiet(gr_read(ch, "How many?", obj,
                                 gr_read_spec("iterative", max_rounds = 3)))$answer, "482")

  # And the screener's reason and criterion, which are what the flow diagram
  # counts exclusions by.
  d <- withr::local_tempdir()
  writeLines("A randomised trial of 482 adults found a benefit.", file.path(d, "a.txt"))
  sc_cl <- gr_mock_client(function(messages, params)
    paste0('{"decision":"exclude","reason":["Not randomised.","No control group."],',
           '"criterion":["Randomised comparison","Adults"],',
           '"quote":["A randomised trial of 482 adults found a benefit."]}'))
  sc <- quiet(gr_screen(d, question = "Q?", include = "Randomised comparison", client = sc_cl))
  expect_identical(sc$table$reason, "Not randomised.\nNo control group.")
  expect_identical(sc$table$criterion, "Randomised comparison\nAdults")
  expect_true(sc$table$verified)
})

test_that("a proposition batch that cannot be decomposed is kept as written", {
  # A batch whose call failed, or whose reply held nothing usable, returned
  # character(0): one bad batch in ten silently removed a tenth of the document
  # from everything downstream. Only a run in which EVERY batch failed noticed.
  k <- new.env(); k$n <- 0L
  cl <- gr_mock_client(function(messages, params) {
    k$n <- k$n + 1L
    if (k$n %% 2L == 1L) '{"propositions":["The trial enrolled 482 adults."]}' else "not json"
  })
  two <- paste(c(paste(rep("The trial enrolled 482 adults at nine sites across the region.", 40),
                       collapse = " "),
                 paste(rep("Mortality fell by 31 percent in the treatment arm of the trial.", 40),
                       collapse = " ")), collapse = "\n\n")
  expect_warning(
    s <- suppressMessages(gr_segment(quiet(gr_ingest(two)),
                                     list(method = "proposition", proposition_batch_tokens = 400),
                                     client = cl)),
    "2 of 4 proposition batch(es) could not be decomposed", fixed = TRUE)
  expect_true(any(grepl("Mortality fell by 31 percent in the treatment arm", s$chunks$text,
                        fixed = TRUE)))
  expect_identical(s$method, "proposition")
  expect_identical(s$extra$batches_kept_as_written, 2L)
  # A kept batch is a paragraph, so it is not counted as a proposition.
  expect_identical(s$extra$propositions, 2L)

  # Every shape a model has returned for the list, and none of it lost.
  ps <- readgpt:::prop_strings
  expect_identical(ps(jsonlite::fromJSON('[{"text":"B1."},{"proposition":"B2."}]')),
                   c("B1.", "B2."))
  expect_identical(ps(jsonlite::fromJSON('[{"text":["B1.","B2."]},{"text":"B3."}]')),
                   c("B1.", "B2.", "B3."))
  expect_identical(ps(jsonlite::fromJSON('[{"p":{"text":"B1."}},{"p":{"text":"B2."}}]')),
                   c("B1.", "B2."))
  expect_identical(ps(matrix(c("a", "b", "c", "d"), nrow = 2, byrow = TRUE)),
                   c("a", "b", "c", "d"))
  expect_identical(ps(list("a", NA, list("b"))), c("a", "b"))
  # An object is read through its text key when it has one: an `id`, a page
  # label or a confidence beside a proposition is not a proposition -- and ids
  # of mixed type arrive as strings, so "only strings" alone would not do.
  f <- jsonlite::fromJSON
  expect_identical(ps(f('[{"id":1,"text":"B1."},{"id":"2","text":"B2."}]')), c("B1.", "B2."))
  expect_identical(ps(f('[{"text":"B1.","page":{"n":3,"label":"iii"}}]')), "B1.")
  expect_identical(ps(f('[{"statement":"S.","confidence":"high"}]')), "S.")
  # ... and through every string when it has none, per object, so a key nobody
  # anticipated loses nothing.
  expect_identical(ps(f('[{"text":"B1."},{"other":"B2."}]')), c("B1.", "B2."))
  expect_identical(ps(f('[{"claimtext":"C1.","n":3}]')), "C1.")
  # Row by row, and an array of any depth in the order it was written.
  expect_identical(ps(f('[{"text":"A1","sentence":"A2"},{"text":"B1","sentence":"B2"}]')),
                   c("A1", "A2", "B1", "B2"))
  expect_identical(ps(f('[[["a","b"],["c","d"]],[["e","f"],["g","h"]]]')),
                   c("a", "b", "c", "d", "e", "f", "g", "h"))
})

test_that("a rerank score that was never given is not a score", {
  paras <- paste(c("Alpha", "Beta", "Gamma", "Delta"),
                 "site enrolled adults into the trial and followed every one of them",
                 "for two full years before the final visit.")
  ch <- quiet(gr_segment(quiet(gr_ingest(paste(paras, collapse = "\n\n"))),
                         list(method = "paragraph", max_tokens = 40)))
  expect_equal(nrow(ch$chunks), 4L)        # one site per chunk, or the rules below blur
  scorer <- function(rule) gr_mock_client(function(messages, params) {
    if (grepl("Rate how useful", messages[[1]]$content, fixed = TRUE)) {
      ex <- messages[[3]]$content
      return(rule(ex))
    }
    "ANSWER"
  })
  spec <- function(...) gr_read_spec("rerank", rerank_candidates = 4L, top_k = 4L, ...)

  # Nothing judged -- two calls unusable, two failed -- degrades and says so.
  # Testing "all failed" and "all unusable" separately left this mix falling
  # through both, and the run answered NOT_IN_DOCUMENT without a word.
  mixed <- scorer(function(ex) if (grepl("Alpha|Gamma", ex)) "not json" else '{"score":"high"}')
  expect_warning(r <- suppressMessages(gr_read(ch, "How many?", mixed, spec())),
                 class = "gr_rerank_degraded")
  expect_identical(r$answer, "ANSWER")
  expect_true(r$partial)
  expect_true(r$notes$degraded_to_bm25)

  # A failed call is not a score of 0: at rerank_min_score = 0 it used to pass
  # the threshold as a model-judged chunk no model had read.
  half <- scorer(function(ex) if (grepl("Alpha", ex)) "not json" else '{"score":0,"reason":"r"}')
  r <- quiet(gr_read(ch, "How many?", half, spec(rerank_min_score = 0)))
  expect_false(any(grepl("Alpha", r$evidence$quote, fixed = TRUE)))
  expect_true(r$partial)          # one candidate was never judged
  expect_identical(r$notes$scoring_failures, 1L)

  # Partly judged and nothing passed: the negative rests on part of the list.
  low <- scorer(function(ex) if (grepl("Alpha", ex)) "not json" else '{"score":1,"reason":"r"}')
  r <- quiet(gr_read(ch, "What colour?", low, spec()))
  expect_identical(r$answer, "NOT_IN_DOCUMENT")
  expect_true(r$partial)

  # Every candidate judged and every score low is a CORRECT negative.
  none <- scorer(function(ex) '{"score":1,"reason":["no","nothing here"]}')
  r <- quiet(gr_read(ch, "What colour?", none, spec()))
  expect_identical(r$answer, "NOT_IN_DOCUMENT")
  expect_false(r$partial)
  # Whatever the model's own `reason` says: outcomes are counted from what
  # happened to the call, not from words a model wrote.
  odd <- scorer(function(ex) '{"score":1,"reason":"unscorable"}')
  r <- quiet(gr_read(ch, "What colour?", odd, spec()))
  expect_false(r$partial)
  expect_identical(r$notes$unscorable, 0L)

  # A candidate the call cap stopped judged nothing either. gr_read()'s
  # pre-flight check keeps this from happening through the front door, so the
  # reader is driven directly: two calls allowed, four candidates.
  old <- gr_options(max_calls = 2)
  on.exit(gr_options(old), add = TRUE)
  zero <- scorer(function(ex) '{"score":0,"reason":"r"}')
  r <- quiet(readgpt:::read_rerank(ch, "How many?", zero, spec(rerank_min_score = 0), gr_trace()))
  expect_length(r$chunks_used, 2L)
  expect_true(r$partial)
  # And with nothing over the bar, "not in the document" rests on half the list.
  r <- quiet(readgpt:::read_rerank(ch, "How many?", zero, spec(rerank_min_score = 4), gr_trace()))
  expect_identical(r$answer, "NOT_IN_DOCUMENT")
  expect_true(r$partial)
})

test_that("max_pdf_pages = Inf samples every page, and NA is the default", {
  skip_if_not_installed("pdftools")
  d <- withr::local_tempdir()
  pf <- file.path(d, "paper.pdf")
  grDevices::pdf(pf, width = 8, height = 11)
  for (i in 1:6) {
    graphics::plot.new()
    # Over 100 characters: compared as TEXT, "1xx" sorts before "40".
    if (i > 3) graphics::text(0.5, 0.5, cex = 0.5, paste(
      "Page", i, "has plenty of text for the probe to find, well over a hundred",
      "characters of it, on one line."))
  }
  grDevices::dev.off()
  st <- function(v) quiet(gr_inventory(d, max_pdf_pages = v))$files$status
  expect_identical(st(3L), "needs_ocr")      # the first three pages are blank
  expect_identical(st(Inf), "ready")         # documented: Inf reads every page
  expect_identical(st(1e10), "ready")
  expect_warning(gr_inventory(d, max_pdf_pages = NA), class = "gr_bad_setting")
  expect_identical(st(NA), "needs_ocr")      # the default, 3
  # ocr_min_chars is a count: as text it was compared as text, and "40" made a
  # page with a text layer look like a scan; NA failed every PDF.
  txt <- function(v) quiet(gr_inventory(d, max_pdf_pages = Inf, ocr_min_chars = v))$files$status
  expect_identical(txt("40"), "ready")
  expect_warning(gr_inventory(d, max_pdf_pages = Inf, ocr_min_chars = NA), class = "gr_bad_setting")
  expect_identical(txt(NA), "ready")
  # And read exactly as ingestion reads it, or the survey predicts nothing:
  # Inf marks every page for OCR in both, and a negative count is refused by
  # both.
  expect_identical(txt(Inf), "needs_ocr")
  expect_identical(gr_ingest_spec(ocr_min_chars = Inf)$ocr_min_chars, Inf)
  expect_warning(gr_ingest_spec(ocr_min_chars = -5), class = "gr_bad_setting")
  expect_identical(quiet(gr_ingest_spec(ocr_min_chars = -5))$ocr_min_chars, 40L)
  expect_identical(formals(gr_inventory)$ocr_min_chars, formals(gr_ingest_spec)$ocr_min_chars)
  # Ingestion takes the Inf it was given: as.integer(Inf) is NA, and the OCR
  # decision `nchar < NA` then stopped the read.
  expect_error(quiet(gr_ingest(pf, spec = gr_ingest_spec(ocr_min_chars = Inf))), NA)
})

test_that("an adequacy bar that cannot be met is not met, and the report still renders", {
  tab <- data.frame(document = paste0("d", 1:8, ".pdf"),
                    decision = c("include", "include", "unclear", "exclude",
                                 "exclude", "exclude", "include", "exclude"),
                    stringsAsFactors = FALSE)
  ref <- data.frame(document = paste0("d", 1:8, ".pdf"),
                    human_decision = c("include", "exclude", "include", "exclude",
                                       "exclude", "include", "include", "exclude"),
                    stringsAsFactors = FALSE)
  sc <- structure(list(table = tab), class = "gr_screening")
  cal <- quiet(gr_calibrate(sc, ref, min_positives = Inf))
  expect_false(cal$adequate)
  expect_identical(cal$min_positives, Inf)
  p <- withr::local_tempfile(fileext = ".html")
  expect_error(quiet(gr_audit_report(p, screening = sc, calibration = cal)), NA)
  expect_true(file.exists(p))

  cls <- character(0)
  cal <- withCallingHandlers(suppressMessages(gr_calibrate(sc, ref, min_positives = NA)),
                             warning = function(w) {
                               cls <<- c(cls, class(w))
                               invokeRestart("muffleWarning")
                             })
  expect_true("gr_bad_setting" %in% cls)
  expect_identical(cal$min_positives, 10)
  expect_false(cal$adequate)

  # Enough eligible studies to clear the default bar: Inf must still not be
  # cleared. Read as an integer it became the default, 10, and a sample of
  # twelve was declared adequate against a bar nobody can reach.
  n <- 30L
  hum <- rep(c("include", "exclude"), c(12L, 18L))
  big_sc <- structure(list(table = data.frame(document = paste0("e", seq_len(n), ".pdf"),
                                              decision = hum, stringsAsFactors = FALSE)),
                      class = "gr_screening")
  big_ref <- data.frame(document = paste0("e", seq_len(n), ".pdf"), human_decision = hum,
                        stringsAsFactors = FALSE)
  expect_true(quiet(gr_calibrate(big_sc, big_ref))$adequate)
  expect_false(quiet(gr_calibrate(big_sc, big_ref, min_positives = Inf))$adequate)
  expect_false(quiet(gr_calibrate(big_sc, big_ref, min_positives = 3e9))$adequate)
})

test_that("two values that differ past the seventh digit are two values", {
  vk <- readgpt:::value_key
  expect_false(identical(vk(0.123456789), vk(0.123456781)))
  expect_false(identical(vk(3000000001), vk(3000000002)))
  expect_identical(vk(3000000001), "3000000001")
  expect_identical(vk(2L), "2")
  expect_identical(vk(0.5), "0.5")
  expect_identical(vk("a"), "a")

  # format() showed both as 3e+09: the conflict went unreported, and once it
  # was reported the adjudicating model was offered two identical options.
  fl <- withr::local_tempfile(fileext = ".txt")
  writeLines(c(paste(rep("We enrolled 3000000001 person-days in site A.", 30), collapse = " "), "",
               paste(rep("We enrolled 3000000002 person-days in site B.", 30), collapse = " ")), fl)
  seen <- new.env(); seen$choose <- character(0)
  cc <- gr_mock_client(function(messages, params) {
    t <- paste(vapply(messages, function(z) as.character(z$content), character(1)), collapse = " ")
    if (grepl("Choose the", t, fixed = TRUE)) {
      seen$choose <- t
      return('{"choices": 2}')          # not the schema's key
    }
    if (grepl("3000000002", t, fixed = TRUE)) {
      '{"n": 3000000002, "n__quote": "We enrolled 3000000002 person-days in site B."}'
    } else '{"n": 3000000001, "n__quote": "We enrolled 3000000001 person-days in site A."}'
  })
  x <- quiet(gr_extract(fl, gr_fields(n = gr_field("Person-days", type = "integer")), client = cc,
                        recipe = "fast", method = "paragraph", max_tokens = 200, resolve = "model"))
  expect_identical(x$table$conflicts, "n")
  expect_true(grepl("1. 3000000001", seen$choose, fixed = TRUE))
  expect_true(grepl("2. 3000000002", seen$choose, fixed = TRUE))
  # `choices` is not `choice`: the first value seen stands, as for no answer.
  # identical(), not equal(): the two differ by 1 part in 3e9, inside
  # expect_equal()'s tolerance.
  expect_identical(x$table$n, 3000000001)
})

test_that("gr_budget takes the session's margin when it is given none it can use", {
  old <- gr_options(safety_margin = 0.3)
  on.exit(gr_options(old), add = TRUE)
  expect_equal(gr_budget("gpt-4o", overhead = 200)$margin, 0.3)
  expect_equal(quiet(gr_budget("gpt-4o", overhead = 200, safety_margin = NA))$margin, 0.3)
  expect_warning(gr_budget("gpt-4o", overhead = 200, safety_margin = NA), class = "gr_bad_setting")
  expect_equal(gr_budget("gpt-4o", overhead = 200, safety_margin = 0.2)$margin, 0.2)
  # Overhead: absent is zero, and a number written as text is that number.
  expect_equal(gr_budget("gpt-4o", overhead = NULL)$input, gr_budget("gpt-4o", overhead = 0)$input)
  expect_equal(gr_budget("gpt-4o", overhead = "200")$input, gr_budget("gpt-4o", overhead = 200)$input)
  # But one number: the first of c(100, 9000) left 9000 tokens uncounted, and a
  # factor's first level is 1 whatever its label says.
  expect_error(gr_budget("gpt-4o", overhead = c(100, 9000)), class = "gr_budget_error")
  expect_error(gr_budget("gpt-4o", overhead = factor("300")), class = "gr_budget_error")
  # An output reserve nobody can read takes the default reserve, not clamp()'s
  # floor of ONE token.
  expect_warning(b <- gr_budget("gpt-4o", reserve_output = NA), class = "gr_bad_setting")
  expect_identical(b$output, gr_budget("gpt-4o")$output)
  expect_gt(b$output, 1L)
})

test_that("a study whose weight is unknown counts as an average one, not a weightless one", {
  claims <- data.frame(claim_id = 1:3, n_support = c(2L, 2L, 1L), n_contradict = 0L)
  support <- data.frame(claim_id = c(1, 1, 2, 2, 3), study = c(1, 2, 3, 4, 5))
  # Claim 1's studies are both unknown; claim 2's are known and light. Imputing
  # within the claim gave the mean of nothing, NaN, which sum(na.rm = TRUE) made
  # 0 -- so claim 1 ranked below claim 2 on no evidence at all.
  w <- c(`1` = NA, `2` = NA, `3` = 0.6, `4` = 0.6, `5` = 1.5)
  expect_identical(readgpt:::claim_order(claims, support, w), c(1L, 2L, 3L))
  # With no weight known anywhere, every study counts the same.
  expect_identical(readgpt:::claim_order(claims, support, c(`1` = NA, `2` = NA, `3` = NA,
                                                          `4` = NA, `5` = NA)), c(1L, 2L, 3L))
})

test_that("a model's claim or heading is read by its own key and no other", {
  # `$` let `claim_draft` answer for `claim` and `headings` for `heading`, so a
  # key the schema never defined produced a real row.
  expect_null(readgpt:::claim_rows(list(list(claim_draft = "X helps.", supported_by = 1))))
  expect_null(readgpt:::claim_rows(data.frame(claim_draft = "X helps.", supported_by = 1)))
  ok <- readgpt:::claim_rows(data.frame(claim = "X helps.", supported_by = 1))
  expect_identical(ok$claim, "X helps.")
  expect_equal(nrow(readgpt:::outline_rows(list(list(headings = "H", brief = "b", claims = 1)))), 0L)
  expect_equal(nrow(readgpt:::outline_rows(data.frame(headings = "H", brief = "b", claims = 1))), 0L)
})

test_that("every setting's missing-value fallback is its formal default", {
  # The fallback is written out by hand, apart from the signature, and the two
  # drifted: gr_segment_spec(max_tokens = NA) cut 800-token chunks while leaving
  # the argument out cut 1200-token ones.
  found <- list()
  walk <- function(e) {
    if (!is.call(e)) return(invisible())
    if (identical(e[[1]], as.name("na_default")) && is.name(e[[2]])) {
      found[[length(found) + 1L]] <<- e
    }
    invisible(lapply(as.list(e)[-1], function(x) if (!missing(x)) walk(x)))
  }
  checked <- character(0)
  for (fn in c("gr_ingest_spec", "gr_segment_spec", "gr_read_spec")) {
    f <- get(fn, envir = asNamespace("readgpt"))
    found <- list()
    walk(body(f))
    for (cl in found) {
      arg <- as.character(cl[[2]])
      if (!arg %in% names(formals(f))) next
      expect_equal(as.numeric(eval(cl[[3]])), as.numeric(eval(formals(f)[[arg]])),
                   info = paste0(fn, "(", arg, ")"))
      checked <- c(checked, paste0(fn, "(", arg, ")"))
    }
  }
  expect_gte(length(checked), 20L)
  expect_true("gr_segment_spec(max_tokens)" %in% checked)
  # And the functions that hand a token count on to gr_read_spec() settle an
  # NA against their OWN default, and range-check it, under their own name:
  # gr_read_spec() would warn about `max_answer_tokens`, which the caller never
  # passed.
  for (fn in c("gr_claims", "gr_synthesise")) {
    f <- get(fn, envir = asNamespace("readgpt"))
    found <- list()
    walk(body(f))
    args <- vapply(found, function(cl) as.character(cl[[2]]), character(1))
    tok <- grep("_tokens$", names(formals(f)), value = TRUE)
    expect_true(all(tok %in% args), info = fn)
    src <- paste(deparse(body(f)), collapse = " ")
    for (a in tok) expect_true(grepl(sprintf('clamp_warn\\(na_default\\(%s, [^)]*\\), [^)]*"%s"\\)',
                                             a, a), src), info = paste(fn, a))
    for (cl in found) {
      arg <- as.character(cl[[2]])
      if (arg %in% tok) {
        expect_equal(as.numeric(eval(cl[[3]])), as.numeric(eval(formals(f)[[arg]])),
                     info = paste0(fn, "(", arg, ")"))
      }
    }
  }
  # NULL is not a setting here any more than NA is: it warns and takes the
  # default, as it does for every other argument with a non-NULL default.
  expect_warning(s <- gr_read_spec("retrieve", min_score = NULL), class = "gr_bad_setting")
  expect_identical(s$min_score, -Inf)
})
