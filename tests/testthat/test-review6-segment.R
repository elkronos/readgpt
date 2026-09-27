# test-review6-segment.R -- the medium and low segmentation findings.
#
# Each block names the finding it guards against. Non-ASCII text is written
# with \u escapes so the source stays ASCII.

# A document of blocks with the pages and sections given, through a registered
# extractor, so a segmenter sees provenance as it would from a PDF.
r6_doc <- function(text, page = NA_integer_, section = NA_character_, env = parent.frame()) {
  local_registries(env)
  name <- paste0("rsix", paste(sample(letters, 8), collapse = ""))
  blocks <- data.frame(text = text, page = page, section = section, kind = "body",
                       stringsAsFactors = FALSE)
  gr_register_extractor(name, name, function(path, spec) blocks)
  f <- withr::local_tempfile(fileext = paste0(".", name), .local_envir = env)
  writeLines("placeholder", f)
  quiet(gr_ingest(f))
}

# Every warning an expression raises, muffled, with its value.
r6_warnings <- function(expr) {
  w <- list()
  value <- withCallingHandlers(suppressMessages(expr), warning = function(cnd) {
    w[[length(w) + 1L]] <<- cnd
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = w,
       messages = vapply(w, conditionMessage, character(1)))
}

r6_trial_paragraphs <- function(n = 14) {
  paste(sprintf("Paragraph %d says site %d enrolled %d patients in the trial and followed them for a year.",
                seq_len(n), seq_len(n), 10 * seq_len(n)), collapse = "\n\n")
}

# ---------------------------------------------------------------------------
# money-08: a contextual chunk left without its paid context line is said.
# ---------------------------------------------------------------------------

r6_blurb_fails <- function() {
  gr_mock_client(function(messages, params) {
    if (grepl("You situate an excerpt", messages[[1]]$content, fixed = TRUE)) {
      stop("HTTP 503 service unavailable")
    }
    "Site 3 enrolled 30 patients."
  })
}

test_that("contextual chunks whose context call failed are counted and warned about", {
  local_clean_cache()
  spec <- gr_segment_spec("contextual", context_source = "llm", max_tokens = 200)
  got <- r6_warnings(gr_segment(r6_trial_paragraphs(), spec, client = r6_blurb_fails()))
  ch <- got$value
  n <- nrow(ch$chunks)
  expect_false(any(startsWith(ch$chunks$text, "[")))
  expect_identical(ch$extra$blurbs_missing, n)
  expect_identical(ch$extra$blurbs_at_limit, 0L)
  fell <- Filter(function(w) inherits(w, "gr_segment_fallback"), got$warnings)
  expect_length(fell, 1L)
  expect_match(conditionMessage(fell[[1]]), sprintf("%d of %d contextual chunk", n, n), fixed = TRUE)
  expect_match(conditionMessage(fell[[1]]), "request failed", fixed = TRUE)
  # Carried with the chunk set, which is how it reaches an answer.
  expect_true(any(grepl("no context line", ch$warnings, fixed = TRUE)))
})

test_that("contextual chunks skipped at the call cap are counted and warned about", {
  local_clean_cache()
  local_registries()
  gr_options(max_calls = 3)
  cl <- gr_mock_client(function(messages, params) "This excerpt is about enrolment.")
  spec <- gr_segment_spec("contextual", context_source = "llm", max_tokens = 200)
  got <- r6_warnings(gr_segment(r6_trial_paragraphs(), spec, client = cl))
  ch <- got$value
  headed <- sum(startsWith(ch$chunks$text, "["))
  expect_identical(headed, 3L)
  expect_identical(ch$extra$blurbs_at_limit, nrow(ch$chunks) - 3L)
  expect_identical(ch$extra$blurbs_missing, nrow(ch$chunks) - 3L)
  expect_true(any(grepl("skipped at the run's call or cost limit", got$messages, fixed = TRUE)))
})

test_that("an answer read from contextual chunks without their context says so", {
  local_clean_cache()
  recipe <- gr_recipe(segment = list(method = "contextual", context_source = "llm",
                                     max_tokens = 200),
                      read = "retrieve")
  got <- r6_warnings(answer_document(r6_trial_paragraphs(), "How many patients did site 3 enrol?",
                                     recipe = recipe, client = r6_blurb_fails()))
  expect_true(any(grepl("no context line", got$value$warnings, fixed = TRUE)))
})

test_that("contextual chunks that all got their context line raise nothing", {
  local_clean_cache()
  cl <- gr_mock_client(function(messages, params) "This excerpt is about enrolment.")
  spec <- gr_segment_spec("contextual", context_source = "llm", max_tokens = 200)
  got <- r6_warnings(gr_segment(r6_trial_paragraphs(), spec, client = cl))
  expect_length(got$warnings, 0L)
  expect_identical(got$value$extra$blurbs_missing, 0L)
  expect_true(all(startsWith(got$value$chunks$text, "[")))
})

# ---------------------------------------------------------------------------
# segment-08: merging a runt backward does not repeat its overlap tail.
# ---------------------------------------------------------------------------

test_that("a runt merged backward with overlap on does not repeat sentences", {
  words <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
             "ten", "eleven", "twelve")
  sents <- sprintf("Sentence number %s is here.", words)
  ch <- quiet(gr_segment(paste(sents, collapse = " "),
                         list(method = "sentence", max_tokens = 60, overlap_tokens = 10,
                              min_tokens = 30)))
  for (s in sents) {
    per_chunk <- lengths(regmatches(ch$chunks$text, gregexpr(s, ch$chunks$text, fixed = TRUE)))
    expect_true(all(per_chunk <= 1L), info = s)
  }
  # Every sentence still reaches a chunk.
  expect_true(all(vapply(sents, function(s) any(grepl(s, ch$chunks$text, fixed = TRUE)),
                         logical(1))))
})

test_that("runt merging with overlap keeps each chunk a contiguous span", {
  flat <- function(x) gsub("\\s+", " ", x)
  vocab <- c("Revenue", "grew", "sharply", "in", "the", "region", "while", "costs", "fell")
  # Seed 10 is one where a merged runt repeated its tail under `recursive`.
  for (seed in c(10L, 1:5)) {
    sents <- withr::with_seed(seed, vapply(1:30, function(i) {
      paste0(paste(sample(vocab, sample(3:9, 1), TRUE), collapse = " "), " ", i, ".")
    }, character(1)))
    txt <- paste(vapply(split(sents, rep(1:10, each = 3)), paste, character(1), collapse = " "),
                 collapse = "\n\n")
    for (m in c("sentence", "paragraph", "recursive")) {
      ch <- quiet(gr_segment(txt, list(method = m, max_tokens = 40, overlap_tokens = 12,
                                       min_tokens = 30)))
      for (k in seq_len(nrow(ch$chunks))) {
        expect_true(grepl(flat(trimws(ch$chunks$text[k])), flat(txt), fixed = TRUE),
                    info = paste(seed, m, k))
      }
    }
  }
})

# ---------------------------------------------------------------------------
# segment-04: the fixed segmenter keeps its final window.
# ---------------------------------------------------------------------------

test_that("fixed keeps a final window whose text also occurs in the window before", {
  minutes <- paste("Item 4. The treasurer moved to approve the budget for the library roof. Motion carried.",
                   "Item 5. The secretary moved to adopt the records policy. Motion carried.",
                   "Item 6. Ms Alvarez moved to reject the parking proposal. Motion carried.")
  ch <- quiet(gr_segment(minutes, list(method = "fixed", max_tokens = 56)))
  # The final window is the closing "Motion carried.", which also occurs in
  # the window before; every word of the minutes is still in some chunk.
  expect_true(endsWith(ch$chunks$text[nrow(ch$chunks)], "Motion carried."))
  expect_identical(sum(lengths(strsplit(ch$chunks$text, " "))), 39L)
  rep_txt <- paste(rep("alpha beta gamma delta", 12), collapse = " ")
  ch <- quiet(gr_segment(rep_txt, list(method = "fixed", max_tokens = 40)))
  expect_identical(sum(lengths(strsplit(ch$chunks$text, " "))), 48L)
})

# ---------------------------------------------------------------------------
# segment-14: the fixed segmenter is not quadratic in its window.
# ---------------------------------------------------------------------------

test_that("fixed finds each window without re-counting it word by word", {
  local_registries()
  n_calls <- 0L
  gr_set_tokenizer("r6_counting", function(x) {
    n_calls <<- n_calls + 1L
    vapply(x, function(s) length(strsplit(trimws(s), "\\s+")[[1]]), integer(1), USE.NAMES = FALSE)
  })
  w <- rep(c("alpha", "beta", "gamma", "delta"), 1000)
  ch <- quiet(gr_segment(paste(w, collapse = " "), list(method = "fixed", max_tokens = 1e5)))
  expect_identical(nrow(ch$chunks), 1L)
  expect_identical(ch$chunks$text, paste(w, collapse = " "))
  # Growing the window one word at a time counted it 4,000 times.
  expect_lt(n_calls, 200L)
  # Overlap is still honoured, measured in words under this tokenizer.
  ch <- quiet(gr_segment(paste(w[1:100], collapse = " "),
                         list(method = "fixed", max_tokens = 40, overlap_tokens = 10)))
  expect_true(all(ch$chunks$tokens <= 40L))
  first <- strsplit(ch$chunks$text[1], " ")[[1]]
  second <- strsplit(ch$chunks$text[2], " ")[[1]]
  expect_identical(utils::tail(first, 9L), second[1:9])
})

# ---------------------------------------------------------------------------
# segment-07: a semantic chunk across pages does not claim its first page.
# ---------------------------------------------------------------------------

test_that("a semantic chunk spanning two pages reports no single page", {
  doc <- r6_doc(c("Revenue in region 1 rose sharply. Revenue in region 2 rose sharply.",
                  "Revenue in region 3 rose sharply. Revenue in region 4 rose sharply.",
                  "The weather was cold. The weather was wet. The weather was grey."),
                page = c(1L, 2L, 2L))
  cl <- gr_mock_client(function(messages, params) "x",
    embed_handler = function(texts, params) {
      t(vapply(texts, function(s) {
        v <- numeric(64); v[if (grepl("weather", s)) 2L else 1L] <- 1; v
      }, numeric(64), USE.NAMES = FALSE))
    })
  ch <- quiet(gr_segment(doc, list(method = "semantic", max_tokens = 40, semantic_window = 1),
                         client = cl))
  revenue <- grepl("region 1", ch$chunks$text, fixed = TRUE)
  expect_true(grepl("region 4", ch$chunks$text[revenue], fixed = TRUE))
  expect_true(is.na(ch$chunks$page[revenue]))
  expect_true(is.na(ch$chunks$block_id[revenue]))
  # A piece wholly on one page keeps it.
  weather <- grepl("weather", ch$chunks$text, fixed = TRUE) & !revenue
  expect_identical(ch$chunks$page[weather], 2L)
})

# ---------------------------------------------------------------------------
# segment-06: the contextual header has room before packing.
# ---------------------------------------------------------------------------

r6_near_cap_doc <- function() {
  para <- function(i, n) {
    paste(rep(sprintf("Finding %d shows that the treatment group improved on the primary endpoint.", i),
              n), collapse = " ")
  }
  md <- paste(c("## Results of the randomised multicentre efficacy and safety evaluation",
                vapply(1:12, function(i) para(i, c(12, 11, 12, 10)[(i - 1) %% 4 + 1]), character(1))),
              collapse = "\n\n")
  f <- withr::local_tempfile(fileext = ".md", .local_envir = parent.frame())
  writeLines(md, f)
  quiet(gr_ingest(f))
}

test_that("contextual chunks near the cap keep their header and their part count", {
  local_clean_cache()
  doc <- r6_near_cap_doc()
  ch <- quiet(gr_segment(doc, list(method = "contextual", max_tokens = 200)))
  n <- nrow(ch$chunks)
  expect_null(ch$extra$cap_enforced)
  expect_true(all(ch$chunks$tokens <= 200L))
  expect_true(all(startsWith(ch$chunks$text, "[Source: ")))
  expect_identical(regmatches(ch$chunks$text, regexpr("Part [0-9]+ of [0-9]+", ch$chunks$text)),
                   sprintf("Part %d of %d", seq_len(n), n))
})

test_that("contextual chunks near the cap keep a model-written header", {
  local_clean_cache()
  doc <- r6_near_cap_doc()
  long <- paste(rep("This excerpt reports a finding from the results section of the trial.", 5),
                collapse = " ")
  cl <- gr_mock_client(function(messages, params) long)
  ch <- quiet(gr_segment(doc, list(method = "contextual", max_tokens = 200, context_source = "llm"),
                         client = cl))
  expect_null(ch$extra$cap_enforced)
  expect_true(all(ch$chunks$tokens <= 200L))
  expect_true(all(startsWith(ch$chunks$text, "[This excerpt")))
  # One call per chunk: no chunk was cut after its call was paid for.
  expect_identical(length(cl$calls()), nrow(ch$chunks))
  # The body is whole: the source text of the chunks is the document's text.
  flat <- function(x) gsub("\\s+", " ", trimws(x))
  expect_true(identical(flat(paste(ch$chunks$source_text, collapse = " ")),
                        flat(paste(doc$blocks$text, collapse = " "))))
})

# ---------------------------------------------------------------------------
# segment-05: no chunk leaves gr_segment() over the cap.
# ---------------------------------------------------------------------------

test_that("an over-cap word inside a sentence is split, not passed over the cap", {
  local_registries()
  gr_register_segmenter("r6_by_bullet", fn = function(doc, spec, client, trace) {
    new_chunks(unlist(strsplit(doc$text, "\n(?=[-*])", perl = TRUE)), "r6_by_bullet", spec)
  })
  blob <- paste(rep("QUJDRGVmZ2hpams", 200), collapse = "")
  txt <- paste0("Items:\n- First bullet.\n- See the attached file. The attachment was ", blob,
                " and nothing else.\n- Third bullet.")
  ch <- quiet(gr_segment(txt, list(method = "r6_by_bullet", max_tokens = 500)))
  expect_identical(gr_chunk_stats(ch)$over_cap, 0L)
  # The blob is cut, not lost: its pieces, in order, are the blob.
  expect_true(grepl(blob, gsub("\\s", "", paste(ch$chunks$text, collapse = "")), fixed = TRUE))
  pieces <- readgpt:::hard_split(paste("See the attached file. The attachment was", blob,
                                       "and nothing else."), 500)
  expect_true(all(gr_count_tokens(pieces) <= 500L))
  expect_true(any(grepl(substr(blob, 1, 40), pieces, fixed = TRUE)))
})

test_that("a tokenizer that counts joining spaces still gets chunks within the cap", {
  local_registries()
  gr_set_tokenizer("chars")
  yes <- paste(rep("Yes. No. Ok. Go.", 200), collapse = " ")
  for (m in c("paragraph", "sentence", "structural", "recursive", "fixed")) {
    ch <- quiet(gr_segment(yes, list(method = m, max_tokens = 64)))
    expect_true(all(ch$chunks$tokens <= 64L), info = m)
    # Packed within the cap by the segmenter itself, not re-cut afterwards.
    expect_null(ch$extra$cap_enforced, info = m)
  }
  expect_true(all(gr_count_tokens(readgpt:::hard_split(yes, 64)) <= 64L))
  # And overlap still fits beside the unit it is carried to.
  ch <- quiet(gr_segment(yes, list(method = "sentence", max_tokens = 64, overlap_tokens = 16)))
  expect_true(all(ch$chunks$tokens <= 64L))
  expect_null(ch$extra$cap_enforced)
})

test_that("a cap that cannot be enforced stops rather than passing a chunk over it", {
  local_registries()
  gr_set_tokenizer("r6_huge", function(x) 100L * nchar(x))
  expect_error(quiet(gr_segment("A few short words, long enough to ingest.",
                                list(method = "paragraph", max_tokens = 32))),
               class = "gr_cap_unenforceable")
})

# ---------------------------------------------------------------------------
# r2-non-latin-text-pipeline-08: inline headings in any script.
# ---------------------------------------------------------------------------

test_that("structural finds accented and Cyrillic headings inline", {
  fr <- paste("1. ÉTUDE", "Nous avons étudié le traitement.",
              "2. MÉTHODES", "Nous avons recruté 300 patients.",
              "3. ÉVALUATION DE LA TOXICITÉ", "Aucun décès lié au traitement.",
              "4. RÉSULTATS", "La survie a augmenté.", sep = "\n\n")
  ch <- quiet(gr_segment(fr, list(method = "structural", max_tokens = 200)))
  tox <- grepl("Aucun décès", ch$chunks$text, fixed = TRUE)
  expect_identical(ch$chunks$section[tox], "3. ÉVALUATION DE LA TOXICITÉ")
  expect_true(startsWith(ch$chunks$text[tox], "## 3. ÉVALUATION"))
  expect_false(any(grepl("Nous avons recrut", ch$chunks$text[tox], fixed = TRUE)))
  expect_identical(ch$chunks$section[1], "1. ÉTUDE")

  ru <- paste("1. ВВЕДЕНИЕ",
              "Мы изучили лечение.",
              "РЕЗУЛЬТАТЫ",
              "Выживаемость выросла.",
              sep = "\n\n")
  ch <- quiet(gr_segment(ru, list(method = "structural", max_tokens = 200)))
  expect_identical(ch$chunks$section,
                   c("1. ВВЕДЕНИЕ",
                     "РЕЗУЛЬТАТЫ"))

  de <- paste("EINFÜHRUNG", "Wir haben die Behandlung untersucht.",
              "2.1 Methoden", "Wir haben Patienten rekrutiert.", sep = "\n\n")
  ch <- quiet(gr_segment(de, list(method = "structural", max_tokens = 200)))
  expect_identical(ch$chunks$section, c("EINFÜHRUNG", "2.1 Methoden"))
})

test_that("structural still takes a lower-case line after a number for text", {
  txt <- paste("1. INTRODUCTION", "We studied it.", "3 patients were enrolled in the trial.",
               "More text follows here.", sep = "\n\n")
  ch <- quiet(gr_segment(txt, list(method = "structural", max_tokens = 200)))
  expect_identical(unique(ch$chunks$section), "1. INTRODUCTION")
})

# ---------------------------------------------------------------------------
# segment-09: overlap text counts toward a chunk's page and block.
# ---------------------------------------------------------------------------

test_that("a chunk that opens with overlap from the page before reports no single page", {
  doc <- r6_doc(c("The contract covers the supply of pumps. It was signed in March by both parties.",
                  "Page four covers delivery terms. Delivery is due within sixty days of the order date."),
                page = c(3L, 4L))
  for (m in c("paragraph", "sentence")) {
    ch <- quiet(gr_segment(doc, list(method = m, max_tokens = 40, overlap_tokens = 20)))
    carried <- which(grepl("signed in March", ch$chunks$text, fixed = TRUE) &
                     grepl("Delivery is due", ch$chunks$text, fixed = TRUE))
    expect_length(carried, 1L)
    expect_true(is.na(ch$chunks$page[carried]), info = m)
    expect_true(is.na(ch$chunks$block_id[carried]), info = m)
  }
  # Without overlap the second chunk is wholly on page 4, and says so.
  ch <- quiet(gr_segment(doc, list(method = "paragraph", max_tokens = 40)))
  expect_identical(ch$chunks$page, c(3L, 4L))
})

# ---------------------------------------------------------------------------
# segment-13: overlap for text written without spaces.
# ---------------------------------------------------------------------------

test_that("overlap is carried for Chinese text", {
  zh_para <- function(i) {
    paste(rep(sprintf("第%d段说明收入在该地区大幅增长。", i),
              12), collapse = "")
  }
  doc <- quiet(gr_ingest(paste(vapply(1:6, zh_para, character(1)), collapse = "\n\n")))
  for (m in c("paragraph", "sentence")) {
    ch <- quiet(gr_segment(doc, list(method = m, max_tokens = 300, overlap_tokens = 60)))
    expect_true(nrow(ch$chunks) > 1L)
    for (k in 2:nrow(ch$chunks)) {
      prev <- ch$chunks$text[k - 1L]
      lead <- substr(ch$chunks$text[k], 1, 10)
      # The chunk opens with text from the end of the one before.
      expect_true(grepl(lead, prev, fixed = TRUE), info = paste(m, k))
    }
    expect_true(all(ch$chunks$tokens <= 300L))
  }
  tail <- readgpt:::tail_by_tokens(zh_para(1), 30)
  expect_true(nzchar(tail))
  expect_true(gr_count_tokens(tail) <= 30L)
  expect_true(endsWith(zh_para(1), tail))
})

test_that("overlap for Latin text still carries whole words only", {
  expect_identical(readgpt:::tail_by_tokens("the internationalization", 5), "")
  expect_identical(readgpt:::tail_by_tokens("alpha beta gamma delta epsilon", 5), "epsilon")
})

# ---------------------------------------------------------------------------
# segment-10: recursive never welds words together.
# ---------------------------------------------------------------------------

test_that("recursive keeps the space after a clause re-split on spaces", {
  contract <- paste("The supplier shall deliver all goods described in the schedule to the buyer's",
                    "premises and the buyer shall inspect them within five working days, after which",
                    "the goods are accepted in writing. Payment is due within 60 days.")
  doc <- quiet(gr_ingest(contract))
  ch <- quiet(gr_segment(doc, list(method = "recursive", max_tokens = 34)))
  expect_false(any(grepl("[.,;][A-Za-z]", ch$chunks$text)))
  # The pieces joined as the packer joins them are the text exactly.
  expect_identical(paste(ch$chunks$text, collapse = ""), doc$text)
})

test_that("recursive past the caller's separators does not weld sentences", {
  txt <- paste0("Intro line here.\n",
                paste(rep("The project team met on Monday and agreed the plan after a long discussion.", 3),
                      collapse = " "),
                " Closing remarks were brief.\nEnd.")
  doc <- quiet(gr_ingest(txt))
  ch <- quiet(gr_segment(doc, list(method = "recursive", max_tokens = 32,
                                   separators = c("\n\n", "\n"))))
  expect_true(all(ch$chunks$tokens <= 32L))
  expect_false(any(grepl("[.][A-Z]", ch$chunks$text)))
  expect_identical(paste(ch$chunks$text, collapse = ""), doc$text)
  # A word with no space in it is still cut by characters and rejoined exactly.
  blob <- paste(rep("QUJDRGVmZ2hpams", 30), collapse = "")
  doc <- quiet(gr_ingest(paste("Start.", blob, "End.")))
  ch <- quiet(gr_segment(doc, list(method = "recursive", max_tokens = 64)))
  expect_identical(paste(ch$chunks$text, collapse = ""), doc$text)
})

# ---------------------------------------------------------------------------
# segment-12: page keeps blocks that have no page.
# ---------------------------------------------------------------------------

test_that("page keeps blocks without a page number as chunks of their own", {
  doc <- r6_doc(c("Page one text.", "More page one.", "Page two text.",
                  "Appendix: the contract value is 4.2 million.", "Page three text."),
                page = c(1L, 1L, 2L, NA, 3L))
  got <- r6_warnings(gr_segment(doc, list(method = "page", max_tokens = 200)))
  ch <- got$value
  expect_identical(ch$chunks$text, c("Page one text.\n\nMore page one.", "Page two text.",
                                     "Appendix: the contract value is 4.2 million.",
                                     "Page three text."))
  expect_identical(ch$chunks$page, c(1L, 2L, NA, 3L))
  expect_true(any(grepl("no page number", got$messages, fixed = TRUE)))
  # A page-numbered document raises nothing.
  doc <- r6_doc(c("Page one text.", "Page two text."), page = 1:2)
  got <- r6_warnings(gr_segment(doc, list(method = "page", max_tokens = 200)))
  expect_length(got$warnings, 0L)
})
