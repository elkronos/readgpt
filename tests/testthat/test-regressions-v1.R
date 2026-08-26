# test-regressions-v1.R
#
# One test per confirmed v1 defect. Each names the original failure so a future
# change that reintroduces it fails here with an explanation, not just a red dot.

test_that("token budgets are positive for every model, including unknown ones", {
  # v1: get_model_limits() fell through to context=4096, output=4096, so
  # `4096 - 4096 - 2000` = -2000. chunk_text_minimal(text, -2000) then emitted
  # one chunk -- and one API call -- per word.
  for (m in c("gpt-3.5-turbo", "gpt-3.5-turbo-0125", "gpt-4", "gpt-4o", "gpt-4o-mini",
              "gpt-4-turbo", "gpt-4.1", "gpt-5", "gpt-5.6-terra", "o3-mini",
              "some-model-released-after-this-package")) {
    b <- quiet(gr_budget(m, reserve_output = 1024, overhead = 200))
    expect_gt(b$input, 0)
    expect_lte(b$input + b$output, b$context_window)
  }
})

test_that("a model whose output reserve cannot fit errors instead of going negative", {
  local_registries()
  gr_register_model("tiny-ctx", context_window = 512, max_output = 400)
  expect_error(gr_budget("tiny-ctx", reserve_output = 400, overhead = 5000),
               class = "gr_budget_error")
})

test_that("gpt-4o is not mistaken for 8k-context gpt-4", {
  # v1: grepl("gpt-4", model) matched gpt-4o and returned an 8192 window,
  # over-chunking every document ~26x.
  expect_equal(gr_model_info("gpt-4o")$context_window, 128000L)
  expect_equal(gr_model_info("gpt-4")$context_window, 8192L)
})

test_that("semantic segmentation survives a one-paragraph document", {
  # v1: `for (i in 2:length(paragraphs))` iterated c(2,1) at length 1, indexed
  # paragraphs[2] = NA, and died in `if (overlap > 0 || nchar(NA) < 200)`.
  one <- gr_ingest("A single paragraph with no blank lines at all in it.")
  for (m in c("fixed", "paragraph", "sentence", "recursive", "structural",
              "semantic", "contextual")) {
    ch <- quiet(gr_segment(one, list(method = m, max_tokens = 200), client = mock_echo()))
    expect_s3_class(ch, "gr_chunks")
    expect_gte(nrow(ch$chunks), 1L)
  }
})

test_that("no segmenter ever emits a chunk over its token cap", {
  # v1: chunk_text_semantic() had no size check on its `current_chunk <- para`
  # branch and emitted single paragraphs 50x over the limit.
  doc <- gr_ingest(paste(sample_doc(3, 3),
                         paste(rep("verylongparagraphwithoutbreaks", 400), collapse = " "),
                         sep = "\n\n"))
  for (m in c("fixed", "paragraph", "sentence", "recursive", "structural", "semantic")) {
    ch <- quiet(gr_segment(doc, list(method = m, max_tokens = 150), client = mock_echo()))
    expect_lte(max(ch$chunks$tokens), 150L,
               label = sprintf("max chunk tokens for segmenter '%s'", m))
  }
})

test_that("chunkers return an empty-but-typed result, never NULL", {
  # v1: chunk_text_naive() returned NULL when nothing survived, and parse_text()
  # cached and returned that NULL.
  expect_error(gr_ingest(""), class = "gr_error")
  expect_error(gr_ingest("   \n\n   "), class = "gr_error")
})

test_that("boilerplate cleaners run before destructive ones", {
  # v1: parse_text() stripped digits BEFORE calling filter_text(), whose
  # patterns all require \\d+. The whole feature was dead code by default.
  txt <- "Page 1\n\nFigure 2: A chart.\n\nReal body content that is long enough to survive."
  out <- gr_clean(txt, steps = c("remove_numbers", "page_numbers", "captions"))
  expect_false(grepl("Page", out))
  expect_false(grepl("Figure", out))
  expect_true(grepl("Real body content", out))
})

test_that("digits survive ingestion by default", {
  # v1: remove_numbers defaulted to TRUE and answer_question() never forwarded
  # an override, so "revenue of 45 million in 2019" reached the model as
  # "revenue of million in".
  doc <- gr_ingest("Acme reported revenue of 45 million dollars in 2019 and 87 million in 2020.")
  expect_true(grepl("45", doc$text))
  expect_true(grepl("2019", doc$text))
  legacy <- gr_ingest("Acme reported revenue of 45 million dollars in 2019 and 87 million in 2020.",
                      "legacy_v1")
  expect_false(grepl("45", legacy$text))
})

test_that("the references cleaner removes the bibliography, not a sentence", {
  # v1: sub("(?mi)References.*", "", text, perl = TRUE) -- without (?s), `.` does
  # not cross newlines, so it deleted the heading line and kept every reference.
  # Unanchored, it also truncated "See References section for details about X."
  body <- paste(rep("Body sentence that carries real content.", 12), collapse = " ")
  txt <- paste0(body, "\n\nReferences\n\n[1] Smith 2020\n[2] Jones 2019\n")
  out <- gr_clean(txt, steps = "references")
  expect_false(grepl("Smith 2020", out))
  expect_true(grepl("Body sentence", out))

  mid <- paste0("See References section for details about method X. ", body)
  expect_true(grepl("details about method X", gr_clean(mid, steps = "references")))
})

test_that("URL removal does not stop at the letter s", {
  # v1: gsub("www\\.[^\\s]+", ...) without perl=TRUE -- TRE reads [^\s] as
  # "not backslash and not s", so www.nasa.gov became the fragment "sa.gov".
  out <- gr_clean("Visit www.nasa.gov today and www.sciencedirect.com/x too.", steps = "urls")
  expect_false(grepl("nasa", out))
  expect_false(grepl("sciencedirect", out))
  expect_false(grepl("sa.gov", out, fixed = TRUE))
})

test_that("the ingestion cache key covers every option that changes the output", {
  # v1: the key was the file path alone, so parse_text(f, chunk_method="semantic")
  # returned the naive chunks cached by an earlier call.
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("Alpha 123 para.", "", "Beta 456 para."), f)
  a <- gr_ingest(f, gr_ingest_spec(clean = "standard"))
  b <- gr_ingest(f, gr_ingest_spec(clean = "legacy_v1"))
  expect_false(identical(a$text, b$text))
  expect_true(grepl("123", a$text))
  expect_false(grepl("123", b$text))
})

test_that("editing the file on disk invalidates the ingestion cache", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("Original content here.", "", "Second original paragraph."), f)
  a <- gr_ingest(f)
  Sys.sleep(1.1)   # mtime has 1-second resolution on some filesystems
  writeLines(c("Completely different content.", "", "Another different paragraph."), f)
  b <- gr_ingest(f)
  expect_false(identical(a$text, b$text))
})

test_that("a failed model call contributes nothing, never the string 'NULL'", {
  # v1: sapply() over responses collapsed to a list when one was NULL, nzchar()
  # deparsed it, and the literal "NULL" was spliced into the merge prompt.
  doc <- gr_ingest(sample_doc(2, 2))
  ch <- gr_segment(doc, list(method = "paragraph", max_tokens = 120))
  flaky <- gr_mock_client(local({
    n <- 0L
    function(messages, params) { n <<- n + 1L; if (n %% 2L == 0L) stop("boom") else "Real finding." }
  }))
  a <- quiet(gr_read(ch, "What happened?", flaky, "map_reduce"))
  prompts <- unlist(lapply(flaky$calls(), function(c)
    vapply(c$messages, function(m) m$content, character(1))))
  expect_false(any(grepl("\\bNULL\\b", prompts)))
  expect_true(a$partial)
})

test_that("an empty completion does not crash any reader", {
  # v1: process_api_call() returned character(0) for content:null, and
  # `is.null(x) || !nzchar(x)` evaluated to NA -> "missing value where
  # TRUE/FALSE needed", after the call had been paid for.
  doc <- gr_ingest(sample_doc(2, 2))
  ch <- gr_segment(doc, list(method = "paragraph", max_tokens = 150))
  for (r in c("stuff", "map_reduce", "refine", "skim", "hierarchical", "retrieve")) {
    a <- quiet(gr_read(ch, "Q?", mock_empty(), list(reader = r, top_k = 2)))
    expect_s3_class(a, "gr_answer")
    expect_type(a$answer, "character")
    expect_length(a$answer, 1L)
  }
})

test_that("model responses keep their formatting", {
  # v1: gsub("\\n+", " ", result) flattened every answer, destroying lists,
  # tables and code blocks unconditionally.
  cl <- gr_mock_client(function(m, p) "1. First\n2. Second\n\n- bullet\n- bullet")
  a <- gr_read(gr_segment(gr_ingest(sample_doc(1, 2)), "paragraph"), "Q?", cl, "stuff")
  expect_true(grepl("\n", a$answer, fixed = TRUE))
})

test_that("'not found' matching is exact, not a substring sweep", {
  # v1: grepl("not found|no information|not applicable", ...) discarded
  # "The log was not found, but revenue was 45 million dollars."
  cl <- gr_mock_client(function(m, p) "The server log was not found, but revenue was 45 million dollars.")
  a <- gr_read(gr_segment(gr_ingest(sample_doc(1, 2)), "paragraph"), "Revenue?", cl, "map_reduce")
  expect_true(grepl("45 million", a$answer))
})

test_that("embeddings depend on content, not string length, and leave the RNG alone", {
  # v1: compute_embedding() was set.seed(nchar(text)); runif(768). Two
  # 48-character strings had cosine similarity exactly 1.0, and the global RNG
  # stream was clobbered on every call.
  cl <- mock_echo()
  a <- "The mitochondrion is the powerhouse of the cell."
  b <- "Quarterly revenue fell by twelve percent in Q3!!"
  expect_equal(nchar(a), nchar(b))
  e <- gr_embed(cl, c(a, b))
  expect_false(isTRUE(all.equal(as.numeric(e[1, ]), as.numeric(e[2, ]))))

  set.seed(42); before <- sample(1000, 5)
  set.seed(42); invisible(gr_embed(cl, c(a, b, "third text")))
  after <- sample(1000, 5)
  expect_identical(before, after)
})

test_that("refine works end to end and accepts forwarded arguments", {
  # v1: refine_answer() called search_text(), which was never defined anywhere
  # in the repository, and had no `...` while main.R forwarded `...` into it --
  # so refine = TRUE could not work under any circumstances.
  doc_text <- sample_doc(2, 2)
  cl <- mock_echo("Refined answer.")
  a <- answer_document(doc_text, "What is it?",
                       gr_recipe("r", segment = "paragraph", read = "refine"),
                       client = cl, model = "gpt-4o", temperature = 0)
  expect_s3_class(a, "gr_answer")
  expect_false(a$partial)
})

test_that("unknown overrides raise an error instead of being silently discarded", {
  # v1: answer_question(f, q, remove_numbers = FALSE) ran cleanly, the argument
  # vanished into a downstream `...`, and digits were stripped anyway.
  expect_error(
    answer_document(sample_doc(1, 2), "Q?", "fast", client = mock_echo(),
                    definitely_not_a_real_parameter = TRUE),
    class = "gr_unknown_override")
})

test_that("out-of-range segmentation settings are clamped loudly", {
  # v1: a negative token limit went straight into
  # split(tokens, ceiling(seq_along(tokens) / limit)), whose negative group
  # indices sort ascending -- feeding the document to the model backwards.
  expect_warning(gr_segment_spec(max_tokens = 100, overlap_tokens = 500), class = "gr_clamped")
  expect_warning(gr_segment_spec(max_tokens = -50), class = "gr_clamped")
  s <- quiet(gr_segment_spec(max_tokens = -50, overlap_tokens = 500))
  expect_gt(s$max_tokens, 0L)
  expect_lt(s$overlap_tokens, s$max_tokens)
})

test_that("a runaway call count is refused before any money is spent", {
  # v1 had no equivalent guard, which is how a negative budget became
  # ~10,001 API calls on a 10,000-word document.
  doc <- gr_ingest(sample_doc(6, 6))
  ch <- gr_segment(doc, list(method = "sentence", max_tokens = 40))
  cl <- mock_echo()
  withr::local_options(list())
  old <- gr_options(max_calls = 5L)
  on.exit(gr_options(old), add = TRUE)
  expect_error(gr_read(ch, "Q?", cl, "map_reduce"), class = "gr_call_cap")
  expect_length(cl$calls(), 0L)
})

test_that("estimated cost is checked before the run starts", {
  doc <- gr_ingest(sample_doc(6, 6))
  ch <- gr_segment(doc, list(method = "sentence", max_tokens = 40))
  old <- gr_options(max_cost_usd = 1e-9, max_calls = 10000L)
  on.exit(gr_options(old), add = TRUE)
  expect_error(gr_read(ch, "Q?", mock_echo(), list(reader = "map_reduce", model = "gpt-4o")),
               class = "gr_cost_cap")
})

# ---------------------------------------------------------------------------
# Defects found while auditing the documentation against the implementation.
# ---------------------------------------------------------------------------

test_that("a mistyped file path is an error, not a document", {
  # Falling through to the inline-text branch meant "~/reports/q3_final.pdf"
  # became the document: a billed call, partial = FALSE, and a confident answer
  # about a filename.
  expect_error(gr_ingest("/no/such/directory/quarterly_report_final_v2.pdf"),
               class = "gr_file_not_found")
  expect_error(answer_document("does_not_exist.docx", "Q?", "fast", client = mock_echo()),
               class = "gr_file_not_found")
  # Ordinary prose that merely contains a dot is still treated as text.
  expect_s3_class(gr_ingest("A sentence about version 2.1 of the specification."),
                  "gr_document")
})

test_that("enforcing the token cap preserves page and section provenance", {
  doc <- gr_ingest(gptread_example())
  ch <- gr_segment(doc, list(method = "structural", max_tokens = 60))
  expect_true(any(!is.na(ch$chunks$section)))
  # The cap-enforcement path rebuilds chunks; provenance must survive it.
  expect_lte(max(ch$chunks$tokens), 60L)
})

test_that("every provenance-carrying segmenter actually carries it", {
  doc <- gr_ingest(gptread_example())
  cl <- mock_echo()
  for (m in c("paragraph", "sentence", "structural", "semantic", "contextual")) {
    ch <- quiet(gr_segment(doc, list(method = m, max_tokens = 120), client = cl))
    expect_true(any(!is.na(ch$chunks$section)),
                label = sprintf("section provenance for segmenter '%s'", m))
  }
  # fixed and recursive work on concatenated text, so NA is the honest answer.
  for (m in c("fixed", "recursive")) {
    ch <- quiet(gr_segment(doc, list(method = m, max_tokens = 120), client = cl))
    expect_true(all(is.na(ch$chunks$section)))
  }
})

test_that("contextual reports the context source it actually used", {
  # Requesting llm blurbs without a client fell back to metadata but still
  # recorded context_source = "llm" -- the trace claimed work never done.
  doc <- gr_ingest(gptread_example())
  call <- function() gr_segment(doc, list(method = "contextual", context_source = "llm",
                                          max_tokens = 200), client = NULL)
  expect_warning(call(), class = "gr_segment_fallback")
  ch <- quiet(call())
  expect_equal(ch$extra$context_source, "metadata")
  # And the blurb it did write is the free metadata one.
  expect_match(ch$chunks$text[1], "^\\[Source:")
})

test_that("the stuff overflow error names a real option value", {
  local_registries()
  doc <- gr_ingest(gptread_example())
  ch <- gr_segment(doc, list(method = "sentence", max_tokens = 40))
  # No prices, so the cost cap cannot be checked -- which the package says out
  # loud. Assert that warning rather than letting it float, or a real regression
  # in the pricing guard would look exactly like this run.
  gr_register_model("tiny-for-overflow", context_window = 900, max_output = 128)
  err <- withCallingHandlers(
    tryCatch(
      gr_read(ch, "Q?", mock_echo(),
              list(reader = "stuff", model = "tiny-for-overflow", on_overflow = "error")),
      gr_overflow = function(e) conditionMessage(e)),
    warning = function(w) {
      expect_match(conditionMessage(w), "no pricing in the registry")
      invokeRestart("muffleWarning")
    })
  expect_type(err, "character")
  # Whatever the message suggests must be an accepted value.
  suggested <- regmatches(err, regexpr("on_overflow = '[a-z]+'", err))
  expect_gt(length(suggested), 0L)
  val <- gsub(".*'([a-z]+)'.*", "\\1", suggested)
  expect_silent(gr_read_spec("stuff", on_overflow = val))
})

test_that("out-of-range reader settings are clamped loudly, like segment settings", {
  expect_warning(gr_read_spec("map_reduce", top_k = -5), class = "gr_clamped")
  expect_warning(gr_read_spec("hierarchical", fan_in = 999), class = "gr_clamped")
  expect_warning(gr_read_spec("iterative", max_rounds = 999), class = "gr_clamped")
  # And the clamp the spec applies is the one the reader honours -- fan_in was
  # stored as 64 but silently re-clamped to 32 inside the reader.
  expect_equal(quiet(gr_read_spec("hierarchical", fan_in = 999))$fan_in, 32L)
})

test_that("the bundled example document exists and ingests", {
  expect_true(file.exists(gptread_example()))
  doc <- gr_ingest(gptread_example())
  expect_gt(doc$stats$tokens, 100L)
  expect_true(any(!is.na(doc$blocks$section)))
  # Digits must survive -- the whole point of the v1 remove_numbers fix.
  expect_true(grepl("45.2", doc$text, fixed = TRUE))
})

test_that("every exported function has a help topic with a value section", {
  # Cheap guard against documentation drifting away from the exports.
  exported <- setdiff(getNamespaceExports("gptread"), "%||%")
  for (fn in exported) {
    topic <- utils::help((fn), package = "gptread", try.all.packages = FALSE)
    expect_gt(length(topic), 0L, label = sprintf("help topic for '%s'", fn))
  }
})
