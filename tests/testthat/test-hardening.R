# test-hardening.R
#
# Regression tests for defects found by adversarial review and by mutation
# testing. Each block names the concrete failure it prevents, because a test
# whose purpose is not written down gets deleted the first time it is
# inconvenient.
#
# Mutation testing drove roughly half of these: the suite was run against
# deliberately broken copies of the package (a comparison flipped, a guard
# deleted, a default changed) and every mutation that still passed is a claim
# the tests were not actually checking. Those are the ones below.

# ---------------------------------------------------------------------------
# Tokenising and truncation
# ---------------------------------------------------------------------------

test_that("the token estimate never undercounts, on any script", {
  hostile <- c(
    "hello world",
    strrep("a", 400),
    paste(rep("中文字", 60), collapse = ""),          # CJK
    paste(rep("\U0001F600", 40), collapse = ""),                   # astral emoji
    paste(rep("مرحبا", 40), collapse = " "),  # Arabic
    "https://example.com/a/very/long/path?with=query&and=more#frag",
    paste(rep("ABCDEFGHIJ", 50), collapse = ""),
    "{\"key\":\"value\",\"n\":[1,2,3,4,5,6,7,8,9,10]}",
    paste(rep("क्ष", 50), collapse = ""),           # Devanagari
    strrep("- ", 300),
    ""
  )
  for (s in hostile) {
    est <- gr_count_tokens(s)
    expect_true(is.integer(est) && !is.na(est) && est >= 0L)
    # A byte-per-token floor is a hard lower bound no real tokenizer beats.
    expect_gte(est, ceiling(nchar(s, type = "bytes") / 8))
  }
})

test_that("truncation respects the budget and survives degenerate inputs", {
  long <- paste(rep("The quarterly revenue figure was reported.", 200), collapse = " ")
  for (n in c(0, 1, 5, 50, 500)) {
    out <- gr_truncate_tokens(long, n)
    expect_lte(gr_count_tokens(out), max(n, 0))
  }
  # Inf used to crash: as.integer(Inf) is NA with a warning.
  expect_identical(gr_truncate_tokens(long, Inf), long)
  expect_identical(gr_truncate_tokens("", 100), "")
  expect_identical(gr_truncate_tokens(long, -5), "")
  expect_identical(gr_truncate_tokens(long, NA), "")
  # Space-free text still has to be cut: word granularity cannot help here.
  blob <- strrep("x", 5000)
  expect_lte(gr_count_tokens(gr_truncate_tokens(blob, 20)), 20)
  # A marker larger than the whole budget is dropped rather than emitted alone.
  short <- gr_truncate_tokens(long, 3, " ...[a very long truncation marker indeed]")
  expect_lte(gr_count_tokens(short), 3)
})

# ---------------------------------------------------------------------------
# Budget arithmetic -- the single chokepoint
# ---------------------------------------------------------------------------

test_that("gr_budget cannot return a non-positive input budget", {
  for (m in gr_models()$id[gr_models()$kind == "chat"]) {
    b <- gr_budget(m)
    expect_gt(b$input, 0)
    expect_lte(b$input + b$output, gr_model_info(m)$context_window)
  }
  # Overhead larger than the window has to raise, not return a negative number
  # that then becomes ceiling(n / negative) group indices.
  expect_error(gr_budget("gpt-4o-mini", overhead = 1e9), class = "gr_budget_error")
})

test_that("gr_register_model rejects an output reserve that exceeds the window", {
  local_registries()
  expect_error(gr_register_model("bad-1", context_window = 100, max_output = 200))
  expect_error(gr_register_model("bad-2", context_window = 0, max_output = 10))
})

test_that("a NULL price does not delete the column and break gr_models()", {
  local_registries()
  # modifyList() DELETES a key whose value is NULL, and data.frame() drops a
  # zero-length column, so one such registration used to make gr_models() throw
  # for the rest of the session.
  gr_register_model("nullprice-1", context_window = 1000, max_output = 100,
                    input_usd = NULL, output_usd = numeric(0), dimensions = NULL)
  tbl <- gr_models()
  expect_true("nullprice-1" %in% tbl$id)
  expect_true(is.na(tbl$input_usd_per_1m[tbl$id == "nullprice-1"]))
  expect_true(all(c("input_usd_per_1m", "output_usd_per_1m") %in% names(tbl)))
})

test_that("an unrecognised model in a known family warns rather than guessing silently", {
  expect_warning(gr_model_info("gpt-4.5-preview"), class = "gr_unknown_model")
  expect_false(quiet(gr_model_info("gpt-4.5-preview"))$certain)
  expect_true(gr_model_info("gpt-4o-mini")$certain)
})

# ---------------------------------------------------------------------------
# Ingest
# ---------------------------------------------------------------------------

test_that("CRLF and lone-CR line endings still split into paragraphs", {
  # Every structural regex in the package anchors on \n. A file saved on Windows
  # arrives as \r\n, so a document became ONE block: one chunk, one enormous
  # prompt, and every structure-aware segmenter silently degraded to `stuff`.
  expect_equal(nrow(gr_ingest("One para.\r\n\r\nTwo para.\r\n\r\nThree.")$blocks), 3)
  expect_equal(nrow(gr_ingest("One para.\r\rTwo para.\r\rThree.")$blocks), 3)
  f <- withr::local_tempfile(fileext = ".txt")
  con <- file(f, "wb"); writeBin(charToRaw("A one.\r\n\r\nB two.\r\n\r\nC three."), con); close(con)
  expect_equal(nrow(gr_ingest(f)$blocks), 3)
})

test_that("non-UTF-8 files are transcoded rather than crashing the run", {
  # readLines(encoding = "UTF-8") LABELS bytes as UTF-8; it does not convert
  # them. And enc2utf8() on an unlabelled string is a no-op in a UTF-8 locale
  # while validUTF8() then reports TRUE for the same bytes -- so the transcoding
  # guard tested the wrong thing and every latin1 file aborted the run inside
  # trimws() with "input string 1 is invalid UTF-8".
  latin <- paste(rep("Café naïve résumé. Revenue was 45.2 million dollars.", 4),
                 collapse = "\n\n")
  windows <- paste(rep("Café “smart quotes” — dash. Revenue was 45.2 million.", 4),
                   collapse = "\n\n")
  cases <- list(latin1 = iconv(latin, "UTF-8", "latin1"),
                cp1252 = iconv(windows, "UTF-8", "CP1252"),
                utf8   = latin,
                crlf   = gsub("\n", "\r\n", iconv(latin, "UTF-8", "latin1")))
  for (nm in names(cases)) {
    skip_if(is.na(cases[[nm]]), sprintf("cannot produce a %s fixture here", nm))
    f <- withr::local_tempfile(fileext = ".txt")
    con <- file(f, "wb"); writeBin(charToRaw(cases[[nm]]), con); close(con)
    d <- expect_no_error(quiet(gr_ingest(f)), message = sprintf("ingesting %s", nm))
    expect_true(validUTF8(d$text), label = sprintf("%s output is valid UTF-8", nm))
    expect_equal(nrow(d$blocks), 4L, label = sprintf("%s block count", nm))
    expect_true(grepl("45.2 million", d$text, fixed = TRUE))
  }
  # Bytes that are not decodable at all must not abort the run either.
  f <- withr::local_tempfile(fileext = ".txt")
  con <- file(f, "wb")
  writeBin(c(charToRaw("Readable start of the document here."), as.raw(c(0xff, 0xfe)),
             charToRaw("\n\nAnd a second paragraph of readable text.")), con)
  close(con)
  d <- expect_no_error(quiet(gr_ingest(f)))
  expect_true(validUTF8(d$text))
})

test_that("document-scoped cleaners fire and keep the block count aligned", {
  doc <- paste0(
    "Introduction\n\nThis paper studies revenue growth across twelve firms.\n\n",
    "Results\n\nRevenue rose twelve percent in the second half of the year.\n\n",
    "References\n\nSmith, J. (2020). A paper about things. Journal, 4(2), 100.\n")
  out <- quiet(gr_ingest(doc, "academic"))
  # references is scope = "document": applied per block it could never match,
  # because the heading and the entries are separate blocks.
  expect_false(grepl("Smith, J.", out$text, fixed = TRUE))
  expect_true(grepl("Revenue rose twelve percent", out$text, fixed = TRUE))
  # Block-aligned provenance must survive: page/section/block_id are indexed by
  # block, so a cleaner that changes the block COUNT corrupts every citation.
  expect_equal(nrow(out$blocks), length(out$blocks$text))
  expect_false(any(is.na(out$blocks$block_id)))

  hdr <- paste(unlist(lapply(1:12, function(i)
    c("ACME CONFIDENTIAL", sprintf("Body paragraph %d with real content here.", i)))),
    collapse = "\n\n")
  scanned <- quiet(gr_ingest(hdr, "scan"))
  expect_false(grepl("ACME CONFIDENTIAL", scanned$text, fixed = TRUE))
  expect_true(grepl("Body paragraph 7", scanned$text, fixed = TRUE))
})

test_that("cleaners do not destroy the content they are not aimed at", {
  # An undecorated number on its own line is far more often a table cell than a
  # page number; stripping those gutted every numeric column under the default
  # preset.
  expect_true(grepl("1200", gr_clean("North\n1200\nSouth\n980\n", steps = "page_numbers")))
  expect_false(grepl("12", gr_clean("Body.\n- 12 -\nMore.", steps = "page_numbers")))
  # [[:alnum:]] is ASCII-only under PCRE here, so the old pattern deleted every
  # accented and non-Latin letter along with the punctuation.
  expect_true(grepl("café", gr_clean("café résumé x!", steps = "remove_punctuation")))
  # ZWJ/ZWNJ are orthographically meaningful; stripping them mis-spelled
  # Devanagari and split one family emoji into three people.
  expect_true(grepl("‍", gr_clean("a‍b", steps = "control_chars"), fixed = TRUE))
  expect_false(grepl("​", gr_clean("a​b", steps = "control_chars"), fixed = TRUE))
})

test_that("a text argument that looks slightly path-like is not treated as a path", {
  for (p in c("The company reported quarterly revenue of 45.2",
              "Please refer to the methodology in section 3.2",
              "Our fiscal year ends in June. See appendix A.1")) {
    expect_no_error(quiet(gr_ingest(paste(rep(p, 3), collapse = " "))))
  }
  # A real-looking path with a registered extension must still fail loudly.
  expect_error(gr_ingest("/missing/report.pdf"), class = "gr_file_not_found")
  expect_error(gr_ingest("/missing/notes.markdown"), class = "gr_file_not_found")
})

test_that("inline text and a vector of blocks do not share a cache key", {
  local_clean_cache()
  d1 <- gr_ingest("First paragraph of the document.\n\nSecond paragraph of it.")
  d2 <- gr_ingest(c("First paragraph of the document.", "Different second block."))
  expect_false(identical(d1$text, d2$text))
})

# ---------------------------------------------------------------------------
# Segmentation
# ---------------------------------------------------------------------------

test_that("no segmenter ever exceeds its token cap", {
  doc <- gr_ingest(paste(sample_doc(3, 3),
                         strrep("unbrokenblobwithnospaces", 400),
                         "https://example.com/" , strrep("x", 3000), sep = "\n\n"))
  for (m in gr_segmenters()$name) {
    ch <- quiet(gr_segment(doc, list(method = m, max_tokens = 120, overlap_tokens = 20)))
    expect_gt(nrow(ch$chunks), 0)
    expect_lte(max(ch$chunks$tokens), 120,
               label = sprintf("segmenter '%s' max chunk tokens", m))
  }
})

test_that("overlap actually appears at chunk boundaries and does not weld words", {
  doc <- gr_ingest(sample_doc(2, 6))
  ch <- gr_segment(doc, list(method = "sentence", max_tokens = 120, overlap_tokens = 40))
  skip_if(nrow(ch$chunks) < 2)
  shared <- vapply(seq_len(nrow(ch$chunks) - 1L), function(i) {
    tail_words <- utils::tail(strsplit(ch$chunks$text[i], "\\s+")[[1]], 6)
    any(vapply(tail_words, function(w) grepl(w, ch$chunks$text[i + 1L], fixed = TRUE), logical(1)))
  }, logical(1))
  expect_true(all(shared))
  # The recursive segmenter re-attaches separators and used to join the overlap
  # tail with "", welding "delta" + "alpha" into "deltaalpha".
  rc <- gr_segment(doc, list(method = "recursive", max_tokens = 120, overlap_tokens = 40))
  expect_false(any(grepl("[a-z]{22,}", rc$chunks$text)))
})

test_that("min_tokens merges runt chunks instead of billing for them", {
  # Mutation survivor: deleting the runt-merge loop changed nothing any test
  # asserted on.
  units <- c(strrep("alpha ", 25), strrep("beta ", 25), "tiny bit")
  loose <- readgpt:::pack_units(units, max_tokens = 30, min_tokens = 0)
  tight <- readgpt:::pack_units(units, max_tokens = 30, min_tokens = 10)
  expect_lt(length(tight$text), length(loose$text))
  expect_lte(max(gr_count_tokens(tight$text)), 30)

  # The invariant, not just the one example: a chunk may sit below the minimum
  # only when absorbing it would have broken the cap.
  for (mt in c(30L, 45L, 60L)) {
    out <- readgpt:::pack_units(units, max_tokens = mt, min_tokens = as.integer(mt / 2))
    toks <- gr_count_tokens(out$text)
    expect_lte(max(toks), mt)
    for (i in seq_along(toks)[-1]) {
      if (toks[i] < mt / 2) {
        joined <- gr_count_tokens(paste(out$text[i - 1L], out$text[i], sep = "\n\n"))
        expect_gt(joined, mt)
      }
    }
  }
})

test_that("structural segmentation does not duplicate the heading it prefixes", {
  doc <- gr_ingest(paste(c("# Alpha Section", "Alpha body one with several words here.",
                           "# Beta Section", "Beta body one with several words here."),
                         collapse = "\n\n"))
  ch <- gr_segment(doc, list(method = "structural", max_tokens = 200))
  for (t in ch$chunks$text) {
    hits <- lengths(regmatches(t, gregexpr("Alpha Section|Beta Section", t)))
    expect_lte(hits, 1)
  }
  expect_false(any(grepl("## #", ch$chunks$text, fixed = TRUE)))
})

test_that("provenance is either right or NA, never a plausible-looking lie", {
  doc <- gr_ingest(sample_doc(3, 4))
  for (m in c("paragraph", "sentence", "structural")) {
    ch <- gr_segment(doc, list(method = m, max_tokens = 150))
    bid <- ch$chunks$block_id
    expect_true(all(is.na(bid) | bid %in% doc$blocks$block_id))
    # Each chunk names the block it STARTS at; a five-chunk section used to
    # report block 1 for all five.
    if (m == "structural") expect_gt(length(unique(stats::na.omit(bid))), 1)
  }
  # `fixed` and `recursive` work on concatenated text and report NA rather than
  # guessing -- that is the documented price of ignoring structure.
  for (m in c("fixed", "recursive")) {
    ch <- gr_segment(doc, list(method = m, max_tokens = 150))
    expect_true(all(is.na(ch$chunks$block_id)))
  }
})

test_that("a segmenter fallback is reported, not silent", {
  doc <- gr_ingest(sample_doc(2, 3))
  expect_warning(gr_segment(doc, list(method = "semantic", max_tokens = 200)),
                 class = "gr_segment_fallback")
  expect_warning(gr_segment(doc, list(method = "page", max_tokens = 200)),
                 class = "gr_segment_fallback")
  out <- quiet(gr_segment(doc, list(method = "page", max_tokens = 200)))
  expect_match(out$method, "->")
})

# ---------------------------------------------------------------------------
# Reading
# ---------------------------------------------------------------------------

test_that("every reader has a unique traversal signature", {
  # This is the package's central claim: two readers that resolve to the same
  # signature are the same methodology under two names, which is exactly what v1
  # shipped.
  sigs <- gr_readers()$signature
  expect_equal(anyDuplicated(sigs), 0L)
  expect_true(all(grepl("^[^|]+\\|[^|]+\\|[^|]+$", sigs)))
})

test_that("an ensemble refuses members that share a signature", {
  local_registries()
  ch <- gr_segment(gr_ingest(sample_doc(2, 3)), list(method = "paragraph", max_tokens = 150))
  # The same member twice is one member after dedup, which is not an ensemble.
  expect_error(
    quiet(gr_read(ch, "q", mock_echo(), list(reader = "ensemble", members = c("stuff", "stuff")))),
    class = "gr_bad_ensemble")
  expect_error(
    quiet(gr_read(ch, "q", mock_echo(), list(reader = "ensemble", members = "ensemble"))),
    class = "gr_bad_ensemble")
  # Two DIFFERENT readers that resolve to the same signature are the same
  # methodology under two names -- exactly the Chunked/Semantic collapse v1 had.
  gr_register_reader("clone_of_stuff",
    function(chunks, question, client, spec, trace) {
      readgpt:::registry_get("readers", "stuff", "readers")$fn(chunks, question, client, spec, trace)
    }, signature = "all|1|none")
  expect_error(
    quiet(gr_read(ch, "q", mock_echo(),
                  list(reader = "ensemble", members = c("stuff", "clone_of_stuff")))),
    class = "gr_bad_ensemble")
})

test_that("an ensemble says so when its members collapse onto the same traversal", {
  # Distinct signatures only guarantee distinct traversals when there is
  # something to traverse. On a one-chunk document every member reads the same
  # text and issues the same prompt, so agreement is not corroboration.
  ch <- gr_segment(gr_ingest("A single short paragraph about revenue of 45.2 million."),
                   list(method = "paragraph", max_tokens = 4000))
  expect_equal(nrow(ch$chunks), 1)
  # Two warnings: one up front from the chunk count, one after the fact from the
  # members having actually returned the same answer over the same chunks.
  expect_warning(
    expect_warning(
      gr_read(ch, "What was revenue?", mock_echo(),
              list(reader = "ensemble", members = c("stuff", "rerank"))),
      class = "gr_ensemble_degenerate"),
    class = "gr_ensemble_degenerate")
})

test_that("readers issue the call pattern their signature advertises", {
  ch <- gr_segment(gr_ingest(sample_doc(3, 4)), list(method = "paragraph", max_tokens = 150))
  n <- nrow(ch$chunks)
  skip_if(n < 3)
  counts <- vapply(c("stuff", "map_reduce", "refine", "skim"), function(r) {
    cl <- mock_echo()
    quiet(gr_read(ch, "What was the endpoint?", cl, list(reader = r)))
    length(cl$calls())
  }, integer(1))
  expect_equal(unname(counts[["stuff"]]), 1L)      # all | 1 | none
  expect_gte(unname(counts[["map_reduce"]]), n)    # all | N + merges | tree
  expect_gte(unname(counts[["refine"]]), n)        # all | N | forward
  expect_gte(unname(counts[["skim"]]), n)          # all | N + 1 | none
  # Distinct signatures must show up as distinct call counts, not just labels.
  expect_gt(length(unique(counts)), 1L)
})

test_that("retrieval ranks by relevance and not by its inverse", {
  # Mutation survivor: flipping the sort direction in the ranker passed the
  # whole suite, because nothing asserted WHICH chunks were selected.
  doc <- gr_ingest(paste(c(
    "Mitochondrial density was measured in every muscle biopsy sample.",
    "The cafeteria menu changes on alternate Tuesdays throughout the term.",
    "Parking permits are issued annually to registered staff members only.",
    "Revenue for the fourth quarter reached 45.2 million dollars exactly."),
    collapse = "\n\n"))
  ch <- gr_segment(doc, list(method = "paragraph", max_tokens = 40))
  cl <- mock_echo("Revenue was 45.2 million dollars.")
  ans <- quiet(gr_read(ch, "What was fourth quarter revenue?", cl,
                       list(reader = "retrieve", top_k = 1)))
  picked <- ch$chunks$text[ans$chunks_used]
  expect_true(any(grepl("45.2 million", picked, fixed = TRUE)))
})

test_that("the evidence table lines up with the chunks it cites", {
  ch <- gr_segment(gr_ingest(sample_doc(2, 4)), list(method = "paragraph", max_tokens = 150))
  ans <- quiet(gr_read(ch, "What was the endpoint?", mock_echo(), list(reader = "skim")))
  skip_if(is.null(ans$evidence) || !nrow(ans$evidence))
  expect_true(all(ans$evidence$chunk_id %in% ch$chunks$chunk_id))
  expect_true(all(nzchar(trimws(ans$evidence$text))))
  expect_true(all(ans$evidence$chunk_id %in% ans$chunks_used))
})

test_that("a total transport failure degrades rather than fabricating an answer", {
  ch <- gr_segment(gr_ingest(sample_doc(2, 3)), list(method = "paragraph", max_tokens = 150))
  for (r in c("stuff", "map_reduce", "refine", "skim", "hierarchical")) {
    ans <- quiet(gr_read(ch, "What was the endpoint?", mock_dead(), list(reader = r)))
    expect_true(isTRUE(ans$partial), label = sprintf("reader '%s' partial flag", r))
    expect_s3_class(ans, "gr_answer")
  }
})

test_that("an empty completion does not become an answer", {
  ch <- gr_segment(gr_ingest(sample_doc(2, 3)), list(method = "paragraph", max_tokens = 150))
  ans <- quiet(gr_read(ch, "q", mock_empty(), list(reader = "map_reduce")))
  expect_true(isTRUE(ans$partial))
})

test_that("the call cap is enforced before the first call, not after the last", {
  local_registries()
  ch <- gr_segment(gr_ingest(sample_doc(4, 4)), list(method = "paragraph", max_tokens = 100))
  skip_if(nrow(ch$chunks) < 5)
  cl <- mock_echo()
  gr_options(max_calls = 3L)
  expect_error(quiet(gr_read(ch, "q", cl, list(reader = "map_reduce"), trace = gr_trace())),
               class = "gr_call_cap")
  # Pre-flight, so nothing was spent discovering the cap.
  expect_equal(length(cl$calls()), 0L)

  # A cap the reader can respect stops it part way instead of refusing outright.
  gr_options(max_calls = 200L)
  tr <- gr_trace()
  cl2 <- mock_echo()
  quiet(gr_read(ch, "q", cl2, list(reader = "map_reduce"), trace = tr))
  expect_lte(length(cl2$calls()), 200L)
})

test_that("turning the call cap off does not break every read", {
  local_registries()
  # `is.finite(NULL)` is logical(0), so `if (is.finite(cap) && ...)` failed with
  # "argument is of length zero" -- every read aborted the moment a user used
  # the documented way to remove the cap.
  gr_options(max_calls = NULL)
  ch <- gr_segment(gr_ingest(sample_doc(2, 2)), list(method = "paragraph", max_tokens = 150))
  expect_no_error(quiet(gr_read(ch, "q", mock_echo(), list(reader = "map_reduce"))))
})

# ---------------------------------------------------------------------------
# Merging
# ---------------------------------------------------------------------------

test_that("merging a finding larger than the whole budget terminates and stays bounded", {
  # It used to spin through seven no-op levels -- a single oversized piece sits
  # alone in its group, and a one-element group is returned unchanged -- and
  # then concatenate everything, returning an "answer" the size of the document.
  spec <- gr_read_spec(model = "gpt-4o-mini", max_answer_tokens = 300)
  big <- paste(rep("The quarterly revenue figure was reported in the filing.", 20000),
               collapse = " ")
  cl <- mock_dead()
  out <- quiet(readgpt:::tree_merge(cl, "What was revenue?",
                                    c(big, "Small finding one.", "Small finding two."),
                                    spec, gr_trace()))
  expect_lte(out$levels, 3L)
  expect_lt(gr_count_tokens(out$text), 4000L)
  expect_gt(out$truncated, 0L)
})

test_that("merging a single finding short-circuits without a call", {
  spec <- gr_read_spec(model = "gpt-4o-mini")
  cl <- mock_echo()
  out <- readgpt:::tree_merge(cl, "q", "the only finding", spec, gr_trace())
  expect_identical(out$text, "the only finding")
  expect_equal(length(cl$calls()), 0L)
})

# ---------------------------------------------------------------------------
# Orchestration and public API
# ---------------------------------------------------------------------------

test_that("gr_recipes is vectorised and names its unknowns", {
  expect_s3_class(gr_recipes("fast"), "gr_recipe")
  got <- gr_recipes(c("fast", "needle"))
  expect_named(got, c("fast", "needle"))
  expect_error(gr_recipes("nope"), "Unknown recipe")
  expect_error(gr_recipes(1), "character vector")
})

test_that("gr_compare rejects an empty recipe set and accepts a bare recipe", {
  cl <- mock_echo()
  expect_error(gr_compare(sample_doc(1, 2), "q", list(), client = cl), class = "gr_no_recipes")
  expect_error(gr_compare(sample_doc(1, 2), "q", character(0), client = cl), class = "gr_no_recipes")
  # A gr_recipe IS a list of four fields, so iterating it walked its FIELDS.
  one <- gr_recipe("solo", read = list(reader = "stuff"))
  out <- quiet(gr_compare(sample_doc(1, 2), "q", one, client = cl))
  expect_named(out$answers, "solo")
})

test_that("gr_compare refuses to bill twice for the same pipeline", {
  cl <- mock_echo()
  a <- gr_recipe("a", segment = list(method = "paragraph", max_tokens = 400),
                 read = list(reader = "map_reduce"))
  b <- gr_recipe("b", segment = list(method = "paragraph", max_tokens = 400),
                 read = list(reader = "map_reduce"))
  expect_warning(gr_compare(sample_doc(2, 3), "q", list(a, b), client = cl),
                 class = "gr_duplicate_recipe")
  # Differing only in a field the old hash ignored must NOT be collapsed.
  c1 <- gr_recipe("c1", segment = list(method = "paragraph", max_tokens = 400),
                  read = list(reader = "retrieve", top_k = 2))
  c2 <- gr_recipe("c2", segment = list(method = "paragraph", max_tokens = 400),
                  read = list(reader = "retrieve", top_k = 9))
  out <- quiet(gr_compare(sample_doc(2, 3), "q", list(c1, c2), client = mock_echo()))
  expect_equal(length(out$answers), 2L)
})

test_that("as_json survives every object that embeds a trace", {
  # A gr_trace is an ENVIRONMENT; handing one to jsonlite fails with "cannot
  # unclass an environment". Anything that nests a trace has to flatten it.
  cl <- mock_echo()
  out <- quiet(gr_compare(sample_doc(2, 2), "What was revenue?", "fast", client = cl))
  for (x in list(out$trace, out$answers[[1]])) {
    j <- expect_no_error(as_json(x))
    expect_true(nzchar(as.character(j)))
    expect_type(jsonlite::fromJSON(as.character(j), simplifyVector = FALSE), "list")
  }
  j <- quiet(answer_question(readgpt_example(), "What was revenue?", mode = "Chunked",
                             return_json = TRUE, client = mock_echo()))
  parsed <- jsonlite::fromJSON(as.character(j), simplifyVector = FALSE)
  expect_true(all(c("answers", "summary", "trace") %in% names(parsed)))
})

test_that("as_json preserves an explicit null rather than dropping the field", {
  # Mutation survivor: changing null = "null" to null = "list" passed.
  j <- as.character(as_json(list(a = 1, b = NULL, c = NA)))
  parsed <- jsonlite::fromJSON(j, simplifyVector = FALSE)
  expect_true("c" %in% names(parsed))
  expect_null(parsed$c)
})

test_that("a deprecation warning survives a handler that turns it into an error", {
  readgpt:::.warn_once_reset()
  on.exit(readgpt:::.warn_once_reset(), add = TRUE)
  # Marking the id BEFORE emitting meant options(warn = 2) consumed the one and
  # only notice: the user saw a failure, fixed the handler, and got silence.
  expect_error(withr::with_options(list(warn = 2), readgpt:::.warn_once("t", "boom")))
  expect_warning(readgpt:::.warn_once("t", "boom"), class = "gr_deprecated")
  expect_silent(readgpt:::.warn_once("t", "boom"))
})

test_that("every v1 shim runs and does not reproduce the v1 bug it replaced", {
  # Mutation survivor: the whole of compat.R could be deleted without a failure.
  readgpt:::.warn_once_reset()
  on.exit(readgpt:::.warn_once_reset(), add = TRUE)
  expect_warning(answer_question(readgpt_example(), "What was revenue?",
                                 mode = "Chunked", client = mock_echo()),
                 class = "gr_deprecated")
  a <- quiet(answer_question(readgpt_example(), "What was revenue?",
                             mode = "Chunked", client = mock_echo()))
  expect_type(a, "character")

  # v1 stripped every digit by default, so no question about a figure could be
  # answered. parse_text() must not do that any more.
  chunks <- quiet(parse_text(readgpt_example(), chunk_token_limit = 200))
  expect_type(chunks, "character")
  expect_gt(length(chunks), 1L)
  expect_true(any(grepl("[0-9]", chunks)))

  for (f in list(gpt_read_chunked, gpt_read_retrieval, gpt_read_hierarchical,
                 gpt_read_multipass)) {
    out <- quiet(f(chunks[1:2], "What was revenue?", client = mock_echo()))
    expect_type(out, "character")
    expect_true(nzchar(out))
  }
})

test_that("gr_options stores an explicit NULL instead of ignoring it", {
  old <- gr_options(max_cost_usd = NULL)
  on.exit(gr_options(old), add = TRUE)
  expect_null(gr_options("max_cost_usd"))
  gr_options(old)
  expect_false(is.null(gr_options("max_cost_usd")))
})

test_that("the trace records what actually happened, not what was intended", {
  # Mutation survivor: several trace fields were never asserted on.
  ch <- gr_segment(gr_ingest(sample_doc(2, 3)), list(method = "paragraph", max_tokens = 150))
  tr <- gr_trace()
  cl <- mock_echo()
  quiet(gr_read(ch, "What was the endpoint?", cl, list(reader = "map_reduce"), trace = tr))
  expect_equal(tr$calls, length(cl$calls()))
  labels <- vapply(tr$steps, function(s) s$label, character(1))
  expect_true(any(grepl("map_reduce|map|reduce|merge", labels)))
  # Every model step must carry its prompt AND its response, or the trace cannot
  # explain the answer it sits next to.
  model_steps <- Filter(function(s) !identical(s$kind, "local"), tr$steps)
  expect_gt(length(model_steps), 0L)
  for (s in model_steps) {
    expect_gt(length(s$prompt), 0L)
    expect_true(!is.null(s$response) || isFALSE(s$ok))
  }
  summ <- gr_trace_summary(tr)
  expect_equal(summ$calls, tr$calls)
})

# ---------------------------------------------------------------------------
# Regressions introduced by the structural-heading fix itself
# ---------------------------------------------------------------------------

test_that("structural segmentation loses no text, on either heading path", {
  md <- withr::local_tempfile(fileext = ".md")
  writeLines(c("# Northwind Annual Report 2024", "",
               "## 3.1 Methods", "",
               "We used a randomised design with several participants involved.", "",
               "## 3.2 Results:", "",
               "Revenue reached 45.2 million dollars in the fourth quarter."), md)
  inline <- paste(readLines(md), collapse = "\n")

  for (nm in c("extractor-supplied sections", "inline heading detection")) {
    doc <- gr_ingest(if (nm == "inline heading detection") inline else md)
    ch <- gr_segment(doc, list(method = "structural", max_tokens = 400))

    # (a) A section that is nothing but its heading -- a document title -- used
    # to be dropped from the body and then skipped for having no body, so the
    # title reached no reader at all.
    expect_true(any(grepl("Northwind Annual Report", ch$chunks$text, fixed = TRUE)),
                label = sprintf("title survives (%s)", nm))
    # (b) The extractor stores "3.1 Methods" while the block still reads
    # "## 3.1 Methods". Comparing a normalised label against a raw one never
    # matched, so the heading was emitted twice.
    for (t in ch$chunks$text) {
      expect_lte(lengths(regmatches(t, gregexpr("Methods", t))), 1)
      expect_lte(lengths(regmatches(t, gregexpr("Results", t))), 1)
    }
    expect_false(any(grepl("## #", ch$chunks$text, fixed = TRUE)))
    # (c) Nothing from the document goes missing.
    for (needle in c("Northwind", "randomised design", "45.2 million")) {
      expect_true(any(grepl(needle, ch$chunks$text, fixed = TRUE)),
                  label = sprintf("'%s' survives (%s)", needle, nm))
    }
  }
})

test_that("the page segmenter reports a block, not just a page", {
  # block_id was never passed, so every `page` chunk claimed NA and a citation
  # could not point below page granularity.
  blocks <- data.frame(
    text = c("First block on page one.", "Second block on page one.",
             "Only block on page two."),
    page = c(1L, 1L, 2L), section = NA_character_, block_id = 1:3,
    stringsAsFactors = FALSE)
  doc <- structure(list(source = "<test>", text = paste(blocks$text, collapse = "\n\n"),
                        blocks = blocks, stats = list(tokens = 30L)),
                   class = "gr_document")
  ch <- quiet(gr_segment(doc, list(method = "page", max_tokens = 400)))
  expect_equal(ch$method, "page")
  expect_false(any(is.na(ch$chunks$block_id)))
  expect_true(all(ch$chunks$block_id %in% blocks$block_id))
})

test_that("hierarchical never asks for citations it cannot support", {
  # It answers from SUMMARIES, which carry no [chunk N] ids. Passing cite = TRUE
  # through asked the model to cite ids that were not in front of it.
  ch <- gr_segment(gr_ingest(sample_doc(3, 3)), list(method = "paragraph", max_tokens = 120))
  cl <- mock_echo()
  quiet(gr_read(ch, "What was the endpoint?", cl,
                list(reader = "hierarchical", cite = TRUE, fan_in = 2)))
  final <- utils::tail(cl$calls(), 1)[[1]]
  expect_match(final$label, "hier.answer")
  expect_false(grepl("Cite the excerpt", final$messages[[1]]$content, fixed = TRUE))
})

test_that("merging is bounded on the call-cap path too, not only on failure", {
  # Two of tree_merge's exits returned raw concatenation while the third was
  # carefully capped -- so hitting the call cap produced an "answer" the size of
  # every finding put together.
  local_registries()
  spec <- gr_read_spec(model = "gpt-4o-mini", max_answer_tokens = 300)
  tr <- gr_trace()
  gr_options(max_calls = 0L)
  pieces <- vapply(1:5, function(i)
    paste(rep(sprintf("Finding %d about the quarterly revenue figure.", i), 120), collapse = " "),
    character(1))
  out <- quiet(readgpt:::tree_merge(mock_echo(), "q", pieces, spec, tr))
  expect_false(out$ok)
  expect_lt(gr_count_tokens(out$text), sum(gr_count_tokens(pieces)))
  expect_lte(gr_count_tokens(out$text), 3L * 300L + 50L)
})

# ---------------------------------------------------------------------------
# The public extension API
#
# `?gr_register_reader` and `?gr_register_segmenter` are advertised as the way
# to extend the package. Both were impossible to follow: the object each one
# must return could only be built with `:::`. These tests exercise the
# documented path exactly as a user would write it.
# ---------------------------------------------------------------------------

test_that("a custom reader can be written with exported functions only", {
  local_registries()
  gr_register_reader("longest", signature = "one|1|none", cost_calls = "1",
    description = "answer from the longest chunk only",
    fn = function(chunks, question, client, spec, trace) {
      d <- chunks$chunks
      i <- which.max(d$tokens)
      res <- gr_call(client, list(list(role = "user",
                                       content = paste0(d$text[i], "\n\nQ: ", question))),
                     model = spec$model, trace = trace, label = "longest.answer")
      new_answer(res$text, "longest", question, d$chunk_id[i], trace,
                 partial = !isTRUE(res$ok))
    })
  ch <- gr_segment(gr_ingest(sample_doc(2, 3)), list(method = "paragraph", max_tokens = 120))
  cl <- mock_echo("the longest chunk answered")
  ans <- quiet(gr_read(ch, "What was the endpoint?", cl, "longest"))
  expect_s3_class(ans, "gr_answer")
  expect_equal(ans$answer, "the longest chunk answered")
  expect_equal(length(cl$calls()), 1L)
  expect_true("longest" %in% gr_readers()$name)
  # A custom reader is subject to the same caps as a built-in.
  expect_false(ans$partial)
})

test_that("a custom segmenter can be written with exported functions only", {
  local_registries()
  gr_register_segmenter("by_bullet", description = "one chunk per bullet",
    fn = function(doc, spec, client, trace) {
      units <- trimws(strsplit(doc$text, "\n(?=[-*])", perl = TRUE)[[1]])
      new_chunks(units, "by_bullet", spec, block_id = 1L)
    })
  doc <- gr_ingest("Findings:\n\n- Revenue rose to 45.2 million.\n- Costs fell.\n- Margin widened.")
  ch <- gr_segment(doc, list(method = "by_bullet", max_tokens = 200))
  expect_s3_class(ch, "gr_chunks")
  expect_equal(ch$method, "by_bullet")
  expect_gt(nrow(ch$chunks), 1)
  expect_true(all(c("chunk_id", "tokens", "chars", "block_id") %in% names(ch$chunks)))
  expect_true("by_bullet" %in% gr_segmenters()$name)
})

test_that("gr_segment still enforces the cap on a segmenter that ignores it", {
  local_registries()
  gr_register_segmenter("whole_document", description = "one chunk, cap ignored",
    fn = function(doc, spec, client, trace) new_chunks(doc$text, "whole_document", spec))
  doc <- gr_ingest(sample_doc(4, 4))
  ch <- quiet(gr_segment(doc, list(method = "whole_document", max_tokens = 100)))
  expect_lte(max(ch$chunks$tokens), 100)
  expect_gt(nrow(ch$chunks), 1)
  expect_true(!is.null(ch$extra$cap_enforced))
})

test_that("gr_read refuses a reader that returns the wrong kind of object", {
  local_registries()
  gr_register_reader("bad", signature = "all|1|none",
                     fn = function(chunks, question, client, spec, trace) "just a string")
  ch <- gr_segment(gr_ingest(sample_doc(1, 2)), list(method = "paragraph", max_tokens = 200))
  expect_error(quiet(gr_read(ch, "q", mock_echo(), "bad")), "gr_answer")
})

test_that("a registered extractor overrides a built-in for the same extension", {
  local_registries()
  # Scanning the registry forwards meant a built-in kept its extension forever:
  # the override registered, appeared in gr_extractors(), and was never called.
  f <- withr::local_tempfile(fileext = ".md")
  writeLines(c("# Heading", "", "Body text of the document."), f)
  gr_register_extractor("shouty_md", "md", description = "md, upper-cased",
                        fn = function(path, opts) toupper(readLines(path, warn = FALSE)))
  expect_true(grepl("BODY TEXT", gr_ingest(f, gr_ingest_spec(clean = "none"))$text, fixed = TRUE))
})

test_that("is_not_found matches the sentinel and only the sentinel", {
  expect_true(is_not_found("NOT_IN_DOCUMENT"))
  expect_true(is_not_found("  not_in_document.  "))
  expect_true(is_not_found("**NOT_IN_DOCUMENT**"))
  expect_true(is_not_found("NOT IN DOCUMENT"))
  expect_true(is_not_found(""))
  expect_true(is_not_found(NULL))
  # A real answer that merely quotes the sentinel is an answer. The v1 test was
  # a substring match over the whole response, which threw these away.
  expect_false(is_not_found("The log said NOT_IN_DOCUMENT, but revenue was 45.2 million."))
  expect_false(is_not_found("Revenue was 45.2 million dollars."))
})

test_that("the mock client agrees with the real client about an empty completion", {
  # The API returns `content: null` on a refusal or content filter, and the real
  # client reports that as ok = FALSE. The mock used to report ok = TRUE with an
  # empty string, so the invariant `gr_result` documents was both violated and
  # untestable offline -- and every reader's empty-answer path went unexercised.
  res <- gr_call(gr_mock_client(function(m, p) ""), "hi")
  expect_false(res$ok)
  expect_identical(res$text, "")
  expect_true(nzchar(res$error))
  ok <- gr_call(gr_mock_client(function(m, p) "a real answer"), "hi")
  expect_true(ok$ok)
  # Every gr_result has a single-string `text`, on both paths.
  for (r in list(res, ok)) {
    expect_type(r$text, "character")
    expect_length(r$text, 1L)
  }
})

test_that("gr_segmenters reports which strategies need a client", {
  d <- gr_segmenters()
  expect_true(all(c("name", "cost", "needs_client", "description") %in% names(d)))
  needs <- d$name[d$needs_client == "TRUE"]
  expect_setequal(needs, c("semantic", "proposition"))
  # And the report matches the behaviour: a segmenter that needs a client and
  # does not get one must warn and fall back, not proceed silently -- with
  # exactly ONE warning, not a generic one plus a specific one for one event.
  doc <- gr_ingest(sample_doc(2, 3))
  for (m in needs) {
    seen <- character(0)
    out <- withCallingHandlers(
      gr_segment(doc, list(method = m, max_tokens = 200)),
      gr_segment_fallback = function(w) {
        seen <<- c(seen, conditionMessage(w)); invokeRestart("muffleWarning")
      })
    expect_length(seen, 1L)
    expect_match(out$method, "->", label = sprintf("'%s' records its downgrade", m))
  }
})

test_that("backoff delays are actually jittered across processes", {
  # The seed was a pure function of `attempt`, so every client retrying attempt
  # N slept for the identical duration -- labelled jitter, with none, so a
  # rate-limited fleet re-collided on every round.
  d <- vapply(1:200, function(i) readgpt:::backoff_delay(1, 3), numeric(1))
  expect_gt(length(unique(d)), 1L)
  expect_true(all(d >= 0 & d <= 60))
  # Still exponential, and still capped.
  expect_lt(mean(vapply(1:50, function(i) readgpt:::backoff_delay(1, 1), numeric(1))),
            mean(vapply(1:50, function(i) readgpt:::backoff_delay(1, 4), numeric(1))))
  expect_lte(max(vapply(1:50, function(i) readgpt:::backoff_delay(1, 30), numeric(1))), 60)
})

test_that("gr_hash does not shadow utils::str for the rest of the package", {
  # A `str()` shim in the package namespace would resolve every internal call to
  # it, not just gr_hash's one use.
  expect_null(readgpt:::gr_state$str)
  expect_false("str" %in% ls(asNamespace("readgpt")))
  # And the hash still works, and still distinguishes.
  expect_false(identical(readgpt:::gr_hash(list(a = 1)), readgpt:::gr_hash(list(a = 2))))
  expect_identical(readgpt:::gr_hash(list(a = 1)), readgpt:::gr_hash(list(a = 1)))
})

test_that("every export has a help topic with an example", {
  # getNamespaceExports(), not the NAMESPACE file: under `R CMD check` the tests
  # run from the installed package, where there is no source NAMESPACE two
  # directories up. Reading the file passed in a checkout and failed in check --
  # the one place this test actually needs to run.
  all_exp <- getNamespaceExports("readgpt")
  exports <- grep("^(print|as_json)\\.", all_exp, value = TRUE, invert = TRUE)
  skip_if(!length(exports))
  rd <- tools::Rd_db("readgpt")
  aliases <- unlist(lapply(rd, function(x)
    unlist(lapply(x[vapply(x, function(e) identical(attr(e, "Rd_tag"), "\\alias"), logical(1))],
                  function(e) trimws(paste(unlist(e), collapse = ""))))))
  expect_setequal(setdiff(exports, aliases), character(0))
  # Topics with no example teach nothing about how to use the function.
  has_example <- vapply(rd, function(x)
    any(vapply(x, function(e) identical(attr(e, "Rd_tag"), "\\examples"), logical(1))), logical(1))
  topics_for <- vapply(exports, function(e) {
    hit <- names(aliases)[aliases == e]
    if (length(hit)) hit[1] else NA_character_
  }, character(1))
  missing_ex <- exports[!is.na(topics_for) & !has_example[topics_for]]
  expect_equal(missing_ex, character(0))
})

test_that("a document passed as text is never handed to basename()", {
  # basename() is bounded by PATH_MAX -- 1024 bytes on macOS, where R warns past
  # it. Raw document text IS a length-1 character vector, so the guard
  # `is.character(x) && length(x) == 1L` sent the whole document to basename():
  # a warning on every macOS run over ~1KB, and a mangled fragment of the
  # document in the trace where the filename belongs. Linux did the same thing
  # silently, which is why the suite did not catch it.
  long <- sample_doc(4, 4)
  expect_gt(nchar(long), 1024)
  cl <- mock_echo()

  expect_no_warning(
    ans <- quiet(answer_document(long, "What was the endpoint?", "fast", client = cl)))
  expect_identical(ans$trace$meta$source, "<inline text>")

  cl2 <- mock_echo()
  expect_no_warning(
    cmp <- quiet(gr_compare(long, "What was the endpoint?", "fast", client = cl2)))
  expect_identical(cmp$trace$meta$source, "<inline text>")

  # A real path still gets its basename, and the label agrees with the label
  # ingestion recorded on the document.
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(long, f)
  a2 <- quiet(answer_document(f, "What was the endpoint?", "fast", client = mock_echo()))
  expect_identical(a2$trace$meta$source, basename(f))
  expect_identical(basename(a2$document$source), basename(f))
})

test_that("source_label refuses anything that is not a real, short path", {
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("hello", f)
  expect_identical(readgpt:::source_label(f), basename(f))
  for (x in list(strrep("a", 2000), "two\nlines", NA_character_, character(0),
                 c("a", "b"), 42, NULL, "/no/such/file.txt")) {
    expect_identical(readgpt:::source_label(x), "<inline text>")
  }
})

# ---------------------------------------------------------------------------
# Locale independence
#
# The same document must produce the same tokens, the same chunks and the same
# cost estimate on every machine. It did not: `enc2utf8()` treats an UNMARKED
# string as native, so in a non-UTF-8 locale it re-encoded bytes that were
# already valid UTF-8. Downstream, the ligature and smart-quote cleaners stopped
# matching and `utf8ToInt()` fell back to counting bytes -- charging a
# one-codepoint em dash as three characters. CI caught it as a README output
# diff; the cause was a 50% token swing on the affected line.
# ---------------------------------------------------------------------------

test_that("token counts depend on the bytes, not on the encoding label", {
  s <- "Northwind Instruments — Annual Report 2024"
  labelled <- s;   Encoding(labelled) <- "UTF-8"
  unlabelled <- s; Encoding(unlabelled) <- "unknown"
  expect_identical(gr_count_tokens(labelled), gr_count_tokens(unlabelled))

  # And across the scripts the estimator special-cases.
  for (txt in c("café résumé", "中文字", "\U0001F600\U0001F600",
                "مرحبا", "plain ascii only")) {
    a <- txt; Encoding(a) <- "UTF-8"
    b <- txt; Encoding(b) <- "unknown"
    expect_identical(gr_count_tokens(a), gr_count_tokens(b), label = sprintf("%s", txt))
  }
})

test_that("to_utf8 labels valid UTF-8 instead of re-encoding it", {
  s <- "Northwind Instruments — Annual Report 2024"
  raw_bytes <- s; Encoding(raw_bytes) <- "unknown"
  out <- readgpt:::to_utf8(raw_bytes)
  expect_identical(Encoding(out), "UTF-8")
  expect_true(validUTF8(out))
  # The bytes must survive untouched -- this is where they were being corrupted.
  expect_identical(charToRaw(out), charToRaw(s))
})

test_that("ingestion and segmentation are identical under a C locale", {
  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!nzchar(old), "no LC_CTYPE to restore")
  on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)), add = TRUE)

  measure <- function() {
    d <- gr_ingest(readgpt_example())
    ch <- gr_segment(d, list(method = "structural", max_tokens = 120))
    list(tokens = d$stats$tokens, chars = d$stats$chars,
         text = d$text, totals = sum(ch$chunks$tokens))
  }
  readgpt:::gr_flush_caches()
  utf8 <- measure()

  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(ok), "cannot switch to the C locale here")
  readgpt:::gr_flush_caches()
  c_loc <- measure()

  expect_identical(c_loc$tokens, utf8$tokens)
  expect_identical(c_loc$chars, utf8$chars)
  expect_identical(c_loc$totals, utf8$totals)
  # The em dash in the bundled title must be normalised by `ligatures` either
  # way; leaving it meant the cleaner had silently stopped matching.
  expect_false(grepl("—", c_loc$text, useBytes = TRUE))
  expect_false(grepl("—", utf8$text, useBytes = TRUE))
})
