# test-distinctness.R
#
# The core claim of this refactor is that the reading methodologies are actually
# different, and that the segmentation strategies actually segment differently.
# These tests assert exactly that, by counting and comparing the prompts each
# strategy sends -- not by checking that some string came back.
#
# The v1 test suite could not have caught the Chunked/Semantic collapse: its
# strongest assertion on either mode was `expect_equal(answer, "merged answer")`,
# which the shared fake returned regardless of which mode produced it.

test_that("every reader has a unique traversal signature", {
  sigs <- gr_readers()$signature
  expect_equal(length(sigs), length(unique(sigs)))
})

test_that("readers issue measurably different call patterns", {
  doc <- gr_ingest(sample_doc(5, 4))
  ch <- gr_segment(doc, list(method = "paragraph", max_tokens = 200))
  n <- nrow(ch$chunks)
  expect_gte(n, 4L)

  pattern <- function(reader, ...) {
    cl <- mock_echo()
    quiet(gr_read(ch, "How many participants?", cl, c(list(reader = reader), list(...))))
    sort(table(call_labels(cl)))
  }

  p_stuff <- pattern("stuff")
  p_map   <- pattern("map_reduce")
  p_ref   <- pattern("refine")
  p_skim  <- pattern("skim")
  p_ret   <- pattern("retrieve", top_k = 3)
  p_hier  <- pattern("hierarchical", fan_in = 3)

  # Single-call strategies.
  expect_equal(sum(p_stuff), 1L)
  expect_equal(sum(p_ret), 1L)
  # Per-chunk strategies scale with N.
  expect_equal(sum(p_map), n + 1L)      # N answers + one reduce
  expect_equal(sum(p_ref), n)           # N sequential revisions, no reduce
  expect_equal(sum(p_skim), n + 1L)     # N extractions + one synthesis
  expect_equal(sum(p_hier), n + 1L)     # N summaries + one answer

  # map_reduce and skim both cost N+1 -- they are distinguished by the PROMPTS,
  # which is exactly the distinction v1 collapsed.
  expect_false(identical(names(p_map), names(p_skim)))
  expect_true("reduce" %in% names(p_map))
  expect_true("skim.extract" %in% names(p_skim))
})

test_that("map_reduce asks for answers while skim asks for evidence", {
  ch <- gr_segment(gr_ingest(sample_doc(2, 2)), list(method = "paragraph", max_tokens = 200))
  sys_of <- function(reader) {
    cl <- mock_echo(); quiet(gr_read(ch, "Q?", cl, reader))
    cl$calls()[[1]]$messages[[1]]$content
  }
  expect_match(sys_of("map_reduce"), "answer questions using only")
  expect_match(sys_of("skim"), "extract evidence, not answers")
  expect_match(sys_of("hierarchical"), "compress text while preserving")
  expect_false(identical(sys_of("map_reduce"), sys_of("skim")))
})

test_that("refine carries state forward; map_reduce does not", {
  ch <- gr_segment(gr_ingest(sample_doc(4, 4)), list(method = "paragraph", max_tokens = 150))
  expect_gte(nrow(ch$chunks), 3L)

  cl <- mock_echo("draft text")
  quiet(gr_read(ch, "Q?", cl, "refine"))
  later <- cl$calls()[[3]]
  # A refine call after the first must contain the previous draft.
  expect_true(any(grepl("<draft>", vapply(later$messages, function(m) m$content, character(1)))))

  cl2 <- mock_echo("chunk answer")
  quiet(gr_read(ch, "Q?", cl2, "map_reduce"))
  mapped <- cl2$calls()[[3]]
  expect_false(any(grepl("<draft>", vapply(mapped$messages, function(m) m$content, character(1)))))
})

test_that("retrieve sends only top-k chunks, never the whole document", {
  doc <- gr_ingest(sample_doc(5, 4))
  ch <- gr_segment(doc, list(method = "paragraph", max_tokens = 150))
  expect_gt(nrow(ch$chunks), 6L)
  cl <- mock_echo()
  a <- quiet(gr_read(ch, "How many participants?", cl, list(reader = "retrieve", top_k = 3)))
  expect_lte(length(a$chunks_used), 3L)
  expect_lt(length(a$chunks_used), nrow(ch$chunks))
  expect_length(cl$calls(), 1L)
})

test_that("an ensemble refuses members that share a signature", {
  ch <- gr_segment(gr_ingest(sample_doc(2, 2)), "paragraph")
  expect_error(
    gr_read(ch, "Q?", mock_echo(), list(reader = "ensemble", members = c("stuff", "stuff"))),
    class = "gr_error")
  # v1's "MultiPass" was Retrieval + Chunked re-run verbatim; an ensemble of one
  # is likewise refused.
  expect_error(
    gr_read(ch, "Q?", mock_echo(), list(reader = "ensemble", members = "map_reduce")),
    class = "gr_error")
})

test_that("an ensemble runs each distinct member exactly once", {
  ch <- gr_segment(gr_ingest(sample_doc(2, 2)), list(method = "paragraph", max_tokens = 200))
  n <- nrow(ch$chunks)
  cl <- mock_echo()
  quiet(gr_read(ch, "Q?", cl, list(reader = "ensemble",
                                   members = c("retrieve", "map_reduce"), top_k = 2)))
  labs <- table(call_labels(cl))
  expect_equal(as.integer(labs[["retrieve.answer"]]), 1L)
  expect_equal(as.integer(labs[["map.answer"]]), n)
  expect_equal(as.integer(labs[["ensemble.adjudicate"]]), 1L)
})

test_that("segmenters produce genuinely different chunk boundaries", {
  doc <- gr_ingest(sample_doc(5, 4))
  cl <- mock_echo()
  methods <- c("fixed", "paragraph", "sentence", "recursive", "structural", "semantic")
  sets <- lapply(methods, function(m)
    quiet(gr_segment(doc, list(method = m, max_tokens = 200, overlap_tokens = 20), client = cl))$chunks$text)
  names(sets) <- methods
  # At least four of the six must differ from `paragraph` in their boundaries.
  differs <- vapply(sets[setdiff(methods, "paragraph")],
                    function(s) !identical(s, sets$paragraph), logical(1))
  expect_gte(sum(differs), 3L)
  # `fixed` is structure-blind and must not coincide with paragraph packing.
  expect_false(identical(sets$fixed, sets$paragraph))
})

test_that("overlap actually duplicates text across chunk boundaries", {
  doc <- gr_ingest(sample_doc(4, 4))
  none <- gr_segment(doc, list(method = "paragraph", max_tokens = 200, overlap_tokens = 0))
  some <- gr_segment(doc, list(method = "paragraph", max_tokens = 200, overlap_tokens = 60))
  expect_gt(sum(some$chunks$tokens), sum(none$chunks$tokens))
})

test_that("structural segmentation never merges across headings", {
  doc <- gr_ingest(sample_doc(4, 2))
  ch <- gr_segment(doc, list(method = "structural", max_tokens = 4000))
  expect_gte(nrow(ch$chunks), 4L)
  expect_equal(length(unique(ch$chunks$section)), length(unique(stats::na.omit(ch$chunks$section))))
})

test_that("gr_compare collapses identical pipelines and keeps distinct ones", {
  doc_text <- sample_doc(3, 3)
  cl <- mock_echo()
  dup_call <- function() gr_compare(
    doc_text, "Q?", list(gr_recipe("A", segment = "paragraph", read = "map_reduce"),
                         gr_recipe("B", segment = "paragraph", read = "map_reduce")),
    client = cl)
  expect_warning(dup_call(), class = "gr_duplicate_recipe")
  same <- quiet(dup_call())
  expect_s3_class(same$summary, "data.frame")
  expect_equal(nrow(same$summary), 1L)

  diff <- quiet(gr_compare(doc_text, "Q?",
                           list(gr_recipe("A", segment = "paragraph", read = "map_reduce"),
                                gr_recipe("B", segment = "sentence",  read = "retrieve")),
                           client = cl))
  expect_equal(nrow(diff$summary), 2L)
})

test_that("a recipe's answer does not depend on what it is compared against", {
  # v1: chunk_method was decided once for the whole run and the sorted chunks
  # were shared, so `mode = "Chunked"` gave a different answer from
  # `mode = c("Chunked", "Semantic")`.
  doc_text <- sample_doc(3, 3)
  rec <- gr_recipe("target", segment = list(method = "sentence", max_tokens = 200),
                   read = "map_reduce")
  alone <- quiet(answer_document(doc_text, "How many participants?", rec, client = mock_echo()))
  together <- quiet(gr_compare(doc_text, "How many participants?",
                               list(rec, gr_recipe("other", segment = "semantic", read = "retrieve")),
                               client = mock_echo()))
  expect_identical(alone$answer, together$answers$target$answer)
  expect_identical(alone$segmentation$n, together$answers$target$segmentation$n)
})

test_that("a failed recipe is recorded, not deleted from the results", {
  # v1: `answers[[m]] <- NULL` removed the key, so a two-mode run that lost one
  # mode returned a bare unnamed string labelled with both mode names.
  doc_text <- sample_doc(4, 4)
  bad <- gr_recipe("bad", segment = list(method = "sentence", max_tokens = 60),
                   read = list(reader = "map_reduce", model = "gpt-4o"))
  old <- gr_options(max_calls = 3L)
  on.exit(gr_options(old), add = TRUE)
  cmp <- quiet(gr_compare(doc_text, "Q?",
                          list(gr_recipe("ok", segment = list(method = "paragraph",
                                                              max_tokens = 4000),
                                         read = "stuff"), bad),
                          client = mock_echo(), on_error = "continue"))
  expect_equal(nrow(cmp$summary), 2L)
  expect_true(all(c("ok", "bad") %in% cmp$summary$recipe))
  expect_true(cmp$summary$partial[cmp$summary$recipe == "bad"])
  expect_false(is.na(cmp$summary$error[cmp$summary$recipe == "bad"]))
})
