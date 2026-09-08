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

# ---------------------------------------------------------------------------
# Signature conformance for the readers whose distinctness IS a loop.
#
# The call-pattern test above runs every reader under `mock_echo()`, which
# answers the iterative prompt with `can_answer: true` on round one, and over
# fixture documents small enough that a single summarise pass always fits. Under
# those conditions the two readers claiming the most in their registered
# signatures -- `topk|rounds*2|forward` and `all|N+tree+1|tree` -- collapse to
# one retrieval and one summarise pass. That is to say they become `retrieve`
# and `map_reduce`, which is precisely the collapse this file exists to catch.
#
# Line coverage said so plainly: every line after `iterative`'s loop was dead,
# and `hierarchical`'s reduction body never ran once in the whole suite. Note
# that the assertion above, `sum(p_hier) == n + 1`, is the arithmetic of the
# NON-recursive case -- the expectation had quietly been written around the
# behaviour the fixtures happened to produce.
#
# Driven by hand both paths turned out correct, which is the good version of the
# news and exactly the version a suite stops being able to promise the moment
# nothing drives them.
# ---------------------------------------------------------------------------

test_that("iterative really iterates, and the loop is what makes it not retrieve", {
  ch <- gr_segment(gr_ingest(sample_doc(5, 4)), list(method = "paragraph", max_tokens = 120))
  cl <- mock_iterative_loop(refuse = 2L)
  a <- quiet(gr_read(ch, "How many participants?", cl,
                     list(reader = "iterative", max_rounds = 5L, top_k = 2L)))
  labs <- call_labels(cl)

  # The claim in "rounds*2": more than one assess step, not one.
  expect_gt(sum(labs == "iterative.step"), 1L)
  expect_gte(a$notes$rounds, 2L)

  # Each refusal drove a NEW query. Without this a loop that asks the same
  # thing repeatedly would satisfy the count above and retrieve nothing new.
  expect_gt(length(a$notes$queries), 1L)
  expect_equal(anyDuplicated(a$notes$queries), 0L)

  # And it accumulated across rounds, rather than re-reading one retrieval.
  expect_gt(a$notes$chunks_seen, 2L)
  expect_equal(nrow(a$evidence), a$notes$chunks_seen)

  # It left through the query-loop guard and answered from everything gathered,
  # which is the post-loop path -- not the model declaring itself satisfied.
  expect_equal(a$notes$stop_reason, "query loop")
  expect_true("iterative.final" %in% labs)
  expect_true(a$partial)

  # The reader it is most easily confused with, on the same document and the
  # same client, does none of that.
  cr <- mock_iterative_loop(refuse = 2L)
  quiet(gr_read(ch, "How many participants?", cr,
                list(reader = "retrieve", top_k = 2L)))
  expect_equal(length(call_labels(cr)), 1L)
  expect_gt(length(labs), length(call_labels(cr)))
})

test_that("iterative stops at max_rounds and still answers from what it gathered", {
  ch <- gr_segment(gr_ingest(sample_doc(5, 4)), list(method = "paragraph", max_tokens = 120))
  cl <- mock_iterative_loop(refuse = 99L)   # never repeats, so rounds run out
  a <- quiet(gr_read(ch, "How many participants?", cl,
                     list(reader = "iterative", max_rounds = 3L, top_k = 2L)))
  expect_equal(a$notes$rounds, 3L)
  expect_equal(a$notes$stop_reason, "max rounds")
  expect_equal(sum(call_labels(cl) == "iterative.step"), 3L)
  # Work already paid for is not thrown away when the budget runs out.
  expect_true("iterative.final" %in% call_labels(cl))
  expect_gt(a$notes$chunks_seen, 0L)
})

test_that("hierarchical really recurses when the summaries do not fit", {
  local_registries()
  gr_register_model("small-window", context_window = 900L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  ch <- gr_segment(gr_ingest(sample_doc(6, 5)), list(method = "paragraph", max_tokens = 60))
  n <- nrow(ch$chunks)
  expect_gt(n, 8L)

  cl <- mock_bulky()
  a <- quiet(gr_read(ch, "Summarise the findings", cl,
                     list(reader = "hierarchical", model = "small-window",
                          fan_in = 3L, max_levels = 6L, max_summary_tokens = 150L)))

  # The claim in "N+tree+1": a tree, so more than one level and fewer summaries
  # coming out than chunks going in.
  expect_gte(a$notes$levels, 2L)
  expect_lt(a$notes$final_summaries, n)
  labs <- call_labels(cl)
  expect_gt(length(labs), n + 1L)
  # Levels are labelled, so the tree is visible in the trace and not just in a note.
  expect_true(any(grepl("^hier\\.summarise\\.L2$", labs)))
  expect_true("hier.answer" %in% labs)
})

test_that("max_levels caps the recursion and says so rather than truncating in silence", {
  local_registries()
  gr_register_model("small-window", context_window = 900L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  ch <- gr_segment(gr_ingest(sample_doc(6, 5)), list(method = "paragraph", max_tokens = 60))
  cl <- mock_bulky()
  expect_warning(
    a <- suppressMessages(gr_read(ch, "Summarise the findings", cl,
                                  list(reader = "hierarchical", model = "small-window",
                                       fan_in = 3L, max_levels = 1L,
                                       max_summary_tokens = 150L))),
    "still exceed the budget")
  expect_equal(a$notes$levels, 1L)
})

test_that("skim_model and summary_model route the cheap pass off the answer model", {
  local_registries()
  gr_register_model("cheap-model", context_window = 128000L, max_output = 4096L,
                    input_usd = 0, output_usd = 0)
  ch <- gr_segment(gr_ingest(sample_doc(3, 3)), list(method = "paragraph", max_tokens = 120))
  models_by_label <- function(cl) {
    calls <- cl$calls()
    stats::setNames(vapply(calls, function(x) as.character(x$params$model), character(1)),
                    vapply(calls, function(x) x$label, character(1)))
  }

  cl <- mock_echo()
  quiet(gr_read(ch, "How many participants?", cl,
                list(reader = "skim", model = "mock-model", skim_model = "cheap-model")))
  m <- models_by_label(cl)
  expect_gt(sum(names(m) == "skim.extract"), 1L)
  expect_true(all(m[names(m) == "skim.extract"] == "cheap-model"))
  expect_true(all(m[names(m) != "skim.extract"] == "mock-model"))

  gr_register_model("small-window", context_window = 900L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  ch2 <- gr_segment(gr_ingest(sample_doc(6, 5)), list(method = "paragraph", max_tokens = 60))
  cl2 <- mock_bulky()
  quiet(gr_read(ch2, "Summarise the findings", cl2,
                list(reader = "hierarchical", model = "small-window", summary_model = "cheap-model",
                     fan_in = 3L, max_levels = 6L, max_summary_tokens = 150L)))
  m2 <- models_by_label(cl2)
  summarise_calls <- grepl("^hier\\.summarise", names(m2))
  expect_true(any(summarise_calls))
  expect_true(all(m2[summarise_calls] == "cheap-model"))
  expect_equal(unname(m2[names(m2) == "hier.answer"]), "small-window")
})

test_that("rerank_min_score keeps chunks out rather than being a number nothing reads", {
  ch <- gr_segment(gr_ingest(sample_doc(3, 3)), list(method = "paragraph", max_tokens = 120))
  # mock_echo() scores every candidate 8.
  keep <- quiet(gr_read(ch, "How many participants?", mock_echo(),
                        list(reader = "rerank", rerank_min_score = 4, top_k = 2L)))
  expect_false(identical(keep$answer, readgpt:::.NOT_FOUND))

  drop <- quiet(gr_read(ch, "How many participants?", mock_echo(),
                        list(reader = "rerank", rerank_min_score = 9, top_k = 2L)))
  expect_identical(drop$answer, readgpt:::.NOT_FOUND)
  expect_match(drop$notes$reason, "no candidate scored")
})


# ---------------------------------------------------------------------------
# survey: the reader that decides how to read before reading.
#
# Its claim is the one no other reader makes -- that the plan, not similarity
# and not position, decides what gets read. So the tests drive the plan and
# check the reading followed it, rather than checking that an answer came back.
# ---------------------------------------------------------------------------

test_that("survey reads what its plan says to read, and nothing it skipped", {
  ch <- gr_segment(gr_ingest(sample_doc(5, 3)), list(method = "structural", max_tokens = 150))
  n_sec <- length(unique(ch$chunks$section))
  expect_equal(n_sec, 5L)

  cl <- mock_planner(c("read", "skip", "skim", "skip", "read"))
  a <- quiet(gr_read(ch, "How many participants?", cl, list(reader = "preview")))
  labs <- call_labels(cl)
  secs <- ch$chunks$section

  # The claim in "1+s+1": one plan, one call per SKIMMED section, one answer.
  expect_equal(sum(labs == "preview.plan"), 1L)
  expect_equal(sum(labs == "preview.skim"), 1L)
  expect_equal(sum(labs == "preview.answer"), 1L)
  expect_equal(length(labs), 3L)

  # The plan is an artifact, not a log line, and it matches what was asked for.
  expect_equal(a$notes$plan$treatment, c("read", "skip", "skim", "skip", "read"))
  expect_equal(a$notes$read, 2L)
  expect_equal(a$notes$skipped, 2L)
  expect_false(a$notes$degraded)

  # The skipped sections' chunks are not in the answer prompt, and the read
  # ones are. This is the whole reader in one assertion.
  body <- cl$calls()[[length(labs)]]$messages[[2]]$content
  for (s in unique(secs)[c(2, 4)]) expect_false(grepl(s, body, fixed = TRUE))
  for (s in unique(secs)[c(1, 5)]) expect_true(grepl(s, body, fixed = TRUE))

  # One call per skimmed SECTION means the call sees the WHOLE section -- that
  # is what makes this cheaper than `skim` without being a worse `skim`. Reading
  # only the section's first chunk would satisfy the call count above and
  # silently look at a third of the text.
  skim_body <- cl$calls()[[which(labs == "preview.skim")]]$messages[[3]]$content
  sec3 <- ch$chunks$text[ch$chunks$section == unique(secs)[3]]
  expect_gt(length(sec3), 1L)
  for (t in sec3) expect_true(grepl(substr(t, nchar(t) - 40L, nchar(t)), skim_body, fixed = TRUE))

  # Not reading part of the document is reported, not hidden.
  expect_true(a$partial)
  expect_gt(a$notes$tokens_skipped, 0L)
  expect_setequal(unique(a$evidence$kind), c("verbatim", "extracted"))
})

test_that("survey plans differently when the plan differs, on identical input", {
  ch <- gr_segment(gr_ingest(sample_doc(5, 3)), list(method = "structural", max_tokens = 150))
  run <- function(treat) {
    cl <- mock_planner(treat)
    a <- quiet(gr_read(ch, "How many participants?", cl, list(reader = "preview")))
    list(calls = length(call_labels(cl)), used = length(a$chunks_used),
         skipped = a$notes$skipped)
  }
  wide <- run(rep("read", 5))
  narrow <- run(c("read", "skip", "skip", "skip", "skip"))
  expect_gt(wide$used, narrow$used)
  expect_equal(wide$skipped, 0L)
  expect_equal(narrow$skipped, 4L)
  # Reading less costs no more: the point of planning is spending less.
  expect_lte(narrow$calls, wide$calls)
})

test_that("survey defaults an unplanned section to read, never to skip", {
  # Silence from the planner must not lose a section. A skipped section is never
  # revisited, so the safe reading of "the plan did not mention it" is "look at
  # it" -- the opposite default would let a malformed reply drop a document's
  # contents without anything saying so.
  ch <- gr_segment(gr_ingest(sample_doc(4, 3)), list(method = "structural", max_tokens = 150))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("You plan how to read a document", messages[[1]]$content, fixed = TRUE)) {
      return(preview_plan_json_ids(c(1L), "skip"))   # speaks about section 1 only
    }
    "ANSWER"
  })
  a <- quiet(gr_read(ch, "Q?", cl, list(reader = "preview")))
  expect_equal(a$notes$plan$treatment, c("skip", "read", "read", "read"))
  expect_false(a$notes$degraded)
})

test_that("a survey that cannot get a plan reads everything and says so", {
  ch <- gr_segment(gr_ingest(sample_doc(4, 3)), list(method = "structural", max_tokens = 150))
  cl <- mock_planner_broken()
  expect_warning(a <- suppressMessages(gr_read(ch, "Q?", cl, list(reader = "preview"))),
                 class = "gr_preview_degraded")
  expect_true(a$notes$degraded)
  expect_equal(a$notes$read, 4L)
  expect_equal(a$notes$skipped, 0L)
  expect_true(a$partial)          # a degraded run is never reported as clean
  expect_identical(a$answer, "FALLBACK ANSWER")
})

test_that("survey demotes to skim what will not fit, rather than dropping it", {
  # The plan can be more ambitious than the context window. A section that does
  # not fit still gets one look; it does not vanish because the planner was
  # optimistic.
  local_registries()
  gr_register_model("small-window", context_window = 1200L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  ch <- gr_segment(gr_ingest(sample_doc(6, 4)), list(method = "structural", max_tokens = 120))
  cl <- mock_planner(rep("read", 6))
  a <- quiet(gr_read(ch, "How many participants?", cl,
                     list(reader = "preview", model = "small-window", max_answer_tokens = 150)))
  expect_gt(a$notes$demoted_to_skim, 0L)
  expect_equal(a$notes$skipped, 0L)         # demoted, not dropped
  expect_gt(sum(call_labels(cl) == "preview.skim"), 0L)
  expect_equal(a$notes$read + a$notes$skimmed, 6L)
})

test_that("survey works when the segmenter recorded no sections at all", {
  # `fixed` and `recursive` leave `section` NA on every row. A planner still
  # needs something to point at, so contiguous blocks stand in.
  ch <- gr_segment(gr_ingest(sample_doc(5, 3)), list(method = "fixed", max_tokens = 150))
  expect_true(all(is.na(ch$chunks$section)))
  cl <- mock_planner(c("read", "skip", "read", "skip", "read", "skip", "read", "skip"))
  a <- quiet(gr_read(ch, "Q?", cl, list(reader = "preview")))
  expect_gt(a$notes$sections, 1L)
  expect_lte(a$notes$sections, nrow(ch$chunks))
  expect_gt(a$notes$skipped, 0L)
  expect_match(a$notes$plan$label[1], "^chunks ")
})


test_that("a name that is both a recipe and a reader resolves loudly, not silently", {
  # `preview` is named `preview` and not `survey` because a recipe already owns
  # `survey` -- and `as_recipe()` resolves recipes first, so asking for the
  # reader by that name would have handed back the recipe's reader instead. The
  # collision is legal; resolving it in silence is not.
  local_registries()
  gr_register_reader("survey", function(chunks, question, client, spec, trace) {
    new_answer("x", "survey", question, integer(0), trace)
  }, signature = "clash|1|none", cost_calls = "1", description = "clashes with the recipe")
  expect_warning(rec <- readgpt:::as_recipe("survey"), class = "gr_ambiguous_name")
  expect_identical(rec$read$reader, "hierarchical")   # the recipe still wins
  # And a name owned by only one of the two stays silent.
  expect_silent(readgpt:::as_recipe("preview"))
  expect_identical(readgpt:::as_recipe("preview")$read$reader, "preview")
})


test_that("a plan that skips every section reads nothing, rather than everything", {
  # `unlist()` on an empty list returns NULL, not integer(0), and `fit_chunks()`
  # reads `order %||% seq_len(nrow(df))` -- so a plan marking every section skip
  # handed it NULL and it read the WHOLE document. The reader did the exact
  # opposite of what the plan said and charged for it, which is the worst
  # direction for this bug to point, and nothing in the answer said so.
  ch <- gr_segment(gr_ingest(sample_doc(3, 2)), list(method = "structural", max_tokens = 200))
  cl <- mock_planner(rep("skip", 3))
  a <- quiet(gr_read(ch, "How many participants?", cl, list(reader = "preview")))

  expect_identical(call_labels(cl), "preview.plan")     # the plan call, and nothing else
  expect_true(is_not_found(a$answer))
  expect_length(a$chunks_used, 0L)
  expect_true(a$partial)
  expect_match(a$notes$reason, "skipped every section")
  expect_equal(a$notes$read, 0L)
  expect_equal(a$notes$skipped, 3L)
})

test_that("a partial plan reads its sections and no others", {
  ch <- gr_segment(gr_ingest(sample_doc(4, 3)), list(method = "structural", max_tokens = 150))
  cl <- mock_planner(c("skip", "read", "skip", "skip"))
  a <- quiet(gr_read(ch, "Q?", cl, list(reader = "preview")))
  expect_equal(a$notes$read, 1L)
  expect_equal(a$notes$skipped, 3L)
  # Exactly the one section's chunks, not all of them and not none.
  expect_gt(length(a$chunks_used), 0L)
  expect_lt(length(a$chunks_used), nrow(ch$chunks))
  in_sec2 <- ch$chunks$chunk_id[ch$chunks$section == unique(ch$chunks$section)[2]]
  expect_setequal(a$chunks_used, in_sec2)
})
