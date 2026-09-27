# test-review6-read.R -- the reading axis, sixth pass: the medium and low
# findings left in the readers, MMR, gr_compare() and the v1 shims.
#
# Each block names the finding and says what the code did before the fix.

# Chunks straight from text, one per element, with no segmenter in between.
r6_chunks <- function(txt, section = NA_character_) {
  new_chunks(txt, "precomputed", gr_segment_spec(max_tokens = max(gr_count_tokens(txt), 32L)),
             section = section)
}

r6_is <- function(m, prompt) identical(m[[1]]$content, prompt)

# The last request a mock client saw.
r6_last <- function(cl) {
  calls <- cl$calls()
  calls[[length(calls)]]
}

# ---------------------------------------------------------------------------
# read-methods-07: iterative scored a query that fell back to lexical vectors
# against the chunks' API vectors, two spaces of different dimension, and
# answered partial = FALSE
# ---------------------------------------------------------------------------

test_that("iterative ranks a query whose embedding fell back in the same space as the chunks", {
  topics <- c("cafeteria", "parking", "enrolment", "budget", "weather", "trial")
  txt <- c("The cafeteria menu changed in spring and now serves soup.",
           "Parking permits are renewed each year at the front desk.",
           "Trial enrolment reached 482 participants across nine sites.",
           "The budget for the year was approved by the board in May.",
           "Weather delayed the groundbreaking by two weeks in March.",
           "The trial enrolment target of 500 was nearly met by June.")
  ch <- r6_chunks(txt)
  keyword <- function(texts) t(vapply(texts, function(x) {
    v <- vapply(topics, function(k) as.numeric(grepl(k, x, ignore.case = TRUE)), numeric(1)) + 0.01
    v / sqrt(sum(v^2))
  }, numeric(6), USE.NAMES = FALSE))
  run <- function(fail_query) {
    n <- 0L
    cl <- gr_mock_client(function(m, p) {
      if (grepl("reading iteratively", m[[1]]$content, fixed = TRUE)) {
        return('{"can_answer": true, "answer": "482 participants.", "next_query": ""}')
      }
      "unused"
    }, embed_handler = function(texts, params) {
      n <<- n + 1L
      if (fail_query && n == 2L) stop("HTTP 429 rate limited")
      keyword(texts)
    })
    suppressMessages(suppressWarnings(
      gr_read(ch, "How many were in the trial enrolment?", cl,
              list(reader = "iterative", top_k = 2, max_rounds = 1))))
  }
  healthy <- run(FALSE)
  expect_setequal(healthy$chunks_used, c(3L, 6L))
  expect_false(healthy$partial)

  # Before: chunks 1 and 2 (cafeteria, parking), partial = FALSE and
  # embedding_fallback = FALSE, with only a warning to say the query fell back.
  a <- run(TRUE)
  expect_setequal(a$chunks_used, c(3L, 6L))
  expect_true(a$partial)
  expect_true(a$notes$embedding_fallback)
  expect_true(any(grepl("embeddings fell back", readgpt:::partial_reasons(a))))
})

# ---------------------------------------------------------------------------
# read-methods-01: an ensemble whose member had failed calls came back
# partial = FALSE
# ---------------------------------------------------------------------------

test_that("an ensemble is partial when a member it adjudicated is", {
  txt <- sprintf("Site %d enrolled %d participants in the trial.", 1:10, 40 + 1:10)
  ch <- r6_chunks(txt)
  cl <- gr_mock_client(function(m, p) {
    body <- paste(vapply(m, function(x) x$content, ""), collapse = "\n")
    ids <- as.integer(regmatches(body, gregexpr("(?<=\\[chunk )[0-9]+", body, perl = TRUE))[[1]])
    if (length(ids) == 1L && ids >= 4L) stop("HTTP 500")
    "Enrolment ranged from 41 to 50 participants."
  })
  a <- quiet(gr_read(ch, "What was enrolment?", cl,
                     list(reader = "ensemble", members = c("retrieve", "map_reduce"))))
  expect_identical(a$notes$adjudication, "llm")
  expect_equal(a$notes$member_notes$map_reduce$failed_calls, 7)
  # Before: partial = FALSE, and nothing on the answer named the failures.
  expect_true(a$partial)
  expect_equal(a$notes$failed_calls, 7)
  expect_identical(a$notes$partial_members, "map_reduce")
  expect_true(any(grepl("7 request\\(s\\) failed", readgpt:::partial_reasons(a))))

  # Members that both read everything leave it whole.
  whole <- quiet(gr_read(ch, "What was enrolment?",
                         gr_mock_client(function(m, p) "Enrolment ranged from 41 to 50."),
                         list(reader = "ensemble", members = c("stuff", "map_reduce"))))
  expect_false(whole$partial)
  expect_length(whole$notes$partial_members, 0L)
})

# ---------------------------------------------------------------------------
# read-methods-02: hierarchical dropped a group whose level-2 summary failed,
# failed_summaries = 0, partial = FALSE
# ---------------------------------------------------------------------------

test_that("hierarchical counts a failed summary at every level and keeps the group", {
  local_registries()
  gr_register_model("r6-tiny-4k", context_window = 4000L, max_output = 1000L,
                    input_usd = 0, output_usd = 0)
  txt <- sprintf("Site %d enrolled %d participants in the trial.", 1:20, 40 + 1:20)
  ch <- r6_chunks(txt)
  long <- paste(rep("Summary sentence about enrolment at the site and its numbers.", 30),
                collapse = " ")
  n2 <- 0L
  cl <- gr_mock_client(function(m, p) {
    if (r6_is(m, readgpt:::.gr_prompts$summarise_system)) {
      if (grepl("^<text>\nSite", m[[3]]$content)) return(long)
      n2 <<- n2 + 1L
      if (n2 == 2L) stop("HTTP 500 transient")
      return(sprintf("L2 summary %d", n2))
    }
    "Final answer from summaries."
  })
  a <- quiet(gr_read(ch, "What was enrolment?", cl,
                     list(reader = "hierarchical", model = "r6-tiny-4k", max_summary_tokens = 600)))
  expect_identical(sum(call_labels(cl) == "hier.summarise.L2"), 4L)
  # Before: partial = FALSE, failed_summaries = 0, and the final prompt held
  # L2 summaries 1, 3 and 4 only.
  expect_true(a$partial)
  expect_equal(a$notes$failed_summaries, 1L)
  body <- r6_last(cl)$messages[[2]]$content
  expect_match(body, "L2 summary 1", fixed = TRUE)
  expect_match(body, "L2 summary 4", fixed = TRUE)
  # The failed group's own summaries went up in its place.
  expect_match(body, "Summary sentence about enrolment", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# model-output-13: iterative returned a blank or null answer given with
# can_answer = true as complete
# ---------------------------------------------------------------------------

test_that("iterative treats 'can answer' with no answer as a failed step", {
  ch <- quiet(gr_segment(gr_ingest(sample_doc(3, 3)), list(method = "paragraph", max_tokens = 120)))
  for (ans in c('""', "null", '"   "')) {
    cl <- gr_mock_client(function(m, p) {
      if (grepl("reading iteratively", m[[1]]$content, fixed = TRUE)) {
        return(sprintf('{"can_answer": true, "answer": %s, "next_query": ""}', ans))
      }
      "The cohort comprised 482 participants."
    })
    a <- quiet(gr_read(ch, "How many participants?", cl, list(reader = "iterative", top_k = 2)))
    # Before: '' or NOT_IN_DOCUMENT, with evidence, partial = FALSE, one call.
    expect_identical(call_labels(cl), c("iterative.step", "iterative.final"), info = ans)
    expect_identical(a$answer, "The cohort comprised 482 participants.", info = ans)
    expect_true(a$partial, info = ans)
    expect_identical(a$notes$stop_reason, "step gave no answer", info = ans)
  }
})

# ---------------------------------------------------------------------------
# read-methods-13: iterative asked for no citations on the step that writes
# its answer
# ---------------------------------------------------------------------------

test_that("iterative asks for citations on the step that answers", {
  ch <- quiet(gr_segment(gr_ingest(sample_doc(3, 3)), list(method = "paragraph", max_tokens = 120)))
  cl <- mock_echo("482 participants.")
  quiet(gr_read(ch, "How many participants?", cl, list(reader = "iterative", cite = TRUE)))
  sys <- cl$calls()[[1]]$messages[[1]]$content
  expect_identical(call_labels(cl), "iterative.step")
  expect_match(sys, "Cite the excerpt", fixed = TRUE)
  expect_match(sys, "reading iteratively", fixed = TRUE)

  cl <- mock_echo("482 participants.")
  quiet(gr_read(ch, "How many participants?", cl, list(reader = "iterative", cite = FALSE)))
  expect_false(grepl("Cite the excerpt", cl$calls()[[1]]$messages[[1]]$content, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# read-core-09: MMR took a raw dot product for redundancy, so vectors not of
# unit length made mmr < 1 pick an irrelevant chunk
# ---------------------------------------------------------------------------

test_that("MMR picks the same chunks whatever the length of the vectors", {
  emb <- rbind(c(1, 0.02, 0, 0), c(1, 0.03, 0, 0), c(0.6, 0.8, 0, 0), c(0, 0, 1, 0.05))
  emb <- emb / sqrt(rowSums(emb^2))
  rel <- c(0.95, 0.94, 0.80, 0.10)
  unit <- readgpt:::mmr_select(rel, emb, 2L, 0.7)
  expect_identical(unit, c(1L, 3L))
  # Before: c(1L, 4L), the cafeteria chunk.
  expect_identical(readgpt:::mmr_select(rel, emb * 30, 2L, 0.7), unit)
  # A zero row stays a legitimate, unpromising choice, and nothing hangs.
  z <- emb; z[4, ] <- 0
  expect_length(readgpt:::mmr_select(rel, z * 30, 4L, 0.5), 4L)

  txt <- c("The office cafeteria menu changed in spring.",
           "Revenue in 2023 was 45.2 million dollars, an increase over 2022.",
           "Revenue in 2023 was 45.2 million dollars, an increase over 2022 figures.",
           "Operating margin rose to 12 percent on lower costs.")
  ch <- r6_chunks(txt)
  vec <- function(x) {
    if (grepl("cafeteria", x)) c(0, 0, 1, 0.05)
    else if (grepl("margin", x)) c(0.6, 0.8, 0, 0)
    else if (grepl("Revenue", x)) c(1, 0.02 * nchar(x) / 60, 0, 0)
    else c(0.9, 0.3, 0.1, 0)
  }
  used <- lapply(c(1, 30), function(s) {
    cl <- gr_mock_client(function(m, p) "ans", embed_handler = function(texts, params) {
      t(vapply(texts, function(x) { v <- vec(x); s * v / sqrt(sum(v^2)) }, numeric(4),
               USE.NAMES = FALSE))
    })
    quiet(gr_read(ch, "What was revenue and margin?", cl,
                  list(reader = "retrieve", top_k = 2, mmr = 0.7)))$chunks_used
  })
  expect_false(1L %in% used[[2]])
  expect_setequal(used[[2]], used[[1]])
})

# ---------------------------------------------------------------------------
# read-methods-09: gr_compare() lost the calls and cost of a recipe that
# failed after spending
# ---------------------------------------------------------------------------

test_that("a comparison's trace keeps the requests of a recipe that failed after making them", {
  local_registries()
  gr_register_model("r6-priced", context_window = 128000L, max_output = 4000L,
                    input_usd = 10, output_usd = 30)
  gr_register_reader("r6_each_then_fail", function(chunks, question, client, spec, trace) {
    for (i in seq_len(nrow(chunks$chunks))) {
      gr_call(client, list(list(role = "user", content = chunks$chunks$text[i])),
              model = spec$model, trace = trace, label = "custom")
    }
    stop("post-processing bug")
  }, signature = "custom|N|none", cost_calls = "N", description = "fails after its calls")
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million.")
  txt <- paste(sprintf("Paragraph %d says revenue rose by %d percent in the year under review.",
                       1:14, 1:14), collapse = "\n\n")
  recs <- list(
    gr_recipe("a", segment = list(method = "paragraph", max_tokens = 40),
              read = list(reader = "r6_each_then_fail", model = "r6-priced")),
    gr_recipe("b", segment = list(method = "paragraph", max_tokens = 4000),
              read = list(reader = "stuff", model = "r6-priced")))
  cmp <- quiet(gr_compare(txt, "What was revenue?", recs, client = cl))
  expect_identical(cmp$summary$error[1], "post-processing bug")
  made <- length(cl$calls())
  expect_gt(made, 2L)
  # Before: the trace recorded 1 call and only recipe b's spend.
  expect_equal(cmp$trace$calls, made)
  expect_equal(sum(as.data.frame(cmp$trace)$recipe == "a", na.rm = TRUE), made - 1L)
  expect_equal(cmp$trace$spent_usd, sum(gr_trace_cost(cmp$trace)$usd))
})

# ---------------------------------------------------------------------------
# state-concurrency-07: gr_compare() segmented each recipe on the shared
# trace, so a later recipe's LLM segmentation met the cap its predecessors
# had spent
# ---------------------------------------------------------------------------

test_that("a recipe's segmentation in a comparison is counted for that recipe alone", {
  local_registries()
  local_clean_cache()
  txt <- paste(vapply(1:24, function(i) {
    paste(rep(sprintf("Paragraph %d notes that site %d enrolled %d people.", i, i, 40 + i), 3),
          collapse = " ")
  }, character(1)), collapse = "\n\n")
  thor <- gr_recipe("thor", segment = list(method = "paragraph", max_tokens = 230),
                    read = list(reader = "map_reduce"))
  prop <- gr_recipe("prop", segment = list(method = "proposition", max_tokens = 400,
                                           proposition_batch_tokens = 300),
                    read = list(reader = "stuff"))
  gr_options(max_calls = 9)
  alone <- quiet(gr_compare(txt, "How many enrolled?", list(prop), client = mock_echo("An answer.")))
  readgpt:::gr_flush_caches()
  both <- quiet(gr_compare(txt, "How many enrolled?", list(thor, prop),
                           client = mock_echo("An answer.")))
  expect_false(any(both$summary$partial))
  a <- alone$answers$prop
  b <- both$answers$prop
  # Before: two proposition batches were kept as written, the chunks differed
  # from the recipe's own, and the answer said nothing.
  expect_false(any(grepl("kept as written", b$warnings)))
  expect_identical(b$warnings, a$warnings)
  expect_identical(b$segmentation, a$segmentation)
  df <- as.data.frame(both$trace)
  expect_equal(sum(df$stage == "segment.proposition"), sum(as.data.frame(alone$trace)$stage ==
                                                            "segment.proposition"))
  expect_equal(both$trace$calls, nrow(df))
})

test_that("a comparison does not share a chunking that a limit cut short", {
  local_registries()
  local_clean_cache()
  txt <- paste(vapply(1:24, function(i) {
    paste(rep(sprintf("Paragraph %d notes that site %d enrolled %d people.", i, i, 40 + i), 3),
          collapse = " ")
  }, character(1)), collapse = "\n\n")
  seg <- list(method = "proposition", max_tokens = 400, proposition_batch_tokens = 300)
  p1 <- gr_recipe("p1", segment = seg, read = list(reader = "stuff"))
  p2 <- gr_recipe("p2", segment = seg, read = list(reader = "stuff", max_answer_tokens = 900))
  # Room for the full chunking once: the first recipe's four batches and its
  # answer, and the second recipe charged the same four as if run alone.
  gr_options(max_calls = 5)
  cmp <- quiet(gr_compare(txt, "How many enrolled?", list(p1, p2), client = mock_echo("An answer.")))
  df <- as.data.frame(cmp$trace)
  # One chunking, shared: made once, and it belongs to no one recipe.
  expect_equal(sum(df$stage == "segment.proposition"), 4L)
  expect_true(all(is.na(df$recipe[df$stage == "segment.proposition"])))
  expect_false(any(cmp$summary$partial))
  expect_equal(cmp$trace$calls, nrow(df))
  expect_equal(cmp$answers$p2$trace$calls, 1L)

  # The second recipe is charged the requests the shared chunking took, as it
  # would be alone: here that leaves too few for map_reduce, as it does alone.
  p3 <- gr_recipe("p3", segment = seg, read = list(reader = "map_reduce"))
  readgpt:::gr_flush_caches()
  alone <- quiet(gr_compare(txt, "How many enrolled?", list(p3), client = mock_echo("An answer.")))
  readgpt:::gr_flush_caches()
  cmp <- quiet(gr_compare(txt, "How many enrolled?", list(p1, p3), client = mock_echo("An answer.")))
  expect_false(is.na(alone$summary$error))
  expect_identical(cmp$summary$error[2], alone$summary$error)

  # With room for only part of it, each recipe cuts the document itself.
  readgpt:::gr_flush_caches()
  gr_options(max_calls = 3)
  cmp <- quiet(gr_compare(txt, "How many enrolled?", list(p1, p2), client = mock_echo("An answer.")))
  df <- as.data.frame(cmp$trace)
  expect_equal(sum(df$stage == "segment.proposition"), 6L)
})

# ---------------------------------------------------------------------------
# read-methods-08: the v1 shims ran chunk text through gr_ingest() as a
# source, and rebuilt a gr_chunks without its unread pages
# ---------------------------------------------------------------------------

test_that("the v1 shims read their chunks as given, and never as a source", {
  local_registries()
  cl <- gr_mock_client(function(m, p) "5 million")
  # gr_ingest() takes a string for a path, a web address or text. Before, a
  # chunk that was a web address was fetched.
  testthat::local_mocked_bindings(gr_ingest = function(...) stop("gr_ingest() was called"),
                                  .package = "readgpt")
  for (x in c("Revenue was 5M.", "The final figures are in appendix.docx",
              "http://127.0.0.1:9/compat-probe")) {
    # Before: "Only 15 characters survived ingestion", "File not found", and a
    # request to the address.
    expect_identical(quiet(gpt_read_chunked(x, "What was revenue?", client = cl)), "5 million",
                     info = x)
  }
  expect_identical(quiet(gpt_read_retrieval("Revenue was 5M.", "What was revenue?", client = cl)),
                   "5 million")
  expect_error(quiet(gpt_read_chunked(c("", "  "), "What was revenue?", client = cl)),
               class = "gr_empty_chunks")

  # A gr_chunks keeps what it knows: pages never read make the answer partial.
  ch <- r6_chunks(c("Revenue was 5 million dollars in the year.",
                    "Costs rose sharply in the second half."))
  ch$unread_pages <- c(3L, 4L)
  j <- jsonlite::fromJSON(quiet(gpt_read_chunked(ch, "What was revenue?", client = cl,
                                                 return_json = TRUE)))
  expect_true(j$partial)
  expect_equal(j$notes$unread_pages, c(3L, 4L))
})

# ---------------------------------------------------------------------------
# read-methods-04: skim ignored a failed consolidation of its evidence, and
# asked for citations from a body with no chunk labels, checking them against
# every chunk
# ---------------------------------------------------------------------------

test_that("skim reports a failed consolidation and checks citations against what survived it", {
  local_registries()
  gr_register_model("r6-tiny-4k", context_window = 4000L, max_output = 1000L,
                    input_usd = 0, output_usd = 0)
  txt <- vapply(1:12, function(i) {
    paste(rep(sprintf("In site %d the enrolment was %d participants.", i, 40 + i), 50), collapse = " ")
  }, character(1))
  ch <- r6_chunks(txt)
  answer <- "Enrolment per site was 41 to 52 participants [chunk 3] [chunk 11]."
  client <- function(consolidate) gr_mock_client(function(m, p) {
    sys <- m[[1]]$content
    if (grepl("extract evidence", sys, fixed = TRUE)) {
      return(sub("^<excerpt>\n\\[chunk [0-9]+\\]\n", "", sub("\n</excerpt>$", "", m[[3]]$content)))
    }
    if (r6_is(m, readgpt:::.gr_prompts$summarise_system)) return(consolidate(m))
    answer
  })
  cl <- client(function(m) stop("HTTP 503"))
  a <- quiet(gr_read(ch, "What was enrolment per site?", cl,
                     list(reader = "skim", model = "r6-tiny-4k", cite = TRUE, max_answer_tokens = 300)))
  expect_true(a$notes$evidence_consolidated)
  # Before: partial = FALSE, failed_calls = 0, cited_unknown not set.
  expect_true(a$partial)
  expect_false(a$notes$merge_ok)
  expect_gt(a$notes$failed_calls, 0L)
  body <- r6_last(cl)$messages[[2]]$content
  shown <- readgpt:::cited_chunks(body)
  expect_false(any(c(3L, 11L) %in% shown))
  expect_setequal(a$notes$cited_unknown, c(3L, 11L))

  # A consolidation that keeps no label is not asked to be cited.
  cl <- client(function(m) "Sites enrolled between 41 and 52 participants each.")
  a <- quiet(gr_read(ch, "What was enrolment per site?", cl,
                     list(reader = "skim", model = "r6-tiny-4k", cite = TRUE, max_answer_tokens = 300)))
  expect_true(a$notes$merge_ok)
  expect_true(a$notes$cite_dropped)
  expect_false(grepl("Cite the excerpt", r6_last(cl)$messages[[1]]$content, fixed = TRUE))
  expect_setequal(a$notes$cited_unknown, c(3L, 11L))
  expect_true(a$partial)
})

# ---------------------------------------------------------------------------
# read-methods-12: retrieve over no chunks sent `[chunk NA]` and returned the
# model's prior as the answer
# ---------------------------------------------------------------------------

test_that("retrieve over no chunks makes no request and is not found", {
  cl <- gr_mock_client(function(m, p) "The trial enrolled 482 participants.")
  a <- quiet(gr_read(r6_chunks(character(0)), "How many enrolled?", cl, list(reader = "retrieve")))
  # Before: one request with '[chunk NA]\nNA', its reply, chunks_used = NA.
  expect_length(cl$calls(), 0L)
  expect_true(is_not_found(a$answer))
  expect_true(a$partial)
  expect_length(a$chunks_used, 0L)
})

# ---------------------------------------------------------------------------
# read-methods-05: rerank dropped chosen chunks that did not fit without
# marking partial, and answered from an empty <excerpts> when none fitted
# ---------------------------------------------------------------------------

test_that("rerank says which chosen chunks did not fit, and does not answer from none", {
  local_registries()
  gr_register_model("r6-tiny-8k", context_window = 8000L, max_output = 2000L,
                    input_usd = 0, output_usd = 0)
  para <- function(i, n) {
    paste(rep(sprintf("Revenue item %d was reported in the ledger for the year.", i), n), collapse = " ")
  }
  ch <- r6_chunks(vapply(1:6, para, character(1), n = 130))
  cl <- mock_echo("Revenue was 42 million dollars.")
  a <- quiet(gr_read(ch, "What was revenue?", cl,
                     list(reader = "rerank", model = "r6-tiny-8k", top_k = 6)))
  expect_length(a$chunks_used, 3L)
  # Before: partial = FALSE with nothing about the three that were dropped.
  expect_true(a$partial)
  expect_equal(a$notes$dropped_chunks, 3L)
  expect_true(any(grepl("did not fit", readgpt:::partial_reasons(a))))

  ch <- r6_chunks(vapply(1:6, para, character(1), n = 650))
  cl <- mock_echo("Revenue was 42 million dollars.")
  a <- quiet(gr_read(ch, "What was revenue?", cl,
                     list(reader = "rerank", model = "r6-tiny-8k", skim_model = "mock-model",
                          top_k = 6)))
  # Before: the answer call went out with an empty <excerpts> and its reply came
  # back as the answer, partial = FALSE.
  expect_false("rerank.answer" %in% call_labels(cl))
  expect_true(is_not_found(a$answer))
  expect_true(a$partial)
})

# ---------------------------------------------------------------------------
# read-methods-06: preview put the skim evidence on top of read rows already
# fitted to the whole budget, and read a demoted section in full as well
# ---------------------------------------------------------------------------

test_that("preview fits its whole answer prompt to the budget", {
  local_registries()
  gr_register_model("r6-tiny-8k", context_window = 8000L, max_output = 2000L,
                    input_usd = 0, output_usd = 0)
  para <- function(s, i) {
    paste(rep(sprintf("Section %d part %d reports enrolment of %d participants.", s, i, 40 + s), 40),
          collapse = " ")
  }
  txt <- unlist(lapply(1:8, function(s) c(para(s, 1), para(s, 2))))
  ch <- r6_chunks(txt, section = rep(sprintf("Part %d", 1:8), each = 2))
  q <- "What was enrolment?"
  spec <- list(reader = "preview", model = "r6-tiny-8k", max_answer_tokens = 1500,
               max_chunk_tokens = 1500)
  bud <- gr_budget("r6-tiny-8k", reserve_output = 1500,
                   overhead = readgpt:::prompt_overhead(q, readgpt:::answer_system(FALSE)))
  run <- function(skim_words) {
    cl <- gr_mock_client(function(m, p) {
      sys <- m[[1]]$content
      if (grepl("You plan how to read a document", sys, fixed = TRUE)) {
        return(preview_plan_json(rep("read", 8)))
      }
      if (grepl("extract evidence, not answers", sys, fixed = TRUE)) {
        # Verbatim, so the evidence verifies: the first words of the section.
        first <- sub("^<excerpt>\n\\[chunk [^]]*\\]\n", "", m[[3]]$content)
        return(paste(utils::head(strsplit(first, " ")[[1]], skim_words), collapse = " "))
      }
      "ANSWER"
    })
    a <- quiet(gr_read(ch, q, cl, spec))
    last <- r6_last(cl)
    list(a = a, last = last,
         body = last$messages[[2]]$content,
         tokens = sum(gr_count_tokens(vapply(last$messages[-1], function(x) x$content, ""))))
  }
  small <- run(60)
  expect_gt(small$a$notes$demoted_to_skim, 0L)
  expect_identical(small$last$label, "preview.answer")
  # Before: 7277 tokens against a 5558 budget, and the reply's room cut from
  # 1500 to 691.
  expect_lte(small$tokens, bud$input)
  expect_equal(small$last$params$max_output, 1500)
  # No chunk is both read and skimmed.
  labels <- regmatches(small$body, gregexpr("\\[chunk [0-9]+", small$body))[[1]]
  expect_false(anyDuplicated(labels) > 0L)

  # Skims too big for the room left are left out, counted, and make it partial.
  big <- run(400)
  expect_identical(big$last$label, "preview.answer")
  expect_lte(big$tokens, bud$input)
  expect_equal(big$last$params$max_output, 1500)
  expect_gt(big$a$notes$dropped_chunks, 0L)
  expect_true(big$a$partial)
  expect_true(any(grepl("did not fit", readgpt:::partial_reasons(big$a))))
  expect_false(any(big$a$evidence$kind == "extracted" &
                   !big$a$evidence$chunk_id %in% readgpt:::cited_chunks(big$body)))
})

# ---------------------------------------------------------------------------
# read-methods-11: preview counted every chunk as sent, so a citation of a
# section the plan skipped was never caught
# ---------------------------------------------------------------------------

test_that("preview catches a citation of a section no request was shown", {
  txt <- sprintf("Budget line %d totalled %d dollars in the year.", 1:8, 1000 + 1:8)
  ch <- r6_chunks(txt, section = rep(sprintf("Part %d", 1:4), each = 2))
  q <- "What did budget line 4 total?"
  cl <- mock_planner(c("read", "skip", "skip", "skip"),
                     answer = "Budget line 4 totalled 1008 dollars [chunk 8].")
  a <- quiet(gr_read(ch, q, cl, list(reader = "preview", cite = TRUE)))
  # Before: cited_unknown NULL.
  expect_equal(a$notes$cited_unknown, 8L)
  expect_true(any(grepl("never sent", readgpt:::partial_reasons(a))))

  # A section a skim was sent for is not an invention to cite.
  cl <- mock_planner(c("read", "skim", "skip", "skip"),
                     answer = "Line 4 [chunk 4] and line 3 [chunk 3], line 1 [chunk 1].")
  a <- quiet(gr_read(ch, q, cl, list(reader = "preview", cite = TRUE)))
  expect_null(a$notes$cited_unknown)
})

# ---------------------------------------------------------------------------
# read-methods-15: map_reduce said failed_calls = 0, partial = FALSE, when a
# merge below the last level failed
# ---------------------------------------------------------------------------

test_that("map_reduce counts a merge request that failed below the last level", {
  local_registries()
  gr_register_model("r6-tiny-4k", context_window = 4000L, max_output = 1000L,
                    input_usd = 0, output_usd = 0)
  txt <- sprintf("Site %d enrolled %d participants in the trial.", 1:12, 40 + 1:12)
  ch <- r6_chunks(txt)
  long <- paste(rep("Finding sentence about enrolment at the site and its numbers.", 70),
                collapse = " ")
  n_merge <- 0L
  cl <- gr_mock_client(function(m, p) {
    if (r6_is(m, readgpt:::.gr_prompts$merge_system)) {
      n_merge <<- n_merge + 1L
      if (n_merge == 1L) stop("HTTP 500")
      return(paste("Merged:", nchar(m[[2]]$content), "chars"))
    }
    long
  })
  a <- quiet(gr_read(ch, "What was enrolment?", cl,
                     list(reader = "map_reduce", model = "r6-tiny-4k", max_answer_tokens = 400,
                          max_chunk_tokens = 1000)))
  expect_true("reduce.level1" %in% call_labels(cl))
  expect_true(a$notes$merge_ok)
  # Before: failed_calls = 0, partial = FALSE, with the error in the trace.
  expect_true(a$partial)
  expect_equal(a$notes$failed_calls, 1L)
  expect_equal(a$notes$merge_failures, 1L)

  # Merges that all work leave it whole.
  cl <- gr_mock_client(function(m, p) {
    if (r6_is(m, readgpt:::.gr_prompts$merge_system)) return("Merged.")
    long
  })
  a <- quiet(gr_read(ch, "What was enrolment?", cl,
                     list(reader = "map_reduce", model = "r6-tiny-4k", max_answer_tokens = 400,
                          max_chunk_tokens = 1000)))
  expect_false(a$partial)
  expect_equal(a$notes$merge_failures, 0L)
})
