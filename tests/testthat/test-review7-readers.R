# test-review7-readers.R -- the readers' share of the cross-file follow-ups
# from the sixth round: what other files' fixes needed from the readers, the
# answer object and the entry points.
#
# Each block names the handoff and says what the code did before the change.

# Chunks straight from text, one per element, with no segmenter in between.
r7_chunks <- function(txt, section = NA_character_, page = NA_integer_) {
  new_chunks(txt, "precomputed", gr_segment_spec(max_tokens = max(gr_count_tokens(txt), 32L)),
             section = section, page = page)
}

r7_is <- function(m, prompt) identical(m[[1]]$content, prompt)

r7_json <- function(x) jsonlite::fromJSON(as_json(x), simplifyVector = FALSE)

# ---------------------------------------------------------------------------
# read-core H1 (r2-non-latin-text-pipeline-09): skim and preview matched NONE
# with an ASCII-only pattern, so "NONE" with an ideographic full stop was kept
# as a passage, failed to verify, and marked a complete read partial
# ---------------------------------------------------------------------------

test_that("skim and preview drop a NONE written with an ideographic full stop", {
  txt <- c("Revenue was 45.2 million in 2023.", "The cafeteria serves soup on Fridays.",
           "Parking is behind the main building.")
  cl <- gr_mock_client(function(m, p) {
    if (r7_is(m, readgpt:::.gr_prompts$extract_system)) {
      if (grepl("Revenue", m[[3]]$content, fixed = TRUE)) return("Revenue was 45.2 million in 2023.")
      return("NONE\u3002")
    }
    if (grepl("You plan how to read a document", m[[1]]$content, fixed = TRUE)) {
      return(preview_plan_json(c("read", "skim")))
    }
    "Revenue was 45.2 million."
  })
  a <- quiet(gr_read(r7_chunks(txt), "What was revenue?", cl, "skim"))
  # Before: partial = TRUE, with_evidence = 3 and two unverified quotations.
  expect_false(a$partial)
  expect_null(a$notes$unverified_evidence)
  expect_identical(a$notes$with_evidence, 1L)

  p <- quiet(gr_read(r7_chunks(txt, section = c("Finance", "Campus", "Campus")),
                     "What was revenue?", cl, "preview"))
  expect_identical(p$notes$skimmed, 1L)
  # Before: the NONE came back as skim evidence that did not verify.
  expect_null(p$notes$unverified_evidence)
  expect_false(p$partial)
  expect_false(any(p$evidence$kind == "extracted"))
})

# ---------------------------------------------------------------------------
# read-core H2 (read-core-13): preview's outline wrote pages with "%d", which
# failed on page labels and fractional pages
# ---------------------------------------------------------------------------

test_that("preview outlines chunks whose pages are labels or fractions", {
  seen <- character(0)
  cl <- gr_mock_client(function(m, p) {
    if (grepl("You plan how to read a document", m[[1]]$content, fixed = TRUE)) {
      seen <<- c(seen, m[[3]]$content)
      return(preview_plan_json("read"))
    }
    "An answer."
  })
  ch <- new_chunks(c("a", "b"), "manual", gr_segment_spec(max_tokens = 100), page = c("iv", "v"))
  # Before: "invalid format '%d'; use format %s for character objects".
  a <- quiet(gr_read(ch, "What is it?", cl, "preview"))
  expect_s3_class(a, "gr_answer")
  expect_true(any(grepl("pp. iv-iv |", seen, fixed = TRUE)))
  # Labels in a section are named first and last; numbers least and greatest.
  txt <- c("Alpha text.", "Beta text.", "Gamma text.")
  sec <- c("Front", "Front", "Back")
  seen <- character(0)
  a <- quiet(gr_read(r7_chunks(txt, section = sec, page = c("iv", "v", "vi")), "What is it?",
                     cl, "preview"))
  expect_true(any(grepl("pp. iv-v |", seen, fixed = TRUE)))
  expect_true(any(grepl("pp. vi-vi |", seen, fixed = TRUE)))
  seen <- character(0)
  # Before: "invalid format '%d'; use format %f, %e, %g or %a for numeric objects".
  a <- quiet(gr_read(r7_chunks(txt, section = sec, page = c(2.5, 1.5, 3)), "What is it?",
                     cl, "preview"))
  expect_s3_class(a, "gr_answer")
  expect_true(any(grepl("pp. 1.5-2.5 |", seen, fixed = TRUE)))
  expect_true(any(grepl("pp. 3-3 |", seen, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# read-core H3 / H4 (r2-export-serialization-fidelity-04): an answer read on
# a shared trace recorded steps from 1, so its record carried every earlier
# run on that trace
# ---------------------------------------------------------------------------

test_that("gr_read() records where its run starts on a trace shared with an earlier read", {
  cl <- gr_mock_client(function(m, p) "It says so.")
  tr <- gr_trace()
  a1 <- quiet(gr_read(r7_chunks("Public memo: revenue was 45.2 million."), "What was revenue?",
                      cl, "stuff", trace = tr))
  n1 <- length(tr$steps)
  a2 <- quiet(gr_read(r7_chunks("Second memo: costs fell 3 percent."), "What about costs?",
                      cl, "stuff", trace = tr))
  expect_identical(unname(a2$trace_steps), c(n1 + 1L, length(tr$steps)))
  # Before: the second answer's record held the first run's prompt.
  j <- as_json(a2)
  expect_false(grepl("Public memo", j, fixed = TRUE))
  expect_true(grepl("Second memo", j, fixed = TRUE))
  expect_false(grepl("Second memo", as_json(a1), fixed = TRUE))

  # A trace holding only the work that led to the read keeps it in the record:
  # the documented gr_segment(trace = tr), then gr_read(trace = tr).
  tr2 <- gr_trace()
  ch <- quiet(gr_segment("Para one says revenue rose.\n\nPara two says costs fell.",
                         list(method = "paragraph", max_tokens = 50), trace = tr2))
  a3 <- quiet(gr_read(ch, "What rose?", cl, "stuff", trace = tr2))
  expect_identical(readgpt:::answer_trace(a3), tr2)
  expect_true("segment" %in% vapply(r7_json(a3)$trace$steps, function(s) s$label, ""))

  # A reader that answers on a trace of its own keeps its own numbering.
  local_registries()
  gr_register_reader("r7_own_trace", function(chunks, question, client, spec, trace) {
    mine <- gr_trace()
    new_answer("Mine.", "r7_own_trace", question, 1L, mine)
  }, signature = "own|0|none", cost_calls = "0", description = "keeps its own trace")
  a4 <- quiet(gr_read(r7_chunks("Anything."), "What?", cl, "r7_own_trace", trace = tr))
  expect_identical(unname(a4$trace_steps), c(1L, 0L))
})

test_that("answer_document() records its whole run, and a failed recipe in a comparison its own", {
  cl <- gr_mock_client(function(m, p) "It says so.")
  tr <- gr_trace()
  quiet(answer_document("Public memo: revenue was 45.2 million in 2023.", "What was revenue?",
                        "fast", client = cl, trace = tr))
  n1 <- length(tr$steps)
  b <- quiet(answer_document("Second memo: costs fell 3 percent in 2024.", "What about costs?",
                             "fast", client = cl, trace = tr))
  # Before: first = 1, and the record held the first document's prompt.
  expect_identical(unname(b$trace_steps), c(n1 + 1L, length(tr$steps)))
  expect_false(grepl("Public memo", as_json(b), fixed = TRUE))
  # Its ingestion and segmentation are in it as well as its read.
  labels <- vapply(r7_json(b)$trace$steps, function(s) s$label, "")
  expect_true(all(c("ingest", "segment", "stuff.answer") %in% labels))
  j <- quiet(answer_document("Third memo: the budget is unchanged.", "What is the budget?",
                             "fast", client = cl, trace = tr, return = "json"))
  expect_false(grepl("Public memo", j, fixed = TRUE))
  expect_false(grepl("Second memo", j, fixed = TRUE))

  # A recipe that fails in a comparison is answered on the comparison's trace.
  local_registries()
  gr_register_reader("r7_call_then_fail", function(chunks, question, client, spec, trace) {
    gr_call(client, list(list(role = "user", content = chunks$chunks$text[1])),
            model = spec$model, trace = trace, label = "r7.custom")
    stop("post-processing bug")
  }, signature = "custom|1|none", cost_calls = "1", description = "fails after a call")
  txt <- paste(sprintf("Paragraph %d says revenue rose by %d percent.", 1:6, 1:6),
               collapse = "\n\n")
  recs <- list(gr_recipe("ok", segment = list(method = "paragraph", max_tokens = 4000),
                         read = list(reader = "stuff")),
               gr_recipe("bad", segment = list(method = "paragraph", max_tokens = 40),
                         read = list(reader = "r7_call_then_fail")))
  cmp <- quiet(gr_compare(txt, "What was revenue?", recs, client = cl))
  bad <- cmp$answers$bad
  steps <- readgpt:::answer_trace(bad)$steps
  labels <- vapply(steps, function(s) s$label, "")
  # Before: steps 1 to where the recipe began, so the other recipe's request
  # and none of its own.
  expect_true("r7.custom" %in% labels)
  expect_false("stuff.answer" %in% labels)
  expect_true(all(vapply(steps, function(s) {
    identical(s$recipe, "bad") || identical(s$recipe, NA_character_)
  }, logical(1))))
})

# ---------------------------------------------------------------------------
# clean h2-tokenize-embed-12 and client H3 (r2-locale-platform-portability-01):
# the entry points took the question as it came, so a CP1252 question failed
# in the token count, and an unlabelled UTF-8 one was pasted into prompts
# unlabelled
# ---------------------------------------------------------------------------

test_that("the entry points take a question in CP1252 or unlabelled UTF-8", {
  cp1252 <- rawToChar(as.raw(c(0x57, 0x68, 0x61, 0x74, 0x20, 0x63, 0x61, 0x66, 0xe9, 0x3f)))
  cl <- gr_mock_client(function(m, p) "It was soup.")
  ch <- r7_chunks("The cafe served soup.")
  # Before: "input string 1 is invalid UTF-8".
  a <- quiet(gr_read(ch, cp1252, cl, "stuff"))
  expect_identical(a$question, "What caf\u00e9?")
  expect_identical(Encoding(a$question), "UTF-8")
  b <- quiet(answer_document("The cafe served soup every day of the week.", cp1252, "fast",
                             client = cl))
  expect_identical(b$question, "What caf\u00e9?")
  cmp <- quiet(gr_compare("The cafe served soup every day of the week.", cp1252, "fast",
                          client = cl))
  expect_identical(cmp$answers$fast$question, "What caf\u00e9?")

  # UTF-8 that arrived without its label is labelled before it is pasted.
  q <- unmarked("What caf\u00e9?")
  expect_identical(Encoding(q), "unknown")
  cl <- gr_mock_client(function(m, p) "It was soup.")
  a <- quiet(gr_read(ch, q, cl, "stuff"))
  expect_identical(Encoding(a$question), "UTF-8")
  sent <- cl$calls()[[1]]$messages
  expect_identical(sent[[length(sent)]]$content, "Question: What caf\u00e9?")

  # In a C locale, paste() wrote the unlabelled letters out as bytes when it
  # joined the question to the document's labelled text.
  cl <- gr_mock_client(function(m, p) "It was soup.")
  fr <- r7_chunks("Le caf\u00e9 servait de la soupe.")
  local({
    withr::local_locale(c(LC_CTYPE = "C"))
    quiet(gr_read(fr, q, cl, list(reader = "stuff", restate = "always")))
  })
  body <- cl$calls()[[1]]$messages[[2]]$content
  # Before: "Question: What caf<c3><a9>?".
  expect_false(grepl("<c3><a9>", body, fixed = TRUE))
  expect_true(startsWith(body, "Question: What caf\u00e9?"))

  # Not a string is still not a question.
  expect_error(gr_read(ch, 42, cl, "stuff"), "non-empty string")
  expect_error(gr_read(ch, NA_character_, cl, "stuff"), "non-empty string")
})

# ---------------------------------------------------------------------------
# surface-05: map_reduce with nothing found listed every chunk as used, and
# gave the same reason when every request had failed
# ---------------------------------------------------------------------------

test_that("map_reduce with nothing found lists no chunk as used and says why", {
  ch <- r7_chunks(c("Alpha is here.", "Beta is here.", "Gamma is here."))
  # Before: chunks_used = 1:3 and "no chunk yielded an answer".
  a <- quiet(gr_read(ch, "What?", mock_dead(), "map_reduce"))
  expect_length(a$chunks_used, 0L)
  expect_identical(a$notes$reason, "every request failed")
  expect_equal(a$notes$failed_calls, 3)
  expect_true(a$partial)

  a <- quiet(gr_read(ch, "What?", gr_mock_client(function(m, p) "NOT_IN_DOCUMENT"), "map_reduce"))
  expect_length(a$chunks_used, 0L)
  expect_identical(a$notes$reason, "no chunk yielded an answer")
  expect_false(a$partial)
})

# ---------------------------------------------------------------------------
# contracts-11: the extract reader said it costs a call per disagreeing field,
# which only resolve = "model" does
# ---------------------------------------------------------------------------

test_that("the extract reader's cost says conflicts are free by default", {
  r <- gr_readers()
  cost <- r$cost_calls[r$name == "extract"]
  expect_false(grepl("one per disagreeing field", cost, fixed = TRUE))
  expect_match(cost, "resolve = 'model'", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# trace cache-trace-05: preflight() and gr_compare() counted only what a run
# paid for, not what a replay's recorded calls cost, so a replay of a run the
# limit stopped made different decisions from the recording
# ---------------------------------------------------------------------------

test_that("the pre-flight counts what a replay's recorded calls cost", {
  local_registries()
  gr_register_model("r7-priced", context_window = 128000L, max_output = 4000L,
                    input_usd = 1, output_usd = 1)
  tr <- gr_trace()
  tr$replayed_usd <- 0.6
  gr_options(max_cost_usd = 0.5)
  cl <- gr_mock_client(function(m, p) "Revenue rose.")
  # Before: the pre-flight saw $0 spent and the read began.
  expect_error(quiet(gr_read(r7_chunks("Revenue rose by 4 percent."), "What rose?", cl,
                             list(reader = "stuff", model = "r7-priced"), trace = tr)),
               class = "gr_cost_cap")
  expect_length(cl$calls(), 0L)
})

test_that("a replay of a comparison the limit stopped stops each recipe where it did", {
  local_registries()
  gr_register_model("r7-priced", context_window = 128000L, max_output = 4000L,
                    input_usd = 0.01, output_usd = 200)
  long <- paste(rep("Revenue rose in this section according to the excerpt.", 30),
                collapse = " ")
  cl <- gr_mock_client(function(m, p) long)
  doc <- paste(sprintf("Section %d. Revenue in region %d rose by %d percent.", 1:40, 1:40, 1:40),
               collapse = "\n\n")
  recs <- list(gr_recipe("a", segment = list(method = "paragraph", max_tokens = 60),
                         read = list(reader = "map_reduce", model = "r7-priced")),
               gr_recipe("b", segment = list(method = "paragraph", max_tokens = 60),
                         read = list(reader = "map_reduce", model = "r7-priced",
                                     max_chunk_tokens = 600)))
  gr_options(max_cost_usd = 0.5)
  live <- quiet(gr_compare(doc, "What rose?", recs, client = cl))
  expect_true(live$trace$budget_stop)
  expect_true(live$answers$a$partial)
  expect_match(live$summary$error[2], "already spent", fixed = TRUE)
  f <- withr::local_tempfile(fileext = ".json")
  gr_trace_save(live$trace, f)

  rp <- gr_replay_client(f)
  again <- quiet(gr_compare(doc, "What rose?", recs, client = rp))
  # Before: recipe b began from nothing counted, went past the recorded stop
  # and missed the recording.
  expect_identical(rp$stats()$misses, 0L)
  expect_identical(again$answers$a$answer, live$answers$a$answer)
  expect_identical(again$summary$error, live$summary$error)
  expect_identical(again$trace$calls, live$trace$calls)
  expect_equal(again$trace$replayed_usd, live$trace$spent_usd, tolerance = 1e-9)
})

# ---------------------------------------------------------------------------
# trace r2-export-serialization-fidelity-06: as_json() wrote one chunk id as a
# number and two as an array, and no notes as [] rather than an object
# ---------------------------------------------------------------------------

test_that("an answer's JSON keeps its lists as arrays at every length", {
  tr <- gr_trace()
  a <- new_answer("Revenue was 45.2 million [chunk 9].", "r7", "What was revenue?",
                  chunks_used = 1L, trace = tr)
  expect_identical(a$notes$cited_unknown, 9L)
  j <- r7_json(a)
  # Before: chunks_used 1 and cited_unknown 9, each a bare number.
  expect_identical(j$chunks_used, list(1L))
  expect_identical(j$notes$cited_unknown, list(9L))
  expect_identical(j$evidence, list())

  none <- new_answer("NOT_IN_DOCUMENT", "r7", "What?", integer(0), tr)
  txt <- as_json(none, pretty = FALSE)
  # Before: "notes":[] and "evidence":null.
  expect_true(grepl('"notes":{}', txt, fixed = TRUE))
  expect_true(grepl('"chunks_used":[]', txt, fixed = TRUE))
  expect_true(grepl('"evidence":[]', txt, fixed = TRUE))

  # An ensemble's lists, and its members' notes, the same way.
  ch <- r7_chunks(sprintf("Site %d enrolled %d participants.", 1:3, 40 + 1:3))
  e <- quiet(gr_read(ch, "What was enrolment?",
                     gr_mock_client(function(m, p) "Enrolment was 41 to 43 [chunk 1]."),
                     list(reader = "ensemble", members = c("stuff", "map_reduce"))))
  je <- r7_json(e)
  expect_identical(je$notes$partial_members, list())
  expect_true(is.list(je$notes$members) && length(je$notes$members) == 2L)
  pages <- new_answer("An answer.", "r7", "What?", 1L, tr, notes = list(unread_pages = 4L))
  expect_identical(r7_json(pages)$notes$unread_pages, list(4L))
  counts <- new_answer("An answer.", "r7", "What?", 1L, tr, notes = list(dropped_chunks = 2L))
  expect_identical(r7_json(counts)$notes$dropped_chunks, 2L)
})

# ---------------------------------------------------------------------------
# read-methods-01 (optional): partial_reasons() did not name an ensemble's
# partial members, and called skim's failed consolidation a failed merge of
# answers
# ---------------------------------------------------------------------------

test_that("partial reasons name an ensemble's partial members and skim's consolidation", {
  tr <- gr_trace()
  ens <- new_answer("An answer.", "ensemble", "What?", 1L, tr, partial = TRUE,
                    notes = list(partial_members = c("retrieve", "map_reduce")))
  # Before: nothing, so print() said "see $notes".
  expect_true(any(grepl("member(s) 'retrieve', 'map_reduce' returned a partial answer",
                        readgpt:::partial_reasons(ens), fixed = TRUE)))
  sk <- new_answer("An answer.", "skim", "What?", 1L, tr, partial = TRUE,
                   notes = list(merge_ok = FALSE))
  expect_true("the evidence could not be consolidated" %in% readgpt:::partial_reasons(sk))
  mr <- new_answer("An answer.", "map_reduce", "What?", 1L, tr, partial = TRUE,
                   notes = list(merge_ok = FALSE))
  expect_true("the answers could not be merged" %in% readgpt:::partial_reasons(mr))
})

# ---------------------------------------------------------------------------
# read-methods-15 / audit (r2-audit-report-truthfulness-01): tree_merge()
# said nothing of a group merge that failed below the last level; its callers
# had to scan the trace for it
# ---------------------------------------------------------------------------

test_that("tree_merge() counts the merge requests that failed, at every level", {
  local_registries()
  gr_register_model("r7-tiny-4k", context_window = 4000L, max_output = 1000L,
                    input_usd = 0, output_usd = 0)
  long <- paste(rep("Finding sentence about enrolment at the site and its numbers.", 70),
                collapse = " ")
  n <- 0L
  cl <- gr_mock_client(function(m, p) {
    n <<- n + 1L
    if (n == 1L) stop("HTTP 500")
    "Merged."
  })
  spec <- gr_read_spec("map_reduce", model = "r7-tiny-4k", max_answer_tokens = 400)
  out <- quiet(readgpt:::tree_merge(cl, "What was enrolment?", rep(long, 6), spec, gr_trace()))
  # Before: no `failed`, and nothing but the trace said a group had failed.
  expect_true(out$ok)
  expect_identical(out$failed, 1L)
  cl <- gr_mock_client(function(m, p) stop("HTTP 500"))
  out <- quiet(readgpt:::tree_merge(cl, "q", c("one finding", "another finding"), spec,
                                    gr_trace()))
  expect_false(out$ok)
  expect_identical(out$failed, 1L)
  expect_identical(readgpt:::tree_merge(cl, "q", "only", spec, gr_trace())$failed, 0L)
})

# ---------------------------------------------------------------------------
# client H1 (client-10): the per-item worst case a parallel batch is held to,
# and the pre-flight's worst case, priced a reasoning model's reply at the cap
# asked for, not the larger one it is sent
# ---------------------------------------------------------------------------

test_that("a reasoning model's requests are priced at the cap they are sent with", {
  local_registries()
  gr_register_model("r7-reason", context_window = 200000L, max_output = 64000L,
                    input_usd = 1, output_usd = 10, reasoning = TRUE,
                    supports_temperature = FALSE)
  gr_register_model("r7-plain", context_window = 200000L, max_output = 64000L,
                    input_usd = 1, output_usd = 10)
  cl <- mock_echo()
  # Before: 100 tokens in and 90 out.
  expect_equal(readgpt:::batch_item_usd(cl, "r7-reason", 100, 90),
               gr_estimate_cost("r7-reason", 100, 2048))
  expect_equal(readgpt:::batch_item_usd(cl, "r7-plain", 100, 90),
               gr_estimate_cost("r7-plain", 100, 90))
  # A cap at or above the floor is priced as it is.
  expect_equal(readgpt:::batch_item_usd(cl, "r7-reason", 100, 3000),
               gr_estimate_cost("r7-reason", 100, 3000))

  pf <- function(model) {
    tr <- gr_trace()
    quiet(gr_read(r7_chunks(c("Revenue rose.", "Costs fell.")), "What rose?", mock_echo(),
                  list(reader = "map_reduce", model = model), trace = tr))
    Filter(function(s) identical(s$label, "preflight"), tr$steps)[[1]]$detail
  }
  r <- pf("r7-reason")
  p <- pf("r7-plain")
  # Before: the same bound for both, every reply at 1,500 tokens.
  expect_identical(r$worst_calls, p$worst_calls)
  extra <- gr_estimate_cost("r7-plain", 0, r$worst_calls * (2048 - 1500))
  expect_gt(extra, 0)
  expect_lt(abs((r$est_cost_usd - p$est_cost_usd) - extra), 2e-4)
})

# ---------------------------------------------------------------------------
# read-core H3 (read-core-12): the worst case of a merge tree counted findings
# bare, while tree_merge() sizes each in its tags
# ---------------------------------------------------------------------------

test_that("the merge tree's worst case counts each finding in its tags", {
  tag <- gr_count_tokens("<findings 999>\n\n</findings 999>\n\n")
  # Ten findings of 100 tokens fit a 1,000-token merge bare, not in tags.
  w <- readgpt:::merge_tree_worst(10, 100, 100, 1000)
  expect_gt(w$calls, 1)                                        # was 1
  expect_equal(w$input, 10 * (100 + tag) + 2 * (100 + tag))

  # An upper bound on what tree_merge() does: 300 one-word findings.
  quiet(gr_register_model("r7-merge-ctx", context_window = 3000L, max_output = 500L,
                          input_usd = 0, output_usd = 0))
  spec <- gr_read_spec("map_reduce", model = "r7-merge-ctx", max_answer_tokens = 300L)
  tr <- gr_trace()
  quiet(readgpt:::tree_merge(gr_mock_client(function(m, p) "Merged answer."),
                             "Did revenue rise in 2023?", rep("Yes.", 300), spec, tr))
  room <- gr_budget("r7-merge-ctx", reserve_output = 300L,
                    overhead = readgpt:::prompt_overhead("Did revenue rise in 2023?",
                                                         readgpt:::.gr_prompts$merge_system,
                                                         "never"))$input
  worst <- readgpt:::merge_tree_worst(300, gr_count_tokens("Yes."), 300, room)
  # Before: one merge, against the several tree_merge() made.
  expect_gte(worst$calls, tr$calls)
})

# ---------------------------------------------------------------------------
# surface-05-summary (optional): gr_compare()'s error column was NA for a
# reader that counts its failures rather than naming one
# ---------------------------------------------------------------------------

test_that("a comparison names the error of a recipe whose reader only counted it", {
  txt <- paste(sprintf("Paragraph %d says revenue rose by %d percent.", 1:4, 1:4),
               collapse = "\n\n")
  cmp <- quiet(gr_compare(txt, "What was revenue?", "thorough", client = mock_dead()))
  # Before: NA beside not_found = TRUE.
  expect_true(cmp$summary$not_found)
  expect_match(cmp$summary$error, "simulated transport failure", fixed = TRUE)

  ok <- quiet(gr_compare(txt, "What was revenue?", "fast",
                         client = gr_mock_client(function(m, p) "It rose.")))
  expect_true(is.na(ok$summary$error))
})
