# test-premise.R
#
# `premise = TRUE`: a question the document contradicts gets a third outcome,
# a correction, instead of an answer on its own terms or NOT_IN_DOCUMENT. These
# tests use a mock that behaves like a model given the rule: it replies with
# the sentinel, decorated the ways models decorate it, only when the rule is in
# its system prompt and the excerpts in front of it say the contrary.

Q <- "Why did revenue fall in 2023?"

premise_fixture <- function() {
  new_chunks(c("Revenue rose 12% in 2023, to 45.2 million.",
               "Costs were flat at 30 million across the year.",
               "The board expects growth to continue next year."),
             "fixture", gr_segment_spec(max_tokens = 400))
}

# Routes on the system prompt, as mock_echo() does. `flag` is what an
# answering request that was given the rule and can see the rise replies.
premise_model <- function(flag = "**FALSE_PREMISE:** Revenue rose 12% in 2023, to 45.2 million.",
                          merge = NULL, adjudicate = NULL, revise = NULL, step = NULL,
                          silent = "NOT_IN_DOCUMENT", costs = NULL) {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    rest <- paste(vapply(messages[-1], function(x) as.character(x$content), ""), collapse = "\n")
    ruled <- grepl("FALSE_PREMISE", sys, fixed = TRUE)
    sees_rise <- grepl("Revenue rose", rest, fixed = TRUE)
    if (grepl("Rate how useful", sys, fixed = TRUE)) return('{"score": 8, "reason": "relevant"}')
    if (grepl("You plan how to read a document", sys, fixed = TRUE)) {
      last <- messages[[length(messages)]]$content
      n <- suppressWarnings(as.integer(sub(".*each of the ([0-9]+) sections.*", "\\1", last)))
      return(preview_plan_json(rep("read", if (is.na(n)) 1L else n)))
    }
    if (grepl("extract evidence, not answers", sys, fixed = TRUE)) {
      return(if (sees_rise) "Revenue rose 12% in 2023, to 45.2 million." else "NONE")
    }
    if (grepl("You compress text", sys, fixed = TRUE)) {
      return(sub("(?s).*<text>\n(.*)\n</text>.*", "\\1", rest, perl = TRUE))
    }
    if (grepl("reading iteratively", sys, fixed = TRUE)) {
      return(step %||% '{"can_answer": true, "answer": "Revenue rose.", "next_query": ""}')
    }
    if (grepl("different reading strategies", sys, fixed = TRUE)) return(adjudicate %||% flag)
    if (grepl("You consolidate partial findings", sys, fixed = TRUE)) return(merge %||% flag)
    if (grepl("You revise a draft", sys, fixed = TRUE)) {
      return(revise %||% sub("(?s).*<draft>\n(.*?)\n</draft>.*", "\\1", rest, perl = TRUE))
    }
    if (!is.null(costs) && grepl("Costs were flat", rest, fixed = TRUE) && !sees_rise) return(costs)
    if (sees_rise) return(if (ruled) flag else "Revenue fell because demand weakened.")
    silent
  })
}

sys_of <- function(call) as.character(call$messages[[1]]$content)
body_of <- function(call) paste(vapply(call$messages[-1], function(x) as.character(x$content), ""),
                                collapse = "\n")

readers_answering <- c("stuff", "map_reduce", "refine", "skim", "retrieve", "rerank",
                       "hierarchical", "iterative", "preview", "ensemble")

# --- the detector ------------------------------------------------------------

test_that("the premise sentinel is read through the ways models decorate it", {
  pc <- readgpt:::premise_correction
  rose <- "Revenue rose 12%."
  forms <- c("FALSE_PREMISE: Revenue rose 12%.",
             "**FALSE_PREMISE:** Revenue rose 12%.",
             "\"FALSE_PREMISE\": Revenue rose 12%.",
             "`FALSE_PREMISE`: Revenue rose 12%.",
             "Answer: FALSE_PREMISE - Revenue rose 12%.",
             "**Verdict:** FALSE_PREMISE — Revenue rose 12%.",
             "FALSE_PREMISE\n\nRevenue rose 12%.",
             "**FALSE_PREMISE: Revenue rose 12%.**",
             "false_premise: Revenue rose 12%.",
             "False premise: Revenue rose 12%.",
             "FALSE PREMISE. Revenue rose 12%.",
             "  FALSE_PREMISE:Revenue rose 12%.  ",
             "「FALSE_PREMISE」：Revenue rose 12%.")
  for (f in forms) expect_identical(pc(f), rose, info = f)
  # The sentence keeps its own quote marks and citations.
  expect_identical(pc("FALSE_PREMISE: \"Revenue rose\" [chunk 1]"), "\"Revenue rose\" [chunk 1]")
})

test_that("an answer that merely mentions or resembles the sentinel is not a verdict", {
  pc <- readgpt:::premise_correction
  for (x in c("Revenue rose 12%.", "NOT_IN_DOCUMENT", "",
              "The log said FALSE_PREMISE: revenue rose.",
              "False premise reasoning is common in surveys.",
              "FALSE_PREMISES were tested in the study.",
              "FALSE_PREMISE.txt was attached to the filing.",
              "FALSE_PREMISE-2 was the code name.")) {
    expect_null(pc(x), label = x)
  }
})

test_that("a bare sentinel is a verdict without its sentence, and says so", {
  for (x in c("FALSE_PREMISE", "**FALSE_PREMISE.**", "FALSE_PREMISE:")) {
    r <- readgpt:::premise_reply(x, TRUE)
    expect_identical(r$premise, readgpt:::.gr_premise_unstated, info = x)
    expect_identical(r$text, r$premise)
    expect_false(is_not_found(r$text))
  }
})

test_that("both sentinels at once read as not found, and an unchecked reply is left alone", {
  r <- readgpt:::premise_reply("FALSE_PREMISE: NOT_IN_DOCUMENT.", TRUE)
  expect_identical(r$text, "NOT_IN_DOCUMENT")
  expect_true(is.na(r$premise))
  # Not checked: the text exactly as it came, sentinel and all, and no field.
  r0 <- readgpt:::premise_reply("**FALSE_PREMISE:** Revenue rose.", FALSE)
  expect_identical(r0$text, "**FALSE_PREMISE:** Revenue rose.")
  expect_null(r0$premise)
})

# --- the spec ------------------------------------------------------------------

test_that("the spec carries `premise` only when it is TRUE", {
  expect_false("premise" %in% names(gr_read_spec()))
  expect_false("premise" %in% names(gr_read_spec(premise = FALSE)))
  s <- gr_read_spec("stuff", premise = TRUE)
  expect_true(s$premise)
  # What was changed from the defaults, for the trace and gr_compare().
  expect_true(readgpt:::read_settings(s)$premise)
  expect_null(readgpt:::read_settings(gr_read_spec("stuff"))$premise)
  # And so the keys a store and a comparison file under are the ones they were.
  expect_identical(readgpt:::gr_hash(unclass(gr_read_spec("stuff", premise = FALSE))),
                   readgpt:::gr_hash(unclass(gr_read_spec("stuff"))))
  expect_false(identical(readgpt:::gr_hash(unclass(s)),
                         readgpt:::gr_hash(unclass(gr_read_spec("stuff")))))
})

test_that("`premise` must be TRUE or FALSE, and is said to do nothing for screen and extract", {
  expect_error(gr_read_spec(premise = "yes"), class = "gr_bad_setting")
  expect_error(gr_read_spec(premise = NA), class = "gr_bad_setting")
  expect_error(gr_read_spec(premise = c(TRUE, FALSE)), class = "gr_bad_setting")
  expect_warning(gr_read_spec("screen", premise = TRUE), class = "gr_read_premise_ignored")
  expect_warning(gr_read_spec("extract", premise = TRUE), class = "gr_read_premise_ignored")
  expect_no_warning(gr_read_spec("stuff", premise = TRUE))
})

# --- the prompts ---------------------------------------------------------------

test_that("the rule is in the string the budget is computed from", {
  for (cite in c(FALSE, TRUE)) {
    base <- readgpt:::answer_system(cite)
    with <- readgpt:::answer_system(cite, premise = TRUE)
    expect_identical(base, if (cite) readgpt:::.gr_prompts$answer_system_cited
                           else readgpt:::.gr_prompts$answer_system)
    expect_true(startsWith(with, base))
    expect_true(grepl("FALSE_PREMISE:", with, fixed = TRUE))
    expect_true(grepl("NOT_IN_DOCUMENT", sub(base, "", with, fixed = TRUE), fixed = TRUE))
    expect_gt(readgpt:::prompt_overhead(Q, with), readgpt:::prompt_overhead(Q, base))
  }
  expect_identical(readgpt:::merge_system_prompt(), readgpt:::.gr_prompts$merge_system)
  expect_identical(readgpt:::refine_system_prompt(), readgpt:::.gr_prompts$refine_system)

  # What `stuff` sends is the string it budgeted with.
  cl <- premise_model()
  quiet(gr_read(premise_fixture(), Q, cl, gr_read_spec("stuff", premise = TRUE, cite = TRUE)))
  expect_identical(sys_of(cl$calls()[[1]]), readgpt:::answer_system(TRUE, premise = TRUE))
})

test_that("without `premise`, no request carries the rule and no answer the field", {
  local_registries()
  ch <- premise_fixture()
  for (r in readers_answering) {
    cl <- premise_model()
    a <- quiet(gr_read(ch, Q, cl, r))
    sys <- vapply(cl$calls(), sys_of, character(1))
    expect_false(any(grepl("FALSE_PREMISE", sys, fixed = TRUE)), info = r)
    expect_false("premise" %in% names(a), info = r)
    expect_false(grepl("\"premise\"", as_json(a), fixed = TRUE), info = r)
    expect_false(any(vapply(a$trace$steps, function(s) identical(s$label, "read.premise"),
                            logical(1))), info = r)
    # The iterative step's schema is the one it always sent.
    sch <- Filter(Negate(is.null), lapply(cl$calls(), function(k) k$params$schema))
    for (s in sch) expect_null(s$properties$false_premise, info = r)
  }
})

test_that("with `premise`, every request that answers carries the rule and no other does", {
  local_registries()
  ch <- premise_fixture()
  answering <- "answer$|^reduce|^refine\\.|^iterative\\.|adjudicate$"
  for (r in readers_answering) {
    cl <- premise_model(step = paste0('{"can_answer": false, "answer": "", ',
                                      '"next_query": "revenue growth", "false_premise": false}'))
    quiet(gr_read(ch, Q, cl, gr_read_spec(r, premise = TRUE, max_rounds = 2)))
    labels <- vapply(cl$calls(), function(k) k$label, character(1))
    ruled <- vapply(cl$calls(), function(k) grepl("FALSE_PREMISE", sys_of(k), fixed = TRUE),
                    logical(1))
    expect_true(any(grepl(answering, labels)), info = r)
    expect_identical(ruled, grepl(answering, labels),
                     info = paste(r, paste(labels, collapse = " ")))
  }
})

# --- each reader ---------------------------------------------------------------

test_that("every reader turns a decorated verdict into the correction, not a not-found", {
  local_registries()
  ch <- premise_fixture()
  decor <- c(stuff = "**FALSE_PREMISE:** Revenue rose 12%.",
             map_reduce = "FALSE PREMISE. Revenue rose 12%.",
             refine = "False premise: Revenue rose 12%.",
             skim = "FALSE_PREMISE\n\nRevenue rose 12%.",
             retrieve = "\"FALSE_PREMISE\": Revenue rose 12%.",
             rerank = "Answer: FALSE_PREMISE - Revenue rose 12%.",
             hierarchical = "**FALSE_PREMISE: Revenue rose 12%.**",
             preview = "「FALSE_PREMISE」：Revenue rose 12%.",
             ensemble = "`FALSE_PREMISE`: Revenue rose 12%.")
  for (r in names(decor)) {
    a <- quiet(gr_read(ch, Q, premise_model(flag = decor[[r]]), gr_read_spec(r, premise = TRUE)))
    expect_identical(a$premise, "Revenue rose 12%.", info = r)
    expect_identical(a$answer, "Revenue rose 12%.", info = r)
    expect_false(is_not_found(a$answer), info = r)
    expect_false(a$partial, info = r)
    st <- Filter(function(s) identical(s$label, "read.premise"), a$trace$steps)
    expect_length(st, 1L)
    expect_true(st[[1]]$detail$flagged, info = r)
    expect_identical(st[[1]]$detail$correction, "Revenue rose 12%.", info = r)
  }
})

test_that("checked and not contradicted is NA, an answer or a not-found as before", {
  local_registries()
  ch <- premise_fixture()
  a <- quiet(gr_read(ch, "What was revenue in 2023?",
                     premise_model(flag = "Revenue was 45.2 million."),
                     gr_read_spec("stuff", premise = TRUE)))
  expect_true("premise" %in% names(a))
  expect_true(is.na(a$premise))
  expect_identical(a$answer, "Revenue was 45.2 million.")
  expect_true(grepl("\"premise\": null", as_json(a), fixed = TRUE))
  # Every chunk silent: not found, and checked.
  m <- quiet(gr_read(ch, "What did the CEO earn?", premise_model(flag = "NOT_IN_DOCUMENT"),
                     gr_read_spec("map_reduce", premise = TRUE)))
  expect_true(is_not_found(m$answer))
  expect_true(is.na(m$premise))
  st <- Filter(function(s) identical(s$label, "read.premise"), m$trace$steps)
  expect_false(st[[1]]$detail$flagged)
})

test_that("map_reduce: a contradicting chunk is a finding, a silent one is dropped", {
  ch <- premise_fixture()
  cl <- premise_model()
  a <- quiet(gr_read(ch, Q, cl, gr_read_spec("map_reduce", premise = TRUE)))
  # One chunk contradicted the question and two were silent: the one finding
  # needs no merge, and is the verdict.
  expect_identical(call_labels(cl), rep("map.answer", 3L))
  expect_identical(a$premise, "Revenue rose 12% in 2023, to 45.2 million.")
  expect_identical(as.integer(a$chunks_used), 1L)
  expect_identical(as.integer(a$notes$premise_chunks), 1L)
  expect_identical(a$notes$answered, 1L)
  # The chunk's own reply, as it came, is the evidence.
  expect_identical(a$evidence$text, "**FALSE_PREMISE:** Revenue rose 12% in 2023, to 45.2 million.")
  expect_true(grepl("\"premise_chunks\": [1]", as_json(a), fixed = TRUE))
})

test_that("map_reduce: the merge weighs a correction against an answer, and may make it", {
  ch <- premise_fixture()
  merged <- "FALSE_PREMISE: Revenue rose 12% in 2023 while costs were flat."
  cl <- premise_model(merge = merged, costs = "Costs were flat at 30 million.")
  a <- quiet(gr_read(ch, Q, cl, gr_read_spec("map_reduce", premise = TRUE)))
  m <- Filter(function(k) identical(k$label, "reduce"), cl$calls())
  expect_length(m, 1L)
  # The merge prompt has the rule; the contradicting chunk reached it as a
  # finding that says what it is, normalised, and the answering chunk as itself.
  expect_identical(sys_of(m[[1]]), readgpt:::merge_system_prompt(TRUE))
  expect_true(grepl("FALSE_PREMISE: Revenue rose 12% in 2023, to 45.2 million.", body_of(m[[1]]),
                    fixed = TRUE))
  expect_false(grepl("**FALSE_PREMISE", body_of(m[[1]]), fixed = TRUE))
  expect_true(grepl("Costs were flat at 30 million.", body_of(m[[1]]), fixed = TRUE))
  expect_identical(a$premise, "Revenue rose 12% in 2023 while costs were flat.")
  expect_setequal(as.integer(a$chunks_used), 1:2)
  expect_identical(as.integer(a$notes$premise_chunks), 1L)

  # A merge that answers instead overrules the chunk; the notes still say which
  # chunk contradicted the question.
  cl2 <- premise_model(merge = "Costs were flat; nothing says revenue fell.",
                       costs = "Costs were flat at 30 million.")
  b <- quiet(gr_read(ch, Q, cl2, gr_read_spec("map_reduce", premise = TRUE)))
  expect_true(is.na(b$premise))
  expect_identical(b$answer, "Costs were flat; nothing says revenue fell.")
  expect_identical(as.integer(b$notes$premise_chunks), 1L)
})

test_that("map_reduce: two-sentinel chunks are silent, and a merge that gave up is no verdict", {
  ch <- premise_fixture()
  a <- quiet(gr_read(ch, Q, premise_model(flag = "FALSE_PREMISE: NOT_IN_DOCUMENT"),
                     gr_read_spec("map_reduce", premise = TRUE)))
  expect_true(is_not_found(a$answer))
  expect_identical(a$notes$reason, "no chunk yielded an answer")

  failing <- gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("You consolidate partial findings", sys, fixed = TRUE)) stop("merge outage")
    excerpt <- messages[[2]]$content
    if (grepl("Revenue rose", excerpt, fixed = TRUE)) return("FALSE_PREMISE: Revenue rose.")
    if (grepl("Costs", excerpt, fixed = TRUE)) return("Costs were flat.")
    "NOT_IN_DOCUMENT"
  })
  b <- quiet(gr_read(ch, Q, failing, gr_read_spec("map_reduce", premise = TRUE)))
  expect_true(b$partial)
  expect_false(b$notes$merge_ok)
  expect_true(is.na(b$premise))
  expect_true(grepl("FALSE_PREMISE: Revenue rose.", b$answer, fixed = TRUE))
  expect_identical(as.integer(b$notes$premise_chunks), 1L)
})

test_that("refine carries the verdict in its draft, and a later excerpt can overturn it", {
  ch <- premise_fixture()
  cl <- premise_model(flag = "FALSE_PREMISE: Revenue rose 12%.")
  a <- quiet(gr_read(ch, Q, cl, gr_read_spec("refine", premise = TRUE)))
  rev <- Filter(function(k) identical(k$label, "refine.revise"), cl$calls())
  expect_length(rev, 2L)
  expect_identical(sys_of(rev[[1]]), readgpt:::refine_system_prompt(TRUE))
  expect_true(grepl("<draft>\nFALSE_PREMISE: Revenue rose 12%.\n</draft>", body_of(rev[[1]]),
                    fixed = TRUE))
  expect_identical(a$premise, "Revenue rose 12%.")

  over <- premise_model(flag = "FALSE_PREMISE: Revenue rose 12%.",
                        revise = "Revenue fell in the fourth quarter, after the rise.")
  b <- quiet(gr_read(ch, Q, over, gr_read_spec("refine", premise = TRUE)))
  expect_true(is.na(b$premise))
  expect_identical(b$answer, "Revenue fell in the fourth quarter, after the rise.")
})

test_that("iterative asks for the verdict in its schema and stops on it", {
  ch <- premise_fixture()
  cl <- premise_model(step = paste0('{"can_answer": false, "answer": "Revenue rose 12%.", ',
                                    '"next_query": "revenue decline", "false_premise": true}'))
  a <- quiet(gr_read(ch, Q, cl, gr_read_spec("iterative", premise = TRUE, max_rounds = 3)))
  steps <- Filter(function(k) identical(k$label, "iterative.step"), cl$calls())
  expect_length(steps, 1L)
  sch <- steps[[1]]$params$schema
  expect_identical(sch$properties$false_premise, list(type = "boolean"))
  expect_true("false_premise" %in% unlist(sch$required))
  expect_true(grepl("set false_premise true", sys_of(steps[[1]]), fixed = TRUE))
  expect_identical(a$premise, "Revenue rose 12%.")
  expect_identical(a$notes$stop_reason, "premise contradicted")
  expect_false(a$partial)

  # The sentinel inside `answer` counts as well, and the field with no
  # sentence is a verdict without one.
  b <- quiet(gr_read(ch, Q, premise_model(step = paste0(
    '{"can_answer": true, "answer": "**FALSE_PREMISE:** Revenue rose.", ',
    '"next_query": "", "false_premise": false}')),
    gr_read_spec("iterative", premise = TRUE)))
  expect_identical(b$premise, "Revenue rose.")
  c2 <- quiet(gr_read(ch, Q, premise_model(step = paste0(
    '{"can_answer": false, "answer": "", "next_query": "x", "false_premise": true}')),
    gr_read_spec("iterative", premise = TRUE)))
  expect_identical(c2$premise, readgpt:::.gr_premise_unstated)
  # The flag with the not-found sentinel as its answer is no verdict: the step
  # is read as can_answer says, and the loop goes on to its final answer.
  cl3 <- premise_model(step = paste0('{"can_answer": false, "answer": "NOT_IN_DOCUMENT", ',
                                     '"next_query": "revenue decline", "false_premise": true}'))
  e <- quiet(gr_read(ch, Q, cl3, gr_read_spec("iterative", premise = TRUE, max_rounds = 2)))
  # The first round gathered all three chunks, so the second found none new.
  expect_identical(sum(call_labels(cl3) == "iterative.step"), 1L)
  expect_identical(e$notes$stop_reason, "no unseen chunks")
  expect_identical(utils::tail(call_labels(cl3), 1L), "iterative.final")
  expect_identical(e$premise, "Revenue rose 12% in 2023, to 45.2 million.")
  # Without the check the field is not asked for, and is ignored if sent.
  d <- quiet(gr_read(ch, Q, premise_model(step = paste0(
    '{"can_answer": true, "answer": "Revenue rose.", "next_query": "", "false_premise": true}')),
    gr_read_spec("iterative")))
  expect_false("premise" %in% names(d))
  expect_identical(d$notes$stop_reason, "model satisfied")
})

test_that("ensemble adjudicates its members' corrections as corrections", {
  local_registries()
  ch <- premise_fixture()
  cl <- premise_model(flag = "FALSE_PREMISE: Revenue rose 12%.",
                      adjudicate = "FALSE_PREMISE: Both strategies found that revenue rose 12%.")
  a <- quiet(gr_read(ch, Q, cl, gr_read_spec("ensemble", members = c("stuff", "map_reduce"),
                                             premise = TRUE)))
  adj <- Filter(function(k) identical(k$label, "ensemble.adjudicate"), cl$calls())
  expect_length(adj, 1L)
  expect_true(startsWith(sys_of(adj[[1]]), readgpt:::merge_system_prompt(TRUE)))
  body <- body_of(adj[[1]])
  expect_identical(lengths(regmatches(body, gregexpr("FALSE_PREMISE: Revenue rose 12%.", body,
                                                     fixed = TRUE))), 2L)
  expect_identical(a$premise, "Both strategies found that revenue rose 12%.")
  expect_setequal(a$notes$premise_members, c("stuff", "map_reduce"))
  expect_true(grepl("\"premise_members\": [", as_json(a), fixed = TRUE))
})

test_that("screen and extract send and return what they always did", {
  local_registries()
  ch <- premise_fixture()
  run <- function(spec) {
    cl <- gr_mock_client(function(messages, params)
      '{"decision":"include","reason":"Fine.","criterion":null,"quote":null}')
    a <- quiet(gr_read(ch, "Q?", cl, spec))
    list(a = a, sent = lapply(cl$calls(), function(k) k$messages))
  }
  s0 <- run(list(reader = "screen", include = "Anything at all"))
  s1 <- run(list(reader = "screen", include = "Anything at all", premise = TRUE))
  expect_identical(s1$sent, s0$sent)
  expect_false("premise" %in% names(s1$a))
  expect_identical(s1$a$answer, s0$a$answer)

  f <- gr_fields(revenue = "Revenue in 2023")
  ex <- function(spec) {
    cl <- gr_mock_client(function(messages, params) '{"revenue":"45.2 million"}')
    a <- quiet(gr_read(ch, "extract", cl, spec))
    list(a = a, sent = lapply(cl$calls(), function(k) k$messages))
  }
  e0 <- ex(gr_read_spec("extract", fields = f))
  e1 <- quiet(ex(gr_read_spec("extract", fields = f, premise = TRUE)))
  expect_identical(e1$sent, e0$sent)
  expect_false("premise" %in% names(e1$a))
})

# --- the object and what shows it -----------------------------------------------

test_that("new_answer() leaves the field out unless a reader passes it", {
  tr <- gr_trace()
  expect_false("premise" %in% names(new_answer("x", "r", "q", 1L, tr)))
  expect_true(is.na(new_answer("x", "r", "q", 1L, tr, premise = NA)$premise))
  expect_true(is.na(new_answer("x", "r", "q", 1L, tr, premise = "  ")$premise))
  expect_identical(new_answer("Revenue rose.", "r", "q", 1L, tr, premise = "Revenue rose.")$premise,
                   "Revenue rose.")
})

test_that("print and the audit report say the model reported the premise false", {
  local_registries()
  ans <- quiet(answer_document("Revenue rose 12% in 2023, to 45.2 million, the report says.", Q,
                               "fast", client = premise_model(), premise = TRUE))
  expect_identical(ans$premise, "Revenue rose 12% in 2023, to 45.2 million.")
  out <- capture.output(print(ans))
  expect_true(any(grepl("The model reports that the document contradicts an assumption",
                        out, fixed = TRUE)))
  expect_true(any(grepl("Revenue rose 12% in 2023, to 45.2 million.", out, fixed = TRUE)))
  # The configuration is in the trace with the outcome.
  pre <- Filter(function(s) identical(s$label, "preflight"), ans$trace$steps)[[1]]
  expect_true(pre$detail$settings$premise)

  page <- quiet(gr_audit_report(tempfile(fileext = ".html"), answer = ans, open = FALSE))
  html <- paste(readLines(page, warn = FALSE), collapse = "\n")
  expect_true(grepl("The model reports that the document contradicts an assumption", html,
                    fixed = TRUE))
  expect_true(grepl("<th>Premise</th>", html, fixed = TRUE))

  # A page about an answer that was not checked has no such row.
  plain <- quiet(answer_document("Revenue rose 12% in 2023.", "What was revenue?", "fast",
                                 client = premise_model(flag = "45.2 million")))
  page2 <- quiet(gr_audit_report(tempfile(fileext = ".html"), answer = plain, open = FALSE))
  expect_false(grepl("<th>Premise</th>", paste(readLines(page2, warn = FALSE), collapse = "\n"),
                     fixed = TRUE))
})

test_that("gr_compare() adds a premise column only when a recipe checks it", {
  local_registries()
  doc <- "Revenue rose 12% in 2023, to 45.2 million. Costs were flat at 30 million."
  recs <- list(checked = gr_recipe("checked", read = list(reader = "stuff", premise = TRUE)),
               plain = gr_recipe("plain", read = list(reader = "stuff")))
  cmp <- quiet(gr_compare(doc, Q, recs, client = premise_model()))
  expect_identical(utils::tail(names(cmp$summary), 1L), "premise")
  expect_identical(cmp$summary$premise, c("Revenue rose 12% in 2023, to 45.2 million.", NA))
  expect_identical(cmp$summary$not_found, c(FALSE, FALSE))
  expect_true(grepl("premise=TRUE", cmp$summary$settings[1], fixed = TRUE))

  cmp0 <- quiet(gr_compare(doc, Q, list(plain = recs$plain), client = premise_model()))
  expect_false("premise" %in% names(cmp0$summary))
})

test_that("gr_read_many() adds a premise column to every row, restored rows included", {
  local_registries()
  local_clean_cache()
  dir <- withr::local_tempdir()
  f1 <- file.path(dir, "a.txt"); writeLines("Revenue rose 12% in 2023, to 45.2 million.", f1)
  f2 <- file.path(dir, "b.txt"); writeLines("Costs were flat at 30 million across the year.", f2)
  store <- file.path(dir, "store")
  srcs <- c(f1, f2, file.path(dir, "missing.txt"))
  # One client throughout: a closure-backed client's id is part of the store
  # key, so only the recipe differs between the runs below.
  cl <- premise_model()
  out <- quiet(gr_read_many(srcs, Q, "fast", client = cl, store = store, premise = TRUE))
  s <- out$summary
  expect_identical(utils::tail(names(s), 1L), "premise")
  expect_identical(s$premise, c("Revenue rose 12% in 2023, to 45.2 million.", NA, NA))
  expect_identical(s$not_found, c(FALSE, TRUE, NA))
  expect_identical(s$status, c("ok", "ok", "failed"))
  expect_true(any(grepl("1 where the model reports the document contradicts",
                        capture.output(print(out)), fixed = TRUE)))

  again <- quiet(gr_read_many(srcs[1:2], Q, "fast", client = cl, store = store, premise = TRUE))
  expect_identical(again$summary$status, c("restored", "restored"))
  expect_identical(again$summary$premise, s$premise[1:2])

  # Not asked: no column, and nothing restored from the checked run's store.
  plain <- quiet(gr_read_many(srcs[1:2], Q, "fast", client = cl, store = store))
  expect_false("premise" %in% names(plain$summary))
  expect_identical(plain$summary$status, c("ok", "ok"))

  # The report's table says what the answer cell holds.
  page <- quiet(gr_audit_report(tempfile(fileext = ".html"), answer = out, open = FALSE))
  html <- paste(readLines(page, warn = FALSE), collapse = "\n")
  expect_true(grepl("premise contradicted, per the model: Revenue rose 12%", html, fixed = TRUE))
})
