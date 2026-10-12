# test-effort.R -- reasoning effort, set per stage and sent only where it can go.
#
# Two promises, tested apart. Unset, it changes nothing: not a byte of a
# request, not a key of a cache, a recording or a corpus store, so every run
# made before the setting existed still replays and still restores. Set, it
# reaches every call it covers, resolved against the label the trace records,
# in the field each API reads, and only for a model that takes it.

# A client that answers every prompt the readers here send, and records the
# effort it was given with each call's label.
effort_client <- function() {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("Rate how useful", sys, fixed = TRUE)) return('{"score": 8, "reason": "relevant"}')
    if (grepl("screen", sys, ignore.case = TRUE) && grepl("criteri", sys, ignore.case = TRUE)) {
      return('{"decision":"include","reason":"It fits.","criterion":null,"quote":null}')
    }
    "Revenue was 45.2 million dollars."
  })
}

efforts_by_label <- function(client) {
  calls <- client$calls()
  stats::setNames(vapply(calls, function(x) if (is.null(x$params$effort)) NA_character_
                                            else x$params$effort, character(1)),
                  vapply(calls, function(x) x$label, character(1)))
}

reasoner <- "gpt-5.6-luna"

test_that("without an effort, a request is built, keyed and recorded exactly as before", {
  msgs <- list(list(role = "system", content = "Be brief."),
               list(role = "user", content = "What was revenue?"))
  info <- gr_model_info(reasoner)
  for (api in c("responses", "chat")) {
    cl <- gr_client(api = api, extra_body = list(seed = 7L))
    a <- readgpt:::build_request_body(cl, msgs, reasoner, 2048L, NULL, NULL, "result", info,
                                      list(store = FALSE))
    b <- readgpt:::build_request_body(cl, msgs, reasoner, 2048L, NULL, NULL, "result", info,
                                      list(store = FALSE), effort = NULL)
    expect_identical(a, b)
    expect_false(any(c("reasoning", "reasoning_effort") %in% names(a)))
  }

  # The cache key, against the formula it had before the setting existed. A
  # change here would turn every stored reply into a miss.
  cl <- gr_mock_client()
  as_chr1 <- readgpt:::as_chr1
  old_cache <- readgpt:::gr_hash(c(
    "readgpt-cache-v2", as_chr1(cl$api, "?"), as_chr1(cl$base_url, "?"),
    as_chr1(cl$.client_id, "<url-addressed>"), reasoner, format(100L), "<null>", "result",
    "<no-schema>", readgpt:::gr_hash(list()), readgpt:::gr_hash(list()),
    readgpt:::key_text(c("system", "Be brief.", "user", "What was revenue?"))))
  expect_identical(readgpt:::cache_key(cl, msgs, reasoner, 100L, NULL, NULL, "result", list()),
                   old_cache)
  # And the replay key, which a recording made by 0.5.1 is looked up by.
  old_replay <- readgpt:::gr_hash(c("readgpt-replay-v1", reasoner,
    readgpt:::key_text(c("system", "Be brief.", "user", "What was revenue?"))))
  expect_identical(readgpt:::replay_key(msgs, reasoner), old_replay)
  expect_identical(readgpt:::replay_key(msgs, reasoner, NULL), old_replay)

  # A handler is given the params it always was, and the trace records them.
  seen <- NULL
  cl <- gr_mock_client(function(m, p) { seen <<- p; "ok" })
  tr <- gr_trace()
  gr_call(cl, msgs, model = reasoner, trace = tr)
  expect_identical(names(seen), c("model", "max_output", "temperature", "schema", "schema_name",
                                  "prompt_tokens"))
  expect_false("effort" %in% names(tr$steps[[1]]$params))
  expect_false("effort" %in% names(as.data.frame(tr)))

  # A spec carries no entry for it, so a corpus store's key is unchanged.
  expect_false("effort" %in% names(gr_read_spec()))
  rec <- as_recipe("fast")
  same <- readgpt:::apply_overrides(rec, list(effort = NULL))
  expect_false("effort" %in% names(same$read))
  expect_identical(readgpt:::corpus_key("some text", "Q?", rec, cl),
                   readgpt:::corpus_key("some text", "Q?", same, cl))
})

test_that("an effort goes in the field each API reads, over the client's default", {
  msgs <- list(list(role = "user", content = "What was revenue?"))
  info <- gr_model_info(reasoner)
  body <- function(api, extra_body = list(), extra = list(), effort = "low") {
    readgpt:::build_request_body(gr_client(api = api, extra_body = extra_body), msgs, reasoner,
                                 2048L, NULL, NULL, "result", info, extra, effort = effort)
  }
  # Exact names: `$reasoning` partial-matches `reasoning_effort`.
  field <- function(b, nm) b[[nm, exact = TRUE]]
  expect_identical(field(body("responses"), "reasoning"), list(effort = "low"))
  expect_null(field(body("responses"), "reasoning_effort"))
  expect_identical(field(body("chat"), "reasoning_effort"), "low")
  expect_null(field(body("chat"), "reasoning"))

  # The client's effort is a default: the stage's replaces it, and the rest of
  # the client's `reasoning` object stays.
  b <- body("responses", extra_body = list(reasoning = list(effort = "high", summary = "auto")))
  expect_identical(b$reasoning$effort, "low")
  expect_identical(b$reasoning$summary, "auto")
  expect_identical(body("chat", extra_body = list(reasoning_effort = "high"))$reasoning_effort,
                   "low")
  # A chat client for a gateway that reads a `reasoning` object: its effort is
  # replaced too, not sent beside the stage's, and the rest of it stays. One
  # without an effort is not given one.
  b <- body("chat", extra_body = list(reasoning = list(effort = "high", max_tokens = 100L)))
  expect_identical(field(b, "reasoning"), list(effort = "low", max_tokens = 100L))
  expect_identical(field(b, "reasoning_effort"), "low")
  b <- body("chat", extra_body = list(reasoning = list(max_tokens = 100L)))
  expect_identical(field(b, "reasoning"), list(max_tokens = 100L))
  expect_identical(field(b, "reasoning_effort"), "low")
  # A field one call writes itself is more specific still.
  expect_identical(body("responses", extra = list(reasoning = list(effort = "minimal")))$reasoning,
                   list(effort = "minimal"))
})

test_that("a map is resolved per label: longest name, whole dotted parts, then the default", {
  e <- c(extract = "low", "synthesise" = "medium", "synthesise.section" = "high", "xhigh")
  f <- function(label) readgpt:::effort_for(e, label)
  expect_identical(f("extract.chunk"), "low")
  expect_identical(f("extract"), "low")
  expect_identical(f("synthesise.section"), "high")
  expect_identical(f("synthesise.merge.level2"), "medium")
  # A name is a whole part, not the start of one.
  expect_identical(f("extraction.other"), "xhigh")
  expect_identical(f("map.answer"), "xhigh")
  # "default" is the unnamed element by another name; with neither, a label
  # nothing names is sent nothing.
  expect_identical(readgpt:::effort_for(c(default = "low", reduce = "high"), "reduce.level3"), "high")
  expect_identical(readgpt:::effort_for(c(default = "low", reduce = "high"), "map.answer"), "low")
  expect_null(readgpt:::effort_for(c(reduce = "high"), "map.answer"))
  expect_identical(readgpt:::effort_for("medium", "anything.at.all"), "medium")
  expect_null(readgpt:::effort_for(NULL, "map.answer"))
})

test_that("an effort reaches a reasoning model and never one that would refuse it", {
  seen <- list()
  cl <- gr_mock_client(function(m, p) { seen[[length(seen) + 1L]] <<- p; "ok" })
  tr <- gr_trace()
  gr_call(cl, "hi", model = reasoner, effort = "low", trace = tr)
  gr_call(cl, "hi", model = "gpt-4o", effort = "low", trace = tr)
  gr_call(cl, "hi", effort = "low", trace = tr)                  # the mock's own model
  expect_identical(seen[[1]]$effort, "low")
  expect_null(seen[[2]]$effort)
  expect_null(seen[[3]]$effort)
  # The trace says which request carried one.
  df <- as.data.frame(tr)
  expect_identical(df$effort, c("low", NA, NA))
  expect_identical(tr$steps[[1]]$params$effort, "low")
  expect_null(tr$steps[[2]]$params$effort)

  # A model registered as a reasoning one takes it, whatever it is called.
  local_registries()
  gr_register_model("gateway-thinker", context_window = 64000, max_output = 8000,
                    input_usd = 0, output_usd = 0, reasoning = TRUE,
                    supports_temperature = FALSE)
  gr_call(cl, "hi", model = "gateway-thinker", effort = c(call = "high"))
  expect_identical(seen[[4]]$effort, "high")
})

test_that("a read sends each stage the effort its label resolves to", {
  ch <- quiet(gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 150)))
  cl <- effort_client()
  quiet(gr_read(ch, "What was revenue?", cl,
                gr_read_spec("map_reduce", model = reasoner,
                             effort = c(map = "low", reduce = "high"))))
  e <- efforts_by_label(cl)
  expect_true(all(e[names(e) == "map.answer"] == "low"))
  expect_identical(unname(e[names(e) == "reduce"]), "high")

  # The merge tree's own levels ("reduce.level1") are covered by its name. A
  # merge takes more than one level only when the findings outgrow one
  # request, so a small window and long findings force one; the reads above
  # merge once, and `fan_in` does not shape this tree.
  local_registries()
  gr_register_model("tiny-thinker", context_window = 6000L, max_output = 2000L,
                    input_usd = 0, output_usd = 0, reasoning = TRUE,
                    supports_temperature = FALSE)
  long <- paste(rep("Revenue was 45.2 million dollars in the year under review.", 60),
                collapse = " ")
  cl <- gr_mock_client(function(m, p) long)
  ch2 <- quiet(gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 60)))
  quiet(gr_read(ch2, "What was revenue?", cl,
                gr_read_spec("map_reduce", model = "tiny-thinker", max_answer_tokens = 1500L,
                             effort = c(map = "low", reduce = "high"))))
  e <- efforts_by_label(cl)
  expect_true("reduce.level1" %in% names(e))
  expect_true(all(e[grepl("^reduce", names(e))] == "high"))
  expect_true(all(e[names(e) == "map.answer"] == "low"))

  # rerank's scoring calls and its answer, apart.
  cl <- effort_client()
  quiet(gr_read(ch, "What was revenue?", cl,
                gr_read_spec("rerank", model = reasoner,
                             effort = c("rerank.score" = "low", "high"))))
  e <- efforts_by_label(cl)
  expect_true(all(e[names(e) == "rerank.score"] == "low"))
  expect_identical(unname(e[names(e) == "rerank.answer"]), "high")
})

test_that("calls made in parallel workers resolve the same effort", {
  skip_if_not_installed("future")
  skip_if_not_installed("future.apply")
  # A worker loads readgpt from the library, not from this source tree, so
  # under load_all() it runs whatever version was installed last.
  installed <- tryCatch(system2(file.path(R.home("bin"), "Rscript"),
    c("-e", shQuote("cat('effort' %in% names(formals(readgpt::gr_call)))")),
    stdout = TRUE, stderr = FALSE), error = function(e) "FALSE", warning = function(w) "FALSE")
  skip_if_not(identical(utils::tail(installed, 1L), "TRUE"),
              "the installed readgpt, which workers load, predates `effort`")
  local_registries()
  # Registered in this session only: a worker knows it is a reasoning model
  # because the model registry is sent with the batch.
  gr_register_model("par-thinker", context_window = 128000L, max_output = 16000L,
                    input_usd = 0, output_usd = 0, reasoning = TRUE,
                    supports_temperature = FALSE)
  gr_options(workers = 2)
  ch <- quiet(gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 150)))
  cl <- gr_mock_client(function(m, p) sprintf("pid %d", Sys.getpid()))
  quiet(gr_read(ch, "What was revenue?", cl,
                gr_read_spec("map_reduce", model = "par-thinker", parallel = TRUE,
                             effort = c(map = "low", reduce = "high"))))
  e <- efforts_by_label(cl)
  expect_true(all(e[names(e) == "map.answer"] == "low"))
  expect_true(all(e[grepl("^reduce", names(e))] == "high"))
  # Made in workers, or the test proves nothing about them.
  replies <- vapply(cl$calls(), function(x) x$result$text, character(1))
  expect_true(any(replies[names(e) == "map.answer"] != sprintf("pid %d", Sys.getpid())))
})

test_that("effort passes through answer_document() and gr_read_many(), into the store key", {
  f <- readgpt_example()
  cl <- effort_client()
  ans <- quiet(answer_document(f, "What was revenue?", "fast", client = cl, model = reasoner,
                               effort = "low"))
  expect_true(all(efforts_by_label(cl) == "low"))
  # The setting is on the run's record, with the read's other settings.
  pre <- Filter(function(st) identical(st$label, "preflight"), ans$trace$steps)[[1]]
  expect_identical(pre$detail$settings$effort, "low")

  rec <- as_recipe("fast")
  k0 <- readgpt:::corpus_key(f, "Q?", rec, cl)
  k1 <- readgpt:::corpus_key(f, "Q?", readgpt:::apply_overrides(rec, list(effort = "low")), cl)
  k2 <- readgpt:::corpus_key(f, "Q?", readgpt:::apply_overrides(rec, list(effort = "high")), cl)
  expect_false(k0 == k1)
  expect_false(k1 == k2)

  # A rerun at another effort is not restored from the store.
  store <- tempfile("store")
  cl <- effort_client()
  quiet(gr_read_many(f, "What was revenue?", "fast", client = cl, store = store,
                     model = reasoner, effort = "low"))
  n1 <- length(cl$calls())
  quiet(gr_read_many(f, "What was revenue?", "fast", client = cl, store = store,
                     model = reasoner, effort = "low"))
  expect_identical(length(cl$calls()), n1)                 # restored
  quiet(gr_read_many(f, "What was revenue?", "fast", client = cl, store = store,
                     model = reasoner, effort = "high"))
  expect_gt(length(cl$calls()), n1)                         # read again
})

test_that("gr_compare() keeps recipes apart that differ only in effort, and says so", {
  cl <- effort_client()
  low <- gr_recipe("low", segment = list(method = "paragraph", max_tokens = 150),
                   read = gr_read_spec("stuff", model = reasoner, effort = "low"))
  high <- gr_recipe("high", segment = list(method = "paragraph", max_tokens = 150),
                    read = gr_read_spec("stuff", model = reasoner, effort = "high"))
  cmp <- quiet(gr_compare(readgpt_example(), "What was revenue?", list(low = low, high = high),
                          client = cl))
  expect_setequal(unname(efforts_by_label(cl)), c("low", "high"))
  expect_match(cmp$summary$settings[cmp$summary$recipe == "low"], "effort=low", fixed = TRUE)
  expect_match(cmp$summary$settings[cmp$summary$recipe == "high"], "effort=high", fixed = TRUE)
  # A map prints with its stages.
  expect_identical(readgpt:::format_settings(list(effort = c(map = "low", "high"))),
                   "effort=map:low+high")
})

test_that("the segmenters that call a model are sent the run's effort", {
  # A segmenter asks the client's model, so the client's model is the
  # reasoning one here.
  cl <- gr_backend_client(function(m, p) "An excerpt about revenue.", model = reasoner)
  quiet(answer_document(readgpt_example(), "What was revenue?",
                        gr_recipe("ctx", segment = gr_segment_spec("contextual", max_tokens = 200,
                                                                   context_source = "llm"),
                                  read = "stuff"),
                        client = cl, effort = c(segment = "minimal", default = "high")))
  e <- efforts_by_label(cl)
  expect_true(any(names(e) == "segment.context"))
  expect_true(all(e[names(e) == "segment.context"] == "minimal"))
  expect_identical(unname(e[names(e) == "stuff.answer"]), "high")
})

test_that("gr_compare() shares chunks between recipes whose segmenter is sent the same effort", {
  ctx <- gr_segment_spec("contextual", max_tokens = 200, context_source = "llm")
  pair <- function(seg, ea, eb) {
    list(a = gr_recipe("a", segment = seg, read = gr_read_spec("stuff", effort = ea)),
         b = gr_recipe("b", segment = seg, read = gr_read_spec("stuff", effort = eb)))
  }
  context_calls <- function(ea, eb) {
    cl <- gr_backend_client(function(m, p) "An excerpt about revenue.", model = reasoner)
    quiet(gr_compare(readgpt_example(), "What was revenue?", pair(ctx, ea, eb), client = cl))
    e <- efforts_by_label(cl)
    e[names(e) == "segment.context"]
  }
  cl <- gr_backend_client(function(m, p) "An excerpt about revenue.", model = reasoner)
  quiet(answer_document(readgpt_example(), "What was revenue?",
                        gr_recipe("one", segment = ctx, read = "stuff"), client = cl))
  n <- sum(names(efforts_by_label(cl)) == "segment.context")
  expect_gt(n, 0L)

  # Efforts that differ only for the reading stage: the context calls are the
  # same requests, made once. They were made once per recipe.
  e <- context_calls(c("stuff.answer" = "low"), c("stuff.answer" = "high"))
  expect_length(e, n)
  expect_true(all(is.na(e)))
  e <- context_calls(c(segment = "low", stuff = "low"), c("segment.context" = "low", "high"))
  expect_length(e, n)
  expect_true(all(e == "low"))
  # Efforts the segmenter is sent differently still cut apart.
  e <- context_calls("low", "high")
  expect_length(e, 2L * n)
  expect_identical(as.vector(table(e)[c("high", "low")]), c(n, n))

  # A segmenter that sends no request takes no effort: the semantic one embeds
  # the document once for both.
  embedded <- 0L
  cl <- gr_backend_client(function(m, p) "Revenue was 45.2 million dollars.", model = reasoner,
                          embed = function(texts, params) {
                            embedded <<- embedded + length(texts)
                            cbind(nchar(texts), seq_along(texts), 1)
                          })
  sem <- gr_segment_spec("semantic", max_tokens = 200)
  quiet(answer_document(readgpt_example(), "What was revenue?",
                        gr_recipe("one", segment = sem, read = "stuff"), client = cl))
  once <- embedded
  expect_gt(once, 0L)
  embedded <- 0L
  quiet(gr_compare(readgpt_example(), "What was revenue?", pair(sem, "low", "high"), client = cl))
  expect_identical(embedded, once)
})

test_that("a segment spec carries only the effort its segmenter's requests resolve to", {
  read <- gr_read_spec("stuff", effort = c(segment = "minimal", stuff = "high"))
  for (m in c("fixed", "paragraph", "sentence", "recursive", "structural", "page", "semantic")) {
    seg <- gr_segment_spec(m)
    expect_identical(readgpt:::segment_effort(seg, read), seg)
  }
  seg <- gr_segment_spec("contextual")                     # the metadata blurb: no request
  expect_identical(readgpt:::segment_effort(seg, read), seg)
  seg <- gr_segment_spec("contextual", context_source = "llm")
  expect_identical(readgpt:::segment_effort(seg, read)$effort, "minimal")
  seg <- gr_segment_spec("proposition")
  expect_identical(readgpt:::segment_effort(seg, read)$effort, "minimal")
  # Nothing covers the segmenter's label: nothing is added.
  expect_identical(readgpt:::segment_effort(seg, gr_read_spec("stuff", effort = c(stuff = "low"))),
                   seg)
  # Unset, the spec is what it always was.
  expect_identical(readgpt:::segment_effort(seg, gr_read_spec("stuff")), seg)

  # A segmenter of the user's, or another function under a built-in's name,
  # is given the whole map, since the labels its calls carry are its own.
  local_registries()
  mine <- function(doc, spec, client, trace) new_chunks(doc$text, "mine", spec)
  gr_register_segmenter("mine", mine)
  expect_identical(readgpt:::segment_effort(gr_segment_spec("mine"), read)$effort, read$effort)
  gr_register_segmenter("paragraph", mine)
  expect_identical(readgpt:::segment_effort(gr_segment_spec("paragraph"), read)$effort,
                   read$effort)
})

test_that("gr_screen() and gr_extract() take it through `...`", {
  f <- tempfile(fileext = ".txt")
  writeLines("We randomly assigned 1,204 participants to two groups.", f)
  cl <- effort_client()
  quiet(gr_screen(f, question = "Does it work?", include = "Reports a randomised comparison",
                  client = cl, model = reasoner, effort = c(screen = "low")))
  expect_identical(unname(efforts_by_label(cl)), "low")

  cl <- gr_mock_client(function(m, p) '{"n":1204,"n__quote":"We randomly assigned 1,204 participants to two groups."}')
  quiet(gr_extract(f, gr_fields(n = gr_field("Participants", type = "integer")), client = cl,
                   model = reasoner, effort = c("extract.chunk" = "minimal")))
  e <- efforts_by_label(cl)
  expect_true(all(e[names(e) == "extract.chunk"] == "minimal"))
})

test_that("gr_claims(), gr_outline() and gr_synthesise() take it at the end of their arguments", {
  for (fn in list(gr_claims, gr_synthesise)) {
    expect_identical(utils::tail(names(formals(fn)), 1L), "effort")
  }
  # gr_outline() gained `alternatives` after it; both are past every argument
  # an existing call can reach by position.
  expect_identical(utils::tail(names(formals(gr_outline)), 2L), c("effort", "alternatives"))
  tab <- data.frame(
    document = c("a.pdf", "b.pdf"), status = "ok", duplicate_of = NA_character_,
    n_filled = 2L, n_unverified = 0L, conflicts = NA_character_,
    design = c("randomised trial", "cross-sectional"),
    finding = c("supports", "contradicts"), stringsAsFactors = FALSE)
  cl <- gr_mock_client(function(messages, params) {
    seen <- paste(vapply(messages, function(m) as.character(m$content), character(1)),
                  collapse = " ")
    if (grepl("<claims>", seen, fixed = TRUE)) {
      return('{"sections":[{"heading":"Findings","brief":"What was found","claims":[1,2],"rationale":"All of it."}]}')
    }
    if (grepl("<studies>", seen, fixed = TRUE) && grepl("claim", messages[[1]]$content)) {
      return(paste0('{"claims":[{"claim":"The effect appears in trials but not in surveys.",',
                    '"kind":"finding","supported_by":[1],"contradicted_by":[2],',
                    '"moderator":"design","scope":"one trial, one survey"},',
                    '{"claim":"Both studies measured it the same way.","kind":"measurement",',
                    '"supported_by":[1,2],"contradicted_by":[],"moderator":null,"scope":null}]}'))
    }
    "One trial found it [study 1] and one survey did not [study 2]."
  })
  cm <- quiet(gr_claims(tab, question = "Does it work?", client = cl, model = reasoner,
                        effort = c(claims = "high")))
  expect_true(nrow(cm$claims) > 0L)
  quiet(gr_outline(cm, client = cl, model = reasoner, effort = "medium"))
  # The further outlines are calls of their own, and a name of
  # "outline.derive" covers them: asked without the effort, they went out at
  # the provider's default while the first went out at the one set.
  quiet(gr_outline(cm, client = cl, model = reasoner, alternatives = 2L,
                   effort = c(outline.derive = "low")))
  quiet(gr_synthesise(tab, question = "Does it work?", outline = c(Findings = "What was found"),
                      client = cl, model = reasoner,
                      effort = c(synthesise = "low", "synthesise.section" = "xhigh")))
  e <- efforts_by_label(cl)
  expect_identical(unname(e[names(e) == "claims.draw"]), "high")
  expect_identical(unname(e[names(e) == "outline.derive"]), c("medium", "low"))
  expect_identical(unname(e[names(e) == "outline.derive.alt2"]), "low")
  expect_true(all(e[names(e) == "synthesise.section"] == "xhigh"))
})

test_that("a cache keeps replies at different efforts apart", {
  dir <- tempfile("cache")
  cl <- gr_cache_client(gr_mock_client(function(m, p) paste("at", p$effort)), gr_cache(dir))
  a <- gr_call(cl, "hi", model = reasoner, effort = "low")
  b <- gr_call(cl, "hi", model = reasoner, effort = "high")
  c2 <- gr_call(cl, "hi", model = reasoner, effort = "low")
  expect_false(isTRUE(b$cached))
  expect_identical(b$text, "at high")
  expect_true(c2$cached)
  expect_identical(c2$text, "at low")
})

test_that("a replay matches on the effort it was recorded with", {
  doc <- readgpt_example()
  cl <- effort_client()
  rec <- gr_recipe("r", segment = list(method = "paragraph", max_tokens = 150),
                   read = gr_read_spec("map_reduce", model = reasoner))
  ans <- quiet(answer_document(doc, "What was revenue?", rec, client = cl, effort = "low"))
  f <- tempfile(fileext = ".json")
  gr_trace_save(ans$trace, f)

  # The same effort replays exactly, from the trace and from the file.
  for (src in list(ans$trace, f)) {
    rp <- gr_replay_client(src)
    again <- quiet(answer_document(doc, "What was revenue?", rec, client = rp, effort = "low"))
    expect_identical(again$answer, ans$answer)
    expect_identical(length(rp$missed()), 0L)
  }
  # Another effort, or none, is a different run, and the miss says why.
  rp <- gr_replay_client(f)
  expect_error(quiet(answer_document(doc, "What was revenue?", rec, client = rp, effort = "high")),
               "recorded for this model at reasoning effort 'low'", class = "gr_replay_miss")
  rp <- gr_replay_client(f)
  expect_error(quiet(answer_document(doc, "What was revenue?", rec, client = rp)),
               "asked for no effort", class = "gr_replay_miss")
})

test_that("a replay does not need the recording session's reasoning-model registration", {
  f <- tempfile(fileext = ".json")
  handler <- function(m, p) paste("effort", p$effort %||% "none")
  local({
    local_registries()
    gr_register_model("gw-thinker", context_window = 128000L, max_output = 16000L,
                      input_usd = 0, output_usd = 0, reasoning = TRUE,
                      supports_temperature = FALSE)
    tr <- gr_trace()
    gr_call(gr_backend_client(handler, model = "gw-thinker"), "What was revenue?",
            effort = "low", trace = tr)
    gr_trace_save(tr, f)
  })
  # This session would send the model no effort: it does not know it reasons.
  expect_false(isTRUE(quiet(gr_model_info("gw-thinker"))$reasoning))
  rp <- gr_replay_client(f)
  expect_identical(quiet(gr_call(rp, "What was revenue?", effort = "low"))$text, "effort low")
  expect_length(rp$missed(), 0L)
  # Another effort, or none, still misses, and the message says which the call
  # asked for. It said "no effort" for every one of them.
  expect_error(quiet(gr_call(gr_replay_client(f), "What was revenue?", effort = "high")),
               "asked for effort 'high'", class = "gr_replay_miss")
  expect_error(quiet(gr_call(gr_replay_client(f), "What was revenue?")),
               "asked for no effort", class = "gr_replay_miss")
})

test_that("a replay looks up the recording's other models at the effort each is sent", {
  ch <- quiet(gr_segment(readgpt_example(), list(method = "paragraph", max_tokens = 150)))
  sp <- gr_read_spec("stuff", effort = "low")
  # A reasoning model and one that is sent no effort, each as the replay
  # client's own model and as the other one.
  for (first in c(reasoner, "gpt-4o")) {
    other <- setdiff(c(reasoner, "gpt-4o"), first)
    tr <- gr_trace()
    a <- gr_backend_client(function(m, p) paste("From", first), model = first)
    b <- gr_backend_client(function(m, p) paste("From", other), model = other)
    quiet(gr_read(ch, "What was revenue?", a, sp, trace = tr))
    quiet(gr_read(ch, "What else was reported?", a, sp, trace = tr))
    quiet(gr_read(ch, "Who audited it?", b, sp, trace = tr))
    rp <- gr_replay_client(tr)
    expect_identical(rp$model, first)
    expect_identical(quiet(gr_read(ch, "Who audited it?", rp, sp))$answer, paste("From", other))
    expect_identical(quiet(gr_read(ch, "What was revenue?", rp, sp))$answer, paste("From", first))
    expect_length(rp$missed(), 0L)
    # At another effort, the reasoning model's read misses; the other model's
    # requests carried no effort, so its read is the same requests.
    rp <- gr_replay_client(tr)
    high <- gr_read_spec("stuff", effort = "high")
    if (identical(other, reasoner)) {
      expect_error(quiet(gr_read(ch, "Who audited it?", rp, high)), class = "gr_replay_miss")
    } else {
      expect_identical(quiet(gr_read(ch, "Who audited it?", rp, high))$answer, paste("From", other))
      expect_error(quiet(gr_read(ch, "What was revenue?", rp, high)), class = "gr_replay_miss")
    }
  }
})

test_that("a call through an ellmer chat is not recorded as sent at the effort it asked for", {
  local_registries()
  gr_register_model("stub-thinker", context_window = 128000L, max_output = 4096L,
                    input_usd = 0, output_usd = 0, reasoning = TRUE,
                    supports_temperature = FALSE)
  # gr_ellmer_client() is a backend client of this class around an ellmer chat
  # (see its last lines). Built by hand, so this runs without ellmer; the
  # adapter itself is tested below when ellmer is installed.
  seen <- character(0)
  cl <- gr_backend_client(function(m, p) {
    seen <<- c(seen, p$effort %||% "none")
    "an answer"
  }, model = "stub-thinker")
  class(cl) <- c("gr_ellmer_client", class(cl))
  tr <- gr_trace()
  gr_call(cl, "hi", effort = "low", trace = tr)
  gr_call(cl, "again", effort = "high", trace = tr)
  # The adapter is still handed it, to say it is not sent.
  expect_identical(seen, c("low", "high"))
  # The trace and the audit do not count it as sent, and keep what was asked.
  expect_false("effort" %in% names(as.data.frame(tr)))
  expect_identical(readgpt:::trace_effort_text(tr), NA_character_)
  expect_null(tr$steps[[1]]$params$effort)
  expect_identical(tr$steps[[1]]$params$unsent_effort, "low")

  # Its replay asks for what the call asked for, and is traced the same way.
  f <- tempfile(fileext = ".json")
  gr_trace_save(tr, f)
  rp <- gr_replay_client(f)
  tr2 <- gr_trace()
  expect_identical(gr_call(rp, "hi", effort = "low", trace = tr2)$text, "an answer")
  expect_false("effort" %in% names(as.data.frame(tr2)))
  expect_identical(tr2$steps[[1]]$params$unsent_effort, "low")
  expect_error(gr_call(gr_replay_client(f), "hi", effort = "high"), class = "gr_replay_miss")
})

test_that("the audit report shows what effort each stage ran at", {
  cl <- effort_client()
  ans <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = cl,
                               model = reasoner, effort = "low"))
  html <- paste(readLines(quiet(gr_audit_report(tempfile(fileext = ".html"), answer = ans,
                                                open = FALSE)), warn = FALSE), collapse = "\n")
  expect_match(html, "low on", fixed = TRUE)
  expect_match(html, ">effort<", fixed = TRUE)
  # Not for a run without it.
  cl <- effort_client()
  ans <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = cl,
                               model = reasoner))
  html <- paste(readLines(quiet(gr_audit_report(tempfile(fileext = ".html"), answer = ans,
                                                open = FALSE)), warn = FALSE), collapse = "\n")
  expect_false(grepl(">effort<", html, fixed = TRUE))
})

test_that("a map that cannot be read is refused; a level that is not known is sent anyway", {
  expect_error(gr_read_spec(effort = NA_character_), class = "gr_bad_setting")
  expect_error(gr_read_spec(effort = 2), class = "gr_bad_setting")
  expect_error(gr_read_spec(effort = " "), class = "gr_bad_setting")
  expect_error(gr_read_spec(effort = c("low", "high")), "more than one value",
               class = "gr_bad_setting")
  expect_error(gr_read_spec(effort = c("low", default = "high")), class = "gr_bad_setting")
  expect_error(gr_read_spec(effort = c(map = "low", map = "high")), "'map'",
               class = "gr_bad_setting")
  expect_error(gr_call(gr_mock_client(), "hi", effort = list("low")), class = "gr_bad_setting")
  expect_error(gr_claims(data.frame(a = 1), question = "Q?", effort = NA_character_),
               class = "gr_bad_setting")

  expect_warning(sp <- gr_read_spec(effort = "hihg"), "but 'high' is", class = "gr_effort_unknown")
  expect_identical(sp$effort, "hihg")
  expect_silent(gr_read_spec(effort = c(map = "minimal", "xhigh", reduce = "none")))
  seen <- NULL
  cl <- gr_mock_client(function(m, p) { seen <<- p$effort; "ok" })
  expect_warning(gr_call(cl, "hi", model = reasoner, effort = "max"), class = "gr_effort_unknown")
  expect_identical(seen, "max")
})

test_that("an ellmer chat is not sent an effort, and says so once", {
  skip_if_not_installed("ellmer")
  local_registries()
  gr_register_model("stub-thinker", context_window = 128000L, max_output = 4096L,
                    input_usd = 0, output_usd = 0, reasoning = TRUE,
                    supports_temperature = FALSE)
  make <- function() {
    self <- new.env(parent = emptyenv())
    self$chat <- function(user, echo = "none") "an answer"
    self$chat_structured <- function(user, type = NULL, echo = "none") list(answer = "an answer")
    self$clone <- function(deep = FALSE) make()
    self$set_turns <- function(value) invisible(self)
    self$set_system_prompt <- function(value) invisible(self)
    self$get_model <- function() "stub-thinker"
    self$get_tokens <- function() data.frame()
    self$last_turn <- function(role = "assistant") NULL
    self
  }
  cl <- gr_ellmer_client(make())
  quiet_cap <- function(expr) withCallingHandlers(expr, gr_ellmer_max_output = function(w)
    invokeRestart("muffleWarning"))
  tr <- gr_trace()
  expect_warning(quiet_cap(gr_call(cl, "hi", effort = "low", trace = tr)),
                 class = "gr_effort_unsupported")
  expect_no_warning(quiet_cap(gr_call(cl, "again", effort = "low", trace = tr)))
  # And the trace does not say it was sent.
  expect_false("effort" %in% names(as.data.frame(tr)))
})

test_that("the help's custom reader and suggested profile do what the help says", {
  man <- test_path("..", "..", "man")
  skip_if_not(dir.exists(man), "the help sources are not beside the tests")
  # The gr_register_reader() example, run as written: its reader passes the
  # spec's effort on. It sent none.
  local_registries()
  ex <- tempfile(fileext = ".R")
  tools::Rd2ex(file.path(man, "gr_register_reader.Rd"), ex)
  utils::capture.output(quiet(source(ex, local = new.env())))
  cl <- effort_client()
  quiet(gr_read(quiet(gr_segment(readgpt_example(), list(method = "sentence", max_tokens = 120))),
                "What was revenue?", cl, gr_read_spec("longest", model = reasoner, effort = "low")))
  expect_identical(unname(efforts_by_label(cl)), "low")

  # The gr_read_spec() profile lowers the mechanical calls and none of the
  # requests that answer the question. `skim = "low"` lowered skim's answer.
  rd <- paste(readLines(file.path(man, "gr_read_spec.Rd"), warn = FALSE), collapse = "\n")
  code <- regmatches(rd, regexpr("effort = c\\(\"extract\\.chunk\"[^)]*\\)", rd))
  expect_length(code, 1L)
  profile <- eval(parse(text = sub("^effort = ", "", code)))
  f <- function(label) readgpt:::effort_for(profile, label) %||% "default"
  for (l in c("skim.extract", "extract.chunk", "rerank.score", "segment.context", "map.answer")) {
    expect_identical(f(l), "low")
  }
  for (l in c("skim.consolidate", "skim.answer", "stuff.answer", "retrieve.answer",
              "rerank.answer", "preview.answer", "hier.answer", "iterative.final", "reduce")) {
    expect_identical(f(l), "default")
  }
})
