# test-review-cache.R -- regressions for the cache, replay, embedding and JSON
# defects found in review. Each block names the finding it pins.

# ---------------------------------------------------------------------------
# cache-trace-01: gr_cache_clear() and gr_cache_stats() touch only cache entries
# ---------------------------------------------------------------------------

test_that("gr_cache_clear() removes cache entries and leaves every other file alone", {
  local_registries()
  dir <- withr::local_tempdir()
  cl <- gr_cache_client(gr_mock_client(function(m, p) "ok"), gr_cache(dir))
  invisible(gr_call(cl, "q"))
  entry <- list.files(dir, pattern = "\\.rds$", recursive = TRUE)
  expect_length(entry, 1L)

  # What a project folder used as a cache directory also holds.
  dir.create(file.path(dir, "models"))
  dir.create(file.path(dir, "ab"))
  saveRDS(1, file.path(dir, "analysis_results.rds"))
  saveRDS(2, file.path(dir, "models", "fit.rds"))
  saveRDS(3, file.path(dir, "0123456789abcdef.rds"))        # a flat store entry
  saveRDS(4, file.path(dir, "ab", "notes.rds"))             # a shard name, not a key
  saveRDS(5, file.path(dir, "ab", "cd23456789abcdef.rds"))  # a key in the wrong shard
  others <- setdiff(list.files(dir, recursive = TRUE), entry)

  cache <- gr_cache(dir)
  expect_identical(gr_cache_stats(cache)$entries, 1L)
  expect_identical(gr_cache_clear(cache), 1L)
  expect_identical(sort(list.files(dir, recursive = TRUE)), sort(others))
  expect_identical(gr_cache_stats(cache)$entries, 0L)
})

test_that("a gr_read_many() store kept in the cache directory survives a clear", {
  local_registries()
  local_clean_cache()
  proj <- withr::local_tempdir()
  saveRDS(data.frame(x = 1:3), file.path(proj, "analysis_results.rds"))
  cache <- gr_cache(proj)
  cl <- gr_cache_client(gr_mock_client(function(m, p) "Revenue was 10 million."), cache)
  doc <- "The company reported revenue of 10 million dollars in 2023. Costs were 4 million."
  quiet(gr_read_many(doc, "What was revenue?", "fast", client = cl, store = proj))
  before <- list.files(proj, recursive = TRUE)
  store <- before[!grepl("/", before, fixed = TRUE) & before != "analysis_results.rds"]
  expect_length(store, 1L)

  expect_identical(gr_cache_stats(cache)$entries, 1L)
  expect_identical(gr_cache_clear(cache), 1L)
  expect_true(all(c("analysis_results.rds", store) %in% list.files(proj, recursive = TRUE)))
})

# ---------------------------------------------------------------------------
# security-01: a cache entry is used only when it is plain data
# ---------------------------------------------------------------------------

cached_entry_file <- function(dir) {
  f <- list.files(dir, pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
  stopifnot(length(f) == 1L)
  f
}

test_that("an entry whose result is an environment is a miss, not a result", {
  local_registries()
  dir <- withr::local_tempdir()
  n <- 0L
  cl <- gr_cache_client(gr_mock_client(function(m, p) { n <<- n + 1L; "fresh answer" }),
                        gr_cache(dir))
  invisible(gr_call(cl, "q"))
  f <- cached_entry_file(dir)

  # Passes the old checks: a list of format 1 whose result inherits gr_result.
  env <- new.env(parent = emptyenv())
  env$ok <- TRUE
  env$text <- "from the file"
  class(env) <- "gr_result"
  saveRDS(list(format = 1L, key = "k", created = Sys.time(), result = env), f)

  res <- gr_call(cl, "q")
  expect_identical(n, 2L)                    # the handler answered
  expect_true(is.list(res))
  expect_identical(res$text, "fresh answer")
  expect_false(res$cached)
})

test_that("an entry holding a function anywhere inside it is a miss", {
  local_registries()
  dir <- withr::local_tempdir()
  n <- 0L
  cl <- gr_cache_client(gr_mock_client(function(m, p) { n <<- n + 1L; "fresh answer" }),
                        gr_cache(dir))
  invisible(gr_call(cl, "q"))
  f <- cached_entry_file(dir)
  good <- readRDS(f)

  bad <- good
  bad$result$usage$input <- function() 1L
  saveRDS(bad, f)
  expect_false(gr_call(cl, "q")$cached)
  expect_identical(n, 2L)

  bad <- good
  attr(bad$result, "extra") <- new.env()     # hidden in an attribute
  saveRDS(bad, f)
  expect_false(gr_call(cl, "q")$cached)
  expect_identical(n, 3L)

  bad <- good
  bad$result$text <- quote(a + b)            # a language object
  saveRDS(bad, f)
  expect_false(gr_call(cl, "q")$cached)
  expect_identical(n, 4L)
})

test_that("a valid entry still reads back as a fresh gr_result", {
  local_registries()
  dir <- withr::local_tempdir()
  n <- 0L
  cl <- gr_cache_client(gr_mock_client(function(m, p) { n <<- n + 1L; "stored answer" }),
                        gr_cache(dir))
  first <- gr_call(cl, "q")
  again <- gr_call(cl, "q")
  expect_identical(n, 1L)
  expect_s3_class(again, "gr_result")
  expect_true(again$cached)
  expect_identical(again$text, "stored answer")
  expect_identical(again$usage, first$usage)
  expect_identical(again$model, first$model)

  # A failure is never written, so an entry claiming one is not trusted either.
  f <- cached_entry_file(dir)
  e <- readRDS(f)
  e$result$ok <- FALSE
  saveRDS(e, f)
  expect_false(gr_call(cl, "q")$cached)
})

test_that("is_plain_data() accepts data and refuses code and references", {
  plain <- readgpt:::is_plain_data
  expect_true(plain(list(a = 1L, b = "x", c = list(d = NULL, e = c(TRUE, NA)))))
  expect_true(plain(data.frame(x = 1:2, y = c("a", "b"))))
  expect_true(plain(Sys.time()))
  expect_false(plain(new.env()))
  expect_false(plain(function() 1))
  expect_false(plain(quote(x)))
  expect_false(plain(list(1, list(2, list(globalenv())))))
  expect_false(plain(structure(list(), meta = new.env())))
})

# ---------------------------------------------------------------------------
# cache-trace-03: replay finds calls under the model they asked for
# ---------------------------------------------------------------------------

test_that("a run whose backend reports its own model replays", {
  local_registries()
  local_clean_cache()
  doc <- readgpt_example()
  q <- "What was revenue?"
  bk <- gr_backend_client(function(messages, params) {
    gr_result(TRUE, text = "Revenue was 45.2 million dollars.", model = "provider-model-2026-01")
  })
  ans <- quiet(answer_document(doc, q, "fast", client = bk))
  models <- vapply(Filter(function(s) !identical(s$kind, "local"), ans$trace$steps),
                   function(s) s$model, character(1))
  expect_true(all(models == "provider-model-2026-01"))

  rp <- gr_replay_client(ans$trace)
  again <- quiet(answer_document(doc, q, "fast", client = rp))
  expect_identical(again$answer, ans$answer)
  expect_identical(rp$stats()$misses, 0L)
  expect_gt(rp$stats()$hits, 0L)
  # The replayed trace still names the model that answered.
  replayed <- vapply(Filter(function(s) !identical(s$kind, "local"), again$trace$steps),
                     function(s) s$model, character(1))
  expect_true(all(replayed == "provider-model-2026-01"))

  # And through a file, which is how a run reaches someone else.
  f <- withr::local_tempfile(fileext = ".json")
  gr_trace_save(ans$trace, f)
  from_file <- quiet(answer_document(doc, q, "fast", client = gr_replay_client(f)))
  expect_identical(from_file$answer, ans$answer)
})

test_that("an ellmer run, the documented example, replays", {
  skip_if_not_installed("ellmer")
  local_registries()
  local_clean_cache()
  make_chat <- function(turns = list(), system = NULL) {
    self <- new.env(parent = emptyenv())
    self$turns <- turns; self$system <- system
    self$chat <- function(user, echo = "none") "Revenue was 45.2 million dollars."
    self$chat_structured <- function(user, type = NULL, echo = "none") {
      list(answer = "Revenue was 45.2 million dollars.")
    }
    self$clone <- function(deep = FALSE) make_chat(self$turns, self$system)
    self$set_turns <- function(value) self$turns <- value
    self$set_system_prompt <- function(value) self$system <- value
    self$get_model <- function() "claude-sonnet-4-5"
    self$get_tokens <- function() data.frame(input = 5, output = 3)
    self
  }
  doc <- readgpt_example()
  q <- "What was revenue?"
  for (recipe in c("fast", "thorough")) {
    ans <- quiet(answer_document(doc, q, recipe, client = gr_ellmer_client(make_chat())))
    rp <- gr_replay_client(ans$trace)
    again <- expect_no_error(quiet(answer_document(doc, q, recipe, client = rp)))
    expect_identical(again$answer, ans$answer)
    expect_identical(rp$stats()$misses, 0L)
  }
})

test_that("a trace without params is still keyed on the model it recorded", {
  local_registries()
  local_clean_cache()
  cl <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
  ans <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = cl))
  parsed <- jsonlite::fromJSON(as.character(as_json(ans$trace)), simplifyVector = FALSE)
  parsed$steps <- lapply(parsed$steps, function(s) { s$params <- NULL; s })
  rp <- gr_replay_client(parsed)
  again <- quiet(answer_document(readgpt_example(), "What was revenue?", "fast", client = rp))
  expect_identical(again$answer, ans$answer)
  expect_identical(rp$stats()$misses, 0L)
})

# ---------------------------------------------------------------------------
# tokenize-embed-02: word matching works in every script
# ---------------------------------------------------------------------------

test_that("lexical terms keep letters outside ASCII, and ASCII text is unchanged", {
  terms <- readgpt:::lexical_terms
  expect_identical(terms("Müller über Größe", 2L), c("müller", "über", "größe"))
  expect_identical(terms("Выручка компании выросла.", 2L), c("выручка", "компании", "выросла"))
  expect_identical(terms("हिन्दी भाषा", 2L), c("हिन्दी", "भाषा"))   # combining marks stay in the word
  # Scripts written without spaces are matched on character pairs.
  expect_identical(terms("收入增长", 2L), c("收入", "入增", "增长"))
  expect_identical(terms("税", 3L), "税")

  # For ASCII the new normaliser is exactly the old one.
  x <- "The CAT, sat_on the mat! 3.14 x -- e-mail (twice) twice."
  old <- tolower(readgpt:::words_of(gsub("[^[:alnum:][:space:]]", " ", x, perl = TRUE)))
  expect_identical(terms(x, 2L), old[nchar(old) > 1L])
  expect_identical(terms(x, 3L), old[nchar(old) > 2L])
})

test_that("BM25 and lexical vectors score non-Latin text", {
  ru <- c("Выручка компании выросла до пяти миллиардов рублей.",
          "Компания открыла новый завод в Казани.",
          "Совет директоров утвердил дивиденды.")
  s <- readgpt:::bm25_scores(ru, "выручка компании")
  expect_gt(s[1], 0)
  expect_identical(which.max(s), 1L)
  expect_equal(rowSums(readgpt:::lexical_embed(ru)^2), c(1, 1, 1))

  zh <- c("公司的收入增长了百分之十", "公司在上海开了新工厂", "董事会批准了股息")
  s <- readgpt:::bm25_scores(zh, "收入增长")
  expect_identical(which.max(s), 1L)
  expect_gt(s[1], 0)
  expect_equal(rowSums(readgpt:::lexical_embed(zh)^2), c(1, 1, 1))
})

test_that("rerank on a Russian document scores the chunk that holds the answer", {
  local_registries()
  local_clean_cache()
  filler <- c("Совет директоров провёл очередное заседание в конце квартала и обсудил планы.",
              "Сотрудники отдела логистики прошли обучение по новым стандартам безопасности.",
              "Компания открыла новый склад в пригороде и наняла дополнительный персонал.",
              "Отдел маркетинга подготовил кампанию для региональных рынков страны.",
              "Юридическая служба завершила проверку договоров с поставщиками сырья.")
  key <- "семь миллиардов"
  answer <- "Выручка компании за отчётный год составила ровно семь миллиардов рублей."
  paras <- vapply(1:16, function(i) if (i == 11) answer else
    paste(filler[((i - 1) %% 5) + 1], sprintf("Пункт номер %d.", i)), character(1))
  ch <- quiet(gr_segment(quiet(gr_ingest(paste(paras, collapse = "\n\n"))),
                         list(method = "paragraph", max_tokens = 80)))
  expect_true(any(grepl(key, ch$chunks$text, fixed = TRUE)))

  scored <- character(0)
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("Rate how useful", messages[[1]]$content, fixed = TRUE)) {
      ex <- messages[[3]]$content
      scored <<- c(scored, ex)
      return(if (grepl(key, ex, fixed = TRUE)) '{"score": 10, "reason": "answer"}'
             else '{"score": 1, "reason": "no"}')
    }
    all <- paste(vapply(messages, function(m) m$content, ""), collapse = "\n")
    if (grepl(key, all, fixed = TRUE)) "ANSWER: seven billion" else "NOT_IN_DOCUMENT"
  })
  q <- "Какой была выручка компании за отчётный год?"
  r <- quiet(gr_read(ch, q, cl, gr_read_spec("rerank", rerank_candidates = 3)))
  expect_true(any(grepl(key, scored, fixed = TRUE)))
  expect_identical(r$answer, "ANSWER: seven billion")

  gr_options(embedder = "lexical")           # restored by local_registries()
  r2 <- quiet(gr_read(ch, q, cl, gr_read_spec("retrieve", top_k = 2)))
  expect_identical(r2$answer, "ANSWER: seven billion")
})

# ---------------------------------------------------------------------------
# money-05: embeddings requests are counted, priced and limited
# ---------------------------------------------------------------------------

# Stands in for the embeddings endpoint: counts requests and answers each with
# vectors and a usage block of 100 tokens per text.
fake_embeddings_endpoint <- function(env = parent.frame()) {
  seen <- new.env(parent = emptyenv())
  seen$n <- 0L
  testthat::local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      seen$n <- seen$n + 1L
      k <- length(body$input)
      out <- list(data = lapply(seq_len(k), function(i) list(embedding = as.list(c(i, 1, 0.5)))),
                  usage = list(prompt_tokens = 100L * k))
      structure(list(url = url, status_code = 200L,
                     headers = structure(list(`content-type` = "application/json; charset=utf-8"),
                                         class = c("insensitive", "list")),
                     content = charToRaw(as.character(jsonlite::toJSON(out, auto_unbox = TRUE)))),
                class = "response")
    },
    .package = "httr", .env = env)
  seen
}

priced_embedding_client <- function() {
  gr_register_model("priced-embed", context_window = 8191L, max_output = 0L,
                    input_usd = 1, kind = "embedding", dimensions = 3L)
  gr_client(api_key = "sk-test", base_url = "https://embed.invalid/v1",
            embedding_model = "priced-embed")
}

test_that("each embeddings request is recorded in the trace and priced", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  seen <- fake_embeddings_endpoint()
  cl <- priced_embedding_client()
  tr <- gr_trace()
  e <- gr_embed(cl, c("one", "two", "three"), batch_size = 2L, trace = tr)
  expect_identical(attr(e, "embedding_source"), "api")
  expect_identical(seen$n, 2L)
  expect_identical(tr$calls, 2L)
  expect_identical(tr$tokens_in, 300L)
  expect_equal(tr$spent_usd, 300 / 1e6)
  cost <- gr_trace_cost(tr)
  expect_identical(cost$model, "priced-embed")
  expect_identical(cost$paid_calls, 2L)
  expect_identical(cost$paid_in, 300L)
  expect_equal(cost$usd, 300 / 1e6)
  expect_identical(as.data.frame(tr)$stage, c("embed.request", "embed.request"))
  # Not model calls: a replay has nothing of them to answer.
  expect_length(readgpt:::replay_steps(tr), 0L)
})

test_that("max_calls = 0 sends no embeddings request, and says so", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE, max_calls = 0)
  seen <- fake_embeddings_endpoint()
  cl <- priced_embedding_client()
  tr <- gr_trace()
  expect_warning(e <- gr_embed(cl, c("one", "two"), trace = tr),
                 class = "gr_embed_fallback", regexp = "call cap")
  expect_identical(seen$n, 0L)
  expect_identical(attr(e, "embedding_source"), "lexical")
  expect_true(isTRUE(attr(e, "embedding_fallback")))
  expect_true(tr$budget_stop)
  expect_identical(tr$stop_reason, "calls")
  expect_identical(tr$calls, 0L)

  # With fallback = "error" the refusal stops the caller instead.
  expect_error(gr_embed(cl, c("one", "two"), trace = gr_trace(), fallback = "error"),
               class = "gr_embed_error")
  expect_identical(seen$n, 0L)
})

test_that("the spending limit stops embeddings requests once it is reached", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE, max_cost_usd = 0.0001)
  seen <- fake_embeddings_endpoint()
  cl <- priced_embedding_client()
  tr <- gr_trace()
  # Each request of two texts costs $0.0002, so the first reaches the limit
  # and the second is never sent.
  expect_warning(gr_embed(cl, c("a", "b", "c", "d"), batch_size = 2L, trace = tr),
                 class = "gr_embed_fallback", regexp = "spending limit")
  expect_identical(seen$n, 1L)
  expect_identical(tr$calls, 1L)
  expect_true(tr$budget_stop)
  expect_identical(tr$stop_reason, "cost")
})

test_that("embeddings served from the session cache are free", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = TRUE)
  seen <- fake_embeddings_endpoint()
  cl <- priced_embedding_client()
  tr1 <- gr_trace()
  invisible(gr_embed(cl, c("one", "two"), trace = tr1))
  tr2 <- gr_trace()
  invisible(gr_embed(cl, c("one", "two"), trace = tr2))
  expect_identical(seen$n, 1L)
  expect_identical(tr1$calls, 1L)
  expect_identical(tr2$calls, 0L)
  expect_identical(tr2$spent_usd, 0)
})

# ---------------------------------------------------------------------------
# r2-export-serialization-fidelity-02: as_json() writes numbers in full
# ---------------------------------------------------------------------------

test_that("as_json() keeps small and long numbers", {
  j <- jsonlite::fromJSON(as.character(as_json(list(p = 0.00003, d = 0.84321, n = 3000000001),
                                               pretty = FALSE)))
  expect_identical(j$p, 3e-05)
  expect_identical(j$d, 0.84321)
  expect_identical(j$n, 3000000001)
  # A caller can still round, as jsonlite lets them.
  expect_identical(as.character(as_json(list(a = 1 / 3), pretty = FALSE, digits = 2)),
                   '{"a":0.33}')
})

test_that("an extraction's answer text carries the values the table holds", {
  local_registries()
  local_clean_cache()
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines(paste("We enrolled 120 participants in the trial.",
                   "The effect was d = 0.84321 with p = 0.00003 overall.", sep = "\n\n"), f)
  fields <- gr_fields(p = gr_field("P value", type = "number"),
                      effect = gr_field("Effect size", type = "number"),
                      n = gr_field("Participants", type = "integer"))
  cl <- gr_mock_client(function(messages, params) {
    paste0('{"p":0.00003,"p__quote":"p = 0.00003","effect":0.84321,',
           '"effect__quote":"d = 0.84321","n":120,"n__quote":"120 participants"}')
  })
  x <- quiet(gr_extract(f, fields, client = cl, recipe = "fast"))
  expect_equal(x$table$p, 3e-05)
  rec <- jsonlite::fromJSON(x$summary$answer)
  expect_identical(rec$p, 3e-05)
  expect_identical(rec$effect, 0.84321)
})
