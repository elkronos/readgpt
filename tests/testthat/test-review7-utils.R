# test-review7-utils.R -- the cross-file handoffs the sixth pass left for the
# shared helpers: utils-misc.R, core-tokenize.R, core-state.R, core-embed.R and
# segment-core.R.
#
# Each block names the handoff and says what the code did before. Non-ASCII
# text is written with \u escapes so the source stays ASCII.

# CP1252 bytes in a UTF-8 session: "cafe q" with an e acute that is one byte.
r7_cp1252 <- function() rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9, 0x20, 0x71)))

# ---------------------------------------------------------------------------
# lower-text-mark: lower_text() handed chartr() an unlabelled string, which in
# a C locale it read as native bytes and rewrote into bytes that are not UTF-8.
# ---------------------------------------------------------------------------

test_that("lower_text() keeps unlabelled UTF-8 intact under a C locale", {
  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!nzchar(old), "no LC_CTYPE to restore")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", old)))
  s <- "\u03b1lpha-\u03b8eta M\u00dcLLER \u0391\u0392"
  want <- "\u03b1lpha-\u03b8eta m\u00fcller \u03b1\u03b2"
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(ok), "cannot switch to the C locale here")

  # Before: ee b1 6c ... e3 9c ..., which is not UTF-8.
  short <- readgpt:::lower_text(unmarked(s))
  expect_true(validUTF8(short))
  expect_identical(charToRaw(short), charToRaw(want))
  # The long-string path (one code point at a time) too.
  long <- readgpt:::lower_text(unmarked(paste(rep(s, 40), collapse = " ")))
  expect_true(validUTF8(long))
  expect_identical(charToRaw(long), charToRaw(paste(rep(want, 40), collapse = " ")))
  # Names, NA and a labelled string are as they were.
  v <- readgpt:::lower_text(c(a = unmarked(s), b = NA, c = s))
  expect_identical(names(v), c("a", "b", "c"))
  expect_true(is.na(v[["b"]]))
  expect_identical(charToRaw(v[["a"]]), charToRaw(want))
  expect_identical(charToRaw(v[["c"]]), charToRaw(want))
  # Bytes that are not UTF-8 are not made any worse.
  expect_identical(charToRaw(readgpt:::lower_text(r7_cp1252())), charToRaw(r7_cp1252()))
})

# ---------------------------------------------------------------------------
# h1-tokenize-embed-12: the token counters ran trimws() and nchar() on the raw
# text, so bytes in another encoding stopped the call with "input string 1 is
# invalid UTF-8" before the documented byte fallback could count them.
# ---------------------------------------------------------------------------

test_that("counting and truncating bytes in another encoding falls back, not over", {
  b <- r7_cp1252()
  expect_false(validUTF8(b))
  # Before: Error in sub(re, "", x, perl = TRUE): input string 1 is invalid UTF-8.
  n <- gr_count_tokens(b)
  expect_true(is.integer(n) && n > 0L)
  # A byte for a character: never fewer than the transcoded text counts.
  expect_gte(n, gr_count_tokens(readgpt:::to_utf8(b)))
  expect_identical(gr_count_tokens(c(b, "", NA, "   ", b)), c(n, 0L, 0L, 0L, n))
  # A line of no-break spaces still costs tokens, as it did.
  expect_gt(gr_count_tokens("\u00a0\u00a0\u00a0"), 0L)

  long <- paste(rep(b, 60), collapse = " ")
  out <- gr_truncate_tokens(long, 12)
  expect_true(validUTF8(out))
  expect_lte(gr_count_tokens(out), 12L)
  expect_true(nzchar(out))
  expect_identical(gr_truncate_tokens(b, 100), b)

  old <- gr_tokenizer()
  withr::defer(gr_set_tokenizer(old))
  gr_set_tokenizer("chars")
  # Before: invalid multibyte string, element 1. Six bytes, six characters.
  expect_identical(gr_count_tokens(c(b, "abcdefgh", "")), c(2L, 2L, 0L))
  gr_set_tokenizer("words")
  expect_identical(gr_count_tokens(b), 2L)
})

test_that("the tiktoken counter hands Python text, not bytes it cannot decode", {
  seen <- character(0)
  fake <- list(encode = function(x, disallowed_special = list()) {
    # Python's str would refuse these bytes.
    if (!validUTF8(x)) stop("UnicodeDecodeError: invalid continuation byte")
    seen <<- c(seen, x)
    as.list(seq_len(nchar(x)))
  })
  local_mocked_bindings(tiktoken_available = function() TRUE,
                        tiktoken_encodings = function(model = NULL) list(fake))
  expect_identical(readgpt:::tok_tiktoken(c(r7_cp1252(), "plain"), NULL), c(6L, 5L))
  expect_identical(seen[1], "caf\u00e9 q")
})

# ---------------------------------------------------------------------------
# h3-state-concurrency-12: gr_flush_caches(), the only way to clear the memory
# caches, was @noRd and not exported.
# ---------------------------------------------------------------------------

test_that("gr_flush_caches() is documented and exported, apart from gr_cache_clear()", {
  src <- test_path("..", "..", "R", "core-state.R")
  skip_if_not(file.exists(src), "no source tree (installed package)")
  lines <- readLines(src, warn = FALSE)
  at <- grep("^gr_flush_caches <- function", lines)
  expect_length(at, 1L)
  start <- at - 1L
  while (start > 1L && grepl("^#'", lines[start - 1L])) start <- start - 1L
  block <- lines[start:(at - 1L)]
  expect_true(any(block == "#' @export"))
  expect_false(any(grepl("@noRd", block, fixed = TRUE)))
  expect_true(any(grepl("[gr_cache_clear()]", block, fixed = TRUE)))
  expect_true(any(grepl("@examples", block, fixed = TRUE)))
})

test_that("gr_flush_caches() clears each memory cache and its bookkeeping", {
  local_clean_cache()
  st <- readgpt:::gr_state
  readgpt:::embed_cache_put(c("e1", "e2"), list(c(1, 0), c(0, 1)))
  readgpt:::doc_cache_put("d1", list(text = "x"))
  expect_identical(gr_flush_caches("embeddings"), "embeddings")
  expect_length(ls(st$embed_cache, all.names = TRUE), 0L)
  expect_identical(ls(st$doc_cache), "d1")
  gr_flush_caches()
  expect_length(ls(st$doc_cache, all.names = TRUE), 0L)
  expect_error(gr_flush_caches("responses"))
})

# ---------------------------------------------------------------------------
# h4-state-concurrency-12: the embedding cache had no bound, while the document
# cache next to it was bounded.
# ---------------------------------------------------------------------------

test_that("the embedding cache drops the vectors used least recently past its budget", {
  local_clean_cache()
  st <- readgpt:::gr_state
  size <- as.numeric(utils::object.size(c(1, 2, 3)))
  put <- function(k, keep = k) {
    readgpt:::embed_cache_put(k, rep(list(c(1, 2, 3)), length(k)), keep = keep,
                              budget = 2.5 * size)
  }
  for (k in c("k1", "k2", "k3", "k4")) put(k)
  expect_identical(ls(st$embed_cache), c("k3", "k4"))
  # A vector read counts as used.
  expect_false(is.null(readgpt:::embed_cache_get("k3")[[1]]))
  put("k5")
  expect_identical(ls(st$embed_cache), c("k3", "k5"))
  # One call larger than the budget keeps all it stores and all it found.
  put(c("a", "b", "c"), keep = c("k5", "a", "b", "c"))
  expect_identical(ls(st$embed_cache), c("a", "b", "c", "k5"))
  put("d")
  expect_identical(ls(st$embed_cache), c("c", "d"))
  # The bookkeeping follows the entries, and a vector stored another way is
  # counted when it is met.
  expect_identical(st$embed_cache$.order, c("c", "d"))
  expect_identical(names(st$embed_cache$.sizes), c("c", "d"))
  assign("stray", c(1, 2, 3), envir = st$embed_cache)
  st$embed_cache$.order <- c("stray", st$embed_cache$.order)
  put("e")
  expect_identical(ls(st$embed_cache), c("d", "e"))
})

test_that("gr_embed() serves vectors from the bounded cache and asks again for dropped ones", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = TRUE)
  seen <- new.env(parent = emptyenv())
  seen$texts <- character(0)
  local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      texts <- unlist(body$input)
      seen$texts <- c(seen$texts, texts)
      data <- lapply(seq_along(texts), function(i) {
        list(index = i - 1L, embedding = as.list(c(nchar(texts[i]), 1, 2)))
      })
      structure(list(url = url, status_code = 200L,
                     headers = structure(list(`content-type` = "application/json"),
                                         class = c("insensitive", "list")),
                     content = charToRaw(as.character(jsonlite::toJSON(
                       list(data = data, usage = list(prompt_tokens = 3L)), auto_unbox = TRUE)))),
                class = "response")
    },
    .package = "httr")
  # Room for two of these vectors.
  local_mocked_bindings(.gr_embed_cache_bytes = 2.5 * as.numeric(utils::object.size(c(1, 2, 3))))
  cl <- gr_client(api_key = "sk-test", base_url = "https://r7.invalid/v1", api = "chat",
                  model = "gpt-4o", embedding_model = "text-embedding-3-small",
                  max_retries = 0L, retry_pause_base = 0)
  emb <- function(x) gr_embed(cl, x)

  first <- emb(c("a", "bb", "ccc"))
  expect_identical(attr(first, "embedding_source"), "api")
  expect_identical(seen$texts, c("a", "bb", "ccc"))
  # All three are kept by the call that stored them; the next store trims.
  again <- emb(c("ccc", "a"))
  expect_identical(seen$texts, c("a", "bb", "ccc"))
  expect_equal(again, first[c(3, 1), ], ignore_attr = TRUE)
  emb("dddd")
  expect_length(ls(readgpt:::gr_state$embed_cache), 2L)
  # "a" was used last before "dddd", so it stayed; "bb" and "ccc" went.
  emb("a")
  expect_identical(seen$texts, c("a", "bb", "ccc", "dddd"))
  emb("bb")
  expect_identical(seen$texts, c("a", "bb", "ccc", "dddd", "bb"))
})

# ---------------------------------------------------------------------------
# segment-13: the sentence boundary for CJK terminators and capitals of every
# script, which the sixth pass put in .gr_sentence_split. Guarded here through
# the paths the handoff names beyond sentences_of() itself.
# ---------------------------------------------------------------------------

test_that("overlap and hard splits of Chinese and Russian text cut at sentences", {
  zh <- paste(rep(c("\u6cbb\u7597\u7ec4\u7684\u6b7b\u4ea1\u7387\u4e0b\u964d\u4e86\u5341\u4e8c\u4e2a\u767e\u5206\u70b9\u3002",
                    "\u5206\u6790\u662f\u9884\u5148\u89c4\u5b9a\u7684\uff01"), 6), collapse = "")
  tail <- readgpt:::tail_by_tokens(zh, 30)
  expect_true(nzchar(tail))
  expect_true(startsWith(tail, "\u6cbb") || startsWith(tail, "\u5206"))
  parts <- readgpt:::hard_split(zh, 40)
  expect_gt(length(parts), 1L)
  expect_true(all(grepl("[\u3002\uff01]$", parts)))

  ru <- paste(rep(paste0("\u0421\u043c\u0435\u0440\u0442\u043d\u043e\u0441\u0442\u044c ",
                         "\u0441\u043d\u0438\u0437\u0438\u043b\u0430\u0441\u044c. ",
                         "\u0410\u043d\u0430\u043b\u0438\u0437 \u0431\u044b\u043b ",
                         "\u0437\u0430\u0440\u0430\u043d\u0435\u0435."), 8), collapse = " ")
  parts <- readgpt:::hard_split(ru, 60)
  expect_gt(length(parts), 1L)
  expect_true(all(grepl("\\.$", parts)))
})
