# test-review4-verify-print.R -- the fourth pass on quotation checking and on
# how an answer prints: the handoffs the earlier fixers left in
# R/read-verify.R and R/read-core.R.
#
# Each block names the finding and says what the old behaviour was.

sm <- function(...) readgpt:::span_match(...)

# ---------------------------------------------------------------------------
# read-core-05, part 2: an elision in a skim quotation may not drop a negation
# or join one sentence's subject to another's claim. span_match() checked only
# that the pieces were there, in order, so "the drug did ... reduce mortality"
# verified against "the drug did not reduce mortality" with match 1, and
# "Revenue ... rose 30%" against "Revenue fell 12%. Costs rose 30%.". The
# extract reader's value check refused both (passage_gaps_ok()); the span check
# the skim reader and gr_verify_evidence() rest on did not.
# ---------------------------------------------------------------------------

test_that("an elision that drops a negation does not verify", {
  src <- "In the trial, the drug did not reduce mortality at 12 months."
  m <- sm("the drug did ... reduce mortality", src)
  expect_false(m$verified)
  expect_lt(m$match, 1)
  expect_false(sm("the drug did [...] reduce mortality at 12 months.", src)$verified)
  expect_false(sm("Patients ... responded to treatment",
                  "Patients never responded to treatment.")$verified)
  expect_false(sm("the drug ... reduced mortality",
                  "The drug wasn't shown to have reduced mortality.")$verified)
  # Every gap of a longer elision is checked, not only the first.
  expect_false(sm("Alpha ... beta ... gamma", "Alpha and beta did not gamma.")$verified)
  # The negation quoted, rather than left out, is a faithful quotation.
  expect_true(sm("the drug did not ... reduce mortality", src)$verified)
  expect_identical(sm("the drug ... reduced mortality",
                      "The drug, given daily, reduced mortality.")$match, 1)
})

test_that("an elision across a sentence end has to resume at a sentence start", {
  src <- "Revenue fell 12%. Costs rose 30%."
  m <- sm("Revenue ... rose 30%", src)
  expect_false(m$verified)
  expect_lt(m$match, 1)
  expect_false(sm("Revenue [...] rose 30%.", src)$verified)
  # Bold taken out of both sides is checked by the same rule.
  expect_false(sm("**Revenue** ... rose 30%", "**Revenue** fell 12%. Costs rose 30%.")$verified)
  # Chinese marks the end of a sentence without a space after it.
  zh <- "\u6536\u5165\u4e0b\u964d12%\u3002\u6210\u672c\u589e\u957f30%\u3002"
  expect_false(sm("\u6536\u5165\u2026\u2026\u589e\u957f30%", zh)$verified)
  expect_true(sm("\u6536\u5165\u2026\u2026\u6210\u672c\u589e\u957f30%", zh)$verified)

  # Faithful elisions across sentences still verify.
  for (q in c("Revenue fell 12% ... Costs rose 30%", "Revenue fell 12%. [...] Costs rose 30%.",
              "Revenue fell ... \"Costs rose 30%\"")) {
    m <- sm(q, src)
    expect_true(m$verified, label = q)
    expect_identical(m$match, 1, label = q)
  }
  # And one whose second piece occurs twice verifies on the occurrence that
  # joins as quoted, not only on the first.
  expect_true(sm("Revenue ... rose 30%",
                 "Revenue fell 12%. Costs rose 30%. Revenue rose 30% in May.")$verified)
  expect_true(sm("the drug did ... reduce mortality",
                 paste("The drug did not reduce mortality in 2019.",
                       "The drug did reduce mortality in 2020."))$verified)
  # Out of order is still refused, whatever the gap.
  expect_false(sm("Costs rose ... Revenue fell", src)$verified)
})

test_that("the search over repeated passages is bounded and fails closed", {
  src <- paste0(paste(rep("alpha not beta.", 20), collapse = " "), " alpha and beta.")
  pieces <- c("alpha", "beta")
  ms <- readgpt:::match_source(src)
  expect_true(readgpt:::found_in_order(pieces, ms))
  # With no gaps left to read, a quotation it has not placed is not verified.
  expect_false(readgpt:::elided_chain(pieces, ms, budget = 1L))
  expect_true(readgpt:::elided_chain(pieces, ms))
})

test_that("a skim quotation that elides a negation makes the answer partial", {
  doc <- "In the trial, the drug did not reduce mortality at 12 months."
  ch <- quiet(gr_segment(gr_ingest(doc), list(method = "paragraph", max_tokens = 200)))
  cl <- gr_mock_client(function(m, p) {
    if (grepl("You extract evidence", m[[1]]$content, fixed = TRUE))
      return("the drug did ... reduce mortality at 12 months.")
    "The drug reduced mortality."
  })
  a <- quiet(gr_read(ch, "Did the drug reduce mortality?", cl, "skim"))
  expect_true(a$partial)
  v <- gr_verify_evidence(a)
  expect_false(v$verified)
  expect_lt(v$match, 1)
  expect_match(paste(capture.output(print(a)), collapse = "\n"),
               "quotation(s) not found in the document", fixed = TRUE)

  # The faithful one does not.
  ok <- gr_mock_client(function(m, p) {
    if (grepl("You extract evidence", m[[1]]$content, fixed = TRUE))
      return("In the trial, the drug did not ... reduce mortality at 12 months.")
    "The drug did not reduce mortality."
  })
  b <- quiet(gr_read(ch, "Did the drug reduce mortality?", ok, "skim"))
  expect_false(b$partial)
  expect_true(gr_verify_evidence(b)$verified)
})

# ---------------------------------------------------------------------------
# read-core-05, part 2 (2): a passage written wholly in Han or Cyrillic was
# dropped under a C LC_CTYPE, where "[[:alnum:]]" matches ASCII only, so
# span_match() returned NA and the extract value check saw no passages.
# Already fixed at b127303 (quote_passages() tests "[\\p{L}\\p{N}]"); this
# keeps it that way.
# ---------------------------------------------------------------------------

test_that("Han and Cyrillic quotations are checked under a C locale", {
  zh <- "\u516c\u53f8\u7684\u6536\u5165\u589e\u957f\u4e86\u3002"
  ru <- "\u0412\u044b\u0440\u0443\u0447\u043a\u0430 \u0432\u044b\u0440\u043e\u0441\u043b\u0430 \u0432 \u043c\u0430\u0435."
  zh_q <- "\u6536\u5165\u589e\u957f"
  ru_q <- "\u0432\u044b\u0440\u0443\u0447\u043a\u0430 \u0432\u044b\u0440\u043e\u0441\u043b\u0430"
  ru_bad <- "\u0432\u044b\u0440\u0443\u0447\u043a\u0430 \u0443\u043f\u0430\u043b\u0430"
  check <- function() {
    expect_true(sm(zh_q, zh)$verified)
    expect_true(sm(ru_q, ru)$verified)
    expect_false(sm(ru_bad, ru)$verified)
    expect_length(readgpt:::quote_passages(ru_q)$raw, 1L)
    expect_true(readgpt:::quote_backs_value("\u0432\u044b\u0440\u043e\u0441\u043b\u0430", ru_q, ru,
                                            gr_field("trend")))
  }
  check()
  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!nzchar(old), "no LC_CTYPE to restore")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", old)))
  ok <- suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))
  skip_if(!nzchar(ok), "cannot switch to the C locale here")
  check()
})

# ---------------------------------------------------------------------------
# H6: print() on an answer called every embeddings request a model call, as
# print.gr_trace() did before the trace fixes, and named the first error in
# the trace as why the answer was partial even when a fallback had recovered
# from it and a later request had failed outright.
# ---------------------------------------------------------------------------

# A trace of one model call, one embeddings request the lexical fallback
# replaced, and, with `fail`, a model call that failed.
r4_trace <- function(fail = TRUE) {
  tr <- gr_trace()
  msg <- list(list(role = "user", content = "q"))
  readgpt:::trace_record(tr, "map.answer", msg,
                         gr_result(TRUE, text = "a", model = "gpt-4o",
                                   usage = list(input = 10L, output = 2L)))
  readgpt:::trace_record(tr, "embed.request", msg,
                         gr_result(FALSE, error = "HTTP 404: no embeddings model",
                                   model = "text-embedding-3-small"),
                         embedding = TRUE)
  readgpt:::trace_mark_recovered(tr, 1L)
  if (fail) {
    readgpt:::trace_record(tr, "map.answer", msg,
                           gr_result(FALSE, error = "HTTP 503: upstream down", model = "gpt-4o"))
  }
  tr
}

test_that("print() on an answer counts embeddings requests apart from model calls", {
  a <- new_answer("Revenue was 45.2 million.", "stuff", "What was revenue?", chunks_used = 1L,
                  trace = r4_trace(), partial = TRUE, notes = list(failed_calls = 1L))
  out <- capture.output(print(a))
  line <- out[3]
  expect_match(line, "2 model call(s), 1 embeddings request(s) (0 tokens), 10 in / 2 out tokens",
               fixed = TRUE)
  expect_match(line, "2 error(s) (1 recovered by a fallback)", fixed = TRUE)

  # An answer that embedded nothing prints as it always did.
  plain <- gr_trace()
  readgpt:::trace_record(plain, "map.answer", list(list(role = "user", content = "q")),
                         gr_result(TRUE, text = "a", model = "gpt-4o",
                                   usage = list(input = 10L, output = 2L)))
  b <- new_answer("x", "stuff", "q?", chunks_used = 1L, trace = plain)
  expect_true(startsWith(capture.output(print(b))[3],
                         "  1 model call(s), 10 in / 2 out tokens, 0 error(s), "))
})

test_that("print() names the first failure nothing recovered from", {
  a <- new_answer("x", "stuff", "q?", chunks_used = 1L, trace = r4_trace(),
                  partial = TRUE, notes = list(failed_calls = 1L))
  why <- readgpt:::partial_reasons(a)
  expect_true("first error: HTTP 503: upstream down" %in% why)
  expect_false(any(grepl("HTTP 404", why)))
  expect_true(any(grepl("first error: HTTP 503: upstream down",
                        capture.output(print(a)), fixed = TRUE)))

  # When every failure was recovered, the first recovered one still says why
  # the fallback was needed.
  b <- new_answer("x", "stuff", "q?", chunks_used = 1L, trace = r4_trace(fail = FALSE),
                  partial = TRUE, notes = list(embedding_fallback = TRUE))
  expect_identical(readgpt:::partial_reasons(b),
                   c("embeddings fell back to word matching",
                     "first error: HTTP 404: no embeddings model"))
  # A note the reader left still comes first.
  d <- new_answer("x", "stuff", "q?", chunks_used = 1L, trace = r4_trace(),
                  partial = TRUE, notes = list(error = "the reader's own error"))
  expect_true("first error: the reader's own error" %in% readgpt:::partial_reasons(d))
})

# Stands in for an OpenAI-shaped server whose embeddings endpoint answers
# `embed_status`, as test-review3-trace.R's r3_endpoint() does.
r4_endpoint <- function(embed_status = 404L, env = parent.frame()) {
  respond <- function(url, status, out) {
    structure(list(url = url, status_code = status,
                   headers = structure(list(`content-type` = "application/json; charset=utf-8"),
                                       class = c("insensitive", "list")),
                   content = charToRaw(as.character(jsonlite::toJSON(out, auto_unbox = TRUE)))),
              class = "response")
  }
  testthat::local_mocked_bindings(
    POST = function(url, ..., body = NULL, encode = NULL) {
      if (grepl("/embeddings$", url)) {
        if (embed_status != 200L) {
          return(respond(url, embed_status, list(error = list(message = "no such model"))))
        }
        texts <- unlist(body$input)
        return(respond(url, 200L, list(
          data = lapply(seq_along(texts), function(i) {
            list(embedding = as.list(c(nchar(texts[i]) %% 7 + 1, 1, i %% 3)))
          }),
          usage = list(prompt_tokens = sum(ceiling(nchar(texts) / 4))))))
      }
      all <- paste(vapply(body$messages, function(m) as.character(m$content), ""),
                   collapse = "\n")
      respond(url, 200L, list(
        id = "r4", object = "chat.completion", model = body$model,
        choices = list(list(index = 0, finish_reason = "stop",
                            message = list(role = "assistant",
                                           content = "Revenue was 45.2 million dollars."))),
        usage = list(prompt_tokens = ceiling(nchar(all) / 4), completion_tokens = 7)))
    },
    .package = "httr", .env = env)
}

test_that("a needle answer on an endpoint without embeddings prints its model calls", {
  local_registries()
  local_clean_cache()
  gr_options(cache_embeddings = FALSE)
  r4_endpoint(404L)
  cl <- gr_client(api_key = "sk-test", base_url = "https://r4.invalid/v1", api = "chat",
                  model = "gpt-4o", embedding_model = "text-embedding-3-small",
                  max_retries = 0L)
  doc <- paste(sprintf("Paragraph %d describes operations in region %d and the staff there.",
                       1:40, 1:40), collapse = "\n\n")
  a <- quiet(answer_document(doc, "What was revenue?", "needle", client = cl))
  s <- gr_trace_summary(a$trace)
  expect_gt(s$embed_calls, 0L)
  line <- capture.output(print(a))[3]
  expect_match(line, sprintf("  %d model call(s), %d embeddings request(s)",
                             s$calls - s$embed_calls, s$embed_calls), fixed = TRUE)
  expect_match(line, sprintf("(%d recovered by a fallback)", s$errors), fixed = TRUE)
})

test_that("a verbatim quote that does not state its value is not called 'not found'", {
  # The extract reader marks such a row verified = FALSE with match 1. The
  # partial reason said the quotation was not in the document, which it is.
  ev <- data.frame(chunk_id = 1:2, text = c("Twenty-four patients were enrolled.", "made up"),
                   verified = c(FALSE, FALSE), match = c(1, 0.2), field = c("n", "design"),
                   stringsAsFactors = FALSE)
  a <- structure(list(evidence = ev, notes = list(unverified_evidence = 2L)), class = "gr_answer")
  why <- readgpt:::partial_reasons(a)
  expect_true(any(grepl("1 quotation(s) found but not stating the value cited", why, fixed = TRUE)))
  expect_true(any(grepl("1 quotation(s) not found in the document", why, fixed = TRUE)))
  # A table without the columns to tell them apart keeps the old wording.
  a$evidence <- NULL
  expect_true(any(grepl("2 quotation(s) not found in the document", readgpt:::partial_reasons(a),
                        fixed = TRUE)))
})

test_that("lower case does not depend on the session's locale", {
  # tolower() lowers only A-Z in a C locale on Linux, so a quotation in lower
  # case of a capitalised Russian sentence failed there and nowhere else.
  up <- intToUtf8(c(0x412, 0x44b, 0x440, 0x443, 0x447, 0x43a, 0x430))       # Cyrillic
  expect_identical(utf8ToInt(readgpt:::lower_text(up))[1], 0x432L)
  expect_identical(utf8ToInt(readgpt:::lower_text(intToUtf8(0x391L))), 0x3B1L)   # Greek
  expect_identical(utf8ToInt(readgpt:::lower_text(intToUtf8(0xC9L))), 0xE9L)     # Latin-1
  expect_identical(utf8ToInt(readgpt:::lower_text(intToUtf8(0x1EA0L))), 0x1EA1L) # Vietnamese
  expect_identical(readgpt:::lower_text("MiXeD 12"), "mixed 12")
  old <- Sys.getlocale("LC_CTYPE")
  skip_if(!nzchar(old), "no LC_CTYPE to restore")
  withr::defer(suppressWarnings(Sys.setlocale("LC_CTYPE", old)))
  skip_if(!nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", "C"))), "cannot switch to the C locale here")
  expect_identical(utf8ToInt(readgpt:::lower_text(up))[1], 0x432L)
})
