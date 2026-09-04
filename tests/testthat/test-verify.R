# test-verify.R -- did the model quote the document, or invent the quote?
#
# The check has to be forgiving in exactly one direction and unforgiving in the
# other, and both halves need proving. Too strict and `partial` fires on every
# answer whose quotes got a trailing full stop, which is how a warning stops
# meaning anything. Too loose and a changed number passes, which is the entire
# thing it exists to catch.

nm  <- function(...) readgpt:::normalise_for_match(...)
tqe <- function(...) readgpt:::trim_quote_edges(...)
sm  <- function(...) readgpt:::span_match(...)
cc  <- function(...) readgpt:::cited_chunks(...)

test_that("normalisation folds typography and nothing else", {
  # The differences a faithful quotation introduces.
  expect_identical(nm("Revenue   rose"), nm("Revenue rose"))
  expect_identical(nm("REVENUE ROSE"), nm("revenue rose"))
  expect_identical(nm("the board\u2019s view"), nm("the board's view"))
  expect_identical(nm("\u201cquoted\u201d"), nm('"quoted"'))
  expect_identical(nm("twenty\u2014five"), nm("twenty-five"))
  expect_identical(nm("a\u00a0b"), nm("a b"))

  # The differences that are not.
  expect_false(identical(nm("45.2 million"), nm("54.2 million")))
  expect_false(identical(nm("revenue rose"), nm("revenue fell")))
  expect_false(identical(nm("rose 12%"), nm("rose 12")))
})

test_that("edge trimming removes quotation packaging, not content", {
  expect_identical(tqe('"quoted text"'), "quoted text")
  expect_identical(tqe("... truncated ..."), "truncated")
  expect_identical(tqe("trailing full stop."), "trailing full stop")
  expect_identical(tqe("  spaced  "), "spaced")
  expect_identical(tqe("dash-ended-"), "dash-ended")

  # Characters that carry meaning survive. A percentage sign or a closing
  # parenthesis is part of the claim, and stripping one would let a quotation
  # that dropped it pass as exact.
  expect_identical(tqe("rose 12%"), "rose 12%")
  expect_identical(tqe("(as restated)"), "(as restated)")
  expect_identical(tqe("45.2"), "45.2")
})

test_that("a faithful quotation verifies, however it was typeset", {
  src <- "Revenue rose to 45.2 million dollars, before tax."
  faithful <- c(
    "Revenue rose to 45.2 million dollars, before tax.",
    "Revenue  rose   to 45.2 million dollars",
    '"Revenue rose to 45.2 million dollars"',
    "... rose to 45.2 million dollars ...",
    "Revenue rose to 45.2 million dollars\u2014",
    "REVENUE ROSE TO 45.2 MILLION DOLLARS",
    "Revenue rose to 45.2 million"                     # truncated, still true
  )
  for (span in faithful) {
    r <- sm(span, src)
    expect_true(r$verified, info = span)
    expect_identical(r$match, 1)
  }
})

test_that("an invented quotation does not verify, and the number says how far off", {
  src <- "Revenue rose to 45.2 million dollars, before tax."

  changed <- sm("Revenue rose to 54.2 million dollars", src)
  expect_false(changed$verified)
  expect_gt(changed$match, 0.2)          # most of it is real
  expect_lt(changed$match, 1)

  invented <- sm("Margins expanded on record overseas demand.", src)
  expect_false(invented$verified)
  expect_lt(invented$match, changed$match)   # and this one is not

  # A single wrong word in the middle is caught, which is the case that matters:
  # the whole span is plausible and one fact is wrong.
  expect_false(sm("Revenue fell to 45.2 million dollars", src)$verified)
})

test_that("the run measure separates quotation from paraphrase", {
  run <- readgpt:::longest_quoted_run
  src <- nm("the cohort comprised 482 participants recruited across nine sites")
  expect_identical(run(strsplit(nm("comprised 482 participants"), " ")[[1]], src), 3L)
  expect_identical(run(strsplit(nm("482 participants were recruited"), " ")[[1]], src), 2L)
  expect_identical(run(character(0), src), 0L)
  expect_identical(run(c("nothing", "matches"), src), 0L)

  # A paraphrase built from the source's own vocabulary shares many words and
  # almost no runs, which is precisely the distinction word overlap cannot make.
  paraphrase <- sm("participants recruited nine cohort sites across", src)
  expect_false(paraphrase$verified)
  expect_lt(paraphrase$match, 0.5)
})

test_that("empty and degenerate inputs are NA, not TRUE", {
  expect_true(is.na(sm("", "some source")$verified))
  expect_true(is.na(sm("a span", "")$verified))
  expect_true(is.na(sm("...", "some source")$verified))    # nothing left after trimming
  expect_identical(nrow(readgpt:::verify_spans(character(0), character(0))), 0L)
})

test_that("citations are parsed out of an answer", {
  expect_identical(cc("Revenue rose [chunk 3]."), 3L)
  expect_identical(cc("Both [chunk 1] and [chunk 12] agree."), c(1L, 12L))
  expect_identical(cc("Repeated [chunk 4] and again [chunk 4]."), 4L)
  expect_identical(cc("[CHUNK 7]"), 7L)
  expect_identical(cc("no citations here"), integer(0))
  expect_identical(cc("a chunk of text, chunk 3, [chunky 5]"), integer(0))
})

# ---------------------------------------------------------------------------
# Through the readers
# ---------------------------------------------------------------------------

verify_doc <- function() {
  "Revenue rose to 45.2 million dollars.\n\nHeadcount grew to 1,204 employees."
}

# A client whose extraction step can be told what to "quote".
extracting <- function(extract) {
  gr_mock_client(function(messages, params) {
    if (grepl("You extract evidence", messages[[1]]$content, fixed = TRUE)) return(extract)
    "Revenue was 45.2 million dollars."
  })
}

test_that("skim verifies its own extracted evidence", {
  local_registries()
  ch <- gr_segment(gr_ingest(verify_doc()), list(method = "paragraph", max_tokens = 40))

  good <- quiet(gr_read(ch, "What was revenue?", extracting("Revenue rose to 45.2 million dollars."),
                        "skim"))
  expect_true(all(good$evidence$verified))
  expect_identical(good$evidence$match, rep(1, nrow(good$evidence)))
  expect_false(good$partial)
  expect_null(good$notes$unverified_evidence)
  expect_gt(good$notes$evidence_verified, 0L)
})

test_that("a fabricated quotation makes the answer partial", {
  # This is the case a reader cannot catch by eye: the span is fluent, on topic,
  # attributed to a real chunk, and not in the document.
  local_registries()
  ch <- gr_segment(gr_ingest(verify_doc()), list(method = "paragraph", max_tokens = 40))
  bad <- quiet(gr_read(ch, "What was revenue?",
                       extracting("Revenue rose to 88.9 billion dollars on record demand."),
                       "skim"))
  expect_false(any(bad$evidence$verified))
  expect_true(bad$partial)
  expect_identical(bad$notes$unverified_evidence, sum(!bad$evidence$verified))

  v <- gr_verify_evidence(bad)
  expect_identical(v$kind, rep("extracted", nrow(v)))
  expect_false(any(v$verified))
  expect_true(all(v$match < 1))
})

test_that("a skim answer can be checked without the chunks, a verbatim one needs them", {
  local_registries()
  ch <- gr_segment(gr_ingest(verify_doc()), list(method = "paragraph", max_tokens = 40))

  # skim carries its sources, because its evidence is model-written.
  sk <- quiet(gr_read(ch, "What was revenue?", extracting("Revenue rose to 45.2 million dollars."),
                      "skim"))
  expect_true(all(gr_verify_evidence(sk)$verified))

  # A verbatim reader does not: storing the source would be storing the span
  # twice to prove a string equals itself.
  rt <- quiet(gr_read(ch, "What was revenue?", extracting("x"),
                      list(reader = "retrieve", top_k = 1)))
  expect_null(rt$evidence$source_text)
  expect_true(all(is.na(gr_verify_evidence(rt)$verified)))
  with_chunks <- gr_verify_evidence(rt, ch)
  expect_identical(with_chunks$kind, rep("verbatim", nrow(with_chunks)))
  expect_true(all(with_chunks$verified))
})

test_that("evidence that is an answer rather than a quotation reports NA", {
  # A map_reduce evidence row is that chunk's ANSWER. Asking whether it appears
  # in the chunk is a category error, and reporting FALSE would be worse than
  # useless -- it would mark every correct map_reduce run as unverified.
  local_registries()
  ch <- gr_segment(gr_ingest(verify_doc()), list(method = "paragraph", max_tokens = 40))
  mr <- quiet(gr_read(ch, "What was revenue?", mock_echo("Revenue was 45.2 million."),
                      "map_reduce"))
  v <- gr_verify_evidence(mr, ch)
  expect_identical(v$kind, rep("answer", nrow(v)))
  expect_true(all(is.na(v$verified)))
  expect_false(mr$partial)
})

test_that("a citation to a chunk that was never sent is caught", {
  local_registries()
  ch <- gr_segment(gr_ingest(verify_doc()), list(method = "paragraph", max_tokens = 40))

  liar <- gr_mock_client(function(m, p)
    "Revenue was 45.2 million [chunk 1], and costs fell [chunk 99].")
  bad <- quiet(gr_read(ch, "What was revenue?", liar, list(reader = "stuff", cite = TRUE)))
  expect_identical(bad$notes$cited_unknown, 99L)
  expect_true(bad$partial)

  honest <- gr_mock_client(function(m, p) "Revenue was 45.2 million [chunk 1].")
  ok <- quiet(gr_read(ch, "What was revenue?", honest, list(reader = "stuff", cite = TRUE)))
  expect_null(ok$notes$cited_unknown)
  expect_false(ok$partial)
})

test_that("an answer with no citations and no evidence is untouched", {
  local_registries()
  ch <- gr_segment(gr_ingest(verify_doc()), list(method = "paragraph", max_tokens = 40))
  plain <- quiet(gr_read(ch, "What was revenue?", mock_echo("Revenue was 45.2 million."),
                         list(reader = "stuff")))
  expect_false(plain$partial)
  expect_null(plain$notes$cited_unknown)
  expect_null(plain$notes$unverified_evidence)
})

test_that("gr_verify_evidence validates and handles nothing to verify", {
  local_registries()
  expect_error(gr_verify_evidence("not an answer"), "gr_answer")
  # An answer with no evidence at all -- `refine` and `hierarchical` produce
  # these, and so does any reader that found nothing.
  none <- new_answer("nothing found", "refine", "What was revenue?", integer(0),
                     gr_trace(), evidence = NULL)
  v <- gr_verify_evidence(none)
  expect_s3_class(v, "data.frame")
  expect_identical(nrow(v), 0L)
  expect_identical(names(v), c("chunk_id", "kind", "verified", "match", "span"))
})
