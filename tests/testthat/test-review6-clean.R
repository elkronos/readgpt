# test-review6-clean.R -- the sixth pass on cleaning and the shared text
# helpers: the medium and low findings left in ingest-clean.R, utils-misc.R and
# ingest.R, and the handoffs the fifth pass left for them.
#
# Each block names the finding and says what the code did before.

r6_sm <- function(span, src) readgpt:::span_match(span, src)
r6_verified <- function(span, src) isTRUE(r6_sm(span, src)$verified)

# ---------------------------------------------------------------------------
# ingest-08: the default page_numbers step deleted a number that ends a
# sentence on a line of its own.
# ---------------------------------------------------------------------------

test_that("a sentence's last number on its own line is not taken for a page number", {
  # Before: "...the total enrolled was" and "Results are shown in Table".
  out <- gr_clean(c("The drug was given in the\nmorning; the total enrolled was\n1200.",
                    "Results are shown in Table\n3.",
                    "The number of sites was:\n12."))
  expect_identical(as.character(out),
                   c("The drug was given in the\nmorning; the total enrolled was\n1200.",
                     "Results are shown in Table\n3.",
                     "The number of sites was:\n12."))

  # A number and a full stop standing alone, or after a finished sentence, is
  # still a page number.
  pn <- function(x) as.character(gr_clean(x, steps = "page_numbers"))
  expect_identical(pn("12."), "")
  expect_identical(pn("The body ends here.\n12."), "The body ends here.\n")
  expect_identical(pn("12.\nThe next page begins."), "\nThe next page begins.")
  expect_identical(pn("Body.\n\n12.\n\nMore."), "Body.\n\n\n\nMore.")
  expect_identical(pn("Page 4"), "")
  expect_identical(pn("- 12 -"), "")
})

# ---------------------------------------------------------------------------
# ingest-12: the captions step (academic and legacy presets) deleted body
# sentences that start "Table 2 shows ..." or "Figure 3 presents ...".
# ---------------------------------------------------------------------------

test_that("the captions step drops captions, not results stated in the body", {
  out <- gr_clean(c(paste0("Table 2 shows that 30-day mortality fell from 21% to 14% in the treated ",
                           "group (p = 0.003), with no difference in stroke."),
                    "Figure 3 presents the dose-response curve.\nThe effect plateaued above 40 mg.",
                    "Table 1 and Figure 2 show the same.", "Table 2, which follows, lists them.",
                    "Table 2. Baseline characteristics"),
                  steps = readgpt:::resolve_clean_steps("academic"))
  # Before: "", "The effect plateaued above 40 mg.", "", "", "".
  expect_identical(as.character(out)[1:4],
                   c(paste0("Table 2 shows that 30-day mortality fell from 21% to 14% in the ",
                            "treated group (p = 0.003), with no difference in stroke."),
                     "Figure 3 presents the dose-response curve.\nThe effect plateaued above 40 mg.",
                     "Table 1 and Figure 2 show the same.", "Table 2, which follows, lists them."))
  expect_identical(as.character(out)[5], "")

  captions <- c("Figure 3: Dose response", "Fig. 3 | Dose", "Table 2", "Table S2. Extra",
                "Table 2 Baseline characteristics", "Table 2 - Baseline", "Table 2 \u2013 Baseline",
                "Exhibit 4: Revenue", "figure 1. lower case label", "Table 3a: sub-table",
                "Figure 1) Flow")
  expect_identical(as.character(gr_clean(captions, steps = "captions")), rep("", length(captions)))
})

# ---------------------------------------------------------------------------
# r-semantics-11: headers_footers dropped every short line repeated three
# times anywhere, table values included.
# ---------------------------------------------------------------------------

test_that("headers_footers keeps repeated table values and drops running heads by position", {
  txt <- paste(c("Outcome measures by site", "Site A mortality", "12", "Site B mortality", "12",
                 "Site C mortality", "12", "Site D mortality", "9",
                 "All sites reported adverse events in full detail to the board."), collapse = "\n")
  # Before: the three 12s were gone, and 9 read as Site C's figure.
  d <- quiet(gr_ingest(txt, gr_ingest_spec(clean = "scan"), cache = FALSE))
  expect_identical(lengths(regmatches(d$text, gregexpr("\n12\n", d$text, fixed = TRUE))), 3L)
  expect_match(d$text, "Site C mortality\n12\nSite D mortality\n9", fixed = TRUE)

  # With the blocks' pages known, the running head at the top of every page
  # goes, and the values repeated inside the pages stay.
  # (The text at the page edges differs from page to page, as body text does.)
  word <- c("first", "second", "third", "fourth", "fifth", "sixth")
  blocks <- character(0); pages <- integer(0)
  for (p in 1:6) {
    blocks <- c(blocks, "Journal of Clinical Trials 2024",
                sprintf(paste0("In the %s cohort the results by site were these.\nSite A mortality\n",
                               "12\nSite B mortality\n12\nYes"), word[p]),
                sprintf("The %s cohort ends here, with sentence %d.\nIts last line is about %s.",
                        word[p], p * 7L, rev(word)[p]))
    pages <- c(pages, p, p, p)
  }
  out <- gr_clean(blocks, steps = "headers_footers", opts = list(.pages = pages))
  expect_false(any(grepl("Journal of Clinical", out, fixed = TRUE)))
  expect_identical(sum(grepl("mortality\n12\nSite B mortality\n12\nYes", out, fixed = TRUE)), 6L)

  # The same through gr_ingest(), which gives the pages: a registered
  # extractor that returns the blocks with their pages.
  local_registries()
  local_clean_cache()
  gr_register_extractor("r6pages", extensions = "r6p", fn = function(path, opts) {
    data.frame(text = blocks, page = pages, stringsAsFactors = FALSE)
  })
  f <- withr::local_tempfile(fileext = ".r6p")
  writeLines("x", f)
  d <- quiet(gr_ingest(f, gr_ingest_spec(clean = "scan"), cache = FALSE))
  expect_false(grepl("Journal of Clinical", d$text, fixed = TRUE))
  expect_identical(lengths(regmatches(d$text, gregexpr("mortality\n12\n", d$text, fixed = TRUE))), 12L)

  # Pages marked by form feeds, as pdftotext writes them.
  ff <- paste(vapply(1:5, function(p) sprintf(paste0(
    "\fSMITH ET AL.\nBody of page %d with a value\n12\nand more on page %d.\n",
    "The page ends with sentence %d."), p, p, p), ""), collapse = "\n")
  o2 <- as.character(gr_clean(ff, steps = "headers_footers"))
  expect_false(grepl("SMITH ET AL.", o2, fixed = TRUE))
  expect_identical(lengths(regmatches(o2, gregexpr("\n12\n", o2, fixed = TRUE))), 5L)
})

# ---------------------------------------------------------------------------
# tokenize-embed-07: gr_new_id's counter was never counted, so two ids made in
# the same millisecond hashed the same input where the clock goes in to the
# second (R before 4.3).
# ---------------------------------------------------------------------------

test_that("two ids made at the same moment differ", {
  st <- readgpt:::gr_state
  now <- as.POSIXct("2026-09-25 10:00:00.123", tz = "UTC")
  before <- st$counter
  a <- readgpt:::gr_new_id("backend", now = now)
  b <- readgpt:::gr_new_id("backend", now = now)
  expect_false(identical(a, b))
  expect_identical(substr(a, 1, 27), substr(b, 1, 27))   # same prefix and millisecond
  expect_equal(st$counter, before + 2)
})

# ---------------------------------------------------------------------------
# r2-non-latin-text-pipeline-07: words hyphenated across a line break were
# rejoined only when both letters beside the hyphen were ASCII. (Fixed by an
# earlier pass; kept here so it stays fixed.)
# ---------------------------------------------------------------------------

test_that("words split across a line break are rejoined in any script", {
  hy <- function(x) as.character(gr_clean(x, steps = c("hyphenation", "collapse_whitespace")))
  expect_identical(hy("le r\u00e9sultat g\u00e9n\u00e9-\nral \u00e9tait"),
                   "le r\u00e9sultat g\u00e9n\u00e9ral \u00e9tait")
  expect_identical(hy("die Wasser-\n\u00fcbertragung war"), "die Wasser\u00fcbertragung war")
  expect_identical(hy("\u0438\u0441\u0441\u043b\u0435-\n\u0434\u043e\u0432\u0430\u043d\u0438\u0435"),
                   "\u0438\u0441\u0441\u043b\u0435\u0434\u043e\u0432\u0430\u043d\u0438\u0435")
  expect_identical(hy("\u03b7 \u03bc\u03b5\u03bb\u03ad-\n\u03c4\u03b7"),
                   "\u03b7 \u03bc\u03b5\u03bb\u03ad\u03c4\u03b7")
})

# ---------------------------------------------------------------------------
# r2-non-latin-text-pipeline-03: sentences_of never split Cyrillic, Greek,
# Arabic or CJK text, nor before a capital with an accent.
# ---------------------------------------------------------------------------

test_that("sentences are split in every script, not only before an ASCII capital", {
  so <- function(x) readgpt:::sentences_of(x)
  # Before: 1 sentence each (French: 1 of 3).
  expect_length(so("Le patient est venu. \u00c0 ce moment il allait bien. \u00c9tat stable."), 3L)
  expect_length(so(paste0("\u041f\u0435\u0440\u0432\u043e\u0435. \u0412\u0442\u043e\u0440\u043e\u0435. ",
                          "\u0422\u0440\u0435\u0442\u044c\u0435.")), 3L)
  expect_length(so("\u03a0\u03c1\u03ce\u03c4\u03b7. \u0394\u03b5\u03cd\u03c4\u03b5\u03c1\u03b7."), 2L)
  expect_length(so("\u0645\u0631\u062d\u0628\u0627. \u0643\u064a\u0641 \u062d\u0627\u0644\u0643\u061f \u0623\u0646\u0627 \u0628\u062e\u064a\u0631."), 3L)
  expect_identical(so("\u6cbb\u7597\u7ec4\u4e0b\u964d\u4e8612%\u3002\u5206\u6790\u9884\u5148\u89c4\u5b9a\u3002"),
                   c("\u6cbb\u7597\u7ec4\u4e0b\u964d\u4e8612%\u3002", "\u5206\u6790\u9884\u5148\u89c4\u5b9a\u3002"))
  # A closing bracket after the full stop stays with its sentence.
  expect_identical(so("\u4ed6\u8bf4\uff1a\u300c\u597d\u3002\u300d\u7136\u540e\u8d70\u4e86\u3002"),
                   c("\u4ed6\u8bf4\uff1a\u300c\u597d\u3002\u300d", "\u7136\u540e\u8d70\u4e86\u3002"))
  # English is split as before: abbreviations, decimals and lower case do not end one.
  expect_identical(so("Dr. Smith saw 3.5 patients, e.g. two. Then left. the end? Yes."),
                   c("Dr. Smith saw 3.5 patients, e.g. two.", "Then left. the end?", "Yes."))

  # Through the sentence segmenter: a Russian report's chunks end at the ends
  # of sentences (before: 24 of 30 ended inside one).
  ru <- paste(rep(paste0("\u0412 \u0438\u0441\u0441\u043b\u0435\u0434\u043e\u0432\u0430\u043d\u0438\u0435 ",
                         "\u0432\u043a\u043b\u044e\u0447\u0438\u043b\u0438 120 \u043f\u0430\u0446\u0438\u0435\u043d\u0442\u043e\u0432 ",
                         "\u0432 \u0447\u0435\u0442\u044b\u0440\u0451\u0445 \u0446\u0435\u043d\u0442\u0440\u0430\u0445. ",
                         "\u0421\u043c\u0435\u0440\u0442\u043d\u043e\u0441\u0442\u044c \u0441\u043d\u0438\u0437\u0438\u043b\u0430\u0441\u044c ",
                         "\u043d\u0430 12 \u043f\u0440\u043e\u0446\u0435\u043d\u0442\u043e\u0432."), 20), collapse = " ")
  ch <- quiet(gr_segment(gr_ingest(ru, cache = FALSE), list(method = "sentence", max_tokens = 80)))
  expect_gt(nrow(ch$chunks), 1L)
  expect_true(all(grepl("\\.$", trimws(ch$chunks$text))))
})

# ---------------------------------------------------------------------------
# r3-real-pdf-two-column-layout-07: the hyphenation step deleted the hyphen of
# a compound broken at a line end ("placebo-" / "controlled").
# ---------------------------------------------------------------------------

test_that("a compound the document writes with its hyphen keeps it across a line break", {
  b <- c("This placebo-controlled trial had follow-up at 12 months and self-reported outcomes.",
         paste0("Participants in the placebo-\ncontrolled arm, in the follow-\nup period, gave self-\n",
                "reported data on mito-\nchondria."))
  out <- as.character(gr_clean(b, steps = "hyphenation"))
  # Before: "placebocontrolled", "followup", "selfreported".
  expect_identical(out[2], paste0("Participants in the placebo-controlled arm, in the follow-up ",
                                  "period, gave self-reported data on mitochondria."))
  expect_identical(out[1], b[1])

  # So a faithful quotation of the default ingest checks out.
  d <- quiet(gr_ingest(paste(b, collapse = "\n\n"), cache = FALSE))
  expect_true(r6_verified("Participants in the placebo-controlled arm, in the follow-up period",
                          d$blocks$text[2]))
  # In capitals too, when the document writes the compound.
  expect_identical(as.character(gr_clean(c("THE PLACEBO-CONTROLLED ARM",
                                           "IN THE PLACEBO-\nCONTROLLED ARM OF THE TRIAL"),
                                         steps = "hyphenation"))[2],
                   "IN THE PLACEBO-CONTROLLED ARM OF THE TRIAL")
  # A compound the document never writes whole is still joined (a documented
  # limitation), and a split word is joined as before.
  expect_identical(readgpt:::rejoin_hyphenated("the investigator-\nblinded arm"),
                   "the investigatorblinded arm")
  expect_identical(readgpt:::rejoin_hyphenated("a-\nb-\nc and mito-\nchondria"),
                   "abc and mitochondria")
})

# ---------------------------------------------------------------------------
# r3-real-pdf-two-column-layout-09 and handoff read-4: the ligatures step
# wrote the em dash as "--" and the en dash as "-", so a quotation that kept
# the source's dash failed, and the boundary test read "all-was" as a compound.
# ---------------------------------------------------------------------------

test_that("en and em dashes are kept, so quotations of them check out", {
  d <- quiet(gr_ingest(paste0("The effects\u2014though modest\u2014were consistent across every ",
                              "prespecified subgroup examined in this trial. Follow-up lasted 12 ",
                              "months\u2026 and ended in 2025."), cache = FALSE))
  # Before: verified FALSE, match 0.625.
  expect_true(r6_verified("The effects\u2014though modest\u2014were consistent", d$text))
  expect_true(r6_verified("The effects-though modest-were consistent", d$text))
  expect_true(r6_verified("lasted 12 months\u2026 and", d$text))

  d2 <- quiet(gr_ingest(paste0("The cohort\u2013482 participants in all\u2013was followed for 3\u20135 ",
                               "years. Values rose 20\u201330 percent."), cache = FALSE))
  expect_true(r6_verified("482 participants in all", d2$text))
  expect_true(r6_verified("3-5 years", d2$text))
  # And a range is still a range: its second number is not a figure of its own.
  expect_false(r6_verified("5 years", d2$text))
  expect_false(r6_verified("30 percent", d2$text))

  # Ligatures and smart quotes are still straightened.
  expect_identical(as.character(gr_clean("\ufb01nal \u201cword\u201d", steps = "ligatures")),
                   "final \"word\"")
})

# ---------------------------------------------------------------------------
# cache-trace-13: the cache and replay keys joined message parts with an
# unescaped U+0002, so differently split prompts collided.
# ---------------------------------------------------------------------------

test_that("a message holding the key separator does not share a key with two messages", {
  h <- readgpt:::gr_hash
  expect_false(identical(h(c("user", "Say A", "user", "Say B")),
                         h(c("user", "Say A\u0002user\u0002Say B"))))
  expect_false(identical(h(list("a", "b")), h(list("a\u0001/2=character:b"))))
  expect_false(identical(h("x\u0003"), h("x\u0003\u0003")))
  # Keys of ordinary text are what they were, so saved caches and recordings
  # are still found.
  expect_identical(h("abc"), "1c690dc1d483121e")
  expect_identical(h(c("user", "Say A", "user", "Say B")), "d0c71ca023983cd4")

  dir <- withr::local_tempdir()
  cl <- gr_cache_client(gr_mock_client(function(m, p) {
    paste(length(m), "message(s):", paste(vapply(m, `[[`, "", "content"), collapse = " | "))
  }), gr_cache(dir))
  a <- quiet(gr_call(cl, list(list(role = "user", content = "Say A"),
                              list(role = "user", content = "Say B"))))
  b <- quiet(gr_call(cl, list(list(role = "user", content = "Say A\u0002user\u0002Say B"))))
  expect_false(isTRUE(b$cached))
  expect_match(b$text, "^1 message")
})

# ---------------------------------------------------------------------------
# state-concurrency-12: the in-memory document cache grew without bound.
# ---------------------------------------------------------------------------

test_that("the document cache drops the documents used least recently past its budget", {
  local_clean_cache()
  put <- function(k, x) readgpt:::doc_cache_put(k, list(x = strrep(x, 1e5)), budget = 3.5e5)
  for (i in 1:5) put(paste0("k", i), letters[i])
  expect_identical(ls(readgpt:::gr_state$doc_cache), c("k3", "k4", "k5"))
  expect_false(is.null(readgpt:::doc_cache_get("k3")))    # k3 used again
  put("k6", "f")
  expect_identical(ls(readgpt:::gr_state$doc_cache), c("k3", "k5", "k6"))
  # One document over the budget is still kept, alone.
  readgpt:::doc_cache_put("big", list(x = strrep("z", 5e5)), budget = 3.5e5)
  expect_identical(ls(readgpt:::gr_state$doc_cache), "big")

  # gr_ingest() still caches and serves from it.
  d1 <- quiet(gr_ingest("A document with enough text to be kept in the cache."))
  expect_true(length(ls(readgpt:::gr_state$doc_cache)) >= 1L)
  expect_identical(quiet(gr_ingest("A document with enough text to be kept in the cache.")), d1)
})

# ---------------------------------------------------------------------------
# tokenize-embed-12: bytes that are not UTF-8 crashed the helpers that only
# ask whether there is any text.
# ---------------------------------------------------------------------------

test_that("the blank-text helpers do not stop on bytes that are not UTF-8", {
  bad <- rawToChar(as.raw(c(0x63, 0x61, 0x66, 0xe9, 0x20, 0x71)))
  skip_if(validUTF8(bad))
  # Before: "input string 1 is invalid UTF-8".
  expect_true(readgpt:::is_nonblank(bad))
  expect_identical(readgpt:::has_content(c(bad, " ", "")), c(TRUE, FALSE, FALSE))
  expect_length(readgpt:::words_of(bad), 2L)
})

# ---------------------------------------------------------------------------
# r2-non-latin-text-pipeline-13: a line of no-break or ideographic spaces did
# not separate paragraphs.
# ---------------------------------------------------------------------------

test_that("a line of no-break or ideographic spaces is a blank line", {
  p1 <- "First paragraph here is long enough to keep."
  p2 <- "Second paragraph here is long enough to keep."
  for (sep in c("\n\u3000\u3000\n", "\n\u00a0\n", "\n \u00a0\t\n")) {
    d <- quiet(gr_ingest(paste0(p1, sep, p2), cache = FALSE))
    expect_identical(d$blocks$text, c(p1, p2))    # before: one block
  }
  expect_identical(readgpt:::paragraphs_of(paste0(p1, "\n\u3000\n\u3000", p2)), c(p1, p2))
  expect_identical(readgpt:::has_content(c("\u00a0\u3000", "\u2003x")), c(FALSE, TRUE))
  expect_false(readgpt:::is_nonblank("\u3000"))
  # Inside a line the spaces stay: "1<no-break space>200" is one number.
  out <- as.character(gr_clean("A total of 1\u00a0200 patients.\n\u00a0\u00a0\nNext."))
  expect_identical(out, "A total of 1\u00a0200 patients.\n\nNext.")
  expect_false(r6_verified("200 patients", out))
})

# ---------------------------------------------------------------------------
# r3-scale-at-review-sizes-06: document-scoped steps searched the whole
# document once per block. (Fixed by an earlier pass; kept here.)
# ---------------------------------------------------------------------------

test_that("document-scoped cleaning takes time in proportion to the document", {
  skip_on_cran()
  mk <- function(n) {
    b <- sprintf("Paragraph %d of the caf\u00e9 report says the value was %d\nand goes on here.",
                 seq_len(n), seq_len(n) %% 97L)
    b[round(n * 0.85)] <- "References"
    b
  }
  timed <- function(n) system.time(gr_clean(mk(n), steps = c("references", "headers_footers")))[["elapsed"]]
  small <- max(timed(1000L), 0.02)
  # Eight times the blocks. Before: about 50 times the time.
  expect_lt(timed(8000L) / small, 20)
})

# ---------------------------------------------------------------------------
# handoff cross-8: lower_text() read every character twice, and in a locale
# that is not UTF-8 took most of a second over 80,000 Chinese characters.
# ---------------------------------------------------------------------------

test_that("lower_text lowers a long string one distinct code point at a time, to the same result", {
  set.seed(3)
  cps <- sample(c(65:90, 97:122, 0xC0:0x24F, 0x370:0x3FF, 0x400:0x4FF, 0x531:0x58F,
                  0x1E00:0x1EFF, 0x2160:0x2188, 0x24B6:0x24E9, 0x4E00:0x4E80, 0x10400:0x1044F,
                  0x130), 5000, TRUE)
  s <- intToUtf8(cps)
  expect_identical(readgpt:::lower_text(s), readgpt:::lower_chars(s))
  expect_identical(readgpt:::lower_text(c(a = "X", b = NA, c = s)),
                   c(a = "x", b = NA, c = readgpt:::lower_chars(s)))
  long <- strrep("\u6cbb\u7597\u7ec4\u6b7b\u4ea1\u7387\u4e0b\u964d", 10000L)
  expect_identical(readgpt:::lower_text(long), long)
  expect_lt(system.time(readgpt:::lower_text(long))[["elapsed"]], 0.2)
})
