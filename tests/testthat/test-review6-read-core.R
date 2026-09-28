# test-review6-read-core.R -- the sixth pass over the reading core: the
# not-found sentinel followed by an explanation or in another script's
# punctuation, evidence pages given to per-chunk answers and found inside
# words, one answer's record carrying the other runs of a shared trace,
# quotation matching across the ways a letter is encoded and in French and
# CJK quote marks, page labels that are not whole numbers, and merge prompts
# sized without their tags.
#
# Each block names the finding and says what the old behaviour was.

sm <- function(span, src, ...) readgpt:::span_match(span, src, ...)
verified <- function(span, src, ...) isTRUE(sm(span, src, ...)$verified)
manual_chunks <- function(texts, ...) {
  new_chunks(texts, "manual", gr_segment_spec(max_tokens = 100), ...)
}

# ---------------------------------------------------------------------------
# read-core-07: the sentinel and then an explanation was an answer. The
# test took the sentinel only alone, so map_reduce merged and listed every
# chunk that said "NOT_IN_DOCUMENT. This excerpt does not mention revenue.",
# and an ensemble adjudicated such a member as an answer.
# ---------------------------------------------------------------------------

test_that("the sentinel opening a reply, then explained, is not-found", {
  expect_true(is_not_found("NOT_IN_DOCUMENT. The excerpt only discusses headcount."))
  expect_true(is_not_found("NOT_IN_DOCUMENT\n\nThe excerpt does not mention revenue."))
  expect_true(is_not_found("NOT_IN_DOCUMENT (the excerpt covers costs only)"))
  expect_true(is_not_found("**NOT_IN_DOCUMENT**: the excerpt is about staffing."))
  expect_true(is_not_found("NOT IN DOCUMENT - the excerpt is about staffing."))
  expect_true(is_not_found("NOT_IN_DOCUMENT [chunk 3]"))
  # A real answer that uses the sentinel, or ordinary words that spell it.
  expect_false(is_not_found("The log said NOT_IN_DOCUMENT, but revenue was 45.2 million."))
  expect_false(is_not_found("NOT_IN_DOCUMENT is what the parser printed; revenue was 45.2 million."))
  expect_false(is_not_found("Not in document form; the figures were in a spreadsheet."))
  expect_false(is_not_found("NOT_IN_DOCUMENTS lists what was left out."))
  expect_false(is_not_found("NOT_IN_DOCUMENT_V2 was the field name."))
  expect_false(is_not_found("NOT_IN_DOCUMENT.txt holds the list."))
  expect_false(is_not_found("NOT_IN_DOCUMENT-2 was the code the parser gave."))
})

test_that("map_reduce neither merges nor lists the chunks that explained a non-answer", {
  ch <- manual_chunks(c("Revenue was 45.2 million in 2023.", "Headcount grew to 1,204 staff.",
                        "Costs fell by 3 percent overall.", "The office moved to Leeds in May."))
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("Revenue was", messages[[2]]$content, fixed = TRUE)) return("Revenue was 45.2 million.")
    "NOT_IN_DOCUMENT. This excerpt does not mention revenue."
  })
  a <- quiet(gr_read(ch, "What was revenue?", cl, "map_reduce"))
  # Was: answered 4, chunks_used 1:4, a merge call, and three sentinel rows
  # in the evidence presented as answers.
  expect_identical(a$notes$answered, 1L)
  expect_identical(as.integer(a$chunks_used), 1L)
  expect_false(any(grepl("NOT_IN_DOCUMENT", a$evidence$text, fixed = TRUE)))
  expect_identical(a$trace$calls, 4L)
  expect_identical(a$answer, "Revenue was 45.2 million.")
})

test_that("an ensemble member that explained its non-answer is not adjudicated as one", {
  ch <- manual_chunks(c("Revenue was 45.2 million in 2023.", "Headcount grew to 1,204 staff."))
  cl <- gr_mock_client(function(messages, params) {
    body <- messages[[2]]$content
    if (grepl("[chunk 1", body, fixed = TRUE) && grepl("[chunk 2", body, fixed = TRUE)) {
      return("NOT_IN_DOCUMENT. The excerpts do not say.")
    }
    if (grepl("Revenue was", body, fixed = TRUE)) return("Revenue was 45.2 million.")
    "NOT_IN_DOCUMENT"
  })
  a <- quiet(gr_read(ch, "What was revenue?", cl,
                     gr_read_spec("ensemble", members = c("stuff", "map_reduce"))))
  expect_identical(a$notes$answered_by, "map_reduce")
  expect_identical(a$answer, "Revenue was 45.2 million.")
})

# ---------------------------------------------------------------------------
# r2-non-latin-text-pipeline-09: the sentinels in CJK, guillemet or curly
# punctuation were real answers. Only ASCII decoration was allowed around
# NOT_IN_DOCUMENT, so a Chinese map_reduce whose chunks all replied with an
# ideographic full stop merged them and answered with the sentinel, not
# not-found. (The NONE filter of skim and preview is in read-methods.R; the
# predicate it is handed is tested here.)
# ---------------------------------------------------------------------------

test_that("the sentinels are recognised in the punctuation of other scripts", {
  for (x in c("NOT_IN_DOCUMENT\u3002", "NOT_IN_DOCUMENT\uff01", "\u300cNOT_IN_DOCUMENT\u300d",
              "\u00ab NOT_IN_DOCUMENT \u00bb", "\u00ab\u00a0NOT_IN_DOCUMENT\u00a0\u00bb",
              "\u201cNOT_IN_DOCUMENT\u201d", "\u201eNOT_IN_DOCUMENT\u201c", "NOT_IN_DOCUMENT\uff0e",
              "\u3000NOT_IN_DOCUMENT\u3002",
              "NOT_IN_DOCUMENT\u3002\u8be5\u6458\u5f55\u672a\u63d0\u53ca\u6536\u5165\u3002")) {
    expect_true(is_not_found(x), info = x)
  }
  none <- readgpt:::is_none_reply
  expect_true(all(none(c("NONE", "NONE.", "**None**", "NONE\u3002", "\u300cNONE\u300d",
                         "\u00ab NONE \u00bb", "NONE\uff01", "\u201cNONE.\u201d"))))
  # A passage copied verbatim may open with the word, and is evidence.
  expect_false(any(none(c("None. All patients completed the study.",
                          "None of the patients withdrew.", NA, ""))))
})

test_that("map_reduce over chunks that all reply the sentinel with an ideographic stop is not-found", {
  ch <- manual_chunks(c("\u4eba\u6570\u589e\u52a0\u4e86\u3002", "\u6210\u672c\u4e0b\u964d\u4e86\u3002",
                        "\u529e\u516c\u5ba4\u642c\u8fc1\u4e86\u3002"))
  cl <- gr_mock_client(function(messages, params) "NOT_IN_DOCUMENT\u3002")
  a <- quiet(gr_read(ch, "What was revenue?", cl, "map_reduce"))
  # Was: four calls (a merge of three sentinels) and an answer that was the
  # sentinel, with is_not_found() FALSE.
  expect_true(is_not_found(a$answer))
  expect_identical(a$trace$calls, 3L)
})

# ---------------------------------------------------------------------------
# read-core-08: resolve_evidence_pages() moved per-chunk ANSWER rows, and
# matched inside words. "Yes." from chunk 3 on page 3 was given page 1,
# where "All eyes were on the board" is.
# ---------------------------------------------------------------------------

test_that("a per-chunk answer keeps its chunk's page, and a quotation is placed as whole words", {
  rp <- readgpt:::resolve_evidence_pages
  blocks <- data.frame(text = c("All eyes were on the board meeting in March.",
                                "Staff numbers were stable through the year.",
                                "Revenue rose 12 percent in 2023."),
                       page = 1:3, section = c("Intro", "Staff", "Finance"),
                       stringsAsFactors = FALSE)
  answer <- data.frame(chunk_id = 3L, text = "Yes.", page = 3L, section = "Finance",
                       score = NA_real_, kind = "answer", stringsAsFactors = FALSE)
  expect_identical(rp(answer, blocks)$page, 3L)              # was 1
  inside <- data.frame(chunk_id = 1L, text = "eye", page = NA_integer_, section = NA_character_,
                       kind = "extracted", verified = TRUE, stringsAsFactors = FALSE)
  expect_true(is.na(rp(inside, blocks)$page))                # was 1
  expect_true(is.na(rp(inside, blocks)$section))             # was "Intro"
  # A quotation the check did not find in its chunk is not placed elsewhere.
  unverified <- data.frame(chunk_id = 2L, text = "All eyes were on the board meeting in March.",
                           page = 2L, section = NA_character_, kind = "extracted",
                           verified = FALSE, stringsAsFactors = FALSE)
  expect_identical(rp(unverified, blocks)$page, 2L)          # was 1
  # One in its chunk word for word that does not state the value it was cited
  # for (the extract reader's FALSE, match 1) is a sentence of the document.
  unstated <- unverified
  unstated$match <- 1
  expect_identical(rp(unstated, blocks)$page, 1L)
  # A quotation that is there, whole, still is.
  found <- data.frame(chunk_id = 3L, text = "\"Revenue rose 12 percent.\"", page = NA_integer_,
                      section = NA_character_, kind = "extracted", verified = TRUE,
                      stringsAsFactors = FALSE)
  out <- rp(found, blocks)
  expect_identical(out$page, 3L)
  expect_identical(out$section, "Finance")
})

test_that("a common phrase on many pages is left unplaced, quickly", {
  rp <- readgpt:::resolve_evidence_pages
  blocks <- data.frame(text = sprintf("Block %d: revenue rose 12%% in year %d.", 1:3000, 1:3000),
                       page = rep(1:1000, each = 3), section = NA_character_,
                       stringsAsFactors = FALSE)
  ev <- data.frame(chunk_id = 1:10, text = "revenue rose 12%", page = NA_integer_,
                   section = NA_character_, kind = "extracted", verified = TRUE,
                   stringsAsFactors = FALSE)
  took <- system.time(out <- rp(ev, blocks))[["elapsed"]]
  expect_true(all(is.na(out$page)))
  expect_lt(took, 10)
})

# ---------------------------------------------------------------------------
# r2-export-serialization-fidelity-04: as_json(answer), print() and the audit
# report took the whole of a trace the caller shared across runs, so one
# answer's record carried another document's text in its prompts, and runs
# made after the answer too.
# ---------------------------------------------------------------------------

test_that("one answer's record holds its own run, not every run of a shared trace", {
  cl <- gr_mock_client(function(messages, params) {
    if (any(grepl("CONFIDENTIAL", vapply(messages, `[[`, "", "content"), fixed = TRUE))) {
      stop("gateway refused")
    }
    "It says so."
  })
  tr <- gr_trace()
  a1 <- quiet(answer_document("Public memo: revenue was 45.2 million in 2023.", "What was revenue?",
                              "fast", client = cl, trace = tr))
  n1 <- length(tr$steps)
  calls1 <- tr$calls
  a2 <- quiet(answer_document("CONFIDENTIAL patient Jane Roe, MRN 00123, diagnosed with X.",
                              "What was the diagnosis?", "fast", client = cl, trace = tr))
  expect_gt(length(tr$steps), n1)
  expect_gt(length(tr$errors), 0L)
  j <- as_json(a1)
  # Was: the second run's prompt, patient name and all, and its error.
  expect_false(grepl("Jane Roe", j, fixed = TRUE))
  got <- jsonlite::fromJSON(j, simplifyVector = FALSE)$trace
  expect_length(got$steps, n1)
  expect_identical(got$summary$calls, calls1)
  expect_length(got$errors, 0L)
  out <- capture.output(print(a1))
  expect_true(any(grepl(sprintf("^  %d model call\\(s\\).*, 0 error\\(s\\)", calls1), out)))
  expect_false(a1$partial)
  # The shared trace itself still holds both runs.
  expect_true(grepl("Jane Roe", as_json(tr), fixed = TRUE))
})

test_that("an answer that records where its run started reports only from there", {
  # The field gr_read() and answer_document() set: steps `first` to `last`.
  cl <- gr_mock_client(function(messages, params) "It says so.")
  tr <- gr_trace()
  quiet(answer_document("Public memo: revenue was 45.2 million in 2023.", "What was revenue?",
                        "fast", client = cl, trace = tr))
  n1 <- length(tr$steps)
  a2 <- quiet(answer_document("Second memo: costs fell 3 percent.", "What happened to costs?",
                              "fast", client = cl, trace = tr))
  a2$trace_steps[["first"]] <- n1 + 1L
  j <- as_json(a2)
  expect_false(grepl("Public memo", j, fixed = TRUE))
  got <- jsonlite::fromJSON(j, simplifyVector = FALSE)$trace
  expect_length(got$steps, length(tr$steps) - n1)
  expect_identical(got$summary$calls, sum(vapply(tr$steps[-seq_len(n1)], function(s)
    !identical(s$kind, "local"), logical(1))))
  # A trace made for the one answer is passed through whole.
  own <- quiet(answer_document("Third memo: the budget for next year is unchanged.",
                              "What is the budget?", "fast", client = cl))
  expect_identical(readgpt:::answer_trace(own), own$trace)
})

# ---------------------------------------------------------------------------
# r2-non-latin-text-pipeline-05: no Unicode normalisation. A decomposed
# source (a letter and its combining accent), full-width digits and the
# ideographic space never matched the quotation a model writes, so faithful
# quotes were unverified and require_quote dropped their values.
# ---------------------------------------------------------------------------

test_that("a quotation verifies across the ways a letter can be encoded", {
  nfc <- "Nous avons randomis\u00e9 120 patients dans cette \u00e9tude."
  nfd <- "Nous avons randomise\u0301 120 patients dans cette e\u0301tude."
  expect_true(verified(nfc, nfd))                            # was FALSE, 0.5
  expect_true(verified(nfd, nfc))
  # Vietnamese: the horn, and two accents on one letter.
  vi_nfc <- "Ng\u01b0\u1eddi b\u1ec7nh \u0111\u01b0\u1ee3c theo d\u00f5i."
  vi_nfd <- "Ngu\u031bo\u031b\u0300i be\u0323\u0302nh \u0111u\u031bo\u031b\u0323c theo do\u0303i."
  expect_true(verified(vi_nfc, vi_nfd))
  # Korean jamo, Russian short i and yo, Greek tonos, Romanian comma below.
  expect_true(verified("\ud55c\uad6d 120\uba85", "\u1112\u1161\u11ab\u1100\u116e\u11a8 120\u1106\u1167\u11bc\uc774 \uc788\ub2e4."))
  expect_true(verified("\u043a\u043e\u0440\u043e\u0442\u043a\u0438\u0439 \u043e\u0442\u0447\u0451\u0442",
                       "\u043a\u043e\u0440\u043e\u0442\u043a\u0438\u0438\u0306 \u043e\u0442\u0447\u0435\u0308\u0442 \u043e 120"))
  expect_true(verified("\u03b7 \u03bc\u03b5\u03bb\u03ad\u03c4\u03b7", "\u0397 \u03bc\u03b5\u03bb\u03b5\u0301\u03c4\u03b7 120."))
  expect_true(verified("\u021bara", "Studiul din t\u0326ara noastr\u0103."))
  # Full-width digits and letters, and the ideographic space.
  expect_true(verified("\u672c\u8a66\u9a13\u3067\u306f120\u4f8b\u3092\u7121\u4f5c\u70ba\u5316\u3057",
                       "\u672c\u8a66\u9a13\u3067\u306f\uff11\uff12\uff10\u4f8b\u3092\u7121\u4f5c\u70ba\u5316\u3057\u3001\u8ffd\u8de1\u3057\u305f\u3002"))
  expect_true(verified("the HbA1c target", "We set the \uff28\uff42\uff21\uff11\uff43 target."))
  expect_true(verified("Revenue rose to 45.2 million", "Revenue\u3000rose to 45.2 million in 2023."))
  expect_true(verified("Revenue rose to 45.2 million", "Revenue\u2002rose to 45.2\u200amillion."))
})

test_that("normalising letters does not let a changed quotation through", {
  # Accents still tell letters apart, composed or not.
  expect_false(verified("el ano pasado", "En el an\u0303o pasado subieron."))
  expect_false(verified("randomis\u00e9 121 patients", "Nous avons randomise\u0301 120 patients."))
  # A full-width minus sign, decimal point or thousands separator is one to
  # the whole-number test. Before, "5%" verified against a full-width "-5%",
  # and a full-width "45" against "45.2" written full width.
  expect_false(verified("5% in 2023", "Margins moved by \uff0d5% in 2023."))
  expect_false(verified("of \uff14\uff15", "Revenue of \uff14\uff15\uff0e\uff12 million."))
  expect_false(verified("200 patients", "We enrolled 1\uff0c200 patients."))
  expect_true(verified("of 45.2 million", "Revenue of \uff14\uff15\uff0e\uff12 million."))
})

test_that("an extraction keeps a value quoted from a decomposed or full-width source", {
  one <- function(text, quote) {
    f <- tempfile(fileext = ".txt")
    writeBin(charToRaw(enc2utf8(paste0("Background about the trial design.\n\n", text, "\n"))), f)
    fl <- gr_fields(n = gr_field("Participants randomised", type = "integer"))
    js <- as.character(jsonlite::toJSON(list(n = 120, n__quote = quote), auto_unbox = TRUE))
    cl <- gr_mock_client(function(messages, params) js)
    quiet(gr_extract(f, fl, client = cl, recipe = "fast", require_quote = TRUE))
  }
  fr <- one("Nous avons randomise\u0301 120 patients dans deux centres.",
            "Nous avons randomis\u00e9 120 patients")
  expect_equal(fr$table$n, 120)                              # was NA
  expect_identical(fr$table$n_unverified, 0L)
  ja <- one("\u672c\u8a66\u9a13\u3067\u306f\uff11\uff12\uff10\u4f8b\u3092\u7121\u4f5c\u70ba\u5316\u3057\u305f\u3002",
            "\u672c\u8a66\u9a13\u3067\u306f120\u4f8b\u3092\u7121\u4f5c\u70ba\u5316\u3057\u305f")
  expect_equal(ja$table$n, 120)                              # was NA
})

# ---------------------------------------------------------------------------
# r2-non-latin-text-pipeline-06: guillemets, corner brackets and the
# ideographic full stop stayed on a quotation, so faithful French and
# Japanese quotes were unverified where the same quote in English quote
# marks verified.
# ---------------------------------------------------------------------------

test_that("French and CJK quote marks and full stops around a quotation are typography", {
  fr <- "Au total, nous avons randomis\u00e9 120 patients dans deux centres."
  expect_true(verified("\u00ab nous avons randomis\u00e9 120 patients \u00bb", fr))      # was 0.714
  expect_true(verified("\u00ab\u00a0nous avons randomis\u00e9 120 patients\u00a0\u00bb", fr))
  expect_true(verified("\u2039nous avons randomis\u00e9 120 patients\u203a", fr))
  ja <- "\u672c\u8a66\u9a13\u3067\u306f120\u4f8b\u3092\u7121\u4f5c\u70ba\u5316\u3057\u3001\u8ffd\u8de1\u3057\u305f\u3002"
  expect_true(verified("\u300c\u672c\u8a66\u9a13\u3067\u306f120\u4f8b\u3092\u7121\u4f5c\u70ba\u5316\u3057\u300d", ja))
  expect_true(verified("\u300e\u672c\u8a66\u9a13\u3067\u306f120\u4f8b\u3092\u7121\u4f5c\u70ba\u5316\u3057\u300f", ja))
  expect_true(verified("\u672c\u8a66\u9a13\u3067\u306f120\u4f8b\u3092\u7121\u4f5c\u70ba\u5316\u3057\u3002", ja))
  expect_true(verified("\u672c\u8a66\u9a13\u3067\u306f120\u4f8b\u3092\u7121\u4f5c\u70ba\u5316\u3057\uff0c", ja))
  expect_true(verified("\u201cWe randomised 120 patients.\u201d", "We randomised 120 patients in two centres."))
  # The content inside them is still checked.
  expect_false(verified("\u00ab nous avons randomis\u00e9 210 patients \u00bb", fr))
  expect_false(verified("\u300c\u672c\u8a66\u9a13\u3067\u306f210\u4f8b\u3092\u7121\u4f5c\u70ba\u5316\u3057\u300d", ja))
})

# ---------------------------------------------------------------------------
# read-core-13: render_chunks() formatted the page with "%d", so a custom
# segmenter that recorded page labels or fractional pages made every reader
# fail with a bare sprintf() error.
# ---------------------------------------------------------------------------

test_that("chunks whose pages are labels or fractions are read, and the prompt names them", {
  seen <- character(0)
  cl <- gr_mock_client(function(messages, params) {
    seen <<- c(seen, messages[[2]]$content)
    "Revenue was 45.2 million."
  })
  ch <- manual_chunks(c("Revenue was 45.2 million.", "Costs fell."), page = c("iv", "1"))
  a <- quiet(gr_read(ch, "What was revenue?", cl, "stuff"))  # was an error
  expect_identical(a$answer, "Revenue was 45.2 million.")
  expect_true(any(grepl("[chunk 1 p.iv]", seen, fixed = TRUE)))
  ch2 <- manual_chunks(c("Revenue was 45.2 million.", "Costs fell."), page = c(1.5, 2))
  a2 <- quiet(gr_read(ch2, "What was revenue?", cl, "map_reduce"))
  expect_false(a2$partial)
  expect_true(any(grepl("[chunk 1 p.1.5]", seen, fixed = TRUE)))
  expect_true(any(grepl("[chunk 2 p.2]", seen, fixed = TRUE)))
  expect_identical(readgpt:::page_text(c(3, 1e5, NA)), c("3", "100000", NA))
  expect_identical(readgpt:::page_text(7L), "7")
})

# ---------------------------------------------------------------------------
# read-core-12: tree_merge() packed findings by their bare size, while each
# goes out in "<findings i>" tags. 300 one-word findings counted 1,200
# tokens against a 2,145-token budget, went out as a 3,501-token prompt,
# and gr_call() refused it for a 3,000-token window.
# ---------------------------------------------------------------------------

test_that("merge groups are sized with the tags each finding is sent in", {
  quiet(gr_register_model("review6-merge-ctx", context_window = 3000L, max_output = 500L,
                          input_usd = 0, output_usd = 0))
  cl <- gr_mock_client(function(messages, params) "Merged answer.")
  spec <- gr_read_spec("map_reduce", model = "review6-merge-ctx", max_answer_tokens = 300L)
  tr <- gr_trace()
  res <- quiet(readgpt:::tree_merge(cl, "Did revenue rise in 2023?", rep("Yes.", 300), spec, tr))
  expect_true(res$ok)                                        # was FALSE
  expect_length(tr$errors, 0L)
  sent <- vapply(tr$steps, function(s)
    sum(gr_count_tokens(vapply(s$prompt, function(m) m$content, character(1)))), numeric(1))
  expect_true(all(sent + 300 <= 3000))
})
