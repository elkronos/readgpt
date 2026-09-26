# test-review2-synth.R -- the write-up's second pass: which model a synthesis
# sizes for, replies cut off at the reply limit, citations the check cannot
# read, and a reference list that sorts the same on every machine.
#
# Every test here failed on the code before its fix, and says what the old
# behaviour was.

r2s_table <- function(n = 3L, finding = c("supports", "contradicts", "supports")) {
  data.frame(document = sprintf("d%02d.pdf", seq_len(n)), document_id = sprintf("h%02d", seq_len(n)),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_,
             design = rep_len(c("randomised trial", "cross-sectional", "randomised trial"), n),
             finding = rep_len(finding, n), stringsAsFactors = FALSE)
}

# Forty studies of about 130 tokens each: one prompt on a large model, several
# on a 4096-token one.
r2s_big <- function(n = 40L) {
  r2s_table(n, finding = paste(rep("The intervention reduced the outcome modestly in adults.", 9),
                               collapse = " "))
}

# A reply the provider reported as stopped at the limit, spelled as the default
# Responses API spells it; gr_result() normalises it to "length".
r2s_cut <- function(text) readgpt:::gr_result(TRUE, text, finish_reason = "incomplete")

# The warnings a call raised, by class, with the value.
r2s_warns <- function(expr) {
  w <- list()
  val <- withCallingHandlers(suppressMessages(expr), warning = function(c) {
    w[[length(w) + 1L]] <<- c
    invokeRestart("muffleWarning")
  })
  list(value = val, classes = unlist(lapply(w, class)),
       first = vapply(w, function(c) class(c)[1], character(1)),
       messages = vapply(w, conditionMessage, character(1)))
}

# ---------------------------------------------------------------------------
# client-01: the client's model sizes the write-up as well as answering it
# ---------------------------------------------------------------------------

test_that("with no model named, the write-up sizes for the model the client asks", {
  # Before: gr_read_spec() left the model NULL, gr_budget(NULL) sized for
  # gr_options("model") and gr_call(model = NULL) asked the client's. On a
  # 4096-token client the forty studies went out as one 5173-token prompt, the
  # call failed before it was sent, and the section came back empty; gr_claims()
  # sent batches of 30 studies (4039 prompt tokens, room for a 25-token reply);
  # and the 'cut' pass kept a whole-draft rewrite the model cannot emit.
  local_registries()
  gr_register_model("tiny-local", context_window = 4096, max_output = 512,
                    input_usd = 0, output_usd = 0)
  seen <- new.env(parent = emptyenv())
  seen$p <- list()
  cl <- gr_backend_client(function(messages, params) {
    seen$p[[length(seen$p) + 1L]] <- params[c("model", "prompt_tokens")]
    sys <- messages[[1]]$content
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      id <- regmatches(messages[[3]]$content, regexpr("(?<=\\[study )[0-9]+", messages[[3]]$content,
                                                      perl = TRUE))
      return(sprintf(paste0('{"claims":[{"claim":"It helps.","kind":"finding","supported_by":[%s],',
                            '"contradicted_by":[],"moderator":null,"scope":null}]}'), id))
    }
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return('{"sections":[{"heading":"Findings","brief":"t","claims":[1],"rationale":null}]}')
    }
    if (grepl("<draft>", messages[[length(messages)]]$content, fixed = TRUE)) return("Revised [study 1].")
    paste(rep("A modest reduction was reported [study 1].", 60), collapse = " ")
  }, model = "tiny-local")
  models <- function() unique(vapply(seen$p, `[[`, "", "model"))
  prompts <- function() vapply(seen$p, function(p) as.numeric(p$prompt_tokens), 0)

  s <- quiet(gr_synthesise(r2s_big(), outline = c(Findings = "what"), question = "Q?", client = cl))
  expect_length(s$trace$errors, 0L)
  expect_false(s$sections$partial)
  expect_identical(models(), "tiny-local")
  expect_true(all(prompts() < 4096))
  expect_true(length(seen$p) > 1L)          # written in batches and merged

  seen$p <- list()
  cm <- quiet(gr_claims(r2s_big(), question = "Q?", client = cl))
  expect_length(cm$trace$errors, 0L)
  expect_identical(models(), "tiny-local")
  # Batches sized for the client's 512-token reply, not the default model's.
  expect_true(all(prompts() < 1024))
  seen$p <- list()
  o <- quiet(gr_outline(cm, client = cl))
  expect_identical(models(), "tiny-local")

  # The revision passes size for the same model: a 700-token draft cannot come
  # back whole from a model that emits 512, so the pass is skipped, naming it.
  r <- r2s_warns(gr_synthesise(r2s_table(), outline = c(Findings = "what"), question = "Q?",
                               client = cl, coherence = "cut"))
  expect_false(r$value$coherence$ran)
  expect_identical(r$value$coherence$reason, "draft exceeds the output limit")
  expect_true("gr_coherence_skipped" %in% r$classes)
  expect_match(r$messages[r$first == "gr_coherence_skipped"], "'tiny-local' can emit at most 512",
               fixed = TRUE)
})

test_that("a model named on the call still wins over the client's", {
  local_registries()
  gr_register_model("tiny-local", context_window = 4096, max_output = 512,
                    input_usd = 0, output_usd = 0)
  gr_register_model("house-a", context_window = 64000, max_output = 4000,
                    input_usd = 0, output_usd = 0)
  seen <- character(0)
  cl <- gr_backend_client(function(messages, params) {
    seen <<- c(seen, params$model)
    "A modest reduction was reported [study 1]."
  }, model = "tiny-local")
  s <- quiet(gr_synthesise(r2s_big(), outline = c(Findings = "what"), question = "Q?",
                           client = cl, model = "house-a"))
  expect_identical(unique(seen), "house-a")
  # Sized for house-a's window too: the forty studies fit one prompt there.
  expect_length(seen, 1L)
  expect_false(s$sections$partial)
})

# ---------------------------------------------------------------------------
# model-output-07: a reply cut off at the limit is not a finished section
# ---------------------------------------------------------------------------

test_that("a section whose reply stopped at the reply limit is partial and says so", {
  # Before: gr_call() keeps a cut-off reply ok = TRUE, usable_text() accepted
  # it, and "However, the trial" came back with partial = FALSE and no warning.
  cl <- gr_mock_client(function(m, p) r2s_cut("One trial found a benefit [study 1]. However, the trial"))
  r <- r2s_warns(gr_synthesise(r2s_table(), outline = c(Findings = "what"), question = "Q?",
                               client = cl))
  s <- r$value$sections
  expect_identical(s$n_truncated, 1L)
  expect_identical(s$n_cited, 1L)
  expect_true(s$partial)
  expect_true("gr_synth_truncated" %in% r$classes)
  expect_match(r$messages[r$first == "gr_synth_truncated"], "1200-token reply limit", fixed = TRUE)
  expect_output(print(r$value), "CUT OFF AT THE REPLY LIMIT")

  # A reply that finished is not flagged.
  ok <- quiet(gr_synthesise(r2s_table(), outline = c(Findings = "what"), question = "Q?",
                            client = gr_mock_client(function(m, p) "One trial found a benefit [study 1].")))
  expect_identical(ok$sections$n_truncated, 0L)
  expect_false(ok$sections$partial)
})

test_that("a batch draft or a merge cut off at the limit marks the section partial", {
  local_registries()
  # A small window, so forty studies are drafted in batches and merged.
  gr_register_model("small-merge", context_window = 3000, max_output = 400,
                    input_usd = 0, output_usd = 0)
  merge_msg <- function(m) grepl("<draft", paste(vapply(m, `[[`, "", "content"), collapse = " "),
                                 fixed = TRUE)
  run <- function(cl) r2s_warns(gr_synthesise(r2s_big(), outline = c(Findings = "what"),
                                              question = "Q?", client = cl, model = "small-merge",
                                              max_section_tokens = 300))

  # Before: the merge reply "... [study 1] but" was the section, partial FALSE.
  r <- run(gr_mock_client(function(m, p) {
    if (merge_msg(m)) r2s_cut("Merged: a modest benefit [study 1] but") else "Draft: a benefit [study 1]."
  }))
  labels <- vapply(r$value$trace$steps, `[[`, "", "label")
  expect_true(all(c("synthesise.batch", "synthesise.merge") %in% labels))
  expect_identical(r$value$sections$n_truncated, 1L)
  expect_true(r$value$sections$partial)
  expect_true("gr_synth_truncated" %in% r$classes)

  # Before: every batch draft cut off, merged as if whole, partial FALSE.
  r <- run(gr_mock_client(function(m, p) {
    if (merge_msg(m)) "Merged: a modest benefit [study 1]." else r2s_cut("Draft: a benefit [study 1] and")
  }))
  n_batches <- sum(vapply(r$value$trace$steps, `[[`, "", "label") == "synthesise.batch")
  expect_identical(r$value$sections$n_truncated, as.integer(n_batches))
  expect_true(r$value$sections$partial)
})

test_that("batch drafts that could not be merged are not a finished section", {
  # Before: every batch was drafted, the merge call failed, and the section was
  # the drafts joined and capped -- with "[merge failed ...]" in it or not --
  # and partial = FALSE.
  local_registries()
  gr_register_model("small-merge", context_window = 3000, max_output = 400,
                    input_usd = 0, output_usd = 0)
  cl <- gr_mock_client(function(m, p) {
    if (grepl("<draft", paste(vapply(m, `[[`, "", "content"), collapse = " "), fixed = TRUE)) {
      stop("merge endpoint down")
    }
    "Draft: a modest benefit [study 1]."
  })
  r <- r2s_warns(gr_synthesise(r2s_big(), outline = c(Findings = "what"), question = "Q?",
                               client = cl, model = "small-merge", max_section_tokens = 300))
  expect_true(r$value$sections$partial)
  expect_true("gr_synth_merge_failed" %in% r$classes)
  expect_match(paste(r$messages, collapse = " "), "merge endpoint down", fixed = TRUE)
  # Not also reported as lost batches: every batch was read.
  expect_false("gr_synth_batch_failed" %in% r$classes)
})

test_that("the closing section, written from the gaps, is partial when its reply is cut off", {
  tab <- r2s_table()
  cl <- gr_mock_client(function(m, p) {
    sys <- m[[1]]$content
    body <- paste(vapply(m, `[[`, "", "content"), collapse = " ")
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      return(paste0('{"claims":[{"claim":"Trials help.","kind":"finding","supported_by":[1,3],',
                    '"contradicted_by":[2],"moderator":"design","scope":null},',
                    '{"claim":"No cost data.","kind":"gap","supported_by":[1],"contradicted_by":[],',
                    '"moderator":null,"scope":null}]}'))
    }
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return('{"sections":[{"heading":"Trials","brief":"t","claims":[1],"rationale":null}]}')
    }
    if (grepl("<gaps>", body, fixed = TRUE)) return(r2s_cut("Nothing on costs [study 1]. Nor on"))
    "Trials found a benefit [study 1] [study 3], a survey did not [study 2]."
  })
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  o <- quiet(gr_outline(cm, client = cl))
  s <- quiet(gr_synthesise(tab, outline = o, question = "Q?", client = cl, claims = cm,
                           gaps = gr_gaps(cm)))
  closing <- s$sections[s$sections$section == attr(o, "closing"), ]
  expect_identical(closing$n_truncated, 1L)
  expect_true(closing$partial)
  expect_false(s$sections$partial[s$sections$section == "Trials"])
})

test_that("gr_claims says which studies a cut-off batch lost, on the result", {
  # Before: the loss was a warning and nothing else; the gr_claims object read
  # as complete to anything that looked at it later.
  tab <- r2s_table(4L, finding = c("supports", "contradicts", "supports", "mixed"))
  # Two studies per batch, so four studies are two batches.
  one <- gr_mock_client(function(m, p) {
    blk <- m[[3]]$content
    if (grepl("[study 3]", blk, fixed = TRUE)) return(r2s_cut('{"claims":[{"claim":"Tr'))
    paste0('{"claims":[{"claim":"Trials help.","kind":"finding","supported_by":[1],',
           '"contradicted_by":[2],"moderator":"design","scope":null}]}')
  })
  r <- r2s_warns(gr_claims(tab, question = "Q?", client = one, max_claim_tokens = 480))
  cm <- r$value
  expect_true("gr_claims_truncated" %in% r$classes)
  expect_true(cm$partial)
  expect_identical(cm$lost, 3:4)
  expect_identical(nrow(cm$claims), 1L)
  expect_output(print(cm), "PARTIAL: 2 of 4 studies contributed nothing")

  # Every batch cut off: no claims, every study lost.
  all_cut <- gr_mock_client(function(m, p) r2s_cut('{"claims":[{"claim":"Tr'))
  cm <- quiet(gr_claims(tab, question = "Q?", client = all_cut))
  expect_true(cm$partial)
  expect_identical(cm$lost, 1:4)

  # A complete run says so.
  fine <- gr_mock_client(function(m, p) paste0(
    '{"claims":[{"claim":"Trials help.","kind":"finding","supported_by":[1],',
    '"contradicted_by":[],"moderator":null,"scope":null}]}'))
  cm <- quiet(gr_claims(tab, question = "Q?", client = fine, max_claim_tokens = 480))
  expect_false(cm$partial)
  expect_identical(cm$lost, integer(0))

  # A batch the call limit stopped is lost too, not a batch with nothing in it.
  local_registries()
  gr_options(max_calls = 1)
  cm <- quiet(gr_claims(tab, question = "Q?", client = fine, max_claim_tokens = 480))
  expect_true(cm$partial)
  expect_identical(cm$lost, 3:4)
})

test_that("gr_outline says its reply was cut off rather than asking to try again", {
  # Before: "did not return usable sections ... or try again", which cannot
  # help: the same claims ask for the same reply, cut off in the same place.
  tab <- r2s_table()
  cm <- quiet(gr_claims(tab, question = "Q?", client = gr_mock_client(function(m, p) paste0(
    '{"claims":[{"claim":"Trials help.","kind":"finding","supported_by":[1],',
    '"contradicted_by":[],"moderator":null,"scope":null}]}'))))
  cl <- gr_mock_client(function(m, p) r2s_cut('{"sections":[{"heading":"Trials","brief":"t","claims":[1'))
  r <- r2s_warns(gr_outline(cm, client = cl))
  expect_true(all(c("gr_outline_truncated", "gr_outline_failed") %in% r$classes))
  expect_match(r$messages[r$first == "gr_outline_truncated"], "stopped at the 1200-token reply limit",
               fixed = TRUE)
  # Every claim is still placed.
  expect_setequal(attr(r$value, "claims")$claim_id, cm$claims$claim_id)
})

# ---------------------------------------------------------------------------
# model-output-02: a citation the check cannot read is not a citation to nothing
# ---------------------------------------------------------------------------

test_that("a section with a citation the check cannot read is partial", {
  # Before: "[studies 1 to 7]" over three studies matched nothing, so the
  # section cited nothing unknown and was partial = FALSE.
  for (txt in c("Three trials found a benefit [studies 1 to 7].",
                "Three trials found a benefit [study one].")) {
    r <- r2s_warns(gr_synthesise(r2s_table(), outline = c(Findings = "what"), question = "Q?",
                                 client = gr_mock_client(function(m, p) txt)))
    s <- r$value$sections
    expect_identical(s$n_unparsed, 1L)
    expect_true(s$partial)
    expect_true("gr_synth_unparsed" %in% r$classes)
    expect_output(print(r$value), "1 THE CHECK CANNOT READ")
  }
  # Forms the grammar reads are not flagged.
  s <- quiet(gr_synthesise(r2s_table(), outline = c(Findings = "what"), question = "Q?",
                           client = gr_mock_client(function(m, p) "A benefit [studies 1, 2, and 3].")))
  expect_identical(s$sections$n_unparsed, 0L)
  expect_identical(s$sections$n_cited, 3L)
  expect_false(s$sections$partial)
})

test_that("a range renders as the studies the check counted, never as its two ends", {
  # Before: "[studies 1-3]" was left as a bare marker in author-year prose
  # whose reference list, alphabetical and unnumbered, gave no way to find
  # "study 2".
  used <- data.frame(study = 1:3, authors = c("Smith, J.", "Garcia, M.", "Lee, K."),
                     year = c("2019", "2020", "2021"), document = paste0(1:3, ".pdf"),
                     stringsAsFactors = FALSE)
  cols <- readgpt:::bib_columns(used)
  keys <- readgpt:::bib_keys(used, cols)
  rc <- function(x, style = "author-year") readgpt:::render_citations(x, used, keys, style)
  expect_identical(rc("A benefit [studies 1-3]."), "A benefit (Garcia, 2020; Lee, 2021; Smith, 2019).")
  expect_identical(rc("A benefit [studies 1-3].", "numeric"), "A benefit (1, 2, 3).")
  expect_identical(rc("A benefit [study 1] [studies 2–3]."),
                   "A benefit (Garcia, 2020; Lee, 2021; Smith, 2019).")
  # A range past the table stays a marker: the check reported it, and "1 and
  # 7" would be two real studies standing in for a fabrication.
  expect_identical(rc("A benefit [studies 1-7]."), "A benefit [studies 1-7].")
  # A locator stays as written rather than losing its page.
  expect_identical(rc("A benefit [study 3 p. 4]."), "A benefit [study 3 p. 4].")
})

test_that("a revision is held to the hedging of range-cited claims too", {
  # Before: only the listed form marked a claim sentence, so a draft citing
  # "[studies 1-3]" had none and "may suggest" -> "All trials suggest" passed.
  st <- readgpt:::strength_guard("Three small trials may suggest a benefit [studies 1-3].",
                                 "All trials suggest a benefit [studies 1-3].")
  expect_false(st$ok)
  expect_match(st$reason, "all", fixed = TRUE)
  expect_length(readgpt:::claim_sentences("A benefit [study 3, page 4]. Framing."), 1L)
})

test_that("a revision that writes a citation the check cannot read is discarded", {
  # Before: "[studies 2 to 9]" was invisible to the citation comparison, the
  # revision replaced the draft, and the published review carried a citation
  # nothing had checked.
  tab <- r2s_table()
  cl <- gr_mock_client(function(m, p) {
    if (grepl("<draft>", m[[length(m)]]$content, fixed = TRUE)) {
      return("A benefit was reported [study 1], and replicated [studies 2 to 9].")
    }
    "A benefit was reported [study 1]."
  })
  r <- r2s_warns(gr_synthesise(tab, outline = c(Findings = "what"), question = "Q?", client = cl,
                               coherence = "register"))
  expect_false(r$value$coherence$kept)
  expect_match(r$value$coherence$reason, "cannot read ([studies 2 to 9])", fixed = TRUE)
  expect_true("gr_coherence_rejected" %in% r$classes)
  expect_false(grepl("[studies 2 to 9]", r$value$text_marked, fixed = TRUE))
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-02: one order on every machine
# ---------------------------------------------------------------------------

test_that("the reference list and in-text order do not depend on the collation locale", {
  # Before, under LC_COLLATE=C: "Zhou" before the umlauted "Ozturk" in the list,
  # and "(Baker, 2018; Oakes, 2016; Zhou, 2020; de la Cruz, 2017; ...)" in the
  # text -- a different document from the one en_US printed.
  used <- data.frame(study = 1:5,
                     authors = c("Zhou, L.", "Öztürk, A.", "Baker, T.", "de la Cruz, M.",
                                 "Oakes, P."),
                     year = c("2020", "2019", "2018", "2017", "2016"),
                     title = c("Z", "O", "B", "D", "Oa"), document = paste0(1:5, ".pdf"),
                     stringsAsFactors = FALSE)
  cols <- readgpt:::bib_columns(used)
  keys <- readgpt:::bib_keys(used, cols)
  want_refs <- c("Baker", "de la Cruz", "Oakes", "Öztürk", "Zhou")
  want_text <- paste0("All (Baker, 2018; de la Cruz, 2017; Oakes, 2016; Öztürk, 2019; ",
                      "Zhou, 2020).")
  check <- function() {
    refs <- readgpt:::reference_list(used, keys, 1:5, cols, "author-year")
    expect_identical(sub("^- ([^,]+),.*", "\\1", refs), want_refs)
    expect_identical(readgpt:::render_citations("All [study 1] [study 2] [study 3] [study 4] [study 5].",
                                                used, keys, "author-year"), want_text)
  }
  check()
  withr::local_collate("C")
  check()
})

test_that("same-author, same-year letters follow the title, not the row order", {
  # Before: by row, so "2019a" named whichever paper's row came first -- and row
  # order followed the locale's collation of the file names.
  dup <- data.frame(study = 1:2, authors = "Smith, J.", year = "2019",
                    title = c("Zebra trial", "Apple trial"), document = c("a.pdf", "b.pdf"),
                    stringsAsFactors = FALSE)
  cols <- readgpt:::bib_columns(dup)
  k <- readgpt:::bib_keys(dup, cols)
  expect_identical(k, c("Smith, 2019b", "Smith, 2019a"))
  refs <- readgpt:::reference_list(dup, k, 1:2, cols, "author-year")
  expect_identical(refs, c("- Smith, J. (2019a). Apple trial.", "- Smith, J. (2019b). Zebra trial."))
  # The same papers in the other row order get the same letters.
  flip <- dup[2:1, ]; flip$study <- 1:2
  expect_identical(readgpt:::bib_keys(flip, cols), c("Smith, 2019a", "Smith, 2019b"))
})

test_that("the sort key folds accents, case and apostrophes by rule, not by locale", {
  key <- readgpt:::bib_sort_key
  expect_identical(key(c("Öztürk", "de la Cruz", "O'Brien", "Łukasz", "Öberg",
                         "Straße")),
                   c("ozturk", "de la cruz", "obrien", "lukasz", "oberg", "strasse"))
  # An unlabelled string, as a non-UTF-8 session hands one over, folds the same.
  expect_identical(key(unmarked("Öztürk")), "ozturk")
})
