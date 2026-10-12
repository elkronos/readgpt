# test-outline-alternatives.R -- gr_outline(alternatives =): more than one
# grouping of the same claims, compared by counting and chosen by the author.
#
# What has to hold: the first request is the one `alternatives = 1` sends, so
# nothing about a run without alternatives changes; each further outline is a
# request of its own whose prompt differs from every other, so the response
# cache cannot hand back one sample k times; each is verified as the first is;
# a grouping that repeats an earlier one is dropped by counting, not by asking;
# and the outline chosen keeps its own headings and claim assignment together
# all the way to the audit report.

oa_table <- function(n = 4L) {
  data.frame(document = paste0(letters[seq_len(n)], ".pdf"),
             document_id = paste0("h", seq_len(n)), status = "ok",
             duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_,
             n = c(400L, 60L, 900L, 25L)[seq_len(n)],
             design = c("randomised trial", "cross-sectional", "randomised trial",
                        "qualitative")[seq_len(n)],
             finding = c("supports", "contradicts", "supports", "mixed")[seq_len(n)],
             stringsAsFactors = FALSE)
}

# Four claims and a gap. Claim 1 is contested, so the comparison has a
# contested claim to count.
oa_claims_json <- paste0(
  '{"claims":[',
  '{"claim":"Effect A appears in trials.","kind":"finding","supported_by":[1],',
  '"contradicted_by":[2],"moderator":"design","scope":null},',
  '{"claim":"Effect B appears too.","kind":"finding","supported_by":[2],',
  '"contradicted_by":[],"moderator":null,"scope":null},',
  '{"claim":"The outcome is measured two ways.","kind":"measurement","supported_by":[3],',
  '"contradicted_by":[],"moderator":null,"scope":null},',
  '{"claim":"Most studies are trials.","kind":"method","supported_by":[4],',
  '"contradicted_by":[],"moderator":null,"scope":null},',
  '{"claim":"Nobody studied adolescents.","kind":"gap","supported_by":[1],',
  '"contradicted_by":[],"moderator":null,"scope":null}]}')

oa_sections <- function(...) {
  s <- list(...)
  parts <- vapply(names(s), function(h) sprintf(
    '{"heading":"%s","brief":"about %s","claims":[%s],"rationale":"why %s"}',
    h, h, paste(s[[h]], collapse = ","), h), character(1), USE.NAMES = FALSE)
  paste0('{"sections":[', paste(parts, collapse = ","), "]}")
}

oa_first <- oa_sections(Effects = 1:2, Methods = 3:4)
oa_by_design <- oa_sections(Trials = c(1, 3), Others = c(2, 4))
oa_renamed <- oa_sections(Findings = 1:2, Approaches = 3:4)

oa_text <- function(messages) {
  paste(vapply(messages, function(m) as.character(m$content), character(1)), collapse = " ")
}

# `replies` maps the proposal number to the outline reply: 1 for the first
# request, 2 for "Write proposal 2", and so on. A function is called with the
# messages and returns the reply.
oa_client <- function(replies = list(oa_first, oa_by_design, oa_renamed),
                      section_text = "It is so [study 1].") {
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    all_txt <- oa_text(messages)
    if (grepl("turn a table of studies", sys, fixed = TRUE)) return(oa_claims_json)
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      j <- suppressWarnings(as.integer(sub(".*Write proposal ([0-9]+).*", "\\1", all_txt)))
      if (is.na(j)) j <- 1L
      r <- if (j <= length(replies)) replies[[j]] else '{"sections":[]}'
      return(if (is.function(r)) r(messages) else r)
    }
    section_text
  })
}

oa_setup <- function(client = oa_client()) {
  cm <- quiet(gr_claims(oa_table(), question = "Does it work?", client = client))
  list(claims = cm, client = client)
}

outline_calls <- function(client) {
  Filter(function(x) startsWith(as_chr_label(x$label), "outline."), client$calls())
}
as_chr_label <- function(x) if (is.null(x)) "" else as.character(x)

# ---------------------------------------------------------------------------
# The default is today's request, and the first outline is today's outline.
# ---------------------------------------------------------------------------

test_that("alternatives = 1 sends the one request it always sent, and attaches nothing", {
  f <- oa_setup()
  o1 <- quiet(gr_outline(f$claims, client = f$client))
  calls1 <- outline_calls(f$client)
  expect_length(calls1, 1L)
  expect_identical(calls1[[1]]$label, "outline.derive")
  # Nothing new on the object: a run that never asked for alternatives gets
  # the object it always got.
  expect_named(attributes(o1), c("names", "claims", "rationale", "closing", "claims_fingerprint"))

  f$client$reset()
  o3 <- quiet(gr_outline(f$claims, client = f$client, alternatives = 3))
  calls3 <- outline_calls(f$client)
  # The first request of three is byte for byte the request of one: the same
  # messages and the same parameters, so the cache, a recorded replay and a
  # corpus store all key it as before.
  expect_identical(calls3[[1]]$messages, calls1[[1]]$messages)
  expect_identical(calls3[[1]]$params, calls1[[1]]$params)
  expect_identical(calls3[[1]]$label, "outline.derive")

  # And the outline returned is the first proposal, exactly as it comes alone.
  bare <- o3
  for (a in c("alternative", "comparison", "alternatives")) attr(bare, a) <- NULL
  expect_identical(bare, o1)
})

test_that("each alternative is its own request, shown the groupings before it", {
  f <- oa_setup()
  o <- quiet(gr_outline(f$claims, client = f$client, alternatives = 3))
  calls <- outline_calls(f$client)
  expect_identical(vapply(calls, `[[`, "", "label"),
                   c("outline.derive", "outline.derive.alt2", "outline.derive.alt3"))
  alt2 <- calls[[2]]$messages
  # Everything the first request said, then one more message.
  expect_identical(alt2[seq_along(calls[[1]]$messages)], calls[[1]]$messages)
  ask <- alt2[[length(alt2)]]$content
  expect_match(ask, "<earlier_outlines>", fixed = TRUE)
  expect_match(ask, "Proposal 1:\n- Effects: claims 1, 2\n- Methods: claims 3, 4", fixed = TRUE)
  expect_match(ask, "Write proposal 2", fixed = TRUE)
  # The closing section is R's, not the model's; shown among the model's
  # sections, it invites the next proposal to copy it.
  expect_false(grepl("What is missing", ask, fixed = TRUE))
  # The third is shown both kept proposals.
  ask3 <- calls[[3]]$messages[[length(calls[[3]]$messages)]]$content
  expect_match(ask3, "Proposal 2:\n- Trials: claims 1, 3\n- Others: claims 2, 4", fixed = TRUE)
  expect_match(ask3, "Write proposal 3", fixed = TRUE)
  # The temperature and reply limit are the first request's.
  expect_identical(calls[[2]]$params$max_output, calls[[1]]$params$max_output)
  expect_identical(calls[[2]]$params$temperature, calls[[1]]$params$temperature)
})

test_that("the response cache cannot hand back one sample as every alternative", {
  local_registries()
  n <- 0L
  base <- oa_client(replies = list(oa_first, oa_renamed, oa_by_design))
  counting <- gr_mock_client(function(m, p) { n <<- n + 1L; base$handler(m, p) })
  cl <- gr_cache_client(counting, gr_cache(withr::local_tempdir()))
  cm <- quiet(gr_claims(oa_table(), question = "Does it work?", client = cl))
  # A first run without alternatives fills the cache with the first outline.
  o1 <- quiet(gr_outline(cm, client = cl))
  before <- n
  o <- quiet(gr_outline(cm, client = cl, alternatives = 3))
  # The first request was a cache hit, and both alternatives reached the
  # model -- including the third, asked after the second was dropped as a
  # repeat, whose kept proposals are the same as the second's were. Its
  # prompt still differs, because it names the proposal it asks for.
  expect_identical(n - before, 2L)
  cmp <- attr(o, "comparison")
  expect_identical(cmp$kept, c(TRUE, FALSE, TRUE))
  # Asked again, every request is served from the cache and the comparison
  # is the same one.
  again <- n
  o_again <- quiet(gr_outline(cm, client = cl, alternatives = 3))
  expect_identical(n, again)
  expect_identical(attr(o_again, "comparison"), cmp)
})

test_that("a run with alternatives replays from its recording with no model", {
  local_registries()
  f <- oa_setup(oa_client(replies = list(oa_first, oa_renamed, oa_by_design)))
  o <- suppressWarnings(quiet(gr_outline(f$claims, client = f$client, alternatives = 3)))
  rp <- gr_replay_client(f$claims$trace)
  cm2 <- quiet(gr_claims(oa_table(), question = "Does it work?", client = rp))
  o2 <- suppressWarnings(quiet(gr_outline(cm2, client = rp, alternatives = 3)))
  expect_identical(rp$stats()$misses, 0L)
  expect_identical(attr(o2, "comparison"), attr(o, "comparison"))
  expect_identical(gr_outline_pick(o2, 3), gr_outline_pick(o, 3))
})

# ---------------------------------------------------------------------------
# Distinctness is counted.
# ---------------------------------------------------------------------------

test_that("outline_distance counts pairs of claims, not headings", {
  d <- readgpt:::outline_distance
  a <- data.frame(section = c("A", "A", "B", "B"), claim_id = 1:4)
  renamed <- data.frame(section = c("X", "X", "Y", "Y"), claim_id = 1:4)
  expect_identical(d(a, renamed), 0)
  # Six pairs; {1,2} and {3,4} together in one, {1,3} and {2,4} in the other:
  # four pairs disagree.
  crossed <- data.frame(section = c("P", "Q", "P", "Q"), claim_id = 1:4)
  expect_equal(d(a, crossed), 4 / 6)
  # Every claim alone against every claim together: no pair agrees.
  alone <- data.frame(section = c("1", "2", "3", "4"), claim_id = 1:4)
  together <- data.frame(section = "all", claim_id = 1:4)
  expect_identical(d(alone, together), 1)
  # Symmetric, and order of rows does not matter.
  expect_identical(d(crossed, a), d(a, crossed))
  expect_identical(d(a[4:1, ], crossed), d(a, crossed))
  # A claim one side never placed agrees with no other claim: {3, 4} is the
  # one pair that changes.
  expect_equal(d(a[1:3, ], a), 1 / 6)
  # One claim has no pairs, and so only one grouping.
  expect_identical(d(a[1, ], crossed[1, ]), 0)
})

test_that("outline_distance agrees with counting every pair", {
  # It is computed from section sizes; this lists the pairs instead.
  by_pairs <- function(a, b) {
    ids <- sort(union(a$claim_id, b$claim_id))
    sa <- a$section[match(ids, a$claim_id)]
    sb <- b$section[match(ids, b$claim_id)]
    sa[is.na(sa)] <- paste0("alone-a", ids[is.na(sa)])
    sb[is.na(sb)] <- paste0("alone-b", ids[is.na(sb)])
    p <- utils::combn(length(ids), 2L)
    mean((sa[p[1, ]] == sa[p[2, ]]) != (sb[p[1, ]] == sb[p[2, ]]))
  }
  withr::local_seed(20261012)
  for (i in 1:25) {
    n <- sample(2:30, 1)
    a <- data.frame(section = sample(c("A", "B", "C", "D, E"), n, TRUE), claim_id = seq_len(n))
    b <- data.frame(section = sample(c("x", "y", "z"), n, TRUE), claim_id = seq_len(n))
    if (i %% 5 == 0) b <- b[-1, ]
    expect_equal(readgpt:::outline_distance(a, b), by_pairs(a, b))
  }
})

test_that("a grouping that repeats an earlier one is dropped, and the comparison says why", {
  f <- oa_setup(oa_client(replies = list(oa_first, oa_renamed, oa_by_design)))
  o <- NULL
  expect_warning(o <- suppressMessages(gr_outline(f$claims, client = f$client, alternatives = 3)),
                 class = "gr_outline_alternatives_short")
  cmp <- attr(o, "comparison")
  expect_identical(cmp$alternative, 1:3)
  expect_identical(cmp$kept, c(TRUE, FALSE, TRUE))
  expect_identical(cmp$distance[2], 0)
  expect_match(cmp$note[2], "grouped the claims exactly as alternative 1 did", fixed = TRUE)
  # Kept: the distinct one only, carrying its own number.
  alts <- attr(o, "alternatives")
  expect_length(alts, 1L)
  expect_identical(attr(alts[[1]], "alternative")$alternative, 3L)
  expect_identical(names(alts[[1]]), c("Trials", "Others", "What is missing"))
  # Recorded on the trace as a local step, so it costs nothing.
  st <- Filter(function(s) identical(s$label, "outline.compare"), f$claims$trace$steps)
  expect_length(st, 1L)
  expect_identical(st[[1]]$kind, "local")
  expect_identical(st[[1]]$detail$kept, "1, 3")
  expect_identical(st[[1]]$detail$dropped, "2")
})

test_that("the comparison is a flat table of what each outline looks like", {
  f <- oa_setup()
  o <- quiet(gr_outline(f$claims, client = f$client, alternatives = 2))
  cmp <- attr(o, "comparison")
  expect_named(cmp, c("alternative", "sections", "claims_per_section", "contested_per_section",
                      "distance", "nearest", "kept", "note", "headings"))
  # No list column: it is printed and written out.
  expect_false(any(vapply(cmp, is.list, logical(1))))
  expect_identical(cmp$sections, c(3L, 3L))
  # Effects {1,2}, Methods {3,4}, What is missing {5}; claim 1 is contested.
  expect_identical(cmp$claims_per_section, c("2, 2, 1", "2, 2, 1"))
  expect_identical(cmp$contested_per_section, c("1, 0, 0", "1, 0, 0"))
  # Ten pairs over five claims; four disagree.
  expect_identical(cmp$distance, c(0, 0.4))
  expect_identical(cmp$nearest, c(NA_real_, 0.4))
  expect_identical(cmp$headings[2], "Trials | Others | What is missing")
  expect_true(all(is.na(cmp$note)))
})

# ---------------------------------------------------------------------------
# Every alternative is verified as the first is.
# ---------------------------------------------------------------------------

test_that("an alternative is verified exactly as the first outline is", {
  # Claim 1 twice, a claim that does not exist, the gap claim under a finding,
  # claim 4 never placed.
  messy <- paste0('{"sections":[{"heading":"One","brief":"b","claims":[1,3,5,99],"rationale":null},',
                  '{"heading":"Two","brief":"b","claims":[1,2],"rationale":null}]}')
  f <- oa_setup(oa_client(replies = list(oa_first, messy)))
  o <- quiet(gr_outline(f$claims, client = f$client, alternatives = 2))
  alt <- gr_outline_pick(o, 2)
  map <- attr(alt, "claims")
  expect_setequal(map$claim_id, 1:5)
  expect_equal(sum(duplicated(map$claim_id)), 0L)
  expect_identical(map$section[map$claim_id == 1], "One")
  expect_identical(map$section[map$claim_id == 5], "What is missing")
  expect_identical(map$section[map$claim_id == 4], "What is missing")
  expect_false(99L %in% map$claim_id)
  expect_identical(attr(alt, "closing"), "What is missing")
  expect_identical(attr(alt, "claims_fingerprint"), attr(o, "claims_fingerprint"))
  expect_identical(names(attr(alt, "rationale")), names(alt))
})

test_that("an alternative's messages say which outline they are about", {
  messy <- oa_sections(Solo = c(1, 3))
  f <- oa_setup(oa_client(replies = list(oa_first, messy)))
  # These are messages, which follow gr_options(verbose): set here, so the test
  # does not pass or fail on what an earlier test left it as.
  old_v <- gr_options(verbose = TRUE)
  on.exit(gr_options(old_v), add = TRUE)
  msgs <- character(0)
  withCallingHandlers(
    suppressWarnings(gr_outline(f$claims, client = f$client, alternatives = 2)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    })
  # Claims 2 and 4, and the gap claim 5, which the reply left out.
  expect_true(any(grepl("^Outline alternative 2: 3 claim\\(s\\) were not placed", msgs)))
  # The first outline's own message is not prefixed.
  expect_true(any(grepl("^1 claim\\(s\\) were not placed", msgs)))
})

test_that("an alternative that fails or is cut off is dropped, not replaced by one section", {
  cut <- function(m) gr_result(TRUE, '{"sections":[{"heading":"Tr', finish_reason = "length")
  f <- oa_setup(oa_client(replies = list(oa_first, '{"sections":[]}', cut)))
  o <- NULL
  w <- character(0)
  o <- withCallingHandlers(
    suppressMessages(gr_outline(f$claims, client = f$client, alternatives = 3)),
    warning = function(cnd) {
      w <<- c(w, class(cnd)[1])
      invokeRestart("muffleWarning")
    })
  expect_true("gr_outline_alternatives_short" %in% w)
  # The first outline's own failure warning is not raised for an alternative.
  expect_false("gr_outline_failed" %in% w)
  cmp <- attr(o, "comparison")
  expect_identical(cmp$kept, c(TRUE, FALSE, FALSE))
  expect_match(cmp$note[2], "no usable sections", fixed = TRUE)
  expect_match(cmp$note[3], "reply limit", fixed = TRUE)
  expect_match(cmp$note[3], "max_outline_tokens", fixed = TRUE)
  expect_true(all(is.na(cmp$sections[2:3])))
  expect_length(attr(o, "alternatives"), 0L)
})

test_that("no alternative is asked for once the run reaches its call cap", {
  local_registries()
  f <- oa_setup()
  # One call drew the claims; the cap leaves room for the first outline and
  # one alternative.
  gr_options(max_calls = 3L)
  o <- NULL
  expect_warning(o <- suppressMessages(gr_outline(f$claims, client = f$client, alternatives = 3)),
                 class = "gr_outline_alternatives_short")
  expect_identical(vapply(outline_calls(f$client), `[[`, "", "label"),
                   c("outline.derive", "outline.derive.alt2"))
  cmp <- attr(o, "comparison")
  expect_identical(cmp$kept, c(TRUE, TRUE, FALSE))
  expect_match(cmp$note[3], "not requested, as the run had reached its call cap", fixed = TRUE)
})

test_that("nothing is asked for when the first outline did not come back", {
  f <- oa_setup(oa_client(replies = list('{"sections":[]}', oa_by_design)))
  w <- character(0)
  o <- withCallingHandlers(
    suppressMessages(gr_outline(f$claims, client = f$client, alternatives = 2)),
    warning = function(cnd) {
      w <<- c(w, class(cnd)[1])
      invokeRestart("muffleWarning")
    })
  expect_true(all(c("gr_outline_failed", "gr_outline_alternatives_short") %in% w))
  expect_length(outline_calls(f$client), 1L)
  expect_match(attr(o, "comparison")$note[2], "first outline did not come back", fixed = TRUE)
})

test_that("one claim is not regrouped, because it can be grouped only one way", {
  one <- paste0('{"claims":[{"claim":"It works.","kind":"finding","supported_by":[1],',
                '"contradicted_by":[],"moderator":null,"scope":null}]}')
  cl <- gr_mock_client(function(messages, params) {
    if (grepl("turn a table of studies", messages[[1]]$content, fixed = TRUE)) return(one)
    oa_sections(Only = 1)
  })
  cm <- quiet(gr_claims(oa_table(), question = "Q?", client = cl))
  o <- quiet(gr_outline(cm, client = cl, alternatives = 3))
  expect_length(outline_calls(cl), 1L)
  expect_match(attr(o, "comparison")$note[2], "grouped only one way", fixed = TRUE)
})

test_that("`alternatives` outside 1 to 10 is clamped, with a warning, before any call", {
  f <- oa_setup()
  expect_warning(o <- suppressMessages(gr_outline(f$claims, client = f$client, alternatives = 0)),
                 class = "gr_clamped")
  expect_length(outline_calls(f$client), 1L)
  expect_null(attr(o, "comparison"))
  f$client$reset()
  expect_warning(suppressMessages(gr_outline(f$claims, client = f$client, alternatives = NA)),
                 class = "gr_bad_setting")
  expect_length(outline_calls(f$client), 1L)
})

# ---------------------------------------------------------------------------
# Choosing one keeps it whole, and the write-up says which was chosen.
# ---------------------------------------------------------------------------

test_that("gr_outline_pick returns one outline whole, and refuses what it cannot return", {
  f <- oa_setup(oa_client(replies = list(oa_first, oa_renamed, oa_by_design)))
  o <- suppressWarnings(quiet(gr_outline(f$claims, client = f$client, alternatives = 3)))
  p3 <- gr_outline_pick(o, 3)
  expect_identical(names(p3), c("Trials", "Others", "What is missing"))
  expect_setequal(unique(attr(p3, "claims")$section), names(p3))
  expect_identical(attr(p3, "alternative")$alternative, 3L)
  expect_identical(attr(p3, "alternative")$of, 3L)
  expect_identical(attr(p3, "alternative")$distinct, 2L)
  expect_null(attr(p3, "alternatives"))
  expect_null(attr(p3, "comparison"))

  p1 <- gr_outline_pick(o, 1)
  expect_identical(names(p1), names(o))
  expect_identical(attr(p1, "claims"), attr(o, "claims"))
  expect_identical(attr(p1, "alternative")$alternative, 1L)
  expect_null(attr(p1, "alternatives"))

  # Dropped as a repeat, so there is nothing to return.
  expect_error(gr_outline_pick(o, 2), "grouped the claims exactly as alternative 1",
               class = "gr_outline_no_alternative")
  expect_error(gr_outline_pick(o, 4), class = "gr_outline_no_alternative")
  expect_error(gr_outline_pick(o, 1.5), class = "gr_outline_no_alternative")
  expect_error(gr_outline_pick(o, "3"), class = "gr_outline_no_alternative")
  # An outline made without alternatives, or one already picked, has none.
  expect_error(gr_outline_pick(p3, 1), class = "gr_outline_no_alternative")
  expect_error(gr_outline_pick(c(A = "a"), 1), class = "gr_outline_no_alternative")
})

test_that("a write-up from a chosen alternative records which, and the audit says so", {
  f <- oa_setup()
  o <- quiet(gr_outline(f$claims, client = f$client, alternatives = 2))
  p <- gr_outline_pick(o, 2)
  s <- quiet(gr_synthesise(oa_table(), outline = p, question = "Does it work?",
                           client = f$client, claims = f$claims))
  # Written from the alternative's own sections and claim assignment.
  expect_identical(s$sections$section[1:2], c("Trials", "Others"))
  expect_identical(s$claim_sections$section[s$claim_sections$claim_id == 3], "Trials")
  alt <- s$outline_alternative
  expect_s3_class(alt, "data.frame")
  expect_identical(alt$alternative, 2L)
  expect_identical(alt$of, 2L)
  expect_identical(alt$distance, 0.4)
  expect_identical(s$trace$meta$outline_alternative, 2L)
  expect_identical(s$trace$meta$outline_of, 2L)
  expect_match(paste(utils::capture.output(print(s)), collapse = "\n"),
               "outline: alternative 2 of the 2 gr_outline() was asked for", fixed = TRUE)

  path <- tempfile(fileext = ".html")
  quiet(gr_audit_report(path, synthesis = s, open = FALSE))
  html <- paste(readLines(path, encoding = "UTF-8"), collapse = "\n")
  expect_match(html, "The sections follow outline 2 of the 2 gr_outline() was asked for",
               fixed = TRUE)
  expect_match(html, "for 40% of the pairs of claims", fixed = TRUE)

  # The first proposal, passed as gr_outline() returned it, is recorded too.
  s1 <- quiet(gr_synthesise(oa_table(), outline = o, question = "Does it work?",
                            client = f$client, claims = f$claims))
  expect_identical(s1$outline_alternative$alternative, 1L)
  path1 <- tempfile(fileext = ".html")
  quiet(gr_audit_report(path1, synthesis = s1, open = FALSE))
  html1 <- paste(readLines(path1, encoding = "UTF-8"), collapse = "\n")
  expect_match(html1, "The sections follow outline 1 of the 2", fixed = TRUE)
  expect_false(grepl("pairs of claims", html1, fixed = TRUE))
})

test_that("a write-up from an outline without alternatives is the object it always was", {
  f <- oa_setup()
  o <- quiet(gr_outline(f$claims, client = f$client))
  s <- quiet(gr_synthesise(oa_table(), outline = o, question = "Does it work?",
                           client = f$client, claims = f$claims))
  expect_false("outline_alternative" %in% names(s))
  expect_named(s$trace$meta, c("readgpt", "stage", "question", "sections", "studies"))
  expect_false(any(grepl("outline: alternative", utils::capture.output(print(s)), fixed = TRUE)))
  path <- tempfile(fileext = ".html")
  quiet(gr_audit_report(path, synthesis = s, open = FALSE))
  expect_false(grepl("The sections follow outline", paste(readLines(path), collapse = "\n"),
                     fixed = TRUE))
})
