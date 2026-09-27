# test-review6-synthesis.R -- the sixth pass over the write-up and its citations.
#
# Each test failed on the code before its fix, and says what that code did:
# claims placed in a renamed or briefless section never written, gaps sent to
# no section, ordinary finding columns taken for bibliographic ones, a
# reference list that could not be followed from citation-key prose, merged
# batches that left studies out in silence, a claims block too large for the
# window aborting the whole write-up, two "in press" papers cited as one, a
# claims check that passed any table without id columns, and a closing section
# handed every study.

r6s_table <- function() {
  data.frame(document = paste0(letters[1:4], ".pdf"), document_id = paste0("h", 1:4),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_,
             design = c("trial", "survey", "trial", "survey"), stringsAsFactors = FALSE)
}

r6s_claim <- function(text, ids, kind = "finding") {
  sprintf(paste0('{"claim":"%s","kind":"%s","supported_by":[%s],"contradicted_by":[],',
                 '"moderator":null,"scope":null}'), text, kind, paste(ids, collapse = ","))
}

# Claims (1: trials, studies 1 and 3; 2: surveys, studies 2 and 4) and an
# outline placing claim 1 in `heads[1]` and claim 2 in `heads[2]`, with the
# briefs given. Every prompt a section is sent is kept in `env$seen`.
r6s_client <- function(heads = c("Trials", "Surveys"), briefs = c("trials", "surveys"),
                       env = new.env(), claims = NULL) {
  env$seen <- list()
  gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      return(claims %||% sprintf('{"claims":[%s,%s]}', r6s_claim("Trials work.", c(1, 3)),
                                 r6s_claim("Surveys show harm.", c(2, 4))))
    }
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      return(sprintf(paste0('{"sections":[{"heading":"%s","brief":"%s","claims":[1],"rationale":null},',
                            '{"heading":"%s","brief":"%s","claims":[2],"rationale":null}]}'),
                     heads[1], briefs[1], heads[2], briefs[2]))
    }
    env$seen[[length(env$seen) + 1L]] <- messages
    if (grepl("does not cover", sys, fixed = TRUE)) return("No cohort study was found.")
    txt <- paste(vapply(messages[-1], function(m) as.character(m$content), ""), collapse = " ")
    if (grepl("Surveys show harm", txt, fixed = TRUE)) return("Surveys found harm [study 2] [study 4].")
    if (grepl("Trials work", txt, fixed = TRUE)) return("Trials found a benefit [study 1] [study 3].")
    "The studies [study 1] [study 2]."
  })
}

r6s_prep <- function(cl, tab = r6s_table()) {
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl))
  list(cm = cm, o = quiet(gr_outline(cm, client = cl)))
}

# The warnings a call raised, by class, with the value.
r6s_warns <- function(expr) {
  w <- list()
  val <- withCallingHandlers(suppressMessages(expr), warning = function(c) {
    w[[length(w) + 1L]] <<- c
    invokeRestart("muffleWarning")
  })
  list(value = val, classes = unlist(lapply(w, class)),
       messages = vapply(w, conditionMessage, character(1)))
}

sent_text <- function(msgs) paste(vapply(msgs, function(m) as.character(m$content), ""),
                                  collapse = "\n")

# ---------------------------------------------------------------------------
# synthesis-05: every claim the assignment places lands in a written section,
# and gaps reach a section or a warning says they did not.
# ---------------------------------------------------------------------------

test_that("a claim placed in a heading the outline no longer has stops the run", {
  # Before: names(o)[1] was renamed, the renamed section found no claims and
  # wrote from rows, claim 1 was never argued, and every section read
  # partial = FALSE with no warning.
  env <- new.env()
  cl <- r6s_client(env = env)
  p <- r6s_prep(cl)
  o2 <- p$o
  names(o2)[1] <- "Randomised trials find a benefit"
  n_before <- length(env$seen)
  expect_error(quiet(gr_synthesise(r6s_table(), outline = o2, question = "Q?", client = cl,
                                   claims = p$cm)),
               "claim(s) 1 to section(s) 'Trials'", fixed = TRUE, class = "gr_claims_unplaced")
  # Stopped before any section was paid for.
  expect_identical(length(env$seen), n_before)
  # Renamed in the assignment as well, it is written with its claim.
  a <- attr(o2, "claims")
  a$section[a$section == "Trials"] <- "Randomised trials find a benefit"
  attr(o2, "claims") <- a
  s <- quiet(gr_synthesise(r6s_table(), outline = o2, question = "Q?", client = cl, claims = p$cm))
  expect_identical(s$sections$n_claims[s$sections$section == "Randomised trials find a benefit"], 1L)
  # An assignment without its columns is refused rather than ignored.
  o3 <- p$o
  attr(o3, "claims") <- data.frame(heading = "Trials", claim = 1L)
  expect_error(quiet(gr_synthesise(r6s_table(), outline = o3, question = "Q?", client = cl,
                                   claims = p$cm)), class = "gr_no_claim_assignment")
})

test_that("a section the outline model gave a blank brief is still written, with its claims", {
  # Before: gr_outline() kept 'Surveys' with brief "", outline_vector() dropped
  # it inside gr_synthesise(), and claim 2 vanished with it, nothing flagged.
  cl <- r6s_client(briefs = c("trials", ""))
  p <- r6s_prep(cl)
  expect_identical(unname(p$o[["Surveys"]]), "")
  s <- quiet(gr_synthesise(r6s_table(), outline = p$o, question = "Q?", client = cl, claims = p$cm))
  sv <- s$sections[s$sections$section == "Surveys", ]
  expect_identical(nrow(sv), 1L)
  expect_identical(sv$n_claims, 1L)
  expect_identical(sv$brief, "Surveys")
  expect_setequal(s$citations$study[s$citations$section == "Surveys"], c(2L, 4L))
})

test_that("gaps with no closing section to state them warn and are not reported as used", {
  # Before: a hand-written outline has no closing heading, so <gaps> went to no
  # prompt while `$gaps` carried them, with no warning.
  env <- new.env()
  cl <- r6s_client(env = env)
  g <- data.frame(kind = "untested", dimension = "design", detail = "no cohort studies")
  r <- r6s_warns(gr_synthesise(r6s_table(), question = "Q?", client = cl, gaps = g,
                               outline = c(Findings = "f", "What is missing" = "gaps")))
  expect_true("gr_gaps_unused" %in% r$classes)
  expect_false(any(vapply(env$seen, function(m) grepl("<gaps>", sent_text(m), fixed = TRUE),
                          logical(1))))
  expect_null(r$value$gaps)
  # Named as the closing section, the same outline sends them there and only there.
  env$seen <- list()
  o <- c(Findings = "f", "What is missing" = "gaps")
  attr(o, "closing") <- "What is missing"
  r <- r6s_warns(gr_synthesise(r6s_table(), question = "Q?", client = cl, gaps = g, outline = o))
  expect_false("gr_gaps_unused" %in% r$classes)
  with_gaps <- vapply(env$seen, function(m) grepl("<gaps>", sent_text(m), fixed = TRUE), logical(1))
  expect_identical(with_gaps, c(FALSE, TRUE))
  # Lines of text, as the argument is documented to take, are one block. As a
  # vector of several they were not "nonblank" and reached no section.
  env$seen <- list()
  quiet(gr_synthesise(r6s_table(), question = "Q?", client = cl, outline = o,
                      gaps = c("- no cohort studies", "- no studies of children")))
  last <- sent_text(env$seen[[2]])
  expect_match(last, "<gaps>\n- no cohort studies\n- no studies of children\n</gaps>", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# synthesis-11: `source`, `date`, `url`, `published` and `publication` are
# findings unless `bib` says otherwise.
# ---------------------------------------------------------------------------

test_that("finding columns named source and date are shown to the writer, not made references", {
  # Before: the section prompt showed only `design`, and the output ended
  # "1. (2010-2014). national cancer registry.  2. (2018). household survey."
  tab <- r6s_table()[1:2, ]
  tab$source <- c("national cancer registry", "household survey")
  tab$date <- c("2010-2014", "2018")
  tab$url <- c("https://registry.example/1", "https://registry.example/2")
  tab$publication <- c("published", "preprint")
  seen <- character(0)
  cl <- gr_mock_client(function(m, p) {
    seen <<- c(seen, sent_text(m))
    "Data came from a registry [study 1] and a survey [study 2]."
  })
  s <- quiet(gr_synthesise(tab, outline = c(Data = "Where the data came from and when"),
                           question = "Q?", client = cl))
  expect_match(seen[1], "source: national cancer registry", fixed = TRUE)
  expect_match(seen[1], "date: 2010-2014", fixed = TRUE)
  expect_match(seen[1], "url: https://registry.example/1", fixed = TRUE)
  expect_match(seen[1], "publication: preprint", fixed = TRUE)
  expect_null(s$references)
  expect_false(grepl("References", s$text, fixed = TRUE))
  expect_identical(readgpt:::bib_columns(tab), list())
  # Named in `bib`, a column is bibliographic as before.
  cols <- readgpt:::bib_columns(tab, list(year = "date", venue = "source"))
  expect_identical(cols, list(year = "date", venue = "source"))
})

test_that("a role given as NA keeps a conventionally named column as a finding", {
  tab <- r6s_table()[1:2, ]
  tab$authors <- c("Smith, J.", "Garcia, M.")
  tab$year <- c(2019L, 2020L)
  tab$journal <- c("Lancet", "BMJ")
  expect_identical(readgpt:::bib_columns(tab)$venue, "journal")
  expect_null(readgpt:::bib_columns(tab, list(venue = NA))$venue)
  expect_null(readgpt:::bib_columns(tab, list(venue = ""))$venue)
  # A named character vector works as the list does, and one that does not
  # carry a role leaves that role to the conventional names.
  expect_identical(readgpt:::bib_columns(tab, c(venue = "journal"))$authors, "authors")
  seen <- character(0)
  cl <- gr_mock_client(function(m, p) { seen <<- c(seen, sent_text(m)); "A trial [study 1]." })
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "f"), question = "Q?", client = cl,
                           bib = list(venue = NA)))
  expect_match(seen[1], "journal: Lancet", fixed = TRUE)
  expect_false(grepl("authors:", seen[1], fixed = TRUE))
  expect_identical(s$references, "- Smith, J. (2019).")
})

# ---------------------------------------------------------------------------
# synthesis-10: with a `citation` field the reference list shows the key.
# ---------------------------------------------------------------------------

test_that("citation-key prose can be followed to its reference entry", {
  # Before: the text read "(Smith & Okafor, 2019a; Smith & Okafor, 2019b)" and
  # the references were "- scan_0007.pdf.", "- scan_0333.pdf.", "- scan_0412.pdf."
  tab <- r6s_table()[1:3, ]
  tab$document <- c("scan_0007.pdf", "scan_0333.pdf", "scan_0412.pdf")
  tab$citation <- c("Smith & Okafor, 2019", "Garcia, 2022", "Smith & Okafor, 2019")
  cl <- gr_mock_client(function(m, p) "A benefit [study 1] [study 3]; a cohort agreed [study 2].")
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "f"), question = "Q?", client = cl))
  expect_identical(s$cite_style, "author-year")
  expect_match(s$text, "(Smith & Okafor, 2019a; Smith & Okafor, 2019b)", fixed = TRUE)
  expect_identical(s$references, c("- Garcia, 2022. scan_0333.pdf.",
                                   "- Smith & Okafor, 2019a. scan_0007.pdf.",
                                   "- Smith & Okafor, 2019b. scan_0412.pdf."))
  # With a title too, the entry leads with the key and the title follows.
  tab$title <- c("Effects of X", "A cohort of Y", "Effects of X in adults")
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "f"), question = "Q?", client = cl))
  expect_identical(s$references, c("- Garcia, 2022. A cohort of Y.",
                                   "- Smith & Okafor, 2019a. Effects of X.",
                                   "- Smith & Okafor, 2019b. Effects of X in adults."))
  # A key built from authors and year is followed through those, as before.
  tab2 <- r6s_table()[1:2, ]
  tab2$authors <- "Smith, J."
  tab2$year <- 2019L
  tab2$title <- c("B trial", "A trial")
  s <- quiet(gr_synthesise(tab2, outline = c(Findings = "f"), question = "Q?",
                           client = gr_mock_client(function(m, p) "Both [study 1] [study 2].")))
  expect_identical(s$references, c("- Smith, J. (2019a). A trial.", "- Smith, J. (2019b). B trial."))
})

# ---------------------------------------------------------------------------
# synthesis-14: no two studies ever share a key.
# ---------------------------------------------------------------------------

test_that("two in-press papers by the same authors are lettered, not merged into one citation", {
  # Before: both keys were "Smith & Okafor, in press", the text read "Two
  # trials found a benefit (Smith & Okafor, in press)." and the references
  # were two identical "- Smith, J., Okafor, A. (in press)." entries.
  tab <- r6s_table()[1:2, ]
  tab$authors <- "Smith, J., Okafor, A."
  tab$year <- "in press"
  tab$title <- c("Second paper", "First paper")
  cl <- gr_mock_client(function(m, p) "Two trials found a benefit [study 1] [study 2].")
  s <- quiet(gr_synthesise(tab, outline = c(Findings = "f"), question = "Q?", client = cl))
  expect_match(s$text, "(Smith & Okafor, in press-a; Smith & Okafor, in press-b)", fixed = TRUE)
  expect_identical(s$references, c("- Smith, J., Okafor, A. (in press-a). First paper.",
                                   "- Smith, J., Okafor, A. (in press-b). Second paper."))
  f <- readgpt:::bib_keys
  cols <- list(citation = "citation")
  # A letter that would repeat a key another row has is skipped.
  k <- f(data.frame(citation = c("Smith, 2019", "Smith, 2019", "Smith, 2019a")), cols)
  expect_identical(k, c("Smith, 2019b", "Smith, 2019c", "Smith, 2019a"))
  expect_identical(f(data.frame(citation = c("WHO, n.d.", "WHO, n.d.")), cols),
                   c("WHO, n.d.-a", "WHO, n.d.-b"))
  nar <- f(data.frame(authors = "Smith, J.", year = "in press"), list(authors = "authors", year = "year"),
           form = "narrative")
  expect_identical(nar, "Smith (in press)")
  nar2 <- f(data.frame(authors = "Smith, J.", year = c("in press", "in press")),
            list(authors = "authors", year = "year"), form = "narrative")
  expect_identical(nar2, c("Smith (in press-a)", "Smith (in press-b)"))
  # The letter a reference entry shows is the one the key was given, read off
  # the key rather than matched: a year printed "2019a" is not a letter.
  g <- readgpt:::bib_key_letter
  expect_identical(g("Smith, 2019b", "2019"), "b")
  expect_identical(g("Smith, 2019", "2019"), "")
  expect_identical(g("Smith, in press-a", "in press"), "-a")
  expect_identical(g("Smith, in press", "in press"), "")
  expect_identical(g("Smith, 2019a", "2019a"), "")
  expect_identical(g("Smith, 2019a-b", "2019a"), "-b")
  expect_identical(g("Smith, 2019aa", "2019"), "aa")
})

# ---------------------------------------------------------------------------
# r2-locale-platform-portability-07: a co-author whose surname starts with a
# letter outside ASCII is kept. Already fixed; kept as a guard.
# ---------------------------------------------------------------------------

test_that("co-authors whose surnames start with a non-ASCII letter are not dropped", {
  # At eeff205: "Smith, J., Ångström, K., Lee, A." gave the surnames
  # Smith and Lee, cited "(Smith & Lee, 2019)"; "Smith, J., Öztürk, E."
  # gave "(Smith, 2019)".
  f <- readgpt:::bib_surnames
  expect_identical(f("Smith, J., Ångström, K., Lee, A."),
                   c("Smith", "Ångström", "Lee"))
  expect_identical(f("Smith, J., Öztürk, E."), c("Smith", "Öztürk"))
  expect_identical(f("Álvarez, M., Østergaard, J."),
                   c("Álvarez", "Østergaard"))
  cols <- list(authors = "authors", year = "year")
  key <- function(a) readgpt:::bib_key(data.frame(authors = a, year = 2019L), cols)
  expect_identical(key("Smith, J., Öztürk, E."), "Smith & Öztürk, 2019")
  expect_identical(key("Smith, J., Ångström, K., Lee, A."), "Smith et al., 2019")
})

# ---------------------------------------------------------------------------
# synthesis-07 and r3-synthesis-layer-call-sizing-03: merging batch drafts
# never loses studies in silence.
# ---------------------------------------------------------------------------

r6s_batch_table <- function(n = 30L) {
  data.frame(document = sprintf("d%02d.pdf", 1:n), document_id = sprintf("h%02d", 1:n),
             status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
             conflicts = NA_character_,
             finding = paste(rep("The intervention reduced the outcome modestly in adults.", 3),
                             collapse = " "),
             stringsAsFactors = FALSE)
}

r6s_ids <- function(x) {
  as.integer(regmatches(x, gregexpr("(?<=\\[study )[0-9]+(?=\\])", x, perl = TRUE))[[1]])
}

# Each batch draft cites every study it was sent; `merge` makes the merge's
# reply from the drafts.
r6s_batch_client <- function(merge, draft = NULL) {
  gr_mock_client(function(m, p) {
    txt <- sent_text(m[-1])
    if (grepl("<draft", txt, fixed = TRUE)) return(merge(txt))
    ids <- r6s_ids(m[[length(m)]]$content)
    if (is.function(draft)) return(draft(ids))
    paste0("Draft: ", paste(sprintf("a benefit [study %d].", ids), collapse = " "))
  })
}

r6s_batched <- function(cl, ...) {
  r6s_warns(gr_synthesise(r6s_batch_table(), outline = c(Findings = "what"), question = "Q?",
                          client = cl, model = "r6s-tiny", max_section_tokens = 200, ...))
}

test_that("a merge that keeps only the first batch's studies marks the section partial", {
  # Before: three batch drafts cited all 30 studies, the merge finished normally
  # and kept only the first draft, and the section read 17 of 30 studies cited,
  # partial = FALSE, with no warning.
  local_registries()
  gr_register_model("r6s-tiny", context_window = 1400L, max_output = 400L,
                    input_usd = 0, output_usd = 0)
  first <- function(txt) paste("Merged:", regmatches(txt, gregexpr("Draft[^<]*", txt))[[1]][1])
  r <- r6s_batched(r6s_batch_client(first))
  labels <- vapply(r$value$trace$steps, `[[`, "", "label")
  expect_gt(sum(labels == "synthesise.batch"), 1L)
  expect_true(r$value$sections$partial)
  expect_true("gr_synth_batch_dropped" %in% r$classes)
  expect_lt(r$value$sections$n_cited, 30L)
  # A merge that keeps every draft's studies is a complete section.
  all_drafts <- function(txt) paste("Merged:", paste(regmatches(txt, gregexpr("Draft[^<]*", txt))[[1]],
                                                     collapse = " "))
  r <- r6s_batched(r6s_batch_client(all_drafts))
  expect_false(r$value$sections$partial)
  expect_identical(r$value$sections$n_cited, 30L)
  expect_false("gr_synth_batch_dropped" %in% r$classes)
})

test_that("a batch draft cut to fit the merge marks the section partial", {
  # tree_merge() cuts a piece longer than its prompt can take and reports it
  # in `$truncated`, which synth_section() never read: a merge that succeeded
  # over a cut draft was a complete section. Reached through tree_merge() that
  # is a merge that then makes no progress, already partial; the report is
  # read whichever way it arrives.
  local_registries()
  gr_register_model("r6s-tiny", context_window = 1400L, max_output = 400L,
                    input_usd = 0, output_usd = 0)
  local_mocked_bindings(tree_merge = function(client, question, pieces, ...) {
    list(text = paste(pieces, collapse = " "), ok = TRUE, levels = 1L, truncated = 1L,
         truncated_calls = 0L)
  })
  r <- r6s_batched(r6s_batch_client(identity))
  expect_true("gr_synth_merge_truncated" %in% r$classes)
  expect_true(r$value$sections$partial)
  expect_identical(r$value$sections$n_cited, 30L)
  local_mocked_bindings(tree_merge = function(client, question, pieces, ...) {
    list(text = paste(pieces, collapse = " "), ok = TRUE, levels = 1L, truncated = 0L,
         truncated_calls = 0L)
  })
  r <- r6s_batched(r6s_batch_client(identity))
  expect_false(r$value$sections$partial)
})

# ---------------------------------------------------------------------------
# r3-synthesis-layer-call-sizing-06: a claims block larger than the window.
# ---------------------------------------------------------------------------

test_that("a section whose claims block exceeds the window is written a few claims at a time", {
  # Before: the whole claims block was fixed overhead, gr_budget() raised
  # gr_budget_error, and the write-up aborted after the first two sections had
  # been written and paid for: no gr_synthesis came back.
  local_registries()
  gr_register_model("r6s-16k", context_window = 16384L, max_output = 4096L,
                    input_usd = 0, output_usd = 0)
  n <- 1000L
  tab <- data.frame(document = sprintf("d%04d.pdf", 1:n), document_id = sprintf("h%04d", 1:n),
                    status = "ok", duplicate_of = NA_character_, n_filled = 2L, n_unverified = 0L,
                    conflicts = NA_character_,
                    design = rep(c("trial", "cohort", "survey"), length.out = n),
                    stringsAsFactors = FALSE)
  prompts <- list()
  cl <- gr_mock_client(function(m, p) {
    sys <- m[[1]]$content
    txt <- sent_text(m)
    if (grepl("turn a table of studies", sys, fixed = TRUE)) {
      ids <- r6s_ids(m[[length(m)]]$content)
      cl5 <- vapply(1:5, function(k) r6s_claim(sprintf("Claim %d from %d.", k, ids[1]), ids), "")
      return(sprintf('{"claims":[%s]}', paste(cl5, collapse = ",")))
    }
    if (grepl("Group the ones", sys, fixed = TRUE)) return('{"groups":[]}')
    if (grepl("sections of a review", sys, fixed = TRUE)) {
      k <- as.integer(regmatches(txt, gregexpr("(?m)^[0-9]+(?=\\. \\[)", txt, perl = TRUE))[[1]])
      return(sprintf(paste0('{"sections":[{"heading":"A","brief":"a","claims":[1],"rationale":null},',
                            '{"heading":"B","brief":"b","claims":[2],"rationale":null},',
                            '{"heading":"Findings","brief":"f","claims":[%s],"rationale":null}]}'),
                     paste(setdiff(k, 1:2), collapse = ",")))
    }
    prompts[[length(prompts) + 1L]] <<- list(tokens = gr_count_tokens(txt), text = txt)
    # A draft cites every study its prompt names, which covers every claim it
    # was given; a merge cites every study in the drafts.
    paste0("Text ", paste(sprintf("[study %d]", unique(r6s_ids(sent_text(m[-1])))), collapse = " "),
           ".")
  })
  cm <- quiet(gr_claims(tab, question = "Q?", client = cl, model = "r6s-16k"))
  o <- quiet(gr_outline(cm, client = cl, model = "r6s-16k"))
  expect_gt(sum(attr(o, "claims")$section == "Findings"), 100L)
  prompts <- list()
  r <- r6s_warns(gr_synthesise(tab, outline = o, question = "Q?", client = cl, claims = cm,
                               model = "r6s-16k", max_section_tokens = 1200))
  s <- r$value
  expect_s3_class(s, "gr_synthesis")
  expect_identical(s$sections$section, c("A", "B", "Findings"))
  f <- s$sections[s$sections$section == "Findings", ]
  expect_identical(f$claims_missed, 0L)
  expect_false(f$partial)
  expect_identical(f$n_unsupplied, 0L)
  # Every prompt fits the window, and none resends the whole claims block.
  usable <- floor(16384 * (1 - gr_options("safety_margin")))
  expect_true(all(vapply(prompts, `[[`, 0, "tokens") + 1200 < usable))
  n_claims <- vapply(prompts, function(x) length(gregexpr("[claim ", x$text, fixed = TRUE)[[1]]), 0)
  expect_true(all(n_claims < f$n_claims))
  expect_false("gr_synth_too_large" %in% r$classes)
})

test_that("a section that cannot be sized is empty and partial, and the others survive", {
  # Before: any gr_budget_error in a section ended the write-up, discarding the
  # sections already written.
  local_registries()
  gr_register_model("r6s-small", context_window = 3000L, max_output = 400L,
                    input_usd = 0, output_usd = 0)
  cl <- gr_mock_client(function(m, p) "A benefit [study 1].")
  huge <- paste(rep("cover every aspect of the evidence in depth", 400), collapse = " ")
  r <- r6s_warns(gr_synthesise(r6s_table(), outline = c(Findings = "what", Everything = huge),
                               question = "Q?", client = cl, model = "r6s-small",
                               max_section_tokens = 300))
  s <- r$value
  expect_identical(s$sections$section, c("Findings", "Everything"))
  expect_false(s$sections$partial[1])
  expect_match(s$sections$text[1], "A benefit", fixed = TRUE)
  expect_true(s$sections$partial[2])
  expect_identical(s$sections$text[2], "")
  expect_true("gr_synth_too_large" %in% r$classes)
})

# ---------------------------------------------------------------------------
# synthesis-15: claims and outline from other runs, and tables without ids.
# ---------------------------------------------------------------------------

test_that("claims from another table are refused even when neither has id columns", {
  # Before: with no document_id or document column, the guard compared
  # character(0) with character(0), and with every document_id NA it compared
  # NAs with NAs, so claims drawn from a different table passed.
  cl <- r6s_client()
  bare <- r6s_table()
  bare$document <- NULL; bare$document_id <- NULL
  p <- r6s_prep(cl, bare)
  other <- bare
  other$design <- rev(other$design)
  expect_error(quiet(gr_synthesise(other, outline = p$o, question = "Q?", client = cl,
                                   claims = p$cm)), class = "gr_claims_mismatch")
  expect_s3_class(quiet(gr_synthesise(bare, outline = p$o, question = "Q?", client = cl,
                                      claims = p$cm)), "gr_synthesis")
  na_ids <- r6s_table()
  na_ids$document_id <- NA_character_
  p2 <- r6s_prep(cl, na_ids)
  moved <- na_ids
  moved$document <- rev(moved$document)
  expect_error(quiet(gr_synthesise(moved, outline = p2$o, question = "Q?", client = cl,
                                   claims = p2$cm)), class = "gr_claims_mismatch")
})

test_that("an outline stamped with other claims is refused", {
  # The claims each section argues are found by number, and numbers are
  # positions in one gr_claims() result. gr_outline() stamps the claims it was
  # given (a handoff to claims.R); stamped here by hand, the check refuses an
  # outline whose claims came back in another order.
  cl <- r6s_client()
  p <- r6s_prep(cl)
  swapped <- r6s_client(claims = sprintf('{"claims":[%s,%s]}',
                                         r6s_claim("Surveys show harm.", c(2, 4)),
                                         r6s_claim("Trials work.", c(1, 3))))
  cm2 <- quiet(gr_claims(r6s_table(), question = "Q?", client = swapped))
  o <- p$o
  attr(o, "claims_fingerprint") <- readgpt:::claims_fingerprint(p$cm$claims)
  expect_error(quiet(gr_synthesise(r6s_table(), outline = o, question = "Q?", client = cl,
                                   claims = cm2)), class = "gr_claims_mismatch")
  expect_s3_class(quiet(gr_synthesise(r6s_table(), outline = o, question = "Q?", client = cl,
                                      claims = p$cm)), "gr_synthesis")
})

# ---------------------------------------------------------------------------
# r3-synthesis-layer-call-sizing-07: the closing section of a write-up from
# claims reads the gaps, not every study.
# ---------------------------------------------------------------------------

test_that("with claims, the closing section is written from the gaps alone, or left out", {
  # Before: the closing section held no claim, fell back to the row-by-row
  # prompt and was sent every study.
  env <- new.env()
  cl <- r6s_client(env = env)
  p <- r6s_prep(cl)
  g <- data.frame(kind = "untested", dimension = "design", detail = "no cohort studies")
  s <- quiet(gr_synthesise(r6s_table(), outline = p$o, question = "Q?", client = cl,
                           claims = p$cm, gaps = g))
  expect_identical(s$sections$section, c("Trials", "Surveys", "What is missing"))
  closing <- env$seen[[length(env$seen)]]
  expect_match(closing[[1]]$content, "You write the 'What is missing' section", fixed = TRUE)
  expect_match(closing[[1]]$content, "no study records", fixed = TRUE)
  expect_false(grepl("<studies>", sent_text(closing), fixed = TRUE))
  expect_match(sent_text(closing), "no cohort studies", fixed = TRUE)
  wm <- s$sections[s$sections$section == "What is missing", ]
  expect_identical(wm$text, "No cohort study was found.")
  expect_false(wm$partial)
  # A study the gaps section names was never shown to it.
  cl2 <- gr_mock_client(function(m, p2) {
    if (grepl("does not cover", m[[1]]$content, fixed = TRUE)) return("Cohorts are missing [study 3].")
    "Trials found a benefit [study 1] [study 3]. Surveys found harm [study 2] [study 4]."
  })
  s2 <- quiet(gr_synthesise(r6s_table(), outline = p$o, question = "Q?", client = cl2,
                            claims = p$cm, gaps = g))
  wm2 <- s2$sections[s2$sections$section == "What is missing", ]
  expect_identical(wm2$n_unsupplied, 1L)
  expect_true(wm2$partial)
  # With no gaps there is nothing to write there, and it is left out.
  env$seen <- list()
  s3 <- quiet(gr_synthesise(r6s_table(), outline = p$o, question = "Q?", client = cl,
                            claims = p$cm))
  expect_identical(s3$sections$section, c("Trials", "Surveys"))
  expect_identical(length(env$seen), 2L)
})

test_that("gaps too long for the window are cut at a line and the closing section is partial", {
  local_registries()
  gr_register_model("r6s-small", context_window = 3000L, max_output = 400L,
                    input_usd = 0, output_usd = 0)
  env <- new.env()
  cl <- r6s_client(env = env)
  p <- r6s_prep(cl)
  g <- data.frame(kind = "combination unstudied", dimension = "design x setting",
                  detail = sprintf("design = 'trial' with setting = 'site %d'", 1:300))
  r <- r6s_warns(gr_synthesise(r6s_table(), outline = p$o, question = "Q?", client = cl,
                               claims = p$cm, gaps = g, model = "r6s-small",
                               max_section_tokens = 300))
  expect_true("gr_gaps_truncated" %in% r$classes)
  wm <- r$value$sections[r$value$sections$section == "What is missing", ]
  expect_true(wm$partial)
  closing <- sent_text(env$seen[[length(env$seen)]])
  expect_match(closing, "site 1'", fixed = TRUE)
  expect_false(grepl("site 300'", closing, fixed = TRUE))
  # Cut between lines, never inside one.
  body <- sub("(?s)^.*<gaps>\n(.*)\n</gaps>.*$", "\\1", closing, perl = TRUE)
  expect_true(all(grepl("^- combination unstudied \\(design x setting\\): design = 'trial' with setting = 'site [0-9]+'$",
                        strsplit(body, "\n", fixed = TRUE)[[1]])))
})

# ---------------------------------------------------------------------------
# Handoff H3 (corpus-trace-2): print counts model calls apart from embeddings.
# ---------------------------------------------------------------------------

test_that("print counts a synthesis's model calls apart from embeddings requests", {
  cl <- gr_mock_client(function(m, p) "A benefit [study 1].")
  s <- quiet(gr_synthesise(r6s_table(), outline = c(Findings = "f"), question = "Q?", client = cl))
  tr <- s$trace
  tr$steps[[length(tr$steps) + 1L]] <- list(kind = "embedding", label = "embed")
  tr$calls <- tr$calls + 1L
  s$trace <- tr
  line <- grep("this run:", capture.output(print(s)), value = TRUE, fixed = TRUE)
  expect_match(line, "this run: 1 model call(s), 1 embeddings request(s), ", fixed = TRUE)
})
