# test-synthesise.R -- the write-up, and whether its citations point anywhere.
#
# The property that matters is not prose quality, which no test can assess. It is
# that every claim can be walked back: a sentence cites a study, the study is a
# row that exists, and the row names the document it came from. A citation to a
# row that is not in the table is a fabrication, and it must be reported rather
# than read past.

synth_fields <- function() {
  gr_fields(design = "The study design",
            n = gr_field("Participants", type = "integer"))
}

# A client that extracts on one prompt shape and writes on the other.
synth_client <- function(section_text = "A trial [study 1] and a cohort [study 2].") {
  gr_mock_client(function(messages, params) {
    seen <- paste(vapply(messages, function(m) paste(as.character(m$content), collapse = ""),
                         character(1)), collapse = "\n")
    if (grepl("<studies>", seen, fixed = TRUE)) {
      return(if (is.function(section_text)) section_text(seen) else section_text)
    }
    if (grepl("randomised trial", seen, fixed = TRUE)) {
      return(paste0('{"design":"randomised trial","n":120,',
                    '"design__quote":"We ran a randomised trial.",',
                    '"n__quote":"We enrolled 120 people."}'))
    }
    if (grepl("cohort study", seen, fixed = TRUE)) {
      return(paste0('{"design":"cohort study","n":900,',
                    '"design__quote":"We ran a cohort study.",',
                    '"n__quote":"We followed 900 people."}'))
    }
    '{"design":null,"n":null,"design__quote":null,"n__quote":null}'
  })
}

two_studies <- function(cl, extra = character(0)) {
  f <- function(txt) { p <- tempfile(fileext = ".txt"); writeLines(txt, p); p }
  srcs <- c(f("We ran a randomised trial. We enrolled 120 people."),
            f("We ran a cohort study. We followed 900 people."), extra)
  quiet(gr_extract(srcs, synth_fields(), client = cl, recipe = "thorough", max_tokens = 40))
}

test_that("each section is one call against its own brief", {
  cl <- synth_client()
  x <- two_studies(cl)
  n_extract <- length(cl$calls())

  s <- quiet(gr_synthesise(x, question = "Does it work?",
                           outline = c("Included studies" = "How many, of what design",
                                       "Findings" = "What they found"),
                           client = cl))
  expect_s3_class(s, "gr_synthesis")
  expect_equal(nrow(s$sections), 2L)
  expect_identical(s$sections$section, c("Included studies", "Findings"))
  expect_identical(length(cl$calls()) - n_extract, 2L)

  # The brief reaches the model: a section written against the wrong one would
  # be indistinguishable from a section written against none.
  wrote <- Filter(function(c) grepl("<studies>", paste(vapply(c$messages,
    function(m) paste(as.character(m$content), collapse = ""), character(1)),
    collapse = "\n"), fixed = TRUE), cl$calls())
  seen <- vapply(wrote, function(c) paste(vapply(c$messages,
    function(m) paste(as.character(m$content), collapse = ""), character(1)),
    collapse = "\n"), character(1))
  expect_true(any(grepl("How many, of what design", seen, fixed = TRUE)))
  expect_true(any(grepl("What they found", seen, fixed = TRUE)))
  expect_true(all(grepl("Does it work?", seen, fixed = TRUE)))

  # The assembled document carries the headings; the sections do not repeat them.
  expect_true(grepl("## Included studies", s$text, fixed = TRUE))
  expect_true(grepl("## Findings", s$text, fixed = TRUE))
})

test_that("every citation resolves to a row, and the ones that do not are reported", {
  cl <- synth_client("A trial [study 1] and a cohort [study 2], plus [study 7].")
  x <- two_studies(cl)
  s <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl))

  expect_identical(s$sections$n_cited, 2L)
  expect_identical(s$sections$n_unknown, 1L)
  expect_true(s$sections$partial)
  expect_output(print(s), "TO A ROW THAT DOES NOT EXIST")

  # And the ones that do resolve carry the document they point at -- each its
  # own, not all of them glued together.
  expect_equal(nrow(s$citations), 2L)
  expect_identical(s$citations$study, c(1L, 2L))
  expect_false(anyDuplicated(s$citations$document_id) > 0L)
  expect_identical(s$citations$document, x$table$document[1:2])
  expect_identical(s$citations$document_id, x$table$document_id[1:2])
})

test_that("a section that cites nothing is flagged, not passed off as prose", {
  cl <- synth_client("Both studies broadly agreed with one another.")
  x <- two_studies(cl)
  s <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl))
  expect_identical(s$sections$n_cited, 0L)
  expect_identical(s$sections$n_unknown, 0L)
  expect_false(s$sections$partial)         # nothing fabricated, but
  expect_output(print(s), "NONE")          # visibly unsupported
  expect_equal(nrow(s$citations), 0L)
  expect_named(s$citations, c("section", "study", "document", "document_id"))
})

test_that("an empty section is partial", {
  cl <- synth_client("")
  x <- two_studies(cl)
  s <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl))
  expect_true(s$sections$partial)
})

test_that("a duplicate study is never written up twice", {
  # The error the whole pipeline exists to avoid, and the easiest place to make
  # it: the rows all look alike by the time they reach here.
  cl <- synth_client()
  f <- function(txt) { p <- tempfile(fileext = ".txt"); writeLines(txt, p); p }
  txt <- "We ran a randomised trial. We enrolled 120 people."
  x <- quiet(gr_extract(c(f(txt), f(txt)), synth_fields(), client = cl,
                        recipe = "thorough", max_tokens = 40))
  expect_identical(x$table$status, c("ok", "duplicate"))

  s <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl))
  expect_equal(nrow(s$studies), 1L)
  expect_identical(s$skipped, 1L)
  expect_output(print(s), "left out")
})

test_that("rows that were never read are left out, and rows with nothing in them", {
  cl <- synth_client()
  x <- two_studies(cl, extra = c("no-such-file.txt",
                                 { p <- tempfile(fileext = ".txt")
                                   writeLines("Nothing relevant in this one.", p); p }))
  expect_identical(x$table$status, c("ok", "ok", "failed", "ok"))
  expect_identical(x$table$n_filled[4], 0L)

  s <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl))
  expect_equal(nrow(s$studies), 2L)
  expect_identical(s$skipped, 2L)

  # Unless you ask for them: a document that reports none of the fields is a
  # finding, and a review may want to say so.
  s2 <- quiet(gr_synthesise(x, question = "Q?", outline = c(Findings = "What they found"),
                            client = cl, include_unclear = TRUE))
  expect_equal(nrow(s2$studies), 3L)       # the failed one is still out
  expect_identical(s2$skipped, 1L)
})

test_that("the studies handed to the model carry their values and their document", {
  cl <- synth_client()
  x <- two_studies(cl)
  used <- x$table[1:2, ]
  used$study <- 1:2
  rendered <- readgpt:::render_studies(used)

  expect_length(rendered, 2L)
  expect_true(grepl("[study 1]", rendered[1], fixed = TRUE))
  expect_true(grepl("design: randomised trial", rendered[1], fixed = TRUE))
  expect_true(grepl("n: 120", rendered[1], fixed = TRUE))
  expect_true(grepl(used$document[1], rendered[1], fixed = TRUE))
  # A field nothing reported says so, rather than being silently absent: "not
  # reported" is a finding, and a review that cannot see it cannot report it.
  empty <- used[1, ]; empty$n <- NA_integer_; empty$study <- 1L
  expect_true(grepl("n: not reported", readgpt:::render_studies(empty), fixed = TRUE))
  # Bookkeeping columns are not offered to the model as findings.
  expect_false(grepl("n_filled", rendered[1], fixed = TRUE))
  expect_false(grepl("duplicate_of", rendered[1], fixed = TRUE))
})

test_that("a table too large for one prompt is written in batches, not truncated", {
  # Taking the first N rows, or summarising the table first, both drop studies
  # without saying which -- the one thing a review may not do.
  local_registries()
  gr_register_model("tiny-ctx", context_window = 900L, max_output = 200L,
                    input_usd = 0, output_usd = 0)
  tab <- data.frame(
    document = sprintf("paper%02d.txt", 1:12),
    document_id = sprintf("id%02d", 1:12),
    design = rep(paste(rep("a randomised controlled trial of some description", 6),
                       collapse = " "), 12),
    n = seq(100L, by = 10L, length.out = 12L),
    n_filled = 2L, n_unverified = 0L, conflicts = NA_character_,
    status = "ok", duplicate_of = NA_character_, error = NA_character_,
    stringsAsFactors = FALSE)

  seen_ids <- new.env(parent = emptyenv()); seen_ids$v <- integer(0)
  cl <- gr_mock_client(function(messages, params) {
    txt <- paste(vapply(messages, function(m) paste(as.character(m$content), collapse = ""),
                        character(1)), collapse = "\n")
    ids <- readgpt:::cited_ids(txt, "study")
    seen_ids$v <- c(seen_ids$v, ids)
    paste(sprintf("Point about [study %d].", ids), collapse = " ")
  })
  s <- quiet(gr_synthesise(tab, question = "Q?", outline = c(Findings = "What they found"),
                           client = cl, model = "tiny-ctx", max_section_tokens = 200L))

  expect_gt(length(cl$calls()), 1L)                    # it batched
  expect_true(all(1:12 %in% seen_ids$v))               # and no study was dropped
  expect_identical(nrow(s$studies), 12L)
})

test_that("gr_synthesise() validates before spending anything", {
  cl <- synth_client()
  x <- two_studies(cl)
  n <- length(cl$calls())
  expect_error(gr_synthesise(x, question = "Q?", client = cl), class = "gr_no_outline")
  expect_error(gr_synthesise(x, outline = c(A = "a"), client = cl))
  expect_error(gr_synthesise(data.frame(), question = "Q?", outline = c(A = "a"),
                             client = cl), class = "gr_no_studies")
  expect_error(gr_synthesise(x, protocol = list(outline = c(A = "a")), client = cl),
               class = "gr_bad_protocol")
  expect_identical(length(cl$calls()), n)

  # Every row unusable is an error, not an empty document that reads as a result.
  dead <- x
  dead$table$status <- "failed"
  expect_error(gr_synthesise(dead, question = "Q?", outline = c(A = "a"), client = cl),
               class = "gr_no_studies")
})

test_that("a protocol supplies the outline and the question", {
  p <- gr_protocol("t", question = "Does the drug reduce events?",
                   fields = synth_fields(),
                   outline = c("Included studies" = "How many, of what design",
                               "Findings" = "What they found"))
  cl <- synth_client()
  x <- two_studies(cl)
  s <- quiet(gr_synthesise(x, p, client = cl))
  expect_identical(s$question, p$question)
  expect_identical(s$outline, p$outline)
  expect_identical(s$sections$section, names(p$outline))

  # An explicit argument still wins.
  s2 <- quiet(gr_synthesise(x, p, outline = c(Only = "Just this"), client = cl))
  expect_identical(s2$sections$section, "Only")
})
