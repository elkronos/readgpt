# test-review6-extract.R -- the sixth pass on extraction and the corpus loop:
# an empty array crashed a paid-for document; a boolean read every word but
# four as FALSE; numbers lost their sign, their decimal comma, their scale
# and their script; an adjudicator's "none of these" was thrown away, and one
# skipped at the call limit failed a fully read document; two spellings of one
# string were a paid conflict; `reader =` in `...` replaced the extract reader;
# a restored duplicate kept naming a document not in the run; a failing
# document's calls went missing from the parent trace under on_error = "stop";
# restorable documents were skipped at a run ceiling; and the store key
# depended on the time zone.

r6_file <- function(..., dir = NULL, name = NULL) {
  f <- if (is.null(dir)) tempfile(fileext = ".txt") else file.path(dir, name)
  writeBin(charToRaw(enc2utf8(paste0(paste(c(...), collapse = "\n\n"), "\n"))), f)
  f
}

# Paragraphs long enough that the paragraph segmenter at max_tokens = 48 puts
# each in a chunk of its own.
r6_say <- function(s) paste(rep(s, 3), collapse = " ")

r6_json <- function(...) {
  as.character(jsonlite::toJSON(list(...), auto_unbox = TRUE, digits = NA, null = "null"))
}

r6_labels <- function(cl) vapply(cl$calls(), function(c) as.character(c$label), character(1))

# ---------------------------------------------------------------------------
# corpus-extract-07: an empty array or object is no value, not a crash
# ---------------------------------------------------------------------------

test_that("a field sent as [] or {} is empty, and the document's other values stand", {
  cf <- readgpt:::coerce_field
  for (type in c("string", "integer", "number", "boolean")) {
    fl <- gr_field("x", type = type)
    expect_null(cf(list(), fl), info = type)
    expect_null(cf(stats::setNames(list(), character(0)), fl), info = type)
    expect_null(cf(list(list()), fl), info = type)
    expect_null(cf(character(0), fl), info = type)
  }
  expect_null(cf(list(), gr_field("x", type = "enum", values = c("a", "b"))))

  txt <- "This was a randomised controlled trial. We enrolled 120 patients."
  fl <- gr_fields(design = "The design", n = gr_field("Participants", type = "integer"),
                  funded = gr_field("Industry funding declared", type = "boolean"))
  for (empty in c("[]", "{}")) {
    cl <- gr_mock_client(function(m, p) paste0(
      '{"design":"randomised controlled trial","n":120,"funded":', empty, ',',
      '"design__quote":"This was a randomised controlled trial.",',
      '"n__quote":"We enrolled 120 patients.","funded__quote":null}'))
    x <- quiet(gr_extract(r6_file(txt), fl, client = cl))
    # It crashed with "subscript out of bounds" after the call was paid, and
    # the document failed with every value it had.
    expect_identical(x$table$status, "ok", info = empty)
    expect_identical(x$table$design, "randomised controlled trial", info = empty)
    expect_identical(x$table$n, 120L, info = empty)
    expect_true(is.na(x$table$funded), info = empty)
    expect_identical(x$table$n_unverified, 0L, info = empty)
  }
})

# ---------------------------------------------------------------------------
# corpus-extract-05: a boolean is a yes, a no, or nothing
# ---------------------------------------------------------------------------

test_that("a boolean field reads only the words that answer yes or no", {
  cf <- readgpt:::coerce_field
  b <- gr_field("f", type = "boolean")
  for (s in c("unclear", "Yes, industry funded", "not stated", "unknown", "partially",
              "Yes - Pfizer funded it", "maybe", "2")) {
    expect_null(cf(s, b), info = s)
  }
  for (s in c("yes", "Yes", "Y", "true", "TRUE", "1", "Yes.")) expect_true(cf(s, b), info = s)
  for (s in c("no", "No", "N", "false", "0", "No.")) expect_false(cf(s, b), info = s)
  expect_true(cf(TRUE, b))
  expect_false(cf(0, b))
  expect_true(cf(1L, b))
})

test_that("a boolean that is not a yes or a no is unknown, not a verified FALSE", {
  txt <- "The trial was funded by Pfizer. We enrolled 120 patients."
  fl <- gr_fields(n = gr_field("Participants", type = "integer"),
                  funded = gr_field("Industry funding declared", type = "boolean"))
  cl <- gr_mock_client(function(m, p) r6_json(
    n = 120, n__quote = "We enrolled 120 patients.",
    funded = "Yes - Pfizer funded it", funded__quote = "The trial was funded by Pfizer."))
  x <- quiet(gr_extract(r6_file(txt), fl, client = cl, keep_answers = TRUE))
  # It was FALSE, verified, n_unverified 0: an industry-funded trial counted
  # as unfunded.
  expect_true(is.na(x$table$funded))
  expect_identical(x$table$n, 120L)
  expect_identical(x$table$n_unverified, 1L)
  expect_identical(x$table$status, "ok")
  a <- x$answers[[1]]
  expect_true(a$partial)
  expect_identical(a$notes$unreadable, "funded")
  expect_true("funded" %in% a$notes$unknown)
  expect_false("funded" %in% a$notes$not_reported)
  expect_output(print(x), "could not be read as its field's type", fixed = TRUE)

  # A way of saying "not reported" is still that, and flags nothing.
  cl2 <- gr_mock_client(function(m, p) r6_json(
    n = 120, n__quote = "We enrolled 120 patients.", funded = "not stated", funded__quote = NULL))
  y <- quiet(gr_extract(r6_file(txt), fl, client = cl2, keep_answers = TRUE))
  expect_true(is.na(y$table$funded))
  expect_identical(y$table$n_unverified, 0L)
  expect_identical(y$answers[[1]]$notes$not_reported, "funded")
  expect_false(y$answers[[1]]$partial)
})

# ---------------------------------------------------------------------------
# corpus-extract-06 and r2-non-latin-text-pipeline-11: how a number is written
# ---------------------------------------------------------------------------

test_that("a number keeps its sign, its decimal point and its scale, or is a miss", {
  cf <- readgpt:::coerce_field
  num <- gr_field("x", type = "number")
  int <- gr_field("x", type = "integer")
  # A typeset minus sign or en dash is a minus.
  expect_identical(cf("\u22120.35", num), -0.35)
  expect_identical(cf("\u20130.35", num), -0.35)
  expect_identical(cf("HR \u22120.2 (95% CI)", num), NULL)   # two numbers
  expect_identical(cf("HR \u22120.2", num), -0.2)
  expect_identical(cf("\u2212120", int), -120L)
  # A leading decimal point is a decimal point.
  expect_identical(cf(".03", num), 0.03)
  expect_identical(cf("p = .03", num), 0.03)
  expect_identical(cf("d = .45", num), 0.45)
  # A comma is a thousands separator between groups of three, a decimal comma
  # after a lone zero, and otherwise a miss rather than a guess.
  expect_identical(cf("1,204", int), 1204L)
  expect_identical(cf("1,204.5", num), 1204.5)
  expect_identical(cf("OR 0,85", num), 0.85)
  expect_null(cf("1,5", num))
  expect_null(cf("12,50", num))
  expect_null(cf("0,85", int))                              # not a whole number
  # A scale word is not dropped: 3 is not the value.
  for (s in c("3 million", "about 2.5 million adults", "12k", "1.2 bn", "3 Million",
              "2 lakh", "1,2 Mio.", "3\u4e07")) {
    expect_null(cf(s, int), info = s)
    expect_null(cf(s, num), info = s)
  }
  # A unit is not a scale.
  expect_identical(cf("1.2 m", num), 1.2)
  expect_identical(cf("5 mg", int), 5L)
  expect_identical(cf("120\u4f8b", int), 120L)
  # A fraction is not rounded into a whole number.
  expect_null(cf("12.7", int))
  expect_null(cf(120.6, int))
  expect_identical(cf(120, int), 120L)
  expect_identical(cf(3e9, int), 3e9)

  nc <- readgpt:::n_column
  expect_identical(nc(c("\u2212120", "3 million", "900 participants", ".5")),
                   c(-120, NA, 900, 0.5))
})

test_that("digits of other scripts are read, not recorded as not reported", {
  cf <- readgpt:::coerce_field
  num <- gr_field("x", type = "number")
  int <- gr_field("x", type = "integer")
  expect_identical(cf("\u0661\u0662\u0660", int), 120L)            # Arabic-Indic
  expect_identical(cf("\u06f1\u06f2\u06f0", int), 120L)            # Persian
  expect_identical(cf("\uff11\uff12\uff10", int), 120L)            # full-width
  expect_identical(cf("\uff0d\uff10\uff0e\uff13\uff15", num), -0.35)
  expect_identical(cf("\u0663\u066b\u0665", num), 3.5)             # Arabic decimal separator
  expect_identical(readgpt:::n_column(c("\u0661\u0662\u0660 \u0645\u0631\u064a\u0636\u0627",
                                        "\uff11\uff12\uff10\u4f8b")), c(120, 120))

  # End to end: the value is read, and its verbatim quote verifies it.
  quote <- "\u0634\u0645\u0644\u062a \u0627\u0644\u062f\u0631\u0627\u0633\u0629 \u0661\u0662\u0660 \u0645\u0631\u064a\u0636\u0627"
  fl <- gr_fields(n = gr_field("Participants", type = "integer"))
  cl <- gr_mock_client(function(m, p) r6_json(n = "\u0661\u0662\u0660", n__quote = quote))
  x <- quiet(gr_extract(r6_file(quote), fl, client = cl, keep_answers = TRUE))
  expect_identical(x$table$n, 120L)
  expect_identical(x$table$n_filled, 1L)
  expect_identical(x$table$n_unverified, 0L)
  expect_false("n" %in% x$answers[[1]]$notes$not_reported)
})

test_that("a value that could not be read is unknown and counted, not 'not reported'", {
  txt <- "We randomised 120 patients (60 per arm)."
  fl <- gr_fields(n = gr_field("Participants", type = "integer"))
  cl <- gr_mock_client(function(m, p) r6_json(n = "120 (60 per arm)", n__quote = txt))
  x <- quiet(gr_extract(r6_file(txt), fl, client = cl, keep_answers = TRUE))
  expect_true(is.na(x$table$n))
  expect_identical(x$table$n_filled, 0L)
  expect_identical(x$table$n_unverified, 1L)
  expect_identical(x$table$status, "ok")
  expect_identical(x$answers[[1]]$notes$not_reported, character(0))
  expect_identical(x$answers[[1]]$notes$unknown, "n")
  expect_true(x$summary$partial)
  # Under require_quote too: nothing to drop, and still counted.
  y <- quiet(gr_extract(r6_file(txt), fl, client = cl, require_quote = TRUE))
  expect_identical(y$table$n_unverified, 1L)
})

# ---------------------------------------------------------------------------
# corpus-extract-08 and corpus-extract-12: what resolve = "model" did not settle
# ---------------------------------------------------------------------------

r6_conflict_doc <- function() {
  r6_file(r6_say("The abstract reports that we enrolled 120 patients in the trial overall."),
          r6_say("The results section reports that we enrolled 150 patients in the trial overall."))
}

r6_conflict_client <- function(choice) {
  gr_mock_client(function(m, p) {
    if (grepl("different values", m[[1]]$content, fixed = TRUE)) {
      if (identical(choice, "fail")) stop("HTTP 500 server error")
      return(sprintf('{"choice": %s}', choice))
    }
    ex <- m[[length(m)]]$content
    if (grepl("abstract", ex, fixed = TRUE)) {
      r6_json(n = 120, n__quote = "The abstract reports that we enrolled 120 patients in the trial overall.")
    } else {
      r6_json(n = 150, n__quote = "The results section reports that we enrolled 150 patients in the trial overall.")
    }
  })
}

test_that("an adjudicator that chooses none of the values leaves the kept value unverified", {
  fl <- gr_fields(n = gr_field("Participants", type = "integer"))
  doc <- r6_conflict_doc()
  x <- quiet(gr_extract(doc, fl, client = r6_conflict_client(0), recipe = "thorough",
                        max_tokens = 48, resolve = "model", keep_answers = TRUE))
  # It was n = 120, verified, n_unverified 0 and not partial: the verdict the
  # call was paid for was thrown away.
  expect_identical(x$table$n, 120L)
  expect_identical(x$table$conflicts, "n")
  expect_identical(x$table$n_unverified, 1L)
  expect_identical(x$table$status, "ok")
  expect_true(x$summary$partial)
  expect_identical(x$evidence$verified, FALSE)
  expect_identical(x$evidence$match, 1)
  expect_identical(x$answers[[1]]$notes$unresolved, c(n = "rejected"))
  expect_match(x$summary$warnings, "'n' (the adjudicating request chose none of the values)",
               fixed = TRUE)
  # require_quote drops it, as it drops any value that is not verified.
  y <- quiet(gr_extract(doc, fl, client = r6_conflict_client(0), recipe = "thorough",
                        max_tokens = 48, resolve = "model", require_quote = TRUE))
  expect_true(is.na(y$table$n))
  expect_identical(y$table$n_unverified, 1L)

  # A real choice is kept, verified, with nothing unresolved.
  z <- quiet(gr_extract(doc, fl, client = r6_conflict_client(2), recipe = "thorough",
                        max_tokens = 48, resolve = "model", keep_answers = TRUE))
  expect_identical(z$table$n, 150L)
  expect_identical(z$table$n_unverified, 0L)
  expect_length(z$answers[[1]]$notes$unresolved, 0L)
  expect_true(is.na(z$summary$warnings))
})

test_that("a failed adjudication is recorded, and the read stays whole", {
  fl <- gr_fields(n = gr_field("Participants", type = "integer"))
  for (choice in c("fail", "7")) {
    x <- quiet(gr_extract(r6_conflict_doc(), fl, client = r6_conflict_client(choice),
                          recipe = "thorough", max_tokens = 48, resolve = "model",
                          keep_answers = TRUE))
    expect_identical(x$table$n, 120L, info = choice)
    expect_identical(x$table$status, "ok", info = choice)
    expect_identical(x$table$n_unverified, 0L, info = choice)
    expect_identical(x$answers[[1]]$notes$unresolved, c(n = "failed"), info = choice)
    expect_match(x$summary$warnings, "'n' (the adjudicating request failed)", fixed = TRUE,
                 info = choice)
  }
})

test_that("an adjudication skipped at the call limit does not fail a fully read document", {
  local_registries()
  fl <- gr_fields(n = gr_field("Participants", type = "integer"))
  doc <- r6_conflict_doc()
  first <- quiet(gr_extract(doc, fl, client = r6_conflict_client(2), recipe = "thorough",
                            max_tokens = 48))
  gr_options(max_calls = first$summary$chunks)
  store <- withr::local_tempdir()
  cl <- r6_conflict_client(2)
  x <- quiet(gr_extract(doc, fl, client = cl, recipe = "thorough", max_tokens = 48,
                        resolve = "model", store = store, keep_answers = TRUE))
  # It was "failed ... stopped at the N-request limit before the document was
  # read in full", with n_filled NA and nothing stored.
  expect_identical(x$table$status, "ok")
  expect_identical(x$table$n, 120L)
  expect_identical(x$table$n_filled, 1L)
  expect_true(is.na(x$table$error))
  expect_false(any(r6_labels(cl) == "extract.resolve"))
  a <- x$answers[[1]]
  expect_identical(a$notes$unresolved, c(n = "limit"))
  expect_null(a$notes$call_cap_reached)
  expect_match(x$summary$warnings, "a request limit was reached before it could be made",
               fixed = TRUE)
  # And it was stored, so a resumed run pays for nothing.
  before <- length(cl$calls())
  again <- quiet(gr_extract(doc, fl, client = cl, recipe = "thorough", max_tokens = 48,
                            resolve = "model", store = store))
  expect_identical(again$table$status, "restored")
  expect_identical(length(cl$calls()), before)
})

# ---------------------------------------------------------------------------
# corpus-extract-13: two spellings of one string are one value
# ---------------------------------------------------------------------------

test_that("string values that differ only in case or a trailing full stop do not conflict", {
  vk <- readgpt:::value_key
  expect_identical(vk("Randomised controlled trial", "string"),
                   vk("randomised controlled trial.", "string"))
  expect_identical(vk("Adults", "string"), vk("\u201cadults\u201d", "string"))
  expect_false(identical(vk("-0.3", "string"), vk("0.3", "string")))
  expect_false(identical(vk("54%", "string"), vk("54", "string")))
  expect_false(identical(vk("adults", "string"), vk("children", "string")))
  # An enum value and an untyped key are compared as they are.
  expect_false(identical(vk("Yes", "enum"), vk("yes", "enum")))
  expect_false(identical(vk("Yes"), vk("yes")))

  fl <- gr_fields(design = "The design", population = "The population")
  doc <- r6_file(r6_say("This was a Randomised controlled trial of the drug in Adults overall."),
                 r6_say("In summary this was a randomised controlled trial of the drug in adults."))
  cl <- gr_mock_client(function(m, p) {
    if (grepl("different values", m[[1]]$content, fixed = TRUE)) return('{"choice": 1}')
    ex <- m[[length(m)]]$content
    if (grepl("Randomised", ex, fixed = TRUE)) {
      r6_json(design = "Randomised controlled trial", population = "Adults",
              design__quote = "This was a Randomised controlled trial of the drug in Adults overall.",
              population__quote = "This was a Randomised controlled trial of the drug in Adults overall.")
    } else {
      r6_json(design = "randomised controlled trial.", population = "adults",
              design__quote = "In summary this was a randomised controlled trial of the drug in adults.",
              population__quote = "In summary this was a randomised controlled trial of the drug in adults.")
    }
  })
  x <- quiet(gr_extract(doc, fl, client = cl, recipe = "thorough", max_tokens = 48,
                        resolve = "model"))
  expect_true(is.na(x$table$conflicts))
  expect_identical(x$table$design, "Randomised controlled trial")
  expect_false(any(r6_labels(cl) == "extract.resolve"))
  expect_false(any(grepl("contradicted", capture.output(print(x)), fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# corpus-extract-14: the reader cannot be overridden through `...`
# ---------------------------------------------------------------------------

test_that("reader = in ... is refused before anything is read", {
  fl <- gr_fields(n = gr_field("Participants", type = "integer"))
  cl <- gr_mock_client(function(m, p) "The study enrolled 300 patients.")
  expect_error(gr_extract(r6_file("We enrolled 300 patients."), fl, client = cl,
                          reader = "stuff"),
               class = "gr_bad_override")
  expect_length(cl$calls(), 0L)
})

# ---------------------------------------------------------------------------
# corpus-extract-09: a restored duplicate is decided in the run that restores it
# ---------------------------------------------------------------------------

test_that("a restored duplicate whose first copy is not in this run is the first copy now", {
  d <- withr::local_tempdir()
  store <- file.path(d, "store")
  src <- file.path(d, "src"); dir.create(src)
  a <- r6_file("We enrolled 120 patients.", dir = src, name = "a.txt")
  b <- r6_file("We enrolled 120 patients.", dir = src, name = "b.txt")
  c <- r6_file("We enrolled 300 patients.", dir = src, name = "c.txt")
  fl <- gr_fields(n = gr_field("Participants", type = "integer"))
  cl <- gr_mock_client(function(m, p) {
    if (grepl("120", m[[length(m)]]$content, fixed = TRUE)) {
      r6_json(n = 120, n__quote = "We enrolled 120 patients.")
    } else r6_json(n = 300, n__quote = "We enrolled 300 patients.")
  })
  first <- quiet(gr_extract(src, fl, client = cl, store = store))
  expect_identical(first$table$duplicate_of, c(NA, "a.txt", NA))

  # Without a.txt: b.txt restored with duplicate_of "a.txt", a row that is not
  # there, and the study dropped out of the distinct set.
  x <- quiet(gr_extract(c(b, c), fl, client = cl, store = store))
  expect_identical(x$table$status, c("restored", "restored"))
  expect_identical(x$table$duplicate_of, c(NA_character_, NA_character_))
  expect_identical(subset(x$table, is.na(duplicate_of))$n, c(120L, 300L))

  # In the other order the first copy comes second, and is the duplicate: the
  # study is counted once, not twice.
  y <- quiet(gr_extract(c(b, a, c), fl, client = cl, store = store))
  expect_identical(y$table$duplicate_of, c(NA, "b.txt", NA))
  expect_identical(nrow(subset(y$table, is.na(duplicate_of))), 2L)
  expect_length(cl$calls(), 2L)          # the first run's; none since

  # A legacy entry with no hash keeps a duplicate_of only when it names a row
  # earlier in this run.
  csd <- readgpt:::corpus_stored_duplicate
  expect_identical(csd("a.txt", c("a.txt", "c.txt")), "a.txt")
  expect_identical(csd("a.txt", "c.txt"), NA_character_)
  expect_identical(csd(NULL, "c.txt"), NA_character_)
})

# ---------------------------------------------------------------------------
# corpus-extract-10: a failing document's calls reach the parent under "stop"
# ---------------------------------------------------------------------------

test_that("on_error = 'stop' still folds the failing document's calls into the traces", {
  local_registries()
  gr_register_reader("r6_crash", function(chunks, question, client, spec, trace) {
    for (k in 1:3) {
      gr_call(client, list(list(role = "user", content = "x")), trace = trace, label = "r6.call")
    }
    stop("the reader crashed after its calls")
  }, signature = "all|3|none")
  d <- withr::local_tempdir()
  r6_file("Alpha document text one.", dir = d, name = "a.txt")
  r6_file("Beta document text two.", dir = d, name = "b.txt")
  cl <- gr_mock_client(function(m, p) "ok")
  parent <- gr_trace()
  expect_error(quiet(gr_read_many(d, "Q?", "fast", client = cl, reader = "r6_crash",
                                  on_error = "stop", trace = parent)),
               "the reader crashed after its calls", fixed = TRUE)
  # It recorded none of them: the absorb came after the rethrow.
  expect_identical(length(cl$calls()), 3L)
  expect_identical(parent$calls, 3L)

  # Under "continue" every document is folded in once, not twice.
  parent2 <- gr_trace()
  res <- quiet(gr_read_many(d, "Q?", "fast", client = cl, reader = "r6_crash", trace = parent2))
  expect_identical(res$summary$status, c("failed", "failed"))
  expect_identical(res$trace$calls, 6L)
  expect_identical(parent2$calls, 6L)
})

# ---------------------------------------------------------------------------
# corpus-extract-11: a run ceiling does not skip what the store can restore
# ---------------------------------------------------------------------------

test_that("documents in the store are restored after a run ceiling is reached", {
  d <- withr::local_tempdir()
  store <- file.path(d, "store")
  src <- file.path(d, "src"); dir.create(src)
  r6_file("We enrolled 120 patients.", dir = src, name = "b.txt")
  r6_file("We enrolled 300 patients.", dir = src, name = "c.txt")
  fl <- gr_fields(n = gr_field("Participants", type = "integer"))
  cl <- gr_mock_client(function(m, p) {
    ex <- m[[length(m)]]$content
    if (grepl("120", ex, fixed = TRUE)) return(r6_json(n = 120, n__quote = "We enrolled 120 patients."))
    if (grepl("300", ex, fixed = TRUE)) return(r6_json(n = 300, n__quote = "We enrolled 300 patients."))
    r6_json(n = 42, n__quote = "We enrolled 42 patients.")
  })
  quiet(gr_extract(src, fl, client = cl, store = store))
  r6_file("We enrolled 42 patients.", dir = src, name = "a.txt")
  r6_file("We enrolled 7 patients.", dir = src, name = "d.txt")
  x <- quiet(gr_extract(src, fl, client = cl, store = store, max_total_calls = 1))
  # b and c were "skipped" with no values although restoring them costs nothing.
  expect_identical(x$table$status, c("ok", "restored", "restored", "skipped"))
  expect_identical(x$table$n[1:3], c(42L, 120L, 300L))
  expect_identical(x$trace$calls, 1L)
  expect_warning(suppressMessages(gr_extract(src, fl, client = cl, store = store,
                                             max_total_calls = 0)),
                 "except those restored from `store`", class = "gr_corpus_call_cap")
})

# ---------------------------------------------------------------------------
# state-concurrency-09: the store key does not depend on the time zone
# ---------------------------------------------------------------------------

test_that("a store entry is found from another time zone, and a same-second edit is new", {
  f <- r6_file("Revenue was 45.2 million.")
  rec <- readgpt:::as_recipe("fast")
  cl <- gr_mock_client()
  k1 <- withr::with_timezone("America/New_York", readgpt:::corpus_key(f, "q", rec, cl))
  k2 <- withr::with_timezone("UTC", readgpt:::corpus_key(f, "q", rec, cl))
  expect_identical(k1, k2)

  # Rewritten within one second, at the same size: a different document.
  Sys.setFileTime(f, as.POSIXct("2026-01-01 00:00:00", tz = "UTC"))
  k3 <- readgpt:::corpus_key(f, "q", rec, cl)
  r6_file("Revenue was 51.8 million.", dir = dirname(f), name = basename(f))
  Sys.setFileTime(f, as.POSIXct("2026-01-01 00:00:00", tz = "UTC"))
  expect_false(identical(k3, readgpt:::corpus_key(f, "q", rec, cl)))
  # Touched but unchanged: the same document.
  k4 <- readgpt:::corpus_key(f, "q", rec, cl)
  Sys.setFileTime(f, as.POSIXct("2026-02-01 00:00:00", tz = "UTC"))
  expect_identical(k4, readgpt:::corpus_key(f, "q", rec, cl))

  # End to end: a run resumed under another TZ pays for nothing.
  store <- withr::local_tempdir()
  calls <- 0L
  bc <- gr_backend_client(function(m, p) { calls <<- calls + 1L; "Revenue was 51.8 million." },
                          id = "review6-tz")
  withr::with_timezone("America/New_York",
                       quiet(gr_read_many(f, "What was revenue?", "fast", client = bc, store = store)))
  expect_identical(calls, 1L)
  again <- withr::with_timezone("UTC",
                                quiet(gr_read_many(f, "What was revenue?", "fast", client = bc,
                                                   store = store)))
  expect_identical(again$summary$status, "restored")
  expect_identical(calls, 1L)
})

# ---------------------------------------------------------------------------
# Handoff H1 (pass 5): embeddings requests are not printed as model calls
# ---------------------------------------------------------------------------

test_that("an extraction prints its model calls and its embeddings requests apart", {
  fl <- gr_fields(n = gr_field("Participants", type = "integer"))
  cl <- gr_mock_client(function(m, p) r6_json(n = 120, n__quote = "We enrolled 120 patients."))
  x <- quiet(gr_extract(r6_file("We enrolled 120 patients."), fl, client = cl))
  readgpt:::embed_record(x$trace, "mock-embed", "We enrolled 120 patients.", NULL,
                         list(data = list(list(embedding = 1)), usage = list(prompt_tokens = 5L)))
  line <- grep("this run:", capture.output(print(x)), value = TRUE, fixed = TRUE)
  expect_match(line, "this run: 1 model call(s), 1 embeddings request(s), ", fixed = TRUE)
})
