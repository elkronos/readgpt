# test-extract.R -- the schema layer, the `extract` reader and gr_extract().
#
# The thing under test is not "does it call the model". It is whether the TABLE
# is trustworthy: that a type survives the round trip, that a value can be traced
# to the sentence it came from, that "not reported" and "never read" stay
# distinguishable, and that a disagreement inside one document is surfaced rather
# than resolved by accident of chunk order.

# A mock that fills the form from whatever excerpt it is shown. Each rule is
# (regex on the excerpt) -> (field, value, quote), so a test controls exactly
# which chunk reports what.
mock_form <- function(rules) {
  gr_mock_client(function(messages, params) {
    txt <- paste(vapply(messages, function(m) as_chr1_t(m$content), character(1)),
                 collapse = "\n")
    ex <- sub(".*<excerpt>", "", txt)
    # Reply with EVERY key, nulling the ones this excerpt cannot fill, which is
    # what a strict-schema endpoint does. A mock that omitted them would be
    # testing a response shape the schema forbids.
    asked <- regmatches(txt, gregexpr("(?m)^- ([A-Za-z][A-Za-z0-9_.]*) \\(", txt, perl = TRUE))[[1]]
    asked <- sub("^- ", "", sub(" \\($", "", asked))
    rec <- list()
    for (nm in asked) { rec[nm] <- list(NULL); rec[paste0(nm, "__quote")] <- list(NULL) }
    for (r in rules) {
      if (grepl(r$where, ex, fixed = TRUE)) {
        rec[[r$field]] <- r$value
        rec[[paste0(r$field, "__quote")]] <- r$quote %||% r$where
      }
    }
    as.character(jsonlite::toJSON(rec, auto_unbox = TRUE, null = "null", na = "null"))
  })
}
as_chr1_t <- function(x) if (length(x)) paste(as.character(x), collapse = "\n") else ""
`%||%` <- function(a, b) if (is.null(a)) b else a

two_para_file <- function(a, b) {
  f <- tempfile(fileext = ".txt")
  writeLines(paste(a, b, sep = "\n\n"), f)
  f
}

# Two paragraphs long enough that the paragraph segmenter really does produce two
# chunks at `max_tokens = 48`. The cap has a floor of 32 tokens, so a fixture of
# two short sentences comes back as ONE chunk however small the cap -- which
# quietly turns a two-chunk test into a one-chunk one and makes a disagreement
# between chunks impossible to stage.
conflict_file <- function() {
  say <- function(w) paste(rep(sprintf("%s the %s chunk of prose here and it continues onward.",
                                       toupper(substr(w, 1, 1)), w), 3), collapse = " ")
  two_para_file(say("first"), say("second"))
}

test_that("field names that would break the table are refused, not renamed", {
  # Each of these is silent if unchecked: data.frame() renames a non-syntactic
  # name, the __quote companion collides, and a reserved name is overwritten by
  # the run status. All three produce a table whose columns are not the ones the
  # caller asked for.
  expect_error(gr_fields(`sample size` = "n"), class = "gr_bad_field")
  expect_error(gr_fields(`2020` = "year"), class = "gr_bad_field")
  expect_error(gr_fields(design__quote = "d"), class = "gr_bad_field")
  expect_error(gr_fields(status = "s"), class = "gr_bad_field")
  expect_error(gr_fields(document = "d"), class = "gr_bad_field")
  expect_error(gr_fields(document_id = "d"), class = "gr_bad_field")
  expect_error(gr_fields(duplicate_of = "d"), class = "gr_bad_field")
  expect_error(gr_fields(n_unverified = "d"), class = "gr_bad_field")
  # Every name the table writes for itself, checked against the table itself --
  # so a column added later without being reserved fails here rather than
  # silently overwriting a user's field.
  tab_cols <- names(quiet(gr_extract(
    two_para_file("A trial was run here.", "It then ended here."),
    gr_fields(zzz = "anything"), client = mock_form(list()),
    recipe = "thorough", max_tokens = 40))$table)
  expect_true(all(setdiff(tab_cols, "zzz") %in% readgpt:::.gr_reserved_fields))
  expect_error(gr_fields(a = "x", a = "y"), class = "gr_bad_field")
  expect_error(gr_fields(), class = "gr_bad_field")
  expect_error(gr_fields("unnamed"), class = "gr_bad_field")
  expect_error(gr_field("d", type = "enum"), class = "gr_bad_field")
  expect_error(gr_field(""), class = "gr_bad_field")

  ok <- gr_fields(design = "d", n.total = "n", `n_2` = "n2")
  expect_named(ok, c("design", "n.total", "n_2"))
})

test_that("every field is nullable in the schema, and each gets a quote companion", {
  # A chunk that does not mention the sample size must be able to say so. Without
  # `null` in the type union a strict-schema endpoint forces a value, which is
  # how extraction pipelines invent numbers.
  f <- gr_fields(a = "alpha", b = gr_field("beta", type = "integer"))
  sch <- readgpt:::fields_schema(f)
  expect_true(all(c("a", "b", "a__quote", "b__quote") %in% names(sch$properties)))
  expect_true("null" %in% sch$properties$a$type)
  expect_true("null" %in% sch$properties$b$type)
  expect_setequal(unlist(sch$required), names(sch$properties))
  expect_false(sch$additionalProperties)

  e <- gr_fields(x = gr_field("x", type = "enum", values = c("p", "q")))
  expect_true("p" %in% unlist(readgpt:::fields_schema(e)$properties$x$enum))
})

test_that("values are coerced to the type the field declares", {
  cf <- readgpt:::coerce_field
  expect_identical(cf("1,204", gr_field("n", type = "integer")), 1204L)
  expect_identical(cf(1204, gr_field("n", type = "integer")), 1204L)
  expect_identical(cf("about 1,200 people", gr_field("n", type = "integer")), 1200L)
  expect_identical(cf("-3", gr_field("n", type = "integer")), -3L)
  expect_identical(cf("0.78", gr_field("hr", type = "number")), 0.78)
  # Already numeric: kept as it is rather than round-tripped through a string,
  # which loses digits (as.character(1/3) is not 1/3).
  expect_identical(cf(1 / 3, gr_field("hr", type = "number")), 1 / 3)
  expect_true(cf("yes", gr_field("f", type = "boolean")))
  expect_false(cf(FALSE, gr_field("f", type = "boolean")))
  expect_identical(cf("p", gr_field("e", type = "enum", values = c("p", "q"))), "p")

  # Unusable is NULL -- an empty cell -- never a guess and never text in a
  # numeric column.
  expect_null(cf("z", gr_field("e", type = "enum", values = c("p", "q"))))
  expect_null(cf("not reported", gr_field("s")))
  expect_null(cf("", gr_field("s")))
  expect_null(cf(NULL, gr_field("s")))
  expect_null(cf(NA, gr_field("n", type = "integer")))
  expect_null(cf("no digits here", gr_field("n", type = "integer")))
})

test_that("the extract reader needs fields and says how to supply them", {
  ch <- quiet(gr_segment("Some text about a trial.",
                         list(method = "paragraph", max_tokens = 100)))
  expect_error(quiet(gr_read(ch, "goal", mock_echo(), "extract")), class = "gr_no_fields")
})

test_that("gr_extract() returns one typed row per document with traceable cells", {
  f <- gr_fields(design = "The study design",
                 n = gr_field("Participants randomised", type = "integer"),
                 outcome = gr_field("Direction of the result", type = "enum",
                                    values = c("positive", "null")),
                 funded = gr_field("Industry funded", type = "boolean"),
                 hr = gr_field("Hazard ratio", type = "number"))

  cl <- mock_form(list(
    list(where = "randomised controlled trial", field = "design",
         value = "randomised controlled trial"),
    list(where = "1,204 participants", field = "n", value = "1,204"),
    list(where = "favoured treatment", field = "outcome", value = "positive"),
    list(where = "hazard ratio of 0.78", field = "hr", value = 0.78,
         quote = "favoured treatment"),
    list(where = "funded by the manufacturer", field = "funded", value = TRUE)))

  a <- two_para_file("We ran a randomised controlled trial and enrolled 1,204 participants.",
                     "It favoured treatment, with a hazard ratio of 0.78, and was funded by the manufacturer.")
  b <- two_para_file("This was a prospective cohort study.",
                     "Follow-up continued for five years.")

  x <- quiet(gr_extract(c(a, b), f, client = cl, recipe = "thorough", max_tokens = 40))
  expect_s3_class(x, "gr_extraction")
  expect_equal(nrow(x$table), 2L)

  # The types are the field's types, not whatever the mock happened to return.
  expect_type(x$table$n, "integer")
  expect_type(x$table$hr, "double")
  expect_type(x$table$funded, "logical")
  expect_type(x$table$design, "character")
  expect_identical(x$table$n[1], 1204L)
  expect_identical(x$table$hr[1], 0.78)
  expect_true(x$table$funded[1])
  expect_identical(x$table$outcome[1], "positive")

  # Document two reports none of these fields. That is a finding, not a failure:
  # NA cells, status "ok".
  expect_identical(x$table$status, c("ok", "ok"))
  expect_true(all(is.na(x$table[2, c("design", "n", "outcome", "funded", "hr")])))
  expect_identical(x$table$n_filled[2], 0L)

  # Every filled cell is attributable to a chunk and a sentence, and the sentence
  # was checked against that chunk's text.
  ev <- x$evidence
  expect_setequal(unique(ev$field), c("design", "n", "outcome", "funded", "hr"))
  expect_true(all(ev$verified))
  expect_true(all(ev$document == x$table$document[1]))
  expect_false("source_text" %in% names(ev))
  expect_equal(nrow(ev), x$table$n_filled[1])
})

test_that("an empty column keeps its declared type", {
  # A field no document reports must still be an integer column of NAs. Built by
  # unlist() it would be logical, and a later rbind() or join against another
  # run's table would change type depending on how much of the corpus was
  # readable.
  f <- gr_fields(n = gr_field("Participants", type = "integer"),
                 hr = gr_field("Hazard ratio", type = "number"),
                 s = "Something")
  cl <- mock_form(list())
  a <- two_para_file("Nothing relevant here.", "Nor here.")
  x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 40))
  expect_type(x$table$n, "integer")
  expect_type(x$table$hr, "double")
  expect_type(x$table$s, "character")
  expect_equal(nrow(x$evidence), 0L)
  expect_true(all(c("document", "field", "verified") %in% names(x$evidence)))

  # And it is a COMPLETE negative result, not a partial one. A document that
  # reports none of the fields was read successfully; flagging it partial would
  # mark every off-topic paper in a screening run as a failed read.
  expect_false(x$summary$partial)
  expect_identical(x$table$status, "ok")
  expect_identical(x$table$n_filled, 0L)
})

test_that("an excerpt that fills nothing is an answer, not a failed call", {
  # A strict-schema endpoint can legitimately reply `{}`. gr_call_json() treats a
  # JSON object with no keys as a parse failure for every other reader, which
  # here would count each chunk as a failed call and mark the document partial --
  # reporting "we could not read this" for "it does not say".
  f <- gr_fields(design = "The study design")
  cl <- gr_mock_client(function(messages, params) "{}")
  a <- two_para_file("Nothing relevant here at all.", "Nor here either.")
  x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 40,
                        keep_answers = TRUE))
  expect_identical(x$table$status, "ok")
  expect_false(x$summary$partial)
  expect_identical(x$answers[[1]]$notes$failed_calls, 0L)
  expect_true(is.na(x$table$design))

  # A genuinely broken reply is still a failure.
  dead <- gr_mock_client(function(messages, params) "this is not json")
  y <- quiet(gr_extract(a, f, client = dead, recipe = "thorough", max_tokens = 40,
                        keep_answers = TRUE))
  expect_gt(y$answers[[1]]$notes$failed_calls, 0L)
  expect_true(y$summary$partial)
})

test_that("the record keeps one entry per field, even when a value is discarded", {
  # `record[[nm]] <- NULL` deletes the key rather than emptying it -- the same
  # trap as modifyList(). A short record makes a length-1 logical index recycle
  # against a length-2 name vector, and the wrong fields get reported as filled.
  f <- gr_fields(n = gr_field("Participants", type = "integer"),
                 design = "The study design")
  cl <- gr_mock_client(function(messages, params) {
    '{"n":1204,"n__quote":"A trial was run here.",
      "design":"randomised trial","design__quote":"Invented sentence, not present."}'
  })
  a <- two_para_file("A trial was run here.", "It then ended here.")
  x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 40,
                        require_quote = TRUE, keep_answers = TRUE))
  rec <- x$answers[[1]]$notes$record
  expect_named(rec, c("n", "design"))
  expect_null(rec$design)
  # And the JSON still says the field was looked for.
  expect_true(grepl('"design":null', x$answers[[1]]$answer, fixed = TRUE))
})

test_that("a record survives a store written before notes carried it", {
  # The fallback path in answer_record(). A store outlives the session that wrote
  # it, so an answer whose notes are shaped differently must still yield its
  # record -- from the JSON in $answer -- rather than silently producing an empty
  # row that reports status "restored". And it must be re-coerced on the way in:
  # JSON has no integer field, so a stored "1,204" is a string until something
  # makes it one.
  f <- gr_fields(design = "The study design",
                 n = gr_field("Participants", type = "integer"))
  ans <- structure(list(answer = '{"design":"cohort","n":"1,204","other":"ignored"}',
                        notes = list()), class = "gr_answer")
  rec <- readgpt:::answer_record(ans, f)
  expect_identical(rec$design, "cohort")
  expect_identical(rec$n, 1204L)

  # An answer that is not readable at all yields an empty record, never a
  # partially-populated one.
  bad <- structure(list(answer = "not json", notes = list()), class = "gr_answer")
  expect_true(all(vapply(readgpt:::answer_record(bad, f), is.null, logical(1))))
  expect_true(all(vapply(readgpt:::answer_record(NULL, f), is.null, logical(1))))

  # The invariant everything downstream leans on: one entry per field, in schema
  # order, whatever went in. `out[[nm]] <- NULL` would silently drop entries and
  # make a logical index recycle against a longer name vector.
  for (a in list(ans, bad, NULL,
                 structure(list(answer = "{}", notes = list()), class = "gr_answer"),
                 structure(list(answer = '{"n":7}', notes = list()), class = "gr_answer"))) {
    expect_named(readgpt:::answer_record(a, f), c("design", "n"))
  }
})

test_that("the table is rebuilt from a stored answer, not from its notes", {
  # A store written by another build hands back an answer whose notes are shaped
  # differently or missing. Every column of the table has to be derivable from
  # the record and the evidence alone -- a count read out of `notes` would come
  # back zero and report "every value is supported", which is the one wrong
  # answer n_unverified must never give.
  f <- gr_fields(n = gr_field("Participants", type = "integer"),
                 design = "The study design")
  ev <- data.frame(chunk_id = 1L, text = "We enrolled 1,204 people.", page = NA_integer_,
                   section = NA_character_, score = NA_real_, kind = "extracted",
                   source_text = "We enrolled 1,204 people.", verified = TRUE,
                   match = 1, field = "n", stringsAsFactors = FALSE)
  ans <- structure(list(answer = '{"n":"1,204","design":"cohort"}',
                        notes = list(), evidence = ev), class = "gr_answer")
  summary <- data.frame(document = "d.txt", status = "ok", error = NA_character_,
                        stringsAsFactors = FALSE)

  tab <- readgpt:::extraction_table("d.txt", list(d.txt = ans), f, summary)
  expect_identical(tab$n, 1204L)
  expect_type(tab$n, "integer")
  expect_identical(tab$design, "cohort")
  expect_identical(tab$n_filled, 2L)
  expect_identical(tab$n_unverified, 1L)     # `design` has no verified span
})

test_that("an unread document is NA everywhere, and says so in status", {
  # The distinction the whole design turns on. A failed row must not look like a
  # document that reports nothing: n_filled is NA, not 0.
  f <- gr_fields(design = "The study design")
  cl <- mock_form(list(list(where = "trial", field = "design", value = "trial")))
  a <- two_para_file("A trial was run.", "It ended.")
  x <- quiet(gr_extract(c(a, "no-such-file.txt"), f, client = cl,
                        recipe = "thorough", max_tokens = 40))
  expect_identical(x$table$status, c("ok", "failed"))
  expect_true(is.na(x$table$n_filled[2]))
  expect_identical(x$table$n_filled[1], 1L)
  expect_true(!is.na(x$table$error[2]))
  expect_true(is.na(x$table$design[2]))
})

test_that("a document that contradicts itself reports the conflict", {
  # Two chunks, two different sample sizes. `resolve = "first"` must not silently
  # pick one and present it as agreed -- the conflict is the finding.
  f <- gr_fields(n = gr_field("Participants randomised", type = "integer"))
  cl <- gr_mock_client(function(messages, params) {
    ex <- sub(".*<excerpt>", "",
              paste(vapply(messages, function(m) as_chr1_t(m$content), character(1)),
                    collapse = "\n"))
    n <- if (grepl("first chunk", ex, fixed = TRUE)) 100L else 250L
    q <- if (grepl("first chunk", ex, fixed = TRUE)) "first chunk" else "second chunk"
    sprintf('{"n":%d,"n__quote":"%s"}', n, q)
  })
  a <- conflict_file()
  x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 48))
  expect_identical(x$table$n[1], 100L)                     # earliest wins
  expect_identical(x$table$conflicts[1], "n")
  expect_identical(length(cl$calls()), 2L)                 # and it cost nothing extra
})

test_that("resolve = 'model' spends exactly one extra call, and only on the conflict", {
  f <- gr_fields(n = gr_field("Participants randomised", type = "integer"),
                 s = "A string field nobody reports")
  cl <- gr_mock_client(function(messages, params) {
    txt <- paste(vapply(messages, function(m) as_chr1_t(m$content), character(1)),
                 collapse = "\n")
    if (grepl("Choose the", txt, fixed = TRUE)) return('{"choice":2}')
    ex <- sub(".*<excerpt>", "", txt)
    n <- if (grepl("first chunk", ex, fixed = TRUE)) 100L else 250L
    q <- if (grepl("first chunk", ex, fixed = TRUE)) "first chunk" else "second chunk"
    sprintf('{"n":%d,"n__quote":"%s"}', n, q)
  })
  a <- conflict_file()
  x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 48,
                        resolve = "model"))
  expect_identical(x$table$n[1], 250L)                     # the adjudicated value
  expect_identical(x$table$conflicts[1], "n")
  expect_identical(length(cl$calls()), 3L)                 # 2 chunks + 1 conflict
  # The evidence follows the chosen value, not the first one seen.
  expect_identical(x$evidence$quote[x$evidence$field == "n"], "second chunk")
})

test_that("a store makes an extraction resumable without holding every answer", {
  # The restore path is the one that breaks quietly: a resumed run gets its
  # answers back from disk, and if the table were built from anything else those
  # rows would come back empty while reporting status "restored".
  f <- gr_fields(design = "The study design")
  cl <- mock_form(list(list(where = "trial", field = "design", value = "trial")))
  a <- two_para_file("A trial was run here.", "It then ended here.")
  store <- withr::local_tempdir()

  first <- quiet(gr_extract(a, f, client = cl, store = store,
                            recipe = "thorough", max_tokens = 40))
  n1 <- length(cl$calls())
  expect_identical(first$table$status, "ok")
  expect_identical(first$table$design, "trial")

  again <- quiet(gr_extract(a, f, client = cl, store = store,
                            recipe = "thorough", max_tokens = 40))
  expect_identical(again$table$status, "restored")
  expect_identical(again$table$design, "trial")            # rebuilt from the store
  expect_identical(again$table$n_filled, 1L)
  expect_equal(nrow(again$evidence), 1L)
  expect_identical(length(cl$calls()), n1)                 # nothing was re-read
  expect_identical(again$answers, list())                  # and nothing was retained
})

test_that("changing the schema is a different job for the store", {
  # The fields are part of what makes a run what it is. If they were not in the
  # key, adding a column would restore the old table and report it as complete.
  cl <- mock_form(list(list(where = "trial", field = "design", value = "trial")))
  a <- two_para_file("A trial was run here.", "It then ended here.")
  store <- withr::local_tempdir()

  quiet(gr_extract(a, gr_fields(design = "The study design"), client = cl,
                   store = store, recipe = "thorough", max_tokens = 40))
  n1 <- length(cl$calls())
  out <- quiet(gr_extract(a, gr_fields(design = "The study design", extra = "Anything else"),
                          client = cl, store = store, recipe = "thorough", max_tokens = 40))
  expect_identical(out$table$status, "ok")
  expect_gt(length(cl$calls()), n1)

  # And so is a changed DESCRIPTION for the same field name: it is the whole
  # instruction the model gets, so it changes what comes back.
  n2 <- length(cl$calls())
  quiet(gr_extract(a, gr_fields(design = "The study design, verbatim"), client = cl,
                   store = store, recipe = "thorough", max_tokens = 40))
  expect_gt(length(cl$calls()), n2)
})

test_that("gr_extract() reads with `extract` whatever reader the recipe names", {
  f <- gr_fields(design = "The study design")
  cl <- mock_form(list(list(where = "trial", field = "design", value = "trial")))
  a <- two_para_file("A trial was run here.", "It then ended here.")
  x <- quiet(gr_extract(a, f, client = cl, recipe = "fast", max_tokens = 40))
  expect_identical(x$table$design, "trial")
  expect_identical(x$summary$reader, "extract")
})

test_that("gr_extract() validates its own arguments before spending anything", {
  cl <- mock_form(list())
  a <- two_para_file("A trial was run here.", "It then ended here.")
  expect_error(gr_extract(a, list(design = "x"), client = cl), class = "gr_no_fields")
  expect_error(gr_extract(a, gr_fields(d = "x"), goal = "  ", client = cl))
  expect_error(gr_extract(a, gr_fields(d = "x"), client = cl, nonsense = 1),
               class = "gr_unknown_override")
  expect_identical(length(cl$calls()), 0L)
})

test_that("evidence keeps its field label when an empty quote is dropped", {
  # evidence_table() filters out rows whose span is blank. Attaching the field
  # names after that filter would line them up against the wrong rows -- the
  # reason `extra =` exists at all.
  et <- readgpt:::evidence_table
  out <- et(chunk_ids = c(1L, 2L, 3L), texts = c("alpha", "  ", "gamma"),
            source_text = c("alpha here", "beta here", "gamma here"),
            kind = "extracted", extra = list(field = c("a", "b", "c")))
  expect_identical(out$field, c("a", "c"))
  expect_identical(out$text, c("alpha", "gamma"))
  expect_true(all(out$verified))

  # And an empty table still carries the column, so rbind() across documents
  # cannot produce a ragged frame.
  none <- et(integer(0), character(0), kind = "extracted",
             extra = list(field = character(0)))
  expect_true("field" %in% names(none))
  expect_equal(nrow(none), 0L)
})

test_that("a model that paraphrases instead of quoting is visible in the table", {
  # The quote is the only thing standing between an extraction table and a list
  # of plausible numbers. A span that is not in the chunk it is attributed to
  # must come back verified = FALSE, and mark the answer partial.
  f <- gr_fields(n = gr_field("Participants randomised", type = "integer"))
  cl <- gr_mock_client(function(messages, params) {
    '{"n":1204,"n__quote":"A sentence that appears nowhere in the document at all."}'
  })
  a <- two_para_file("A trial was run here.", "It then ended here.")
  x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 40,
                        keep_answers = TRUE))
  expect_identical(x$table$n[1], 1204L)
  expect_false(any(x$evidence$verified))
  expect_true(x$answers[[1]]$partial)
  expect_identical(x$answers[[1]]$notes$unverified_evidence, 1L)
  expect_identical(x$table$n_unverified[1], 1L)
})

test_that("a value with no quote is counted, not hidden", {
  # The quiet failure. An unverified span is loud -- it sits in $evidence with
  # verified = FALSE. A value with NO span produced no evidence row at all, so a
  # number nothing supported looked exactly like a number the document stated
  # outright. n_unverified is what separates them.
  f <- gr_fields(n = gr_field("Participants randomised", type = "integer"),
                 design = "The study design")
  cl <- gr_mock_client(function(messages, params) {
    # `n` is quoted and the quote is real; `design` is asserted with no quote.
    '{"n":1204,"n__quote":"A trial was run here.",
      "design":"randomised trial","design__quote":null}'
  })
  a <- two_para_file("A trial was run here.", "It then ended here.")
  x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 40,
                        keep_answers = TRUE))

  expect_identical(x$table$n_filled, 2L)
  expect_identical(x$table$n_unverified, 1L)
  expect_identical(x$table$design, "randomised trial")   # kept, not discarded
  expect_identical(sort(x$evidence$field), "n")          # but with no evidence row
  expect_true(x$answers[[1]]$partial)
  expect_identical(x$answers[[1]]$notes$unsupported, "design")
})

test_that("a paraphrase counts as unverified too", {
  f <- gr_fields(n = gr_field("Participants randomised", type = "integer"))
  cl <- gr_mock_client(function(messages, params) {
    '{"n":1204,"n__quote":"A sentence that appears nowhere in this document."}'
  })
  a <- two_para_file("A trial was run here.", "It then ended here.")
  x <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 40))
  expect_identical(x$table$n_filled, 1L)
  expect_identical(x$table$n_unverified, 1L)
  expect_identical(x$table$n, 1204L)                     # still there
  expect_false(x$evidence$verified)                      # and still visible
})

test_that("require_quote discards what it cannot verify, and says how much", {
  f <- gr_fields(n = gr_field("Participants randomised", type = "integer"),
                 design = "The study design")
  cl <- gr_mock_client(function(messages, params) {
    '{"n":1204,"n__quote":"A trial was run here.",
      "design":"randomised trial","design__quote":"Invented sentence, not present."}'
  })
  a <- two_para_file("A trial was run here.", "It then ended here.")

  lax <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 40))
  expect_identical(lax$table$design, "randomised trial")
  expect_identical(lax$table$n_filled, 2L)
  expect_identical(lax$table$n_unverified, 1L)

  strict <- quiet(gr_extract(a, f, client = cl, recipe = "thorough", max_tokens = 40,
                             require_quote = TRUE, keep_answers = TRUE))
  expect_true(is.na(strict$table$design))                # dropped
  expect_identical(strict$table$n, 1204L)                # the verified one survives
  expect_identical(strict$table$n_filled, 1L)
  expect_identical(strict$table$n_unverified, 1L)        # and the loss is still counted
  expect_identical(strict$answers[[1]]$notes$dropped_unverified, "design")
  # Evidence must not cite a cell that is no longer in the table.
  expect_false("design" %in% strict$evidence$field)
})

test_that("print.gr_extraction() reports the shape without erroring on the edges", {
  f <- gr_fields(design = "The study design")
  cl <- mock_form(list(list(where = "trial", field = "design", value = "trial")))
  a <- two_para_file("A trial was run here.", "It then ended here.")
  x <- quiet(gr_extract(c(a, "no-such-file.txt"), f, client = cl,
                        recipe = "thorough", max_tokens = 40))
  expect_output(print(x), "2 document\\(s\\) x 1 field\\(s\\)")
  expect_output(print(x), "1 ok, 1 failed")
  expect_output(print(x), "cells filled: 1 of 1")
  expect_output(print(x), "verbatim in the cited chunk")
})

test_that("gr_extract() fills a duplicate row rather than blanking it", {
  # A duplicate WAS read -- once, as another row. Treating it as unread would
  # blank a row that is fully known and mark it as a job to redo.
  f <- gr_fields(design = "The study design")
  cl <- mock_form(list(list(where = "trial", field = "design", value = "trial")))
  txt <- "A trial was run here and it then ended here."
  a <- tempfile(fileext = ".txt"); writeLines(txt, a)
  b <- tempfile(fileext = ".txt"); writeLines(txt, b)

  x <- quiet(gr_extract(c(a, b), f, client = cl, recipe = "thorough", max_tokens = 40))
  expect_identical(x$table$status, c("ok", "duplicate"))
  expect_identical(x$table$design, c("trial", "trial"))
  expect_identical(x$table$n_filled, c(1L, 1L))
  expect_identical(x$table$n_unverified, c(0L, 0L))
  expect_identical(x$table$duplicate_of, c(NA, basename(a)))
  expect_identical(length(cl$calls()), 1L)

  # Which makes the distinct set one subset() away.
  expect_equal(nrow(subset(x$table, is.na(duplicate_of))), 1L)
})

test_that("the extraction tables carry the citable id, not just the filename", {
  f <- gr_fields(design = "The study design")
  cl <- mock_form(list(list(where = "trial", field = "design", value = "trial")))
  txt <- "A trial was run here and it then ended here."
  a <- tempfile(fileext = ".txt"); writeLines(txt, a)
  b <- tempfile(fileext = ".txt"); writeLines(txt, b)

  x <- quiet(gr_extract(c(a, b), f, client = cl, recipe = "thorough", max_tokens = 40))
  expect_false(anyNA(x$table$document_id))
  expect_identical(x$table$document_id[1], x$table$document_id[2])
  # Evidence joins to the table on either column, and on the stable one across
  # runs -- which is the point of putting it in both.
  expect_true("document_id" %in% names(x$evidence))
  expect_setequal(x$evidence$document_id, x$table$document_id)
})
