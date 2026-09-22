# test-protocol.R -- the three decisions a review must make before it reads.
#
# A protocol exists so that criteria are fixed in advance. What the tests here
# actually check is that nothing quietly changes them afterwards: that the file
# round trip is lossless, that a built-in template is a valid schema rather than
# prose that happens to parse, and that passing a protocol to gr_extract() uses
# the protocol's question and pipeline rather than silently substituting the
# defaults.

test_that("a protocol needs a name and a question", {
  expect_error(gr_protocol("", question = "Q?"), class = "gr_bad_protocol")
  expect_error(gr_protocol("x", question = "  "), class = "gr_bad_protocol")
  expect_error(gr_protocol("x"), class = "gr_bad_protocol")
  # `fields` is optional -- a screening-only protocol collects nothing -- but it
  # must be a schema if given.
  expect_error(gr_protocol("x", question = "Q?", fields = "design"),
               class = "gr_bad_protocol")
  expect_s3_class(gr_protocol("x", question = "Q?"), "gr_protocol")
  # A plain named list of descriptions is accepted and becomes a schema, so a
  # protocol read from a file needs no special case.
  p <- gr_protocol("x", question = "Q?", fields = list(design = "The design"))
  expect_s3_class(p$fields, "gr_fields")
})

test_that("criteria and outline are cleaned rather than taken as given", {
  p <- gr_protocol("x", question = "Q?",
                   include = c("  Reports a randomised comparison  ", "", "   "),
                   exclude = character(0),
                   outline = c("Findings", "Limitations"))
  expect_identical(p$include, "Reports a randomised comparison")
  expect_identical(p$exclude, character(0))
  # An unnamed outline entry is its own heading: "Methods, Findings,
  # Limitations" is a perfectly good outline and demanding names for it is
  # bureaucracy.
  expect_named(p$outline, c("Findings", "Limitations"))
  expect_identical(unname(p$outline), c("Findings", "Limitations"))

  named <- gr_protocol("x", question = "Q?",
                       outline = c(Findings = "What the studies found"))
  expect_identical(named$outline[["Findings"]], "What the studies found")
})

test_that("the built-in protocols are usable schemas, not prose", {
  # A template whose field names collide with the extraction table's own columns
  # would fail the first time anyone ran it. Checking here means the templates
  # are held to the same rule as a user's schema -- which is how the systematic
  # review template's `conflicts` field was caught.
  tab <- gr_protocols()
  expect_setequal(tab$name, c("bibliography", "claims", "evidence_table", "systematic_review"))
  for (nm in tab$name) {
    p <- gr_protocols(nm)
    expect_s3_class(p, "gr_protocol")
    expect_true(readgpt:::is_nonblank(p$question))
    expect_s3_class(p$fields, "gr_fields")
    expect_silent(readgpt:::check_field_names(names(p$fields)))
    # Every field carries a real instruction, not just a name.
    expect_true(all(vapply(p$fields, function(f) nchar(f$description) > 8L, logical(1))))
  }
  expect_error(gr_protocols("no-such-protocol"), class = "gr_unknown_method")
})

test_that("a protocol survives a file round trip unchanged", {
  # A protocol that changed when it was shared would be worse than no protocol:
  # the whole point is that the criteria someone else checks are the criteria the
  # run used.
  p <- gr_protocol(
    "round", question = "Does it work?",
    include = c("First criterion", "Second criterion"),
    exclude = "Only exclusion",
    fields = gr_fields(
      design = "The study design",
      n = gr_field("Participants randomised", type = "integer"),
      dir = gr_field("Direction", type = "enum", values = c("up", "down", "flat")),
      funded = gr_field("Industry funded", type = "boolean"),
      hr = gr_field("Hazard ratio", type = "number")),
    outline = c(Findings = "What was found", Limits = "What limits it"),
    recipe = "survey", description = "A round-trip fixture")

  f <- withr::local_tempfile(fileext = ".json")
  gr_protocol_save(p, f)
  back <- gr_protocol_read(f)

  for (part in c("name", "question", "description", "include", "exclude",
                 "outline", "recipe")) {
    expect_identical(back[[part]], p[[part]], info = part)
  }
  expect_identical(back$fields, p$fields)          # types and enum values too
  expect_identical(back$fields$dir$values, c("up", "down", "flat"))

  # And it reads from a JSON string as well as a path, so a protocol can travel
  # in a script or a database column rather than only as a file.
  expect_identical(gr_protocol_read(paste(readLines(f), collapse = "\n"))$question,
                   p$question)
  expect_error(gr_protocol_read("not json at all"), class = "gr_bad_protocol")
})

test_that("a recipe object is written as its name, not as its settings", {
  # A file that pinned every clean and segmentation setting would silently pin
  # them for whoever reads it, on whatever version they have. A protocol says
  # what to look for; the pipeline is the reader's business.
  p <- gr_protocol("x", question = "Q?", fields = gr_fields(a = "Anything"),
                   recipe = gr_recipes("precise"))
  f <- withr::local_tempfile(fileext = ".json")
  gr_protocol_save(p, f)
  raw <- jsonlite::fromJSON(paste(readLines(f), collapse = "\n"), simplifyVector = FALSE)
  expect_identical(raw$recipe, "precise")
  expect_identical(gr_protocol_read(f)$recipe, "precise")
})

test_that("a protocol can be registered and comes back the same", {
  local_registries()
  before <- nrow(gr_protocols())
  p <- gr_protocol("mine", question = "What did each report conclude?",
                   fields = gr_fields(conclusion = "The report's own conclusion"),
                   description = "A registered fixture")
  gr_register_protocol("mine", p)
  expect_equal(nrow(gr_protocols()), before + 1L)
  expect_identical(gr_protocols("mine")$question, p$question)
  expect_true("mine" %in% gr_protocols()$name)
  expect_error(gr_register_protocol("bad", list(question = "Q?")),
               class = "gr_bad_protocol")

  # The registry key wins over the object's own name, so a protocol registered
  # under a new name reports the name it answers to.
  gr_register_protocol("renamed", p)
  expect_identical(gr_protocols("renamed")$name, "renamed")
})

test_that("gr_extract() takes a protocol in place of a schema", {
  cl <- gr_mock_client(function(messages, params) {
    '{"conclusion":"It worked.","conclusion__quote":"We conclude that it worked."}'
  })
  f <- tempfile(fileext = ".txt")
  writeLines("We ran a study of the thing. We conclude that it worked.", f)

  p <- gr_protocol("mine", question = "What did each report conclude?",
                   fields = gr_fields(conclusion = "The report's own conclusion"),
                   recipe = "fast")

  x <- quiet(gr_extract(f, p, client = cl, max_tokens = 40, keep_answers = TRUE))
  expect_identical(x$table$conclusion, "It worked.")
  # The protocol's question is the framing the model was given.
  seen <- paste(vapply(cl$calls()[[1]]$messages,
                       function(m) as.character(m$content), character(1)), collapse = "\n")
  expect_true(grepl(p$question, seen, fixed = TRUE))

  # And the protocol's recipe was used, not gr_extract()'s default. The reader is
  # always `extract`, so the segmenter is what tells them apart: "fast" chunks by
  # paragraph, the "research" default structurally.
  expect_identical(x$answers[[1]]$segmentation$method, "paragraph")
  default <- quiet(gr_extract(f, p$fields, client = cl, max_tokens = 40,
                              keep_answers = TRUE))
  expect_false(identical(default$answers[[1]]$segmentation$method, "paragraph"))

  # An explicit argument still wins over the protocol.
  cl2 <- gr_mock_client(function(messages, params) {
    '{"conclusion":"It worked.","conclusion__quote":"We conclude that it worked."}'
  })
  quiet(gr_extract(f, p, goal = "A different framing entirely", client = cl2,
                   recipe = "thorough", max_tokens = 40))
  seen2 <- paste(vapply(cl2$calls()[[1]]$messages,
                        function(m) as.character(m$content), character(1)), collapse = "\n")
  expect_true(grepl("A different framing entirely", seen2, fixed = TRUE))
  expect_false(grepl(p$question, seen2, fixed = TRUE))
})

test_that("a protocol with no schema says so before spending anything", {
  cl <- gr_mock_client(function(messages, params) "{}")
  f <- tempfile(fileext = ".txt"); writeLines("Some text here.", f)
  screening_only <- gr_protocol("screen", question = "Is it relevant?",
                                include = "Reports original research")
  # Named, and with the fix. The generic "must come from gr_fields()" is bad
  # advice when what was passed IS a valid protocol.
  expect_error(gr_extract(f, screening_only, client = cl), class = "gr_no_fields")
  expect_error(gr_extract(f, screening_only, client = cl), "Protocol 'screen' has no")
  expect_identical(length(cl$calls()), 0L)
})

# ---------------------------------------------------------------------------
# The `claims` template, and refusing a template nobody edited.
# ---------------------------------------------------------------------------

test_that("the claims template codes what must be compared and quotes what must be checked", {
  p <- gr_protocols("claims")
  f <- p$fields

  # The point of this template. `evidence_table` records the design "in the
  # paper's own words", which is right for a table a person reads and wrong for
  # one that gets crosstabbed: "RCT", "randomised trial" and "randomized
  # controlled trial" become three designs, and a gap analysis then reports a
  # design as absent while three of them sit in the table.
  expect_identical(f$design$type, "enum")
  expect_identical(f$finding$type, "enum")
  expect_gt(length(f$design$values), 5L)
  # Coded FOR the review question, not for an intervention -- which is what lets
  # a non-interventional literature have contradictions at all.
  expect_setequal(f$finding$values,
                  c("supports", "contradicts", "mixed", "no clear finding", "not applicable"))

  # Every coded field is paired with the paper's own wording, so the coding can
  # be checked rather than trusted.
  expect_true("design_note" %in% names(f))
  expect_identical(f$design_note$type, "string")

  # An effect stays a STRING. Effect metrics are not commensurable across
  # studies, and coercing "d = 0.42 (0.11, 0.73)" to a number is the shape of
  # fault that turned "120 (60 per arm)" into 12060.
  expect_identical(f$effect$type, "string")
  expect_identical(f$n$type, "integer")

  # The fields the other templates lack, and the reason this one exists.
  expect_true(all(c("measure", "limitation") %in% names(f)))
  expect_silent(readgpt:::check_field_names(names(f)))
  # An outline it can actually support, rather than a topic list.
  expect_gt(length(p$outline), 1L)
})

test_that("a template nobody edited is refused before anything is spent", {
  # A run could screen a whole corpus against "REPLACE: the population the
  # review is about" and extract against "REPLACE THIS with your review
  # question", paying in full for a table framed by an instruction to supply the
  # framing. It matters most for `claims`, whose `finding` values are defined
  # relative to the question: unedited, "supports" and "contradicts" mean
  # nothing, and they are what the claims layer reads to find a disagreement.
  cl <- gr_mock_client(function(m, p) "{}")
  f <- withr::local_tempfile(fileext = ".txt")
  writeLines("We ran a randomised trial and it worked.", f)

  # A one-row table, so the synthesis path can be reached without an extraction.
  tab <- data.frame(document = "d1.pdf", document_id = "h1", status = "ok",
                    duplicate_of = NA_character_, n_filled = 1L, n_unverified = 0L,
                    conflicts = NA_character_, finding = "supports",
                    stringsAsFactors = FALSE)
  for (nm in c("claims", "systematic_review")) {
    p <- gr_protocols(nm)
    expect_error(gr_extract(f, p, client = cl), class = "gr_protocol_unedited")
    expect_error(gr_screen(f, protocol = p, client = cl), class = "gr_protocol_unedited")
    # And the write-up, which is where the unedited question does the most
    # damage: it becomes the framing of every section.
    expect_error(gr_synthesise(tab, protocol = p, client = cl),
                 class = "gr_protocol_unedited")
  }
  # Nothing was spent finding out.
  expect_length(cl$calls(), 0L)

  # Editing the question is all it takes.
  ok <- gr_protocol("mine", question = "Does spacing improve retention?",
                    fields = gr_protocols("claims")$fields, recipe = "fast")
  expect_silent(readgpt:::check_protocol_edited(ok))
})

test_that("the placeholder guard does not refuse a real protocol that says REPLACE", {
  # Anchored, on the whole word: there are real trials called REPLACE, and a
  # review of one is not a template.
  p <- gr_protocol("mine", question = "Did the REPLACE trial change practice?",
                   include = "Cites the REPLACE trial",
                   fields = gr_fields(x = "Anything at all"))
  expect_silent(readgpt:::check_protocol_edited(p))

  # And it looks at the criteria, not only the question.
  q <- gr_protocol("mine", question = "A real question about a real thing?",
                   include = c("REPLACE: the population", "Reports original data"),
                   fields = gr_fields(x = "Anything at all"))
  expect_error(readgpt:::check_protocol_edited(q), class = "gr_protocol_unedited")
})
