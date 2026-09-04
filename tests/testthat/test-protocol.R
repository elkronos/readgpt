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
  expect_setequal(tab$name, c("bibliography", "evidence_table", "systematic_review"))
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
