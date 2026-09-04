# test-select.R -- which chunks go in the prompt, and where they sit in it.
#
# Two knobs, both off by default, and the first thing to prove is exactly that:
# a default run must be bit-for-bit what it was before these existed. After
# that, the interesting properties are that MMR actually trades relevance for
# novelty (rather than just permuting the same top-k), and that ordering changes
# only the arrangement and never the selection -- if it changed the set, it
# would be a retrieval feature wearing a formatting feature's name.

mmr <- function(...) readgpt:::mmr_select(...)
arr <- function(...) readgpt:::arrange_context(...)

# Four unit vectors: the first three nearly parallel, the fourth orthogonal.
redundant_emb <- function() {
  m <- rbind(c(1, 0, 0), c(0.99, 0.14, 0), c(0.98, 0.20, 0), c(0, 0, 1))
  m / sqrt(rowSums(m^2))
}

test_that("lambda = 1 is exactly top-k", {
  set.seed(1)
  for (i in 1:20) {
    rel <- stats::runif(12)
    emb <- matrix(stats::rnorm(12 * 5), nrow = 12)
    emb <- emb / sqrt(rowSums(emb^2))
    expect_identical(mmr(rel, emb, 4, 1), order(rel, decreasing = TRUE)[1:4])
  }
  # And with no embeddings to measure redundancy against.
  rel <- c(0.1, 0.9, 0.5)
  expect_identical(mmr(rel, NULL, 2, 0.5), order(rel, decreasing = TRUE)[1:2])
})

test_that("below 1, a novel chunk beats a near-duplicate of what is already in", {
  rel <- c(0.90, 0.85, 0.80, 0.20)          # the novel chunk is the LEAST relevant
  emb <- redundant_emb()

  expect_identical(mmr(rel, emb, 3, 1), c(1L, 2L, 3L))     # top-k takes all three copies
  picked <- mmr(rel, emb, 3, 0.5)
  expect_identical(picked[1], 1L)                          # still starts with the best
  expect_true(4L %in% picked)                              # and reaches the different one
  expect_lt(which(picked == 4L), 3L)                       # before the third near-duplicate
})

test_that("MMR degenerates sensibly at the edges", {
  rel <- c(0.9, 0.85, 0.8, 0.2)
  emb <- redundant_emb()
  expect_length(mmr(rel, emb, 0, 0.5), 0L)
  expect_length(mmr(rel, emb, 99, 0.5), 4L)          # k above n is n
  expect_identical(sort(mmr(rel, emb, 4, 0.5)), 1:4) # and selects everything exactly once
  expect_length(mmr(rep(-Inf, 4), emb, 3, 0.5), 0L)  # nothing eligible is empty, not an error
  expect_length(mmr(numeric(0), NULL, 3, 0.5), 0L)
  # lambda = 0 is pure novelty: it still starts from the most relevant, then
  # stops caring, so the orthogonal chunk comes second.
  expect_identical(mmr(rel, emb, 2, 0)[2], 4L)
})

test_that("an ineligible candidate is never selected, whatever lambda", {
  rel <- c(0.9, -Inf, 0.8, 0.2)
  emb <- redundant_emb()
  for (lam in c(1, 0.7, 0)) {
    expect_false(2L %in% mmr(rel, emb, 3, lam))
  }
})

test_that("a mismatched embedding matrix falls back to top-k rather than misaligning", {
  # emb rows must line up with rel. If they do not, using them would pair each
  # chunk with another chunk's vector -- silently, and the answer would look fine.
  rel <- c(0.9, 0.5, 0.1)
  expect_identical(mmr(rel, matrix(1, nrow = 99L, ncol = 3L), 2, 0.5), c(1L, 2L))
  expect_identical(mmr(rel, "not a matrix", 2, 0.5), c(1L, 2L))
})

test_that("ordering rearranges and never changes the set", {
  idx <- c(7L, 3L, 9L, 1L, 5L, 4L)
  for (how in c("relevance", "document", "edges")) {
    expect_setequal(arr(idx, how), idx)
  }
  expect_identical(arr(idx, "relevance"), idx)
  expect_identical(arr(idx, "document"), sort(idx))
  # Ranks 1,2,3,4,5,6 -> 1,3,5,6,4,2: best first, second-best last, weakest
  # buried where attention is thinnest.
  expect_identical(arr(1:6, "edges"), c(1L, 3L, 5L, 6L, 4L, 2L))
  expect_identical(arr(1:5, "edges"), c(1L, 3L, 5L, 4L, 2L))
  # Too short to have a middle, and an unknown name, both pass through.
  expect_identical(arr(c(2L, 1L), "edges"), c(2L, 1L))
  expect_identical(arr(idx, "nonsense"), idx)
})

# ---------------------------------------------------------------------------
# Through the readers
# ---------------------------------------------------------------------------

# A document where three paragraphs answer the question the same way and three
# say something else, so redundancy is a real cost rather than a hypothetical.
redundant_doc <- function() {
  paste(c("Revenue was 45.2 million dollars in fiscal 2024.",
          "Total revenue reached 45.2 million dollars in the 2024 fiscal year.",
          "In fiscal 2024 the company recorded revenue of 45.2 million dollars.",
          "Headcount grew to 1,204 employees across nine clinical sites.",
          "The board approved a dividend of 0.42 dollars per share in March.",
          "Costs per averted event were estimated at 3,140 dollars in the base case."),
        collapse = "\n\n")
}

local_lexical <- function(env = parent.frame()) {
  local_registries(env)
  gr_options(embedder = "lexical")   # deterministic, so these assertions are stable
}

test_that("the defaults are exactly the previous behaviour", {
  local_lexical()
  cl <- mock_echo("an answer")
  ch <- gr_segment(gr_ingest(redundant_doc()), list(method = "paragraph", max_tokens = 40))
  plain <- quiet(gr_read(ch, "What was revenue?", cl, list(reader = "retrieve", top_k = 3)))
  spelled <- quiet(gr_read(ch, "What was revenue?", cl,
                           list(reader = "retrieve", top_k = 3, mmr = 1,
                                context_order = "relevance")))
  expect_identical(plain$chunks_used, spelled$chunks_used)
  expect_identical(plain$notes$mmr, 1)
  expect_identical(plain$notes$context_order, "relevance")
})

test_that("MMR changes which chunks are paid for, and it is recorded", {
  local_lexical()
  cl <- mock_echo("an answer")
  ch <- gr_segment(gr_ingest(redundant_doc()), list(method = "paragraph", max_tokens = 40))
  expect_gte(nrow(ch$chunks), 5L)

  topk <- quiet(gr_read(ch, "What was revenue?", cl, list(reader = "retrieve", top_k = 3)))
  diverse <- quiet(gr_read(ch, "What was revenue?", cl,
                           list(reader = "retrieve", top_k = 3, mmr = 0.3)))

  expect_length(topk$chunks_used, length(diverse$chunks_used))
  expect_false(identical(sort(topk$chunks_used), sort(diverse$chunks_used)))
  expect_identical(diverse$notes$mmr, 0.3)
  # The best chunk is still the best chunk: diversity trades off relevance, it
  # does not abandon it.
  expect_identical(diverse$chunks_used[1], topk$chunks_used[1])
})

test_that("context_order rearranges the prompt without changing the selection", {
  local_lexical()
  cl <- mock_echo("an answer")
  ch <- gr_segment(gr_ingest(redundant_doc()), list(method = "paragraph", max_tokens = 40))
  runs <- lapply(c("relevance", "document", "edges"), function(o)
    quiet(gr_read(ch, "What was revenue?", cl,
                  list(reader = "retrieve", top_k = 4, context_order = o))))
  names(runs) <- c("relevance", "document", "edges")

  sets <- lapply(runs, function(a) sort(a$chunks_used))
  expect_identical(sets$relevance, sets$document)
  expect_identical(sets$relevance, sets$edges)          # same chunks throughout
  expect_identical(runs$document$chunks_used, sort(runs$document$chunks_used))
  expect_false(identical(runs$relevance$chunks_used, runs$edges$chunks_used))
  expect_identical(runs$edges$notes$context_order, "edges")
})

test_that("the prompt really is in the arranged order", {
  # chunks_used could be reordered while the rendered prompt was not. Read the
  # prompt the client actually received.
  local_lexical()
  cl <- mock_echo("an answer")
  ch <- gr_segment(gr_ingest(redundant_doc()), list(method = "paragraph", max_tokens = 40))
  prompt_of <- function(o) {
    cl$reset()
    a <- quiet(gr_read(ch, "What was revenue?", cl,
                       list(reader = "retrieve", top_k = 4, context_order = o)))
    call <- Filter(function(x) identical(x$label, "retrieve.answer"), cl$calls())[[1]]
    txt <- paste(vapply(call$messages, function(m) m$content, character(1)), collapse = "\n")
    list(answer = a, positions = vapply(a$chunks_used, function(id)
      regexpr(sprintf("chunk %s", id), txt, fixed = TRUE)[[1]], numeric(1)))
  }
  for (o in c("relevance", "document", "edges")) {
    p <- prompt_of(o)
    # Every used chunk appears, and in the order chunks_used claims.
    expect_true(all(p$positions > 0))
    expect_false(is.unsorted(p$positions))
  }
})

test_that("min_score is applied before selection, so top_k is still filled", {
  # Applied after selection, a chunk below the floor could displace one above it
  # and then be dropped, quietly returning fewer chunks than asked for.
  local_lexical()
  cl <- mock_echo("an answer")
  ch <- gr_segment(gr_ingest(redundant_doc()), list(method = "paragraph", max_tokens = 40))
  n <- nrow(ch$chunks)
  loose <- quiet(gr_read(ch, "What was revenue?", cl,
                         list(reader = "retrieve", top_k = 3, min_score = -Inf)))
  expect_length(loose$chunks_used, 3L)

  # An impossible floor still yields the single best chunk, as documented.
  strict <- quiet(gr_read(ch, "What was revenue?", cl,
                          list(reader = "retrieve", top_k = 3, min_score = 99)))
  expect_length(strict$chunks_used, 1L)
  expect_identical(strict$chunks_used, loose$chunks_used[1])
  expect_gt(n, 3L)
})

test_that("rerank arranges its prompt too", {
  # The scores have to make the relevance order differ from document order, or
  # "document" and "relevance" agree by accident and the assertion proves
  # nothing -- which is exactly what the first version of this test did, using a
  # mock that gave every chunk the same score.
  local_lexical()
  ch <- gr_segment(gr_ingest(redundant_doc()), list(method = "paragraph", max_tokens = 40))
  scoring <- gr_mock_client(function(messages, params) {
    sys <- messages[[1]]$content
    if (!grepl("Rate how useful", sys, fixed = TRUE)) return("an answer")
    excerpt <- messages[[length(messages)]]$content
    sc <- if (grepl("dividend", excerpt, fixed = TRUE)) 9L
          else if (grepl("Headcount", excerpt, fixed = TRUE)) 8L
          else if (grepl("Costs", excerpt, fixed = TRUE)) 7L else 5L
    sprintf('{"score": %d, "reason": "scored"}', sc)
  })
  run <- function(o) quiet(gr_read(ch, "What was revenue?", scoring,
                                   list(reader = "rerank", top_k = 4, rerank_candidates = 6,
                                        context_order = o)))
  by_rel <- run("relevance")
  by_doc <- run("document")
  by_edge <- run("edges")

  expect_gte(length(by_rel$chunks_used), 3L)
  expect_setequal(by_rel$chunks_used, by_doc$chunks_used)
  expect_setequal(by_rel$chunks_used, by_edge$chunks_used)
  expect_true(is.unsorted(by_rel$chunks_used))            # the scores really do reorder
  expect_identical(by_doc$chunks_used, sort(by_rel$chunks_used))
  expect_false(identical(by_rel$chunks_used, by_edge$chunks_used))
})

test_that("iterative accepts mmr and never re-reads a chunk", {
  local_lexical()
  ch <- gr_segment(gr_ingest(redundant_doc()), list(method = "paragraph", max_tokens = 40))
  it <- quiet(gr_read(ch, "What was revenue?", mock_echo("an answer"),
                      list(reader = "iterative", top_k = 2, max_rounds = 2, mmr = 0.4)))
  expect_s3_class(it, "gr_answer")
  expect_identical(anyDuplicated(it$chunks_used), 0L)
})

test_that("the spec validates both settings", {
  expect_error(gr_read_spec(context_order = "sideways"))
  expect_identical(gr_read_spec()$mmr, 1)
  expect_identical(gr_read_spec()$context_order, "relevance")
  expect_identical(gr_read_spec(mmr = 0.5)$mmr, 0.5)
  # Out of range is clamped with a warning, like every other numeric setting.
  expect_warning(s <- gr_read_spec(mmr = 4))
  expect_identical(s$mmr, 1)
  expect_warning(s2 <- gr_read_spec(mmr = -1))
  expect_identical(s2$mmr, 0)
})

test_that("both settings survive a recipe and a comparison", {
  local_lexical()
  cl <- mock_echo("an answer")
  cmp <- quiet(gr_compare(
    redundant_doc(), "What was revenue?",
    list(topk = list(segment = list(method = "paragraph", max_tokens = 40),
                     read = list(reader = "retrieve", top_k = 3, mmr = 1)),
         mmr  = list(segment = list(method = "paragraph", max_tokens = 40),
                     read = list(reader = "retrieve", top_k = 3, mmr = 0.3))),
    client = cl))
  # Two recipes differing only in mmr are NOT the same pipeline: the read spec
  # is hashed whole, so they must both actually run.
  expect_identical(nrow(cmp$summary), 2L)
  expect_false(identical(cmp$answers$topk$chunks_used, cmp$answers$mmr$chunks_used))
})

# ---------------------------------------------------------------------------
# A bug the new NULL-valued option surfaced
# ---------------------------------------------------------------------------

test_that("gr_options() survives repeated set-and-restore of a NULL-valued option", {
  # gr_options() documents that it "composes with on.exit()". It did not, for any
  # option whose stored value was NULL. The returned "old" value was built with
  # modifyList(), which DELETES a key whose value is NULL, so after one restore
  # the name came back as NA and the NEXT gr_options(old) failed with
  # "Unknown option(s): NA" -- the second use of the documented pattern, in a
  # function that had already been fixed once for exactly this NULL trap.
  local_registries()
  for (opt in c("embedder", "temperature", "max_cost_usd", "cache_dir")) {
    before <- gr_options(opt)
    for (i in 1:3) {
      old <- do.call(gr_options, stats::setNames(list("x"), opt))
      expect_identical(names(old), opt)
      expect_no_error(gr_options(old))
    }
    expect_identical(gr_options(opt), before)   # and it really is back
  }
})

test_that("gr_options() returns the value it replaced, NULL included", {
  local_registries()
  expect_null(gr_options(embedder = "lexical")$embedder)
  expect_identical(gr_options(embedder = "api")$embedder, "lexical")
  expect_identical(gr_options("embedder"), "api")
})
