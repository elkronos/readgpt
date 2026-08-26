# demo.R -- runs entirely offline against a mock client.
#
# Demonstrates the three axes and, in particular, PROVES that the reading
# strategies are distinct by counting the prompts each one actually sends.
# Run with:  Rscript inst/examples/demo.R

library(gptread)
gr_options(verbose = FALSE, model = "gpt-4o", max_cost_usd = NULL)

doc_text <- paste(
  "# Annual Report 2024", "",
  "Acme Corp reported revenue of 45.2 million dollars in fiscal 2024, up from 38.7 million in 2023.",
  "Gross margin improved to 41 percent from 37 percent the prior year.", "",
  "## Operations", "",
  "The Sheffield plant produced 12,400 units. Downtime totalled 3 days in Q2 owing to a coolant failure.",
  "Headcount at Sheffield was 214 at year end.", "",
  "The Leipzig plant produced 9,850 units with no unplanned downtime.",
  "Leipzig headcount was 168, unchanged year on year.", "",
  "## Risk factors", "",
  "Currency exposure remains the principal risk. A 10 percent move in EUR/USD shifts operating income by 2.1 million dollars.",
  "Supply concentration is secondary: two vendors account for 61 percent of input cost.", "",
  "## Outlook", "",
  "Management expects revenue between 49 and 52 million dollars in 2025.",
  "Capital expenditure is budgeted at 6.4 million dollars.",
  sep = "\n")

# A deterministic offline stand-in for the API. It records every prompt, which
# is what lets us assert on call patterns rather than on returned strings.
cl <- gr_mock_client(function(messages, params) {
  txt <- paste(vapply(messages, function(m) m$content, character(1)), collapse = " ")
  if (grepl("Rate how useful", txt, fixed = TRUE)) return('{"score": 7, "reason": "on topic"}')
  if (grepl("reading iteratively", txt, fixed = TRUE)) {
    return('{"can_answer": false, "next_query": "Sheffield plant downtime", "answer": ""}')
  }
  if (grepl("45.2 million", txt, fixed = TRUE)) "Revenue was 45.2 million dollars in fiscal 2024."
  else "NOT_IN_DOCUMENT"
})

rule <- function(s) cat("\n", strrep("=", 72), "\n", s, "\n", strrep("=", 72), "\n", sep = "")
question <- "What was revenue in fiscal 2024?"

# ---------------------------------------------------------------------------
rule("AXIS 1 -- ingest: cleaning presets change what the model ever sees")
for (p in c("none", "standard", "academic", "legacy_v1")) {
  d <- gr_ingest(doc_text, p)
  cat(sprintf("  %-10s %2d blocks, %4d tokens, %3d chars removed | %s\n",
              p, d$stats$blocks, d$stats$tokens, d$stats$chars_removed,
              substr(d$blocks$text[2], 1, 52)))
}
cat("\n  Note `legacy_v1` (v1's defaults): every digit is gone, so no question\n",
    " about revenue, headcount or a date can be answered at all.\n", sep = "")

# ---------------------------------------------------------------------------
rule("AXIS 2 -- segment: same document, different boundary hypotheses")
doc <- gr_ingest(doc_text)
stats <- do.call(rbind, lapply(
  c("fixed", "paragraph", "sentence", "recursive", "structural", "semantic", "contextual"),
  function(m) gr_chunk_stats(suppressWarnings(
    gr_segment(doc, list(method = m, max_tokens = 90, overlap_tokens = 15), client = cl)))))
print(stats, row.names = FALSE)

cat("\n  Overlap is orthogonal to the method:\n")
base <- gr_chunk_stats(gr_segment(doc, list(method = "sentence", max_tokens = 90,
                                            overlap_tokens = 0)))$total_tokens
for (ov in c(0, 20, 40)) {
  s <- gr_chunk_stats(gr_segment(doc, list(method = "sentence", max_tokens = 90,
                                           overlap_tokens = ov)))
  cat(sprintf("    overlap=%2d -> %2d chunks, %4d total tokens (+%.0f%% duplication)\n",
              ov, s$n, s$total_tokens, 100 * (s$total_tokens / base - 1)))
}

# ---------------------------------------------------------------------------
rule("AXIS 3 -- read: distinct traversal signatures, distinct call patterns")
ch <- gr_segment(doc, list(method = "paragraph", max_tokens = 90))
cat(sprintf("  (%d chunks)\n\n", nrow(ch$chunks)))
cat(sprintf("  %-13s %-24s %5s  %s\n", "reader", "signature", "calls", "breakdown"))
cat("  ", strrep("-", 68), "\n", sep = "")
for (r in gr_readers()$name) {
  cl$reset()
  spec <- list(reader = r, top_k = 3, rerank_candidates = 4, max_rounds = 2, fan_in = 3)
  if (r == "ensemble") spec$members <- c("retrieve", "map_reduce", "refine")
  a <- suppressWarnings(gr_read(ch, question, cl, spec))
  lab <- table(vapply(cl$calls(), function(x) x$label, character(1)))
  cat(sprintf("  %-13s %-24s %5d  %s\n", r, a$signature, sum(lab),
              paste(sprintf("%s:%d", names(lab), as.integer(lab)), collapse = " ")))
}
cat("\n  map_reduce shows N calls rather than N+1 here because only one chunk\n",
    " yielded a finding, and a one-item reduce needs no call. On a document\n",
    " where several chunks contribute it is N + tree-reduce levels.\n", sep = "")

# ---------------------------------------------------------------------------
rule("The prompts differ, not just the labels")
for (r in c("map_reduce", "skim", "hierarchical", "refine")) {
  cl$reset(); invisible(suppressWarnings(gr_read(ch, question, cl, r)))
  cat(sprintf("  %-13s -> %s...\n", r, substr(cl$calls()[[1]]$messages[[1]]$content, 1, 66)))
}

# ---------------------------------------------------------------------------
rule("Pipelines are isolated: a recipe's answer does not depend on its company")
rec <- gr_recipe("target", segment = list(method = "sentence", max_tokens = 90),
                 read = "map_reduce")
alone <- answer_document(doc_text, question, rec, client = cl)
group <- suppressWarnings(gr_compare(doc_text, question,
  list(rec, gr_recipe("other", segment = "semantic", read = "retrieve"),
       gr_recipe("third", segment = "structural", read = "hierarchical")), client = cl))
cat(sprintf("  alone        : %d chunks -> %s\n", alone$segmentation$n, alone$answer))
cat(sprintf("  in a group   : %d chunks -> %s\n",
            group$answers$target$segmentation$n, group$answers$target$answer))
cat(sprintf("  identical    : %s\n", identical(alone$answer, group$answers$target$answer)))
cat("\n")
print(group$summary, row.names = FALSE)

# ---------------------------------------------------------------------------
rule("Duplicate pipelines are refused, not billed twice")
withCallingHandlers(
  invisible(gr_compare(doc_text, question,
    list(gr_recipe("A", segment = "paragraph", read = "map_reduce"),
         gr_recipe("B", segment = "paragraph", read = "map_reduce")), client = cl)),
  gr_duplicate_recipe = function(w) {
    cat("  ", conditionMessage(w), "\n", sep = ""); invokeRestart("muffleWarning") })

# ---------------------------------------------------------------------------
rule("Guard rails")
cat(sprintf("  budget for an unknown model stays positive : %d tokens\n",
            suppressWarnings(gr_budget("model-from-2027", reserve_output = 1024)$input)))
cat("  runaway call count is refused up front    : ")
old <- gr_options(max_calls = 3L)
cat(tryCatch({ gr_read(gr_segment(doc, list(method = "sentence", max_tokens = 40)),
                       question, cl, "map_reduce"); "NOT REFUSED" },
             gr_call_cap = function(e) "refused with an actionable message"), "\n")
gr_options(old)
cat("  every reader survives a dead API          : ")
dead <- gr_mock_client(function(m, p) stop("503"))
cat(all(vapply(gr_readers()$name, function(r) {
  a <- suppressWarnings(tryCatch(gr_read(ch, question, dead,
        list(reader = r, top_k = 2, members = c("retrieve", "map_reduce"))),
        error = function(e) NULL))
  inherits(a, "gr_answer") && is.character(a$answer) && length(a$answer) == 1L
}, logical(1))), "\n")

rule("Done")
