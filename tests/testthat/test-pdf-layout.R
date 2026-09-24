# test-pdf-layout.R
#
# PDF pages put back in reading order: two columns read one after the other,
# running heads and feet removed, headings found. The helpers are tested on
# lines like the ones pdftools::pdf_text() produces, so they run without
# pdftools; the last tests build real PDFs and need it.

pad <- function(left, right, at = 60L) {
  paste0(left, strrep(" ", max(2L, at - nchar(left))), right)
}

two_column_lines <- function(n = 8L, page = 1L) {
  vapply(seq_len(n), function(i)
    pad(sprintf("Left %d.%d: the spacing effect was studied.", page, i),
        sprintf("Right %d.%d: retention fell when massed.", page, i)),
    character(1))
}

test_that("a gutter is found on a two-column page and not on a single-column one", {
  g <- readgpt:::column_gutter(two_column_lines())
  expect_false(is.na(g))
  # Between the end of the longest left text and the start of the right one.
  expect_gt(g, nchar("Left 1.1: the spacing effect was studied."))
  expect_lt(g, 61L)

  prose <- rep(paste("The cohort comprised 482 participants recruited across nine clinical",
                     "sites, and adherence exceeded 91 percent."), 8)
  expect_true(is.na(readgpt:::column_gutter(prose)))
  # Justified text puts double spaces here and there, but not in one place.
  justified <- vapply(1:8, function(i) {
    words <- strsplit(prose[1], " ")[[1]]
    words[i + 2L] <- paste0(words[i + 2L], " ")
    paste(words, collapse = " ")
  }, character(1))
  expect_true(is.na(readgpt:::column_gutter(justified)))
  expect_true(is.na(readgpt:::column_gutter(c("Short.", "Lines.", "Only.", "Here.", "Now."))))
  # Lines that line up are not a gutter when most lines run straight through
  # the same place...
  mostly_prose <- c(prose, prose, prose[1:3], two_column_lines(6L))
  expect_true(is.na(readgpt:::column_gutter(mostly_prose)))
  # ...but a full-width abstract above two columns does not hide them.
  first_page <- c(prose, two_column_lines(10L))
  expect_false(is.na(readgpt:::column_gutter(first_page)))
  # And a page that is mostly a table is still read row by row.
  table_page <- c(prose[1:2], vapply(1:12, function(i)
    pad(sprintf("Site %d, clinic %s", i, LETTERS[i]), sprintf("%d participants", 40 + i)),
    character(1)))
  expect_true(is.na(readgpt:::column_gutter(table_page)))
})

test_that("a two-column page is read left column first, in bands around full-width lines", {
  lines <- c("A Title That Runs Across Both Columns Of The Page In Full Width",
             two_column_lines(3L),
             "Figure 1. A caption set across both columns of the page, full width.",
             two_column_lines(2L, page = 2L))
  g <- readgpt:::column_gutter(lines)
  out <- trimws(readgpt:::reorder_columns(lines, g))
  out <- out[nzchar(out)]
  expect_identical(substr(out, 1, 9), c(
    "A Title T",
    "Left 1.1:", "Left 1.2:", "Left 1.3:", "Right 1.1", "Right 1.2", "Right 1.3",
    "Figure 1.",
    "Left 2.1:", "Left 2.2:", "Right 2.1", "Right 2.2"))
})

test_that("running heads and feet are dropped from page edges, and only there", {
  page <- function(i) c("Journal of Learning Studies, vol. 12", "",
                        sprintf("Body text of page %d, which differs on every page.", i),
                        "Repeated line in the body.",
                        sprintf("More body text for page %d.", i),
                        sprintf("Page %d of 5", i))
  pages <- lapply(1:5, page)
  out <- readgpt:::drop_running_lines(pages)
  flat <- unlist(out)
  expect_false(any(grepl("Journal of Learning Studies", flat)))
  expect_false(any(grepl("^Page [0-9] of 5$", flat)))
  # Repeated, but not at an edge: kept on every page.
  expect_identical(sum(flat == "Repeated line in the body."), 5L)
  expect_identical(sum(grepl("^Body text of page", flat)), 5L)
  # Too few pages to tell a running head from a coincidence.
  expect_identical(readgpt:::drop_running_lines(pages[1:2]), pages[1:2])
  # A long line at the foot of every page is content, even when its numbers
  # are the only thing that changes.
  long <- lapply(1:5, function(i) c(sprintf("Opening sentence of page %d, which is content.", i),
    sprintf("Total assets at the end of quarter %d were %d thousand dollars across all sites.", i, i * 7)))
  expect_identical(readgpt:::drop_running_lines(long), long)
  # The same long line on every page is furniture: a notice, not content.
  notice <- "This report is confidential and intended only for the board; do not forward or copy it."
  noted <- lapply(1:5, function(i) c(sprintf("Opening sentence of page %d, which is content.", i),
                                     notice))
  expect_false(notice %in% unlist(readgpt:::drop_running_lines(noted)))
})

test_that("headings come from the bookmarks, or from a standard section name", {
  m <- readgpt:::heading_matcher(c("1 Introduction", "Study Design and Setting"))
  expect_identical(m("1. Introduction"), "1 Introduction")
  expect_identical(m("  study design and setting  "), "Study Design and Setting")
  expect_identical(m("2.1 Results"), "2.1 Results")
  expect_identical(m("DISCUSSION"), "DISCUSSION")
  expect_identical(m("Appendix B"), "Appendix B")
  expect_true(is.na(m("The results were clear.")))
  expect_true(is.na(m("Results of the second trial were mixed across the nine sites")))
  expect_true(is.na(m("")))
})

test_that("blocks carry the heading they fall under, across pages", {
  pages <- list(c("Introduction", "First paragraph.", "", "Second paragraph."),
                c("Continued on page two.", "", "2. Methods", "How it was done."))
  b <- readgpt:::pdf_page_blocks(pages, readgpt:::heading_matcher())
  expect_identical(b$kind, c("heading", "body", "body", "body", "heading", "body"))
  expect_identical(b$section, c("Introduction", "Introduction", "Introduction",
                                "Introduction", "2. Methods", "2. Methods"))
  expect_identical(b$page, c(1L, 1L, 1L, 2L, 2L, 2L))
})

test_that("the layout setting is kept out of a default spec, so keys do not move", {
  expect_null(gr_ingest_spec()[["layout", exact = TRUE]])
  expect_identical(gr_ingest_spec(layout = "raw")$layout, "raw")
  expect_error(gr_ingest_spec(layout = "columns"))
  # Accepted as an override, like any other ingestion setting.
  rec <- readgpt:::apply_overrides(readgpt:::as_recipe("fast"), list(layout = "raw"))
  expect_identical(rec$ingest$layout, "raw")
})

# ---------------------------------------------------------------------------
# Real PDFs
# ---------------------------------------------------------------------------

two_column_pdf <- function(pages = 4L) {
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 8.5, height = 11)
  for (pg in seq_len(pages)) {
    graphics::plot.new()
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.window(c(0, 1), c(0, 1))
    graphics::text(0.05, 0.97, "Journal of Learning Studies, vol. 12", adj = 0, cex = 0.8)
    if (pg == 1) graphics::text(0.05, 0.93, "Introduction", adj = 0)
    if (pg == 3) graphics::text(0.05, 0.93, "2. Methods", adj = 0)
    graphics::text(0.05, 0.88 - (0:7) * 0.02,
                   sprintf("Left %d.%d: the spacing effect was studied in adults.", pg, 1:8),
                   adj = 0, cex = 0.8)
    graphics::text(0.55, 0.88 - (0:7) * 0.02,
                   sprintf("Right %d.%d: retention fell when practice was massed.", pg, 1:8),
                   adj = 0, cex = 0.8)
    graphics::text(0.5, 0.03, as.character(pg), cex = 0.8)
  }
  invisible(grDevices::dev.off())
  f
}

test_that("a two-column PDF is read in reading order, with sections and without its running head", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  f <- two_column_pdf()
  doc <- gr_ingest(f)
  text <- doc$text
  # Every left-column line of a page comes before any right-column line of it.
  for (pg in 1:4) {
    left <- regexpr(sprintf("Left %d.8", pg), text, fixed = TRUE)
    right <- regexpr(sprintf("Right %d.1", pg), text, fixed = TRUE)
    expect_gt(left, 0L)
    expect_gt(right, left)
  }
  expect_false(grepl("Journal of Learning Studies", text, fixed = TRUE))
  expect_identical(unique(doc$blocks$section), c("Introduction", "2. Methods"))
  expect_identical(doc$blocks$section[doc$blocks$page == 4][1], "2. Methods")
  expect_true(all(doc$blocks$kind[doc$blocks$text %in% c("Introduction", "2. Methods")] == "heading"))

  # layout = "raw" is the page as pdftools lays it out: the columns interleave.
  raw <- gr_ingest(f, gr_ingest_spec(layout = "raw"))
  expect_match(raw$text, "Left 1.1: the spacing effect was studied in adults.\\s+Right 1.1")
  expect_true(grepl("Journal of Learning Studies", raw$text, fixed = TRUE))
  expect_true(all(is.na(raw$blocks$section)))
})

test_that("a single-column PDF reads as it did", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f)
  graphics::plot.new()
  graphics::text(0, 0.9 - (0:5) * 0.05,
                 sprintf("Sentence %d of a plain page runs across the whole width of it.", 1:6),
                 adj = 0)
  invisible(grDevices::dev.off())
  auto <- gr_ingest(f)
  raw <- gr_ingest(f, gr_ingest_spec(layout = "raw"))
  expect_identical(auto$text, raw$text)
})
