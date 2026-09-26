# test-review3-pdf.R
#
# Regression tests for defects that the earlier fixes to the PDF reading-order
# code (R/ingest-pdf.R) introduced, found when those fixes were verified:
# two-column rows glued into one line (hanging-indent references, code, a
# heading on a page's first row, a justified last row), page numbers and
# section feet left on every page, table notes and table headers dropped, and
# a one-sided gutter found on a single-column table. The page text is the
# kind pdftools::pdf_text() returns.

# A two-column page in pdftotext's shape: left lines of about 55 characters,
# the right column starting at position `at`.
two_col <- function(left, right, at = 58L) {
  if (!nzchar(right)) return(left)
  paste0(left, strrep(" ", max(2L, at - 1L - nchar(left))), right)
}
l_text <- function(i) sprintf("Left %02d: the spacing effect was studied in adults, and", i)
r_text <- function(i) sprintf("Right %02d: retention fell when practice was massed in", i)
col_tag <- function(x) sub("^((Left|Right) [0-9]+):.*", "\\1", x)
col_tags <- function(side, i) sprintf("%s %02d", side, i)

read_page <- function(lines) {
  g <- readgpt:::column_gutter(lines)
  expect_false(is.na(g))
  out <- trimws(readgpt:::reorder_columns(lines, g))
  out[nzchar(out)]
}

# ---------------------------------------------------------------------------
# Two-column rows that are not a table (ingest-1, r3-...-04)
# ---------------------------------------------------------------------------

test_that("references with a hanging indent are read one column after the other", {
  ref <- function(k, side) c(
    sprintf("[%02d] Author%s A, Writer B (%d). A title", k, side, 2000 + k),
    sprintf("     of the paper number %02d goes here", k),
    sprintf("     Journal of Things, %d, 1-20.", k))
  left <- unlist(lapply(1:6, ref, side = "L"))
  right <- unlist(lapply(7:12, ref, side = "R"))
  lines <- paste0(formatC(left, width = -45), "     ", right)
  out <- read_page(lines)
  # Each entry's first line is a line of its own, and the left column's
  # entries all come before the right column's.
  firsts <- grep("^\\[[0-9]{2}\\]", out, value = TRUE)
  expect_identical(substr(firsts, 1, 4), sprintf("[%02d]", 1:12))
  expect_identical(out[2:3], c("of the paper number 01 goes here", "Journal of Things, 1, 1-20."))
  expect_false(any(grepl("A title +\\[", out)))
})

test_that("code set in both columns is read one column after the other", {
  code_l <- c("    typedef typename", "        traits::storage_type<RTYPE>::type",
              "        stored_type;", "    inline stored_type get(int i) const {",
              "        return cache[i];", "    }")
  code_r <- c("    public:", "    typedef typename", "        ::Rcpp::traits::result_of<F>::type;",
              "    const static int RESULT_R_TYPE =", "        result_type>::rtype;", "    };")
  lines <- c(vapply(1:4, function(i) two_col(l_text(i), r_text(i)), character(1)),
             vapply(1:6, function(i) two_col(code_l[i], code_r[i]), character(1)),
             vapply(5:8, function(i) two_col(l_text(i), r_text(i + 6L)), character(1)))
  out <- read_page(lines)
  expect_identical(out[5:10], trimws(code_l))
  expect_identical(out[19:24], trimws(code_r))
  expect_identical(col_tag(out[-c(5:10, 19:24)]),
                   c(col_tags("Left", 1:8), col_tags("Right", c(1:4, 11:14))))
})

test_that("a heading beside the other column on a page's first row is kept as a heading", {
  # The first row of the page: the last line of a paragraph in the left
  # column, and a heading set in the right one, off the column's left edge.
  first <- paste0("   You should now have a clear understanding of the differ-",
                  strrep(" ", 24), "3 Results")
  lines <- c(first, vapply(1:8, function(i) two_col(l_text(i), r_text(i), at = 62L), character(1)))
  lines[2] <- two_col("ence between fixed and random effects, but let us sum this",
                      r_text(1), at = 62L)
  out <- read_page(lines)
  expect_identical(out[1:2], c("You should now have a clear understanding of the differ-",
                               "ence between fixed and random effects, but let us sum this"))
  b <- readgpt:::pdf_page_blocks(list(out), readgpt:::heading_matcher())
  expect_identical(b$text[b$kind == "heading"], "3 Results")
  expect_identical(b$section[b$kind == "body"][2], "3 Results")
})

test_that("a justified line at the foot of a page is not glued across the columns", {
  # pdftotext leaves runs of three spaces in a loosely justified line.
  lines <- vapply(1:8, function(i) two_col(l_text(i), r_text(i)), character(1))
  lines[8] <- two_col("throughout the prespecified observation window. In",
                      "adherence   trajectories after adjustment  for socioeconomic")
  out <- read_page(lines)
  expect_identical(out[8], "throughout the prespecified observation window. In")
  expect_identical(out[16], "adherence   trajectories after adjustment  for socioeconomic")
  expect_identical(col_tag(out[-c(8, 16)]), c(col_tags("Left", 1:7), col_tags("Right", 1:7)))
})

test_that("a running head set apart, a title block and a full-width table still span", {
  # The running head: the first row, set apart by a blank one.
  head <- two_col("Journal of Testing 12(3)", paste0(strrep(" ", 40), "Smith et al."))
  lines <- c(head, "", vapply(1:8, function(i) two_col(l_text(i), r_text(i)), character(1)))
  out <- read_page(lines)
  expect_match(out[1], "^Journal of Testing 12\\(3\\) +Smith et al\\.$")
  # The authors of a first page, side by side above the columns.
  title <- c(paste0(strrep(" ", 43), "A Trial of Widgets"),
             paste0(strrep(" ", 43), "A. Smith", strrep(" ", 20), "B. Lee"), "", "")
  out <- read_page(c(title, vapply(1:8, function(i) two_col(l_text(i), r_text(i)), character(1))))
  expect_identical(gsub(" +", " ", out[1:2]), c("A Trial of Widgets", "A. Smith B. Lee"))
  expect_identical(col_tag(out[-(1:2)]), c(col_tags("Left", 1:8), col_tags("Right", 1:8)))
})

test_that("a left line that runs a word past the gutter keeps the word", {
  # The row's last words reach into the gutter, a space after a word that
  # ends at the left column's margin; there is no right line on the row.
  lines <- vapply(1:8, function(i) two_col(l_text(i), r_text(i), at = 64L), character(1))
  long <- "Left 05: the spacing effect was studied in adults, and so favourable"
  lines <- c(lines[1:4], long,
             two_col("adherence trajectories following independent checks.", r_text(9), at = 64L),
             lines[5:8])
  out <- read_page(lines)
  expect_identical(out[5:6], c(long, "adherence trajectories following independent checks."))
  expect_identical(col_tag(out[-(5:6)]), c(col_tags("Left", 1:8), col_tags("Right", c(1:4, 9, 5:8))))
})

test_that("right-only lines set a little left of the gutter are read with their column", {
  # Twelve rows share both columns; below them the rows alternate, and the
  # right column's lines start a character left of the gutter.
  shared <- vapply(1:12, function(i) two_col(l_text(i), r_text(i), at = 60L), character(1))
  inter <- as.vector(rbind(vapply(13:18, l_text, character(1)),
                           paste0(strrep(" ", 55), vapply(13:18, r_text, character(1)))))
  out <- read_page(c(shared, inter))
  expect_identical(col_tag(out), c(col_tags("Left", 1:18), col_tags("Right", 1:18)))
})

# ---------------------------------------------------------------------------
# One-sided gutter (ingest-7)
# ---------------------------------------------------------------------------

test_that("a single-column table whose last column wraps is not read as two columns", {
  row <- function(a, b, c, d) sprintf("%-29s%-14s%-16s%s", a, b, c, d)
  cont <- function(d) paste0(strrep(" ", 59), d)
  lines <- c(row("Stage", "", "Function", "What it does"),
             row("Parsing stage", "(Part 2)", "parseSpec", "Reads a model formula,"),
             cont("the data and the other inputs,"), cont("and returns what the next"),
             cont("stage needs to go on."),
             row("Fitting stage", "(Part 3)", "fitSpec", "Takes the parsed pieces"),
             cont("and returns a function that"), cont("gives the objective for any"),
             cont("value of the parameters."),
             row("Search stage", "(Part 4)", "searchSpec", "Takes the objective and"),
             cont("returns the values at which"), cont("it is smallest."),
             row("Output stage", "(Part 5)", "wrapSpec", "Takes the smallest values"),
             cont("and returns the fitted model."), "",
             "                    Table 1: The stages of a model fit.", "",
             "The first stage checks the formula against the data and builds the matrices.",
             "the parsing stage,", "",
             "> parsed <- parseSpec(formula = y ~ x + (x | group), data = d)", "",
             "the fitting stage,", "",
             "> objective <- do.call(fitSpec, parsed)", "",
             "the search stage,", "",
             "> best <- searchSpec(objective)")
  expect_true(is.na(readgpt:::column_gutter(lines)))
})

# ---------------------------------------------------------------------------
# Running heads and feet (ingest-2, ingest-03, ingest-6, r3-...-04)
# ---------------------------------------------------------------------------

rl_body <- function(p) sprintf("Body sentence %d on page %d talks about outcomes at length here.",
                               1:6, p)
rl_page <- function(p, head = NULL, foot = NULL) c(head, "", rl_body(p), "", foot)
dropped <- function(pages) setdiff(unlist(pages), unlist(readgpt:::drop_running_lines(pages)))

test_that("page numbers go when one page has another bare number at the same edge", {
  # Page 6 ends with an axis label just above its page number.
  pages <- lapply(1:10, function(p) rl_page(p, foot = sprintf("%30d", p)))
  pages[[6]] <- c(rl_body(6), "   12", sprintf("%30d", 6))
  expect_setequal(dropped(pages), sprintf("%30d", 1:10))
  # The same with the page number at the top, and the label below it.
  pages <- lapply(1:6, function(p) rl_page(p, head = as.character(p)))
  pages[[4]] <- c("4", "150", rl_body(4))
  expect_setequal(dropped(pages), as.character(1:6))
  # A word line too: page 7 has a body line "Page 3" above its foot.
  pages <- lapply(1:10, function(p) rl_page(p, foot = sprintf("Page %d", p)))
  pages[[7]] <- c(rl_body(7), "Page 3", "", "Page 7")
  out <- readgpt:::drop_running_lines(pages)
  expect_false(any(grepl("^Page ([0-9]|10)$", unlist(out)[unlist(out) != "Page 3"])))
  expect_identical(sum(unlist(out) == "Page 3"), 1L)
})

test_that("feet numbered by section or chapter are dropped", {
  sec <- lapply(1:12, function(p) rl_page(p, foot = sprintf("Page %d-%d", (p - 1) %/% 4 + 1,
                                                            (p - 1) %% 4 + 1)))
  expect_identical(length(dropped(sec)), 12L)
  chap <- lapply(1:8, function(p) rl_page(p, foot = sprintf("Chapter %d | %d", (p - 1) %/% 2 + 1, p)))
  expect_identical(length(dropped(chap)), 8L)
  # Rows of a table, whose numbers do not follow the page, are still kept.
  set.seed(4)
  rows <- lapply(1:6, function(p) c(sprintf("Region %d  %d  %.1f", (p - 1) * 10 + 1:10,
                                            sample(100:999, 10), stats::runif(10, 1, 9))))
  expect_identical(dropped(rows), character(0))
})

test_that("a long head whose page number stands in a cell of its own is dropped", {
  heads <- list(
    bmc = function(p) sprintf("Smith et al. BMC Medicine (2026) 24:101        Page %d of 12", p),
    plos = function(p) sprintf("PLOS ONE | https://doi.org/10.1371/journal.pone.0212345   March 5, 2019        %d / 15", p),
    mdpi = function(p) sprintf("Nutrients 2019, 11, 512        %d of 15", p),
    frontiers = function(p) sprintf("Frontiers in Psychology | www.frontiersin.org   %d   March 2019 | Volume 10 | Article 512", p))
  for (h in heads) {
    pages <- lapply(1:12, function(p) rl_page(p, head = h(p)))
    expect_identical(readgpt:::drop_running_lines(pages), lapply(1:12, function(p) rl_page(p)))
  }
  # A number inside a sentence is still content.
  inside <- lapply(1:6, function(p) rl_page(p, head = sprintf(
    "In cluster %d the investigators documented the outcome of the trial.", p + 40)))
  expect_identical(readgpt:::drop_running_lines(inside), inside)
})

test_that("the header of a table continued across pages is kept above its rows", {
  set.seed(3)
  years <- 1961:2020
  pages <- lapply(1:5, function(p) c(
    sprintf("Table B1 (continued). Annual incidence of disease X, part %s", letters[p]), "",
    "Year    Rate    SE     Cases",
    sprintf("%d    %4.1f    %3.1f    %d", years[(p - 1) * 12 + 1:12],
            stats::runif(12, 10, 60), stats::runif(12, 1, 9), sample(1000:9999, 12)),
    "", sprintf("Page %d of 5", p)))
  out <- readgpt:::drop_running_lines(pages)
  expect_identical(vapply(out, function(l) sum(l == "Year    Rate    SE     Cases"), integer(1)),
                   rep(1L, 5))
  expect_identical(sum(grepl("^(19|20)[0-9]{2} ", unlist(out))), 60L)
  expect_false(any(grepl("^Page [0-9] of 5$", unlist(out))))
  # A running head above a blank line is not a table's header.
  heads <- lapply(1:5, function(p) c("Journal of Learning Studies, vol. 12", "", pages[[p]][-1]))
  expect_false("Journal of Learning Studies, vol. 12" %in% unlist(readgpt:::drop_running_lines(heads)))
})

test_that("a note under two tables of a short paper is not taken for a running foot", {
  note <- "Note: Standard errors in parentheses. * p < 0.10, ** p < 0.05, *** p < 0.01."
  tab <- function(k) c(sprintf("Table %d: Regression of outcome on treatment", k),
                       "                 (1)        (2)",
                       sprintf("Treatment      0.%d3**    0.2%d*", k, k),
                       "               (0.05)     (0.11)", "Observations    1,204      1,204")
  body2 <- function(p) rl_body(p)[1:2]
  for (numbered in c(FALSE, TRUE)) {
    pages <- list(c("A Working Paper on Widgets", "Jane Doe", body2(1), if (numbered) "1"),
                  c(body2(2), tab(1), note, if (numbered) "2"),
                  c(body2(3), if (numbered) "3"),
                  c(body2(4), tab(2), note, if (numbered) "4"))
    out <- readgpt:::drop_running_lines(pages)
    expect_identical(sum(unlist(out) == note), 2L)
    expect_identical(sum(unlist(out) == "Observations    1,204      1,204"), 2L)
    expect_false(any(unlist(out) %in% as.character(1:4)))
  }
  # A line ending a sentence at the foot of two pages is content too.
  line <- "the predefined noninferiority margin of the primary analysis."
  pages <- lapply(1:5, function(p) c(rl_body(p), if (p %in% c(3, 5)) line))
  expect_identical(readgpt:::drop_running_lines(pages), pages)
  # Alternating heads of a short paper still go.
  alt <- lapply(1:5, function(p) c(
    if (p == 2 || p == 4) "Alice Smith, Bao Lee, and Chidi Okafor",
    if (p == 3 || p == 5) "Effects of widgets: a randomised trial", rl_body(p)))
  expect_identical(readgpt:::drop_running_lines(alt), lapply(1:5, rl_body))
})

# ---------------------------------------------------------------------------
# Real PDFs
# ---------------------------------------------------------------------------

test_that("page numbers are left out of a PDF whose chart puts a number by one of them", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 8.5, height = 11)
  for (p in 1:6) {
    graphics::plot.new()
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.window(c(0, 1), c(0, 1))
    graphics::text(0.5, 0.97, as.character(p), cex = 0.9)
    y <- 0.9
    if (p == 4) {
      # A number set alone just below the page number, as a chart's top axis
      # label is.
      graphics::text(0.08, 0.93, "150", adj = 0, cex = 0.9)
      y <- 0.85
    }
    for (k in 1:10) {
      graphics::text(0.08, y, sprintf("Sentence %d of page %d explains the method used.", k, p),
                     adj = 0, cex = 0.9)
      y <- y - 0.035
    }
  }
  invisible(grDevices::dev.off())
  doc <- suppressMessages(gr_ingest(f, cache = FALSE))
  nums <- trimws(doc$blocks$text)[grepl("^[0-9]+$", trimws(doc$blocks$text))]
  expect_identical(nums, "150")
})
