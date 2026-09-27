# test-review5-pdf.R
#
# Regression tests for defects that the fourth round of fixes to the PDF
# reading-order code (R/ingest-pdf.R) left or introduced, found when those
# fixes were verified: full-width abstract lines cut at a word boundary near
# the gutter, two tables set side by side glued row to row, headings and
# author blocks set side by side read the wrong way, the rows of short
# continued tables dropped as running lines, table notes dropped as running
# feet, a running head kept above a continued table, journal feet kept in
# short papers, and a one-sided gutter found in a two-cell table. The page
# text is the kind pdftools::pdf_text() returns.

two_col <- function(left, right, at = 58L) {
  if (!nzchar(right)) return(left)
  paste0(left, strrep(" ", max(2L, at - 1L - nchar(left))), right)
}
l_text <- function(i) sprintf("Left %02d: the spacing effect was studied in adults, and", i)
r_text <- function(i) sprintf("Right %02d: retention fell when practice was massed in", i)
col_tag <- function(x) sub("^((Left|Right) [0-9]+):.*", "\\1", x)
col_tags <- function(side, i) sprintf("%s %02d", side, i)
body_rows <- function(i) vapply(i, function(k) two_col(l_text(k), r_text(k)), character(1))

read_page <- function(lines) {
  g <- readgpt:::column_gutter(lines)
  expect_false(is.na(g))
  out <- trimws(readgpt:::reorder_columns(lines, g))
  out[nzchar(out)]
}
# Text laid out at fixed character positions, as pdftotext sets a row.
at_cols <- function(cells, at) {
  out <- strrep(" ", max(at) + max(nchar(cells)))
  for (k in seq_along(cells)) substr(out, at[k], at[k] + nchar(cells[k]) - 1L) <- cells[k]
  sub(" +$", "", out)
}

# ---------------------------------------------------------------------------
# A full-width abstract above two columns (pdf-1)
# ---------------------------------------------------------------------------

test_that("the lines of a full-width abstract are not cut at a word boundary near the gutter", {
  # Each line of the structured abstract has a word ending just left of
  # where the right column starts, one space before the next word; above and
  # below it are the heading and the paragraph's short last line.
  a <- c("Background: Spaced practice is thought to aid the recall",
         "Methods: We randomised 482 adults at nine sites to space",
         "Results: Recall at one year was higher in the spaced arm")
  b <- c("of facts in adults, but trials of it have been small.",
         "or mass their practice, and tested recall after a year.",
         "than in the massed arm, and the effect held at all sites.")
  expect_identical(nchar(a), rep(56L, 3))
  lines <- c("Abstract", paste(a, b), "Spacing should be the default.",
             "Keywords: spacing, recall", "", "", body_rows(1:8))
  out <- read_page(lines)
  expect_identical(out[1:6], c("Abstract", paste(a, b), "Spacing should be the default.",
                               "Keywords: spacing, recall"))
  expect_identical(col_tag(out[-(1:6)]), c(col_tags("Left", 1:8), col_tags("Right", 1:8)))
  # Nor is one cut where its lines end near the gutter above and below.
  lines <- c("Abstract", "Etiam euismod. Fusce facilisis lacinia dui. Suspendisse.",
             paste(a[2], b[2]), "Phasellus id magna. Duis malesuada interdum arcu. Integer",
             "Keywords: alpha, beta", "", "", body_rows(1:8))
  out <- read_page(lines)
  expect_identical(out[3], paste(a[2], b[2]))
})

test_that("a right-column heading beside full lines of the left column is still split off", {
  # The right column leaves a blank row above and below its heading; the
  # left column's lines run on beside it, one space short of the heading.
  lines <- body_rows(1:6)
  lines <- c(lines, l_text(7), paste0(substr(paste0(l_text(8), " so"), 1L, 56L), " 3 Methods"),
             l_text(9), body_rows(10:14))
  out <- read_page(lines)
  expect_true("3 Methods" %in% out)
  expect_identical(col_tag(out[out != "3 Methods"]),
                   c(col_tags("Left", 1:14), col_tags("Right", c(1:6, 10:14))))
})

# ---------------------------------------------------------------------------
# Tables, headings and captions set side by side (pdf-2, ingest-1)
# ---------------------------------------------------------------------------

test_that("two tables set side by side are read one after the other, not glued", {
  lt <- c(15L, 27L, 35L, 43L)
  rt <- c(76L, 90L, 98L, 106L)
  row <- function(l, r) {
    x <- if (length(l)) at_cols(l, lt) else ""
    if (!length(r)) return(x)
    y <- at_cols(r, rt)
    paste0(x, substr(y, nchar(x) + 1L, nchar(y)))
  }
  left <- list(c("Group", "N", "Mean", "P"), c("Control", "726", "94.4", "0.92"),
               c("Low dose", "272", "37.4", "0.42"), c("Placebo", "248", "89.0", "0.02"))
  right <- list(c("Outcome", "N", "Mean", "P"), c("Mortality", "576", "71.9", "0.73"),
                c("Stroke", "939", "64.9", "0.40"), c("Infection", "60", "95.9", "0.84"),
                c("Fracture", "111", "67.5", "0.65"), c("Bleeding", "295", "24.3", "0.69"))
  tabs <- vapply(1:6, function(k) row(if (k <= 4) left[[k]], right[[k]]), character(1))
  prose <- vapply(1:8, function(k) two_col(l_text(k), r_text(k), at = 64L), character(1))
  out <- gsub(" +", " ", read_page(c(tabs, "", prose)))
  flat <- vapply(left, paste, character(1), collapse = " ")
  flat_r <- vapply(right, paste, character(1), collapse = " ")
  # Each row of either table is a line of its own, and each table is whole.
  expect_identical(out[1:4], flat)
  at <- match(flat_r[1], out)
  expect_identical(out[at + 0:5], flat_r)
  expect_false(any(grepl("0.92 Mortality", out, fixed = TRUE)))
  # A table set across the page still spans it.
  full <- c(at_cols(c("Characteristic", "Treatment", "Control", "Difference", "P value"),
                    c(15L, 30L, 44L, 64L, 76L)),
            at_cols(c("Age, years", "61.2", "60.8", "0.4", "0.71"), c(15L, 30L, 44L, 64L, 76L)),
            at_cols(c("Women, n (%)", "118 (49)", "121 (50)", "-1", "0.78"), c(15L, 30L, 44L, 64L, 76L)))
  out <- gsub(" +", " ", read_page(c(body_rows(1:4), "", full, "", body_rows(5:8))))
  expect_true("Age, years 61.2 60.8 0.4 0.71" %in% out)
  # So does one whose column right of the gutter holds words, its cells
  # evenly spaced across the page.
  at <- c(15L, 32L, 44L, 60L, 70L, 80L)
  studies <- c(at_cols(c("Study", "Country", "Design", "Arm", "N", "Outcome"), at),
               at_cols(c("Smith (2019)", "UK", "Cohort", "RCT", "120", "Mortality"), at),
               at_cols(c("Lee (2021)", "Korea", "Trial", "Usual", "96", "Stroke"), at))
  out <- gsub(" +", " ", read_page(c(body_rows(1:4), "", studies, "", body_rows(5:8))))
  expect_true(all(c("Smith (2019) UK Cohort RCT 120 Mortality", "Lee (2021) Korea Trial Usual 96 Stroke")
                  %in% out))
})

test_that("a caption or heading beside a table's row is read with its own column", {
  # The left table's caption runs to a second line, beside the right
  # table's header; below the tables, a heading sits beside the right
  # table's last row.
  lt <- c(15L, 27L, 35L, 43L)
  rt <- c(76L, 90L, 98L, 106L)
  cells_r <- function(x) at_cols(x, rt)
  paste_over <- function(x, y) paste0(x, substr(y, nchar(x) + 1L, nchar(y)))
  lines <- c(paste_over("Table 2: The modest network mostly recovered while the",
                        paste0(strrep(" ", 67), "Table 3: The early circuit shifted.")),
             paste_over("school doubled.", cells_r(c("Group", "N", "Mean", "P"))),
             paste_over(at_cols(c("Group", "N", "Mean", "P"), lt), cells_r(c("student", "448", "84.5", "0.24"))),
             paste_over(at_cols(c("harbour", "480", "36.7", "0.63"), lt), cells_r(c("sample", "636", "83.4", "0.70"))),
             paste_over(at_cols(c("variance", "994", "93.1", "0.54"), lt), cells_r(c("bridge", "263", "43.8", "0.99"))),
             paste_over("1 Introduction", cells_r(c("nurse", "945", "21.0", "0.71"))), "",
             vapply(1:8, function(k) two_col(l_text(k), r_text(k), at = 64L), character(1)))
  out <- gsub(" +", " ", read_page(lines))
  expect_true("school doubled." %in% out)
  expect_true("Group N Mean P" %in% out)
  expect_false(any(grepl("Group N Mean P student", out, fixed = TRUE)))
  expect_true("1 Introduction" %in% out)
  b <- readgpt:::pdf_page_blocks(list(out), readgpt:::heading_matcher())
  expect_identical(b$text[b$kind == "heading"], "1 Introduction")
  # Section headings set side by side at the head of both columns.
  heads <- at_cols(c("INTRODUCTION", "BACKGROUND"), c(19L, 73L))
  out <- read_page(c(paste0(strrep(" ", 40), "Widgets and outcomes"), "", heads, "", body_rows(1:8)))
  b <- readgpt:::pdf_page_blocks(list(out), readgpt:::heading_matcher())
  expect_identical(b$text[b$kind == "heading"], c("INTRODUCTION", "BACKGROUND"))
  # The same as the page's first row, with no running head above it.
  out <- read_page(c(heads, "", body_rows(1:8)))
  b <- readgpt:::pdf_page_blocks(list(out), readgpt:::heading_matcher())
  expect_identical(b$text[b$kind == "heading"], c("INTRODUCTION", "BACKGROUND"))
})

test_that("code set in both columns at the same height is read one column after the other", {
  code_l <- sprintf("lft_v%d = lft_v%d + %d;   // lft step %d", 1:9, 0:8, 1:9, 1:9)
  code_r <- sprintf("rgt_v%d = rgt_v%d + %d;   // rgt step %d", 1:9, 0:8, 1:9, 1:9)
  rows <- function(l, r) unname(mapply(two_col, l, r, MoreArgs = list(at = 64L)))
  lines <- c(rows(vapply(1:4, l_text, ""), vapply(1:4, r_text, "")), "", rows(code_l, code_r), "",
             rows(vapply(5:8, l_text, ""), vapply(5:8, r_text, "")))
  out <- read_page(lines)
  expect_identical(out[5:13], code_l)
  expect_false(any(grepl("lft.*rgt", out)))
  # Code blocks that start and end on different rows, prose beside them.
  pad <- function(s, w) formatC(s, width = -w)
  prose <- function(tag, n) sprintf("%s%02d words of the column go here in a line ok", tag, seq_len(n))
  cl <- c("x <- rnorm(100)   # draw", "y <- 2 * x + 1    # linear", "fit <- lm(y ~ x)  # fit",
          "coef(fit)         # show", "summary(fit)      # more", "plot(fit)         # look")
  cr <- c("NumericVector v(n);   // alloc", "for (int i = 0; i < n; i++)", "  v[i] = i * 2.0;     // fill",
          "double s = sum(v);    // total", "return wrap(s);       // back", "}")
  L <- c(prose("L", 6), "", cl, "", prose("L", 12))
  R <- c(prose("R", 9), "", cr, "", prose("R", 9))
  R <- c(R, rep("", length(L) - length(R)))
  out <- read_page(paste0(pad(L, 46), "    ", R))
  expect_false(any(grepl("(rnorm|lm\\(|coef|summary|plot|<-).*(Vector|for \\(|v\\[i\\]|R[0-9]{2})", out)))
})

# ---------------------------------------------------------------------------
# Author blocks (pdf-4)
# ---------------------------------------------------------------------------

test_that("authors and long affiliations set side by side above the columns span the page", {
  title <- paste0(strrep(" ", 40), "Effects of widgets on outcomes")
  names <- at_cols(c("Alice Smith", "Bao Lee"), c(31L, 81L))
  affil <- at_cols(c("Northfield University", "Eastbay Institute"), c(27L, 79L))
  out <- read_page(c(title, names, affil, "", "", body_rows(1:8)))
  expect_identical(gsub(" +", " ", out[2:3]),
                   c("Alice Smith Bao Lee", "Northfield University Eastbay Institute"))
  expect_identical(col_tag(out[-(1:3)]), c(col_tags("Left", 1:8), col_tags("Right", 1:8)))
  # An acmart block, with no blank row between it and the columns.
  country <- at_cols(c("UK", "UK"), c(36L, 86L))
  first <- two_col("Abstract", "weak morning. The weak control often improved within the")
  out <- read_page(c(title, names, affil, country, first, body_rows(1:8)))
  expect_identical(gsub(" +", " ", out[2:4]),
                   c("Alice Smith Bao Lee", "Northfield University Eastbay Institute", "UK UK"))
  expect_identical(out[5], "Abstract")
  # One author's name, set right of the gutter above the abstract.
  lines <- c(title, "", paste0(strrep(" ", 52), "Maria Gonzalez"), "", "Abstract",
             "Etiam euismod. Fusce facilisis lacinia dui. Suspendisse potenti.", "", "",
             body_rows(1:8))
  out <- read_page(lines)
  expect_identical(out[2:3], c("Maria Gonzalez", "Abstract"))
})

# ---------------------------------------------------------------------------
# A right line pulled left of the gutter beside a heading (pdf-5)
# ---------------------------------------------------------------------------

test_that("a row whose right line starts left of the gutter after a run of spaces is split there", {
  # The left column is ragged, so the gutter is found right of where its
  # lines end; the right column's last lines start three characters left of
  # where its lines usually do, beside a heading of the left column.
  lv <- function(i) sprintf("Left %02d: the spacing effect was studied in adults%s", i,
                            if (i %% 2) " so" else "")
  rows <- function(i) vapply(i, function(k) two_col(lv(k), r_text(k), at = 56L), character(1))
  lines <- c(rows(1:8),
             two_col("2    Background", "The strong effect steadily moved without the late", at = 53L),
             paste0(strrep(" ", 52), "policy while the group shifted. The weak demand"),
             rows(9:10))
  out <- read_page(lines)
  at <- match("The strong effect steadily moved without the late", out)
  expect_false(is.na(at))
  expect_identical(out[at + 1L], "policy while the group shifted. The weak demand")
  expect_identical(out[9], "2    Background")
  expect_identical(col_tag(out[-c(9, at, at + 1L)]),
                   c(col_tags("Left", 1:10), col_tags("Right", 1:10)))
})

# ---------------------------------------------------------------------------
# Running lines: continued tables, notes, heads and feet (ingest-2, ingest-03,
# pdf-3, ingest-6, pdf-6, pdf-7, r3-...-04)
# ---------------------------------------------------------------------------

rl_body <- function(p) sprintf("Body sentence %d on page %d talks about outcomes at length here.",
                               1:6, p)

test_that("no row of a short continued table is dropped because one figure follows the page", {
  # These seeds give one-decimal cells that go up by one from page to page
  # (the standard errors 6.3, 6.4, 6.5 of seed 1's last rows).
  tab <- function(n, caption = TRUE, pn = FALSE) lapply(seq_len(n), function(p) {
    c(if (caption) sprintf("Table B1 (page %d of %d). Annual incidence of disease X", p, n),
      if (caption) "Year    Rate    SE     Cases",
      sprintf("%d    %4.1f    %3.1f    %d", 1960 + (p - 1) * 12 + 1:12, stats::runif(12, 10, 60),
              stats::runif(12, 1, 9), sample(1000:9999, 12)),
      if (pn) c("", as.character(p + 40)))
  })
  for (cfg in list(c(3, 1), c(3, 2), c(5, 2), c(5, 6))) {
    for (caption in c(TRUE, FALSE)) {
      set.seed(cfg[2])
      pages <- tab(cfg[1], caption, pn = caption)
      out <- unlist(readgpt:::drop_running_lines(pages))
      rows <- grep("^(19|20)[0-9]{2} ", unlist(pages), value = TRUE)
      expect_true(all(rows %in% out))
      expect_false(any(as.character(41:45) %in% out))
    }
  }
  # A monthly table with one year a page: the year follows the page, the
  # figures do not.
  set.seed(3)
  mon <- lapply(1:5, function(p) c("Year  Month   Rate    SE",
                                   sprintf("%d   %s    %.1f   %.1f", 2014 + p, month.abb,
                                           stats::runif(12, 10, 60), stats::runif(12, 1, 9))))
  expect_identical(readgpt:::drop_running_lines(mon), mon)
  # Feet numbered by section still go.
  sec <- lapply(1:9, function(p) c(rl_body(p), "", sprintf("%d-%d", (p - 1) %/% 3 + 1, (p - 1) %% 3 + 1)))
  expect_identical(readgpt:::drop_running_lines(sec), lapply(1:9, function(p) c(rl_body(p), "")))
})

test_that("a continued numeric table keeps every row through gr_ingest in three pages", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  set.seed(1)
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 8.5, height = 11, family = "Courier")
  years <- 1961:1996
  for (p in 1:3) {
    graphics::plot.new()
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.window(c(0, 1), c(0, 1))
    y <- 0.95
    put <- function(s) {
      graphics::text(0.05, y, s, adj = c(0, 0.5), cex = 0.9)
      y <<- y - 0.035
    }
    put(sprintf("Table B1 (page %d of 3). Annual incidence of disease X", p))
    put("Year    Rate    SE     Cases")
    for (yr in years[(p - 1) * 12 + 1:12]) {
      put(sprintf("%d    %4.1f    %3.1f    %d", yr, stats::runif(1, 10, 60),
                  stats::runif(1, 1, 9), sample(1000:9999, 1)))
    }
  }
  invisible(grDevices::dev.off())
  doc <- suppressMessages(gr_ingest(f, cache = FALSE))
  found <- vapply(years, function(y) grepl(sprintf("\\b%d\\s+[0-9]+\\.[0-9]", y), doc$text),
                  logical(1))
  expect_true(all(found))
})

test_that("notes under the tables or charts of a short paper are not taken for running feet", {
  tab <- function(k) c(sprintf("Table %d: Household income by region", k), "Region        2023      2024",
                       sprintf("North        41.%d      43.2", k), "South        38.4      39.9")
  chart <- function(k) c(sprintf("Figure %d: Household income by region", k), "   [chart]")
  notes <- c("Source: Authors' calculations from the 2024 Household Survey",
             "Notes: Standard errors in parentheses (clustered by school).",
             "Source: Office for National Statistics (ONS).",
             "Figures are percentages of all households (%).",
             "Source: World Bank, World Development Indicators",
             "Notes: * p<0.1, ** p<0.05, *** p<0.01")
  for (under in list(tab, chart)) for (note in notes) for (n in 3:5) {
    on <- switch(as.character(n), "3" = 2:3, "4" = c(2, 4), "5" = c(3, 5))
    pages <- lapply(1:n, function(p) c(if (p == 1) c("A Short Brief on Incomes", ""), rl_body(p),
                                       if (p %in% on) c("", under(p), note)))
    expect_identical(sum(unlist(readgpt:::drop_running_lines(pages)) == note), 2L)
  }
  # The heads of a short paper still go, a question and a citation among them.
  alt <- lapply(1:6, function(p) c(
    if (p %in% c(2, 4, 6)) "Smith et al.",
    if (p %in% c(3, 5)) "Does a widget intervention improve adherence?", rl_body(p)))
  expect_identical(readgpt:::drop_running_lines(alt), lapply(1:6, rl_body))
  cite <- lapply(1:5, function(p) c(if (p %in% c(3, 5)) "Evid Synth 2026; 14: 101-118", rl_body(p)))
  expect_identical(readgpt:::drop_running_lines(cite), lapply(1:5, rl_body))
})

test_that("a journal's citation foot is dropped from a short paper, a row of figures is kept", {
  foot <- "MNRAS 000, 1-3 (0000)"
  pages <- lapply(1:3, function(p) c(rl_body(p), if (p > 1) c("", foot)))
  expect_false(foot %in% unlist(readgpt:::drop_running_lines(pages)))
  row <- "Observations    1,204      1,204"
  pages <- lapply(1:5, function(p) c(rl_body(p), if (p %in% c(2, 4)) row))
  expect_identical(sum(unlist(readgpt:::drop_running_lines(pages)) == row), 2L)
})

test_that("a running head as long as a continued table is wide is not kept as its header", {
  set.seed(2)
  mk <- function(head) lapply(1:4, function(p) c(head, "", "",
    sprintf("                         %d   %4.1f   %d", 1880 + (p - 1) * 10 + 1:10,
            stats::runif(10, 10, 60), sample(1000:9999, 10))))
  out <- readgpt:::drop_running_lines(mk("Statistical annex 2025"))
  expect_false("Statistical annex 2025" %in% unlist(out))
  # A header that stands over its rows is kept, as the first line of a page
  # or below a caption, and set on single spaces too.
  set.seed(2)
  hdr <- lapply(1:5, function(p) c("Region            2019     2020     2021",
    sprintf("Region %-3d        %d      %d      %d", (p - 1) * 10 + 1:10, sample(100:999, 10),
            sample(100:999, 10), sample(100:999, 10))))
  expect_identical(readgpt:::drop_running_lines(hdr), hdr)
  tight <- lapply(1:5, function(p) c("Year Rate SE Cases",
    sprintf("%d  %.1f  %.1f  %d", 1960 + (p - 1) * 12 + 1:12, stats::runif(12, 10, 60),
            stats::runif(12, 1, 9), sample(1000:9999, 12)), "", as.character(p)))
  out <- readgpt:::drop_running_lines(tight)
  expect_identical(sum(unlist(out) == "Year Rate SE Cases"), 5L)
})

# ---------------------------------------------------------------------------
# One-sided gutter (ingest-7)
# ---------------------------------------------------------------------------

test_that("a two-cell table whose descriptions wrap is not read as two columns", {
  prose <- c("The trial recorded a small set of variables for every participant at each visit. Each value was checked",
             "against the source forms by a monitor, and every query was resolved before the database was locked.",
             "The data dictionary below lists the variables that the analysis used, with the rule for each. The",
             "analysis population was defined before unblinding, and the statistical analysis plan was signed off",
             "by the steering committee at its second meeting. Participants who withdrew consent had their data",
             "removed from every table. Sites were visited by a monitor at least twice a year, and more often")
  item <- function(label, desc) c(sprintf("             %-32s%s", label, desc[1]),
                                  paste0(strrep(" ", 45), desc[-1]))
  desc <- c("Unique identifier assigned at the screening", "visit; never reused across sites or study",
            "waves in the data, and kept for the audit", "trail at every site.")
  table <- c(sprintf("             %-32s%s", "Variable", "Description"),
             unlist(lapply(c("participant id (integer)", "randomised arm (factor)",
                             "baseline sbp (numeric)", "followup sbp (numeric)",
                             "adverse events (count)", "adherence pct (numeric)"), item, desc = desc)))
  lines <- c("1    Data", prose, "", table, "", prose, prose)
  expect_true(is.na(readgpt:::column_gutter(lines)))
})
