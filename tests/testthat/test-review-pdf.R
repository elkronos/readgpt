# test-review-pdf.R
#
# Regression tests for defects found in review of the PDF reading-order code
# (R/ingest-pdf.R): running-line removal that ate table rows and missed common
# running heads, and a two-column reader that cut full-width tables at the
# gutter, spliced rows where the columns sit one space apart, and found no
# gutter when the columns' rows do not line up. The page text below is the
# kind pdftools::pdf_text() returns; the excerpts from LaTeX papers were taken
# from pdftotext's output for them.

reordered <- function(lines) {
  g <- readgpt:::column_gutter(lines)
  expect_false(is.na(g))
  out <- trimws(readgpt:::reorder_columns(lines, g))
  out[nzchar(out)]
}

# A two-column page in pdftotext's shape: justified left lines of about 55
# characters, the right column starting at position 58.
col_line <- function(left, right, at = 58L) {
  if (!nzchar(right)) return(left)
  paste0(left, strrep(" ", max(2L, at - 1L - nchar(left))), right)
}
left_text <- function(i) sprintf("Left %02d: the spacing effect was studied in adults, and", i)
right_text <- function(i) sprintf("Right %02d: retention fell when practice was massed in", i)
# Which column line each output line is ("Left 03", "Right 11"), or the line.
tag <- function(x) sub("^((Left|Right) [0-9]+):.*", "\\1", x)
tags <- function(side, i) sprintf("%s %02d", side, i)

# ---------------------------------------------------------------------------
# Running heads and feet (ingest-03, r3-real-pdf-two-column-layout-04)
# ---------------------------------------------------------------------------

# The body of page p: lines of their own, none of which reads as a running
# line (the page number sits inside a sentence, not at an end).
body <- function(p) c(
  sprintf("Body text of page %d, which differs on every page of the paper.", p),
  paste("The", c("first", "second", "third", "fourth", "fifth", "sixth")[p],
        "page goes on in words of its own."),
  sprintf("A closing line that names page %d in passing, mid-sentence.", p))

test_that("rows of a table continued across pages are not taken for running lines", {
  set.seed(3)
  years <- 1961:2020
  pages <- lapply(1:5, function(p) {
    rows <- sprintf("%d    %4.1f    %3.1f    %d", years[(p - 1) * 12 + 1:12],
                    stats::runif(12, 10, 60), stats::runif(12, 1, 9), sample(1000:9999, 12))
    c(sprintf("Table B1 (continued). Annual incidence of disease X, part %s", letters[p]), "",
      "Year    Rate    SE     Cases", rows, "", sprintf("Page %d of 5", p))
  })
  out <- unlist(readgpt:::drop_running_lines(pages))
  # Every data row is kept, the first and last of each page included...
  expect_identical(sum(grepl("^(19|20)[0-9]{2} ", out)), 60L)
  for (y in c(1972, 1983, 1984, 1995, 2020)) expect_true(any(startsWith(out, as.character(y))))
  # ...while the page foot, whose number goes up with the page, still goes.
  expect_false(any(grepl("^Page [0-9] of 5$", out)))

  # A row repeated at the foot of every page with changing figures is content
  # too, however short.
  totals <- lapply(1:5, function(p) c(body(p), sprintf("Total %d %d", sample(100:999, 1),
                                                       sample(100:999, 1))))
  expect_identical(readgpt:::drop_running_lines(totals), totals)
})

test_that("a running head is dropped when its page number is set at one end of a long line", {
  heads <- lapply(1:5, function(p) c(
    paste0("IEEE TRANSACTIONS ON EVIDENCE SYNTHESIS, VOL. 14, NO. 8, AUGUST 2026",
           strrep(" ", 40), p), body(p)))
  expect_identical(readgpt:::drop_running_lines(heads), lapply(1:5, body))

  # The same with the number first, as on the even pages of a two-sided paper.
  springer <- lapply(1:6, function(p) c(
    if (p %% 2 == 0) paste0(p, strrep(" ", 50), "Journal of Evidence Synthesis 14 (2026) 101-118"),
    body(p)))
  expect_identical(readgpt:::drop_running_lines(springer), lapply(1:6, body))

  # A long line whose changing number sits inside it is still content.
  inside <- lapply(1:5, function(p) c(
    sprintf("In cluster %d the investigators documented the outcome of the trial.", p + 40),
    body(p)))
  expect_identical(readgpt:::drop_running_lines(inside), inside)
})

test_that("alternating heads, and heads after a title page, are dropped from a short paper", {
  # Five pages: no head on the title page, one head on even pages and another
  # on odd ones. Neither reaches three pages.
  alt <- lapply(1:5, function(p) c(
    if (p == 2 || p == 4) "Alice Smith, Bao Lee, and Chidi Okafor",
    if (p == 3 || p == 5) "Effects of widgets: a randomised trial", body(p)))
  expect_identical(readgpt:::drop_running_lines(alt), lapply(1:5, body))

  # Three pages whose first is a title page: the head is on two pages.
  three <- lapply(1:3, function(p) c(
    if (p > 1) "Journal of Testing 12(3)                      Smith et al.", body(p)))
  expect_identical(readgpt:::drop_running_lines(three), lapply(1:3, body))

  # Two pages that fit no pattern (the second and third of five) are chance.
  chance <- lapply(1:5, function(p) c(
    if (p %in% 2:3) "A line that happens to open two pages", body(p)))
  expect_identical(readgpt:::drop_running_lines(chance), chance)
})

test_that("a running head left on a two-column page is not split into the body", {
  lines <- c(col_line("Journal of Testing 12(3)", paste0(strrep(" ", 40), "Smith et al.")), "",
             vapply(1:8, function(i) col_line(left_text(i), right_text(i)), character(1)))
  out <- reordered(lines)
  expect_match(out[1], "^Journal of Testing 12\\(3\\) +Smith et al\\.$")
  expect_identical(tag(out[-1]), c(tags("Left", 1:8), tags("Right", 1:8)))
})

test_that("a word hyphenated from the left column's foot to the right column's head is joined", {
  # Where a running head used to be glued into "Out-comes", the blank rows
  # under the left column's last line now part it from the right column's
  # first: the right column runs on for two rows, and the page ends in blanks.
  lines <- c(vapply(1:5, function(i) col_line(left_text(i), right_text(i)), character(1)),
             col_line("feedback to the multicomponent intervention team. Out-", right_text(6)),
             paste0(strrep(" ", 57), right_text(7:8)), "", "")
  lines[1] <- col_line(left_text(1), "comes for cluster 34 were analysed by intention to treat")
  g <- readgpt:::column_gutter(lines)
  out <- trimws(readgpt:::reorder_columns(lines, g))
  at <- match("feedback to the multicomponent intervention team. Out-", out)
  expect_identical(out[at + 1L], "comes for cluster 34 were analysed by intention to treat")
  b <- readgpt:::pdf_page_blocks(list(out), readgpt:::heading_matcher())
  expect_true(any(grepl("team. Out-\ncomes for cluster 34", b$text, fixed = TRUE)))
})

# ---------------------------------------------------------------------------
# Full-width tables (r3-real-pdf-two-column-layout-01)
# ---------------------------------------------------------------------------

test_that("a full-width table row with a blank at the gutter stays whole", {
  # pdftotext's page 3 of a twocolumn article with a table* on top. The gap
  # between the control and difference columns falls on the gutter.
  lines <- c(
    "                                           Table 1: Baseline characteristics",
    "                Characteristic          Treatment (n=241) Control (n=241)               Difference    P value",
    "                Age, years                     61.2                60.8                     0.4        0.71",
    "                Women, n (%)                 118 (49)            121 (50)                   -1         0.78",
    "                Systolic BP, mmHg             142.3                141.9                    0.4        0.66",
    "                Diabetes, n (%)              60 (25)              58 (24)                    1         0.83",
    "", "",
    "tors the. Sentence 4790: at investigators by twelve twelve         comes protocol protocol according intervention the pa-",
    "were either outcomes twelve usual were either with inves-          tients twelve for outcomes site. Sentence 9411: accord-",
    "tigators. Sentence 1001: were each for outcomes twelve             ing investigators twelve usual intervention to usual twelve",
    "the investigators were and site assigned investigators in-         either each and patients were according. Sentence 1846:",
    "vestigators to. Sentence 1385: usual care receive blinded          investigators and usual according protocol investigators",
    "intervention to outcomes according patients twelve the             at either receive to twelve site and site.")
  out <- gsub(" +", " ", reordered(lines))
  expect_identical(out[3:6], c("Age, years 61.2 60.8 0.4 0.71",
                               "Women, n (%) 118 (49) 121 (50) -1 0.78",
                               "Systolic BP, mmHg 142.3 141.9 0.4 0.66",
                               "Diabetes, n (%) 60 (25) 58 (24) 1 0.83"))
  # The body below is still read left column first.
  expect_identical(out[7], "tors the. Sentence 4790: at investigators by twelve twelve")
  expect_identical(out[13], "comes protocol protocol according intervention the pa-")

  # The same in a made-up page, with the table between two bands of columns.
  row <- function(cells) {
    at <- c(15L, 30L, 44L, 64L, 76L)
    out <- strrep(" ", 90)
    for (k in seq_along(cells)) substr(out, at[k], at[k] + nchar(cells[k]) - 1L) <- cells[k]
    sub(" +$", "", out)
  }
  page <- c(vapply(1:4, function(i) col_line(left_text(i), right_text(i)), character(1)), "",
            row(c("Characteristic", "Treatment", "Control", "Difference", "P value")),
            row(c("Age, years", "61.2", "60.8", "0.4", "0.71")),
            row(c("Women, n (%)", "118 (49)", "121 (50)", "-1", "0.78")), "",
            vapply(5:8, function(i) col_line(left_text(i), right_text(i)), character(1)))
  out <- gsub(" +", " ", reordered(page))
  expect_identical(tag(out), c(tags("Left", 1:4), tags("Right", 1:4),
                               "Characteristic Treatment Control Difference P value",
                               "Age, years 61.2 60.8 0.4 0.71",
                               "Women, n (%) 118 (49) 121 (50) -1 0.78",
                               tags("Left", 5:8), tags("Right", 5:8)))
})

test_that("a table inside one column is still read with its column", {
  cell <- function(i) sprintf("      Measure %d        %.1f      %.1f", i, 20 + i, 70 - i)
  lines <- c(vapply(1:3, function(i) col_line(left_text(i), right_text(i)), character(1)),
             vapply(1:5, function(i) col_line(cell(i), right_text(i + 3L)), character(1)),
             vapply(4:6, function(i) col_line(left_text(i), right_text(i + 5L)), character(1)))
  out <- gsub(" +", " ", reordered(lines))
  expect_identical(out[4:8], sprintf("Measure %d %.1f %.1f", 1:5, 20 + 1:5, 70 - 1:5))
  expect_identical(tag(out[12:22]), tags("Right", 1:11))
})

# ---------------------------------------------------------------------------
# One space between the columns (r3-real-pdf-two-column-layout-02)
# ---------------------------------------------------------------------------

test_that("rows where pdftotext left one space between the columns are split, not spliced", {
  # Page 2 of an elsarticle [5p] paper. Rows 5, 13 and 14 hold a left and a
  # right line one space apart: after a hyphen-broken word (the right line
  # pulled left of its column) or with the right line in its place.
  lines <- c(
    "recruitment centres.                                                  dent verification of electronic health records. In cluster 52 the",
    "     In cluster 31 the investigators compared clinically mean-        steering committee examined considerable variability in phar-",
    "ingful improvements in functional capacity throughout the pre-        macokinetic parameters following independent verification of",
    "specified observation window. In cluster 32 the statistical team      electronic health records. In cluster 53 participating clinicians",
    "characterised comprehensive documentation of concomitant med- reported methodologically rigorous randomisation procedures",
    "ications throughout the prespecified observation window. In           following independent verification of electronic health records.",
    "cluster 33 outcome adjudicators evaluated longitudinal deterio-       In cluster 54 research nurses characterised substantially hetero-",
    "ration in renal function notwithstanding differential attrition in    geneous cardiovascular responses within the predefined nonin-",
    "the comparator arm. In cluster 34 the steering committee quan-        feriority margin of the primary analysis.",
    "tified unexpectedly persistent inflammatory biomarkers through-",
    "out the prespecified observation window. In cluster 35 the steer-     2.3. Outcomes",
    "ing committee reported methodologically rigorous randomisa-               In cluster 55 research nurses tabulated comprehensive doc-",
    "tion procedures across geographically dispersed recruitment cen- umentation of concomitant medications among participants al-",
    "tres. In cluster 36 research nurses evaluated consistently favourable located to the multicomponent intervention. In cluster 56 the",
    "adherence trajectories within the predefined noninferiority mar-      data monitoring board reviewed clinically meaningful improve-",
    "gin of the primary analysis.                                          ments in functional capacity after adjustment for socioeconomic",
    "                                                                     and demographic characteristics. In cluster 57 the statistical",
    "2.2. Randomisation and masking                                       team reviewed methodologically rigorous randomisation pro-")
  out <- reordered(lines)
  at <- function(x) match(x, out)
  # The left column's heading is read before the right column's.
  expect_lt(at("2.2. Randomisation and masking"), at("2.3. Outcomes"))
  # Each row is cut where its right line starts.
  expect_false(is.na(at("characterised comprehensive documentation of concomitant med-")))
  above <- at("electronic health records. In cluster 53 participating clinicians")
  expect_identical(out[above + 1L],
                   "reported methodologically rigorous randomisation procedures")
  expect_identical(out[at("tion procedures across geographically dispersed recruitment cen-") + 1L],
                   "tres. In cluster 36 research nurses evaluated consistently favourable")
  expect_identical(out[at("In cluster 55 research nurses tabulated comprehensive doc-") + 1:2],
                   c("umentation of concomitant medications among participants al-",
                     "located to the multicomponent intervention. In cluster 56 the"))
  # The left column is one run of lines and the right one another.
  expect_identical(at("2.2. Randomisation and masking") + 1L,
                   at("dent verification of electronic health records. In cluster 52 the"))
})

test_that("a made-up row with one space before the right column is split there", {
  lines <- vapply(1:8, function(i) col_line(left_text(i), right_text(i)), character(1))
  # The left line runs to one space short of the right column.
  lines[4] <- paste0(substr(paste0(left_text(4), " as well as the"), 1L, 56L), " ", right_text(4))
  out <- reordered(lines)
  expect_identical(tag(out), c(tags("Left", 1:8), tags("Right", 1:8)))
  # A hyphen-broken word before a right line pulled left of its column.
  lines[4] <- paste0("Left 04: the spacing effect was studied in adu- ", right_text(4))
  out <- reordered(lines)
  expect_identical(tag(out), c(tags("Left", 1:8), tags("Right", 1:8)))
  expect_identical(out[4], "Left 04: the spacing effect was studied in adu-")
  # A right line pulled left of its column after a full left line.
  lines[4] <- paste0("Left 04: the spacing effect was studied in adults so ", right_text(4))
  out <- reordered(lines)
  expect_identical(tag(out), c(tags("Left", 1:8), tags("Right", 1:8)))
  expect_identical(out[4], "Left 04: the spacing effect was studied in adults so")
})

# ---------------------------------------------------------------------------
# Columns whose rows do not line up (r3-real-pdf-two-column-layout-03)
# ---------------------------------------------------------------------------

test_that("a gutter is found when the columns' rows do not line up", {
  # Each row holds one column only, as pdftotext sets columns whose baselines
  # are offset.
  inter <- as.vector(rbind(vapply(1:10, left_text, character(1)),
                           paste0(strrep(" ", 57), vapply(1:10, right_text, character(1)))))
  out <- reordered(inter)
  expect_identical(tag(out), c(tags("Left", 1:10), tags("Right", 1:10)))

  # The last page of the elsarticle paper: a few rows share both columns, one
  # of them one space apart, and the rest alternate.
  lines <- c(
    "tres. In cluster 143 the steering committee summarised method-      5. Conclusion",
    "ologically rigorous randomisation procedures across geograph-",
    "ically dispersed recruitment centres. In cluster 144 research            In cluster 163 the investigators reviewed longitudinal dete-",
    "nurses reviewed longitudinal deterioration in renal function within rioration in renal function across geographically dispersed re-",
    "the predefined noninferiority margin of the primary analysis.       cruitment centres. In cluster 164 the statistical team quantified",
    "                                                                    moderately increased gastrointestinal intolerance within the pre-",
    "4.1. Limitations                                                    defined noninferiority margin of the primary analysis. In cluster",
    "                                                                    165 outcome adjudicators reported methodologically rigorous",
    "     In cluster 145 the investigators reported methodologically",
    "                                                                    randomisation procedures following independent verification of",
    "rigorous randomisation procedures following independent ver-",
    "                                                                    electronic health records. In cluster 166 the data monitoring",
    "ification of electronic health records. In cluster 146 the data",
    "                                                                    board examined methodologically rigorous randomisation pro-",
    "monitoring board characterised longitudinal deterioration in re-",
    "                                                                    cedures notwithstanding differential attrition in the comparator")
  out <- reordered(lines)
  at <- function(x) match(x, out)
  expect_lt(at("4.1. Limitations"), at("5. Conclusion"))
  expect_identical(out[at("In cluster 163 the investigators reviewed longitudinal dete-") + 1L],
                   "rioration in renal function across geographically dispersed re-")
  # Lines of one column that alternated with the other's are one paragraph
  # again, so a word broken across them can be rejoined.
  raw <- trimws(readgpt:::reorder_columns(lines, readgpt:::column_gutter(lines)))
  from <- match("In cluster 145 the investigators reported methodologically", raw)
  expect_identical(raw[from + 1:3],
                   c("rigorous randomisation procedures following independent ver-",
                     "ification of electronic health records. In cluster 146 the data",
                     "monitoring board characterised longitudinal deterioration in re-"))
  b <- readgpt:::pdf_page_blocks(list(raw), readgpt:::heading_matcher())
  expect_identical(b$text[b$kind == "heading"], c("4.1. Limitations", "5. Conclusion"))
})

test_that("a gutter is found when the right column is only a few lines long", {
  lines <- c(vapply(1:4, function(i) col_line(left_text(i), right_text(i)), character(1)),
             vapply(5:20, left_text, character(1)))
  out <- reordered(lines)
  expect_identical(tag(out), c(tags("Left", 1:20), tags("Right", 1:4)))
  # A single-column page with a few indented lines is still not two columns.
  prose <- rep(paste("The cohort comprised 482 participants recruited across nine clinical",
                     "sites, and adherence was high."), 12)
  expect_true(is.na(readgpt:::column_gutter(c(prose, paste0(strrep(" ", 57), right_text(1:3))))))
})

# ---------------------------------------------------------------------------
# Real PDFs
# ---------------------------------------------------------------------------

test_that("a continued numeric table keeps every row through gr_ingest", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  set.seed(1)
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 8.5, height = 11, family = "Courier")
  years <- 1961:2020
  for (p in 1:5) {
    graphics::plot.new()
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.window(c(0, 1), c(0, 1))
    y <- 0.95
    put <- function(s) {
      graphics::text(0.05, y, s, adj = c(0, 0.5), cex = 0.9)
      y <<- y - 0.035
    }
    put(sprintf("Table B1 (page %d of 5). Annual incidence of disease X", p))
    put("Year    Rate    SE     Cases")
    for (yr in years[(p - 1) * 12 + 1:12]) {
      put(sprintf("%d    %4.1f    %3.1f    %d", yr, stats::runif(1, 10, 60),
                  stats::runif(1, 1, 9), sample(1000:9999, 1)))
    }
    graphics::text(0.5, 0.03, as.character(p + 40), cex = 0.9)
  }
  invisible(grDevices::dev.off())
  doc <- suppressMessages(gr_ingest(f, cache = FALSE))
  found <- vapply(years, function(y) grepl(sprintf("\\b%d\\s+[0-9]+\\.[0-9]", y), doc$text),
                  logical(1))
  expect_true(all(found))
  expect_false(grepl("(^|\n)4[1-5](\n|$)", doc$text))
})

test_that("a two-column PDF whose columns' rows do not line up is read column by column", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 8.5, height = 11)
  for (pg in 1:3) {
    graphics::plot.new()
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.window(c(0, 1), c(0, 1))
    graphics::text(0.05, 0.9 - (0:9) * 0.03,
                   sprintf("Left %d.%d: the spacing effect was studied in adults.", pg, 0:9),
                   adj = 0, cex = 0.8)
    # The right column's baselines fall halfway between the left column's.
    graphics::text(0.55, 0.885 - (0:9) * 0.03,
                   sprintf("Right %d.%d: retention fell when practice was massed.", pg, 0:9),
                   adj = 0, cex = 0.8)
  }
  invisible(grDevices::dev.off())
  raw <- pdftools::pdf_text(f)[1]
  skip_if(grepl("Left 1.0[^\n]*Right 1.0", raw), "pdftotext put the two columns on shared rows")
  doc <- suppressMessages(gr_ingest(f, cache = FALSE))
  for (pg in 1:3) {
    left <- regexpr(sprintf("Left %d.9", pg), doc$text, fixed = TRUE)
    right <- regexpr(sprintf("Right %d.0", pg), doc$text, fixed = TRUE)
    expect_gt(left, 0L)
    expect_gt(right, left)
  }
})

test_that("a long running head with its page number is left out of a two-column PDF", {
  skip_if_not_installed("pdftools")
  local_clean_cache()
  f <- tempfile(fileext = ".pdf")
  grDevices::pdf(f, width = 8.5, height = 11)
  for (pg in 1:5) {
    graphics::plot.new()
    graphics::par(mar = c(0, 0, 0, 0))
    graphics::plot.window(c(0, 1), c(0, 1))
    graphics::text(0.05, 0.97, adj = 0, cex = 0.7,
                   "IEEE TRANSACTIONS ON EVIDENCE SYNTHESIS, VOL. 14, NO. 8, AUGUST 2026")
    graphics::text(0.95, 0.97, as.character(pg), adj = 1, cex = 0.7)
    graphics::text(0.05, 0.9 - (0:7) * 0.02,
                   sprintf("Left %d.%d: the spacing effect was studied in adults.", pg, 1:8),
                   adj = 0, cex = 0.8)
    graphics::text(0.55, 0.9 - (0:7) * 0.02,
                   sprintf("Right %d.%d: retention fell when practice was massed.", pg, 1:8),
                   adj = 0, cex = 0.8)
  }
  invisible(grDevices::dev.off())
  doc <- suppressMessages(gr_ingest(f, cache = FALSE))
  expect_false(grepl("IEEE TRANSACTIONS", doc$text, fixed = TRUE))
  expect_false(any(grepl("^[0-9]+$", trimws(doc$blocks$text))))
  for (pg in 1:5) {
    expect_gt(regexpr(sprintf("Right %d.1", pg), doc$text, fixed = TRUE),
              regexpr(sprintf("Left %d.8", pg), doc$text, fixed = TRUE))
  }
})
