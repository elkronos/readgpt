# test-calibrate.R -- how often the screener is wrong, measured.
#
# The statistics are checked against hand-computed values, because a plausible
# wrong number here is worse than no number: it is the figure that would go in a
# methods section and license trusting everything downstream of it.
#
# The other half is refusing to answer questions the sample cannot answer. A
# sample drawn only from exclusions contains no kept records, so sensitivity and
# specificity computed on it are artifacts of the frame -- 0% and 100% -- and
# reporting them would be the most damaging kind of wrong, because both look
# like findings.

screening_of <- function(decisions, documents = NULL) {
  structure(list(table = data.frame(
    document = documents %||% paste0("d", seq_along(decisions), ".pdf"),
    decision = decisions, reason = "because", stringsAsFactors = FALSE)),
    class = "gr_screening")
}

test_that("the confusion matrix and its rates are what they should be", {
  # Worked by hand. Kept means include OR unclear -- a deferral is not a miss.
  #   d1 include/include  TP      d5 exclude/exclude  TN
  #   d2 include/exclude  FP      d6 exclude/include  FN
  #   d3 unclear/include  TP      d7 include/include  TP
  #   d4 exclude/exclude  TN      d8 exclude/exclude  TN
  scr <- screening_of(c("include", "include", "unclear", "exclude",
                        "exclude", "exclude", "include", "exclude"))
  ref <- data.frame(document = paste0("d", 1:8, ".pdf"),
                    human_decision = c("include", "exclude", "include", "exclude",
                                       "exclude", "include", "include", "exclude"),
                    stringsAsFactors = FALSE)
  cal <- gr_calibrate(scr, ref)

  expect_identical(unname(cal$counts[c("tp", "fn", "fp", "tn")]), c(3L, 1L, 1L, 3L))
  est <- function(m) cal$metrics$estimate[cal$metrics$metric == m]
  expect_equal(est("sensitivity (as deployed)"), 0.75)
  # d3 was kept but not actively included, so strict sensitivity is lower. The
  # gap between the two is the reading a person still has to do.
  expect_equal(est("sensitivity (strict include)"), 0.5)
  expect_equal(est("specificity"), 0.75)
  expect_equal(est("reading avoided"), 0.5)

  # Cohen's kappa, by hand: po = 5/8, pe = (3*4 + 1*0 + 4*4)/64 = 28/64,
  # kappa = (0.625 - 0.4375) / (1 - 0.4375) = 1/3.
  expect_equal(cal$kappa, 1/3, tolerance = 1e-8)

  # The misses themselves, which say more than the rate does.
  expect_equal(nrow(cal$missed), 1L)
  expect_identical(cal$missed$document, "d6.pdf")
  expect_equal(nrow(cal$disagreements), 2L)     # the miss, and the false include
})

test_that("the interval is Wilson, and stays sensible where the textbook one does not", {
  # At 5 of 5 the normal-approximation interval is [1, 1] -- certainty from five
  # observations. Wilson does not do that, which is the whole reason for it,
  # because screening proportions live at the ends of the scale.
  five <- readgpt:::prop_row("x", 5, 5)
  expect_equal(five$estimate, 1)
  expect_lt(five$lower, 1)
  expect_gt(five$lower, 0.5)
  expect_equal(five$upper, 1)

  none <- readgpt:::prop_row("x", 0, 5)
  expect_equal(none$estimate, 0)
  expect_equal(none$lower, 0)
  expect_gt(none$upper, 0)

  # Against a worked value: 3 of 4, z = 1.96 -> [0.3006, 0.9544].
  three <- readgpt:::prop_row("x", 3, 4)
  expect_equal(three$lower, 0.3006, tolerance = 1e-3)
  expect_equal(three$upper, 0.9544, tolerance = 1e-3)

  # A bigger sample says more, which is the property that makes the interval
  # worth printing at all.
  wide <- readgpt:::prop_row("x", 15, 20)
  narrow <- readgpt:::prop_row("x", 150, 200)
  expect_equal(wide$estimate, narrow$estimate)
  expect_lt(narrow$upper - narrow$lower, wide$upper - wide$lower)

  # And nothing at all is NA rather than a division by zero.
  expect_true(is.na(readgpt:::prop_row("x", 0, 0)$estimate))
})

test_that("kappa notices a screener that agrees by doing nothing", {
  # At a realistic inclusion rate, excluding everything is ~95% accurate. That
  # is why accuracy is not reported and kappa is.
  k <- readgpt:::cohen_kappa
  expect_equal(k(rep("exclude", 20), c(rep("exclude", 19), "include")), 0, tolerance = 1e-8)
  expect_equal(k(c("include", "exclude"), c("include", "exclude")), 1)
  # Perfect disagreement is negative, not zero.
  expect_lt(k(c("include", "exclude"), c("exclude", "include")), 0)
  # One rater using one label throughout leaves nothing to correct for.
  expect_true(is.na(k(rep("include", 5), rep("include", 5))))
})

test_that("a sample of exclusions reports what was lost, not a sensitivity", {
  # THE trap. Every row in this frame was excluded by the model, so sensitivity
  # computes to 0% and specificity to 100% -- both artifacts of where the sample
  # came from, both alarming or flattering, neither a fact about the screener.
  scr <- screening_of(c(rep("exclude", 85), rep("include", 10), rep("unclear", 5)))
  ref <- gr_reference(scr, n = 20, of = "excluded", seed = 1)
  ref$human_decision <- c(rep("exclude", 18), "include", "include")
  cal <- suppressWarnings(gr_calibrate(scr, ref))

  expect_setequal(cal$metrics$metric, c("eligible among the excluded", "correctly excluded"))
  expect_false(any(grepl("sensitivity", cal$metrics$metric)))
  expect_false(any(grepl("specificity", cal$metrics$metric)))
  expect_equal(cal$metrics$estimate[cal$metrics$metric == "eligible among the excluded"], 0.1)
  # Kappa needs both raters to use both labels; here the model said one thing
  # throughout, so it would be arithmetic on a constant.
  expect_true(is.na(cal$kappa))

  # The number somebody actually acts on: what the rate implies for the whole
  # discarded pile. 10% of 85.
  expect_equal(cal$projected$frame_n, 85L)
  expect_equal(cal$projected$lost, 8.5)
  expect_lt(cal$projected$lower, cal$projected$upper)
  expect_output(print(cal), "eligible studies lost")
  expect_output(print(cal), "sampled from exclusions only")
})

test_that("a sample of kept records reports what is worth keeping, not a sensitivity", {
  scr <- screening_of(c(rep("exclude", 20), rep("include", 8), rep("unclear", 2)))
  ref <- gr_reference(scr, n = Inf, of = "kept")
  expect_equal(nrow(ref), 10L)
  ref$human_decision <- c(rep("include", 6), rep("exclude", 4))
  cal <- gr_calibrate(scr, ref)

  expect_setequal(cal$metrics$metric, c("eligible among those kept", "deferred to a person"))
  expect_equal(cal$metrics$estimate[cal$metrics$metric == "eligible among those kept"], 0.6)
  expect_null(cal$projected)
  expect_output(print(cal), "sampled from kept records only")
})

test_that("the frame survives a CSV, because that is what gets emailed and read back", {
  # An attribute does not survive write.csv, Excel, and a fortnight. Without the
  # frame the misleading metrics come back: an exclusions sample would report
  # sensitivity 0% and specificity 100%.
  scr <- screening_of(c(rep("exclude", 50), rep("include", 8), rep("unclear", 2)))
  f <- withr::local_tempfile(fileext = ".csv")
  quiet(gr_reference(scr, n = 10, of = "excluded", seed = 1, path = f))

  d <- utils::read.csv(f, stringsAsFactors = FALSE)
  expect_true(all(c("document", "human_decision", "sampled_from") %in% names(d)))
  # Blind by default: a person shown the model's answer agrees with it.
  expect_false("model_decision" %in% names(d))
  expect_true(all(is.na(d$human_decision) | !nzchar(d$human_decision)))

  d$human_decision <- c(rep("exclude", 9), "include")
  utils::write.csv(d, f, row.names = FALSE, na = "")
  cal <- suppressWarnings(gr_calibrate(scr, f))
  expect_identical(cal$frame$of, "excluded")
  expect_equal(cal$frame$frame_n, 50L)
  expect_false(any(grepl("sensitivity", cal$metrics$metric)))
})

test_that("the sample is reproducible, and blind can be turned off deliberately", {
  scr <- screening_of(c(rep("exclude", 40), rep("include", 10)))
  expect_identical(gr_reference(scr, n = 8, seed = 42)$document,
                   gr_reference(scr, n = 8, seed = 42)$document)
  expect_false(identical(gr_reference(scr, n = 8, seed = 42)$document,
                         gr_reference(scr, n = 8, seed = 7)$document))
  # Asking for more than exists takes the frame, rather than erroring.
  expect_equal(nrow(gr_reference(scr, n = 999, of = "kept")), 10L)
  expect_true("model_decision" %in% names(gr_reference(scr, n = 5, blind = FALSE, seed = 1)))
  expect_error(gr_reference(screening_of(rep("include", 3)), of = "excluded"),
               class = "gr_no_reference_frame")
})

test_that("an unfilled row is not an exclusion, and a bad one is not guessed at", {
  scr <- screening_of(c("include", "exclude", "exclude", "include"))
  ref <- data.frame(document = paste0("d", 1:4, ".pdf"),
                    human_decision = c("include", NA, "", "exclude"),
                    stringsAsFactors = FALSE)
  expect_warning(cal <- gr_calibrate(scr, ref), class = "gr_reference_incomplete")
  expect_equal(cal$n, 2L)          # the two that were actually judged

  bad <- data.frame(document = "d1.pdf", human_decision = "maybe", stringsAsFactors = FALSE)
  expect_error(gr_calibrate(scr, bad), class = "gr_bad_reference")

  # A reference naming documents this run does not contain is two different
  # runs, and comparing them would produce a number about nothing.
  other <- data.frame(document = "somewhere-else.pdf", human_decision = "include",
                      stringsAsFactors = FALSE)
  expect_error(gr_calibrate(scr, other), class = "gr_reference_mismatch")
  expect_error(gr_calibrate(scr, data.frame(document = "d1.pdf")), class = "gr_bad_reference")
})

test_that("a document that was never read is left out of the comparison", {
  # NA is the absence of a judgement, not an exclusion. Counting it as one would
  # charge the screener for a document it never saw.
  scr <- screening_of(c("include", NA, "exclude", "include"))
  ref <- data.frame(document = paste0("d", 1:4, ".pdf"),
                    human_decision = c("include", "include", "exclude", "include"),
                    stringsAsFactors = FALSE)
  cal <- gr_calibrate(scr, ref)
  expect_equal(cal$n, 3L)
  expect_equal(cal$n_positives, 2L)
  expect_equal(cal$metrics$estimate[cal$metrics$metric == "sensitivity (as deployed)"], 1)
})

test_that("too few eligible studies is said out loud rather than printed as a rate", {
  # "100%" from two observations is not a finding, and it is exactly the figure
  # somebody would quote.
  scr <- screening_of(c("include", "include", rep("exclude", 8)))
  ref <- data.frame(document = paste0("d", 1:10, ".pdf"),
                    human_decision = c("include", "include", rep("exclude", 8)),
                    stringsAsFactors = FALSE)
  cal <- gr_calibrate(scr, ref)
  expect_equal(cal$metrics$estimate[cal$metrics$metric == "sensitivity (as deployed)"], 1)
  expect_false(cal$adequate)
  expect_equal(cal$n_positives, 2L)
  expect_output(print(cal), "Hand-screen more")
  # The interval says the same thing in numbers.
  sens <- cal$metrics[cal$metrics$metric == "sensitivity (as deployed)", ]
  expect_lt(sens$lower, 0.5)

  # Enough eligible studies, and it stops complaining.
  big <- screening_of(c(rep("include", 12), rep("exclude", 20)))
  ref2 <- data.frame(document = paste0("d", 1:32, ".pdf"),
                     human_decision = c(rep("include", 12), rep("exclude", 20)),
                     stringsAsFactors = FALSE)
  expect_true(gr_calibrate(big, ref2)$adequate)
})

test_that("duplicates are not screened twice into the calibration", {
  tab <- data.frame(document = paste0("d", 1:4, ".pdf"),
                    decision = c("include", "include", "exclude", "exclude"),
                    duplicate_of = c(NA, "d1.pdf", NA, NA), stringsAsFactors = FALSE)
  scr <- structure(list(table = tab), class = "gr_screening")
  expect_equal(nrow(gr_reference(scr, n = Inf, of = "all")), 3L)
})
