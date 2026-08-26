#!/usr/bin/env Rscript

library(testthat)
library(stringr)

test_that("packages.R sources without error", {
  expect_silent(suppressWarnings(source(file.path("..", "R", "packages.R"))))
})

# Define the required packages as they are in packages.R
required_packages <- c(
  "shiny", "shinyFiles", "shinyjs",
  "future", "future.apply", "httr",
  "stringr", "pdftools", "magick",
  "tesseract", "readtext"
)

test_that("All required packages are installed", {
  for (pkg in required_packages) {
    expect_true(requireNamespace(pkg, quietly = TRUE),
                info = paste("Package", pkg, "should be installed."))
  }
})

test_that("All required packages are loaded", {
  for (pkg in required_packages) {
    # When a package is attached, it appears in the search path as "package:pkg"
    pkg_attached <- any(grepl(paste0("package:", pkg), search(), fixed = TRUE))
    expect_true(pkg_attached,
                info = paste("Package", pkg, "should be loaded."))
  }
})
