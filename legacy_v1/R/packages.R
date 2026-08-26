# packages.R

# Define required packages
required_packages <- c(
  "shiny", "shinyFiles", "shinyjs",
  "future", "future.apply", "httr",
  "stringr", "pdftools", "magick",
  "tesseract", "readtext"
)

# Install any packages that are not already installed
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) {
  install.packages(new_packages)
}

# Load packages silently
invisible(lapply(required_packages, function(pkg) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}))
