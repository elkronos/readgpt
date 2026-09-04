# zzz.R -- package load hooks.
#
# WHY THIS FILE EXISTS
# `packages.R` ran `install.packages()` at source() time, every time, with no
# `repos`, no `dependencies`, and no check that it worked. On a machine without
# a CRAN mirror set it emitted only a warning and then `library()`d packages
# that were never installed, failing with a misleading "there is no package
# called X". It also omitted `jsonlite`, which five files actually needed. And
# `library()` inside a sourced script attaches packages to the user's search
# path whether they want them or not.
#
# A package declares its dependencies in DESCRIPTION and lets the user install
# them. Nothing here installs anything.

.onLoad <- function(libname, pkgname) {
  gr_state$seed_models <- stats::setNames(.gr_seed_models(),
                                          vapply(.gr_seed_models(), `[[`, character(1), "id"))
  gr_state$model_patterns <- .gr_seed_patterns()
  register_builtin_extractors()
  register_builtin_cleaners()
  register_builtin_segmenters()
  register_builtin_readers()
  register_builtin_embedders()
  invisible(NULL)
}

.onAttach <- function(libname, pkgname) {
  missing <- Filter(function(p) !requireNamespace(p, quietly = TRUE),
                    c("pdftools", "tesseract", "magick", "xml2"))
  if (length(missing)) {
    packageStartupMessage(sprintf(
      "readgpt: optional packages not installed: %s. PDF, OCR and HTML ingestion need them; everything else works without them.",
      paste(missing, collapse = ", ")))
  }
}

# Development helper: source every R file in dependency order, for working on a
# checkout without installing. Deliberately NOT exported -- a user who has the
# package installed should use `library(readgpt)`, and a developer working from
# a checkout can reach this as `readgpt:::gr_load_all()`. It exists because the
# v1 repository was a set of `source()`d scripts with hard-coded `../R/...`
# paths that resolved from exactly one working directory.
#' @noRd
gr_load_all <- function(dir = "R") {
  files <- list.files(dir, pattern = "\\.[Rr]$", full.names = TRUE)
  order_first <- c("utils-misc.R", "core-state.R", "core-tokenize.R", "core-models.R")
  files <- c(file.path(dir, order_first[file.exists(file.path(dir, order_first))]),
             setdiff(files, file.path(dir, order_first)))
  for (f in files) source(f, local = FALSE)
  .onLoad(NULL, "readgpt")
  invisible(files)
}
