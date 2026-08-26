#!/usr/bin/env bash
#
# run-tests.sh -- install readgpt and run its test suite.
#
# Works from the package directory or from the repository root (it finds
# DESCRIPTION either here or in ./readgpt). Nothing it does touches the network
# except installing missing R packages, and it asks before doing that.
#
#   bash run-tests.sh              # install deps if needed, install pkg, run tests
#   bash run-tests.sh --check      # full R CMD check instead (slower, stricter)
#   bash run-tests.sh --no-install # skip dependency installation
#   bash run-tests.sh --yes        # install missing deps without asking
#
set -euo pipefail

MODE=test; ASK=1; DO_INSTALL=1
for a in "$@"; do
  case "$a" in
    --check)      MODE=check ;;
    --no-install) DO_INSTALL=0 ;;
    --yes|-y)     ASK=0 ;;
    -h|--help)    sed -n '2,14p' "$0"; exit 0 ;;
    *) echo "Unknown option: $a" >&2; exit 2 ;;
  esac
done

# --- locate the package ----------------------------------------------------
if   [ -f DESCRIPTION ];         then PKG="$PWD"
elif [ -f readgpt/DESCRIPTION ]; then PKG="$PWD/readgpt"
else
  echo "ERROR: no DESCRIPTION here or in ./readgpt." >&2
  echo "       Run this from the package directory or the repository root." >&2
  exit 1
fi
echo "Package: $PKG"

# --- R present? ------------------------------------------------------------
if ! command -v Rscript >/dev/null 2>&1; then
  cat >&2 <<'EOF'
ERROR: Rscript not found.

Install R first, then re-run this script:
  - Download from https://cran.r-project.org/bin/macosx/  (recommended), or
  - brew install --cask r

If R is installed but not on your PATH, it usually lives at
  /Library/Frameworks/R.framework/Resources/bin/Rscript
EOF
  exit 1
fi
echo "R:       $(Rscript -e 'cat(R.version.string)')"

# R >= 4.1 is required (the package uses the native |> era baseline).
Rscript -e 'if (getRversion() < "4.1.0") { cat("ERROR: readgpt needs R >= 4.1.0; you have", as.character(getRversion()), "\n"); quit(status = 1) }'

# --- a writable library ----------------------------------------------------
# On macOS the system library often is not writable, which is the single most
# common reason `R CMD INSTALL` fails here. Use a personal library instead.
LIB=$(Rscript -e '
  p <- Sys.getenv("R_LIBS_USER")
  p <- strsplit(p, .Platform$path.sep)[[1]][1]
  if (!nzchar(p) || is.na(p)) p <- file.path(path.expand("~"), "R", "readgpt-lib")
  dir.create(p, recursive = TRUE, showWarnings = FALSE)
  cat(normalizePath(p))')
echo "Library: $LIB"
export R_LIBS_USER="$LIB"

# --- dependencies ----------------------------------------------------------
# Only these four are needed to run the suite. Everything else in Suggests
# gates optional features (PDF, OCR, HTML, parallelism) that the tests skip.
REQUIRED='c("testthat", "withr", "jsonlite", "httr")'
MISSING=$(Rscript -e "cat(paste(setdiff($REQUIRED, rownames(installed.packages())), collapse=' '))")

if [ -n "$MISSING" ]; then
  echo
  echo "Missing R packages needed to run the tests: $MISSING"
  if [ "$DO_INSTALL" -eq 0 ]; then
    echo "ERROR: --no-install was given, so stopping here." >&2; exit 1
  fi
  if [ "$ASK" -eq 1 ]; then
    printf "Install them into %s? [y/N] " "$LIB"
    read -r reply
    case "$reply" in [Yy]*) ;; *) echo "Aborted."; exit 1 ;; esac
  fi
  Rscript -e "install.packages(strsplit('$MISSING', ' ')[[1]],
                lib = '$LIB', repos = 'https://cloud.r-project.org')"
  STILL=$(Rscript -e "cat(paste(setdiff($REQUIRED, rownames(installed.packages())), collapse=' '))")
  if [ -n "$STILL" ]; then
    echo "ERROR: still missing after install: $STILL" >&2
    echo "       Install them by hand, then re-run with --no-install." >&2
    exit 1
  fi
fi
echo "Deps:    ok"

# --- install the package ---------------------------------------------------
echo
echo "Installing readgpt..."
R CMD INSTALL "$PKG" -l "$LIB" --no-byte-compile >/tmp/readgpt-install.log 2>&1 || {
  echo "ERROR: install failed. Last 25 lines of /tmp/readgpt-install.log:" >&2
  tail -25 /tmp/readgpt-install.log >&2
  exit 1
}
echo "Installed."

# --- run ------------------------------------------------------------------
echo
if [ "$MODE" = "check" ]; then
  echo "Building and checking (this takes a couple of minutes)..."
  TMP=$(mktemp -d)
  trap 'rm -rf "$TMP"' EXIT
  # Check the built tarball, not the source directory: that is what CI does,
  # and it is the only way .Rbuildignore is honoured.
  ( cd "$TMP" && R CMD build "$PKG" >/dev/null )
  TARBALL=$(ls "$TMP"/*.tar.gz | head -1)
  echo "Built: $(basename "$TARBALL")"
  ( cd "$TMP" && _R_CHECK_FORCE_SUGGESTS_=false R CMD check "$TARBALL" --no-manual --no-build-vignettes )
  exit $?
fi

echo "Running the test suite..."
echo
Rscript -e "
  .libPaths(c('$LIB', .libPaths()))
  suppressPackageStartupMessages({ library(readgpt); library(testthat) })
  gr_options(verbose = FALSE)
  res <- as.data.frame(testthat::test_dir(file.path('$PKG', 'tests', 'testthat'),
                                          package = 'readgpt',
                                          reporter = 'progress',
                                          stop_on_failure = FALSE))
  cat('\n----------------------------------------\n')
  cat(sprintf('PASS %d   FAIL %d   ERROR %d   SKIP %d\n',
              sum(res\$passed), sum(res\$failed), sum(res\$error), sum(res\$skipped)))
  bad <- res[res\$failed > 0 | res\$error > 0, ]
  if (nrow(bad)) {
    cat('\nFailing tests:\n'); print(bad[, c('file', 'test', 'failed', 'error')], row.names = FALSE)
    quit(status = 1)
  }
  cat('\nAll good.\n')
"
