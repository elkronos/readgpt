# core-progress.R: one line that shows how far a long read has got.
#
# A reader that sends one request per chunk can take several minutes on a long
# document, and all it printed was the stage it had started. Now, in an
# interactive session with `verbose` on, it keeps one line up to date with the
# count so far and what the run has spent, and removes the line when it is done.
# Scripts and tests are not interactive, and a document being knitted captures
# messages into its output, so none of them see anything new.
#
# The line is a message, so suppressMessages() hides it. Any other message, a
# warning, an error or an interrupt raised while it is showing clears it first,
# so R never prints on the same line.

#' Is anyone watching?
#' @noRd
progress_wanted <- function() {
  session_interactive() && isTRUE(gr_options("verbose")) &&
    !isTRUE(getOption("knitr.in.progress"))
}

#' interactive(), kept apart so the tests can say yes.
#' @noRd
session_interactive <- function() interactive()

#' A progress line over `n` items, named by `label`, or NULL when none should be
#' shown: one item, nobody watching, or a line already showing. Nothing is
#' drawn until with_progress() runs.
#' @noRd
progress_start <- function(n, label, trace = NULL) {
  if (n < 2L || isTRUE(gr_state$progress_on) || !progress_wanted()) return(NULL)
  p <- new.env(parent = emptyenv())
  p$n <- as.integer(n)
  p$label <- as_chr1(label, "item")
  p$trace <- if (inherits(trace, "gr_trace")) trace else NULL
  p$width <- 0L
  p$last <- -Inf
  # Steps already checked for a model with no price, and the verdict so far.
  p$seen <- 0L
  p$unpriced <- FALSE
  p$priced <- logical(0)
  p
}

#' Redraw after `k` items are done. At most four times a second, so a cached
#' rerun that finishes a request in a millisecond does not spend its time
#' printing, but always for the last item.
#' @noRd
progress_tick <- function(p, k) {
  if (is.null(p)) return(invisible(NULL))
  now <- proc.time()[["elapsed"]]
  if (k > 0L && k < p$n && now - p$last < 0.25) return(invisible(NULL))
  p$last <- now
  txt <- sprintf("  %s %d of %d%s", p$label, k, p$n, progress_cost(p))
  # A line wider than the console wraps, and "\r" returns only to the start of
  # the last row, so each redraw would leave a row behind.
  width <- as_int1(getOption("width"), 80L)
  if (nchar(txt) > width - 1L) txt <- substr(txt, 1L, max(width - 1L, 1L))
  progress_draw(p, txt)
}

#' What the run has spent, for the line. A request to a model with no registered
#' price adds nothing to `spent_usd`, so once one has been made the total is not
#' known and the line says so rather than show a figure that is too low.
#' @noRd
progress_cost <- function(p) {
  tr <- p$trace
  if (is.null(tr)) return("")
  n <- length(tr$steps)
  if (n > p$seen) {
    for (st in tr$steps[(p$seen + 1L):n]) {
      if (identical(st$kind, "local") || isTRUE(st$cached)) next
      m <- as_chr1(st$model, "unknown")
      known <- unname(p$priced[m])
      if (is.na(known)) {
        known <- !is.na(tryCatch(suppressWarnings(gr_estimate_cost(m, 1, 1)),
                                 error = function(e) NA_real_))
        p$priced[m] <- known
      }
      if (!known) p$unpriced <- TRUE
    }
    p$seen <- n
  }
  # A run made of parts (the documents of a corpus, the recipes of a
  # comparison) gives each part its own trace, holding what the run spent
  # before it in `spent_before`, so the line shows the run's total.
  before <- if (is.null(tr$spent_before)) 0 else as_num1(tr$spent_before, NA_real_)
  if (p$unpriced || is.na(before)) return(", cost unknown")
  sprintf(", $%.4f spent", before + as_num1(tr$spent_usd, 0))
}

#' Write the line over the previous one. The count and the spend only grow, so
#' a line is never shorter than the one it covers; the width is kept so the
#' line can be blanked.
#' @noRd
progress_draw <- function(p, txt) {
  p$width <- max(p$width, nchar(txt))
  progress_emit(paste0("\r", txt))
}

#' Blank the line and return to its start.
#' @noRd
progress_clear <- function(p) {
  if (is.null(p) || p$width == 0L) return(invisible(NULL))
  progress_emit(paste0("\r", strrep(" ", p$width), "\r"))
  p$width <- 0L
  # Redrawn at the next item, whenever that is.
  p$last <- -Inf
  invisible(NULL)
}

#' Clear the line for good and let another one start. The flag first: if
#' clearing is interrupted, the next read can still draw its line.
#' @noRd
progress_done <- function(p) {
  if (is.null(p)) return(invisible(NULL))
  gr_state$progress_on <- FALSE
  progress_clear(p)
  invisible(NULL)
}

#' A message of its own class, so the handler in with_progress() can tell the
#' line from the messages it has to make room for.
#' @noRd
progress_emit <- function(txt) {
  message(structure(class = c("gr_progress", "message", "condition"),
                    list(message = txt, call = NULL)))
}

#' Evaluate `expr` with `p` showing, and remove the line afterwards, however
#' `expr` ends. The line is claimed and first drawn only once the exit handler
#' that releases it is in place.
#' @noRd
with_progress <- function(p, expr) {
  if (is.null(p)) return(expr)
  on.exit(progress_done(p), add = TRUE)
  gr_state$progress_on <- TRUE
  progress_tick(p, 0L)
  # An error or an interrupt that reaches the console is printed before
  # on.exit() runs, so the line is cleared here, when it is signalled.
  withCallingHandlers(expr,
    message = function(m) if (!inherits(m, "gr_progress")) progress_clear(p),
    warning = function(w) progress_clear(p),
    error = function(e) progress_clear(p),
    interrupt = function(i) progress_clear(p))
}
