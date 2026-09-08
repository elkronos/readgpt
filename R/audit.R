# audit.R -- the run, written out so somebody else can check it.
#
# WHY THIS FILE EXISTS
# Everything needed to audit a review is already recorded: the criteria that were
# fixed in advance, which documents were seen and what was decided about each,
# every extracted value with the sentence it came from and whether that sentence
# is really in the document, and what the whole thing cost. It is recorded across
# four objects and half a dozen data frames, which means that in practice nobody
# looks at it.
#
# This assembles it into one file. Not for the person who ran it -- they can
# index into `$evidence` -- but for the reviewer, co-author or regulator who did
# not, and whose question is "how do you know?"
#
# THE REPORT MUST NOT FLATTER THE RUN. An audit that shows only the rows that
# worked is worse than no audit, because it looks like diligence. So the
# unverified quotes, the documents that could not be read, the screening calls
# the model would not make, the fields the extraction could not tie to a
# sentence, and the citations pointing at rows that do not exist are all in it,
# near the top, counted. If a run went badly, the report says so.
#
# And it states what the checking does NOT establish, because a verification
# column that a reader over-reads is a liability. Confirming that a quoted
# sentence appears in the document is not confirming that it supports the claim.

#' Count what happened to every document
#'
#' The numbers a flow diagram is made of, from what a run already recorded.
#' Every source is accounted for at every stage it reached, so the arithmetic
#' closes: nothing leaves the count without a row saying why.
#'
#' @param screening A `gr_screening` from [gr_screen()], or `NULL`.
#' @param extraction A `gr_extraction` from [gr_extract()], or `NULL`.
#' @return A data frame of `stage`, `n` and `note`.
#' @param records A [gr_records()], so the counts begin where the search did:
#'   records identified, duplicates removed, reports sought and reports never
#'   retrieved. Without one the diagram starts at "sources given", which is
#'   already past the step that decides whether the review can be repeated.
#' @seealso [gr_audit_report()], [gr_screen()], [gr_extract()]
#' @export
#' @examples
#' cl <- gr_mock_client(function(messages, params) {
#'   '{"decision":"include","reason":"Reports a revenue figure.",
#'     "criterion":"Reports a revenue figure","quote":null}'
#' })
#' f <- tempfile(fileext = ".txt"); writeLines("Revenue was 45.2 million.", f)
#' s <- gr_screen(f, question = "What was revenue?",
#'                include = "Reports a revenue figure", client = cl)
#' gr_flow(s)
gr_flow <- function(screening = NULL, extraction = NULL, records = NULL) {
  rows <- list()
  add <- function(stage, n, note = "") {
    rows[[length(rows) + 1L]] <<- data.frame(stage = stage, n = as.integer(n),
                                             note = note, stringsAsFactors = FALSE)
  }
  # The rows above "screened", which a folder of PDFs cannot produce and which
  # PRISMA item 16 requires: how many records the search returned, how many were
  # the same work, and how many reports were sought but never obtained. Without
  # a record set the diagram starts at "sources given", which is already past
  # the step that decides whether the review can be repeated.
  if (inherits(records, "gr_records")) {
    for (i in seq_len(nrow(records$counts))) {
      st <- records$counts$stage[i]
      add(st, records$counts$n[i],
          switch(st,
                 "records identified" = paste(sprintf("%s %d", names(records$by_database),
                                                      as.integer(records$by_database)),
                                              collapse = ", "),
                 "duplicates removed" = sprintf("same work by %s", records$dedupe),
                 "reports not retrieved" = "no document was found for these records",
                 ""))
    }
  }
  if (!is.null(screening)) {
    t <- screening$table
    dup <- sum(!is.na(t$duplicate_of))
    add("sources given", nrow(t))
    add("duplicates removed", dup, "same cleaned text as a document already seen")
    add("screened", nrow(t) - dup)
    for (d in c("include", "exclude", "unclear")) {
      add(sprintf("  %s", d), sum(!is.na(t$decision) & t$decision == d & is.na(t$duplicate_of)),
          if (d == "unclear") "the excerpt did not settle it -- for a person to decide" else "")
    }
    add("  could not be read", sum(is.na(t$decision) & is.na(t$duplicate_of)),
        "no decision was recorded; these are outstanding")
  }
  if (!is.null(extraction)) {
    t <- extraction$table
    dup <- sum(!is.na(t$duplicate_of))
    add("extracted from", nrow(t) - dup)
    add("  reported nothing", sum(t$status %in% c("ok", "restored") & !is.na(t$n_filled) &
                                    t$n_filled == 0L & is.na(t$duplicate_of)),
        "read successfully; none of the fields are in the document")
    add("  failed to read", sum(!t$status %in% c("ok", "restored", "duplicate")),
        "no values; these are outstanding")
    add("values unsupported", sum(t$n_unverified, na.rm = TRUE),
        "no verbatim span in the chunk cited")
  }
  if (!length(rows)) {
    return(data.frame(stage = character(0), n = integer(0), note = character(0),
                      stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Write the run out as an auditable report
#'
#' One self-contained HTML file holding everything a reader needs to check the
#' work without running it: the protocol that was fixed in advance, what happened
#' to every document, every extracted value with the sentence and page it came
#' from, whether that sentence is really there, what was written and which rows
#' each claim rests on, and what the run cost.
#'
#' Pass whichever stages you ran. Nothing is required, and anything omitted is
#' simply absent from the report.
#'
#' @param path Where to write the file.
#' @param screening,extraction,synthesis The objects from [gr_screen()],
#'   [gr_extract()] and [gr_synthesise()].
#' @param protocol The [gr_protocol()] the run was made under. Worth passing even
#'   when the other objects carry its pieces: the criteria as *written* are what
#'   a reader checks the decisions against.
#' @param title A heading for the report.
#' @return `path`, invisibly.
#'
#' @section What the report is for:
#' Not for the person who ran it, who can index into `$evidence`. For the
#' reviewer, co-author or regulator who did not, and whose question is "how do
#' you know?" The chain it lays out is: a claim cites a study, the study is a row,
#' the row's values each cite a sentence, and each sentence was checked against
#' the page it was attributed to.
#'
#' @section What it does not tell you:
#' Verification means a quoted sentence really occurs in the chunk it was
#' credited to. It does not mean the sentence supports the value extracted from
#' it, and it does not mean the value is right. A verified quote and a wrong
#' reading of it look identical here; what the check rules out is the quote
#' having been invented. The report says so, in the report, because a column a
#' reader over-reads is worse than no column.
#'
#' @section It does not flatter the run:
#' Unverified quotes, documents that could not be read, screening calls the model
#' declined to make, fields nothing supported and citations pointing at rows that
#' do not exist are all counted near the top. An audit that showed only what
#' worked would look like diligence and be the opposite.
#'
#' @param records A [gr_records()]. Adds the search itself to the report --
#'   which sources, with what query, on what date -- and starts the flow counts
#'   at identification. Without one the report says so, because a missing search
#'   is a defect in the review rather than in the report.
#' @seealso [gr_flow()], [gr_screen()], [gr_extract()], [gr_synthesise()],
#'   [gr_verify_evidence()]
#' @export
#' @examples
#' fields <- gr_fields(design = "The study design")
#' cl <- gr_mock_client(function(messages, params) {
#'   '{"design":"randomised trial","design__quote":"We ran a randomised trial."}'
#' })
#' f <- tempfile(fileext = ".txt"); writeLines("We ran a randomised trial.", f)
#' x <- gr_extract(f, fields, client = cl)
#'
#' out <- gr_audit_report(tempfile(fileext = ".html"), extraction = x)
#' file.exists(out)
gr_audit_report <- function(path, screening = NULL, extraction = NULL,
                            synthesis = NULL, protocol = NULL, records = NULL,
                            title = NULL) {
  if (!is_nonblank(path)) gr_abort("`path` must be a file path.")
  for (pair in list(list(screening, "gr_screening", "screening"),
                    list(extraction, "gr_extraction", "extraction"),
                    list(synthesis, "gr_synthesis", "synthesis"),
                    list(protocol, "gr_protocol", "protocol"))) {
    if (!is.null(pair[[1]]) && !inherits(pair[[1]], pair[[2]])) {
      gr_abort(sprintf("`%s` must be a %s object.", pair[[3]], pair[[2]]),
               class = "gr_bad_audit_input")
    }
  }
  if (is.null(screening) && is.null(extraction) && is.null(synthesis)) {
    gr_abort(paste0("Nothing to report. Pass at least one of `screening`, `extraction` or ",
                    "`synthesis` -- a report of nothing is not evidence that nothing happened."),
             class = "gr_bad_audit_input")
  }

  question <- as_chr1(protocol$question %||% synthesis$question %||%
                        screening$summary$document[0] %||% "", "")
  body <- c(
    audit_header(title, question, protocol, screening, extraction, synthesis),
    audit_protocol(protocol, extraction),
    audit_flow(screening, extraction, records),
    audit_screening(screening),
    audit_extraction(extraction),
    audit_evidence(extraction),
    audit_synthesis(synthesis),
    audit_search(records),
    audit_cost(screening, extraction, synthesis),
    audit_caveats()
  )
  write_utf8_lines(c(audit_head(title), body, "</body>", "</html>"), path)
  gr_msg(sprintf("Audit report written to %s", path))
  invisible(path)
}

# --- internals -------------------------------------------------------------

#' Write UTF-8 text that is still UTF-8 on a machine with no UTF-8 locale.
#'
#' Two traps, both hit while writing this. `file(path, encoding = "UTF-8")` does
#' not mean "write UTF-8"; it means "re-encode from the native encoding on the
#' way out", so handing it strings that are ALREADY UTF-8 makes R convert them
#' through a C locale that cannot represent them -- "invalid char string in
#' output conversion", and the em dash silently lost. A binary connection plus
#' `useBytes = TRUE` writes the bytes that are there.
#'
#' And a connection passed inline to `writeLines()` is never closed, so the last
#' buffer is not flushed: the first attempt produced a report that stopped, mid
#' tag, at exactly 4096 bytes.
#' @noRd
write_utf8_lines <- function(lines, path) {
  con <- file(path, open = "wb")
  on.exit(close(con), add = TRUE)
  writeLines(mark_utf8(lines), con, useBytes = TRUE)
  invisible(path)
}

#' Escape text for HTML.
#'
#' Everything in this report is text somebody else wrote -- a document, or a
#' model's reply to one. Interpolating that into HTML unescaped breaks the page
#' on the first angle bracket in a chemical formula, and turns a report meant to
#' be *shared* into a way of running whatever the document happened to contain.
#' `&` first, or the escapes escape each other.
#' @noRd
esc <- function(x) {
  x <- vapply(x, function(e) as_chr1(e, ""), character(1), USE.NAMES = FALSE)
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub('"', "&quot;", x, fixed = TRUE)
  gsub("'", "&#39;", x, fixed = TRUE)
}

#' @noRd
audit_head <- function(title) {
  c('<!DOCTYPE html>', '<html lang="en">', '<head>', '<meta charset="utf-8">',
    sprintf('<title>%s</title>', esc(title %||% "readgpt audit report")),
    '<style>',
    'body{font:15px/1.55 -apple-system,BlinkMacSystemFont,"Segoe UI",Helvetica,Arial,sans-serif;',
    '  max-width:1100px;margin:2rem auto;padding:0 1.25rem;color:#1a1a1a}',
    'h1{font-size:1.6rem;margin-bottom:.2rem} h2{font-size:1.15rem;margin-top:2.2rem;',
    '  border-bottom:1px solid #ddd;padding-bottom:.3rem}',
    'table{border-collapse:collapse;width:100%;margin:.6rem 0;font-size:13px}',
    'th,td{border:1px solid #ddd;padding:.35rem .5rem;text-align:left;vertical-align:top}',
    'th{background:#f6f6f6;font-weight:600}',
    'td.num{text-align:right;font-variant-numeric:tabular-nums}',
    '.sub{color:#666} .flag{color:#a11;font-weight:600} .ok{color:#161}',
    'blockquote{margin:.4rem 0;padding:.3rem .8rem;border-left:3px solid #ccc;color:#333}',
    'code{background:#f4f4f4;padding:.05rem .3rem;border-radius:3px}',
    '.note{background:#fbfbf6;border:1px solid #e6e3cf;padding:.7rem 1rem;margin:1rem 0}',
    '</style>', '</head>', '<body>')
}

#' @noRd
audit_header <- function(title, question, protocol, screening, extraction, synthesis) {
  ran <- c(if (!is.null(screening)) "screened", if (!is.null(extraction)) "extracted",
           if (!is.null(synthesis)) "synthesised")
  c(sprintf("<h1>%s</h1>", esc(title %||% "readgpt audit report")),
    if (nzchar(question)) sprintf("<p><strong>Question.</strong> %s</p>", esc(question)),
    sprintf("<p class='sub'>Stages run: %s. Generated %s by readgpt %s.</p>",
            esc(paste(ran, collapse = ", ")),
            esc(format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
            esc(as.character(utils::packageVersion("readgpt")))))
}

#' @noRd
audit_protocol <- function(protocol, extraction) {
  fields <- protocol$fields %||% extraction$fields
  if (is.null(protocol) && is.null(fields)) return(NULL)
  crit <- function(label, v) {
    if (!length(v)) return(NULL)
    c(sprintf("<p><strong>%s</strong></p><ul>", esc(label)),
      sprintf("<li>%s</li>", esc(v)), "</ul>")
  }
  c("<h2>The protocol</h2>",
    "<p class='sub'>Fixed before any document was read. These are what the decisions below",
    "are checked against.</p>",
    crit("Include only if all of:", protocol$include),
    crit("Exclude if any of:", protocol$exclude),
    if (!is.null(fields)) c(
      "<p><strong>Extraction schema</strong></p>",
      html_table(data.frame(field = names(fields),
                            type = vapply(fields, function(f) f$type, character(1)),
                            description = vapply(fields, function(f) f$description, character(1)),
                            stringsAsFactors = FALSE))),
    if (length(protocol$outline)) c(
      "<p><strong>Write-up outline</strong></p>",
      html_table(data.frame(section = names(protocol$outline),
                            `must cover` = unname(protocol$outline),
                            check.names = FALSE, stringsAsFactors = FALSE))))
}

#' @noRd
audit_flow <- function(screening, extraction, records = NULL) {
  fl <- gr_flow(screening, extraction, records)
  if (!nrow(fl)) return(NULL)
  c("<h2>What happened to every document</h2>",
    "<p class='sub'>Every source is accounted for at every stage it reached.</p>",
    html_table(fl, numeric_cols = "n"))
}

#' @noRd
audit_screening <- function(screening) {
  if (is.null(screening)) return(NULL)
  t <- screening$table
  keep <- c("document", "decision", "criterion", "reason", "quote", "verified", "error")
  c("<h2>Screening decisions</h2>",
    sprintf("<p class='sub'>One model call per document.%s</p>",
            if (any(t$truncated, na.rm = TRUE))
              " Where a document did not fit one prompt the decision was made on its opening."
            else ""),
    html_table(t[, intersect(keep, names(t)), drop = FALSE],
               flag = list(decision = function(v) v %in% c("unclear", NA),
                           verified = function(v) !is.na(v) & !v)))
}

#' @noRd
audit_extraction <- function(extraction) {
  if (is.null(extraction)) return(NULL)
  t <- extraction$table
  drop <- c("document_id", "duplicate_of", "conflicts")
  c("<h2>What was extracted</h2>",
    html_table(t[, setdiff(names(t), drop), drop = FALSE],
               numeric_cols = intersect(c("n_filled", "n_unverified"), names(t)),
               flag = list(n_unverified = function(v) !is.na(v) & v > 0,
                           status = function(v) !v %in% c("ok", "restored", "duplicate"))),
    if (any(!is.na(t$conflicts))) c(
      "<p class='sub'><strong>Documents that contradicted themselves.</strong> The field is named;",
      "the value kept is the earlier one unless <code>resolve = \"model\"</code> was set.</p>",
      html_table(t[!is.na(t$conflicts), c("document", "conflicts"), drop = FALSE])))
}

#' @noRd
audit_evidence <- function(extraction) {
  ev <- extraction$evidence
  if (!is.data.frame(ev) || !nrow(ev)) return(NULL)
  keep <- intersect(c("document", "field", "page", "section", "quote", "verified", "match"),
                    names(ev))
  bad <- sum(!isTRUE_vec(ev$verified))
  c("<h2>Where every value came from</h2>",
    sprintf("<p class='sub'>%d span(s); %s</p>", nrow(ev),
            if (bad) sprintf("<span class='flag'>%d could not be found in the chunk cited</span>.", bad)
            else "<span class='ok'>every one was found in the chunk cited</span>."),
    html_table(ev[, keep, drop = FALSE],
               numeric_cols = intersect(c("page", "match"), keep),
               flag = list(verified = function(v) !isTRUE_vec(v))))
}

#' @noRd
audit_synthesis <- function(synthesis) {
  if (is.null(synthesis)) return(NULL)
  s <- synthesis$sections
  cites <- synthesis$citations
  per <- function(i) {
    ci <- if (is.null(cites)) NULL else cites[cites$section == s$section[i], , drop = FALSE]
    c(sprintf("<h3>%s</h3>", esc(s$section[i])),
      sprintf("<p class='sub'>Must cover: %s</p>", esc(s$brief[i])),
      sprintf("<blockquote>%s</blockquote>", esc(s$text[i])),
      if (s$n_unknown[i] > 0L)
        sprintf("<p class='flag'>%d citation(s) point at a row that does not exist.</p>",
                s$n_unknown[i]),
      if (s$n_cited[i] == 0L)
        "<p class='flag'>This section cites nothing.</p>",
      if (!is.null(ci) && nrow(ci))
        html_table(ci[, intersect(c("study", "document"), names(ci)), drop = FALSE],
                   numeric_cols = "study"))
  }
  c("<h2>What was written, and what each section rests on</h2>",
    unlist(lapply(seq_len(nrow(s)), per), use.names = FALSE),
    if (isTRUE(synthesis$skipped > 0L))
      sprintf(paste0("<p class='sub'>%d row(s) were left out of the write-up: a duplicate, a ",
                     "document that could not be read, or one with nothing extracted.</p>"),
              synthesis$skipped))
}

#' @noRd
audit_cost <- function(...) {
  stages <- list(...)
  # Named for the reader, not after the class. "gr_screening" is what the object
  # is called in the code and means nothing to the person the report is for.
  names(stages) <- c("screening", "extraction", "synthesis")[seq_along(stages)]
  traces <- Filter(function(x) inherits(x$trace, "gr_trace"), stages)
  if (!length(traces)) return(NULL)
  rows <- lapply(names(traces), function(nm) {
    x <- traces[[nm]]
    cost <- gr_trace_cost(x$trace)
    data.frame(stage = nm, calls = as.integer(x$trace$calls),
               cached = as.integer(x$trace$cached %||% 0L),
               tokens_in = as.integer(x$trace$tokens_in),
               tokens_out = as.integer(x$trace$tokens_out),
               usd = if (nrow(cost)) sum(cost$usd) else 0,
               stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  c("<h2>What the run cost</h2>",
    "<p class='sub'>Counting only the calls that were really issued -- a reply served from a",
    "cache spent nothing, however large its prompt.</p>",
    html_table(tab, numeric_cols = c("calls", "cached", "tokens_in", "tokens_out", "usd")))
}

#' @noRd
audit_caveats <- function() {
  c("<h2>What this report does not establish</h2>",
    "<div class='note'>",
    "<p><strong>A verified quote is a quote that is really there.</strong> The check confirms",
    "that the sentence credited with a value occurs in the chunk it was attributed to. It does",
    "not confirm that the sentence supports the value, and it does not confirm the value is",
    "right. A correct quote read wrongly looks exactly like a correct quote read rightly. What",
    "the check rules out is the quote having been invented, which is the failure that is",
    "otherwise invisible.</p>",
    "<p><strong>A page number is where the sentence is, not where the reasoning is.</strong>",
    "Spans found on several pages, or not found at all, are left without one rather than",
    "given the likeliest.</p>",
    "<p><strong>Screening saw what the excerpt showed.</strong> Where a document was",
    "truncated the decision was made on its opening; the screening table says which.</p>",
    "</div>")
}

#' A data frame as an HTML table.
#'
#' `flag` is a named list of predicates over a column; rows where one holds are
#' marked. It is how the report shows the parts of a run that went wrong without
#' the reader having to compare columns by eye.
#' @noRd
html_table <- function(df, numeric_cols = character(0), flag = list()) {
  if (!is.data.frame(df) || !nrow(df)) return("<p class='sub'>(nothing)</p>")
  cell <- function(v, col) {
    txt <- ifelse(is.na(v), "<span class='sub'>&mdash;</span>", esc(v))
    cls <- if (col %in% numeric_cols) " class=\"num\"" else ""
    if (!is.null(flag[[col]])) {
      hit <- tryCatch(flag[[col]](v), error = function(e) rep(FALSE, length(v)))
      hit[is.na(hit)] <- FALSE
      txt[hit] <- sprintf("<span class='flag'>%s</span>", txt[hit])
    }
    sprintf("<td%s>%s</td>", cls, txt)
  }
  cells <- lapply(names(df), function(col) cell(df[[col]], col))
  body <- do.call(paste0, cells)
  c("<table>", sprintf("<tr>%s</tr>", paste0(sprintf("<th>%s</th>", esc(names(df))),
                                             collapse = "")),
    sprintf("<tr>%s</tr>", body), "</table>")
}


#' The search, as the review has to report it.
#'
#' PRISMA items 6 and 7, and item 7 asks for the full strategy for at least one
#' database *so that it could be repeated*. Printing it beside the results is
#' the difference between a report a reader can check and one they must take on
#' trust -- and when it is absent, saying so is more useful than leaving the
#' section out, because a missing search is a defect in the review rather than
#' in the report.
#' @noRd
audit_search <- function(records) {
  se <- if (inherits(records, "gr_records")) records$search else NULL
  if (is.null(se)) {
    return(c("<h2>The search</h2>",
             "<p class='sub'>Not recorded. A systematic review must state which sources were",
             "searched, with what query and on what date (PRISMA items 6 and 7); attach a",
             "<code>gr_search()</code> to <code>gr_records()</code> and it appears here.</p>"))
  }
  tab <- data.frame(source = names(se$databases), query = unname(se$databases),
                    searched = se$dates, stringsAsFactors = FALSE)
  extra <- c(if (!all(is.na(se$limits))) sprintf("<p><b>Limits:</b> %s</p>",
                                                 esc(paste(se$limits, collapse = "; "))),
             if (length(se$other)) sprintf("<p><b>Other sources:</b> %s</p>",
                                           esc(paste(se$other, collapse = "; "))),
             sprintf("<p><b>Registration:</b> %s</p>",
                     if (is.na(se$registration))
                       "<span class='flag'>not registered</span>" else esc(se$registration)),
             if (!is.na(se$notes)) sprintf("<p>%s</p>", esc(se$notes)))
  c("<h2>The search</h2>",
    "<p class='sub'>What was searched, with what query and when. Reproducing the review",
    "starts here.</p>",
    html_table(tab), extra)
}
