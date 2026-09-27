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
#' @param claims A [gr_claims()] result, to add the claim-level counts.
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
gr_flow <- function(screening = NULL, extraction = NULL, records = NULL, claims = NULL) {
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
          if (d == "unclear") "the excerpt did not settle it; for a person to decide" else "")
    }
    add("  could not be read", sum(is.na(t$decision) & is.na(t$duplicate_of)),
        "no decision was recorded; these are outstanding")
  }
  if (!is.null(extraction)) {
    t <- extraction$table
    dup <- sum(!is.na(t$duplicate_of))
    # Each row below counts distinct documents only, and no document twice, so
    # none can come to more than "extracted from".
    own <- is.na(t$duplicate_of %||% rep(NA, nrow(t)))
    add("extracted from", nrow(t) - dup)
    add("  reported nothing", sum(t$status %in% c("ok", "restored") & !is.na(t$n_filled) &
                                    t$n_filled == 0L & own),
        "read successfully; none of the fields are in the document")
    # "incomplete" has values, so it is not a failed read: counted there, it
    # was described as having none and a table of real values looked empty.
    # Its empty cells are the part that is outstanding.
    add("  read in part", sum(t$status %in% "incomplete" & own),
        "a request failed; the values are real, but an empty cell is unknown, not unreported")
    add("  failed to read", sum(!t$status %in% c("ok", "restored", "duplicate", "incomplete") &
                                  own),
        "no values; these are outstanding")
    # Not "no verbatim span": a sentence that is in the chunk word for word but
    # does not state the value is unverified too (verified = FALSE, match = 1).
    # Distinct documents only, as "extracted from" is: a duplicate row carries
    # its first copy's values and count, and summed with them it counted the
    # same unsupported values twice.
    add("values unsupported", sum(t$n_unverified[own], na.rm = TRUE),
        paste0("not verified: no quote, a quote not found in the chunk cited, or one that ",
               "does not state the value in any form the check reads"))
  }
  if (inherits(claims, "gr_claims")) {
    cw <- claims$claims
    # Studies whose claims batch came back with nothing. Without this row a
    # claims table missing most of the corpus read as complete here, although
    # gr_claims() had warned and recorded them. `%||%` for a claims table saved
    # before the field existed.
    add("studies lost to a claims batch", length(claims$lost %||% integer(0)),
        "their claims batch was cut off at the reply limit, failed or not sent; see $lost")
    add("claims drawn", nrow(cw), "statements about the literature, each attached to studies")
    add("  contested", sum(cw$n_contradict > 0L), "studies on both sides")
    add("  unexplained", sum(cw$n_contradict > 0L & is.na(cw$moderator)),
        "contested with nothing in the table to explain the split")
    add("  on one study", sum(cw$n_support == 1L), "no replication in this corpus")
    # `$dropped` has a row for three different events, and only one of them
    # removes a claim: a bad study number is taken off a claim that is kept, a
    # moderator that is not a column is cleared from one. Counted as one
    # number, "claims dropped 3" was quoted for a run that dropped none.
    why <- as.character(claims$dropped$reason %||% character(0))
    detail <- as.character(claims$dropped$detail %||% rep(NA_character_, length(why)))
    refs <- grepl("^study number", why)
    add("claims dropped", sum(why == "no supporting study left after verification"),
        "no supporting study was left after verification; see the claims object's $dropped")
    add("study numbers removed", sum(lengths(strsplit(detail[refs & !is.na(detail)], ",",
                                                         fixed = TRUE))),
        paste0("cited a study not in the table, or one not shown to the batch; the claim ",
               "stays if others remain"))
    add("moderators cleared", sum(why == "moderator is not a column in the table"),
        "named a column the table does not have; the claim is kept without it")
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
#'   a reader checks the decisions against. The report compares it with what
#'   each stage recorded using (the screening's question and criteria, the
#'   extraction's schema, the write-up's question and outline). Where they
#'   differ, as they do for a protocol edited after the run, it says so at the
#'   top of the protocol section and shows what each stage actually used,
#'   rather than presenting the edited criteria as the ones fixed in advance.
#'   Where no stage recorded anything to compare it with, it says that too.
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
#' A page is given to a span found on one page of the document. A span found on
#' several pages, or not found at all, keeps the page of the chunk it was
#' credited to, so an unverified quote shown with a page was not found on it.
#'
#' @section It does not flatter the run:
#' Unverified quotes, documents that could not be read, screening calls the model
#' declined to make, fields nothing supported and citations pointing at rows that
#' do not exist are all counted near the top. An audit that showed only what
#' worked would look like diligence and be the opposite.
#'
#' @param calibration A [gr_calibrate()] result, to add a section saying how good
#'   the screening is: sensitivity, specificity, kappa, and how many eligible
#'   studies the screener threw away. Without it the report says what the run did
#'   and nothing about whether it did it well.
#' @param claims A [gr_claims()] result, or `NULL` to take the one the synthesis
#'   carries. It adds the link the rest of the report cannot make: the claim a
#'   sentence is making, back to the studies meant to support it.
#' @param records A [gr_records()]. Adds the search itself to the report
#'   (which sources, with what query, on what date) and starts the flow counts
#'   at identification. Without one the report says so, because a missing search
#'   is a defect in the review rather than in the report.
#' @param answer A [gr_answer] from [answer_document()], or a `gr_corpus` from
#'   [gr_read_many()]. Adds the question, the answer, whether it is complete,
#'   what it cost, and the passages it came from in document order; see "An
#'   answer" below.
#' @param open Open the report once it is written: in the RStudio viewer when
#'   the file is under [tempdir()], and otherwise in the web browser. By
#'   default only in an interactive session.
#'
#' @section An answer:
#' With `answer`, the report shows the answer or says that it was not found,
#' whether it is partial and why, the recipe, reader, number of requests and
#' cost, and any warnings. Then the passages behind it, grouped by chunk and in
#' document order, each with its page, section and chunk number:
#' \itemize{
#'   \item A quotation a reader copied out (`skim`, `extract`) is highlighted in
#'     the chunk it came from. One that is not in that chunk is listed under it
#'     and flagged.
#'   \item A chunk a reader sent whole (`stuff`, `retrieve`, `rerank`) is shown
#'     with the numbers from the answer highlighted where they occur. That shows
#'     where to look, not that the chunk supports the answer. A chunk the answer
#'     cites as `[chunk n]` is marked as cited.
#'   \item `map_reduce` answers each chunk and then combines the answers. Its
#'     answer from each chunk is shown as such: the model's words, not the
#'     document's. `refine` and `hierarchical` keep no passages, and the report
#'     says so.
#' }
#' A passage over 6,000 characters, such as a whole document sent in one
#' request, is cut to the text around what is highlighted, with each cut shown
#' as "\[...\]".
#' Last comes one row per request, from `as.data.frame()` on the answer's
#' trace (see [gr_trace()]), without the prompts and replies.
#'
#' A `gr_corpus` gives one row per document, then each document's answer and
#' passages. Answers are there only if the run kept them (`keep_answers`).
#'
#' "Not partial" is said only when the reader did not mark the answer partial
#' and no request on its trace failed. A failed request is flagged even when
#' the reader did not count it; a trace shared with other runs flags theirs
#' too, because the report cannot tell the runs on one trace apart. An answer
#' that is "not found" because the request that would have given it failed is
#' shown as no answer, not as a finding about the document.
#'
#' @section Web addresses:
#' A document fetched from a web address is shown without the user name,
#' password, query string or fragment in the address, which is where presigned
#' and tokenised links carry their credentials. The query is replaced by a short
#' fingerprint, so two addresses that differ only there stay apart. This covers
#' the document names, the error and warning text the report shows, and the
#' answer's document. It does not change what the objects themselves hold.
#'
#' @section Claims and the write-up:
#' The claims table has a `section` column only when the synthesis recorded
#' which section each claim was given to. When a revision pass
#' (`coherence`) was kept, "What was written" shows the published text and the
#' studies each of its headings cites, then the sections as first drafted,
#' with their checks, under "Draft before revision", and the passes that ran.
#' The cost table includes the calls [gr_claims()] made and, when it was left
#' to record on the claims' trace, [gr_outline()].
#' @seealso [gr_flow()], [gr_screen()], [gr_extract()], [gr_synthesise()],
#'   [gr_verify_evidence()], [answer_document()], [gr_read_many()]
#' @export
#' @examples
#' fields <- gr_fields(design = "The study design")
#' cl <- gr_mock_client(function(messages, params) {
#'   '{"design":"randomised trial","design__quote":"We ran a randomised trial."}'
#' })
#' f <- tempfile(fileext = ".txt"); writeLines("We ran a randomised trial.", f)
#' x <- gr_extract(f, fields, client = cl)
#'
#' out <- gr_audit_report(tempfile(fileext = ".html"), extraction = x, open = FALSE)
#' file.exists(out)
#'
#' # One answer and the passages behind it.
#' cl2 <- gr_mock_client(function(m, p) "Revenue was 45.2 million dollars.")
#' ans <- answer_document(readgpt_example(), "What was revenue?", "fast", client = cl2)
#' page <- gr_audit_report(tempfile(fileext = ".html"), answer = ans, open = FALSE)
gr_audit_report <- function(path, screening = NULL, extraction = NULL,
                            synthesis = NULL, protocol = NULL, title = NULL,
                            claims = NULL, records = NULL, calibration = NULL,
                            answer = NULL, open = interactive()) {
  # `claims` and `records` are APPENDED, not slotted in where they belong
  # thematically. Inserting `claims` fourth silently rebound the fourth
  # positional argument of every existing call -- gr_audit_report(p, s, x, syn)
  # wrote a report with the synthesis filed as claims and no synthesis section
  # in it, and nothing said so. A new argument goes at the end.
  if (!is_nonblank(path)) gr_abort("`path` must be a file path.")
  for (pair in list(list(screening, "gr_screening", "screening"),
                    list(extraction, "gr_extraction", "extraction"),
                    list(synthesis, "gr_synthesis", "synthesis"),
                    list(protocol, "gr_protocol", "protocol"),
                    list(claims, "gr_claims", "claims"),
                    list(records, "gr_records", "records"),
                    list(calibration, "gr_calibration", "calibration"))) {
    if (!is.null(pair[[1]]) && !inherits(pair[[1]], pair[[2]])) {
      gr_abort(sprintf("`%s` must be a %s object.", pair[[3]], pair[[2]]),
               class = "gr_bad_audit_input")
    }
  }
  if (!is.null(answer) && !inherits(answer, c("gr_answer", "gr_corpus"))) {
    gr_abort("`answer` must be a gr_answer from answer_document() or a gr_corpus from gr_read_many().",
             class = "gr_bad_audit_input")
  }
  if (is.null(screening) && is.null(extraction) && is.null(synthesis) && is.null(answer)) {
    gr_abort(paste0("Nothing to report. Pass at least one of `screening`, `extraction`, ",
                    "`synthesis` or `answer`: a report of nothing is not evidence that nothing ",
                    "happened."),
             class = "gr_bad_audit_input")
  }

  question <- as_chr1(protocol$question %||% synthesis$question %||% report_question(answer) %||%
                        screening$summary$document[0] %||% "", "")
  # The screening and extraction objects now carry the record set they were run
  # over, so the search reaches the report whether or not the caller remembered
  # to hand it over a second time at the end. Passing `records` still wins.
  records <- records %||% screening$records %||% extraction$records
  review <- !is.null(screening) || !is.null(extraction) || !is.null(synthesis)
  body <- c(
    audit_header(title, question, protocol, screening, extraction, synthesis, answer),
    audit_answer(answer),
    audit_protocol(protocol, extraction, screening, synthesis),
    audit_flow(screening, extraction, records, claims %||% synthesis$claims),
    audit_screening(screening),
    audit_calibration(calibration),
    audit_extraction(extraction),
    audit_evidence(extraction),
    audit_claims(synthesis, claims),
    audit_synthesis(synthesis),
    # The search belongs to a review. A report on one answer has none to show.
    if (review || !is.null(records)) audit_search(records),
    audit_cost(screening = screening, extraction = extraction, synthesis = synthesis,
               reading = answer, claims = claims %||% synthesis$claims),
    audit_caveats(screening = !is.null(screening), answer = !is.null(answer),
                  quotes = review || answer_has_quotes(answer))
  )
  write_utf8_lines(c(audit_head(title), body, "</body>", "</html>"), path)
  gr_msg(sprintf("Audit report written to %s", path))
  if (isTRUE(open)) audit_open(path)
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
    '.card{border:1px solid #ddd;border-radius:4px;padding:.5rem .9rem;margin:.9rem 0}',
    '.where{font-size:13px;color:#555;margin:.2rem 0 .4rem}',
    '.passage{white-space:pre-wrap;margin:.3rem 0;font-size:14px}',
    'mark{background:#fde68a;padding:0 .1rem}',
    '</style>', '</head>', '<body>')
}

#' @noRd
audit_header <- function(title, question, protocol, screening, extraction, synthesis,
                         answer = NULL) {
  ran <- c(if (!is.null(screening)) "screened", if (!is.null(extraction)) "extracted",
           if (!is.null(synthesis)) "synthesised",
           if (inherits(answer, "gr_answer")) "answered",
           if (inherits(answer, "gr_corpus"))
             sprintf("answered across %d document(s)", NROW(answer$summary)))
  c(sprintf("<h1>%s</h1>", esc(title %||% "readgpt audit report")),
    if (nzchar(question)) sprintf("<p><strong>Question.</strong> %s</p>", esc(question)),
    sprintf("<p class='sub'>Stages run: %s. Generated %s by readgpt %s.</p>",
            esc(paste(ran, collapse = ", ")),
            esc(format(Sys.time(), "%Y-%m-%d %H:%M %Z")),
            esc(as.character(utils::packageVersion("readgpt")))))
}

#' The protocol, checked against what the run recorded using.
#'
#' A protocol is a plain list anyone can edit after the run, and the report is
#' handed whichever one the caller has now. Rendered under "fixed before any
#' document was read" without a look at the stages, criteria added after seeing
#' the results were presented as the ones the decisions were made against --
#' the drift the protocol exists to rule out. The screening records its
#' question and criteria, the extraction its schema and the write-up its
#' question and outline, so each is compared, and a difference is said first.
#' @noRd
audit_protocol <- function(protocol, extraction, screening = NULL, synthesis = NULL) {
  screened <- inherits(screening, "gr_screening") &&
    (length(screening$include) || length(screening$exclude))
  fields <- protocol$fields %||% extraction$fields
  if (is.null(protocol) && is.null(fields) && !screened) return(NULL)
  if (is.null(protocol)) {
    return(c("<h2>The protocol</h2>",
             "<p class='sub'>No protocol was given with the report. These are the criteria and",
             "the schema the run recorded using.</p>",
             if (screened) c(protocol_criteria("Include only if all of:", screening$include),
                             protocol_criteria("Exclude if any of:", screening$exclude)),
             protocol_schema(fields)))
  }
  drift <- protocol_drift(protocol, screening, extraction, synthesis)
  intro <- if (length(drift$differ)) {
    sprintf(paste0("<p class='flag'>This is not the protocol the run was made under. %s The ",
                   "decisions below were made against what each stage recorded, shown after ",
                   "the protocol, not against this.</p>"),
            esc(paste(drift$differ, collapse = " ")))
  } else if (length(drift$checked)) {
    # Before THOSE stages read anything: that much the match establishes.
    sprintf(paste0("<p class='sub'>Fixed before %s read any document: %s recorded exactly ",
                   "these. They are what the decisions below are checked against.</p>"),
            esc(and_words(drift$checked)), if (length(drift$checked) == 1L) "it" else "each")
  } else {
    paste0("<p class='sub'>Given with the report. No stage passed to it recorded the criteria ",
           "it ran under, so the report cannot confirm these are the ones the run used.</p>")
  }
  c("<h2>The protocol</h2>", intro,
    protocol_criteria("Include only if all of:", protocol$include),
    protocol_criteria("Exclude if any of:", protocol$exclude),
    # The extraction's own schema when the protocol has none: labelled as
    # that, since the protocol cannot have fixed a schema it does not carry.
    protocol_schema(fields, if (is.null(protocol$fields))
      "Extraction schema (as the extraction recorded it; the protocol has none)"),
    protocol_outline(protocol$outline),
    drift$shown)
}

#' A list of criteria under a label, or nothing when there are none.
#' @noRd
protocol_criteria <- function(label, v) {
  if (!length(v)) return(NULL)
  c(sprintf("<p><strong>%s</strong></p><ul>", esc(label)), sprintf("<li>%s</li>", esc(v)), "</ul>")
}

#' @noRd
protocol_schema <- function(fields, label = "Extraction schema") {
  if (is.null(fields)) return(NULL)
  c(sprintf("<p><strong>%s</strong></p>", esc(label)),
    html_table(data.frame(field = names(fields),
                          type = vapply(fields, function(f) as_chr1(f$type, NA_character_),
                                        character(1)),
                          description = vapply(fields, function(f) as_chr1(f$description,
                                                                           NA_character_),
                                               character(1)),
                          stringsAsFactors = FALSE)))
}

#' @noRd
protocol_outline <- function(outline, label = "Write-up outline") {
  if (!length(outline)) return(NULL)
  c(sprintf("<p><strong>%s</strong></p>", esc(label)),
    html_table(data.frame(section = names(outline), `must cover` = unname(outline),
                          check.names = FALSE, stringsAsFactors = FALSE)))
}

#' Where a protocol and the stages run under it disagree.
#'
#' `checked` names the stages that recorded something to compare, `differ`
#' says in a sentence each how one departs from the protocol, and `shown` is
#' what that stage actually used, for the report. Criteria are compared as
#' sets, after the trimming criteria_vector() gives both: order carries no
#' meaning in a list of criteria, and neither does space around one.
#' @noRd
protocol_drift <- function(protocol, screening = NULL, extraction = NULL, synthesis = NULL) {
  checked <- character(0); differ <- character(0); shown <- character(0)
  same_set <- function(a, b) setequal(criteria_vector(a, ""), criteria_vector(b, ""))
  words <- function(q) gsub("[[:space:]]+", " ", trimws(as_chr1(q, "")))
  same_q <- function(q) is.null(q) || identical(words(q), words(protocol$question))
  if (inherits(screening, "gr_screening")) {
    checked <- c(checked, "the screening")
    # The question is on the trace gr_read_many() made for the screening.
    sq <- if (inherits(screening$trace, "gr_trace"))
      screening$trace$meta[["question", exact = TRUE]]
    bad <- c(if (!same_q(sq)) "question",
             if (!same_set(protocol$include, screening$include)) "inclusion criteria",
             if (!same_set(protocol$exclude, screening$exclude)) "exclusion criteria")
    if (length(bad)) {
      differ <- c(differ, sprintf("The screening differs from it in its %s.", and_words(bad)))
      shown <- c(shown, "<h3>What the screening was run against</h3>",
                 if (!same_q(sq)) sprintf("<p><strong>Question.</strong> %s</p>", esc(sq)),
                 protocol_criteria("Include only if all of:", screening$include),
                 protocol_criteria("Exclude if any of:", screening$exclude),
                 if (!length(screening$include) && !length(screening$exclude))
                   "<p class='sub'>(no criteria recorded)</p>")
    }
  }
  if (inherits(extraction, "gr_extraction") && !is.null(protocol$fields) &&
      !is.null(extraction$fields)) {
    checked <- c(checked, "the extraction")
    if (!same_fields(protocol$fields, extraction$fields)) {
      differ <- c(differ, "The extraction differs from it in its schema.")
      shown <- c(shown, "<h3>The schema the extraction used</h3>",
                 protocol_schema(extraction$fields,
                                 "Extraction schema, as the extraction recorded it"))
    }
  }
  if (inherits(synthesis, "gr_synthesis")) {
    checked <- c(checked, "the write-up")
    wq <- synthesis$question
    wo <- synthesis$outline
    # Only an outline the protocol has: without one, the write-up's outline
    # came from somewhere else by design (gr_outline(), or the caller).
    moved <- length(protocol$outline) &&
      !(identical(unname(words_each(names(wo))), unname(words_each(names(protocol$outline)))) &&
          identical(unname(words_each(wo)), unname(words_each(protocol$outline))))
    bad <- c(if (!same_q(wq)) "question", if (moved) "outline")
    if (length(bad)) {
      differ <- c(differ, sprintf("The write-up differs from it in its %s.", and_words(bad)))
      shown <- c(shown, "<h3>What the write-up was made with</h3>",
                 if (!same_q(wq)) sprintf("<p><strong>Question.</strong> %s</p>", esc(wq)),
                 if (moved) protocol_outline(wo, "Outline, as the write-up recorded it"))
    }
  }
  list(checked = checked, differ = differ, shown = shown)
}

#' Two schemas define the same fields: names, types, descriptions and allowed
#' values alike, in any order.
#' @noRd
same_fields <- function(a, b) {
  key <- function(f) {
    if (!length(f)) return(character(0))
    vapply(seq_along(f), function(i) {
      g <- f[[i]]
      paste(names(f)[i], as_chr1(g$type, ""), trimws(as_chr1(g$description, "")),
            paste(sort(as.character(g$values %||% character(0))), collapse = "\u001f"),
            sep = "\u001e")
    }, character(1))
  }
  ka <- key(a); kb <- key(b)
  length(ka) == length(kb) && setequal(ka, kb)
}

#' Each string with its space runs made one space and its ends trimmed.
#' @noRd
words_each <- function(x) gsub("[[:space:]]+", " ", trimws(as.character(x %||% character(0))))

#' "a", "a and b", "a, b and c".
#' @noRd
and_words <- function(x) {
  if (length(x) <= 1L) return(paste(x, collapse = ""))
  paste(paste(x[-length(x)], collapse = ", "), "and", x[length(x)])
}

#' @noRd
audit_flow <- function(screening, extraction, records = NULL, claims = NULL) {
  fl <- gr_flow(screening, extraction, records, claims)
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
  # A duplicate's rows are its first copy's, repeated under its name: counted,
  # they doubled that copy's spans and its unverified ones, and the tally no
  # longer matched the flow's. Left out, and said so.
  t <- extraction$table
  copies <- if (is.data.frame(t) && !is.null(t[["document"]]) && !is.null(t[["duplicate_of"]]))
    as.character(t[["document"]][!is.na(t[["duplicate_of"]])]) else character(0)
  repeated <- if (is.null(ev[["document"]])) rep(FALSE, nrow(ev))
              else as.character(ev[["document"]]) %in% copies
  n_repeated <- sum(repeated)
  ev <- ev[!repeated, , drop = FALSE]
  if (!nrow(ev)) return(NULL)
  keep <- intersect(c("document", "field", "page", "section", "quote", "verified", "match"),
                    names(ev))
  unver <- !isTRUE_vec(ev$verified)
  # `verified` asks two things of a quote: that it is in the chunk cited, and
  # that it states the value. A sentence that is there word for word
  # (match = 1) and does not state it was reported as not found.
  there <- sum(unver & cited_verbatim(ev))
  gone <- sum(unver) - there
  c("<h2>Where every value came from</h2>",
    sprintf("<p class='sub'>%d span(s); %s%s</p>", nrow(ev),
            if (gone || there) sprintf("<span class='flag'>%s</span>.", paste(c(
              if (gone) sprintf("%d could not be found in the chunk cited", gone),
              if (there) sprintf(paste0("%d found in the chunk cited but not stating the value ",
                                        "cited in any form the check reads"), there)),
              collapse = "; "))
            else "<span class='ok'>every one was found in the chunk cited</span>.",
            if (n_repeated) sprintf(paste0(" %d more for duplicate documents are left out: they ",
                                           "repeat the spans of the copy each duplicates."),
                                    n_repeated)
            else ""),
    html_table(ev[, keep, drop = FALSE],
               numeric_cols = intersect(c("page", "match"), keep),
               flag = list(verified = function(v) !isTRUE_vec(v))))
}

#' Which evidence rows quote their chunk word for word (`match` 1) for the
#' value of a field.
#'
#' `verified` asks more of a quote cited for a field: that it state the value
#' (quote_backs_value()), so such a row can be `verified = FALSE` and still be
#' in the chunk. gr_verify_evidence() draws the same line. FALSE where `match`
#' or `field` is absent or NA. `[[`, not `$`: `$` on a data frame
#' partial-matches.
#' @noRd
cited_verbatim <- function(ev) {
  n <- nrow(ev)
  m <- if (is.null(ev[["match"]])) rep(NA_real_, n) else suppressWarnings(as.numeric(ev[["match"]]))
  f <- if (is.null(ev[["field"]])) rep(NA_character_, n) else as.character(ev[["field"]])
  !is.na(m) & m >= 1 & !is.na(f) & nzchar(f)
}

#' The claims, the studies behind them, and where each one ended up.
#'
#' This is the link that makes the layer defensible rather than merely present.
#' Without it a reader can walk a sentence back to a study and a study back to a
#' quote, but not the claim the sentence is making back to the studies that were
#' supposed to support it -- which is the step where a synthesis is either right
#' or invented.
#' @noRd
audit_claims <- function(synthesis, claims = NULL) {
  cm <- claims %||% synthesis$claims
  if (!inherits(cm, "gr_claims")) return(NULL)
  lost <- cm$lost %||% integer(0)
  # The studies whose claims batch came back with nothing, counted against the
  # corpus: "over 90 studies" of a table drawn from 30 of them flattered the run.
  lost_note <- if (length(lost)) sprintf(paste0(
    "<p class='flag'>%d of the %d studies contributed nothing: their claims batch was cut off ",
    "at the reply limit, failed or was not sent, so no claim, and no part of the review ",
    "written from these claims, rests on them (study %s).</p>"),
    length(lost), nrow(cm$studies),
    paste(c(utils::head(lost, 20L), if (length(lost) > 20L) "..."), collapse = ", "))
  if (!nrow(cm$claims)) {
    if (!length(lost)) return(NULL)
    return(c("<h2>What the review claims, and what each claim rests on</h2>",
             "<p class='flag'>No claims were drawn.</p>", lost_note))
  }
  # Which section each claim was given to. gr_synthesise() rebuilds the outline
  # with setNames(), which drops the "claims" attribute gr_outline() put on it,
  # so read from there alone the column was empty for every claim. A column
  # that is always empty says the claims went nowhere; left out instead, with
  # a line saying the assignment was not recorded.
  sec <- synthesis[["claim_sections", exact = TRUE]] %||% attr(synthesis$outline, "claims")
  sec_ok <- is.data.frame(sec) && all(c("section", "claim_id") %in% names(sec))
  tab <- cm$claims[, c("claim_id", "claim", "kind", "moderator", "scope",
                       "n_support", "n_contradict"), drop = FALSE]
  if (sec_ok) tab$section <- as.character(sec$section)[match(tab$claim_id, sec$claim_id)]
  sup <- cm$support
  docs <- cm$studies$document[match(sup$study, cm$studies$study)]
  detail <- data.frame(claim_id = sup$claim_id, study = sup$study, role = sup$role,
                       document = docs, stringsAsFactors = FALSE)
  contested <- sum(tab$n_contradict > 0L)
  open <- sum(tab$n_contradict > 0L & is.na(tab$moderator))
  lone <- sum(tab$n_support == 1L)
  c("<h2>What the review claims, and what each claim rests on</h2>",
    sprintf(paste0("<p class='sub'>%d claim(s) over %s study/studies. %d contested, %s. ",
                   "%d resting on a single study.</p>"),
            nrow(tab),
            if (length(lost)) sprintf("%d of %d", nrow(cm$studies) - length(lost), nrow(cm$studies))
            else nrow(cm$studies),
            contested,
            if (open) sprintf("<span class='flag'>%d of those with nothing in the table to explain the disagreement</span>", open)
            else "<span class='ok'>each with a distinguishing field named</span>",
            lone),
    lost_note,
    if (!is.null(synthesis) && !sec_ok)
      paste0("<p class='sub'>Which section each claim was given to was not recorded with this ",
             "synthesis, so it is not shown.</p>"),
    html_table(tab, numeric_cols = c("claim_id", "n_support", "n_contradict"),
               flag = list(n_support = function(v) suppressWarnings(as.numeric(v)) <= 1,
                           # A claim the outline gave to no section was not written up.
                           section = function(v) is.na(v))),
    "<h3>Every claim, study by study</h3>",
    html_table(detail, numeric_cols = c("claim_id", "study"),
               flag = list(role = function(v) v == "contradicts")),
    if (nrow(cm$dropped))
      c("<h3>Removed or cleared in verification</h3>",
        sprintf(paste0("<p class='sub'>A claims table that looks thin has to be tellable from a ",
                       "literature that is, so what verification removed is printed too. Only a ",
                       "row whose reason is that no supporting study was left removed a claim; ",
                       "the others took a study number or a moderator off a claim that was ",
                       "kept.</p>")),
        html_table(cm$dropped)))
}

#' What was written, and what each part of it rests on.
#'
#' The sections and their citation tables are the draft. A kept revision pass
#' (`coherence`) can merge, rename or cut sections and move a citation from one
#' to another while citing the same studies overall, and `$text` is then the
#' revision, not the sections. Shown as "what was written", the draft put
#' headings, text and citation placements in front of a reader that the
#' published review does not have. So with a kept pass the published text
#' comes first, with the studies each of its headings cites, and the draft
#' follows under its own heading with the checks that ran on it.
#' @noRd
audit_synthesis <- function(synthesis) {
  if (is.null(synthesis)) return(NULL)
  s <- synthesis$sections
  cites <- synthesis$citations
  rep_df <- synthesis$coherence
  revised <- is.data.frame(rep_df) && nrow(rep_df) && any(isTRUE_vec(rep_df$kept))
  lvl <- if (revised) "h4" else "h3"
  col <- function(nm, i) if (is.null(s[[nm]])) NA else s[[nm]][i]
  per <- function(i) {
    ci <- if (is.null(cites)) NULL else cites[cites$section == s$section[i], , drop = FALSE]
    blank <- !nzchar(trimws(as_chr1(s$text[i], "")))
    # Each flag below is one of the reasons synth_section() marks a section
    # partial, so a section marked partial with none of them in `fl` is
    # partial for a reason not recorded apart, and is flagged for that below.
    fl <- c(
      if (isTRUE(col("n_unknown", i) > 0L))
        sprintf("<p class='flag'>%d citation(s) point at a row that does not exist.</p>",
                s$n_unknown[i]),
      # A real row the section was never shown: the model cannot have read it
      # there, so the citation is as unsupported as one to no row at all. The
      # section is already partial for it; without this the report said nothing.
      # `is.null()` for a synthesis saved before the column existed.
      if (isTRUE(col("n_unsupplied", i) > 0L))
        sprintf("<p class='flag'>%d citation(s) point at a study this section was not given.</p>",
                s$n_unsupplied[i]),
      # A bracket the citation check could not read names studies nobody checked.
      if (isTRUE(col("n_unparsed", i) > 0L))
        sprintf(paste0("<p class='flag'>%d citation(s) could not be read, so the studies they ",
                       "name were not checked.</p>"), s$n_unparsed[i]),
      if (isTRUE(col("n_truncated", i) > 0L))
        sprintf(paste0("<p class='flag'>%d response(s) for this section were cut off at the ",
                       "output cap, so the section may be incomplete.</p>"), s$n_truncated[i]),
      # An empty section is partial too, and "cites nothing" did not say why.
      if (blank)
        paste0("<p class='flag'>This section is empty: no usable reply came back for it, or ",
               "the run reached its call or cost ceiling before it.</p>"),
      if (isTRUE(col("claims_missed", i) > 0L))
        sprintf(paste0("<p class='flag'>%d of the %d claim(s) this section was given were not ",
                       "written up.</p>"), s$claims_missed[i], s$n_claims[i]),
      # The batch counts, when the synthesis recorded them. `col()` for one
      # saved before they were kept.
      if (isTRUE(col("lost_batches", i) > 0L))
        sprintf(paste0("<p class='flag'>%d batch(es) of studies failed, so the studies in them ",
                       "are not in this section.</p>"), as.integer(s$lost_batches[i])),
      if (isTRUE(col("capped_batches", i) > 0L))
        sprintf(paste0("<p class='flag'>%d batch(es) of studies were not sent: the run reached ",
                       "its call or cost ceiling, so the studies in them are not in this ",
                       "section.</p>"), as.integer(s$capped_batches[i])),
      if (isTRUE(col("merge_failed", i)))
        paste0("<p class='flag'>The batches of studies were drafted but not merged, so this ",
               "section is those drafts joined end to end rather than one account.</p>"))
    # sections$partial also counts a batch of studies that failed or was never
    # sent, and a merge that failed, which a synthesis made before those were
    # recorded does not break out. Read alone, the flags above passed such a
    # section as clean while the run had warned that studies were missing.
    if (isTRUE(col("partial", i)) && !length(fl)) {
      fl <- paste0("<p class='flag'>This section is marked partial: a batch of the studies it ",
                   "was written from failed or was not sent, or the batches could not be merged, ",
                   "so studies may be missing from it. The warnings the run raised say which.</p>")
    }
    c(sprintf("<%s>%s</%s>", lvl, esc(s$section[i]), lvl),
      sprintf("<p class='sub'>Must cover: %s</p>", esc(s$brief[i])),
      if (!blank) sprintf("<blockquote>%s</blockquote>", esc(s$text[i])),
      fl,
      if (!blank && isTRUE(col("n_cited", i) == 0L))
        "<p class='flag'>This section cites nothing.</p>",
      if (!is.null(ci) && nrow(ci))
        html_table(ci[, intersect(c("study", "document"), names(ci)), drop = FALSE],
                   numeric_cols = "study"))
  }
  lost <- synthesis$claims$lost %||% integer(0)
  # Honest citations the revision left as markers; see gr_synthesise(). print()
  # and the gr_synth_unrendered warning say so, and the report did not.
  # `%||%` for a synthesis made before the field existed.
  unrendered <- synthesis$unrendered %||% integer(0)
  drafts <- unlist(lapply(seq_len(nrow(s)), per), use.names = FALSE)
  c(if (revised) "<h2>What was written, and what it rests on</h2>"
    else "<h2>What was written, and what each section rests on</h2>",
    # Every section can be complete against claims that are not: claims drawn
    # from a third of the corpus give a review of a third of the corpus.
    if (length(lost))
      sprintf(paste0("<p class='flag'>Written from claims that %d of the %d studies contributed ",
                     "nothing to: their claims batch was cut off, failed or was not sent.</p>"),
              length(lost), nrow(synthesis$studies)),
    if (length(unrendered))
      sprintf(paste0("<p class='flag'>%s %s %s cited honestly but left as a [study N] marker ",
                     "in the revised text: the revision made that citation impossible to tell ",
                     "from one a section made without being given the study.</p>"),
              if (length(unrendered) == 1L) "Study" else "Studies",
              esc(and_words(as.character(unrendered))),
              if (length(unrendered) == 1L) "is" else "are"),
    if (revised) c(synthesis_published(synthesis), "<h3>Draft before revision</h3>",
                   paste0("<p class='sub'>The sections as first written, each with the checks ",
                          "that ran on it. The revision above was made from these.</p>"))
    else if (is.data.frame(rep_df) && nrow(rep_df))
      paste0("<p class='sub'>Revision passes ran and none was kept, so the sections below are ",
             "the text as published.</p>"),
    drafts,
    if (is.data.frame(rep_df) && nrow(rep_df)) c(
      "<h3>Revision passes</h3>",
      html_table(rep_df[, intersect(c("pass", "ran", "kept", "reason"), names(rep_df)),
                        drop = FALSE])),
    if (isTRUE(synthesis$skipped > 0L))
      sprintf(paste0("<p class='sub'>%d row(s) were left out of the write-up: a duplicate, a ",
                     "document that could not be read in full, or one with nothing ",
                     "extracted.</p>"),
              synthesis$skipped))
}

#' The published text of a synthesis whose revision was kept, and the studies
#' each of its headings cites, found again in the text the citation check
#' reads (`text_marked`), since no section's own table describes it.
#' @noRd
synthesis_published <- function(synthesis) {
  marked <- as_chr1(synthesis$text_marked, "")
  lines <- strsplit(marked, "\n", fixed = TRUE)[[1]]
  is_head <- grepl("^#{1,6}[[:space:]]", lines)
  group <- cumsum(is_head)
  heads <- c("(before the first heading)", trimws(sub("^#{1,6}[[:space:]]+", "", lines[is_head])))
  studies <- synthesis$studies
  rows <- lapply(sort(unique(group)), function(g) {
    ids <- cited_ids(paste(lines[group == g], collapse = "\n"), "study")
    if (!length(ids)) return(NULL)
    data.frame(heading = heads[g + 1L], study = ids,
               document = if (is.null(studies$document)) NA_character_
                          else as.character(studies$document)[match(ids, studies$study)],
               stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  unknown <- if (is.null(tab)) 0L else sum(!tab$study %in% studies$study)
  c(paste0("<p class='sub'>A revision pass was kept, so this is the text as published: the ",
           "draft below after revision. Its headings, and where each citation sits, are the ",
           "revision's.</p>"),
    sprintf("<blockquote class='passage'>%s</blockquote>", esc(as_chr1(synthesis$text, marked))),
    if (unknown)
      sprintf(paste0("<p class='flag'>%d citation(s) in the published text point at a row that ",
                     "does not exist.</p>"), unknown),
    if (is.null(tab)) "<p class='flag'>The published text cites nothing.</p>"
    else c("<p class='sub'>The studies each heading of the published text cites.</p>",
           html_table(tab, numeric_cols = "study",
                      flag = list(document = function(v) is.na(v)))))
}

#' @noRd
audit_cost <- function(screening = NULL, extraction = NULL, synthesis = NULL, reading = NULL,
                       claims = NULL) {
  # Named for the reader, not after the class. "gr_screening" is what the object
  # is called in the code and means nothing to the person the report is for.
  stages <- list(screening = screening, extraction = extraction,
                 claims = claims_cost_stage(claims), synthesis = synthesis, answer = reading)
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
    if ("claims" %in% names(traces))
      paste0("<p class='sub'>The claims row is the calls gr_claims() made and, where it ",
             "recorded them on the claims' trace, those gr_outline() made.</p>"),
    "<p class='sub'>Only requests that were sent are counted. A reply served from a cache",
    "cost nothing, however large its prompt.</p>",
    html_table(tab, numeric_cols = c("calls", "cached", "tokens_in", "tokens_out", "usd")))
}

#' The claims stage of the cost table, as a stage audit_cost() can read: its
#' own requests only, from the claims' trace, or NULL when there are none.
#'
#' gr_claims() and gr_outline() record their calls on `claims$trace`, and
#' gr_synthesise() keeps a trace of its own, so a claims-based review's
#' drawing, reconciling and outlining were in no row of the table. Not the
#' trace's own counters: gr_claims(trace = tr) records on `tr` itself, which
#' also holds whatever else the caller ran on it -- a screening or write-up
#' already in the table, counted twice. Its steps are picked out by label.
#' @noRd
claims_cost_stage <- function(claims) {
  if (!inherits(claims, "gr_claims") || !inherits(claims$trace, "gr_trace")) return(NULL)
  mine <- Filter(function(st) grepl("^(claims|outline)\\.", as_chr1(st$label, "")),
                 claims$trace$steps)
  req <- Filter(function(st) !identical(st$kind, "local") && !is.null(st$tokens), mine)
  if (!length(req)) return(NULL)
  tr <- gr_trace(meta = list(stage = "claims"))
  tr$steps <- req
  tr$calls <- length(req)
  tr$cached <- sum(vapply(req, function(st) isTRUE(st$cached), logical(1)))
  model_call <- !vapply(req, function(st) identical(st$kind, "embedding"), logical(1))
  tok <- function(k) sum(vapply(req[model_call], function(st) as_int1(st$tokens[[k]], 0L),
                                integer(1)))
  tr$tokens_in <- tok("input")
  tr$tokens_out <- tok("output")
  list(trace = tr)
}

#' @noRd
audit_caveats <- function(screening = TRUE, answer = FALSE, quotes = TRUE) {
  c("<h2>What this report does not establish</h2>",
    "<div class='note'>",
    if (quotes) c(
      "<p><strong>A verified quote is one that is in the document.</strong> The check confirms",
      "that the sentence credited with a value occurs in the chunk it was attributed to. It does",
      "not confirm that the sentence supports the value, and it does not confirm the value is",
      "right. A correct quote read wrongly looks exactly like a correct quote read rightly. What",
      "the check rules out is the quote having been invented, which is the failure that is",
      "otherwise invisible.</p>"),
    # What resolve_evidence_pages() does: it leaves the chunk's page on a span
    # it cannot place on one page. Said as it is -- "left without one" was
    # printed above a fabricated quote listed with page 1.
    "<p><strong>A page number is where the sentence is, not where the reasoning is.</strong>",
    "A span found on one page of the document is given that page. One found on several pages,",
    "or not found at all, keeps the page of the chunk it was credited to, which says where",
    "that chunk is, not where the sentence is: an unverified quote shown with a page was not",
    "found on it. A span from a chunk that runs across pages, and not placed, has no page.</p>",
    if (screening) c(
      "<p><strong>Screening saw what the excerpt showed.</strong> Where a document was",
      "truncated the decision was made on its opening; the screening table says which.</p>"),
    if (answer) c(
      "<p><strong>A passage shows where an answer came from, not that it is right.</strong>",
      "The passages are the ones the reader used. A highlighted number is a number from the",
      "answer found in the passage: it shows where to look, not that the passage supports the",
      "answer. An answer a reader wrote for one chunk is the model's account of that chunk,",
      "which is why it is labelled and not highlighted.</p>",
      "<p><strong>Not found means not found in what was read.</strong> A reader that picks a",
      "few chunks reports on those chunks, not on the whole document.</p>"),
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
  # A document fetched from the web is named by its address, and an error
  # about it quotes the address; either can carry the link's credentials.
  # Here, because every table of documents in the report passes through.
  for (nm in intersect(c("document", "duplicate_of"), names(df))) {
    if (is.character(df[[nm]]) || is.factor(df[[nm]])) df[[nm]] <- report_url(df[[nm]])
  }
  for (nm in intersect(c("error", "warnings"), names(df))) {
    if (is.character(df[[nm]]) || is.factor(df[[nm]])) df[[nm]] <- report_url_text(df[[nm]])
  }
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


#' How good the screening is, when somebody measured it.
#'
#' gr_calibrate() computed sensitivity, specificity and kappa and there was
#' nowhere to put them: the number a reviewer actually asks about -- how many
#' eligible studies the screener threw away -- had to be copied into a methods
#' section by hand, which is the one place it cannot be checked against the run.
#' @noRd
audit_calibration <- function(calibration) {
  if (!inherits(calibration, "gr_calibration")) return(character(0))
  m <- calibration$metrics
  tab <- data.frame(
    measure = m$metric,
    estimate = ifelse(is.na(m$estimate), "--", sprintf("%.1f%%", 100 * m$estimate)),
    interval = ifelse(is.na(m$lower) | is.na(m$upper), "--",
                      sprintf("%.1f%% to %.1f%%", 100 * m$lower, 100 * m$upper)),
    n = m$n, stringsAsFactors = FALSE)
  head <- sprintf(paste0("<p class='sub'>%d record(s) screened by hand, %d of them eligible, ",
                         "sampled from: %s.</p>"),
                  calibration$n, calibration$n_positives, esc(as_chr1(calibration$frame$of, "all")))
  warn <- if (!isTRUE(calibration$adequate)) sprintf(
    paste0("<p class='flag'>Only %d eligible record(s) in the sample, below the %s this ",
           "calibration asks for. The intervals are too wide to conclude much.</p>"),
    # %s and format(), not %d: min_positives is a double now, and may be Inf --
    # "never adequate" -- which %d refuses outright, taking the report with it.
    calibration$n_positives, format(calibration$min_positives))
  kap <- if (!is.na(calibration$kappa))
    sprintf("<p><b>Cohen's kappa:</b> %.2f</p>", calibration$kappa)
  proj <- if (!is.null(calibration$projected)) {
    pr <- calibration$projected
    sprintf(paste0("<p class='flag'>Across all %s excluded record(s), that rate implies about ",
                   "%.0f eligible stud%s lost (%.0f to %.0f).</p>"),
            format(pr$frame_n), pr$lost, if (round(pr$lost) == 1) "y" else "ies",
            pr$lower, pr$upper)
  }
  miss <- if (nrow(calibration$missed)) sprintf(
    "<p class='flag'>%d eligible stud%s excluded by the screener: %s.</p>",
    nrow(calibration$missed), if (nrow(calibration$missed) == 1L) "y was" else "ies were",
    esc(paste(report_url(utils::head(calibration$missed$document, 8)), collapse = ", ")))
  c("<h2>How good the screening is</h2>", head, html_table(tab), kap, proj, warn, miss)
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

# --- an answer, and the passages behind it --------------------------------

#' The question a gr_answer or gr_corpus was asked, or NULL.
#' @noRd
report_question <- function(x) {
  if (inherits(x, "gr_answer")) return(x$question)
  if (inherits(x, "gr_corpus")) return(x$trace$meta[["question", exact = TRUE]])
  NULL
}

#' Does an answer, or any answer in a corpus, rest on quotations a reader
#' copied out? Only then does the report explain what verifying one means.
#' @noRd
answer_has_quotes <- function(x) {
  has <- function(a) inherits(a, "gr_answer") && is.data.frame(a$evidence) &&
    any(as.character(a$evidence$kind) %in% "extracted")
  if (inherits(x, "gr_corpus")) return(any(vapply(x$answers %||% list(), has, logical(1))))
  has(x)
}

#' The answer sections: one answer, or every document of a corpus.
#' @noRd
audit_answer <- function(x) {
  if (inherits(x, "gr_corpus")) return(audit_corpus(x))
  if (!inherits(x, "gr_answer")) return(NULL)
  c("<h2>The answer</h2>", answer_summary(x),
    "<h2>Where it came from</h2>", answer_passages(x),
    "<h2>Every request</h2>", request_table(x$trace))
}

#' A document's name for the report: the file name, not the folders above it,
#' which say nothing about the answer and may say something about the machine.
#' @noRd
report_doc_name <- function(src) {
  src <- as_chr1(src, NA_character_)
  if (is.na(src) || identical(src, "<inline text>")) return(src)
  # A web address in full, since its host and path are what name it -- but not
  # its credentials; see report_url().
  if (is_url(src)) return(report_url(src))
  basename(src)
}

#' A web address as the report may show it: without a user name and password,
#' and with its query string and fragment replaced by a short fingerprint.
#'
#' Presigned and tokenised links (S3, GCS and Azure signatures, download
#' tokens) carry their credentials in the query, and the address was copied
#' whole into the document column, the headings and the fetch errors of a
#' report written to be handed to other people. The fingerprint keeps two
#' addresses that differ only in the query apart without showing either.
#'
#' Applied to an address with a scheme, and to one without a scheme that
#' starts with a dotted host followed by a path or a query, which is how a
#' corpus labels a fetched document (corpus_label() drops the scheme). There
#' only the query is hidden, not a "#" and what follows it, which in a file
#' name is part of the name. Anything else, a file name included, is returned
#' as it is.
#' @noRd
report_url <- function(x) {
  x <- as.character(x)
  out <- x
  # Vectorised, and one fingerprint per distinct query: every table of
  # documents in the report passes through here, some with thousands of rows.
  m <- regexpr("^[[:space:]]*https?://", x, ignore.case = TRUE)
  n <- ifelse(!is.na(m) & m > 0L, attr(m, "match.length"), 0L)
  rest <- substring(x, n + 1L)
  host <- "^([^/?#@[:space:]]+@)?[A-Za-z0-9-]+(\\.[A-Za-z0-9-]+)+(:[0-9]+)?[/?]"
  hit <- !is.na(x) & (n > 0L | grepl(host, rest, perl = TRUE))
  if (!any(hit)) return(out)
  scheme <- substr(x[hit], 1L, n[hit])
  # The user name and password, which sit before an "@" ahead of the path.
  r <- sub("^[^/?#@[:space:]]*@", "", rest[hit], perl = TRUE)
  at <- ifelse(n[hit] > 0L, regexpr("[?#]", r), regexpr("?", r, fixed = TRUE))
  q <- at > 0L
  if (any(q)) {
    hidden <- substring(r[q], at[q])
    u <- unique(hidden)
    fp <- vapply(u, function(h) substr(gr_hash(h), 1L, 6L), character(1), USE.NAMES = FALSE)
    r[q] <- paste0(substr(r[q], 1L, at[q] - 1L),
                   sprintf("?[query hidden %s]", fp[match(hidden, u)]))
  }
  out[hit] <- paste0(scheme, r)
  out
}

#' `x` with every web address in it shown as report_url() shows one: for error
#' and warning text, which quotes the address it failed to fetch.
#' @noRd
report_url_text <- function(x) {
  x <- as.character(x)
  has <- !is.na(x) & grepl("https?://", x, ignore.case = TRUE)
  if (!any(has)) return(x)
  m <- gregexpr("https?://[^[:space:]'\"<>]+", x[has], ignore.case = TRUE)
  found <- regmatches(x[has], m)
  shown <- report_url(unlist(found, use.names = FALSE))
  regmatches(x[has], m) <- unname(split(shown, factor(rep(seq_along(found), lengths(found)),
                                                      levels = seq_along(found))))
  x
}

#' Rows of label and value, leaving out values nobody recorded.
#' @noRd
fact_table <- function(labels, values) {
  keep <- !is.na(values) & nzchar(values)
  if (!any(keep)) return(NULL)
  c("<table>", sprintf("<tr><th>%s</th><td>%s</td></tr>", esc(labels[keep]), esc(values[keep])),
    "</table>")
}

#' The failed requests behind an answer, counted as failed_note() counts them
#' for a corpus row: the reader's own counts or the trace's failures, whichever
#' is larger, since the two overlap, and not the ones a fallback recovered.
#'
#' `answer_error` is the reader's record that its answering request failed or
#' was never sent (`notes$error`), and `any_ok` whether any model request on
#' the trace came back at all. The trace may hold other runs when the caller
#' shared one; their failures are counted too, since nothing on it says which
#' run a step belonged to, and a false flag is the safe side of that doubt.
#' @noRd
answer_failures <- function(x) {
  n <- as.list(x$notes %||% list())
  num <- function(k) as_num1(n[[k, exact = TRUE]], 0)
  tr <- if (inherits(x$trace, "gr_trace")) x$trace else NULL
  errs <- if (is.null(tr)) list() else tr$errors
  open <- Filter(Negate(is_recovered_error), errs)
  err <- as_chr1(n[["error", exact = TRUE]], "")
  steps <- if (is.null(tr)) list() else
    Filter(function(st) !identical(st$kind, "local") && !identical(st$kind, "embedding") &&
             !is.null(st$tokens), tr$steps)
  list(failed = max(num("failed_calls") + num("scoring_failures") + num("failed_summaries") +
                      isTRUE(n[["failed_call", exact = TRUE]]), length(open)),
       recovered = length(errs) - length(open),
       first = if (nzchar(err) || !length(open)) err else as_chr1(open[[1]]$error, ""),
       answer_error = nzchar(err),
       any_steps = length(steps) > 0L,
       any_ok = any(vapply(steps, function(st) isTRUE(st$ok), logical(1))))
}

#' The answer, whether it is complete, and what it took. `listed` when the
#' page goes on to list the requests, as it does for one answer.
#' @noRd
answer_summary <- function(x, listed = TRUE) {
  f <- answer_failures(x)
  # "Not found" is a finding about the document only when something was read.
  # When the request that would have answered failed or was never sent, the
  # reader's sentinel is no answer at all, and the page said "Not found in the
  # part of the document that was read" of a document nothing had read.
  said <- if (is_not_found(x$answer) && (f$answer_error || (f$any_steps && !f$any_ok))) {
    paste0("<p><strong>No answer: the request that would have given one failed or was not ",
           "sent, so this says nothing about whether the document holds one.</strong></p>")
  } else if (is_not_found(x$answer)) {
    sprintf("<p><strong>%s</strong></p>", esc(not_found_wording(x)))
  } else sprintf("<blockquote class='passage'>%s</blockquote>", esc(x$answer))
  # Not from `partial` alone. Several readers leave it FALSE after a request
  # failed, and the green "every request succeeded" line then sat above the
  # table flagging the failed request. The trace is what says whether one did.
  status <- if (isTRUE(x$partial)) {
    why <- partial_reasons(x)
    sprintf("<p class='flag'>Partial: %s.</p>",
            esc(report_url_text(if (length(why)) paste(why, collapse = "; ")
                                else "see the answer's notes")))
  } else if (f$failed > 0) {
    sprintf(paste0("<p class='flag'>The reader did not mark this answer partial, but %s ",
                   "request(s) failed%s, so it may rest on less than the reader meant to ",
                   "use.%s</p>"),
            format(f$failed, scientific = FALSE),
            if (nzchar(f$first)) esc(sprintf(" (first error: %s)",
                                             report_url_text(substr(f$first, 1, 200)))) else "",
            if (listed) " Each request is listed below." else "")
  } else if (f$recovered > 0) {
    sprintf(paste0("<p class='ok'>Not partial: the reader reported nothing it chose to read as ",
                   "left out, and the %d request(s) that failed were recovered by a ",
                   "fallback.</p>"), as.integer(f$recovered))
  } else {
    paste0("<p class='ok'>Not partial: no request failed, and the reader reported nothing it ",
           "chose to read as left out.</p>")
  }
  tr <- x$trace
  recipe <- as_chr1(x$recipe, NA_character_)
  auto <- as.list(x$notes %||% list())[["auto_recipe", exact = TRUE]]
  if (!is.na(recipe) && !is.null(auto)) recipe <- sprintf("%s (chosen by \"auto\")", recipe)
  used <- unique(x$chunks_used %||% integer(0))
  facts <- fact_table(
    c("Document", "Recipe", "Reader", "Chunks", "Chunks used", "Requests", "Cost"),
    c(report_doc_name(x$document$source), recipe, as_chr1(x$reader, NA_character_),
      as_chr1(x$segmentation$n, NA_character_),
      as.character(length(used)),
      if (inherits(tr, "gr_trace"))
        sprintf("%d%s", as.integer(tr$calls),
                if (isTRUE(tr$cached > 0L)) sprintf(" (%d from a cache)", as.integer(tr$cached))
                else "")
      else NA_character_,
      if (inherits(tr, "gr_trace")) format_trace_cost(tr) else NA_character_))
  w <- x[["warnings", exact = TRUE]] %||% character(0)
  c(said, status, facts,
    if (length(w)) c(sprintf("<p class='flag'>%d warning(s) while reading:</p>", length(w)),
                     "<ul>", sprintf("<li>%s</li>", esc(report_url_text(unname(w)))), "</ul>"))
}

#' The passages behind an answer, grouped by chunk, in document order.
#' @noRd
answer_passages <- function(x) {
  ev <- x$evidence
  if (!is.data.frame(ev) || !nrow(ev)) {
    return("<p class='sub'>The reader recorded no passages for this answer.</p>")
  }
  col <- function(nm) if (is.null(ev[[nm]])) rep(NA, nrow(ev)) else ev[[nm]]
  page <- suppressWarnings(as.numeric(col("page")))
  chunk <- suppressWarnings(as.numeric(col("chunk_id")))
  # By chunk, which segmenters number in reading order, then by page. Not page
  # first: a chunk that runs across a page break has no single page, and would
  # be put after every chunk that has one. A row with neither keeps the place
  # the reader gave it, after the rest.
  o <- order(is.na(chunk), chunk, is.na(page), page, seq_len(nrow(ev)))
  key <- ifelse(is.na(chunk), paste0("row", seq_len(nrow(ev))), paste0("chunk", chunk))
  groups <- unique(key[o])
  cited <- cited_chunks(x$answer)
  nums <- answer_numbers(x$answer)
  kind <- as.character(col("kind"))
  quoted <- !is.na(kind) & kind == "extracted"
  unver <- quoted & !is.na(col("verified")) & !isTRUE_vec(col("verified"))
  # An extracted value's quote can be in the chunk word for word and still not
  # verify, by not stating the value (cited_verbatim()): not "not found".
  there <- sum(unver & cited_verbatim(ev))
  bad <- sum(unver) - there
  c(sprintf("<p class='sub'>%d passage(s) from %d chunk(s), in the order they appear in the document.%s%s</p>",
            nrow(ev), length(groups),
            if (bad) sprintf(" <span class='flag'>%d quotation(s) could not be found in the chunk they cite.</span>", bad)
            else "",
            if (there) sprintf(paste0(" <span class='flag'>%d quotation(s) found in the chunk they ",
                                      "cite do not state the value they are cited for.</span>"), there)
            else ""),
    unlist(lapply(groups, function(g) {
      rows <- ev[o[key[o] == g], , drop = FALSE]
      evidence_card(rows, cited, nums)
    }), use.names = FALSE))
}

#' One chunk's passages: where it is, the text with what supports the answer
#' marked, and anything that could not be placed.
#' @noRd
evidence_card <- function(rows, cited, nums) {
  get <- function(nm) if (is.null(rows[[nm]])) rep(NA, nrow(rows)) else rows[[nm]]
  kind <- as.character(get("kind"))
  kind[is.na(kind)] <- "verbatim"
  text <- vapply(get("text"), function(t) as_chr1(t, ""), character(1), USE.NAMES = FALSE)
  id <- suppressWarnings(as.integer(get("chunk_id")[1]))
  pages <- sort(unique(stats::na.omit(suppressWarnings(as.numeric(get("page"))))))
  secs <- unique(stats::na.omit(as.character(get("section"))))
  secs <- secs[nzchar(trimws(secs))]
  score <- suppressWarnings(as.numeric(get("score")))
  score <- if (any(!is.na(score))) max(score, na.rm = TRUE) else NA_real_
  where <- c(if (length(pages)) sprintf("%s %s", if (length(pages) == 1L) "page" else "pages",
                                        paste(format(pages, trim = TRUE), collapse = ", ")),
             if (length(secs)) sprintf("section \"%s\"", paste(secs, collapse = "\", \"")),
             if (!is.na(id)) sprintf("chunk %d", id),
             if (!is.na(score)) sprintf("relevance %.2f", score),
             if (!is.na(id) && id %in% cited) "cited in the answer")
  where <- paste(where, collapse = ", ")
  if (nzchar(where)) where <- paste0(toupper(substr(where, 1, 1)), substring(where, 2))

  quoted <- kind == "extracted"
  src <- vapply(get("source_text"), function(t) as_chr1(t, NA_character_), character(1),
                USE.NAMES = FALSE)
  passage <- if (any(quoted & !is.na(src))) src[quoted & !is.na(src)][1]
             else if (any(kind == "verbatim")) text[kind == "verbatim"][1]
             else NA_character_
  # One encoding for the searches and the cuts made at the positions they find.
  if (!is.na(passage)) passage <- to_utf8(passage)
  spans <- list()
  unplaced <- character(0)
  if (any(quoted)) {
    verified <- as.logical(get("verified"))
    match <- suppressWarnings(as.numeric(get("match")))
    unstated <- cited_verbatim(rows)
    folded <- if (!is.na(passage)) normalised_with_map(passage)
    for (i in which(quoted)) {
      # Only a quotation the check found is marked, so the page and
      # gr_verify_evidence() cannot disagree about which ones are there.
      sp <- if (isTRUE(verified[i])) quote_span(text[i], folded)
      if (!is.null(sp)) { spans[[length(spans) + 1L]] <- sp; next }
      unplaced <- c(unplaced, if (isTRUE(verified[i]))
        sprintf("<p class='sub'>Quoted, and found in this chunk, but not placed in the text shown: &ldquo;%s&rdquo;</p>",
                esc(text[i]))
      # In the chunk word for word, and not verified: an extracted value its
      # quote does not state. "Not found ... 100%" said the opposite of both.
      else if (identical(verified[i], FALSE) && unstated[i])
        sprintf(paste0("<p class='flag'>Quoted, and found in this chunk, but it does not state ",
                       "the value it is cited for: &ldquo;%s&rdquo; (not in any form the check ",
                       "reads)</p>"),
                esc(text[i]))
      else if (identical(verified[i], FALSE))
        sprintf("<p class='flag'>Quoted, but not found in this chunk%s: &ldquo;%s&rdquo;</p>",
                if (!is.na(match[i])) sprintf(" (the longest run of its words that is: %d%%)",
                                              as.integer(round(100 * match[i]))) else "",
                esc(text[i]))
      else sprintf("<p class='sub'>Quoted, not checked against the document: &ldquo;%s&rdquo;</p>",
                   esc(text[i])))
    }
  }
  # Numbers only where nothing was quoted: a quotation already says which part
  # of the chunk the answer rests on.
  if (!length(spans) && !any(quoted)) spans <- number_spans(passage, nums)
  said <- text[kind == "answer" & nzchar(text)]
  c("<div class='card'>",
    if (nzchar(where)) sprintf("<p class='where'>%s</p>", esc(where)),
    if (!is.na(passage)) sprintf("<div class='passage'>%s</div>", excerpt_html(passage, spans)),
    unplaced,
    if (length(said)) c(
      "<p class='sub'>The reader's answer from this chunk. These are the model's words, not the document's.</p>",
      sprintf("<blockquote class='passage'>%s</blockquote>", esc(said))),
    "</div>")
}

#' `passage` folded the way normalise_for_match() folds text, with a map from
#' each folded character back to the character it came from.
#'
#' Folding keeps each character one character, and a run of space becomes one
#' space whose place is the run's first character. So a quotation found in the
#' folded text by an exact search is found exactly where gr_verify_evidence()
#' found it, and the map gives its place in the original.
#' @noRd
normalised_with_map <- function(passage) {
  ch <- strsplit(to_utf8(passage), "", fixed = TRUE)[[1]]
  if (!length(ch)) return(list(text = "", map = integer(0)))
  ch <- fold_for_match(ch)
  sp <- grepl("[[:space:]]", ch, perl = TRUE)
  keep <- !(sp & c(FALSE, sp[-length(sp)]))
  out <- ch[keep]
  out[sp[keep]] <- " "
  list(text = paste(out, collapse = ""), map = which(keep))
}

#' Where a quotation sits in the passage `folded` came from, as c(start, end)
#' in characters, or NULL when it is not there.
#' @noRd
quote_span <- function(quote, folded) {
  if (is.null(folded) || !nzchar(folded$text)) return(NULL)
  q <- trim_quote_edges(normalise_for_match(as_chr1(quote, "")))
  if (!nzchar(q)) return(NULL)
  at <- regexpr(q, folded$text, fixed = TRUE)
  if (at < 1L) return(NULL)
  c(folded$map[at], folded$map[at + nchar(q) - 1L])
}

#' The numbers an answer states, longest first, leaving out single digits and
#' the chunk numbers in its citations.
#' @noRd
answer_numbers <- function(text) {
  text <- as_chr1(text, "")
  if (is_not_found(text)) return(character(0))
  text <- gsub(cite_pattern("chunk"), " ", text, perl = TRUE, ignore.case = TRUE)
  m <- regmatches(text, gregexpr("[0-9]+(?:[.,][0-9]+)*", text, perl = TRUE))[[1]]
  m <- unique(m[nchar(m) >= 2L])
  m[order(-nchar(m), m)]
}

#' Where those numbers occur in `passage`, whole: 45.2 is not marked inside
#' 145.2 or 45.25.
#' @noRd
number_spans <- function(passage, nums) {
  passage <- as_chr1(passage, NA_character_)
  if (!length(nums) || is.na(passage) || !nzchar(passage)) return(list())
  alt <- paste(gsub(".", "\\.", nums, fixed = TRUE), collapse = "|")
  pat <- sprintf("(?<![0-9])(?<![0-9][.,])(?:%s)(?![0-9]|[.,][0-9])", alt)
  m <- tryCatch(gregexpr(pat, passage, perl = TRUE)[[1]], error = function(e) -1L)
  if (m[1] < 1L) return(list())
  len <- attr(m, "match.length")
  lapply(seq_along(m), function(i) c(as.integer(m[i]), as.integer(m[i] + len[i] - 1L)))
}

#' `text` as HTML with the character ranges in `spans` wrapped in <mark>.
#' Overlapping ranges are merged; everything is escaped.
#' @noRd
mark_html <- function(text, spans) {
  if (!length(spans)) return(esc(text))
  s <- vapply(spans, `[[`, integer(1), 1L)
  e <- vapply(spans, `[[`, integer(1), 2L)
  o <- order(s)
  s <- s[o]; e <- e[o]
  out <- character(0)
  pos <- 1L
  i <- 1L
  while (i <= length(s)) {
    start <- s[i]; end <- e[i]
    while (i < length(s) && s[i + 1L] <= end + 1L) { i <- i + 1L; end <- max(end, e[i]) }
    out <- c(out, esc(substr(text, pos, start - 1L)), "<mark>", esc(substr(text, start, end)),
             "</mark>")
    pos <- end + 1L
    i <- i + 1L
  }
  paste0(c(out, esc(substr(text, pos, nchar(text)))), collapse = "")
}

#' A passage as HTML, cut to the parts around what is marked when it is long.
#'
#' A recipe that sends the whole document in one request has the whole document
#' as its one passage. Shown in full, the report would repeat the document; so a
#' long passage keeps `context` characters either side of each mark, and one
#' with nothing marked keeps its opening. Cuts are shown as "[...]".
#' @noRd
excerpt_html <- function(text, spans, limit = 6000L, context = 400L) {
  n <- nchar(text)
  if (n <= limit) return(mark_html(text, spans))
  gap <- "<span class='sub'> [...] </span>"
  if (!length(spans)) {
    return(paste0(esc(substr(text, 1L, limit)),
                  sprintf("<span class='sub'> [... %d more characters]</span>", n - limit)))
  }
  s <- vapply(spans, `[[`, integer(1), 1L)
  e <- vapply(spans, `[[`, integer(1), 2L)
  o <- order(s)
  s <- s[o]; e <- e[o]
  ws <- pmax(1L, s - as.integer(context))
  we <- pmin(n, e + as.integer(context))
  wins <- list()
  cs <- ws[1]; ce <- we[1]
  for (i in seq_along(ws)[-1]) {
    if (ws[i] <= ce + 1L) ce <- max(ce, we[i])
    else { wins[[length(wins) + 1L]] <- c(cs, ce); cs <- ws[i]; ce <- we[i] }
  }
  wins[[length(wins) + 1L]] <- c(cs, ce)
  parts <- vapply(wins, function(w) {
    inside <- which(s >= w[1] & e <= w[2])
    mark_html(substr(text, w[1], w[2]),
              lapply(inside, function(k) c(s[k] - w[1] + 1L, e[k] - w[1] + 1L)))
  }, character(1))
  paste0(if (wins[[1]][1] > 1L) gap, paste(parts, collapse = gap),
         if (wins[[length(wins)]][2] < n) gap)
}

#' One row per request, from as.data.frame() on the trace, without the prompts
#' and replies, which would put the document in the report again.
#' @noRd
request_table <- function(trace) {
  if (!inherits(trace, "gr_trace")) return("<p class='sub'>No trace was kept.</p>")
  df <- as.data.frame(trace)
  if (!nrow(df)) return("<p class='sub'>No requests were made.</p>")
  # A trace written before requests were timed has no times: say so rather
  # than add them up to nothing.
  timed <- !is.na(df$seconds)
  took <- if (!any(timed)) "their times were not recorded"
          else sprintf("%s seconds in all%s", format(round(sum(df$seconds[timed]), 1), nsmall = 1),
                       if (all(timed)) "" else sprintf(" for the %d that were timed", sum(timed)))
  df$usd <- ifelse(is.na(df$usd), NA_character_, formatC(df$usd, format = "f", digits = 6))
  df$seconds <- round(df$seconds, 2)
  keep <- c("step", "stage", "model", "ok", "cached", "tokens_in", "tokens_out", "usd",
            "seconds", "error")
  c(sprintf(paste0("<p class='sub'>%d request(s), %s. The prompts and replies ",
                   "are in <code>as.data.frame(answer$trace)</code>.</p>"),
            nrow(df), took),
    html_table(df[, keep, drop = FALSE],
               numeric_cols = c("step", "tokens_in", "tokens_out", "usd", "seconds"),
               flag = list(ok = function(v) !v, error = function(v) !is.na(v))))
}

#' Every document of a corpus: one row each, then each answer and its passages.
#' @noRd
audit_corpus <- function(x) {
  s <- x$summary
  if (!is.data.frame(s) || !nrow(s)) return(NULL)
  # A copy found on a resumed run is "restored", not "duplicate"; `duplicate_of`
  # is what says it is a copy either way.
  dup_of <- if (is.null(s$duplicate_of)) rep(NA_character_, nrow(s)) else as.character(s$duplicate_of)
  keep <- intersect(c("document", "status", if (any(!is.na(dup_of))) "duplicate_of", "answer",
                      "partial", "reader", "calls", "cost_usd", "seconds", "error", "warnings"),
                    names(s))
  tab <- s[, keep, drop = FALSE]
  if (!is.null(tab$answer)) {
    nf <- if (is.null(s$not_found)) rep(FALSE, nrow(s)) else isTRUE_vec(s$not_found)
    a <- as.character(tab$answer)
    a <- ifelse(!is.na(a) & nchar(a) > 200L, paste0(substr(a, 1, 200), " [...]"), a)
    tab$answer <- ifelse(nf, "not found", a)
  }
  if (!is.null(tab$cost_usd)) {
    tab$cost_usd <- ifelse(is.na(tab$cost_usd), NA_character_,
                           formatC(as.numeric(tab$cost_usd), format = "f", digits = 4))
  }
  answers <- x$answers %||% list()
  per <- function(i) {
    lab <- as.character(s$document[i])
    head <- sprintf("<h3>%s</h3>", esc(report_url(lab)))
    status <- as.character(s$status[i])
    if (identical(status, "duplicate") || !is.na(dup_of[i])) {
      return(c(head, sprintf("<p class='sub'>Same text as %s, which is shown there.</p>",
                             esc(report_url(as_chr1(dup_of[i], "an earlier document"))))))
    }
    a <- answers[[lab]]
    if (!inherits(a, "gr_answer")) {
      return(c(head, sprintf("<p class='sub'>No answer: %s.</p>", esc(switch(status,
        skipped = "the run's ceiling was reached before this document",
        failed = report_url_text(as_chr1(s$error[i], "it could not be read")),
        "none was kept")))))
    }
    c(head, answer_summary(a, listed = FALSE), "<h4>Where it came from</h4>", answer_passages(a))
  }
  c("<h2>Every document</h2>",
    sprintf("<p class='sub'>%d document(s). Costs are what each document cost when it was read; a restored row keeps the figure from the run that read it.</p>",
            nrow(s)),
    html_table(tab, numeric_cols = intersect(c("calls", "cost_usd", "seconds"), names(tab)),
               flag = list(status = function(v) v %in% c("failed", "skipped"),
                           partial = function(v) isTRUE_vec(v))),
    # Said only when there were answers to keep: a run in which every document
    # failed has none either way, and each row says why.
    if (!length(answers) && any(s$status %in% c("ok", "restored", "duplicate")))
      "<p class='sub'>The answers were not kept (<code>keep_answers = FALSE</code>), so their passages cannot be shown.</p>"
    else c("<h2>Each answer and where it came from</h2>",
           unlist(lapply(seq_len(nrow(s)), per), use.names = FALSE)))
}

#' Show the report: the RStudio viewer can display files under tempdir(), and a
#' browser the rest.
#' @noRd
audit_open <- function(path) {
  full <- normalizePath(path, winslash = "/", mustWork = FALSE)
  tmp <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  viewer <- getOption("viewer")
  if (is.function(viewer) && startsWith(full, paste0(tmp, "/"))) viewer(full)
  else open_in_browser(full)
  invisible(NULL)
}

#' @noRd
open_in_browser <- function(path) utils::browseURL(path)
