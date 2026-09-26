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
    add("values unsupported", sum(t$n_unverified, na.rm = TRUE),
        "no verbatim span in the chunk cited")
  }
  if (inherits(claims, "gr_claims")) {
    cw <- claims$claims
    add("claims drawn", nrow(cw), "statements about the literature, each attached to studies")
    add("  contested", sum(cw$n_contradict > 0L), "studies on both sides")
    add("  unexplained", sum(cw$n_contradict > 0L & is.na(cw$moderator)),
        "contested with nothing in the table to explain the split")
    add("  on one study", sum(cw$n_support == 1L), "no replication in this corpus")
    add("claims dropped", nrow(claims$dropped),
        "verification removed them; see the claims object's $dropped")
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
    audit_protocol(protocol, extraction),
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
               reading = answer),
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
  if (!inherits(cm, "gr_claims") || !nrow(cm$claims)) return(NULL)
  sec <- attr(synthesis$outline, "claims")
  tab <- cm$claims[, c("claim_id", "claim", "kind", "moderator", "scope",
                       "n_support", "n_contradict"), drop = FALSE]
  tab$section <- if (is.null(sec)) NA_character_ else
    sec$section[match(tab$claim_id, sec$claim_id)]
  sup <- cm$support
  docs <- cm$studies$document[match(sup$study, cm$studies$study)]
  detail <- data.frame(claim_id = sup$claim_id, study = sup$study, role = sup$role,
                       document = docs, stringsAsFactors = FALSE)
  contested <- sum(tab$n_contradict > 0L)
  open <- sum(tab$n_contradict > 0L & is.na(tab$moderator))
  lone <- sum(tab$n_support == 1L)
  c("<h2>What the review claims, and what each claim rests on</h2>",
    sprintf(paste0("<p class='sub'>%d claim(s) over %d study/studies. %d contested, %s. ",
                   "%d resting on a single study.</p>"),
            nrow(tab), nrow(cm$studies), contested,
            if (open) sprintf("<span class='flag'>%d of those with nothing in the table to explain the disagreement</span>", open)
            else "<span class='ok'>each with a distinguishing field named</span>",
            lone),
    html_table(tab, numeric_cols = c("claim_id", "n_support", "n_contradict"),
               flag = list(n_support = function(v) suppressWarnings(as.numeric(v)) <= 1)),
    "<h3>Every claim, study by study</h3>",
    html_table(detail, numeric_cols = c("claim_id", "study"),
               flag = list(role = function(v) v == "contradicts")),
    if (nrow(cm$dropped))
      c("<h3>Dropped in verification</h3>",
        sprintf(paste0("<p class='sub'>A claims table that looks thin has to be tellable from a ",
                       "literature that is, so what verification removed is printed too.</p>")),
        html_table(cm$dropped)))
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
      # A real row the section was never shown: the model cannot have read it
      # there, so the citation is as unsupported as one to no row at all. The
      # section is already partial for it; without this the report said nothing.
      # `is.null()` for a synthesis saved before the column existed.
      if (!is.null(s$n_unsupplied) && isTRUE(s$n_unsupplied[i] > 0L))
        sprintf("<p class='flag'>%d citation(s) point at a study this section was not given.</p>",
                s$n_unsupplied[i]),
      # A bracket the citation check could not read names studies nobody checked.
      if (!is.null(s$n_unparsed) && isTRUE(s$n_unparsed[i] > 0L))
        sprintf(paste0("<p class='flag'>%d citation(s) could not be read, so the studies they ",
                       "name were not checked.</p>"), s$n_unparsed[i]),
      if (!is.null(s$n_truncated) && isTRUE(s$n_truncated[i] > 0L))
        sprintf(paste0("<p class='flag'>%d response(s) for this section were cut off at the ",
                       "output cap, so the section may be incomplete.</p>"), s$n_truncated[i]),
      if (s$n_cited[i] == 0L)
        "<p class='flag'>This section cites nothing.</p>",
      if (!is.null(s$claims_missed) && s$claims_missed[i] > 0L)
        sprintf(paste0("<p class='flag'>%d of the %d claim(s) this section was given were not ",
                       "written up.</p>"), s$claims_missed[i], s$n_claims[i]),
      if (!is.null(ci) && nrow(ci))
        html_table(ci[, intersect(c("study", "document"), names(ci)), drop = FALSE],
                   numeric_cols = "study"))
  }
  c("<h2>What was written, and what each section rests on</h2>",
    unlist(lapply(seq_len(nrow(s)), per), use.names = FALSE),
    if (isTRUE(synthesis$skipped > 0L))
      sprintf(paste0("<p class='sub'>%d row(s) were left out of the write-up: a duplicate, a ",
                     "document that could not be read in full, or one with nothing ",
                     "extracted.</p>"),
              synthesis$skipped))
}

#' @noRd
audit_cost <- function(screening = NULL, extraction = NULL, synthesis = NULL, reading = NULL) {
  # Named for the reader, not after the class. "gr_screening" is what the object
  # is called in the code and means nothing to the person the report is for.
  stages <- list(screening = screening, extraction = extraction, synthesis = synthesis,
                 answer = reading)
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
    "<p class='sub'>Only requests that were sent are counted. A reply served from a cache",
    "cost nothing, however large its prompt.</p>",
    html_table(tab, numeric_cols = c("calls", "cached", "tokens_in", "tokens_out", "usd")))
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
    "<p><strong>A page number is where the sentence is, not where the reasoning is.</strong>",
    "Spans found on several pages, or not found at all, are left without one rather than",
    "given the likeliest.</p>",
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
    esc(paste(utils::head(calibration$missed$document, 8), collapse = ", ")))
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
  if (is.na(src) || is_url(src) || identical(src, "<inline text>")) return(src)
  basename(src)
}

#' Rows of label and value, leaving out values nobody recorded.
#' @noRd
fact_table <- function(labels, values) {
  keep <- !is.na(values) & nzchar(values)
  if (!any(keep)) return(NULL)
  c("<table>", sprintf("<tr><th>%s</th><td>%s</td></tr>", esc(labels[keep]), esc(values[keep])),
    "</table>")
}

#' The answer, whether it is complete, and what it took.
#' @noRd
answer_summary <- function(x) {
  said <- if (is_not_found(x$answer)) sprintf("<p><strong>%s</strong></p>", esc(not_found_wording(x)))
          else sprintf("<blockquote class='passage'>%s</blockquote>", esc(x$answer))
  status <- if (isTRUE(x$partial)) {
    why <- partial_reasons(x)
    sprintf("<p class='flag'>Partial: %s.</p>",
            esc(if (length(why)) paste(why, collapse = "; ") else "see the answer's notes"))
  } else {
    "<p class='ok'>Not partial: every request succeeded and nothing the reader chose to read was left out.</p>"
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
                     "<ul>", sprintf("<li>%s</li>", esc(unname(w))), "</ul>"))
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
  bad <- sum(quoted & !is.na(col("verified")) & !isTRUE_vec(col("verified")))
  c(sprintf("<p class='sub'>%d passage(s) from %d chunk(s), in the order they appear in the document.%s</p>",
            nrow(ev), length(groups),
            if (bad) sprintf(" <span class='flag'>%d quotation(s) could not be found in the chunk they cite.</span>", bad)
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
    folded <- if (!is.na(passage)) normalised_with_map(passage)
    for (i in which(quoted)) {
      # Only a quotation the check found is marked, so the page and
      # gr_verify_evidence() cannot disagree about which ones are there.
      sp <- if (isTRUE(verified[i])) quote_span(text[i], folded)
      if (!is.null(sp)) { spans[[length(spans) + 1L]] <- sp; next }
      unplaced <- c(unplaced, if (isTRUE(verified[i]))
        sprintf("<p class='sub'>Quoted, and found in this chunk, but not placed in the text shown: &ldquo;%s&rdquo;</p>",
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
    head <- sprintf("<h3>%s</h3>", esc(lab))
    status <- as.character(s$status[i])
    if (identical(status, "duplicate") || !is.na(dup_of[i])) {
      return(c(head, sprintf("<p class='sub'>Same text as %s, which is shown there.</p>",
                             esc(as_chr1(dup_of[i], "an earlier document")))))
    }
    a <- answers[[lab]]
    if (!inherits(a, "gr_answer")) {
      return(c(head, sprintf("<p class='sub'>No answer: %s.</p>", esc(switch(status,
        skipped = "the run's ceiling was reached before this document",
        failed = as_chr1(s$error[i], "it could not be read"),
        "none was kept")))))
    }
    c(head, answer_summary(a), "<h4>Where it came from</h4>", answer_passages(a))
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
