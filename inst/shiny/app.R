# inst/shiny/app.R -- a UI that exposes the knobs.
#
# WHY THIS FILE IS DIFFERENT FROM v1's Shiny/app.R
#
#  * v1 called answer_question() TWICE per submission -- once for the answer,
#    once with return_json = TRUE for the "chain of thought" tab -- exactly
#    doubling every user's API bill. At any temperature above 0 the two runs were
#    independent generations, so the trace shown did not explain the answer
#    shown. Here one run produces both.
#  * v1 stored the API key with Sys.setenv(), which is PROCESS-wide. Two browser
#    sessions in one R process overwrote each other's key, so user A's questions
#    were billed to user B. The key now lives in a per-session reactiveVal and is
#    handed to a per-session gr_client().
#  * v1's `volumes <- getVolumes()` exposed the entire filesystem to any browser
#    client, who could pick /etc/anything and have it read aloud through the chat
#    bubble. Roots are now an explicit allow-list.
#  * v1 hard-coded `source(file.path("..", "R", ...))`, which breaks on any
#    deployment that bundles only the app directory. This is a package now.
#  * v1's UI exposed model, temperature and one penalty. Everything else --
#    chunking method, chunk size, overlap, cleaning, top-k, reader -- was
#    unreachable, which is the opposite of fine-grained control.
#  * v1 ran `output$lastMessage <- renderText({ invalidateLater(500, session);
#    ... })`, a permanent 2 Hz recompute for the life of every session, which
#    could not display progress anyway because a blocking observer never yields
#    to the reactive flush. The progress bar here is driven by withProgress.

library(shiny)
library(readgpt)

ALLOWED_ROOTS <- local({
  env <- Sys.getenv("GPTREAD_DOC_ROOTS")
  if (nzchar(env)) {
    p <- strsplit(env, .Platform$path.sep, fixed = TRUE)[[1]]
    p <- p[dir.exists(p)]
    if (length(p)) {
      # normalizePath resolves symlinks and "..", so the containment check below
      # compares real paths. make.unique keeps two roots that share a basename
      # (~/work/docs and ~/personal/docs) from collapsing into one entry, where
      # picking the second silently opened the first.
      p <- normalizePath(p, winslash = "/", mustWork = FALSE)
      return(stats::setNames(p, make.unique(basename(p), sep = " #")))
    }
  }
  d <- file.path(path.expand("~"), "Documents")
  d <- if (dir.exists(d)) d else path.expand("~")
  stats::setNames(normalizePath(d, winslash = "/", mustWork = FALSE), "documents")
})

# The documents the UI offers under one root. list.files() leaves out dotfiles
# and hidden directories, and the pattern leaves out the other types the
# extractors read (csv, log, ...), so neither can be picked.
DOC_PATTERN <- "\\.(pdf|docx|txt|md|html?|png|jpe?g|tiff?)$"
list_documents <- function(root) {
  if (!length(root) || is.na(root)) return(character(0))
  list.files(root, full.names = TRUE, recursive = TRUE, pattern = DOC_PATTERN,
             ignore.case = TRUE)
}

# The UI offers only files under ALLOWED_ROOTS, but `input$file` is whatever the
# browser sends -- a crafted websocket message can set it to any path on the
# server. Without this check the app is an arbitrary-file-read oracle that reads
# the file back to the caller through the answer bubble.
#
# Only a path the UI listed is accepted, compared as a string before anything
# touches the filesystem. Checking "exists and sits under a root" let a crafted
# value read hidden files and unlisted types there (.env.txt, *.csv, *.log)
# through the free preview. And it called normalizePath() and file.exists() on
# the untrusted string first: on Windows a UNC path (\\host\share\x) makes the
# server open an SMB session to that host with its own credentials, and on macOS
# /net/<host>/ makes the automounter contact it, before the path is refused.
safe_path <- function(path, offered) {
  if (!is.character(path) || length(path) != 1L || is.na(path) || !nzchar(path)) return(NULL)
  if (!path %in% offered) return(NULL)
  # A listed path can still be a symlink out of the root: resolve it and check.
  p <- suppressWarnings(normalizePath(path, winslash = "/", mustWork = FALSE))
  if (!file.exists(p) || dir.exists(p)) return(NULL)
  roots <- paste0(sub("/+$", "", ALLOWED_ROOTS), "/")
  if (!any(startsWith(p, roots))) return(NULL)
  p
}

# What a blank cost field means, said where it is typed.
cap_label <- local({
  cap <- gr_options("max_cost_usd")
  sprintf("Abort if estimated cost exceeds (USD; blank = %s)",
          if (is.null(cap)) "no limit, as this R session sets none"
          else sprintf("this R session's cap, $%s", format(cap)))
})

# A history entry has to survive JSON encoding. A gr_answer carries its trace,
# which is an ENVIRONMENT, and jsonlite refuses to encode one -- so the download
# button failed on the very first click of every session that had asked anything.
plain_answer <- function(a) {
  list(reader = a$reader, signature = a$signature, answer = a$answer,
       partial = isTRUE(a$partial), chunks_used = a$chunks_used,
       evidence = if (is.null(a$evidence)) NULL else a$evidence,
       notes = a$notes)
}

seg_choices <- gr_segmenters()$name
# Not extract or screen: they answer a protocol (the fields to fill, the
# criteria to judge by), not a question, and the app has nowhere to enter one,
# so ticking either always failed with a message about what was missing.
read_choices <- setdiff(gr_readers()$name, c("extract", "screen"))
clean_choices <- gr_cleaners()$name

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .bubble{padding:10px 14px;border-radius:12px;margin:6px 0;max-width:85%;white-space:pre-wrap}
    .you{background:#e3f2fd;margin-left:auto}
    .bot{background:#f1f8e9}
    .meta{font-size:11px;color:#666;margin-top:4px}
    .chat{border:1px solid #ddd;padding:12px;height:460px;overflow-y:auto;background:#fafafa}
    .knob-help{font-size:11px;color:#777;margin:-8px 0 10px 0}
  "))),
  titlePanel("readgpt - document Q&A with explicit control over ingest, chunking and reading"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Document"),
      selectInput("root", "Folder", choices = names(ALLOWED_ROOTS)),
      uiOutput("file_ui"),
      hr(),

      h4("1. Ingest"),
      selectInput("clean_preset", "Cleaning preset",
                  choices = c("standard", "minimal", "academic", "scan", "none", "custom",
                              "legacy (reproduces the pre-0.2 defaults, strips digits)" = "legacy"),
                  selected = "standard"),
      conditionalPanel("input.clean_preset == 'custom'",
        checkboxGroupInput("clean_steps", NULL, choices = clean_choices,
                           selected = c("page_numbers", "hyphenation", "control_chars",
                                        "ligatures", "collapse_whitespace"))),
      selectInput("ocr", "OCR", choices = c("auto", "always", "never")),
      hr(),

      h4("2. Segment"),
      selectInput("segmenter", "Method", choices = seg_choices, selected = "paragraph"),
      div(class = "knob-help", textOutput("seg_help", inline = TRUE)),
      sliderInput("max_tokens", "Max tokens per chunk", 100, 8000, 1200, step = 100),
      sliderInput("overlap", "Overlap tokens", 0, 1000, 0, step = 20),
      sliderInput("min_tokens", "Merge chunks below (tokens)", 0, 500, 0, step = 25),
      actionButton("preview", "Preview chunking (free)", class = "btn-default btn-sm"),
      hr(),

      h4("3. Read"),
      checkboxGroupInput("readers", "Strategy (tick several to compare)",
                         choices = read_choices, selected = "map_reduce"),
      div(class = "knob-help", textOutput("read_help", inline = TRUE)),
      sliderInput("top_k", "top_k (retrieve / rerank / iterative)", 1, 30, 6),
      checkboxInput("cite", "Ask for chunk citations", FALSE),
      hr(),

      h4("Model"),
      selectInput("model", "Model",
                  choices = gr_models()[gr_models()$kind == "chat", "id"],
                  selected = gr_options("model")),
      numericInput("temperature", "Temperature (blank = model default)", NA, 0, 2, 0.1),
      passwordInput("api_key", "API key (this browser session only)"),
      numericInput("max_cost", cap_label, 2, 0, 1000, 0.5),
      checkboxInput("parallel", "Parallel per-chunk calls", FALSE),
      hr(),
      textAreaInput("question", "Question", "", rows = 3),
      actionButton("go", "Ask", class = "btn-primary"),
      downloadButton("dl", "Download run history")
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        tabPanel("Answers", br(), div(class = "chat", uiOutput("chat"))),
        tabPanel("Chunking", br(),
                 helpText(textOutput("chunk_help", inline = TRUE)),
                 tableOutput("chunk_stats"), hr(), verbatimTextOutput("chunk_preview")),
        tabPanel("Comparison", br(), tableOutput("cmp_table")),
        tabPanel("Trace", br(),
                 helpText("The trace for the run that produced the answer above - same run, ",
                          "not a second billed pass."),
                 verbatimTextOutput("trace_json")),
        tabPanel("Reference", br(),
                 h4("Segmenters"), tableOutput("seg_tbl"),
                 h4("Readers"), tableOutput("read_tbl"),
                 h4("Cleaners"), tableOutput("clean_tbl"))
      )
    )
  )
)

server <- function(input, output, session) {
  history <- reactiveVal(list())
  last_trace <- reactiveVal(NULL)
  last_cmp <- reactiveVal(NULL)

  # Per-session client: the key never touches the process environment, so two
  # concurrent users cannot bill each other.
  client <- reactive({
    key <- input$api_key
    if (!isTruthy(key)) return(NULL)
    gr_client(model = input$model, api_key = key)
  })

  # The chosen root's documents, listed once per root: the file menu shows
  # them, and safe_path() accepts nothing else. input$root is checked too; a
  # crafted value names no root. Before the menu has registered, the root is
  # the first, which is what the menu selects.
  offered <- reactive({
    nm <- input$root %||% names(ALLOWED_ROOTS)[1]
    root <- if (is.character(nm) && length(nm) == 1L && nm %in% names(ALLOWED_ROOTS))
      ALLOWED_ROOTS[[nm]] else NA_character_
    list(root = root, files = list_documents(root))
  })

  output$file_ui <- renderUI({
    root <- offered()$root
    files <- offered()$files
    if (!length(files)) {
      return(helpText(sprintf("No supported documents under %s. Set GPTREAD_DOC_ROOTS to point elsewhere.", root)))
    }
    selectInput("file", "Document",
                choices = stats::setNames(files, substr(basename(files), 1, 60)))
  })

  output$seg_help <- renderText({
    d <- gr_segmenters(); as.character(d$description[d$name == input$segmenter])
  })
  output$read_help <- renderText({
    d <- gr_readers()
    paste(sprintf("%s [%s]: %s", d$name, d$cost_calls, d$description)[d$name %in% input$readers],
          collapse = "  |  ")
  })
  output$seg_tbl   <- renderTable(gr_segmenters())
  output$read_tbl  <- renderTable(gr_readers())
  output$clean_tbl <- renderTable(gr_cleaners())

  ingest_spec <- reactive({
    # An empty checkbox group arrives as NULL, and gr_ingest_spec(clean = NULL)
    # is the standard preset: "custom" with nothing ticked, meaning no cleaning,
    # removed page numbers and joined hyphenated words all the same.
    clean <- if (identical(input$clean_preset, "custom")) input$clean_steps %||% character(0)
             else input$clean_preset
    gr_ingest_spec(clean = clean, ocr = input$ocr)
  })
  segment_spec <- reactive({
    gr_segment_spec(method = input$segmenter, max_tokens = input$max_tokens,
                    overlap_tokens = input$overlap, min_tokens = input$min_tokens,
                    parallel = isTRUE(input$parallel))
  })

  # Whether the chosen segmenter spends anything, read from the registry so a
  # registered segmenter is judged the same way. The preview was labelled free
  # and said no model calls were made while the menu offered `proposition` and
  # `semantic`, and it ran them with no cost cap and no trace: one paid request
  # per 900 tokens of the document, unbounded and unrecorded.
  seg_paid <- reactive({
    d <- gr_segmenters()
    cost <- as.character(d$cost[d$name %in% input$segmenter])
    length(cost) > 0 && !identical(cost[1], "free")
  })
  observe({
    updateActionButton(session, "preview", label = if (seg_paid())
      "Preview chunking (makes model calls)" else "Preview chunking (free)")
  })
  output$chunk_help <- renderText({
    if (seg_paid()) {
      paste("How your ingest + segment settings break the document up. This segmenter makes",
            "model calls, held to the cost cap in the Model panel; the preview says what they cost.")
    } else {
      paste("Free preview: how your ingest + segment settings break the document up.",
            "No model calls are made.")
    }
  })

  # The cost field, checked. NULL is a blank field, which leaves the R session's
  # cap (gr_options("max_cost_usd")) in force, as the label says: blank used to
  # mean no cap at all, so clearing the field lifted every spending limit
  # without a word. NA means the field was refused and the user told why.
  # gr_options() refuses a cap it cannot compare, and a refusal raised outside
  # any tryCatch would end the handler with R's error text instead of a message
  # about the field the user just typed in.
  cost_cap <- function() {
    cap <- if (isTruthy(input$max_cost)) as.numeric(input$max_cost)[1] else NULL
    if (!is.null(cap) && (is.na(cap) || cap < 0)) {
      showNotification(paste("The cost cap must be zero or more, or left blank for this",
                             "R session's cap."), type = "error")
      return(NA)
    }
    cap
  }
  # The options a run sets for the cost field: none for a blank one.
  cap_option <- function(cap) if (is.null(cap)) list() else list(max_cost_usd = cap)

  # The question the last run answered. Ask blocks the R process for the whole
  # run and the box is cleared only once it returns, so a second click made
  # meanwhile arrives afterwards with the old question still in input$question,
  # and it ran -- and billed -- the whole comparison again. A question stays
  # answered until the browser sends a new value for the box, the clear
  # included. The higher priority runs this before the Ask handler when both
  # arrive together, so it cannot undo the handler's record.
  answered <- reactiveVal(NULL)
  observeEvent(input$question, answered(NULL), priority = 10)

  observeEvent(input$preview, {
    path <- safe_path(input$file, offered()$files)
    if (is.null(path)) {
      showNotification("Pick a document from the list.", type = "error"); return()
    }
    paid <- seg_paid()
    if (paid) {
      cap <- cost_cap()
      if (identical(cap, NA)) return()
      old <- gr_options(cap_option(cap))
      on.exit(gr_options(old), add = TRUE)
    }
    # Its own trace, so the run's limits hold and what it spent can be reported.
    tr <- gr_trace(meta = list(stage = "preview"))
    withProgress(message = if (paid) "Segmenting (makes model calls)" else "Segmenting (no model calls)",
                 value = 0.4, {
      out <- tryCatch({
        doc <- gr_ingest(path, ingest_spec())
        gr_segment(doc, segment_spec(), client = client(), trace = tr)
      }, error = function(e) e)
    })
    if (inherits(out, "error")) {
      output$chunk_stats <- renderTable(data.frame(error = conditionMessage(out)))
      output$chunk_preview <- renderText("")
      return()
    }
    output$chunk_stats <- renderTable(gr_chunk_stats(out))
    output$chunk_preview <- renderText(paste(
      sprintf("--- chunk %d (%d tokens%s) ---\n%s",
              out$chunks$chunk_id, out$chunks$tokens,
              ifelse(is.na(out$chunks$section), "", paste0(", ", out$chunks$section)),
              substr(out$chunks$text, 1, 700)),
      collapse = "\n\n"))
    spent <- if (tr$calls > 0) {
      sprintf(" Segmenting made %d model call(s), about $%s%s.", tr$calls,
              format(signif(tr$spent_usd, 2), scientific = FALSE),
              if (isTRUE(tr$budget_stop)) paste0(", and stopped at the ",
                                                 if (identical(tr$stop_reason, "cost")) "cost cap"
                                                 else "call cap")
              else "")
    } else ""
    showNotification(sprintf("%d chunks, %d tokens total (%s).%s", nrow(out$chunks),
                             sum(out$chunks$tokens), out$method, spent),
                     type = if (isTRUE(tr$budget_stop)) "warning" else "message")
  })

  observeEvent(input$go, {
    req(nzchar(trimws(input$question %||% "")), length(input$readers) > 0)
    if (identical(input$question, answered())) {
      showNotification("That question was just answered. Edit it to ask again.",
                       type = "message")
      return()
    }
    path <- safe_path(input$file, offered()$files)
    if (is.null(path)) {
      showNotification("Pick a document from the list.", type = "error"); return()
    }
    if (is.null(client())) {
      showNotification("Enter an API key first.", type = "error"); return()
    }
    cap <- cost_cap()
    if (identical(cap, NA)) return()
    old <- gr_options(c(cap_option(cap), list(parallel = isTRUE(input$parallel))))
    on.exit(gr_options(old), add = TRUE)

    # A blank numericInput sends NA, but before the input has registered it is
    # NULL, and `if (is.na(NULL))` is `if (logical(0))` -- "argument is of length
    # zero", which killed the session on the first Ask of a fresh page.
    temp <- if (isTruthy(input$temperature)) as.numeric(input$temperature)[1] else NULL
    recipes <- lapply(input$readers, function(r) gr_recipe(
      name = r, ingest = ingest_spec(), segment = segment_spec(),
      read = gr_read_spec(reader = r, model = input$model, temperature = temp,
                          top_k = input$top_k, cite = isTRUE(input$cite),
                          members = if (identical(r, "ensemble")) c("retrieve", "map_reduce", "refine"),
                          parallel = isTRUE(input$parallel))))

    # ONE run produces both the answers and the trace.
    res <- withProgress(message = "Reading", value = 0.1, {
      incProgress(0.2, detail = "ingest and segment")
      tryCatch(gr_compare(path, input$question, recipes, client = client(),
                          on_error = "continue"),
               error = function(e) e)
    })

    if (inherits(res, "error")) {
      showNotification(conditionMessage(res), type = "error", duration = 12)
      return()
    }
    # Why each partial answer is partial, beside its error. `error` is set only
    # when a recipe threw; a reader that records its failures in its notes (every
    # call failing on a mistyped key, say) left it NA on a row reading
    # not_found = TRUE, which says the document does not contain the answer.
    summary <- res$summary
    summary$partial_because <- vapply(summary$recipe, function(nm) {
      a <- res$answers[[nm]]
      if (is.null(a) || !isTRUE(a$partial)) NA_character_ else why_partial(a)
    }, character(1), USE.NAMES = FALSE)
    last_trace(res$trace)
    last_cmp(summary)
    answered(input$question)
    history(c(history(), list(list(
      question = input$question,
      asked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      document = basename(path),
      answers = res$answers,
      plain = lapply(res$answers, plain_answer),
      summary = summary,
      trace_json = as.character(as_json(res$trace))))))
    updateTextAreaInput(session, "question", value = "")
  })

  output$chat <- renderUI({
    h <- history()
    if (!length(h)) return(helpText("Ask a question to begin."))
    do.call(tagList, lapply(rev(h), function(e) {
      tagList(
        div(class = "bubble you", tags$strong("You: "), e$question),
        do.call(tagList, lapply(names(e$answers), function(nm) {
          a <- e$answers[[nm]]
          why <- if (isTRUE(a$partial)) why_partial(a) else note_error(a)
          div(class = "bubble bot",
              tags$strong(sprintf("%s [%s]", nm, a$signature %||% "")),
              tags$br(), answer_text(a),
              div(class = "meta", sprintf(
                "%d chunk(s) used%s%s", length(a$chunks_used),
                if (isTRUE(a$partial)) " - PARTIAL" else "",
                if (!is.na(why)) paste0(" - ", why) else "")))
        })),
        tags$hr())
    }))
  })

  output$cmp_table <- renderTable({ s <- last_cmp(); if (is.null(s)) NULL else s })
  output$trace_json <- renderText({
    h <- history(); if (!length(h)) "" else h[[length(h)]]$trace_json
  })

  output$dl <- downloadHandler(
    filename = function() sprintf("readgpt_history_%s.json", Sys.Date()),
    content = function(file) {
      h <- lapply(history(), function(e) list(
        question = e$question, asked_at = e$asked_at, document = e$document,
        answers = e$plain, summary = e$summary,
        trace = jsonlite::fromJSON(e$trace_json, simplifyVector = FALSE)))
      # The bytes as they are. writeLines() to a path re-encodes to the
      # session's encoding, so in a C or single-byte locale a question or an
      # answer with a character that has no place there (a "greater than or
      # equal" sign, an accented name) was saved as "<U+2265>". The helper is
      # internal, but this app ships inside the package, as why_partial() says.
      readgpt:::write_utf8_lines(as.character(jsonlite::toJSON(
        h, pretty = TRUE, auto_unbox = TRUE, null = "null", na = "null", force = TRUE)),
        file)
    })
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# notes$error as one string, or NA. `[[exact = TRUE]]`: `$` partial-matches.
note_error <- function(a) {
  x <- as.list(a$notes)[["error", exact = TRUE]]
  if (length(x) && !is.na(x[[1]])) as.character(x[[1]]) else NA_character_
}

# Why an answer is partial, in the words print() uses, or NA when it cannot
# say. The bubble showed notes$error alone, which only a recipe that threw
# sets: a run whose every request failed (a mistyped key: HTTP 401 on each)
# read "NOT_IN_DOCUMENT - PARTIAL" with no reason. The helper is internal, but
# this app ships inside the package, so the two cannot drift apart.
why_partial <- function(a) {
  why <- tryCatch(readgpt:::partial_reasons(a), error = function(e) character(0))
  # A recipe's own error in full, as the bubble showed it before: the helper
  # cuts its "first error" to 120 characters, which can lose what to change.
  err <- note_error(a)
  if (!is.na(err)) why <- c(why[!startsWith(why, "first error: ")], err)
  if (length(why)) paste(why, collapse = "; ") else NA_character_
}

# The answer as a person reads it. The sentinel is a value for code; shown
# raw it read as "the document does not say" even of a run that read nothing.
answer_text <- function(a) {
  if (!is_not_found(a$answer)) return(a$answer)
  tryCatch(readgpt:::not_found_wording(a), error = function(e) a$answer)
}

shinyApp(ui, server)
