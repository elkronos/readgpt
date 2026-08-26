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
library(gptread)

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

# The UI offers only files under ALLOWED_ROOTS, but `input$file` is whatever the
# browser sends -- a crafted websocket message can set it to any path on the
# server. Without this check the app is an arbitrary-file-read oracle that reads
# the file back to the caller through the answer bubble.
safe_path <- function(path) {
  if (!isTruthy(path)) return(NULL)
  p <- suppressWarnings(normalizePath(path, winslash = "/", mustWork = FALSE))
  if (!file.exists(p) || dir.exists(p)) return(NULL)
  roots <- paste0(sub("/+$", "", ALLOWED_ROOTS), "/")
  if (!any(startsWith(p, roots))) return(NULL)
  p
}

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
read_choices <- gr_readers()$name
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
  titlePanel("gptread - document Q&A with explicit control over ingest, chunking and reading"),
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
                              "legacy_v1 (reproduces v1, strips digits)" = "legacy_v1"),
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
      numericInput("max_cost", "Abort if estimated cost exceeds (USD)", 2, 0, 1000, 0.5),
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
                 helpText("Free preview: how your ingest + segment settings break the document up. ",
                          "No model calls are made."),
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

  output$file_ui <- renderUI({
    root <- ALLOWED_ROOTS[[input$root]]
    files <- list.files(root, full.names = TRUE, recursive = TRUE,
                        pattern = "\\.(pdf|docx|txt|md|html?|png|jpe?g|tiff?)$",
                        ignore.case = TRUE)
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
    clean <- if (identical(input$clean_preset, "custom")) input$clean_steps else input$clean_preset
    gr_ingest_spec(clean = clean, ocr = input$ocr)
  })
  segment_spec <- reactive({
    gr_segment_spec(method = input$segmenter, max_tokens = input$max_tokens,
                    overlap_tokens = input$overlap, min_tokens = input$min_tokens,
                    parallel = isTRUE(input$parallel))
  })

  observeEvent(input$preview, {
    path <- safe_path(input$file)
    if (is.null(path)) {
      showNotification("Pick a document from the list.", type = "error"); return()
    }
    withProgress(message = "Segmenting (no model calls)", value = 0.4, {
      out <- tryCatch({
        doc <- gr_ingest(path, ingest_spec())
        gr_segment(doc, segment_spec(), client = client())
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
    showNotification(sprintf("%d chunks, %d tokens total.", nrow(out$chunks),
                             sum(out$chunks$tokens)), type = "message")
  })

  observeEvent(input$go, {
    req(nzchar(trimws(input$question %||% "")), length(input$readers) > 0)
    path <- safe_path(input$file)
    if (is.null(path)) {
      showNotification("Pick a document from the list.", type = "error"); return()
    }
    if (is.null(client())) {
      showNotification("Enter an API key first.", type = "error"); return()
    }
    old <- gr_options(max_cost_usd = if (isTruthy(input$max_cost)) input$max_cost else NULL,
                      parallel = isTRUE(input$parallel))
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
    last_trace(res$trace)
    last_cmp(res$summary)
    history(c(history(), list(list(
      question = input$question,
      asked_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
      document = basename(path),
      answers = res$answers,
      plain = lapply(res$answers, plain_answer),
      summary = res$summary,
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
          div(class = "bubble bot",
              tags$strong(sprintf("%s [%s]", nm, a$signature %||% "")),
              tags$br(), a$answer,
              div(class = "meta", sprintf(
                "%d chunk(s) used%s%s", length(a$chunks_used),
                if (isTRUE(a$partial)) " - PARTIAL" else "",
                if (!is.null(a$notes$error)) paste0(" - ", a$notes$error) else "")))
        })),
        tags$hr())
    }))
  })

  output$cmp_table <- renderTable({ s <- last_cmp(); if (is.null(s)) NULL else s })
  output$trace_json <- renderText({
    h <- history(); if (!length(h)) "" else h[[length(h)]]$trace_json
  })

  output$dl <- downloadHandler(
    filename = function() sprintf("gptread_history_%s.json", Sys.Date()),
    content = function(file) {
      h <- lapply(history(), function(e) list(
        question = e$question, asked_at = e$asked_at, document = e$document,
        answers = e$plain, summary = e$summary,
        trace = jsonlite::fromJSON(e$trace_json, simplifyVector = FALSE)))
      writeLines(as.character(jsonlite::toJSON(h, pretty = TRUE, auto_unbox = TRUE,
                                               null = "null", na = "null", force = TRUE)),
                 file)
    })
}

`%||%` <- function(x, y) if (is.null(x)) y else x

shinyApp(ui, server)
