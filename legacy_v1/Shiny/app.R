# app.R

# Source utility and package scripts first (relative to the Shiny folder)
source(file.path("..", "R", "utils.R"))
source(file.path("..", "R", "packages.R"))

# Load required libraries
library(shiny)
library(shinyFiles)
library(shinyjs)
library(jsonlite)

# Source your processing scripts from the R folder
source(file.path("..", "R", "text_processing.R"))
source(file.path("..", "R", "retrieval_mode.R"))
source(file.path("..", "R", "chunked_mode.R"))
source(file.path("..", "R", "hierarchical_mode.R"))
source(file.path("..", "R", "multi_pass_mode.R"))
source(file.path("..", "R", "main.R"))

# Define UI with custom styling for the chat bubbles, a modal for API parameters,
# and a processing log output at the bottom of the chat window.
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .chat-window {
        border: 1px solid #ccc;
        padding: 10px;
        height: 400px;
        overflow-y: auto;
        background-color: #f9f9f9;
      }
      .chat-bubble {
        padding: 10px;
        border-radius: 10px;
        margin: 5px;
        max-width: 70%;
      }
      .left {
        background-color: #e1f5fe;
        text-align: left;
      }
      .right {
        background-color: #c8e6c9;
        text-align: right;
        margin-left: auto;
      }
      /* Style for file/directory buttons when selected */
      .selected-btn {
        background-color: #c8e6c9 !important;
      }
    "))
  ),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('toggleSubmit', function(message) {
      $('#submitBtn').prop('disabled', message.disable);
    });
  ")),
  
  titlePanel("Document Q&A Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons("input_type", "Select Input Type:",
                   choices = c("Single File", "Directory"), selected = "Single File"),
      shinyFilesButton("file", "Choose a Document", "Select a file", multiple = FALSE),
      shinyDirButton("dir", "Choose a Directory", "Select a directory"),
      uiOutput("dir_file_ui"),
      hr(),
      checkboxGroupInput("methods", "Select Reading Method(s):",
                         choices = c("Retrieval", "Chunked", "Semantic", "Hierarchical", "MultiPass")),
      hr(),
      textAreaInput("question", "Enter your Question:", "", rows = 3),
      actionButton("submitBtn", "Submit", class = "btn-primary", style = "margin-top:10px;", disabled = TRUE),
      hr(),
      downloadButton("downloadHistory", "Download History")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Chat",
                 div(class = "chat-window", id = "chatWindow",
                     uiOutput("chatOutput")
                 ),
                 hr(),
                 # Show the last processing message in small text
                 tags$small(textOutput("lastMessage"))
        ),
        tabPanel("JSON Chain-of-Thought",
                 verbatimTextOutput("jsonOutput")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Set up shinyFiles for file and directory selection
  volumes <- getVolumes()
  shinyFileChoose(input, "file", roots = volumes, session = session)
  shinyDirChoose(input, "dir", roots = volumes, session = session)
  
  # Reactive value to store the selected file path
  selectedFile <- reactiveVal(NULL)
  
  # Update file path when a single file is chosen and change button color
  observeEvent(input$file, {
    fileinfo <- parseFilePaths(volumes, input$file)
    if (nrow(fileinfo) > 0) {
      selectedFile(as.character(fileinfo$datapath[1]))
      runjs("$('#file').addClass('selected-btn');")
    }
  })
  
  # Reactive value for directory path
  dirPath <- reactive({
    if (!is.null(input$dir)) {
      parseDirPath(volumes, input$dir)
    } else {
      NULL
    }
  })
  
  # Render UI to select a file from the chosen directory
  output$dir_file_ui <- renderUI({
    if (input$input_type == "Directory" && !is.null(dirPath())) {
      files <- list.files(dirPath(), pattern = "\\.(pdf|docx|txt)$", full.names = TRUE, ignore.case = TRUE)
      if (length(files) == 0) {
        return(tags$p("No supported document files found in this directory."))
      }
      selectInput("dir_file", "Select a Document from Directory:", choices = files)
    }
  })
  
  # When a file is chosen from the directory, update selectedFile and change button color
  observeEvent(input$dir_file, {
    selectedFile(input$dir_file)
    runjs("$('#dir').addClass('selected-btn');")
  })
  
  # Reactive values for API parameters, conversation history, and last processing message
  apiParams <- reactiveValues(apiKey = NULL, model = "gpt-3.5-turbo", temperature = 0.0, penalty = 0.0)
  conversationHistory <- reactiveVal(list())
  jsonHistory <- reactiveVal(list())
  lastMessage <- reactiveVal("Idle")
  
  # Observer to print the lastMessage to the terminal each time it updates.
  observe({
    msg <- lastMessage()
    print(paste("Processing Log Update:", msg))
  })
  
  # Show modal dialog to capture API key and GPT parameters at startup
  showModal(modalDialog(
    title = "Enter API Key and GPT Parameters",
    textInput("apiKey", "API Key:"),
    textInput("gptModel", "Model:", value = "gpt-3.5-turbo"),
    numericInput("temperature", "Temperature:", value = 0.0, min = 0, max = 1, step = 0.1),
    numericInput("penalty", "Presence/Frequency Penalty:", value = 0.0, min = 0, max = 2, step = 0.1),
    footer = tagList(
      actionButton("saveParams", "Save")
    ),
    easyClose = FALSE
  ))
  
  observeEvent(input$saveParams, {
    if (nchar(trimws(input$apiKey)) < 1) {
      showNotification("Please enter a valid API key.", type = "error")
    } else {
      apiParams$apiKey <- input$apiKey
      apiParams$model <- input$gptModel
      apiParams$temperature <- input$temperature
      apiParams$penalty <- input$penalty
      # Set API key as environment variable for downstream API calls
      Sys.setenv(OPENAI_API_KEY = input$apiKey)
      removeModal()
    }
  })
  
  # Enable the submit button only when a file is selected, at least one method is chosen, and the question is nonempty.
  observe({
    valid <- !is.null(selectedFile()) &&
      length(input$methods) > 0 &&
      nchar(trimws(input$question)) > 0
    session$sendCustomMessage("toggleSubmit", list(disable = !valid))
  })
  
  # Render the processing log text output and force periodic updates.
  output$lastMessage <- renderText({
    invalidateLater(500, session)
    lastMessage()
  })
  
  # Process the submission when the Submit button is clicked
  observeEvent(input$submitBtn, {
    req(selectedFile(), input$methods, nchar(trimws(input$question)) > 0)
    
    # Disable the submit button during processing
    session$sendCustomMessage("toggleSubmit", list(disable = TRUE))
    
    filePath <- selectedFile()
    questionText <- input$question
    methods <- input$methods
    
    # Pack extra parameters for answer_question
    extraParams <- list(
      model = apiParams$model,
      temperature = apiParams$temperature,
      presence_penalty = apiParams$penalty,
      frequency_penalty = apiParams$penalty
    )
    
    # Use a progress bar to indicate processing status and update lastMessage.
    finalAns <- withProgress(message = "Processing...", value = 0, {
      incProgress(0.3, detail = "Parsing document and preparing text chunks...")
      lastMessage("Parsing document and preparing text chunks...")
      Sys.sleep(0.5)  # simulate a brief pause
      ans <- tryCatch({
        do.call(answer_question, c(list(file_path = filePath,
                                        question = questionText,
                                        mode = methods,
                                        use_parallel = FALSE,
                                        refine = FALSE), extraParams))
      }, error = function(e) {
        paste("Error:", e$message)
      })
      incProgress(0.6, detail = "Generating answer...")
      lastMessage("Generating answer...")
      Sys.sleep(0.5)
      ans
    })
    
    jsonAns <- withProgress(message = "Processing JSON chain-of-thought...", value = 0, {
      incProgress(0.5)
      lastMessage("Generating JSON chain-of-thought...")
      tryCatch({
        do.call(answer_question, c(list(file_path = filePath,
                                        question = questionText,
                                        mode = methods,
                                        use_parallel = FALSE,
                                        refine = FALSE,
                                        return_json = TRUE), extraParams))
      }, error = function(e) {
        paste("Error:", e$message)
      })
    })
    
    lastMessage("Processing complete.")
    
    # Append the new conversation entry to the history
    convEntry <- list(
      question = questionText,
      methods = methods,
      answer = finalAns
    )
    conversationHistory(append(conversationHistory(), list(convEntry)))
    
    # Append the JSON chain-of-thought entry
    jsonEntry <- list(
      question = questionText,
      methods = methods,
      json_chain = jsonAns
    )
    jsonHistory(append(jsonHistory(), list(jsonEntry)))
    
    # Update the chat output to display conversation entries:
    output$chatOutput <- renderUI({
      histEntries <- conversationHistory()
      chatElements <- lapply(histEntries, function(entry) {
        tagList(
          div(class = "chat-bubble right", style = "margin-bottom:10px;",
              strong("You:"), br(), entry$question),
          div(class = "chat-bubble left", style = "margin-bottom:5px;",
              strong("GPT (" , paste(entry$methods, collapse = ", "), "):"),
              br(),
              if (is.list(entry$answer)) {
                tagList(lapply(names(entry$answer), function(m) {
                  tags$p(tags$em(m), ": ", entry$answer[[m]])
                }))
              } else {
                entry$answer
              }
          )
        )
      })
      do.call(tagList, chatElements)
    })
    
    # Update the JSON output tab
    output$jsonOutput <- renderText({
      toJSON(jsonHistory(), pretty = TRUE, auto_unbox = TRUE)
    })
    
    # Clear the question input for next submission
    updateTextAreaInput(session, "question", value = "")
    
    # Re-enable the submit button after processing is complete
    session$sendCustomMessage("toggleSubmit", list(disable = FALSE))
  })
  
  # Download handler for conversation history
  output$downloadHistory <- downloadHandler(
    filename = function() {
      paste0("conversation_history_", Sys.Date(), ".json")
    },
    content = function(file) {
      hist <- list(
        conversation = conversationHistory(),
        json_chain_of_thought = jsonHistory()
      )
      write(toJSON(hist, pretty = TRUE, auto_unbox = TRUE), file)
    }
  )
}

shinyApp(ui, server)
