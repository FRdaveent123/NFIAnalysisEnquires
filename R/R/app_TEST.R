# app.R
# NFI Enquiries Dashboard (Shiny)
# --------------------------------

# ---- Packages ----
library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(stringr)
library(lubridate)
library(yaml)
library(plotly)
library(reactable)
library(htmltools)

# Parallel compute (Windows-safe)
library(future)
library(future.apply)
plan(multisession)

# Working directory (local)
setwd("C:/Users/david.entwistle/OneDrive - Forest Research/Documents/Project/NFI_Enquiries_Dashboard")

# ---- Null coalescing ----
`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- Load config ----
cfg <- yaml::read_yaml("config/paths.yaml")
root_path       <- cfg$paths$enquiries_root

# ---- Source logic ----
source("R/file_scanner.R")
source("R/readme_parser.R")
source("R/helpers.R")

# =====================================================================================
# SAFE ReadMe PARSING
# =====================================================================================

safe_read_lines <- function(file) {
  if (requireNamespace("readr", quietly = TRUE)) {
    utf8_try <- try(readr::read_lines(file, locale = readr::locale(encoding = "UTF-8")), silent = TRUE)
    if (!inherits(utf8_try, "try-error")) return(utf8_try)
    
    latin_try <- try(readr::read_lines(file, locale = readr::locale(encoding = "Latin1")), silent = TRUE)
    if (!inherits(latin_try, "try-error")) return(latin_try)
  }
  readLines(file, warn = FALSE)
}

parse_readme_safe <- function(request_path) {
  file <- file.path(request_path, "ReadMe.txt")
  if (!file.exists(file)) return(NULL)
  
  lines <- safe_read_lines(file)
  pattern <- "^([0-9]{2}[/-]?[0-9]{2}[/-]?[0-9]{2,4})\\s*[- ]\\s*(.*)$"
  m <- str_match(lines, pattern)
  m <- m[!is.na(m[,1]), , drop = FALSE]
  if (nrow(m) == 0) return(NULL)
  
  dates <- suppressWarnings(dmy(m[,2]))
  dates[is.na(dates)] <- suppressWarnings(dmy(str_replace_all(m[,2], "-", "")))
  
  tibble(date = dates, text = m[,3]) %>% filter(!is.na(date))
}

# =====================================================================================
# PARALLEL CACHE BUILDER
# =====================================================================================

load_all_data <- function() {
  tryCatch({
    df <- build_request_data(root_path)
    if (nrow(df) == 0) return(list(df = df, timelines = list()))
    
    npaths <- normalizePath(df$path, winslash = "/", mustWork = FALSE)
    
    timelines <- future_lapply(
      seq_along(npaths),
      function(i) parse_readme_safe(npaths[i])
    )
    names(timelines) <- npaths
    
    list(df = df, timelines = timelines)
    
  }, error = function(e) {
    message("Error in loading data: ", e)
    list(df = tibble(), timelines = list())
  })
}

# =====================================================================================
# UI
# =====================================================================================

header <- dashboardHeader(title = "NFI Enquiries Dashboard")

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = "tabs",
    menuItem("Overview", tabName = "overview", icon = icon("table")),
    menuItem("Detail",   tabName = "detail",   icon = icon("file-alt"))
  ),
  hr(),
  tags$h4("Filters"),
  textInput("search_text", "Search title / notes", placeholder = "Type to search..."),
  uiOutput("status_filter_ui"),
  uiOutput("resp_filter_ui"),
  sliderInput("age_filter", "Age (days)", min = 0, max = 180, value = c(0, 180)),
  hr(),
  helpText("Auto-refresh: disabled")
)

body <- dashboardBody(
  tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
  tabItems(
    # ----------------- Overview -----------------
    tabItem(
      tabName = "overview",
      fluidRow(
        valueBoxOutput("kpi_total", width = 3),
        valueBoxOutput("kpi_overdue", width = 3),
        valueBoxOutput("kpi_avg_age", width = 3),
        valueBoxOutput("kpi_last_refreshed", width = 3)
      ),
      fluidRow(
        box(
          title = "Open Requests",
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          DTOutput("requests_table")
        )
      ),
      fluidRow(
        box(
          title = "Age Distribution (days)",
          width = 6, status = "info", solidHeader = TRUE,
          plotlyOutput("age_plot")
        ),
        box(
          title = "By Responsible",
          width = 6, status = "info", solidHeader = TRUE,
          plotlyOutput("resp_plot")
        )
      )
    ),
    # ----------------- Detail -----------------
    tabItem(
      tabName = "detail",
      fluidRow(
        box(
          title = "Selected Request",
          width = 4, status = "primary", solidHeader = TRUE,
          uiOutput("detail_meta"),
          br(), downloadButton("download_timeline", "Download Timeline (CSV)")
        ),
        box(
          title = "ReadMe Timeline",
          width = 8, status = "primary", solidHeader = TRUE,
          reactableOutput("timeline_reactable")
        )
      )
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = "blue")

# =====================================================================================
# SERVER
# =====================================================================================

server <- function(input, output, session) {
  
  # 1) Load everything once at startup (NO AUTO-REFRESH)
  requests_rv <- reactiveVal(load_all_data())
  
  requests <- reactive({ requests_rv()$df })
  timeline_cache <- reactive({ requests_rv()$timelines })
  
  # ---- Age buckets ----
  add_buckets <- function(df) {
    df %>%
      mutate(
        age_bucket = case_when(
          age_days <  30 ~ "< 30d",
          age_days <  60 ~ "30–59d",
          age_days <  90 ~ "60–89d",
          TRUE           ~ "90d+"
        )
      )
  }
  
  # 4) Populate filters
  observe({
    df <- requests()
    if (nrow(df) == 0) return()
    
    df <- add_buckets(df)
    
    output$status_filter_ui <- renderUI({
      selectInput("status_filter", "Status",
                  choices = sort(unique(df$status)),
                  selected = sort(unique(df$status)),
                  multiple = TRUE)
    })
    
    output$resp_filter_ui <- renderUI({
      selectInput("resp_filter", "Responsible",
                  choices = sort(unique(df$responsible)),
                  selected = sort(unique(df$responsible)),
                  multiple = TRUE)
    })
  })
  
  # ---- Filtering ----
  filtered <- reactive({
    df <- requests()
    if (nrow(df) == 0) return(df)
    df <- add_buckets(df)
    
    df <- df %>% filter(
      is.na(age_days) | between(age_days, input$age_filter[1], input$age_filter[2])
    )
    if (!is.null(input$status_filter)) df <- df %>% filter(status %in% input$status_filter)
    if (!is.null(input$resp_filter))   df <- df %>% filter(is.na(responsible) | responsible %in% input$resp_filter)
    
    if (isTruthy(input$search_text)) {
      s <- tolower(input$search_text)
      df <- df %>% filter(
        grepl(s, tolower(title)) |
          grepl(s, tolower(last_note))
      )
    }
    df
  })
  
  # ---- Overview Table ----
  table_df <- reactive({
    filtered() %>%
      arrange(desc(age_days)) %>%
      mutate(last_update = as.character(last_update)) %>%
      select(code, user, title, responsible, status, age_days, last_update, last_note, path)
  })
  
  output$requests_table <- renderDT({
    df <- table_df()
    datatable(
      df %>% select(-path),
      filter = "top",
      rownames = FALSE,
      selection = "single",
      options = list(pageLength = 15, scrollX = TRUE)
    )
  })
  
  # ---- KPIs ----
  output$kpi_total <- renderValueBox({
    valueBox(nrow(filtered()), "Open Requests", icon = icon("inbox"), color = "aqua")
  })
  
  output$kpi_overdue <- renderValueBox({
    valueBox(nrow(filtered() %>% filter(age_days >= 90)),
             "90d+ Overdue", icon = icon("hourglass-end"), color = "red")
  })
  
  output$kpi_avg_age <- renderValueBox({
    df <- filtered()
    avg <- ifelse(nrow(df) == 0, 0, round(mean(df$age_days, na.rm=TRUE),1))
    valueBox(avg, "Average Age (days)", icon = icon("calendar-day"), color = "yellow")
  })
  
  output$kpi_last_refreshed <- renderValueBox({
    valueBox(format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
             "Loaded at Startup", icon = icon("sync"), color = "green")
  })
  
  # ---- Selection ----
  selected_request <- reactive({
    s <- input$requests_table_rows_selected
    df <- table_df()
    if (length(s) == 0) return(NULL)
    df[s,]
  })
  
  # ---- Detail Meta ----
  output$detail_meta <- renderUI({
    req <- selected_request()
    validate(need(!is.null(req), "Select a request"))
    
    wellPanel(
      strong("Code: "), req$code, br(),
      strong("User: "), req$user, br(),
      strong("Title: "), req$title, br(),
      strong("Responsible: "), req$responsible %||% "(Unassigned)", br(),
      strong("Status: "), req$status, br(),
      strong("Age (days): "), req$age_days, br(),
      strong("Last update: "), req$last_update, br(),
      strong("Last note: "), req$last_note
    )
  })
  
  # =====================================================================================
  # AGE PLOT
  # =====================================================================================
  
  output$age_plot <- renderPlotly({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data available"))
    
    plot_ly(
      df,
      x = ~age_days,
      type = "histogram",
      marker = list(color = "#2C82C9")
    ) %>%
      layout(
        xaxis = list(title = "Age (days)"),
        yaxis = list(title = "Count")
      )
  })
  
  # =====================================================================================
  # RESPONSIBLE PLOT
  # =====================================================================================
  
  output$resp_plot <- renderPlotly({
    df <- filtered()
    validate(need(nrow(df) > 0, "No data available"))
    
    summary <- df %>% count(responsible)
    
    plot_ly(
      summary,
      x = ~responsible,
      y = ~n,
      type = "bar",
      marker = list(color = "#28A745")
    ) %>%
      layout(
        xaxis = list(title = "Responsible"),
        yaxis = list(title = "Requests"),
        margin = list(b = 80)
      )
  })
  
  # =====================================================================================
  # REACTABLE TIMELINE
  # =====================================================================================
  
  output$timeline_reactable <- renderReactable({
    req <- selected_request()
    validate(need(!is.null(req), "Select a request"))
    
    p <- normalizePath(req$path, winslash = "/", mustWork = FALSE)
    tl <- timeline_cache()[[p]]
    
    if (is.null(tl) || nrow(tl) == 0) {
      return(reactable(data.frame(Message = "No timeline entries found")))
    }
    
    reactable(
      tl,
      columns = list(
        date = colDef(name = "Date", width = 120),
        text = colDef(show = FALSE)
      ),
      striped = TRUE,
      highlight = TRUE,
      pagination = TRUE,
      defaultPageSize = 10,
      details = function(index) {
        htmltools::div(
          style = "
            background-color: #f8f9fa;
            padding: 12px;
            border-left: 3px solid #007bff;
            white-space: pre-wrap;
            font-family: Consolas, 'Courier New', monospace;
          ",
          tl$text[index]
        )
      }
    )
  })
  
  # ---- CSV Download ----
  output$download_timeline <- downloadHandler(
    filename = function() {
      req <- selected_request()
      paste0(req$code, "_timeline_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      req <- selected_request()
      p <- normalizePath(req$path, winslash="/", mustWork = FALSE)
      tl <- timeline_cache()[[p]] %||% tibble(date=as.Date(character()), text="")
      readr::write_csv(tl, file)
    }
  )
  
  observeEvent(input$requests_table_rows_selected, {
    updateTabItems(session, "tabs", "detail")
  })
}

# =====================================================================================
# RUN APP
# =====================================================================================

shinyApp(ui, server)