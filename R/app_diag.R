# app_diag.R  — minimal diagnostics app for NFI dashboard
# -------------------------------------------------------

library(shiny)
library(DT)
library(dplyr)
library(yaml)

setwd("C:/Users/david.entwistle/OneDrive - Forest Research/Documents/Project/NFI_Enquiries_Dashboard")

# --- Load config (force a clear, actionable error if missing)
cfg <- yaml::read_yaml("config/paths.yaml")
root_path <- cfg$paths$enquiries_root

# --- Source your data logic (same files you already have)
source("R/file_scanner.R")
source("R/readme_parser.R")
source("R/helpers.R")

# --- Helper to safely build data and capture problems
load_data_safe <- function() {
  out <- list()
  out$wd <- getwd()
  out$root_path <- root_path
  out$root_exists <- dir.exists(root_path)
  
  # List first-level request folders (or error)
  out$folders <- tryCatch(
    list.dirs(root_path, recursive = FALSE),
    error = function(e) e
  )
  out$n_folders <- if (inherits(out$folders, "error")) NA_integer_ else length(out$folders)
  
  # Build the request data using your real pipeline
  t0 <- Sys.time()
  out$data <- tryCatch(
    build_request_data(root_path),
    error = function(e) e
  )
  out$elapsed_sec <- round(as.numeric(Sys.time() - t0, units = "secs"), 2)
  
  # Summaries
  if (inherits(out$data, "error")) {
    out$n_rows <- NA_integer_
    out$cols <- NA_character_
  } else if (is.null(out$data)) {
    out$n_rows <- 0L
    out$cols <- character()
    out$data <- tibble()  # make sure it's a tibble, not NULL
  } else {
    out$n_rows <- nrow(out$data)
    out$cols <- names(out$data)
  }
  
  out
}

ui <- fluidPage(
  titlePanel("NFI Diagnostics — Minimal"),
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh"),
      tags$hr(),
      verbatimTextOutput("summary")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table",
                 DTOutput("table")),
        tabPanel("Folders Found",
                 verbatimTextOutput("folders_print")),
        tabPanel("Head of Data",
                 verbatimTextOutput("data_head"))
      )
    )
  )
)

server <- function(input, output, session) {
  diag <- reactiveVal(load_data_safe())
  
  observeEvent(input$refresh, ignoreInit = TRUE, {
    diag(load_data_safe())
  })
  
  output$summary <- renderPrint({
    d <- diag()
    list(
      working_directory = d$wd,
      root_path = d$root_path,
      root_exists = d$root_exists,
      n_folders_found = d$n_folders,
      data_build_elapsed_seconds = d$elapsed_sec,
      n_rows_in_data = d$n_rows,
      columns = d$cols
    )
  })
  
  output$folders_print <- renderPrint({
    d <- diag()
    if (inherits(d$folders, "error")) {
      cat("ERROR listing folders:\n")
      print(d$folders)
    } else {
      cat("First-level request folders:\n")
      print(d$folders)
    }
  })
  
  output$data_head <- renderPrint({
    d <- diag()
    if (inherits(d$data, "error")) {
      cat("ERROR building data:\n")
      print(d$data)
    } else if (nrow(d$data) == 0) {
      cat("No rows returned from build_request_data().")
    } else {
      print(utils::head(d$data, 10))
    }
  })
  
  output$table <- renderDT({
    d <- diag()
    if (inherits(d$data, "error")) {
      DT::datatable(data.frame(ERROR = as.character(d$data)))
    } else {
      DT::datatable(d$data, options = list(pageLength = 15, scrollX = TRUE))
    }
  })
}

shinyApp(ui, server)