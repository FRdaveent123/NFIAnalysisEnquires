# =====================================================================
# app.R  -- Enquiries Dashboard (Stable, Posit-safe)
# =====================================================================

#setwd("C:/Users/david.entwistle/OneDrive - Forest Research/Documents/Project/NFIAnalysisEnquires/R")

library(shiny)
library(shinydashboard)
library(DT)
library(dplyr)
library(stringr)
library(lubridate)
library(plotly)
library(reactable)
library(htmltools)
library(tidyr)
library(shinycssloaders)
library(shinyjs)
source("fr_branding_css.R")
source("helpers.R")


# =====================================================================
# UPLOAD GATE
# =====================================================================

upload_gate_ui <- div(
  class = "upload-bg",
  tags$img(src = "FR_RGB.jpg", class = "fr-logo-top-right"),
  fluidRow(
    column(
      width = 6, offset = 3,
      box(
        width = NULL,
        title = "Upload NFI Dashboard Export File",
        status = "primary",
        solidHeader = TRUE,
        class = "upload-card",
        
        tags$p(strong("Export folder location (copy & paste into File Explorer):")),
        tags$pre(
          "O:\\0600_Advice_Enquiries_Support\\0602_Open_Requests\\enquires_dashboard_files"
        ),
        tags$p(
          "Upload the file: ",
          strong("nfi_dashboard_export.rds")
        ),
        br(),
        
        fileInput(
          "upload_data",
          "Choose nfi_dashboard_export.rds",
          accept = ".rds",
          width = "100%"
        )
      )
    )
  )
)

# =====================================================================
# DASHBOARD UI
# =====================================================================

status_colours <- c(
  "waiting on client"        = "#FF7F0E",
  "work in progress"         = "#1F77B4",
  "internal meeting planned" = "#9467BD",
  "client review/completed"  = "#2CA02C"
)

dashboard_ui <- dashboardPage(
  dashboardHeader(
    title = tagList(
      span("Enquiries Dashboard"),
      tags$img(src = "FR_RGB.jpg", class = "fr-header-logo")
    )
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Overview", tabName = "overview", icon = icon("table")),
      menuItem("Detail", tabName = "detail", icon = icon("file-alt"))
    ),
    hr(),
    tags$h4("Filters"),
    textInput("search_text", "Search title / notes"),
    sliderInput("age_filter", "Age (days)", min = 0, max = 180, value = c(0,180)),
    hr()
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "overview",
        fluidRow(
          valueBoxOutput("kpi_total", width = 3),
          valueBoxOutput("kpi_overdue", width = 3),
          valueBoxOutput("kpi_avg_age", width = 3),
          valueBoxOutput("kpi_last_refreshed", width = 3)
        ),
        fluidRow(
          # LEFT: Open Requests table
          box(
            title = "Open Requests",
            width = 8,
            status = "primary",
            solidHeader = TRUE,
            
            actionButton(
              "reset_chart_filter",
              "Reset filter",
              icon = icon("undo"),
              class = "btn-default",
              style = "margin-bottom:6px;"
            ),
            
            DTOutput("requests_table") %>% withSpinner()
          ),
          
          # RIGHT: stacked charts
          column(
            width = 4,
            
            box(
              title = "Requests by Owner",
              width = 12,
              status = "info",
              plotlyOutput("owner_plot", height = "240px") %>% withSpinner()
            ),
            
            box(
              title = "Status Distribution",
              width = 12,
              status = "info",
              plotlyOutput("statusid_plot", height = "240px") %>% withSpinner()
            )
          )
        ),
        
        # Keep lower charts full width
        fluidRow(
          box(
            title="Organisation Distribution",
            width=12,
            status="info",
            plotlyOutput("organisation_plot") %>% withSpinner()
          )
        )
      ),
      
      tabItem(
        tabName = "detail",
        fluidRow(
          box(title="Selected Request", width=4, status="primary", uiOutput("detail_meta")),
          box(title="ReadMe Timeline", width=8, status="primary", reactableOutput("timeline_reactable"))
        )
      )
    )
  ),
  
  skin = "black"
)

# =====================================================================
# SERVER
# =====================================================================

server <- function(input, output, session) {
  
  uploaded_data <- reactiveVal(NULL)
  
  observeEvent(input$upload_data, {
    req(input$upload_data)
    
    # Filename validation
    if (basename(input$upload_data$name) != "nfi_dashboard_export.rds") {
      showNotification(
        "Incorrect file selected. Please upload nfi_dashboard_export.rds.",
        type = "error",
        duration = NULL
      )
      return()
    }
    
    
    obj <- try(readRDS(input$upload_data$datapath), silent = TRUE)
    
    # Structure validation (minimal, non-breaking)
    required <- c("df", "timeline_cache", "header_meta")
    if (inherits(obj, "try-error") || !all(required %in% names(obj))) {
      showNotification(
        "Invalid export file. Please re-run the export script.",
        type = "error",
        duration = NULL
      )
      return()
    }
    
    uploaded_data(obj)
    
    showNotification(
      paste("Export loaded successfully.", "Requests:", nrow(obj$df)),
      type = "message"
    )
  })
  
  observeEvent(uploaded_data(), {
    hide("upload_section")
    show("dashboard_section")
  })
  
  observeEvent(input$requests_table_rows_selected, {
    if (!is.null(uploaded_data())) {
      updateTabItems(session, "tabs", "detail")
    }
  })
  
  # Data accessors
  requests <- reactive(uploaded_data()$df)
  timeline_cache <- reactive(uploaded_data()$timeline_cache)
  header_meta_df <- reactive(uploaded_data()$header_meta)
  
  
  
  filtered_meta_with_charts <- reactive({
    df <- filtered_meta()
    
    if (!is.null(chart_status_filter())) {
      df <- df %>% filter(Status == chart_status_filter())
    }
    
    if (!is.null(chart_owner_filter())) {
      df <- df %>% filter(Owner == chart_owner_filter())
    }
    
    df
  })
  
  
  
  chart_status_filter <- reactiveVal(NULL)
  chart_owner_filter  <- reactiveVal(NULL)
  
  observeEvent(input$reset_chart_filter, {
    chart_status_filter(NULL)
    chart_owner_filter(NULL)
  })
  

  
  add_buckets <- function(df) {
    df %>% mutate(
      age_bucket = case_when(
        age_days < 30 ~ "<30d",
        age_days < 60 ~ "30–59d",
        age_days < 90 ~ "60–89d",
        TRUE ~ "90d+"
      )
    )
  }
  
  filtered <- reactive({
    df <- requests()
    req(nrow(df) > 0)
    
    df <- add_buckets(df)
    
    df <- df %>%
      filter(
        is.na(age_days) |
          between(age_days, input$age_filter[1], input$age_filter[2])
      )
    
    if (isTruthy(input$search_text)) {
      s <- tolower(input$search_text)
      df <- df %>%
        filter(grepl(s, tolower(title)) | grepl(s, tolower(last_note)))
    }
    
    df
  })

  filtered_meta <- reactive({
    df <- filtered()
    meta <- header_meta_df()
    
    df %>%
      left_join(meta, by = "path") %>%
      mutate(
        Owner = str_replace(`Owner (initials)`, "^\\(initials\\):\\s*", ""),
        Status = recode(
          as.character(StatusID),
          "1" = "waiting on client",
          "2" = "work in progress",
          "3" = "internal meeting planned",
          "4" = "client review/completed"
        )
      )
  })
  
  table_df <- reactive({
    df <- filtered()
    meta <- header_meta_df()
    
    df %>%
      left_join(meta, by = "path") %>%
      mutate(
        # Remove "(initials): " from Owner
        `Owner (initials)` = str_replace(
          `Owner (initials)`,
          "^\\(initials\\):\\s*",
          ""
        ),
        
        # Remove "(Y/N): " from Chargeable
        `Chargeable (Y/N)` = str_replace(
          `Chargeable (Y/N)`,
          "^\\(Y/N\\):\\s*",
          ""
        ),
        
        # Recode StatusID
        StatusID = recode(
          as.character(StatusID),
          "1"="waiting on client",
          "2"="work in progress",
          "3"="internal meeting planned",
          "4"="client review/completed",
          .default="(unknown)"
        )
      ) %>%
      select(
        code, user, title,
        Owner = `Owner (initials)`,
        Status = StatusID,
        Organisation,
        Chargeable = `Chargeable (Y/N)`,
        age_days,
        path
      )
    
    
  })
  
  output$requests_table <- renderDT({
    
    df <- table_df()
    
    if (!is.null(chart_status_filter())) {
      df <- df %>% filter(Status == chart_status_filter())
    }
    
    if (!is.null(chart_owner_filter())) {
      df <- df %>% filter(Owner == chart_owner_filter())
    }
    
    datatable(
      df %>% select(-path),
      filter = "top",
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        scrollCollapse = TRUE,
        autoWidth = TRUE,
        dom = "tip"
      )
    ) %>%
      formatStyle(
        "Status",
        backgroundColor = styleEqual(
          names(status_colours),
          unname(status_colours)
        ),
        color = "white",
        fontWeight = "600"
      )
  })
  
  
  selected_request <- reactive({
    s <- input$requests_table_rows_selected
    if (!length(s)) return(NULL)
    table_df()[s,]
  })
  
  output$kpi_total <- renderValueBox({
    styledValueBox(
      nrow(filtered()),
      "Open Requests",
      icon("inbox"),
      "purple"
    )
  })
  
  output$kpi_overdue <- renderValueBox({
    styledValueBox(
      nrow(filtered() %>% filter(age_days >= 90)),
      "90d+ Overdue",
      icon("hourglass-end"),
      "red"
    )
  })
  
  output$kpi_avg_age <- renderValueBox({
    df <- filtered()
    avg <- ifelse(nrow(df)==0,0, round(mean(df$age_days, na.rm=TRUE),1))
    styledValueBox(
      avg,
      "Average Age (days)",
      icon("calendar-day"),
      "purple"
    )
  })
  
  output$kpi_last_refreshed <- renderValueBox({
    styledValueBox(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "Data Load Time",
      icon("sync"),
      "green"
    )
    
  })
  

  output$owner_plot <- renderPlotly({
    
    d <- filtered_meta_with_charts() %>%
      filter(!is.na(Owner), Owner != "") %>%
      count(Owner)
    
    plot_ly(
      data = d,
      x = ~Owner,
      y = ~n,
      type = "bar",
      source = "owner_plot"
    ) %>%
      layout(
        yaxis = list(title = ""),
        xaxis = list(title = ""),
        showlegend = FALSE
      )
  })
  
  
  output$organisation_plot <- renderPlotly({
    
    d <- filtered_meta_with_charts() %>%
      filter(!is.na(Organisation), Organisation != "", Organisation != "0") %>%
      count(Organisation)
    
    plot_ly(
      data = d,
      x = ~Organisation,
      y = ~n,
      type = "bar"
    ) %>%
      layout(
        yaxis = list(title = ""),
        xaxis = list(title = ""),
        showlegend = FALSE
      )
  })
  
  output$statusid_plot <- renderPlotly({
    
    d <- filtered_meta_with_charts() %>%
      filter(Status %in% names(status_colours)) %>%
      count(Status)
    
    plot_ly(
      data = d,
      x = ~Status,
      y = ~n,
      type = "bar",
      color = ~Status,
      colors = status_colours,
      source = "status_plot"
    ) %>%
      layout(
        yaxis = list(title = ""),
        xaxis = list(title = ""),
        showlegend = FALSE
      )
  })

  # ---------------------------
  # Chart click interactions
  # ---------------------------
  
  status_clicked <- reactive({
    d <- event_data("plotly_click", source = "status_plot")
    if (is.null(d)) return(NULL)
    d$x
  })
  
  owner_clicked <- reactive({
    d <- event_data("plotly_click", source = "owner_plot")
    if (is.null(d)) return(NULL)
    d$x
  })
  
  observeEvent(status_clicked(), {
    chart_status_filter(status_clicked())
  })
  
  observeEvent(owner_clicked(), {
    chart_owner_filter(owner_clicked())
  })
  
  
  output$detail_meta <- renderUI({
    req <- selected_request()
    if (is.null(req)) return(NULL)
    req_list <- as.list(req)
    
    h <- header_meta_df() %>% filter(path == req_list$path)
    
    wellPanel(
      strong("Code:"), req_list$code, br(),
      strong("User:"), req_list$user, br(),
      strong("Title:"), req_list$title, br(),
      hr(),
      strong("Owner:"), h$`Owner (initials)` %||% "(None)", br(),
      strong("Chargeable:"), h$`Chargeable (Y/N)` %||% "(None)", br(),
      strong("Organisation:"), h$Organisation %||% "(None)", br(),
      strong("Time Spent:"), h$`Time Spent (number counting days)` %||% "(None)", br(),
      strong("StatusID:"), h$StatusID %||% "(None)", br(),
      hr(),
      strong("Age (days):"), req_list$age_days, br(),
      strong("Last update:"), req_list$last_update, br(),
      strong("Last note:"), req_list$last_note
    )
  })
  
  output$timeline_reactable <- renderReactable({
    req <- selected_request()
    tl <- timeline_cache()[[req$path]]$timeline
    if (is.null(tl) || nrow(tl) == 0)
      return(reactable(data.frame(Message = "No timeline entries found")))
    reactable(tl, striped = TRUE, highlight = TRUE, defaultPageSize = 1000)
  })
}


# =====================================================================
# RUN APP
# =====================================================================

ui <- fluidPage(
  useShinyjs(),
  fr_branding_css,
  div(id = "upload_section", upload_gate_ui),
  hidden(div(id = "dashboard_section", dashboard_ui))
)

shinyApp(ui = ui, server = server)