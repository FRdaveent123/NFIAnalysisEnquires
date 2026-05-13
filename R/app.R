# =====================================================================
# app.R  -- Enquiries Dashboard
# =====================================================================
# Purpose: Overview and triage of open enquiries

#setwd("C:/Users/david.entwistle/OneDrive - Forest Research/Documents/Project/NFIAnalysisEnquires/R")

# =====================================================================
# Package setup:
# =====================================================================

# =====================================================================
# Package load
# =====================================================================

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
source("helpers.R")
source("fr_branding_css.R")

# =====================================================================
# DASHBOARD UI
# =====================================================================

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
    uiOutput("age_filter_ui"),
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
  
  RDS_PATH <- "nfi_dashboard_export.rds"
  
  uploaded_data <- reactiveVal(NULL)
  chart_status_filter <- reactiveVal(NULL)
  chart_owner_filter  <- reactiveVal(NULL)
  chart_org_filter <- reactiveVal(NULL)
  
  # Load dashboard data  
  
  observe({
    if (!is.null(uploaded_data())) return()
    
    if (!file.exists(RDS_PATH)) {
      showNotification(
        "Dashboard data file not found.",
        type = "error",
        duration = NULL
      )
      return()
    }
    
    obj <- try(readRDS(RDS_PATH), silent = TRUE)
    
    required <- c("df", "timeline_cache", "header_meta")
    if (inherits(obj, "try-error") || !all(required %in% names(obj))) {
      showNotification(
        "Failed to load dashboard data. Check the export file.",
        type = "error",
        duration = NULL
      )
      return()
    }
    
    uploaded_data(obj)
    
    showNotification(
      paste("Data loaded automatically.", "Requests:", nrow(obj$df)),
      type = "message"
    )
  })
  
  observeEvent(input$requests_table_rows_selected, {
    req(uploaded_data())
    updateTabItems(session, "tabs", "detail")
  })
  
  observeEvent(input$reset_chart_filter, {
    chart_status_filter(NULL)
    chart_owner_filter(NULL)
    chart_org_filter(NULL)
  })
  
  requests <- reactive({
    req(uploaded_data())
    uploaded_data()$df
  })
  
  timeline_cache <- reactive({
    req(uploaded_data())
    uploaded_data()$timeline_cache
  })
  
  header_meta_df <- reactive({
    req(uploaded_data())
    uploaded_data()$header_meta
  })
  
  filtered_meta_with_charts <- reactive({
    df <- filtered_meta()
    
    if (!is.null(chart_status_filter())) {
      df <- df %>% filter(Status == chart_status_filter())
    }
    
    if (!is.null(chart_owner_filter())) {
      df <- df %>% filter(Owner == chart_owner_filter())
    }
    
    if (!is.null(chart_org_filter())) {
      df <- df %>% filter(Organisation == chart_org_filter())
    }
    
    df
  })
  
  output$age_filter_ui <- renderUI({
    df <- requests()
    req(df)
    
    max_age <- max(df$age_days, na.rm = TRUE)
    
    sliderInput(
      "age_filter",
      "Age (days)",
      min = 0,
      max = max_age,
      value = c(0, max_age)
    )
  })
  
  
  
  add_buckets <- function(df) {
    df %>% mutate(
      age_bucket = case_when(
        age_days < 30 ~ "<30d",
        age_days < 60 ~ "30–59d",
        age_days < 90 ~ "60–89d",
        TRUE          ~ "90d+"
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
  
  table_df <- reactive({
    df <- filtered()
    meta <- header_meta_df()
    
    df %>%
      left_join(meta, by = "path") %>%
      mutate(
        `Owner (initials)` = str_replace(`Owner (initials)`, "^\\(initials\\):\\s*", ""),
        `Chargeable (Y/N)` = str_replace(`Chargeable (Y/N)`, "^\\(Y/N\\):\\s*", ""),
        StatusID = recode(
          as.character(StatusID),
          "1" = "waiting on client",
          "2" = "work in progress",
          "3" = "internal meeting planned",
          "4" = "client review/completed",
          .default = "(unknown)"
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
  
  status_colours <- c(
    "waiting on client"        = "#FF7F0E",
    "work in progress"         = "#1F77B4",
    "internal meeting planned" = "#9467BD",
    "client review/completed"  = "#2CA02C"
  )
  
  output$requests_table <- renderDT({
    
    df <- table_df()
    
    if (!is.null(chart_status_filter())) {
      df <- df %>% filter(Status == chart_status_filter())
    }
    
    if (!is.null(chart_owner_filter())) {
      df <- df %>% filter(Owner == chart_owner_filter())
    }
    
    if (!is.null(chart_org_filter())) {
      df <- df %>% filter(Organisation == chart_org_filter())
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
    table_df()[s, ]
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
    avg <- ifelse(nrow(df) == 0, 0, round(mean(df$age_days, na.rm = TRUE), 1))
    
    styledValueBox(
      avg,
      "Average Age (days)",
      icon("calendar-day"),
      "purple"
    )
  })
  
  output$kpi_last_refreshed <- renderValueBox({
    styledValueBox(
      format(file.info(RDS_PATH)$mtime, "%Y-%m-%d %H:%M:%S"),
      "Data Last Refreshed",
      icon("sync"),
      "green"
    )
  })
  
  meta_df2 <- reactive({
    header_meta_df() %>%
      mutate(
        `Owner (initials)` = str_replace(`Owner (initials)`, "^\\(initials\\):\\s*", "")
      )
  })
  
  output$owner_plot <- renderPlotly({
    d <- filtered_meta_with_charts() %>%
      filter(!is.na(Owner), Owner != "") %>%
      count(Owner)
    
    plot_ly(
      data   = d,
      x      = ~Owner,
      y      = ~n,
      type   = "bar",
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
      type = "bar",
      source = "org_plot"
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
      data   = d,
      x      = ~Status,
      y      = ~n,
      type   = "bar",
      color  = ~Status,
      colors = status_colours,
      source = "status_plot"
    ) %>%
      layout(
        yaxis = list(title = ""),
        xaxis = list(title = ""),
        showlegend = FALSE
      )
  })
  
  # Capture chart interactions
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
  
  org_clicked <- reactive({
    d <- event_data("plotly_click", source = "org_plot")
    if (is.null(d)) return(NULL)
    d$x
  })
  
  observeEvent(org_clicked(), {
    chart_org_filter(org_clicked())
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
      strong("Age (days):"), req_list$age_days
    )
  })
  
  output$timeline_reactable <- renderReactable({
    req <- selected_request()
    tl <- timeline_cache()[[req$path]]$timeline
    if (is.null(tl) || nrow(tl) == 0) {
      reactable(data.frame(Message = "No timeline entries found"))
    } else {
      reactable(tl, striped = TRUE, highlight = TRUE, defaultPageSize = 1000)
    }
  })
}

# =====================================================================
# RUN APP
# =====================================================================

ui <- fluidPage(
  useShinyjs(),
  fr_branding_css,
  dashboard_ui
)

shinyApp(ui = ui, server = server)