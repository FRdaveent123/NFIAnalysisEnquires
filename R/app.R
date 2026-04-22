# =====================================================================
# app.R  -- Enquiries Dashboard (Posit-safe, guided upload)
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

# =====================================================================
# FOREST RESEARCH BRANDING CSS
# =====================================================================

fr_branding_css <- tags$head(
  tags$style(HTML("

    :root {
      --fr-purple: #6E177E;
      --fr-purple-dark: #4E0F59;
      --fr-purple-light: #A884B2;
      --fr-bg-light: #FAF9FC;
      --fr-bg-panel: #FFFFFF;
      --fr-border: #DDDDE2;
      --fr-text: #2A2A2A;
    }

    body {
      background-color: var(--fr-bg-light) !important;
      color: var(--fr-text);
      font-family: 'Segoe UI', sans-serif;
    }

    .skin-black .main-header .navbar,
    .skin-black .main-header .logo {
      background-color: var(--fr-purple) !important;
      height: 60px !important;
      line-height: 60px !important;
      padding-left: 20px !important;
      border: none !important;
      color: white !important;
      font-weight: 600 !important;
      font-size: 22px !important;
    }

    .fr-header-logo {
      position: absolute;
      right: 20px;
      top: 5px;
      height: 50px;
    }

    .skin-black .main-header .navbar .sidebar-toggle,
    .sidebar-toggle {
      display: none !important;
    }

    .main-sidebar {
      margin-top: 60px !important;
      width: 300px !important;
      background-color: white !important;
      border-right: 1px solid var(--fr-border);
    }

    .box {
      border-radius: 10px !important;
      border: 1px solid var(--fr-border) !important;
      background-color: var(--fr-bg-panel) !important;
    }

    .box-header {
      background-color: var(--fr-purple) !important;
      color: white !important;
      font-weight: 600 !important;
    }

    .btn-default {
      background-color: var(--fr-purple) !important;
      color: white !important;
      border-radius: 6px !important;
      border: none !important;
    }

  "))
)

# =====================================================================
# UPLOAD GATE — GUIDED, VALIDATED UPLOAD
# =====================================================================

upload_gate_ui <- div(
  tags$img(src = "FR_RGB.jpg", class = "fr-header-logo"),
  
  box(
    width = 6,
    offset = 3,
    title = "Upload NFI Dashboard Export File",
    status = "primary",
    solidHeader = TRUE,
    
    tags$p(strong("Step 1 – Locate the export file on your computer")),
    tags$p("Open File Explorer and paste this path into the address bar:"),
    tags$pre(
      "O:\\0600_Advice_Enquiries_Support\\0602_Open_Requests\\enquires_dashboard_files"
    ),
    tags$p(
      "Select ",
      strong("nfi_dashboard_export.rds"),
      " and upload it below."
    ),
    br(),
    
    fileInput(
      "upload_data",
      "Upload nfi_dashboard_export.rds",
      accept = ".rds",
      width = "100%"
    ),
    
    uiOutput("upload_info")
  )
)

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
      menuItem("Overview", tabName = "overview"),
      menuItem("Detail", tabName = "detail")
    ),
    hr(),
    tags$h4("Filters"),
    textInput("search_text", "Search title / notes"),
    sliderInput("age_filter", "Age (days)", min = 0, max = 180, value = c(0,180))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(
        "overview",
        DTOutput("requests_table")
      ),
      tabItem(
        "detail",
        uiOutput("detail_meta"),
        reactableOutput("timeline_reactable")
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
    
    # ✅ Filename validation
    if (basename(input$upload_data$name) != "nfi_dashboard_export.rds") {
      showNotification(
        "Incorrect file. Please upload nfi_dashboard_export.rds.",
        type = "error",
        duration = NULL
      )
      return()
    }
    
    obj <- try(readRDS(input$upload_data$datapath), silent = TRUE)
    
    # ✅ Structural validation
    required <- c("df", "timeline_cache", "header_meta", "exported_at")
    if (inherits(obj, "try-error") || !all(required %in% names(obj))) {
      showNotification(
        "Invalid export file. Please re-run the export script.",
        type = "error",
        duration = NULL
      )
      return()
    }
    
    uploaded_data(obj)
    
    # ✅ Export age warning
    age_hrs <- difftime(Sys.time(), obj$exported_at, units = "hours")
    if (age_hrs > 2) {
      showNotification(
        "Warning: export file is more than 2 hours old.",
        type = "warning",
        duration = NULL
      )
    }
  })
  
  observeEvent(uploaded_data(), {
    hide("upload_section")
    show("dashboard_section")
  })
  
  # ✅ Confirm successful upload
  output$upload_info <- renderUI({
    req(uploaded_data())
    tags$div(
      style = "background:#EAF6ED; padding:10px; border-radius:6px;",
      strong("Export loaded successfully ✅"),
      tags$p(paste("Records:", nrow(uploaded_data()$df))),
      tags$p(paste("Loaded:", format(uploaded_data()$exported_at)))
    )
  })
  
  output$requests_table <- renderDT({
    req(uploaded_data())
    datatable(uploaded_data()$df, selection = "single")
  })
  
  observeEvent(input$requests_table_rows_selected, {
    updateTabItems(session, "tabs", "detail")
  })
  
  output$detail_meta <- renderUI({
    req(input$requests_table_rows_selected)
    str(uploaded_data()$df[input$requests_table_rows_selected, ])
  })
  
  output$timeline_reactable <- renderReactable({
    req(input$requests_table_rows_selected)
    path <- uploaded_data()$df$path[input$requests_table_rows_selected]
    tl <- uploaded_data()$timeline_cache[[path]]$timeline
    reactable(tl, defaultPageSize = 100)
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