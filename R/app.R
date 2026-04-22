# =====================================================================
# app.R  -- Enquiries Dashboard (Stable, Posit-safe)
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

    /* ===============================
       HEADER
       =============================== */
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
      text-shadow: 0px 1px 2px rgba(0,0,0,0.25);
    }

    .fr-header-logo {
      position: absolute;
      right: 20px;
      top: 5px;
      height: 50px;
    }

    /* Remove sidebar toggle entirely */
    .skin-black .main-header .navbar .sidebar-toggle,
    .sidebar-toggle {
      display: none !important;
    }

    body.sidebar-collapse .main-sidebar {
      margin-left: 0 !important;
    }
    body.sidebar-collapse .content-wrapper {
      margin-left: 300px !important;
    }

    .content-wrapper,
    .right-side {
      margin-top: 0 !important;
      background-color: var(--fr-bg-light) !important;
    }

    /* ===============================
       SIDEBAR
       =============================== */
    .main-sidebar {
      margin-top: 60px !important;
      background-color: white !important;
      border-right: 1px solid var(--fr-border);
      width: 300px !important;
      padding: 10px;
    }

    .sidebar-menu > li > a {
      color: var(--fr-text) !important;
      padding: 12px 20px !important;
      font-size: 15px !important;
      font-weight: 500 !important;
    }

    .sidebar-menu > li.active > a,
    .sidebar-menu > li:hover > a {
      background-color: var(--fr-purple-light) !important;
      color: white !important;
    }

    .sidebar-menu i { 
      color: var(--fr-purple) !important; 
    }

    /* ===============================
       FILTERS – VISUAL CLARITY ✅
       =============================== */

    /* Filters heading */
    .sidebar h4 {
      color: #000000 !important;
      font-weight: 700;
      margin-top: 10px;
      margin-bottom: 10px;
    }

    /* Group filter controls visually */
    .sidebar .form-group {
      background-color: #F6F6F9;
      padding: 12px;
      border-radius: 8px;
      border: 1px solid #E0E0E5;
      margin-bottom: 12px;
    }

    /* Search box text + border */
    .sidebar .form-control {
      background-color: #FFFFFF !important;
      color: #000000 !important;
      border: 1px solid #8F8FA3 !important;
      border-radius: 6px !important;
    }

    .sidebar .form-control::placeholder {
      color: #6F6F6F;
    }

    .sidebar .form-control:focus {
      border-color: var(--fr-purple) !important;
      box-shadow: 0 0 0 2px rgba(110,23,126,0.15);
    }

    /* Slider label text (Age (days)) */
    .sidebar .control-label {
      color: #000000 !important;
      font-weight: 600;
    }

    /* Slider track */
    .sidebar .irs-bar,
    .sidebar .irs-bar-edge {
      background-color: var(--fr-purple) !important;
    }

    /* Slider handles */
    .sidebar .irs-handle {
      border-color: var(--fr-purple) !important;
      background-color: #FFFFFF !important;
    }

    /* Slider min/max bubbles */
    .sidebar .irs-from,
    .sidebar .irs-to,
    .sidebar .irs-single {
      background-color: var(--fr-purple) !important;
      color: white !important;
      font-size: 11px;
    }

    /* ===============================
       BOXES
       =============================== */
    .box {
      background-color: var(--fr-bg-panel) !important;
      border-radius: 10px !important;
      border: 1px solid var(--fr-border) !important;
      box-shadow: 0 2px 6px rgba(0,0,0,0.06) !important;
    }

    .box-header {
      background-color: var(--fr-purple) !important;
      color: white !important;
      font-weight: 600 !important;
      border-radius: 10px 10px 0 0 !important;
      border-bottom: 1px solid var(--fr-border) !important;
    }

    /* ===============================
       BUTTONS
       =============================== */
    .btn-default {
      background-color: var(--fr-purple) !important;
      color: white !important;
      border-radius: 6px !important;
      border: none !important;
    }

    .btn-default:hover {
      background-color: var(--fr-purple-dark) !important;
    }
    
    /* ===============================
   CHART BOX COLOURS (FR BRANDING)
   =============================== */

/* Open Requests (primary) */
.box.box-primary > .box-header {
  background-color: #6E177E !important;  /* FR Purple */
}

/* Requests by Owner */
.box.box-info > .box-header {
  background-color: #2E6F7E !important;  /* Muted teal */
}

/* Status Distribution */
.box.box-warning > .box-header {
  background-color: #2A4F8A !important;  /* Deep blue */
}

/* Organisation Distribution */
.box.box-danger > .box-header {
  background-color: #8D5A7A !important;  /* Soft plum */
}

/* Ensure header text is readable */
.box > .box-header {
  color: #FFFFFF !important;
  font-weight: 600;
}

/* ======================================================
   AGGRESSIVE SIDEBAR SQUEEZE (LAPTOP SCREENS)
   ====================================================== */

@media (max-width: 1600px) {

  /* Shrink sidebar hard */
  .main-sidebar {
    width: 150px !important;
    padding: 6px !important;
  }

  /* Force main content to move left */
  .content-wrapper,
  .right-side {
    margin-left: 150px !important;
  }

  /* Compress sidebar menu */
  .sidebar-menu > li > a {
    padding: 8px 10px !important;
    font-size: 13px !important;
  }

  /* Compress filter blocks */
  .sidebar .form-group {
    padding: 6px !important;
    margin-bottom: 6px !important;
  }

  /* Smaller filter headings */
  .sidebar h4 {
    font-size: 13px !important;
    margin-bottom: 6px !important;
  }

  /* Smaller labels */
  .sidebar .control-label {
    font-size: 12px !important;
  }

  /* Compact inputs */
  .sidebar .form-control {
    font-size: 12px !important;
    padding: 4px 6px !important;
  }
}

/* ===============================
   COMPACT PLOTLY CHARTS (LAPTOP)
   =============================== */
@media (max-width: 1600px) {
  .plotly.html-widget {
    height: 180px !important;
  }
}


/* ======================================================
   REMOVE DEAD SPACE – DASHBOARD DENSITY TUNING
   ====================================================== */

/* Reduce vertical spacing between rows */
.content .row {
  margin-bottom: 6px !important;
}

/* Reduce space between boxes */
.content .box {
  margin-bottom: 8px !important;
}

/* Tighten box headers */
.box-header {
  padding: 6px 10px !important;
  min-height: auto !important;
}

/* Tighten box bodies */
.box-body {
  padding: 8px 10px !important;
}

/* Reduce spacing between stacked chart boxes */
.content .col-md-4 .box {
  margin-bottom: 6px !important;
}

/* Table container padding */
.dataTables_wrapper {
  padding-top: 4px !important;
  padding-bottom: 4px !important;
}

/* ===============================
   TIGHTEN VERTICAL SPACING
   =============================== */

.content .row {
  margin-bottom: 4px !important;
}

.content .box {
  margin-bottom: 6px !important;
}

.box-header {
  padding: 6px 10px !important;
}

.box-body {
  padding: 6px 10px !important;
}

"))
)

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
            DTOutput("requests_table") %>% withSpinner()
          ),
          
          # RIGHT: stacked charts
          column(
            width = 4,
            
            box(
              title = "Requests by Owner",
              width = 12,
              status = "info",
              plotlyOutput("owner_plot") %>% withSpinner()
            ),
            
            box(
              title = "Status Distribution",
              width = 12,
              status = "info",
              plotlyOutput("statusid_plot") %>% withSpinner()
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
    datatable(
      table_df() %>% select(-path),
      filter = "top",
      rownames = FALSE,
      selection = "single",
      options = list(
        pageLength = 10,
        scrollX = TRUE,          # force horizontal scroll
        scrollCollapse = TRUE,
        autoWidth = TRUE,        # let columns size naturally
        dom = "tip"
      )
      
    )
  })
  
  
  selected_request <- reactive({
    s <- input$requests_table_rows_selected
    if (!length(s)) return(NULL)
    table_df()[s,]
  })
  
  output$kpi_total <- renderValueBox({
    valueBox(nrow(filtered()), "Open Requests", icon = icon("inbox"), color = "purple")
  })
  
  output$kpi_overdue <- renderValueBox({
    valueBox(
      nrow(filtered() %>% filter(age_days >= 90)),
      "90d+ Overdue",
      icon = icon("hourglass-end"),
      color = "red"
    )
  })
  
  output$kpi_avg_age <- renderValueBox({
    df <- filtered()
    avg <- ifelse(nrow(df)==0,0, round(mean(df$age_days, na.rm=TRUE),1))
    valueBox(avg, "Average Age (days)", icon = icon("calendar-day"), color = "purple")
  })
  
  output$kpi_last_refreshed <- renderValueBox({
    valueBox(
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "Data Load Time",
      icon = icon("sync"),
      color = "green"
    )
  })
  
  meta_df2 <- reactive({
    header_meta_df() %>%
      mutate(
        `Owner (initials)` = str_replace(
          `Owner (initials)`,
          "^\\(initials\\):\\s*",
          ""
        )
      )
  })
  
  output$owner_plot <- renderPlotly({
    
    d <- meta_df2() %>% count(`Owner (initials)`)
    
    plot_ly(
      data = d,
      x = ~`Owner (initials)`,
      y = ~n,
      type = "bar",
      color = ~`Owner (initials)`
    ) %>%
      layout(
        yaxis = list(title = ""),
        xaxis = list(title = ""),
        showlegend = FALSE
      )
  })
  
  output$organisation_plot <- renderPlotly({
    
    d <- meta_df2() %>%
      filter(!is.na(Organisation), Organisation != "", Organisation != "0") %>%
      count(Organisation)
    
    plot_ly(
      data = d,
      x = ~Organisation,
      y = ~n,
      type = "bar",
      color = ~Organisation
    ) %>%
      layout(
        yaxis = list(title = ""),
        xaxis = list(title = ""),
        showlegend = FALSE
      )
  })
  
  output$statusid_plot <- renderPlotly({
    
    d <- meta_df2() %>%
      mutate(
        StatusID = recode(
          as.character(StatusID),
          "1" = "waiting on client",
          "2" = "work in progress",
          "3" = "internal meeting planned",
          "4" = "client review/completed"
        )
      ) %>%
      count(StatusID)
    
    plot_ly(
      data = d,
      x = ~StatusID,
      y = ~n,
      type = "bar",
      color = ~StatusID
    ) %>%
      layout(
        yaxis = list(title = ""),
        xaxis = list(title = ""),
        showlegend = FALSE
      )
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