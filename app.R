
# app.R
# NFI Enquiries Dashboard (Shiny)

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
library(tidyr)

library(future)
library(future.apply)
plan(multisession)

setwd('C:/Users/david.entwistle/OneDrive - Forest Research/Documents/Project/NFI_Enquiries_Dashboard')

`%||%` <- function(x,y) if (is.null(x)) y else x

cfg <- yaml::read_yaml('config/paths.yaml')
root_path <- cfg$paths$enquiries_root

source('R/file_scanner.R')
source('R/helpers.R')

safe_read_lines <- function(file){
  if(requireNamespace('readr', quietly=TRUE)){
    utf8_try <- try(readr::read_lines(file, locale=readr::locale(encoding='UTF-8')), silent=TRUE)
    if(!inherits(utf8_try,'try-error')) return(utf8_try)
    latin_try<-try(readr::read_lines(file, locale=readr::locale(encoding='Latin1')),silent=TRUE)
    if(!inherits(latin_try,'try-error')) return(latin_try)
  }
  readLines(file, warn=FALSE)
}

parse_readme_safe <- function(request_path){
  file <- file.path(request_path,'ReadMe.txt')
  if(!file.exists(file)) return(NULL)
  lines <- safe_read_lines(file)
  dash_idx <- grep('^[-]{5,}$', lines)
  header <- NULL
  body_lines <- lines
  if(length(dash_idx)>=2){
    start <- dash_idx[1]
    end <- dash_idx[2]
    header_lines <- lines[(start+1):(end-1)]
    body_lines <- lines[(end+1):length(lines)]
    kv_pattern <- '^([^:]+):\\s*(.*)$'
    m <- stringr::str_match(header_lines, kv_pattern)
    m <- m[!is.na(m[,1]),,drop=FALSE]
    if(nrow(m)>0){
      header <- tibble(field=trimws(m[,2]), value=trimws(m[,3])) %>%
        tidyr::pivot_wider(names_from=field, values_from=value)
    }
  }
  pattern <- '^([0-9]{2}[/-]?[0-9]{2}[/-]?[0-9]{2,4})\\s*[:\\- ]\\s*(.*)$'
  m <- str_match(body_lines, pattern)
  m <- m[!is.na(m[,1]),,drop=FALSE]
  if(nrow(m)==0){ timeline <- tibble(date=as.Date(character()), text=character()) }
  else{
    dates <- suppressWarnings(dmy(m[,2]))
    dates[is.na(dates)] <- suppressWarnings(dmy(str_replace_all(m[,2],'-','')))
    timeline <- tibble(date=dates, text=m[,3]) %>% filter(!is.na(date))
  }
  list(header=header, timeline=timeline)
}


# =====================================================================================
# CACHE BUILDER
# =====================================================================================

load_all_data <- function() {
  tryCatch({
    df <- build_request_data(root_path)
    if (nrow(df) == 0) return(list(df = df, timelines = list()))
    
    npaths <- normalizePath(df$path, winslash = '/', mustWork = FALSE)
    
    timelines <- future_lapply(
      seq_along(npaths),
      function(i) parse_readme_safe(npaths[i])
    )
    names(timelines) <- npaths
    
    list(df = df, timelines = timelines)
    
  }, error = function(e) {
    message('Error in loading data: ', e)
    list(df = tibble(), timelines = list())
  })
}

# =====================================================================================
# UI
# =====================================================================================

header <- dashboardHeader(title = 'NFI Enquiries Dashboard')

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    id = 'tabs',
    menuItem('Overview', tabName = 'overview', icon = icon('table')),
    menuItem('Detail',   tabName = 'detail',   icon = icon('file-alt'))
  ),
  hr(),
  tags$h4('Filters'),
  textInput('search_text', 'Search title / notes', placeholder = 'Type to search...'),
  sliderInput('age_filter', 'Age (days)', min = 0, max = 180, value = c(0, 180)),
  hr(),
  helpText('Auto-refresh: disabled')
)

body <- dashboardBody(
  tags$head(tags$link(rel='stylesheet', type='text/css', href='style.css')),
  tabItems(
    tabItem(
      tabName = 'overview',
      fluidRow(
        valueBoxOutput('kpi_total', width = 3),
        valueBoxOutput('kpi_overdue', width = 3),
        valueBoxOutput('kpi_avg_age', width = 3),
        valueBoxOutput('kpi_last_refreshed', width = 3)
      ),
      fluidRow(
        box(
          title = 'Open Requests',
          width = 12,
          status = 'primary',
          solidHeader = TRUE,
          DTOutput('requests_table')
        )
      ),
      
      ### METADATA PLOTS ###
      fluidRow(
        box(
          title = 'Chargeable (Y/N)',
          width = 4, status = 'info', solidHeader = TRUE,
          plotlyOutput('chargeable_plot')
        ),
        box(
          title = 'Status Distribution',
          width = 4, status = 'info', solidHeader = TRUE,
          plotlyOutput('statusid_plot')
        ),
        box(
          title = 'Requests by Owner',
          width = 4, status = 'info', solidHeader = TRUE,
          plotlyOutput('owner_plot')
        ),
        box(
          title = "Organisation Distribution",
          width = 12, status = "info", solidHeader = TRUE,
          plotlyOutput("organisation_plot")
        )
      )
    ),
    
    tabItem(
      tabName = 'detail',
      fluidRow(
        box(
          title = 'Selected Request',
          width = 4, status = 'primary', solidHeader = TRUE,
          uiOutput('detail_meta')
          ),
        box(
          title = 'ReadMe Timeline',
          width = 8, status = 'primary', solidHeader = TRUE,
          reactableOutput('timeline_reactable')
        )
      ),
    )
  )
)

ui <- dashboardPage(header, sidebar, body, skin = 'blue')



# =====================================================================================
# SERVER
# =====================================================================================

server <- function(input, output, session) {

  status_lookup <- function(x) {
    dplyr::recode(
      as.character(x),
      "1" = "waiting on client",
      "2" = "work in progress",
      "3" = "internal meeting planned",
      "4" = "client review/completed",
      .default = "(unknown)"
    )
  }
  
  # Load everything once
  requests_rv <- reactiveVal(load_all_data())
  requests <- reactive({ requests_rv()$df })
  timeline_cache <- reactive({ requests_rv()$timelines })
  
  # ---- Age buckets ----
  add_buckets <- function(df) {
    df %>% mutate(
      age_bucket = case_when(
        age_days < 30 ~ '< 30d',
        age_days < 60 ~ '30–59d',
        age_days < 90 ~ '60–89d',
        TRUE ~ '90d+'
      )
    )
  }
  
  # ---- Filters ----
  observe({
    df <- requests()
    if (nrow(df) == 0) return()
    df <- add_buckets(df)
    
    output$status_filter_ui <- renderUI({
      selectInput('status_filter','Status',
                  choices=sort(unique(df$status)),
                  selected=sort(unique(df$status)), multiple=TRUE)
    })
    
    output$resp_filter_ui <- renderUI({
      selectInput('resp_filter','Responsible',
                  choices=sort(unique(df$responsible)),
                  selected=sort(unique(df$responsible)), multiple=TRUE)
    })
  })
  
  # ---- Filtered table ----
  filtered <- reactive({
    df <- requests()
    if (nrow(df)==0) return(df)
    
    df <- add_buckets(df)
    df <- df %>% filter(is.na(age_days) | between(age_days, input$age_filter[1], input$age_filter[2]))
    if (!is.null(input$status_filter)) df <- df %>% filter(status %in% input$status_filter)
    if (!is.null(input$resp_filter))   df <- df %>% filter(is.na(responsible) | responsible %in% input$resp_filter)
    
    if (isTruthy(input$search_text)){
      s <- tolower(input$search_text)
      df <- df %>% filter(grepl(s, tolower(title)) | grepl(s, tolower(last_note)))
    }
    df
  })

  header_meta_df <- reactive({
    tc <- timeline_cache()
    reqs <- requests()
    
    if (length(tc) == 0 || nrow(reqs) == 0) return(tibble())
    
    purrr::map_dfr(reqs$path, function(p) {
      p_norm <- normalizePath(p, winslash = "/", mustWork = FALSE)
      obj <- tc[[p_norm]]
      if (is.null(obj$header)) return(NULL)
      
      h <- obj$header
      h$path <- p_norm
      h
    })
  })

  table_df <- reactive({
    df <- filtered() %>%
      arrange(desc(age_days)) %>%
      mutate(last_update = as.character(last_update))
    
    meta <- header_meta_df()
    
    # LEFT JOIN on path
    df2 <- df %>%
      left_join(meta, by = "path") %>%
      mutate(
        StatusID = status_lookup(StatusID)
      ) %>%
      select(
        code,
        user,
        title,
        Owner = `Owner (initials)`,
        Status = StatusID,       
        Chargeable = `Chargeable (Y/N)`,
        Organisation,
        JobCode = `Job Code`,
        age_days,
        last_update,
        last_note,
        path
      )
    
    
    df2
  })
  
  selected_request <- reactive({
    s <- input$requests_table_rows_selected
    df <- table_df()
    if (length(s) == 0) return(NULL)
    df[s,]
  })
  
  # -------------------------
  # DETAIL META PANEL
  # -------------------------
  output$detail_meta <- renderUI({
    req <- selected_request()
    validate(need(!is.null(req), "Select a request"))
    
    req <- as.list(req)
    
    # Fetch header metadata
    p <- normalizePath(req$path, winslash='/', mustWork=FALSE)
    obj <- timeline_cache()[[p]]
    header <- obj$header %||% list()
    
    # Safely extract metadata fields
    owner       <- header$`Owner (initials)` %||% "(None)"
    chargeable  <- header$`Chargeable (Y/N)` %||% "(None)"
    organisation <- header$Organisation %||% "(None)"
    timespent   <- header$`Time Spent (number counting days)` %||% "(None)"
    statusid <- status_lookup(header$StatusID)    
    wellPanel(
      strong("Code: "), req$code, br(),
      strong("User: "), req$user, br(),
      strong("Title: "), req$title, br(),
      
      tags$hr(),
      
      strong("Owner: "), owner, br(),
      strong("Chargeable: "), chargeable, br(),
      strong("Organisation: "), organisation, br(),
      strong("Time Spent (days): "), timespent, br(),
      strong("StatusID: "), statusid, br(),
      
      tags$hr(),
      
      strong("Age (days): "), req$age_days, br(),
      strong("Last update: "), req$last_update, br(),
      strong("Last note: "), req$last_note
    )
  })
  
  
  
  output$requests_table <- renderDT({
    df <- table_df()
    datatable(
      df %>% select(-path), filter='top', rownames=FALSE,
      selection='single', options=list(pageLength=15, scrollX=TRUE)
    )
  })
  
  # ---- TOTALS ----
  output$kpi_total <- renderValueBox({ valueBox(nrow(filtered()), 'Open Requests', icon=icon('inbox'), color='aqua') })
  output$kpi_overdue <- renderValueBox({ valueBox(nrow(filtered() %>% filter(age_days >= 90)), '90d+ Overdue', icon=icon('hourglass-end'), color='red') })
  output$kpi_avg_age <- renderValueBox({
    df <- filtered()
    avg <- ifelse(nrow(df)==0,0, round(mean(df$age_days, na.rm=TRUE),1))
    valueBox(avg, 'Average Age (days)', icon=icon('calendar-day'), color='yellow')
  })
  output$kpi_last_refreshed <- renderValueBox({
    valueBox(format(Sys.time(), '%Y-%m-%d %H:%M:%S'), 'Data Load Time', icon=icon('sync'), color='green')
  })
  
  # =====================================================================================
  # METADATA AGGREGATION for table
  # =====================================================================================
  
  meta_df <- reactive({
    tc <- timeline_cache()
    if (length(tc) == 0) return(tibble())
    
    reqs <- requests()
    purrr::map_dfr(reqs$path, function(p){
      p_norm <- normalizePath(p, winslash='/', mustWork=FALSE)
      obj <- tc[[p_norm]]
      if (is.null(obj$header)) return(NULL)
      h <- obj$header
      h$path <- p_norm
      h
    })
  })
  
  # ---- Chargeable Pie ----
  output$chargeable_plot <- renderPlotly({
    m <- meta_df()
    validate(need(nrow(m) > 0, "No metadata found"))
    
    d <- m %>%
      count(`Chargeable (Y/N)`) %>%
      # KEEP ONLY Y or N
      filter(
        `Chargeable (Y/N)` %in% c("Y", "N")
      )
    
    plot_ly(
      d,
      labels = ~`Chargeable (Y/N)`,
      values = ~n,
      type = "pie",
      textinfo = "label+percent",
      insidetextorientation = "radial",
      marker = list(
        colors = c("#4CAF50", "#F44336")  # Y = green, N = red
      )
    )
  })
  
  output$organisation_plot <- renderPlotly({
    m <- meta_df()
    validate(need(nrow(m) > 0, "No metadata found"))
    
    d <- m %>%
      filter(!is.na(Organisation), Organisation != "", Organisation != "0") %>%
      count(Organisation)
    
    # Generate colours for each organisation
    orgs <- d$Organisation
    n_org <- length(orgs)
    colours <- RColorBrewer::brewer.pal(max(3, n_org), "Set3")
    
    plot_ly(
      d,
      x = ~Organisation,
      y = ~n,
      type = "bar",
      marker = list(color = colours)
    ) %>% layout(
      xaxis = list(title = "Organisation"),
      yaxis = list(title = "Requests"),
      margin = list(b = 120)  # More room for long organisation names
    )
  })
  
  # ---- StatusID Bar ----
  output$statusid_plot <- renderPlotly({
    m <- meta_df(); validate(need(nrow(m) > 0, "No metadata found"))
    
    d <- m %>%
      mutate(StatusID = status_lookup(StatusID)) %>%
      filter(StatusID != "(unknown)") %>%
      count(StatusID)
    
    colours <- c(
      "waiting on client"       = "#FFC107", # amber
      "work in progress"        = "#2196F3", # blue
      "internal meeting planned"= "#9C27B0", # purple
      "client review/completed" = "#4CAF50"  # green
    )
    
    plot_ly(
      d,
      x = ~StatusID,
      y = ~n,
      type = "bar",
      marker = list(color = colours[d$StatusID])
    ) %>% layout(
      xaxis = list(title = "Status"),
      yaxis = list(title = "Count")
    )
  })
  
  # ---- Owner Bar ----
  output$owner_plot <- renderPlotly({
    m <- meta_df(); validate(need(nrow(m) > 0, "No metadata found"))
    
    d <- m %>% count(`Owner (initials)`)
    
    # Auto-generate distinct colours for each owner
    owner_names <- d$`Owner (initials)`
    colours <- RColorBrewer::brewer.pal(max(3, length(owner_names)), "Set2")
    
    plot_ly(
      d,
      x = ~`Owner (initials)`,
      y = ~n,
      type = "bar",
      marker = list(color = colours)
    ) %>% layout(
      xaxis = list(title = "Owner"),
      yaxis = list(title = "Requests")
    )
  })
  
  
  
  # =====================================================================================
  # HEADER TABLE
  # =====================================================================================
  
  output$header_table <- renderReactable({
    req <- selected_request()
    validate(need(!is.null(req), 'Select a request'))
    
    p <- normalizePath(req$path, winslash='/', mustWork=FALSE)
    obj <- timeline_cache()[[p]]
    header <- obj$header
    
    if (is.null(header)) return(reactable(data.frame(Message='No header found')))
    
    reactable(header, striped=TRUE, highlight=TRUE)
  })
  
  # =====================================================================================
  # TIMELINE TABLE
  # =====================================================================================
  
  output$timeline_reactable <- renderReactable({
    req <- selected_request()
    validate(need(!is.null(req), 'Select a request'))
    
    p <- normalizePath(req$path, winslash='/', mustWork=FALSE)
    obj <- timeline_cache()[[p]]
    tl <- obj$timeline
    
    if (is.null(tl) || nrow(tl)==0)
      return(reactable(data.frame(Message='No timeline entries found')))
    
    reactable(
      tl,
      columns=list(
        date = colDef(name='Date', width=120),
        text = colDef(show=FALSE)
      ),
      striped=TRUE,
      highlight=TRUE,
      pagination=TRUE,
      defaultPageSize=10,
      details=function(i) {
        htmltools::div(
          style='background-color:#f8f9fa; padding:12px; border-left:3px solid #007bff; white-space:pre-wrap; font-family:Consolas, Courier New, monospace;',
          tl$text[i]
        )
      }
    )
  })
  
  # =====================================================================================
  # CSV DOWNLOAD
  # =====================================================================================
  
  
  
} # ---- END SERVER ----

# =====================================================================================
# RUN APP
# =====================================================================================

shinyApp(ui, server)
