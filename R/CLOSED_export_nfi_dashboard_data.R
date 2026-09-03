cat(Sys.time(), " - scheduler started\n",
    file = "scheduler_run.log",
    append = TRUE)

# ----------------------------------------------------------
# NFI Dashboard Export Script (fully compatible with app.R)
# ----------------------------------------------------------

root_path <- "U:/Forest Inventory/0600_Advice_Enquiries_Support/0603_Closed_Requests"
 setwd("C:/Users/david.entwistle/OneDrive - Forest Research/Documents/Project/NFIAnalysisEnquires/R")

library(dplyr)
library(purrr)
library(readr)
library(tibble)
library(stringr)

source("file_scanner.R")
source("helpers.R")
source("readme_parser.R")

log_file <- "task_scheduler_log.txt"
log_con <- file(log_file, open = "wt")

sink(log_con)
sink(log_con, type = "message")

cat(Sys.time(), "- Script started\n")

on.exit({
  cat(Sys.time(), "- Script finished\n")
  sink(type = "message")
  sink()
  close(log_con)
})


# -------------------------------------------------------------------
# 1. Build core dataframe
# -------------------------------------------------------------------
df <- build_request_data(root_path, closed_request = TRUE)

if (!"path" %in% names(df)) {
  stop("build_request_data() did not return a 'path' column")
}

folders <- df$path

# -------------------------------------------------------------------
# 2. Build timeline cache (MUST BE A LIST WITH $timeline)
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# 2. Build timeline cache (must match app.R structure exactly)
# -------------------------------------------------------------------
timeline_cache <- map(folders, function(p) {
  tl_obj <- parse_readme_safe(p)
  
  # If no timeline, return an empty tibble
  if (is.null(tl_obj) || is.null(tl_obj$timeline)) {
    return(list(
      timeline = tibble(
        date = as.Date(character()),
        text = character()
      )
    ))
  }
  
  # ✅ Correct single-level structure
  list(timeline = tl_obj$timeline)
})

names(timeline_cache) <- folders



days_open <- map_dbl(folders, function(p) {
  
  tl_obj <- parse_readme_safe(p)
  
  if (
    is.null(tl_obj) ||
    is.null(tl_obj$timeline) ||
    nrow(tl_obj$timeline) < 2
  ) {
    return(NA_real_)
  }
  
  tl <- tl_obj$timeline
  
  as.numeric(
    max(tl$date, na.rm = TRUE) -
      min(tl$date, na.rm = TRUE)
  )
  
})

df$days_open <- days_open


# -------------------------------------------------------------------
# 3. Header parser
# -------------------------------------------------------------------
parse_header <- function(path) {
  file <- file.path(path, "ReadMe.txt")
  if (!file.exists(file)) return(NULL)
  
  lines <- readLines(file, warn = FALSE)
  
  tibble(
    `Owner (initials)` = str_match(lines, "^Owner[: ]+(.*)$")[,2] %>% na.omit() %>% first(),
    `Chargeable (Y/N)` = str_match(lines, "^Chargeable[: ]+(.*)$")[,2] %>% na.omit() %>% first(),
    Organisation       = str_match(lines, "^Organisation[: ]+(.*)$")[,2] %>% na.omit() %>% first(),
    `Job Code`         = str_match(lines, "^Job Code[: ]+(.*)$")[,2] %>% na.omit() %>% first(),
    StatusID           = str_match(lines, "^StatusID[: ]+(.*)$")[,2] %>% na.omit() %>% first(),
    `Time Spent (number counting days)` =
      str_match(lines, "^Time Spent[: ]+(.*)$")[,2] %>% na.omit() %>% first(),
    path = path
  )
}

header_meta <- map_dfr(folders, function(p) {
  h <- parse_header(p)
  
  if (is.null(h) || ncol(h) == 0) {
    h <- tibble(
      `Owner (initials)` = NA,
      `Chargeable (Y/N)` = NA,
      Organisation = NA,
      `Job Code` = NA,
      StatusID = NA,
      `Time Spent (number counting days)` = NA,
      path = p
    )
  }
  
  h
})

# -------------------------------------------------------------------
# 4. Save RDS
# -------------------------------------------------------------------
# -------------------------------------------------------------------
# 4. Save RDS (to TWO locations)
# -------------------------------------------------------------------

export_object <- list(
  df = df,
  timeline_cache = timeline_cache,
  header_meta = header_meta,
  exported_at = Sys.time()
)

# Location 1 (existing network location)
path1 <- "O:/0600_Advice_Enquiries_Support/0602_Open_Requests/enquires_dashboard_files/CLOSED_nfi_dashboard_export.rds"

# Location 2 (your OneDrive location)
path2 <- "C:/Users/david.entwistle/OneDrive - Forest Research/Documents/Project/NFIAnalysisEnquires/R/CLOSED_nfi_dashboard_export.rds"

# Save both
saveRDS(export_object, path1)
saveRDS(export_object, path2)

