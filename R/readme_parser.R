# R/readme_parser.R
# ------------------
# Functions to read and parse ReadMe.txt files inside each request folder.

library(stringr)
library(lubridate)
library(dplyr)

# Parse the ReadMe.txt into a tibble of (date, text)
parse_readme <- function(request_path) {
  file <- file.path(request_path, "ReadMe.txt")
  if (!file.exists(file)) return(NULL)
  
  lines <- readLines(file, warn = FALSE)
  
  # Pattern supports:
  #  DD/MM/YYYY Some text
  #  DDMMYY - Some text
  #  DD-MM-YYYY - Some text
  pattern <- "^([0-9]{2}[/-]?[0-9]{2}[/-]?[0-9]{2,4})\\s*[- ]\\s*(.*)$"
  
  m <- str_match(lines, pattern)
  m <- m[!is.na(m[,1]), , drop = FALSE]
  
  if (nrow(m) == 0) return(NULL)
  
  # Try robust date parsing
  dates <- suppressWarnings(dmy(m[,2]))
  # If NA, try removing dashes
  dates[is.na(dates)] <- suppressWarnings(dmy(str_replace_all(m[,2], "-", "")))
  
  tibble(
    date = dates,
    text = m[,3]
  ) %>% filter(!is.na(date))
}

# Get the latest update entry for the request
get_latest_update <- function(request_path) {
  df <- parse_readme(request_path)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  df %>% arrange(desc(date)) %>% slice(1)
}