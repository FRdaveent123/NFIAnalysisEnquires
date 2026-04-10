# R/readme_parser.R
# ------------------
# Robust parsing of timeline entries inside ReadMe.txt files.

library(stringr)
library(lubridate)
library(dplyr)

# ---------------------------------------------------------------
# Parse the ReadMe.txt into a list(timeline = tibble(date, text))
# ---------------------------------------------------------------
parse_readme_safe <- function(request_path) {
  
  file <- file.path(request_path, "ReadMe.txt")
  if (!file.exists(file)) return(NULL)
  
  lines <- readLines(file, warn = FALSE)
  
  # Match any date-like structure at line start
  # Supports: 20/02/2026, 20-02-26, 20022026, 20/02/2026: text, etc.
  pattern <- "^\\s*([0-9]{2}[/-]?[0-9]{2}[/-]?[0-9]{2,4})[ :–-]*\\s*(.*)$"
  
  m <- str_match(lines, pattern)
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  
  if (nrow(m) == 0) return(NULL)
  
  # Robust date parsing
  dates <- suppressWarnings(dmy(m[, 2]))
  dates[is.na(dates)] <- suppressWarnings(dmy(str_replace_all(m[, 2], "[^0-9]", "")))
  dates[is.na(dates)] <- suppressWarnings(ymd(m[, 2]))
  
  tl <- tibble(
    date = dates,
    text = m[, 3]
  ) %>% filter(!is.na(date))
  
  # ✅ Return in the structure used throughout your project
  list(timeline = tl)
}