# R/helpers.R
# ------------

library(dplyr)
library(stringr)
library(lubridate)

# ------------------------------------------------------------------------------
# Extract likely responsible initials from update text (fallback)
# ------------------------------------------------------------------------------
extract_person <- function(text) {
  # Looks for 1–3 uppercase letters representing initials (DB, RG, SD, etc.)
  initials <- str_extract(text, "\\b[A-Z]{1,3}\\b")
  initials
}

# ------------------------------------------------------------------------------
# Determine status using simple keyword classification
# (This is used as a fallback; StatusID is now preferred)
# ------------------------------------------------------------------------------
extract_status <- function(text) {
  t <- tolower(text)
  
  case_when(
    str_detect(t, "on hold")         ~ "On Hold",
    str_detect(t, "awaiting")        ~ "Awaiting Info",
    str_detect(t, "meeting")         ~ "Meeting Arranged",
    str_detect(t, "complete|done")   ~ "Completed",
    TRUE                             ~ "Active"
  )
}

# ------------------------------------------------------------------------------
# Extract initials from a single line containing "assigned to ___"
# Robust to case, optional ":" or "-", and extra whitespace.
# ------------------------------------------------------------------------------
extract_assigned_to_from_line <- function(text) {
  if (is.null(text) || length(text) == 0) return(NA_character_)
  
  m <- str_match(
    text,
    regex("\\bassigned\\s*to\\s*[:\\-]?\\s*([A-Za-z]{1,3})\\b", ignore_case = TRUE)
  )
  
  if (is.na(m[1, 2])) return(NA_character_)
  toupper(m[1, 2])
}

# ------------------------------------------------------------------------------
# Scan the entire ReadMe timeline and return the FIRST "Assigned to ___"
# (Earliest chronologically)
# ------------------------------------------------------------------------------
extract_assigned_from_readme <- function(request_path) {
  tl <- parse_readme_safe(request_path)$timeline
  if (is.null(tl) || nrow(tl) == 0) return(NA_character_)
  
  tl <- tl %>% arrange(date)
  
  assigned <- vapply(tl$text, extract_assigned_to_from_line, character(1))
  first_non_na <- assigned[!is.na(assigned) & assigned != ""]
  
  if (length(first_non_na) == 0) return(NA_character_)
  first_non_na[1]
}

# ------------------------------------------------------------------------------
# Turn one scanned request into a processed row
# This version now uses the *first timeline entry* to compute age.
# ------------------------------------------------------------------------------
process_request <- function(request_row) {
  
  latest <- get_latest_update(request_row$path)
  if (is.null(latest)) return(NULL)
  
  # Responsible person (owner)
  assigned_resp <- extract_assigned_from_readme(request_row$path)
  fallback_resp <- extract_person(latest$text)
  responsible_val <- if (!is.na(assigned_resp) && assigned_resp != "") assigned_resp else fallback_resp
  
  # ---- NEW AGE LOGIC ---------------------------------------------------------
  # Compute age from *earliest* timeline entry
  tl_safe <- try(parse_readme_safe(request_row$path)$timeline, silent = TRUE)
  
  if (!inherits(tl_safe, "try-error") &&
      !is.null(tl_safe) && nrow(tl_safe) > 0) {
    
    first_date <- min(tl_safe$date, na.rm = TRUE)
    
  } else {
    # Fallback: if no timeline or unreadable, fall back to last update
    first_date <- latest$date
  }
  
  age_val <- as.numeric(Sys.Date() - first_date)
  # ---------------------------------------------------------------------------
  
  tibble(
    code         = request_row$code,
    user         = request_row$user,
    title        = request_row$title,
    path         = request_row$path,
    last_update  = latest$date,
    last_note    = latest$text,
    responsible  = responsible_val,
    status       = extract_status(latest$text),
    age_days     = age_val   # <-- FIXED
  )
}

# ------------------------------------------------------------------------------
# Build final dataset of *all* requests
# ------------------------------------------------------------------------------
build_request_data <- function(root_path) {
  
  folders <- scan_request_folders(root_path)
  
  results <- lapply(seq_len(nrow(folders)), function(i) {
    row <- folders[i, ]
    
    # Skip malformed folder names (rare)
    if (is.na(row$code) || is.na(row$title)) {
      return(NULL)
    }
    
    out <- process_request(row)
    
    # Fallback if ReadMe.txt missing
    if (is.null(out)) {
      return(tibble(
        code         = row$code,
        user         = row$user,
        title        = row$title,
        path         = row$path,
        last_update  = NA,
        last_note    = "No ReadMe.txt",
        responsible  = NA,
        status       = "No Data",
        age_days     = NA_real_
      ))
    }
    
    out
  })
  
  bind_rows(results)
}