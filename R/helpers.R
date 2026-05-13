# R/helpers.R
# -------------------------------------------------------

library(dplyr)
library(stringr)
library(lubridate)
library(tibble)

styledValueBox <- function(value, subtitle, icon, color) {
  valueBox(
    value = HTML(
      paste0(
        "<div style='font-size:26px; font-weight:600; line-height:1.1;'>",
        value,
        "</div>"
      )
    ),
    subtitle = HTML(
      paste0(
        "<div style='font-size:12px;'>",
        subtitle,
        "</div>"
      )
    ),
    icon = icon,
    color = color
  )
}

get_latest_update <- function(path) {
  parsed <- parse_readme_safe(path)
  if (is.null(parsed) || is.null(parsed$timeline)) return(NULL)
  
  tl <- parsed$timeline
  if (nrow(tl) == 0) return(NULL)
  
  tl %>% arrange(desc(date)) %>% slice(1)
}

extract_person <- function(text) {
  str_extract(text, "\\b[A-Z]{1,3}\\b")
}

extract_status <- function(text) {
  t <- tolower(text)
  
  case_when(
    str_detect(t, "on hold")       ~ "On Hold",
    str_detect(t, "awaiting")      ~ "Awaiting Info",
    str_detect(t, "meeting")       ~ "Meeting Arranged",
    str_detect(t, "complete|done") ~ "Completed",
    TRUE                           ~ "Active"
  )
}

extract_assigned_to_from_line <- function(text) {
  if (is.null(text) || length(text) == 0) return(NA_character_)
  
  m <- str_match(
    text,
    regex("\\bassigned\\s*to\\s*[:\\-]?\\s*([A-Za-z]{1,3})\\b", ignore_case = TRUE)
  )
  
  if (is.na(m[1, 2])) return(NA_character_)
  toupper(m[1, 2])
}

extract_assigned_from_readme <- function(request_path) {
  tl <- parse_readme_safe(request_path)$timeline
  if (is.null(tl) || nrow(tl) == 0) return(NA_character_)
  
  tl <- tl %>% arrange(date)
  assigned <- vapply(tl$text, extract_assigned_to_from_line, character(1))
  first_non_na <- assigned[!is.na(assigned) & assigned != ""]
  
  if (length(first_non_na) == 0) return(NA_character_)
  first_non_na[1]
}

process_request <- function(request_row) {
  
  latest <- get_latest_update(request_row$path)
  if (is.null(latest)) return(NULL)
  
  assigned_resp <- extract_assigned_from_readme(request_row$path)
  fallback_resp <- extract_person(latest$text)
  responsible_val <- if (!is.na(assigned_resp) && assigned_resp != "") assigned_resp else fallback_resp
  
  tl_safe <- try(parse_readme_safe(request_row$path)$timeline, silent = TRUE)
  
  if (!inherits(tl_safe, "try-error") &&
      !is.null(tl_safe) &&
      nrow(tl_safe) > 0) {
    first_date <- min(tl_safe$date, na.rm = TRUE)
  } else {
    first_date <- latest$date
  }
  
  age_val <- as.numeric(Sys.Date() - first_date)
  
  tibble(
    code        = request_row$code,
    user        = request_row$user,
    title       = request_row$title,
    path        = request_row$path,
    last_update = latest$date,
    last_note   = latest$text,
    responsible = responsible_val,
    status      = extract_status(latest$text),
    age_days    = age_val
  )
}

build_request_data <- function(root_path) {
  
  folders <- scan_request_folders(root_path)
  
  results <- lapply(seq_len(nrow(folders)), function(i) {
    row <- folders[i, ]
    
    if (is.na(row$code) || is.na(row$title)) return(NULL)
    
    out <- process_request(row)
    
    if (is.null(out)) {
      return(tibble(
        code        = row$code,
        user        = row$user,
        title       = row$title,
        path        = row$path,
        last_update = NA,
        last_note   = "No ReadMe.txt",
        responsible = NA,
        status      = "No Data",
        age_days    = NA_real_
      ))
    }
    
    out
  })
  
  bind_rows(results)
}

# R/readme_parser.R
# -------------------------------------------------------

library(stringr)
library(lubridate)
library(dplyr)
library(tibble)

parse_readme_safe <- function(request_path) {
  
  file <- file.path(request_path, "ReadMe.txt")
  if (!file.exists(file)) return(NULL)
  
  lines <- readLines(file, warn = FALSE)
  
  pattern <- "^\\s*([0-9]{2}[/-]?[0-9]{2}[/-]?[0-9]{2,4})[ :–-]*\\s*(.*)$"
  
  m <- str_match(lines, pattern)
  m <- m[!is.na(m[, 1]), , drop = FALSE]
  
  if (nrow(m) == 0) return(NULL)
  
  clean_dates <- m[, 2] %>%
    str_replace_all("[^0-9/\\-]", "") %>%
    
    # Fix 2-digit years → force into 20xx (adjust if needed)
    str_replace(
      "(\\d{2}[/-]\\d{2}[/-])(\\d{2})$",
      "\\120\\2"
    )
  
  dates <- suppressWarnings(dmy(clean_dates))
  dates[is.na(dates)] <- suppressWarnings(ymd(clean_dates))
  
  
  tl <- tibble(
    date = dates,
    text = m[, 3]
  )
  
  # If ALL dates failed → don't drop everything
  if (all(is.na(tl$date))) return(NULL)
  
  # Otherwise keep valid ones
  tl <- tl %>% filter(!is.na(date))
  
  list(timeline = tl)
}

# R/file_scanner.R
# -------------------------------------------------------

library(stringr)
library(dplyr)
library(tibble)

parse_folder_name <- function(folder_name) {
  parts <- str_split(folder_name, " - ", n = 3)[[1]]
  
  if (length(parts) < 3) {
    return(tibble(
      code  = NA,
      user  = NA,
      title = folder_name
    ))
  }
  
  tibble(
    code  = parts[1],
    user  = parts[2],
    title = parts[3]
  )
}

scan_request_folders <- function(root_path) {
  dirs <- list.dirs(root_path, full.names = TRUE, recursive = FALSE)
  
  bind_rows(lapply(dirs, function(d) {
    folder <- basename(d)
    info <- parse_folder_name(folder)
    info$path <- d
    info
  }))
}
