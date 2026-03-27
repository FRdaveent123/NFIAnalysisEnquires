# R/file_scanner.R
# -----------------
# Functions for scanning the requests directory structure.

library(stringr)
library(dplyr)

# Parse folder name format: "CODE - User Name - Title"
parse_folder_name <- function(folder_name) {
  # Split by ' - '
  parts <- str_split(folder_name, " - ", n = 3)[[1]]
  
  if (length(parts) < 3) {
    # Fallback in case format is irregular
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

# Scan root path for first‑level directories representing enquiries
scan_request_folders <- function(root_path) {
  dirs <- list.dirs(root_path, full.names = TRUE, recursive = FALSE)
  
  bind_rows(lapply(dirs, function(d) {
    folder <- basename(d)
    info <- parse_folder_name(folder)
    info$path <- d
    info
  }))
}