# ----------------------------------------------------------
# NFI Dashboard Export Script
# Generates nfi_dashboard_export.rds for Shiny upload
# ----------------------------------------------------------

library(dplyr)
library(purrr)
library(readr)
library(tibble)

# Load your existing logic
source("file_scanner.R")
source("helpers.R")
source("app.R")  # ONLY if parse_readme_safe is defined there

# ---- 1. Set root path (U:/ drive version) ----
root_path <- "U:/Forest Inventory/0600_Advice_Enquiries_Support/0602_Open_Requests"

# ---- 2. Build request dataframe (existing logic) ----
df <- build_request_data(root_path)

# ---- 3. Build timeline cache (existing logic) ----
folders <- df$path
timeline_cache <- map(folders, parse_readme_safe)
names(timeline_cache) <- folders

# ---- 4. Build header metadata (existing logic) ----
header_meta <- map_dfr(folders, function(p) {
  obj <- parse_readme_safe(p)
  if (is.null(obj$header)) return(NULL)
  h <- obj$header
  h$path <- p
  h
})

# ---- 5. Save export file ----
export_path <- file.path(root_path, "enquires_dashboard_files", "nfi_dashboard_export.rds")

saveRDS(
  list(
    df = df,
    timeline_cache = timeline_cache,
    header_meta = header_meta
  ),
  export_path
)

cat("Export complete! File saved to:\n", export_path, "\n")