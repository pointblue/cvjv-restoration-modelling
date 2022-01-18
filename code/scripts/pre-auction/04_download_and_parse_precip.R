# Script to download and parse precipitation data

# Load necessary functions and directories
# Must either run 00_definitions.R before this file or source it, replacing getwd() with your base working directory below
code_file <- file.path(getwd(), "code/00_definitions.R")
if (!file.exists(code_file)) {
  stop("Code file with required definitions not found in specified location. Please update path code_file.")
} else {
  source(code_file)  
}

# Load required packages
library(rvest)
library(dplyr)
library(readr)

# Function to download precip data from CDEC
download_cdec_precip <- function(station_index) {
  
  html <- read_html(paste0("http://cdec4gov.water.ca.gov/dynamicapp/QueryMonthly?s=", station_index))
  
  precip_table <- html %>%
    html_element("table#monthly_values") %>% 
    html_table(html, header = TRUE) %>%
    rename("Date" = 1, "Rain_Inches" = 2, "Note" = 3) %>%
    mutate("Index" = station_index, .before = "Date")
  
  return(precip_table)
  
}

# Observed precipitation index for Northern Sierras (8-station index; 8SI)
precip_sierras <- download_cdec_precip("8SI")
write_csv(precip_sierras, file.path(pcp_dir, paste0("precip_sierras_", format(Sys.Date(), format = "%Y"), ".csv")))

# Observed precipitation index for San Joaquin (5-station index; 5SI)
precip_sanjoaquin <- download_cdec_precip("6SI")
write_csv(precip_sanjoaquin, file.path(pcp_dir, paste0("precip_sanjoaquin_", format(Sys.Date(), format = "%Y"), ".csv")))

# Observed precipitation index for Tulare (6-station index; 6SI)
precip_tulare <- download_cdec_precip("6SI")
write_csv(precip_tulare, file.path(pcp_dir, paste0("precip_tulare_", format(Sys.Date(), format = "%Y"), ".csv")))

# Combine
precip <- bind_rows(precip_sierras, precip_sanjoaquin, precip_tulare)
write_csv(precip, file.path(pcp_dir, paste0("precip_", format(Sys.Date(), format = "%Y"), ".csv")))
