# Script to download and parse runoff data

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

# Download runoff data from CDEC, saving as a text file
# Historical
wsihist <- read_html("https://cdec.water.ca.gov/reportapp/javareports?name=WSIHIST")
wsihist <- html_element(wsihist, 'pre') %>% 
  html_text2()

wsihist_file <- file.path(run_dir, 'wsihist.txt')
write(wsihist, wsihist_file)

# Forecasted
wsi <- read_html("https://cdec.water.ca.gov/reportapp/javareports?name=WSI")
wsi <- html_element(wsi, "pre") %>% 
  html_text2() 

wsi_file <- file.path(run_dir, paste0("wsi_", format(Sys.Date(), format = "%Y"), ".txt"))
write(wsi, wsi_file)

# Parse historical runoff data -----------------------

# Read each line as a new element in a vector
wsihist_data <- readLines(wsihist_file)

# Pull out table of values
# Table begins with 1901, but Sacramento data only goes back to 1906
hist_start <- grep("^1906", wsihist_data)[1]
hist_end <- grep("^min", wsihist_data)[1]
hist_split <- strsplit(wsihist_data[hist_start:(hist_end - 1)], " +")
hist_split <- hist_split[sapply(hist_split, length) > 0]

# Parse
hist_years <- sapply(hist_split, `[[`, 1)
runoff_history <- tibble("Valley" = rep(c("Sacramento", "San Joaquin"), each = length(hist_years)))
runoff_history$WaterYear <- rep(hist_years, times = 2)

runoff_history$Runoff <- c(sapply(hist_split, `[[`, 4), sapply(hist_split, `[[`, 9))
runoff_history$Index <- c(sapply(hist_split, `[[`, 5), sapply(hist_split, `[[`, 10))
runoff_history$Type <- c(sapply(hist_split, `[[`, 6), sapply(hist_split, `[[`, 11))

# Export
write_csv(runoff_history, file.path(run_dir, "runoff_historical.csv"))

# Parse forecasted runoff data ---------------------

# Combine all forecast data into a single table
# Differs from historical data in that these are forecasted values from May of a given year
runoff_forecast <- NULL
forecast_files <- list.files(run_dir, pattern = "^wsi_[0-9]{4}.txt$", full.names = TRUE)
for (ff in forecast_files) {
  
  message_ts("Parsing file ", basename(ff))
  forecast_data <- readLines(ff)
  
  # Identify table locations
  # Sac Valley uses the 40-30-30 index; San Joaquin uses the 60-20-20
  forecast_lines <- c(grep("^SACRAMENTO VALLEY WATER YEAR TYPE INDEX  40-30-30", forecast_data),
                grep("^SAN JOAQUIN VALLEY WATER YEAR TYPE INDEX 60-20-20", forecast_data))
  
  # Parse
  for (fl in forecast_lines) {
    
    # Set valley
    valley <- ifelse(fl == forecast_lines[1], "Sacramento", "San Joaquin")
    
    # Split
    forecast_split <- strsplit(forecast_data[(fl + 4):(fl + 9)], " +")
    forecast_split <- forecast_split[sapply(forecast_split, length) > 0]
    
    forecast_parsed <- tibble("Valley" = rep(valley), 
                         "Year" = sapply(forecast_split, `[[`, 3),
                         "Month" = sapply(forecast_split, `[[`, 1),
                         "Index" = as.numeric(sapply(forecast_split, `[[`, 7)))
    
    # Append
    if (is.null(runoff_forecast)) {
      runoff_forecast <- forecast_parsed
    } else {
      runoff_forecast <- add_row(runoff_forecast, forecast_parsed)
    }
    
  }

}

# Load water year type definitions
wyt_defs <- read_csv(file.path(run_dir, "water_year_type_definitions.csv"))

# Match drought type code
runoff_forecast$Type <- mapply(val = runoff_forecast$Valley, 
                               idx = runoff_forecast$Index, 
                               FUN = function(val, idx) {
                                 wyt_defs$Type[wyt_defs$Valley == as.character(val) & 
                                                 wyt_defs$IndexMin <= idx & wyt_defs$IndexMax > idx]
                               })

# Export
write_csv(runoff_forecast, file.path(run_dir, "runoff_forecasted.csv"))



