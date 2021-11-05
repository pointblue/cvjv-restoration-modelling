# Defines directories and shared functions for analyzing bids to put water on the landscape in
# California's central valley.
#  
# Point Blue, California Rice Commission

# Define a basic logging function to add a timestamp to messages
# Wrapper for base 'message' function
add_ts <- function(...) paste0("[", Sys.time(), "] - ", ...)
message_ts <- function(...) message(add_ts(...))

# Set directories ---------------------------------------------

base_dir <- getwd() #replace as necessary with "YOUR/BASE/DIR"
code_dir <- file.path(base_dir, "code")
data_dir <- file.path(base_dir, "data")
lc_dir <- file.path(data_dir, "landcover")
run_dir <- file.path(data_dir, "runoff")
pcp_dir <- file.path(data_dir, "precip")
wtr_dir <- file.path(data_dir, "water")
avg_dir <- file.path(data_dir, "water_averages")
cov_dir <- file.path(data_dir, "other_covariates")

axn_dir <- file.path(data_dir, "auction")
fld_dir <- file.path(axn_dir, "fields")

wfc_dir <- file.path(axn_dir, "water_forecast")
wxl_dir <- file.path(axn_dir, "water_x_landcover")
fcl_dir <- file.path(axn_dir, "water_focal")

# Check that specified directories exist, optionally creating if missing
check_dir <- function(directory, create = FALSE, verbose = FALSE) {
  
  for (d in directory) {
    
    if (!file.exists(d)) {
      
      if (create == FALSE) {
        
        message_ts()
        stop(paste("Directory", d, "is required but does not exist."))
        
      } else {
        
        message("Creating directory ", d)
        dir.create(d)
        
      }
      
    } else if (verbose == TRUE) {
      
      message("Directory", d, "exists.")
      
    } 
    
  }
  
}
check_dir(base_dir)
if(file.exists(base_dir)) {
  check_dir(c(code_dir, data_dir, lc_dir, run_dir, pcp_dir, wtr_dir, avg_dir, cov_dir, axn_dir, 
              fld_dir, wfc_dir, wxl_dir, fcl_dir), create = TRUE)
}

# Check that code_dir actually contains the necessary code


# Shared Functions --------------------
# Function to get the nth subelement from each item in a list
extract_subelement <- function(x, element) sapply(x, `[[`, element) 

# Function to parse the filenames of project files
parse_filename <- function(files) {
  
  paths <- dirname(files)
  fns <- basename(files)
  
  # Extension
  ext_pos <- regexpr("\\.([[:alnum:]]+)$", fns)
  exts <- ifelse(ext_pos > -1L, substring(fns, ext_pos + 1L), "")
  fns_no_ext <- ifelse(ext_pos > -1L, substring(fns, 1, ext_pos - 1L), fns)
  
  # Order of data in filenames is location, descriptor, year, month, additional info
  fns_split <- strsplit(fns_no_ext, "_")
  locs <- extract_subelement(fns_split, 1)
  desc <- extract_subelement(fns_split, 2)
  yrs <- extract_subelement(fns_split, 3)
  mths <- extract_subelement(fns_split, 4)
  
  # Amount of additional info not set; could be none or could be 3-4
  n_splits <- length(fns_split[[1]])
  info <- paste(ifelse(n_splits > 4, extract_subelement(fns, 5), ""),
                ifelse(n_splits > 5, extract_subelement(fns, 6), ""),
                ifelse(n_splits > 6, extract_subelement(fns, 7), ""),
                sep = "_")
  
  # Combine into data frame and export
  file_df <- data.frame("File" = files,
                        "Path" = paths, 
                        "Filename" = fns,
                        "Extension" = exts,
                        "Location" = locs,
                        "Description" = desc,
                        "Year" = yrs,
                        "Month" = mths,
                        "AdditionalInfo" = info)
  
  return(file_df)
  
}



