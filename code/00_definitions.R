# Defines directories and shared functions for analyzing bids to put water on the landscape in
# California's central valley.
#  
# Point Blue, California Rice Commission

# Define a basic loggings function to add a timestamp to messages
# Wrapper for base 'message' function
message_ts <- function(...) message("[", Sys.time(), "] - ", ...)


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
  check_dir(c(code_dir, data_dir, lc_dir, run_dir, pcp_dir, wtr_dir, avg_dir, cov_dir), create = TRUE)
}

# Check that code_dir actually contains the necessary code


# Shared Functions --------------------




