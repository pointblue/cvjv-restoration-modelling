# Create bird predictions for specified cells
# Uses package 'future' to split between multiple cores

# Load packages
library(future)
library(terra)

# Set temp directory for terra on non-boot drive if possible
terraOptions(tempdir = "E:/rtemp")

# Load definitions and code
code_dir <- "V:/Project/wetland/FWSPartners/code/cvjv-restoration-modelling/code"
def_file <- file.path(code_dir, "definitions.R")
code_files <- list.files(file.path(code_dir, "functions"), pattern = ".*R$", full.names = TRUE)
sapply(c(def_file, code_files), FUN = function(x) source(x))

# Check directories, creating if missing
check_dir(c(log_dir, cell_dir, anl_dir, wxl_dir, fcl_dir, prd_dir, stat_dir, cell_stat_dir), create = TRUE)

# Specify the (monthly) long term average water files to process
water_files <- file.path(wtr_dir, "valley_average_Apr_2011-2021_snapped.tif")

# Select uids
uid_df <- read.csv(file.path(grid_dir, "unique_ids_suitable.csv"))
uids_ag <- uid_df$UID[uid_df$Landcover != "GrassPasture"]
uids_grass <- uid_df$UID[uid_df$Landcover == "GrassPasture"]

# Set uids to use
uids <- uids_ag[1:1000]

# Set number of processes
# Caps at number of cores - 1
cores_to_use <- 16 #for tsinfo3
n_sessions <- min(length(uids), cores_to_use, availableCores() - 1)
plan(multisession, workers = n_sessions)

# Split data into one chunk per session
# Randomize files to prevent a large bloc of unprocessed files from being assigned to the same chunk
if (length(uids) > n_sessions) {
	uids_subset <- chunk(sample(uids), n_sessions)
} else {
	uids_subset <- uids
}

# Execute the code in n splits
f <- list()
for (n in 1:n_sessions) {
	
	f[[n]] <- future({
		
	  # Write to log
	  pid <- Sys.getpid()
	  timestr <- format(Sys.time(), format = "%Y-%m-%d_%H%M")
	  #log_file <- file.path(log_dir, paste0("log_", timestr, "_", pid, ".txt"))
	  #sink(log_file, split = TRUE)
  	  
  	print_ts("Running on process ID: ", pid)
	
	  # Overlay water and landcover ----------------------------------
	  wxl_files <- overlay_water_landcover(water_files, 
	                                       model_lc_files, #from definitions,
	                                       uid_raster = file.path(grid_dir, "unique_ids_masked.tif"),
	                                       uids = uids_subset[[n]],
	                                       uid_lkp_file = file.path(grid_dir, "uid_full_lookup.rds"),
	                                       imposed_landcover = "WetlandNatural",
	                                       cell_dir = cell_dir, 
	                                       output_dir = wxl_dir, 
	                                       overwrite = FALSE)
	                                       #printmessage = "print") #implement for logging
	
  	
  	# Create mean neighborhood water rasters
  	# Function defined in functions/water_moving_window.R
  	fcl_files <- mean_neighborhood_water(wxl_files,                #previously-created water x landcover files
  	                                     distances = c(250, 5000), #250m and 5km
  	                                     output_dir = fcl_dir, 
  	                                     overwrite = FALSE)
  	
  	# Predict							 
  	prd_files <- predict_bird_rasters(water_files_longterm = fcl_files,                  
  	                                  scenarios = "2011-2021",
  	                                  water_months = c("Apr"),
  	                                  model_files = shorebird_model_files_long, 
  	                                  model_names = shorebird_model_names_long, 
  	                                  static_cov_files = bird_model_cov_files, 
  	                                  static_cov_names = bird_model_cov_names,
  	                                  monthly_cov_files = tmax_files,
  	                                  monthly_cov_months = tmax_months,
  	                                  monthly_cov_names = tmax_names,
  	                                  output_dir = prd_dir,
  	                                  on_error_rerun = TRUE)
  	
  	# Column that contains the names of the fields to extract prediction data for
  	# Fields with the same name in a flooding area are grouped
  	area_5k_files <-  file.path(cell_dir, paste0("cell_", uids_subset[[n]], "_buffered5k.tif"))
  	stat_files <- extract_predictions(prd_files, 
  	                                  area_files = area_5k_files,
  	                                  n_predictions = 8,
  	                                  output_dir = cell_stat_dir)
	  
  	#sink()
  	
  })
}

# Shows info on the future
f

# Shows the values
value(f)

#area_5k_files <- file.path(cell_dir, paste0("cell_", uids, "_buffered5k.tif"))
#prd_files <- list.files(prd_dir, pattern = ".tif$", full.names = TRUE)

# Read and combine all stats
message_ts("Reading and combining data...")
stat_files <- list.files(cell_stat_dir, pattern = "summary.rds$", full.names = TRUE)
long_df <- do.call(rbind, lapply(stat_files, function(x) readRDS(x)))
saveRDS(long_df, file.path(stat_dir, "cell_summary_long.rds"))
write.csv(long_df, file.path(stat_dir, "cell_summary_long.csv"), row.names = FALSE)


