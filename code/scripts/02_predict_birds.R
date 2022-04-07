# Run auction-wide analysis
#
# This needs to be run only once as the predecessor to the split-level analyses

# Library path
#.libPaths(c( "E:/nelliott/R/win-library/R-4.1.2" , .libPaths()))
library(future)
#library(terra)
#terraOptions(memfrac=0.5, tempdir = "c:/temp")

# Load definitions and code
code_dir <- "V:/Project/wetland/FWSPartners/code/cvjv-restoration-modelling/code"
def_file <- file.path(code_dir, "definitions.R")
code_files <- file.path(code_dir, "functions",
													c("00_shared_functions.R", "01_overlay_water_landcover.R"))
sapply(c(def_file, code_files), FUN = function(x) source(x))

# Create directories
check_dir(c(log_dir, cell_dir, anl_dir, wxl_dir, fcl_dir, prd_dir, stat_dir), create = TRUE)

# Overlay water and landcover layers --------------------------
# This section is pretty fast, but it may make sense to split by landcovers here, and process each the rest of the
# way on a separate core

# Specify the (monthly) long term average water files to process; usually 3-4 per auction
water_files <- file.path(wtr_dir, "valley_average_Apr_2011-2021_snapped.tif")




# Calculate moving windows -------------------------------------
# Use returned filenames from previous function as input; alternatively could define via table or directory search
# This is where it gets slow; may want to split this among multiple cores by dividing up wxl_files
#
# These files are required by the split-level bird predictions

# Select uids
uid_ag_df <- read.csv(file.path(grid_dir, "unique_ids_ag.csv"))
uids_ag <- uid_ag_df$UID
uids <- uids_ag[1:100]

# Set number of processes
n_sessions <- min(length(uids), 16)
plan(multisession, workers = n_sessions)

# Split data into n chunks
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
	  log_file <- file.path(log_dir, paste0("log_", Sys.Date(), "_", pid, ".txt"))
	  sink(log_file, append = FALSE, split = TRUE)
	  
	  message_ts("Running on process ID: ", pid)
	  
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
	})
}
		
		# Create mean neighborhood water rasters
		# Function defined in functions/04_water_moving_window.R
		fcl_files <- mean_neighborhood_water(wxl_files,                #previously-created water x landcover files
                                    distances = c(250, 5000), #250m and 5km
                                    output_dir = avg_fcl_dir, 
                                    trim_extent = FALSE)      #only set for TRUE with splits

		
		# Predict							 
		prd_files <- predict_bird_rasters(fcl_files,                  
                                  fcl_files_longterm,
                                  scenarios = scenarios_filter,
                                  water_months = c("Feb", "Mar", "Apr"),
                                  model_files = shorebird_model_files_reallong, 
                                  model_names = shorebird_model_names_reallong, 
                                  static_cov_files = bird_model_cov_files, 
                                  static_cov_names = bird_model_cov_names,
                                  monthly_cov_files = tmax_files,
                                  monthly_cov_months = tmax_months,
                                  monthly_cov_names = tmax_names,
                                  output_dir = imp_prd_dir)
								  
		# Column that contains the names of the fields to extract prediction data for
		# Fields with the same name in a flooding area are grouped
		stat_files <- extract_predictions(prd_files, 
										  floodarea_files, 
										  field_column = "Fild_ID",
										  area_column = "acres_1",
										  output_dir = imp_stat_dir)
		
	})

}

# Shows info on the future
f

# Shows the values
value(f)




# Create bird predictions -------------------------------------
# This is messy because the models need a ton of files from different places. 
# Everything is passed via the function call. 

# Can pass all fcl_files if divided processing upstream or subset fcl_files here and call multiple instances.
# If splitting, ensure all files from one flooding area and month are included

# fcl_files for the auction-level analysis are the long term ones
fcl_files_longterm <- fcl_files

# Can subset files using the scenarios parameter, which is applied as a regex filter
scenarios_filter <- "water"

# values passed to model_files, model_names, covariate_files, covariate_names, and monthly_files 
#    will change rarely and are specified in definitions.R

prd_files <- predict_bird_rasters(fcl_files,                  
                                  fcl_files_longterm,
                                  scenarios = scenarios_filter,
                                  water_months = c("Aug", "Sep"),
                                  model_files = shorebird_model_files_long, 
                                  model_names = shorebird_model_names_long, 
                                  static_cov_files = bird_model_cov_files, 
                                  static_cov_names = bird_model_cov_names,
                                  monthly_cov_files = tmax_files,
                                  monthly_cov_months = tmax_months,
                                  monthly_cov_names = tmax_names,
                                  output_dir = avg_prd_dir)


