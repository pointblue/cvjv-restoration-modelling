# Run auction-wide analysis
#
# This needs to be run only once as the predecessor to the split-level analyses

# Library path
.libPaths(c( "E:/nelliott/R/win-library/R-4.1.2" , .libPaths()))
library(future)

# Load definitions and code
code_dir <- "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/code/water_tracker/code"
def_file <- file.path(code_dir, "definitions.R")
code_files <- file.path(code_dir, "functions",
													c("00_shared_functions.R", 
                                                      "01_process_field_file.R",
                                                      "02_impose_flooding.R",
                                                      "03_water_x_landcover.R", 
                                                      "04_water_moving_window.R", 
                                                      "05_predict_birds.R",
                                                      "06_extract_predictions.R",
                                                      "07_summarize_predictions.R"))
sapply(c(def_file, code_files), FUN = function(x) source(x))

# Overlay water and landcover layers --------------------------
# This section is pretty fast, but it may make sense to split by landcovers here, and process each the rest of the
# way on a separate core

# Specify the (monthly) long term average water files to process; usually 3-4 per auction
avg_dir <-  "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/water_averages"
water_files <- file.path(avg_dir, c("p44r33_average_Feb_2011-2021.tif", 
									"p44r33_average_Mar_2011-2021.tif", 
									"p44r33_average_Apr_2011-2021.tif", 
                                    "p44r33_average_May_2011-2021.tif"))
water_files <- file.path(avg_dir, c("p44r33_average_May_2011-2021.tif"))

# Specify landcover files; lc_dir defined in definitions.R
landcovers <- c("Rice", "Corn", "Grain", "NonRiceCrops", "TreatedWetland", "Wetland_SemiSeas", "AltCrop")
lc_dir <- "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/landcover"
lc_files <- file.path(lc_dir, paste0(landcovers, "_p44r33.tif"))

# Overlay water and landcover
# Function defined in functions/03_water_x_landcover.R
wxl_files <- overlay_water_landcover(water_files, 
                                     lc_files, 
                                     output_dir = avg_wxl_dir)  #avg_wxl_dir defined in definitions.R

# Calculate moving windows -------------------------------------
# Use returned filenames from previous function as input; alternatively could define via table or directory search
# This is where it gets slow; may want to split this among multiple cores by dividing up wxl_files
#
# These files are required by the split-level bird predictions


# Set number of processes
n_sessions <- min(length(lc_files), 16)
plan(multisession, workers = n_sessions)

# Split data into n chunks
# Randomize files to prevent a large bloc of unprocessed files from being assigned to the same chunk
if (length(lc_files) > n_sessions) {
	lc_split_files <- chunk(sample(lc_files), n_sessions)
} else {
	lc_split_files <- lc_files
}

# Execute the code in n splits
f <- list()
for (n in 1:n_sessions) {
	
	f[[n]] <- future({
		message_ts("Running on process ID: ", Sys.getpid())
		
		# Overlay water and landcover
		# Function defined in functions/03_water_x_landcover.R
		wxl_files <- overlay_water_landcover(water_files, 
											 lc_split_files[[n]], 
											 output_dir = avg_wxl_dir)  #imp_wxl_dir defined in definitions.R

		
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


