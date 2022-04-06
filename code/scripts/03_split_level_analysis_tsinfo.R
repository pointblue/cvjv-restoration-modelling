# Run split-level analysis
#
# 


# Library path
.libPaths(c( "E:/nelliott/R/win-library/R-4.1.2" , .libPaths()))
library(future)

# Raster temp dir
library(raster)
rasterOptions(tmpdir = "E:/nelliott/temp")

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


# Create directories for auction
auction_dirs <- c(axn_dir, fld_dir, split_dir, scn_avg_dir, avg_wtr_dir, avg_wxl_dir, avg_fcl_dir,
                  avg_prd_dir, avg_stat_dir, scn_imp_dir, imp_wtr_dir, imp_wxl_dir, imp_fcl_dir,
                  imp_prd_dir, imp_stat_dir)
check_dir(auction_dirs, create = TRUE)

# Split field file ------------------------------------------
# This section happens once and must complete before any subsequent steps run
# Name of the shapefile
floodarea_shapefile <- file.path(fld_dir, "sod_wetland_ponds_utm10.shp") #YOUR_SHAPEFILE_NAME.shp"

# Name of the column with names on which to group and split shapefile; should not contain special characters
# This would be the column of field name for a field-level split analysis and the column of the bid for a bid-level one
# If doing a combination, must create a new column as a composite key prior to running
split_column <- "FloodID"

# Reference raster
ref_file <- file.path(cov_dir, "data_type_constant_ebird_valley.tif")

# Specify the (monthly) forecasted (or long-term average) water files to process; usually 3-4 per auction
water_files <- file.path(avg_dir, "valley_average_Apr_2011-2021_snapped.tif")

# Specify landcover files; lc_dir defined in definitions.R
landcovers <- c("Rice", "Corn", "Grain", "NonRiceCrops", "TreatedWetland", "Wetland_SemiSeas", "AltCrop")
lc_files <- file.path(lc_dir, paste0(landcovers, "_valley.tif"))

# Get flooding areas
library(rgdal)
fa_shp <- readOGR(dirname(floodarea_shapefile), gsub(".shp", "", basename(floodarea_shapefile)))
flooding_areas <- unique(fa_shp[[split_column]])

# Set number of processes
n_sessions <- 14
plan(multisession, workers = n_sessions)

# Execute the code in n splits
f <- list()
n <- 0
for (fa in rev(flooding_areas)) {
	
  # Counter
  n <- n + 1
  
	f[[n]] <- future({
		message_ts("Running on process ID: ", Sys.getpid())
		
	  # Split, rasterize, and buffer field shapefile
	  # Function defined in functions/01_process_floodarea_file.R
	  floodarea_files <- split_flooding_area(floodarea_shapefile, 
	                                         split_column, 
	                                         guide_raster = ref_file, 
	                                         output_dir = split_dir,            #defined in definitions.R
	                                         flooding_areas = fa,
	                                         rasterize_file = TRUE,           #required for next step
	                                         buffer_dist = 10000)             #required for later masking that dramatically speeds processing 
	  
	  
	  # Specify field files; want tifs, not the shapefiles
	  #floodarea_files <- list.files(split_dir, pattern = ".*shp$", full.names = TRUE)
	  floodarea_raster_files <- gsub(".shp", ".tif", floodarea_files)
	  
	  # Mask (call impose_flooding function for side effect of masking only)
	  avg_wtr_files <- impose_flooding(water_files, 
	                                   floodarea_raster_files, 
	                                   output_dir = avg_wtr_dir,
	                                   imposed_value = NULL,       #call for side effect only
	                                   mask = TRUE)                #significantly speeds up processing in later steps
	  
	  # Overlay water and landcover; average
	  # Function defined in functions/03_water_x_landcover.R
	  avg_wxl_files <- overlay_water_landcover(avg_wtr_files, 
	                                       lc_files, 
	                                       output_dir = avg_wxl_dir)  #avg_wxl_dir defined in definitions.R
	  
	  # Create mean neighborhood water rasters; average
	  avg_fcl_files <- mean_neighborhood_water(avg_wxl_files,                #previously-created water x landcover files
	                                       distances = c(250, 5000), 	   #250m and 5km
	                                       output_dir = avg_fcl_dir, 
	                                       trim_extent = TRUE)           #only set for TRUE with splits
	  
	  
		# Impose flooding
		# Function defined in functions/02_impose_flooding.R
		imp_wtr_files <- impose_flooding(water_files, 
										   floodarea_raster_files, 
										   output_dir = imp_wtr_dir,   #imp_wtr_dir defined in definitions.R
										   mask = TRUE)                #significantly speeds up processing in later steps
										   
		# Overlay water and landcover; imposed
		# Function defined in functions/03_water_x_landcover.R
		imp_wxl_files <- overlay_water_landcover(imp_wtr_files, 
											  lc_files, 
											  output_dir = imp_wxl_dir)  #imp_wxl_dir defined in definitions.R

		# Create mean neighborhood water rasters; imposed
		imp_fcl_files <- mean_neighborhood_water(imp_wxl_files,                #previously-created water x landcover files
										  distances = c(250, 5000), 	   #250m and 5km
										  output_dir = imp_fcl_dir, 
										  trim_extent = TRUE)           #only set for TRUE with splits
		
		# Predict							 
		prd_files <- predict_bird_rasters(imp_fcl_files,                  
                                  avg_fcl_files,
                                  scenarios = "imposed",
                                  water_months = c("Apr"),
                                  model_files = shorebird_model_files_reallong, 
                                  model_names = shorebird_model_names_reallong, 
                                  static_cov_files = bird_model_cov_files, 
                                  static_cov_names = bird_model_cov_names,
                                  monthly_cov_files = tmax_files,
                                  monthly_cov_months = tmax_months,
                                  monthly_cov_names = tmax_names,
                                  output_dir = imp_prd_dir)
								  
		#sapply(c(def_file, code_files), FUN = function(x) source(x))
		#prd_files <- list.files(imp_prd_dir, pattern = ".*58.*tif$", full.names = TRUE)

		# Column that contains the names of the fields to extract prediction data for
		# Fields with the same name in a flooding area are grouped
		stat_files <- extract_predictions(prd_files, 
										  floodarea_files, 
										  field_column = "PondName", #uniquely identifies fields within a flooding area
										  area_column = "GISAcres",
										  output_dir = imp_stat_dir)
		
	})

}

# Shows info on the future
f

# Shows the values
value(f)






# Combine bird predictions -----------------------------------------
# This can only be run after every prediction has finished and all have been extracted
stat_files <- list.files(imp_stat_dir, pattern = ".*summary.rds$", full.names = TRUE)

summary_files <- summarize_predictions(stat_files,
                                       metadata_csv_file = file.path(fld_dir, "bid_metadata.csv"),
                                       output_dir = imp_stat_dir)

