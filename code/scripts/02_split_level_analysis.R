# Run split-level analysis
#
# 

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

# Split field file ------------------------------------------
# This section happens once and must complete before any subsequent steps run
# Name of the shapefile
floodarea_shapefile <- file.path(fld_dir, "Bid4Birds_Fields_Spring2022_metadata_utm10.shp") #YOUR_SHAPEFILE_NAME.shp"

# Name of the column with names on which to group and split shapefile; should not contain special characters
# This would be the column of field name for a field-level split analysis and the column of the bid for a bid-level one
# If doing a combination, must create a new column as a composite key prior to running
split_column <- "Splt_ID"

# Reference raster
ref_file <- file.path(cov_dir, "data_type_constant_ebird_p44r33.tif")

# Split, rasterize, and buffer field shapefile
# Function defined in functions/01_process_floodarea_file.R
floodarea_files <- split_flooding_area(floodarea_shapefile, 
                                       split_column, 
                                       guide_raster = ref_file, 
                                       output_dir = fld_dir,            #defined in definitions.R
                                       rasterize_file = TRUE,           #required for next step
                                       buffer_dist = 10000)             #required for later masking that dramatically speeds processing 

#floodarea_files <- list.files(fld_dir, pattern = ".shp$", full.names = TRUE)[1:6]

# Impose flooding on fields -----------------------------------
# In this section, the number of files becomes water_files * floodarea_files; usually ~120 (3*40)
# One option for parallelization would be to split each field or even each water/field combo into it's own core here and process
# separately until all the bird predictions are done
#
# Specify the (monthly) forecasted (or long-term average) water files to process; usually 3-4 per auction
water_files <- file.path(wfc_dir, c("L8_p44r33_2022_Feb_water_forecast.tif", 
									"L8_p44r33_2022_Mar_water_forecast.tif", 
									"L8_p44r33_2022_Apr_water_forecast.tif", 
                                    "L8_p44r33_2022_May_water_forecast.tif"))

# Specify field files; want tifs, not the shapefiles
floodarea_raster_files <- gsub(".shp", ".tif", floodarea_files)

# Impose flooding
# Function defined in functions/02_impose_flooding.R
water_imp_files <- impose_flooding(water_files, 
                                   floodarea_raster_files, 
                                   output_dir = imp_imp_dir,   #imp_imp_dir defined in definitions.R; imp_imp is not a typo
                                   mask = TRUE)                #significantly speeds up processing in later steps


# Overlay water and landcover layers --------------------------
# Specify the water water files to process; usually the results of impose_flooding above

# Specify landcover files; lc_dir defined in definitions.R
landcovers <- c("Rice", "Corn", "Grain", "NonRiceCrops", "TreatedWetland", "Wetland_SemiSeas", "AltCrop")
lc_files <- file.path(lc_dir, paste0(landcovers, "_p44r33.tif"))

# Overlay water and landcover
# Function defined in functions/03_water_x_landcover.R
wxl_files <- overlay_water_landcover(water_imp_files, 
                                     lc_files, 
                                     output_dir = imp_wxl_dir)  #imp_wxl_dir defined in definitions.R

# Calculate moving windows -------------------------------------
# Use returned filenames from previous function as input; alternatively could define via table or directory search

# Create mean neighborhood water rasters
# Function defined in functions/04_water_moving_window.R
#
# The last iteration produced major performace gains in this step
fcl_files <- mean_neighborhood_water(wxl_files,                #previously-created water x landcover files
                                     distances = c(250, 5000), #250m and 5km
                                     output_dir = imp_fcl_dir, 
                                     trim_extent = TRUE)       #only set for TRUE with splits


# Create bird predictions -------------------------------------
# This is messy because the models need a ton of files from different places. 
# Everything is passed via the function call. 

# Can pass all fcl_files if divided processing upstream or subset fcl_files here and call multiple instances.
# If splitting, ensure all files from one flooding area and month are included

# water_files_longterm are created by the auction-level analysis
fcl_files_longterm <- list.files(avg_fcl_dir, pattern = "imposed.*tif$", full.names = TRUE) ##############CHANGE###############

# Can subset files using the scenarios parameter, which is applied as a regex filter
scenarios_filter <- "water_forecast_imposed"

# values passed to model_files, model_names, covariate_files, covariate_names, and monthly_files 
#    will change rarely and are specified in definitions.R

prd_files <- predict_bird_rasters(fcl_files,                  
                                  fcl_files_longterm,
                                  scenarios = scenarios_filter,
                                  water_months = "Aug",
                                  model_files = shorebird_model_files_reallong, 
                                  model_names = shorebird_model_names_reallong, 
                                  static_cov_files = bird_model_cov_files, 
                                  static_cov_names = bird_model_cov_names,
                                  monthly_cov_files = tmax_files,
                                  monthly_cov_months = tmax_months,
                                  monthly_cov_names = tmax_names,
                                  output_dir = imp_prd_dir)

# Extract bird predictions ----------------------------------------

# Column that contains the names of the fields to extract prediction data for
# Fields with the same name in a flooding area are grouped
stat_files <- extract_predictions(prd_files, 
                                  floodarea_files, 
                                  field_column = "Name",
                                  area_column = "AreaAcres",
                                  output_dir = imp_stat_dir)


# Combine bird predictions -----------------------------------------
# This can only be run after every prediction has finished and all have been extracted
stat_files <- list.files(imp_stat_dir, pattern = ".*summary.rds$", full.names = TRUE)

summary_files <- summarize_predictions(stat_files,
                                       metadata_csv_file = file.path(fld_dir, "bid_metadata.csv"),
                                       output_dir = imp_stat_dir)

