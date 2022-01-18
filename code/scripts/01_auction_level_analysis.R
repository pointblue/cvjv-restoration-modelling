# Run auction-wide analysis
#
# This needs to be run only once as the predecessor to the split-level analyses

# Load definitions and code
def_file <- file.path(getwd(), "code/definitions.R")
code_files <- file.path(getwd(), "code/functions/", c("00_shared_functions.R", "03_water_x_landcover.R", "04_water_moving_window.R", "05_predict_birds.R"))
sapply(c(def_file, code_files), FUN = function(x) source(x))

# Overlay water and landcover layers --------------------------
# This section is pretty fast, but it may make sense to split by landcovers here, and process each the rest of the
# way on a separate core

# Specify the (monthly) long term average water files to process; usually 3-4 per auction
water_files <- file.path(avg_dir, c("p44r33_average_Aug_2010-2020.tif", "p44r33_average_Sep_2010-2020.tif"))

# Specify landcover files; lc_dir defined in definitions.R
landcovers <- c("Rice", "Corn", "Grain", "NonRiceCrops", "TreatedWetland", "Wetland_SemiSeas", "AltCrop")
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

# Create mean neighborhood water rasters
# Function defined in functions/04_water_moving_window.R
fcl_files <- mean_neighborhood_water(wxl_files,                #previously-created water x landcover files
                                    distances = c(250, 5000), #250m and 5km
                                    output_dir = avg_fcl_dir, 
                                    trim_extent = FALSE)      #only set for TRUE with splits


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


