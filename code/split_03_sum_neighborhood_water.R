# Third script to be run in split analyses
# 
# Calculates the neighborhood sums of forecasted water (with imposed flooding) by landcover

# Load necessary functions and directories
# Must either run 00_definitions.R before this file or source it, replacing getwd() with your base working directory below
code_file <- file.path(getwd(), "code/00_definitions.R")
if (!file.exists(code_file)) {
  stop("Code file with required definitions not found in specified location. Please update path code_file.")
} else {
  source(code_file)  
}

# Packages
library(rgdal)
library(raster)

# Load field file
fld_fn <- "test_2021-11"
field_shp <- readOGR(fld_dir, fld_fn)

# Set bid and field being worked on
# Eventually pass via command line
bid <- "DEL-T1"
field <- "1"

# Check they exist
bid_field_shp <- field_shp[field_shp$Bid == bid & field_shp$Field == field,]
if (nrow(bid_field_shp) == 0) {
  stop(add_ts("No field in shapefile matches specified bid and field names: ", bid, " and field ", field))
} else if (nrow(bid_field_shp) > 1){
  stop(add_ts("Multiple fields in shapefile match specified bid and field names: ", bid, " and field ", field, 
              ". Please check field shapefile."))
} else {
  #message_ts("Found matching shapefile for bid ", bid, " and field ", field)
}

# Define wanted landcovers
landcovers <- c("Rice", "Corn", "Grain", "NonRiceCrops", "TreatedWetland", "Wetland_SemiSeas", "AltCrop", "AllCrops")
n_lc <- length(landcovers)

# List and check files exist
wxl_bid_files <- lapply(paste0(".*_imposed.*_bid_", bid, "_field_", field, "_x_", landcovers, ".*tif$"), 
                          FUN = function(ptn) { list.files(wxl_dir, pattern = ptn, full.names = TRUE) })
n_files <- sapply(wxl_bid_files, FUN = length)

if (any(n_files == 0)) {
  
  stop(add_ts("The following landcover(s) are missing their water overlays for bid ", bid, " and field ", field, ":\n",
              paste("\t", landcovers[n_files == 0], collapse = "\n")))
  
} else if (!all(n_files == max(n_files))) {
  
  stop(add_ts("Inconsistent numbers of landcover water files existfor bid ", bid, " and field ", field, ". ",
              "The following landcover(s) are missing one or more water overlays:\n",
              paste("\t", landcovers[n_files < max(n_files)], collapse = "\n")))
  
}

# Convert to a vector for function
wxl_bid_files <- unlist(wxl_bid_files)

# Function to calculate moving window sums of water
# Takes water (by landcover) files and distance as input
# Specify output_dir to write overlay files as they create
# Distance is in meters
sum_neighborhood_water <- function(water_files, distance, output_dir = NULL, overwrite = FALSE) {
  
  fcl_stk <- stack()
  
  # Loop across passed water files
  for (wf in water_files) {
    
    wfn <- basename(wf)
    message_ts("Summing neighborhood water for water file ", wfn)
    
    wtr_rst <- raster(wf)
    
    # Loop across passed distances
    for (d in distance) {
      
      message_ts("Creating focal matrix for distance ", d, "m...")
      fwm <- focalWeight(wtr_rst, d, type = "circle")
      
      # Calculate overlay, optionally writing to file if output_dir is specified
      message_ts("Calculating neighborhood statistics...")
      if (is.null(output_dir)) {
        
        fcl_rst <- focal(wtr_rst, fwm, fun = mean, na.rm = TRUE)
        
      } else {
        
        fcl_file <- file.path(output_dir, paste0(substr(wfn, 0, nchar(wfn) - 4), "_", d, "m.tif"))
        
        # Check file existence and whether or not to overwrite
        if (file.exists(fcl_file) & overwrite != TRUE) {
          
          message_ts("File already processed and overwrite not set to TRUE. Loading from file...")
          fcl_rst <- raster(fcl_file)
          
        } else {
          
          if (file.exists(fcl_file)) message_ts("Overwriting previous output...")
          fcl_rst <- focal(wtr_rst, fwm, fun = mean, na.rm = TRUE, filename = fcl_file, overwrite = overwrite)
          
        }
        
        
      }
      
      fcl_stk <- stack(fcl_stk, fcl_rst)
      message_ts("Complete.")
      
    }
    
  }
  
  return(fcl_stk)
  
}

fcl_stk <- sum_neighborhood_water(water_files = wxl_bid_files, 
                                   distance = c(250, 500), 
                                   output_dir = fcl_dir, 
                                   overwrite = FALSE)
