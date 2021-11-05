# Second script to be run in split analyses
# 
# Calculates the overlap of forecasted water (with imposed flooding) and landcover

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
auction <- "test_2021-11"
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

# Check that they have been processed
water_bid_files <- list.files(wfc_dir, pattern = paste0(".*_imposed.*_bid_", bid, "_field_", field, ".tif$"), full.names = TRUE)
if (length(water_bid_files) == 0) {
  stop(add_ts("No water forecast found for ", bid, " and field ", field, ". Ensure that the previous step ran successfully."))
} else {
  message_ts("Found", length(water_bid_files), " matching imposed water forecast(s) for ", bid, " and field ", field, ":\n",
             paste("\t", basename(water_bid_files), collapse = "\n"))
}

# Define wanted landcovers
landcovers <- c("Rice", "Corn", "Grain", "NonRiceCrops", "TreatedWetland", "Wetland_SemiSeas", "AltCrop", "AllCrops")
lc_files <- file.path(lc_dir, paste0(landcovers, "_p44r33.tif"))

# Function to overlay water and landcover files
# Takes water and landcover files as inputs
# Specify output_dir to write overlay files as they create
overlay_water_landcover <- function(water_files, landcover_files, output_dir = NULL, overwrite = FALSE) {
  
  wxl_stk <- stack()
  
  # Loop across passed water files
  for (wf in water_files) {
    
    wfn <- basename(wf)
    message_ts("Creating landcover overlays for water file ", wfn)
    
    wtr_rst <- raster(wf)
    
    # Loop across passed landcover files
    for (lcf in landcover_files) {
      
      lcfn <- basename(lcf)
      message_ts("Calculating overlay for ", lcfn, "...")
      lc_rst <- raster(lcf)
      
      # Calculate overlay, optionally writing to file if output_dir is specified
      if (is.null(output_dir)) {
        
        wxl_rst <- overlay(x = wtr_rst, y = lc_rst, fun = function(x, y) {x * y})
        
      } else {
        
        wxl_file <- file.path(output_dir, paste0(substr(wfn, 0, nchar(wfn) - 4), "_x_", lcfn))
        
        # Check file existence and whether or not to overwrite
        if (file.exists(wxl_file) & overwrite != TRUE) {
          
          message_ts("File already processed and overwrite not set to TRUE. Loading from file...")
          wxl_rst <- raster(wxl_file)
          
        } else {
          
          if (file.exists(wxl_file)) message_ts("Overwriting previous output...")
          wxl_rst <- overlay(x = wtr_rst, y = lc_rst, fun = function(x, y) {x * y}, filename = wxl_file, overwrite = overwrite)
          
        }
        
      }
      
      wxl_stk <- stack(wxl_stk, wxl_rst)
      message_ts("Complete.")
      
    }
    
  }
  
  return(wxl_stk)
  
}

wxl_stk <- overlay_water_landcover(water_files = water_bid_files, 
                                   landcover_files = lc_files, 
                                   output_dir = wxl_dir, 
                                   overwrite = FALSE) 
