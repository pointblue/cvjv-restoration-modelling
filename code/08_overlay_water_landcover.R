# Creates water x landcover overlays
# Used in turn to create moving-window summations of water x landcover, which are bird model inputs

# Load necessary functions and directories
# Must either run 00_definitions.R before this file or source it, replacing getwd() with your base working directory below
code_file <- file.path(getwd(), "code/00_definitions.R")
if (!file.exists(code_file)) {
  stop("Code file with required definitions not found in specified location. Please update path code_file.")
} else {
  source(code_file)  
}

# Load packages
library(rgdal)
library(raster)

# Define wanted landcovers
landcovers <- c("Rice", "Corn", "Grain", "NonRiceCrops", "TreatedWetland", "Wetland_SemiSeas", "AltCrop", "AllCrops")
lc_files <- file.path(lc_dir, paste0(landcovers, "_p44r33.tif"))

# List and parse water files
water_files <- list.files(wfc_dir, pattern = ".*tif$", full.names = TRUE)
water_df <- parse_filename(water_files)

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
        
        wtr_lc_rst <- overlay(x = wtr_rst, y = lc_rst, fun = function(x, y) {x * y})
       
      } else {
        
        wxl_file <- file.path(output_dir, paste0(substr(wfn, 0, nchar(wfn) - 4), "_x_", lcfn))
        wxl_rst <- overlay(x = wtr_rst, y = lc_rst, fun = function(x, y) {x * y}, filename = wxl_file, overwrite = overwrite)
        
      }
      
      wxl_stk <- stack(wxl_stk, wxl_rst)
      message_ts("Complete.")
      
    }
    
  }
  
  return(wxl_stk)
  
}

# Overlay water files with landcovers, specifying subsets of water and landcover files as desired
# This exmaple selects all water layers from Feb - Apr 2021 and overlays them with all landcover files
wtr_files <- water_df$File[water_df$Year == 2021 & water_df$Month %in% c("Feb", "Mar", "Apr")]
wxl_stk <- overlay_water_landcover(water_files = wtr_files, 
                                   landcover_files = lc_files, 
                                   output_dir = wxl_dir, 
                                   overwrite = FALSE) 
