# First script to be run for the split analyses
#
# Test

# Load necessary functions and directories
# Must either run 00_definitions.R before this file or source it, replacing getwd() with your base working directory below
code_file <- file.path(getwd(), "code/00_definitions.R")
if (!file.exists(code_file)) {
  stop("Code file with required definitions not found in specified location. Please update path code_file.")
} else {
  source(code_file)  
}

# Packages
library(sp)
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
  message_ts("Found matching shapefile for bid ", bid, " and field ", field)
}

# Load guide raster
guide_rst <- raster(file.path(cov_dir, "data_type_constant_ebird_p44r33.tif"))

# Rasterize
message_ts("Rasterizing field...")
bid_field_fn <- paste0(fld_fn, "_bid_", bid, "_field_", field, ".tif")
bid_field_file <- file.path(fld_dir, bid_field_fn)
bid_field_rst <- rasterize(bid_field_shp, guide_rst, field = 1, filename = bid_field_file, overwrite = TRUE)

# Turn values within 10km of field to 2s instead of NAs
# Used for masking later
# Width is in meters
message_ts("Calculating 10km buffer...")
bid_field_10k_rst <- buffer(bid_field_rst, width = 10000)
message_ts("Adding buffer to field raster...")
bid_field_rst <- overlay(x = bid_field_rst, y = bid_field_10k_rst, fun = function(x, y) {
                                                                          ifelse(is.na(x) & y == 1, 2, x) },
                         filename = bid_field_file, overwrite = TRUE)

# Define function to impose flooding
# Takes water and field files as inputs
# Specify output_dir to write overlay files as they create
impose_flooding <- function(water_files, flood_files, output_dir = NULL, overwrite = FALSE) {
  
  imp_stk <- stack()
  
  # Loop across passed water files
  for (wf in water_files) {
    
    wfn <- basename(wf)
    message_ts("Imposing flooding on water file ", wfn)
    
    wtr_rst <- raster(wf)
    
    # Loop across passed flood files
    for (ff in flood_files) {
      
      ffn <- basename(ff)
      message_ts("Imposing flooding for ", ffn, "...")
      fld_rst <- raster(ff)
      
      # Calculate overlay, optionally writing to file if output_dir is specified
      if (is.null(output_dir)) {
        
        imp_rst <- overlay(x = wtr_rst, y = fld_rst, fun = function(x, y) {
                                                            ifelse(is.na(y), NA, 
                                                                   ifelse(y != 2, y, x))
                                                          })
        
      } else {
        
        imp_file <- file.path(output_dir, paste0(substr(wfn, 0, nchar(wfn) - 4), "_imposed_", ffn))
        imp_rst <- overlay(x = wtr_rst, y = fld_rst, fun = function(x, y) {
                                                                ifelse(is.na(y), NA, 
                                                                       ifelse(y != 2, y, x))
                                                              }, 
                           filename = imp_file, overwrite = overwrite)
        
      }
      
      imp_stk <- stack(imp_stk, imp_rst)
      message_ts("Complete.")
      
    }
    
  }
  
  return(imp_stk)
  
}

# Impose flooding
water_files <- list.files(wfc_dir, pattern = ".*tif$", full.names = TRUE)
water_df <- parse_filename(water_files)
water_files <- water_df$File[water_df$Year == 2021 & water_df$Month %in% c("Feb", "Mar", "Apr")]
imp_stk <- impose_flooding(water_files, bid_field_file, output_dir = wfc_dir, overwrite = TRUE)

