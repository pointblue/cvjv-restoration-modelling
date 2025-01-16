# Create bird predictions for specified cells
# Uses package 'future' to split between multiple cores

# Load packages
library(dplyr)
library(terra)
library(gstat)

# Set temp directory for terra on non-boot drive if possible
terraOptions(tempdir = "E:/rtemp")

# Load definitions and code
code_dir <- "V:/Project/wetland/FWSPartners/code/cvjv-restoration-modelling/code"
def_file <- file.path(code_dir, "definitions.R")
code_files <- list.files(file.path(code_dir, "functions"), pattern = ".*R$", full.names = TRUE)
sapply(c(def_file, code_files), FUN = function(x) source(x))

# Check directories, creating if missing
check_dir(c(log_dir, cell_dir, anl_dir, wxl_dir, fcl_dir, prd_dir, stat_dir, stat_cell_dir), create = TRUE)

# Global options
overwrite <- FALSE
verbose <- FALSE

# Load reference raster and stats
uid_rst <- rast(file.path(grid_dir, "unique_ids_coarse.tif"))
#ens_vct <- vect(file.path(map_dir, "ensembled_summary.shp"))

# Load / create ag footprint file
ag_rsp_file <- file.path(grid_dir, "ag_footprint_resampled.tif")
if (!file.exists(ag_rsp_file) | overwrite == TRUE) {
  
  message_ts("Resampling ag footprint raster")
  ag_rst <- rast(file.path(grid_dir, "ag_footprint.tif"))
  ag_rsp_rst <- resample(ag_rst, uid_rst, filename = ag_rsp_file)
  ag_rst <- ag_rsp_rst
  
} else {
  ag_rst <- rast(ag_rsp_file)
}

# Load suitable cells
ag_df <- read.csv(file.path(grid_dir, "unique_ids_suitable_xy.csv"), stringsAsFactors = FALSE)
ag_df <- filter(ag_df, Landcover != "GrassPasture")

# Unique species/month/year/region combos
ens_df <- na.omit(readRDS(file.path(stat_cell_dir, "_ensembled_summary.rds")))
unique_df <- unique(ens_df[c("Month", "Year", "Species", "SummaryRegion")])

# Summary regions
for (n in 1:nrow(unique_df)) {
  
  yr <- unique_df$Year[n]
  mth <- unique_df$Month[n]
  sp <- unique_df$Species[n]
  sr <- unique_df$SummaryRegion[n]
  message_ts("Working on ", yr, ", ", mth, ", ", sp, ", ", sr)
  
  suit_file <- file.path(map_um_mth_dir, paste0("suitability_", yr, 
                                         "_", mth, "_", sp, "_",
                                         tolower(gsub(" ", "-", sr)), ".tif"))
  if (!file.exists(suit_file) | overwrite == TRUE) {
    
    # Subset
    message_ts("Subsetting")
    scn_df <- ens_df[ens_df$Month == mth & ens_df$Year == yr & ens_df$Species == sp & 
                         ens_df$SummaryRegion == sr & !is.na(ens_df$SuitabilitySum), ]
    
    # Add all ag points
    cmb_df <- left_join(ag_df, 
                        scn_df[c("Cell", "SuitabilitySum")], 
                        by = join_by(UID == Cell))
    
    idw_mdl <- gstat(id = "SuitabilitySum", formula = SuitabilitySum ~ 1, 
                     data = scn_df, locations = ~ Easting + Northing, 
                     nmax = 9, set = list(idp = 3))
    
    idw_prd <- predict(idw_mdl, cmb_df)
    names(idw_prd)[names(idw_prd) == "SuitabilitySum.pred"] <- "SuitabilitySum"
    
    # Rasterize
    message_ts("Rasterizing")
    suit_rst <- rasterize(as.matrix(idw_prd[c("Easting", "Northing")]), 
                          uid_rst, values = idw_prd$SuitabilitySum, background = NA,
                          filename = suit_file, overwrite = TRUE)
    
  } else {
    
    message_ts("Raster already created. Loading...")
    suit_rst <- rast(suit_file)
  
  }
  
  suit_msk_file <- file.path(map_mth_dir, paste0("suitability_", yr, 
                                                "_", mth, "_", sp, "_",
                                                tolower(gsub(" ", "-", sr)), ".tif"))
  if (!file.exists(suit_msk_file) | overwrite == TRUE) {
    
    message_ts("Masking suitability raster...")
    suit_msk_rst <- mask(suit_rst, ag_rst, filename = suit_msk_file, overwrite = TRUE)
    
  } else {
    
    message_ts("Suitability raster already masked. Moving to next...")
    
  }
  
}
