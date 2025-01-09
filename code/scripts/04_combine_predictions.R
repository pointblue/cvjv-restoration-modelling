# Create bird predictions for specified cells
# Uses package 'future' to split between multiple cores

# Load packages
library(dplyr)
library(terra)

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
global_overwrite <- FALSE
global_verbose <- FALSE

# Combine
stat_cmb_file <- file.path(stat_cell_dir, "_combined_summary.rds")
if (file.exists(stat_cmb_file) & global_overwrite != TRUE) {
  
  message_ts("Loading combined stats")
  stat_df <- readRDS(stat_cmb_file)
  
} else {
  
  message_ts("Combining stat files")
  #stat_df <- do.call(rbind, lapply(stat_files, function(x) readRDS(x))) #takes a while
  #stat_files <- list.files(stat_cell_dir, pattern = "^cell-[0-9]*_summary.rds$", full.names = TRUE)
  stat_files <- list.files(stat_cell_dir, pattern = "^cell-[0-9]*_summary(()|(-ducks)).rds$", full.names = TRUE)
  sfl <- length(stat_files)
  stat_df <- data.frame()
  for (n in 1:sfl) {
    sf <- stat_files[n]
    if (n %% 100 == 1) message_ts("Loading file ", n, " of ", sfl, ": ",  basename(sf))
    sf_df <- readRDS(sf)
    stat_df <- rbind(stat_df, sf_df)
  }
  saveRDS(stat_df, stat_cmb_file)
    
}

# Join with cell pos
cell_df <- read.csv(file.path(grid_dir, "unique_ids_suitable_xy.csv"), stringsAsFactors = FALSE)
stat_xy_df <- stat_df |>
  mutate(Cell = as.integer(Cell)) |>
  left_join(cell_df, by = join_by(Cell == UID), multiple = "first")

# Calculate ensemble weights and ensemble
# Farthest north, use weight of 2N : 1S; in S, use weight of 2S : 1N
north_min <- min(stat_xy_df$Northing)
north_max <- max(stat_xy_df$Northing)
north_range <- north_max - north_min
weight_df <- stat_xy_df |>
  mutate(EnsembleWeight = ifelse(ModelLocation == "N", 
                                 ((Northing - north_min) / north_range) + 1,
                                 ((north_max - Northing) / north_range) + 1))

#test_df <- weight_df[1:100,]

ens_df <- weight_df |>
  group_by(Cell, Easting, Northing, Landcover, Month, Year, Species, SummaryRegion) |>
  summarize(SummaryAreaSqm = mean(SummaryAreaSqm),
            SuitabilitySum = weighted.mean(SuitabilitySum, EnsembleWeight, na.rm = TRUE))


# Export
saveRDS(ens_df, file.path(stat_cell_dir, "_ensembled_summary.rds"))

# Convert to shapefile
ref_rst <- rast(file.path(grid_dir, "unique_ids_masked.tif"))
ens_df$Easting <- as.numeric(ens_df$Easting)
ens_vct <- vect(ens_df, geom = c("Easting", "Northing"), crs = crs(ref_rst))
writeVector(ens_vct, file.path(map_dir, "ensembled_summary.shp"), overwrite = TRUE)

# Export

