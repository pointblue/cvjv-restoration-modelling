# Defines project directories and input file characteristics
#  
# Point Blue, California Rice Commission

# Set directories ---------------------------------------------
base_dir <- "V:/Project/wetland/FWSPartners" #getwd() #replace as necessary with "YOUR/BASE/DIR"

log_dir <- file.path(base_dir, "logs")

data_dir <- file.path(base_dir, "code/cvjv-restoration-modelling/data")

lc_dir <- file.path(data_dir, "landcover")
avg_dir <- file.path(data_dir, "water_averages")
cov_dir <- file.path(data_dir, "other_covariates")

mdl_dir <- file.path(data_dir, "models")
brd_mdl_dir <- file.path(mdl_dir, "birds")

wtr_dir <- file.path(data_dir, "water_average")

ref_file <- file.path(wtr_dir, "valley_average_Apr_2011-2021_snapped.tif")

# Analysis
anl_dir <- file.path(base_dir, "analysis")

grid_dir <- file.path(anl_dir, "grid")
cell_dir <- file.path(grid_dir, "cells")

wxl_dir <- file.path(anl_dir, "water_x_landcover")
fcl_dir <- file.path(anl_dir, "water_focal")
prd_dir <- file.path(anl_dir, "bird_predictions")
stat_dir <- file.path(anl_dir, "stats")

# Landcover files
lc_defs <- list("Unsuitable" = 0, "GrassPasture" = 1, "Corn" = 3, 
                "GrainPlus" = c(5, 7, 13), "Alfalfa" = 11, "Rice" = 12, 
                "NonRiceCrop" = c(3, 5, 7, 13, 11, 99),
                "WetlandNatural" = c(20, 30, 40), "WetlandTreated" = 50,
                "AlternatingCrop" = 99)
model_lcs <- c("Rice", "Corn", "GrainPlus", "AlternatingCrop", "NonRiceCrop", "WetlandNatural", "WetlandTreated")
model_lc_files <- file.path(lc_dir, paste0(model_lcs, "_2014-2021.tif"))

# Basins
basins <- c("American", "Butte", "Colusa", "Delta", "San Joaquin", "Suisun", "Sutter", "Tulare", "Yolo")

# Bird definitions
bird_df <- data.frame("CommonName" = c("American Avocet", "Black-necked Stilt", "Dowitcher", "Dunlin", 
                                       "Northern Pintail", "Northern Shoveler", "Green-winged Teal"),
                      "CommonCode" = c("AMAV", "BNST", "DOWI", "DUNL", "NOPI", "NSHO", "GWTE"),
                      "ScientificCode" =c("REAM", "HIME", "LISPP", "CALA", "ANAC", "ANCL", "ANCR"))

# Bird models
shorebird_sci_base <- paste(rep(c("CALA", "HIME", "LISPP", "REAM"), each = 2), c("N", "S"), sep = "_")
shorebird_com_base <- paste(rep(c("DUNL", "BNST", "DOWI", "AMAV"), each = 2), c("N", "S"), sep = "_")
# Two models for each species and model type, one for the north valley one and for the south
# Ensemble at (2N + 1S) / 3 for north and (1N + 2S) / 3 for south
# Reallong models that combine long-term and real-time water data
shorebird_model_files_reallong <- file.path(brd_mdl_dir, paste0(shorebird_sci_base, "_Reallong_nopattern_subset.rds"))
shorebird_model_names_reallong <- paste0(shorebird_com_base, "_reallong")

# Long models that just use the long-term average
shorebird_model_files_long <- file.path(brd_mdl_dir, paste0(shorebird_sci_base, "_Long_lowN_subset.rds"))
shorebird_model_names_long <-  paste0(shorebird_com_base, "_longterm")

# Static covariates
#bird_model_cov_files <- file.path(cov_dir, c("data_type_constant_ebird_p44r33.tif", "valley_roads_p44r33.tif"))
bird_model_cov_files <- file.path(cov_dir, c("data_type_constant_ebird_valley.tif", "valley_roads_valley.tif"))
bird_model_cov_names <- c("COUNT_TYPE2", "roads5km")

# Monthly covariates (tmax)
#mths <- month.abb[c(1:5, 7:12)] #no June
mths <- month.abb[4]
#tmax_files <- file.path(cov_dir, paste0("tmax_", mths, "_p44r33.tif"))
tmax_files <- file.path(cov_dir, paste0("tmax_", mths, "_valley_snapped2.tif"))
tmax_months <- mths
tmax_names <- rep("tmax250m", length(mths))
