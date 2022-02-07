# Readme file of directories
The template directory has the skeleton of a new auction's directory structure. It contains 
all folders that are needed in the auction analysis.  

Copy 'template' and all subdirectories to a new folder bearing a unique name for the auction
you are working on. This should then be defined as data_dir in the copied definitions.R file.

Most are empty, as they are containers for data that will be created during tha analysis. 

The following folders are populated, as the data they contain will remain relatively constant 
going forward and will be needed by each auction:
  * landcover
  * other_covariates
  * models/birds

If you change the names of folders here, you must update the definitions.R file to 
reflect the changes.

Following is a list of of the directories, corresponding to how they are defined 
in definitions.R.

# Landcover
# lc_dir <- file.path(data_dir, "landcover")

# Runoff and precipitation (used for water forecasting)
# Not set up because forecasting in the cloud has not yet been implemented
# run_dir <- file.path(data_dir, "runoff")
# pcp_dir <- file.path(data_dir, "precip")

# Water data; average and forecasted
# avg_dir <- file.path(data_dir, "water_average")
# wfc_dir <- file.path(data_dir, "water_forecast")

# Other (mostly static) covariates
# cov_dir <- file.path(data_dir, "other_covariates")

# Bird models
# mdl_dir <- file.path(data_dir, "models")
# brd_mdl_dir <- file.path(mdl_dir, "birds")

# Set the auction name
# This will change
# axn_dir <- file.path(data_dir, "auction")
# fld_dir <- file.path(axn_dir, "fields")

# Average flooding (auction-level)
# scn_avg_dir <- file.path(axn_dir, "scenario_average_water")
# avg_wxl_dir <- file.path(scn_avg_dir, "water_x_landcover")
# avg_fcl_dir <- file.path(scn_avg_dir, "water_focal")
# avg_prd_dir <- file.path(scn_avg_dir, "bird_predictions")
# avg_stat_dir <- file.path(scn_avg_dir, "stats")

# Directories required for each split
# Imposed flooding (field or bid-level)
# scn_imp_dir <- file.path(axn_dir, "scenario_imposed_water")
# imp_imp_dir <- file.path(scn_imp_dir, "water_imposed")
# imp_wxl_dir <- file.path(scn_imp_dir, "water_x_landcover")
# imp_fcl_dir <- file.path(scn_imp_dir, "water_focal")
# imp_prd_dir <- file.path(scn_imp_dir, "bird_predictions")
# imp_stat_dir <- file.path(scn_imp_dir, "stats")
