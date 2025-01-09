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
overwrite <- FALSE
verbose <- FALSE

distances <- c("inside", "landscape-250m", "landscape-5km")
seasons <- list("year" = 1:12, "spring" = 3:5, "summer" = 6:8,
                "fall" = 9:11, "winter" = c(12, 1:2))

#species <- c("AMAV", "BNST", "DOWI", "DUNL")

groups <- list("AMAV" = "AMAV",
               "BNST" = "BNST", 
               "DOWI" = "DOWI",
               "DUNL" = "DUNL",
               "ANAC" = "ANAC",
               "ANCL" = "ANCL",
               "ANCR" = "ANCR",
               "shorebirds" = c("AMAV", "BNST", "DOWI", "DUNL"), 
               "ducks" = c("ANAC", "ANCL", "ANCR"),
               "all" = c("AMAV", "BNST", "DOWI", "DUNL", "ANAC", "ANCL", "ANCR"))

# Loop across distances
for (d in distances) {
  
  message_ts("Working on distance ", d)
  
  # Loop across seasons
  for (n in 1:length(seasons)) {
    
    ssn <- names(seasons)[n]
    mths <- seasons[[n]]
    message_ts("Working on season ", ssn)
    
    if (ssn == "year") {
      out_dir <- map_dir
    } else {
      out_dir <- map_ssn_dir
    }
    
    # Individually by species
    #suit_stk <- rast() #results in blank first layer
    suit_sp_files <- c()
    for (gn in 1:length(groups)) {
      
      grp <- names(groups)[gn]
      sp <- groups[[gn]]
      message_ts("Working on group ", grp, " (species: ", paste0(sp, collapse = ", "), ")")
      suit_sp_file <- file.path(out_dir, paste0("suitability_2013-2022_",
                                             ssn, "_", grp, "_",
                                             d, ".tif"))
      
      if (file.exists(suit_sp_file) & overwrite != TRUE) {
        
        message_ts("Already created. Loading")
        suit_sp_files <- c(suit_sp_files, suit_sp_file)
        
      } else {
        
        message_ts("Loading")
        sp_files <- list.files(map_mth_dir, 
                               pattern = paste0("suitability_2013-2022_((", 
                                                paste0(month.abb[mths], collapse = ")|("),
                                                "))_((", 
                                                paste0(sp, collapse = ")|("),
                                                "))_", d, ".tif"),
                               full.names = TRUE)
        sp_stk <- rast(sp_files)
        
        message_ts("Averaging")
        suit_sp_rst <- mean(sp_stk, filename = suit_sp_file, overwrite = TRUE)
        
      }
      
    }
    
  }
  
}

