# Prep Zonation layers

# Load packages
library(dplyr)

# Set temp directory for terra on non-boot drive if possible
terraOptions(tempdir = "E:/rtemp")

# Load definitions and code
code_dir <- "E:/code/cvjv-restoration-modelling/code"
def_file <- file.path(code_dir, "definitions.R")
code_files <- list.files(file.path(code_dir, "functions"), pattern = ".*R$", full.names = TRUE)
sapply(c(code_files, def_file), FUN = function(x) source(x))

# Local dirs
base_dir <- file.path("E:/code/cvjv-restoration-modelling")
sum_dir <- file.path(base_dir, "analysis/bird_summaries")
sum_ssn_dir <- file.path(sum_dir, "by_season")

anl_dir <- file.path(base_dir, "analysis")
zon_dir <- file.path(anl_dir, "zonation")
zon_in_dir <- file.path(zon_dir, "inputs")
zon_out_dir <- file.path(zon_dir, "runs")

# Loop across seasons and species, rescaling landscape suitability
seasons <- c("spring", "summer", "fall", "winter")
for (ssn in seasons) {
  
  message_ts("Prepping layers for season ", ssn)
  
  for (sp in bird_df$FilenameCode) {
    
    message_ts("Prepping layers for species ", sp)
    
    # Load
    ins_file <- file.path(sum_ssn_dir, 
                             paste0("suitability_2013-2022_", 
                                    ssn, "_",
                                    sp, "_", 
                                    "inside.tif"))
    lnd_file <- file.path(sum_ssn_dir, 
                                paste0("suitability_2013-2022_", 
                                       ssn, "_",
                                       sp, "_", 
                                       "landscape-5km.tif"))
    
    ins_rst <- rast(ins_file)
    lnd_rst <- rast(lnd_file)
    
    # Convert to density (per hectare)
    ins_rst <- ins_rst / 36
    lnd_rst <- lnd_rst / 11214.81
    #ins_rst
    #lnd_rst
    
    # Rescale landscape to match range of inside raster
    ins_min <- global(ins_rst, "min", na.rm = TRUE)$min[1]
    ins_max <- global(ins_rst, "max", na.rm = TRUE)$max[1]
    
    lnd_min <- global(lnd_rst, "min", na.rm = TRUE)$min[1]
    lnd_max <- global(lnd_rst, "max", na.rm = TRUE)$max[1]
    
    lnd_rst <- (((lnd_rst - lnd_min) / (lnd_max - lnd_min)) + lnd_min) * ins_max / (1 + lnd_min)
    #lnd_rst
    
    # Rescale both to 0 - 65k and write as INT2U
    # This equalizes species suitability, which is not desired
    # Would need to dived by global species max; file size savings (100kb each) not worth it
    #ins_out_rst <- round(ins_rst * 65000 / ins_max, 0)
    #ins_out_rst
    #lnd_out_rst <- round(lnd_rst * 65000 / ins_max, 0)
    #lnd_out_rst
    
    # Compare
    #hist(ins_rst)
    #hist(ins_out_rst)
    
    #hist(lnd_rst)
    #hist(lnd_out_rst)
    
    # Export
    writeRaster(ins_rst, 
                file.path(zon_in_dir, paste0("suitability_2013-2022_", 
                                             ssn, "_",
                                             sp, "_", 
                                             "inside.tif")),
                #datatype = "INT2U",
                overwrite = TRUE)
    writeRaster(lnd_rst, 
                file.path(zon_in_dir, paste0("suitability_2013-2022_", 
                                             ssn, "_",
                                             sp, "_", 
                                             "landscape.tif")),
                #datatype = "INT2U",
                overwrite = TRUE)
    
    #stop()
    
  }
  
}
  