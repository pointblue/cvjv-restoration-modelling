# Create maps of suitability

# Packages
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load definitions and code
code_dir <- "E:/code/cvjv-restoration-modelling/code"
def_file <- file.path(code_dir, "definitions.R")
code_files <- list.files(file.path(code_dir, "functions"), pattern = ".*R$", full.names = TRUE)
sapply(c(code_files, def_file), FUN = function(x) source(x))

# Local dirs
base_dir <- file.path("E:/code/cvjv-restoration-modelling")
sum_dir <- file.path(base_dir, "analysis/bird_summaries")
sum_ssn_dir <- file.path(sum_dir, "by_season")

zon_dir <- file.path(base_dir, "zonation")
zon_tu_dir <- file.path(zon_dir, "by_season")

anl_dir <- file.path(base_dir, "analysis")
map_dir <- file.path(anl_dir, "maps")
stat_dir <- file.path(anl_dir, "stats")

zon_dir <- file.path(anl_dir, "zonation")
zon_in_dir <- file.path(zon_dir, "inputs")
zon_out_dir <- file.path(zon_dir, "runs")

sa_file <- file.path(base_dir, "data/study_area.tif")
if (!file.exists(sa_file)) {
  message_ts("Creating study area footprint")
  
  lc_rst <- rast(file.path(base_dir, "data/landcover", "cropscape_combined_2014-2021.tif"))
  sa_rst <- (lc_rst + 1) / (lc_rst + 1)
  
  sp_files <- list.files(sum_ssn_dir, pattern = paste0("^suitability.*", sp, ".*tif$"), full.names = TRUE)
  sa_rst <- resample(sa_rst, rast(sp_files[1]))
  writeRaster(sa_rst, sa_file, overwrite = TRUE)
  
}

sa_rst <- rast(sa_file)
names(sa_rst) <- "StudyArea"
sa_df <- as.data.frame(sa_rst, xy = TRUE)
overwrite <- FALSE

zon_folders <- list.files(zon_out_dir, pattern = "CAZ2$", full.names = TRUE)
zon_runs <- basename(zon_folders)

species_groups <- c("shorebirds", "ducks", "all")
for (sg in species_groups) {
  
  message_ts("Working on species group ", sg)
  
  if (sg == "all") {
    sg_lbl <- "All Bird Species"
  } else if (sg == "shorebirds") {
    sg_lbl <- "Shorebird Species"
  } else if (sg == "ducks") {
    sg_lbl <- "Duck Species"
  } else {
    sg_lbl <- sg
  }
  
  sg_folders <- zon_folders[grepl(paste0(sg, ".*combined"), zon_runs)]
  sg_files <- file.path(sg_folders, "rankmap.tif")
  
  # Load and convert to df
  sg_stk <- rast(sg_files)
  names(sg_stk) <- basename(sg_folders)
  
  sg_wide_df <- as.data.frame(sg_stk, xy = TRUE) 
  
  # Turn into data frame and calculate mean suitability
  sg_df <- sg_wide_df |>
    pivot_longer(names(sg_stk),
                 names_to = c("Season", "Species", "SpatialScale", "Mode"),
                 names_sep = "_",
                 values_to = "ZonationRank") |>
    mutate(Season = extract_subelement(strsplit(Season, "-"), 2),
           Species = extract_subelement(strsplit(Species, "-"), 2),
           SpatialScale = extract_subelement(strsplit(SpatialScale, "-"), 2),
           Mode = extract_subelement(strsplit(Mode, "-"), 2),
           ZonationRank = ZonationRank * 100)
  
  # Plot
  sg_yr_map <- ggplot() + 
    geom_tile(data = sa_df, aes(x = x, y = y), fill = "gray") +
    geom_tile(data = sg_df[sg_df$Season == "fullyear", ], aes(x = x, y = y, fill = ZonationRank)) +
    #facet_grid(Season ~ SpatialScale) +
    theme_bw(base_size = 8) +
    #theme(panel.background = element_rect(fill = "#EEEEEE")) +
    ggtitle(paste0("Wetland Restoration Priority for ", sg_lbl, 
                   " in California's Central Valley")) +
    labs(subtitle = "Relative conservation value based on estimated suitabilities from simulated wetland restorations") +
    xlab("") +
    ylab("") +
    scale_fill_viridis_c(name = "Zonation\nRank", option = "D", 
                         breaks = c(0, 25, 50, 75, 99.9), labels = c(0, 25, 50, 75, 100)) + #hack
    #scale_fill_gradient2(low = "#a50026", mid = "#ffffbf", high = "#313695") + #lighter red: #d73027, ligher blue: #4575b4; #Br/bl #8c510a, #f5f5f5, #01665e
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL)
      
  sg_yr_map
  
  # Export
  message_ts("Exporting")
  ggsave(plot = sg_yr_map, 
         filename = file.path(map_dir,
                              paste0("restoration-prioritization_time-annual_species-", sg, 
                                     "_scale-combined.png")),
         width = 5.5, height = 7, units = "in", dpi = 600)
    
}

