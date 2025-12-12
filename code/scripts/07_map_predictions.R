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

cor_df <- data.frame()
for (sp in bird_df$FilenameCode) {
  
  sp_lbl <- bird_df$CommonName[bird_df$FilenameCode == sp]
  message_ts("Working on species ", sp_lbl, ": ", sp)
  
  sp_files <- list.files(sum_ssn_dir, pattern = paste0("^suitability.*", sp, ".*tif$"), full.names = TRUE)
  sp_files
  
  # Load and convert to df
  sp_stk <- rast(sp_files)
  names(sp_stk) <- gsub(".tif$", "", basename(sp_files))
  
  sp_wide_df <- as.data.frame(sp_stk, xy = TRUE) 
  
  # Turn into data frame and calculate mean suitability
  sp_df <- sp_wide_df |>
    pivot_longer(names(sp_stk),
                 names_to = c("Layer", "Years", "Season", "Species", "SpatialScale"),
                 names_sep = "_",
                 values_to = "SuitabilitySum") |>
    mutate(SuitabilityMean = case_when(SpatialScale == "inside" ~ SuitabilitySum / 360000, 
                                       SpatialScale == "landscape-250m" ~ SuitabilitySum / 1102500, 
                                       SpatialScale == "landscape-5km" ~ SuitabilitySum / 112148100),
           SuitabilityMean = SuitabilityMean * 10000) |> #rescale to be per hectare rather than sqm
    group_by(SpatialScale) |>
    mutate(SuitabilityMin_SpeciesSpatialScale = min(SuitabilityMean),
           SuitabilityMax_SpeciesSpatialScale = max(SuitabilityMean),
           RestorationScore = (SuitabilityMean - SuitabilityMin_SpeciesSpatialScale) / 
             (SuitabilityMax_SpeciesSpatialScale - SuitabilityMin_SpeciesSpatialScale) * 100) |> #rescale 0 - 1
    ungroup()
  
  # Get spatial scale correlations
  sp_wide_scale_df <- sp_df |>
    group_by(x, y, SpatialScale) |>
    summarize(SuitabilityMean = mean(SuitabilityMean)) |>
    pivot_wider(names_from = SpatialScale,
                values_from = SuitabilityMean) |>
    ungroup() |>
    select(!c(x, y))
  sp_cor_df <- data.frame(Species = sp,
                          as.data.frame(cor(sp_wide_scale_df)))
  cor_df <- rbind(cor_df, sp_cor_df)
  
  #cor(sp_df$SuitabilityMean)
  
  #sp_df <- sp_df[sp_df$SpatialScale == "inside" & sp_df$Season == "fall",]
  
  # Plot
  sp_map <- ggplot() + 
    geom_tile(data = sa_df, aes(x = x, y = y), fill = "gray") +
    geom_tile(data = sp_df, aes(x = x, y = y, fill = SuitabilityMean)) +
    facet_grid(Season ~ SpatialScale) +
    theme_bw(base_size = 14) +
    #theme(panel.background = element_rect(fill = "#EEEEEE")) +
    #theme(strip.text = element_text(size = 12)) + 
    #ggtitle(paste0("Estimated Suitability for ", sp_lbl, 
    #               " at Potential Wetland Restoration Sites in California's Central Valley")) +
    #labs(subtitle = "Relative mean estimated suitability per hectare at three scales based on simulated wetland restorations") +
    xlab("") +
    ylab("") +
    scale_fill_viridis_c(name = "Mean\nSuitability\nScore", 
                         option = "D") +
    #scale_fill_gradient2(low = "#a50026", mid = "#ffffbf", high = "#313695") + #lighter red: #d73027, ligher blue: #4575b4; #Br/bl #8c510a, #f5f5f5, #01665e
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL)
      
  #sp_map
  
  # Export
  message_ts("Exporting")
  ggsave(plot = sp_map, 
         filename = file.path(map_dir,
                              paste0("restoration_suitability_", sp, ".png")),
         #width = 4000, height = 5000, units = "px")
         width = 8, height = 10, units = "in", dpi = 400)
    
}

write.csv(cor_df, file.path(stat_dir, "correlations_spatial_scale.csv"), row.names = FALSE)
cor_df
cor_inside_250m <- mean(cor_df$inside[seq(from = 2, by = 3, length.out = 7)])
message_ts("Mean correlation between inside and 250m: ", cor_inside_250m)

cor_inside_5k <- mean(cor_df$inside[seq(from = 3, by = 3, length.out = 7)])
message_ts("Mean correlation between inside and 5k: ", cor_inside_5k)
