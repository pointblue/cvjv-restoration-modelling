# Create maps of suitability

# Packages
library(terra)
library(dplyr)
library(tidyr)
library(ggplot2)

# Load definitions and code
code_dir <- "C:/code/cvjv-restoration-modelling/code"
def_file <- file.path(code_dir, "definitions.R")
code_files <- list.files(file.path(code_dir, "functions"), pattern = ".*R$", full.names = TRUE)
sapply(c(code_files, def_file), FUN = function(x) source(x))

# Local dirs
base_dir <- file.path("C:/code/cvjv-restoration-modelling")
sum_dir <- file.path(base_dir, "analysis/bird_summaries")
sum_ssn_dir <- file.path(sum_dir, "by_season")

zon_dir <- file.path(base_dir, "zonation")
zon_tu_dir <- file.path(zon_dir, "by_season")

lc_rst <- rast(file.path(base_dir, "data/landcover", "cropscape_combined_2014-2021.tif"))
fp_rst <- (lc_rst + 1) / (lc_rst + 1)

overwrite <- FALSE

for (sp in bird_df$FilenameCode) {
  
  sp_lbl <- bird_df$CommonName[bird_df$FilenameCode == sp]
  message_ts("Working on species ", sp_lbl, ": ", sp)
  
  sp_files <- list.files(sum_ssn_dir, pattern = paste0("^suitability.*", sp, ".*tif$"), full.names = TRUE)
  sp_files
  
  # Load and convert to df
  sp_stk <- rast(sp_files)
  names(sp_stk) <- gsub(".tif$", "", basename(sp_files))
  
  sp_df <- as.data.frame(sp_stk, xy = TRUE) |>
    pivot_longer(names(sp_stk),
                 names_to = c("Layer", "Years", "Season", "Species", "SpatialScale"),
                 names_sep = "_",
                 values_to = "SuitabilitySum") |>
    mutate(SuitabilityMean = case_when(SpatialScale == "inside" ~ SuitabilitySum / 360000, 
                                       SpatialScale == "landscape-250m" ~ SuitabilitySum / 1102500, 
                                       SpatialScale == "landscape-5km" ~ SuitabilitySum / 112148100),
           RestorationScore = (SuitabilityMean / max(SuitabilityMean)) * 100) #rescale 0 - 1
  
  sp_map <- ggplot(sp_df, aes(x = x, y = y, fill = RestorationScore)) + geom_tile() +
    facet_grid(Season ~ SpatialScale) +
    theme_bw() +
    #theme(panel.background = element_rect(fill = "#EEEEEE")) +
    ggtitle(paste0("Estimated Restoration Score of ", sp_lbl, 
                   " for Potential Wetland Restorations in California's Central Valley")) +
    labs(subtitle = "Relative mean estimated suitability at three scales based on a simulated restoration simulation") +
    xlab("") +
    ylab("") +
    scale_fill_viridis_c(name = "Relative\nRestoration\nScore", 
                         option = "D") +
    scale_x_continuous(breaks = NULL) +
    scale_y_continuous(breaks = NULL)
      
  sp_map
  
  ggsave(sp_map)
  
  #stop()
    
}


# Plot
diff_files <- list.files(prd_dir, pattern = "ALL.*ShastaDiff_real.tif", full.names = TRUE)
diff_stk <- rast(diff_files)
names(diff_stk) <- substr(basename(diff_files), 12, 14)


#plot(diff_stk)
diff_df <- as.data.frame(diff_stk, xy = TRUE) |>
  pivot_longer(names(diff_stk),
               names_to = "Month",
               values_to = "DroughtEffect") |>
  mutate(Month = factor(Month, levels = month.abb))
diff_map <- ggplot(diff_df, aes(x = x, y = y, fill = DroughtEffect)) + geom_tile() +
  facet_wrap(~ Month, ncol = 3) +
  scale_fill_gradient2(low = "#a50026", mid = "#ffffbf", high = "#313695") + #lighter red: #d73027, ligher blue: #4575b4; #Br/bl #8c510a, #f5f5f5, #01665e
  #scale_fill_viridis_c(name = "Drought\nEffect", option = "H", direction = -1) +
  scale_x_continuous(breaks = NULL) +
  scale_y_continuous(breaks = NULL) + 
  theme_bw() +
  theme(strip.text = element_text(size = 12)) + 
  labs(title = paste0("Effects of Drought on Bird Suitability in the Grasslands Area of California's Central Valley"),
       subtitle = paste0("Ensembled Monthly Average, Shasta Critical vs Normal, 2013 - 2025")) +
  xlab("") +
  ylab("")

diff_map

ggsave(plot = diff_map, 
       filename = file.path(map_dir,
                            paste0("ensembled_suitability_drought_effect.png")),
       width = 4000, height = 5000, units = "px")
