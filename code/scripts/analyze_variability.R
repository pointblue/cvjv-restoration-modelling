# Code to combine all bird prediction stats into a single file and do preliminary analysis
#

# Packages
library(dplyr)
library(ggplot2)

# Read data
message_ts("Reading and combining data...")
message_ts("Reading and combining data...")
stat_files <- list.files(cell_stat_dir, pattern = "summary.rds$", full.names = TRUE)
long_df <- do.call(rbind, lapply(stat_files, function(x) readRDS(x)))

# Export
saveRDS(long_df, file.path(stat_dir, "cell_summary_long.rds"))
write.csv(long_df, file.path(stat_dir, "cell_summary_long.csv"), row.names = FALSE)

# Mean sd as percent of mean by species and model
long_df <- mutate(long_df, CellStdDevPct = CellStdDev / CellMean * 100)

ggplot(long_df, aes(y = CellStdDevPct, x = Species, color = ModelLocation)) + geom_boxplot() +
  theme_bw() + ggtitle("Variability within Cells: Standard Deviation as a Percent of Cell Mean by Species and Model Location") +
  ylab("Standard Deviation as a Percent of Cell Mean")
ggsave(file.path(plot_dir, "variability_within_cells_boxplot.jpg"))

# Table
within_df <- long_df %>%
  group_by(PredictionMonth, Species, Model, ModelLocation) %>%
  summarize(MeanStdDevPct = mean(CellStdDevPct))
write.csv(within_df, file.path(stat_dir, "variability_within_cells_summary.csv"), row.names = FALSE)

# Across cells
ggplot(long_df, aes(y = CellMean, x = Species, color = ModelLocation)) + geom_boxplot() +
  theme_bw() + ggtitle("Variability Across Cells: Mean Cell Values by Species and Model Location") +
  ylab("Mean Cell Value")
ggsave(file.path(plot_dir, "variability_across_cells_boxplot.jpg"))

ggplot(long_df, aes(y = LandscapeMean, x = Species, color = ModelLocation)) + geom_boxplot() +
  theme_bw() + ggtitle("Variability Across Cells: Mean Landscape Values by Species and Model Location") +
  ylab("Mean Landscape Value")
ggsave(file.path(plot_dir, "variability_across_cells_landscape_boxplot.jpg"))

# Table
across_df <- long_df %>%
  group_by(PredictionMonth, Species, Model, ModelLocation) %>%
  summarize(CellStdDev = sd(CellMean),
            LandscapeStdDev = sd(LandscapeMean))
write.csv(across_df, file.path(stat_dir, "variability_across_cells_summary.csv"), row.names = FALSE)


# Read metadata
md_df <- read.csv(metadata_csv_file, stringsAsFactors = FALSE)

# Drop duplicate periods
names(md_df) <- gsub("\\.\\.", "\\.", names(md_df))

# Export
long_file <- file.path(output_dir, "00_prediction_summary_long.csv")
if (file.exists(long_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")  
} else {
  write.csv(overlap_df, long_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, long_file)
}

# Calculate ensembles
message_ts("Calculating ensembles...")
ens_df <- overlap_df %>%
  rename(Bid = Bid.Bid.Name) %>% #Bid_ID
  group_by(Bid, FloodingArea, FieldName, FieldAreaAcres, PredictionMonth, FloodDateStart, FloodDateEnd, DaysOverlap, Species, Model) %>%
  mutate(EnsembleWeights = ifelse(ModelLocation == "S", 1, 2)) %>%
  summarize(SuitabilityMean = weighted.mean(PredictionMean, EnsembleWeights), 
            SuitabilitySum = sum(PredictionSum * EnsembleWeights / 3), 
            LandscapeMean = weighted.mean(PredictionMean_Landscape, EnsembleWeights),
            LandscapeSum = sum(PredictionSum_Landscape * EnsembleWeights / 3))

# Export
ens_file <- file.path(output_dir, "01_prediction_summary_ensembled.csv")
if (file.exists(ens_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")  
} else {
  write.csv(ens_df, ens_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, ens_file)
}

# Summarize by flooding area
message_ts("Summarizing by flooding area...")
fa_df <- ens_df %>%
  ungroup() %>%
  group_by(Bid, FloodingArea, PredictionMonth, FloodDateStart, FloodDateEnd, DaysOverlap, Species, Model) %>%
  summarise(FloodingAreaAcres = sum(FieldAreaAcres), SuitabilityMean = weighted.mean(SuitabilityMean, FieldAreaAcres),
            SuitabilitySum = sum(SuitabilitySum), 
            LandscapeMean = weighted.mean(LandscapeMean, FieldAreaAcres))

# Export
fa_file <- file.path(output_dir, "02_prediction_summary_flooding_area.csv")
if (file.exists(fa_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")  
} else {
  write.csv(fa_df, fa_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, fa_file)
}

# Drop months with no overlap
message_ts("Dropping months with no overlap...")
mth_df <- fa_df %>%
  filter(DaysOverlap > 0)

# Export
fa_mth_file <- file.path(output_dir, "03_prediction_summary_flooding_area_month.csv")
if (file.exists(fa_mth_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")  
} else {
  write.csv(mth_df, fa_mth_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, fa_file)
}

# Combine by month
fa_cmb_df <- fa_df %>%
  filter(DaysOverlap > 0) %>%
  group_by(Bid, FloodingArea, FloodingAreaAcres, FloodDateStart, FloodDateEnd, Species) %>%
  summarise(PredictionYear = "Combined", PredictionMonth = "Combined", BidLength = sum(DaysOverlap),
            SuitMean = weighted.mean(SuitabilityMean, DaysOverlap), 
            SuitSum = sum(SuitabilityMean * DaysOverlap * FloodingAreaAcres), LandscapeMean = weighted.mean(LandscapeMean, DaysOverlap))

# Export
fa_cmb_file <- file.path(output_dir, "04_prediction_summary_flooding_area_across_months.csv")
if (file.exists(fa_cmb_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")  
} else {
  write.csv(fa_cmb_df, fa_cmb_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, fa_cmb_file)
}

# Calculate totals across species
fa_cmb_wide_df <- fa_cmb_df %>%
  select(-PredictionYear, -PredictionMonth, -LandscapeMean) %>%
  pivot_wider(names_from = Species, values_from = c(SuitMean, SuitSum)) %>%
  mutate(SuitMean_Total = mean(c(SuitMean_AMAV, SuitMean_BNST, SuitMean_DOWI, SuitMean_DUNL)),
         SuitSum_Total = sum(c(SuitSum_AMAV, SuitSum_BNST, SuitSum_DOWI, SuitSum_DUNL)))

# Export
fa_cmb_wide_file <- file.path(output_dir, "05_prediction_summary_flooding_area_totals_wide.csv")
if (file.exists(fa_cmb_wide_file) & overwrite != TRUE) {
  message_ts("File already exists and overwrite != TRUE. Moving to next...")  
} else {
  write.csv(fa_cmb_wide_df, fa_cmb_wide_file, row.names = FALSE)
  message_ts("Exported.")
  processed_files <- c(processed_files, fa_cmb_wide_file)
}

