# Test and evaluate some interpolation methods
#
#

# Load definitions and code
code_dir <- "V:/Project/wetland/FWSPartners/code/cvjv-restoration-modelling/code"
def_file <- file.path(code_dir, "definitions.R")
code_files <- list.files(file.path(code_dir, "functions"), pattern = ".*R$", full.names = TRUE)
sapply(c(def_file, code_files), FUN = function(x) source(x))

# Packages
library(dplyr)
library(terra)
library(gstat)
library(sp)
library(sf)
library(fields)
library(ggplot2)
library(ggpubr)

# Directories
int_dir <- file.path(anl_dir, "interpolation")

# Read data
message_ts("Reading and combining data...")
stat_files <- list.files(cell_stat_dir, pattern = "summary.rds$", full.names = TRUE)
long_raw_df <- do.call(rbind, lapply(stat_files, function(x) readRDS(x)))

# Link xy coordinates
xy_df <- read.csv(file.path(grid_dir, "unique_ids_suitable_xy.csv"))

# Test
dup_rice_uids <- xy_df[duplicated(xy_df$UID) & xy_df$Landcover == "Rice",]$UID
xy_df[xy_df$UID %in% dup_rice_uids,]

# Drop dups
xy_df <- xy_df[!duplicated(xy_df$UID), ]

# Link
long_df <- xy_df %>%
  mutate(UID = as.character(UID)) %>%
  inner_join(long_raw_df, by = c("UID" = "Cell")) %>%
  filter(Plurality == TRUE) %>%
  select(!Count & !Plurality) %>%
  rename("CurrentLandcover" = "Landcover", "LandcoverPercent" = "Percentage")

# Load coarse UID raster
uid_cell_rst <- rast(file.path(grid_dir, "unique_ids_coarse.tif"))
ncols <- ncol(uid_cell_rst)

# Drop points if all 3 or 4 orthogonal points have data
uid_df <- data.frame(UID = unique(long_df$UID))
uids <- as.numeric(uid_df$UID)
uid_df$Include <- sapply(as.numeric(uid_df$UID), FUN = function(x) {
  include <- ifelse(sum(c(x - 1, x + 1, x - ncols, x + ncols) %in% uids) < 3, TRUE, FALSE)
  if (!include) {
    uids <<- uids[uids != x]
  }
  return(include)
})
include_df <- long_df %>%
  left_join(uid_df) %>%
  na.omit() %>%
  mutate(UID = as.numeric(UID))
train_df <- include_df[include_df$Include == TRUE,]
test_df <- include_df[include_df$Include == FALSE, ]

# View
plot(include_df$Easting, include_df$Northing)
plot(train_df$Easting, train_df$Northing)

# Export points
write.csv(include_df, file.path(int_dir, "all_points.csv"), row.names = FALSE)
write.csv(train_df, file.path(int_dir, "train_points.csv"), row.names = FALSE)
write.csv(test_df, file.path(int_dir, "test_points.csv"), row.names = FALSE)

# Create mask raster trimmed to study area
mask_rst <- trim(classify(uid_cell_rst, rcl = data.frame(as.numeric(include_df$UID), rep(1)), othersNA = TRUE))

# Define functions
interpolate_gstat <- function(model, x, crs, ...) {
  v <- st_as_sf(x, coords=c("x", "y"), crs=crs)
  p <- predict(model, v, ...)
  as.data.frame(p)[,1:2]
}

rmse <- function(x, y, collapse = TRUE) {
  rmse_vals <- sqrt((x - y)^2)
  if(collapse) mean(rmse_vals)
}

calc_append_rmse <- function(data_frame, actual, predicted, species, model_loc, method) {
  
  rmse_val <- rmse(actual, predicted)
  rmse_pct <- (rmse_val / mean(actual)) * 100
  mean_bias <- mean(actual) - mean(predicted)
  
  temp_df <- data.frame("Species" = species, "ModelLocation" = model_loc,
                        "InterpolationMethod" = method, "RMSE" = rmse_val, 
                        "RMSE_Percent" = rmse_pct, "MeanBias" = mean_bias)

  rbind(data_frame, temp_df)
  
}

calc_append_values <- function(data_frame, actual, predicted, species, model_loc, method) {
  
  temp_df <- data.frame("Species" = species, "ModelLocation" = model_loc,
                        "InterpolationMethod" = method, 
                        "Actual" = actual, "Predicted" = predicted)
  
  rbind(data_frame, temp_df)
  
}

# Stat df
val_df <- data.frame()
stat_df <- data.frame()

# Loop across species and regions
for (sp in unique(train_df$Species)) {
  
  message_ts("Working on species ", sp)
  
  for (ml in unique(train_df$ModelLocation)) {
    
    message_ts("Working on model_location ", ml)
    
    # Subset data
    spml_df <- include_df %>% 
      filter(Species == sp & ModelLocation == ml)
    
    spml_train_df <- train_df %>% 
      filter(Species == sp & ModelLocation == ml)
    train_spdf <- SpatialPointsDataFrame(spml_train_df[c("Easting", "Northing")], 
                                         spml_train_df, proj4string = CRS(crs(uid_cell_rst, proj = TRUE)))
    
    spml_test_df <- test_df %>% 
      filter(Species == sp & ModelLocation == ml)
    test_spdf <- SpatialPointsDataFrame(spml_test_df[c("Easting", "Northing")], 
                                         spml_test_df, proj4string = CRS(crs(uid_cell_rst, proj = TRUE)))
    
    # Raster of predicted values
    message_ts("Creating raster of actual predicted values...")
    prd_rst <- trim(classify(uid_cell_rst, rcl = data.frame(as.numeric(spml_df$UID), spml_df$CellMean), othersNA = TRUE))
    writeRaster(prd_rst, file.path(int_dir, paste0(sp, "_", ml, "_predicted.tif")), overwrite = TRUE)
    
    # Nearest neighbor
    int_method <- "Nearest"
    nn_mdl <- gstat(formula = CellMean ~ 1, data = train_spdf, nmax = 1, set = list(idp = 0))
    nn_prd <- predict(nn_mdl, test_spdf)
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, nn_prd$var1.pred, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, nn_prd$var1.pred, 
                               species = sp, model_loc = ml, method = int_method)
    
    message_ts("Interpolating...")
    nn_rst <- interpolate(mask_rst, nn_mdl, fun = interpolate_gstat, crs = crs(uid_cell_rst))
    
    message_ts("Masking...")
    nn_mask_rst <- mask(nn_rst[[1]], mask_rst, 
                         filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    # Average of three nearest
    int_method <- "Nearest3"
    near_mdl <- gstat(formula = CellMean ~ 1, data = train_spdf, nmax = 3, set = list(idp = 0))
    near_prd <- predict(near_mdl, test_spdf)
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, near_prd$var1.pred, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, near_prd$var1.pred, 
                               species = sp, model_loc = ml, method = int_method)
    
    message_ts("Interpolating...")
    near_rst <- interpolate(mask_rst, near_mdl, fun = interpolate_gstat, crs = crs(uid_cell_rst))
    
    message_ts("Masking...")
    near_mask_rst <- mask(near_rst[[1]], mask_rst, 
                            filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    # IDW, power of 1
    int_method <- "IDW1"
    idw1_mdl <- gstat(formula = CellMean ~ 1, data = train_spdf, set = list(idp = 1))
    idw1_prd <- predict(idw1_mdl, test_spdf)
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, idw1_prd$var1.pred, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, idw1_prd$var1.pred, 
                                 species = sp, model_loc = ml, method = int_method)
    
    message_ts("Interpolating...")
    idw1_rst <- interpolate(mask_rst, idw1_mdl, fun = interpolate_gstat, crs = crs(uid_cell_rst))
    
    message_ts("Masking...")
    idw1_mask_rst <- mask(idw1_rst[[1]], mask_rst, 
                          filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    # IDW, power of 2
    int_method <- "IDW2"
    idw2_mdl <- gstat(formula = CellMean ~ 1, data = train_spdf, set = list(idp = 2))
    idw2_prd <- predict(idw2_mdl, test_spdf)
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, idw2_prd$var1.pred, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, idw2_prd$var1.pred, 
                                 species = sp, model_loc = ml, method = int_method)
    
    message_ts("Interpolating...")
    idw2_rst <- interpolate(mask_rst, idw2_mdl, fun = interpolate_gstat, crs = crs(uid_cell_rst))
    
    message_ts("Masking...")
    idw2_mask_rst <- mask(idw2_rst[[1]], mask_rst, 
                          filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    # IDW, power of 3
    int_method <- "IDW3"
    idw3_mdl <- gstat(formula = CellMean ~ 1, data = train_spdf, set = list(idp = 3))
    idw3_prd <- predict(idw3_mdl, test_spdf)
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, idw3_prd$var1.pred, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, idw3_prd$var1.pred, 
                                 species = sp, model_loc = ml, method = int_method)
    
    message_ts("Interpolating...")
    idw3_rst <- interpolate(mask_rst, idw3_mdl, fun = interpolate_gstat, crs = crs(uid_cell_rst))
    
    message_ts("Masking...")
    idw3_mask_rst <- mask(idw3_rst[[1]], mask_rst, 
                       filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    # IDW, power of 4
    int_method <- "IDW4"
    idw4_mdl <- gstat(formula = CellMean ~ 1, data = train_spdf, set = list(idp = 4))
    idw4_prd <- predict(idw4_mdl, test_spdf)
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, idw4_prd$var1.pred, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, idw4_prd$var1.pred, 
                                 species = sp, model_loc = ml, method = int_method)
    
    message_ts("Interpolating...")
    idw4_rst <- interpolate(mask_rst, idw4_mdl, fun = interpolate_gstat, crs = crs(uid_cell_rst))
    
    message_ts("Masking...")
    idw4_mask_rst <- mask(idw4_rst[[1]], mask_rst, 
                       filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    # IDW, power of 5
    int_method <- "IDW5"
    idw5_mdl <- gstat(formula = CellMean ~ 1, data = train_spdf, set = list(idp = 5))
    idw5_prd <- predict(idw5_mdl, test_spdf)
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, idw5_prd$var1.pred, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, idw5_prd$var1.pred, 
                                 species = sp, model_loc = ml, method = int_method)
    
    message_ts("Interpolating...")
    idw5_rst <- interpolate(mask_rst, idw5_mdl, fun = interpolate_gstat, crs = crs(uid_cell_rst))
    
    message_ts("Masking...")
    idw5_mask_rst <- mask(idw5_rst[[1]], mask_rst, 
                          filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    # IDW, power of 6
    int_method <- "IDW6"
    idw6_mdl <- gstat(formula = CellMean ~ 1, data = train_spdf, set = list(idp = 6))
    idw6_prd <- predict(idw6_mdl, test_spdf)
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, idw6_prd$var1.pred, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, idw6_prd$var1.pred, 
                                 species = sp, model_loc = ml, method = int_method)
    
    message_ts("Interpolating...")
    idw6_rst <- interpolate(mask_rst, idw6_mdl, fun = interpolate_gstat, crs = crs(uid_cell_rst))
    
    message_ts("Masking...")
    idw6_mask_rst <- mask(idw6_rst[[1]], mask_rst, 
                          filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    
    # Simple Kriging
    # Parameter estimation
    vg <- variogram(gstat(formula = CellMean ~ 1, data = train_spdf))
    #fit.variogram(vg, vgm("Exp"))
    # Estimate nugget (y intercept) as half the lowest value
    yint <- min(vg$gamma) / 2
    
    # Estimate sill begins before gamma first reaches 70% of max
    # Halfway between point with 70% and one prior
    gamma_prp <- vg$gamma / max(vg$gamma)
    sill_dist <- min(vg$dist[gamma_prp > 0.7])
    sill_val <- vg$gamma[vg$dist == sill_dist]
    
    # Exponential
    int_method <- "Krig_Exp"
    krig_exp_vgm <- vgm(psill = sill_val, model = "Exp", range = sill_dist, nugget = yint)
    krig_exp_mdl <- gstat(formula = CellMean ~ 1, data = train_spdf, 
                          model = krig_exp_vgm)
    krig_exp_prd <- predict(krig_exp_mdl, test_spdf)
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, krig_exp_prd$var1.pred, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, krig_exp_prd$var1.pred, 
                                 species = sp, model_loc = ml, method = int_method)
    
    message_ts("Interpolating...")
    krig_exp_rst <- interpolate(mask_rst, krig_exp_mdl, fun = interpolate_gstat, crs = crs(uid_cell_rst))
    
    message_ts("Masking...")
    krig_exp_mask_rst <- mask(krig_exp_rst[[1]], mask_rst, 
                       filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    # Spherical
    int_method <- "Krig_Sph"
    krig_sph_vgm <- vgm(psill = sill_val, model = "Sph", range = sill_dist, nugget = yint)
    krig_sph_mdl <- gstat(formula = CellMean ~ 1, data = train_spdf, 
                          model = krig_sph_vgm)
    krig_sph_prd <- predict(krig_sph_mdl, test_spdf)
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, krig_sph_prd$var1.pred, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, krig_sph_prd$var1.pred, 
                                 species = sp, model_loc = ml, method = int_method)
    
    message_ts("Interpolating...")
    krig_sph_rst <- interpolate(mask_rst, krig_sph_mdl, fun = interpolate_gstat, crs = crs(uid_cell_rst))
    
    message_ts("Masking...")
    krig_sph_mask_rst <- mask(krig_sph_rst[[1]], mask_rst, 
                       filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    # Cokriging did not help at all
    #cokrig_mdl <- gstat(id = "CellMean", formula = CellMean ~ 1, data = train_spdf)
    #cokrig_mdl <- gstat(cokrig_mdl, id = "LandscapeMean", formula = LandscapeMean ~ 1, data = train_spdf)
    
    #plot(variogram(cokrig_mdl))
    
    #cokrig_prd <- predict(cokrig_mdl, test_spdf)
    #calc_append_rmse(stat_df, test_spdf$CellMean, cokrig_prd$CellMean.pred, 
    #                 species = sp, model_loc = ml, method = "CoKrig_Test")
    
    #cokrig_exp_mdl <- gstat(cokrig_mdl, id = c("CellMean"), model = krig_exp_vgm)
    #cokrig_exp_mdl <- gstat(cokrig_exp_mdl, id = c("LandscapeMean"), model = vgm(0.000005, "Exp", 7000, 0.0000001))
    #plot(variogram(cokrig_exp_mdl))
    
    #cokrig_exp_mdl <- gstat(cokrig_exp_mdl, id = c("CellMean", "LandscapeMean"), model = vgm(0.00005, "Exp", 15000, -0.0000001))
    
    #cokrig_exp_prd <- predict(cokrig_exp_mdl, test_spdf)
    
    #stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, cokrig_exp_prd$CellMean.pred, 
    #                            species = sp, model_loc = ml, method = "CoKrig_Exp")
    
    # Gaussian Krig
    int_method <- "Krig_Gauss"
    krig_gauss_mdl <- Krig(spml_train_df[c("Easting", "Northing")], spml_train_df$CellMean)
    krig_gauss_prd <- predict(krig_gauss_mdl, spml_test_df[c("Easting", "Northing")])
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, krig_gauss_prd, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, krig_gauss_prd, 
                                 species = sp, model_loc = ml, method = int_method)
    
    # Problems predicting to surface; not a priority for now
    #message_ts("Interpolating...")
    #krig_gauss_rst <- interpolate(mask_rst, krig_gauss_mdl, crs = crs(uid_cell_rst))
    
    #message_ts("Masking...")
    #krig_gauss_mask_rst <- mask(krig_gauss_rst[[1]], mask_rst, 
    #                   filename = file.path(int_dir, paste0(sp, "_", ml, "_interpolated_", int_method, ".tif")), overwrite = TRUE)
    
    # Thin plate spline
    #Tps(coordinates(aq), aq$OZDLYAV)
    int_method <- "TPS"
    tps_mdl <- Tps(spml_train_df[c("Easting", "Northing")], spml_train_df$CellMean)
    tps_prd <- predict(tps_mdl, spml_test_df[c("Easting", "Northing")])
    stat_df <- calc_append_rmse(stat_df, test_spdf$CellMean, tps_prd, 
                                species = sp, model_loc = ml, method = int_method)
    val_df <- calc_append_values(val_df, test_spdf$CellMean, tps_prd, 
                                 species = sp, model_loc = ml, method = int_method)
    
    
    # Print
    print(stat_df)
    
  }
  
}

# Calculate difference
val_df$Difference <- val_df$Actual - val_df$Predicted
val_df$DifferencePercent <- val_df$Difference / val_df$Actual * 100

# Export
write.csv(val_df, file.path(int_dir, "interpolation_method_values.csv"), row.names = FALSE)
write.csv(stat_df, file.path(int_dir, "interpolation_method_stats.csv"), row.names = FALSE)

# Define factor levels
int_methods <- c("Nearest", "Nearest3",
                 "IDW1", "IDW2", "IDW3", "IDW4", "IDW5", "IDW6", 
                 "Krig_Exp", "Krig_Sph", "Krig_Gauss",
                 "TPS")
val_df$InterpolationMethod <- factor(val_df$InterpolationMethod, levels = int_methods)

# Scatterplot + correlation
max_df <- val_df %>%
  group_by(Species, ModelLocation, InterpolationMethod) %>%
  summarize(ActualMax = max(Actual),
            PredictedMax = max(Predicted))
val_df <- val_df %>%
  left_join(max_df) %>%
  mutate(ActualScaled = Actual / ActualMax,
         PredictedScaled = Predicted / PredictedMax,
         ActualRank = row_number(Actual),
         PredictedRank = row_number(Predicted))

ggplot(val_df, aes(x = ActualScaled, y = PredictedScaled)) + facet_grid(InterpolationMethod~Species+ModelLocation) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() + 
  stat_cor(aes(label = ..rr.label..), color = "red", size = 3, geom = "label", label.x = 0.1, label.y = 1.0) +
  ggtitle("Evaluation of Interpolation Methods by Species and Model Location: Correlation") +
  xlab("Scale Modelled Values (Actual)") + ylab("Scaled Interpolated Values")
ggsave(file.path(int_dir, "interpolation_methods_scatterplot.jpg"))

ggplot(val_df, aes(x = ActualRank, y = PredictedRank)) + facet_grid(InterpolationMethod~Species+ModelLocation) +
  geom_point() + geom_smooth(method = "lm") + theme_bw() + 
  stat_cor(aes(label = ..rr.label..), color = "red", size = 3, geom = "label", label.x = 0.1, label.y = 20000) +
  ggtitle("Evaluation of Interpolation Methods by Species and Model Location: Ranked Correlation") +
  xlab("Scale Modelled Values (Actual)") + ylab("Scaled Interpolated Values")
ggsave(file.path(int_dir, "interpolation_methods_scatterplot_ranked.jpg"))

# Difference
ggplot(val_df, aes(x = InterpolationMethod, y = Difference)) + facet_grid(Species~ModelLocation) + 
  geom_boxplot() + theme_bw() + 
  ggtitle("Evaluation of Interpolation Methods by Species and Model Location: Difference, Actual - Interpolated")
ggsave(file.path(int_dir, "interpolation_methods_boxplot_diff.jpg"))

# Pct Diff
ggplot(val_df, aes(x = InterpolationMethod, y = DifferencePercent)) + facet_grid(Species~ModelLocation) + 
  geom_boxplot() + theme_bw() + 
  ggtitle("Evaluation of Interpolation Methods by Species and Model Location: Percent Difference, Actual vs Interpolated")
ggsave(file.path(int_dir, "interpolation_methods_boxplot_diff_pct.jpg"))

# Rank each
rank_df <- stat_df %>%
  group_by(Species, ModelLocation) %>% 
  mutate("Rank" = row_number(RMSE)) %>%
  arrange(Species, ModelLocation, Rank)
write.csv(rank_df, file.path(int_dir, "interpolation_method_ranks.csv"), row.names = FALSE)

# Calculate method with lowest average error
sum_df <- rank_df %>%
  group_by(InterpolationMethod) %>%
  summarize("RankSum" = sum(Rank),
            "Mean_RMSE" = mean(RMSE), 
            "Mean_RMSE_Percent" = mean(RMSE_Percent),
            "RMSE_RMSE_Percent" = rmse(RMSE_Percent, rep(0))) %>%
  arrange(RankSum)
write.csv(sum_df, file.path(int_dir, "interpolation_method_summary_error.csv"), row.names = FALSE)

# Correlations
cor_df <- val_df %>%
  group_by(Species, ModelLocation, InterpolationMethod) %>%
  summarize("CorrelationAbsolute" = cor(Actual, Predicted),
            "CorrelationRanked" = cor(Actual, Predicted, method = "spearman")) %>%
  group_by(Species, ModelLocation) %>%
  mutate("Rank" = row_number(desc(CorrelationRanked))) %>%
  arrange(Species, ModelLocation, Rank)
write.csv(cor_df, file.path(int_dir, "interpolation_method_correlations.csv"), row.names = FALSE)

cor_sum_df <- cor_df %>%
  group_by(InterpolationMethod) %>%
  summarize("RankSum" = sum(Rank),
            "MeanCorAbs" = mean(CorrelationAbsolute),
            "MeanCorRanked" = mean(CorrelationRanked)) %>%
  arrange(RankSum)
write.csv(cor_sum_df, file.path(int_dir, "interpolation_method_summary_correlations.csv"), row.names = FALSE)

ggplot(cor_df, aes(x = InterpolationMethod, y = CorrelationRanked)) + geom_boxplot() + theme_bw() +
  ggtitle("Ranked Correlation across Species and Models, Actual vs Interpolated")
ggsave(file.path(int_dir, "interpolation_methods_boxplot_correlation.jpg"))
