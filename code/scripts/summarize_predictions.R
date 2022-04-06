# Code to combine all bird prediction stats into a single file and do preliminary analysis
#

# TODO: add parameters for joining data
summarize_predictions <- function(stat_files, metadata_csv_file, output_dir, overwrite = FALSE) {
  
  stat_files <- list.files(imp_stat_dir, pattern = ".*rds$", full.names = TRUE)
  metadata_csv_file <- file.path(fld_dir, "SDW Spring Delta 2022 Bid Details 2022-03-01.csv")
  output_dir <- imp_stat_dir
  
  # Load required packages
  if (!require(rgdal)) stop(add_ts("Library rgdal is required"))
  if (!require(raster)) stop(add_ts("Library raster is required"))
  if (!require(dplyr)) stop(add_ts("Library dplyr is required"))
  if (!require(tidyr)) stop(add_ts("Library tidyr is required"))
  
  # Check input files
  if (!all(file.exists(stat_files))) stop(add_ts("The following stat_files do not exist:\n", 
                                                       paste0(stat_files[!file.exists(stat_files)], collapse = ", ")))
  
  if (!(file.exists(metadata_csv_file))) stop(add_ts("metadata_csv_file does not exist"))
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check other parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  
  # Initialize output
  processed_files <- c()
  
  # Read data
  message_ts("Reading and combining data...")
  long_df <- do.call(rbind, lapply(stat_files, function(x) readRDS(x)))
  
  # Read metadata
  md_df <- read.csv(metadata_csv_file, stringsAsFactors = FALSE)
  
  # Drop duplicate periods
  names(md_df) <- gsub("\\.\\.", "\\.", names(md_df))
  
  # Join to metadata
  md_df$FieldNameFixed <- clean_string_remove_underscores(md_df$Fild_ID)
  joined_df <- left_join(long_df, md_df, by = c("FloodingArea" = "Flooding.Area.Flooding.Area.ID")) #add as arguments
  
  # Fill in blank start/end dates #work on
  joined_df$Flooding.Start.Date <- mapply(bid = joined_df$Bid.Bid.Name, fsd = joined_df$Flooding.Start.Date, 
         FUN = function(bid, fsd) {
           if(is.na(fsd) | fsd == "") {
              new_dt <- md_df$Flooding.Start.Date[md_df$Bid.Bid.Name == bid]
              new_dt[!is.na(new_dt) & new_dt != ""][1]
            } else {
              fsd
            }
          })
  joined_df$Flooding.End.Date <- mapply(bid = joined_df$Bid.Bid.Name, fed = joined_df$Flooding.End.Date, 
                                          FUN = function(bid, fed) {
                                            if(is.na(fed) | fed == "") {
                                              new_dt <- md_df$Flooding.End.Date[md_df$Bid.Bid.Name == bid]
                                              new_dt[!is.na(new_dt) & new_dt != ""][1]
                                            } else {
                                              fed
                                            }
                                          })
  
  # Overlap
  overlap_df <- joined_df %>%
    mutate(PredictionDateStart = as.Date(paste0("2022", "-", match(PredictionMonth, month.abb), "-1")), #need to fix year somehow
           PredictionDateEnd = as.Date(paste0("2022", "-", match(PredictionMonth, month.abb), "-", ifelse(PredictionMonth == "Feb", "28", 
                                                                              ifelse(PredictionMonth %in% c("Apr", "Jun", "Sep", "Nov"), "30", "31")))),
           FloodDateStart = as.Date(Flooding.Start.Date, format = "%m/%d/%Y"), #column name changes
           #FloodDateEnd = FloodDateStart + wks_fld * 7) %>%
           FloodDateEnd = as.Date(Flooding.End.Date, format = "%m/%d/%Y")) %>%
    mutate(OverlapMin = pmax(as.Date(FloodDateStart), PredictionDateStart), OverlapMax = pmin(as.Date(FloodDateEnd), PredictionDateEnd),
           DaysOverlap = pmax(0, difftime(OverlapMax, OverlapMin, unit = "days")))
  
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
  
  # Return
  return(processed_files)
  
}

