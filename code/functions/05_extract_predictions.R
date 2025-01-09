# Extract predicted values for a set of flooding areas and prediction files
#
 
# Returns character vector of created files
extract_predictions <- function(prediction_files, area_files,
                                output_dir, 
                                n_predictions = NULL, overwrite = FALSE) {
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Check input files
  if (!all(file.exists(prediction_files))) stop(add_ts("The following prediction_files do not exist:\n", 
                                                  paste0(prediction_files[!file.exists(prediction_files)], collapse = ", ")))
  if (!all(file.exists(area_files))) stop(add_ts("The following area_files do not exist:\n", 
                                                       paste0(area_files[!file.exists(area_files)], collapse = ", ")))
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check other parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  
  # Initialize output
  processed_files <- c()
  
	# Loop across area_files
	for (af in area_files) {
	  
    afn <- basename(af)
		message_ts("Working on area ", afn)
		
	  # Pull out cell
	  cell <- paste0("cell_", extract_subelement(strsplit(afn, "_"), 2))
	  
		# Subset to matching flood areas
		prd_files <- prediction_files[grepl(cell, prediction_files)]
		n_files <- length(prd_files)
		message_ts(length(prd_files), " matching predictions found.")
		if (n_files == 0) {
			
			message_ts("No matches found; moving to next.")
			next
			
		} else if (!is.null(n_predictions)) {
			
			if (n_files != n_predictions) { 
			
				message_ts("Incorrect number of matches. Found ", n_files, "; expecting ", n_predictions, ". Skipping...")
				next
				
			}
			
		}
		
		# Check if already processed
		clean_name <- paste0(clean_string(cell), "_summary.rds")
		stat_file <- file.path(output_dir, clean_name)
		if (file.exists(stat_file)) {
			
			message_ts("Data for area ", af, " already calculated. Moving to next...")
			next
			
		} 
		
		# Load
		area_rst <- rast(af)
		
		# Build data frame for extracted values
		message_ts("Building data frame...")
		fn_split <- strsplit(basename(prd_files), "_")
		prd_df <- data.frame("PredictionFilename" = basename(prd_files),
		                     "CellFile" = afn,
		                     "Cell" = extract_subelement(fn_split, 2),
        								 "PredictionYear" = extract_subelement(fn_split, 4), 
        								 "PredictionMonth" = extract_subelement(fn_split, 3),
        								 "Species" = extract_subelement(fn_split, 5),
        								 "Model" = substr(extract_subelement(fn_split, 7), 0, nchar(extract_subelement(fn_split, 7)) - 4),
        								 "ModelLocation" = extract_subelement(fn_split, 6),
        								 "CellMean" = rep(NA),
        								 "CellStdDev" = rep(NA),
        								 "LandscapeMean" = rep(NA))
							 
		# Load prediction rasters
		message_ts("Loading prediction stack...")
		prd_stk <- rast(prd_files)
		
		# Crop if needed
		if (ext(prd_stk) != ext(area_rst)) {
		  message_ts("Cropping prediction stack to area...")
		  prd_stk <- crop(prd_stk, area_rst)
		}
		
		# Check extent
		if (ext(prd_stk) != ext(area_rst)) {
		  stop(add_ts("Incorrect extents."))
		}
		
		# Get mean values for cell (1) and landscape (0)
		message_ts("Extracting mean area suitability...")
		zonal_df <- zonal(prd_stk, area_rst, fun = mean, na.rm = TRUE)
		prd_df$CellMean <- as.numeric(zonal_df[zonal_df$lyr1 == 1, 2:ncol(zonal_df)])
		prd_df$LandscapeMean <- as.numeric(zonal_df[zonal_df$lyr1 == 0, 2:ncol(zonal_df)])
		
		message_ts("Calculating standard deviation...")
		zonal_sd_df <- zonal(prd_stk, area_rst, fun = function(x) {sd(x, na.rm = TRUE)})
		prd_df$CellStdDev <- as.numeric(zonal_sd_df[zonal_sd_df$lyr1 == 1, 2:ncol(zonal_sd_df)])
		
		# Export
		saveRDS(prd_df, stat_file)
		message_ts("Data exported.")
		
		# Append to output
		processed_files <- c(processed_files, stat_file)
		
	}

  # Return
  return(processed_files)
  
}

