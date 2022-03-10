# Extract predicted values for a set of flooding areas and prediction files
#
 
# Returns character vector of created files
extract_predictions <- function(prediction_files, floodarea_shapefiles, field_column, area_column, 
                                output_dir, n_predictions = NULL, overwrite = FALSE) {
  
  # Load required packages
  if (!require(rgdal)) stop(add_ts("Library rgdal is required"))
  if (!require(raster)) stop(add_ts("Library raster is required"))
  
  # Check input files
  if (!all(file.exists(prediction_files))) stop(add_ts("The following prediction_files do not exist:\n", 
                                                  paste0(prediction_files[!file.exists(prediction_files)], collapse = ", ")))
  if (!all(file.exists(floodarea_shapefiles))) stop(add_ts("The following floodarea_shapefiles do not exist:\n", 
                                                       paste0(floodarea_shapefiles[!file.exists(floodarea_shapefiles)], collapse = ", ")))
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check other parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  
  # Initialize output
  processed_files <- c()
  
	# Loop across floodarea_shapefiles
	for (fas in floodarea_shapefiles) {

		message_ts("Working on shapefile ", fas)
		fa <- substr(basename(fas), 0, nchar(basename(fas)) - 4)
		
		# Check if flooding area shapefile exists and load
		fa_shp <- readOGR(dirname(fas), fa)
		
		# Check column names
		if (!(field_column) %in% names(fa_shp@data)) stop(add_ts("field_column ", field_column, " does not exist in fa_shp ", fa, "."))
		if (!(area_column) %in% names(fa_shp@data)) stop(add_ts("area_column ", area_column, " does not exist in fa_shp ", fa, "."))
		
		# Subset to matching flood areas
		prd_files <- prediction_files[grepl(fa, prediction_files)]
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
		
		# Loop across fields
		fields <- unique(fa_shp@data[[field_column]])
		for (fld in fields) {
		
			message_ts("Working on field ", fld)
			fld_shp <- fa_shp[fa_shp[[field_column]] == fld, ]
			
			fld_cln <- clean_string_remove_underscores(fld)
			
			# Check if already processed
			clean_name <- paste0(clean_string_remove_underscores(fa), "_", fld_cln, "_summary.rds")
			prd_data_file <- file.path(output_dir, clean_name)
			if (file.exists(prd_data_file)) {
				
				message_ts("Data for flooding area ", fa, " and field ", fld, " already calculated. Moving to next...")
				next
				
			} 
      
			# Build data frame for extracted values
			message_ts("Building data frame...")
			fn_split <- strsplit(basename(prd_files), "_")
			prd_df <- data.frame("PredictionFilename" = basename(prd_files),
			                     "FloodingArea" = extract_subelement(fn_split, 1),
          								 "FieldName" = rep(fld_cln),
          								 "FieldAreaAcres" = rep(sum(fld_shp[[area_column]])), #need to check data type of area_column
          								 #"PredictionYear" = extract_subelement(fn_split, 2), #need to add
          								 "PredictionMonth" = extract_subelement(fn_split, 2),
          								 "Species" = extract_subelement(fn_split, 4),
          								 "Model" = substr(extract_subelement(fn_split, 6), 0, nchar(extract_subelement(fn_split, 6)) - 4),
          								 "ModelLocation" = extract_subelement(fn_split, 5),
          								 "PredictionMean" = rep(NA),
          								 "PredictionSum" = rep(NA),
          								 "PredictionMean_Landscape" = rep(NA))
								 
			# Load prediction rasters
			message_ts("Loading prediction stack...")
			prd_stk <- stack(prd_files)
			
			# Flooding area mean
			message_ts("Extracting mean flooding area suitability...")
			prd_df$PredictionMean <- as.numeric(raster::extract(prd_stk, fld_shp, fun = mean, na.rm = TRUE))
			
			# Flooding area sum
			message_ts("Extracting total flooding area suitability...")
			prd_df$PredictionSum <- as.numeric(raster::extract(prd_stk, fld_shp, fun = sum, na.rm = TRUE))
			
			# Landscape mean
			# Values need to be NA outside area of interest
			message_ts("Calculating mean landscape suitability...")
			prd_df$PredictionMean_Landscape <- cellStats(prd_stk, "mean", na.rm = TRUE)
			
			# Landscape sum
			# Values need to be NA outside area of interest
			message_ts("Calculating total landscape suitability...")
			prd_df$PredictionSum_Landscape <- cellStats(prd_stk, "mean", na.rm = TRUE)
			
			# Export
			saveRDS(prd_df, prd_data_file)
			message_ts("Data exported.")
			
			# Append to output
			processed_files <- c(processed_files, prd_data_file)
			
		}
		
	}

  # Return
  return(processed_files)
  
}
