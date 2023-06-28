# Moving window analysis

# lt_fcl_dir <- "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/code/water_tracker/data/longterm_averages/water_focal"
# Function to calculate moving window sums of water
# Takes water (by landcover) files and distances as input
# Distance is in meters
# Returns a vector of created files
overlay_imposed_on_neighborhood <- function(imposed_files, focal_files, 
                                           output_dir, focal_filter = "valley_2013-2022",
                                           overwrite = FALSE, 
                                           verbose = TRUE) {
  
  if (verbose) message_ts("Entering overlay_imposed_on_neighborhood()")
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Check input files
  if (!all(file.exists(imposed_files))) stop(add_ts("The following imposed_files do not exist:\n", 
                                                  paste0(imposed_files[!file.exists(imposed_files)], collapse = ", ")))
  if (!all(file.exists(focal_files))) stop(add_ts("The following focal_files do not exist:\n", 
                                                  paste0(focal_files[!file.exists(focal_files)], collapse = ", ")))
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check other parameters
  #if (!is.function(overlay_function)) stop(add_ts("Argument 'overlay_function' must be a function taking ", #too complicated!
  #                                                "an imposed water raster as arg 1 and focal water raster as argument 2"))
  #if (!is.logical(trim_extent)) stop(add_ts("Argument 'trim_extent' must be TRUE or FALSE"))
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  
  # Initialize output
  processed_files <- c()
  
  # Subset to 5000m imposed files
  imp_5k_files <- imposed_files[grepl("5000m", imposed_files)]
  
	# Loop across 5k imposed files
	for (f in imp_5k_files) {
		
		fn <- basename(f)
		if (verbose) message_ts("Working on imposed water file ", fn)
		
		# Check for matching 250m file
		f250 <- gsub("5000m", "250m", f)
		if (!file.exists(f250)) {
		  file.create(gsub(".tif$", ".missing", f250))
		  stop(add_ts("Missing file: ", f250))
		}
		
		# Match focal_files
		fn_split <- strsplit(fn, "_")
		uid <- fn_split[[1]][2]
		mth <- substr(fn_split[[1]][5], 7, 9)
		matching_focal_files <- focal_files[grepl(paste0(focal_filter, "_", mth), focal_files)]
		if (length(matching_focal_files) == 0) {
		  #stop(add_ts("No focal files for month ", mth, " found."))
		  message_ts("No focal files for month ", mth, " found.")
		  next
		}
	
		# Parse matching_focal_files
		distances <- ifelse(grepl("5000m", matching_focal_files), "5000m", "250m")
		ffn_splits <- strsplit(basename(matching_focal_files), "_")
		yrs <- extract_subelement(ffn_splits, 2)
		lcs <- extract_subelement(ffn_splits, 5)
		lcs <- ifelse(lcs == "Wetland", "WetlandSemiSeas", lcs)
		
		# Build out_files and check existence
		out_files <- file.path(output_dir, paste0("cell-", uid, "_", 
		                                          "buffered-", distances, "_",
		                                          "filled-semiperm_",
		                                          "month-", mth, "_",
		                                          "year-", yrs, "_", 
		                                          "landcover-", lcs, ".tif"))
		if (all(file.exists(out_files)) & overwrite != TRUE) {
			
			# Append to output
			processed_files <- c(processed_files, out_files)
			
			if (verbose) message_ts("All moving windows calculated for this file. Moving to next...")
			next
			
		}
		if (length(out_files) != length(matching_focal_files)) stop(add_ts("Length mismatch between fcl and out files"))
		
		# Load
		imp_5000m_rst <- rast(f)
		imp_250m_rst <- rast(f250)
		
		# Extend 250m raster
		imp_250m_ext_rst <- extend(imp_250m_rst, imp_5000m_rst)
		
		# Loop across focal files
		for (n in 1:length(out_files)) {
		  
		  out_file <- out_files[n]
		  lc <- lcs[n]
		  d <- distances[n]
		  
			# Check file existence
			if (file.exists(out_file) & overwrite != TRUE) {
			  
				if (verbose) message_ts(basename(out_file), " already processed and overwrite not set to TRUE. Moving to next...")
			  next
			 
			}
		  
		  # Choose imposed file
		  if (d == "250m") {
		    imp_rst <- imp_250m_ext_rst
		  } else if (d == "5000m") {
		    imp_rst <- imp_5000m_rst
		  } else {
		    stop(add_ts("Unrecognized distance: ", d))
		  }
		  
		  # Load focal file
		  ff <- matching_focal_files[n]
		  ffn <- basename(ff)
		  fcl_rst <- rast(ff)
		  
			# Crop to area of imposed raster
			if (verbose) message_ts("Cropping focal file ", ffn, " to match imposed file...")
			fcl_crp_rst <- crop(fcl_rst, imp_rst)
			
			# Impose water
			if (verbose) message_ts("Applying imposed water...")
			if (lc == "WetlandSemiSeas") {
			  if (verbose) message_ts("Wetland fxn...")
			  fcl_imp_rst <- app(c(fcl_crp_rst, imp_rst), fun = sum, na.rm = TRUE) #unaffected areas of imp_rst are NAs, so need to use app and remove them
			} else {
			  if (verbose) message_ts("Other cover fxn...")
			  fcl_imp_rst <- fcl_crp_rst
			}
			
			# Export
			if (verbose) message_ts("Exporting...")
			writeRaster(fcl_imp_rst, out_file, overwrite = TRUE)
		  
			if (verbose) message_ts("Complete.")
			
			# Append to output
			processed_files <- c(processed_files, out_file)
			
		}
		
	}
		
	# Return
	return(processed_files)
		
}



# TESTING #
# Run start of setup file
# uids <- uids_split[[1]]
# #uids_ptn <- paste0("cell_(", paste0(uids, collapse = "|"), ").*tif$")
#cell_100366_buffered-250m_filled-semiperm_month-Apr
imp_files <- file.path(imp_dir, paste0("cell_", rep(uids, each = 24), "_",
                                       "buffered-", rep(c(250, 5000), each = 12, times = 2), "m_",
                                       "filled-semiperm_",
                                       "month-", rep(month.abb, times = 2),
                                       ".tif"))
imp_files <- imp_files[grepl("-Mar.tif", imp_files)]
stopifnot(all(file.exists(imp_files)))

imposed_files <- imp_files
focal_files <- lt_fcl_files[grepl("Wetland_SemiSeas.*250m", lt_fcl_files)]
output_dir <- fcl_dir
focal_filter <- "valley_2013-2022"
overwrite <- FALSE
verbose <- TRUE
fcl_imp_files <- overlay_imposed_on_neighborhood(imp_files, focal_files, output_dir,
                                                 focal_filter = "valley_2013-2022",
                                                 overwrite = overwrite, verbose = verbose)
