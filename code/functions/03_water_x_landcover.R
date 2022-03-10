# Create water x landcover overlays

# Function to overlay water and landcover files
# Takes water and landcover files as inputs
# Returns a vector of created files
overlay_water_landcover <- function(water_files, landcover_files, output_dir = NULL, overwrite = FALSE) {
  
  # Load required packages
  if (!require(rgdal)) stop(add_ts("Library rgdal is required"))
  if (!require(raster)) stop(add_ts("Library raster is required"))
  
  # Check input files
  if (!all(file.exists(water_files))) stop(add_ts("The following water_files do not exist:\n", 
                                                  paste0(water_files[!file.exists(water_files)], collapse = ", ")))
  if (!all(file.exists(landcover_files))) stop(add_ts("The following landcover_files do not exist:\n", 
                                                  paste0(landcover_files[!file.exists(landcover_files)], collapse = ", ")))
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check other parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  
  # Initialize output
  processed_files <- c()
  
	# Loop across passed water files
	for (wf in water_files) {
		
		wfn <- basename(wf)
		message_ts("Creating landcover overlays for water file ", wfn)
		
		# Check output files
		out_files <- file.path(output_dir, paste0(substr(wfn, 0, nchar(wfn) - 4), "_x_", basename(landcover_files)))
		if (all(file.exists(out_files)) & overwrite != TRUE) {
			
			# Append to output
			processed_files <- c(processed_files, out_files)
			
			message_ts("All water x landcover overlays created for this file. Moving to next...")
			next
			
		}
		
		# Load
		wtr_rst <- raster(wf)
		
		# Loop across passed landcover files
		for (lcf in landcover_files) {
		  
			lcfn <- basename(lcf)
			message_ts("Calculating overlay for ", lcfn, "...")
			
			# Load
			lc_rst <- raster(lcf)
		  
			# Check file existence and whether or not to overwrite
			out_file <- file.path(output_dir, paste0(substr(wfn, 0, nchar(wfn) - 4), "_x_", lcfn))
			if (file.exists(out_file) & overwrite != TRUE) {
				message_ts("File already processed and overwrite not set to TRUE. Moving to next...")
			  next
			}
			 
			# Create overlay
			message_ts("Overlaying water and landcover...")
			message_ts("Output file: ", out_file)
			wxl_rst <- overlay(x = wtr_rst, y = lc_rst, fun = function(x, y) {x * y}, filename = out_file, overwrite = overwrite)
		  message_ts("Complete.")
		  
		  # Append to output
		  processed_files <- c(processed_files, out_file)
		  
		}
		
	}
  
  # Return
  return(processed_files)
  
}
