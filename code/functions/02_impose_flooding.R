# Impose flooding on fields

# Define function to impose flooding
# Takes water and field files as inputs
# Returns a vector of created files
impose_flooding <- function(water_files, field_files, output_dir, imposed_value = 1, mask = FALSE, overwrite = FALSE) {
	
  # Load required packages
  if (!require(sp)) stop(add_ts("Library sp is required"))
  if (!require(rgdal)) stop(add_ts("Library rgdal is required"))
  if (!require(raster)) stop(add_ts("Library raster is required"))
  
  # Check input files
  if (!all(file.exists(water_files))) stop(add_ts("The following water_files do not exist:\n", 
                                                  paste0(water_files[!file.exists(water_files)], collapse = ", ")))
  if (!all(file.exists(field_files))) stop(add_ts("The following field_files do not exist:\n", 
                                                  paste0(field_files[!file.exists(field_files)], collapse = ", ")))
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check other parameters
  if (!is.logical(mask)) stop(add_ts("Argument 'mask' must be TRUE or FALSE"))
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!is.numeric(imposed_value) & !is.null(imposed_value)) stop(add_ts("Argument 'imposed_value' must be numeric or null"))

  # Initialize output
	processed_files <- c()

	# Loop across passed water files
	for (wf in (water_files)) {
		
		wfn <- basename(wf)
		message_ts("Imposing flooding on water file ", wfn)
		
		# Load
		wtr_rst <- raster(wf)
		
		# Loop across passed flood files
		for (ff in field_files) {
		  
			ffn <- basename(ff)
			message_ts("Using field file ", ffn, "...")
			
			# Build export filename and check if has been processed
			out_fn_base <- paste(extract_subelement(strsplit(ffn, "\\."), 1), extract_subelement(strsplit(wfn, "\\."), 1), sep = "_")
			out_file <- file.path(output_dir, paste0(out_fn_base, "_imposed.tif"))
			if (file.exists(out_file) & overwrite != TRUE) {
				
				# Append to output
				processed_files <- c(processed_files, out_file)
				
				message_ts("Flooding already imposed. Moving to next...")
				next
			} 
			
			# Load flooding area raster
			fld_rst <- raster(ff)
			
			# Mask if requested (speeds up subsequent processing)
			wtr_msk_rst <- wtr_rst
			if (mask == TRUE) {
			  
				message_ts("Masking to non-NA areas of field raster...")
				values(wtr_msk_rst)[is.na(values(fld_rst))] <- NA
				
			} 
			
			# Find extent of field
			is_field <- !is.na(values(fld_rst)) & values(fld_rst) == 1
			
			# Copy values
			imp_rst <- wtr_msk_rst
			
			# Impose flooding
			if (is.null(imposed_value)) {
			  
			  message_ts("Using existing flood value value...")
			  
			} else {
			  
			  message_ts("Imposing constant flood value...")
			  values(imp_rst)[is_field] <- imposed_value
			  
			}

			message_ts("Output file: ", out_file)
			writeRaster(imp_rst, filename = out_file, overwrite = TRUE)
			message_ts("Complete.")
			
			# Append to output
			processed_files <- c(processed_files, out_file)
			
	  }
  
	}
	
	# Return
	return(processed_files)

}
