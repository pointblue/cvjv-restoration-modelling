# Code to create bird predictions for longterm models of the full Bird Returns dataset

# Predict bird rasters using the longterm model
# Returns character vector of processed files
predict_birds <- function(water_files_longterm, scenarios, water_months,
                                 model_files, model_names, 
                                 static_cov_files, static_cov_names, 
                                 monthly_cov_files, monthly_cov_months, monthly_cov_names,
                                 output_dir, overwrite = FALSE,
                                 on_missing_landcover = "stop",
                                 verbose = TRUE,
                                 on_error_rerun = FALSE) {
	
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  if (!require(gbm)) stop(add_ts("Library gbm is required"))
  if (!require(dismo)) stop(add_ts("Library dismo is required"))
  
  # Check flood areas and scenarios
  if (!is.character(scenarios)) stop(add_ts("Argument 'scenarios' must be a character vector"))
  if (!is.character(water_months)) stop(add_ts("Argument 'water_months' must be a character vector"))
  if (!(all(water_months %in% month.abb))) stop(add_ts("Argument 'water_months' must consist of valid 3-letter month abbreviations"))
  
  # Check input files
  if (!all(file.exists(water_files_longterm))) stop(add_ts("The following water_files do not exist:\n", 
                                                  paste0(water_files_longterm[!file.exists(water_files_longerm)], collapse = ", ")))
  if (!all(file.exists(model_files))) stop(add_ts("The following model_files do not exist:\n", 
                                                      paste0(model_files[!file.exists(model_files)], collapse = ", ")))
  if (!all(file.exists(static_cov_files))) stop(add_ts("The following static_cov_files do not exist:\n", 
                                                  paste0(static_cov_files[!file.exists(static_cov_files)], collapse = ", ")))
  if (!all(file.exists(monthly_cov_files))) stop(add_ts("The following monthly_cov_files do not exist:\n", 
                                                       paste0(monthly_cov_files[!file.exists(monthly_cov_files)], collapse = ", ")))
  
  # Check model names
  if (!is.character(model_names)) stop(add_ts("Argument 'model_names' must be a character vector"))
  if (length(model_files) != length(model_names)) stop(add_ts("Arguments 'model_files' and 'model_names' must be of the same length"))
  
  # Check static covariate names
  if (!is.character(static_cov_names)) stop(add_ts("Argument 'static_cov_names' must be a character vector"))
  if (length(static_cov_files) != length(static_cov_names)) stop(add_ts("Arguments 'static_cov_files' and 'static_cov_names' must be of the same length"))
  
  # Check monthly covariate months and names
  if (!is.character(monthly_cov_months)) stop(add_ts("Argument 'monthly_cov_months' must be a character vector"))
  if (!(all(monthly_cov_months %in% month.abb))) stop(add_ts("Argument 'monthly_cov_months' must consit of valid 3-letter month abbreviations"))
  if (length(monthly_cov_files) != length(monthly_cov_months)) stop(add_ts("Arguments 'monthly_cov_files' and 'monthly_cov_months' must be of the same length"))
  
  if (!is.character(monthly_cov_names)) stop(add_ts("Argument 'monthly_cov_names' must be a character vector"))
  if (length(monthly_cov_files) != length(monthly_cov_names)) stop(add_ts("Arguments 'monthly_cov_files' and 'monthly_cov_names' must be of the same length"))
  
  # Check dirs
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check other parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!is.logical(on_error_rerun)) stop(add_ts("Argument 'on_error_rerun' must be TRUE or FALSE"))
  if (!(on_missing_landcover %in% c("next", "stop"))) stop(add_ts("Argument 'on_missing_landcover' must either be 'stop' or 'next'"))
  
  # Define what water files are required
  # Long term (average)
  landcover_lt_df <- data.frame(NameModel = c("long_rXw_250m", "long_2wetlandXw250m", "long_treatXw_250m", 
                                              "long_cornXw_250m", "long_grainXw_250m", "long_otherXw_250m",
                                              "long_rXw_5km", "long_2wetlandXw5km", "long_treatXw_5km", "long_allcropXw_5km"), 
                                NameLandcover = c("Rice", "WetlandSemiSeas", "TreatedWetland", "Corn", "Grain", "AltCrop",
                                                  "Rice", "WetlandSemiSeas", "TreatedWetland", "NonRiceCrops"), 
                                Distance = c(rep("250m", 6), rep("5000m", 4)))
  landcover_lt_df$LandcoverDistance <- paste(landcover_lt_df$NameLandcover, landcover_lt_df$Distance, sep = "_")
  
  # Load static covariates
  static_cov_stk <- stack(static_cov_files)
  names(static_cov_stk) <- static_cov_names
  
  # Initialize output
  processed_files <- c()
  
  # Get unique flooding areas
  fns_split <- strsplit(basename(water_files_longterm), "_")
  flood_areas <- unique(extract_subelement(fns_split, 1))
  
	# Loop across flooding areas
	for (fa in flood_areas) {

		if (verbose) message_ts("Working on area ", fa)
	  fac <- clean_string(fa)
		
		# Files for this flooding area
		fa_files <- water_files_longterm[grepl(fa, water_files_longterm)]
		
		# Loop across unique scenarios
		for (scn in scenarios) {
			
			if (verbose) message_ts("Working on scenario ", scn)
		  scn_files <- fa_files[grepl(scn, fa_files)]
		  if (length(scn_files) == 0) {
		    if (verbose) message_ts("WARNING: no files found matching scenario ", scn, " for flood area ", fa, ". Moving to next...")
		    next
		  }
		  
		  # Loop across months
  		for (mth in water_months) {
  		  
  		  if (verbose) message_ts("Working on month ", mth)
  		  mth_files <- scn_files[grepl(mth, scn_files)]
  		  if (length(mth_files) == 0) {
  		    if (verbose) message_ts("WARNING: no files found matching month ", mth, " for flood area ", fa, " and scenario ", scn, ". Moving to next...")
  		    next
  		  }
  		  
  		  # Check if all models have been predicted
  		  prd_files <- file.path(output_dir, paste0(fac, "_month-", mth, "_", scn, 
  		                                            "_model-", gsub("_", "-", model_names), ".tif"))
  		  if (all(file.exists(prd_files))) {
  		    
  			  # Append to output
    			processed_files <- c(processed_files, prd_files)
			
  		    if (verbose) message_ts("All bird predictions have been created for this scenario. Moving to next...")
  		    next
  		    
  		  }
  		  
  		  # LONGTERM WATER (average)
  		  if (verbose) message_ts("Checking and loading long-term water x landcover moving window files...")

  		  # Check files
        # landcover_lt_df$File <- mapply(nm = landcover_lt_df$NameLandcover, dst = landcover_lt_df$Distance,
        #                  			       FUN = function(nm, dst) {
        #                    			         y <- fa_files[grep(paste0(mth, ".*", nm, "_.*", dst), fa_files)]
        #                    			         #print(y)
        #                    			         if(length(y) > 1) {
        #                    			           stop(add_ts("Multiple files found for ", fac, ", ", mth, ", ", nm, ", ", dst,
        #                    			                       ". Should only be one."))
        #                    			         } else if (length(y) == 0) {
        #                    			           y <- 0
        #                    			         }
        #                    			         y
        #                    			       })
  		  
  		  landcover_lt_df$File <- file.path(dirname(water_files_longterm[1]), 
  		                                    paste0(fa, "_", 
  		                                           "buffered-", landcover_lt_df$Distance, "_",
  		                                           "filled-semiperm_",
  		                                           "month-", mth, "_",
  		                                           scn, "_",
  		                                           "landcover-", landcover_lt_df$NameLandcover,
  		                                           ".tif"))
  		  
  		  #if (any(is.na(landcover_lt_df$File))) {
  		  landcover_lt_df$FileExists <- file.exists(landcover_lt_df$File)
  		  if (!all(landcover_lt_df$FileExists)) {
  		    
  		    if(on_missing_landcover == "stop") {
  		      stop(add_ts("The following longterm water x landcover moving window combinations are missing from water_files_longterm for flood area ", fa, 
  		                  ", scenario ", scn, ", and month ", mth, ":\n\t",
  		                  paste0(landcover_lt_df$LandcoverDistance[!landcover_lt_df$FileExists], collapse = "\n\t"),
  		                  "\n\nHalting execultion."))
  		    } else {
  		      if (verbose) message_ts("The following longterm water x landcover moving window combinations are missing from water_files_longterm for flood area ", fa, 
  		                 ", scenario ", scn, ", and month ", mth, ":\n\t",
  		                 paste0(landcover_lt_df$LandcoverDistance[!landcover_lt_df$FileExists], collapse = "\n\t"),
  		                 "\n\nMoving to next...")
  		      next
  		    }
  		    
  		  }
  		  
  		  # Load required imposed water files and match names
  		  wtr_lt_stk <- stack(landcover_lt_df$File)
  		  names(wtr_lt_stk) <- landcover_lt_df$NameModel
  		  
  			# Load basic covariates
  			if (verbose) message_ts("Loading model covariates...")
  			#static_cov_stk <- stack(static_cov_files) #already loaded
  			#names(static_cov_stk) <- static_cov_names
  			
  			# Load month file(s)
        if (!(mth %in% monthly_cov_months)) {
          stop(add_ts("Invalid month: no matching month files for month ", mth, 
                      ". Please update files in cov_dir and tmax files in definitions.R"))
        }
  			mth_match <- which(monthly_cov_months == mth)
  			#print(mth)
  			#str(mth)
  			#print(mth_match)
  			#str(mth_match)
    		mth_cov_stk <- stack(monthly_cov_files[mth_match])
  			names(mth_cov_stk) <- monthly_cov_names[mth_match]
    		
  			# Crop stacks to field area
  			if (verbose) message_ts("Cropping covariate stacks to field...")
  			#print(extent(static_cov_stk))
  			#print(extent(mth_cov_stk))
  			#print(extent(wtr_lt_stk))
  			if (identical(extent(static_cov_stk), extent(mth_cov_stk))) {
  			  if (verbose) message_ts("As stack...")
  			  cov_stk <- stack(static_cov_stk, mth_cov_stk)
  			  cov_stk <- crop(cov_stk, wtr_lt_stk)
  			} else {
  			  if (verbose) message_ts("Individually...")
  			  static_cov_stk <- crop(static_cov_stk, wtr_lt_stk)
  			  mth_cov_stk <- crop(mth_cov_stk, wtr_lt_stk)
  			  cov_stk <- stack(static_cov_stk, mth_cov_stk)
  			}
  			
  			# Check extents and combine
  			cov_stk <- stack(cov_stk, wtr_lt_stk)
  			
  			# Error counter
  			error_counter <- 0
  			
  			# Loop across bird models
  			for (mn in 1:length(model_files)) {
  			  
  			  mdl_file <- model_files[mn]
  			  mdl_name <- model_names[mn]
  				
  				if (verbose) message_ts("Working on bird model ", mdl_name)
  				
  				prd_file <- prd_files[mn]
  				
  				# Check if predicted
  				if (file.exists(prd_file) & overwrite != TRUE) {
  					
  					if (verbose) message_ts("Surface already predicted and overwrite != TRUE.  Moving to next...")
  				  next
  				
  				} 
  				
  				# Load model
  				mdl <- readRDS(mdl_file)
  				
  				# Predict
  				if (verbose) message_ts("Predicting surface...")
  				if (verbose) message_ts("Output file: ", prd_file)
  				
  				tryCatch({
    				# Must define factors when predicting
    				prd_rst <- terra::predict(cov_stk, mdl, n.trees = mdl$gbm.call$best.trees, 
    				                   type = "response", factors = list("COUNT_TYPE2" = c(1, 2)))
    				writeRaster(prd_rst, filename = prd_file, overwrite = TRUE)
    				#print(summary(prd_rst)) #catches predictions that are empty
    				if (verbose) message_ts("Complete.")
  				
    				# Append to output
    				processed_files <- c(processed_files, prd_file)
    				
  				}, error = function(e) {
  				  
  				  if (verbose) message_ts("Error when predicting: ", e)
  				  
  				  error_counter <<- error_counter + 1
  				  
  				})
  				
  				# Break if encountered multiple errors with this cell, optionally re-running prior processing
  				if (error_counter > 1) {
  				  
  				  if (on_error_rerun == TRUE) {
  				    
  				    if (verbose) message_ts("Multiple errors encountered with cell ", fac, ". Attempting to reprocess input layers...")
  				    
  				    # TODO: Contains parameters not passed via function call
  				    
  				    tryCatch({
    				    
    				    # Overlay water and landcover ----------------------------------
    				    wxl_files_temp <- overlay_water_landcover(water_files,
    				                                         model_lc_files, #from definitions,
    				                                         uid_raster = file.path(grid_dir, "unique_ids_masked.tif"),
    				                                         uids = uids_subset[[n]],
    				                                         uid_lkp_file = file.path(grid_dir, "uid_full_lookup.rds"),
    				                                         imposed_landcover = "WetlandNatural",
    				                                         cell_dir = cell_dir, 
    				                                         output_dir = wxl_dir, 
    				                                         overwrite = TRUE)
    				    #printmessage = "print") #implement for logging
    				    
    				    
    				    # Create mean neighborhood water rasters
    				    # Function defined in functions/water_moving_window.R
    				    fcl_files_temp <- mean_neighborhood_water(wxl_files_temp,                #previously-created water x landcover files
    				                                         distances = c(250, 5000), #250m and 5km
    				                                         output_dir = fcl_dir, 
    				                                         overwrite = TRUE)
    				    
    				    # Re-predict
    				    prd_files_temp <- predict_bird_rasters(water_files_longterm,                  
    				                                      scenarios = scenarios,
    				                                      water_months = water_months,
    				                                      model_files = model_files, 
    				                                      model_names = model_names, 
    				                                      static_cov_files = static_cov_files, 
    				                                      static_cov_names = static_cov_names,
    				                                      monthly_cov_files = monthly_cov_files,
    				                                      monthly_cov_months = monthly_cov_months,
    				                                      monthly_cov_names = monthly_cov_names,
    				                                      output_dir = output_dir,
    				                                      verbose = TRUE,
    				                                      on_error_rerun = FALSE) #set to false to prevent endless recursion
    				    
    				    # Append
    				    processed_files <- c(processed_files, prd_files_temp)
    				    
  				    }, error = function(e) {
  				      
  				      if (verbose) message_ts("Error when attempting to reprocess files for area ", fac, ": ", e)
  				      if (verbose) message_ts("Will need to be fixed manually. Moving to next...")
  				      
  				    })
    				    
  				  } else {
  				    
  				    if (verbose) message_ts("Multiple errors encountered with cell ", fac, ". Skipping to next cell...")
  				    break 
  				   
  				  }
  				  
  				}
  
  			}
  			
		  }
		
		}
		
	}
  
  # Return
  return(processed_files)
  
}

#cell-597751_buffered-250m_filled-semiperm_month-Feb_year-2013-2022_landcover-TreatedWetland
#cell-100366_buffered-250m_filled-semiperm_month-Apr_year-2013-2022_landcover-AltCrop
# uids <- uids_split[[1]]
# fcl_imp_files <- file.path(fcl_dir, paste0("cell-", rep(uids, each = 12 * 2 * 7), "_",
#                                            "buffered-", rep(c(250, 5000), each = 1, times = 12 * 7 * length(uids)), "m_", 
#                                            "filled-semiperm_",
#                                            "month-", rep(month.abb, each = 7 * 2, times = length(uids)),"_",
#                                            "year-2013-2022_",
#                                            "landcover-", rep(gsub("_", "", landcovers), each = 2, times = 12 * length(uids)),
#                                            ".tif"))
# stopifnot(all(file.exists(fcl_imp_files)))
# predict_birds(water_files_longterm = fcl_imp_files,                  
#               scenarios = "year-2013-2022",
#               water_months = month.abb[1:2], #c("Apr"),
#               model_files = shorebird_model_files_long, 
#               model_names = shorebird_model_names_long, 
#               static_cov_files = bird_model_cov_files, 
#               static_cov_names = bird_model_cov_names,
#               monthly_cov_files = tmax_files,
#               monthly_cov_months = tmax_months,
#               monthly_cov_names = tmax_names,
#               output_dir = prd_dir,
#               verbose = TRUE)
