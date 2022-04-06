# Code to create bird predictions for realtime models of the full Bird Returns dataset

# Predict bird rasters using the combination model of realtime and longterm
# If using a realtime-only or longterm-only model, pass the same set of layers to both water_files arguments
#   The model object will only use the files from either the realtime or longterm
# Returns character vector of processed files
predict_bird_rasters <- function(water_files_realtime, water_files_longterm, scenarios, water_months,
                                 model_files, model_names, 
                                 static_cov_files, static_cov_names, 
                                 monthly_cov_files, monthly_cov_months, monthly_cov_names,
                                 output_dir, overwrite = FALSE,
                                 on_missing_landcover = "stop") {
	
  # Load required packages
  if (!require(rgdal)) stop(add_ts("Library rgdal is required"))
  if (!require(raster)) stop(add_ts("Library raster is required"))
  if (!require(gbm)) stop(add_ts("Library gbm is required"))
  if (!require(dismo)) stop(add_ts("Library dismo is required"))
  
  # Check flood areas and scenarios
  if (!is.character(scenarios)) stop(add_ts("Argument 'scenarios' must be a character vector"))
  if (!is.character(water_months)) stop(add_ts("Argument 'water_months' must be a character vector"))
  if (!(all(water_months %in% month.abb))) stop(add_ts("Argument 'water_months' must consist of valid 3-letter month abbreviations"))
  
  # Check input files
  if (!all(file.exists(water_files_realtime))) stop(add_ts("The following water_files do not exist:\n", 
                                                  paste0(water_files_realtime[!file.exists(water_files_realtime)], collapse = ", ")))
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
  if (!(on_missing_landcover %in% c("next", "stop"))) stop(add_ts("Argument 'on_missing_landcover' must either be 'stop' or 'next'"))
  
  # Define what water files are required
  # Long term (average)
  landcover_lt_df <- data.frame(NameModel = c("long_rXw_250m", "long_2wetlandXw250m", "long_treatXw_250m", 
                                              "long_cornXw_250m", "long_grainXw_250m", "long_otherXw_250m",
                                              "long_rXw_5km", "long_2wetlandXw5km", "long_treatXw_5km", "long_allcropXw_5km"), 
                                NameLandcover = c("Rice", "Wetland_SemiSeas", "TreatedWetland", "Corn", "Grain", "AltCrop",
                                                  "Rice", "Wetland_SemiSeas", "TreatedWetland", "NonRiceCrops"), 
                                Distance = c(rep("250m", 6), rep("5000m", 4)))
  landcover_lt_df$LandcoverDistance <- paste(landcover_lt_df$NameLandcover, landcover_lt_df$Distance, sep = "_")
  
  # Real-time (imposed)
  landcover_rt_df <- data.frame(NameModel = c("rXw_250m", "wetland2Xw250m", "treatXw_250m", "cornXw_250m", "grainXw_250m", "otherXw_250m",
                                              "rXw_5km", "wetland2Xw5km", "treatXw_5km", "allcropXw_5km"), 
                                NameLandcover = c("Rice", "Wetland_SemiSeas", "TreatedWetland", "Corn", "Grain", "AltCrop",
                                                  "Rice", "Wetland_SemiSeas", "TreatedWetland", "NonRiceCrops"), 
                                Distance = c(rep("250m", 6), rep("5000m", 4)))
  landcover_rt_df$LandcoverDistance <- paste(landcover_rt_df$NameLandcover, landcover_rt_df$Distance, sep = "_")
  
  # Load static covariates
  static_cov_stk <- stack(static_cov_files)
  names(static_cov_stk) <- static_cov_names
  
  # Initialize output
  processed_files <- c()
  
  # Get unique flooding areas
  flood_areas <- unique(extract_subelement(strsplit(basename(water_files_realtime), "_"), 1))
  
	# Loop across flooding areas
	for (fa in flood_areas) {

		message_ts("Working on flooding area ", fa)
	  fac <- clean_string(fa)
		
		# Files for this flooding area
		fa_files <- water_files_realtime[grepl(fa, water_files_realtime)]
		
		# Loop across unique scenarios
		for (scn in scenarios) {
			
			message_ts("Working on scenario ", scn)
		  scn_files <- fa_files[grepl(scn, fa_files)]
		  if (length(scn_files) == 0) {
		    message_ts("WARNING: no files found matching scneario ", scn, " for flood area ", fa, ". Moving to next...")
		    next
		  }
		  
		  # Loop across months
  		for (mth in water_months) {
  		  
  		  message_ts("Working on month ", mth)
  		  mth_files <- scn_files[grepl(mth, scn_files)]
  		  if (length(mth_files) == 0) {
  		    message_ts("WARNING: no files found matching month ", mth, " for flood area ", fa, " and scenario ", scn, ". Moving to next...")
  		    next
  		  }
  		  
  		  # Check if all models have been predicted
  		  prd_files <- file.path(output_dir, paste0(fac, "_", mth, "_", scn, "_", model_names, ".tif"))
  		  if (all(file.exists(prd_files))) {
  		    
			  # Append to output
  			processed_files <- c(processed_files, prd_files)
			
  		    message_ts("All bird predictions have been created for this scenario. Moving to next...")
  		    next
  		    
  		  }
  		  
  		  # LONGTERM WATER (average)
  		  message_ts("Checking and loading long-term water x landcover moving window files...")

  		  # Check files
  		  landcover_lt_df$File <- mapply(nm = landcover_lt_df$NameLandcover, dst = landcover_lt_df$Distance, 
                          			       FUN = function(nm, dst) {
                            			         y <- water_files_longterm[grep(paste0(mth, ".*", nm, "_.*", dst), water_files_longterm)]
                            			         if(length(y) > 1) {
                            			           print(y)
                            			           message_ts("Filtering by flood area...")
                            			           y <- y[grepl(fa, y)]
                            			           print(y)
                            			           if(length(y) > 1) stop(add_ts("Multiple landcover matches found. Must specify."))
                            			         } else if (length(y) == 0) {
                            			           y <- 0
                            			         }
                            			         y
                            			       })
  		  
  		  if (any(is.na(landcover_lt_df$File))) {
  		    
  		    if(on_missing_landcover == "stop") {
  		      stop(add_ts("The following longterm water x landcover moving window combinations are missing from water_files_realtime for flood area ", fa, 
  		                  ", scenario ", scn, ", and month ", mth, ":\n\t",
  		                  paste0(landcover_lt_df$LandcoverDistance[is.na(landcover_lt_df$File)], collapse = "\n\t"),
  		                  "\n\nHalting execultion."))
  		    } else {
  		      message_ts("The following longterm water x landcover moving window combinations are missing from water_files_realtime for flood area ", fa, 
  		                 ", scenario ", scn, ", and month ", mth, ":\n\t",
  		                 paste0(landcover_lt_df$LandcoverDistance[is.na(landcover_lt_df$File)], collapse = "\n\t"),
  		                 "\n\nMoving to next...")
  		      next
  		    }
  		    
  		  }
  		  
  		  # Load required imposed water files and match names
  		  wtr_lt_stk <- stack(landcover_lt_df$File)
  		  names(wtr_lt_stk) <- landcover_lt_df$NameModel
  		  
  			# REALTIME WATER (imposed)
  			message_ts("Checking and loading imposed water x landcover moving window files...")
  			
  			# Check files
  			landcover_rt_df$File <- mapply(nm = landcover_rt_df$NameLandcover, dst = landcover_rt_df$Distance, 
  			                               FUN = function(nm, dst) {
  			                                 y <- mth_files[grep(paste0(nm, "_.*", dst), mth_files)]
  			                                 if(length(y) > 1) {
  			                                   stop(add_ts("Multiple matches found for real-time (imposed) water files"))
  			                                 } else if (length(y) == 0) {
  			                                   y <- 0
  			                                 }
  			                                 y
  			                               })
  			
  			if (any(is.na(landcover_rt_df$File))) {
  			  
  			  if(on_missing_landcover == "stop") {
  			    stop(add_ts("The following imposed water x landcover moving window combinations are missing from water_files_realtime for flood area ", fa, 
  			                ", scenario ", scn, ", and month ", mth, ":\n\t",
  			                paste0(landcover_rt_df$LandcoverDistance[is.na(landcover_rt_df$File)], collapse = "\n\t"),
  			                "\n\nHalting execultion."))
  			  } else {
  			    message_ts("The following imposed water x landcover moving window combinations are missing from water_files_realtime for flood area ", fa, 
  			                ", scenario ", scn, ", and month ", mth, ":\n\t",
  			                paste0(landcover_rt_df$LandcoverDistance[is.na(landcover_rt_df$File)], collapse = "\n\t"),
  			               "\n\nMoving to next...")
  			    next
  			  }
  			  
  			}
  			
  			# Load required imposed water files and match names
  			message_ts("Loading imposed water x landcover moving window files...")
  			wtr_rt_stk <- stack(landcover_rt_df$File)
  			names(wtr_rt_stk) <- landcover_rt_df$NameModel
  			
  			# Load basic covariates
  			message_ts("Loading model covariates...")
  			#static_cov_stk <- stack(static_cov_files) #already loaded
  			#names(static_cov_stk) <- static_cov_names
  			
  			# Load month file(s)
        if (!(mth %in% monthly_cov_months)) stop(add_ts("Invalid month: no matching month files for month ", mth, ". Please update files in cov_dir and tmax files in definitions.R"))
  			mth_match <- which(monthly_cov_months == mth)
  			#print(mth)
  			#str(mth)
  			#print(mth_match)
  			#str(mth_match)
    		mth_cov_stk <- stack(monthly_cov_files[mth_match])
  			names(mth_cov_stk) <- monthly_cov_names[mth_match]
    		
  			# Crop stacks to field area
  			message_ts("Cropping covariate stacks to field...")
  			#print(extent(static_cov_stk))
  			#print(extent(mth_cov_stk))
  			#print(extent(wtr_lt_stk))
  			#print(extent(wtr_rt_stk))
  			if (identical(extent(static_cov_stk), extent(mth_cov_stk))) {
  			  message_ts("As stack...")
  			  cov_stk <- stack(static_cov_stk, mth_cov_stk)
  			  cov_stk <- crop(cov_stk, wtr_rt_stk)
  			} else {
  			  message_ts("Individually...")
  			  static_cov_stk <- crop(static_cov_stk, wtr_rt_stk)
  			  mth_cov_stk <- crop(mth_cov_stk, wtr_rt_stk)
  			  cov_stk <- stack(static_cov_stk, mth_cov_stk)
  			}
  			wtr_lt_stk <- crop(wtr_lt_stk, wtr_rt_stk)
  			
  			# Check extents and combine
  			if (!identical(extent(wtr_lt_stk), extent(wtr_rt_stk))) {
  			  message_ts("Real-time water layers:\n", paste(landcover_rt_df$File, collapse = "\n"))
  			  message_ts("Long-term water layers:\n", paste(landcover_lt_df$File, collapse = "\n"))
  			  stop(add_ts("Real-time and long-term water layers have different extents. Check specified files."))
  			}
  			
  			cov_stk <- stack(cov_stk, wtr_lt_stk, wtr_rt_stk)
  			
  			# Loop across bird models
  			for (mn in 1:length(model_files)) {
  			  
  			  mdl_file <- model_files[mn]
  			  mdl_name <- model_names[mn]
  				
  				message_ts("Working on bird model ", mdl_name)
  				
  				prd_file <- file.path(output_dir, paste0(fac, "_", mth, "_", scn, "_", mdl_name, ".tif"))
  				
  				# Check if predicted
  				if (file.exists(prd_file) & overwrite != TRUE) {
  					
  					message_ts("Surface already predicted and overwrite != TRUE.  Moving to next...")
  				  next
  				
  				} 
  				
  				# Load model
  				mdl <- readRDS(mdl_file)
  				
  				# Predict
  				message_ts("Predicting surface...")
  				message_ts("Output file: ", prd_file)
  				
  				# Must define factors when predicting
  				prd_rst <- raster::predict(cov_stk, mdl, n.trees = mdl$gbm.call$best.trees, type = "response", factors = list("COUNT_TYPE2" = c(1, 2)),
  							filename = prd_file, overwrite = TRUE)
  				print(summary(prd_rst))
  				message_ts("Complete.")
  				
  				# Append to output
  				processed_files <- c(processed_files, prd_file)
  
  			}
  			
		  }
		
		}
		
	}
  
  # Return
  return(processed_files)
  
}