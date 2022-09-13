# Function to create water x landcover layers for FWS Partners analysis from input grid
# and landcover layers

# Define function to split and buffer flooding areas
# Shapefile must have a column containing the names you wish to group and split the analysis
# Use of buffer_dist is recommended to speed processing; set as 2x your largest moving window
# Returns a vector of created files

overlay_water_landcover <- function(water_files, landcover_files, 
                                    uid_raster, uids = NULL, uid_lkp_file = NULL,
                                    imposed_water_value = NULL, imposed_landcover = "WetlandNatural",
                                    cell_dir, output_dir, overwrite = FALSE) { #buffer_dist?

  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Check simple parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!is.numeric(imposed_water_value) & !is.null(imposed_water_value)) {
    stop(add_ts("Argument 'imposed_water_value' must be numeric or null"))
  }
  if (!(imposed_landcover %in% c("WetlandNatural"))) {
    stop(add_ts("Unrecognized 'imposed_landcover'"))
  }
  
  # Check input files
  if (!all(file.exists(water_files))) stop(add_ts("The following water_files do not exist:\n", 
                                                  paste0(water_files[!file.exists(water_files)], collapse = ", ")))
  if (!all(file.exists(landcover_files))) stop(add_ts("The following landcover_files do not exist:\n", 
                                                      paste0(landcover_files[!file.exists(landcover_files)], collapse = ", ")))
  
  # Check and load lkp file
  if (!is.null(uid_lkp_file) & !file.exists(uid_lkp_file)) stop(add_ts("Missing uid_lkp_file"))
  if (file.exists(uid_lkp_file)) {
    uid_lkp_df <- readRDS(uid_lkp_file)
  }
  
  # Check output dir
  if (!(file.exists(cell_dir))) stop(add_ts("cell_dir does not exist"))
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))

  # Check and load guide raster
  if (is.raster(uid_raster)) {
    
    uid_rst <- uid_raster
    
  } else if (is.character(uid_raster)) {
    
    if (length(uid_raster) != 1) stop(add_ts("uid_raster be a single raster or filename"))
    if (!file.exists(uid_raster)) stop(add_ts("uid_raster does not exist: ", uid_raster, " not found."))
    
    uid_rst <- rast(uid_raster)
    
  } else {
    
    stop(add_ts("uid_raster must be a raster or filename of a raster"))
    
  }
  
  # Initialize output
  processed_files <- c()
  
	# Get unique ids if not specified
  if (is.null(uids)) uids <- unique(values(uid_rst))
  
  # Loop across unique cells
	for (uid in uids) {
		
	  message_ts("Creating layers for uid ", uid, "...")
	  
	  # Subset lookup df if exists
	  if (file.exists(uid_lkp_file)) {
	    lkp_df <- uid_lkp_df[uid_lkp_df$UID == uid,]
	  }
	  
    # 10k buffer (for creating water layers used in prediction)
    cell_10k_file <- file.path(cell_dir, paste0("cell_", uid, "_buffered10k.tif"))
    if (!file.exists(cell_10k_file) | overwrite == TRUE) {
      cell_rst <- buffer_rect(uid_rst, buffer_dist = 10000, match_value = uid,
                              out_file = cell_10k_file, overwrite = TRUE)
    } else {
      cell_rst <- rast(cell_10k_file)
    }
    
    # 5k buffer (for summarizing landscape later; not used further here)
    cell_5k_file <- file.path(cell_dir, paste0("cell_", uid, "_buffered5k.tif"))
    if (!file.exists(cell_5k_file) | overwrite == TRUE) {
      cell_5k_rst <- buffer_rect(uid_rst, buffer_dist = 5000, match_value = uid,
                              out_file = cell_5k_file, overwrite = TRUE)
    }
    
	  # Impose water
	  for (wf in water_files) {
	    
	    wfn <- basename(wf)
	    message_ts("Working on water_file ", wfn)
	    wfn_split <- strsplit(wfn, "_")
	    wmth <- extract_subelement(wfn_split, 3)
	    wyr <- extract_subelement(wfn_split, 4)
	    
	    # Skip if all files have been processed
	    landcovers <- extract_subelement(strsplit(landcover_files, "_"), 1)
	    out_files <- file.path(wxl_dir, paste0("cell_", uid, "_", 
	                                          wyr, "_", wmth, "_water_x_",
	                                          landcovers, ".tif"))
	    if (all(file.exists(wxl_file)) & overwrite != TRUE) {
	      message_ts("All landcover overlays created for this water file. Moving to next...")
	      next
	    }
	    
	    # Load
	    wtr_rst <- rast(wf)
	    
	    # Crop to buffered cell extent
	    wtr_rst <- crop(wtr_rst, cell_rst)
	    
	    # Impose water
	    if (is.null(imposed_water_value) & is.null(uid_lkp_file)) {
	      
	      message_ts("Not imposing water.")
	      wtr_imp_rst <- wtr_rst
	      
	    } else {
	      
	      if (file.exists(uid_lkp_file)) {
	      
	        message_ts("Looking up imposed value from file...")
	        imp_val <- lkp_df$AvgWater[lkp_df$ClassName == "Semi-permanent Wetland" & lkp_df$Month == wmth]
	        if (length(imp_val) != 1) {
	          stop(add_ts("Incorrect number of matches found: ", length(imp_val)))
	        } else {
	          message_ts("Using lookup imposed value of ", imp_val)
	        }
	      
	      } else if (is.numeric(imposed_water_value)) {
	      
	        message_ts("Using constant imposed value of ", imposed_water_value)
	        imp_val <- imposed_water_value
	        
	      } else {
	        
	        stop(add_ts("Should not get here"))
	        
	      }
	      
	      message_ts("Imposing flooding...")
	      wtr_imp_rst <- lapp(c(cell_rst, wtr_rst), fun = function(x, y) {
	        ifelse(!is.na(x) & x == 1, imp_val, y)
	      })
	      
	    }
	     
	    # Overlay water and landcover
	    #landcover_files <- model_lc_files
	    for (lcf in landcover_files) {
	      
	      lcfn <- basename(lcf)
	      message_ts("Working on landcover_file ", lcfn)
	      lc <- extract_subelement(strsplit(lcfn, "_"), 1)
	      
	      # Check if created
	      wxl_file <- file.path(wxl_dir, paste0("cell_", uid, "_", 
	                                            wyr, "_", wmth, "_water_x_",
	                                            lc, ".tif"))
	      if (!file.exists(wxl_file) | overwrite == TRUE) {
	        
	        message_ts("Working on landcover ", lc, "...")
	        
	        # Load
	        lc_rst <- rast(lcf)
	        
	        # Crop
	        lc_rst <- crop(lc_rst, wtr_rst)
	        
	        # Impose landcover
	        if (!is.null(imposed_landcover)) {
	          message_ts("Imposing landcover...")
	          if (lc == imposed_landcover) {
	            message_ts("Setting cell as ", lc, "...")
	            lc_imp_rst <- lapp(c(cell_rst, lc_rst), fun = function(x, y) {
	              ifelse(!is.na(x) & x == 1, 1, y)
	            })
	          } else {
	            message_ts("Setting cell as NOT ", lc, "...")
	            lc_imp_rst <- lapp(c(cell_rst, lc_rst), fun = function(x, y) {
	              ifelse(!is.na(x) & x == 1, 0, y)
	            })
	          }
	        } else {
	          lc_imp_rst <- lc_rst
	        }
	        
	        # Overlay
	        message_ts("Overlaying to ", basename(wxl_file), "...")
	        wxl_rst <- lapp(c(wtr_imp_rst, lc_imp_rst), fun = function(x, y) {
	          x * y
	        }, filename = wxl_file, overwrite = TRUE)
	        
	      } else {
	        
	        message_ts("File ", wxl_file, " already created. Moving to next...")
	        processed_files <- c(processed_files, wxl_file)
	        
	      }
	      
	    }
	    
	  }
	
	}
  
	return(processed_files)
	
}


