# Function to process input field shapefile, including splitting and rasterizing

# Define function to split and buffer flooding areas
# Shapefile must have a column containing the names you wish to group and split the analysis
# Use of buffer_dist is recommended to speed processing; set as 2x your largest moving window
# Returns a vector of created files
split_flooding_area <- function(field_shapefile, field_column_name, guide_raster, output_dir, rasterize_file = TRUE, buffer_dist = NULL, overwrite = FALSE) {
  
  # Load required packages
  if (!require(sp)) stop(add_ts("Library sp is required"))
  if (!require(rgdal)) stop(add_ts("Library rgdal is required"))
  if (!require(raster)) stop(add_ts("Library raster is required"))
  
  # Check simple parameters
  if (!is.logical(rasterize_file)) stop(add_ts("Argument 'rasterize_file' must be TRUE or FALSE"))
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!is.null(buffer_dist)) {
    if (!is.numeric(buffer_dist)) stop(add_ts("Argument 'buffer_dist' must either be NULL for no buffering or a number specifying the buffer distance"))
  }
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check and load shapefile
  if (class(field_shapefile) == "SpatialPolygonsDataFrame") {
    
    field_shp <- field_shapefile
    
  } else if (is.character(field_shapefile)) {
    
    if (length(field_shapefile) != 1) stop(add_ts("field_shapefile must be a single shapefile or filename"))
    if (!file.exists(field_shapefile)) stop(add_ts("field_shapefile does not exist: ", field_shapefile, " not found."))
    
    # Parse and load
    field_dir <- dirname(field_shapefile)
    field_fn <- basename(field_shapefile)
    field_fn <- substr(field_fn, 0, nchar(field_fn) - 4)
    
    field_shp <- readOGR(field_dir, field_fn)
    
  } else {
    
    stop(add_ts("field_shapefile must be a SpatialPolygonsDataFrame or a filename of an ESRI shapefile"))
    
  }
  
  # Check field column name
	if (!(field_column_name) %in% names(field_shp@data)) stop(add_ts("Column ", field_column_name, " does not exist in field_shapefile."))
  
  # Check and load guide raster
  if (is.raster(guide_raster)) {
    
    guide_rst <- guide_raster
    
  } else if (is.character(guide_raster)) {
    
    if (length(guide_raster) != 1) stop(add_ts("field_shapefile must be a single shapefile or filename"))
    if (!file.exists(guide_raster)) stop(add_ts("field_shapefile does not exist: ", field_shapefile, " not found."))
    
    guide_rst <- raster(guide_raster)
    
  } else {
    
    stop(add_ts("guide_raster must be a raster or filename of a raster"))
    
  }
  
	# Reproject shapefile if needed
	if (projection(field_shp) != projection(guide_rst)) {
		
		message_ts("Reprojecting shapefile to match guide_rst...")
		field_shp <- spTransform(field_shp, crs(guide_rst))
	
	}
	
  # Initialize output
  processed_files <- c()
  
	# Get unique flooding areas
	flooding_areas <- unique(field_shp@data[[field_column_name]])
	for (fa in flooding_areas) {
		
	  # Make name safe for exporting
	  # Replace underscores because later functions assume first chunk before an underscore is the entire flooding area name
	  clean_name <- clean_string_remove_underscores(fa)
	  
	  # Split shapefiles ----------------
	  
		# Check file existence
		fa_file <- file.path(output_dir, paste0(clean_name, ".shp"))
		if (file.exists(fa_file) & overwrite != TRUE) {
		  
		  message_ts("Split shapefile for flooding area ", fa, " already created and overwrite != TRUE. Moving to next...")
		  
		} else {
		  
		  #need to delete if previously existing and overwrite = TRUE
		  
		  # Subset
		  message_ts("Subsetting area ", fa, "...")
		  fa_shp <- field_shp[field_shp[[field_column_name]] == fa, ]
		  
		  # Export
		  writeOGR(fa_shp, output_dir, clean_name, driver = "ESRI Shapefile")
		  message_ts("Complete.")
		  
		  # Append to return
		  processed_files <- c(processed_files, fa_file)
		  
		}
		
		# Rasterize if requirested -----------
		if (rasterize_file == TRUE) {
		  
		  # Check file existence
		  fa_rst_file <- file.path(output_dir, paste0(clean_name, ".tif"))
		  if (file.exists(fa_rst_file) & overwrite != TRUE) {
		    message_ts("Flooding area ", fa, " already rasterized and overwrite != TRUE. Moving to next...")
		    next
		  }
		  
		  # Rasterize and immediately export if not buffering
		  if (is.null(buffer_dist)) {
		    
		    # Rasterize
		    message_ts("Rasterizing...")
		    fa_rst <- rasterize(fa_shp, guide_rst, field = 1, filename = fa_rst_file, overwrite = TRUE)
		    
		  # Rasterize and buffer if requested
		  } else {
		    
		    # Rasterize
		    message_ts("Rasterizing...")
		    fa_rst <- rasterize(fa_shp, guide_rst, field = 1) #keep in memory, as overwriting in subsequent call causes error
		    
		    # Turn values within buffer distance of field to 2s instead of NAs
		    # Used for masking later to speed processing
		    # Width is in meters
		    message_ts("Calculating ", buffer_dist, "m buffer...")
		    fa_buf_rst <- buffer(fa_rst, width = buffer_dist)
		    message_ts("Adding buffer to flooding area raster...")
		    fa_out_rst <- overlay(x = fa_rst, y = fa_buf_rst, fun = function(x, y) { ifelse(is.na(x) & y == 1, 2, x) },
		                          filename = fa_rst_file, overwrite = TRUE)
		    
		  }
		  
		  # Append to return
		  processed_files <- c(processed_files, fa_rst_file)
		  
		}
		
	}
	
	return(processed_files)
	
}


