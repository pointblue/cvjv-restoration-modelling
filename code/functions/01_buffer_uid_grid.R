# Create buffer rasters

# Fill value of NA will lookup from file

buffer_uid_grid <- function(uid_raster, uids = NULL, 
                            buffer_distances = c(250, 5000), 
                            fill_values = 0, fill_labels = NA,
                            output_dir, overwrite = FALSE, verbose = TRUE) { #lookup type?
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Check simple parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!is.numeric(buffer_distances)) stop(add_ts("Argument 'buffer_distances' must be a numeric vector"))
  if (!is.numeric(fill_values)) stop(add_ts("Argument 'fill_values' must be a numeric vector"))
  if (!is.character(fill_labels) & !any(is.na(fill_labels))) stop(add_ts("Argument 'fill_labels' must be a character vector or NA"))
  
  # Check lengths
  if (length(fill_values) == 1) {
    fill_values <- rep(fill_values, length(buffer_distances))
  } else if (length(fill_values) != length(buffer_distances)) {
    stop(add_ts("Argument 'fill_values' must be length 1 or the same as the length of 'buffer_distances'"))
  }
  if (length(fill_labels) == 1) {
    fill_labels <- rep(fill_labels, length(buffer_distances))
  } else if (length(fill_labels) != length(buffer_distances)) {
    stop(add_ts("Argument 'fill_labels' must be length 1 or the same as the length of 'buffer_distances'"))
  }
  
  # Check output dir
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
    
    if (verbose) message_ts("Creating layers for uid ", uid, "...")
    
    # Loop across buffers
    for (n in 1:length(buffer_distances)) {
      
      bd <- buffer_distances[n]
      if (verbose) message_ts("Creating buffer for distance ", bd)
      
      val <- fill_values[n]
      
      # If fill_labels is NA, use value
      lbl <- fill_labels[n]
      if (is.na(lbl)) {
        lbl <- val
      } 
      
      out_file <- file.path(output_dir, paste0("cell_", uid, "_buffered-", bd, "m_filled-", lbl, ".tif"))
      if (!file.exists(out_file) | overwrite == TRUE) {
        
        if (verbose) message_ts("Using fill value of ", val, " (label = ", lbl, ")")
        buf_rst <- buffer_rect(uid_rst, buffer_dist = bd, match_value = uid, fill_value = val,
                               out_file = out_file, overwrite = TRUE)
        
      }
      
      processed_files <- c(processed_files, out_file)
      
    }
    
  }

  return(processed_files)
  
}

# uids <- uids_subset[1:2]
# buffer_uid_grid(uid_raster = file.path(grid_dir, "unique_ids_masked.tif"), 
#                 uids = uids, #[[n]], #uids_split[[1]][1:3]
#                 buffer_distances = c(250, 5000), 
#                 fill_values = c(0, 0), 
#                 fill_labels = NA,
#                 output_dir = cell_dir, 
#                 overwrite = overwrite,
#                 verbose = TRUE)
