
impose_water_lookup <- function(uid_files, lookup_file, 
                                match_strings, match_col,
                                class_col, class_name, value_col, 
                                loop_col = NA, loop_classes = NA,
                                class_label = NA, value_adjustment = "coverage", 
                                output_dir, overwrite = FALSE,
                                verbose = TRUE) { #lookup type?
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Check simple parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!is.character(class_label) & !any(is.na(class_label))) stop(add_ts("Argument 'class_label' must be character or NA"))
  if (!is.character(match_strings)) stop(add_ts("Argument 'match_strings' must be character"))
  if (length(match_strings) != length(uid_files)) stop(add_ts("Argument 'match_strings' must same length as 'uid_files'"))
  
  # Check output dir
  if (!(file.exists(output_dir))) stop(add_ts("output_dir does not exist"))
  
  # Check and load lkp file
  if (!file.exists(lookup_file)) stop(add_ts("Argument 'lookup_file' does not exist"))
  if (verbose) message_ts("Loading lookup data...")
  ext <- file_ext(lookup_file)
  if (ext == "rds") {
    lookup_df <- readRDS(lookup_file)
  } else if (ext == "csv") {
    lookup_df <- read.csv(lookup_file)
  } else {
    stop(add_ts("'lookup_file' not in a readable format; must be rds or csv"))
  }
  
  # Check columns
  if (!(match_col %in% names(lookup_df))) stop(add_ts("Argument 'match_col' must be a column in 'lookup_file'"))
  if (!(class_col %in% names(lookup_df))) stop(add_ts("Argument 'class_col' must be a column in 'lookup_file'"))
  if (!(value_col %in% names(lookup_df))) stop(add_ts("Argument 'value_col' must be a column in 'lookup_file'"))
  if (!(class_name %in% as.character(unique(lookup_df[[class_col]])))) { 
    stop(add_ts("Argument 'class_name' must be a value in the 'class_col' column of 'lookup_df'")) 
  }
  if (!is.na(loop_col)) {
    if (!(loop_col %in% names(lookup_df))) stop(add_ts("Argument 'loop_col' must be a column in 'lookup_file' or NA"))
  }
  if (!all(loop_classes %in% as.character(unique(lookup_df[[loop_col]])))) {
    stop(add_ts("All elements of argument 'loop_classes' must be values in the 'loop_col' column of 'lookup_df'"))
  }
  
  # Handle if no looping
  if (is.null(loop_col) | is.na(loop_col)) {
    loop_col <- class_col
    loop_classes <- class_name
    loop_labels <- NULL
  } else {
    loop_labels <- TRUE
  }
  
  # Subset lookup file to valid uids for quicker matching (subsetting uid_df is the slowest part of the process)
  if (verbose) message_ts("Subsetting lookup data...")
  lookup_df <- lookup_df[as.character(lookup_df[[match_col]]) %in% match_strings, ]
  
  # Initialize output
  processed_files <- c()
  
  # Loop across unique cells
  for (n in 1:length(uid_files)) {
    
    uf <- uid_files[n]
    ufn <- basename(uf)
    ufn_match <- match_strings[n]
    if (verbose) message_ts("Working on file ", ufn, ", matching to ", ufn_match, "...")
    
    lkp_file_df <- lookup_df[as.character(lookup_df[[match_col]]) == ufn_match, ]
    if (nrow(lkp_file_df) == 0) {
      stop(add_ts("No matches found for this value"))
    }
    
    # Loop across loop_classes
    for (lc in loop_classes) {
      
      # Create labels
      if (is.na(class_label)) class_label <- class_name
      if (is.null(loop_labels)) {
        loop_label <- ""
      } else {
        loop_label <- paste0("_", tolower(clean_string(loop_col)), "-", clean_string(lc))
      }
      if (verbose) message_ts("Working on loop class ", lc, " (label = ", loop_label, ")...")
      
      # Subset df
      lkp_df <- lkp_file_df[lkp_file_df[[class_col]] == class_name & lkp_file_df[[loop_col]] == lc, ]
      if (nrow(lkp_df) != 1) {
        stop(add_ts("Incorrect number of matches found: ", nrow(lkp_df)))
      }
      
      # Check if already processed
      out_fn <- gsub("filled-[0,9]+", paste0("filled-", class_label, loop_label), ufn)
      out_file <- file.path(output_dir, out_fn)
      # Check file existence
      if (file.exists(out_file) & overwrite != TRUE) {
        
        processed_files <- c(processed_files, out_file)
        if (verbose) message_ts(basename(out_file), " already processed and overwrite not set to TRUE. Moving to next...")
        next
        
      }
      
      
      # Load raster
      uid_rst <- rast(uf)
      
      # Get fill values
      if (verbose) message_ts("Filling...")
      val <- lkp_df[[value_col]][lkp_df[[class_col]] == class_name]
      
      if (value_adjustment == "coverage") {
        
        adj_val <- val * sum(terra::values(uid_rst)) / ncell(uid_rst)
        
      } else if (is.numeric(value_adjustment)) {
        
        adj_val <- val * value_adjustment  
        
      } else {
        
        stop(add_ts("Argument 'value_adjustment' invald format"))
        
      }
      
      # Fill
      # If matrix, need a constant grid
      if (is.matrix(adj_val)) {
        
        # Create/load constant grid
        constant_file <- gsub("filled-[0,9]+", "filled-1", uf)
        if (!file.exists(constant_file) | overwrite == TRUE) {
          
          if (verbose) message_ts("Creating constant file...")
          constant_rst <- uid_rst
          terra::values(constant_rst)[!is.na(terra::values(constant_rst))] <- 1
          writeRaster(constant_rst, constant_file, overwrite = TRUE)
          
        } else {
          
          constant_rst <- rast(constant_file)
          out_rst <- uid_rst
          terra::values(out_rst)[!is.na(terra::values(out_rst))] <- val
          writeRaster(out_rst, out_file, overwrite = TRUE)
          
        }
        
        out_rst <- uid_rst * adj_val
         
      # If one value
      } else if (is.numeric (adj_val)) {
        
        out_rst <- uid_rst
        terra::values(out_rst)[!is.na(terra::values(out_rst))] <- adj_val
        
      } else {
        
        stop(add_ts("Value in wrong format"))
        
      }
      
      writeRaster(out_rst, out_file, overwrite = TRUE)
      processed_files <- c(processed_files, out_file)
  
    }
    
  }
  
  return(processed_files)
  
}


# uid_files <- file.path(cell_dir, paste0("cell_", rep(uids_subset[1:2], each = 2), 
#                                         "_buffered-", c("250m", "5000m"), "_filled-0.tif"))
# impose_water_lookup(uid_files = uid_files, 
#                     lookup_file = file.path(grid_dir, "uid_full_lookup.rds"), 
#                     match_strings = as.character(rep(uids_subset[1:2], each = 2)), 
#                     match_col = "UID",
#                     class_col = "ClassName", 
#                     class_name = "Semi-permanent Wetland", 
#                     value_col = "AvgWater", 
#                     loop_col = "Month", 
#                     loop_classes = month.abb,
#                     class_label = "semiperm", 
#                     value_adjustment = "coverage", 
#                     output_dir = imp_dir, 
#                     overwrite = FALSE,
#                     verbose = TRUE)
