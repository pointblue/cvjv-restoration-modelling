# Low-level functions for water tracker project, including logging and filename parsing
#  
# Point Blue, California Rice Commission

# Shared Functions --------------------

# Basic logging function to add a timestamp to strings
add_ts <- function(...) paste0("[", Sys.time(), "] - ", ...)

# Add as wrapper for base 'message' function
message_ts <- function(...) message(add_ts(...))

# Add as wrapper for base 'print' function
# Futures can't capture messages (output to stderr) so use print instead
print_ts <- function(...) print(add_ts(...))

printmessage_ts <- function(..., type = "message") {
  if (type == "message") {
    message_ts(...)
  } else {
    print_ts(...)
  }
}

# Function to check directories exist, optionally loading
check_dir <- function(directory, create = FALSE, verbose = FALSE) {
  
  for (d in directory) {
    
    if (!file.exists(d)) {
      
      if (create == FALSE) {
        
        stop(add_ts("Directory", d, "is required but does not exist."))
        
      } else {
        
        message_ts("Creating directory ", d)
        dir.create(d)
        
      }
      
    } else if (verbose == TRUE) {
      
      message_ts("Directory", d, "exists.")
      
    } 
    
  }
  
}

# Function to get the nth subelement from each item in a list
extract_subelement <- function(x, element) sapply(x, `[[`, element) 

# Function to make a string filename safe
clean_string <- function(x, sub_char = "-", ...) {
  y <- gsub("[^a-zA-Z0-9_\\-]", sub_char, x, ...)
  
  # Condense multiple sub_chars
  y <- gsub(paste0(sub_char, "+"), sub_char, y, ...)
  
  # Drop terminal sub_char
  y <- gsub(paste0(sub_char, "$"), "", y, ...)
  
  return(y)
}

# Function to make a string filename safe
clean_string_remove_underscores <- function(x, sub_char = "-", ...) {
  y <- gsub("[^a-zA-Z0-9\\-]", sub_char, x, ...)
  
  # Condense multiple sub_chars
  y <- gsub(paste0(sub_char, "+"), sub_char, y, ...)
  
  # Drop terminal sub_char
  y <- gsub(paste0(sub_char, "$"), "", y, ...)
  
  return(y)
}

# Function to split a vector into n chunks of equal size
chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 

# Function to parse the filenames of project files
parse_filename <- function(files) {
  
  paths <- dirname(files)
  fns <- basename(files)
  
  # Extension
  ext_pos <- regexpr("\\.([[:alnum:]]+)$", fns)
  exts <- ifelse(ext_pos > -1L, substring(fns, ext_pos + 1L), "")
  fns_no_ext <- ifelse(ext_pos > -1L, substring(fns, 1, ext_pos - 1L), fns)
  
  # Order of data in filenames is location, descriptor, year, month, additional info
  fns_split <- strsplit(fns_no_ext, "_")
  locs <- extract_subelement(fns_split, 1)
  desc <- extract_subelement(fns_split, 2)
  yrs <- extract_subelement(fns_split, 3)
  mths <- extract_subelement(fns_split, 4)
  
  # Amount of additional info not set; could be none or could be 3-4
  n_splits <- length(fns_split[[1]])
  info <- paste(ifelse(n_splits > 4, extract_subelement(fns_split, 5), ""),
                ifelse(n_splits > 5, extract_subelement(fns_split, 6), ""),
                ifelse(n_splits > 6, extract_subelement(fns_split, 7), ""),
                sep = "_")
  
  # Combine into data frame and export
  file_df <- data.frame("File" = files,
                        "Path" = paths, 
                        "Filename" = fns,
                        "Extension" = exts,
                        "Location" = locs,
                        "Description" = desc,
                        "Year" = yrs,
                        "Month" = mths,
                        "AdditionalInfo" = info)
  
  return(file_df)
  
}


# Function to more quickly trim rasters
# raster::trim is very slow
trim_faster <- function(x, out = "raster"){
  
  if(!require(terra)) stop(add_ts("Package 'terra' is required"))
  
  # Check inputs
  if(!(class(x) %in% c("RasterLayer", "matrix"))) stop("Input must be a raster or matrix")
  if(!(out %in% c("raster", "matrix"))) stop("Output must be a raster or matrix")
  if(class(x) == "matrix" & out == "raster") stop("if you supply a matrix, you must use out='matrix'")
  
  # Convert to matrix
  if(class(x) == "RasterLayer") {
    if(out == "raster") { 
      cres <- 0.5 * res(x)
      crs <- projection(x)
      ref_rst <- x 
    }
    x <- matrix(raster::as.array(x), nrow = nrow(x), ncol = ncol(x))
  }
  
  # Check rows and columns for NAs
  na_rows <- apply(x, MARGIN = 1, FUN = function(x) { all(is.na(x)) })
  na_cols <- apply(x, MARGIN = 2, FUN = function(x) { all(is.na(x)) })
  
  # Find first/last non-NA rows and columns
  r1 <- min(which(!na_rows))
  r2 <- max(which(!na_rows))
  c1 <- min(which(!na_cols))
  c2 <- max(which(!na_cols))
  
  # Subset matrix
  x <- x[r1:r2, c1:c2]
  
  # Reformat as raster
  if (out == "raster") {
    xs <- xFromCol(ref_rst, col = c(c1, c2)) + c(-1, 1) * cres[1]
    ys <- yFromRow(ref_rst, row = c(r2, r1)) + c(-1, 1) * cres[2]
    x <- rast(x, xmn = xs[1], xmx = xs[2], ymn = ys[1], ymx = ys[2], crs = crs)
  }
  
  return(x)
  
}


# Different rectangular buffer method
# Buff dist in map units (usually m)
buffer_rect <- function(in_rst, buffer_dist, match_value = 1, fill_value = 0, out_file = NULL, overwrite = FALSE) {
  
  # Load required packages
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Check simple parameters
  if (!is.logical(overwrite)) stop(add_ts("Argument 'overwrite' must be TRUE or FALSE"))
  if (!is.numeric(buffer_dist)) stop(add_ts("Argument 'buffer_dist' must be numeric"))
  if (!is.numeric(match_value)) stop(add_ts("Argument 'match_value' must be numeric"))
  if (!is.na(fill_value) & !is.numeric(fill_value)) stop(add_ts("Argument 'fill_value' must be numeric or NA"))
  
  # Check path of out_file
  if (!is.null(out_file)) {
    
    out_path <- dirname(out_file)
    if(!file.exists(out_path)) stop(add_ts(out_path, " does not exist."))
    
    if (file.exists(out_file) & overwrite != TRUE) {
      message_ts("out_file already exists and overwrite != TRUE. Exiting...")
      return(rast(out_file))
    }
    
  } 
  
  # Check and load guide raster
  if (class(in_rst) == "SpatRaster") {
    
    match_rst <- in_rst
    
  } else if (is.character(in_rst)) {
    
    if (length(rst) != 1) stop(add_ts("in_rst be a single raster or filename"))
    if (!file.exists(rst)) stop(add_ts("in_rst does not exist: ", in_rst, " not found."))
    
    match_rst <- rast(in_rst)
    
  } else {
    
    stop(add_ts("in_rst must be a raster or filename of a raster"))
    
  }
  
  # Calculate number of cells
  message_ts("Converting distance of ", buffer_dist, " map units to cells...")
  xres <- res(match_rst)[1]
  yres <- res(match_rst)[2]
  x_dist <- round(buffer_dist / xres) * xres
  y_dist <- round(buffer_dist / yres) * yres
  
  # Get coordinates
  match_cells <- unlist(cells(match_rst, match_value))
  match_xys <- xyFromCell(match_rst, match_cells)
  
  xmin <- min(match_xys[,1]) - x_dist - xres / 2
  xmax <- max(match_xys[,1]) + x_dist - xres / 2
  ymin <- min(match_xys[,2]) - y_dist - yres / 2
  ymax <- max(match_xys[,2]) + y_dist - yres / 2
  
  # Buffer
  message_ts("Buffering...")
  buff_rst <- rast(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                   crs = crs(match_rst), resolution = c(xres, yres), vals = fill_value)
  buff_rst <- crop(buff_rst, match_rst) #needed if near edge
  match_rst <- crop(match_rst, buff_rst) #drop wide swath of areas outside buffer
  
  # Combine
  combined_rst <- lapp(c(match_rst, buff_rst), fun = function(x, y) {
    ifelse(!is.na(x) & x == match_value, 1, y)
  }, filename = out_file, overwrite = TRUE)
  
  message_ts("Complete")
  return(combined_rst)
  
}

# Simple version of buffer--within a certain number of cells
# Overbuffers: buffers as a square rather than circle, including cells on diagonal that are ok

buffer_simple <- function(x, width = 1, buffer_value = 1) UseMethod("buffer_simple")

buffer_simple.default <- function(x, width = 1, buffer_value = 1) {
  stop(add_ts("Incompatible data type: must be numeric vector, matrix, or SpatRaster."))
}

# Numeric method for simple buffer
buffer_simple.numeric <- function(x, width = 1, buffer_value = 1) {
  
  # Report
  #message(message_debug_enter())
  
  # Determine rows in the column that have the mask value (1)
  matches_value <- which(x == 1)
  
  # Calculate the rows within a given number of cells of the mask values
  in_range_min <- pmax(1, matches_value - width)
  in_range_max <- pmin(length(x), matches_value + width)
  in_range <- Map(`:`, in_range_min, in_range_max)
  in_range <- unique(unlist(in_range, use.names = FALSE))
  
  # Replace values in range with the mask value
  x[in_range] <- buffer_value

  return(x)
  
}

# Matrix method for simple buffer. Apply buffer to rows and columns 
#   by calling numeric (vector) method.
buffer_simple.matrix <- function(mat, width = 1, buffer_value = 1) {
  
  
  # Check if input is numeric; this lets us specify the method for the buffering below for a small performance gain
  if (!is.numeric(mat)) stop(add_ts("Matrix is not numeric; only numeric matrices can be buffered."))
  
  # Buffer within columns
  m1 <- apply(mat, 2, FUN = buffer_simple.numeric, width = width, buffer_value = buffer_value)
  #print(m1)
  
  # Buffer within rows; need to transpose because apply is weird
  m2 <- t(apply(m1, 1, FUN = buffer_simple.numeric, width = width, buffer_value = buffer_value))
  #print(m2)
  
  return(m2)
  
}

# Raster method for simple buffer. Converts to matrix, buffers, and
#   turns back into raster.
buffer_simple.SpatRaster <- function(rst, width = 1, buffer_value = 1) {
  
  if (!require(terra)) stop(add_ts("Library terra is required"))
  
  # Turn into matrix
  mat <- as.matrix(rst)
  
  # Buffer
  mat_buf <- buffer_simple(mat, width = width, buffer_value = buffer_value)
  
  # Convert back to raster
  rst_buf <- raster(mat_buf, template = rst)
  
  return(rst_buf)
  
}



