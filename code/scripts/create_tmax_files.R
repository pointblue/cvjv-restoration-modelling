# Script to snap tmax files


# Load packages
library(terra)
library(future)
library(doFuture)
library(progressr)

# Set temp directory for terra on non-boot drive if possible
terraOptions(tempdir = "E:/rtemp")

# Load definitions and code
code_dir <- "V:/Project/wetland/FWSPartners/code/cvjv-restoration-modelling/code"
def_file <- file.path(code_dir, "definitions.R")
code_files <- list.files(file.path(code_dir, "functions"), pattern = ".*R$", full.names = TRUE)
sapply(c(def_file, code_files), FUN = function(x) source(x))



# Load tmax files
#tmax_dir <- "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/tmax"
tmax_dir <- "V:/Data/climate/FlintBCM/version8/tmx"
tmax_files <- list.files(tmax_dir, pattern = "tmx.*asc$")

# Ref file
ref_file
test_rst <- rast("V:/Data/climate/FlintBCM/version8/projection_defined.tif")
in_crs <- crs(test_rst)

# Set number of processes
# Caps at number of cores - 1
cores_to_use <- 12
n_sessions <- min(12, cores_to_use, availableCores() - 1)

handlers(global = TRUE)
handlers(handler_progress(
  format = "[:bar] :percent (:current/:total) - Elapsed: :elapsed, ETA: :eta",
  clear = FALSE))


average_and_snap <- function(in_dir, fn_start, lbl, mths, years,
                              ref_file, output_dir, in_crs = NULL,
                             overwrite = FALSE, verbose = TRUE) {
  
  p <- progressor(steps = length(mths) * 3, auto_finish = FALSE)
  
  foreach(mth = mths) %dofuture% {
    
    # Check files
    if (!file.exists(ref_file)) stop(add_ts("ref_file does not exist"))
    in_files <- file.path(in_dir, paste0(fn_start, years, tolower(mth), ".asc"))
    if (!all(file.exists(in_files))) stop(add_ts("Missing files"))
    
    # Report
    if (verbose) p(add_ts("Started processing ", mth), class = "sticky", amount = 0)
    
    # Load files
    ref_rst <- rast(ref_file)
    
    in_rst <- rast(in_files)
    if (!is.null(in_crs)) crs(in_rst) <- in_crs
    
    # Crop
    crp_file <- file.path(output_dir, paste0(lbl, "_", min(years), "-", max(years), "_", mth, "_cropped.tif"))
    if (file.exists(crp_file) & overwrite != TRUE) {
      
      if (verbose) p(add_ts("File ", basename(crp_file), " already created and overwrite != TRUE; loading"), 
                     class = "sticky", amount = 0)
      crp_rst <- rast(crp_file)
      
    } else {
      
      # Check projection
      if (crs(ref_rst) != crs(in_rst)) {
        
        ref_prj_file <- gsub(".tif", "_crsmatch.tif", ref_file)
        if (file.exists(ref_prj_file)) {
          ref_crp_rst <- rast(ref_prj_file)
        } else {
          if (verbose) p(add_ts("Reprojecting ref_rst to match in_rst for cropping"), class = "sticky", amount = 0)
          ref_crp_rst <- project(ref_rst, in_rst, filename = ref_prj_file)
        }
        
        
      } else {
        
        ref_crp_rst <- ref_rst
        
      }
      
      if (verbose) p(add_ts("Cropping to ", basename(crp_file), "..."), class = "sticky", amount = 0)
      crp_rst <- crop(in_rst, ref_crp_rst, filename = crp_file, overwrite = TRUE)
      if (verbose) p(add_ts("File ", basename(crp_file), " cropped."), class = "sticky", amount = 0)
      
    }
    
    p(amount = 1)
    
    
    # Average
    avg_file <- file.path(output_dir, paste0(lbl, "_", min(years), "-", max(years), "_", mth, "_mean.tif"))
    if (file.exists(avg_file) & overwrite != TRUE) {
      
      if (verbose) p(add_ts("File ", basename(avg_file), " already created and overwrite != TRUE; loading"), 
                     class = "sticky", amount = 0)
      avg_rst <- rast(avg_file)
      
    } else {
      
      if (verbose) p(add_ts("Calculating average to ", basename(avg_file), "..."), class = "sticky", amount = 0)
      avg_rst <- app(crp_rst, fun = "mean", na.rm = TRUE, filename = avg_file, overwrite = TRUE)
      if (verbose) p(add_ts("File ", basename(avg_file), " averaged."), class = "sticky", amount = 0)
      
    }
    
    # Check projection and assign if missing (not sure why it would be, but it was)
    if (crs(avg_rst) == "") {
      
      if (verbose) p(add_ts("Assigning missing projection for ", basename(avg_file), "..."), class = "sticky", amount = 0)
      crs(avg_rst) <- in_crs
      
    }
    
    p(amount = 1)
    
    
    # Reproject / resample
    snp_file <- file.path(output_dir, paste0(lbl, "_", min(years), "-", max(years), "_", mth, "_mean_reproj.tif"))
    if (file.exists(snp_file) & overwrite != TRUE) {
      
      if (verbose) p(add_ts("File ", basename(snp_file), " already created and overwrite != TRUE; loading"), 
                     class = "sticky", amount = 0)
      avg_snp_rst <- rast(snp_file)
      
    } else {
      
      # Reproject if needed
      if (crs(avg_rst) != crs(ref_rst)) {
        
        if (verbose) p(add_ts("Reprojecting and snapping to ", basename(snp_file), "..."), class = "sticky", amount = 0)
        avg_snp_rst <- project(avg_rst, ref_rst, filename = snp_file, overwrite = TRUE)
        if (verbose) p(add_ts("File ", basename(snp_file), " reprojected."), class = "sticky", amount = 0)
        
      # Otherwise just resample
      } else {
        
        if (verbose) p(add_ts("Resampling to ", basename(snp_file), "..."), class = "sticky", amount = 0)
        avg_snp_rst <- resample(avg_rst, ref_rst, filename = snp_file, overwrite = TRUE)
        if (verbose) p(add_ts("File ", basename(snp_file), " resampled."), class = "sticky", amount = 0)
        
      }
      
      
    }
    
    p(amount = 1)
    
  }
  
}

# Run sequentially (testing)
plan(sequential)
evaluation <- average_and_snap(tmax_dir, 
                               fn_start = "tmx", 
                               lbl = "tmax", 
                               mths = month.abb[1], 
                               years = 2019:2020, #1991:2020,
                               in_crs = in_crs,
                               ref_file = ref_file, 
                               output_dir = cov_dir, 
                               overwrite = FALSE, 
                               verbose = TRUE)



# Run multisession
plan(multisession, workers = 12)
evaluation <- average_and_snap(tmax_dir, 
                               fn_start = "tmx", lbl = "tmax", 
                               mths = month.abb[1:12], years = 1991:2020,
                               ref_file = ref_file, 
                               output_dir = cov_dir, 
                               overwrite = FALSE, 
                               verbose = TRUE)


