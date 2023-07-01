
# Load packages
library(terra)

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



in_dir <- tmax_dir 
fn_start <- "tmx"
lbl <- "tmax"
mths <- month.abb[10]
years <- 1990:2019
output_dir <- cov_dir
overwrite <- FALSE
verbose <- TRUE


for (mth in mths) {
  
  message_ts("Working on month ", mth)
  
  # Check files
  if (!file.exists(ref_file)) stop(add_ts("ref_file does not exist"))
  in_files <- file.path(in_dir, paste0(fn_start, years, tolower(mth), ".asc"))
  if (!all(file.exists(in_files))) stop(add_ts("Missing files"))
  
  # Report
  if (verbose) message_ts("Started processing ", mth)
  
  # Load files
  ref_rst <- rast(ref_file)
  
  in_rst <- rast(in_files)
  if (!is.null(in_crs)) crs(in_rst) <- in_crs
  
  # Crop
  crp_file <- file.path(output_dir, paste0(lbl, "_", min(years), "-", max(years), "_", mth, "_cropped.tif"))
  if (file.exists(crp_file) & overwrite != TRUE) {
    
    if (verbose) message_ts("File ", basename(crp_file), " already created and overwrite != TRUE; loading")
    crp_rst <- rast(crp_file)
    
  } else {
    
    # Check projection
    if (crs(ref_rst) != crs(in_rst)) {
      
      ref_prj_file <- gsub(".tif", "_crsmatch.tif", ref_file)
      if (file.exists(ref_prj_file)) {
        ref_crp_rst <- rast(ref_prj_file)
      } else {
        if (verbose) message_ts("Reprojecting ref_rst to match in_rst for cropping")
        ref_crp_rst <- project(ref_rst, in_rst, filename = ref_prj_file)
      }
      
      
    } else {
      
      ref_crp_rst <- ref_rst
      
    }
    
    if (verbose)message_ts("Cropping to ", basename(crp_file), "...")
    crp_rst <- crop(in_rst, ref_crp_rst, filename = crp_file, overwrite = TRUE)
    if (verbose) message_ts("File ", basename(crp_file), " cropped.")
    
  }

  
  # Average
  avg_file <- file.path(output_dir, paste0(lbl, "_", min(years), "-", max(years), "_", mth, "_mean.tif"))
  if (file.exists(avg_file) & overwrite != TRUE) {
    
    if (verbose) message_ts("File ", basename(avg_file), " already created and overwrite != TRUE; loading")
    avg_rst <- rast(avg_file)
    
  } else {
    
    if (verbose) message_ts("Calculating average to ", basename(avg_file), "...")
    avg_rst <- app(crp_rst, fun = "mean", na.rm = TRUE, filename = avg_file, overwrite = TRUE)
    if (verbose) message_ts("File ", basename(avg_file), " averaged.")
    
  }
  
  if (crs(avg_rst) == "") {
    
    if (verbose) message_ts("Assigning missing projection for ", basename(avg_file), "...")
    crs(avg_rst) <- in_crs
    
  }

  # Reproject / resample
  snp_file <- file.path(output_dir, paste0(lbl, "_", min(years), "-", max(years), "_", mth, "_mean_snapped.tif"))
  if (file.exists(snp_file) & overwrite != TRUE) {
    
    if (verbose) message_ts("File ", basename(snp_file), " already created and overwrite != TRUE; loading")
    avg_snp_rst <- rast(snp_file)
    
  } else {
    
    # Reproject if needed
    if (crs(avg_rst) != crs(ref_rst)) {
      
      if (verbose) message_ts("Reprojecting and snapping to ", basename(snp_file), "...")
      avg_snp_rst <- project(avg_rst, ref_rst, filename = snp_file, overwrite = TRUE)
      if (verbose) message_ts("File ", basename(snp_file), " reprojected.")
      
      # Otherwise just resample
    } else {
      
      if (verbose) message_ts("Resampling to ", basename(snp_file), "...")
      avg_snp_rst <- resample(avg_rst, ref_rst, filename = snp_file, overwrite = TRUE)
      if (verbose) message_ts("File ", basename(snp_file), " resampled.")
      
    }
    
    
  }
  
}


