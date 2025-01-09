# Create bird predictions for specified cells
# Uses package 'future' to split between multiple cores

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

# Check directories, creating if missing
check_dir(c(log_dir, cell_dir, anl_dir, wxl_dir, fcl_dir, prd_dir, stat_dir, stat_cell_dir), create = TRUE)

# Global options
global_overwrite <- FALSE
global_verbose <- FALSE

# Specify the (monthly) long term average water file(s) to process
water_files <- file.path(wtr_dir, "valley_average_Apr_2011-2021_snapped.tif")

# Select uids

# Unprocessed
uids_processed_file <- file.path(anl_dir, "uids_processed.rds") 
if (!file.exists(uids_processed_file) | global_overwrite == TRUE) {
  message_ts("Determining which UIDs have been processed")
  
  #uid_files_processed <- list.files(imp_dir, pattern = ".tif$")
  #uids_processed <- extract_subelement(strsplit(uid_files_processed, "_"), 2)
  #uids_subset <- uids_subset[!(uids_subset %in% uids_processed)]
  #uids_subset <- uids_processed
  
  uid_files_processed <- list.files(prd_dir, pattern = ".tif$")
  files_processed_shorebirds <- uid_files_processed[grepl("((AMAV)|(BNST)|(DOWI)|(DUNL))", uid_files_processed)]
  uids_shorebirds <- unique(extract_subelement(strsplit(files_processed_shorebirds, "_"), 1))
  files_processed_ducks <- uid_files_processed[grepl("((ANAC)|(ANCL)|(ANCR))", uid_files_processed)]
  #uids_ducks <- unique(extract_subelement(strsplit(files_processed_ducks, "_"), 1))
  uids_ducks <- table(extract_subelement(strsplit(files_processed_ducks, "_"), 1))
  uids_ducks_complete <- names(uids_ducks[uids_ducks == 72])
  uids_subset <- uids_shorebirds[!(uids_shorebirds %in% uids_ducks_complete)]
  uids_subset <- gsub("cell-", "", uids_subset)
  
  saveRDS(uids_subset, uids_processed_file)
  
} else {
  
  message_ts("Loading processed UIDs from file")
  uids_subset <- readRDS(uids_processed_file)
  
}

# Split uids into chunks of chunk_size for processing
# Compromise between max speed (n_splits = n_cores) and error resistance /
#   getting some results quick (n_splits = 1 per uid)
chunk_size <- 10
n_splits <- ceiling(length(uids_subset) / chunk_size)

# Randomize files to prevent a large bloc of unprocessed files from being assigned to the same chunk
uids_split <- chunk(sample(uids_subset), n_splits)

# Get all available focal files
fcl_imp_list_file <- file.path(fcl_dir, "focal_file_list.rds")
if (!file.exists(fcl_imp_list_file) | global_overwrite == TRUE) {
  message_ts("Getting list of focal rasters")
  fcl_imp_files_all <- list.files(fcl_dir, pattern = ".tif$", full.names = TRUE)
  saveRDS(fcl_imp_files_all, fcl_imp_list_file)
} else {
  message_ts("Loading list of focal rasters from file")
  fcl_imp_files_all <- readRDS(fcl_imp_list_file)
}

# Execute the code in n splits
evaluate_cells <- function(uid_list, overwrite = FALSE, retry_times = 0, retry_counter = NULL,
                           verbose_level = 1, p = NULL, global_prog_mult = 1) {
  
  
  uid_list_len <- length(uid_list)
  uid_sublist_len <- mean(unlist(lapply(uid_list, length)))
  
  if (is.null(p)) p <- progressor(steps = uid_list_len * uid_sublist_len, auto_finish = FALSE)
  
  if (retry_times > 2) stop(add_ts("Only supports two levels of retry upon error."))
  if (is.null(retry_counter)) { 
    retry_counter <- 0
  } else {
    retry_counter <- retry_counter + 1
  }
  
  if (verbose_level > 1) {
    show_msg_prg <- TRUE
    show_msg_fxn <- TRUE
  } else if (verbose_level == 1) {
    show_msg_prg <- TRUE
    show_msg_fxn <- FALSE
  } else {
    show_msg_prg <- FALSE
    show_msg_fxn <- FALSE
  }
  
  if (retry_counter > 0) p(add_ts("Retry count: ", retry_counter), class = "sticky", amount = 0)
  
  foreach(uids = uid_list) %dofuture% {
  #future_lapply(uid_list, function(uids, ...) {
    
    # Labels for messages
    fxn <- "setup"
    uid_set <- which(unname(unlist(lapply(uid_list, function(x) identical(x, uids)))))
    uid_set_len <- length(uids)
    if (uid_set_len > 1) {
      lbl <- paste0("Set ", uid_set, " of ", uid_list_len, " (start=", uids[1], ", n=", uid_set_len, ") - ")
      prg_mult <- uid_set_len
    } else {
      lbl <- paste0("Subset ", uid_set, " of ", uid_list_len, " (", uids[1], ") - ")
      prg_mult <- 1
    }
    if (show_msg_prg) p(add_ts(lbl, "started"), class = "sticky", amount = 0)
    
    # Streamlined error catching
    tryCatch({
    
      # Get focal water files for these uids
      fxn <- "get-focal-files"
      fcl_imp_files <- fcl_imp_files_all[grepl(paste0("((", paste0(uids, collapse = ")|("), "))"), fcl_imp_files_all)]
      if (show_msg_prg) p(add_ts("fcl_imp_files (", length(fcl_imp_files), "): ", paste0(basename(fcl_imp_files), collapse = ", ")),
                          class = "sticky", amount = 0)
      
      # Create predictions
      fxn <- "predict-birds"
      prd_files <- predict_birds(water_files_longterm = fcl_imp_files,
                                 scenarios = "year-2013-2022",
                                 water_months = month.abb[1:12], #c("Apr"),
                                 model_files = duck_model_files_long,
                                 model_names = duck_model_names_long,
                                 static_cov_files = bird_model_cov_files,
                                 static_cov_names = bird_model_cov_names,
                                 monthly_cov_files = tmax_files,
                                 monthly_cov_months = tmax_months,
                                 monthly_cov_names = tmax_names,
                                 output_dir = prd_dir,
                                 overwrite = overwrite,
                                 verbose = show_msg_fxn)

      if (show_msg_prg) p(add_ts("prd_files (", length(prd_files), "): ", paste0(basename(prd_files), collapse = ", ")),
                          class = "sticky", amount = 0)
      p(amount = 0.6 * prg_mult * global_prog_mult)
      
      }, error = function(e) {
        if (retry_counter == 0) {
          lbl <- ""
        } else if (retry_counter == 1) {
          lbl <- paste0("UID ", uids, " - ")
        } else {
          lbl <- paste0("UID ", uids, ", retry ", retry_counter, " - ")
        }

        p(add_ts("ERROR - ", fxn, " - ", lbl, e), class = "sticky", amount = 0)

        saveRDS(e, file.path(log_dir, paste0("error_function-", fxn, "_UID-", paste0(uids, collapse = "-"),
                                             "_retry-", retry_counter, "-of-", retry_times,
                                             "_date-", format(Sys.time(), format = "%Y-%m-%d"), ".rds")))

        # If retry request, loop across individual UIDs
        if (retry_times >= 1 & retry_counter == 0) {
          p(add_ts("Retrying UIDs in this set individually to find problematic UID(s)..."),
            class = "sticky", amount = 0)#.3 * prg_mult * -1)
          evaluate_cells(unlist(uids), overwrite = overwrite,
                         retry_times = min(retry_times, 2), retry_counter = retry_counter,
                         p = p, global_prog_mult = 0)
        # If repeating a second time, set overwrite to TRUE
        } else if (retry_times > 1 & retry_counter == 1) {
          p(add_ts("Retrying problematic UID with overwrite == TRUE..."), class = "sticky", amount = 0)#.3 * prg_mult * -1)
          evaluate_cells(unlist(uids), overwrite = TRUE,
                         retry_times = min(retry_times, 2), retry_counter = retry_counter,
                         p = p, global_prog_mult = 0)
        # If failed on last attempt (including an attempt that is first & only), remove progress for it
        } else {
          p(amount = -1 * prg_mult)
        }

      }
    )
    
    if (show_msg_prg) p(add_ts(lbl, "Processing complete."), class = "sticky", amount = 0)
    
  }
    
}

# Start reporting

handlers(global = TRUE)
handlers(handler_progress(
  format = "[:bar] :percent (:current/:total) - Elapsed: :elapsed, ETA: :eta",
  clear = FALSE))


# Run sequentially (testing)
#plan(sequential)
#evaluation <- evaluate_cells(uids_subset[1:2], verbose_level = 2, retry_times = 1)

# Set number of processes
# Caps at number of cores - 1
cores_to_use <- 8
n_sessions <- min(length(uids_subset), cores_to_use, availableCores() - 1)

# Run multisession
plan(multisession, workers = n_sessions)
evaluation <- evaluate_cells(uids_split, verbose_level = 1, retry_times = 1, overwrite = FALSE)




