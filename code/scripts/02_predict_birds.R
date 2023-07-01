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
check_dir(c(log_dir, cell_dir, anl_dir, wxl_dir, fcl_dir, prd_dir, stat_dir, cell_stat_dir), create = TRUE)

# Global options
global_overwrite <- FALSE
global_verbose <- FALSE

# Specify the (monthly) long term average water file(s) to process
water_files <- file.path(wtr_dir, "valley_average_Apr_2011-2021_snapped.tif")

# Select uids
uid_df <- read.csv(file.path(grid_dir, "unique_ids_suitable.csv"))
uids_ag <- uid_df$UID[uid_df$Landcover != "GrassPasture"]
uids_grass <- uid_df$UID[uid_df$Landcover == "GrassPasture"]

# Set uids to use
#uids <- uids_ag[1:1000]

# Every 10th
#uids_subset <- uids_ag[seq(1, length(uids_ag), 10)] #starting at 1
uids_subset <- uids_ag[seq(5, length(uids_ag), 10)] #starting at 5

# Unprocessed
#uid_files_processed <- list.files(imp_dir, pattern = ".tif$")
#uids_processed <- extract_subelement(strsplit(uid_files_processed, "_"), 2)
#uids_subset <- uids_subset[!(uids_subset %in% uids_processed)]

# Split uids into chunks of chunk_size for processing
# Compromise between max speed (chunks = n_cores) and error resistance /
#   getting some results quick (chunks = 1 per uid)
chunk_size <- 10
n_splits <- ceiling(length(uids_subset) / chunk_size)

# Randomize files to prevent a large bloc of unprocessed files from being assigned to the same chunk
uids_split <- chunk(sample(uids_subset), n_splits)

# Set number of processes
# Caps at number of cores - 1
cores_to_use <- 16
n_sessions <- min(length(uids_subset), cores_to_use, availableCores() - 1)

handlers(global = TRUE)
handlers(handler_progress(
  format = "[:bar] :percent (:current/:total) - Elapsed: :elapsed, ETA: :eta",
  clear = FALSE))

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
    
      # Buffer uids
      fxn <- "buffer-uid"
      uid_files <- buffer_uid_grid(uid_raster = file.path(grid_dir, "unique_ids_masked.tif"), 
                                   uids = uids, #[[n]], #uids_split[[1]][1:3]
                                   buffer_distances = c(250, 5000), 
                                   fill_values = c(0, 0), 
                                   fill_labels = NA,
                                   output_dir = cell_dir, 
                                   overwrite = overwrite,
                                   verbose = show_msg_fxn)
      
      if (show_msg_prg) p(add_ts("Buffered uid_files (", length(uid_files), "): ", paste0(basename(uid_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      #if (show_msg_prg) p(add_ts("Buffering complete. Imposing water..."), class = "sticky", amount = 0)
      p(amount = 0.05 * prg_mult * global_prog_mult)
    
      # Lookup imposed flooding and impose to cell
      cells <- extract_subelement(strsplit(uid_files, "_"), 2)
      fxn <- "impose-water-lookup"
      imp_files <- impose_water_lookup(uid_files = uid_files, 
                                       lookup_file = file.path(grid_dir, "uid_full_lookup.rds"), 
                                       match_strings = cells, 
                                       match_col = "UID",
                                       class_col = "ClassName", 
                                       class_name = "Semi-permanent Wetland", 
                                       value_col = "AvgWater", 
                                       loop_col = "Month", 
                                       loop_classes = month.abb,
                                       class_label = "semiperm", 
                                       value_adjustment = "coverage", 
                                       output_dir = imp_dir, 
                                       overwrite = overwrite,
                                       verbose = show_msg_fxn)

      if (show_msg_prg) p(add_ts("imp_files (", length(imp_files), "): ", paste0(basename(imp_files), collapse = ", ")), 
                     class = "sticky", amount = 0)
      #if (show_msg_prg) p(add_ts("Water imposed"), class = "sticky", amount = 0)
      p(amount = 0.15 * prg_mult * global_prog_mult)
  
      #imp_files <- imp_files[grepl("-Feb.tif", imp_files)]
      
      # Overlay imposed water on pre-calculated valley-wide focal water
      fxn <- "overlay-imposed"
      fcl_imp_files <- overlay_imposed_on_neighborhood(imposed_files = imp_files,
                                                       focal_files = lt_fcl_files, #[grepl("_Feb_", lt_fcl_files)],
                                                       output_dir = fcl_dir,
                                                       focal_filter = "valley_2013-2022",
                                                       overwrite = overwrite,
                                                       verbose = show_msg_fxn)

      if (show_msg_prg) p(add_ts("fcl_imp_files (", length(fcl_imp_files), "): ",
                                 paste0(basename(fcl_imp_files), collapse = ", ")),
                          class = "sticky", amount = 0)
      p(amount = 0.2 * prg_mult * global_prog_mult)

      # Create predictions
      fxn <- "predict-birds"
      prd_files <- predict_birds(water_files_longterm = fcl_imp_files,
                                 scenarios = "year-2013-2022",
                                 water_months = month.abb[1:12], #c("Apr"),
                                 model_files = shorebird_model_files_long,
                                 model_names = shorebird_model_names_long,
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

# Run sequentially (testing)
plan(sequential)
evaluation <- evaluate_cells(uids_subset[1:2], verbose_level = 2, retry_times = 1)




# Run multisession
plan(multisession, workers = n_sessions)
evaluation <- evaluate_cells(uids_split, verbose_level = 1, retry_times = 1, overwrite = FALSE)






test_dofuture <- function(uid_list) {
  
  p <- progressor(along = uid_list)
  foreach(uids = uid_list) %dofuture% {
    p(add_ts("Working on UID group: ", paste0(uids, collapse = ", "), "\nfoo\n"), class = "sticky")
    #p(add_ts("shorter_message\nfoobar"), class = "sticky")
    
    Sys.sleep(2)
  }
}
#handlers(handler_progress(width = 200))

plan(multisession, workers = 4)
result <- test_dofuture(uids_split[1:12])



library(future.apply)


test_flapply <- function(uid_list) {
  
  p <- progressor(along = uid_list)
  future_lapply(uid_list, function(uids, ...) {
    p(add_ts("Working on UIDS: ", paste0(uids, collapse = ", ")), class = "sticky")
    Sys.sleep(2)
  })
}

plan(multisession, workers = 4)
test_flapply(uids_split[1:12])


library(parallel)
library(MASS)
test_parallel <- function(uid_list, ncores) {
  
  p <- progressor(along = uid_list)
  cl <- setup_cluster(min(ncores, length(uid_list)))
  tryCatch({
    out <- parLapply(cl, uid_list, function(x) {
      message_ts("UIDS: ", paste0(x, collapse = ", "))
      Sys.sleep(4)
      return(x)
    })
    }, finally = {
      stopCluster(cl)
    })
    
  res <- unlist(out)
  return(res)
  
}

test_parallel(uids_split[1:12], 4)




plan(multisession, workers = 4)
evaluate_cells(uids_split[1:4])
  
f <- list()
for (n in 1:n_splits) {
	
	f[[n]] <- future({
		
	  # Message
	  pid <- Sys.getpid()
	  timestr <- format(Sys.time(), format = "%Y-%m-%d_%H%M")
	  #log_file <- file.path(log_dir, paste0("log_", timestr, "_", pid, ".txt"))
	  #sink(log_file, split = TRUE)
  	  
  	print_ts("Running on process ID: ", pid)
	  
  	# Buffer uids
  	uid_files <- buffer_uid_grid(uid_raster = file.path(grid_dir, "unique_ids_masked.tif"), 
  	                             uids = uids_split[[1]][1:4], #[[n]], #uids_split[[1]][1:3]
  	                             buffer_distances = c(250, 5000), 
  	                             fill_values = c(0, 0), 
  	                             fill_labels = NA,
  	                             output_dir = cell_dir, 
  	                             overwrite = global_overwrite)
  	
  	imp_files <- impose_water_lookup(uid_files = uid_files, 
  	                                 lookup_file = file.path(grid_dir, "uid_full_lookup.rds"), 
  	                                 match_strings = extract_subelement(strsplit(uid_files, "_"), 2), 
  	                                 match_col = "UID",
  	                                 class_col = "ClassName", 
  	                                 class_name = "Semi-permanent Wetland", 
  	                                 value_col = "AvgWater", 
  	                                 loop_col = "Month", 
  	                                 loop_classes = month.abb,
  	                                 class_label = "semiperm", 
  	                                 value_adjustment = "coverage", 
  	                                 output_dir = imp_dir, 
  	                                 overwrite = global_overwrite)
  	
	  # Overlay (add) imposed values on focal wetland rasters
	  wxl_files <- overlay_water_landcover(water_files, 
	                                       model_lc_files, #from definitions,
	                                       uid_raster = file.path(grid_dir, "unique_ids_masked.tif"),
	                                       uids = uids_split[[n]],
	                                       lookup_file = file.path(grid_dir, "uid_full_lookup.rds"),
	                                       imposed_landcover = "WetlandNatural",
	                                       cell_dir = cell_dir, 
	                                       output_dir = wxl_dir, 
	                                       overwrite = global_overwrite)
	                                       #printmessage = "print") #implement for logging
	  
	  # Overlay (subtract) imposed values from other landcover rasters based on original landcover
  	
  	# Create mean neighborhood water rasters
  	# Function defined in functions/water_moving_window.R
  	fcl_files <- c(fcl_wetland_files, fcl_other_files)
  	
  	# Predict							 
  	prd_files <- predict_bird_rasters(water_files_longterm = fcl_files,                  
  	                                  scenarios = "2011-2021",
  	                                  water_months = c("Apr"),
  	                                  model_files = shorebird_model_files_long, 
  	                                  model_names = shorebird_model_names_long, 
  	                                  static_cov_files = bird_model_cov_files, 
  	                                  static_cov_names = bird_model_cov_names,
  	                                  monthly_cov_files = tmax_files,
  	                                  monthly_cov_months = tmax_months,
  	                                  monthly_cov_names = tmax_names,
  	                                  output_dir = prd_dir,
  	                                  on_error_rerun = TRUE)
  	
  	# Column that contains the names of the fields to extract prediction data for
  	# Fields with the same name in a flooding area are grouped
  	area_5k_files <-  file.path(cell_dir, paste0("cell_", uids_split[[n]], "_buffered5k.tif"))
  	stat_files <- extract_predictions(prd_files, 
  	                                  area_files = area_5k_files,
  	                                  n_predictions = 8,
  	                                  output_dir = cell_stat_dir)
	  
  	#sink()
  	
  })
}

# Shows info on the future
f

# Shows the values
value(f)

# Read and combine all stats
message_ts("Reading and combining data...")
stat_files <- list.files(cell_stat_dir, pattern = "summary.rds$", full.names = TRUE)
long_df <- do.call(rbind, lapply(stat_files, function(x) readRDS(x)))
saveRDS(long_df, file.path(stat_dir, "cell_summary_long.rds"))
write.csv(long_df, file.path(stat_dir, "cell_summary_long.csv"), row.names = FALSE)


