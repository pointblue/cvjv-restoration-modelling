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
uids_subset <- uids_ag[seq(1, length(uids_ag), 10)] #starting at 1
#uids_subset <- uids_ag[seq(5, length(uids_ag), 10)] #starting at 5

# Unprocessed
#uid_files_processed <- list.files(imp_dir, pattern = ".tif$")
#uids_processed <- extract_subelement(strsplit(uid_files_processed, "_"), 2)
#uids_subset <- uids_subset[!(uids_subset %in% uids_processed)]

# Split uids into chunks of chunk_size for processing
# Compromise between max speed (chunks = n_cores) and error resistance /
#   getting some results quick (chunks = 1 per uid)
chunk_size <- 10
n_splits <- ceiling(length(uids_subset) / 10)

# Randomize files to prevent a large bloc of unprocessed files from being assigned to the same chunk
uids_split <- chunk(sample(uids_subset), n_splits)

# Set number of processes
# Caps at number of cores - 1
cores_to_use <- 12
n_sessions <- min(length(uids_subset), cores_to_use, availableCores() - 1)

handlers(global = TRUE)
handlers(handler_progress(
  format = "[:bar] :percent (:current/:total) - Elapsed: :elapsed, ETA: :eta",
  clear = FALSE))

# Execute the code in n splits
evaluate_cells <- function(uid_list, retry = FALSE, verbose_level = 1, p = NULL, global_prog_mult = 1) {
  
  if (verbose_level > 1) {
    msg_prg <- TRUE
    msg_fxn <- TRUE
  } else if (verbose_level == 1) {
    msg_prg <- TRUE
    msg_fxn <- FALSE
  } else {
    msg_prg <- FALSE
    msg_fxn <- FALSE
  }
  
  uid_list_len <- length(uid_list)
  uid_sublist_len <- mean(unlist(lapply(uid_list, length)))
  
  if (is.null(p)) p <- progressor(steps = uid_list_len * uid_sublist_len, auto_finish = FALSE)
  foreach(uids = uid_list) %dofuture% {
  #future_lapply(uid_list, function(uids, ...) {
    
    # Labels for messages
    uid_set <- which(unname(unlist(lapply(uid_list, function(x) identical(x, uids)))))
    uid_set_len <- length(uids)
    if (uid_set_len > 1) {
      lbl <- paste0("UID set ", uid_set, " (start=", uids[1], ", n=", uid_set_len, ") - ")
      prg_mult <- uid_set_len * global_prog_mult
    } else {
      lbl <- paste0("UID subset ", uid_set, " - ")
      prg_mult <- 1 * global_prog_mult
    }
    if (msg_prg) p(add_ts(lbl, "started (out of ", uid_list_len, ")"), class = "sticky", amount = 0)
    
    # Buffer uids
    uid_files <- tryCatch({
                    buffer_uid_grid(uid_raster = file.path(grid_dir, "unique_ids_masked.tif"), 
                                   uids = uids, #[[n]], #uids_split[[1]][1:3]
                                   buffer_distances = c(250, 5000), 
                                   fill_values = c(0, 0), 
                                   fill_labels = NA,
                                   output_dir = cell_dir, 
                                   overwrite = global_overwrite,
                                   verbose = msg_fxn)
      }, error = function(e) p(add_ts("ERROR - buffer_uid_grid() - ", e), class = "sticky", amount = 0)
    )
    
    if (msg_prg) p(add_ts("Buffered uid_files (", length(uid_files), "): ", paste0(basename(uid_files), collapse = ", ")), 
                   class = "sticky", amount = 0)
    #if (msg_prg) p(add_ts("Buffering complete. Imposing water..."), class = "sticky", amount = 0)
    p(amount = 0.1 * prg_mult)
    
    # Lookup imposed flooding and impose to cell
    cells <- extract_subelement(strsplit(uid_files, "_"), 2)
    imp_files <- tryCatch({
                  impose_water_lookup(uid_files = uid_files, 
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
                                       overwrite = global_overwrite,
                                       verbose = msg_fxn)
      }, error = function(e) {
          p(add_ts("ERROR - impose_water_lookup() -", e), class = "sticky", amount = 0)
          saveRDS(e, file.path(log_dir, paste0("_ERROR_function-impose-water-lookup_UIDset-", uid_set, 
                                             "UIDs-", paste0(uids, collapse = "-"), ".rds")))
          if (retry) {
            p(add_ts("Retrying UIDs in this set individually..."), class = "sticky", amount = 0)
            evaluate_cells(unlist(uids), retry = FALSE, p = p, global_prog_mult = 0)
          }
      }
    )
    
    if (msg_prg) p(add_ts("imp_files (", length(imp_files), "): ", paste0(basename(imp_files), collapse = ", ")), 
                   class = "sticky", amount = 0)
    #if (msg_prg) p(add_ts("Water imposed"), class = "sticky", amount = 0)
    p(amount = 0.2 * prg_mult)

    imp_files <- imp_files[grepl("-Mar.tif", imp_files)]
    
    # Overlay imposed water on pre-calculated valley-wide focal water
    fcl_imp_files <- tryCatch({
                      overlay_imposed_on_neighborhood(imposed_files = imp_files, 
                                                       focal_files = lt_fcl_files[grepl("_Mar_", lt_fcl_files)], 
                                                       output_dir = fcl_dir, 
                                                       focal_filter = "valley_2013-2022",
                                                       overwrite = global_overwrite,
                                                       verbose = msg_fxn)
      }, error = function(e) {
        p(add_ts("ERROR - overlay_imposed_...() - ", e), class = "sticky", amount = 0)
        saveRDS(e, file.path(log_dir, paste0("_ERROR_function-overlay-imposed-on-neighborhood_", 
                                             "UIDs-", paste0(uids, collapse = "-"), ".rds")))
        if (retry) {
          p(add_ts("Retrying UIDs in this set individually..."), class = "sticky", amount = 0)#.3 * prg_mult * -1)
          evaluate_cells(unlist(uids), retry = FALSE, p = p, global_prog_mult = 0)
        }
      }
    )
    
    if (msg_prg) p(add_ts(lbl, " Processing complete."), class = "sticky", amount = 0)
    p(amount = 0.7 * prg_mult)
    
  }
    
}


# Run sequentially (testing)
plan(sequential)
evaluation <- evaluate_cells(uids_split[3], verbose_level = 1, retry = TRUE)




# Run multisession
plan(multisession, workers = n_sessions)
evaluation <- evaluate_cells(uids_split, verbose = FALSE)






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


