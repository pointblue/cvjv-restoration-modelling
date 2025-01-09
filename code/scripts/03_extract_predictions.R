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

# Get prediction files
prd_df_file <- file.path(prd_dir, "prediction_table.rds")
if (!file.exists(prd_df_file) | global_overwrite == TRUE) {
  
  message_ts("Getting prediction files...")
  prd_files <- list.files(prd_dir)
  prd_tifs <- prd_files[grepl("tif$", prd_files)]
  
  message_ts("Parsing prediction files...")
  prd_df <- parse_filename(prd_tifs)
  names(prd_df)[names(prd_df) == "Month"] <- "Model"
  names(prd_df)[names(prd_df) == "Description"] <- "Month"
  prd_df$Path <- prd_dir
  prd_df$Location <- gsub("cell-", "", prd_df$Location)
  prd_df$Month <- gsub("month-", "", prd_df$Month)
  prd_df$Year <- gsub("year-", "", prd_df$Year)
  prd_df$Species <- extract_subelement(strsplit(prd_df$Model, "-"), 2)
  prd_df$ModelLocation <- extract_subelement(strsplit(prd_df$Model, "-"), 3)
  
  saveRDS(prd_df, prd_df_file)
  
}
prd_df <- readRDS(prd_df_file)

# Subset to ducks only
duck_df <- prd_df[prd_df$Species %in% c("ANAC", "ANCL", "ANCR"),]
#duck_count_df <- table(duck_df$Location, duck_df$Month)
prd_df <- duck_df

# Get matrix for summarizing cell values
cell_mat_file <- file.path(data_dir, "cell_matrix.rds")
if (!file.exists(cell_mat_file) | global_overwrite == "TRUE") {
  
  message_ts("Creating cell matrix file")
  
  
  # Predictions are 353 x 353 cells
  # Summarize 5k buffer
  # 353 x 353 cells
  
  # Summarize 250m buffer
  # 35 x 35 cells
  
  # Summarize restored area
  # 20 x 20 cells
  
  
  cell_5k_rst <- rast(file.path(cell_dir, "cell_100366_buffered-5000m_filled-0.tif"))
  cell_5k_mat <- as.matrix(cell_rst)
  
  cell_250m_rst <- rast(file.path(cell_dir, "cell_100366_buffered-250m_filled-0.tif"))
  values(cell_250m_rst) <- 1
  cell_250m_rst <- extend(cell_250m_rst, cell_5k_rst, fill = 0)
  
  cell_rst <- c(cell_5k_rst, cell_250m_rst)
  cell_mat <- as.matrix(cell_rst)
  saveRDS(cell_mat, cell_mat_file)
  
}
cell_mat <- readRDS(cell_mat_file)

# Locations
uids <- unique(prd_df$Location)

# Split uids into chunks of chunk_size for processing
# Compromise between max speed (chunks = n_cores) and error resistance /
#   getting some results quick (chunks = 1 per uid)
chunk_size <- 10
n_splits <- ceiling(length(uids) / chunk_size)

# Randomize files to prevent a large bloc of unprocessed files from being assigned to the same chunk
uids_split <- chunk(sample(uids), n_splits)

# Set number of processes
# Caps at number of cores - 1
cores_to_use <- 4
n_sessions <- min(length(uids_split), cores_to_use, availableCores() - 1)

handlers(global = TRUE)
handlers(handler_progress(
  format = "[:bar] :percent (:current/:total) - Elapsed: :elapsed, ETA: :eta",
  clear = FALSE))

# Execute the code in n splits
summarize_cells <- function(uid_list, overwrite = FALSE, 
                            retry_times = 0, retry_counter = NULL, skip_bad = FALSE,
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
    
    # Streamlined error catching
    tryCatch({
      
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
      
      # Loop across UIDs
      for (uid in uids) {
        
        if (show_msg_prg) p(add_ts("Working on UID ", uid), class = "sticky", amount = 0)
        if (show_msg_fxn) message_ts("Working on UID ", uid)
        uid_df <- prd_df[prd_df$Location == uid,]
        nrows <- nrow(uid_df)
        
        # Check if created
        out_file <- file.path(stat_dir, "by_cell", paste0("cell-", uid, "_summary-ducks.rds"))
        if (file.exists(out_file) & overwrite != TRUE) {
          
          if (show_msg_fxn) message_ts("Already created; skipping...")
          next
          
        } else {
          
          if (show_msg_prg) p(add_ts("Summarizing ", nrows, " files..."), 
                              class = "sticky", amount = 0 * global_prog_mult)
          
          # Create summary data frame
          # Three rows for each, because summarized at three distances
          out_df <- data.frame(Filename = rep(uid_df$Filename, each = 3),
                               Cell = rep(uid_df$Location, each = 3),
                               Month = rep(uid_df$Month, each = 3),
                               Year = rep(uid_df$Year, each = 3),
                               Species = rep(uid_df$Species, each = 3),
                               ModelLocation = rep(uid_df$ModelLocation, each = 3),
                               SummaryRegion = rep(c("Inside", "Landscape 250m", "Landscape 5km"), times = nrow(uid_df)),
                               SummaryAreaSqm = rep(c(20 * 20 * 900, 35 * 35 * 900, 353 * 353 * 900), times = nrow(uid_df)),
                               SuitabilitySum = rep(NA))
          
          for (n in 1:nrows) {
            
            fn <- uid_df$Filename[n]
            #if (show_msg_fxn) p(add_ts("Summarizing file ", n, ": ", fn), class = "sticky", amount = 0)
            if (show_msg_fxn) message_ts("Summarizing file ", n, ": ", fn)
            
            # Load
            prd_rst <- rast(file.path(prd_dir, fn))
            
            # Create raster stack of predictions to summarize by regions
            # Non-zero values in restoration area (prd_rst * cell_mat[,1]), 
            #   250m (prd_rst * cell_mat[,2]), and landscape (prd_rst)
            rgn_rst <- c(c(prd_rst, prd_rst) * cell_mat, prd_rst)
            
            # Calculate sums
            sum_df <- global(c(rgn_rst), fun = "sum")
            
            # Add to summary data frame
            start_row <- 1 + (3 * (n - 1))
            out_df$SuitabilitySum[start_row] <- sum_df$sum[1]
            out_df$SuitabilitySum[start_row + 1] <- sum_df$sum[2]
            out_df$SuitabilitySum[start_row + 2] <- sum_df$sum[3]
            
          }
          
          # Export
          saveRDS(out_df, out_file)
          if (show_msg_prg) p(add_ts("Summarized predictions for cell ", uid, " to: ", basename(out_file)), 
                              class = "sticky", amount = 1 * global_prog_mult)
          
        }
        
      }
      
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
      
      # If retry request, repeat individual
      if (retry_times >= 1 & retry_counter == 0) {
        p(add_ts("Retrying UIDs in this set individually to find problematic UID(s)..."),
          class = "sticky", amount = 0)#.3 * prg_mult * -1)
        summarize_cells(uid, overwrite = overwrite,
                       retry_times = min(retry_times, 2), retry_counter = retry_counter,
                       p = p, global_prog_mult = 0)
      # If skipping bad, go to next uid
      } else if (skip_bad == TRUE) {
        p(add_ts("Skipping bad UID..."))
        bad_pos <- which(uids == uid)
        if (length(bad_pos) == 0) {
          p(add_ts("None left to skip."))
        } else {
          uids_after <- uids[bad_pos:length(uids)]
          summarize_cells(uids_after, overwrite = overwrite,
                          retry_times = min(retry_times, 2), retry_counter = retry_counter,
                          p = p, global_prog_mult = 0)
        }
          
      # If repeating a second time, set overwrite to TRUE
      } else if (retry_times > 1 & retry_counter == 1) {
        p(add_ts("Retrying problematic UID with overwrite == TRUE..."), class = "sticky", amount = 0)#.3 * prg_mult * -1)
        summarize_cells(uid, overwrite = TRUE,
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
evaluation <- summarize_cells(uids_split[1:2], verbose_level = 2, retry_times = 1)

# Run multisession
plan(multisession, workers = n_sessions)
evaluation <- summarize_cells(uids_split, verbose_level = 1, retry_times = 0, skip_bad = TRUE, overwrite = FALSE)



# Read and combine all stats
message_ts("Reading and combining data...")
stat_files <- list.files(cell_stat_dir, pattern = "summary.rds$", full.names = TRUE)
long_df <- do.call(rbind, lapply(stat_files, function(x) readRDS(x)))
saveRDS(long_df, file.path(stat_dir, "cell_summary_long.rds"))
write.csv(long_df, file.path(stat_dir, "cell_summary_long.csv"), row.names = FALSE)


