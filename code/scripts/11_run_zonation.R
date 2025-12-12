# Run Zonation

# Load packages
library(dplyr)

# Set temp directory for terra on non-boot drive if possible
terraOptions(tempdir = "E:/rtemp")

# Load definitions and code
code_dir <- "E:/code/cvjv-restoration-modelling/code"
def_file <- file.path(code_dir, "definitions.R")
code_files <- list.files(file.path(code_dir, "functions"), pattern = ".*R$", full.names = TRUE)
sapply(c(code_files, def_file), FUN = function(x) source(x))

# Local dirs
base_dir <- file.path("E:/code/cvjv-restoration-modelling")
sum_dir <- file.path(base_dir, "analysis/bird_summaries")
sum_ssn_dir <- file.path(sum_dir, "by_season")

anl_dir <- file.path(base_dir, "analysis")
zon_dir <- file.path(anl_dir, "zonation")
zon_in_dir <- file.path(zon_dir, "inputs")
zon_out_dir <- file.path(zon_dir, "runs")

# Boilerplate for zonation call files
zon_call_hdr <- c("@setlocal",
                  "@PATH=C:/Program Files/Zonation5;%PATH%")
zon_call_ftr <- c("@pause")

# Species weights
sp_weight_df <- data.frame(Species = rep(bird_df$CommonCode, each = 4),
                           Season = rep(c("fall", "winter", "spring", "summer"), times = 7),
                           Weight = c(1, 1, 1, 1, #AMAV
                                      1, 1, 1, 1, #BNST
                                      1, 1, 1, 0, #DOWI
                                      0.5, 1, 1, 0, #DUNL
                                      1, 1, 1, 0, #NOPI
                                      1, 1, 1, 0, #NSHO
                                      1, 1, 1, 0)) #GWTE

ssn_weight_df <- sp_weight_df |>
  group_by(Season) |>
  summarize(Weight = sum(Weight))

# Zonation by time period
time_unit <- c("spring", "summer", "fall", "winter", "fullyear")
spatial_unit <- c("inside", "landscape", "combined-equal")
species_groups <- c("shorebirds", "ducks", "all")
zon_mode <- c("CAZMAX", "CAZ2")
for (tu in time_unit) {
  
  message_ts("Working on time unit ", tu)
  if (tu == "fullyear") {
    tus <- time_unit[1:4]
  } else {
    tus <- tu
  }
  
  for (su in spatial_unit) {
    
    message_ts("Working on spatial unit ", su)
    if (su == "combined-equal") {
      sus <- spatial_unit[1:2]
    } else {
      sus <- su
    }
    
    # Loop across species groups
    for (sg in species_groups) {
      
      message_ts("Working on species group ", sg)
      
      if (sg == "all") {
        sp <- bird_df$FilenameCode
      } else {
        sp <- bird_df$FilenameCode[bird_df$SpeciesGroup == sg]  
      }
      
      message_ts("Total of ", length(sp), " bird species included: ", paste0(sp, collapse = ", "))
      
      # Get layers
      lyr_files <- file.path(zon_in_dir, 
                             paste0("suitability_2013-2022_", 
                                    rep(tus, each = length(sp) * length(sus)), "_",
                                    rep(sp, each = length(sus), times = length(tus)), "_",
                                    rep(sus, times = length(tus) * length(sp)),
                                    ".tif"))
      if (!all(file.exists(lyr_files))) {
        stop(add_ts("The following files are missing:\n\t", 
                    paste(lyr_files[!file.exists(lyr_files)], collapse = "\n\t")))
      }
      #message_ts("Files to include in prioritization:\n\t", paste(lyr_files, collapse = "\n\t"))
      
      # Turn into data frame
      zon_fl_df <- data.frame(filename = lyr_files) |>
        mutate(SpeciesCode = extract_subelement(strsplit(basename(filename), "_"), 4),
               Season = extract_subelement(strsplit(basename(filename), "_"), 3)) |>
        left_join(bird_df, by = join_by(SpeciesCode == FilenameCode)) |>
        left_join(sp_weight_df, by = join_by(CommonCode == Species, Season == Season)) |>
        mutate(Weight = format(round(Weight, 2), nsmall = 2)) |>
        select(Weight, filename) |>
        rename('"weight"' = Weight, '"filename"' = filename)
      
      # Loop across zonation method
      for (zm in zon_mode) {
        
        zon_run <- paste0("time-", tu, "_species-", sg,
                          "_spatial-", su, "_mode-", zm)
        message_ts("Zonation run: ", zon_run)
        
        # Set directories
        # There's a character max in the settings file around 160 in file path or 180 in whole line
        zon_run_dir <- file.path(zon_out_dir, zon_run)
        if (!file.exists(zon_run_dir)) {
          message_ts("Creating directory for zonation run at ", zon_run_dir)
          dir.create(zon_run_dir)
        }
        
        # Create feature list file
        zon_fl_file <- file.path(zon_run_dir, paste0("_run_featurelist.txt"))
        write_delim(zon_fl_df, zon_fl_file, quote = "none", escape = "none")
        
        # Create settings file
        zon_set_file <- file.path(zon_run_dir, paste0( "_run_settings.z5"))
        zon_sets <- c(paste0("feature list file = ", zon_fl_file))
        write_lines(zon_sets, 
                    zon_set_file)
        
        # Create zonation call file
        zon_cmd_file <- file.path(zon_run_dir,  paste0("_run.cmd"))
        zon_call_cmd <- paste0("z5 -w --mode=", zm, " --gui ", zon_set_file, " ", zon_run_dir)
        zon_call <- c(zon_call_hdr,
                      zon_call_cmd,
                      zon_call_ftr)
        write_lines(zon_call,
                    zon_cmd_file)
        
        # Optionally, run
        message_ts("Running without GUI")
        zon_cmd_silent_file <- file.path(zon_run_dir,  paste0("_run_silent.cmd"))
        zon_call_silent <- gsub("(--gui)|(@pause)", "", zon_call)
        write_lines(zon_call_silent,
                    zon_cmd_silent_file)
        shell(zon_cmd_silent_file)
        
      }
      
    }
    
  }

}

