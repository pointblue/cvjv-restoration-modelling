# Script to create 10-year average water layers
# Requires the 

# Load necessary functions and directories
# Must either run 00_definitions.R before this file or source it, replacing getwd() with your base working directory below
code_file <- file.path(getwd(), "code/00_definitions.R")
if (!file.exists(code_file)) {
  stop("Code file with required definitions not found is specified location. Please update path code_file.")
} else {
  source(code_file)  
}

# Packages
