# Command line test and example

# cd PATH/TO/CODE
# Rsript.exe command_line_test.R foo bar -f baz

# Test -----------------

# Get arguments
arguments <- commandArgs(trailingOnly = TRUE) #don't want the full call, just the arguments

# View values and structure
print("Arguments:")
print(arguments)
print("Arguments are stored as a character vector:")
print(str(arguments))

# Arguments are indexed by position, 1:number of args
for (n in 1:length(arguments)) {
  
  arg <- arguments[n]
  print(paste0("Element ", n, ":"))
  print(arg)

}

# Notes -----------------------
# The functionality of the R function commandArgs is basic
# 
# Flags are treated the same as any other argument, so it's a bit involved to get key/value pairs from '--flag value'
# 
# For simple use, prefer just to pass arguments in specified order
#
# There are R packages that extend this functionality with rigourous parsing of the command line call, but
#    for our case, it probably makes sense just to stick with the simple version


# Example usage ---------------
# Place at the beginning of the R script file being called

# Get command line arguments
arguments <- commandArgs(trailingOnly = TRUE)

# Check that there are the expected number of arguments; change # as appropriate
n_args <- length(arguments)
n_args_wanted <- 4
if (n_args != n_args_wanted) stop("Incorrect number of arguments passed: expected ", n_args_wanted, " but got ", n_args)

# Assign arguments as variables by position (index starts at 1)
field_file <- arguments[1]
important_info <- arguments[2]
other_var <- arguments[3]
last_thing <- arguments[4]

# Rest of script


