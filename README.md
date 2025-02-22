# cvjv-restoration-modelling
#
# Code for evaluating areas in California's Central Valley for potential restoration

# Organization
## code folder
#### definitions.R
includes directories and some model metadata (e.g., landcover files and names; model objects)

#### functions
- executable files that actually do the processing, organized in general order of use  
- 00_shared_functions.R -- contains shared utility functions that are used in many of the other functions and scripts; must be sourced
- 01_overlay_water_landcover.R -- 
- 02_water_moving_window.R -- 
- 03_predict_birds.R -- 
- 04_extract_predictions.R -- zonal stats for mean within cell and in 5km landscape

#### scripts
executable files that actually do the processing, organized in order of calling

#### data folder ####
data file with some of the required data for predictions, including constant eBird layer  

includes model objects

## Required model covariates not included in project folder (too large for Github):
##### in V:\Project\wetland\FWSPartners\code\cvjv-restoration-modelling\data\other_covariates
- monthly average water data
- monthly tmax
- roads raster
##### in V:\Project\wetland\FWSPartners\analysis\grid
- grid raster unique_ids_masked.tif

## Notes on processing
Multi-core processing currently implemented using the future package. Convenient but with the major downside of messages and other conditions not being loggable in any way due to a limitation in R with capturing stderr().  

Each function returns a list of processed files which is then used as an input in the next function. Functions have no external dependencies beyond 00_shared_functions.R (which must be sourced) and required packages (terra, dplyr, gbm, dismo).  

# Todos:
- finish summary code
- add additional months beyond April
- explore logging options


