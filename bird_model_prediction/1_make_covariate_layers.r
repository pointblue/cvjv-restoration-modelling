# Re-created by Erin on 4/10/2020; modified by Nathan for TNC work in July.  Only predict to Sac (p44r33).
# This script multiplies water layers by land cover layers.
# Inputs: water layers and TNC land cover layers
# Output: single layer of flooded cover type (0/1) for each (cover type) X (water layer)
# Output is used to create moving window layers for spatial predictions.

# Load packages
library(rgdal)
library(raster)

# Function for outputting timestamped message
messageTS <- function(...) message("[", Sys.time(), "] - ", ...)

# Set directories
baseDir <- "blah"
tmaxDir <- file.path(baseDir, "tmax")
lcDir <- file.path(baseDir, "DividedTNCcover")
fcDir <- file.path(baseDir, "ForecastingTNC")
snpDir <- file.path(fcDir, "landcover")
wrkDir <- file.path(fcDir, "2013-2019")

wtrDir <- file.path(wrkDir, "water_forecasts")
wxlDir <- file.path(wrkDir, "water_x_landcover")

# Parameters
overwrite <- FALSE

# List files
waterFiles <- list.files(wtrDir, pattern = "p44r33.*tif$", full.names = TRUE)
lcFilenames <- c("Rice", "Corn", "Grain", "NonRiceCrops", "TreatedWetland", "Wetland_SemiSeas", "AltCrop", "AllCrops")
#lcFilenames <- c("AllCrops")

# Load template raster
guideRst <- raster(waterFiles[1])

# Snap road raster
rdRspFile <- file.path(fcDir, "valley_roads_p44r33.tif")
if (file.exists(rdRspFile)) { 

	messageTS("Road layer already snapped.")

} else {
	
	messageTS("Will snap road layer to match ", basename(waterFiles[1]))
	rdRst <- raster(file.path(baseDir, "vally_rds5km.tif"))
	
	messageTS("Cropping...")
	crpRst <- crop(rdRst, guideRst)
	
	messageTS("Resampling...")
	rdRst <- resample(crpRst, guideRst, filename = rdRspFile, overwrite = TRUE)
	
} 

# Snap tmax
tmaxFiles <- list.files(tmaxDir, pattern = paste0("^tmx.*_250m.tif$"), full.names = TRUE)
for (tf in tmaxFiles) {
	
	tmaxRspFile <- file.path(fcDir, paste0("p44r33_", basename(tf)))
	
	if (file.exists(tmaxRspFile)) { 

		messageTS("Tmax layer already snapped.")

	} else {
		
		messageTS("Will snap tmax layer to match ", basename(waterFiles[1]))
		tmaxRst <- raster(tf)
		
		messageTS("Projecting...")
		prjRst <- projectRaster(tmaxRst, guideRst)
		
		messageTS("Cropping...")
		crpRst <- crop(prjRst, guideRst)
		
		messageTS("Resampling...")
		rdRst <- resample(crpRst, guideRst, filename = tmaxRspFile, overwrite = TRUE)
		
	} 

} 

# Loop across water files, then landcover files, snapping and multiplying each
#waterFiles <- rev(waterFiles)
for (wf in waterFiles) {
  
	wfn <- basename(wf)
	messageTS("Working on landcover layers for ", wfn)
	wtrRst <- raster(wf)
	
	# Landcover files to snap
	#lcFilenames <- rev(lcFilenames)
	for (lcf in lcFilenames) {
		
		wxlFile <- file.path(wxlDir, paste0(substr(wfn, 37, nchar(wfn) - 13), "_x_", lcf, ".tif"))
		#wxlFile <- file.path(wxlDir, paste0(substr(wfn, 1, nchar(wfn) - 4), "_x_", lcf, ".tif"))
		
		# Skip if already created
		if (file.exists(wxlFile) & overwrite != TRUE) {
		
			messageTS("Water overlay already calculated for ", lcf, ". Skipping...")
		
		# Else process
		} else {
			
			messageTS("Working on landcover layer ", lcf)
			
			# Check if landcover has been snapped
			lcRspFile <- file.path(snpDir, paste0(lcf, "_p44r33.tif"))
			if (file.exists(lcRspFile)) {
				
				messageTS("Snapped landcover already created for ", lcf, ". Loading...")
				lcRst <- raster(lcRspFile)
			
			# Else create
			} else {
				
				messageTS("Will snap landcover layer ", lcf, " to match ", wfn)
				lcRst <- raster(file.path(lcDir, paste0(lcf, ".tif")))
				
				messageTS("Cropping...")
				crpRst <- crop(lcRst, guideRst)
				
				messageTS("Resampling...")
				lcRst <- resample(crpRst, guideRst, filename = lcRspFile, overwrite = TRUE)
			
				messageTS("Complete.")
			
			}
			
			# Process
			messageTS("Calculating overlap...")
			wtrLcRst <- overlay(x = wtrRst, y = lcRst, fun = function(x, y) {x * y}, filename = wxlFile, overwrite = TRUE)
			messageTS("Complete.")
		
		}
		
	}
  
} 

