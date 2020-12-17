# Script to apply bird models
#
# nelliott, July 2020

# Load packages
library(rgdal)
library(raster)
library(dismo)
library(gbm)

# Function for outputting timestamped message
messageTS <- function(...) message("[", Sys.time(), "] - ", ...)

# Set directories
baseDir <- "blah"
mdlDir <- file.path(baseDir, "Final_models")
tncDir <- file.path(baseDir, "CVJV_misc_pred_layer/ForecastingTNC")
fcDir <- file.path(tncDir,  "2013-2019")
wtrDir <- file.path(fcDir, "water_forecasts")
snpDir <- file.path(fcDir, "landcover")
mvDir <- file.path(fcDir, "moving_window")
prdDir <- file.path(fcDir, "bird_forecasts")

# Temp dir for processing not on C drive
rasterOptions(tmpdir = "E:/temp")

# Birds 
birds <- c("AMAV", "BNST", "DOWI", "DUNL", "GWTE", "NOPI", "NSHO")
#birds <- c("DOWI", "DUNL", "GWTE")
#birds <- rev(birds)
birds <- "AMAV"

# Load unchanging covariates

# Roads
roadRst <- raster(file.path(tncDir, "valley_roads_p44r33.tif"))
names(roadRst) <- "roads5km"

# Count Type layer set as eBird data (was used by Erin for predictions)
# factorized; eBird = 1, Structured = 2; predict using eBird
countRst <- roadRst
names(countRst) <- "COUNT_TYPE2"
values(countRst)[!is.na(values(roadRst))] <- 1
#values(countRst) <- NA #does not work

# Loop across months
mthNums <- c(8:12 1:4)
for (mth in mthNums) {
	
	mthAbb <- month.abb[mth]
	messageTS("Working on month ", mthAbb)
	
	# Tmax
	#tmx2015sep_250m
	tmaxRst <- raster(file.path(tncDir, paste0("p44r33_tmx2015", tolower(mthAbb), "_250m.tif")))
	names(tmaxRst) <- "tmax250m"
	
	# Landcover x water, 250m
	#long_rXw_250m = Within a 250m moving window, flooded rice 
	#long_wetland2Xw250m = Within a 250m moving window, flooded seasonal and semipermanent 
	#long_treatXw250m = Within a 250m moving window, flooded treated wetlands  -- OR long_2wetlandXw250m, some confusion
	#long_cornXw_250m = Within a 250m moving window, flooded corn 
	#long_grainXw_250m = Within a 250m moving window, flooded field, row, or grain crops 
	#long_otherXw_250m = Within a 250m moving window, flooded alternating crops, called “Other” in the TNC layer 
	lc250mFns <- paste0("L8_p44r33_2020_", mthAbb, "_water_x_", c("Rice", "Wetland_SemiSeas", "TreatedWetland", "Corn", "Grain", "AltCrop"), "_250m.tif")
	lc250mStk <- stack(file.path(mvDir, lc250mFns))
	names(lc250mStk) <- c("long_rXw_250m", "long_2wetlandXw250m", "long_treatXw_250m", "long_cornXw_250m", "long_grainXw_250m", "long_otherXw_250m")

	# Landcover x water, 5km
	#long_rXw_5km = Within a 5km moving window, flooded rice 
	#long_wetland2Xw5km = Within a 5km moving window, flooded seasonal and semipermanent  -- confusion
	#long_treatXw5km = Within a 5km moving window, flooded treated wetlands 
	#long_allcropXw_5km = Within a 5km moving window, flooded corn, row, field, grain or alternating crops in the TNC layer 
	lc5kmFns <- paste0("L8_p44r33_2020_", mthAbb, "_water_x_", c("Rice", "Wetland_SemiSeas", "TreatedWetland", "AllCrops"), "_5km.tif")
	lc5kmStk <- stack(file.path(mvDir, lc5kmFns))
	names(lc5kmStk) <- c("long_rXw_5km", "long_2wetlandXw5km", "long_treatXw_5km", "long_allcropXw_5km")
	
	# Stack all
	covStk <- stack(roadRst, countRst, tmaxRst, lc250mStk, lc5kmStk)
	
	# Loop across birds
	for (brd in birds) {

		messageTS("Working on bird ", brd)
		
		# Filenames
		brdDir <- list.files(mdlDir, pattern = brd, full.names = TRUE)
		mdlFile <- list.files(brdDir, pattern = "N_Long", full.names = TRUE)
		prdFile <- file.path(prdDir, paste0("p44r33_2020_", mthAbb, "_forecast_", basename(mdlFile), ".tif"))
		
		if (file.exists(prdFile)) {
			
			messageTS("Surface already predicted.  Moving to next...")
		
		} else {
			
			# Load model (saved as extensionless Rdata file)
			messageTS("Loading model...")
			mdlNm <- load(mdlFile)
			mdl <- get(mdlNm[1])
			
			# Predict
			messageTS("Predicting surface...")
			#const = add
			#depending on version of R and Raster, may need to change call to explicitly define used factors
			prdRst <- predict(covStk, mdl, n.trees = mdl$gbm.call$best.trees, type = "response", filename = prdFile, overwrite = TRUE)
			print(summary(prdRst))
			messageTS("Complete.")
			
		}

	}

}

