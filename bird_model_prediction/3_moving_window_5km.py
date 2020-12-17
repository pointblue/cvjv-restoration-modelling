# Calculate 5km moving window layers for bird forecasting

# Code from Erin (winter 2020), modified by Nathan (July 2020)

# Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Check out the ArcGIS Spatial Analyst extension license  
arcpy.CheckOutExtension("Spatial")  

arcpy.env.addOutputsToMap = 0 # I think the script runs faster if you do not add map output

# Import other tools
import os
from datetime import datetime

# Get a list of the rasters and shapefiles (path should not be in the parenthetical - see IDE in ArcMap for instructions)
baseDir = "blah"
wxlDir = baseDir + "/water_x_landcover"
mvDir = baseDir + "/moving_window"
env.workspace = wxlDir
rasterFiles = arcpy.ListRasters()


# Locations of the rasters and shapefile masks and the destination of analyzed rasters and ascii files
outDir = "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/moving_window"

# Need to have the extent and edges line up (i.e. snapped) to this raster
guideRstFile = "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/2020_Fall/no_imposed_flooding/water_forecasts/forecasting_model_april_predictions_L8_p44r33_2020_Aug_water_forecast.tif"
arcpy.env.extent = guideRstFile
arcpy.env.snapRaster = guideRstFile

# Set the outputname for each output to be the same as the input , but in different places
# Run focal statistics (moving window mean) and save the raster
#for rf in rasterFiles:  
for rf in reversed(rasterFiles):

	inRst = wxlDir + "/" + rf
	outRstFile = mvDir + "/" + rf[:-4] + "_5km.tif"
	
	if os.path.isfile(outRstFile):
		
		print("[" + datetime.now().strftime('%Y-%m-%d %H:%M:%S') + "] - " + "File " + rf + " already processed. Moving to next...")
		
	else: 
		
		print("[" + datetime.now().strftime('%Y-%m-%d %H:%M:%S') + "] - " + "Working on file " + rf)
		
		#arcpy.gp.FocalStatistics_sa("infile", "outfile", "Circle 167 CELL", "MEAN", "DATA", "90")
		outRst = arcpy.gp.FocalStatistics_sa(inRst, outRstFile, "Circle 167 CELL", "MEAN", "DATA", "90")
		
		# Old method; no longer works (error 999999) as of Octo 2020)
		#outRst = FocalStatistics(inRst, NbrCircle(167, "CELL"), "MEAN", "DATA") #distance in cells of 30m
		#outRst.save(outRstFile)  # Save output

