# Calculate 250m moving window layers for bird forecasting

# Code from Erin (winter 2020), modified by Nathan (July 2020)

# Import system modules
import arcpy
from arcpy import env
from arcpy.sa import *

# Check out the ArcGIS Spatial Analyst extension license  
arcpy.CheckOutExtension("Spatial")  

arcpy.env.addOutputsToMap = False # I think the script runs faster if you do not add map output

# Import other tools
import os
from datetime import datetime

# Get a list of the rasters and shapefiles (path should not be in the parenthetical - see IDE in ArcMap for instructions)
#baseDir = "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/2020_Fall/imposed_flooding"
baseDir = "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/2013-2019"
#baseDir = "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/2013-2019/CONFIDENTIAL_bird_returns/imposed_flooding"
wxlDir = baseDir + "/water_x_landcover"
mvDir = baseDir + "/moving_window"
env.workspace = wxlDir
rasterFiles = arcpy.ListRasters()

# Need to have the extent and edges line up (i.e. snapped) to this reference raster
refRstFile = "V:/Project/wetland/NASA_water/CVJV_misc_pred_layer/ForecastingTNC/2020_Fall/no_imposed_flooding/water_forecasts/forecasting_model_april_predictions_L8_p44r33_2020_Aug_water_forecast.tif"
arcpy.env.extent = refRstFile
arcpy.env.snapRaster = refRstFile
  
# Set the outputname for each output to be the same as the input , but in different places
# Run focal statistics (moving window mean) and save the raster
for rf in rasterFiles:  
	
	inRst = wxlDir + "/" + rf
	outRstFile = mvDir + "/" + rf[:-4] + "_250m.tif"
	
	if os.path.isfile(outRstFile):
		
		print("[" + datetime.now().strftime('%Y-%m-%d %H:%M:%S') + "] - " + "File " + rf + " already processed. Moving to next...")
		
	else: 
		
		print("[" + datetime.now().strftime('%Y-%m-%d %H:%M:%S') + "] - " + "Working on file " + rf)
		
		#arcpy.gp.FocalStatistics_sa("infile", "outfile", "Circle 8 CELL", "MEAN", "DATA", "90")
		outRst = arcpy.gp.FocalStatistics_sa(inRst, outRstFile, "Circle 8 CELL", "MEAN", "DATA", "90")
		
		# Old method that gives error 999999 as of Oct 2020
		#outRst = FocalStatistics(inRst, NbrCircle(167, "CELL"), "MEAN", "DATA") #distance in cells of 30m
		#outRst.save(outRstFile)  # Save output

