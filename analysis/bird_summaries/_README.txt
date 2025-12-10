Naming convention: 
  suitability_yyyy-yyyy_temporal-period_TAXON_summary-distance.tif
  yyyy-yyyy: range of years used for calculating suitability; all are 2013 – 2022
  temporal-period: which part of the year the data are for.  Base folder contains the full-year means (year), 
    by_month has monthy summaries (three-letter month codes), and by_season has summaries by bioclimatic season 
    (spring = Mar – May, summer = Jun – Aug, fall = Sep – Nov, winter = Dec – Feb).
 SpeciesCode: taxon being summarized; all = all species, otherwise four letter species code 
    (AMAV = American Avocet, BNST = Black-necked Stilt, DOWI = Dowitcher spp, DUNL = Dunlin)
 summary-distance: distance for which the suitability has been summed. Inside = within the restoration area, 
    landscape-250m = within the restoration area + the surrounding 250m, landscape-5km = within the restoration area + the surrounding 5km. 
 
Raster metadata:
 Format: floating point geotiffs
Projection: UTM 10N (WGS 84)
raster values: total estimated suitability for the specified summary-distance and taxon averaged across year and season

Methods summary:
  We set the study area boundaries using the Central Valley Joint Venture region
  In consultation with partners, we set a target restoration size for the analysis as about 90 acres, or 600 x 600m
  We created a fishnet grid across the CVJV using this resolution and aggregated landcover / landuse type within each 600x600m pixel.  
   A pixel was deemed as having potential for restoration if it was majority non-orchard agriculture.  
    This was to exclude rocky terrain, forests, urban areas, existing wetlands, pastures, orchards, and other areas we deemed unsuitable for restoration.  
  We further excluded areas with a high slope (don’t remember the exact cutoff but I can look it up – maybe 5 degrees?)
  Within each CVJV basin, we calculated average monthly flooding in existing semipermanent wetlands by month using our footprint of existing wetlands and our time 
    series of flooding 2013 – 2020.  This was to get geographic differences in flood amount/timing across the CVJV.
  We then imposed this average monthly flooding value to each potential restoration area based on the location (basin) of the area.  
    This simulates the area being restored and managed tomatch the vegetation and flooding schedule of nearby, existing semipermanent wetlands.
  Next, we created the neighborhood statistics for each restoration site, month, and habitat type included in the model, which includes, e.g., 
    the total flooded area of rice within 5km of the restoration site or the total flooded area of wetland within the restoration site.  
    (This was done at a 30m resolution and was by far the slowest part of the analysis.)
  With these neighborhood water x habitat rasters, we calculated suitability for AMAV,  BNST, DOWI, and DUNL using the models Erin created.
  We calculated total estimated suitability for each potential restoration site, month, and species at three spatial scales: 
    total suitability within the restoration site (600 x 600m), 
    total suitability within 250m of the boundary of the restoration site (including what is within the site), and
    total suitability within 5km of the boundary of the site (again including what is within the site).  
  We summarized across months and species by averaging suitability
  These values were assigned to the 600 x 600m grid of potential restoration sites

                                                                                                                                           