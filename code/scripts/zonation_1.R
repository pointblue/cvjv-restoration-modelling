# Zonation 5: https://zonationteam.github.io/Zonation5/
# --> unclear if "zonator" package works with updated Zonation software
# (structure has changed substantially), so create setup files manually

library(tidyverse)
library(terra)
library(sf)

pointblue.palette <- c('#4495d1', '#74b743', '#f7941d', '#005baa', '#bfd730',
                       '#a7a9ac', '#666666', '#456d28',
                       #add a few more complementary colors
                       '#b74374', '#5e2a84', '#d2c921')


# riparian frame --------- (already done so this is commented out and the raster is loaded below)

#ripmask = rast('data_raw/scenario_rasters/baseline.tif') 
#set all non riparian to NA (riparian values are from 71-78)
  #ripmask[ripmask < 70 | ripmask > 79] <- NA 

#plot(ripmask)
#writeRaster(ripmask, 'data_clean/ripmask.tif')



# project set up -----------

#### load species suitability rasters and crop/mask them to ripmask ####

#the following that is commented out was already run, and the cropped/masked rasters already exist

#spp_list = paste0('C:/01_R_projects/PFW_rip_priori/',
#                  list.files('data_raw/Kristys_HSMs/baseline_cv/', '.tif$', full.names = TRUE)) |>
#  path.expand() |> as_tibble() |> set_names('filename')

#other_spp_list = paste0('C:/01_R_projects/PFW_rip_priori/',
#                  list.files('data_raw/other_spp_finals/', '.tif$', full.names = TRUE)) |>
#  path.expand() |> as_tibble() |> set_names('filename')

# Load riparian mask (should already have NA for non-riparian areas)
ripmask <- rast('data_clean/ripmask.tif')

# Count non-NA pixels for the raster
non_na_count <- global(!is.na(ripmask), "sum", na.rm = TRUE)

# Total area in acres (number of riparian pixels times the pixel size, adjusted to acres)
total_area_acres <- ((non_na_count$sum) * (900))/ 4047    #900 m2 because they are all 30 m by 30 m pixels
#166,774.8 acres, which is more than the CVJV 2020 Impl Plan estimate of 141,500
# possibly because of partial coverage in pixels so this is a slight overestimate (~25k acres)

# Loop through species rasters and apply mask and crop (changed to "other_spp_list when I added more spp)
#other_spp_list$filename |>
#  walk(function(f) {
#    spp_raster <- rast(f)
#    
#    # Optional crop first (faster masking)
#    spp_raster_crop <- crop(spp_raster, ripmask)
#    
#    # Apply mask
#    spp_raster_masked <- mask(spp_raster_crop, ripmask)
#    
#    # Create output filename
#    out_name <- file.path('data_clean/HSMs_ripmask', basename(f))
#    
#    # Write the masked raster
#    writeRaster(spp_raster_masked, out_name, overwrite = TRUE)
#  })

spp_list_masked = paste0('C:/01_R_projects/PFW_rip_priori/',
                  list.files('data_clean/HSMs_ripmask/', '.tif$', full.names = TRUE)) |>
  path.expand() |> as_tibble() |> set_names('filename')


## SET UP SPECIES VARIATIONS AND WEIGHTS:  #####
# (first create a "zonation" subdirectory in the RStudio project)
# all 9 bird species:
spp_list_masked |>
  filter(!grepl('dana|wept', filename)) |>
  mutate(weight = 1.0) |>
  select(weight, filename) |>
  write_delim('zonation/features_equal_birds.txt')
write_file(x = 'feature list file = features_equal_birds.txt',
           file = 'zonation/settings_equal_birds.z5')

# all 11 species; birds and Monarchs and WEPT:
spp_list_masked |>
  mutate(weight = 1.0) |>
  select(weight, filename) |>
  write_delim('zonation/features_equal_birds_monarch_wept.txt')
write_file(x = 'feature list file = features_equal_birds_monarch_wept.txt',
           file = 'zonation/settings_equal_birds_monarch_wept.z5')



# 7 species; remove LAZB and YBCH:
#spp_list_masked |>
#  filter(!grepl('LAZB|YBCH', filename)) |>
#  mutate(weight = 1.0) |>
#  select(weight, filename) |>
#  write_delim('zonation/features_equal_noLAZB_YBCH.txt')
#write_file(x = 'feature list file = features_equal_noLAZB_YBCH.txt',
#           file = 'zonation/settings_equal_noLAZB_YBCH.z5')

# all 9 species but LAZB given twice the weight:
#spp_list_masked |>
#  mutate(weight = 1.0) |>
#  mutate(weight = case_when(
#    filename == 'C:/01_R_projects/PFW_rip_priori/data_clean/HSMs_ripmask/LAZB.tif' ~ 2, 
#    TRUE ~ weight)) |>
#  select(weight, filename) |>
#  write_delim('zonation/features_equal_lazbw2.txt')
#write_file(x = 'feature list file = features_equal_lazbw2.txt',
#           file = 'zonation/settings_equal_lazbw2.z5')


# all 9 species but YBCH given twice the weight:
#spp_list_masked |>
#  mutate(weight = 1.0) |>
#  mutate(weight = case_when(
#    filename == 'C:/01_R_projects/PFW_rip_priori/data_clean/HSMs_ripmask/YBCH.tif' ~ 2, 
#    TRUE ~ weight)) |>
#  select(weight, filename) |>
#  write_delim('zonation/features_equal_ybchw2.txt')
#write_file(x = 'feature list file = features_equal_ybchw2.txt',
#           file = 'zonation/settings_equal_ybchw2.z5')



## SET UP ZONATION ALGORITHMS FOR EACH SPECIES VARIATION
write_file(
  x = '@setlocal
@PATH=C:\\Program Files (x86)\\Zonation5;%PATH%


z5 -w --mode=CAZ1 --gui settings_equal_birds_monarch_wept.z5 caz1_equal_out_birds_monarch_wept

#z5 -w --mode=CAZ1 --gui settings_equal.z5 caz1_equal_out
#z5 -w --mode=CAZ1 --gui settings_equal_noLAZB_YBCH.z5 caz1_equal_out_noLAZB_noYBCH
#z5 -w --mode=CAZ2 --gui settings_equal_noLAZB_YBCH.z5 caz2_equal_out_noLAZB_noYBCH

#z5 -w --mode=RAND --gui settings_equal.z5 RAND_all
#z5 -w --mode=RAND --gui settings_equal_lazbw2.z5 RAND_lazbw2
#z5 -w --mode=RAND --gui settings_equal_ybchw2.z5 RAND_ybchw2

#z5 -w --mode=CAZ2 --gui settings_equal.z5 caz2_equal_out  
#z5 -w --mode=CAZ2 --gui settings_equal_lazbw2.z5 caz2_equal_out_lazbw2
#z5 -w --mode=CAZ2 --gui settings_equal_ybchw2.z5 caz2_equal_out_ybch_w2


@pause',
file = 'zonation/z5_run.cmd')
# --> run the cmd file just by double-clicking on it. It will open the Zonation
# GUI and show you progress on each run. You do have to manually exit the
# Zonation GUI (once it says "Done") to prompt the next run to start.



## Autumn left off here

# results-------
tidal_ranks = list.files('zonation', 'rankmap.tif', recursive = TRUE,
                         full.names = TRUE) |> rast()
names(tidal_ranks) = gsub('_out/rankmap.tif', '',
                          list.files('zonation', 'rankmap.tif', recursive = TRUE))
# caz1_equal vs. caz2_equal
plot(tidal_ranks,
     breaks = c(0.00, 0.20, 0.50, 0.75, 0.90, 0.95, 0.98, 1.00),
     col = c("#2b83ba", "#80bfab", "#c7e8ad", "#ffffbf",
             "#fdc980", "#f07c4a", "#d7191c"))


## TOP 5%-------
tidal_class = tidal_ranks |>
  classify(rcl = matrix(c(-Inf, 0.5, 0,
                          0.5, 0.75, 1,
                          0.75, 0.95, 2,
                          0.95, Inf, 3), nrow = 4, byrow = TRUE))
plot(tidal_class)

tidal_class = as.factor(tidal_class)
levels(tidal_class[[1]]) = c('0-50%', '50-75%', '75-95%', '>95%')

tidal_class = crop(tidal_class, boundary)
writeRaster(tidal_class$caz1_equal, 'SDM_results/tidal/v10/caz1_equal_classes.tif')

## plot-------
boundary = read_sf('GIS/LegalDelta_Suisun_boundary.shp') |>
  st_transform(crs(tidal_ranks))
palette = c("gray70", "#ffffbf", "#fdc980", "#d7191c")

showtext_auto()
ggplot() + geom_spatraster(data = tidal_class$caz1_equal) +
  scale_fill_manual(values = palette, na.value = 'white', name = 'Priority rank') +
  geom_sf(data = boundary, fill = NA, color = 'black') + # add study area boundary
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        #aspect.ratio = 1.4,
        strip.text = element_text(family = 'sourcesans', size = 12, face = 'bold', hjust = 0),
        legend.title = element_text(family = 'sourcesans', size = 12, face = 'bold'),
        legend.text = element_text(family = 'sourcesans', size = 10))
ggsave(filename = 'fig/DRAFT_zonation.jpg', height = 7, width = 7)

# STATS-------

baseline21 = rast('landscape_rasters/baseline_veg_2021.tif')

top5 = tidal_ranks$caz1_equal |>
  classify(rcl = matrix(c(-Inf, 0.95, NA,
                          0.95, Inf, 1), nrow = 2, byrow = TRUE))
top5_landcover = mask(baseline21, top5)
plot(top5_landcover)
freq(top5_landcover) |> as_tibble() |> mutate(ha = count * .09) |>
  mutate(total = sum(ha), prop = ha/total)

top5_patches = patches(top5, directions = 8)
freq(top5_patches) |> mutate(ha = count * .09) |> arrange(ha)






# NOT UPDATED YET!---------


### PROTECTED STATUS---------
#
# # protected areas
# # https://data.cnra.ca.gov/dataset/california-protected-areas-database
# cpad_shp = read_sf('GIS/CPAD_2022a/CPAD_2022a_Holdings.shp') %>%
#   st_intersection(delta_shp)
#   # st_crop(st_bbox(delta_shp))
# cpad = cpad_shp %>% mutate(protected = 10) %>%
#   st_transform(crs = crs(delta)) %>%
#   vect() %>% rasterize(., delta, field = 'protected') %>% mask(delta)
#
# # easements
# # https://data.cnra.ca.gov/dataset/california-conservation-easement-database
# cced_shp = read_sf('GIS/CCED_2022a/CCED_2022a_Release.shp') %>%
#   st_intersection(delta_shp)
#   # st_crop(st_bbox(delta_shp))
# cced = cced_shp %>% mutate(protected = 20) %>%
#   st_transform(crs = crs(delta)) %>%
#   vect() %>% rasterize(., delta, field = 'protected') %>% mask(delta)
#
# # priority restoration areas
# priority_shp = read_sf('GIS/ER_P3/ER_P3.shp') %>%
#   st_transform(crs = st_crs(32610)) %>%
#   st_intersection(delta_shp)
# priority = priority_shp %>% mutate(code = 100) %>% vect() %>%
#   rasterize(delta, field = 'code')
#
# # combined
# status = c(cpad, cced, priority) %>% sum(na.rm = TRUE)
# freq(status)
# # some are in both protected areas and easements databases
#
# # overlay on top 5%
# results_status = c(
#   c(status, results_top_sum$A.Equal) %>% sum(na.rm = TRUE),
#   c(status, results_top_sum$B.Weighted) %>% sum(na.rm = TRUE)
# )
# names(results_status) = c('A.Equal', 'B.Weighted')
# results_status_df = freq(results_status) %>% as_tibble() %>%
#   mutate(protection = case_when(value < 10 | (value > 40 & value < 110) ~ 'none',
#                                 (value >= 10 & value < 20) |
#                                   (value >= 110 & value < 120) ~ 'protected',
#                                 (value >= 20 & value < 30) |
#                                   (value >= 120 & value < 130) ~ 'easement',
#                                 (value >= 30 & value < 40) |
#                                   (value >= 130 & value < 140) ~ 'both'),
#          zonation = case_when(value %in% c(10, 20, 30, 100, 110, 120, 130) ~ 'none',
#                               value %in% c(3, 13, 23, 33, 103, 113, 123, 133) ~ 'both',
#                               value %in% c(2, 12, 22, 32, 102, 112, 122, 132) ~ 'riparian',
#                               value %in% c(1, 11, 21, 31, 101, 111, 121, 131) ~ 'waterbirds',
#                               TRUE ~ 'top5'),
#          restoration = case_when(value < 100 ~ 'none',
#                                  value >= 100 ~ 'priority'))
#
# # summary by restoration priority & protection status
# results_status_df %>% filter(zonation != 'none') %>% group_by(layer) %>%
#   summarize(total_area = sum(count) * .09,
#             priority_area = sum(count[restoration == 'priority']) * .09,
#             protected_area = sum(count[protection != 'none']) * .09,
#             prop_priority = sum(count[restoration == 'priority']) / sum(count),
#             prop_protected = sum(count[protection != 'none']) / sum(count))
# # 28% of all top 5% zonation priorities are within priority restoration area
# # 37-39% are currently protected or in an easement
#
# results_status_df %>% filter(zonation == 'both') %>%
#   group_by(layer) %>%
#   summarize(total_area = sum(count) * .09,
#             priority_area = sum(count[restoration == 'priority']) * .09,
#             protected_area = sum(count[protection != 'none']) * .09,
#             prop_priority = sum(count[restoration == 'priority']) / sum(count),
#             prop_protected = sum(count[protection != 'none']) / sum(count))
# # 84-87% of all pixels that are within top5% for both riparian landbirds and
# # waterbirds (~600 ha) are currently protected or in conservation easement
# # 11-19% are within priority restoration area
#
# results_status_df %>% filter(zonation != 'both' & zonation != 'none') %>%
#   group_by(layer) %>%
#   summarize(total_area = sum(count) * .09,
#             prop_priority = sum(count[restoration == 'priority']) / sum(count),
#             prop_protected = sum(count[protection != 'none']) / sum(count))
# # 36-38% of all remaining top 5% zonation priorities are currently protected or in
# # an easement; 25-28% are within priority restoration area
#
# # riparian birds
# results_status_df %>% filter(zonation %in% c('both', 'riparian')) %>%
#   group_by(layer) %>%
#   summarize(total_area = sum(count) * .09,
#             priority_area = sum(count[restoration == 'priority']) * .09,
#             protected_area = sum(count[protection != 'none']) * .09,
#             prop_priority = sum(count[restoration == 'priority']) / sum(count),
#             prop_protected = sum(count[protection != 'none']) / sum(count))
# # 33% protected; 30% priority restoration area
#
# results_status_df %>% filter(zonation %in% c('both', 'waterbirds')) %>%
#   group_by(layer) %>%
#   summarize(total_area = sum(count) * .09,
#             priority_area = sum(count[restoration == 'priority']) * .09,
#             protected_area = sum(count[protection != 'none']) * .09,
#             prop_priority = sum(count[restoration == 'priority']) / sum(count),
#             prop_protected = sum(count[protection != 'none']) / sum(count))
# # 45-50% protected; 16-25% priority restoration area
#
#
### OVERLAY VEG CLASS----
# baseline = rast('C:/Users/kdybala/Documents/delta_scenarios/GIS/landscape_rasters/veg_baseline.tif')
# baseline_win = rast('C:/Users/kdybala/Documents/delta_scenarios/GIS/landscape_rasters/veg_baseline_winter.tif')
# key = readxl::read_excel('C:/Users/kdybala/Documents/delta_scenarios/GIS/VEG_Delta10k_baseline_metadata.xlsx')
#
# results_top_seg = segregate(results_top_sum[[1]], other = NA)
# # mask(baseline, results_top_sum %>% subst(from = c(2:3), 1)) %>%
# mask(baseline, results_top_seg) %>%
#   freq() %>%
#   as_tibble() %>%
#   left_join(key %>% select(value = CODE_BASELINE, CODE_NAME), by = 'value') %>%
#   mutate(group = case_when(value %in% c(11:26,28:52) ~ 'AG',
#                            value == 27 ~ 'CORN',
#                            value %in% c(71:77) ~ 'RIPARIAN',
#                            value %in% c(81:82) ~ 'MANAGED WETLAND',
#                            value %in% c(83:89) ~ 'OTHER WETLAND',
#                            TRUE ~ CODE_NAME)) %>%
#   group_by(layer, group) %>%
#   summarize(count = sum(count)) %>%
#   group_by(layer) %>%
#   mutate(total_area = count * .09,
#          prop = count / sum(count)) %>%
#   filter(group %in% c('AG', 'CORN', 'MANAGED WETLAND', 'OTHER WETLAND', 'RIPARIAN'))
#
# # examine other ag types more closely
# mask(baseline, results_top_seg) %>%
#   freq() %>%
#   as_tibble() %>%
#   left_join(key %>% select(value = CODE_BASELINE, CODE_NAME), by = 'value') %>%
#   mutate(group = case_when(value %in% c(11:19) ~ 'PERENNIAL',
#                            value %in% c(71:77) ~ 'RIPARIAN',
#                            value %in% c(81:89) ~ 'WETLAND',
#                            TRUE ~ CODE_NAME)) %>%
#   group_by(layer, group) %>%
#   summarize(count = sum(count), .groups = 'drop') %>%
#   group_by(layer) %>%
#   mutate(total_area = count * .09,
#          prop = count / sum(count)) %>%
#   # filter(group %in% c('FIELD_CORN', 'RICE', 'RIPARIAN', 'WETLAND')) %>%
#   arrange(layer, desc(prop))
#
# mask(baseline, results_top_seg) %>%
#   freq() %>%
#   as_tibble() %>%
#   left_join(key %>% select(value = CODE_BASELINE, CODE_NAME), by = 'value') %>%
#   mutate(group = case_when(value %in% c(11:52) ~ 'AG',
#                            # value %in% c(71:77) ~ 'RIPARIAN',
#                            value %in% c(81:89) ~ 'WETLAND',
#                            TRUE ~ CODE_NAME)) %>%
#   group_by(layer, group) %>%
#   summarize(count = sum(count), .groups = 'drop') %>%
#   group_by(layer) %>%
#   mutate(total_area = count * .09,
#          prop = count / sum(count)) %>%
#   # filter(group %in% c('FIELD_CORN', 'RICE', 'RIPARIAN', 'WETLAND')) %>%
#   arrange(layer, desc(prop))
#
### FLOOD RISK---------
# delta_shp = read_sf('C:/Users/kdybala/Documents/delta_scenarios/GIS/boundaries/Legal_Delta_Boundary.shp') %>%
#   st_transform(crs = st_crs(32610))
# delta = rast('C:/Users/kdybala/Documents/delta_scenarios/GIS/boundaries/delta.tif')
#
# # from DeltaAdapts
# gdb.path = 'C:/Users/kdybala/Documents/delta_scenarios/GIS/original_source_data/210115_DeltaAdapts_FloodExposure_External.gdb'
#
# filelist = c('DeltaAdapts_basepolygons_201001', 'M6_200yrpoly', 'M6_100yrpoly',
#              'M6_50yrpoly', 'M6_10yrpoly')
# da = purrr::map(c(1:5),
#                  function(x) {
#                    read_sf(dsn = gdb.path, layer = filelist[x]) %>%
#                      st_transform(crs = st_crs(32610)) %>%
#                      st_intersection(delta_shp) %>%
#                      mutate(risk = x * 10) %>%
#                      vect() %>%
#                      rasterize(delta, field = 'risk')}
# )
# fkey = data.frame(id = c(1:5),
#                   label = c('very low', 'low', 'medium', 'high', 'very high'),
#                   risk.annual = c('<0.5%', '0.5-1%', '1-2%', '2-10%', '>10%'),
#                   col = c('lightskyblue', 'dodgerblue', 'royalblue', 'blue3', 'midnightblue'))
# floodrisk = cover(da[[5]], da[[4]]) %>% cover(da[[3]]) %>% cover(da[[2]]) %>%
#   cover(da[[1]])
# # levels(floodrisk) <- fkey
# # coltab(floodrisk) <- fkey %>% select(id, col) %>%
# #   complete(id = c(0:255)) %>% pull(col)
# # plot(floodrisk)
#
# floodrisk = rast('C:/Users/kdybala/Documents/delta_scenarios/GIS/scenario_inputs/floodrisk2050.tif')
# freq(floodrisk)
#
# # overlay on top 5%
# results_floodrisk = c(
#   c(floodrisk, results_top_sum$A.Equal) %>% sum(na.rm = TRUE),
#   c(floodrisk, results_top_sum$B.Weighted) %>% sum(na.rm = TRUE)
# )
# names(results_floodrisk) = c('A.Equal', 'B.Weighted')
# freq(results_floodrisk)
#
# results_floodrisk_df = freq(results_floodrisk) %>% as_tibble() %>%
#   mutate(risk = case_when(value < 10 ~ 'none',
#                           value >= 10 & value < 20 ~ 'very low',
#                           value >= 20 & value < 30 ~ 'low',
#                           value >= 30 & value < 40 ~ 'medium',
#                           value >= 40 & value < 50 ~ 'high',
#                           value >= 50 ~ 'very high'),
#          zonation = case_when(value %in% c(10, 20, 30, 40, 50) ~ 'none',
#                               value %in% c(3, 13, 23, 33, 43, 53) ~ 'both',
#                               value %in% c(2, 12, 22, 32, 42, 52) ~ 'riparian',
#                               value %in% c(1, 11, 21, 31, 41, 51) ~ 'waterbirds',
#                               TRUE ~ 'top5')) %>%
#   # summary by restoration priority & protection status
#   filter(zonation != 'none') %>%
#   group_by(layer) %>% mutate(total_area = sum(count) * 0.09) %>%
#   group_by(layer, total_area, risk, zonation) %>%
#   summarize(area = sum(count) * .09,
#             prop = area / total_area,
#             .groups = 'drop')
#
# # high and very high risk?
# results_floodrisk_df %>% filter(risk %in% c('high', 'very high')) %>%
#   group_by(layer) %>%
#   summarize(across(c(area, prop), sum))
# # 18% of all top 5% zonation priorities are at high or very high risk of flooding (13% riparian, 5% waterbirds, <1% both)
#
### additional------
# # additional exploration of unprotected areas (at reviewer request)
# results_top_sum = list.files(pattern = 'zonation5_caz1_top5.*.tif$') %>% rast()
#
# # combine cpad and cced polygons and rasterize
# delta_shp = read_sf('GIS/Legal_Delta_Boundary.shp') %>% st_transform(crs = 3310)
# delta = rast('C:/Users/kdybala/OneDrive - Point Blue/Documents/A_Projects/delta_scenarios/GIS/boundaries/delta.tif')
#
# cpad_shp = read_sf('GIS/CPAD_2022a/CPAD_2022a_Holdings.shp') %>%
#   st_intersection(delta_shp)
# cced_shp = read_sf('GIS/CCED_2022a/CCED_2022a_Release.shp') %>%
#   st_intersection(delta_shp)
# protected_shp = bind_rows(cpad_shp, cced_shp) %>% mutate(protected = 1) %>%
#   select(protected) %>% st_cast('MULTIPOLYGON') %>% st_cast('POLYGON') %>%
#   st_transform(crs = 32610)
# protected = rasterize(vect(protected_shp), y = delta, field = 1) %>% mask(delta)
# writeRaster(protected, 'GIS/protected_all.tif')
#
# # mask out priority areas that are protected
# priority_unprotected = results_top_sum$A.Equal %>% mask(protected, inverse = TRUE)
#
# # overlay veg class
# baseline = rast('C:/Users/kdybala/OneDrive - Point Blue/Documents/A_Projects/delta_scenarios/GIS/scenario_rasters/baseline.tif')
# baseline_win = rast('C:/Users/kdybala/OneDrive - Point Blue/Documents/A_Projects/delta_scenarios/GIS/scenario_rasters/baseline_win.tif')
#
# ## if it's unprotected, what land cover is it?
# byunprotected = mask(baseline, priority_unprotected) %>%
#   freq() %>%
#   as_tibble() %>%
#   mutate(group = case_when(grepl('ORCHARD|VINEYARD|GRAIN|FIELD_OTHER|ROW|RICE|IDLE|PASTURE', value) ~ 'AG',
#                            value == 'FIELD_CORN' ~ 'CORN',
#                            grepl('RIPARIAN', value) ~ 'RIPARIAN',
#                            grepl('WETLAND', value) ~ 'WETLAND',
#                            TRUE ~ value)) %>%
#   group_by(group) %>%
#   summarize(count = sum(count)) %>%
#   mutate(total_area = count * .09,
#          prop = count / sum(count))
# # --> majority ag (32.3%, corn 26.6%, and riparian 23.7%)
#
# ## if it's x land cover class, what proportion unprotected?
# bylandcover = mask(baseline, results_top_sum$A.Equal) %>% freq() %>% as_tibble() %>%
#   mutate(group = case_when(grepl('ORCHARD|VINEYARD|GRAIN|FIELD_OTHER|ROW|RICE|IDLE|PASTURE', value) ~ 'AG',
#                            value == 'FIELD_CORN' ~ 'CORN',
#                            grepl('RIPARIAN', value) ~ 'RIPARIAN',
#                            grepl('WETLAND', value) ~ 'WETLAND',
#                            TRUE ~ value)) %>%
#   group_by(group) %>%
#   summarize(count = sum(count)) %>%
#   mutate(total_area = count * .09)
#
# left_join(bylandcover %>% select(group, total_area),
#           byunprotected %>% select(group, unprotected_area = total_area),
#           by = c('group')) %>%
#   mutate(prop = unprotected_area/total_area)
# # >> 83.5% of ag, 70% of corn, and 63% of riparian priorities is unprotected;
# # compare to 20.7% of wetland
#
# ## any species heavily limited to the unprotected priority areas?
# # compare find the mean probability of presence in protected vs. unprotected priority areas?
# # >> check separate script in the delta_scenarios project folder
#
# # double check veg overlays -- consider winter?
# mask(baseline, results_top_sum$A.Equal) %>% freq() %>% as_tibble() %>%
#   mutate(group = case_when(grepl('ORCHARD|VINEYARD|GRAIN|FIELD_OTHER|ROW|RICE|IDLE|PASTURE', value) ~ 'AG',
#                            value == 'FIELD_CORN' ~ 'CORN',
#                            grepl('RIPARIAN', value) ~ 'RIPARIAN',
#                            grepl('WETLAND', value) ~ 'WETLAND',
#                            TRUE ~ value)) %>%
#   group_by(group) %>%
#   summarize(count = sum(count)) %>%
#   mutate(total_area = count * .09,
#          prop = total_area/sum(total_area))
# ## >> 23% ag, 23% corn, 23% riparian, 24% wetland
#
# mask(baseline_win, results_top_sum$A.Equal) %>% freq() %>% as_tibble() %>%
#   mutate(group = case_when(grepl('ORCHARD|VINEYARD|GRAIN|FIELD_OTHER|ROW|RICE|IDLE|PASTURE', value) ~ 'AG',
#                            value == 'FIELD_CORN' ~ 'CORN',
#                            grepl('RIPARIAN', value) ~ 'RIPARIAN',
#                            grepl('WETLAND', value) ~ 'WETLAND',
#                            TRUE ~ value)) %>%
#   group_by(group) %>%
#   summarize(count = sum(count)) %>%
#   mutate(total_area = count * .09,
#          prop = total_area/sum(total_area))
# ## >> 25% ag, 21% corn, 23% riparian, 24% wetland (slightly more "other ag")
#
## WRITE METADATA----------
# # for sharing
#
# results_zonation = list.files('zprojects', 'rankmap.tif', recursive = TRUE,
#                               full.names = TRUE) %>%
#   str_subset(pattern = 'caz1_equal') %>% rast()
# names(results_zonation) <- c('PriorityRank_riparianlandbird',
#                              'PriorityRank_waterbird_fall',
#                              'PriorityRank_waterbird_win')
# plot(results_zonation)
# writeRaster(results_zonation,
#             paste0('GIS/zonation_results/', names(results_zonation), '.tif'),
#             overwrite = TRUE)
#
# results_combined = rast('zonation5_caz1_top5_A.Equal.tif')
# names(results_combined) <- c('Priority Bird Conservation Areas')
# levels(results_combined) <- data.frame(
#   code = c(1:3),
#   label = c('Waterbirds', 'Riparian landbirds', 'Both'))
# coltab(results_combined) <- c(NA, "#4495d1", "#74b743", "#d7191c")
# plot(results_combined)
# writeRaster(results_combined, 'GIS/zonation_results/PriorityBirdConservationAreas.tif', overwrite = TRUE)
#
#
## PLOT---------
# delta_shp = read_sf('GIS/Legal_Delta_Boundary.shp') %>%
#   st_transform(crs = st_crs(32610))
# delta = rast('GIS/delta.tif')
#
# baseline = rast('GIS/veg_baseline.tif')
# water = baseline %>% classify(rcl = matrix(c(90, 0), nrow = 1), others = NA) %>%
#   mask(delta)
# water_df = as.data.frame(water, xy = TRUE) %>% pivot_longer(-(x:y))
#
#
#
# windowsFonts()
# library(showtext)
# font_add_google('Source Sans Pro', 'sourcesans')
# # font_files() %>% filter(grepl('Source', family, ignore.case = TRUE))
# # font_add("Franklin Gothic", regular = "FRABK.ttf", italic = 'FRABKIT.TTF')
# # font_add("SourceSansPro", regular = "SourceSansPro-Regular.ttf")
# # showtext_auto()
# showtext_opts(dpi = 300) #default for ggsave
#
#### BY TAXONOMIC GROUP--------
# # manuscript version with two columns and three rows
#
# results_zonation_df = results_zonation %>%
#   as.data.frame(xy = TRUE, na.rm = FALSE) %>%
#   pivot_longer(-(x:y)) %>%
#   drop_na() %>%
#   mutate(group = case_when(grepl('riparian', name) ~ 'A',
#                            grepl('waterbird_fall', name) ~ 'B',
#                            grepl('waterbird_win', name) ~ 'C'),
#          # mode = case_when(grepl('_equal', name) ~ 'equal',
#          #                  grepl('_weighted', name) ~ 'weighted'),
#          value_cut = cut(value, include.lowest = TRUE,
#                          breaks = c(0.00, 0.50, 0.75, 0.90, 0.95, 1.00),
#                          labels = c("0.00-0.50", "0.50-0.75",
#                                     "0.75-0.90", "0.90-0.95", "0.95-1.00")),
#          value_cut = factor(value_cut, levels = rev(levels(value_cut))))
#
#### version 1----------
#
# # left column (equal weights)
# p1 = results_zonation_df %>% filter(mode == 'equal') %>%
#   ggplot() + facet_wrap(~group, nrow = 3) +
#   geom_raster(aes(x, y, fill = value_cut)) +
#   geom_sf(data = delta_shp, fill = NA) +
#   scale_fill_discrete(type = rev(c("#2b83ba", "#80bfab", "#ffffbf",
#                                    "#fdc980", "#d7191c")),
#                       na.translate = FALSE) +
#   labs(x = NULL, y = NULL, fill = 'Priority Rank') +
#   theme_minimal() +
#   theme(axis.text = element_blank(),
#         panel.grid = element_blank(),
#         aspect.ratio = 1.4,
#         plot.title = element_text(family = 'Franklin Gothic', size = 11, hjust = 0.5),
#         strip.text = element_text(family = 'Franklin Gothic', size = 10, hjust = 0),
#         legend.title = element_text(family = 'Franklin Gothic', size = 10))
#
# p2 = results_zonation_df %>% filter(mode == 'weighted') %>%
#   ggplot() + facet_wrap(~group, nrow = 3) +
#   geom_raster(aes(x, y, fill = value_cut)) +
#   geom_sf(data = delta_shp, fill = NA) +
#   scale_fill_discrete(type = rev(c("#2b83ba", "#80bfab", "#ffffbf",
#                                    "#fdc980", "#d7191c")),
#                       na.translate = FALSE) +
#   labs(x = NULL, y = NULL, fill = 'Priority Rank') +
#   theme_minimal() +
#   theme(axis.text = element_blank(),
#         panel.grid = element_blank(),
#         aspect.ratio = 1.4,
#         plot.title = element_text(family = 'Franklin Gothic', size = 11, hjust = 0.5),
#         strip.text = element_blank(),
#         legend.title = element_text(family = 'Franklin Gothic', size = 10))
#
# library(patchwork)
# p1 + p2 + plot_layout(guides = 'collect')
# ggsave(filename = 'zonation5_caz1.jpg', height = 8.5, width = 6.5)
#
#### version 2---------
# # showing equal weights separately from weighted (moved to appendix)
#
# # equal weights
# showtext_auto()
# showtext_opts(dpi = 300) #default for ggsave
#
# results_zonation_df %>% #filter(mode == 'equal') %>%
#   ggplot() + facet_wrap(~group, ncol = 3) +
#   geom_sf(data = delta_shp %>% st_transform(crs = st_crs(32610)), fill = 'gray90') +
#   geom_raster(aes(x, y, fill = value_cut)) +
#   geom_tile(data = water_df, aes(x, y), fill = 'white') +
#   geom_sf(data = delta_shp %>% st_transform(crs = st_crs(32610)), fill = NA) +
#   # scale_fill_discrete(type = rev(c("#2b83ba", "#80bfab", "#ffffbf",
#   #                                  "#fdc980", "#d7191c")),
#   #                     na.translate = FALSE) +
#   scale_fill_discrete(type = rev(c("#2c7bb6", "#abd9e9", "#ffffbf",
#                                    "#fdae61", "#d7191c")),
#                       na.translate = FALSE) +
#   labs(x = NULL, y = NULL, fill = 'Priority Rank') +
#   theme_minimal() +
#   theme(axis.text = element_blank(),
#         panel.grid = element_blank(),
#         aspect.ratio = 1.4,
#         # plot.title = element_text(family = 'Franklin Gothic', size = 11, hjust = 0.5),
#         strip.text = element_text(family = 'sourcesans', size = 9.5, face = 'bold', hjust = 0),
#         legend.title = element_text(family = 'sourcesans', size = 9.5, face = 'bold'),
#         legend.text = element_text(family = 'sourcesans', size = 8),
#         legend.key.height = unit(0.5, 'cm'))
# # ggsave(filename = 'zonation5_caz1_equal.jpg', height = 2.75, width = 6.5)
# ggsave(filename = 'figure2.jpg', height = 2.75, width = 6.5)
# showtext_auto(F)
#
# results_zonation_df %>% filter(mode == 'weighted') %>%
#   ggplot() + facet_wrap(~group, ncol = 3) +
#   geom_sf(data = delta_shp %>% st_transform(crs = st_crs(32610)), fill = 'gray90') +
#   geom_raster(aes(x, y, fill = value_cut)) +
#   geom_tile(data = water_df, aes(x, y), fill = 'white') +
#   geom_sf(data = delta_shp %>% st_transform(crs = st_crs(32610)), fill = NA) +
#   scale_fill_discrete(type = rev(c("#2b83ba", "#80bfab", "#ffffbf",
#                                    "#fdc980", "#d7191c")),
#                       na.translate = FALSE) +
#   labs(x = NULL, y = NULL, fill = 'Priority Rank') +
#   theme_minimal() +
#   theme(axis.text = element_blank(),
#         panel.grid = element_blank(),
#         aspect.ratio = 1.4,
#         plot.title = element_text(family = 'Franklin Gothic', size = 11, hjust = 0.5),
#         strip.text = element_text(family = 'Franklin Gothic', size = 10, hjust = 0),
#         legend.title = element_text(family = 'Franklin Gothic', size = 10))
# ggsave(filename = 'zonation5_caz1_weighted.jpg', height = 3, width = 6.5)
#
#
#### TOP 5%----------
# results_top_sum = list.files(pattern = 'zonation5_caz1_top5_.*tif$', full.names = TRUE) %>%
#   rast()
#
# results_top_df = results_top_sum %>%
#   as.data.frame(., xy = TRUE, na.rm = FALSE) %>%
#   pivot_longer(-(x:y), names_to = 'panel') %>%
#   drop_na() %>%
#   mutate(label = case_when(value == 1 ~ 'Waterbirds',
#                            value == 2 ~ 'Riparian landbirds',
#                            value == 3 ~ 'Both'),
#          panel = gsub('\\.', '\\. ', panel))
#
# priority_shp = read_sf('GIS/ER_P3/ER_P3.shp') %>%
#   st_transform(crs = st_crs(32610)) %>%
#   st_intersection(delta_shp) %>%
#   mutate(label = recode(Region_,
#                         'Lower San Joaquin River Floodplain' = 'Lower San Joaquin\nRiver Floodplain',
#                         'Yolo Bypass' = 'Yolo\nBypass',
#                         'Cache Slough' = 'Cache\nSlough',
#                         'Suisun Marsh' = 'Suisun\nMarsh',
#                         'Western Delta' = 'Western\nDelta',
#                         'Cosumunes-Mokelumne' = 'Cosumnes-\nMokelumne'))
#
# key_areas = read_sf('GIS/key_protected_areas_dissolve.shp') %>%
#   st_transform(crs = st_crs(32610)) %>%
#   st_intersection(delta_shp) %>%
#   mutate(label = recode(PARK_NAME,
#                         'Cosumnes River Preserve' = 'Cosumnes\nRiver\nPreserve',
#                         'Yolo Bypass Wildlife Area' = 'Yolo Bypass\nWildlife Area',
#                         'Sacramento Regional County Sanitation District' = 'Sacramento Regional\nCounty Sanitation\nDistrict',
#                         'Stone Lakes National Wildlife Refuge' = 'Stone Lakes\nNWR'))
#
# alt.palette = c('#4495d1', '#F7C60D', '#055396', '#FF0202')
#
# showtext_auto()
# showtext_opts(dpi = 300) #default for ggsave
#
# ggplot(results_top_df %>% filter(panel == 'A. Equal')) +
#   # facet_wrap(~panel, nrow = 1) +
#   geom_sf(data = delta_shp %>% st_transform(crs = st_crs(32610)),
#           fill = 'gray90') +
#   geom_tile(data = water_df, aes(x, y), fill = 'white') +
#   geom_sf(data = key_areas, fill = 'gray60', color = 'gray20', alpha = 0.5) +
#   geom_tile(aes(x, y, fill = label)) + #main priority data
#   geom_sf(data = key_areas, fill = NA,
#           aes(color = 'Select Protected Areas')) +
#   geom_sf(data = delta_shp, fill = NA, color = 'black') +
#   geom_sf(data = priority_shp, fill = NA,
#           aes(color = 'Priority Habitat Restoration Areas')) +
#   geom_sf_text(data = key_areas, aes(label = label),
#                family = 'sourcesans', color = 'gray20',
#                fun.geometry = st_centroid, size = 2.5, lineheight = 0.75,
#                hjust = c(0, 0, 0, 1),
#                nudge_x = c(1000, 3000, 1000, -2000),
#                nudge_y = c(0, 0, 5000, 5000)) +
#   geom_sf_text(data = priority_shp, aes(label = label),
#                family = 'sourcesans',
#                color = alt.palette[3], fun.geometry = st_centroid, size = 3,
#                nudge_x = c(-7000, -3000, -2000, -3000, 0, -1500),
#                nudge_y = c(3000, 3000, 2500, 0, 0, -1000),
#                lineheight = 1) +
#   scale_fill_manual(values = c('Riparian landbirds' = alt.palette[2],
#                                'Waterbirds' = alt.palette[1],
#                                'Both' = alt.palette[4])) +
#   scale_color_manual(values = c('Priority Habitat Restoration Areas' = alt.palette[3],
#                                 'Select Protected Areas' = 'gray20')) +
#   labs(x = NULL, y = NULL,
#        fill = 'Priority Bird Conservation Area',
#        color = NULL) +
#   guides(fill = guide_legend(order = 1),
#          color = guide_legend(order = 2)) +
#   # guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
#   theme_minimal() +
#   theme(axis.text = element_blank(),
#         panel.grid = element_blank(),
#         aspect.ratio = 1.4,
#         legend.title = element_text(family = 'sourcesans', face = 'bold',
#                                     size = 9.5),
#         legend.text = element_text(family = 'sourcesans', size = 8),
#         legend.key.size = unit(0.4, "cm"),
#         legend.position = c(0, 0), legend.justification = c(0, 0),
#         # legend.spacing.y = unit(0.1, "lines"),
#         legend.margin = margin(0, 0, 0, 0))
# # ggsave(filename = 'zonation5_caz1_top5.jpg', height = 5.25, width = 6.5)
# # ggsave(filename = 'zonation5_caz1_top5_equal_v4.jpg', height = 8, width = 6.5)
# ggsave(filename = 'figure3.jpg', height = 8, width = 6)
#
# tmp = raster::raster(results_top_sum$A.Equal)
# mapview::mapview(tmp,
#                  na.color = 'black',
#                  at = c(0.5, 1.5, 2.5, 3.5),
#                  col.regions = c("#c7e8ad", "#fdc980", "#d7191c"),
#                  alpha.regions = 0.5)
#
