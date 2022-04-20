####################################################
### subsample shp files and turn them into RData ###
####################################################

meow <- readOGR(dsn = paste0("/Users/", Sys.info()[7], "/Downloads/MEOW"), layer = "meow_ecos")
meow <- meow %>% st_as_sf()

lme <- readOGR("/Users/ktanaka/Google Drive/Research/GIS/LME66/LMEs66.shp")
lme <- rmapshaper::ms_simplify(lme, keep = 0.001, keep_shapes = F)
lme <- lme %>% st_as_sf()

eez <- readOGR(dsn = "/Users/ktanaka/clim_geo_disp/data/EEZ_land_union", layer = "EEZ_land_v2_201410")
eez <- rmapshaper::ms_simplify(eez, keep = 0.001, keep_shapes = F)
eez <- eez %>% st_as_sf()

save('/Users/ktanaka/extreme_normalizations/data/eez_sf_dataframe_0.001.RData') 
save('/Users/ktanaka/extreme_normalizations/data/lme_sf_dataframe_0.001.RData') 
save('/Users/ktanaka/extreme_normalizations/data/meow_sf_dataframe.RData') 