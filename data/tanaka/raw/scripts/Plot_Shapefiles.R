library(readr)
library(dplyr)
library(ggplot2)
library(rgdal)

rm(list = ls())

### plot LME shapefile

lme <- readOGR("/Users/kisei/Google Drive/research/gis/LME66/LMEs66.shp")
lme <- rmapshaper::ms_simplify(lme, keep = 0.01, keep_shapes = F)
lme <- lme %>% st_as_sf()  

world <- fortify(rworldmap::getMap())

png("/Users/Kisei/Desktop/LME.png", units = "in", res = 100, height = 18, width = 30)

lme %>% 
  # subset(LME_NAME %in% c("East Brazil Shelf", "Somali Coastal Current", "Sulu-Celebes Sea")) %>%
  # subset(LME_NAME %in% c("California Current", "Humboldt Current", "Canadian High Arctic - North Greenland")) %>% 
  ggplot() + 
  geom_sf(aes(group = LME_NAME, fill = LME_NAME), color = "NA", show.legend = T) + 
  scale_fill_discrete("") +
  theme_void() + 
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),color = "gray60", fill = "gray40", size = 0.001) + 
  guides(fill = guide_legend(nrow = 5), "") + 
  theme(legend.position = "bottom")

dev.off()

### plot EEZ ###
eez <- readOGR(dsn = "/Users/kisei/climate_geographic_disparity/data/EEZ_land_union", layer = "EEZ_land_v2_201410")
eez <- rmapshaper::ms_simplify(eez, keep = 0.01, keep_shapes = F)
eez <- eez %>% st_as_sf() 

png("/Users/Kisei/Desktop/EEZ.png", units = "in", res = 100, height = 20, width = 30)

eez %>%
  # subset(Country %in% c("Maldives", "Tanzania", "Liberia")) %>%
  # subset(Country %in% c("Kiribati", "Ecuador", "Peru")) %>%
  ggplot() + 
  geom_sf(aes(group = Country, fill = Country), color = "NA", show.legend = T) + 
  scale_fill_discrete("") + 
  theme_void() + 
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id), color = "gray60", fill = "gray40", size = 0.001) + 
  guides(fill = guide_legend(nrow = 20), "") + 
  theme(legend.position = "bottom")

dev.off()


### plot BGCP ###
load("data/bgcp_raster_0.25.RData")
load("outputs/HadI/extremes_1980-1989_0.95.RData")
anom = anom[, c(1:2, 15)]
x <- raster(xmn  =-180, xmx = 180, ymn = -90, ymx = 90, res = 1, crs = "+proj=longlat +datum=WGS84")
anom <- rasterize(anom[, c('x', 'y')], x, anom[, 'sum'], fun = mean)

bgcp = resample(bgcp, anom, method = "bilinear") 

anom = as.data.frame(rasterToPoints(anom))
bgcp = as.data.frame(rasterToPoints(bgcp))

colnames(anom)[3] = "sum"
colnames(bgcp)[3] = "bgcp"

bgcp = merge(anom, bgcp, all = T)

bgcp$bgcp = round(bgcp$bgcp, 0)
bgcp$bgcp = as.factor(as.character(bgcp$bgcp))

bgcp_names <- readr::read_csv("data/NAME_BGCP_2019_REYGONDEAU.csv")
bgcp_names = bgcp_names[,c("NAME", "BGCP")]
colnames(bgcp_names) = c("name", "bgcp")
bgcp = merge(bgcp, bgcp_names)
bgcp$bgcp = bgcp$name

png("/Users/Kisei/Desktop/BGCP.png", units = "in", res = 100, height = 17, width = 30)

p = bgcp %>% 
  # subset(bgcp %in% c("Indian monsoon gyre", "Western tropical Atlantic", "Northwest Arabian Sea upwelling")) %>%
  # subset(bgcp %in% c("Pacific equatorial divergence", "North Pacific polar front", "North Atlantic Drift")) %>%
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = bgcp)) +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "gray60", fill = "gray40", size = 0.001) + 
  # geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),color = "gray60", fill = "gray40", size = 0.001) + 
  scale_fill_discrete("") +
  coord_fixed() + 
  theme_void() + 
  guides(fill = guide_legend(nrow = 5), "") + 
  theme(legend.position = "bottom")

print(p)

dev.off()


### plot world seas ###
ws <- readOGR("/Users/Kisei/Dropbox/PAPER Kisei heat extremes/data/World_Seas_IHO_v1/World_Seas.shp")
ws <- rmapshaper::ms_simplify(ws, keep = 0.1, keep_shapes = F)
ws <- ws %>% st_as_sf()  

world <- fortify(rworldmap::getMap())

png("/Users/Kisei/Desktop/World_Seas.png", units = "in", res = 100, height = 10, width = 10)
ws %>% 
  # subset(LME_NAME %in% c("East Brazil Shelf", "Somali Coastal Current", "Sulu-Celebes Sea")) %>%
  # subset(LME_NAME %in% c("California Current", "Humboldt Current", "Canadian High Arctic - North Greenland")) %>% 
  ggplot() + 
  geom_sf(aes(group = NAME, fill = NAME), color = "NA", show.legend = T) + 
  scale_fill_viridis_d("") + 
  theme_pubr() + 
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),color = "gray60", fill = "gray40", size = 0.001) + 
  guides(fill = guide_legend(nrow = 8), "") + 
  theme(legend.position = "bottom")

dev.off()
