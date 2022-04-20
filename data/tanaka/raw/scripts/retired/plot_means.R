library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
# library(ggdark)
library(ggjoy)
library(rworldmap)
library(ggalt)
library(readr)
library(lwgeom)

rm(list = ls())

dir = Sys.info()[7]; dir = paste0("/Users/", dir)

cutoff = c(0.95, 0.975)[1]

period = c("1980-1989", "1990-1999", "2000-2009", "2010-2018")

data = c("HadI", "COBE", "ER")

world <- ne_countries(scale = "small", returnclass = "sf") 

worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

world <- fortify(getMap())

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# meow <- readOGR(dsn = paste0("/Users/", Sys.info()[7], "/Downloads/MEOW"), layer = "meow_ecos")
# meow <- meow %>% st_as_sf()  
# 
# lme <- readOGR("/Users/ktanaka/Google Drive/Research/GIS/LME66/LMEs66.shp")
# lme <- rmapshaper::ms_simplify(lme, keep = 0.01, keep_shapes = F)
# lme <- lme %>% st_as_sf()  
# 
# eez <- readOGR(dsn = "/Users/ktanaka/clim_geo_disp/data/EEZ_land_union", layer = "EEZ_land_v2_201410")
# eez <- rmapshaper::ms_simplify(eez, keep = 0.01, keep_shapes = F)
# eez <- eez %>% st_as_sf()  

load('/Users/ktanaka/extreme_normalizations/eez_sf_dataframe_0.001.RData') 
load('/Users/ktanaka/extreme_normalizations/lme_sf_dataframe_0.001.RData') 
load('/Users/ktanaka/extreme_normalizations/meow_sf_dataframe.RData') 

#IPCC - Temperature -
ipcc_temp <- c(rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
               rgb(178, 24, 43, maxColorValue = 255, alpha = 255),
               rgb(214, 96, 77, maxColorValue = 255, alpha = 255),
               rgb(244, 165, 130, maxColorValue = 255, alpha = 255),
               rgb(253, 219, 199, maxColorValue = 255, alpha = 255),
               rgb(247, 247, 247, maxColorValue = 255, alpha = 255),
               rgb(209, 229, 240, maxColorValue = 255, alpha = 255),
               rgb(146, 197, 222, maxColorValue = 255, alpha = 255),
               rgb(67, 147, 195, maxColorValue = 255, alpha = 255),
               rgb(33, 102, 172, maxColorValue = 255, alpha = 255),
               rgb(5, 48, 97, maxColorValue = 255, alpha = 255))

ipcc_temp_4_cols <- c(rgb(153, 0, 2, maxColorValue = 255, alpha = 255),
                      rgb(196, 121, 0, maxColorValue = 255, alpha = 255),
                      rgb(112, 160, 205, maxColorValue = 255, alpha = 255),
                      rgb(0, 52, 102, maxColorValue = 255, alpha = 255))

invert_geom_defaults()

rank_mean = function(region){
  
  region = "eez"
  
  if (region == "meow"){
    shape = meow; shape$UNIT = shape$PROVINCE
  } 
  
  if (region == "lme") {
    shape = lme; shape$UNIT = shape$LME_NAME
  } 
  
  if (region == "eez") {
    shape = eez; shape$UNIT = shape$Country
  } 
  
  tas_combined = NULL
  
  for (i in 1:length(period)){
    
    # i = 1
    
    load(paste0(dir, "/extreme_normalizations/results/HadI/SST_Anomalies_", period[[i]], "_", cutoff, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    summary(tas)
    # hadi <- st_intersection(tas, shape)
    hadi <- st_intersection(tas, st_make_valid(shape))
    # hadi <- st_intersection(tas, st_buffer(shape, 0))    
    hadi$sum = range01(hadi$sum)
    hadi <- hadi %>% group_by(UNIT) %>% 
      summarise(mean = mean(sum, na.rm = T),
                sd = sd(sum, na.rm = T), 
                n = n()) %>%
      mutate(se = sd/sqrt(n),
             lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
             upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
    hadi$source = "HadISST v1.1"; hadi$period = period[[i]]
    
    load(paste0(dir, "/extreme_normalizations/results/COBE/SST_Anomalies_", period[[i]], "_", cutoff, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    # cobe <- st_intersection(tas, shape)
    cobe <- st_intersection(tas, st_make_valid(shape))
    # cobe <- st_intersection(tas, st_buffer(shape, 0))
    cobe$sum = range01(cobe$sum)
    cobe <- cobe %>% group_by(UNIT) %>% 
      summarise(mean = mean(sum, na.rm = T),
                sd = sd(sum, na.rm = T), 
                n = n()) %>%
      mutate(se = sd/sqrt(n),
             lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
             upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
    cobe$source = "COBE v2"; cobe$period = period[[i]]
    
    load(paste0(dir, "/extreme_normalizations/results/ER/SST_Anomalies_", period[[i]], "_", cutoff, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    # er <- st_intersection(tas, shape)
    er <- st_intersection(tas, st_make_valid(shape))
    # er <- st_intersection(tas, st_buffer(shape, 0))
    er$sum = range01(er$sum)
    er <- er %>% group_by(UNIT) %>%
      summarise(mean = mean(sum, na.rm = T),
                sd = sd(sum, na.rm = T),
                n = n()) %>%
      mutate(se = sd/sqrt(n),
             lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
             upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
    er$source = "ERSST v4"; er$period = period[[i]]
    
    tas = rbind(hadi, cobe, er)
    tas = as.data.frame(tas)
    
    tas_combined = rbind(tas_combined, tas)
    
    
  }
  
  # tas_combined = subset(tas_combined, source %in% c("HadISST v1.1", "COBE v2")) # remove ERSST
  
  if (region == "lme") {
    tas_combined_sub = subset(tas_combined, UNIT %in% c("Scotian Shelf", 
                                                        "California Current", 
                                                        "East Brazil Shelf"))
  } 
  
  if (region == "meow") {
    tas_combined_sub = subset(tas_combined, UNIT %in% c("Central Indian Ocean Islands", 
                                                        "Cold Temperate Northwest Pacific", 
                                                        "Galapagos")) 
  } 
  
  if (region == "eez") {
    
    exclude_list = c("Area en controversia (disputed - Peruvian point of view)", 
                     "Area of overlap Australia/Indonesia", 
                     "Conflict zone China/Japan/Taiwan", 
                     "Conflict zone Japan/Russia",
                     "Conflict zone Japan/South Korea",
                     "Disputed Barbados/Trinidad & Tobago",
                     "Disputed Kenya/Somalia",
                     "Disputed Western Sahara/Mauritania",
                     "Joint development area Australia/East Timor",
                     "Joint regime Colombia/Jamaica",
                     "Joint regime Japan/Korea",
                     "Joint regime Nigeria/Sao Tome and Principe",
                     "Protected zone Australia/Papua New Guinea", 
                     "Spratly Islands", 
                     "Antarctica", 
                     "Gaza Strip")
    
    tas_combined = tas_combined[ ! tas_combined$UNIT %in% exclude_list, ]
    
    tas_combined$UNIT = gsub("&", "and", tas_combined$UNIT)
    tas_combined$UNIT = gsub(" Is.", " Islands", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub(" I.", " Island", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub("Congo, DRC", "DR Congo", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub("Bonaire, Sint-Eustasius, Saba", "Netherlands", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub("United States ", "US ", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub("US Virgin Islands", "Virgin Islands, US", tas_combined$UNIT, fixed = T)
    tas_combined$UNIT = gsub("St. ", "Saint ", tas_combined$UNIT, fixed = T)
    
    tas_combined_sub = subset(tas_combined, UNIT %in% c("United States", 
                                                        "Greenland", 
                                                        "Japan")) 
  } 
  
  pdf(paste0("~/Desktop/Mean_", region, "_selected_", cutoff, ".pdf"), height = 5, width = 6)
  
  p = tas_combined_sub %>% 
    mutate(unit = forcats::fct_reorder(UNIT, mean)) %>% 
    ggplot() +
    geom_segment(aes(
      color = period, 
      y = period, 
      yend = period,
      x = lower.ci, 
      xend = upper.ci),
      size = 0.1) +
    geom_point(aes(
      color = period,
      y = period,
      x = mean),
      size = 2) +
    coord_flip() +
    facet_grid(source ~ UNIT, scales = "free") +
    scale_color_manual(values = rev(ipcc_temp_4_cols), "") +
    xlab("") +
    ylab("") +
    # ylim(0,1) +
    theme(legend.position = "bottom",
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  print(p)
  
  dev.off()
  
  p = tas_combined %>% 
    mutate(unit = forcats::fct_reorder(UNIT, mean)) %>% 
    subset(n > 10) %>% 
    ggplot() +
    geom_segment(aes(
      color = period, 
      x = unit, 
      xend = unit,
      y = lower.ci, 
      yend = upper.ci),
      size = 0.5) +
    geom_point(aes(
      color = period,
      x = unit,
      y = mean),
      size = 2) +
    coord_flip() +
    facet_wrap(.~source) +
    scale_color_manual(values = rev(ipcc_temp_4_cols), "") +
    xlab("") +
    ylab("") +
    ylim(0,1) +
    # dark_theme_bw() +
    theme(legend.position = "right")
  
  pdf(paste0("~/Desktop/", cutoff, "_Mean_", region, ".pdf"), height = 15, width = 10)
  print(p)
  dev.off()
  
}

rank_mean("lme")
rank_mean("eez")



