library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(ggdark)
library(ggjoy)
library(rworldmap)
library(ggalt)
library(readr)
library(lwgeom)
library(sp)
library(maptools)
library(colorRamps)
library(metR)

rm(list = ls())

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

meow <- readOGR(dsn = paste0("/Users/", Sys.info()[7], "/Downloads/MEOW"), layer = "meow_ecos")
meow <- meow %>% st_as_sf()  

lme <- readOGR("/Users/", Sys.info()[7], "/Google Drive/Research/GIS/LME66/LMEs66.shp")
lme <- rmapshaper::ms_simplify(lme, keep = 0.01, keep_shapes = F)
lme <- lme %>% st_as_sf()  

eez <- readOGR(dsn = "/Users/", Sys.info()[7], "/clim_geo_disp/data/EEZ_land_union", layer = "EEZ_land_v2_201410")
eez <- rmapshaper::ms_simplify(eez, keep = 0.01, keep_shapes = F)
eez <- eez %>% st_as_sf()  

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

load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/HadI/extremes_1980-1989_", cutoff, ".RData")); hadi1 = anom; hadi1$source = "HadISST v1.1"; hadi1$period = "1980-1989"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/HadI/extremes_1990-1999_", cutoff, ".RData")); hadi2 = anom; hadi2$source = "HadISST v1.1"; hadi2$period = "1990-1999"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/HadI/extremes_2000-2009_", cutoff, ".RData")); hadi3 = anom; hadi3$source = "HadISST v1.1"; hadi3$period = "2000-2009"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/HadI/extremes_2010-2019_", cutoff, ".RData")); hadi4 = anom; hadi4$source = "HadISST v1.1"; hadi4$period = "2010-2019"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/COBE/extremes_1980-1989_", cutoff, ".RData")); cobe1 = anom; cobe1$source = "COBE v2"; cobe1$period = "1980-1989"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/COBE/extremes_1990-1999_", cutoff, ".RData")); cobe2 = anom; cobe2$source = "COBE v2"; cobe2$period = "1990-1999"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/COBE/extremes_2000-2009_", cutoff, ".RData")); cobe3 = anom; cobe3$source = "COBE v2"; cobe3$period = "2000-2009"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/COBE/extremes_2010-2019_", cutoff, ".RData")); cobe4 = anom; cobe4$source = "COBE v2"; cobe4$period = "2010-2019"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/ER/extremes_1980-1989_", cutoff, ".RData")); er1 = anom; er1$source = "ERSST v5"; er1$period = "1980-1989"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/ER/extremes_1990-1999_", cutoff, ".RData")); er2 = anom; er2$source = "ERSST v5"; er2$period = "1990-1999"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/ER/extremes_2000-2009_", cutoff, ".RData")); er3 = anom; er3$source = "ERSST v5"; er3$period = "2000-2009"
load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/ER/extremes_2010-2019_", cutoff, ".RData")); er4 = anom; er4$source = "ERSST v5"; er4$period = "2010-2019"

#all periods
anom = rbind(hadi1, hadi2, hadi3, hadi4, 
             cobe1, cobe2, cobe3, cobe4,
             er1, er2, er3, er4)

anom$source = factor(anom$source, levels=c("HadISST v1.1","COBE v2",  "ERSST v5"))
anom = subset(anom, source %in% c("COBE v2", "HadISST v1.1"))

latlon = anom[,c("x", "y")]

coordinates(latlon)=~x+y
statarea<-rgdal::readOGR(paste0("/Users/", Sys.info()[7], "/Google Drive/Research/GIS/LME66/LMEs66.shp"))
CRS.new <- CRS("+proj=aeqd +lat_0=0 +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
proj4string(latlon) <- CRS.new 
proj4string(statarea) <- CRS.new
area <- over(latlon,statarea)

colnames(area)[1] = "LME"
anom = cbind(anom, area)

anom = subset(anom, USLMES == "Yes")

if (mode == "annual") {
  
  anom$sum = range01(anom$sum)
  table(anom$LME_NAME)
  anom_i = subset(anom, LME_NAME %in% c("Northeast U.S. Continental Shelf"))
  anom_i = subset(anom, LME_NAME %in% c("California Current"))
  anom_i = subset(anom, LME_NAME %in% c("Insular Pacific-Hawaiian"))
  
  xlims <- range(pretty(anom_i$x));ylims <- range(pretty(anom_i$y))
  
  p = ggplot(anom_i) +
    geom_tile(aes(x = x, y = y, fill = sum)) +
    annotation_map(map_data("world")) +
    coord_fixed(xlim = xlims,ylim = ylims) +
    # scale_fill_gradientn(colors = rev(ipcc_temp), "", breaks = c(0,0.5,1), limits = c(0,1)) +
    scale_fill_gradientn(colors = matlab.like(100), "", breaks = c(0,0.5,1), limits = c(0,1)) +
    # scale_x_continuous(expand = c(-0.005, 0), "") +
    # scale_y_continuous(expand = c(-0.005, 0), "") +
    scale_x_longitude(breaks = NULL) +
    scale_y_latitude(breaks = NULL) +
    facet_wrap( ~ period, ncol = 4) +
    ggdark::dark_theme_bw() +
    theme(
      # axis.title.x = element_blank(),
      # axis.title.y = element_blank(),
      # axis.text.x = element_blank(),
      # axis.text.y = element_blank(),
      # axis.ticks.x = element_blank(),
      # axis.ticks.y = element_blank(),
      legend.position = "bottom",
      legend.justification = c(1,0)) + 
    ggtitle(unique(anom_i$LME_NAME))
  
  pdf(paste0("/Users/", Sys.info()[7], "/Desktop/SST_Anomalies_Annual_", cutoff, ".pdf"), height = 7, width = 10)
  # png(paste0("/Users/", Sys.info()[7], "/Desktop/Fig1_", Sys.Date(), "_", cutoff, ".png"), height = 3, width = 9, units = "in", res = 500)
  print(p)
  dev.off()
  
}

if (mode == "seasonal") {
  
  table(anom$LME_NAME)
  anom_i = subset(anom, LME_NAME %in% c("Northeast U.S. Continental Shelf"))
  anom_i = subset(anom, LME_NAME %in% c("California Current"))
  
  xlims <- range(pretty(anom_i$x));ylims <- range(pretty(anom_i$y))
  
  season_1 = anom_i[,c("x", "y", "jan", "feb", "mar", "source", "period")]; season_1$season = "Jan_Feb_Mar"
  season_2 = anom_i[,c("x", "y", "jul", "aug", "sep", "source", "period")]; season_2$season = "Jul_Aug_Sep"
  
  season_1$sum = rowSums(season_1[3:5])
  season_2$sum = rowSums(season_2[3:5])
  
  season_1 = season_1[,c(1,2, 6:9)]
  season_2 = season_2[,c(1,2, 6:9)]
  
  anom_i = rbind(season_1, season_2)
  
  anom_i = subset(anom_i, source %in% c("COBE v2", "HadISST v1.1"))
  
  anom_i$sum = range01(anom_i$sum)
  
  p = ggplot(anom_i) +
    geom_tile(aes(x = x, y = y, fill = sum), interpolate = F) +
    # geom_point(aes(x, y, color = sum, fill = sum)) +
    borders(xlim = xlims,ylim = ylims, fill = "gray") +
    coord_fixed(xlim = xlims,ylim = ylims) +
    # scale_fill_gradientn(colors = matlab.like(100), "", limits = c(0,1), breaks = c(0,0.5,1)) +
    scale_fill_gradientn(colors = rev(ipcc_temp), "") +
    scale_x_continuous(expand = c(-0.005, 0), "") +
    scale_y_continuous(expand = c(-0.005, 0), "") +
    facet_grid(season ~ period) +
    theme_pubr(I(14)) +
    theme(axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "right",
          legend.justification = c(1,0))
  
  pdf(paste0("/Users/", Sys.info()[7], "/Desktop/SST_Anomalies_Season_", cutoff, ".pdf"), height = 5, width = 10)
  # png(paste0("/Users/", Sys.info()[7], "/Desktop/Fig2_", Sys.Date(), "_", cutoff, ".png"), height = 6, width = 9, units = "in", res = 300)
  print(p)
  dev.off()
  
}
