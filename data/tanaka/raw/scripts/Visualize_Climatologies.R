library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(rworldmap)
library(ggplot2)
library(geosphere)
library(gpclib)
library(ggalt)

rm(list = ls())

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

# World map
worldMap <- getMap()
world.points <- fortify(worldMap)
world.points$region <- world.points$id

world.df <- world.points[,c("long","lat","group", "region")]

plot_clim = function(data, mode){
  
  # data = c("HadI", "COBE", "ER")[1]
  # mode = "mean"
  
  # setwd("/Users/Kisei/Dropbox/PAPER Kisei heat extremes")
  setwd("/Users/ktanaka/Dropbox (MBA)/PAPER Kisei heat extremes")
  
  load(paste0("data/", data, "_SST.RData"))
  
  # e = extent(-140, -100, 30, 40)
  # df = crop(df, e); rm(e)
  
  # set baseline Jan 1870 - Dec 1919, 50 years
  Baseline <- df[[1:600]] 
  values(Baseline)[values(Baseline) == -1000] = -1.8
  names(Baseline)
  
  if (mode == "mean") {
    
    jan = calc(Baseline[[seq(1, 600, 12)]], mean)
    jan = as.data.frame(rasterToPoints(jan))
    jan$month = "jan"

    feb = calc(Baseline[[seq(2, 600, 12)]], mean)
    feb = as.data.frame(rasterToPoints(feb))
    feb$month = "feb"
    
    mar = calc(Baseline[[seq(3, 600, 12)]], mean)
    mar = as.data.frame(rasterToPoints(mar))
    mar$month = "mar"
    
    apr = calc(Baseline[[seq(4, 600, 12)]], mean)
    apr = as.data.frame(rasterToPoints(apr))
    apr$month = "apr"
    
    may = calc(Baseline[[seq(5, 600, 12)]], mean)
    may = as.data.frame(rasterToPoints(may))
    may$month = "may"
    
    jun = calc(Baseline[[seq(6, 600, 12)]], mean)
    jun = as.data.frame(rasterToPoints(jun))
    jun$month = "jun"
    
    jul = calc(Baseline[[seq(7, 600, 12)]], mean)
    jul = as.data.frame(rasterToPoints(jul))
    jul$month = "jul"
    
    aug = calc(Baseline[[seq(8, 600, 12)]], mean)
    aug = as.data.frame(rasterToPoints(aug))
    aug$month = "aug"
    
    sep = calc(Baseline[[seq(9, 600, 12)]], mean)
    sep = as.data.frame(rasterToPoints(sep))
    sep$month = "sep"
    
    oct = calc(Baseline[[seq(10, 600, 12)]], mean)
    oct = as.data.frame(rasterToPoints(oct))
    oct$month = "oct"
    
    nov = calc(Baseline[[seq(11, 600, 12)]], mean)
    nov = as.data.frame(rasterToPoints(nov))
    nov$month = "nov"
    
    dec = calc(Baseline[[seq(12, 600, 12)]], mean)
    dec = as.data.frame(rasterToPoints(dec))
    dec$month = "dec"
    
    stat = rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
    
  }
  
  if (mode == "sd") {
    
    jan = calc(Baseline[[seq(1, 600, 12)]], sd)
    jan = as.data.frame(rasterToPoints(jan))
    jan$month = "jan"
    
    feb = calc(Baseline[[seq(2, 600, 12)]], sd)
    feb = as.data.frame(rasterToPoints(feb))
    feb$month = "feb"
    
    mar = calc(Baseline[[seq(3, 600, 12)]], sd)
    mar = as.data.frame(rasterToPoints(mar))
    mar$month = "mar"
    
    apr = calc(Baseline[[seq(4, 600, 12)]], sd)
    apr = as.data.frame(rasterToPoints(apr))
    apr$month = "apr"
    
    may = calc(Baseline[[seq(5, 600, 12)]], sd)
    may = as.data.frame(rasterToPoints(may))
    may$month = "may"
    
    jun = calc(Baseline[[seq(6, 600, 12)]], sd)
    jun = as.data.frame(rasterToPoints(jun))
    jun$month = "jun"
    
    jul = calc(Baseline[[seq(7, 600, 12)]], sd)
    jul = as.data.frame(rasterToPoints(jul))
    jul$month = "jul"
    
    aug = calc(Baseline[[seq(8, 600, 12)]], sd)
    aug = as.data.frame(rasterToPoints(aug))
    aug$month = "aug"
    
    sep = calc(Baseline[[seq(9, 600, 12)]], sd)
    sep = as.data.frame(rasterToPoints(sep))
    sep$month = "sep"
    
    oct = calc(Baseline[[seq(10, 600, 12)]], sd)
    oct = as.data.frame(rasterToPoints(oct))
    oct$month = "oct"
    
    nov = calc(Baseline[[seq(11, 600, 12)]], sd)
    nov = as.data.frame(rasterToPoints(nov))
    nov$month = "nov"
    
    dec = calc(Baseline[[seq(12, 600, 12)]], sd)
    dec = as.data.frame(rasterToPoints(dec))
    dec$month = "dec"
    
    stat = rbind(jan, feb, mar, apr, may, jun, jul, aug, sep, oct, nov, dec)
    
  }
  
  # pdf(paste0("~/Dropbox (MBA)/PAPER Kisei heat extremes/figures/Climatologies/", data, "_Climatology_1870-1929.pdf"), height = 10, width = 8.5)
  # par(mfrow = c(2,1))
  # plot(calc(Baseline, mean), col = matlab.like(100), axes = F, main = "Mean", zlim = c(-3, 33))
  # map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
  # plot(calc(Baseline, sd), col = matlab.like(100), axes = F, main = "SD", zlim = c(0,10))
  # map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
  # dev.off()
  
  return(stat)
  
}

d1 = plot_clim("ER", "mean"); d1$source = "ERSST v5"
d2 = plot_clim("HadI", "mean"); d2$source = "HadISST v1.1"
d3 = plot_clim("COBE", "mean"); d3$source = "COBE v2"

d4 = plot_clim("ER", "sd"); d4$source = "ERSST v5"
d5 = plot_clim("HadI", "sd"); d5$source = "HadISST v1.1"
d6 = plot_clim("COBE", "sd"); d6$source = "COBE v2"

mean = rbind(d1, d2, d3)
sd = rbind(d4, d5, d6)

world <- fortify(getMap())

mean$month = factor(mean$month, levels=c('jan', 'feb', 'mar', 
                                         'apr', 'may', 'jun', 
                                         'jul', 'aug', 'sep', 
                                         'oct', 'nov', 'dec'))

sd$month = factor(sd$month, levels=c('jan', 'feb', 'mar', 
                                     'apr', 'may', 'jun', 
                                     'jul', 'aug', 'sep', 
                                     'oct', 'nov', 'dec'))

mean$source = factor(mean$source, levels=c('HadISST v1.1', 'COBE v2', 'ERSST v5'))
sd$source = factor(sd$source, levels=c('HadISST v1.1', 'COBE v2', 'ERSST v5'))

mean$stat = "Mean"
sd$stat = "SD"

d = rbind(mean, sd)

p = d %>% 
  # sample_frac(0.1) %>%
  subset(stat %in% c("Mean")) %>% 
  subset(source %in% c("HadISST v1.1", "COBE v2")) %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = layer)) +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "gray", fill = "gray", size = 0.01) +
  scale_fill_gradientn(colors = rev(ipcc_temp), "") + 
  # coord_proj("+proj=wintri") +
  facet_grid(month ~ source) +
  # scale_x_continuous(expand = c(-0.1, 0), "") +
  # scale_y_continuous(expand = c(-0.1, 0), "") +
  coord_fixed() + 
  theme_void() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right")

pdf("~/Desktop/s1.pdf", width = 10, height = 10)
p
dev.off()

p = d %>% 
  # sample_frac(0.01) %>%
  subset(stat %in% c("SD")) %>% 
  subset(source %in% c("HadISST v1.1", "COBE v2")) %>% 
  ggplot() + 
  geom_raster(aes(x = x, y = y, fill = layer), interpolate = T) +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "gray", fill = "gray", size = 0.01) +
  scale_fill_gradientn(colors = rev(ipcc_temp), "") + 
  # coord_proj("+proj=wintri") +
  facet_grid(month ~ source) +
  # scale_x_continuous(expand = c(-0.1, 0), "") +
  # scale_y_continuous(expand = c(-0.1, 0), "") +
  coord_fixed() + 
  theme_void() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right")

pdf("~/Desktop/s2.pdf", width = 10, height = 10)
p
dev.off()
