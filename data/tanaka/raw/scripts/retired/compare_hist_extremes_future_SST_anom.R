setwd("~/extreme_normalizations/data/")

library(raster)
library(colorRamps)
library(dplyr)
library(ggplot2)

rm(list = ls())

#IPCC RCP 8.5 SST Anomalies 2006-2055 & 2050-2099
rcp_1 = stack("8.5_1.nc", varname = "anomaly")
rcp_2 = stack("8.5_2.nc", varname = "anomaly")

rcp_1 = raster::rotate(rcp_1) #rotate to -180:180
rcp_2 = raster::rotate(rcp_2) #rotate to -180:180

rcp_1 <- rcp_1 %>% rasterToPoints() %>% data.frame()
rcp_2 <- rcp_2 %>% rasterToPoints() %>% data.frame()

rcp_1$time = "2006-2055"
rcp_2$time = "2050-2099"

rcp = rbind(rcp_1, rcp_2)
rm(rcp_1, rcp_2)

rcp = rcp %>% group_by(x, y) %>% summarise(sst = mean(Sea.Surface.Temperature))

range01 <- function(x){(x-min(x))/(max(x)-min(x))}
rcp$sst = range01(rcp$sst)

rcp %>% ggplot(aes(x, y, fill = sst)) + 
  geom_tile() + 
  scale_fill_viridis_c() + 
  coord_fixed()

setwd("~/extreme_normalizations/results/")

#historical extreme indices
cutoff = c(0.95, 0.975)[1]

load(paste0("HadI/anomalies_1980-1989_", cutoff, ".RData")); hadi1 = anom; hadi1$source = "HadISST v1.1"; hadi1$period = "1980-1989"
load(paste0("HadI/anomalies_1990-1999_", cutoff, ".RData")); hadi2 = anom; hadi2$source = "HadISST v1.1"; hadi2$period = "1990-1999"
load(paste0("HadI/anomalies_2000-2009_", cutoff, ".RData")); hadi3 = anom; hadi3$source = "HadISST v1.1"; hadi3$period = "2000-2009"
load(paste0("HadI/anomalies_2010-2019_", cutoff, ".RData")); hadi4 = anom; hadi4$source = "HadISST v1.1"; hadi4$period = "2010-2019"
load(paste0("COBE/anomalies_1980-1989_", cutoff, ".RData")); cobe1 = anom; cobe1$source = "COBE v2"; cobe1$period = "1980-1989"
load(paste0("COBE/anomalies_1990-1999_", cutoff, ".RData")); cobe2 = anom; cobe2$source = "COBE v2"; cobe2$period = "1990-1999"
load(paste0("COBE/anomalies_2000-2009_", cutoff, ".RData")); cobe3 = anom; cobe3$source = "COBE v2"; cobe3$period = "2000-2009"
load(paste0("COBE/anomalies_2010-2019_", cutoff, ".RData")); cobe4 = anom; cobe4$source = "COBE v2"; cobe4$period = "2010-2019"

anom = rbind(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)
rm(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)

anom = anom %>% group_by(x, y) %>% summarise(sum = mean(sum))
anom$sum = range01(anom$sum)

anom %>% ggplot(aes(x, y, fill = sum)) +
  geom_tile() + 
  scale_fill_viridis_c() + 
  coord_fixed()

r = merge(anom, rcp)
r$past_future = r$sum + r$sst
r$past_future = range01(r$past_future)

r1 = r %>% select(x, y, sst) %>% mutate(t = "rcp8.5 SST anomalies (2006-2099)", z = sst) %>% select(x, y, z, t)
r2 = r %>% select(x, y, sum) %>% mutate(t = "normalized extremes (1980-2019)", z = sum) %>% select(x, y, z, t)
r3 = r %>% select(x, y, past_future) %>% mutate(t = "normalized extremes + SST anomalies", z = past_future) %>% select(x, y, z, t)

r = rbind(r1, r2, r3)

world <- fortify(rworldmap::getMap())

#IPCC temperature color theme
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

r$t = factor(r$t, levels = c("normalized extremes (1980-2019)", 
                             "rcp8.5 SST anomalies (2006-2099)",  
                             "normalized extremes + SST anomalies"))

r %>% ggplot(aes(x, y, fill = z)) + 
  geom_raster(interpolate = T) +
  geom_map(data = world, 
           map = world, 
           aes(x = long, y = lat, map_id = id),
           color = "gray20", fill = "gray20", size = 0.001) + 
  scale_fill_gradientn(colors = rev(ipcc_temp), "", limits = c(0,1), breaks = c(0,0.5,1)) +
  coord_fixed() + theme_minimal(I(15)) + 
  ylab("") + xlab("") + 
  facet_wrap(.~t, ncol = 1) +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right")
