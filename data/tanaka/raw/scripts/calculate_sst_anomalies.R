library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)

rm(list = ls())

# cutoff = c(0.95, 0.975)[1]

calculate_anomalies = function(period, data){
  
  # period = "2019"
  # data = "HadI"
  
  setwd("/Users/Kisei/Dropbox/PAPER Kisei heat extremes")
  
  load(paste0("data/", data, "_SST.RData"))
  
  # set baseline Jan 1870 - Dec 1919, 50 years
  Baseline <- df[[1:600]]
  
  # set baseline Jan 1956 - Dec 2005, 50 years
  Baseline <- df[[1033:1632]]
  
  names(Baseline)
  
  # pdf(paste0("~/Desktop/", data, "_Climatology_1870-1929.pdf"), height = 10, width = 8.5)
  # par(mfrow = c(2,1))
  # plot(calc(Baseline, mean), col = matlab.like(100), axes = F, main = "Mean", zlim = c(-3, 33))
  # map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
  # plot(calc(Baseline, sd), col = matlab.like(100), axes = F, main = "SD", zlim = c(0,10))
  # map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
  # dev.off()
  
  Baseline <- Baseline %>% rasterToPoints() %>% data.frame()
  if (data == "HadI") Baseline[Baseline == -1000] <- -1.8
  
  # View(names(df)) #look at time steps
  
  # set target period
  if (period == "1980-1989") Target <- df[[1321:1440]] #Jan 1980 - Dec 1989
  if (period == "1990-1999") Target <- df[[1441:1560]] #Jan 1990 - Dec 1999
  if (period == "2000-2009") Target <- df[[1561:1680]] #Jan 2000 - Dec 2009
  if (period == "2010-2019") Target <- df[[1681:1800]] #Jan 2010 - Dec 2019
  if (period == "2019") Target <- df[[1789:1800]] #Jan 2019-Dec 2019

  Target <- Target %>% rasterToPoints() %>% data.frame()
  if (data == "HadI") Target[Target == -1000] <- -1.8
  
  ll_anom = NULL
  
  # calculate anomalies at every lot/lon grid cell
  for (ll in 1:dim(Baseline)[1]) { 
    
    # ll = 10000
    
    print(ll)
    
    monthly_anom = NULL
    
    for (m in 1:12) { # every month
      
      # m = 8
      
      interval = seq(m+2, dim(Baseline)[2], by = 12)
      
      baseline = Baseline[ll, c(interval)]
      baseline = t(baseline)
      baseline = as.data.frame(baseline)
      baseline = baseline[,1]
      baseline = mean(baseline)
  
      interval = seq(m+2, dim(Target)[2], by = 12)
      
      present = Target[ll, c(interval)]
      present = Target[ll, m+2]
      present = t(present)
      present = as.data.frame(present)
      present = present[,1]
      sum = present - baseline
      
      monthly_anom = cbind(monthly_anom, sum)
      
    }
    
    ll_anom = rbind(ll_anom, monthly_anom)
    
  }
  
  colnames(ll_anom) = c("jan", "feb", "mar", "apr", "may", "jun",
                        "jul", "aug", "sep", "oct", "nov", "dec")
  
  anom = cbind(Target[1:2], ll_anom)
  
  anom$sum = rowSums(anom[3:14])
  
  save(anom, file = paste0("/Users/", Sys.info()[7], "/extreme_normalizations/", data, "_sst_anomalies_2019.RData"))
  
  beepr::beep(2)
  
}

calculate_anomalies("2019", "HadI")
calculate_anomalies("2019", "COBE")

load("/Users/Kisei/extreme_normalizations/outputs/COBE/sst_anomalies_2019.Rdata"); cobe = anom; cobe$data = "COBE"; cobe$anom = rowMeans(cobe[3:14])
load("/Users/Kisei/extreme_normalizations/outputs/HadI/sst_anomalies_2019.RData"); hadi = anom; hadi$data = "HadI"; hadi$anom = rowMeans(hadi[3:14])

#IPCC Temperature colors
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

anom = rbind(cobe, hadi) %>% dplyr::select(x, y, anom) %>% group_by(x, y) %>% summarise(anom = mean(anom))

world <- fortify(rworldmap::getMap())

anom %>% ggplot(aes(x, y, fill = anom)) +  
  geom_raster() + 
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "gray20", fill = "gray20", size = 0.001) +
  scale_fill_gradientn(colors = rev(ipcc_temp), "") +
  scale_x_continuous(expand = c(-0.005, 0), "") +
  scale_y_continuous(expand = c(-0.005, 0), "") +
  coord_fixed()
