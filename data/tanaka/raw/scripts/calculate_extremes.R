library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)

rm(list = ls())

cutoff = c(0.95, 0.975, 0.98)[3]

calculate_anomalies = function(period, data){
  
  # period = "1980-1989"
  # data = "COBE"
  
  setwd(paste0("/Users/", Sys.info()[7], "/Dropbox/PAPER Kisei heat extremes"))
  
  load(paste0("data/", data, "_SST.RData"))
  
  # e = extent(-140, -100, 30, 40)
  # df = crop(df, e); rm(e)
  
  # set baseline Jan 1870 - Dec 1919, 50 years
  Baseline <- df[[1:600]]
  values(Baseline)[values(Baseline) == -1000] = -1.8

  # set baseline Jan 1956 - Dec 2005, 50 years
  # Baseline <- df[[1033:1632]]
  
  names(Baseline)
  
  # pdf(paste0("~/Desktop/", data, "_Climatology_1870-1929.pdf"), height = 10, width = 8.5)
  # par(mfrow = c(2,1))
  # plot(calc(Baseline, mean), col = matlab.like(100), axes = F, main = "Mean", zlim = c(-3, 33))
  # map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
  # plot(calc(Baseline, sd), col = matlab.like(100), axes = F, main = "SD", zlim = c(0,10))
  # map(add = T, lwd = 0.1, fill = T, col = "gray"); degAxis(1); degAxis(2, las = 1)
  # dev.off()
  
  Baseline <- Baseline %>% rasterToPoints() %>% data.frame()
  # if (data == "HadI") Baseline[Baseline == -1000] <- -1.8
  
  # View(names(df)) #look at time steps
  
  # set target period
  if (period == "1980-1989") Target <- df[[1321:1440]] #Jan 1980 - Dec 1989
  if (period == "1990-1999") Target <- df[[1441:1560]] #Jan 1990 - Dec 1999
  if (period == "2000-2009") Target <- df[[1561:1680]] #Jan 2000 - Dec 2009
  if (period == "2010-2019") Target <- df[[1681:1800]] #Jan 2010 - Dec 2019
  if (period == "2019") Target <- df[[1789:1800]] #Jan 2010 - Dec 2019
  
  values(Target)[values(Target) == -1000] = -1.8 
  
  Target <- Target %>% rasterToPoints() %>% data.frame()
  # if (data == "HadI") Target[Target == -1000] <- -1.8
  
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
      
      q = quantile(baseline, prob = cutoff)
      # hist(baseline, breaks = 100, col = matlab.like(100), lty = "blank")
      # abline(v = q)
      
      interval = seq(m+2, dim(Target)[2], by = 12)
      
      present = Target[ll, c(interval)]
      present = t(present)
      present = as.data.frame(present)
      present = present[,1]
      sum = sum(q < present)
      
      monthly_anom = cbind(monthly_anom, sum)
      
    }
    
    ll_anom = rbind(ll_anom, monthly_anom)
    
  }
  
  colnames(ll_anom) = c("jan", "feb", "mar", "apr", "may", "jun",
                        "jul", "aug", "sep", "oct", "nov", "dec")
  
  anom = cbind(Target[1:2], ll_anom)
  
  anom$sum = rowSums(anom[3:14])
  
  save(anom, file = paste0("/Users/", Sys.info()[7], "/extreme_normalizations/", data, "_", cutoff, "_SST_Anomalies_", period, ".RData"))

  # beepr::beep(2)
  
}

calculate_anomalies("1980-1989", "HadI")
calculate_anomalies("1990-1999", "HadI")
calculate_anomalies("2000-2009", "HadI")
calculate_anomalies("2010-2019", "HadI")
calculate_anomalies("2019", "HadI")

calculate_anomalies("1980-1989", "COBE")
calculate_anomalies("1990-1999", "COBE")
calculate_anomalies("2000-2009", "COBE")
calculate_anomalies("2010-2019", "COBE")
calculate_anomalies("2019", "COBE")

calculate_anomalies("1980-1989", "ER")
calculate_anomalies("1990-1999", "ER")
calculate_anomalies("2000-2009", "ER")
calculate_anomalies("2010-2019", "ER")
calculate_anomalies("2019", "ER")



