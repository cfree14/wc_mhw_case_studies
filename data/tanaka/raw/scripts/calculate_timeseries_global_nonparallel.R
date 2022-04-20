library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)

rm(list = ls())

data = c("HadI", "COBE", "ER")

calculate_anomalies = function(data){
  
  # data = "COBE"
  
  setwd("~/Dropbox/PAPER Kisei heat extremes")
  
  load(paste0("data/", data, "_SST.RData"))
  
  # e = extent(-120, -100, 30, 40)
  # df = crop(df, e); rm(e)
  
  # set baseline Jan 1870 - Dec 1929, 60 years
  Baseline <- df[[1:720]] 
  names(Baseline)
  
  Baseline <- Baseline %>% rasterToPoints() %>% data.frame()
  
  Target <- df[[1321:1788]] #Jan 1980 - Dec 2018
  Target <- Target %>% rasterToPoints() %>% data.frame()
  
  yy_anom = NULL
  
  for (y in 1:39) { #every year between 1980-2018
    
    # y = 2
    
    interval_year = seq(3, 470, by = 12) 
    
    first_month = interval_year[y]
    last_month = first_month+11
    
    target = Target[,first_month:last_month]; names(target) # target year
    ll_anom = NULL
    
    for (ll in 1:dim(Baseline)[1]) { # calculate anomalies at every lot/lon grid cell

      # ll = 10000

      print(ll)
      
      monthly_anom = NULL

      for (m in 1:12) { # every month
        
        # m = 1
        
        interval_month = seq(m+2, dim(Baseline)[2], by = 12)
        
        baseline = Baseline[ll, c(interval_month)]; names(baseline) #pick corresponding month from every baseline year
        baseline = t(baseline)
        baseline = as.data.frame(baseline)
        baseline = baseline[,1]
        
        q = quantile(baseline, prob = 0.975)
        # hist(baseline, breaks = 60, col = matlab.like(60), lty = "blank")
        # abline(v = q)
        
        present = target[ll, m]; present
        sum = ifelse(q < present, 1, 0)
        
        monthly_anom = cbind(monthly_anom, sum)
        
      }
      
      ll_anom = rbind(ll_anom, monthly_anom)
      
    }
    
    colnames(ll_anom) = c("jan", "feb", "mar", "apr", "may", "jun",
                          "jul", "aug", "sep", "oct", "nov", "dec")
    
    year_sum = rowSums(ll_anom)
    
    yy_anom = cbind(yy_anom, year_sum)
    
  }
  
  colnames(yy_anom) = 1980:2018
  
  yy_anom = cbind(Target[1:2], yy_anom)
  
  save(yy_anom, file = paste0("~/extreme_normalizations/results/", data, "/SST_TippingPoints.RData"))
  
  beepr::beep(2)
  
  
}

calculate_anomalies("HadI")
calculate_anomalies("COBE")
calculate_anomalies("ER")

# plot(1980:2018, colMeans(yy_anom[3:41]), type = "o", pch = 20, axes = F); axis(1, at = seq(1980, 2018, 2)); axis(2, las = 2)