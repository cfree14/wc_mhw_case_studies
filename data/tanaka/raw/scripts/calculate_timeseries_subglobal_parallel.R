rm(list = ls())

library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sp)
library(maptools)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
library(doParallel)

cores = detectCores()/2
registerDoParallel(cores = cores)

p = c(0.95, 0.98)[2]

calculate_anomalies = function(data){

  data = c("HadI", "COBE", "ER")[3]
  
  setwd(paste0("/Users/", Sys.info()[7], "/Dropbox/PAPER Kisei heat extremes/data/"))
  
  load(paste0(data, "_SST.RData"))
  
  # e = extent(-132, -110, 22.5, 47.5)
  # e = extent(-72, -66, 41, 45)
  # df = crop(df, e); rm(e)
  
  # set baseline Jan 1870 - Dec 1919, 50 years
  Baseline <- df[[1:600]] 
  values(Baseline)[values(Baseline) == -1000] = -1.8
  
  names(Baseline)
  
  Baseline <- Baseline %>% rasterToPoints() %>% data.frame()
  
  Target <- df[[361:1800]] #Jan 1900 - Dec 2019
  values(Target)[values(Target) == -1000] = -1.8 
  
  Target <- Target %>% rasterToPoints() %>% data.frame()
  
  #remove polar regions
  names(Target)
  latlon = Target[,c(1:2)]; plot(latlon, pch = ".")
  coordinates(latlon) = ~x+y
  statarea <- rgdal::readOGR("/Users/ktanaka/Dropbox/PAPER Kisei heat extremes/data/World_Seas_IHO_v1/World_Seas.shp")
  CRS.new <- CRS("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0+datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0") #EPSG:102003
  proj4string(latlon) <- CRS.new
  proj4string(statarea) <- CRS.new
  area <- over(latlon,statarea)
  colnames(area)[1] = "area"
  area = as.data.frame(area[,1])
  Baseline = cbind(area, Baseline)
  Target = cbind(area, Target)
  colnames(Baseline)[1] = "area"
  colnames(Target)[1] = "area"
  Baseline = Baseline %>% subset(area %in% c("South Pacific Ocean", "North Pacific Ocean", "Indian Ocean", "South Atlantic Ocean", "North Atlantic Ocean"))
  Target = Target %>% subset(area %in% c("South Pacific Ocean", "North Pacific Ocean", "Indian Ocean", "South Atlantic Ocean", "North Atlantic Ocean"))
  
  Baseline = Baseline[,c(2:603)]
  Target = Target[,c(2:1443)]

  yy_anom = NULL
  
  for (y in 1:120) { #every year between 1980-2018 (39) or 1900-2018 (119)
    
    # y = 100
    
    interval_year = seq(3, dim(Target)[2], by = 12) 
    
    first_month = interval_year[y]
    last_month = first_month + 11
    
    target = Target[,first_month:last_month]; names(target); dim(target) # target year
    # ll_anom = NULL
    
    # ptime <- system.time({
    
    r <- foreach(ll = 1:dim(target)[1], .combine = rbind) %dopar% {
      
      # ll = 2
      
      monthly_anom = NULL
      
      for (m in 1:12) { # every month
        
        # m = 1
        
        interval_month = seq(m+2, dim(Baseline)[2], by = 12)
        
        baseline = Baseline[ll, c(interval_month)]; names(baseline) #pick corresponding month from every baseline year
        baseline = t(baseline)
        baseline = as.data.frame(baseline)
        baseline = baseline[,1]
        
        q = quantile(baseline, prob = p)
        # hist(baseline, breaks = 60, col = matlab.like(60), lty = "blank")
        # abline(v = q)
        
        present = target[ll, m]; present
        sum = ifelse(q < present, 1, 0)
        
        monthly_anom = cbind(monthly_anom, sum)
        
      }
      
      monthly_anom
      
      # ll_anom = rbind(ll_anom, monthly_anom)
      
    }
    
    # })[3]; ptime
    
    r = as.data.frame(r)
    
    colnames(r) = c("jan", "feb", "mar", "apr", "may", "jun",
                    "jul", "aug", "sep", "oct", "nov", "dec")
    
    # year_sum = rowSums(r)
    year_sum = colSums(r) #sum number of grid above monthly baseline 
    year_sum = colSums(r)/dim(Baseline)[1] #divide by total grid cell
    year_sum = as.data.frame(year_sum)
    year_sum$time = paste0(1899+y, "-", seq(1, 12, 1))
    
    yy_anom = rbind(yy_anom, year_sum)
    
    print(y)
    
  }
  
  plot(yy_anom$year_sum, type = "o", axes = F, pch = ".", xlab = "", ylab = "")
  axis(1)
  axis(2, las = 2, at = seq(0, 0.8, 0.1))
  abline(h = 0.5, lty = 2)
  
  save(yy_anom, file = paste0("/Users/", 
                              Sys.info()[7], 
                              "/extreme_normalizations/results/", 
                              data,
                              "/timeseries_sub_global_", p, ".RData"))
  
}

calculate_anomalies("HadI")
calculate_anomalies("COBE")
calculate_anomalies("ER")
