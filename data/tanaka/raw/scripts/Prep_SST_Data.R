library(raster)
library(colorRamps)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)

rm(list = ls())

#COBE
df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/COBESST/COBE_sst.nc"), varname = "sst")
df = raster::rotate(df) #rotate to -180:180
df = df[[241:2040]] #trim to 1870-2019
assign("df", df, .GlobalEnv)
df = readAll(df)
save(df, file = paste0("/Users/", Sys.info()[7], "/Desktop/COBESST/COBE_SST.RData"))


#Hadley
df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/HadISST/HadI_sst.nc"), varname = "sst")
# df = stack(paste0("/Users/", Sys.info()[7], "/Dropbox (MBA)/PAPER Kisei heat extremes/data/original_files/HadISST/HadI_sst_ice.nc"), varname = "sic")
df = df[[1:1800]] #trim to 1870-2019
assign("df", df, .GlobalEnv)
df = readAll(df)
save(df, file = paste0("/Users/", Sys.info()[7], "/Desktop/HadISST/HadI_SST.RData"))


#ERSST
df = stack(paste0("/Users/", Sys.info()[7], "/Desktop/ERSST/ERSSTv5.nc"), varname = "sst")
df = raster::rotate(df) #rotate to -180:180
df = df[[193:1992]] #trim to 1870-2019
assign("df", df, .GlobalEnv)
df = readAll(df)
save(df, file = paste0("/Users/", Sys.info()[7], "/Dropbox (MBA)/PAPER Kisei heat extremes/data/ER_SST.RData"))
