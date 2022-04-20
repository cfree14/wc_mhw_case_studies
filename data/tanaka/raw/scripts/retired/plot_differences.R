library(raster)
library(colorRamps)
library(ggpubr)
library(rnaturalearth)
library(sf)
library(rgdal)
library(dplyr)
library(maps)
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

world <- fortify(getMap())

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

load(paste0(dir, '/extreme_normalizations/data/eez_sf_dataframe_0.001.RData'))
load(paste0(dir, '/extreme_normalizations/data/lme_sf_dataframe_0.001.RData')) 
load(paste0(dir, '/extreme_normalizations/data/meow_sf_dataframe.RData'))

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

rank_mean = function(region){
  
  # region = "eez"
  
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
    
    load(paste0(dir, "/extreme_normalizations/results/HadI/anomalies_", period[[i]], "_", cutoff, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    summary(tas)
    # hadi <- st_intersection(tas, shape)
    hadi <- st_intersection(tas, st_make_valid(shape))
    # hadi <- st_intersection(tas, st_buffer(shape, 0))    
    hadi$sum = range01(hadi$sum)
    hadi <- hadi %>% group_by(UNIT) %>% 
      summarise(median = median(sum, na.rm = T),
                sd = sd(sum, na.rm = T), 
                n = n()) %>%
      mutate(se = sd/sqrt(n),
             lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
             upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
    hadi$source = "HadISST v1.1"; hadi$period = period[[i]]
    
    load(paste0(dir, "/extreme_normalizations/results/COBE/anomalies_", period[[i]], "_", cutoff, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    # cobe <- st_intersection(tas, shape)
    cobe <- st_intersection(tas, st_make_valid(shape))
    # cobe <- st_intersection(tas, st_buffer(shape, 0))
    cobe$sum = range01(cobe$sum)
    cobe <- cobe %>% group_by(UNIT) %>% 
      summarise(median = median(sum, na.rm = T),
                sd = sd(sum, na.rm = T), 
                n = n()) %>%
      mutate(se = sd/sqrt(n),
             lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
             upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
    cobe$source = "COBE v2"; cobe$period = period[[i]]
    
    # load(paste0(dir, "/extreme_normalizations/results/ER/anomalies_", period[[i]], "_", cutoff, ".RData"))
    # anom = anom[, c(1:2, 15)]
    # tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    # # er <- st_intersection(tas, shape)
    # er <- st_intersection(tas, st_make_valid(shape))
    # # er <- st_intersection(tas, st_buffer(shape, 0))
    # er$sum = range01(er$sum)
    # er <- er %>% group_by(UNIT) %>%
    #   summarise(median = median(sum, na.rm = T),
    #             sd = sd(sum, na.rm = T),
    #             n = n()) %>%
    #   mutate(se = sd/sqrt(n),
    #          lower.ci = median - qt(1 - (0.05 / 2), n - 1) * se,
    #          upper.ci = median + qt(1 - (0.05 / 2), n - 1) * se)
    # er$source = "ERSST v4"; er$period = period[[i]]
    
    tas = rbind(hadi, cobe)
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
  
  d1 = tas_combined %>% 
    subset(period == "1980-1989") %>% 
    mutate(p1 = median) %>% 
    select("UNIT", "p1", "source")
  
  d2 = tas_combined %>% 
    subset(period == "2010-2018") %>% 
    mutate(p2 = median) %>% 
    select("UNIT", "p2", "source")
  
  d = merge(d1, d2) %>% mutate(diff = p2-p1)
  
  d <- d %>% group_by(UNIT) %>%
    summarise(mean = mean(diff),
              sd = sd(diff),
              n = n()) %>%
    mutate(se = sd/sqrt(n),
           lower.ci = mean - qt(1 - (0.05 / 2), n - 1) * se,
           upper.ci = mean + qt(1 - (0.05 / 2), n - 1) * se)
  
  # top = d %>% top_n(10, mean)
  # bottom = d %>% top_n(-10, mean)
  # d = tbl_df(bind_rows(top, bottom))
  
  p = d %>% 
    mutate(UNIT = forcats::fct_reorder(UNIT, mean)) %>% 
    ggplot() +
    geom_segment(aes(
      color = mean,
      x = UNIT, 
      xend = UNIT,
      y = lower.ci, 
      yend = upper.ci),
      size = 0.5) +
    geom_point(aes(
      color = mean,
      y = mean,
      x = UNIT),
      size = 2) +
    coord_flip() +
    scale_color_gradientn(colors = rev(ipcc_temp), "", values = scales::rescale(c(-0.5, -0.1, 0, 0.1, 0.5))) +
    xlab("") +
    ylab("") +
    theme_pubr(I(10)) +
    theme(legend.position = c(0.1, 0.9))
  
  print(p)
  
  pdf(paste0(dir, "/Desktop/", cutoff, "_Difference_", region, ".pdf"), height = 18, width = 8)
  print(p)
  dev.off()
  
}

rank_mean("lme")
rank_mean("eez")



