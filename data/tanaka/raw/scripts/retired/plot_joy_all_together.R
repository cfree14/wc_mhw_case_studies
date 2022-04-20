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
library(patchwork)

rm(list = ls())

percentile = c(0.95, 0.98)[2]

period = c("1980-1989", "1990-1999", "2000-2009", "2010-2019")

data = c("HadI", "COBE", "ER")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

#IPCC color theme
ipcc_cols <- c(rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
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

ipcc_4_cols <- c(rgb(153, 0, 2, maxColorValue = 255, alpha = 255),
                 rgb(196, 121, 0, maxColorValue = 255, alpha = 255),
                 rgb(112, 160, 205, maxColorValue = 255, alpha = 255),
                 rgb(0, 52, 102, maxColorValue = 255, alpha = 255))

#load EEZ and LME shp files
load(paste0('/Users/', Sys.info()[7], '/extreme_normalizations/data/eez_sf_dataframe_0.001.RData'))
load(paste0('/Users/', Sys.info()[7], '/extreme_normalizations/data/lme_sf_dataframe_0.001.RData'))

rank_joy_eez_lme = function(region){
  
  # region = "lme"
  # region = "eez"
  
  if (region == "lme") {
    shape = lme; shape$UNIT = shape$LME_NAME
  } 
  
  if (region == "eez") {
    shape = eez; shape$UNIT = shape$Country
  } 
  
  tas_combined = NULL
  
  for (i in 1:length(period)){
    
    # i = 1
    
    load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/results/HadI/extremes_", period[[i]], "_", percentile, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    summary(tas)
    hadi <- st_intersection(tas, st_make_valid(shape))
    hadi$sum = range01(hadi$sum)
    prov_levels <- hadi %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum,UNIT) %>%
      group_by(UNIT) %>%
      mutate(mean_of_mean = mean(sum))
    levels <- unique(prov_levels$UNIT[order(prov_levels$mean_of_mean)])
    hadi$UNIT <- factor(hadi$UNIT, levels = levels, ordered=TRUE)
    df = table(hadi$UNIT)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "UNIT"
    hadi = merge(hadi, df)
    hadi$source = "HadISST v1.1"; hadi$period = period[[i]]
    
    load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/results/COBE/extremes_", period[[i]], "_", percentile, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    cobe <- st_intersection(tas, st_make_valid(shape))
    cobe$sum = range01(cobe$sum)
    prov_levels <- cobe %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum,UNIT) %>%
      group_by(UNIT) %>%
      mutate(mean_of_mean = mean(sum))
    levels <- unique(prov_levels$UNIT[order(prov_levels$mean_of_mean)])
    cobe$UNIT <- factor(cobe$UNIT, levels = levels, ordered=TRUE)
    df = table(cobe$UNIT)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "UNIT"
    cobe = merge(cobe, df)
    cobe$source = "COBE v2"; cobe$period = period[[i]]
    
    load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/results/ER/extremes_", period[[i]], "_", percentile, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    er <- st_intersection(tas, st_make_valid(shape))
    er$sum = range01(er$sum)
    prov_levels <- er %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum,UNIT) %>%
      group_by(UNIT) %>%
      mutate(mean_of_mean = mean(sum))
    levels <- unique(prov_levels$UNIT[order(prov_levels$mean_of_mean)])
    er$UNIT <- factor(er$UNIT, levels = levels, ordered=TRUE)
    df = table(er$UNIT)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "UNIT"
    er = merge(er, df)
    er$source = "ERSST v4"; er$period = period[[i]]
    
    tas = rbind(hadi, cobe, er)
    
    tas_combined = rbind(tas_combined, tas)
    
    
  }
  
  # remove disputed EEZs (see FML 2019 to find out why)
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
    
  } 
  
  # count changes in number of unit between 1980-1989 and 2010-2019
  n1 = tas_combined %>% 
    subset(period %in% c("1980-1989")) %>%
    group_by(UNIT) %>% 
    summarise(sum = round(mean(sum), 2)) %>% 
    subset(sum >= 0.66) #use upper tercile
  
  n2 = tas_combined %>% 
    subset(period %in% c("2010-2019")) %>%
    group_by(UNIT) %>% 
    summarise(sum = round(mean(sum), 2)) %>% 
    subset(sum >= 0.66) #use upper tercile
  
  # remove ERSST and average over 1980-2019
  tas_combined = tas_combined %>% 
    mutate(location_id = as.character(geometry)) %>% 
    subset(source %in% c("HadISST v1.1", "COBE v2")) %>% 
    group_by(UNIT, location_id) %>% 
    summarise(sum = mean(sum))
  
  # Reorder levels by 1980-2019
  prov_levels <- tas_combined %>% 
    dplyr::select(sum, UNIT) %>%
    group_by(UNIT) %>%
    mutate(mean_of_mean = median(sum, na.rm = T))
  
  levels <- unique(prov_levels$UNIT[order(prov_levels$mean_of_mean)])
  tas_combined$UNIT <- factor(tas_combined$UNIT, levels = levels, ordered = TRUE)
  
  ipcc_temp_expand = colorRampPalette(rev(ipcc_cols))
  ipcc_temp_expand = ipcc_temp_expand(length(unique(tas_combined$UNIT)))
  
  # summary = tas_combined %>% 
  #   group_by(UNIT, period) %>% 
  #   summarise_each(funs(mean, sd, se = sd(.)/sqrt(n())), sum)
  # 
  # summary = as.data.frame(summary)
  # summary = summary[,c('UNIT', 'period', 'mean', 'sd', 'se')]
  # summary$UNIT = as.character(summary$UNIT)
  # summary <- summary[order(summary$UNIT),]
  # summary[,3:5] = round(summary[,3:5], 2)
  # summary$UNIT[duplicated(summary$UNIT)] <- ""
  # colnames(summary) = c("Unit", "Period", "Mean", "SD", "SE")
  # 
  # s1 = summary %>% subset(Period == "1980-1989")
  # s2 = summary %>% subset(Period == "1990-1999")
  # s3 = summary %>% subset(Period == "2000-2009")
  # s4 = summary %>% subset(Period == "2010-2019")
  # 
  # summary = cbind(s1, s2, s3, s4)
  # write_csv(summary, paste0('~/Desktop/', region, "_", cutoff, ".csv"))
  
  p = ggplot(tas_combined, aes(x = sum, y = UNIT, fill = UNIT)) +
    geom_joy(scale = 2, alpha = 0.8, size = 0.3, bandwidth = 0.03) +
    theme_minimal() +
    scale_y_discrete(expand = c(-0.01, 0)) + 
    scale_x_continuous(limits = c(0, 1), expand = c(-0.05, 0.1), breaks = c(0.25,  0.75)) +
    scale_fill_cyclical(values = ipcc_temp_expand)+
    ylab(NULL) + xlab(NULL) +
    theme(axis.text.y = element_text(size = 10),
          panel.background = element_blank(),
          legend.position = "none")
  
  p
  
  pdf(paste0("~/Desktop/Joy_", region, "_", percentile, ".pdf"), height = 15, width = 10)
  print(p)
  dev.off()
  
  print(paste0("number_of_units_in_upper_tercile_1980_1989 = ", nrow(n1)))
  print(paste0("number_of_units_in_upper_tercile_2010_2019 = ", nrow(n2)))
  
  return(tas_combined)
  
}
rank_joy_bgcp = function(){
  
  tas_combined = NULL
  
  for (i in 1:length(period)){
    
    # i = 1
    
    load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/data/bgcp_raster_0.25.RData"))
    load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/results/HadI/extremes_", period[[i]], "_", percentile, ".RData"))
    anom = anom[, c(1:2, 15)]
    x <- raster(xmn  =-180, xmx = 180, ymn = -90, ymx = 90, res = 1, crs = "+proj=longlat +datum=WGS84")
    anom <- rasterize(anom[, c('x', 'y')], x, anom[, 'sum'], fun = mean)
    bgcp = resample(bgcp, anom, method = "bilinear") 
    anom = as.data.frame(rasterToPoints(anom))
    bgcp = as.data.frame(rasterToPoints(bgcp))
    colnames(anom)[3] = "sum"
    colnames(bgcp)[3] = "bgcp"
    bgcp = merge(anom, bgcp, all = T)
    bgcp$bgcp = round(bgcp$bgcp, 0)
    bgcp$bgcp = as.factor(as.character(bgcp$bgcp))
    bgcp_names <- read_csv(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/data/NAME_BGCP_2019_REYGONDEAU.csv"))
    bgcp_names = bgcp_names[,c("NAME", "BGCP")]
    colnames(bgcp_names) = c("name", "bgcp")
    bgcp = merge(bgcp, bgcp_names)
    bgcp$bgcp = bgcp$name
    bgcp$sum = (bgcp$sum-min(bgcp$sum, na.rm = T))/(max(bgcp$sum, na.rm = T) - min(bgcp$sum, na.rm = T))
    prov_levels <- bgcp %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum, bgcp) %>%
      group_by(bgcp) %>%
      mutate(mean_of_mean = mean(sum, na.rm = T))
    levels <- unique(prov_levels$bgcp[order(prov_levels$mean_of_mean)])
    bgcp$bgcp <- factor(bgcp$bgcp, levels = levels, ordered = TRUE)
    df = table(bgcp$bgcp)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "bgcp"
    bgcp = merge(bgcp, df)
    df = bgcp[complete.cases(bgcp), ]
    df = df[,c("x", "y", "bgcp", "sum")]
    hadi = df
    hadi$source = "HadISST v1.1"; hadi$period = period[[i]]
    
    load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/data/bgcp_raster_0.25.RData"))
    load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/results/COBE/extremes_", period[[i]], "_", percentile, ".RData"))
    anom = anom[, c(1:2, 15)]
    x <- raster(xmn  =-180, xmx = 180, ymn = -90, ymx = 90, res = 1, crs = "+proj=longlat +datum=WGS84")
    anom <- rasterize(anom[, c('x', 'y')], x, anom[, 'sum'], fun = mean)
    bgcp = resample(bgcp, anom, method = "bilinear") 
    anom = as.data.frame(rasterToPoints(anom))
    bgcp = as.data.frame(rasterToPoints(bgcp))
    colnames(anom)[3] = "sum"
    colnames(bgcp)[3] = "bgcp"
    bgcp = merge(anom, bgcp, all = T)
    bgcp$bgcp = round(bgcp$bgcp, 0)
    bgcp$bgcp = as.factor(as.character(bgcp$bgcp))
    bgcp_names <- read_csv(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/data/NAME_BGCP_2019_REYGONDEAU.csv"))
    bgcp_names = bgcp_names[,c("NAME", "BGCP")]
    colnames(bgcp_names) = c("name", "bgcp")
    bgcp = merge(bgcp, bgcp_names)
    bgcp$bgcp = bgcp$name
    bgcp$sum = (bgcp$sum-min(bgcp$sum, na.rm = T))/(max(bgcp$sum, na.rm = T) - min(bgcp$sum, na.rm = T))
    prov_levels <- bgcp %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum, bgcp) %>%
      group_by(bgcp) %>%
      mutate(mean_of_mean = mean(sum, na.rm = T))
    levels <- unique(prov_levels$bgcp[order(prov_levels$mean_of_mean)])
    bgcp$bgcp <- factor(bgcp$bgcp, levels = levels, ordered = TRUE)
    df = table(bgcp$bgcp)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "bgcp"
    bgcp = merge(bgcp, df)
    df = bgcp[complete.cases(bgcp), ]
    df = df[,c("x", "y", "bgcp", "sum")]
    cobe = df
    cobe$source = "COBE v2"; cobe$period = period[[i]]
    
    load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/data/bgcp_raster_0.25.RData"))
    load(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/results/ER/extremes_", period[[i]], "_", percentile, ".RData"))
    anom = anom[, c(1:2, 15)]
    x <- raster(xmn  =-180, xmx = 180, ymn = -90, ymx = 90, res = 2, crs = "+proj=longlat +datum=WGS84")
    anom <- rasterize(anom[, c('x', 'y')], x, anom[, 'sum'], fun = mean)
    bgcp = resample(bgcp, anom, method = "bilinear") 
    anom = as.data.frame(rasterToPoints(anom))
    bgcp = as.data.frame(rasterToPoints(bgcp))
    colnames(anom)[3] = "sum"
    colnames(bgcp)[3] = "bgcp"
    bgcp = merge(anom, bgcp, all = T)
    bgcp$bgcp = round(bgcp$bgcp, 0)
    bgcp$bgcp = as.factor(as.character(bgcp$bgcp))
    bgcp_names <- read_csv(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/data/NAME_BGCP_2019_REYGONDEAU.csv"))
    bgcp_names = bgcp_names[,c("NAME", "BGCP")]
    colnames(bgcp_names) = c("name", "bgcp")
    bgcp = merge(bgcp, bgcp_names)
    bgcp$bgcp = bgcp$name
    bgcp$sum = (bgcp$sum-min(bgcp$sum, na.rm = T))/(max(bgcp$sum, na.rm = T) - min(bgcp$sum, na.rm = T))
    prov_levels <- bgcp %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum, bgcp) %>%
      group_by(bgcp) %>%
      mutate(mean_of_mean = mean(sum, na.rm = T))
    levels <- unique(prov_levels$bgcp[order(prov_levels$mean_of_mean)])
    bgcp$bgcp <- factor(bgcp$bgcp, levels = levels, ordered = TRUE)
    df = table(bgcp$bgcp)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "bgcp"
    bgcp = merge(bgcp, df)
    df = bgcp[complete.cases(bgcp), ]
    df = df[,c("x", "y", "bgcp", "sum")]
    er = df
    er$source = "ERSST v4"; er$period = period[[i]]
    
    tas = rbind(hadi, cobe, er)
    
    tas_combined = rbind(tas_combined, tas)
    
    
  }
  
  # count changes in number of unit between 1980-1989 and 2010-2019
  n1 = tas_combined %>% 
    subset(period %in% c("1980-1989")) %>%
    group_by(bgcp) %>% 
    summarise(sum = round(mean(sum), 2)) %>% 
    subset(sum >= 0.66)
  
  n2 = tas_combined %>% 
    subset(period %in% c("2010-2019")) %>%
    group_by(bgcp) %>% 
    summarise(sum = round(mean(sum), 2)) %>% 
    subset(sum >= 0.66)
  
  # remove ERSST and average over 1980-2019
  tas_combined = tas_combined %>% 
    subset(source %in% c("HadISST v1.1", "COBE v2")) %>% 
    group_by(bgcp, x, y) %>% 
    summarise(sum = mean(sum))# remove ERSST
  
  tas_combined = tas_combined %>% mutate(bgcp = gsub("\xca", "", bgcp)) 

  prov_levels <- tas_combined %>% # Reorder levels by 2010-2019
    dplyr::select(sum, bgcp) %>%
    group_by(bgcp) %>%
    mutate(mean_of_mean = median(sum, na.rm = T))
  
  levels <- unique(prov_levels$bgcp[order(prov_levels$mean_of_mean)])
  tas_combined$bgcp <- factor(tas_combined$bgcp, levels = levels, ordered = TRUE)
  
  ipcc_temp_expand = colorRampPalette(rev(ipcc_cols))
  ipcc_temp_expand = ipcc_temp_expand(length(unique(tas_combined$bgcp)))
  
  p = tas_combined %>% 
    ggplot(aes(x = sum, y = bgcp, fill = bgcp)) +
    geom_joy(scale = 2, alpha = 0.8, size = 0.3, bandwidth = 0.03) +
    theme_minimal() +
    scale_x_continuous(limits = c(0, 1), expand = c(-0.05, 0.1), breaks = c(0.25,  0.75)) +
    scale_fill_cyclical(values = ipcc_temp_expand)+
    ylab(NULL) + xlab(NULL) +
    theme(axis.text.y = element_text(size = 10),
          panel.background = element_blank(),
          legend.position = "none")
  
  p
  
  pdf(paste0("~/Desktop/Joy_bgcp_", percentile, ".pdf"), height = 10, width = 10)
  print(p)
  dev.off()
  
  print(paste0("number_of_units_in_upper_tercile_1980_1989 = ", nrow(n1)))
  print(paste0("number_of_units_in_upper_tercile_2010_2019 = ", nrow(n2)))
  
  return(tas_combined)
  
}

lme = rank_joy_eez_lme("lme")
eez = rank_joy_eez_lme("eez")
bgcp = rank_joy_bgcp()

df1 = lme %>% group_by(UNIT) %>% summarise(m = mean(sum), freq = n())  %>% filter(freq > 20) %>% top_n(15, m)
df2 = lme %>% group_by(UNIT) %>% summarise(m = mean(sum), freq = n())  %>% filter(freq > 20) %>% top_n(-15, m)
sub = rbind(df1, df2)
sub = as.vector(sub$UNIT)
lme_sub = subset(lme, UNIT %in% sub)
lme_sub = lme_sub %>% group_by(UNIT) %>% mutate(m = median(sum)) %>% arrange(UNIT, m)
lme_sub = lme_sub[,c("UNIT", "sum")]; lme_sub = as.data.frame(lme_sub); lme_sub = lme_sub[1:2]; lme_sub$class = "LME"

df1 = eez %>% group_by(UNIT) %>% summarise(m = mean(sum), freq = n()) %>% filter(freq > 20) %>% top_n(15, m)
df2 = eez %>% group_by(UNIT) %>% summarise(m = mean(sum), freq = n())  %>% filter(freq > 20) %>% top_n(-15, m)
sub = rbind(df1, df2)
sub = as.vector(sub$UNIT)
eez_sub = subset(eez, UNIT %in% sub)
eez_sub = eez_sub %>% group_by(UNIT) %>% mutate(m = median(sum)) %>% arrange(UNIT, m)
eez_sub = eez_sub[,c("UNIT", "sum")]; eez_sub = as.data.frame(eez_sub); eez_sub = eez_sub[1:2]; eez_sub$class = "EEZ"

df1 = bgcp %>% group_by(bgcp) %>% summarise(m = mean(sum), freq = n()) %>% filter(freq > 20) %>% top_n(15, m); df1 = df1 %>% top_n(15)
df2 = bgcp %>% group_by(bgcp) %>% summarise(m = mean(sum), freq = n()) %>% filter(freq > 20) %>% top_n(-15, m)
sub = rbind(df1, df2)
sub = as.vector(sub$bgcp)
bgcp_sub = subset(bgcp, bgcp %in% sub)
bgcp$bgcp = as.character(bgcp$bgcp)
bgcp_sub = bgcp_sub[,c("bgcp", "sum")]; bgcp_sub = as.data.frame(bgcp_sub); colnames(bgcp_sub)[1] = "UNIT"; bgcp_sub$class = "BGCP"

ipcc_temp_expand = colorRampPalette(rev(ipcc_cols))

#####################
### Plot Figure 2 ###
#####################

pdf(paste0("~/Desktop/Fig2_LME.", percentile, "_", Sys.Date(), ".pdf"), width = 8, height = 6)
p1 = lme_sub %>% 
  mutate(UNIT = forcats::fct_reorder(UNIT, sum)) %>% 
  ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
  geom_joy(scale = 10, alpha = 0.8, size = 0.1) +
  theme_joy(grid = F) +
  scale_y_discrete(expand = c(0.05, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0, 0.5, 1)) +
  scale_fill_cyclical(values = ipcc_temp_expand(length(unique(lme_sub$UNIT))))+
  ylab(NULL) + xlab(NULL) +
  coord_fixed(ratio = 0.1) + 
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")+ 
  ggtitle("(b) Large Marine Ecosystem")
print(p1)
dev.off()

pdf(paste0("~/Desktop/Fig2_EEZ.", percentile, "_", Sys.Date(), ".pdf"), width = 8, height = 6)
p2 = eez_sub %>% 
  mutate(UNIT = forcats::fct_reorder(UNIT, sum)) %>% 
  ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
  geom_joy(scale = 10, alpha = 0.8, size = 0.1) +
  theme_joy(grid = F) +
  scale_y_discrete(expand = c(0.05, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0, 0.5, 1)) +
  scale_fill_cyclical(values = ipcc_temp_expand(length(unique(eez_sub$UNIT))))+
  ylab(NULL) + xlab(NULL) +
  coord_fixed(ratio = 0.1) + 
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none") + 
  ggtitle("(c) Exclusive Ecoonmic Zone")
print(p2)
dev.off()

pdf(paste0("~/Desktop/Fig2_BGCP.", percentile, "_", Sys.Date(), ".pdf"), width = 8, height = 6)
p3 = bgcp_sub %>%  
  mutate(UNIT = gsub("\xca", "", UNIT)) %>% 
  mutate(UNIT = forcats::fct_reorder(UNIT, sum)) %>% 
  ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
  geom_joy(scale = 10, alpha = 0.8, size = 0.1) +
  theme_joy(grid = F) +
  scale_y_discrete(expand = c(0.05, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0, 0.5, 1)) +
  scale_fill_cyclical(values = ipcc_temp_expand(length(unique(bgcp_sub$UNIT))))+
  ylab(NULL) + xlab(NULL) +
  coord_fixed(ratio = 0.1) + 
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none")+ 
  ggtitle("(a) Biogeographic Province")
print(p3)
dev.off()

p3 + p1 + p2
dev.off()
