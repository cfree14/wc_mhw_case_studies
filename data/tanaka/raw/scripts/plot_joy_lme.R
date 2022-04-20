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

# rescale function
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

# coarse shape files, see prep_shapefile.R
load(paste0('data/eez_sf_dataframe_0.001.RData'))
load(paste0('data/lme_sf_dataframe_0.001.RData'))

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

ipcc_temp_4_cols <- c(rgb(153, 0, 2, maxColorValue = 255, alpha = 255),
                      rgb(196, 121, 0, maxColorValue = 255, alpha = 255),
                      rgb(112, 160, 205, maxColorValue = 255, alpha = 255),
                      rgb(0, 52, 102, maxColorValue = 255, alpha = 255))

ipcc_temp_expand = colorRampPalette(rev(ipcc_temp))

rank_joy_lme_eez = function(region){
  
  region = "lme"
  
  shape = lme; shape$UNIT = shape$LME_NAME
  
  tas_combined = NULL
  
  for (i in 1:length(period)){
    
    # i = 1
    
    load(paste0("outputs/HadI/extremes_", period[[i]], "_", percentile, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    summary(tas)
    # hadi <- st_intersection(tas, shape)
    hadi <- st_intersection(tas, st_make_valid(shape))
    # hadi <- st_intersection(tas, st_buffer(shape, 0))
    hadi$sum = range01(hadi$sum)
    prov_levels <- hadi %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum,UNIT) %>%
      group_by(UNIT) %>%
      mutate(unit_median = median(sum))
    levels <- unique(prov_levels$UNIT[order(prov_levels$unit_median)])
    hadi$UNIT <- factor(hadi$UNIT, levels = levels, ordered = TRUE)
    df = table(hadi$UNIT)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "UNIT"
    hadi = merge(hadi, df)
    hadi$source = "HadISST v1.1"; hadi$period = period[[i]]
    
    load(paste0("outputs/COBE/extremes_", period[[i]], "_", percentile, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    # cobe <- st_intersection(tas, shape)
    cobe <- st_intersection(tas, st_make_valid(shape))
    # cobe <- st_intersection(tas, st_buffer(shape, 0))
    cobe$sum = range01(cobe$sum)
    prov_levels <- cobe %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum,UNIT) %>%
      group_by(UNIT) %>%
      mutate(unit_median = median(sum))
    levels <- unique(prov_levels$UNIT[order(prov_levels$unit_median)])
    cobe$UNIT <- factor(cobe$UNIT, levels = levels, ordered = TRUE)
    df = table(cobe$UNIT)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "UNIT"
    cobe = merge(cobe, df)
    cobe$source = "COBE v2"; cobe$period = period[[i]]
    
    load(paste0("outputs/ER/extremes_", period[[i]], "_", percentile, ".RData"))
    anom = anom[, c(1:2, 15)]
    tas <- st_as_sf(x = anom, coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0" )
    # er <- st_intersection(tas, shape)
    er <- st_intersection(tas, st_make_valid(shape))
    # er <- st_intersection(tas, st_buffer(shape, 0))
    er$sum = range01(er$sum)
    prov_levels <- er %>% # Reorder levels by mean risk by privince 
      dplyr::select(sum,UNIT) %>%
      group_by(UNIT) %>%
      mutate(unit_median = median(sum))
    levels <- unique(prov_levels$UNIT[order(prov_levels$unit_median)])
    cobe$UNIT <- factor(cobe$UNIT, levels = levels, ordered = TRUE)
    df = table(er$UNIT)
    df = as.data.frame(df)
    df = subset(df, Freq > 2)
    colnames(df)[1] = "UNIT"
    er = merge(er, df)
    er$source = "ERSST v5"; er$period = period[[i]]
    
    tas = rbind(hadi, cobe, er)
    
    tas_combined = rbind(tas_combined, tas)
    
    
  }
    
  # #pick large LMEs
  # pdf(paste0("~/Desktop/joy_", region, "_selected_", percentile, ".pdf"), height = 10, width = 10)
  # 
  # big_lmes = tas_combined %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 2000)
  # big_lmes = as.data.frame(big_lmes)
  # big_lmes = big_lmes[, 1, drop = FALSE]
  # tas_combined_sub = subset(tas_combined, UNIT %in% dplyr::pull(big_lmes))
  # 
  # p = tas_combined_sub %>% 
  #   mutate(location_id = as.character(geometry)) %>%
  #   subset(source %in% c("HadISST v1.1", "COBE v2")) %>%
  #   group_by(UNIT, period, location_id) %>%
  #   summarise(sum = mean(sum)) %>%
  #   ggplot()  +
  #   geom_density(aes(x = sum, fill = period), alpha = 0.8, size = 0.01) +
  #   scale_x_continuous(
  #     limits = c(0, 1),
  #     expand = c(0.05, 0.01),
  #     breaks = c(0, 0.5, 1)) +
  #   scale_fill_manual(values = rev(ipcc_temp_4_cols), "") +
  #   facet_wrap( ~ UNIT, scales = "fixed") +
  #   coord_fixed(ratio = 0.1) + 
  #   ylab(NULL) + xlab(NULL) +
  #   theme(
  #     axis.text.y = element_blank(),
  #     axis.ticks = element_blank(),
  #     legend.position = "top")
  # 
  # print(p)
  # 
  # dev.off()
  
  tas_combined$UNIT = gsub("Southeast ", "SE ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Northeast ", "NE ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Northwest ", "NW ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Southeast ", "SE ", tas_combined$UNIT, fixed = T)
  
  tas_combined$UNIT = gsub("East ", "E ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("West ", "W ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("North ", "N ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("South ", "S ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Central ", "Cent ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Central", "Cent", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Pacific ", "Pac ", tas_combined$UNIT, fixed = T)
  
  tas_combined$UNIT = gsub("Continental ", "Continenta ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Current ", "Curr ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("California ", "Calif ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("Australian ", "Australia ", tas_combined$UNIT, fixed = T)
  tas_combined$UNIT = gsub("U.S. ", "US ", tas_combined$UNIT, fixed = T)
  
  tas_combined$UNIT = gsub("Canadian High Arctic - N Greenland", "Canada N Greenland", tas_combined$UNIT, fixed = T)
  
  
  ### just keep HadISST and COBESST. discard unecessary columns ###
  tas_combined = subset(tas_combined, source %in% c("HadISST v1.1", "COBE v2")) # remove ERSSTv5
  tas_combined = tas_combined %>% as.data.frame() %>% select(UNIT, period, source, sum, geometry)
  tas_combined = tas_combined[!is.na(tas_combined$UNIT),]
  
  # count changes in number of unit between 1980-1989 and 2010-2019
  n1 = tas_combined %>% 
    subset(period %in% c("1980-1989")) %>%
    group_by(UNIT) %>% 
    summarise(sum = round(median(sum), 2)) %>% 
    subset(sum >= 0.66) #use upper tercile
  
  n2 = tas_combined %>% 
    subset(period %in% c("2010-2019")) %>%
    group_by(UNIT) %>% 
    summarise(sum = round(median(sum), 2)) %>% 
    subset(sum >= 0.66) #use upper tercile
  
  
  ##########################
  ### expand IPCC colors ###
  ##########################
  ipcc_temp_expand = colorRampPalette(rev(ipcc_temp))
  ipcc_temp_expand = ipcc_temp_expand(length(unique(tas_combined$UNIT)))
  
  
  #############################
  ### save full list as csv ###
  #############################
  summary = tas_combined %>%
    group_by(UNIT, period) %>%
    summarise_each(funs(mean, sd, se = sd(.)/sqrt(n())), sum)
  
  summary = as.data.frame(summary)
  summary = summary[,c('UNIT', 'period', 'mean', 'sd', 'se')]
  summary$UNIT = as.character(summary$UNIT)
  summary <- summary[order(summary$UNIT),]
  summary[,3:5] = round(summary[,3:5], 2)
  summary$UNIT[duplicated(summary$UNIT)] <- ""
  colnames(summary) = c("Unit", "Period", "Mean", "SD", "SE")
  
  s1 = summary %>% subset(Period == "1980-1989")
  s2 = summary %>% subset(Period == "1990-1999")
  s3 = summary %>% subset(Period == "2000-2009")
  s4 = summary %>% subset(Period == "2010-2019")
  
  summary = cbind(s1, s2, s3, s4)
  summary =  summary[!is.na(summary$Unit),]
  
  write_csv(summary, paste0("outputs/", region, "_", percentile, ".csv"))
  
  
  
  ################
  ### plot joy ###
  ################
  
  all_unit = tas_combined %>%
    mutate(location_id = as.character(geometry)) %>%
    select(UNIT, period, location_id, sum) %>%
    group_by(UNIT, period, location_id) %>%
    summarise(sum = median(sum, na.rm = T))
  
  p = all_unit %>%
    ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
    geom_joy(scale = 5, alpha = 0.8, size = 0.1, bandwidth = 0.03) +
    theme_minimal() +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(-0.05, 0.1),
                       limits = c(0, 1),
                       breaks = c(0.25,  0.75)) +
    scale_fill_cyclical(values = ipcc_temp_expand)+
    facet_wrap(.~period, ncol = 4) +
    ylab(NULL) + xlab(NULL) +
    coord_fixed(ratio = 0.1) +
    theme(axis.text.y = element_text(size = 10),
          panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          legend.position = "none")
  
  pdf(paste0("outputs/joy_", region, "_", percentile, ".pdf"), height = 10, width = 10)
  print(p)
  dev.off()
  
  return(tas_combined)
  
}

lme = rank_joy_lme_eez("lme")

#########################################
### Reorder units by 2010-2019 median ###
#########################################

# merge hadi and cobe from 2010-2019
lme = lme %>% 
  subset(period %in% c("2010-2019")) %>% 
  mutate(location_id = as.character(geometry)) %>%
  group_by(UNIT, location_id) %>%
  summarise(sum = median(sum, na.rm = T))

#you have to repeaet ranking because some units are ranked at same spots
df1 = lme %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 20) %>% top_n(15, m); df1 = df1 %>% top_n(15)
df2 = lme %>% group_by(UNIT) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 20) %>% top_n(-15, m)
sub = rbind(df1, df2)
sub = as.vector(sub$UNIT); sub #see top 15 bottom 15
lme_sub = subset(lme, UNIT %in% sub) #subset
lme_sub = lme_sub %>% group_by(UNIT) %>% mutate(m = median(sum)) %>% arrange(UNIT, m)
lme_sub = lme_sub[,c("UNIT", "sum")]; lme_sub = as.data.frame(lme_sub); lme_sub = lme_sub[1:2]; lme_sub$class = "LME"

pdf(paste0("outputs/Fig2_LME.", percentile, "_", Sys.Date(), ".pdf"), width = 8, height = 6)
p = lme_sub %>% 
  mutate(UNIT = forcats::fct_reorder(UNIT, sum)) %>%
  ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
  geom_joy(scale = 3, alpha = 0.8, size = 0.1) +
  theme_joy(grid = F) +
  scale_y_discrete(expand = c(0.05, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0,0.5, 1)) +
  scale_fill_cyclical(values = ipcc_temp_expand(length(unique(lme_sub$UNIT))))+
  ylab(NULL) + xlab(NULL) +
  coord_fixed(ratio = 0.1) + 
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none") + 
  labs(tag = "(b) Large Marine Ecosystem")
print(p)
dev.off()