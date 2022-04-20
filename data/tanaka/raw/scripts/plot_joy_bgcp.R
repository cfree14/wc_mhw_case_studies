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

rank_joy_bgcp = function(){
  
  tas_combined = NULL
  
  for (i in 1:length(period)){
    
    # i = 1
    
    load("data/bgcp_raster_0.25.RData")
    load(paste0("outputs/HadI/extremes_", period[[i]], "_", percentile, ".RData"))
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
    bgcp_names <- read_csv(paste0("data/NAME_BGCP_2019_REYGONDEAU.csv"))
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
    
    load(paste0("data/bgcp_raster_0.25.RData"))
    load(paste0("outputs/COBE/extremes_", period[[i]], "_", percentile, ".RData"))
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
    bgcp_names <- read_csv(paste0("data/NAME_BGCP_2019_REYGONDEAU.csv"))
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
    
    load(paste0("data/bgcp_raster_0.25.RData"))
    load(paste0("outputs/ER/extremes_", period[[i]], "_", percentile, ".RData"))
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
    bgcp_names <- read_csv(paste0("data/NAME_BGCP_2019_REYGONDEAU.csv"))
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
  
  # big_bgcps = tas_combined %>% group_by(bgcp) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 5000)
  # big_bgcps = as.data.frame(big_bgcps)
  # big_bgcps = big_bgcps[, 1, drop = FALSE]
  # tas_combined_sub = subset(tas_combined, bgcp %in% dplyr::pull(big_bgcps))
  # 
  # p = tas_combined_sub %>% 
  #   subset(source %in% c("HadISST v1.1", "COBE v2")) %>%
  #   group_by(x, y, bgcp, period) %>% 
  #   summarise(sum = mean(sum)) %>% 
  #   ggplot() +
  #   geom_density(aes(x = sum, fill = period), alpha = 0.8, size = 0.01) +
  #   scale_x_continuous(
  #     limits = c(0, 1),
  #     expand = c(0.05, 0.01),
  #     breaks = c(0, 0.5, 1)) +
  #   scale_fill_manual(values = rev(ipcc_temp_4_cols), "") +
  #   facet_wrap( ~ bgcp, scales = "fixed") +
  #   coord_fixed(ratio = 0.05) + 
  #   ylab(NULL) + xlab(NULL) +
  #   theme(
  #     axis.text.y = element_blank(),
  #     axis.ticks = element_blank(),
  #     # axis.text.x = element_text(size = 10),
  #     legend.position = "top")
  # 
  # pdf(paste0("~/Desktop/joy_bgcp_selected_", percentile, ".pdf"), height = 5, width = 6)
  # print(p)
  # dev.off()
  
  tas_combined = tas_combined %>% mutate(bgcp = gsub("\xca", "", bgcp)) 

  tas_combined$bgcp = gsub("Southeast ", "SE ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("Northeast ", "NE ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("Northwest ", "NW ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("Southeast ", "SE ", tas_combined$bgcp, fixed = T)
  
  tas_combined$bgcp = gsub("Western ", "W ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("Eastern ", "E ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("Nothern ", "N ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("Southern ", "S ", tas_combined$bgcp, fixed = T)
  
  tas_combined$bgcp = gsub("East ", "E ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("West ", "W ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("North ", "N ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("South ", "S ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("Central ", "Cent ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("Central", "Cent", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("Pacific ", "Pac ", tas_combined$bgcp, fixed = T)
  
  tas_combined$bgcp = gsub("Californian ", "Calif ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("current", "Curr", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("coast", "Coast", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("equatorial", "Equ", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("subtropical", "Subtrop", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("tropical", "Trop", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("gyre", "Gyre", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("monsoon", "Monsoon", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("downwelling", "Downwelling", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("upwelling", "Upwelling", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("polar", "Polar", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("subarctic", "Subarctic", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("counter", "Counter", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("shelves", "Shelves", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("divergence", "Divergence", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("convergence", "Convergence", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("water ring", "Water Ring", tas_combined$bgcp, fixed = T)
  tas_combined$bgcp = gsub("warm pool", "Warm Pool", tas_combined$bgcp, fixed = T)
  
  ### just keep HadISST and COBESST. discard unecessary columns ###
  tas_combined = subset(tas_combined, source %in% c("HadISST v1.1", "COBE v2")) # remove ERSST
  tas_combined = tas_combined %>% as.data.frame() %>% select(bgcp, period, source, sum, x, y)
  tas_combined = tas_combined[!is.na(tas_combined$bgcp),]
  
  # count changes in number of unit between 1980-1989 and 2010-2019
  n1 = tas_combined %>% 
    subset(period %in% c("1980-1989")) %>%
    group_by(bgcp) %>% 
    summarise(sum = round(median(sum), 2)) %>% 
    subset(sum >= 0.66) #use upper tercile
  
  n2 = tas_combined %>% 
    subset(period %in% c("2010-2019")) %>%
    group_by(bgcp) %>% 
    summarise(sum = round(median(sum), 2)) %>% 
    subset(sum >= 0.66) #use upper tercile
  
  ##########################
  ### expand IPCC colors ###
  ##########################
  ipcc_temp_expand = colorRampPalette(rev(ipcc_temp))
  ipcc_temp_expand = ipcc_temp_expand(length(unique(tas_combined$bgcp)))
  
  summary = tas_combined %>%
    group_by(bgcp, period) %>%
    summarise_each(funs(mean, sd, se = sd(.)/sqrt(n())), sum)

  summary = as.data.frame(summary)
  summary = summary[,c('bgcp', 'period', 'mean', 'sd', 'se')]
  summary$bgcp = as.character(summary$bgcp)
  summary <- summary[order(summary$bgcp),]
  summary$bgcp[duplicated(summary$bgcp)] <- ""
  summary[,3:5] = round(summary[,3:5], 2)
  colnames(summary) = c("Unit", "Period", "Mean", "SD", "SE")

  s1 = summary %>% subset(Period == "1980-1989")
  s2 = summary %>% subset(Period == "1990-1999")
  s3 = summary %>% subset(Period == "2000-2009")
  s4 = summary %>% subset(Period == "2010-2019")
  
  summary = cbind(s1, s2, s3, s4)
  summary =  summary[!is.na(summary$Unit),]

  write_csv(summary, paste0("outputs/bgcp_", percentile, ".csv"))
  
  all_unit = tas_combined %>%
    mutate(location_id = paste0(x, "_", y)) %>%
    select(bgcp, period, location_id, sum) %>%
    group_by(bgcp, period, location_id) %>%
    summarise(sum = median(sum, na.rm = T)) %>% 
    group_by(bgcp, period) %>% 
    mutate(median = median(sum))
  
  ipcc_temp_expand = colorRampPalette(rev(ipcc_temp))
  ipcc_temp_expand = ipcc_temp_expand(length(unique(all_unit$median)))
  
  p = all_unit %>% 
    ggplot(aes(x = sum, y = bgcp, fill = as.factor(median))) +
    geom_joy(scale = 5, alpha = 0.8, size = 0.1, bandwidth = 0.03, show.legend = F) +
    theme_minimal() +
    scale_y_discrete(expand = c(0, 0)) +
    scale_x_continuous(expand = c(-0.05, 0.1),
                       limits = c(0, 1),
                       breaks = c(0.25,  0.75)) +
    scale_fill_cyclical(values = ipcc_temp_expand) +
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
  
  png(paste0("outputs/joy_bgcp_", percentile, ".png"), height = 10, width = 10, units = "in", res = 500)
  print(p)
  dev.off()
  
  return(tas_combined)
  
}

bgcp = rank_joy_bgcp()

#########################################
### Reorder units by 2010-2019 median ###
#########################################

# merge hadi and cobe from 2010-2019
bgcp = bgcp %>% 
  subset(period %in% c("2010-2019")) %>% 
  mutate(location_id = paste0(x, "_", y)) %>%
  group_by(bgcp, location_id) %>%
  summarise(sum = median(sum, na.rm = T))

df1 = bgcp %>% group_by(bgcp) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 20) %>% top_n(15, m)
df2 = bgcp %>% group_by(bgcp) %>% summarise(m = median(sum), freq = n()) %>% filter(freq > 20) %>% top_n(-15, m)
sub = rbind(df1, df2)
sub = as.vector(sub$bgcp); sub #see top 15 bottom 15
bgcp_sub = subset(bgcp, bgcp %in% sub) #subset
bgcp_sub = bgcp_sub %>% group_by(bgcp) %>% mutate(m = median(sum)) %>% arrange(bgcp, m)
bgcp_sub = bgcp_sub[,c("bgcp", "sum")]; bgcp_sub = as.data.frame(bgcp_sub); bgcp_sub = bgcp_sub[1:2]; colnames(bgcp_sub)[1] = "UNIT"; bgcp_sub$class = "BGCP"

pdf(paste0("outputs/Fig2_BGCP.", percentile, "_", Sys.Date(), ".pdf"), width = 8, height = 6)
# png(paste0("outputs/Fig2_BGCP.", percentile, "_", Sys.Date(), ".png"), width = 12, height = 8, res = 500, units = "in")

p = bgcp_sub %>%  
  mutate(UNIT = gsub("\xca", "", UNIT)) %>% 
  mutate(UNIT = forcats::fct_reorder(UNIT, sum)) %>%
  ggplot(aes(x = sum, y = UNIT, fill = UNIT)) +
  geom_joy(scale = 3, alpha = 0.8, size = 0.1) +
  theme_joy(grid = F) +
  scale_y_discrete(expand = c(0.05, 0)) + # will generally have to set the `expand` option
  scale_x_continuous(limits = c(0, 1), expand = c(0, 0), breaks = c(0,0.5, 1)) +
  scale_fill_cyclical(values = ipcc_temp_expand(length(unique(bgcp_sub$UNIT))))+
  ylab(NULL) + xlab(NULL) +
  coord_fixed(ratio = 0.1) + 
  theme(axis.text.y = element_text(size = 10),
        legend.position = "none") + 
  labs(tag = "(a) Biogeographic Province", caption = "proportion of extremes")
print(p)
dev.off()
