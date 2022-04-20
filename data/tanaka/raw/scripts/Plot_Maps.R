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

percentile = c(0.95, 0.98)[2]

period = c("1980-1989", "1990-1999", "2000-2009", "2010-2019")

data = c("HadI", "COBE", "ER")

world <- fortify(getMap())

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

ipcc_col <- c(rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
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

map = function(mode){
  
  setwd(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/"))
  
  load(paste0("HadI/extremes_1980-1989_", percentile, ".RData")); hadi1 = anom; hadi1$source = "HadISST v1.1"; hadi1$period = "1980-1989"
  load(paste0("HadI/extremes_1990-1999_", percentile, ".RData")); hadi2 = anom; hadi2$source = "HadISST v1.1"; hadi2$period = "1990-1999"
  load(paste0("HadI/extremes_2000-2009_", percentile, ".RData")); hadi3 = anom; hadi3$source = "HadISST v1.1"; hadi3$period = "2000-2009"
  load(paste0("HadI/extremes_2010-2019_", percentile, ".RData")); hadi4 = anom; hadi4$source = "HadISST v1.1"; hadi4$period = "2010-2019"
  load(paste0("COBE/extremes_1980-1989_", percentile, ".RData")); cobe1 = anom; cobe1$source = "COBE v2"; cobe1$period = "1980-1989"
  load(paste0("COBE/extremes_1990-1999_", percentile, ".RData")); cobe2 = anom; cobe2$source = "COBE v2"; cobe2$period = "1990-1999"
  load(paste0("COBE/extremes_2000-2009_", percentile, ".RData")); cobe3 = anom; cobe3$source = "COBE v2"; cobe3$period = "2000-2009"
  load(paste0("COBE/extremes_2010-2019_", percentile, ".RData")); cobe4 = anom; cobe4$source = "COBE v2"; cobe4$period = "2010-2019"
  load(paste0("ER/extremes_1980-1989_", percentile, ".RData")); er1 = anom; er1$source = "ERSST v5"; er1$period = "1980-1989"
  load(paste0("ER/extremes_1990-1999_", percentile, ".RData")); er2 = anom; er2$source = "ERSST v5"; er2$period = "1990-1999"
  load(paste0("ER/extremes_2000-2009_", percentile, ".RData")); er3 = anom; er3$source = "ERSST v5"; er3$period = "2000-2009"
  load(paste0("ER/extremes_2010-2019_", percentile, ".RData")); er4 = anom; er4$source = "ERSST v5"; er4$period = "2010-2019"
  
  if (mode == "annual") {
    
    anom = rbind(hadi1, hadi2, hadi3, hadi4, 
                 cobe1, cobe2, cobe3, cobe4,
                 er1, er2, er3, er4)
    
    anom$source = factor(anom$source, levels = c("HadISST v1.1", "COBE v2",  "ERSST v5"))
    
    anom %>% 
      subset(source %in% c("HadISST v1.1", "COBE v2")) %>% 
      group_by(period) %>% 
      mutate(sum = range01(sum)) %>% 
      summarise(mean = mean(sum))
    
    p = anom %>% 
      mutate(sum = range01(sum)) %>% 
      subset(source %in% c("COBE v2", "HadISST v1.1")) %>%
      group_by(x, y, period) %>% 
      summarise(sum = mean(sum)) %>% 
      # subset(y %in% seq(-70, 70, by = 0.1)) %>%
      ggplot() + 
      geom_point(aes(x, y, color = sum, fill = sum),  size = 0.8, alpha = 0.5, shape = 16) +
      # geom_raster(aes(x, y, color = sum, fill = sum)) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id), color = "gray30", fill = "gray30", size = 0.001) +
      scale_color_gradientn(colors = rev(ipcc_col), "", limits = c(0, 1), breaks = c(0, 0.5, 1)) +
      scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c(0, 1), breaks = c(0, 0.5, 1)) +
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      # coord_sf(xlim = range(anom$x), ylim = range(anom$y)) +
      facet_grid(~period) +
      dark_theme_void() +
      coord_map("ortho", orientation = c(0, 330, 0)) + #normal
      # coord_map("ortho", orientation = c(-90, 45, 0)) + #arctic centered, 0, 0, 0 gives you a normal look
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.ticks = element_blank(), 
            strip.text.x = element_text(size = 10),
            legend.position = "right")
    
    p = anom %>% 
      # sample_frac(0.01) %>%
      mutate(sum = range01(sum)) %>% 
      subset(source %in% c("HadISST v1.1", "COBE v2")) %>%
      # subset(y %in% seq(-60, 60, by = 0.1)) %>%
      # group_by(x, y, period, source) %>% 
      group_by(x, y) %>% 
      summarise(sum = median(sum)) %>% 
      ggplot(aes(x = x, y = y, color = sum)) + 
      geom_point(alpha = 0.5, shape = 16) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id), color = "gray20", fill = "gray20", size = 0.001) + 
      scale_color_gradientn(colors = rev(ipcc_col), "", limits = c(0,1), breaks = c(0,0.5,1)) +
      # coord_proj("+proj=wintri") +
      # facet_grid(source ~ period) +
      # facet_grid(~ period) +
      theme_minimal(I(20)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom", 
            legend.justification = c(1,0))
    
    p = anom %>% 
      # sample_frac(0.01) %>%
      mutate(sum = range01(sum)) %>% 
      subset(source %in% c("HadISST v1.1", "COBE v2")) %>%
      # subset(y %in% seq(-60, 60, by = 0.1)) %>%
      group_by(x, y, period, source) %>% 
      summarise(sum = median(sum)) %>% 
      ggplot() +
      geom_raster(aes(x = x, y = y, fill = sum), interpolate = T) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.001) +
      scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c(0,1), breaks = c(0,0.5,1)) +
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      coord_fixed() +
      facet_grid(source ~ period) +
      theme_minimal(I(20)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom",
            legend.justification = c(1,0))
    
    pdf(paste0("/Users/", Sys.info()[7], "/Desktop/s2_", Sys.Date(), "_", percentile, ".pdf"), height = 4, width = 8)
    print(p)
    dev.off()
    
  }
  
  if (mode == "seasonal_difference") {
    
    season_1 = anom[,c("x", "y", "jan", "feb", "mar", "source", "period")]; season_1$season = "Jan_Feb_Mar"
    season_2 = anom[,c("x", "y", "jul", "aug", "sep", "source", "period")]; season_2$season = "Jul_Aug_Sep"
    
    season_1$sum = rowSums(season_1[3:5])
    season_2$sum = rowSums(season_2[3:5])
    
    season_1 = season_1 %>% group_by(x, y) %>% summarise(sum = mean(sum))
    season_2 = season_2 %>% group_by(x, y) %>% summarise(sum = mean(sum))
    
    seasonal_differnece = range01(season_2$sum) - range01(season_1$sum) #summer - winter
    seasonal_differnece = cbind(season_1[,1:2], seasonal_differnece)
    colnames(seasonal_differnece)[3] = "diff"
    
    p = seasonal_differnece %>% 
      ggplot() +
      geom_raster(aes(x = x, y = y, 
                      # fill = abs(diff)
                      fill = diff), interpolate = T) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.001) +
      scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c(-1, 1), breaks = c(-1, 1)) +
      # scale_fill_viridis_c("", limits = c(-1, 1), breaks = c(-1, 1)) + 
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      coord_fixed() +
      theme_minimal(I(20)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom",
            legend.justification = c(1,0))
    
    pdf(paste0("/Users/", Sys.info()[7], "/Desktop/s3.pdf"), height = 10, width = 10)
    print(p)
    dev.off()
    
  }
  
  if (mode == "combine") {
    
    #all periods
    annual = rbind(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)
    annual$sum = range01(annual$sum)
    annual$season = "Annual"
    annual = annual[, c("x", "y", "sum", "source", "period", "season")]
    
    #seasonals
    anom = rbind(hadi1, hadi2, hadi3, hadi4, cobe1, cobe2, cobe3, cobe4)
    season_1 = anom[,c("x", "y", "jan", "feb", "mar", "source", "period")]; season_1$season = "Jan-Mar"
    season_2 = anom[,c("x", "y", "jul", "aug", "sep", "source", "period")]; season_2$season = "Jul-Sep"
    season_1$sum = rowSums(season_1[3:5])
    season_2$sum = rowSums(season_2[3:5])
    season_1 = season_1[,c(1,2, 6:9)]
    season_2 = season_2[,c(1,2, 6:9)]
    season = rbind(season_1, season_2)
    season$sum = range01(season$sum)
    season = season[, c("x", "y", "sum", "source", "period", "season")]
    
    anom = rbind(annual, season) %>% group_by(x, y, period, season) %>% summarise(sum = median(sum))
    
    p = anom %>% 
      sample_frac(0.01) %>%
      ggplot(size = 5, alpha = 0.8) + 
      geom_point(aes(x = x, y = y, color = sum), size = 1, alpha = 0.5, shape = 16) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.1) + 
      # scale_color_gradientn(colors = matlab.like(100), "", limits = c(0,1)) +
      scale_color_gradientn(colors = rev(ipcc_col), "", limits = c(0,1), breaks = c(0, 0.5, 1)) +
      # coord_proj("+proj=wintri") +
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      coord_fixed() +
      facet_grid(season ~ period) +
      theme_minimal(I(20)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(), 
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom", 
            legend.justification = c(1,0))
    
    p = ggplot(anom) +
      geom_raster(aes(x = x, y = y, fill = sum), interpolate = T) +
      geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
               color = "gray20", fill = "gray20", size = 0.001) +
      scale_fill_gradientn(colors = rev(ipcc_col), "", limits = c(0,1), breaks = c(0,0.5,1)) +
      scale_x_continuous(expand = c(-0.005, 0), "") +
      scale_y_continuous(expand = c(-0.005, 0), "") +
      coord_fixed() +
      facet_grid(season ~ period) +
      theme_minimal(I(20)) +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.x = element_blank(),
            axis.ticks.y = element_blank(),
            legend.position = "bottom",
            legend.justification = c(1,0))
    
    pdf(paste0("/Users/", Sys.info()[7], "/Desktop/Fig1_", Sys.Date(), "_", percentile, ".pdf"), height = 10, width = 10)
    # png(paste0("/Users/", Sys.info()[7], "/Desktop/Fig1_", Sys.Date(), "_", percentile, ".png"), height = 10, width = 10, units = "in", res = 100)
    print(p)
    dev.off()
    
  }
  
  
}

map("annual")
map("seasonal_difference")
map("combine")
