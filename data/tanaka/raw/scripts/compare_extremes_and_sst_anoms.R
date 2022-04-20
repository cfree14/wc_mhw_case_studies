############################
#### Fig 4 in main text ####
############################

rm(list = ls())

library(dplyr)
library(ggthemes)
library(precrec)
library(ggplot2)
library(patchwork)

world <- fortify(rworldmap::getMap())
range01 <- function(x){(x-min(x))/(max(x)-min(x))}

setwd("~/extreme_normalizations/outputs/")
  
# conventional SST anomalies for 2019
load("COBE/sst_anomalies_2019.Rdata"); cobe_ipcc = anom; cobe_ipcc$data = "COBE"; cobe_ipcc$sum = rowMeans(cobe_ipcc[3:14])
load("HadI/sst_anomalies_2019.Rdata"); hadi_ipcc = anom; hadi_ipcc$data = "HadI"; hadi_ipcc$sum = rowMeans(hadi_ipcc[3:14])
anom_ipcc = rbind(cobe_ipcc, hadi_ipcc) %>% dplyr::select(x, y, sum) %>% group_by(x, y) %>% summarise(anom = mean(sum))

#99th percentile and area fractions
q = quantile(anom_ipcc$anom, 0.99); q
d = anom_ipcc %>% subset(anom >= q)
(dim(d)[1]/dim(anom_ipcc)[1])*100

# Normalized Extreme Index for 2019
load("HadI/extremes_2019_0.98.RData"); hadi_extreme = anom; hadi_extreme$data = "HadI"
load("COBE/extremes_2019_0.98.RData"); cobe_extreme = anom; cobe_extreme$data = "COBE"
anom_extreme = rbind(cobe_extreme, hadi_extreme) %>% dplyr::select(x, y, sum) %>% group_by(x, y) %>% summarise(anom = mean(sum))
anom_extreme$anom = range01(anom_extreme$anom)

#99th percentile and area fractions
q = quantile(anom_extreme$anom, 0.99); q
d = anom_extreme %>% subset(anom >= q)
(dim(d)[1]/dim(anom_extreme)[1])*100

#IPCC temp color theme
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

p1 = anom_ipcc %>% ggplot(aes(x, y, fill = anom)) +  
  geom_tile() + 
  geom_tile(data = subset(anom_ipcc, anom >= 1.375803), 
            aes(x, y), fill = rgb(102, 255, 102, 
                                  maxColorValue = 255, 
                                  alpha = 255), 
            alpha = 0.9) + 
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "gray20", fill = "gray20", size = 0.001) +
  scale_fill_gradientn(colors = rev(ipcc_col), "") +
  scale_x_continuous(expand = c(-0.005, 0), "") +
  scale_y_continuous(expand = c(-0.005, 0), "") +
  coord_fixed() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        legend.justification = c(1, 0)) + 
  labs(tag = "a")

p2 = anom_extreme %>% ggplot(aes(x, y, fill = anom)) +  
  geom_tile() + 
  geom_tile(data = subset(anom_extreme, anom >= 1),
            aes(x, y), fill = rgb(102, 255, 102, 
                                  maxColorValue = 255, 
                                  alpha = 255), 
            alpha = 0.9) + 
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
          color = "gray20", fill = "gray20", size = 0.001) +
  scale_fill_gradientn(colors = rev(ipcc_col), "") +
  scale_x_continuous(expand = c(-0.005, 0), "") +
  scale_y_continuous(expand = c(-0.005, 0), "") +
  coord_fixed() + 
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "right",
        legend.justification = c(1, 0)) + 
  labs(tag = "b")

p1/p2

pdf(paste0("~/Desktop/Fig4_", Sys.Date(), ".pdf"), height = 5, width = 5)

p1/p2

dev.off()
