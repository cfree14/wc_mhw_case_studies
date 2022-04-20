rm(list = ls())

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

### plot historical baseines ###
data = "COBE"

setwd("~/Dropbox/PAPER Kisei heat extremes/")

load(paste0("data/", data, "_SST.RData"))

# e = extent(-160, -140, 30, 40)
# df = crop(df, e); rm(e)

# set baseline Jan 1870 - Dec 1929, 50 years
Baseline <- df[[1:600]] 
names(Baseline)

Baseline <- Baseline %>% rasterToPoints() %>% data.frame()

Target <- df[[1321:1800]] #Jan 1980 - Dec 2019
Target <- Target %>% rasterToPoints() %>% data.frame()

yy_anom = NULL

y = 2

interval_year = seq(3, 470, by = 12) 

first_month = interval_year[y]
last_month = first_month+11

target = Target[,first_month:last_month]; names(target) # target year
ll_anom = NULL

ll = 25000

monthly_anom = NULL

baseline_12 = NULL
q_12 = NULL

for (m in 1:12) { # every month
  
  # m = 1
  
  interval_month = seq(m+2, dim(Baseline)[2], by = 12)
  
  baseline = Baseline[ll, c(interval_month)]; names(baseline) #pick corresponding month from every baseline year
  baseline = t(baseline)
  baseline = as.data.frame(baseline)
  baseline = baseline[,1]
  
  q = quantile(baseline, prob = 0.98)
  # hist(baseline, breaks = 60, col = matlab.like(60), lty = "blank")
  # abline(v = q)
  
  present = target[ll, m]; present
  sum = ifelse(q < present, 1, 0)
  
  monthly_anom = cbind(monthly_anom, sum)
  
  baseline = as.data.frame(baseline)
  baseline$m = m
  q = as.data.frame(q)
  q$m = m
  baseline_12 = rbind(baseline_12, baseline)
  q_12 = rbind(q_12, q)
  
  
}

ll_anom = rbind(ll_anom, monthly_anom)

### plot(baseline climatology at one location) ###

baseline_12 = as.data.frame(baseline_12)
q_12 = as.data.frame(q_12)
world <- fortify(getMap())

Baseline$mean = rowMeans(Baseline[,3:602])

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

p1 = ggplot(Baseline, aes(x, y, color = mean)) + 
  geom_point(show.legend = F) + 
  scale_color_gradientn(colors = rev(ipcc_temp), "") +
  theme_void() + 
  geom_point(data = Baseline[ll,], aes(x, y), color = "green", size = 5, stroke = 2, shape = 8) +
  geom_map(data = world, map = world, aes(x = long, y = lat, map_id = id),
           color = "gray40", fill = "gray40", size = 0.001) +
  scale_x_continuous(expand = c(-0.005, 0), "") +
  scale_y_continuous(expand = c(-0.005, 0), "")

p2 = ggplot(baseline_12, aes(baseline, fill = factor(m))) + 
  geom_density(aes(y = ..density..),position = "identity", size = 0.01) + 
  facet_wrap(~m, scales = 'free_x', nrow = 1) + 
  geom_vline(data = q_12, aes(xintercept = q), size = 2) + 
  coord_flip() + 
  ggdark::dark_theme_bw(I(20)) +
  xlab("SST (deg C)") + 
  scale_x_reverse() + 
  theme(legend.position = "none", 
        axis.title.x = element_blank(),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        strip.background = element_blank(),
        strip.placement = "inside",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "white"))

png(paste0("~/Desktop/Climatologies_", ll, ".png"), height = 12, width = 8, units = "in", res = 500)
cowplot::plot_grid(p1, p2, ncol = 1)
dev.off()

pdf(paste0("~/Desktop/Climatologies_", ll, ".pdf"), height = 6, width = 6)
# cowplot::plot_grid(p1, p2, ncol = 1)
p1
dev.off()

pdf(paste0("~/Desktop/Climatologies_", ll, ".pdf"), height = 2, width = 6)
# cowplot::plot_grid(p1, p2, ncol = 1)
p2
dev.off()
   