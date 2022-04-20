library(colorRamps)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(patchwork)

rm(list = ls())

#########################
### choose percentile ###
#########################
p = c(0.95, 0.98)[2]

########################################################
### load area fraction time series results 1900-2019 ###
########################################################

setwd(paste0("/Users/", Sys.info()[7], "/extreme_normalizations/outputs/"))

load(paste0("HadI/timeseries_global_", p, ".RData")); hadi_global = yy_anom; hadi_global$region = "Global"
load(paste0("HadI/timeseries_global_no_polar_", p, ".RData")); hadi_sub_global = yy_anom; hadi_sub_global$region = "Sub_Global"
load(paste0("HadI/timeseries_arctic_", p, ".RData")); hadi_arctic = yy_anom; hadi_arctic$region = "Arctic"
load(paste0("HadI/timeseries_indian_", p, ".RData")); hadi_indian = yy_anom; hadi_indian$region = "Indian"
load(paste0("HadI/timeseries_north_atlantic_", p, ".RData")); hadi_north_atlantic = yy_anom; hadi_north_atlantic$region = "N.Atlantic"
load(paste0("HadI/timeseries_north_pacific_", p, ".RData")); hadi_north_pacific = yy_anom; hadi_north_pacific$region = "N.Pacific"
load(paste0("HadI/timeseries_south_atlantic_", p, ".RData")); hadi_south_atlantic = yy_anom; hadi_south_atlantic$region = "S.Atlantic"
load(paste0("HadI/timeseries_south_pacific_", p, ".RData")); hadi_south_pacific = yy_anom; hadi_south_pacific$region = "S.Pacific"
load(paste0("HadI/timeseries_southern_", p, ".RData")); hadi_southern_ocean = yy_anom; hadi_southern_ocean$region = "Southern"
load(paste0("HadI/timeseries_gom_0.95.RData")); hadi_gom = yy_anom; hadi_gom$region = "Gulf of Maine"

load(paste0("COBE/timeseries_global_", p, ".RData")); cobe_global = yy_anom; cobe_global$region = "Global"
load(paste0("COBE/timeseries_global_no_polar_", p, ".RData")); cobe_sub_global = yy_anom; cobe_sub_global$region = "Sub_Global"
load(paste0("COBE/timeseries_arctic_", p, ".RData")); cobe_arctic = yy_anom; cobe_arctic$region = "Arctic"
load(paste0("COBE/timeseries_indian_", p, ".RData")); cobe_indian = yy_anom; cobe_indian$region = "Indian"
load(paste0("COBE/timeseries_north_atlantic_", p, ".RData")); cobe_north_atlantic = yy_anom; cobe_north_atlantic$region = "N.Atlantic"
load(paste0("COBE/timeseries_north_pacific_", p, ".RData")); cobe_north_pacific = yy_anom; cobe_north_pacific$region = "N.Pacific"
load(paste0("COBE/timeseries_south_atlantic_", p, ".RData")); cobe_south_atlantic = yy_anom; cobe_south_atlantic$region = "S.Atlantic"
load(paste0("COBE/timeseries_south_pacific_", p, ".RData")); cobe_south_pacific = yy_anom; cobe_south_pacific$region = "S.Pacific"
load(paste0("COBE/timeseries_southern_", p, ".RData")); cobe_southern_ocean = yy_anom; cobe_southern_ocean$region = "Southern"
load(paste0("COBE/timeseries_gom_0.95.RData")); cobe_gom = yy_anom; cobe_gom$region = "Gulf of Maine"

hadi = rbind(hadi_global, 
             hadi_sub_global,
             hadi_arctic, 
             hadi_indian, 
             hadi_north_atlantic, 
             hadi_north_pacific,
             hadi_south_atlantic, 
             hadi_south_pacific, 
             hadi_southern_ocean,
             hadi_gom)

cobe = rbind(cobe_global, 
             cobe_sub_global,
             cobe_arctic, 
             cobe_indian, 
             cobe_north_atlantic, 
             cobe_north_pacific,
             cobe_south_atlantic, 
             cobe_south_pacific, 
             cobe_southern_ocean,
             cobe_gom)

rm(hadi_global, 
   hadi_sub_global,
   hadi_arctic, 
   hadi_indian, 
   hadi_north_atlantic, 
   hadi_north_pacific,
   hadi_south_atlantic, 
   hadi_south_pacific, 
   hadi_southern_ocean, 
   cobe_global, 
   cobe_sub_global,
   cobe_arctic, 
   cobe_indian, 
   cobe_north_atlantic, 
   cobe_north_pacific,
   cobe_south_atlantic, 
   cobe_south_pacific, 
   cobe_southern_ocean,
   yy_anom,
   hadi_gom, 
   cobe_gom)

hadi$data = "HadISST"
cobe$data = "COBESST"

df = rbind(hadi, cobe); rm(cobe, hadi)

df = tidyr::separate(df, time, c("Year", "Month"), sep = "-")
df$Month <- sprintf("%02d", as.numeric(df$Month))
df$Day = 01
df$Day <- sprintf("%02d", as.numeric(df$Day))
df$Time = paste(df$Year, df$Month, df$Day, sep = "-")
df$Time = as.Date(df$Time)

exceeded_times = df %>% 
  group_by(Year, region) %>% 
  summarise(year_sum = mean(year_sum)) %>% 
  mutate(y = ifelse(year_sum > 0.5, 1, 0)) %>% 
  dplyr::select(Year, region, y) %>% 
  subset(y > 0) 

exceeded_times$region = factor(exceeded_times$region, levels = c(
  "Southern",
  "S.Pacific",
  "N.Atlantic",
  "Arctic",
  "N.Pacific",
  "Global",
  "Sub_Global",
  "Indian",
  "S.Atlantic"))

cols <- c(rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
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

exceeded_times  %>% 
  ggplot(aes(Year, y = region, fill = region)) +
  scale_fill_manual(values = cols, "") +
  geom_tile(show.legend = F) +
  labs(x = "", y = "") +
  theme_minimal(I(15)) + 
  geom_hline(yintercept = seq(0.5, 8), color = 'white', size = 2) + 
  theme(axis.text.x = element_text(angle = 270, vjust = 0.5, hjust=1))

all_year = data.frame(Year = seq(1950, 2019, by = 1))

region_n = c(
  "Southern",
  "S.Pacific",
  "Arctic",
  "N.Pacific",
  "Global",
  "Sub_Global",
  "N.Atlantic",
  "Indian",
  "S.Atlantic")

for (i in 1:length(region_n)) {
  
  # i = 2
  ponr = subset(exceeded_times, region == region_n[[i]])
  ponr = merge(ponr, all_year, all = T)
  print(ponr)
  
}

rm(all_year, region_n, cols, i, ponr)

############
### plot ###
############

timeseries = df %>% group_by(Year, region) %>% summarise(year_sum = mean(year_sum))
timeseries$Year = as.numeric(timeseries$Year)

ElNino = subset(df, Year %in% c(1905, 1906, 
                                1911, 1912, 1914, 1915, 
                                1940, 1941, 1942, 
                                1965, 1966, 
                                1972, 1973,
                                1982, 1983, 1987, 1988, 
                                1991, 1992, 1997, 1998, 
                                2015, 2016))

# calculate % changes in area fraction above 50% 1900-1959 vs. 1960-2019
t = timeseries %>% mutate(period = ifelse(Year %in% c(1900:1959), "1st", "2nd")) %>% 
  group_by(region, period) %>%
  summarise(sum = mean(year_sum)) %>% 
  mutate(percent = (sum/lag(sum)-1)*100)
t

readr::write_csv(t, "~/Dropbox/PAPER Kisei heat extremes/figures/supplemental/TableS3.csv")

timeseries$linesize = ifelse(timeseries$region == "Global", 2, 1)

col1 <- c("gray40", 
          rgb(67, 147, 195, maxColorValue = 255, alpha = 255),
          rgb(33, 102, 172, maxColorValue = 255, alpha = 255),
          rgb(5, 48, 97, maxColorValue = 255, alpha = 255))

col2 <- c("gray40", 
          rgb(103, 0, 31, maxColorValue = 255, alpha = 255),
          rgb(178, 24, 43, maxColorValue = 255, alpha = 255),
          rgb(214, 96, 77, maxColorValue = 255, alpha = 255),
          rgb(244, 165, 130, maxColorValue = 255, alpha = 255))

if (p == 0.95) {
  
  timeseries$region[timeseries$region == "Global"] <- "Global (2009)"
  timeseries$region[timeseries$region == "Arctic"] <- "Arctic (2016)"
  timeseries$region[timeseries$region == "N.Atlantic"] <- "N.Atlantic (2003)"
  timeseries$region[timeseries$region == "N.Pacific"] <- "N.Pacific (2014)"
  timeseries$region[timeseries$region == "Southern"] <- "Southern (n/a)"
  timeseries$region[timeseries$region == "S.Atlantic"] <- "S.Atlantic (1993)"
  timeseries$region[timeseries$region == "S.Pacific"] <- "S.Pacific (n/a)"
  timeseries$region[timeseries$region == "Indian"] <- "Indian (1995)"
  
  nh = timeseries %>% subset(region %in% c("Global (2009)", "Arctic (2016)","N.Atlantic (2003)","N.Pacific (2014)"))
  sh = timeseries %>% subset(region %in% c("Global (2009)", "Southern (n/a)","S.Atlantic (1993)","S.Pacific (n/a)", "Indian (1995)"))
  
  p1 = nh %>% 
    mutate(region = factor(region, levels = c("Global (2009)", "Arctic (2016)","N.Atlantic (2003)","N.Pacific (2014)"))) %>% 
    ggplot(aes(Year, year_sum, group = region, colour = region, size = linesize)) +
    geom_smooth(span = 0.05, se = F, alpha = 0.8)  +
    scale_size(range = c(1, 3), guide = "none") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
    scale_colour_manual(values = col1, "") +
    labs(x = "", y = "") +
    scale_x_continuous(breaks = seq(1900, 2020, 20), limits = c(1900, 2020)) + 
    scale_y_continuous(breaks = c(seq(0, 1, by = 0.2)), limits = c(0, max(timeseries$year_sum))) + 
    ggthemes::theme_few(I(20)) +
    theme(legend.position = c(0.15, 0.8),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.length = unit(-0.25, "cm")) +  
    labs(tag = "a")
  
  p2 = sh %>% 
    mutate(region = factor(region, levels = c("Global (2009)", "Southern (n/a)","S.Atlantic (1993)","S.Pacific (n/a)", "Indian (1995)"))) %>% 
    ggplot(aes(Year, year_sum, group = region, colour = region, size = linesize)) +
    geom_smooth(span = 0.05, se = F, alpha = 0.8)  +
    scale_size(range = c(1, 3), guide = "none") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
    scale_colour_manual(values = col2, "") +
    labs(x = "", y = "proportion of area extent") +
    scale_x_continuous(breaks = seq(1900, 2020, 20), limits = c(1900, 2020)) + 
    scale_y_continuous(breaks = c(seq(0, 1, by = 0.2)), limits = c(0, max(timeseries$year_sum))) + 
    ggthemes::theme_few(I(20)) +
    theme(legend.position = c(0.15, 0.8),
          legend.title = element_blank(),
          axis.ticks.length = unit(-0.25, "cm"),
          axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
          axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))) +  
    labs(tag = "b")
  
  p1/p2
  
  pdf(paste0('~/Dropbox (MBA)/PAPER Kisei heat extremes/figures/Fig3_', p, '_', Sys.Date(), '.pdf'), height = 11, width = 10)
  p1/p2
  dev.off()
  
}

if (p == 0.98) {
  
  timeseries$region[timeseries$region == "Global"] <- "Global (2014)"
  timeseries$region[timeseries$region == "Arctic"] <- "Arctic (2016)"
  timeseries$region[timeseries$region == "N.Atlantic"] <- "N.Atlantic (n/a)"
  timeseries$region[timeseries$region == "N.Pacific"] <- "N.Pacific (2014)"
  timeseries$region[timeseries$region == "Southern"] <- "Southern (n/a)"
  timeseries$region[timeseries$region == "S.Atlantic"] <- "S.Atlantic (1998)"
  timeseries$region[timeseries$region == "S.Pacific"] <- "S.Pacific (n/a)"
  timeseries$region[timeseries$region == "Indian"] <- "Indian (2007)"
  
  nh = timeseries %>% subset(region %in% c("Global (2014)", "Arctic (2016)","N.Atlantic (n/a)","N.Pacific (2014)"))
  sh = timeseries %>% subset(region %in% c("Global (2014)", "Southern (n/a)","S.Atlantic (1998)","S.Pacific (n/a)", "Indian (2007)"))
  
  p1 = nh %>% 
    mutate(region = factor(region, levels = c("Global (2014)", "Arctic (2016)","N.Atlantic (n/a)","N.Pacific (2014)"))) %>%
    # subset(region %in% c("N.Atlantic (n/a)")) %>%
    ggplot(aes(Year, year_sum, group = region, colour = region, size = linesize)) +
    # geom_smooth(span = 0.05, se = F, alpha = 0.85)  +
    geom_line(alpha = 0.85) + 
    scale_size(range = c(1, 3), guide = "none") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
    scale_colour_manual(values = col1, "") +
    labs(x = "", y = "") +
    scale_x_continuous(breaks = seq(1900, 2020, 10), limits = c(1900, 2020)) + 
    scale_y_continuous(breaks = c(seq(0, 1, by = 0.2)), limits = c(0, max(timeseries$year_sum))) + 
    ggthemes::theme_few(I(20)) +
    theme(legend.position = c(0.15, 0.8),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          legend.title = element_blank(),
          axis.ticks.length = unit(-0.25, "cm")) +  
    labs(tag = "a")
  
  p2 = sh %>% 
    mutate(region = factor(region, levels = c("Global (2014)", "Southern (n/a)","S.Atlantic (1998)","S.Pacific (n/a)", "Indian (2007)"))) %>% 
    ggplot(aes(Year, year_sum, group = region, colour = region, size = linesize)) +
    # geom_smooth(span = 0.05, se = F, alpha = 0.85)  +
    geom_line(alpha = 0.85) + 
    scale_size(range = c(1, 3), guide = "none") +
    geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
    scale_colour_manual(values = col2, "") +
    labs(x = "", y = "proportion of area extent") +
    scale_x_continuous(breaks = seq(1900, 2020, 10), limits = c(1900, 2020)) + 
    scale_y_continuous(breaks = c(seq(0, 1, by = 0.2)), limits = c(0, max(timeseries$year_sum))) + 
    ggthemes::theme_few(I(20)) +
    theme(legend.position = c(0.15, 0.8),
          legend.title = element_blank(),
          axis.ticks.length = unit(-0.25, "cm"),
          axis.text.x = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")),
          axis.text.y = element_text(margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"))) +  
    labs(tag = "b")
  
  p1/p2
  
  pdf(paste0('~/Dropbox/PAPER Kisei heat extremes/figures/Fig3_', p, '_', Sys.Date(), '.pdf'), height = 11, width = 10)
  p1/p2
  dev.off()
  
}

##################
### facet plot ###
##################
timeseries %>% 
  ggplot(aes(Year, year_sum, colour = year_sum)) +
  scale_size(range = c(1, 3), guide = "none") +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") + 
  labs(x = "", y = "proportion of area extent") +
  scale_color_viridis_c() + 
  scale_x_continuous(breaks = seq(1900, 2020, 40), limits = c(1900, 2020)) + 
  scale_y_continuous(breaks = c(seq(0, 1, by = 0.2))) + 
  cowplot::theme_cowplot(I(20)) +
  facet_wrap(~region, scales = "free_y", ncol = 3) + 
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))


df1 = df %>% group_by(Year, data, region) %>% summarise(year_sum = mean(year_sum))
df1$Year = as.numeric(df1$Year)

df1$region = factor(df1$region, levels = c("Global", 
                                           "Sub_Global",  
                                           "Arctic",
                                           "N.Atlantic",
                                           "S.Atlantic",
                                           "N.Pacific",
                                           "S.Pacific",
                                           "Indian",
                                           "Southern",
                                           "Gulf of Maine"))

pdf("~/Desktop/s6.pdf", height = 8, width = 8)
df1 %>% 
  # subset(region %in% c("Global", "Gulf of Maine")) %>%
  group_by(Year, region) %>% 
  summarise(year_sum = mean(year_sum)) %>% 
  ggplot(aes(x = Year, y = year_sum, color = region)) +
  geom_point(alpha = 0.8, size = 2) +
  geom_line(alpha = 0.8) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "gray") +
  labs(x = "", y = "Area Fraction") +
  # cowplot::theme_cowplot(I(20)) +
  ggdark::dark_theme_classic(I(24)) +
  # facet_wrap( ~ region, scales = "free") + 
  # scale_color_brewer(palette = "Set1", "") + 
  scale_color_discrete("") + 
  scale_x_continuous(breaks = seq(1900, 2020, 60), limits = c(1900, 2020)) + 
  theme(legend.position = "top",
        # panel.spacing.x = unit(2, "lines"),
        legend.justification = c(0,1))
dev.off()

