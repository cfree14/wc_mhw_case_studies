
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/environmental/indices/processed/"
mhwdir <- "data/heatwaves/processed"
plotdir <- "figures"

# Read data
data_reg_orig <- readRDS(file.path(datadir, "1982_2020_daily_mhw_stats_regional.Rds"))
data_wc_orig <- readRDS(file.path(datadir, "1982_2020_daily_mhw_stats.Rds"))
events_orig <- readRDS(file.path(datadir, "1983_2021_mhw_events.Rds"))

# Meta-data
# https://oceanview.pfeg.noaa.gov/erddap/info/cciea_OC_MHW_regions/index.html



# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country="Canada", returnclass = "sf")


# MHW raster data
################################################################################

# Read MHW rasters
mhw_ras_orig <- readRDS(file=file.path(mhwdir, "COBE_1891_2022_NE_mhw_coverage.Rds"))

# Dates use
mhw_dates <- seq(ymd("2014-09-01"), ymd("2016-09-01"), by="4 months")[1:6]

# Build data
mhw_ras <- mhw_ras_orig %>%
  # Reduce to dates of interest
  filter(date %in% mhw_dates) %>%
  # Reduce to MHW
  filter(mhw_yn==T) %>%
  # Recode date
  mutate(date_chr=as.character(date),
         date_label=recode_factor(date_chr,
                                  "2014-09-01"="Sep 2014",
                                  "2015-01-01"="Jan 2015",
                                  "2015-05-01"="May 2015",
                                  "2015-09-01"="Sep 2015",
                                  "2016-01-01"="Jan 2016",
                                  "2016-05-01"="May 2016"))


# Format data
################################################################################

# Format regional data
data_reg <- data_reg_orig %>%
  # Format region
  mutate(region=recode_factor(region,
                              "Southern California"="sCA",
                              "Central California"="cCA",
                              "Northern California"="nCA",
                              "Oregon"="OR",
                              "Washington"="WA"))

# Format event data
events <- events_orig %>%
  # Shorten code
  mutate(mhw_name_short=gsub("NEP19|NEP20", "", mhw_name))

# Format SST data
sst <- sst_orig %>%
  filter(date=="2015-09-01")



# Build plot
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   plot.title=element_blank(),
                   plot.tag = element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.2, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Map
g1 <- ggplot() +
  # Plot regional dividers
  # geom_hline(yintercept = c(49, 46, 42, 38.6, 34.448, 32.5)) +
  # Plot raster
  geom_tile(data=mhw_ras, mapping=aes(x=long_dd, lat_dd, fill=date_label), alpha=0.5) +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2) +
  # Extent / axes
  coord_sf(xlim = c(-150, -114), ylim = c(25, 60)) +
  scale_x_continuous(breaks=seq(-150, -110, 10)) +
  # Labels
  labs(tag="A") +
  # Legend
  scale_fill_ordinal(name="MHW extent") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title=element_blank(),
        axis.text=element_text(size=6),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        legend.position = c(0.82, 0.85),
        legend.key.size = unit(0.5, "cm"))
g1

# Coastwide coverage/intensity
g2 <- ggplot(data_wc_orig, aes(x=date, y=area_km2/1e6, color=intensity_c)) +
  geom_line(lwd=0.2) +
  # Labels
  labs(x="", y="Coverage (millions sqkm)", tag="B") +
  # Date axis
  scale_x_date(breaks=seq(ymd("1980-01-01"), ymd("2022-01-01"), by="5 years"), date_labels = "%Y") +
  # Legend
  scale_color_gradientn(name="Intensity (°C)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g2

# Regional coverage
g3 <- ggplot(data_reg, aes(y=region, x=date, fill=coverage_perc)) +
  geom_tile() +
  # Labels
  labs(x="", y="", tag="C") +
  # Date axis
  scale_x_date(breaks=seq(ymd("1980-01-01"), ymd("2022-01-01"), by="5 years"), date_labels = "%Y") +
  # Legend
  scale_fill_gradientn(name="Coverage (%)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5))
g3


# Merge
layout_matrix <- matrix(data=c(1,2,
                               3,3), byrow=T, ncol=2)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, widths=c(0.36, 0.74), heights=c(0.6, 0.4))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_mhw_timeline.png"),
       width=6.5, height=4.5, units="in", dpi=600)

