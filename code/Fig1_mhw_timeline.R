
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

# Read MHW rasters
mhw_ras_orig <- readRDS(file=file.path(mhwdir, "COBE_1891_2022_NE_mhw_coverage.Rds"))
stats_all_orig <- readRDS(file=file.path(mhwdir, "COBE_1891_2022_NE_mhw_stats_overall.Rds"))
stats_region_orig <- readRDS(file=file.path(mhwdir, "COBE_1891_2022_NE_mhw_stats_region.Rds"))

# Read region shapefile
regions <- readRDS("data/heatwaves/regions/regions.Rds")

# Get US states and Mexico
usa <- rnaturalearth::ne_states(country = "United States of America", returnclass = "sf")
mexico <- rnaturalearth::ne_countries(country="Mexico", returnclass = "sf")
canada <- rnaturalearth::ne_countries(country="Canada", returnclass = "sf")


# Format data
################################################################################

# Dates use
mhw_dates <- seq(ymd("2013-09-01"), ymd("2016-09-01"), by="6 months")[1:6]

# Build data
mhw_ras <- mhw_ras_orig %>%
  # Reduce to dates of interest
  filter(date %in% mhw_dates) %>%
  # Reduce to MHW
  filter(mhw_yn==T) %>%
  # Recode date
  mutate(date_chr=as.character(date),
         date_label=recode_factor(date_chr,
                                  "2013-09-01"="Sep 2013",
                                  "2014-03-01"="Mar 2014",
                                  "2014-09-01"="Sep 2015",
                                  "2015-03-01"="Mar 2015",
                                  "2015-09-01"="Sep 2016",
                                  "2016-03-01"="Mar 2016"))


# Region stats
stats_region <- stats_region_orig %>%
  filter(date>="1980-01-01" & date <= "2019-12-01")
stats_all <- stats_all_orig %>%
  filter(date>="1980-01-01" & date <= "2019-12-01")

# Build plot
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_blank(),
                   plot.tag = element_text(size=9),
                   plot.tag.position = c(0.007, 1),
                   # Margin
                   plot.margin = margin(-3, 0, 4, 2),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.2, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Map
g1 <- ggplot(data=mhw_ras, mapping=aes(x=long_dd, lat_dd, fill=mhw_c)) +
  facet_wrap(~date_label, nrow=1) +
  # Plot raster
  geom_tile() +
  # Plot regions
  geom_sf(data=regions, fill=NA, color="grey30", lwd=0.2, inherit.aes = F) +
  # Plot states
  geom_sf(data=usa, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=mexico, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  geom_sf(data=canada, fill="grey85", col="white", size=0.2, inherit.aes = F) +
  # Extent / axes
  coord_sf(xlim = c(-155, -114), ylim = c(25, 60)) +
  scale_x_continuous(breaks=seq(-150, -110, 10)) +
  # Labels
  labs(tag="A", y=" \n ") +
  # Legend
  scale_fill_gradientn(name="Heatwave\nintensity (°C)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text=element_blank(),
        legend.position = "right")
#g1

# Overall
g2 <- ggplot(stats_all, aes(x=date, y=coverage, color=intensity_c)) +
  geom_line(lwd=0.8) +
  # Labels
  labs(x="", y="Coverage (%)", tag="B") +
  # Date axis
  scale_y_continuous(labels=scales::percent) +
  scale_x_date(breaks=seq(ymd("1980-01-01"), ymd("2022-01-01"), by="5 years"), date_labels = "%Y") +
  # Legend
  scale_color_gradientn(name="Heatwave\nintensity (°C) ",
                        colors=RColorBrewer::brewer.pal(9, "YlOrRd")[2:9]) +
  guides(color = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g2

# Region
g3 <- ggplot(stats_region, aes(y=region_code, x=date, fill=coverage)) +
  geom_tile() +
  # Labels
  labs(x="", y="Region", tag="C") +
  # Date axis
  scale_x_date(breaks=seq(ymd("1980-01-01"), ymd("2022-01-01"), by="5 years"), date_labels = "%Y") +
  # Legend
  scale_fill_gradientn(name="Heatwave\ncoverage (%)", colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       labels=scales::percent) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5))
g3

# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=1, heights=c(0.35, 0.65/2, 0.65/2))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig1_mhw_timeline.png"),
       width=6.5, height=5, units="in", dpi=600)

