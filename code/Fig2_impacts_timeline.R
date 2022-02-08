
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
mhwdir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/wc_cc_synthesis/data/environmental/indices/processed/"
datadir <- "data/mhw_impacts_timeline"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_excel(file.path(datadir, "MHW_impacts_timeline.xlsx"))

# Read MHW data
mhw_orig <- readRDS(file.path(mhwdir, "1982_2020_daily_mhw_stats.Rds"))



# Format data
################################################################################

# Date limit
date_min <- ymd("2013-09-01")
date_max <- ymd("2016-11-01")

# Levels
levels_do <- c("Physical", "Phytoplankton", "Zooplankton", "Fish/invertebrates",  "Mammals", "Seabirds", "Fisheries")

# Format data
data <- data_orig %>%
  # Order levels
  mutate(level=factor(level, levels=levels_do)) %>%
  # Format data
  mutate(date1=ymd(date1),
         date2=ymd(date2)) %>%
  # Cap date
  mutate(date1_cap=pmax(date1, date_min),
         date2_cap=pmin(date2, date_max))

# Format heatwave data
mhw <- mhw_orig %>%
  # Reduce to dates of interest
  filter(date>=date_min & date <= date_max) %>%
  # Add label
  mutate(level=factor("Physical", levels=levels_do))



# Build plot
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=8),
                  axis.title=element_blank(),
                  legend.text=element_text(size=6),
                  legend.title=element_text(size=8),
                  plot.title=element_blank(),
                  # Gridlines
                  axis.line.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(),
                  panel.border = element_blank(),
                  axis.line = element_line(colour = "black"),
                  # Legend
                  legend.position = "bottom",
                  legend.background = element_rect(fill=alpha('blue', 0)))

# Date breaks
date_breaks <- seq(ymd("2014-01-01"), ymd("2017-01-01"), by="6 months")

# Plot
g <- ggplot(data) +
  # Plot MHW intensity
  geom_raster(data=mhw, mapping=aes(y=level, x=date, fill=perc_cover), alpha=0.5) +
  # Plot reference lines
  geom_hline(yintercept = levels_do, color="grey80", lwd=1) +
  geom_vline(xintercept=date_breaks, color="grey80", linetype="dotted") +
  # Plot event lines
  geom_segment(data=data, mapping=aes(y=level, yend=level, x=date1_cap, xend=date2_cap), position = position_jitter(width=0, height=0.4, seed = 1), size=0.2) +
  # Plot event labels
  geom_point(data=data, mapping=aes(y=level, x=date1_cap), size=2, position = position_jitter(width=0, height=0.4, seed = 1)) +
  geom_text(data=data, mapping=aes(y=level, x=date1_cap, label=event), hjust=-0.1, vjust=-0.25, size=2, position = position_jitter(width=0, height=0.4, seed = 1)) +
  # Labels
  labs(x="", y="") +
  # Y-axis
  scale_y_discrete(drop=F) +
  # X-axis
  scale_x_date(breaks=date_breaks,
              date_labels = "%b\n%Y",
              limits=c(date_min, date_max)) +
  # Legend
  scale_fill_gradientn(name='MHW\ncoverage', colors=RColorBrewer::brewer.pal(9, "YlOrRd"), labels=scales::percent) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g


# Export
ggsave(g, filename=file.path(plotdir, "Fig2_mhw_impacts_timeline.png"),
       width=6.5, height=6.5, units="in", dpi=600)

