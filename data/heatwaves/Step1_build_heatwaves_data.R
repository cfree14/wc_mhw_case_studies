
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)
library(heatwaveR)

# Directories
plotdir <- "figures"
outdir <- "data/heatwaves/processed"

# Read data
cobe_orig <- raster::brick("/Users/cfree/Dropbox/Chris/UCSB/data/sst/cobe/processed/COBE_1891_2021_monthly.grd")
raster::plot(cobe_orig, 1)

# Get dates
dates_orig <- names(cobe_orig) %>% gsub("X", "", .) %>% ymd()

# Detecting events in gridded data
# https://robwschlegel.github.io/heatwaveR/articles/gridded_event_detection.html


# Format SST data
################################################################################

# Crop raster
nep_extent <- raster::extent(c(-180, -100, 20, 80))
cobe <- raster::crop(x=cobe_orig, y=nep_extent)
raster::plot(cobe, 1)

# Build dataframe
cobe_df <- raster::as.data.frame(cobe, xy=T) %>%
  # Rename
  rename(long_dd=x, lat_dd=y) %>%
  # Gather dates
  gather(key="date", value="sst_c", 3:ncol(.)) %>%
  # Format date
  mutate(date=date %>% gsub("X", "", .) %>% lubridate::ymd() %>% as.Date()) %>%
  # Build cell id
  mutate(cell_id=paste(long_dd, lat_dd, sep="-")) %>%
  # Remove cells without SST data
  filter(!is.na(sst_c))

# Extract cell key
# cells <- cobe_df %>%
#   select(cell_id, long_dd, lat_dd) %>%
#   unique()


# Calculate cell area
cell_key <- raster::area(cobe) %>%
  # Convert to df with lat/long
  raster::as.data.frame(xy=T) %>%
  # Rename
  rename(long_dd=x, lat_dd=y, area_km2=layer) %>%
  # Build cell id
  mutate(cell_id=paste(long_dd, lat_dd, sep="-"))
cell_ids <- cells$cell_id

# Build heatwave data
################################################################################

# Climatology
clim_dates <- seq(ymd("1980-01-01"), ymd("2009-12-01"), by="1 month")
length(clim_dates) == (12*30)

# Build heatwave data
data <- cobe_df %>%
  # Add year and month
  mutate(year=year(date),
         month=month(date)) %>%
  # Mark whether inside climatology
  mutate(clim_yn=date %in% clim_dates) %>%
  # Compute climatology
  group_by(cell_id, month) %>%
  mutate(clim_c=mean(sst_c[date %in% clim_dates]),
         thresh_c=quantile(sst_c[date %in% clim_dates], probs=0.9)) %>%
  ungroup() %>%
  # Detect and measure heatwave
  mutate(mhw_yn=sst_c>thresh_c,
         mhw_c=ifelse(mhw_yn, sst_c-thresh_c, NA)) %>%
  # Add cell area
  left_join(cell_key %>% select(cell_id, area_km2), by="cell_id") %>%
  # Arrange
  select(cell_id, long_dd, lat_dd, area_km2, year, month, date,
         clim_c, thresh_c, sst_c, mhw_yn, mhw_c,
         everything())

# Inspect
# freeR::complete(data)


# Plot heatwave data
################################################################################

# Plot function
date <- "1982-10-01"
plot_heatwave<- function(data, date){

  # Subset data
  date_do <- date
  sdata <- data %>%
    filter(date==date_do) %>%
    filter(mhw_yn==T)

  # World
  world <- rnaturalearth::ne_countries(scale="small", returnclass = "sf")

  # Plot data
  g <- ggplot(sdata, aes(x=long_dd, y=lat_dd, fill=mhw_c)) +
    # Plot MHW
    geom_tile() +
    # Plot land
    geom_sf(data=world, fill="grey80", color="white", lwd=0.2, inherit.aes = F) +
    # Labels
    labs(x="", y="", title=date_do) +
    # Legend
    scale_fill_gradientn(name="MHW intensity (°C)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
    guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
    # Crop
    coord_sf(xlim=c(min(data$long_dd), max(data$long_dd)),
             ylim=c(min(data$lat_dd), max(data$lat_dd)),
             expand=F) +
    # Theme
    theme_bw() +
    theme(axis.text=element_text(size=7),
          axis.title=element_blank(),
          legend.text=element_text(size=6),
          legend.title=element_text(size=8),
          plot.title=element_text(size=10),
          axis.text.y = element_text(angle = 90, hjust = 0.5),
          # Gridlines
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          # Legend
          legend.background = element_rect(fill=alpha('blue', 0)))
  g

  # Plot/return
  print(g)
  return(g)

}

plot_heatwave(data=data, date="2016-04-01")


# Export heatwave data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "COBE_1891_2022_NE_mhw_coverage.Rds"))

