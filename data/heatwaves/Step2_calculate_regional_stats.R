
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
shpdir <-  "data/heatwaves/regions"

# Read data
data_orig <- readRDS(file=file.path(outdir, "COBE_1891_2022_NE_mhw_coverage.Rds"))

# Read region shapefile
region_orig <- sf::st_read(dsn=file.path(shpdir, "regions_final.shp"))


# Format shapefile
################################################################################

# Format
region <- region_orig %>%
  mutate(region=recode(region,
                       "Northern Californ"="Northern California"),
         region_code=recode_factor(region,
                                   "Southern California"="sCA",
                                   "Central California"="cCA",
                                   "Northern California"= "nCA",
                                   "Oregon"="OR",
                                   "Washington"="WA",
                                   "British Columbia"="BC",
                                   "Gulf of Alaska"="AK")) %>%
  arrange(region_code) %>%
  mutate(region_km2=sf::st_area(.) %>% as.numeric() / 1000^2)



# Find region for each cell
################################################################################

# Build cell key
cells <- data_orig %>%
  select(cell_id, long_dd, lat_dd, area_km2) %>%
  unique()

# Build raster
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")
cell_raster <- raster::rasterFromXYZ(cells %>% select(long_dd, lat_dd, cell_id), crs = wgs84)

# Find region for each raster cell
region_ras <- fasterize::fasterize(region, cell_raster, field="region_code")
region_ras_df <- region_ras %>%
  raster::as.data.frame(xy=T) %>%
  # Rename
  rename(long_dd=x, lat_dd=y, region_code=layer) %>%
  mutate(region_code=levels(region$region_code)[region_code]) %>%
  # Build cell id
  mutate(cell_id=paste(long_dd, lat_dd, sep="-"))

# Build cell key
cell_key <- cells %>%
  left_join(region_ras_df)

# Plot
g <- ggplot(cell_key, aes(x=long_dd, y=lat_dd, color=region_code, size=area_km2)) +
  geom_point()
g

# Add region to data
data <- data_orig %>%
  left_join(cell_key %>% select(cell_id, region_code)) %>%
  filter(!is.na(region_code)) %>%
  mutate(region_code=factor(region_code, levels=levels(region$region_code)))



# Region-wide
################################################################################

# Build stats
tot_area_km2 <- sum(region$region_km2)
stats1 <- data %>%
  filter(!is.na(mhw_c)) %>%
  group_by(year, date) %>%
  summarize(area_km2=sum(area_km2),
            intensity_c=mean(mhw_c)) %>%
  ungroup() %>%
  mutate(coverage=area_km2/tot_area_km2)

# Plot
ggplot(stats1, aes(x=date, y=area_km2/1e6, color=intensity_c)) +
  geom_line() +
  # Labels
  labs(x="Year", y="Coverage") +
  lims(x=c(ymd("1980-01-01"), ymd("2019-12-01"))) +
  # Legend
  scale_color_gradientn(name="Intensity (°C)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  # Theme
  theme_bw()


# Analysis
################################################################################

# Summarize by region
stats <- data %>%
  filter(!is.na(mhw_c)) %>%
  group_by(region_code, year, date) %>%
  summarize(area_km2=sum(area_km2),
            intensity_c=mean(mhw_c)) %>%
  ungroup() %>%
  left_join(region %>% sf::st_drop_geometry() %>% select(region_code, region_km2), by="region_code") %>%
  mutate(coverage=area_km2/region_km2,
         coverage=pmin(1, coverage))

# Plot
ggplot(stats, aes(x=date, y=region_code, size=area_km2, color=intensity_c)) +
  geom_point() +
  # Labels
  labs(x="Year", y="Region") +
  lims(x=c(ymd("1980-01-01"), ymd("2019-12-01"))) +
  # Legend
  scale_color_gradientn(name="Intensity (°C)", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  # Theme
  theme_bw()

# Plot
ggplot(stats, aes(x=date, y=region_code, fill=coverage)) +
  geom_tile() +
  # Labels
  labs(x="Year", y="Region") +
  lims(x=c(ymd("1980-01-01"), ymd("2019-12-01"))) +
  # Legend
  scale_fill_gradientn(name="% coverage", colors=RColorBrewer::brewer.pal(9, "YlOrRd"),
                       labels=scales::percent, lim=c(0,1)) +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw()









