
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "data/landings"

# Read NOAA data
noaa_orig <- wcfish::noaa

# Read PACFIN data
pacfin_orig <- wcfish::pacfin_all1


# Format PACFIN data
################################################################################

# Years
range(pacfin_orig$year)

# Format West Coast data
pacfin <- pacfin_orig %>%
  # Simplify
  select(comm_name, year, landings_lb, value_usd) %>%
  # Format common name
  mutate(comm_name=gsub("Nom. ", "", comm_name)) %>%
  # Reduce to 2011-2020
  filter(year %in% c(2011:2020)) %>%
  # Summarize
  group_by(comm_name) %>%
  summarise(value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Sort
  arrange(desc(value_usd))
