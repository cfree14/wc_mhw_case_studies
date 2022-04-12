
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
data_orig <- wcfish::noaa


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # WC rec fisheries
  filter(fishery=="Recreational" & state %in% c("California", "Oregon", "Washington", "Alaska")) %>%
  # Years of interest
  filter(year%in%2011:2019) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Simplify
  select(state, comm_name_orig, period, year, landings_lb) %>%
  # Arrange
  arrange(state, comm_name_orig, year) %>%
  # Remove missing data
  filter(!is.na(landings_lb)) %>%
  # Summarize by state, species, period
  group_by(state, comm_name_orig, period) %>%
  summarize(landings_lb=mean(landings_lb)) %>%
  ungroup() %>%
  # Format species
  mutate(comm_name=wcfish::convert_names(comm_name_orig, to="regular") %>% stringr::str_to_sentence())

# Plot data
g <- ggplot(data, aes(x=period, y=comm_name, fill=landings_lb)) +
  facet_wrap(~state) +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw()
g





