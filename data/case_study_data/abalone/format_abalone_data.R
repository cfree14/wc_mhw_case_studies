
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/case_study_data/abalone/raw"
outdir <- "data/case_study_data/abalone"

# Source
# https://wildlife.ca.gov/Conservation/Marine/Invertebrates/Abalone/Abalone-Report-Card
# https://nrm.dfg.ca.gov/FileHandler.ashx?DocumentID=133567&inline

# Read data
data_orig <- readxl::read_excel(file.path(indir, "AbCatch2002-15.xlsx"))

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Gather
  gather(key="year", value="landings_n", 4:ncol(.)) %>%
  # Remove averages
  filter(year!="Averages") %>%
  # Convert to numeric
  mutate(year=year %>% gsub("\\*", "", .) %>% as.numeric(.),
         landings_n=landings_n %>% gsub("\\*", "", .) %>% as.numeric(.))

# Remove totals
data1 <- data %>%
  filter(county!="Statistics")

# Extract totals
data_tots <- data %>%
  # Reduce to totals
  filter(county=="Statistics") %>%
  # Remove useless
  select(-c(county, code)) %>%
  # Spread
  spread(key="site", value="landings_n") %>%
  # Rename
  rename("landings_ci"="95% Confidence limit",
         "landings_n_hi"="High (plus 95% confidence)",
         "landings_n_lo"="Low (minus 95% confidence)",
         "landings_n"="Total estimate") %>%
  # Arrange
  select(year, landings_n, landings_ci, landings_n_hi, landings_n_lo)

# Inspect
str(data1)
range(data1$year)
sort(unique(data1$county))
sort(unique(data1$site))


# Export data
################################################################################

# Export data
saveRDS(data1, file=file.path(outdir, "CDFW_2002_2015_red_abalone_catch_by_site.Rds"))
saveRDS(data_tots, file=file.path(outdir, "CDFW_2002_2015_red_abalone_catch.Rds"))





