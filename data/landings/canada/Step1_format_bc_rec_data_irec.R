
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/landings/canada/raw"
outdir <- "data/landings/canada/processed"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "iREC estimates Jul 2012 to Mar 2022.xlsx"), sheet="iREC estimates",
                                na=c("N/A"))


# Mixture of catch/effort

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(region=logistical_area)

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$year)
table(data$month)
table(data$region)
table(data$area)
table(data$method)
table(data$adipose_modifier)
table(data$item)
table(data$disposition)
table(data$retainable)
table(data$item_group)
