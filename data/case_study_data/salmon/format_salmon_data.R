
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/case_study_data/salmon/raw"
outdir <- "data/case_study_data/salmon"

# Read data
table24_orig <- read.csv(file.path(indir, "Table2-4.csv"), as.is=T, na.strings=c("--", "NA"))


# Format data
################################################################################

# Format data
table24 <- table24_orig %>%
  # Convert year
  mutate(year = year %>% gsub("c/", "", .) %>% as.numeric(.),
         preseason = preseason %>% gsub(",|b/|d/", "", .) %>% as.numeric(.),
         postseason = postseason %>% gsub(",", "", .) %>% as.numeric(.))


# Inspect
str(table24)
range(table24$year)

# Plot data
g <- ggplot(table24 %>% filter(age=="Total Adults"), aes(x=year, y=postseason)) +
  geom_line()
g


# Export data
################################################################################

# Export data
saveRDS(table24, file=file.path(outdir, "PFMC_1985_2022_klamath_fall_chinook_forecast_obs.Rds"))





