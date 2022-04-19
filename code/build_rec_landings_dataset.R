
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "data/landings"

# Read CDFW data
cpfv_ca_orig <- wcfish::cdfw_cpfv

# Read RECFIN data
recfin_orig <- wcfish::recfin_cte2
salmon_ca_orig <- wcfish::recfin_cte7
salmon_orwa_orig <- wcfish::recfin_cte5


# Build data
################################################################################

# Build CA salmon data
salmon_ca <- salmon_ca_orig %>%
  # Summarize
  group_by(state, mode, comm_name, year) %>%
  summarize(retained_n=sum(retained_n)) %>%
  ungroup() %>%
  # Add source
  mutate(source="RECFIN CTE007")

# Build OR/WA salmon data
salmon_orwa <- salmon_orwa_orig %>%
  # Summarize
  group_by(state, mode, comm_name, year) %>%
  summarize(retained_n=sum(retained_n)) %>%
  ungroup() %>%
  # Add source
  mutate(source="RECFIN CTE005")

# Build salmon
salmon <- bind_rows(salmon_ca, salmon_orwa)

# Build RECFIN data
recfin <- recfin_orig %>%
  # Reduce to retained
  filter(status=="Retained") %>%
  # Reduce to USA waters (not Canada or Mexico)
  filter(!water_area %in% c("Canada", "Mexico")) %>%
  # Summarize
  group_by(state, mode, comm_name, year) %>%
  summarize(retained_n=sum(catch_n)) %>%
  ungroup() %>%
  # Add source
  mutate(source="RECFIN CTE002")


# Build CDFW data
cdfw <- cpfv_ca_orig %>%
  # Rename
  rename(retained_n=landings_n) %>%
  # Reduce to US waters
  filter(waters=="USA") %>%
  # Add columns
  mutate(state="California",
         mode="Party/Charter Boats",
         source="CDFW CPFV data") %>%
  # Arrange
  select(-c(waters, comm_name_orig, sci_name)) %>%
  select(source, state, mode, comm_name, year, retained_n, everything())

# CDFW species
cdfw_spp <- sort(unique(cdfw$comm_name))




# Compare CA data with RECFIN data
################################################################################

# Build CA RECFIN data
recfin_ca <- recfin_orig %>%
  # Reduce to CA and US waters
  filter(state=="California" & !water_area %in% c("Canada", "Mexico") & mode=="Party/Charter Boats") %>%
  # Summarize
  group_by(year) %>%
  summarise(retained_n_recfin=sum(catch_n)) %>%
  ungroup()

# Build CA CDFW data
cdfw_ca <- cdfw %>%
  # Summarize
  group_by(year) %>%
  summarise(retained_n_cdfw=sum(retained_n)) %>%
  ungroup() %>%
  # Add RECFIN
  left_join(recfin_ca)

# Plot
g <- ggplot(cdfw_ca, aes(x=year, y=retained_n_cdfw/1e6)) +
  geom_line(color="red") +
  geom_line(data=cdfw_ca, mapping=aes(x=year,y=retained_n_recfin/1e6), color="blue")
g


# Build final dataset
################################################################################

# Build merged data
data <- bind_rows(recfin, salmon) %>%
  # Arrange
  select(source, state, mode, comm_name, year, retained_n, everything()) %>%
  arrange(source, state, mode, comm_name, year)


table(data$mode)
# Data
# state, mode, common name, year, landings


# Export data
saveRDS(data, file=file.path(outdir, "RECFIN_annual_rec_landings_by_state.Rds"))







