
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
data_orig <- readxl::read_excel(file.path(indir, "2001 to 2021 Above Sawmill Recreational Catch_manually_formatted.xlsx"),
                                na=c("unknown", "N/A"))

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Remove empty rows
  filter(fishery!="") %>%
  # Format fishery
  mutate(fishery=stringr::str_to_title(fishery)) %>%
  # Format species
  mutate(species=recode(species,
                        "CH"="Chinook salmon",
                        "Chinook"="Chinook salmon",
                        "Chinook-jack"="Chinook salmon (jack)",
                        "PK"="Pink salmon",
                        "Pink"="Pink salmon",
                        "SK"="Sockeye salmon",
                        "Sockeye"="Sockeye salmon")) %>%
  # Format year
  mutate(year=as.numeric(year)) %>%
  # Mark closed/open and creel yes/no
  mutate(status=ifelse(effort_hrs=="Closed" | (anglers_n=="Closed" & !is.na(anglers_n)), "Closed", "Open"),
         survey=ifelse(effort_hrs=="No Creel", "No creel survey", "Creel survey")) %>%
  # Format effort
  mutate(effort_hrs=case_when(effort_hrs=="Closed" ~ "0",
                              effort_hrs=="No Creel" ~ "",
                              TRUE ~ effort_hrs) %>% as.numeric()) %>%
  # Format anglers
  mutate(anglers_n=case_when(anglers_n=="Closed" ~ "0",
                             anglers_n=="No Creel" ~ "",
                              TRUE ~ anglers_n) %>% as.numeric()) %>%
  # Format harvest
  mutate(harvest_n=case_when(harvest_n=="Closed" ~ "0",
                             harvest_n=="No Creel" ~ "",
                             TRUE ~ harvest_n) %>% as.numeric()) %>%
  # Format release
  mutate(release_n=case_when(release_n=="Closed" ~ "0",
                             release_n=="No Creel" ~ "",
                             TRUE ~ release_n) %>% as.numeric()) %>%
  # Format catch
  mutate(catch_n=case_when(catch_n=="Closed" ~ "0",
                           catch_n=="No Creel" ~ "",
                           TRUE ~ catch_n) %>% as.numeric()) %>%
  # Format CPUE
  mutate(cpue=case_when(cpue=="Closed" ~ "0",
                             cpue=="No Creel" ~ "",
                             TRUE ~ cpue) %>% as.numeric()) %>%
  # Arrange
  select(fishery, location, species, year, status, survey, everything()) %>%
  arrange(fishery, location, species, year)

# Inspect
str(data)
freeR::complete(data)

# Inspect more
sort(unique(data$species))
sort(unique(data$location))


# Plot data
################################################################################

# Plot data
g <- ggplot(data, aes(y=location, x=year, fill=harvest_n)) +
  facet_grid(species~., scales="free_y", space="free_y") +
  geom_raster() +
  # Closed years
  geom_point(data= data %>% filter(status=="Closed"), pch="X", size=2) +
  geom_point(data= data %>% filter(survey=="No creel survey"), pch="o", size=2) +
  # Labels
  labs(x="Year", y="") +
  # Legend
  scale_fill_gradientn(name="Catch", colors=RColorBrewer::brewer.pal(9, "YlOrRd")) +
  # Theme
  theme_bw()
g






