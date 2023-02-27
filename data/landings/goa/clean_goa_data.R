
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/landings/goa/raw"
outdir <- "data/landings/goa/processed"

# Species codes
pacfin_spp <- wcfish::pacfin_species

# Read data
data_orig1 <- readxl::read_excel(file.path(indir, "GOA_EXVESSEL_REVENUE_02242023.xlsx"), sheet="annual")
data_orig2 <- readxl::read_excel(file.path(indir, "GOA_EXVESSEL_REVENUE_02242023.xlsx"), sheet="annual_category")
data_orig3 <- readxl::read_excel(file.path(indir, "GOA_EXVESSEL_REVENUE_02242023.xlsx"), sheet="annual_species")


# Format data
################################################################################

# Format data
data1 <- data_orig1 %>%
  janitor::clean_names("snake") %>%
  rename(value_usd=value)

# Inspect
str(data1)

# Format data
data2 <- data_orig2 %>%
  janitor::clean_names("snake") %>%
  rename(value_usd=value)

# Inspect
str(data2)

# Format data
data3 <- data_orig3 %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(value_usd=value) %>%
  # Format common name
  mutate(comm_name=wcfish::convert_names(species_name, to = "regular"),
         comm_name=recode(comm_name,
                          "Pacific geoduck clam"="Geoduck",
                          # "Weathervane scallop"="",
                          # "Majestic squid"="",
                          "Bairdi crab tanner"="Bairdi tanner crab",
                          "Golden (brown) king crab"="Golden king crab",
                          "Alaska plaice flounder"="Alaska plaice",
                          # "General flounder"="",
                          # "Pacific sleeper shark"="",
                          # "General snailfish"="",
                          "eels or eel-like fish"="Eels or eel-like fish",
                          "jellyfish"="Jellyfish",
                          # "Smooth lumpsucker"="",
                          "prowfish"="Prowfish",
                          "sea cucumber"="Sea cucumber",
                          # "Salmon shark"="",
                          "wolf-eel"="Wolf eel",
                          "grenadier (rattail)"="Grenadiers",
                          # "North Pacific octopus"="",
                          # "General sculpin"="",
                          "Spiny dogfish shark"="Spiny dogfish",
                          "Alaskan skate"="Alaska skate",
                          # "Whiteblotched skate"="",
                          # "Other skate"="",
                          "Bocaccio rockfish"="Bocaccio",
                          # "Other rockfish"="",
                          "Thornyhead (idiots) rockfish"="Thornyheads",
                          "Yelloweye (red snapper) rockfish"="Yelloweye rockfish",
                          "Pacific (gray) cod"="Pacific cod",
                          "Atka mackerel greenling"="Atka mackerel",
                          "lingcod"="Lingcod",
                          "sablefish (blackcod)"="Sablefish",
                          "Sm. chinook"="Chinook salmon",
                          # "Roe (unknown species) salmon"="",
                          "Steelhead trout"="Steelhead",
                          "Coonstriped shrimp"="Coonstripe shrimp",
                          # "Spot shrimp"=""
                          )) %>%
  # Add scientific name
  left_join(pacfin_spp %>% select(comm_name, sci_name), by="comm_name") %>%
  mutate(sci_name=case_when(comm_name=="Weathervane scallop" ~ "Patinopecten caurinus",
                            comm_name=="Majestic squid" ~ "Patinopecten caurinus",
                            comm_name=="Pacific sleeper shark" ~ "Somniosus pacificus",
                            comm_name=="Smooth lumpsucker" ~ "Aptocyclus ventricosus",
                            comm_name=="Salmon shark" ~ "Lamna ditropis",
                            comm_name=="North Pacific octopus" ~ "Enteroctopus dofleini",
                            comm_name=="Whiteblotched skate" ~ "Bathyraja maculata",
                            comm_name=="Coonstripe shrimp" ~ "Pandalus hypsinotus",
                            comm_name=="Spot shrimp" ~ "Pandalus platyceros",
                            T ~ sci_name)) %>%
  # Format sci name
  mutate(sci_name=recode(sci_name,
                         "Cancer magister"="Metacarcinus magister",
                         "Clupea harengus pallasii"="Clupea pallasii pallasii",
                         "Pleuronectus quadrituberculatus"="Pleuronectes quadrituberculatus"))

# Which common names are missing species names?
data3 %>%
  filter(is.na(sci_name)) %>%
  pull(comm_name) %>% unique()

# Check names
freeR::check_names(data3$sci_name %>% unique() %>% sort())
