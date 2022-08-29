
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "data/landings"

# The goal is to have a dataframe with the following columns
# state, mgmt_group, comm_name, sci_name, level, year, landings_lb, value (ultimately in 2020 USD)

# The steps for making this happen can be:
# 1) Format each individually, getting as close as reasonable
# 2) Merge, then inspect formatting


# Format PACFIN data (CA/OR/WA)
################################################################################

# Read PACFIN data
pacfin_orig <- wcfish::pacfin_all1
pacfin_spp <- wcfish::pacfin_species

# Format West Coast data
pacfin <- pacfin_orig %>%
  # Simplify
  select(state, mgmt_group, comm_name, sci_name, year, landings_lb, value_usd) %>%
  # Fix some species
  mutate(comm_name=recode(comm_name,
                          "Black skate"="Roughtail skate",
                          "Native littleneck"="Pacific littleneck clam",
                          "Blue or bay mussel"="Blue mussel",
                          "Dorado/dolphinfish"="Dolphinfish",
                          "Mola/ocean sunfish"="Mola mola",
                          "Black and yellow rockfish"="Black-and-yellow rockfish",
                          "Nom. Calif halibut"="Nom. California halibut",
                          "Nom. Vermillion rockfish"="Nom. Vermilion rockfish",
                          "Nom. Squarespot"="Nom. Squarespot rockfish",
                          "Nom. Chilipepper"="Nom. Chilipepper rockfish",
                          "Nom. Pop"="Nom. Pacific ocean perch",
                          "Nom. Rougheye + blackspotted"="Nom. Rougheye + blackspotted rockfish")) %>%
  # Mark nominal / not nominal
  mutate(nominal=ifelse(grepl("nom.", tolower(comm_name)), "yes", "no")) %>%
  # Mark parent species (to merge nominal and true)
  mutate(comm_name_parent=gsub("Nom. ", "", comm_name)) %>%
  # Format confidential data
  mutate(mgmt_group=recode(mgmt_group, "Withheld for confidentiality"="Other"),
         comm_name=ifelse(is.na(comm_name), "Unknown", comm_name),
         comm_name_parent=ifelse(is.na(comm_name_parent), "Unknown", comm_name_parent)) %>%
  # Fix a few management groups
  mutate(mgmt_group=ifelse(comm_name=="Roughtail skate", "Groundfish", mgmt_group),
         mgmt_group=ifelse(comm_name=="Unknown", "Other", mgmt_group)) %>%
  # Mark species level
  mutate(level=ifelse(grepl("unknown|mixed|spp\\.|unsp\\.|other|misc\\.|gen\\.|\\+", tolower(comm_name_parent)), "group", "species")) %>%
  # Summarize
  group_by(state, mgmt_group, comm_name_parent, level, year) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Rename
  rename(comm_name=comm_name_parent) %>%
  # Add scientific name
  left_join(pacfin_spp %>% select(comm_name, sci_name)) %>%
  # Correct a few groups
  mutate(level=ifelse(grepl("spp", sci_name), "group", level)) %>%
  # Erase scientific names for groups
  mutate(sci_name=ifelse(level=="group", NA, sci_name)) %>%
  # Fix a few scientific names
  mutate(sci_name=recode(sci_name,
                         "Arpisturus brunneus"="Apristurus brunneus",
                         "Callianassa californiensis"="Neotrypaea californiensis",
                         "Cancer magister"="Metacarcinus magister",
                         "Clupea harengus pallasii"="Clupea pallasii pallasii",
                         "Crassostrea gigas kumamoto"="Crassostrea sikamea",
                         "Etrumeus teres"="Etrumeus teres",
                         "Galeorhinus zyopterus"="Galeorhinus galeus",
                         "Istiphorus platypterus"="Istiophorus platypterus",
                         "Loligo opalescens"="Doryteuthis opalescens",
                         "Nuttallia nuttalli"="Nuttallia nuttallii",
                         "Pleuronichthys guttulatus"="Hypsopsetta guttulata",
                         "Saxidomus giganteus"="Saxidomus gigantea",
                         "Scorpaena gutatta"="Scorpaena guttata",
                         # "Sebastes diaconus"="",
                         "Strongylocentrotus franciscanus"="Mesocentrotus franciscanus",
                         "Tetrapterus audax"="Kajikia audax",
                         "Theragra chalcogramma"="Gadus chalcogrammus")) %>%
  # Fill in some sciname gaps
  mutate(sci_name=ifelse(comm_name=="Mola mola", "Mola mola", sci_name)) %>%
  mutate(sci_name=ifelse(comm_name=="Black-and-yellow rockfish", "Sebastes chrysomelas", sci_name)) %>%
  mutate(sci_name=ifelse(comm_name=="Dolphinfish", "Coryphaena hippurus", sci_name)) %>%
  mutate(sci_name=ifelse(comm_name=="Pacific littleneck clam", "Leukoma staminea", sci_name)) %>%
  mutate(sci_name=ifelse(comm_name=="Blue mussel", "Mytilus edulis", sci_name)) %>%

  # Arrange
  select(state, mgmt_group, comm_name, sci_name, level, year, everything()) %>%
  arrange(state, mgmt_group, comm_name, sci_name, level, year)

# Inspect data
freeR::complete(pacfin) # only sci_name can be zero

# Inspect columns
table(pacfin$mgmt_group)

# Inspect species key
spp_key1 <- pacfin %>%
  select(mgmt_group, comm_name, sci_name, level) %>%
  unique()

# Perform checks
freeR::check_names(spp_key1$sci_name) # Etrumeus teres", "Gadus chalcogrammus", "Sebastes diaconus" are fins
freeR::which_duplicated(spp_key1$sci_name) # must be zero
freeR::which_duplicated(spp_key1$comm_name) # must be zero
spp_key1$comm_name[is.na(spp_key1$sci_name) & spp_key1$level=="species"] # must be zero




# Format NOAA data (AK)
################################################################################

# Read NOAA data
noaa_orig <- wcfish::noaa

# Format Alaska data
alaska <- noaa_orig %>%
  # Alaskan commercial fisheries
  filter(fishery=="Commercial" & state=="Alaska") %>%
  # Simplify
  select(state, comm_name, sci_name, level, year, landings_lb, value_usd) %>%
  # Confirm that this is unique? Looks good.
  group_by(state, comm_name, sci_name, level, year) %>%
  mutate(n=n()) %>%
  ungroup() %>%
  select(-n) %>%
  # Format level
  mutate(level=ifelse(level=="species", "species", "group")) %>%
  # Format common names
  mutate(comm_name=recode(comm_name,
                          "Rainbow trout"="Steelhead",
                          "Eulachon smelt"="Eulachon",
                          "Pacific ocean perch rockfish"="Pacific ocean perch",
                          "Bocaccio rockfish"="Bocaccio",
                          "Albacore tuna"="Albacore",
                          "Sea mussel"="Blue mussel",
                          "Nuttall cockle"="Basket cockle",
                          "Pacific razor clam"='Razor clam',
                          "Pacific geoduck clam"="Geoduck",
                          "California market squid"="Market squid",
                          "Jumbo squid"="Humbolt squid",
                          "Ocean shrimp"="Pacific pink shrimp",
                          "Southern tanner crab"="Bairdi tanner crab",
                          "Spiny shark dogfish"="Spiny dogfish")) %>%
  # Format scientific names
  mutate(sci_name=ifelse(level=="group", NA, sci_name)) %>%
  mutate(sci_name=recode(sci_name,
                         "Cancer magister"="Metacarcinus magister",
                         "Clupea pallasii"="Clupea pallasii pallasii",
                         # "Gadus chalcogrammus"
                         "Loligo opalescens"="Doryteuthis opalescens",
                         "Protothaca staminea"="Leukoma staminea",
                         "Squalus acanthias"="Squalus suckleyi",
                         "Paraplagusia bilineata"="Lepidopsetta bilineata",
                         "Amblyraja radiata"="Beringraja stellulata"))

# Alaska species key
spp_key2 <- alaska %>%
  # Unique AK species
  select(comm_name, sci_name, level) %>%
  unique() %>%
  # Assign mgmt group
  left_join(spp_key1 %>% select(comm_name, mgmt_group), by="comm_name") %>%
  # Fill in missing mgmt groups
  mutate(mgmt_group=ifelse(is.na(mgmt_group) & grepl("pacific salmon", tolower(comm_name)), "Salmon", mgmt_group),
         mgmt_group=ifelse(is.na(mgmt_group) & grepl("crab|crustaceans", tolower(comm_name)), "Crabs", mgmt_group),
         mgmt_group=ifelse(is.na(mgmt_group) & grepl("clam|scallop|conch|abalone", tolower(comm_name)), "Shellfish", mgmt_group),
         mgmt_group=ifelse(is.na(mgmt_group) & grepl("shrimp", tolower(comm_name)), "Shrimp", mgmt_group),
         mgmt_group=ifelse(is.na(mgmt_group) & grepl("yellowfin sole|greenland halibut|righteye flounders|flatfishes|rockfishes|soles|shark|skate|sculpin|grenadier|octopus|squid", tolower(comm_name)), "Groundfish", mgmt_group),
         mgmt_group=ifelse(is.na(mgmt_group), "Other", mgmt_group))

# Check
freeR::check_names(spp_key2$sci_name) # "Gadus chalcogrammus" "Salvelinus alpinus" are fine
freeR::which_duplicated(spp_key2$sci_name) # must be zero
freeR::which_duplicated(spp_key2$comm_name) # must be zero
freeR::complete(spp_key2) # only sci name can be zero (should have filled mgmt groups)
spp_key2$comm_name[is.na(spp_key2$sci_name) & spp_key2$level=="species"] # must be zero


# Add mgmt group to Alaska data
alaska1 <- alaska %>%
  # Add management group
  left_join(spp_key2) %>%
  # Reduce to post-1980
  filter(year>=1980)

# Inspect
freeR::complete(alaska1) # should only be missing sci name and one value


# Merge Alaska and West Coast data
################################################################################

# Merge
usa <- bind_rows(pacfin, alaska1) %>%
  # Arrange
  select(state, mgmt_group, comm_name, sci_name, level, year, landings_lb, value_usd, everything()) %>%
  arrange(state, mgmt_group, comm_name, sci_name, level, year) %>%
  # Fix a few common names (to align w/ Canada data)
  mutate(comm_name=recode(comm_name,
                          "Mola mola"="Ocean sunfish",
                          "Humbolt squid"="Humboldt squid",
                          "C-o sole"="C-O sole",
                          "Pacific whiting"="Pacific hake",
                          "Bairdi tanner crab"="Tanner crab",
                          "Rock sole"="Southern rock sole")) %>%
  # Fix a few scientific names
  mutate(sci_name=recode(sci_name,
                         "Raja binoculata"="Beringraja binoculata",
                         "Raja rhina"="Beringraja rhina",
                         "Raja stellulata"="Beringraja stellulata"))

# Inspect USA data
freeR::complete(usa)

# Check that common names have unique scientific names
usa_key1 <- usa %>%
  group_by(mgmt_group, comm_name, level) %>%
  summarize(n_sci=n_distinct(sci_name),
            sci_name=paste(sort(unique(sci_name)), collapse=", ")) %>%
  ungroup()
sum(usa_key1$n_sci>1)

# Check that scientific names have unique common names
usa_key2 <- usa %>%
  group_by(mgmt_group, sci_name, level) %>%
  summarize(n_comm=n_distinct(comm_name),
            comm_name=paste(sort(unique(comm_name)), collapse=", ")) %>%
  ungroup() %>%
  filter(level=="species")
sum(usa_key2$n_comm>1)

# US key
spp_key_usa <- usa %>%
  select(mgmt_group, comm_name, sci_name, level) %>%
  unique() %>%
  arrange(mgmt_group, comm_name)
freeR::which_duplicated(spp_key_usa$comm_name) # must be zero
freeR::which_duplicated(spp_key_usa$sci_name) # must be zero

# #
# write.csv(spp_key, file.path(outdir, "NOAA_PACFIN_comm_landings_spp_key_temp.csv"), row.names = F)
# spp_key_use <- readxl::read_excel(file.path(outdir, "NOAA_PACFIN_comm_landings_spp_key.xlsx"))

# Update species info
# data_out <- data %>%
#   select(-c(complex, sci_name)) %>%
#   left_join(spp_key_use %>% select(comm_name, mgmt_group_use)) %>%
#   select(state, mgmt_group, mgmt_group_use, comm_name, level, everything()) %>%
#   arrange(state, mgmt_group, mgmt_group_use, comm_name, year)


# Format DFO (BC)
################################################################################

# Read DFO data
canada_orig <- readRDS("data/landings/canada/processed/2000_2020_dfo_pacific_landings.Rds")

# 2020 exchange rate
# 1 USD = 1.3415 CD
# X USD = Y CD / 1.3415
er <- 1.3415

# Format data
canada <- canada_orig %>%
  # Rename
  rename(level=type, value_cd2020=value_2020) %>%
  # Summarize
  group_by(comm_name, sci_name, level, year, ) %>%
  summarize(landings_kg=sum(landings_kg, na.rm=T),
            value_cd2020=sum(value_cd2020, na.rm=T)) %>%
  ungroup() %>%
  # Convert kg to lb
  mutate(landings_lb=measurements::conv_unit(landings_kg, "kg", "lbs")) %>%
  # Convert from 2020 Canadian dollars to 2020 US dollars
  mutate(value_usd2020=value_cd2020/er) %>%
  # Erase scientific names from groups
  mutate(sci_name=ifelse(level=="group", NA, sci_name)) %>%
  # Fix a few common names (to align with USA) %>%
  mutate(comm_name=recode(comm_name,
                          "Prawn"="Spotted prawn")) %>%
  # Add management group
  left_join(spp_key_usa %>% select(comm_name, mgmt_group), by="comm_name") %>%
  # Final formatting
  mutate(state="British Columbia") %>% rename(value_usd=value_usd2020) %>%
  select(state, mgmt_group, comm_name, sci_name, level, year, landings_lb, value_usd)

# Check Canada species key
spp_canada <- canada %>%
  # Canada names
  select(mgmt_group, comm_name, sci_name,  level) %>%
  unique() %>%
  rename(comm_name_dfo=comm_name) %>%
  # Add US common name
  left_join(spp_key_usa %>% filter(level=="species") %>% select(sci_name, comm_name), by=c("sci_name")) %>%
  mutate(check=comm_name_dfo==comm_name)

# Check
freeR::check_names(spp_canada$sci_name) # "Beringraja binoculata"  "Beringraja rhina"       "Gadus chalcogrammus"    "Tetronarce californica" are fine is fine
freeR::which_duplicated(spp_canada$sci_name)
freeR::which_duplicated(spp_canada$comm_name)


# Merge
################################################################################

# Merge data
data1 <- bind_rows(usa, canada)

# Species key
spp_key <- data1 %>%
  select(mgmt_group, comm_name, sci_name, level) %>%
  unique() %>%
  arrange(mgmt_group, sci_name)
freeR::which_duplicated(spp_key$comm_name)
write.csv(spp_key, file.path(outdir, "WC_comm_landings_spp_key_temp.csv"), row.names = F)
spp_key_use <- readxl::read_excel(file.path(outdir, "WC_comm_landings_spp_key_temp.xlsx"))

# Add mgmt group
data2 <- data1 %>%
  left_join(spp_key_use %>% select(mgmt_group_use, comm_name) %>% unique(.), by="comm_name") %>%
  select(-mgmt_group) %>%
  rename(mgmt_group=mgmt_group_use) %>%
  select(state, mgmt_group, comm_name, everything())

# Inspect
freeR::complete(data2)

# Export
################################################################################

# Export
saveRDS(data2, file=file.path(outdir, "WC_1980_2021_commercial_landings.Rds"))








