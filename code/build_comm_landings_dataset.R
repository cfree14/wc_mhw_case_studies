
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

# Format West Coast data
pacfin <- pacfin_orig %>%
  # Simplify
  select(state, mgmt_group, complex, comm_name, sci_name, year, landings_lb, value_usd) %>%
  # Fix some species
  mutate(comm_name=recode(comm_name,
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
  # Mark species level
  mutate(level=ifelse(grepl("unknown|mixed|spp\\.|unsp\\.|other|misc\\.|gen\\.|\\+", tolower(comm_name_parent)), "group", "species")) %>%
  # Summarize
  group_by(state, mgmt_group, comm_name_parent, level, year) %>%
  summarize(complex=paste(sort(unique(complex)), collapse = ", "),
            landings_lb=sum(landings_lb),
            value_usd=sum(value_usd)) %>%
  ungroup() %>%
  # Rename
  rename(comm_name=comm_name_parent) %>%
  # Format complex
  mutate(complex=recode(complex, "...., Miscellaneous groundfish"="Miscellaneous groundfish")) %>%
  # Arrange
  select(state, mgmt_group, complex, comm_name, level, year, everything()) %>%
  arrange(state, mgmt_group, complex, comm_name, level, year)

# Inspect data
freeR::complete(pacfin)

# Inspect columns
table(pacfin$mgmt_group)
table(pacfin$complex)

# Inspect species
spp_key1 <- pacfin %>%
  count(mgmt_group, complex, comm_name, level) %>%
  arrange(mgmt_group, complex, comm_name)


# Visualize coverage
ggplot(pacfin %>% filter(level=="species") %>% select(state, mgmt_group, comm_name) %>% unique(),
       aes(y=comm_name, x=state)) +
  facet_grid(mgmt_group~., space="free_y", scale="free") +
  geom_raster() +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw() +
  theme(axis.title=element_blank(),
        axis.text.y=element_text(size=4))


# Format NOAA data
################################################################################

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
                          "Spiny shark dogfish"="Spiny dogfish"))

# Alaska species key
spp_key2 <- alaska %>%
  # Unique AK species
  select(comm_name, sci_name, level) %>%
  unique() %>%
  # Assign mgmt group
  left_join(spp_key1 %>% select(comm_name, mgmt_group, complex), by="comm_name") %>%
  # Fill in missing mgmt groups
  mutate(mgmt_group=ifelse(is.na(mgmt_group) & grepl("pacific salmon", tolower(comm_name)), "Salmon", mgmt_group),
         mgmt_group=ifelse(is.na(mgmt_group) & grepl("crab|crustaceans", tolower(comm_name)), "Crabs", mgmt_group),
         mgmt_group=ifelse(is.na(mgmt_group) & grepl("clam|scallop|conch|abalone", tolower(comm_name)), "Shellfish", mgmt_group),
         mgmt_group=ifelse(is.na(mgmt_group) & grepl("shrimp", tolower(comm_name)), "Shrimp", mgmt_group),
         mgmt_group=ifelse(is.na(mgmt_group) & grepl("yellowfin sole|greenland halibut|righteye flounders|flatfishes|rockfishes|soles|shark|skate|sculpin|grenadier|octopus|squid", tolower(comm_name)), "Groundfish", mgmt_group),
         mgmt_group=ifelse(is.na(mgmt_group), "Other", mgmt_group))

# Add mgmt group to Alaska data
alaska1 <- alaska %>%
  # Add management group
  left_join(spp_key2) %>%
  # Reduce to post-1980
  filter(year>=1980)

# Inspect
freeR::complete(alaska1) # MISSING COMPLEX - okay for now


# Merge Alaska and West Coast data
################################################################################

# Merge
data <- bind_rows(pacfin, alaska1) %>%
  # Arrange
  select(state, mgmt_group, complex, comm_name, sci_name, level, year, landings_lb, value_usd, everything()) %>%
  arrange(state, mgmt_group, complex, comm_name, sci_name, level, year)

# Build species key
spp_key <- data %>%
  group_by(mgmt_group, comm_name, level) %>%
  summarize(sci_name=paste(sort(unique(sci_name)), collapse=", "),
            complex=paste(sort(unique(complex)), collapse=", ")) %>%
  ungroup()
write.csv(spp_key, file.path(outdir, "NOAA_PACFIN_comm_landings_spp_key_temp.csv"), row.names = F)
spp_key_use <- readxl::read_excel(file.path(outdir, "NOAA_PACFIN_comm_landings_spp_key.xlsx"))

# Update species info
data_out <- data %>%
  select(-c(complex, sci_name)) %>%
  left_join(spp_key_use %>% select(comm_name, mgmt_group_use)) %>%
  select(state, mgmt_group, mgmt_group_use, comm_name, level, everything()) %>%
  arrange(state, mgmt_group, mgmt_group_use, comm_name, year)

# Export
saveRDS(data_out, file=file.path(outdir, "NOAA_PACFIN_1980_2021_commercial_landings.Rds"))








