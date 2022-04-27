
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
outdir <- "data/landings"

# Read CDFW data (2000-2019)
cpfv_ca_orig <- wcfish::cdfw_cpfv

# Read RECFIN data (WA: 2004-2021, OR: 2001-2021, CA: 2005-2021)
recfin_orig <- wcfish::recfin_cte2

# Read RECFIN salmon data
salmon_ca_orig <- wcfish::recfin_cte7 # 1976-2020
salmon_orwa_orig <- wcfish::recfin_cte5 # OR: 2001-2021, WA: 2004-2021

# Read ADFG data
adfg_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_landings_data/data/adfg/processed/ADFG_1996_2000_statewide_landings_discards.Rds") # 1996-2020


# Format data
################################################################################

# Step 1. Format and merge RECFIN salmon datasets
# Step 2. Format primary RECFIN dataset and remove CA CPFV landings
# Step 3. Format CA CPFV dataset and remove salmon landings
# Step 4. Format ADFG dataset
# Step 5. Merge RECFIN salmon, RECFIN primary (no CA CPFV), CA CPFV (no salmon), ADFG


# Step 1. Format salmon data
##########################################

# Build CA salmon data
recfin_salmon_ca <- salmon_ca_orig %>%
  # Summarize
  group_by(state, mode, comm_name, sci_name, level, year) %>%
  summarize(retained_n=sum(retained_n)) %>%
  ungroup() %>%
  # Add source
  mutate(source="RECFIN CTE007",
         taxa_catg="Salmon")

# Build OR/WA salmon data
recfin_salmon_orwa <- salmon_orwa_orig %>%
  # Summarize
  group_by(state, mode, comm_name, sci_name, level, year) %>%
  summarize(retained_n=sum(retained_n)) %>%
  ungroup() %>%
  # Add source
  mutate(source="RECFIN CTE005",
         taxa_catg="Salmon")

# Build salmon
recfin_salmon <- bind_rows(recfin_salmon_ca, recfin_salmon_orwa) %>%
  select(source, state, mode, taxa_catg, comm_name, sci_name, level, year, retained_n) %>%
  arrange(mode, comm_name, year)

# Inspect
freeR::complete(recfin_salmon)


# Step 2. Format RECFIN data
##########################################

# Build RECFIN data
recfin_other <- recfin_orig %>%
  # Reduce to retained
  filter(status=="Retained") %>%
  # Reduce to USA waters (not Canada or Mexico)
  filter(!water_area %in% c("Canada", "Mexico")) %>%
  # Summarize
  group_by(state, mode, taxa_catg, comm_name, sci_name, level, year) %>%
  summarize(retained_n=sum(catch_n)) %>%
  ungroup() %>%
  # Add source
  mutate(source="RECFIN CTE002") %>%
  # Format
  mutate(level=recode(level, "general"="group"),
         sci_name=ifelse(comm_name=="Unidentified (sharks)", "Selachimorpha spp.", sci_name)) %>%
  # Remove CA CPFV data
  filter( !(state=="California" & mode=="Party/Charter Boats") )

# Inspect
table(recfin_other$state)
table(recfin_other$mode)


# Step 3. Format CDFW data
##########################################

# Build CDFW data --- DOES NOT HAVE MGMT GROUP
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
  select(-c(waters, comm_name_orig)) %>%
  select(source, state, mode, comm_name, sci_name, level, year, retained_n, everything()) %>%
  # Format some common names to match RECFIN
  mutate(comm_name=recode(comm_name,
                          "White croaker (kingfish)"="White croaker",
                          "Yellowtail (amberjack)"="Yellowtail",
                          "Shark"="Unidentified (sharks)",
                          "Pacific mackerel"="Pacific (chub) mackerel",
                          "Pacific jack mackerel"="Jack mackerel",
                          "Common dolphinfish"="Dolphinfish",
                          "California barracuda"="Pacific barracuda")) %>%
  # Remove salmon
  filter(!grepl("salmon", tolower(comm_name))) %>%
  # Add taxa group
  left_join(recfin_other %>% select(comm_name, taxa_catg) %>% unique(), by="comm_name") %>%
  mutate(taxa_catg=ifelse(comm_name=="Croaker", "Drums", taxa_catg),
         taxa_catg=ifelse(comm_name=="Rockfish", "Rockfish", taxa_catg),
         taxa_catg=ifelse(comm_name=="Sturgeon", "Sturgeons", taxa_catg),
         taxa_catg=ifelse(comm_name %in% c("Flatfish", "Sanddab"), "Flatfish", taxa_catg),
         taxa_catg=ifelse(comm_name %in% c("Skipjack tuna", "Yellowfin tuna"), "Tunas, mackerels, pompanos", taxa_catg),
         taxa_catg=ifelse(comm_name %in% c("Striped marlin", "Wahoo"), "Tunas, mackerels, pompanos", taxa_catg),
         taxa_catg=ifelse(comm_name %in% c("Fish", "Invertebrate"), "Other", taxa_catg))

# Check taxa groups
cdfw %>% filter(is.na(taxa_catg)) %>% pull(comm_name) %>% unique() %>% sort()


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


# Format CA data
################################################################################

# CA CPFV species
ca_spp_cdfw <- sort(unique(sort(unique(cdfw$comm_name))))

# CA RECFIN
spp_recfin <- recfin_orig %>%
  pull(comm_name) %>% unique() %>% sort()

# Which CPFV species aren't in RECFIN?
ca_spp_cdfw_check <- ca_spp_cdfw[!ca_spp_cdfw %in% spp_recfin]
ca_spp_cdfw_check


# Step 4. Format ADFG data
##########################################

# Build ADFG data
adfg <- adfg_orig %>%
  # Reduce to marine
  filter(mgmt_group!="Freshwater") %>%
  # Add a few
  mutate(source="ADFG",
         state="Alaska",
         mode="Unknown") %>%
  # Arrange
  select(-c(catch_n, discards_n, mgmt_group)) %>%
  select(source, state, mode, taxa_catg, comm_name, sci_name, level, year, retained_n, everything())


# Build species key
################################################################################

# Individual keys
salmon_spp <- recfin_salmon %>%
  select(taxa_catg, comm_name, sci_name, level) %>% unique()
recfin_spp <- recfin_other %>%
  select(taxa_catg, comm_name, sci_name, level) %>% unique()
adfg_spp <- adfg_orig %>%
  select(taxa_catg, comm_name, sci_name, level) %>% unique()
cdfw_spp <- cdfw %>%
  select(taxa_catg, comm_name, sci_name, level) %>% unique()

# Merge
spp_key <- bind_rows(salmon_spp, recfin_spp, adfg_spp, cdfw_spp) %>%
  unique()

# Check for duplicates
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name) #  "Oncorhynchus mykiss" "Oncorhynchus nerka" = fine


# Step 6. Build final dataset
################################################################################

# Build merged data
data <- bind_rows(recfin_other, recfin_salmon, cdfw, adfg) %>%
  # Arrange
  select(source, state, mode, taxa_catg, comm_name, sci_name, level, year, retained_n, everything()) %>%
  arrange(source, state, mode, taxa_catg, comm_name, year) %>%
  # Remove incomplete CA data
  filter(! (state=="California" & (year>2019 | year < 2005)))

# Inspect
str(data)
freeR::complete(data)

# Inspect
table(data$source)
table(data$state)
table(data$mode)

# Export data
saveRDS(data, file=file.path(outdir, "RECFIN_annual_rec_landings_by_state.Rds"))







