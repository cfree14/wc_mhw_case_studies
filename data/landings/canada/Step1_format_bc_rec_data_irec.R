
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


# Read species key
spp_key <- readxl::read_excel(file.path(outdir, "DFO_species_key.xlsx"))
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name)
freeR::check_names(spp_key$sci_name) # Beringraja binoculata, Beringraja rhina, Gadus chalcogrammus, Tetronarce californica are fine


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(region=logistical_area,
         species_group=item_group,
         comm_name=item) %>%
  # Format adipose modifier
  mutate(adipose_modifier=stringr::str_to_sentence(adipose_modifier)) %>%
  # Format item group
  mutate(species_group=stringr::str_to_sentence(species_group),
         species_group=recode(species_group,
                           "Othshellfish"="Other shellfish",
                           "Prawncrab"="Crabs and prawns")) %>%
  # Format item
  mutate(comm_name=stringr::str_to_sentence(comm_name) %>% stringr::str_squish() %>% stringr::str_trim(),
         comm_name=ifelse(comm_name %in% c("Chinook", "Chum", "Coho", "Pink", "Sockeye"),
                          paste(comm_name, "salmon"), comm_name),
         comm_name=ifelse(species_group=="Rockfish" & !grepl("rockfish", comm_name),
                          paste(comm_name, "rockfish"), comm_name),
         comm_name=recode(comm_name,
                          "Clams (butter)"="Butter clam",
                          "Clams (littleneck)"="Littleneck clam",
                          "Clams (manila)"="Manila clam",
                          "Geoduck"="Geoduck clam",
                          "Shore crab"="Asian shore crab",
                          "Dogfish"="Spiny dogfish",
                          "Giant barnacle"="Giant acorn barnacle",
                          "Humpback shrimp"="Coonstripe shrimp",
                          "Scallop (pink and spiny)"="Pink and spiny scallops",
                          "Herring"="Pacific herring",
                          "Chub mackerel"="Pacific (chub) mackerel")) %>%
  # Add scientific name and level
  left_join(spp_key %>% select(taxa_catg, comm_name, sci_name, level), by="comm_name") %>%
  # Arrange
  select(year, month, region, area, method,
         species_group, taxa_catg, comm_name, sci_name, level,
         disposition, retainable, adipose_modifier,
         estimate, variance,
         everything())

# Inspect
str(data)
freeR::complete(data)

# Species key
spp_key_check <- data %>%
  select(species_group, taxa_catg, comm_name, sci_name, level) %>%
  unique() %>%
  arrange(species_group, taxa_catg, comm_name)

# Inspect
table(data$year)
table(data$month)
table(data$region)
table(data$area)
table(data$method)
table(data$adipose_modifier)
freeR::uniq(data$comm_name)
table(data$disposition)
table(data$retainable)

# Split catch and effort data
################################################################################

# Catch data
data1 <- data %>%
  filter(species_group!="Effort")

# Inspect
str(data1)
freeR::complete(data1)

# Effort data
data2 <- data %>%
  # Reduce to effort
  filter(species_group=="Effort") %>%
  # Get rid of useless columns
  select(-c(species_group, sci_name, level, disposition, retainable, adipose_modifier)) %>%
  # Rename
  rename(effort_metric=comm_name)

# Inspect
str(data2)
freeR::complete(data2)


# Plot data
################################################################################

# Summarise
stats <- data1 %>%
  filter(disposition=="Kept") %>%
  group_by(year, species_group) %>%
  summarize(nfish=sum(estimate)) %>%
  ungroup()

# Plot
ggplot(stats, aes(x=year, y=nfish/1e6, fill=species_group)) +
  geom_bar(stat="identity") +
  labs(x="Year", y="Millions of fish") +
  scale_x_continuous(breaks=2010:2022) +
  theme_bw()


# Export data
################################################################################

# Export
saveRDS(data1, file=file.path(outdir, "DFO_2012_2022_irec_rec_catch_estimates.Rds"))
saveRDS(data2, file=file.path(outdir, "DFO_2012_2022_irec_rec_effort_estimates.Rds"))
