
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
data_orig <- readxl::read_excel(file.path(indir, "Pac Fisheries Data 2020-2000 for Sean Anderson.xlsx"), sheet="Combined Data")

# Read species key
spp_key <- readxl::read_excel(file.path(indir, "dfo_pacific_species_key.xlsx"))
freeR::which_duplicated(spp_key$comm_name)
anyDuplicated(spp_key$comm_name)


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(fishery=fishery_nme,
         spp_group=species_group1,
         comm_name=common_nme,
         landings_kg=sum_of_rd_kgs,
         value_2020=sum_of_value_adjusted_2020, ) %>%
  # Format fishery
  mutate(fishery=stringr::str_to_sentence(fishery)) %>%
  # Format species group
  mutate(spp_group=stringr::str_to_sentence(spp_group)) %>%
  mutate(spp_group=recode(spp_group,
                          "Chinook"="Chinook salmon",
                          "Chum"="Chum salmon",
                          "Coho"="Coho salmon",
                          "Crabs"="Crab",
                          "Pink"="Pink salmon",
                          "Sea urchin, red"="Red sea urchin",
                          "Sockeye"="Sockeye salmon",
                          "(Blank)"="Unspecified")) %>%
  # Format common name
  mutate(comm_name=stringr::str_to_sentence(comm_name)) %>%
  mutate(comm_name=ifelse(is.na(comm_name), "Unknown species", comm_name)) %>%
  mutate(comm_name=recode(comm_name,
                          "(Blank)"="Unknown species",
                          'Bigeye thresher'='Bigeye thresher shark',
                          "Black skate (sandpaper)"="Sandpaper skate",
                          "Black rockfish, black bass"="Black rockfish",
                          'Box crabs'='Box crab',
                          'C-o sole'='C-O sole',
                          'C-o sole(popeyes)'='C-O sole',
                          'Chilipepper'='Chilipepper rockfish',
                          "Chinook"="Chinook salmon",
                          "Chum"="Chum salmon",
                          "Coho"="Coho salmon",
                          "Pink"="Pink salmon",
                          "Salmon trout"="Steelhead",
                          "Other rockfish"="Rockfishes",
                          "Sockeye"="Sockeye salmon",
                          "Shortspine thornyhead rockfish, idiot"="Shortspine thornyhead",
                          "Sixgill shark"="Bluntnose sixgill shark",
                          'Tanner crabs'='Tanner crab',
                          "Tope shark"="Soupfin shark",
                          "Wrymouths"="Wrymouth",
                          "Flatfishes//unspecified flounder"="Unspecified flounder")) %>%
  # Add scientific name
  left_join(spp_key, by="comm_name") %>%
  # Arrange
  select(year:comm_name, sci_name, type, everything())

# Inspect
table(data$fishery)
table(data$spp_group)
sort(unique(data$comm_name))


# If building key for first time
if(F){

  # Species key
  spp_key <- data %>%
    select(comm_name) %>%
    unique() %>%
    mutate(sci_name=harmonize_names(comm_name, "comm", "sci"))

  # Export key
  write.csv(spp_key, file=file.path(indir, "dfo_pacific_species_key.csv"), row.names=F)

}

# Species key
spp_key2 <- data %>%
  select(comm_name, sci_name) %>%
  unique()

# Check names
freeR::check_names(spp_key$sci_name)
freeR::which_duplicated(spp_key$sci_name)
freeR::which_duplicated(spp_key$comm_name)

# Export data
saveRDS(data, file=file.path(outdir, "2000_2020_dfo_pacific_landings.Rds"))
