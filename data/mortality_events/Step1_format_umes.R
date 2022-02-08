
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/mortality_events/raw"
outdir <- "data/mortality_events/processed"
plotdir <- "data/mortality_events"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "NOAA_umes.xlsx"))

# Location key
# Region and state aren't right
loc_key <- readxl::read_excel(file.path(indir, "location_key.xlsx"))


# Format data
################################################################################

# Read data
data <- data_orig %>%
  # Format id
  mutate(number=number %>% stringr::str_trim() %>% as.numeric()) %>%
  # Add year
  mutate(year1=substr(year, 1, 4) %>% as.numeric()) %>%
  # Add region
  mutate(location=ifelse(name=="Mid-Atlantic Small Cetacean", "Atlantic Ocean, New Jersey, Maryland, North Carolina, South Carolina, Georgia", location)) %>%
  left_join(loc_key %>% select(ocean, region, location), by="location") %>%
  # Format type
  mutate(cause_orig=stringr::str_to_sentence(cause),
         cause=recode(cause_orig,
                      "Human interaction (vessel strike/rope entanglement)"="Human interaction",
                      "Infectious disease secondary to ecological factors (e.g. change in forage)"="Infectious disease/ecological factors",
                      "Suspect human interaction (entanglement)/infectious disease"="Human interaction/infectious disease",
                      "Suspect human interaction (vessel strike)"="Human interaction",
                      "Undetermined (suspect infectious disease)"="Infectious disease",
                      "Undetermined; secondary ecological factors"="Ecological factors")) %>%
  # Format species
  mutate(species=gsub("[:alnum:]", "", species),
         species=recode(species,
                       # "Bearded, ribbon, ringed, spotted seal, and walrus"="Bearded seal, Ribbon seal, Ringed seal, Spotted seal, Walrus",
                       # "Bearded, ringed, and spotted seal"="Bearded seal, Ringed seal, Spotted seal",
                       # "Bottlenose dolphin, Atlantic spotted dolphin, spinner dolphin, melon-headed whale, sperm whale, pygmy sperm whale, dwarf sperm whale, pantropical spotted dolphin, Gulf of Mexico Bryde’s whale, minke whale, short-finned pilot whale, Risso’s dolphin, Blainville’s beaked whale, Clymene dolphin, rough-toothed dolphin"="Bottlenose dolphin, Atlantic spotted dolphin, Spinner dolphin, Melon-headed whale, Sperm whale, Pygmy sperm whale, Dwarf sperm whale, Pantropical spotted dolphin, Gulf of Mexico Bryde’s whale, Minke whale, Short-finned pilot whale, Risso’s dolphin, Blainville’s beaked whale, Clymene dolphin, rough-toothed dolphin",
                       # "Bottlenose dolphin, harbor porpoise, Atlantic white-sided dolphin, short-beaked common dolphin, Risso’s dolphin, short- and long-finned pilot whale, minke whale"="Bottlenose dolphin, Harbor porpoise, Atlantic white-sided dolphin, Short-beaked common dolphin, Risso’s dolphin, Short-finned pilot whale, Long-finned pilot whale, Minke whale",
                       # "Bottlenose dolphin, manatee"="Bottlenose dolphin, Manatee",
                       # "Bottlenose dolphin, short-beaked common dolphin, Risso's dolphin, pygmy sperm whale, short-finned pilot whale, dwarf sperm whale, minke whale,  Atlantic white-sided dolphin, Atlantic spotted dolphin, pantropical spotted dolphin, Clymene dolphin, Sowerby’s beaked whale, Gervais’ beaked whale, fin whale"="Bottlenose dolphin, Short-beaked common dolphin, Risso's dolphin, Pygmy sperm whale, Short-finned pilot whale, Dwarf sperm whale, minke whale,  Atlantic white-sided dolphin, Atlantic spotted dolphin, Pantropical spotted dolphin, Clymene dolphin, Sowerby’s beaked whale, Gervais’ beaked whale, Fin whale",
                       # "California and Steller sea lion, harbor seal"="California sea lion, Steller sea lion, Harbor seal",
                       # "California sea lion, short-beaked common dolphin, sea otter"="California sea lion, Short-beaked common dolphin, Sea otter",
                       # "Fin and humpback whale"="Fin whale, Humpback whale",
                       # "Gray and harbor seal"="Gray seal, Harbor seal",
                       # "Gray Whale"="Gray whale",
                       # "Gray, harbor, harp, and hooded seal"="Gray seal, Harbor seal, Harp seal, Hooded seal",
                       # "Guadalupe fur seal and northern fur seal"="Guadalupe fur seal, Northern fur seal",
                       # "Harbor and gray seal"="Harbor seal, Gray seal",
                       # "Humpback whale, fin whale, minke whale, long-finned pilot whale"="Humpback whale, Fin whale, Minke whale, Long-finned pilot whale",
                       # "Minke whale, harbor seal"="Minke whale, Harbor seal",
                       # "Minke, humpback, and fin, and sperm whale"="Minke whale, Humpback whale, Fin whale, Sperm whale",
                       "Short-beaked common dolphin, harbor porpoise, minke whale, sperm whale"="Short-beaked common dolphin, Harbor porpoise, Minke whale, Sperm whale")) %>%
  # Arrange
  select(type, number, year, year1, ocean, region, location, name, species, everything())

# Inspect data
# str(data)
# table(data$cause_orig)
# table(data$cause)
sort(unique(data$species))
# sort(unique(data$location))


# Expand data
################################################################################


# Plot data
################################################################################

# Stats
stats1 <- count(data, year1, cause)
stats2 <- count(data, year1, ocean)

# Plot data
g1 <- ggplot(stats1, aes(x=year1, y=n, fill=cause)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Number of UMEs") +
  # Legend
  scale_fill_discrete(name="Cause") +
  # Theme
  theme_bw()
g1

# Plot data
g2 <- ggplot(stats2, aes(x=year1, y=n, fill=ocean)) +
  geom_bar(stat="identity") +
  # Labels
  labs(x="", y="Number of UMEs") +
  # Legend
  scale_fill_discrete(name="Cause") +
  # Theme
  theme_bw()
g2


# Plot data
################################################################################

# Subset
sdata <- data %>%
  filter(ocean=="Pacific")

# West Coast
g3 <- ggplot(sdata, aes(x=year1, y=species, color=cause)) +
  # Plot MHW years
  geom_rect(xmin=2012.5, xmax=2017.5, ymin=1, ymax=20, fill="grey90", inherit.aes = F) +
  # Plot UMEs
  geom_point() +
  # Labels
  labs(x="", y="") +
  # Theme
  theme_bw()
g3





