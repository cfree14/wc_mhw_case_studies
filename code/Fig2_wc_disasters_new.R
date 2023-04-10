
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/disasters/raw"
outdir <- "data/disasters/raw/v2"
plotdir <- "figures"

# Read data
data_orig <- readxl::read_xlsx(file.path(outdir, "DisastersData_updated.xlsx"), sheet=2)


# Build data
################################################################################

# Non GOA AK regions
ak_regions_exclude <- c("Bering Sea",
                        "Eastern Bering Sea",
                        "Bristol Bay",
                        "Kuskokwim River", # flows into EBS
                        "Norton Sound",
                        "Norton Sound (New)",
                        "Chignik", # West of Kodiak
                        "Chignik salmon fishery", # West of Kodiak
                        "Chignik Management Area", # West of Kodiak
                        "Yukon River", # flows into EBS
                        "Nelson Lagoon", # EBS side of AI
                        "Port Clarence")

# Build data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  rename(disaster_id=disaster_number,
         year=disaster_year,
         sector=comm_rec_tribal,
         status=status_as_of_8_4_2022,
         mgmt_type=state_federal_tribal,
         cause=federally_stated_anth_env_comb,
         region=area_season_affected,) %>%
  # Simplify
  select(disaster_id, state, year, fishery, region, sector, mgmt_type, status, cause) %>%
  # Reduce to states of interest
  filter(state %in% c("California", "Oregon", "Washington", "Alaska")) %>%
  # Format fishery sector
  mutate(sector=ifelse(grepl("Tribal", sector), "Tribal", sector)) %>%
  # Format fishery
  mutate(fishery=stringr::str_to_sentence(fishery),
         fishery=recode(fishery,
                        "Chum"="Chum salmon",
                        "Chinook"="Chinook salmon",
                        "Red urchin"="Red sea urchin",
                        "Groundfish (multispecies)"="Groundfish",
                        "Salmon (sockeye?)"="Salmon",
                        "Salmon (chinook?)"="Salmon",
                        "Coastal salmon"="Salmon")) %>%
  # Add fishery type
  mutate(fishery_type=ifelse(grepl("salmon", tolower(fishery)), "Salmon",
                             ifelse(grepl("crab|urchin", tolower(fishery)), "Inverts", "Other"))) %>%
  # Format state
  mutate(state=recode(state, "Alaska"="Alaska (GOA)"),
         state=factor(state, levels=c("California", "Oregon", "Washington", "Alaska (GOA)"))) %>%
  # Format status
  mutate(status=factor(status, levels=c("Pending", "Denied", "Approved"))) %>%
  # Format cause
  mutate(cause=ifelse(is.na(cause), "Not officially determined", cause),
         cause=recode(cause, "Combination"="Human/environmental"),
         cause=factor(cause, levels=c("Environmental", "Human/environmental", "Not officially determined"))) %>%
  # Reduce to AK GOA
  filter(!region %in% ak_regions_exclude)


# Inspect
table(data$cause)
table(data$sector)
table(data$mgmt_type)
table(data$status)

# Stats
stats <- data %>%
  count(state, fishery_type, fishery, cause, status, year)


# Inspect
ak_regions <- data %>%
  select(state, region) %>%
  unique() %>%
  filter(state=="Alaska (GOA)")


# Plot data
################################################################################

# Rect data
rect_data <- expand.grid(state=levels(data$state),
                         xmin=2013.5,
                         xmax=2016.5)

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   # Gridlines
                   panel.grid.major.x = element_blank(),
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.5, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot
g <- ggplot(stats, aes(x=year, y=fishery, shape=status, color=cause, size=n)) +
  facet_grid(fishery_type~state, space="free_y", scales="free_y") +
  # Plot heatwave
  geom_rect(data=rect_data, mapping=aes(xmin=xmin, xmax=xmax), ymin=0, ymax=10, fill="grey90", inherit.aes = F) +
  # geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=10, fill="grey90", inherit.aes = F) +
  # Plot disasters
  geom_point() +
  # Labels
  labs(x="Year", y="") +
  scale_x_continuous(breaks=seq(1995, 2020, 5)) +
  # Legends
  scale_shape_manual(name="Status", values=c(3, 4, 16)) +
  scale_size_continuous(name="# of impacted fisheries", breaks=c(1,5,10)) +
  scale_color_manual(name="Cause", values=c("red", "blue", "grey30")) +
  guides(shape = guide_legend(order = 1), color = guide_legend(order = 2), size = guide_legend(order = 3)) +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig2_wc_disasters_new.png"),
       width=6.5, height=3, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "Fig2_wc_disasters_new.pdf"),
       width=6.5, height=3, units="in", dpi=600)



# # Plot
# g <- ggplot(data, aes(x=year, y=fishery, color=cause, shape=status)) +
#   facet_grid(fishery_type~state, space="free_y", scales="free_y") +
#   # Plot heatwave
#   geom_rect(data=rect_data, mapping=aes(xmin=xmin, xmax=xmax), ymin=0, ymax=10, fill="grey90", inherit.aes = F) +
#   # geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=10, fill="grey90", inherit.aes = F) +
#   # Plot disasters
#   geom_point() +
#   # Labels
#   labs(x="Year", y="") +
#   scale_x_continuous(breaks=seq(1995, 2020, 5)) +
#   # Legends
#   scale_shape_manual(name="Status", values=c(3, 4, 16)) +
#   scale_color_manual(name="Cause", values=c("red", "blue", "grey30")) +
#   # Theme
#   theme_bw() + my_theme
# g
