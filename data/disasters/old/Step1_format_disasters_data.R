
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/disasters/raw"
outdir <- "data/disasters/processed"
plotdir <- "data/disasters/figures"

# Read data
data_orig <- readxl::read_excel(file.path(indir, "Bellquist_etal_TableS4.xlsx"),  na="N/A")


# Format data
################################################################################

# Format data
data <- data_orig %>% 
  # Column names
  janitor::clean_names("snake") %>% 
  rename(disaster_id=disaster_number, 
         fisheries=fishery_ies,
         mgmt_zone=management_zone, 
         year=determination_year, 
         cause=disaster_cause, 
         appropriation_usd=appropriation_2019_usd,
         revenue_change_usd=net_revenue_change_2019_usd, 
         revenue_change_confidence=confidence_in_revenue_data,
         revenue_change_notes=revenue_data_source_s_notes) %>% 
  # Format cause
  mutate(cause=recode_factor(cause, 
                             "Anthro"="Anthropogenic", 
                             "Envr"="Environmental",
                             "Combo"="Combination"),
         revenue_change_confidence=ifelse(is.na(revenue_change_confidence), 
                                          "Low", revenue_change_confidence),
         revenue_change_confidence=factor(revenue_change_confidence,
                                          levels=c("Low", "Medium", "High"))) %>% 
  # Assign groups
  mutate(fisheries=recode(fisheries, "Coho and Chinook"="Coho and Chinook Salmon"), 
         fishery_type=ifelse(grepl("Salmon", fisheries), "Salmon", NA),
         fishery_type=ifelse(fisheries %in% c("Groundfish (Multi spp)", "Pacific Cod"), 
                             "Grounfish", fishery_type),
         fishery_type=ifelse(grepl("Crab", fisheries), "Crab", fishery_type),
         fishery_type=ifelse(fisheries=="Multi spp", "Multi-species", fishery_type),
         fishery_type=ifelse(grepl("Shrimp", fisheries), "Shrimp", fishery_type),
         fishery_type=ifelse(fisheries%in%c("Pacific Sardine", "Red Sea Urchin",
                                            "Oyster", "American Lobster", "Shellfish (Multi spp)"), "Other", fishery_type)) %>% 
  # Arrange
  select(disaster_id, fishery_type, fisheries, everything()) %>% 
  arrange(fishery_type)

# Inspect
table(data$mgmt_zone)
table(data$cause)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outdir, "US_federal_fishery_disasters_summary.Rds"))


# Plot data
################################################################################

# West Coast data
wc_data <- data %>% 
  filter(mgmt_zone %in% c("Alaska", "West Coast"))

# Plot data
g <- ggplot(wc_data, aes(x=year, y=fisheries, fill=revenue_change_usd/1e6)) +
  facet_grid(fishery_type~., space="free", scales = "free_y") +
  geom_tile(color="grey30") +
  # Add points for type and confidence
  geom_point(mapping=aes(x=year, y=fisheries, 
                          shape=cause, alpha=revenue_change_confidence)) +
  # Labels
  labs(x="", y="", title="West Coast Federal fisheries disasters") +
  # Axes
  scale_x_continuous(breaks=seq(1995, 2020, 5)) +
  # Legends
  scale_fill_gradient2(name="Change in revenue\n(2019 $US millions)") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  scale_shape_discrete(name="Disaster cause") +
  scale_alpha_discrete(name="Confidence in revenue change") +
  # Theme
  theme_bw() +
  theme(legend.position = "right",
        axis.text=element_text(size=8),
        axis.title=element_blank(),
        strip.text = element_text(size=8),
        legend.text=element_text(size=6),
        legend.title=element_text(size=8),
        plot.title=element_text(size=12),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "wc_federal_fisheries_disasters.png"), 
       width=6.5, height=4, units="in", dpi=600)

