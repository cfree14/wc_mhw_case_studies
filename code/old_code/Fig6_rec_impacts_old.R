
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
data_orig <- wcfish::noaa


# Build data
################################################################################

# Build data
data <- data_orig %>%
  # WC rec fisheries
  filter(fishery=="Recreational" & state %in% c("California", "Oregon", "Washington", "Alaska")) %>%
  # Years of interest
  filter(year%in%2011:2019) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Format species
  mutate(comm_name=wcfish::convert_names(comm_name_orig, to="regular") %>% stringr::str_to_sentence()) %>%
  # Simplify
  select(state, comm_name, period, year, landings_lb) %>%
  # Arrange
  arrange(state, comm_name, year) %>%
  # Summarize by state, species, period
  group_by(state, comm_name, period) %>%
  summarize(landings_lb=mean(landings_lb, na.rm=T)) %>%
  ungroup() %>%
  # Expand so that every period is represented
  complete(state, comm_name, period, fill=list(landings_lb=NA)) %>%
  # Calculate percent difference from during
  group_by(state, comm_name) %>%
  mutate(landings_lb_pre=landings_lb[period=="Before"],
         landings_pdiff=(landings_lb - landings_lb_pre) / landings_lb_pre * 100,
         landings_pdiff_cap=pmin(landings_pdiff, 200)) %>%
  ungroup() %>%
  # Remove state-species without before/during
  group_by(state, comm_name) %>%
  mutate(use_yn=ifelse( !is.na(landings_lb[period=="Before"]) & !is.na(landings_lb[period=="During"]), "yes", "no")) %>%
  ungroup() %>%
  # Filter
  filter(use_yn=="yes") %>%
  # Factor period
  mutate(period=factor(period, levels=c("Before", "During", "After")))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data, aes(x=period, y=comm_name, fill=landings_pdiff_cap)) +
  facet_wrap(~state) +
  geom_tile() +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_fill_gradient2(name="% difference", midpoint = 0, mid="white", high="navy", low="darkred", na.value="grey30") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme
g





