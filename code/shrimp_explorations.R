
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"


# Build data
################################################################################

# Read PACFIN data
pacfin_orig <- wcfish::pacfin_all6

# Format data
data <- pacfin_orig %>%
  # Reduce to shrimp/prawns
  filter(grepl("prawn|shrimp", tolower(comm_name))) %>%
  # Summarize
  group_by(state, comm_name, sci_name, year) %>%
  summarize(landings_mt=sum(landings_mt, na.rm = T),
            value_usd=sum(revenues_usd, na.rm=T)) %>%
  # Factor states
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()))

# Format data to focus on
data_use <- data %>%
  # Reduce to species of interest
  filter(comm_name %in% c("Pacific pink shrimp", "Ridgeback prawn", "Spotted prawn")) %>%
  # Make species label
  mutate(spp_label=paste0(comm_name, "\n(", sci_name, ")"))



# Plot data
################################################################################

# Plot all
g <- ggplot(data, mapping=aes(x=year, y=value_usd/1e6, fill=state)) +
  facet_wrap(~comm_name, scales = "free_y") +
  # Mark MHW
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  # Bars
  geom_bar(stat="identity", col="grey30", lwd=0.3) +
  # Labels
  labs(x="Year", y="Value (USD millions)") +
  # Theme
  theme_bw()
g

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=9),
                   plot.tag = element_text(size=9),
                   plot.tag.position = c(0.01, 0.98),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key.size = unit(0.3, "cm"),
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot specific
g <- ggplot(data_use, mapping=aes(x=year, y=value_usd/1e6, fill=state)) +
  facet_wrap(~spp_label, scales = "free_y") +
  # Mark MHW
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  # Bars
  geom_bar(stat="identity", col="grey30", lwd=0.3) +
  # Labels
  labs(x="Year", y="Value (USD millions)") +
  scale_fill_discrete(name="State") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.1, 0.8))
g

# Export
ggsave(g, filename=file.path(plotdir, "shrimp_figure.png"),
       width=6.5, height=2.5, units="in", dpi=600)









