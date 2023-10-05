
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
datadir <- "data/landings"
plotdir <- "figures"

# Read data
data_orig <- readRDS(data, file=file.path(datadir, "WC_1980_2021_commercial_landings.Rds"))


# Build data
################################################################################

# Species key
spp_key <- data_orig %>%
  select(mgmt_group, comm_name, level) %>%
  unique()

# Build data
data <- data_orig %>%
  # Filter to data of interest
  filter(state!="At-Sea" & level=="species" & year %in% 2011:2019 & !is.na(value_usd)) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize
  group_by(state, comm_name, period) %>%
  summarize(value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Add missing periods
  complete(state, comm_name, period, fill=list(value_usd=NA)) %>%
  # Remove state-species without before data
  group_by(state, comm_name) %>%
  mutate(value_usd_pre=value_usd[period=="Before"],
         use_yn=ifelse(is.na(value_usd_pre), "no", "yes")) %>%
  ungroup() %>%
  filter(use_yn=="yes") %>%
  select(-use_yn) %>%
  # Compute percents
  group_by(state, comm_name) %>%
  mutate(value_usd_pdiff=(value_usd - value_usd_pre) / value_usd_pre * 100,
         value_usd_pdiff_cap=pmin(value_usd_pdiff, 200)) %>%
  ungroup() %>%
  # Order state
  mutate(state_abbrev=recode(state,
                             "California"="CA",
                             "Oregon"="OR",
                             "Washington"="WA",
                             "British Columbia"="BC",
                             "Alaska"="AK"),
         state_abbrev=factor(state_abbrev, levels=c("CA", "OR", "WA", "BC", "AK"))) %>%
  # Add management group
  left_join(spp_key, by="comm_name") %>%
  # Format management group
  mutate(mgmt_group=recode(mgmt_group,
                               "Crabs"="Crustaceans",
                               "Shrimp"="Crustaceans",
                               "Coastal pelagic species"="Coastal\npelagics",
                               "Highly migratory species"="Highly migratory\nspecies",
                               "Miscellaneous groundfish"="Other\ngroundfish")) %>%
  # Remove species with zero catch before heatwave (impossible to calculate percent change)
  filter(value_usd_pre>0)


# Order data
################################################################################

# Build species order
spp_order_key <- data %>%
  # Focus on during
  filter(period=="During") %>%
  # Summarize
  group_by(mgmt_group, comm_name) %>%
  summarize(pdiff_avg=mean(value_usd_pdiff, na.rm=T)) %>%
  ungroup() %>%
  # Order
  arrange(mgmt_group, pdiff_avg)

# Order data
data_ordered <- data %>%
  mutate(comm_name=factor(comm_name, levels=spp_order_key$comm_name))


# Split data
################################################################################

# Groundfish mgmt groups
table(data$mgmt_group)
groundfish_groups <- c("Rockfish", "Flatfish", "Roundfish", "Other\ngroundfish")

# Divide into GF and non-GF
data1 <- data_ordered %>%
  filter(mgmt_group %in% groundfish_groups)
data2 <- data_ordered %>%
  filter(!mgmt_group %in% groundfish_groups)

# Plot data - long
################################################################################

# Plot data
g <- ggplot(data, aes(x=period, y=comm_name, fill=value_usd_pdiff_cap)) +
  facet_grid(mgmt_group~state_abbrev, space="free_y", scale="free_y") +
  geom_tile() +
  # Labels
  labs(x="Heatwave period", y="") +
  # Legend
  scale_fill_gradient2(name="% difference\nfrom pre-MHW\nrevenues",
                       midpoint = 0, mid="white", high="navy", low="darkred", na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g


# Plot data - wide
################################################################################

# Theme
my_theme <- theme(axis.text=element_text(size=6),
                    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                    axis.title=element_text(size=7),
                    axis.title.y=element_blank(),
                    legend.text=element_text(size=5),
                    legend.title=element_text(size=7),
                    strip.text=element_text(size=7),
                    plot.title=element_text(size=8),
                    # Gridlines
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.background = element_blank(),
                    axis.line = element_line(colour = "black"),
                    # Legend
                    legend.background = element_rect(fill=alpha('blue', 0)))

# Plot groundfish
g1 <- ggplot(data1, aes(x=period, y=comm_name, fill=value_usd_pdiff_cap)) +
  facet_grid(mgmt_group~state_abbrev, space="free_y", scale="free_y", drop=F) +
  geom_tile() +
  # Before points
  geom_point(data=data1 %>% filter(period=="Before"),
             mapping=aes(x=period, y=comm_name, size=value_usd/1e6)) +
  # Labels
  labs(x="Heatwave period", y="", title="Groundfish") +
  # Legend
  scale_size_continuous(name="Mean annual\npre-MHW revenues\n(USD millions)") +
  scale_fill_gradient2(name="% difference\nfrom pre-MHW\nrevenues",
                       lim=c(-100, 200),
                       breaks=seq(-100, 200, 100),
                       labels=c("-100", "0", "100", ">200"),
                       midpoint = 0, mid="white", high="navy", low="darkred", na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Plot non-groundfish
g2 <- ggplot(data2, aes(x=period, y=comm_name, fill=value_usd_pdiff_cap)) +
  facet_grid(mgmt_group~state_abbrev, space="free_y", scale="free_y", drop=F) +
  geom_tile() +
  # Before points
  geom_point(data=data2 %>% filter(period=="Before"),
             mapping=aes(x=period, y=comm_name, size=value_usd/1e6)) +
  # Labels
  labs(x="Heatwave period", y="", title="Non-groundfish") +
  # Legend
  scale_size_continuous(name="Mean annual\npre-MHW revenues\n(USD millions)") +
  scale_fill_gradient2(name="% difference\nfrom pre-MHW\nrevenues",
                       lim=c(-100, 200),
                       breaks=seq(-100, 200, 100),
                       labels=c("-100", "0", "100", ">200"),
                       midpoint = 0, mid="white", high="navy", low="darkred", na.value="grey90") +
  guides(size=guide_legend(order=1),
         fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2)) +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.43, 0.57))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_comm_impacts_specific_updated.png"),
       width=8, height=8, units="in", dpi=600)






