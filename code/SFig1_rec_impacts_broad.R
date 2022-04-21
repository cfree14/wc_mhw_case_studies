
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
data_orig <- readRDS("data/landings/RECFIN_annual_rec_landings_by_state.Rds")

# RECFIN species key
spp_key <- wcfish::recfin_species %>%
  mutate(category=ifelse(is.na(category), "Other", category))



# Build data
################################################################################

# Build data
data <- data_orig %>%
  # Reduce to years of interest
  filter(year %in% 2005:2020) %>%
  # Summarize
  group_by(state, year) %>%
  summarize(retained_n=sum(retained_n)) %>%
  ungroup()

# Build period averages
data_avgs <- data %>%
  # Reduce to years of interest
  filter(year %in% 2011:2019) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize
  group_by(state, period) %>%
  summarize(retained_n_avg=mean(retained_n)) %>%
  ungroup() %>%
  # Mark years
  mutate(yr1=recode(period,
                    "Before"=2011, "During"=2014, "After"=2017) %>% as.numeric(),
         yr2=recode(period,
                    "Before"=2013, "During"=2016, "After"=2019) %>% as.numeric())


# Build state-period-group averages
data_group <- data_orig %>%
  # Reduce to years of interest
  filter(year %in% 2011:2019) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Add category
  rename(comm_name_orig=comm_name) %>%
  left_join(spp_key) %>%
  # Summarize
  group_by(state, category, period) %>%
  summarize(retained_n=sum(retained_n)) %>%
  ungroup() %>%
  # Calculate percent difference
  group_by(state, category) %>%
  mutate(retained_n_pre=retained_n[period=="Before"],
         retained_n_pdiff=(retained_n - retained_n_pre) / retained_n_pre * 100,
         retained_n_pdiff_cap=pmin(retained_n_pdiff, 200)) %>%
  ungroup() %>%
  # Recode state
  mutate(state=recode(state,
                      "California"="CA", "Oregon"="OR", "Washington"="WA"))




# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.tag=element_text(size=9),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot totals
g1 <- ggplot(data, aes(x=year, y=retained_n/1e6, color=state)) +
  # Plot heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90", inherit.aes = F) +
  annotate(geom="text", label="MHW", x=2015, y=11.5, size=2.1) +
  # Plat landings
  geom_line() +
  # Plot period averages
  geom_segment(data=data_avgs, mapping=aes(x=yr1,
                                           xend=yr2,
                                           y=retained_n_avg/1e6,
                                           yend=retained_n_avg/1e6,
                                           color=state), linetype="dotted", lwd=0.5) +
   # Labels
  labs(x="Year", y="Landings\n(millions of fish)", tag="A") +
  scale_color_discrete(name="") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size=unit(0.2, "cm"),
        legend.position = c(0.2, 0.2))
g1

# Plot data
g2 <- ggplot(data_group, aes(x=period, y=category, size=retained_n/1e6, fill=retained_n_pdiff_cap)) +
  facet_wrap(~state) +
  geom_point(pch=21) +
  # Labels
  labs(x="Heatwave period", y="", tag="B") +
  # Size legend
  scale_size_continuous(name="Mean annual landings\n(millions of fish)") +
  # Color legend
  scale_fill_gradient2(name="% difference\nfrom pre-MHW\nlandings", midpoint = 0, mid="white", high="navy", low="darkred", na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size=unit(0.4, "cm"),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.4, 0.6))
g

# Export
ggsave(g, filename=file.path(plotdir, "FigS1_rec_impacts_broad.png"),
       width=6.5, height=3, units="in", dpi=600)

