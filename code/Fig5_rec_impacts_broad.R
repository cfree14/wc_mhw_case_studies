
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"
datadir <- "data/landings"

# Read data
data_orig <- readRDS("data/landings/annual_rec_landings_by_state.Rds") %>%
  mutate(comm_name=recode(comm_name, "Steelhead"="Steelhead trout"))

# Read species key
spp_key <- readxl::read_excel(file.path(datadir, "rec_species_key.xlsx")) %>%
  mutate(taxa_catg=recode(taxa_catg,
                          "Drums, wrasses, grunts, tilefishes"="Drums, wrasses,\ngrunts, tilefishes"))
freeR::which_duplicated(spp_key$comm_name)
freeR::which_duplicated(spp_key$sci_name)
freeR::check_names(spp_key$sci_name)


# Check species
################################################################################

# Species key
spp_key_check <- data_orig %>%
  select(comm_name, sci_name, level) %>% unique()
#write.csv(spp_key, file=file.path(datadir, "rec_species_key.csv"), row.names = F)

# Confirm that all species are in key
# If they aren't, manually add new species to the key
spp_key_check$comm_name[!spp_key_check$comm_name %in% spp_key$comm_name]


# Build data
################################################################################

# Build data
data_tot <- data_orig %>%
  # Reduce to years of interest
  filter(year %in% 2000:2020) %>%
  # Summarize
  group_by(state, year) %>%
  summarize(retained_n=sum(retained_n)) %>%
  ungroup() %>%
  # Format state
  mutate(state=recode(state,
                      "California"="California (below)",
                      "British Columbia"="British Columbia (below)"),
         state=factor(state, levels=c("California (below)", "Oregon", "Washington", "British Columbia (below)", "Alaska") %>% rev()))

# Build period averages
data_avgs <- data_tot %>%
  # Reduce to years of interest
  filter(year %in% 2011:2019) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize totals by year
  group_by(state, period, year) %>%
  summarise(retained_n=sum(retained_n)) %>%
  ungroup() %>%
  # Summarize average totals by period
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
  # Add taxa category
  left_join(spp_key %>% select(comm_name, taxa_catg), by=c("comm_name")) %>%
  # Reduce to years of interest
  filter(year %in% 2011:2019) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize totals by year
  group_by(state, taxa_catg, period, year) %>%
  summarise(retained_n=sum(retained_n)) %>%
  ungroup() %>%
  # Summarize
  group_by(state, taxa_catg, period) %>%
  summarize(retained_n=mean(retained_n)) %>%
  ungroup() %>%
  # Complete periods
  complete(state, taxa_catg, period, fill=list(retained_n=NA)) %>%
  # Calculate percent difference
  group_by(state, taxa_catg) %>%
  mutate(retained_n_pre=retained_n[period=="Before"],
         retained_n_pdiff=(retained_n - retained_n_pre) / retained_n_pre * 100,
         retained_n_pdiff_cap=pmin(retained_n_pdiff, 200)) %>%
  ungroup() %>%
  # Recode state
  mutate(state=recode_factor(state,
                      "California"="CA", "Oregon"="OR", "Washington"="WA", "British Columbia"="BC", "Alaska"="AK"))

# Group order
group_order <- data_group %>%
  filter(period=="During") %>%
  group_by(taxa_catg) %>%
  summarize(pdiff_avg=mean(retained_n_pdiff, na.rm=T)) %>%
  ungroup() %>%
  arrange(pdiff_avg)

# Apply order
data_group_ordered <- data_group %>%
  mutate(taxa_catg=factor(taxa_catg, levels=group_order$taxa_catg))





# Plot data
################################################################################

# Colors
ca_color <- RColorBrewer::brewer.pal(9, "Reds")[5]
or_color <- RColorBrewer::brewer.pal(9, "Blues")[5]
wa_color <- RColorBrewer::brewer.pal(9, "Greens")[5]
bc_color <- "goldenrod1" # RColorBrewer::brewer.pal(9, "Oranges")[5]
ak_color <- RColorBrewer::brewer.pal(9, "Purples")[5]
state_colors <- c(ca_color, or_color, wa_color, bc_color, ak_color) %>% rev()

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.tag=element_text(size=8),
                   plot.title = element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot totals by state and year
ymax1 <- data_tot %>% filter(!grepl("below", state)) %>% pull(retained_n) %>% max()/1e6
g1 <- ggplot(data_tot %>% filter(!grepl("below", state)), aes(x=year, y=retained_n/1e6, color=state)) +
  # Plot heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey95", inherit.aes = F) +
  annotate(geom="text", label="MHW", x=2015, y=ymax1*1.1, size=2.1) +
  # Plot landings
  geom_line(alpha=0.5, lwd=0.6) +
  # Plot period averages
  geom_segment(data=data_avgs %>% filter(!grepl("below", state)), mapping=aes(x=yr1,
                                                                                  xend=yr2,
                                                                                  y=retained_n_avg/1e6,
                                                                                  yend=retained_n_avg/1e6,
                                                                                  color=state), lwd=0.6) +
  # Labels
  lims(y=c(0,NA), x=c(2000,2020)) +
  labs(x="Year", y="Landings\n(millions of fish)", tag="A") +
  scale_color_manual(name="", values=state_colors, drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.35, 0.48),
        legend.key.size = unit(0.3, "cm"),
        axis.title.x = element_blank())
g1

# Plot totals by state and year
ymax2 <- data_tot %>% filter(grepl("below", state)) %>% pull(retained_n) %>% max()/1e6
g2 <- ggplot(data_tot %>% filter(grepl("below", state)), aes(x=year, y=retained_n/1e6, color=state)) +
  # Plot heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey95", inherit.aes = F) +
  annotate(geom="text", label="MHW", x=2015, y=ymax2*1.1, size=2.1) +
  # Plot landings
  geom_line(alpha=0.5, lwd=0.6) +
  # Plot period averages
  geom_segment(data=data_avgs %>% filter(grepl("below", state)), mapping=aes(x=yr1,
                                                                                   xend=yr2,
                                                                                   y=retained_n_avg/1e6,
                                                                                   yend=retained_n_avg/1e6,
                                                                                   color=state), lwd=0.6) +
  # Labels
  lims(y=c(0,NA), x=c(2000,2020)) +
  labs(x="Year", y="Landings\n(millions of fish)") +
  scale_color_manual(name="", values=state_colors, drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none",
        legend.key.size = unit(0.3, "cm"),
        axis.title.x = element_blank())
g2

# Plot percent change by management group
g3 <- ggplot(data_group_ordered, aes(x=period, y=taxa_catg, fill=retained_n_pdiff_cap)) +
  facet_wrap(~state, nrow=1) +
  # Plot ratser
  geom_raster() +
  # Plot points
  geom_point(data=data_group %>% filter(period=="Before"),
             mapping=aes(x=period, y=taxa_catg, size=retained_n/1e6), inherit.aes = F ) +
  # Labels
  labs(x="Heatwave period", y="", tag="B") +
  # Legends
  scale_size_continuous(name="Mean annual\npre-MHW landings\n(millions of fish)") +
  scale_fill_gradient2(name="% difference\nfrom pre-MHW\nlandings",
                       midpoint = 0, mid="white", high="navy", low="darkred", na.value="grey90") +
  guides(size=guide_legend(order=1),
         fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", order=2)) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.key.size = unit(0.3, "cm"))
g3

# Merge
layout_matrix <- matrix(data=c(1,3,
                               2,3), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, layout_matrix=layout_matrix, heights=c(0.6, 0.4), widths=c(0.4, 0.6))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_rec_impacts_broad.png"),
       width=6.5, height=4, units="in", dpi=600)

