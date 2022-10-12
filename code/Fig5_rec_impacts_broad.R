
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


# Build data
################################################################################

# Derive species key
spp_key <- data_orig %>%
  select(taxa_catg, comm_name, sci_name, level) %>% unique() %>%
  arrange(taxa_catg, comm_name) %>%
  # Merge some categories
  mutate(taxa_catg_orig=taxa_catg,
         taxa_catg=recode(taxa_catg_orig,
                          "Tunas, mackerels, pompanos"="Large pelagics",
                          "Other large pelagics"="Large pelagics",
                          "Smelts"="Coastal pelagics",
                          "Sardines, herring, anchovies"="Coastal pelagics",
                          "Other coastal pelagics"="Coastal pelagics",
                          "Silversides"="Coastal pelagics",
                          "Gobies"="Gobies, blennies",
                          "Blennies"="Gobies, blennies",
                          "Shellfish"="Shellfish\nand other inverts",
                          "Invertebrate"="Shellfish\nand other inverts",
                          "Drums"="Drums, wrasses,\ngrunts, tilefishes",
                          "Wrasses"="Drums, wrasses,\ngrunts, tilefishes",
                          "Grunts"="Drums, wrasses,\ngrunts, tilefishes",
                          "Tilefish"="Drums, wrasses,\ngrunts, tilefishes",
                          "Eels"="Other fish",
                          "Hagfish, lampreys"="Other fish",
                          "Other"="Other fish",
                          "Sturgeons"="Other fish",
                          "Lizardfishes"="Other fish",
                          "Groupers, sea basses"="Other fish"))

table(spp_key$taxa_catg)

# Build data
data <- data_orig %>%
  # Reduce to years of interest
  filter(year %in% 2000:2020) %>%
  # Summarize
  group_by(state, year) %>%
  summarize(retained_n=sum(retained_n)) %>%
  ungroup() %>%
  # Format state
  mutate(state=recode(state, "California"="California (below)"),
         state=factor(state, levels=c("California (below)", "Oregon", "Washington", "Alaska") %>% rev()))

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
  # Update taxa category
  select(-taxa_catg) %>%
  left_join(spp_key %>% select(comm_name, taxa_catg)) %>%
  # Reduce to years of interest
  filter(year %in% 2011:2019) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize
  group_by(state, taxa_catg, period) %>%
  summarize(retained_n=sum(retained_n)) %>%
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
                      "California"="CA", "Oregon"="OR", "Washington"="WA", "Alaska"="AK"))

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
ak_color <- RColorBrewer::brewer.pal(9, "Purples")[5]
state_colors <- c(ca_color, or_color, wa_color, ak_color) %>% rev()

# Theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.tag=element_text(size=9),
                   plot.title = element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot totals by state and year
ymax1 <- data %>% filter(state!="California (below)") %>% pull(retained_n) %>% max()/1e6
g1 <- ggplot(data %>% filter(state!="California (below)"), aes(x=year, y=retained_n/1e6, color=state)) +
  # Plot heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90", inherit.aes = F) +
  annotate(geom="text", label="MHW", x=2015, y=ymax1*1.1, size=2.1) +
  # Plot landings
  geom_line(alpha=0.5, lwd=0.6) +
  # Plot period averages
  geom_segment(data=data_avgs %>% filter(state!="California (below)"), mapping=aes(x=yr1,
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
  theme(legend.position = c(0.25, 0.48),
        legend.key.size = unit(0.3, "cm"),
        axis.title.x = element_blank())
g1

# Plot totals by state and year
ymax2 <- data %>% filter(state=="California (below)") %>% pull(retained_n) %>% max()/1e6
g2 <- ggplot(data %>% filter(state=="California (below)"), aes(x=year, y=retained_n/1e6, color=state)) +
  # Plot heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90", inherit.aes = F) +
  annotate(geom="text", label="MHW", x=2015, y=ymax2*1.1, size=2.1) +
  # Plot landings
  geom_line(alpha=0.5, lwd=0.6) +
  # Plot period averages
  geom_segment(data=data_avgs %>% filter(state=="California (below)"), mapping=aes(x=yr1,
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
       width=6.5, height=3, units="in", dpi=600)

