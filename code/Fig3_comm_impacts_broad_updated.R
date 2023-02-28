
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

# Read AK total data
ak_tot <- readRDS("data/landings/goa/processed/2004_2021_ak_goa_total.Rds") %>%
  mutate(state="Alaska (below)")

# Rad and format AK by category data
ak_catg <- readRDS("data/landings/goa/processed/2011_2019_ak_goa_by_catg.Rds") %>%
  rename(mgmt_group=category) %>%
  # Add state
  mutate(state="Alaska") %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize
  group_by(state, mgmt_group, period) %>%
  summarise(value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup()


# Build data
################################################################################

# Cap
pdiff_max <- 200

# Totals by state and year
data_tot <- data_orig %>%
  # Reduce
  filter(year>=2000 & year<=2020 & state!="At-Sea") %>%
  # Summarize
  group_by(state, year) %>%
  summarize(landings_lb=sum(landings_lb, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  # Remove Alaska
  filter(state!="Alaska") %>%
  # Add Alaska
  bind_rows(ak_tot) %>%
  # Format state
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington", "British Columbia", "Alaska (below)")))


# Total averages by state and period
data_tot_avg <- data_tot %>%
  # Reguce
  filter(year>=2011 & year<=2019) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize
  group_by(state, period) %>%
  summarize(landings_lb=mean(landings_lb, na.rm=T),
            value_usd=mean(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Mark years
  mutate(yr1=recode(period,
                    "Before"=2011, "During"=2014, "After"=2017) %>% as.numeric(),
         yr2=recode(period,
                    "Before"=2013, "During"=2016, "After"=2019) %>% as.numeric())


# Average by state, group, period
data_group <- data_orig %>%
  # Reduce
  filter(year>=2011 & year<=2019) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize
  group_by(state, mgmt_group, period) %>%
  summarise(value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Remove Alaska
  filter(state!="Alaska") %>%
  # Add Alaska
  bind_rows(ak_catg) %>%
  # Format management group
  mutate(mgmt_group=recode(mgmt_group,
                           "Bivalve"="Bivalves",
                           "Other groundfish"="Other\ngroundfish",
                           "Miscellaneous groundfish"="Other\ngroundfish",
                           "Highly migratory species"="Highly migratory\nspecies",
                           "Coastal pelagic species"="Coastal pelagic\nspecies")) %>%
  # Complete
  complete(state, mgmt_group, period, fill=list(value_usd=NA)) %>%
  # Calculate percent difference
  group_by(state, mgmt_group) %>%
  mutate(value_usd_pre=value_usd[period=="Before"],
         value_usd_pdiff=(value_usd - value_usd_pre) / value_usd_pre * 100,
         value_usd_pdiff_cap=pmin(value_usd_pdiff, pdiff_max)) %>%
  ungroup() %>%
  # Format state
  filter(state!="At-Sea") %>%
  mutate(state_abbrev=recode_factor(state,
                                    "California"="CA",
                                    "Oregon"="OR",
                                    "Washington"="WA",
                                    "British Columbia"="BC",
                                    "Alaska"="AK")) %>%
  # Format value
  mutate(value_usd=ifelse(value_usd==0, NA, value_usd))

# Group order
group_order <- data_group %>%
  filter(period=="During") %>%
  group_by(mgmt_group) %>%
  summarize(pdiff_avg=mean(value_usd_pdiff, na.rm=T)) %>%
  ungroup() %>%
  arrange(pdiff_avg)

# Order data
data_group_ordered <- data_group %>%
  mutate(mgmt_group=factor(mgmt_group, levels=group_order$mgmt_group))


# Plot data
################################################################################

# Colors
ca_color <- RColorBrewer::brewer.pal(9, "Reds")[5]
or_color <- RColorBrewer::brewer.pal(9, "Blues")[5]
wa_color <- RColorBrewer::brewer.pal(9, "Greens")[5]
ak_color <- "goldenrod1" # RColorBrewer::brewer.pal(9, "Oranges")[5]
bc_color <- RColorBrewer::brewer.pal(9, "Purples")[5]
state_colors <- c(ca_color, or_color, wa_color, bc_color, ak_color)

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
ymax1 <- data_tot %>% filter(state!="Alaska (below)") %>% pull(value_usd) %>% max()/1e6
g1 <- ggplot(data_tot %>% filter(state!="Alaska (below)"), aes(x=year, y=value_usd/1e6, color=state)) +
  # Plot heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90", inherit.aes = F) +
  annotate(geom="text", label="MHW", x=2015, y=ymax1*1.1, size=2.1) +
  # Plot revenues
  geom_line(alpha=0.5, lwd=0.6) +
  # Plot period averages
  geom_segment(data=data_tot_avg %>% filter(state!="Alaska (below)"), mapping=aes(x=yr1,
                                           xend=yr2,
                                           y=value_usd/1e6,
                                           yend=value_usd/1e6,
                                           color=state), lwd=0.6) +
  # Labels
  lims(y=c(0,NA), x=c(2000,2020)) +
  labs(x="Year", y="Revenues\n(USD millions)", tag="A") +
  scale_color_manual(name="", values=state_colors, drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.25, 0.83),
        legend.key.size = unit(0.3, "cm"),
        axis.title.x = element_blank())
g1

# Plot totals by state and year
ymax2 <- data_tot %>% filter(state=="Alaska (below)") %>% pull(value_usd) %>% max()/1e6
g2 <- ggplot(data_tot %>% filter(state=="Alaska (below)"), aes(x=year, y=value_usd/1e6, color=state)) +
  # Plot heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90", inherit.aes = F) +
  annotate(geom="text", label="MHW", x=2015, y=ymax2*1.1, size=2.1) +
  # Plot revenues
  geom_line(alpha=0.5, lwd=0.6) +
  # Plot period averages
  geom_segment(data=data_tot_avg %>% filter(state=="Alaska (below)"), mapping=aes(x=yr1,
                                                                          xend=yr2,
                                                                          y=value_usd/1e6,
                                                                          yend=value_usd/1e6,
                                                                          color=state), lwd=0.6) +
  # Labels
  lims(y=c(0,NA), x=c(2000,2020)) +
  labs(x="Year", y="Revenues\n(USD millions)") +
  scale_color_manual(name="", values=state_colors, drop=F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g2

# Plot percent change by management group
g3 <- ggplot(data_group_ordered, aes(x=period, y=mgmt_group, fill=value_usd_pdiff_cap)) +
  facet_wrap(~state_abbrev, nrow=1) +
  # Plot ratser
  geom_raster() +
  # Plot points
  geom_point(data=data_group %>% filter(period=="Before"),
             mapping=aes(x=period, y=mgmt_group, size=value_usd/1e6), inherit.aes = F ) +
  # Labels
  labs(x="Heatwave period", y="", tag="B") +
  # CLegends
  scale_size_continuous(name="Mean annual\npre-MHW revenues\n(USD millions)",
                        breaks=c(10, 100, 200, 500, 1000, 2000)) +
  scale_fill_gradient2(name="% difference\nfrom pre-MHW\nrevenues",
                       midpoint = 0, mid="white", high="navy", low="darkred", na.value="grey90",
                       breaks=c(-100, 0, 100, pdiff_max),
                       labels=c("-100", "0", "100", ">200"),
                       lim=c(-100, NA)) +
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
ggsave(g, filename=file.path(plotdir, "Fig3_comm_impacts_broad_updated.png"),
       width=6.5, height=3, units="in", dpi=600)

