
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/wc_landings_data/data/pacfin/processed/"
data_orig <- readRDS(file.path(datadir, "PACFIN_ALL001_1980_2021_all_species_landings.Rds"))


# Build data
################################################################################

# Year range
range(data_orig$year)

# Build data
data <- data_orig %>%
  # Mark years
  mutate(year_catg=cut(year, breaks=c(0, 2010, 2013, 2016, 2019, Inf),
                       labels=c("Early (<2010)", "Pre-MHW (2011-2013)", "MHW (2014-2016)", "Post-MHW (2017-2019)", "Late (>=2020")),
         year_catg=as.character(year_catg)) %>%
  # Reduce to years of interest
  filter(year %in% 2011:2019) %>%
  # Order states
  mutate(state=factor(state, levels=c("At-Sea", "Washington", "Oregon", "California"))) %>%
  # Fix some species
  mutate(comm_name=recode(comm_name,
                          "Black and yellow rockfish"="Black-and-yellow rockfish",
                          "Nom. Calif halibut"="Nom. California halibut",
                          "Nom. Vermillion rockfish"="Nom. Vermilion rockfish",
                          "Nom. Squarespot"="Nom. Squarespot rockfish",
                          "Nom. Chilipepper"="Nom. Chilipepper rockfish",
                          "Nom. Pop"="Nom. Pacific ocean perch",
                          "Nom. Rougheye + blackspotted"="Nom. Rougheye + blackspotted rockfish")) %>%
  # Mark nominal / not nominal
  mutate(nominal=ifelse(grepl("nom.", tolower(comm_name)), "yes", "no")) %>%
  # Mark parent species (to merge nominal and true)
  mutate(comm_name_parent=gsub("Nom. ", "", comm_name)) %>%
  # Expand management group code
  mutate(mgmt_group=recode(mgmt_group_code,
                           "CPEL"="Coastal Pelagics",
                           "CRAB"="Crabs",
                           "GRND"="Groundfish",
                           "HMSP"="Highly Migratory Species",
                           "OTHR"="Other",
                           "SAMN"="Salmon",
                           "SHLL"="Shellfish",
                           "SRMP"="Shrimp",
                           "XXXX"="Not specified")) %>%
  # Arrange
  select(state, year_catg, year, everything())

# Do years make sense?
year_key <- data %>%
  select(year, year_catg) %>%
  unique() %>%
  arrange(year)

# Do species make sense?
spp_key <- data %>%
  group_by(mgmt_group, mgmt_group_code, complex_code, species_code, comm_name_parent, comm_name, nominal) %>%
  summarize(n=n()) %>%
  ungroup()

spp_key1 <- spp_key %>%
  select(comm_name_parent, mgmt_group) %>%
  unique()



# Build stats
################################################################################

# Summarize by state-year
stats_state_yr <- data %>%
  # Summarize by state
  group_by(state, year_catg, year) %>%
  summarize(landings_mt=sum(landings_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Recode state
  mutate(state=recode(state,
                      "Washington"="WA",
                      "Oregon"="OR",
                      "California"="CA"))

# Summarize by state-period
stats_state_period <- stats_state_yr %>%
  group_by(state, year_catg) %>%
  summarize(yr1=min(year),
            yr2=max(year),
            landings_mt=mean(landings_mt),
            value_usd=mean(value_usd)) %>%
  ungroup()

# Summarize by state-management group
stats_mgmt <- data %>%
  # Annual totals
  group_by(state, mgmt_group, year_catg, year) %>%
  summarize(landings_mt=sum(landings_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Period means
  group_by(state, mgmt_group, year_catg) %>%
  summarize(landings_mt=mean(landings_mt, na.rm=T),
            value_usd=mean(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Complete
  complete(state, mgmt_group, year_catg, fill=list(landings_mt=0, value_usd=0)) %>%
  # Relative to pre-MHW
  group_by(state, mgmt_group) %>%
  mutate(landings_ref=landings_mt[year_catg=="Pre-MHW (2011-2013)"],
         landings_pdiff=(landings_mt - landings_ref) / landings_ref * 100,
         value_ref=value_usd[year_catg=="Pre-MHW (2011-2013)"],
         value_pdiff=(value_usd - value_ref) / value_ref * 100) %>%
  ungroup() %>%
  # Recode MWH periods
  mutate(mhw_catg=recode_factor(year_catg,
                                "Pre-MHW (2011-2013)"="Before",
                                "MHW (2014-2016)"="During",
                                "Post-MHW (2017-2019)"="After")) %>%
  # Recode state
  mutate(state=recode(state,
                      "Washington"="WA",
                      "Oregon"="OR",
                      "California"="CA")) %>%
  # Recode mgmt groupg
  mutate(mgmt_group=recode(mgmt_group,
                           "Highly Migratory Species"="HMS",
                           "Coastal Pelagics"="CPS"))

# Case studies
case_spp <- c("Dungeness crab", "Market squid", "Chinook salmon",
              "Northern anchovy", "Pacific sardine",
              "Pacific hake", "Pacific cod", "Shortbelly rockfish",
              "Red sea urchin", "Bluefin tuna")

# Summarize by state-management group
stats_spp <- data %>%
  # Annual totals
  group_by(state, mgmt_group, comm_name_parent, year_catg, year) %>%
  summarize(landings_mt=sum(landings_mt, na.rm=T),
            value_usd=sum(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Period means
  group_by(state, mgmt_group, comm_name_parent, year_catg) %>%
  summarize(landings_mt=mean(landings_mt, na.rm=T),
            value_usd=mean(value_usd, na.rm=T)) %>%
  ungroup() %>%
  # Complete
  # requires temporarily removing mgmt group
  select(-mgmt_group) %>%
  complete(state, comm_name_parent, year_catg, fill=list(landings_mt=0, value_usd=0)) %>%
  # Relative to pre-MHW
  group_by(state, comm_name_parent) %>%
  mutate(landings_ref=landings_mt[year_catg=="Pre-MHW (2011-2013)"],
         landings_pdiff=(landings_mt - landings_ref) / landings_ref * 100,
         value_ref=value_usd[year_catg=="Pre-MHW (2011-2013)"],
         value_pdiff=(value_usd - value_ref) / value_ref * 100) %>%
  ungroup() %>%
  # Recode MWH periods
  mutate(mhw_catg=recode_factor(year_catg,
                                "Pre-MHW (2011-2013)"="Before",
                                "MHW (2014-2016)"="During",
                                "Post-MHW (2017-2019)"="After")) %>%
  # Add mgmt group back in
  left_join(spp_key1 %>% select(comm_name_parent, mgmt_group), by="comm_name_parent") %>%
  # Cap the values
  mutate(value_pdiff_cap=pmin(value_pdiff, 200)) %>%
  # Format management groups
  mutate(mgmt_group=recode(mgmt_group,
                           "Crabs"="Crabs/shrimp",
                           "Shrimp"="Crabs/shrimp",
                           "Coastal Pelagics"="CPS",
                           "Highly Migratory Species"="HMS")) %>%
  # Remove ones you aren't interested in
  filter(state!="At-Sea" & mgmt_group!="Not specified" &
           !grepl("unsp.|mixed|other|misc", tolower(comm_name_parent))) %>%
  # Remove ones with NAs in all three states
  group_by(comm_name_parent) %>%
  mutate(all3_na=ifelse(sum(is.na(value_pdiff[year_catg=="MHW (2014-2016)"]))==3, "yes", "no")) %>%
  filter(all3_na=="no") %>%
  # Mark case stuides
  mutate(comm_name_parent=ifelse(comm_name_parent %in% case_spp, paste0(comm_name_parent, "***"), comm_name_parent))

# Determine order
spp_order <- stats_spp %>%
  # During MHW
  filter(year_catg=="MHW (2014-2016)") %>%
  # Average across states
  group_by(mgmt_group, comm_name_parent) %>%
  summarize(value_pdiff_avg=mean(value_pdiff, na.rm=T)) %>%
  ungroup() %>%
  # Order
  arrange(mgmt_group, desc(value_pdiff_avg)) %>%
  # Mark case studies
  mutate(case_yn=ifelse(comm_name_parent %in% case_spp, "yes", "no"),
         color=ifelse(case_yn=="yes", "black", "grey60"))

# Order species
stats_spp_ordered <- stats_spp %>%
  mutate(comm_name_parent=factor(comm_name_parent, levels=spp_order$comm_name_parent))


# Plot data
################################################################################

# Theme
my_theme1 <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   strip.text=element_text(size=7),
                   plot.title=element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot state data
g1 <- ggplot(stats_state_yr, aes(x=year, y=value_usd/1e6, color=state)) +
  # Plot MHW
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=300, inherit.aes = F, fill="grey90") +
  # Plot data
  geom_line() +
  # geom_point() +
  # Plot means
  geom_segment(data=stats_state_period,
               mapping=aes(x=yr1, xend=yr2, y=value_usd/1e6, yend=value_usd/1e6),
               linetype="dotted", lwd=1) +
  # Labels
  labs(x="", y="Revenues (USD millions)", tag="A") +
  # Axis
  scale_x_continuous(breaks=2011:2019) +
  # Legend
  scale_color_discrete(name="") +
  guides(color=guide_legend(title.position="top")) +
  # Theme
  theme_bw() + my_theme1 +
  theme(legend.position = "bottom",
        axis.title.x=element_blank(),
        legend.title=element_blank())
g1

# Plot stage-mgmt group data
g2 <- ggplot(stats_mgmt, mapping=aes(x=mhw_catg, y=mgmt_group, fill=value_pdiff)) +
  facet_wrap(~state, nrow=1) +
  geom_tile() +
  # Plot points
  geom_point(data=stats_mgmt %>% filter(mhw_catg=="Before" & value_usd>0),
             mapping=aes(x=mhw_catg, y=mgmt_group, size=value_usd/1e6), inherit.aes = F) +
  # Labels
  labs(x="", y="", tag="B") +
  # Legend
  scale_size_continuous(name="Mean annual\npre-MHW revenues\n(USD millions)") +
  scale_fill_gradient2(name="% difference\nfrom pre-MHW\nrevenues",
                       high="navy", low="darkred", mid="white", midpoint = 0, na.value = "grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black", title.position = "top"),
         size=guide_legend(title.position="top")) +
  # Theme
  theme_bw() + my_theme1 +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(legend.position = "right",
        legend.key.size = unit(0.3, "cm"),
        axis.title.x=element_blank())
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.4, 0.6))

# Export
ggsave(g, filename=file.path(plotdir, "Fig4_comm_impacts_state_group.png"),
       width=6.5, height=3, units="in", dpi=600)


# By species - merged
################################################################################

stats_spp_ordered_gf <- stats_spp_ordered %>%
  filter(mgmt_group=="Groundfish") %>%
  mutate(state=recode(state,
                      "Washington"="WA",
                      "Oregon"="OR",
                      "California"="CA"))

stats_spp_ordered_other <- stats_spp_ordered %>%
  filter(mgmt_group!="Groundfish") %>%
  mutate(state=recode(state,
                      "Washington"="WA",
                      "Oregon"="OR",
                      "California"="CA"))

# Plot groundfish
g1 <- ggplot(stats_spp_ordered_gf,
             mapping=aes(x=mhw_catg, y=comm_name_parent, fill=value_pdiff_cap)) +
  facet_grid(mgmt_group~state, scales="free_y", space="free_y") +
  geom_tile() +
  # Plot points
  geom_point(data=stats_spp_ordered_gf %>% filter(mhw_catg=="Before" & value_usd>0),
             mapping=aes(x=mhw_catg, y=comm_name_parent, size=value_usd/1e6), inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_size_continuous(name="Mean annual\nrevenues (USD millions)",
                        breaks=c(1, 5, 10, 20, 40, 60)) +
  scale_fill_gradient2(name="% difference\nfrom pre-MHW\nrevenues",
                       high="navy", low="darkred", mid="white", midpoint = 0, na.value = "grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme2 +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position = "none")
g1

# Plot other
g2 <- ggplot(stats_spp_ordered_other,
             mapping=aes(x=mhw_catg, y=comm_name_parent, fill=value_pdiff_cap)) +
  facet_grid(mgmt_group~state, scales="free_y", space="free_y") +
  geom_tile() +
  # Plot points
  geom_point(data=stats_spp_ordered_other %>% filter(mhw_catg=="Before" & value_usd>0),
             mapping=aes(x=mhw_catg, y=comm_name_parent, size=value_usd/1e6), inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Legend
  scale_size_continuous(name="Mean annual\npre-MHW revenues\n(USD millions)",
                        breaks=c(1, 5, 10, 20, 40, 60)) +
  scale_fill_gradient2(name="% difference\nfrom pre-MHW\nrevenues",
                       high="navy", low="darkred", mid="white", midpoint = 0, na.value = "grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + my_theme2 +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, nrow=1, widths=c(0.45, 0.55))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig5_comm_impacts_state_spp.png"),
       width=8, height=8, units="in", dpi=600)

