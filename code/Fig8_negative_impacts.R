
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
plotdir <- "figures"


# Dungeness crab data
################################################################################

# Read data
datadir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/merged/processed"
closuredir <- "/Users/cfree/Dropbox/Chris/UCSB/projects/domoic_acid_mgmt/data/closures/processed"
closures_orig <- readRDS(file.path(closuredir, "2015_2020_WC_dcrab_closures.Rds"))

# Seasons
seasons_do <- 2014:2020

# Sonoma-Mendocino county line
son_mend_county <- 38+46.125/60

# Washington season
# December 1st to September 15th
openers_wa <- paste0(seasons_do, "-12-01") %>% ymd()
closers_wa <- paste0(seasons_do+1, "-09-15") %>% ymd()
seasons_wa <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                     open=openers_wa,
                     close=closers_wa)

# Oregon season
# December 1st to August 14th
openers_or <- paste0(seasons_do, "-12-01") %>% ymd()
closers_or <- paste0(seasons_do+1, "-08-14") %>% ymd()
seasons_or <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                     open=openers_or,
                     close=closers_or)

# California-North season
# December 1st to July 15th
openers_ca_n <- paste0(seasons_do, "-12-01") %>% ymd()
closers_ca_n <- paste0(seasons_do+1, "-07-15") %>% ymd()
seasons_ca_n <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                       open=openers_ca_n,
                       close=closers_ca_n)

# California-Central season
# November 15th to June 30th
openers_ca_c <- paste0(seasons_do, "-11-15") %>% ymd()
closers_ca_c <- paste0(seasons_do+1, "-06-30") %>% ymd()
seasons_ca_c <- tibble(year=paste(seasons_do, seasons_do+1, sep="-"),
                       open=openers_ca_c,
                       close=closers_ca_c)

# Fix data
closures <- closures_orig %>%
  mutate(status=as.character(status),
         # Rename whale closures
         status=recode(status,
                       "Whale entanglement closure"="Marine life entanglement closure",
                       "Domoic acid delay"="Domoic acid delay/closure",
                       "Body condition delay"="Meat quality delay",
                       "Body condition/domoic acid delay"="Meat quality/domoic acid delay"),
         # Fix out of season
         status=ifelse(status=="Out-of-season", NA, status),
         status=ifelse(date>ymd("2021-08-14") & lat_dd<46.25000 & lat_dd>42.00000, "Out-of-season", status),
         status=ifelse(date>ymd("2021-07-15") & lat_dd<42.00000 & lat_dd>son_mend_county, "Out-of-season", status),
         status=ifelse(date>ymd("2021-06-30") & lat_dd<son_mend_county, "Out-of-season", status),
         # Factor
         status=factor(status, levels=c("Season open", "Meat quality delay",
                                        "Meat quality/domoic acid delay", "Domoic acid delay/closure",
                                        "Evisceration order", "Marine life entanglement closure")))

# Starting date
date_min_do <- ymd(paste0(min(seasons_do), "-01-01"))
date_max_do <- ymd(paste0(max(seasons_do)+1, "-01-01"))


# Pacific sardine data
################################################################################

# PACFIN data
pacfin_orig <- wcfish::pacfin_all5

# Build data
sardine <- pacfin_orig %>%
  # Reduce to sardine
  filter(comm_name %in% c("Pacific sardine")) %>%
  # Summarize by total
  group_by(state, year) %>%
  summarize(landings_mt=sum(landings_mt),
            value_usd=sum(revenues_usd)) %>%
  ungroup() %>%
  # Order states
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev()))

# Pacific cod data
################################################################################

# Read data
pcod_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/wc_landings_data/data/akfin/processed/GFSA08_2003_2020_GOA_groundfish_landings_by_gear_subarea_species.Rds")

# Build data
pcod <- pcod_orig %>%
  filter(species=="Pacific Cod" & subarea!="All Subareas" & gear!="All Gear") %>%
  group_by(year, gear) %>%
  summarize(value_usd=sum(value_usd)) %>%
  ungroup()

# Red abalone data
################################################################################

# Read data
abalone_orig <- readRDS("data/case_study_data/abalone/CDFW_2002_2015_red_abalone_catch_by_site.Rds")

# Format data
abalone <- abalone_orig %>%
  group_by(county, year) %>%
  summarise(landings_n=sum(landings_n, na.rm=T)) %>%
  ungroup() %>%
  # Order counties
  mutate(county=factor(county, levels=c("Del Norte", "Humboldt", "Mendocino", "Sonoma", "Marin")))

# Chinook data
################################################################################

# Read data
salmon_orig <- readRDS("data/case_study_data/salmon/PFMC_1985_2022_klamath_fall_chinook_forecast_obs.Rds")

# Format data
salmon <- salmon_orig %>%
  # Reduce to totals
  filter(age=="Total Adults") %>%
  # Simplify
  select(year, preseason, postseason) %>%
  # Gather
  gather(key="type", value="abundance", 2:ncol(.)) %>%
  # Recode
  mutate(type=recode(type,
                     "preseason"="Preseason forecast",
                     "postseason"="Postseason estimtate")) %>%
  # Record lowest
  group_by(type) %>%
  mutate(rank=rank(abundance))

# Plot data
################################################################################

# Colors
ca_color <- RColorBrewer::brewer.pal(9, "Reds")[5]
or_color <- RColorBrewer::brewer.pal(9, "Blues")[5]
wa_color <- RColorBrewer::brewer.pal(9, "Greens")[5]
state_colors <- c(ca_color, or_color, wa_color) %>% rev()

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
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Dungeness crab
g1 <- ggplot(closures, aes(x=date, y=lat_dd, fill=status)) +
  # Season shading
  geom_rect(data=seasons_wa, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=46.25, ymax=48.48, fill="grey90") +
  geom_rect(data=seasons_or, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=42, ymax=46.25, fill="grey90") +
  geom_rect(data=seasons_ca_n, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=son_mend_county, ymax=42, fill="grey90") +
  geom_rect(data=seasons_ca_c, inherit.aes=F, mapping=aes(xmin=open, xmax=close), ymin=35, ymax=son_mend_county, fill="grey90") +
  # Plot raster
  geom_raster() +
  # State/region lines
  geom_hline(yintercept=c(48.43333, 46.25000, 42.00000), size=0.5) +
  geom_hline(yintercept = son_mend_county, linetype="dashed", size=0.5) + # Sonoma/Mendocino
  # Label state lines
  annotate(geom="text", x=date_min_do, y=48.48, hjust=0, vjust=1.5, label="Washington", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=46.25, hjust=0, vjust=1.5, label="Oregon", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=42, hjust=0, vjust=1.5, label="N. California", color="grey30", size=2.5) +
  annotate(geom="text", x=date_min_do, y=son_mend_county, hjust=0, vjust=1.5, label="C. California", color="grey30", size=2.5) +
  # Limits
  scale_y_continuous(limits=c(35, 48.5), breaks=seq(34, 48, 2)) +
  scale_x_date(breaks=seq(date_min_do, date_max_do, by="1 year"), labels=year(date_min_do):year(date_max_do)) +
  # Labels
  labs(x="Date", y=" \nLatitude (°N)", tag="A", title="Commercial Dungeness crab fishery") +
  # Legends
  scale_fill_manual(name="Season status", values=c("grey90", "pink", "orange", "darkred", "magenta3", "navy"), na.translate = F) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.key.size = unit(0.4, "cm"))
g1

# Pacific cod
ymax2 <- pcod %>% group_by(year) %>% summarize(val=sum(value_usd/1e6)) %>% pull(val) %>% max()
g2 <-ggplot(pcod, aes(x=year, y=value_usd/1e6, fill=gear)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90", show.legend = F) +
  annotate(geom="text", label="MHW", x=2015, y=ymax2*1.05, size=2.1) +
  # Revenues
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Label closure
  geom_segment(x=2020, xend=2020, y=0, yend=55, linetype="dotted") +
  annotate(geom="text", label="Fishery\nclosure", x=2019.5, y=55, hjust=1, size=2.1) +
  # Labels
  labs(x="", y="Revenues\n(USD millions)", title="Commercial Pacific cod fishery", tag="B") +
  scale_x_continuous(lim=c(1980, 2022)) +
  # Legend
  scale_fill_manual(name="Gear", values=RColorBrewer::brewer.pal(3, "Purples")) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8),
        legend.key.size = unit(0.3, "cm"))
g2

# Read abalone
ymax3 <- abalone %>% group_by(year) %>% summarize(val=sum(landings_n/1000)) %>% pull(val) %>% max()
ncounties <- abalone %>% pull(county) %>% n_distinct()
g3 <- ggplot(abalone, aes(x=year, y=landings_n/1000, fill=county)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  annotate(geom="text", label="MHW", x=2015, y=ymax3*1.05, size=2.1) +
  # Value
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Label closure
  geom_segment(x=2017, xend=2017, y=0, yend=250, linetype="dotted") +
  annotate(geom="text", label="Fishery\nclosure", x=2017.5, y=250, hjust=0, size=2.1) +
  # Labels
  labs(x="", y="Landings\n(1000s of abalone)", title="Recreational red abalone fishery", tag="C") +
  scale_fill_manual(name="California county\n(north to south)", values=RColorBrewer::brewer.pal(ncounties, "Reds")) +
  scale_x_continuous(lim=c(1980, 2022)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.7),
        legend.key.size = unit(0.3, "cm"))
g3


# Chinook salmon
ymax4 <- max(salmon$abundance/1e3, na.rm=T)
g4 <- ggplot(salmon, aes(x=year, y=abundance/1e3, linetype=type)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90", show.legend = F) +
  annotate(geom="text", label="MHW", x=2015, y=ymax4*1.05, size=2.1) +
  geom_line() +
  # Labels
  labs(x="", y="Abundance\n(1000s of salmon)", title="Klamath River Fall Chinook salmon fishery", tag="D") +
  scale_x_continuous(lim=c(1980, 2022)) +
  # Legend
  scale_linetype_discrete(name="Abundance type") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.45, 0.8),
        legend.key.size = unit(0.3, "cm"))
g4

# Pacific sardine
ymax5 <- sardine %>% group_by(year) %>% summarize(val=sum(value_usd/1e6)) %>% pull(val) %>% max()
g5 <- ggplot(sardine, aes(x=year, y=value_usd/1e6, fill=state)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  annotate(geom="text", label="MHW", x=2015, y=ymax5*1.05, size=2.1) +
  # Value
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Label closure
  geom_segment(x=2015, xend=2015, y=0, yend=6, linetype="dotted") +
  annotate(geom="text", label="Fishery\nclosure", x=2015.5, y=6, hjust=0, size=2.1) +
  # Labels
  labs(x="", y="Revenues\n(USD millions)", title="Commercial Pacific sardine fishery", tag="E") +
  scale_fill_manual(name="State", values=state_colors) +
  scale_x_continuous(lim=c(1980, 2022)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8),
        legend.key.size = unit(0.3, "cm"))
g5


# Merge
layout_matrix <- matrix(data=c(1,1,
                               2,3,
                               4,5), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5,
                             layout_matrix=layout_matrix)
g

# Export plot
################################################################################

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig8_negative_impacts.png"),
       width=6.5, height=6.5, units="in", dpi=600)




