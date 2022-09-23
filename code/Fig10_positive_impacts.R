
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"


# Shortbelly rockfish
################################################################################

# Read GEMM data
gemm_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/gemm/processed/GEMM_2002_2020_data.Rds")
pacfin_orig <- wcfish::pacfin_all5
noaa_orig <- wcfish::noaa

# Build GEMM catch
shortbelly1 <- gemm_orig %>%
  # Reduce to shortbelly rockfish
  filter(species=="Shortbelly Rockfish") %>%
  # Summarize by total
  group_by(year) %>%
  summarize(landings_mt=sum(landings_mt),
            discards_mt=sum(discards_mt_tot)) %>%
  ungroup() %>%
  # Gather
  gather(key="catch_type", value="catch_mt", 2:ncol(.)) %>%
  mutate(catch_type=recode(catch_type,
                           "landings_mt"="Landings",
                           "discards_mt"="Discards"))

# Build PACFIN catch
shortbelly2 <- pacfin_orig %>%
  # Reduce to shortbelly rockfish
  filter(comm_name %in% c("Shortbelly rockfish", "Nom. Shortbelly rockfish")) %>%
  # Summarize by total
  group_by(year) %>%
  summarize(landings_mt=sum(landings_mt)) %>%
  ungroup() %>%
  # Format
  rename(catch_mt=landings_mt) %>%
  mutate(catch_type="Landings")

# Reduce to years missing from GEMM data
shortbelly2_use <- shortbelly2 %>%
  filter(!year %in% shortbelly1$year)

# Build NOAA catch
shortbelly3 <- noaa_orig %>%
  filter(comm_name=="Shortbelly rockfish") %>%
  group_by(year) %>%
  summarize(catch_lb=sum(landings_lb, na.rm=T)) %>%
  ungroup() %>%
  mutate(catch_kg=measurements::conv_unit(catch_lb, "lbs", "kg"),
         catch_mt=catch_kg/1000)

# Merge
shortbelly <- bind_rows(shortbelly1, shortbelly2_use)

# Shortbelly catch limit
sb_limit_lb <- 13900 * 1000
sb_limit_kg <- measurements::conv_unit(sb_limit_lb, "lbs", "kg")
sb_limit_mt <- sb_limit_kg / 1000

# Plot data
g <- ggplot(shortbelly1, aes(x=year, y=catch_mt, fill=catch_type)) +
  geom_bar(stat="identity", alpha=0.5) +
  geom_line(data=shortbelly2, aes(x=year, y=catch_mt), inherit.aes=F) +
  geom_line(data=shortbelly3, aes(x=year, y=catch_mt), inherit.aes=F, linetype="dotted") +
  labs(x="Year", y="Catch (mt)") +
  theme_bw()
g


# Market squid
################################################################################

# Problem ports
problem_ports <- c("Landed in WA, transported to OR 1", "Other/Unknown California ports 2", "Other/Unknown Oregon ports 2")

# Port order
ca_ports <- c("San Diego", "Los Angeles", "Santa Barbara", "Morro Bay", "Monterey",
              "San Francisco", "Bodega Bay", "Fort Bragg", "Eureka", "Crescent City")
or_ports <- c("Brookings", "Coos Bay", "Newport", "Tillamook", "Columbia River")

# Build squid
squid <- pacfin_orig %>%
  # Reduce to squid
  filter(comm_name %in% c("Market squid")) %>%
  # Summarize by total
  group_by(state, port_name, year) %>%
  summarize(landings_mt=sum(landings_mt),
            value_usd=sum(revenues_usd)) %>%
  ungroup() %>%
  # Format port complexes
  rename(port_complex=port_name) %>%
  mutate(port_complex=gsub(" Area ports", "", port_complex),
         port_complex=recode(port_complex, "Columbia River - Oregon"="Columbia River")) %>%
  # Remove uninteresting ports
  filter(!port_complex %in% problem_ports) %>%
  # Factor ports
  mutate(port_complex=factor(port_complex, levels=c(ca_ports, or_ports)))

count(squid, state, port_complex) %>% arrange(state)

# Bluefin tuna
################################################################################

# Read bluefin data
bluefin_orig <- read.csv("data/case_study_data/bluefin/CPFV_PBF_Totals.csv", as.is=T)

# Format
bluefin <- bluefin_orig %>%
  rename(region=EEZ, landings_n=totalPBF) %>%
  mutate(region=recode_factor(region,
                              "United States"="USA",
                              "Mexico"="Mexico"))


# Shrimp
################################################################################

# Read PACFIN data
pacfin_all5 <- wcfish::pacfin_all5

# Format data
shrimp <- pacfin_all5 %>%
  # Reduce to shrimp/prawns
  filter(grepl("prawn|shrimp", tolower(comm_name))) %>%
  # Summarize
  group_by(state, comm_name, sci_name, year) %>%
  summarize(landings_mt=sum(landings_mt, na.rm = T),
            value_usd=sum(revenues_usd, na.rm=T)) %>%
  # Factor states
  mutate(state=factor(state, levels=c("California", "Oregon", "Washington") %>% rev())) %>%
  # Reduce to species of interest
  filter(comm_name %in% c("Pacific pink shrimp", "Ridgeback prawn", "Spotted prawn")) %>%
  mutate(comm_name=recode(comm_name, "Spotted prawn"="Spot prawn")) %>%
  # Make species label
  mutate(spp_label=paste0(comm_name, "\n(", sci_name, ")"))

# Bocaccio
################################################################################

# Read data
boc_orig <- read.csv("data/case_study_data/bocaccio/boc.csv")


# Plot data
################################################################################

# Colors
ca_color <- RColorBrewer::brewer.pal(9, "Reds")[5]
or_color <- RColorBrewer::brewer.pal(9, "Blues")[5]
wa_color <- RColorBrewer::brewer.pal(9, "Greens")[5]
mex_color <- RColorBrewer::brewer.pal(9, "YlOrRd")[2]
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


# Shortbelly
ymax1 <- shortbelly %>% group_by(year) %>% summarize(val=sum(catch_mt)) %>% pull(val) %>% max()
g1 <- ggplot(shortbelly, aes(x=year, y=catch_mt, fill=catch_type)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  annotate(geom="text", label="MHW", x=2015, y=ymax1*1.05, size=2.1) +
  # Plot GEMM datch
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Plot catch limit
  annotate(geom="text", x=2008, y=50, label="50 mt limit", color="grey30", vjust=-0.4, hjust=0, size=2.2) +
  geom_segment(x=2008, xend=2015, y=50, yend=50,  color="grey30") +
  annotate(geom="text", x=2017, y=500, label="500 mt limit", color="grey30", vjust=-0.4, hjust=1, size=2.2) +
  geom_segment(x=2015, xend=2020, y=500, yend=500,  color="grey30") +
  # Plot PACFIN catch
  # Labels
  labs(x="", y="\nCatch (mt)", title="Shortbelly rockfish bycatch fishery", tag="A") +
  scale_fill_manual(name="Catch type", values=c("grey60", "grey20")) +
  scale_x_continuous(lim=c(1980, 2022)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8),
        legend.key.size = unit(0.3, "cm"))
g1

# Market squid
ca_colors <- freeR::colorpal(RColorBrewer::brewer.pal(9, "Reds"), n_distinct(ca_ports)) %>% rev()
or_colors <- RColorBrewer::brewer.pal(n_distinct(or_ports), "Blues")
squid_colors <- c(ca_colors, or_colors)
ymax2 <- squid %>% group_by(year) %>% summarize(val=sum(value_usd/1e6)) %>% pull(val) %>% max()
g2 <- ggplot(squid, aes(x=year, y=value_usd/1e6, fill=port_complex)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  annotate(geom="text", label="MHW", x=2015, y=ymax2*1.05, size=2.1) +
  # Plot PACFIN value
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y="Revenues\n(USD millions)", title="Commercial market squid fishery", tag="B") +
  scale_x_continuous(lim=c(1980, 2022)) +
  # Legend
  scale_fill_manual(name="Port complex\n(south to north)",
                    values=squid_colors) +
  guides(fill=guide_legend(ncol=2)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.28, 0.7),
        legend.key.size = unit(0.2, "cm"),
        legend.text = element_text(size=5))
g2

# Shrimp
g3 <- ggplot(shrimp, mapping=aes(x=year, y=value_usd/1e6, fill=state)) +
  facet_wrap(~comm_name, scales = "free_y") +
  # Mark MHW
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  # Bars
  geom_bar(stat="identity", col="grey30", lwd=0.3) +
  # Labels
  labs(x="Year", y="Value\n(USD millions)", title="Commercial shrimp fisheries", tag="C") +
  scale_fill_discrete(name="State") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.08, 0.8),
        legend.key.size = unit(0.3, "cm"))
g3

# Bluefin
ymax4 <- bluefin %>% group_by(year) %>% summarize(val=sum(landings_n/1e3)) %>% pull(val) %>% max()
g4 <- ggplot(bluefin, aes(x=year, y=landings_n/1e3, fill=region)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  annotate(geom="text", label="MHW", x=2015, y=ymax4*1.05, size=2.1) +
  # Plot PACFIN value
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y="Landings\n(1000s of tuna)",
       title="Recreational Pacific bluefin tuna fishery", tag="D") +
  scale_fill_manual(name="Source waters", values=c(ca_color, mex_color)) +
  scale_x_continuous(lim=c(1980, 2022)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8),
        legend.key.size = unit(0.3, "cm"))
g4

# Bocaccio
ymax5 <- boc_orig %>% pull(recruitment) %>% max()
g5 <- ggplot(boc_orig, aes(x=year, y=recruitment)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  annotate(geom="text", label="MHW", x=2015, y=ymax5*1.1, size=2.1) +
  # Plot recruitment
  geom_line() +
  # Scale
  scale_y_continuous(trans="log10") +
  scale_x_continuous(breaks=seq(1930,2020,10)) +
  # Labels
  labs(x="", y="Recruitment\n(1000s of age-1 fish)",
       title="Bocaccio rockfish bycatch fishery", tag="E") +
  # Theme
  theme_bw() + my_theme
g5

# Merge
layout_matrix <- matrix(data=c(1,2,
                               3,3,
                               4,5), ncol=2, byrow=T)
g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5,
                             layout_matrix=layout_matrix)
g

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig10_positive_impacts.png"),
       width=6.5, height=6.5, units="in", dpi=600)



