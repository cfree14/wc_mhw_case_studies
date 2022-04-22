
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
            discards_mt=sum(discards_mt)) %>%
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

# Build squid
squid <- pacfin_orig %>%
  # Reduce to squid
  filter(comm_name %in% c("Market squid")) %>%
  # Summarize by total
  group_by(state, year) %>%
  summarize(landings_mt=sum(landings_mt),
            value_usd=sum(revenues_usd)) %>%
  ungroup()


# Bluefin tuna
################################################################################

# CDFW data
cpfv1 <- wcfish::cdfw_cpfv
cpfv2 <- wcfish::cdfw_cpfv_port

# Bluefin
bluefin1 <- cpfv1 %>%
  # Bluefin tuna
  filter(comm_name=="Bluefin tuna") %>%
  # Sum by year
  group_by(waters, year) %>%
  summarize(landings_n=sum(landings_n)) %>%
  ungroup() %>%
  # Add region
  rename(region=waters)

# Bluefin
bluefin2 <- cpfv2 %>%
  # Bluefin tuna
  filter(comm_name=="Bluefin tuna") %>%
  # Summarize
  group_by(year) %>%
  summarise(landings_n=sum(landings_n)) %>%
  ungroup() %>%
  # Add region
  mutate(region="USA+Mexico") %>%
  # Reduce
  filter(year < 2000)

# Merge
bluefin <- bind_rows(bluefin1, bluefin2) %>%
  mutate(region=factor(region, levels=c("USA", "Mexico", "USA+Mexico")))


# Plot data
################################################################################

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
  # geom_hline(yintercept=sb_limit_mt, linetype="dotted") +
  # Plot PACFIN catch
  # geom_line(data=shortbelly2, aes(x=year, y=catch_mt), inherit.aes = F) +
  # Labels
  labs(x="", y="\nCatch (mt)", title="Shortbelly rockfish bycatch fishery", tag="A") +
  scale_fill_discrete(name="Catch type") +
  scale_x_continuous(lim=c(1980, 2022)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8),
        legend.key.size = unit(0.3, "cm"))
g1

# Market squid
ymax2 <- squid %>% group_by(year) %>% summarize(val=sum(value_usd/1e6)) %>% pull(val) %>% max()
g2 <- ggplot(squid, aes(x=year, y=value_usd/1e6, fill=state)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  annotate(geom="text", label="MHW", x=2015, y=ymax2*1.05, size=2.1) +
  # Plot PACFIN value
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y="Revenues\n(USD millions)", title="Commercial market squid fishery", tag="B") +
  scale_fill_discrete(name="State") +
  scale_x_continuous(lim=c(1980, 2022)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8),
        legend.key.size = unit(0.3, "cm"))
g2

# Bluefin
ymax3 <- bluefin %>% group_by(year) %>% summarize(val=sum(landings_n/1e3)) %>% pull(val) %>% max()
g3 <- ggplot(bluefin, aes(x=year, y=landings_n/1e3, fill=region)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90") +
  annotate(geom="text", label="MHW", x=2015, y=ymax3*1.05, size=2.1) +
  # Plot PACFIN value
  geom_bar(stat="identity", color="grey30", lwd=0.2) +
  # Labels
  labs(x="", y="Landings\n(1000s of tuna)",
       title="Recreational Pacific bluefin fishery", tag="C") +
  scale_fill_discrete(name="Source waters") +
  scale_x_continuous(lim=c(1980, 2022)) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = c(0.2, 0.8),
        legend.key.size = unit(0.3, "cm"))
g3


# Merge
g <- gridExtra::grid.arrange(g1, g2, g3, ncol=2)
g



# Plot data
################################################################################

# Export plot
ggsave(g, filename=file.path(plotdir, "Fig10_positive_impacts.png"),
       width=6.5, height=4.5, units="in", dpi=600)



