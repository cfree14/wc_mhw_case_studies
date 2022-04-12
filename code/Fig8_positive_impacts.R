
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
plotdir <- "figures"

# Read PACFIN data
pacfin_orig <- wcfish::pacfin_all6
cpfv_orig <- wcfish::cdfw_cpfv

# Read GEMM data
gemm_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/consulting/halibut_bycatch/data/gemm/processed/GEMM_2002_2020_data.Rds")


# Build data
################################################################################

# Build shortbelly catch
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

# Build shortbelly cath
shortbelly2 <- pacfin_orig %>%
  # Reduce to shortbelly rockfish
  filter(comm_name %in% c("Shortbelly rockfish", "Nom. Shortbelly rockfish")) %>%
  # Summarize by total
  group_by(year) %>%
  summarize(landings_mt=sum(landings_mt)) %>%
  ungroup()

# Build market squid catch
squid <- pacfin_orig %>%
  # Reduce to squid
  filter(comm_name %in% c("Market squid")) %>%
  # Summarize by total
  group_by(state, year) %>%
  summarize(landings_mt=sum(landings_mt),
            value_usd=sum(revenues_usd)) %>%
  ungroup()

# Urchin
urchin <- pacfin_orig %>%
  # Reduce to squid
  filter(comm_name %in% c("Red sea urchin")) %>%
  # Summarize by total
  group_by(state, year) %>%
  summarize(landings_mt=sum(landings_mt),
            value_usd=sum(revenues_usd)) %>%
  ungroup()

# Sardine
sardine <- pacfin_orig %>%
  # Reduce to squid
  filter(comm_name %in% c("Pacific sardine")) %>%
  # Summarize by total
  group_by(state, year) %>%
  summarize(landings_mt=sum(landings_mt),
            value_usd=sum(revenues_usd)) %>%
  ungroup()

# Bluefin
bluefin <- cpfv_orig %>%
  # Bluefin
  filter(comm_name=="Bluefin tuna") %>%
  # Summarize by total
  group_by(year) %>%
  summarize(landings_n=sum(landings_n)) %>%
  ungroup()



# Plot data
################################################################################

# Plot data
g1 <- ggplot(shortbelly1, aes(x=year, y=catch_mt, fill=catch_type)) +
  geom_bar(stat="identity") +
  theme_bw()
g1

# Plot data
g1 <- ggplot(shortbelly2, aes(x=year, y=landings_mt)) +
  geom_bar(stat="identity") +
  theme_bw()
g1


# Plot squid data
g1 <- ggplot(squid, aes(x=year, y=value_usd/1e6, fill=state)) +
  geom_bar(stat="identity") +
  # Landings
  labs(x="", y="Revenues (USD millions)") +
  # Theme
  theme_bw()
g1

# Plot urchin data
g1 <- ggplot(urchin, aes(x=year, y=value_usd/1e6, fill=state)) +
  geom_bar(stat="identity") +
  # Landings
  labs(x="", y="Revenues (USD millions)") +
  # Theme
  theme_bw()
g1

# Plot sardine data
g1 <- ggplot(sardine, aes(x=year, y=value_usd/1e6, fill=state)) +
  geom_bar(stat="identity") +
  # Landings
  labs(x="", y="Revenues (USD millions)") +
  # Theme
  theme_bw()
g1

# Plot bluefin
g1 <- ggplot(bluefin, aes(x=year, y=landings_n/1000)) +
  geom_bar(stat="identity") +
  # Landings
  labs(x="", y="Bluefin landings\n(1000s of fish)") +
  # Theme
  theme_bw()
g1



