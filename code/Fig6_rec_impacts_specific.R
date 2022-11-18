
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
data <- data_orig %>%
  # Reduce to years of interest
  filter(year %in% 2011:2019) %>%
  # Simplify
  select(state, comm_name, year, retained_n) %>%
  # Fill in zeros (ASSUMES THAT NO DATA MEANS NO CATCH)
  complete(state, comm_name, year, fill=list(retained_n=0)) %>%
  # Summarize by year (across modes)
  group_by(state, comm_name, year) %>%
  summarize(retained_n=sum(retained_n, na.rm=T)) %>%
  ungroup() %>%
  # Limit BC to
  filter(state!="British Columbia" | (state=="British Columbia" & year>2012)) %>%
  # Add period
  mutate(period=cut(year, breaks=c(2010, 2013, 2016, 2020), labels=c("Before", "During", "After"), right=T)) %>%
  # Summarize by state, species, period
  group_by(state, comm_name, period) %>%
  summarize(retained_n=mean(retained_n, na.rm=T)) %>%
  ungroup() %>%
  # Calculate percent difference from during
  group_by(state, comm_name) %>%
  mutate(retained_n_pre=retained_n[period=="Before"],
         retained_n_pdiff=(retained_n - retained_n_pre) / retained_n_pre * 100,
         retained_n_pdiff_cap=pmin(retained_n_pdiff, 200)) %>%
  ungroup() %>%
  # Remove states without any before catch
  group_by(state, comm_name) %>%
  mutate(use_yn=ifelse(retained_n[period=="Before"]==0, "no", "yes")) %>%
  # Remove state-species without before/during
  mutate(use_yn=ifelse( is.na(retained_n[period=="Before"]) & is.na(retained_n[period=="During"]), "no", use_yn)) %>%
  ungroup() %>%
  # Filter
  filter(use_yn=="yes") %>%
  # Factor period
  mutate(period=factor(period, levels=c("Before", "During", "After"))) %>%
  # Complete values
  select(-use_yn) %>%
  complete(state, comm_name, period, fill=list(retained_n=NA)) %>%
  # Add taxa info
  left_join(spp_key) %>%
  # Filter to species
  filter(level=="species") %>%
  # Add state abbrevation
  mutate(state_abbrev=recode_factor(state,
                             "California"="CA",
                             "Oregon"="OR",
                             "Washington"="WA",
                             "British Columbia"="BC",
                             "Alaska"="AK")) %>%
  # Rename a few species
  mutate(comm_name=recode(comm_name,
                          "Pacific pompano (butterfish)"="Pacific pompano",
                          "Pacific (chub) mackerel"="Pacific chub mackerel")) %>%
  # Format categories
  mutate(taxa_catg=recode(taxa_catg,
                          "Coastal pelagics"="Coastal\npelagics",
                          "Gobies, blennies"="Gobies,\nblennies",
                          "Large pelagics"="Large\npelagics",
                          "Roundfish"="Roundfish,\nsculpins",
                          "Sculpins"="Roundfish,\nsculpins",
                          "Other invertebrates"="Other fish"))

# Before values
before <- data %>%
  filter(period=="Before") %>%
  select(state, comm_name, retained_n)

# Plot histogram
cutoff <- 0
ggplot(before, aes(x=retained_n)) +
  geom_histogram(binwidth=10000 ) +
  geom_vline(xintercept = cutoff ) +
  theme_bw()

# Subset data to use
data_use <- data %>%
  filter(retained_n_pre >= cutoff | is.na(retained_n_pre))

# Determine species order
spp_order <- data_use %>%
  filter(period=="During") %>%
  group_by(taxa_catg, comm_name) %>%
  summarize(pdiff_avg=mean(retained_n_pdiff, na.rm=T)) %>%
  ungroup() %>%
  arrange(taxa_catg, pdiff_avg)

# Order data
data_use_ordered <- data_use %>%
  # Order common names
  mutate(comm_name=factor(comm_name, levels=spp_order$comm_name))


# Plot data - long format
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=5.5),
                   axis.title=element_text(size=8),
                   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=6.5),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))


# Plot data
g <- ggplot(data_use_ordered, aes(x=period, y=comm_name, fill=retained_n_pdiff_cap)) +
  facet_grid(taxa_catg~state, scales="free_y", space="free_y", drop=F) +
  geom_tile() +
  # Plot points
  geom_point(data=data_use_ordered %>% filter(period=="Before" & retained_n>0),
             mapping=aes(x=period, y=comm_name, size=retained_n/1e3), inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Color legend
  scale_fill_gradient2(name="% difference", midpoint = 0, mid="white", high="navy", low="darkred", na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Size legend
  scale_size_continuous(name="Mean annual\npre-MHW landings\n(1000s of fish)") +
  # Theme
  theme_bw() + my_theme
g

# Export data
# ggsave(g, filename=file.path(plotdir, "Fig6_rec_impacts_long.png"),
#        width=4.5, height=11, units="in", dpi=600)



# Plot data - wide format
################################################################################

# Categories 1
catgs1 <- c("Rockfish", "Flatfish", "Sharks, skates, rays", "Salmon", "Crustaceans")

# Break data into two
data1 <- data_use_ordered %>%
  filter(taxa_catg %in% catgs1)
data2 <- data_use_ordered %>%
  filter(!taxa_catg %in% catgs1)

# Plot first panel
g1 <- ggplot(data1, aes(x=period, y=comm_name, fill=retained_n_pdiff_cap)) +
  facet_grid(taxa_catg~state_abbrev, scales="free_y", space="free_y", drop=F) +
  geom_tile() +
  # Plot points
  geom_point(data=data1 %>% filter(period=="Before" & retained_n>0),
             mapping=aes(x=period, y=comm_name, size=retained_n/1e3), inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Color legend
  scale_fill_gradient2(name="% difference", midpoint = 0, mid="white", high="navy", low="darkred", na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Size legend
  scale_size_continuous(name="Mean annual\npre-MHW landings\n(1000s of fish)") +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "none")
g1

# Plot second panel
g2 <- ggplot(data2, aes(x=period, y=comm_name, fill=retained_n_pdiff_cap)) +
  facet_grid(taxa_catg~state_abbrev, scales="free_y", space="free_y", drop=F) +
  geom_tile() +
  # Plot points
  geom_point(data=data2 %>% filter(period=="Before" & retained_n>0),
             mapping=aes(x=period, y=comm_name, size=retained_n/1e3), inherit.aes = F) +
  # Labels
  labs(x="", y="") +
  # Color legend
  scale_fill_gradient2(name="% difference\nfrom pre-MHW\nlandings", midpoint = 0, mid="white", high="navy", low="darkred", na.value="grey90") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Size legend
  scale_size_continuous(name="Mean annual\npre-MHW landings\n(1000s of fish)") +
  # Theme
  theme_bw() + my_theme
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, ncol=2, widths=c(0.4, 0.6))
g

# Export
ggsave(g, filename=file.path(plotdir, "Fig6_rec_impacts_wide.png"),
       width=8, height=8, units="in", dpi=600)





