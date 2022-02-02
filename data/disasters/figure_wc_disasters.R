
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(tidyverse)

# Directories
indir <- "data/disasters/raw"
outdir <- "data/disasters/processed"
plotdir <- "data/disasters/figures"

# Read data
data_orig <- readRDS(file=file.path(outdir, "1989_2021_federal_disaster_database.Rds"))

# Build data
################################################################################

# Build data
data <- data_orig %>% 
  # WC disasters
  filter(region %in% c("Alaska", "West Coast")) %>% 
  # Simplify
  select(disaster_id, disaster_year1, region, state, fishery, fishery_type1, fishery_type2, determination, cause_catg_simple) %>% 
  # Format causes
  rename(cause_catg=cause_catg_simple) %>% 
  mutate(cause_catg=ifelse(cause_catg=="", "Not officially determined", cause_catg),
         cause_catg=recode(cause_catg, 
                           "Human Causes"="Human", 
                           "Environmental Anomalies"="Environmental",
                           "Human Causes and Environmental Anomalies"="Human/environmental",
                           "Not officially determined"="Not officially determined")) %>%
  # Add species
  mutate(species=recode(fishery,
                        "Chinook"="Chinook salmon",                                       
                        "Chinook and Chum Salmon"="Chinook salmon",                       
                        "Chinook Salmon"="Chinook salmon",                                
                        "Chinook, Chum, Coho, Pink"="Chinook salmon, Chum salmon, Coho salmon, Pink salmon",                       
                        "Chinook, Coho, Chum, Sockeye (all ocean-specific)"="",
                        "Chinook, Sockeye, Coho, Pink, Chum"="",         
                        "Chum"="Chum salmon",                                        
                        "Coastal Salmon"="Salmon",                               
                        "Coho and Chinook (Ocean Troll)"="Coho salmon, Chinook salmon",                 
                        "Coho and Pink Salmon"="Coho salmon, Pink salmon",                         
                        "Coho Salmon"="Coho salmon",                                   
                        "Coho, Chinook, Chum "="Coho salmon, Chinook salmon, Chum salmon",                      
                        "Dungeness and Rock Crab"="Dungeness crab, Rock crab",                      
                        "Dungeness Crab"="Dungeness crab",                          
                        "Groundfish (multispecies)"="Groundfish",                     
                        "Ocean Salmon Troll"="Salmon",                          
                        "Pacific Cod"="Pacific cod",                                  
                        "Pacific Sardine"="Pacific sardine",                               
                        "Pink Salmon"="Pink salmon",                                    
                        "Puget Sound Coho Salmon"="Coho salmon",                      
                        "Red King Crab"="Red king crab",                               
                        "Red Sea Urchin"="Red sea urchin",                                
                        "Salmon"="Salmon",                                     
                        "salmon, Pacific cod, Tanner Crab"="Salmon, Pacific cod, Tanner crab",            
                        "Scallop"="Scallop",                                
                        "Snow Crab"="Snow crab",                             
                        "Sockey, Chum, Coho, Pink"="",                     
                        "Sockeye Salmon"="Sockeye salmon",                              
                        "Sockeye, Chum"="Sockeye salmon",   
                        "Sockeye, Chum, Coho"="Sockeye salmon, Chum salmon, Coho salmon",                              
                        "Spring Chinook"="Chinook salmon",                                    
                        "Tanner Crab"="Tanner crab")) %>% 
  # Arrange
  select(disaster_id, disaster_year1, region, state, fishery, species, 
         fishery_type1, fishery_type2, determination, cause_catg, everything())


# Expand data
data_exp <- purrr::map_df(1:nrow(data), function(x){
  
  # Extract row
  row <- data[x, ]
  
  # Extract and expand species
  species_chr <- row$species
  species_vec <- strsplit(species_chr, split=", ") %>% unlist()
  nspp <- length(species_vec)
  
  # Duplicate rows
  row_dupl <- rbind(row[rep(1, nspp), ], make.row.names=F) %>% 
    mutate(species=species_vec)
  
})

# Species order
species_order <- c("Snow crab", "Tanner crab", "Red king crab", "Rock crab", "Dungeness crab", "Red sea urchin", "Scallop",
                   "Groundfish", "Pacific cod", "Pacific sardine", 
                   "Salmon", "Pink salmon", "Coho salmon", "Chum salmon", "Chinook salmon", "Sockeye salmon")

# Build stats
stats <- data_exp %>% 
  # Summarize
  group_by(region, species, disaster_year1, cause_catg, determination) %>% 
  summarize(n_disasters=n()) %>% 
  ungroup() %>% 
  # Add taxa
  mutate(taxa_group=ifelse(grepl("salmon", tolower(species)), "Salmon",
                           ifelse(grepl("urchin|crab|scallop", tolower(species)), "Invertebrate", "Other"))) %>% 
  # Factor cause
  mutate(cause_catg=factor(cause_catg, levels=c("Human", 'Environmental', "Human/environmental", "Not officially determined"))) %>% 
  # Factor determination
  mutate(determination=recode(determination, "Partially Approved"="Partially approved"),
         determination=factor(determination, levels=c("TBD", "Denied", "Partially approved", "Approved"))) %>% 
  # Order species
  mutate(species=factor(species, levels=rev(species_order)))


# Plot data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   # Gridlines
                   panel.grid.major.x = element_blank(), 
                   panel.grid.minor.x = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
g <- ggplot(stats, aes(x=disaster_year1, y=species, 
                       color=cause_catg, size=n_disasters, shape=determination)) +
  # Facet
  facet_grid(taxa_group~region, space="free", scales="free_y") +
  # Plot MHW years
  geom_rect(xmin=2014.5, xmax=2018.5, ymin=1, ymax=20, fill="grey90", inherit.aes = F) +
  # Plot disasters
  geom_point() +
  # Axis
  scale_x_continuous(breaks=seq(1990,2020,5)) +
  # Labels
  labs(x="Year", y="") +
  # Legends
  scale_color_discrete(name="Diaster cause") +
  scale_size_continuous(name="Number of disasters", breaks=c(1,2,3), range=c(1.5,4)) +
  scale_shape_manual(name="Status", values=c(3, 4, 1, 16)) +
  guides(color = guide_legend(order = 2), size = guide_legend(order = 1), shape = guide_legend(order = 3))  +
  # Theme
  theme_bw() + my_theme
g

# Export plot
ggsave(g, filename=file.path(plotdir, "wc_federal_fisheries_disasters.png"), 
       width=6.5, height=4, units="in", dpi=600)




