
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
plotdir <- "figures"

# Read data
data_orig <- read.csv("data/case_study_data/anchovy/mean larval and yoy anchovy.csv")

# Format data
data1 <- data_orig %>%
  mutate(variable=case_when(variable=="ln mean yoy anchovy from southern CA" ~ "YOY",
                            T ~ "Larval abundance"))

# Format data
data2 <- read.csv("data/case_study_data/anchovy/spring anchovy from core CalCOFI stations.csv")  %>%
  select(-X) %>%
  setNames(c("year", "n", "n_log"))



# Plot data
################################################################################

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.title=element_text(size=8),
                   axis.title.x=element_blank(),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   plot.tag = element_text(size=9),
                   plot.tag.position = "topleft",
                   # Gridlines
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(),
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

# Plot data
# data1 <- data %>% filter(variable=="Larval abundance")
g1 <- ggplot(data2, aes(x=year, y=n)) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90", show.legend = F) +
  annotate(geom="text", label="MHW", x=2015, y=max(data1$value)*1.05, size=2.1) +
  # Data
  geom_line() +
  # Labels
  labs(x="Year", y="Larval abundance\n(CalCOFI spring survey)", tag="A") +
  scale_x_continuous(breaks=seq(1950,2020,5), lim=range(data2$year)) +
  # Theme
  theme_bw() + my_theme
g1

# Plot data
data3 <- data1 %>% filter(variable=="YOY")
g2 <- ggplot(data3, aes(x=year, y=exp(value))) +
  # Label heatwave
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="grey90", show.legend = F) +
  annotate(geom="text", label="MHW", x=2015, y=max(data3$value %>% exp())*1.05, size=2.1) +
  # Data
  geom_line() +
  # Labels
  labs(x="Year", y="Young of the year abundance\n(RREAS S. California survey)", tag="B") +
  scale_x_continuous(breaks=seq(1950,2020,5), lim=range(data2$year)) +
  # Theme
  theme_bw() + my_theme
g2

# Merge plots
g <- gridExtra::grid.arrange(g1, g2, ncol=1)

# Export plot
ggsave(g, filename=file.path(plotdir, "FigS3_anchovy_case_study.png"),
       width=6.5, height=4, units="in", dpi=600)
ggsave(g, filename=file.path(plotdir, "FigS3_anchovy_case_study.pdf"),
       width=6.5, height=4, units="in", dpi=600)


