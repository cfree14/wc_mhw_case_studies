
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(wcfish)
library(tidyverse)

# Directories
datadir <- "data/case_study_data/bluefin/lyall"

# Read data
data_orig <- read.table(file.path(datadir, "WON_Bluefin_Data_1968-2019.txt"), header = TRUE, sep = "\t")


# Format and analzye data
################################################################################

# Prep data
######################################

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>%
  # Remove useless
  select(-species) %>%
  # Adjust year
  mutate(year_adj=year-min(year)+1)

# Parameters
n_fish <- nrow(data)
n_yr <- max(data$year_adj)

# Data
years <- data$year_adj
weights <- data$weight_kg


# Fit model
######################################

# Specify model in BUGS
sink("ssm.jags")
cat("
    model {
    # Priors and constraints
    N.est[1] ~ dunif(0, 100)            # Prior for initial yearly max size
    mean.mu ~ dunif(-10, 10)          # Prior for mean rate of change
    sigma.proc ~ dunif(0, 10)           # Prior for sd of state process
    sigma2.proc <- pow(sigma.proc, 2)
    tau.proc <- pow(sigma.proc, -2)
    sigma.obs ~ dunif(0, 100)           # Prior for sd of observation process
    sigma2.obs <- pow(sigma.obs, 2)
    tau.obs <- pow(sigma.obs, -2)

    # Likelihood
    # State process
    for (t in 1:(n_yr-1)){
    mu[t] ~ dnorm(mean.mu, tau.proc)
    N.est[t+1] <- N.est[t] + mu[t]
    }

    # Observation process
    for (i in 1:N.bluef) {
    weights[i] ~ dnorm(N.est[years[i]], tau.obs)
    }
    }
    ",fill = TRUE)
sink()

# Bundle data
jags.data <- list(weights=weights,
                  years=years,
                  N.bluef=n_fish,
                  n_yr=n_yr)

# Parameters monitored
parameters <- c("mu", "mean.mu", "sigma2.obs", "sigma2.proc", "N.est")

# MCMC settings
ni <- 25000
nt <- 3
nb <- 10000
nc <- 3

# Call JAGS from R (BRT <1 min)
ssm <- R2jags::jags(jags.data,
                   inits=NULL,
                   parameters, "ssm.jags",
                   n.chains = nc, n.thin = nt, n.iter = ni, n.burnin = nb, working.directory = getwd())

# Save model
saveRDS(ssm, file=file.path(datadir, "model.Rds"))





# Plot data
################################################################################

# Read model fit
ssm <- readRDS(file=file.path(datadir, "model.Rds"))

# Extract year estimate
est <- ssm$BUGSoutput$summary %>%
  # Convert to dataframe
  as.data.frame() %>%
  rownames_to_column(var = "param") %>%
  # Reduce to weight estimates
  filter(grepl("N.est", param)) %>%
  # Add year
  mutate(year_adj=1:n(),
         year=year_adj-1+min(data$year)) %>%
  # Arrange
  select(-param) %>%
  select(year_adj, year, everything())

# Theme
my_theme <- theme(axis.text=element_text(size=7),
                  axis.title=element_text(size=8),
                  # Gridlines
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.line = element_line(colour = "black"))


# Plot
g <- ggplot(data, aes(x=year, y=weight_kg, group=year)) +
  # MWH
  geom_rect(xmin=2013.5, xmax=2016.5, ymin=0, ymax=Inf, fill="coral", alpha=0.3, inherit.aes = F) +
  # Data
  geom_jitter(color="grey40", width=0.2) +
  # Model fit
  geom_ribbon(data=est, mapping=aes(x=year, ymin=`2.5%`, ymax=`97.5%`), inherit.aes = F, fill="grey50", alpha=0.5) +
  geom_line(data=est, mapping=aes(x=year, y=`50%`), inherit.aes = F, lwd=0.8) +
  # Labels
  labs(x="Year", y="Weight (kg)") +
  scale_x_continuous(breaks=seq(1970, 2020, 5)) +
  # Theme
  theme_bw() + my_theme
g

# Export
ggsave(g, filename=file.path(datadir, "FigS1_bluefin_tuna_size.png"),
       width=6.5, height=3, units="in", dpi=600)







