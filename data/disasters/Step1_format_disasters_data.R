
# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(ggsankey)
library(tidyverse)

# Directories
indir <- "data/disasters/raw"
outdir <- "data/disasters/processed"
plotdir <- "data/disasters/figures"

# Read data
# data_orig <- readxl::read_excel(file.path(indir, "FisheryDisasters_1989-2021.xlsx"), col_types = "text")
data_orig <- read.csv(file.path(indir, "FisheryDisasters_1989-2021.csv"), as.is=T, na.strings = c("", "N/A", "?"))


# Format data
################################################################################

# Many things to do on this dataset
# Format dates
# Recalculate lags
# Format disaster years
# Add N_states and N_years so N_state_years makes sense
# Better categorize causes
# Mark decided vs. pending
# Convert NE and GOM to states- check against state years?

# Format data
data <- data_orig %>%
  # Rename columns
  janitor::clean_names("snake") %>%
  rename(extra_notes=x,
         disaster_id=disaster_number,
         disaster_years=year,
         disaster_year1_orig=year_1,
         region=management_zone,
         area_season=area_season_affected,
         mgmt_type=state_federal,
         sector_type_orig=comm_rec_tribal,
         requesters=requester_s,
         request_dates=request_date,
         request_year_lag_yrs=request_year_disaster_year,
         determination=secretary_of_commerce_determination,
         determination_dates=determination_date,
         determination_dates_lag_days=determination_lag_d,
         appropriation_amount_usd=appropriation_amount,
         appropriation_amount_usd_2019=appropriation_amount_2019_usd,
         correction_factor_2019usd=us_bls_correction_factor_jan_oct_2019,
         cause_formal=formal_federally_stated_cause,
         cause=cause_of_disaster,
         cause_simplified=cause_simplified,
         cause_catg=cause_more_simplified,
         damages_est_usd=total_est_damages) %>%
  # Remove useless rows
  filter(state!="REPEAT OF NO. 110") %>%
  # Format region
  mutate(region=gsub(" Region", "", region)) %>%
  # Format state
  mutate(state=stringr::str_trim(state),
         state=recode(state,
                      "Alaska"="AK",
                      "American Samoa"="AS",
                      "California"="CA",
                      "Florida"="FL",
                      "Florida, USVI, Puerto Rico"="FL, VI, PR",
                      "Georgia"="GA",
                      "Georgia and South Carolina"="GA, SC",
                      "Guam and Northern Mariana Islands"="GU, MP",
                      "Hawaii"="HI",
                      "Louisiana"="LA",
                      "Louisiana, Mississippi, Alabama"="LA, MS, AL",
                      "LA, MS, AL, and FL"="LA, MS, AL, FL",
                      "MA, ME, NH, CT, RI, and NY"="MA, ME, NH, CT, RI, NY",
                      "Maine"="ME",
                      "Massachusetts"="MA",
                      "Mississippi"="MS",
                      "New Hampshire"="NH",
                      "New Jersey and New York"="NJ, NY",
                      "New York"="NY",
                      "North Carolina"="NC",
                      "Oregon"="OR",
                      "Oregon and California"="OR, CA",
                      "Rhode Island"="RI",
                      "Texas"="TX",
                      "Virginia"="VA",
                      "Washington"="WA")) %>%
  # Format management type,
  mutate(mgmt_type=recode(mgmt_type,
                          "Both"="Federal/State",
                          "Federal and State"="Federal/State",
                          "Federal, State"="Federal/State",
                          "Federal, Tribal"="Federal/Tribal")) %>%
  # Format sector type
  mutate(sector_type_orig=stringr::str_trim(sector_type_orig),
         sector_type=gsub("\\s*\\([^\\)]+\\)", "", sector_type_orig)) %>%
  mutate(sector_type=recode(sector_type,
                            "Commercial and Recreational"="Commercial, Recreational",
                            "Commercial and Tribal"="Commercial, Tribal",
                            "Tribal / Commercial"="Commercial, Tribal",
                            "Swinomish, Lummi, Upper Skagit"="Tribal",
                            "Port Gamble S'Klallam"="Tribal",
                            "Commercial, Recreational, Subsistence, Other Fishing Industry"="All")) %>%
  # Add tribes
  mutate(tribes=gsub("(?<=\\()[^()]*(?=\\))(*SKIP)(*F)|.", "", sector_type_orig, perl=T)) %>%
  mutate(tribes=ifelse(sector_type_orig %in% c("Port Gamble S'Klallam", "Swinomish, Lummi, Upper Skagit"), sector_type_orig, tribes),
         tribes=recode(tribes, "including Chignik"="Chignik")) %>%
  # Format disaster year
  mutate(disaster_year1=substr(disaster_year1_orig, 1, 4) %>% as.numeric(.)) %>%
  # Format request date
  #
  # Format request year
  mutate(request_year=as.numeric(request_year)) %>%
  # Convert appropriation amounts
  mutate(appropriation_amount_usd=appropriation_amount_usd %>% gsub("\\(cont. from 2008-2009\\)", "", .) %>% stringr::str_trim(),
         appropriation_amount_usd=appropriation_amount_usd %>% gsub("\\$", "", .) %>% gsub(",", "", .) %>% as.numeric(),
         appropriation_amount_usd_2019=appropriation_amount_usd_2019 %>% gsub("\\$", "", .) %>% gsub(",", "", .) %>% as.numeric()) %>%
  # Arrange
  select(disaster_id,
         disaster_years, disaster_year1_orig, disaster_year1,
         region, state, area_season,
         state_years,
         mgmt_type, sector_type, tribes,
         fishery,
         requesters, request_year, request_dates, request_year_lag_yrs, request_letter,
         determination, determination_year, determination_dates, determination_dates_lag_days,
         determination_authority, determination_letter, press_release,
         funding_authority, correction_factor_2019usd, appropriation_amount_usd, appropriation_amount_usd_2019, damages_est_usd,
         cause_formal, cause, cause_simplified, cause_catg,
         notes,
         everything()) %>%
  # Remove useless columns
  select(-c(freq, sector_type_orig, -x_1)) %>%
  # Arrange
  arrange(disaster_id)

# Is ID unique?
anyDuplicated(data$disaster_id)

# Inspect data
str(data)
freeR::complete(data)
colnames(data)


##################################


# Geography
table(data$region)
table(data$state)
# Fishery type
table(data$mgmt_type)
table(data$sector_type)
sort(unique(data$tribes))
sort(unique(data$fishery))
# Determination
table(data$determination)
table(data$determination_authority)
table(data$funding_authority)
# Causes
sort(unique(data$cause_formal))
sort(unique(data$cause))
table(data$cause_catg)
table(data$cause_catg_simple)

# Export data
saveRDS(data, file=file.path(outdir, "1989_2021_federal_disaster_database.Rds"))

