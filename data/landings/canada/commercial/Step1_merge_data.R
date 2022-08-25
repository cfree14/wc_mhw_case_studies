

# Clear workspace
rm(list = ls())

# Setup
################################################################################

# Packages
library(lubridate)
library(tidyverse)

# Directories
indir <- "data/landings/canada/commercial/raw"

# Files to merge
files2merge <- list.files(indir)


# Merge and format
################################################################################

# Loop through and merge
x <- files2merge[1]
data_orig <- purrr::map_df(files2merge, function(x){

  # Read data
  fdata_orig <- readxl::read_xls(file.path(indir, x), skip=2) %>%
    mutate_all(as.character) %>%
    mutate(filename=x)

})

# Format
data <- data_orig %>%
  # Rename
  rename(comm_name=Species) %>%
  # Add year
  mutate(year=gsub("pv_e.xls|s", "", filename) %>% as.numeric()) %>%
  # Eliminate empty rows
  filter(!is.na(`Total Canada`)) %>%
  # Gather
  select(-filename) %>%
  select(year, everything()) %>%
  gather(key="province", value="value", 3:ncol(.)) %>%
  # Mark confidential
  mutate(confidential_flag=ifelse(value=="x", "***", "")) %>%
  # Format value
  mutate(value=gsub("x", "", value) %>% as.numeric()) %>%
  # Remove totals
  filter(!grepl("Total", province)) %>%
  # Recode province
  mutate(province=recode(province,
                         "PEI"="Prince Edward Island",
                         "NFLD"="Newfoundland"))

# Inspect
table(data$province)






