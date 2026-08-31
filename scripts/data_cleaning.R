## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Created: 08/31/2026
## Updated: 08/31/2026

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, here, stringr, gt, readxl, purrr, 
               writexl, digest)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load CSV data

## raw data (only one version)
raw_data <- read_csv2(here("data/raw/prs_survey_data_PILOT.csv"))

## raw data (all versions including more than one facility data)
# TBA

# Clean names
raw_data <- clean_names(raw_data)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Deidentify emails ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

anonymous_data <- raw_data %>%
  mutate(
    # Normalize email so capitalization/extra spaces do not affect matching
    email_normalized = email %>%
      str_trim() %>%
      str_to_lower(),
    
    # Create non-readable duplicate identifier
    email_id = if_else(
      is.na(email_normalized) | email_normalized == "",
      NA_character_,
      vapply(
        email_normalized,
        digest,
        FUN.VALUE = character(1),
        algo = "sha256",
        serialize = FALSE
      )
    )
  ) %>%
  
  # Remove identifiable email fields from analysis data
  select(email_id, everything(), -email, -email_normalized)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Cleaning columns ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Drop columns not used or just info
clean_data <- anonymous_data %>% 
  select(!c(2:9, #id
            17, #thank you note
            167:178 #id end
            ))

# Drop binary columns (repeat)
# clean_data_1 <- clean_data %>% 
#   select(!c(country_currently_angola:country_currently_zimbabwe, #country
#             subspecialty_burns:subspecialty_general_reconstructive, #subspecialty
#             workload_categories_burns:workload_categories_aesthetic)) #workload

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Separate multiple answers with ; ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Function to replace spaces with ;
clean_data_2 <- clean_data %>% 
  mutate(
    across(
      c(country_currently, subspecialty, workload_categories),
      ~ str_replace_all(.x, " ", ";")
    )
  )


