## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Created: 08/31/2026
## Updated: 10/05/2026

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
raw_data <- read_csv2(here("data/raw/prs_survey_data_20260510.csv"))

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
  select(!c(
    logos_note,info_intro, info_contact, consent_header, consent_declined, #NA
            thank_you_for_your_i_s_study_at_this_time, #thank you note
            starts_with("note_"),
            167:176 #id
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

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Filter for weeding questions ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
consent_ssa <- clean_data_2 %>% 
  filter(consent == "yes", # Consent to study
         practicing_ssa == "yes",
         country_practice != "Switzerland") # practice in SSA

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Clean categorical variables ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean the categorical variables
clean_cv_data <- consent_ssa %>% # Clean age group
  mutate(
    age_group = case_when(
      age_group == "lt30" ~ "<30",
      age_group == "30_39" ~ "30-39",
      age_group == "40_49" ~ "40-49",
      age_group == "50_59" ~ "50-59",
      age_group == "gte60" ~ ">=60"
    ),
    # Calculate years accredited ~ years in practice
    year_prs_accredited = 2026 - accreditation_year 
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save clean dataset ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save dataset
write_csv(clean_cv_data, "data/clean_data_20261005.csv")
