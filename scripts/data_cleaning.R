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
clean_cv_data <- consent_ssa %>% 
  mutate(
    
    # Clean age group
    age_group = case_when(
      age_group == "lt30" ~ "<30",
      age_group == "30_39" ~ "30-39",
      age_group == "40_49" ~ "40-49",
      age_group == "50_59" ~ "50-59",
      age_group == "gte60" ~ ">=60"
    ),
    
    # Calculate years accredited ~ years in practice
    year_prs_accredited = 2026 - accreditation_year ,
    
    # Standarize country practice
    country_practice = str_trim(str_to_lower(country_practice)),
    country_practice = case_when(
      country_practice == "côte d’ivoire" ~ "cote_divoire",
      country_practice %in% c("drc", "dr congo") ~
        "democratic_republic_of_the_congo",
      TRUE ~ country_practice
    )
)

# Create factors for categorical variables
clean_cv_data <- clean_cv_data %>% 
  mutate(
    # Factorize age_group
    age_group = factor(
      age_group,
      levels = c("<30", "30-39", "40-49", "50-59", ">=60")),
    # Factorize gender
    gender = factor(
      gender,
      levels = c("male", "female"),
      labels = c("Male", "Female")),
    # Factorize qualification
    qualification = factor(
      qualification,
      levels = c("doctoral_degree", "surgical_fellowship", "masters_surgery", 
                 "surgical_membership", "primary_medical_degree"),
      labels = c("Doctoral/research degree", "Fellowship", "Master’s degree in Surgery", 
                 "Membership qualification", "Primary medical degree"))
  )

# Create factors
df <- df %>% 
  mutate(
    
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Country of practice ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract unique countries from each column
clean_country_practice <- clean_cv_data %>% 
  separate_rows(country_currently, sep = ";") %>% 
  mutate(country_currently = str_trim(country_currently))

# # Country of practice vs country currently practicing
# unique(clean_cv_data$country_practice)
# unique(clean_country_practice$country_currently)
# 
# # Countries in country_practice but NOT in country_currently
# setdiff(
#   unique(clean_cv_data$country_practice),
#   unique(clean_country_practice$country_currently)
#   )
#   
# # Countries in country_currently but NOT in country_practice
# setdiff(
#   unique(clean_country_practice$country_currently),
#   unique(clean_cv_data$country_practice)
# )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save clean dataset ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save dataset
# write_csv(clean_cv_data, "data/clean_data_20261005.csv")
