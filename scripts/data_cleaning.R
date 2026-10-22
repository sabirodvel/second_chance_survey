## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Created: 08/31/2026
## Updated: 10/22/2026

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
## ID & Deidentify emails ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

anonymous_data <- raw_data %>%
  mutate(
    
    # Assign a unique ID to each row
    id = row_number(),
    
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

# Function to replace spaces with ;
clean_data_2 <- clean_data %>%
  mutate(
    across(
      c(country_currently, subspecialty, workload_categories),
      ~ str_replace_all(.x, " ", ";")
    ),
    
    # Standardize country names
    country_practice = case_when(
      str_to_lower(str_squish(country_practice)) %in%
        c("drc", "dr congo") ~ "Democratic Republic of the Congo",
      
      str_to_lower(str_squish(country_practice)) %in%
        c("côte d’ivoire", "côte d'ivoire") ~ "Côte d'Ivoire",
      
      TRUE ~ str_to_title(str_squish(country_practice))
    )
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Filter for weeding questions ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Define countries in Sub-Saharan Africa
ssa_countries <- c(
  "Angola", "Benin", "Botswana", "Burkina Faso", "Burundi",
  "Cabo Verde", "Cameroon", "Central African Republic", "Chad",
  "Comoros", "Congo", "Democratic Republic of the Congo",
  "Côte d'Ivoire", "Djibouti", "Equatorial Guinea", "Eritrea",
  "Eswatini", "Ethiopia", "Gabon", "Gambia", "Ghana", "Guinea",
  "Guinea-Bissau", "Kenya", "Lesotho", "Liberia", "Madagascar",
  "Malawi", "Mali", "Mauritania", "Mauritius", "Mozambique",
  "Namibia", "Niger", "Nigeria", "Rwanda", "São Tomé and Príncipe",
  "Senegal", "Seychelles", "Sierra Leone", "Somalia",
  "South Africa", "South Sudan", "Sudan", "Tanzania", "Togo",
  "Uganda", "Zambia", "Zimbabwe"
)

# Filter eligible participants
consent_ssa <- clean_data_2 %>%
  filter(
    consent == "yes",
    practicing_ssa == "yes",
    country_practice %in% ssa_countries
  )

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
    
    # Clean other from accreditation
    accreditation_body = case_when(
      accreditation_body == "other" & accreditation_other %in% c(
        "CAMES",
        "FRCSCanada") ~ "international", 
      TRUE ~ as.character(accreditation_body)
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
                 "Membership qualification", "Primary medical degree")),
    # Factorize accreditation
    accreditation_body = factor(
      accreditation_body,
      levels = c("international", "cosecsa", "wacs", "national", "other"),
      labels = c("International training program", "COSECSA",
                 "WACS","National training program", "Other")),
    # Factorize position
    position = factor(
      position,
      levels = c("academic_faculty", "dept_head", "consultant", "resident", "fellow"),
      labels = c("Academic faculty", "Department head/chair", "Consultant/Attending surgeon",
                 "Resident/Trainee", "Fellow"))
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Country of practice ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract unique countries from each column
clean_country_practice <- clean_cv_data %>% 
  separate_rows(country_currently, sep = ";") %>% 
  mutate(country_currently = str_trim(country_currently))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Subspecialty ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract unique subspecialties from each answer
clean_subspecialty <- clean_cv_data %>% 
  separate_rows(subspecialty, sep = ";") %>% 
  mutate(subspecialty = str_trim(subspecialty))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save clean dataset ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save dataset
#write_csv(clean_cv_data, "data/clean_data_20261022.csv")
