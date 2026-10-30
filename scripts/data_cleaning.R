## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Created: 08/31/2026
## Updated: 10/30/2026

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
raw_data <- read_csv2(here("data/raw/prs_survey_data_20261014.csv")) %>% 
  # Clean names
  clean_names()

# Load XLS data (facilities)
raw_data_facilities <- read_excel(here("data/raw/prs_survey_facilities_data.xlsx"), sheet = 2) %>% 
  clean_names()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Join two datasets ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# View unique ID 
# unique(raw_data$id)
# unique(raw_data_facilities$submission_id)
# 
# setdiff(raw_data$id, raw_data_facilities$submission_id)
# setdiff(raw_data_facilities$submission_id, raw_data$id)
# 
# Verfiy data for merging
# anyDuplicated(raw_data$id)
# 
# raw_data_facilities %>% 
#   count(submission_id) %>% 
#   filter(n > 1)

# Merge two datasets
merged_data <- raw_data %>%
  left_join(raw_data_facilities, 
            by = c("id" = "submission_id"),
            relationship = "one-to-many")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ID & Deidentify emails ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

anonymous_data <- merged_data %>%
  
  # Remove identifiable email fields from analysis data
  select(index.x, id, everything(), -email) %>% 
  
  # Drop test entry while keeping NAs
  filter(is.na(country_practice) | country_practice != "Switzerland")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Cleaning columns ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Drop columns not used or just info
clean_data <- anonymous_data %>% 
  select(!c(
    logos_note,info_intro, info_contact, consent_header, consent_declined, #NA
    thank_you_for_your_i_s_study_at_this_time, #thank you note
    3,4,6,
    169:175, #ids and more
    201:210,
    starts_with("note_"))
  )

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
      age_group == "gte60" ~ ">59"
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
)

# Create factors for categorical variables
clean_cv_data <- clean_cv_data %>% 
  mutate(
    # Factorize age_group
    age_group = factor(
      age_group,
      levels = c("<30", "30-39", "40-49", "50-59", ">59")),
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
## Clean discrepencies ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
clean_disc <- clean_cv_data %>% 
  mutate(
    # Get rid of wrong accreditation year data
    accreditation_year = if_else(
      age_group == "50-59" &
        accreditation_year == 1950 & year_prs_accredited == 76,
      NA_real_,
      accreditation_year
    ),
    year_prs_accredited = if_else(
      age_group == "50-59" & year_prs_accredited == 76, 
      NA_real_,
      year_prs_accredited
    )
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Country of practice ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract unique countries from each column
clean_country_practice <- clean_disc %>% 
  separate_rows(country_currently, sep = ";") %>% 
  mutate(country_currently = str_trim(country_currently))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Subspecialty ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extract unique subspecialties from each answer
clean_subspecialty <- clean_disc %>% 
  separate_rows(subspecialty, sep = ";") %>% 
  mutate(subspecialty = str_trim(subspecialty))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Clean facility names ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Explicit duplicate mappings
facility_map <- c(
  # ALERT
  "alert comprehensive specialized hospital" =
    "ALERT Comprehensive Specialized Hospital",
  "alert" =
    "ALERT Comprehensive Specialized Hospital",
  
  # Kijabe
  "aic kijabe hospital" = "AIC Kijabe Hospital",
  "kijabe hospital" = "AIC Kijabe Hospital",
  
  # Mulago
  "mulago national referral hospital" =
    "Mulago National Referral Hospital",
  "mulago hospital" =
    "Mulago National Referral Hospital",
  
  # Felege Hiwot
  "felgehiwot hospital" = "Felege Hiwot Hospital",
  "felegehiwot" = "Felege Hiwot Hospital",
  
  # Yekatit
  "yekatit 12 hospital medical college" =
    "Yekatit 12 Hospital Medical College",
  "yekatit" =
    "Yekatit 12 Hospital Medical College",
  
  # Rwanda Military Hospital
  "rwanda military teachingchospital" =
    "Rwanda Military Teaching and Referral Hospital",
  "rwanda millitary hospital" =
    "Rwanda Military Teaching and Referral Hospital",
  "rwanda military teaching and referral hospital" =
    "Rwanda Military Teaching and Referral Hospital",
  
  # Worabe
  "werabe comprehe sive specialised hospital" =
    "Worabe Comprehensive Specialized Hospital",
  "worabe comprehensive specialized hospital" =
    "Worabe Comprehensive Specialized Hospital",
  
  # CHUK
  "chuk" = "University Teaching Hospital of Kigali (CHUK)",
  "university teaching hospital of kigali" =
    "University Teaching Hospital of Kigali (CHUK)",
  
  # Kayunga
  "kayunaga regional referral hospital" =
    "Kayunga Regional Referral Hospital",
  
  # Person's name entered instead of facility
  "thierry cyuzuzo" =
    "University Teaching Hospital of Kigali (CHUK)",
  
  # Nakasero
  "nakasero" = "Nakasero Hospital Limited",
  
  # Kamuzu Central Hospital
  "kch" = "Kamuzu Central Hospital",
  
  # Muhimbili
  "muhimbili" = "Muhimbili National Hospital"
)

# Standardize names and remove multiple-facility observation
df <- df %>%
  filter(
    !str_detect(
      coalesce(facility_name, ""),
      regex(
        "kayunga regional referral hospital and",
        ignore_case = TRUE
      )
    )
  ) %>%
  mutate(
    facility_name = str_squish(facility_name),
    facility_name = coalesce(
      unname(facility_map[str_to_lower(facility_name)]),
      facility_name
    )
  )

# # Check standardized facility names
# sort(unique(clean_facility$facility_name))
# 
# # Count unique facilities
# n_distinct(clean_facility$facility_name)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save clean dataset ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save dataset
# write_csv(clean_disc, "data/clean_data_20261022.csv")

# Save as RDS (preserves factors, levels, and variable types)
# saveRDS(
#   clean_disc, here("data/clean_data_20261030.rds")
# )
