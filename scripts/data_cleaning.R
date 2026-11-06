## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Cleaning Data for Analysis
## 
## Author: Sabina Rodriguez
##
## Created: 08/31/2026
## Updated: 11/05/2026

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, here, readxl, writexl)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load CSV data

## raw data (only one version)
raw_data <- read_csv2(here("data/raw/prs_survey_data_20261014.csv")) %>% 
  # Clean names
  clean_names() %>% 
  # Make sure that its character
  mutate(id = as.character(id))

# Load XLS data (facilities)
raw_data_facilities <- read_excel(here("data/raw/prs_survey_facilities_data.xlsx"), 
                                  sheet = 2) %>% 
  #Clean names
  clean_names() %>% 
  mutate(submission_id = as.character(submission_id))

# Validate respondent IDs 
# stopifnot("id" %in% names(raw_data))
# stopifnot("submission_id" %in% names(raw_data_facilities))

# Check that it is correct
# if (anyNA(raw_data$id) || anyDuplicated(raw_data$id)) {
#   stop("Participant IDs must be nonmissing and unique. Inspect raw_data$id.")
# }

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## ID & Deidentify emails ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean the raw_data 
anonymous_data <- raw_data %>%
  # Remove identifiable email fields from analysis data
  select(id, everything(), -email) %>% 
  # Drop test entry while keeping NAs
  filter(is.na(country_practice) | country_practice != "Switzerland")


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Clean main dataframe ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Cleaning columns ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Drop columns not used or just info
clean_data <- anonymous_data %>% 
  select(!c(
    logos_note,info_intro, info_contact, consent_header, consent_declined, #NA
    thank_you_for_your_i_s_study_at_this_time, #thank you note
    3,4,6,
    169:174, #ids and more
    starts_with("note_"))
  ) %>% 

# Function to replace spaces with ;
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

      country_practice == "Ruanda" ~ "Rwanda",
      
      TRUE ~ str_to_title(str_squish(country_practice))
    )
  ) 

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Filter for weeding questions ----
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
consent_ssa <- clean_data %>%
  filter(
    consent == "yes",
    practicing_ssa == "yes",
    country_practice %in% ssa_countries
  )

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Clean variables ----
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
    # Factorize age_group
    age_group = factor(age_group,
      levels = c("<30", "30-39", "40-49", "50-59", ">59")),
    
    # Clean other from accreditation
    accreditation_body = case_when(
      accreditation_body == "other" & accreditation_other %in% c(
        "CAMES",
        "FRCSCanada") ~ "international", 
      TRUE ~ as.character(accreditation_body)
    ),
    
    # Investigate the implausible 1950 entry before deriving years.
    accreditation_year = if_else(
      age_group == "50-59" & accreditation_year == 1950,
      NA_real_, as.numeric(accreditation_year)
    ),
    year_prs_accredited = 2026 - accreditation_year,

    # Factorize gender
    gender = factor(gender,
      levels = c("male", "female"),
      labels = c("Male", "Female")),
    # Factorize qualification
    qualification = factor(qualification,
      levels = c("doctoral_degree", "surgical_fellowship", "masters_surgery", 
                 "surgical_membership", "primary_medical_degree"),
      labels = c("Doctoral/research degree", "Fellowship", "Master’s degree in Surgery", 
                 "Membership qualification", "Primary medical degree")),
    # Factorize accreditation
    accreditation_body = factor(accreditation_body,
      levels = c("international", "cosecsa", "wacs", "national", "other"),
      labels = c("International training program", "COSECSA",
                 "WACS","National training program", "Other")),
    # Factorize position
    position = factor(position,
      levels = c("academic_faculty", "dept_head", "consultant", "resident", "fellow"),
      labels = c("Academic faculty", "Department head/chair", "Consultant/Attending surgeon",
                 "Resident/Trainee", "Fellow"))
)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Clean facility data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### Clean facility names ----
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

# Manual name corrections (casing, spelling, punctuation)
name_fixes <- c(
  "Black lion hospital"
  = "Black Lion Hospital",
  "Ethio tebib hospital"
  = "Ethio Tebib Hospital",
  "Heal venture"
  = "Heal Venture",
  "Institut Raoul Follereau de Cote D'ivoire"
  = "Institut Raoul Follereau de Côte d'Ivoire",
  "Children's Surgical hospital"
  = "Children's Surgical Hospital",
  "UNIVERSITY COLLEGE HOSPITAL,IBADAN"
  = "University College Hospital, Ibadan",
  "WOUNDSOLUTIONS CLINIC"
  = "WoundSolutions Clinic",
  "kayunga regional referral hospital and kamapal international and teaching hospital"
  = "Kayunga Regional Referral Hospital / Kamapal International and Teaching Hospital",
  "DEFENCE Forces Memorial Hospital"
  = "Defence Forces Memorial Hospital",
  "Jimma University medical center"
  = "Jimma University Medical Center",
  "Grandville Medical and laser Abuja"
  = "Grandville Medical and Laser Abuja",
  "Adama Hospital medical college"
  = "Adama Hospital Medical College",
  "Saint Paul's hospital millennium medical college"
  = "Saint Paul's Hospital Millennium Medical College",
  "Kalkaal hospital"
  = "Kalkaal Hospital",
  "KING FAISAL HOSPITAL"
  = "King Faisal Hospital",
  "KALEMBELEMBE PEDIATRIC HOSPITAL"
  = "Kalembelembe Pediatric Hospital",
  "Gertrude’s Children’s Hospital" 
  = "Gertrude's Children's Hospital"
)

# Clean facilities
clean_facilities <- raw_data_facilities %>%
  # Map the facilities
  mutate(
    facility_name_original = facility_name,
    facility_name = str_squish(facility_name),
    # Standardize country names
    facility_country = case_when(
      str_to_lower(str_squish(facility_country)) == "dr congo" ~ "DR Congo",
      str_to_lower(str_squish(facility_country)) == "côte d’ivoire" ~ "Côte d’Ivoire",
      TRUE ~ str_to_title(str_squish(facility_country))
    ),
    # Standarize duplicate facility names
    facility_name = coalesce(
      unname(facility_map[str_to_lower(facility_name)]), facility_name
    ),
    # Apply manual spelling, casing, and punctuation fixes
    facility_name = coalesce(
      unname(name_fixes[
        match(
          str_to_lower(facility_name),
          str_to_lower(names(name_fixes))
        )
      ]),
      facility_name
    ),
    # Clean classification variables
    across(any_of(c("facility_location_type", "facility_level",
                    "facility_ownership")),
           ~ na_if(str_to_lower(str_squish(as.character(.x))), ""))) %>% 
  # Fix facilities mismatches
  mutate(
    # Clean the location levels
    facility_location_type = case_when(
      facility_name == "Worabe Comprehensive Specialized Hospital" & facility_location_type == "rural" ~ "urban",
    TRUE ~ facility_location_type),
    # Clean the facility levels
    facility_level = case_when(
      facility_name == "AIC Kijabe Hospital" & facility_level == "other" ~ "district",
      facility_name == "ALERT Comprehensive Specialized Hospital" & facility_level %in% c("other", "university") ~ "central",
      facility_name == "AaBET Hospital" & facility_level %in% c("central", "university") ~ "central university",
      facility_name == "Rwanda Military Teaching and Referral Hospital" & facility_level %in% c("provincial central", "university") ~ "central university",
      facility_name == "University Teaching Hospital of Kigali (CHUK)" & facility_level %in% c("provincial central university", "university") ~ "provincial university",
      facility_name == "Worabe Comprehensive Specialized Hospital" & facility_level == "district" ~ "central",
      facility_name == "Yekatit 12 Hospital Medical College" & facility_level %in% c("other", "university") ~ "central university",
      # Fix others
      facility_name == "Polyclinelle" & facility_level == "other" ~ "district",
      facility_name == "CORSU" & facility_level == "other" ~ "central",
      facility_name == "Nakasero Hospital Limited" & facility_level == "other" ~ "central",
      facility_name == "WoundSolutions Clinic" & facility_level == "other" ~ "district",
      facility_name == "Defence Forces Memorial Hospital" & facility_level == "other" ~ "central university",
      facility_name == "Kenyatta National Hospital" & facility_level == "other" ~ "central",
      facility_name == "Nkinga Referral Hospital" & facility_level == "other" ~ "provincial",
      facility_name == "Yordanos Hospital" & facility_level == "other" ~ "central",
      facility_name == "Hôpital Jean Baptiste Vatelot" & facility_level == "other" ~ "district",
      facility_name == "Gertrude's Children's Hospital" & facility_level == "other" ~ "central university",
      TRUE ~ facility_level),
    # Clean the facility ownership
    facility_ownership = case_when(
      facility_name == "ALERT Comprehensive Specialized Hospital" & facility_ownership == "military" ~ "public",
      facility_name == "ALERT Comprehensive Specialized Hospital" & facility_ownership == "ngo" ~ "public",
      facility_name == "Felege Hiwot Hospital" & facility_ownership %in% c("military", "mission") ~ "public",
      facility_name == "University Teaching Hospital of Kigali (CHUK)" & facility_ownership == "public" ~ "quasi_gov",
      facility_name == "Worabe Comprehensive Specialized Hospital" & facility_ownership == "ngo" ~ "public",
      facility_name == "Yekatit 12 Hospital Medical College" & facility_ownership %in% c("mission", "military") ~ "public",
      TRUE ~ facility_ownership)
  )

# # Flag a response containing two facility names; do not drop it blindly.
# facilities_needing_split <- clean_facilities %>%
#   filter(str_detect(coalesce(facility_name, ""),
#                     regex("kayunga regional referral hospital and", ignore_case = TRUE)))

# # Review repeated reports of the SAME facility by the SAME participant.
# repeated_facility_reports <- clean_facilities %>%
#   filter(!is.na(facility_name)) %>%
#   count(submission_id, facility_name, name = "n_reports") %>%
#   filter(n_reports > 1)
# 
# # Facility-level inconsistencies across respondents.
# facility_conflicts <- clean_facilities %>%
#   filter(!is.na(facility_name), facility_name != "") %>%
#   group_by(facility_name) %>%
#   summarise(
#     n_location_types = n_distinct(facility_location_type, na.rm = TRUE),
#     n_levels = n_distinct(facility_level, na.rm = TRUE),
#     n_ownerships = n_distinct(facility_ownership, na.rm = TRUE),
#     location_values = paste(sort(unique(na.omit(facility_location_type))), collapse = "; "),
#     level_values = paste(sort(unique(na.omit(facility_level))), collapse = "; "),
#     ownership_values = paste(sort(unique(na.omit(facility_ownership))), collapse = "; "),
#     .groups = "drop"
#   ) %>%
#   filter(n_location_types > 1 | n_levels > 1 | n_ownerships > 1)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Join two datasets ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# View unique ID 
# unique(raw_data$id)
# unique(raw_data_facilities$submission_id)
# 
# setdiff(raw_data$id, raw_data_facilities$submission_id)
# setdiff(raw_data_facilities$submission_id, raw_data$id)

# Merge two datasets
merged_data <- clean_cv_data %>%
  left_join(clean_facilities, 
            by = c("id" = "submission_id"),
            relationship = "one-to-many")

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save clean dataset ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save dataset
# write_csv(clean_disc, "data/clean_data_20261022.csv")

# Save as RDS (preserves factors, levels, and variable types)
saveRDS(
  clean_cv_data, here("data/clean_data_20261105.rds")
)
# 
saveRDS(
  merged_data, here("data/clean_merged_20261105.rds")
)
