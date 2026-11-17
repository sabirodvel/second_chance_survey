## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Extracting number of respondents who did not fill out survey
## 
## Author: Sabina Rodriguez
##
## Created: 11/16/2026
## Updated: 11/16/2026

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, here, readxl, writexl, openxlsx)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load data ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Load CSV data

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Coding theme ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Extract surgeon IDs and reported training needs
training_needs_responses <- df %>%
  select(
    id,
    training_needs,
    training_needs_specify
  ) %>%
  mutate(
    training_needs_specify = str_squish(
      as.character(training_needs_specify)
    )
  ) %>%
  filter(
    !is.na(training_needs_specify),
    training_needs_specify != "",
    !str_to_lower(training_needs_specify) %in%
      c("na", "n/a", "not applicable")
  ) %>%
  arrange(id)

# View extracted responses
training_needs_responses

# Create manual coding template
training_needs_coding <- training_needs_responses %>%
  mutate(
    theme = NA_character_,
    coding_note = NA_character_
  ) %>%
  select(
    id,
    training_needs,
    training_needs_specify,
    theme,
    coding_note
  )

training_needs_coding


# -------------------------------------------------------
# 1. Manually classify training needs by actual surgeon ID
# -------------------------------------------------------

training_theme_codes <- tibble(
  id = as.character(c(
    852528874, 857637566, 857647797, 857695060,
    857705698, 857976756, 858126712, 858214149,
    858342312, 858610151, 860211108, 866715778,
    871068197, 874789331, 875348198, 878215249,
    878250559, 878314288, 878596485, 878713066,
    879556934, 880497882, 880512692, 880539361,
    880639539, 880656633, 880701074, 880891451,
    881402774, 881460719, 881761761, 882420412,
    882652781, 882724761, 882833544, 882850913,
    882879283, 882936086, 883592696, 893791850
  )),
  
  theme = c(
    # 1-5
    "Aesthetic and cosmetic surgery",
    "Aesthetic and cosmetic surgery",
    "Emerging surgical technologies",
    "Aesthetic and cosmetic surgery",
    "Microsurgery;Fellowships and mentorship",
    
    # 6-10
    "Microsurgery",
    "Microsurgery",
    "Training equipment and infrastructure",
    "Microsurgery;Craniofacial and cleft surgery;Aesthetic and cosmetic surgery",
    "Craniofacial and cleft surgery;Aesthetic and cosmetic surgery;Breast and oncologic reconstruction",
    
    # 11-15
    "Craniofacial and cleft surgery;General reconstructive surgery",
    "Hand and peripheral nerve surgery;Fellowships and mentorship;Global health training",
    "Skills and simulation training;Training equipment and infrastructure",
    "Burn and scar reconstruction;Fellowships and mentorship",
    "Microsurgery;Skills and simulation training;Training equipment and infrastructure",
    
    # 16-20
    "Craniofacial and cleft surgery;General reconstructive surgery",
    "Microsurgery;Breast and oncologic reconstruction;Craniofacial and cleft surgery",
    "Microsurgery;Fellowships and mentorship",
    "Craniofacial and cleft surgery",
    "Breast and oncologic reconstruction;Burn and scar reconstruction",
    
    # 21-25
    "Skills and simulation training",
    "General reconstructive surgery;Hand and peripheral nerve surgery;Skills and simulation training;Training equipment and infrastructure",
    "Microsurgery;Training equipment and infrastructure",
    "Microsurgery;Burn and scar reconstruction;Hand and peripheral nerve surgery;Craniofacial and cleft surgery;Breast and oncologic reconstruction",
    "Craniofacial and cleft surgery;Aesthetic and cosmetic surgery",
    
    # 26-30
    "Aesthetic and cosmetic surgery;Craniofacial and cleft surgery",
    "Hand and peripheral nerve surgery;Microsurgery;Craniofacial and cleft surgery;Fellowships and mentorship",
    "Breast and oncologic reconstruction",
    "Aesthetic and cosmetic surgery",
    "Craniofacial and cleft surgery;Fellowships and mentorship",
    
    # 31-35
    "Breast and oncologic reconstruction;Aesthetic and cosmetic surgery;Hand and peripheral nerve surgery",
    "Microsurgery;Burn and scar reconstruction;Training equipment and infrastructure",
    "Craniofacial and cleft surgery;Burn and scar reconstruction",
    "General reconstructive surgery",
    "Hand and peripheral nerve surgery;Craniofacial and cleft surgery;Breast and oncologic reconstruction;Burn and scar reconstruction",
    
    # 36-40
    "Microsurgery;Hand and peripheral nerve surgery",
    "Fellowships and mentorship",
    "Aesthetic and cosmetic surgery",
    "Burn and scar reconstruction;General reconstructive surgery",
    "Burn and scar reconstruction;Microsurgery;Training equipment and infrastructure"
  )
)


# -------------------------------------------------------
# 2. Create coded dataset using original responses
# -------------------------------------------------------

training_needs_coded <- df %>%
  mutate(
    id = as.character(id),
    training_needs_specify = str_squish(
      as.character(training_needs_specify)
    )
  ) %>%
  filter(
    !is.na(training_needs_specify),
    training_needs_specify != "",
    !str_to_lower(training_needs_specify) %in%
      c("na", "n/a", "not applicable")
  ) %>%
  select(
    id,
    training_needs,
    training_needs_specify
  ) %>%
  left_join(
    training_theme_codes,
    by = "id",
    relationship = "many-to-one"
  )

# Check for responses without assigned themes
training_needs_coded %>%
  filter(is.na(theme))

# Display coded dataset
training_needs_coded


# -------------------------------------------------------
# 3. Separate multiple themes into individual rows
# -------------------------------------------------------

training_themes_long <- training_needs_coded %>%
  filter(!is.na(theme)) %>%
  separate_rows(
    theme,
    sep = ";"
  ) %>%
  mutate(
    theme = str_trim(theme)
  ) %>%
  distinct(id, theme)

# Display
training_themes_long


# -------------------------------------------------------
# 4. Calculate training need theme frequencies
# -------------------------------------------------------

n_training_respondents <- training_needs_coded %>%
  summarise(n = n_distinct(id)) %>%
  pull(n)

training_theme_count <- training_themes_long %>%
  count(
    theme,
    name = "n"
  ) %>%
  mutate(
    percentage = 100 * n / n_training_respondents,
    pct_label = sprintf(
      "%.1f%% (n = %s)",
      percentage,
      n
    )
  ) %>%
  arrange(desc(n), theme)

# Display summary
training_theme_count

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save clean dataset ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save as RDS (preserves factors, levels, and variable types)
saveRDS(
  training_theme_count, here("data/training_theme_count.rds")
)
