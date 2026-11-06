## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 
## Title: Extracting number of respondents who did not fill out survey
## 
## Author: Sabina Rodriguez
##
## Created: 11/06/2026
## Updated: 11/06/2026

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load packages ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

if (!require(pacman)) install.packages("pacman")
pacman::p_load(tidyverse, janitor, lubridate, here, readxl, writexl, openxlsx)

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
recruitment_list <- read_excel(here(
  "data/raw/prs_participant_second_chance_online_list.xlsx")) %>% 
  #Clean names
  clean_names()

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Identify participants who completed the survey ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Check available column names to identify email variables
names(raw_data)
names(second_chance_online_list)

# IMPORTANT:
# Replace these with the actual email column names in your datasets.
survey_email_col <- "email"
list_email_col   <- "email"

# Confirm that the specified columns exist
stopifnot(
  survey_email_col %in% names(raw_data),
  list_email_col %in% names(second_chance_online_list)
)

# Standardize emails for matching
clean_email <- function(x) {
  x <- str_to_lower(str_trim(as.character(x)))
  x <- na_if(x, "")
  x <- na_if(x, "na")
  x <- na_if(x, "n/a")
  x
}

# Prepare survey responses
survey_responses <- raw_data %>%
  mutate(email_match = clean_email(.data[[survey_email_col]]))

# Prepare participant list
participant_list <- recruitment_list %>%
  mutate(email_match = clean_email(.data[[list_email_col]]))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Deduplicate email addresses ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Count survey submissions per email
survey_emails <- survey_responses %>%
  filter(!is.na(email_match)) %>%
  count(email_match, name = "n_submissions")

# Keep one participant per unique email
unique_participants <- participant_list %>%
  filter(!is.na(email_match)) %>%
  distinct(email_match, .keep_all = TRUE)

# Identify survey submissions with duplicate emails
duplicate_submissions <- survey_emails %>%
  filter(n_submissions > 1) %>%
  arrange(desc(n_submissions))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Match survey responses to participant list ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Merge using the standardized email identifier
participant_status <- unique_participants %>%
  left_join(survey_emails, by = "email_match") %>%
  mutate(
    n_submissions = replace_na(n_submissions, 0L),
    survey_status = if_else(
      n_submissions > 0,
      "Completed",
      "Not Completed"
    )
  ) %>%
  relocate(email_match, survey_status, n_submissions) %>%
  arrange(survey_status, email_match)

# Participants who answered
completed_participants <- participant_status %>%
  filter(survey_status == "Completed")

# Participants who have not answered
pending_participants <- participant_status %>%
  filter(survey_status == "Not Completed")

# Survey respondents absent from original participant list
unmatched_respondents <- survey_emails %>%
  anti_join(unique_participants, by = "email_match")

# Participant records missing an email
missing_email_participants <- participant_list %>%
  filter(is.na(email_match))

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Generate response summary ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

n_invited <- nrow(unique_participants)
n_completed <- nrow(completed_participants)
n_pending <- nrow(pending_participants)

response_rate <- if (n_invited > 0) {
  round(n_completed / n_invited * 100, 1)
} else {
  NA_real_
}

response_summary <- tibble(
  Metric = c(
    "Unique invited participants",
    "Completed surveys",
    "Not completed",
    "Response rate (%)",
    "Total survey submissions with email",
    "Unique survey respondent emails",
    "Duplicate respondent emails",
    "Respondents not on invitation list",
    "Participant records missing email"
  ),
  Value = c(
    n_invited,
    n_completed,
    n_pending,
    response_rate,
    sum(survey_emails$n_submissions),
    nrow(survey_emails),
    nrow(duplicate_submissions),
    nrow(unmatched_respondents),
    nrow(missing_email_participants)
  )
)

print(response_summary)

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create shareable Excel workbook ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create output directory
output_file <- file.path(
  here("outputs"),
  "PRS_Survey_Participant_Response_Tracker.xlsx"
)

# Create workbook
wb <- createWorkbook()

# Excel styles
header_style <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#244062",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom",
  borderColour = "#D9E2F3"
)

completed_style <- createStyle(
  fontColour = "#166534",
  fgFill = "#DCFCE7"
)

pending_style <- createStyle(
  fontColour = "#9A3412",
  fgFill = "#FFEDD5"
)

# Helper function for writing formatted sheets
add_formatted_sheet <- function(wb, sheet_name, data) {
  
  addWorksheet(wb, sheet_name, gridLines = FALSE)
  
  writeData(
    wb,
    sheet = sheet_name,
    x = data,
    headerStyle = header_style,
    withFilter = TRUE
  )
  
  freezePane(wb, sheet_name, firstActiveRow = 2)
  
  setColWidths(
    wb,
    sheet = sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
  
  setRowHeights(wb, sheet_name, rows = 1, heights = 28)
}

# Create sheets
add_formatted_sheet(wb, "Summary", response_summary)
add_formatted_sheet(wb, "All Participants", participant_status)
add_formatted_sheet(wb, "Completed", completed_participants)
add_formatted_sheet(wb, "Not Completed", pending_participants)
add_formatted_sheet(wb, "Unmatched Responses", unmatched_respondents)
add_formatted_sheet(wb, "Duplicate Submissions", duplicate_submissions)
add_formatted_sheet(wb, "Missing Emails", missing_email_participants)

# Highlight response status in the combined list
status_col <- which(names(participant_status) == "survey_status")
status_col_letter <- int2col(status_col)

if (nrow(participant_status) > 0) {
  conditionalFormatting(
    wb, "All Participants",
    cols = status_col,
    rows = 2:(nrow(participant_status) + 1),
    type = "expression",
    rule = paste0('$', status_col_letter, '2="Completed"'),
    style = completed_style
  )
  
  conditionalFormatting(
    wb, "All Participants",
    cols = status_col,
    rows = 2:(nrow(participant_status) + 1),
    type = "expression",
    rule = paste0('$', status_col_letter, '2="Not Completed"'),
    style = pending_style
  )
}

# Save workbook
saveWorkbook(wb, output_file, overwrite = TRUE)

message("Excel workbook created: ", output_file)
