
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##
## Title: PRS Survey Response Rate Analysis
##
## Author: Sabina Rodriguez
##
## Updated: 16/11/2026
##
## Overall and Country-Specific Response Rates
##
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

if (!require("pacman")) install.packages("pacman")

pacman::p_load(
  tidyverse,
  janitor,
  lubridate,
  here,
  readxl,
  openxlsx,
  gt,
  scales
)


# ============================================================
# 2. LOAD DATA
# ============================================================

# Raw survey responses
raw_data <- read_csv2(
  here("data/raw/prs_survey_data_20261014.csv")
) %>%
  clean_names() %>%
  mutate(
    id = as.character(id)
  )

# Recruitment list:
# 2nd Chance network and online references
recruitment_list <- read_excel(
  here(
    "data/raw/prs_participant_second_chance_online_list.xlsx"
  )
) %>%
  clean_names()


# ============================================================
# 3. IDENTIFY MATCHING VARIABLES
# ============================================================

# Inspect column names
names(raw_data)
names(recruitment_list)

# Update these if your column names differ
survey_email_col <- "email"
list_email_col <- "email"

# Recruitment-list country variable
# Change this if needed
country_col <- "country"

# Verify required columns exist
required_survey <- c(survey_email_col)

required_list <- c(
  list_email_col,
  country_col
)

if (!all(required_survey %in% names(raw_data))) {
  stop("Survey email column not found in raw_data.")
}

if (!all(required_list %in% names(recruitment_list))) {
  stop(
    "Recruitment email or country column not found. ",
    "Check names(recruitment_list)."
  )
}


# ============================================================
# 4. STANDARDIZE EMAIL ADDRESSES
# ============================================================

clean_email <- function(x) {
  
  x <- str_to_lower(
    str_squish(as.character(x))
  )
  
  x <- na_if(x, "")
  x <- na_if(x, "na")
  x <- na_if(x, "n/a")
  
  return(x)
}


# ============================================================
# 5. STANDARDIZE COUNTRY NAMES
# ============================================================

clean_country <- function(x) {
  
  x <- str_squish(
    as.character(x)
  )
  
  x <- na_if(x, "")
  x <- na_if(x, "NA")
  x <- na_if(x, "N/A")
  
  return(x)
}


# ============================================================
# 6. PREPARE SURVEY RESPONSES
# ============================================================

survey_responses <- raw_data %>%
  mutate(
    email_match = clean_email(
      .data[[survey_email_col]]
    )
  )

# Count submissions associated with each email
survey_emails <- survey_responses %>%
  filter(!is.na(email_match)) %>%
  count(
    email_match,
    name = "n_submissions"
  )

# Identify duplicate survey submissions
duplicate_submissions <- survey_emails %>%
  filter(n_submissions > 1) %>%
  arrange(desc(n_submissions))


# ============================================================
# 7. PREPARE RECRUITMENT LIST
# ============================================================

participant_list <- recruitment_list %>%
  mutate(
    email_match = clean_email(
      .data[[list_email_col]]
    ),
    
    facility_country = clean_country(
      .data[[country_col]]
    )
  )

# Identify participants without an email
missing_email_participants <- participant_list %>%
  filter(is.na(email_match))


# ============================================================
# 8. CHECK CONFLICTING COUNTRY ASSIGNMENTS
# ============================================================

# Determine whether duplicate emails are assigned
# to different countries

country_conflicts <- participant_list %>%
  filter(!is.na(email_match)) %>%
  group_by(email_match) %>%
  summarise(
    n_countries = n_distinct(
      facility_country,
      na.rm = TRUE
    ),
    
    countries = paste(
      unique(na.omit(facility_country)),
      collapse = "; "
    ),
    
    .groups = "drop"
  ) %>%
  filter(n_countries > 1)

# Stop if conflicting country assignments exist
if (nrow(country_conflicts) > 0) {
  
  print(country_conflicts)
  
  stop(
    "Some participant emails have conflicting countries. ",
    "Resolve these before calculating country response rates."
  )
}


# ============================================================
# 9. IDENTIFY UNIQUE INVITED PARTICIPANTS
# ============================================================

unique_participants <- participant_list %>%
  filter(!is.na(email_match)) %>%
  arrange(
    email_match,
    is.na(facility_country)
  ) %>%
  distinct(
    email_match,
    .keep_all = TRUE
  )

# Number of unique invited participants
n_invited <- nrow(unique_participants)


# ============================================================
# 10. MATCH SURVEY RESPONSES TO RECRUITMENT LIST
# ============================================================

participant_status <- unique_participants %>%
  left_join(
    survey_emails,
    by = "email_match",
    relationship = "one-to-one"
  ) %>%
  mutate(
    n_submissions = replace_na(
      n_submissions,
      0L
    ),
    
    survey_status = if_else(
      n_submissions > 0,
      "Responded",
      "Not Responded"
    )
  ) %>%
  relocate(
    email_match,
    facility_country,
    survey_status,
    n_submissions
  ) %>%
  arrange(
    facility_country,
    survey_status,
    email_match
  )


# ============================================================
# 11. IDENTIFY RESPONDENTS AND NONRESPONDENTS
# ============================================================

# Participants who submitted a survey
completed_participants <- participant_status %>%
  filter(
    survey_status == "Responded"
  )

# Participants without a matching survey submission
pending_participants <- participant_status %>%
  filter(
    survey_status == "Not Responded"
  )

# Survey respondents not found in recruitment list
unmatched_respondents <- survey_emails %>%
  anti_join(
    unique_participants,
    by = "email_match"
  )


# ============================================================
# 12. CALCULATE OVERALL RESPONSE RATE
# ============================================================

n_responded <- nrow(completed_participants)

n_pending <- nrow(pending_participants)

response_rate <- if (n_invited > 0) {
  round(
    100 * n_responded / n_invited,
    1
  )
} else {
  NA_real_
}

# Create overall response summary
response_summary <- tibble(
  Metric = c(
    "Unique invited participants",
    "Survey respondents",
    "Nonrespondents",
    "Overall response rate (%)",
    "Total survey submissions with email",
    "Unique survey respondent emails",
    "Duplicate respondent emails",
    "Respondents not on invitation list",
    "Recruitment records missing email",
    "Unique invitees missing country"
  ),
  
  Value = c(
    n_invited,
    n_responded,
    n_pending,
    response_rate,
    sum(survey_emails$n_submissions),
    nrow(survey_emails),
    nrow(duplicate_submissions),
    nrow(unmatched_respondents),
    nrow(missing_email_participants),
    sum(is.na(unique_participants$facility_country))
  )
)

# Display summary
print(response_summary)


# ============================================================
# 13. CALCULATE RESPONSE RATE BY COUNTRY
# ============================================================

country_response_rate <- participant_status %>%
  
  # Retain participants with known country
  filter(
    !is.na(facility_country),
    facility_country != ""
  ) %>%
  
  group_by(facility_country) %>%
  
  summarise(
    n_invited = n(),
    
    n_responded = sum(
      survey_status == "Responded"
    ),
    
    n_not_responded = sum(
      survey_status == "Not Responded"
    ),
    
    response_rate = round(
      100 * n_responded / n_invited,
      1
    ),
    
    .groups = "drop"
  ) %>%
  
  arrange(desc(response_rate), desc(n_invited))

# Display country response rates
country_response_rate


# ============================================================
# 14. CREATE COUNTRY RESPONSE RATE TABLE
# ============================================================

gt_response_rate <- country_response_rate %>%
  
  gt() %>%
  
  tab_header(
    title = md(
      "**Survey Response Rate by Country**"
    ),
    
    subtitle = paste(
      "Participants recruited through the 2nd Chance",
      "network and online references"
    )
  ) %>%
  
  cols_label(
    facility_country = "Country",
    
    n_invited = "Invited (N)",
    
    n_responded = "Responded (N)",
    
    n_not_responded = "Did Not Respond (N)",
    
    response_rate = "Response Rate (%)"
  ) %>%
  
  fmt_number(
    columns = c(
      n_invited,
      n_responded,
      n_not_responded
    ),
    decimals = 0,
    use_seps = TRUE
  ) %>%
  
  fmt_number(
    columns = response_rate,
    decimals = 1
  ) %>%
  
  cols_align(
    align = "right",
    columns = -facility_country
  ) %>%
  
  tab_options(
    table.font.size = px(13),
    data_row.padding = px(6),
    heading.align = "left",
    column_labels.font.weight = "bold",
    table.border.top.color = "grey70",
    table.border.bottom.color = "grey70"
  ) %>%
  
  tab_source_note(
    source_note = md(
      paste(
        "Response rates were calculated using unique",
        "invited email addresses matched to survey submissions.",
        "Participants with missing countries are excluded",
        "from country-specific calculations."
      )
    )
  )

# Display table
gt_response_rate


# ============================================================
# 15. CREATE RESPONSE RATE BAR CHART
# ============================================================

country_response_plot <- country_response_rate %>%
  
  mutate(
    country_label = factor(
      facility_country,
      levels = facility_country[
        order(response_rate)
      ]
    ),
    
    pct_label = sprintf(
      "%.1f%% (%s/%s)",
      response_rate,
      n_responded,
      n_invited
    )
  ) %>%
  
  ggplot(
    aes(
      x = country_label,
      y = response_rate,
      fill = response_rate
    )
  ) +
  
  # Horizontal bars
  geom_col(
    width = 0.75
  ) +
  
  # Percentage and counts
  geom_text(
    aes(
      label = pct_label,
      
      hjust = if_else(
        response_rate >= 75,
        1.05,
        -0.1
      ),
      
      color = if_else(
        response_rate >= 75,
        "white",
        "black"
      )
    ),
    size = 3.2,
    show.legend = FALSE
  ) +
  
  scale_color_identity() +
  
  # Consistent color gradient
  scale_fill_distiller(
    palette = "YlOrRd",
    direction = 1,
    limits = c(0, 100)
  ) +
  
  scale_y_continuous(
    breaks = seq(0, 100, by = 20),
    labels = label_number(suffix = "%"),
    expand = expansion(mult = c(0, 0))
  ) +
  
  coord_flip(
    ylim = c(0, 100),
    clip = "off"
  ) +
  
  labs(
    title = "PRS Survey Response Rate by Country",
    
    subtitle = "Response rates among recruited participants",
    
    x = NULL,
    y = "Response Rate (%)",
    
    caption = paste(
      "Response rate = unique invited participants",
      "with matching survey submissions / unique invitees.",
      "Labels show percentage (responded/invited).",
      "Participants without country information excluded."
    )
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    plot.title = element_text(
      face = "bold",
      size = 13
    ),
    
    axis.ticks = element_blank(),
    
    axis.title.y = element_blank(),
    
    panel.grid.major.y = element_blank(),
    
    panel.grid.minor = element_blank(),
    
    legend.position = "none",
    
    plot.caption = element_text(
      size = 9,
      color = "grey40",
      hjust = 0
    ),
    
    plot.margin = margin(
      10, 55, 10, 10
    )
  )

# Display figure
country_response_plot


# ============================================================
# 16. CREATE OUTPUT DIRECTORY
# ============================================================

output_dir <- here("outputs")

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 17. CREATE FORMATTED EXCEL WORKBOOK
# ============================================================

output_file <- file.path(
  output_dir,
  "PRS_Survey_Response_Rate_Analysis.xlsx"
)

wb <- createWorkbook()

# Header style
header_style <- createStyle(
  fontColour = "#FFFFFF",
  fgFill = "#244062",
  textDecoration = "bold",
  halign = "center",
  valign = "center",
  border = "Bottom",
  borderColour = "#D9E2F3"
)

# Responded status
completed_style <- createStyle(
  fontColour = "#166534",
  fgFill = "#DCFCE7"
)

# Not responded status
pending_style <- createStyle(
  fontColour = "#9A3412",
  fgFill = "#FFEDD5"
)


# ------------------------------------------------------------
# Helper function to format Excel sheets
# ------------------------------------------------------------

add_formatted_sheet <- function(
    wb,
    sheet_name,
    data
) {
  
  addWorksheet(
    wb,
    sheet_name,
    gridLines = FALSE
  )
  
  writeData(
    wb,
    sheet = sheet_name,
    x = data,
    headerStyle = header_style,
    withFilter = TRUE
  )
  
  freezePane(
    wb,
    sheet_name,
    firstActiveRow = 2
  )
  
  setColWidths(
    wb,
    sheet = sheet_name,
    cols = seq_len(ncol(data)),
    widths = "auto"
  )
  
  setRowHeights(
    wb,
    sheet = sheet_name,
    rows = 1,
    heights = 28
  )
}


# ============================================================
# 18. ADD WORKSHEETS
# ============================================================

add_formatted_sheet(
  wb,
  "Summary",
  response_summary
)

add_formatted_sheet(
  wb,
  "All Participants",
  participant_status
)

add_formatted_sheet(
  wb,
  "Responded",
  completed_participants
)

add_formatted_sheet(
  wb,
  "Not Responded",
  pending_participants
)

add_formatted_sheet(
  wb,
  "Country Response Rates",
  country_response_rate
)

add_formatted_sheet(
  wb,
  "Unmatched Responses",
  unmatched_respondents
)

add_formatted_sheet(
  wb,
  "Duplicate Submissions",
  duplicate_submissions
)

add_formatted_sheet(
  wb,
  "Missing Emails",
  missing_email_participants
)


# ============================================================
# 19. HIGHLIGHT RESPONSE STATUS
# ============================================================

status_col <- which(
  names(participant_status) == "survey_status"
)

status_col_letter <- int2col(status_col)

if (nrow(participant_status) > 0) {
  
  conditionalFormatting(
    wb,
    "All Participants",
    
    cols = status_col,
    
    rows = 2:(nrow(participant_status) + 1),
    
    type = "expression",
    
    rule = paste0(
      '$', status_col_letter,
      '2="Responded"'
    ),
    
    style = completed_style
  )
  
  conditionalFormatting(
    wb,
    "All Participants",
    
    cols = status_col,
    
    rows = 2:(nrow(participant_status) + 1),
    
    type = "expression",
    
    rule = paste0(
      '$', status_col_letter,
      '2="Not Responded"'
    ),
    
    style = pending_style
  )
}


# ============================================================
# 20. SAVE FIGURE
# ============================================================

figure_file <- file.path(
  output_dir,
  "PRS_Response_Rate_by_Country.png"
)

ggsave(
  filename = figure_file,
  plot = country_response_plot,
  width = 11,
  height = 7,
  dpi = 300,
  bg = "white"
)


# ============================================================
# 21. ADD FIGURE TO EXCEL WORKBOOK
# ============================================================

addWorksheet(
  wb,
  "Response Rate Figure",
  gridLines = FALSE
)

insertImage(
  wb,
  sheet = "Response Rate Figure",
  file = figure_file,
  startRow = 2,
  startCol = 2,
  width = 10,
  height = 6,
  units = "in"
)


# ============================================================
# 22. SAVE WORKBOOK
# ============================================================

saveWorkbook(
  wb,
  output_file,
  overwrite = TRUE
)

message(
  "Excel workbook created: ",
  output_file
)

message(
  "Country response-rate figure created: ",
  figure_file
)

message(
  "Response-rate analysis completed successfully."
)
