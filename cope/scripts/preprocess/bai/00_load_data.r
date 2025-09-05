# ---------------------------------------------------------
# Load BAI Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw BAI assessment data and consent records.
# ---------------------------------------------------------

# Libraries
library(readxl)     # for reading Excel files
library(readr)      # for reading CSV files
library(dplyr)      # for data manipulation

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# BAI Assessment data
BAI <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/BAI_a.csv") #8743 obs. of 52 var

# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var
# ---------------------------------------------------------
# Rename n select BAI column names for consistency
# ---------------------------------------------------------

BAI <- BAI %>%
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,
    any_of(c("calc_ans", "calc_tot"))
  ) %>%
  rename(
    calc_bai_items_answered = calc_ans,
    bai_sum                 = calc_tot
  ) %>%
  mutate(
    # prorate to the full 21 items when ≥70% answered
     bai_sum_prorated = if_else(
       calc_bai_items_answered / 21 >= 0.7,
       (bai_sum / pmax(calc_bai_items_answered, 1)) * 21,
       NA_real_
     )
  ) %>%
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,
    bai_sum, bai_sum_prorated
  )

# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

# valid ids = consent 1 or NA
valid_ids <- consent %>%
  mutate(consent = as.integer(consent)) %>%
  filter(is.na(consent) | consent == 1L) %>%
  distinct(respondent_id)

# keep only those ATQ rows
BAI <- BAI %>%
  semi_join(valid_ids, by = "respondent_id")        #7210 obs. of 7 variables
  


# Replace empty strings with NA only in character columns
BAI  <- BAI  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(BAI))
