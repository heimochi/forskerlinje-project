# ---------------------------------------------------------
# Load MCQ Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw MCQ assessment data and consent records.
# ---------------------------------------------------------

# Libraries
library(readxl)     # for reading Excel files
library(readr)      # for reading CSV files
library(dplyr)      # for data manipulation

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# BAI Assessment data
MCQ <- read_excel("/Users/maggieheimvik/Desktop/COPE/data/dataset/MCQ_avid.xls") #4100 obs of 68 var

# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var
# ---------------------------------------------------------
# Rename n select MCQ column names for consistency
# ---------------------------------------------------------

# Select only from the dataset
MCQ <- MCQ %>%
  select(
    `assessment instance context label`,
    `assessment instance created date`,
    `treatment id`,
    `treatment name`,
    `treatment type id`,
    `respondent id`,
    starts_with("Q")
  ) %>%
  rename(
    assessment_context_label = `assessment instance context label`,
    assessment_created_date  = `assessment instance created date`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`,
    respondent_id            = `respondent id`
  )

# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

  # Merge only the 'consent' column with the MCQ dataset by 'respondent_id'
MCQ <- merge(MCQ, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
MCQ  <- MCQ  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(MCQ))