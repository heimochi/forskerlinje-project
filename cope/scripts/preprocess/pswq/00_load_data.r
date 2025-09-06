# ---------------------------------------------------------
# Load PSWQ Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw PSWQ assessment data and consent records.
# ---------------------------------------------------------

# Libraries
library(readxl)     # for reading Excel files
library(readr)      # for reading CSV files
library(dplyr)      # for data manipulation

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# PSWQ Assessment data
PSWQ <- read_excel("/Users/maggieheimvik/Desktop/COPE/data/dataset/PSWQavid.xls")     # 2,729 

# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var
# ---------------------------------------------------------
# Rename n select PSWQ column names for consistency
# ---------------------------------------------------------

# Select only from the dataset
PSWQ <- PSWQ %>%
  select(
    `assessment instance context label`,
    `treatment id`,
    `treatment name`,
    `treatment type id`,
    `respondent id`,
    starts_with("calc")
  ) %>%
  rename(
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`,
    respondent_id            = `respondent id`,

    # Calculated scores
    calc_pswq_total_sum       = `calculation:PSWQ-ALL-SUM`
  )

# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

  # Merge only the 'consent' column with the PSWQ dataset by 'respondent_id'
PSWQ <- merge(PSWQ, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
PSWQ  <- PSWQ  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(PSWQ))