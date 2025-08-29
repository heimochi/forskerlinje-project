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

# BAI Assessment data
PSWQ <- read_excel("/Users/maggieheimvik/Desktop/COPE/data/dataset/BDI_avid.xls") #12035 obs. 53 var
# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var
# ---------------------------------------------------------
# Rename n select BDI column names for consistency
# ---------------------------------------------------------

# Select only from the dataset
BDI <- BDI %>%
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
    calc_bdi_answers_amount   = `calculation:BDI-II-ANS-AMOUNT`,
    calc_bdi_total_sum        = `calculation:BDI-II-TOT`,
    calc_bdi_answers_percent  = `calculation:BDI-II-ANS-AMOUNT-PERCENT`
  )


# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

  # Merge only the 'consent' column with the BDI dataset by 'respondent_id'
BDI <- merge(BDI, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
BDI  <- BDI  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(BDI))