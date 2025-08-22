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
    respondent_id, 
    assessment_context_label, 
    treatment_id,
    treatment_name, 
    treatment_type_id, 
    starts_with("Q"), 
    calc_ans,
    calc_tot
  )

# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

  # Merge only the 'consent' column with the BAI dataset by 'respondent_id'
BAI <- merge(BAI, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
BAI  <- BAI  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(BAI))