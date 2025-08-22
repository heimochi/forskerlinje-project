# ---------------------------------------------------------
# Load ATQ Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-04
# Description: Loads raw ATQ assessment data and consent records.
# ---------------------------------------------------------

# Libraries
library(readxl)     # for reading Excel files
library(readr)      # for reading CSV files
library(dplyr)      # for data manipulation

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# ATQ Assessment data
ATQ <- read_excel("/Users/maggieheimvik/Desktop/COPE/data/dataset/ATQ_avid.xls")  # 3710 obs. of 57 variables

# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv")  # 5901 obs. of 30 variables

# ---------------------------------------------------------
# Rename n select ATQ column names for consistency
# ---------------------------------------------------------

ATQ <- ATQ %>%
  select(
    `respondent id`,
    `assessment instance context label`,
    `treatment id`,
    `treatment name`,
    `treatment type id`,
    starts_with("Q"),
    starts_with("calc")
  ) %>%
  rename(
    respondent_id = `respondent id`,
    assessment_context_label = `assessment instance context label`,
    treatment_id = `treatment id`,
    treatment_name = `treatment name`,
    treatment_type_id = `treatment type id`,
    calc_aa = `calculation:MODUMBAD-ATQ-AA`,
    calc_pan = `calculation:MODUMBAD-ATQ-PAN-AA`,
    calc_pan_sum = `calculation:MODUMBAD-ATQ-PAN-SUM`,
    calc_ptsd = `calculation:MODUMBAD-ATQ-PTSD-AA`,
    calc_ptsd_sum = `calculation:MODUMBAD-ATQ-PTSD-SUM`,
    calc_sos = `calculation:MODUMBAD-ATQ-SOS-AA`,
    calc_sos_sum = `calculation:MODUMBAD-ATQ-SOS-SUM`,
    calc_sum = `calculation:MODUMBAD-ATQ-SUM`
  )


# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

  # Merge only the 'consent' column with the ATQ dataset by 'respondent_id'
ATQ <- merge(ATQ, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
ATQ  <- ATQ  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(ATQ ))