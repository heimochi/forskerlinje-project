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
    starts_with("calc")
  ) %>%
  rename(
    # Short descriptive BAI items
    bai_numbness       = Q1,
    bai_hot            = Q2,
    bai_wobbly_legs    = Q3,
    bai_cannot_relax   = Q4,
    bai_fear_worst     = Q5,
    bai_dizzy          = Q6,
    bai_heart_racing   = Q7,
    bai_unsteady       = Q8,
    bai_terrified      = Q9,
    bai_nervous        = Q10,
    bai_choking        = Q11,
    bai_trembling      = Q12,
    bai_shaky          = Q13,
    bai_fear_control   = Q14,
    bai_breathing_diff = Q15,
    bai_fear_dying     = Q16,
    bai_scared         = Q17,
    bai_indigestion    = Q18,
    bai_faint          = Q19,
    bai_flushed        = Q20,
    bai_sweats         = Q21,

    # Calculated scores
    calc_bai_aa               = `calculation:MODUMBAD-BAI-AA`,
    calc_bai_total_sum        = `calculation:MODUMBAD-BAI-SUM`,
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