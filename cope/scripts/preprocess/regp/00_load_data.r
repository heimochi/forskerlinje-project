# ---------------------------------------------------------
# Load Patient Registration Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw REGP assessment data and consent records.
# ---------------------------------------------------------

# Libraries
library(readxl)     # for reading Excel files
library(readr)      # for reading CSV files
library(dplyr)      # for data manipulation

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# REGP Assessment data
REGP <- read_excel("/Users/maggieheimvik/Desktop/COPE/data/dataset/PSWQavid.xls")

# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var
# ---------------------------------------------------------
# Rename n select REGP column names for consistency
# ---------------------------------------------------------

# Select only from the dataset
REGP <- REGP %>%
  select(
   respondent_id,
   assessment_context_label,
   treatment_type_id,
   treatment_type_name,
   respondent_gender,
   treatment_name,
   Q1:Q16
  ) %>%
  rename(
    birth = `Q1`,
    partner = `Q2`,
    number_of_children = `Q3`,
    work_situation = `Q4`,
    work_paid = `Q5`,
    work = `Q6`,
    work_paid_months = `Q7`,
    sick_leave_months = `Q8`,
    sick_leave_reason = `Q9`,
    pre_treat_out = `Q10`,
    pre_treatment_out_l= `Q11`,
    pre_treat_in= `Q12`,
    pre_treat_in_l= `Q13`,
    pre_treat_mb= `Q14`,
    pre_treat_mb_l= `Q15`,
    year_onset= `Q16`
  )

# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

  # Merge only the 'consent' column with the REGP dataset by 'respondent_id'
REGP <- merge(REGP, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
REGP  <- REGP  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(REGP))