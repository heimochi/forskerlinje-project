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
PSWQ <- read_excel("/Users/maggieheimvik/Desktop/COPE/data/dataset/PSWQavid.xls")

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
    starts_with("Q"),
    starts_with("calc")
  ) %>%
  rename(
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`,
    respondent_id            = `respondent id`,

    # PSWQ items (short, recognizable)
    pswq_no_time_worry   = Q1,
    pswq_overwhelm       = Q2,
    pswq_no_tend         = Q3,
    pswq_many_situations = Q4,
    pswq_cannot_help     = Q5,
    pswq_pressure        = Q6,
    pswq_always          = Q7,
    pswq_dismiss         = Q8,
    pswq_after_task      = Q9,
    pswq_never           = Q10,
    pswq_no_more         = Q11,
    pswq_lifetime        = Q12,
    pswq_notice          = Q13,
    pswq_cannot_stop     = Q14,
    pswq_all_time        = Q15,
    pswq_until_done      = Q16,

    # Calculated scores
    calc_pswq_total_aa        = `calculation:PSWQ-ALL-AA`,
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