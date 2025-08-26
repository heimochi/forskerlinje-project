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

# MCQ Assessment data
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

    # MCQ subscales
    calc_mcq_cc_aa            = `calculation:MCQ-30-CC-AA`,
    # cognitive confidence
    calc_mcq_cc_sum           = `calculation:MCQ-30-CC`,
    calc_mcq_csc_aa           = `calculation:MCQ-30-CSC-AA`,
    # cognitive self-consciousness
    calc_mcq_csc_sum          = `calculation:MCQ-30-CSC`,
    calc_mcq_nc_aa            = `calculation:MCQ-30-NC-AA`,
    # need to control thoughts
    calc_mcq_nc_sum           = `calculation:MCQ-30-NC`,
    calc_mcq_neg_aa           = `calculation:MCQ-30-NEG-AA`,
    # negative beliefs
    calc_mcq_neg_sum          = `calculation:MCQ-30-NEG`,
    calc_mcq_pos_aa           = `calculation:MCQ-30-POS-AA`,
    # positive beliefs
    calc_mcq_pos_sum          = `calculation:MCQ-30-POS`,
    calc_mcq_total_aa         = `calculation:MCQ-30-Total-AA`,
    calc_mcq_total_sum        = `calculation:MCQ-30-Total`
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