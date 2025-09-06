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

# ---------------------------------------------------------
# Rename n select ATQ column names for consistency
# ---------------------------------------------------------
#reminder helper in utilities 

ATQ <- ATQ %>%
  select(
    `respondent id`,
    `assessment instance context label`,
    `treatment id`,
    `treatment name`,
    `treatment type id`,
    `calculation:MODUMBAD-ATQ-SUM`,   # total sum
    `calculation:MODUMBAD-ATQ-AA`     # items answered
  ) %>%
  rename(
    respondent_id            = `respondent id`,
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`,
    atq_sum                = `calculation:MODUMBAD-ATQ-SUM`,
    atq_items_answered     = `calculation:MODUMBAD-ATQ-AA`
  ) %>%
  mutate(
    atq_sum_prorated = score_prorate(atq_sum, atq_items_answered, n_total = 23, min_prop = 0.70)
  ) %>%
  # keep only those with ≥70% items answered
  filter(atq_items_answered / 23 >= 0.70) %>%
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,
    atq_sum, atq_sum_prorated
  )

# Check N
print(summarize_patient_counts(ATQ))
