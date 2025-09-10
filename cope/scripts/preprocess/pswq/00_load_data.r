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
PSWQ <- read_excel(file.path(data_dir, "PSWQavid.xls"))    # 2,729 

# this questionnaire has a few items that are reverse scored
# Define reverse scoring function (1<->5, 2<->4, 3 stays)
reverse_score <- function(x) ifelse(is.na(x), NA, 6 - x)

# ---------------------------------------------------------
# Rename n select PSWQ column names for consistency
# ---------------------------------------------------------

# Select only from the dataset
PSWQ <- PSWQ %>%
  select(
    `respondent id`, `assessment instance context label`,
    `treatment id`, `treatment name`, `treatment type id`,
    Q1:Q16,
    `calculation:PSWQ-ALL-AA`,
    `calculation:PSWQ-ALL-SUM`
  ) %>%
  rename(
    respondent_id            = `respondent id`,
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`,
    pswq_total_aa            = `calculation:PSWQ-ALL-AA`,
    pswq_total               = `calculation:PSWQ-ALL-SUM`
  ) %>%
  # Apply reverse scoring to Q1, Q3, Q8, Q10, Q11
  mutate(across(c(Q1, Q3, Q8, Q10, Q11), reverse_score)) %>%
  mutate(
    # Core Worries (8 items)
    pswq_core = rowSums(across(c(Q1, Q2, Q3, Q8, Q10, Q11, Q15, Q16)), na.rm = TRUE),
    pswq_core_aa  = rowSums(!is.na(across(c(Q1, Q2, Q3, Q8, Q10, Q11, Q15, Q16)))),
    pswq_core_prorated = if_else(pswq_core_aa/8 >= 0.70,
                                 score_prorate(pswq_core, pswq_core_aa, n_total = 8), NA_real_),

    # Uncontrollability of Worry (7 items)
    pswq_uncont = rowSums(across(c(Q4, Q5, Q6, Q7, Q12, Q13, Q14)), na.rm = TRUE),
    pswq_uncont_aa  = rowSums(!is.na(across(c(Q4, Q5, Q6, Q7, Q12, Q13, Q14)))),
    pswq_uncont_prorated = if_else(pswq_uncont_aa/7 >= 0.70,
                                   score_prorate(pswq_uncont, pswq_uncont_aa, n_total = 7), NA_real_),

    # Worry Engagement (1 item: Q9)
    pswq_engage = Q9,
    pswq_engage_aa  = if_else(!is.na(Q9), 1L, 0L),
    pswq_engage_prorated = if_else(pswq_engage_aa/1 >= 0.70,
                                   score_prorate(pswq_engage, pswq_engage_aa, n_total = 1), NA_real_),

    # Total (16 items, already reverse coded where needed)
    pswq_total_prorated = if_else(pswq_total_aa/16 >= 0.70,
                                  score_prorate(rowSums(across(Q1:Q16), na.rm = TRUE),
                                                pswq_total_aa, n_total = 16), NA_real_)
  ) %>%
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,
    pswq_core_prorated,
    pswq_uncont_prorated,
    pswq_engage_prorated
  )

  # Quality Control
sapply(PSWQ, function(x) sum(is.na(x)))
summary(PSWQ)

# Check N
print(summarize_patient_counts(PSWQ))