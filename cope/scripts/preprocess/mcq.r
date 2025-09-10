# ---------------------------------------------------------
# Load MCQ Data
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw MCQ assessment data
# ---------------------------------------------------------

library(readxl)
library(dplyr)

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# MCQ Assessment data
MCQ <- read_excel(file.path(data_dir, "MCQ_avid.xls")) #4100 obs of 68 var

# ---------------------------------------------------------
# Rename n select MCQ column names for consistency
# ---------------------------------------------------------

# Select only from the dataset
MCQ <- MCQ %>%
  select(
    `respondent id`,
    `assessment instance context label`,
    `treatment id`,
    `treatment name`,
    `treatment type id`,

    # raw
    `calculation:MCQ-30-CC`,
    `calculation:MCQ-30-POS`,
    `calculation:MCQ-30-CSC`,
    `calculation:MCQ-30-NEG`,
    `calculation:MCQ-30-NC`,
    `calculation:MCQ-30-Total`,

    # answered
    `calculation:MCQ-30-CC-AA`,
    `calculation:MCQ-30-POS-AA`,
    `calculation:MCQ-30-CSC-AA`,
    `calculation:MCQ-30-NEG-AA`,
    `calculation:MCQ-30-NC-AA`,
    `calculation:MCQ-30-Total-AA`
  ) %>%
  rename(
    respondent_id            = `respondent id`,
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`,

    mcq_cc    = `calculation:MCQ-30-CC`,
    mcq_pos   = `calculation:MCQ-30-POS`,
    mcq_csc   = `calculation:MCQ-30-CSC`,
    mcq_neg   = `calculation:MCQ-30-NEG`,
    mcq_nc    = `calculation:MCQ-30-NC`,
    mcq_total = `calculation:MCQ-30-Total`,

    mcq_cc_aa    = `calculation:MCQ-30-CC-AA`,
    mcq_pos_aa   = `calculation:MCQ-30-POS-AA`,
    mcq_csc_aa   = `calculation:MCQ-30-CSC-AA`,
    mcq_neg_aa   = `calculation:MCQ-30-NEG-AA`,
    mcq_nc_aa    = `calculation:MCQ-30-NC-AA`,
    mcq_total_aa = `calculation:MCQ-30-Total-AA`
  ) %>%
  mutate(
  # prorated subs (each has 6 items)
  mcq_cc_prorated = if_else(
    mcq_cc_aa / 6 >= 0.70,
    score_prorate(mcq_cc, mcq_cc_aa, n_total = 6),
    NA_real_
  ),
  mcq_pos_prorated = if_else(
    mcq_pos_aa / 6 >= 0.70,
    score_prorate(mcq_pos, mcq_pos_aa, n_total = 6),
    NA_real_
  ),
  mcq_csc_prorated = if_else(
    mcq_csc_aa / 6 >= 0.70,
    score_prorate(mcq_csc, mcq_csc_aa, n_total = 6),
    NA_real_
  ),
  mcq_neg_prorated = if_else(
    mcq_neg_aa / 6 >= 0.70,
    score_prorate(mcq_neg, mcq_neg_aa, n_total = 6),
    NA_real_
  ),
  mcq_nc_prorated = if_else(
    mcq_nc_aa / 6 >= 0.70,
    score_prorate(mcq_nc, mcq_nc_aa, n_total = 6),
    NA_real_
  ),

  # total has 30 items
  mcq_total_prorated = if_else(
    mcq_total_aa / 30 >= 0.70,
    score_prorate(mcq_total, mcq_total_aa, n_total = 30),
    NA_real_
  )
) %>%
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,
    mcq_cc_prorated,
    mcq_pos_prorated,
    mcq_csc_prorated,
    mcq_neg_prorated,
    mcq_nc_prorated
  )

  # Quality Control
sapply(MCQ, function(x) sum(is.na(x)))
summary(MCQ)

#respondent 761 has way too high values, doesnt even make sense
MCQ <- MCQ %>%
  filter(respondent_id != 761)

# Check N
print(summarize_patient_counts(MCQ))
