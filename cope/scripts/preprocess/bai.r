# ---------------------------------------------------------
# Load BAI Data
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw BAI assessment data
# ---------------------------------------------------------

library(readr)
library(dplyr)

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# BAI Assessment data
BAI <- read_csv(file.path(data_dir, "scripts/anon/BAI_a.csv")) #8743 obs. of 52 var

# ---------------------------------------------------------
# Rename n select BAI column names for consistency
# ---------------------------------------------------------

BAI <- BAI %>%
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,
    any_of(c("calc_ans", "calc_tot"))
  ) %>%
  rename(
    bai_items_answered = calc_ans,
    bai_sum            = calc_tot
  ) %>%
mutate(
  bai_sum_prorated = score_prorate(
    bai_sum,
    bai_items_answered,
    n_total = 21,
    min_prop = 0.70
    )
  ) %>%
  # keep only those with ≥70% items answered
  filter(bai_items_answered / 21 >= 0.70) %>%
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,
    bai_sum_prorated
  )

    # Quality Control
sapply(BAI, function(x) sum(is.na(x)))
summary(BAI)