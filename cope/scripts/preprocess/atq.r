# ---------------------------------------------------------
# Load ATQ Data
# Author: MochiBear.Hei
# Created: 2025-08-04
# Description: Loads raw ATQ assessment data
# ---------------------------------------------------------

library(readxl)
library(dplyr)

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# ATQ Assessment data
ATQ <- read_excel(file.path(data_dir, "ATQ_avid.xls"))  # 3710 obs. of 57 variables

# ---------------------------------------------------------
# Rename n select ATQ column names for consistency
# ---------------------------------------------------------
#reminder helper in utilities 

atq_items <- paste0("Q", 1:23)

ATQ <- ATQ %>%
  select(
    `respondent id`, `assessment instance context label`,
    `treatment id`, `treatment name`, `treatment type id`,
    all_of(atq_items)
  ) %>%
  rename(
    respondent_id            = `respondent id`,
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`
  ) %>%
  # coerce & sanity-cap items to 0–10
  mutate(across(all_of(atq_items),
                ~ pmin(pmax(suppressWarnings(as.numeric(.)), 0), 10))) %>%
  # answered count & raw 0–10 sum
  mutate(
    atq_items_answered_10 = rowSums(across(all_of(atq_items), ~ !is.na(.))),
    atq_sum_10            = rowSums(across(all_of(atq_items)), na.rm = TRUE)
  ) %>%
  # prorate on 0–10 scale if ≥70% (≥16 items), else NA
  mutate(
    atq_sum_prorated_10 = if_else(
      atq_items_answered_10 / 23 >= 0.70,
      atq_sum_10 * (23 / atq_items_answered_10),
      NA_real_
    )
  ) %>%
  # rescale totals from 0–10 → 0–4 (multiply by 0.4) for standard ATQ range 0–92
  mutate(
    atq_sum           = atq_sum_10 * 0.4,
    atq_sum_prorated  = atq_sum_prorated_10 * 0.4
  ) %>%
  select(
    respondent_id, assessment_context_label, treatment_id, treatment_name, treatment_type_id,
    atq_sum_prorated
  )


    # Quality Control
sapply(ATQ, function(x) sum(is.na(x)))
summary(ATQ)

#ATQ %>% filter(atq_sum > 92) %>% select(respondent_id, atq_sum)
# Many people with nonsensical outputs
# Calculated the sum score from the raw data instead
