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
  )

#keep only those with >70% items answered 
  score_prorate <- function(sum, n_answered, n_total = 23, min_prop = 0.7) {
  ok <- n_answered / n_total >= min_prop
  ifelse(ok, (sum / pmax(n_answered, 1)) * n_total, NA_real_)
}

ATQ <- ATQ %>%
  mutate(atq_sum_prorated = score_prorate(atq_sum, atq_items_answered))

ATQ <- ATQ %>% select(-atq_items_answered)

# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

# valid ids = consent 1 or NA
valid_ids <- consent %>%
  mutate(consent = as.integer(consent)) %>%
  filter(is.na(consent) | consent == 1L) %>%
  distinct(respondent_id)

# keep only those ATQ rows
ATQ <- ATQ %>%
  semi_join(valid_ids, by = "respondent_id")        #2803 obs. of 9 variables
  

# Replace empty strings with NA only in character columns
ATQ  <- ATQ  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(ATQ ))
