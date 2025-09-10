# ---------------------------------------------------------
# Load Therapist Registration Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw REGT assessment data and consent records.
# ---------------------------------------------------------

# Libraries
  library(dplyr)
  library(stringr)
  library(tidyr)
# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# REGP Assessment data
REGT <- read_csv(file.path(data_dir, "reg_t_a.csv")) # 242 obs. of 30 var

# ---------------------------------------------------------
# Rename n select REGP column names for consistency
# ---------------------------------------------------------

# helper: normalize ICD codes (uppercase, keep A–Z/0–9/dot, trim; blanks -> NA)
normalize_icd <- function(x) {
  x %>%
    as.character() %>%
    str_to_upper() %>%
    str_remove_all("[^A-Z0-9\\.]") %>%
    str_trim() %>%
    na_if("")
}

REGT <- REGT %>%
  # keep only what we need
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id, treatment_type_name,
    Q30:Q46
  ) %>%
  # rename ICD fields
  rename(
    regt_icd_primary = Q30,
    regt_icd_bi_1    = Q31,
    regt_icd_bi_2    = Q32,
    regt_icd_bi_3    = Q33
    # Q34 = MINI performed (ignored)
    # Q35 = SCID performed (ignored)
  ) %>%
  # normalize ICDs once
  mutate(
    across(c(regt_icd_primary, regt_icd_bi_1, regt_icd_bi_2, regt_icd_bi_3), normalize_icd)
  ) %>%
  # --- derivations ---
  mutate(
    # 1) Number of Diagnoses (ICD present across 4 fields)
    regt_number_of_diagnoses = rowSums(across(
      c(regt_icd_primary, regt_icd_bi_1, regt_icd_bi_2, regt_icd_bi_3),
      ~ !is.na(.x)
    )),

    # 2) Mood disorder (F30–F39 anywhere) -> 1 else 0
    regt_mood_disorder = if_else(
      str_detect(
        paste(regt_icd_primary, regt_icd_bi_1, regt_icd_bi_2, regt_icd_bi_3, sep = " "),
        "\\bF3[0-9](?:\\.[0-9A-Z]+)?\\b"
      ),
      1L, 0L
    )
  ) %>%
  # 3) Personality disorder from SCID-II (Q36–Q46: 1=Ja, 2=Nei)
  mutate(
    # ensure numeric; treat missing as "No" (2)
    across(Q36:Q46, ~ ifelse(is.na(suppressWarnings(as.integer(.x))), 2L, as.integer(.x))),
    regt_personality_disorder = if_else(
      if_any(Q36:Q46, ~ .x == 1L),  # any PD = 1
      1L, 0L
    )
  ) %>%
  # final minimal output
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id, treatment_type_name,
    regt_number_of_diagnoses, regt_mood_disorder, regt_personality_disorder
  )

# Quality Control
sapply(REGT, function(x) sum(is.na(x)))
summary(REGT)

#code treats NA in PD and MD as no due to missing 221 in PD 16 in MD

# Check N
print(summarize_patient_counts(REGT))