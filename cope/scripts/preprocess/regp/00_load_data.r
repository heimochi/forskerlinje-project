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
library(lubridate)

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# REGP Assessment data
REGP <- read_csv2(file.path(data_dir, "regp.csv")) #487 obs. of 44 variables with the new dataset

# ---------------------------------------------------------
# Rename n select REGP column names for consistency
# ---------------------------------------------------------

# helper: robust numeric (blank/space/junk -> NA)
clean_num <- function(x) suppressWarnings(as.numeric(na_if(trimws(as.character(x)), "")))

REGP <- REGP %>%
  select(
    respondent.id,
    assessment.instance.context.label,
    assessment.instance.start.date,
    treatment.id, treatment.name,
    treatment.type.id, treatment.type.name,
    respondent.gender,
    Q1, Q2, Q5, Q6, Q8, Q10, Q12, Q16
  ) %>%
  rename(
    respondent_id            = respondent.id,
    assessment_context_label = assessment.instance.context.label,
    assessment_start_date    = assessment.instance.start.date,
    treatment_id             = treatment.id,
    treatment_name           = treatment.name,
    treatment_type_id        = treatment.type.id,
    treatment_type_name      = treatment.type.name,
    regp_gender              = respondent.gender,

    regp_birth_year    = Q1,   # Year of birth
    regp_partner       = Q2,   # 1=Married/Partner, 2=Lives alone, 3=Other
    regp_work_pastyear = Q5,   # 1=Yes, 2=No
    regp_work_current  = Q6,   # 1=Yes, 2=No
    regp_sickleave_m   = Q8,   # Months on sick leave (character with some junk)
    regp_prev_outpt    = Q10,  # 1=Yes, 2=No
    regp_prev_inpt     = Q12,  # 1=Yes, 2=No
    regp_onset_year    = Q16   # Year of symptom onset
  ) %>%
  mutate(
    # --- admission date & year ---
    regp_admission_dt   = lubridate::dmy_hm(assessment_start_date, tz = "UTC"),
    regp_admission_date = as_date(regp_admission_dt),
    admission_year      = lubridate::year(regp_admission_dt),

    # --- numeric cleaning ---
    regp_birth_year_num   = clean_num(regp_birth_year),
    regp_onset_year_num0  = clean_num(regp_onset_year),
    regp_sickleave_m_num0 = clean_num(regp_sickleave_m),

    # Sick leave months: keep valid (>=0 & <= 12); junk -> NA
    regp_sickleave_m_num = dplyr::if_else(
      !is.na(regp_sickleave_m_num0) & regp_sickleave_m_num0 >= 0 & regp_sickleave_m_num0 <= 12,
      regp_sickleave_m_num0, NA_real_
    ),

    # Onset year: drop implausible values (<1930 or > admission year)
    regp_onset_year_num = dplyr::if_else(
      !is.na(regp_onset_year_num0) & regp_onset_year_num0 >= 1930 &
        (!is.na(admission_year) & regp_onset_year_num0 <= admission_year),
      regp_onset_year_num0, NA_real_
    ),

    # --- derived continuous ---
    age_at_admission = dplyr::if_else(
      !is.na(admission_year) & !is.na(regp_birth_year_num),
      admission_year - regp_birth_year_num, NA_real_
    ),
    # (optional) wipe out clearly impossible ages
    age_at_admission = dplyr::if_else(
      !is.na(age_at_admission) & (age_at_admission < 16 | age_at_admission > 100),
      NA_real_, age_at_admission
    ),

    symptom_duration = dplyr::if_else(
      !is.na(admission_year) & !is.na(regp_onset_year_num),
      admission_year - regp_onset_year_num, NA_real_
    ),

    # --- binaries you want ---
    regp_partner_bin = dplyr::case_when(
      regp_partner == 1           ~ 1,   # married/partner
      regp_partner %in% c(2, 3)   ~ 0,   # alone or other
      TRUE                        ~ NA_real_
    ),
    regp_work_pastyear_bin = dplyr::case_when(
      regp_work_pastyear == 1 ~ 1,
      regp_work_pastyear == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    regp_work_current_bin = dplyr::case_when(
      regp_work_current == 1 ~ 1,
      regp_work_current == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    regp_sickleave_bin = dplyr::case_when(
      regp_sickleave_m_num > 0  ~ 1,
      regp_sickleave_m_num == 0 ~ 0,
      TRUE ~ NA_real_
    ),
    regp_prev_outpt_bin = dplyr::case_when(
      regp_prev_outpt == 1 ~ 1,
      regp_prev_outpt == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    regp_prev_inpt_bin = dplyr::case_when(
      regp_prev_inpt == 1 ~ 1,
      regp_prev_inpt == 2 ~ 0,
      TRUE ~ NA_real_
    ),
    symptom_duration_bin = dplyr::case_when(
      symptom_duration >= 5 ~ 1,   # chronic
      symptom_duration <  5 ~ 0,   # acute
      TRUE ~ NA_real_
    )
  ) %>%
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id, treatment_type_name,
    regp_gender,
    age_at_admission,
    regp_partner_bin,
    regp_work_pastyear_bin,
    regp_work_current_bin,
    regp_prev_outpt_bin,
    regp_prev_inpt_bin,
    symptom_duration_bin
  )

#Quality control
summary(REGP[c("age_at_admission","regp_sickleave_bin","symptom_duration_bin")])
sapply(REGP, function(x) sum(is.na(x)))

# I dropped Sick leave (Q8) → ~20% missing 
# Symptom duration (Q16) → ~18% missing but i have to keep it in for now

# Check N
print(summarize_patient_counts(REGP))