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

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# REGP Assessment data
REGP <- read_csv2("/Users/maggieheimvik/Desktop/COPE/data/dataset/regp.csv") #487 obs. of 44 variables with the new dataset

# ---------------------------------------------------------
# Rename n select REGP column names for consistency
# ---------------------------------------------------------

REGP <- REGP %>%
  select(
    respondent.id,
    assessment.instance.context.label,
    treatment.id, treatment.name,
    treatment.type.id, treatment.type.name,
    respondent.gender,
    Q1:Q16
  ) %>%
  rename(
    respondent_id            = respondent.id,
    assessment_context_label = assessment.instance.context.label,
    treatment_id             = treatment.id,
    treatment_name           = treatment.name,
    treatment_type_id        = treatment.type.id,
    treatment_type_name      = treatment.type.name,
    regp_gender              = respondent.gender,

    regp_birth_year          = Q1,
    regp_partner             = Q2,
    regp_children_n          = Q3,
    regp_work_status         = Q4,  # 1..7
    regp_work_pastyear       = Q5,  # 1/2 (Yes/No)
    regp_work_current        = Q6,  # 1/2 (Yes/No)
    regp_work_paid_months    = Q7,
    regp_sickleave_months    = Q8,
    regp_sickleave_reason    = Q9,  # 1..4 
    regp_prev_outpatient     = Q10, # 1/2 (Yes/No)
    regp_prev_outpatient_len = Q11,
    regp_prev_inpatient      = Q12, # 1/2 (Yes/No)
    regp_prev_inpatient_len  = Q13,
    regp_prev_mbad           = Q14,
    regp_prev_mbad_len       = Q15,
    regp_onset_year          = Q16
  ) %>%
  mutate(
    # maps / types
    regp_partner = factor(regp_partner, levels = 1:3,
                          labels = c("Partnered","Alone","Other")),
    regp_prev_outpatient = factor(regp_prev_outpatient, levels = c(1,2),
                                  labels = c("Yes","No")),
    regp_prev_inpatient  = factor(regp_prev_inpatient,  levels = c(1,2),
                                  labels = c("Yes","No")),

    regp_work_status = factor(regp_work_status,
      levels = 1:7,
      labels = c("Own work","Dependent income","Sick pay",
                 "Work allowance","Disability benefit","Student loans","Other")
    ),
    regp_work_pastyear = factor(regp_work_pastyear, levels = c(1,2), labels = c("Yes","No")),
    regp_work_current  = factor(regp_work_current,  levels = c(1,2), labels = c("Yes","No")),

    regp_work_paid_months = suppressWarnings(as.numeric(regp_work_paid_months)),
    regp_sickleave_months = suppressWarnings(as.numeric(regp_sickleave_months)),
    regp_work_paid_months = ifelse(is.na(regp_work_paid_months), NA, pmax(0, pmin(12, regp_work_paid_months))),
    regp_sickleave_months = ifelse(is.na(regp_sickleave_months), NA, pmax(0, pmin(12, regp_sickleave_months))),

    regp_sickleave_reason = factor(regp_sickleave_reason,
      levels = 1:4,
      labels = c("Yes (due to admission problem)","No","Don't know","Not on sick leave")
    ),
    regp_sickleave_reason_collapsed = dplyr::case_when(
      regp_sickleave_reason == "Yes (due to admission problem)" ~ "Due_to_problem",
      regp_sickleave_reason %in% c("No","Not on sick leave")     ~ "Not_due_or_not_on",
      TRUE ~ "Unknown"
    ),

    regp_employed_bin = dplyr::case_when(
      regp_work_current == "Yes" ~ 1L,
      regp_work_status == "Own work" ~ 1L,
      is.na(regp_work_current) & is.na(regp_work_status) ~ NA_integer_,
      TRUE ~ 0L
    ),
    regp_employment_cat = dplyr::case_when(
      regp_work_current == "Yes" | regp_work_status == "Own work" ~ "Employed",
      regp_work_status %in% c("Sick pay","Work allowance","Disability benefit") ~ "On_benefits",
      regp_work_status == "Student loans" ~ "Student",
      regp_work_status %in% c("Dependent income","Other") ~ "Other_unsupported",
      is.na(regp_work_status) ~ "Unknown",
      TRUE ~ "Other_unsupported"
    ),
    regp_work_paid_months_bin = dplyr::case_when(
      is.na(regp_work_paid_months) ~ "Unknown",
      regp_work_paid_months == 0 ~ "0",
      regp_work_paid_months <= 5 ~ "1_5",
      TRUE ~ "6_12"
    ),
    regp_sickleave_months_bin = dplyr::case_when(
      is.na(regp_sickleave_months) ~ "Unknown",
      regp_sickleave_months == 0 ~ "0",
      regp_sickleave_months <= 3 ~ "1_3",
      regp_sickleave_months <= 6 ~ "4_6",
      TRUE ~ "7_12"
    )
  ) %>%
  # keep only requested + work/functional
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,

    # requested keeps
    regp_gender, regp_birth_year, regp_partner,
    regp_prev_outpatient, regp_prev_inpatient,

    # work/functional originals (cleaned)
    regp_work_status, regp_work_pastyear, regp_work_current,
    regp_work_paid_months, regp_sickleave_months, regp_sickleave_reason,

    # derived bins
    regp_employed_bin, regp_employment_cat,
    regp_work_paid_months_bin, regp_sickleave_months_bin,
    regp_sickleave_reason_collapsed
  )
                                                                  #429 obs. of 11 variables 

# Check N
print(summarize_patient_counts(REGP))