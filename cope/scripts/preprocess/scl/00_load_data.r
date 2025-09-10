# ---------------------------------------------------------
# Load Symptom Checklist 90 Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw SCL assessment data and consent records.
# ---------------------------------------------------------

# Libraries
library(readxl)     # for reading Excel files
library(readr)      # for reading CSV files
library(dplyr)      # for data manipulation

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# SCL Assessment data
SCL<- read_csv2(file.path(data_dir, "SCL90.csv"))  #2081 obs of 1 variable

# ---------------------------------------------------------
# Rename n select SCL column names for consistency
# ---------------------------------------------------------
SCL <- SCL %>%
  # keep IDs, treatment, gender, and only the T-score columns we need
  select(
    respondent.id,
    assessment.instance.context.label,
    treatment.id, treatment.name, treatment.type.id,
    respondent.gender,
    
    # Global indices (T by sex + validity)
    calculation.GSI.T.Female, calculation.GSI.T.Male, calculation.GSI.Valid,
    calculation.PSDI.T.Female, calculation.PSDI.T.Male,
    calculation.PST.T.Female,  calculation.PST.T.Male,
    
    # Dimension T by sex
    calculation.Angst.T.Female,           calculation.Angst.T.Male,
    calculation.Depresjon.T.Female,       calculation.Depresjon.T.Male,
    calculation.Fiendtlighet.T.Female,    calculation.Fiendtlighet.T.Male,
    calculation.Fobisk.T.Female,          calculation.Fobisk.T.Male,
    calculation.Interpersonlig.T.Female,  calculation.Interpersonlig.T.Male,
    calculation.Paranoid.T.Female,        calculation.Paranoid.T.Male,
    calculation.Psykotisisme.T.Female,    calculation.Psykotisisme.T.Male,
    calculation.Somatisering.T.Female,    calculation.Somatisering.T.Male,
    calculation.Tvangssymptomer.T.Female, calculation.Tvangssymptomer.T.Male
  ) %>%
  # standardize names
  rename(
    respondent_id            = respondent.id,
    assessment_context_label = assessment.instance.context.label,
    treatment_id             = treatment.id,
    treatment_name           = treatment.name,
    treatment_type_id        = treatment.type.id,
    scl_gender               = respondent.gender
  ) %>%
  # collapse sex-specific T to a single T per scale
  mutate(
    # globals
    calc_scl_gsi_t  = case_when(
      scl_gender == "female" ~ calculation.GSI.T.Female,
      scl_gender == "male"   ~ calculation.GSI.T.Male,
      TRUE ~ NA_real_
    ),
    calc_scl_psdi_t = case_when(
      scl_gender == "female" ~ calculation.PSDI.T.Female,
      scl_gender == "male"   ~ calculation.PSDI.T.Male,
      TRUE ~ NA_real_
    ),
    calc_scl_pst_t  = case_when(
      scl_gender == "female" ~ calculation.PST.T.Female,
      scl_gender == "male"   ~ calculation.PST.T.Male,
      TRUE ~ NA_real_
    ),
    
    # dimensions
    calc_scl_anxiety_t        = if_else(scl_gender == "female", calculation.Angst.T.Female,           calculation.Angst.T.Male,           missing = NA_real_),
    calc_scl_depression_t     = if_else(scl_gender == "female", calculation.Depresjon.T.Female,       calculation.Depresjon.T.Male,       missing = NA_real_),
    calc_scl_hostility_t      = if_else(scl_gender == "female", calculation.Fiendtlighet.T.Female,    calculation.Fiendtlighet.T.Male,    missing = NA_real_),
    calc_scl_phobic_t         = if_else(scl_gender == "female", calculation.Fobisk.T.Female,          calculation.Fobisk.T.Male,          missing = NA_real_),
    calc_scl_interpersonal_t  = if_else(scl_gender == "female", calculation.Interpersonlig.T.Female,  calculation.Interpersonlig.T.Male,  missing = NA_real_),
    calc_scl_paranoid_t       = if_else(scl_gender == "female", calculation.Paranoid.T.Female,        calculation.Paranoid.T.Male,        missing = NA_real_),
    calc_scl_psychoticism_t   = if_else(scl_gender == "female", calculation.Psykotisisme.T.Female,    calculation.Psykotisisme.T.Male,    missing = NA_real_),
    calc_scl_somatization_t   = if_else(scl_gender == "female", calculation.Somatisering.T.Female,    calculation.Somatisering.T.Male,    missing = NA_real_),
    calc_scl_ocd_t            = if_else(scl_gender == "female", calculation.Tvangssymptomer.T.Female, calculation.Tvangssymptomer.T.Male, missing = NA_real_)
  ) %>%
  # final tidy set: IDs + one T per scale
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,
    calc_scl_gsi_t, calc_scl_gsi_valid = calculation.GSI.Valid,
    calc_scl_psdi_t, calc_scl_pst_t,
    calc_scl_anxiety_t, calc_scl_depression_t, calc_scl_hostility_t, calc_scl_phobic_t,
    calc_scl_interpersonal_t, calc_scl_paranoid_t, calc_scl_psychoticism_t,
    calc_scl_somatization_t, calc_scl_ocd_t
  )

# Keep only those where scl is valid
SCL <- SCL %>%
  filter(as.integer(calc_scl_gsi_valid) == 1L)  %>%    # 2070 obs. of 18 variables 
  select(-calc_scl_gsi_valid, -calc_scl_gsi_t)    
                        # remove the column
# Quality Control
sapply(SCL, function(x) sum(is.na(x)))
summary(SCL)

# Check N
print(summarize_patient_counts(SCL))
