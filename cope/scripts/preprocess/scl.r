# ---------------------------------------------------------
# Load Symptom Checklist 90 Data
# Author: MochiBear.Hei
# Created: 2025-09-10
# Description: Loads raw SCL assessment data
# ---------------------------------------------------------

library(readr)
library(dplyr)

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# SCL Assessment data
SCL<- read_csv2(file.path(data_dir, "SCL90.csv"))  #2081 obs of 1 variable

# ---------------------------------------------------------
# Rename n select SCL column names for consistency
# ---------------------------------------------------------
SCL <- SCL %>%
  select(
    respondent.id,
    assessment.instance.context.label,
    treatment.id, treatment.name, treatment.type.id,
    respondent.gender,

    # GSI + validity
    calculation.GSI.T.Female, calculation.GSI.T.Male,
    calculation.GSI.Valid,

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
    calculation.Tvangssymptomer.T.Female, calculation.Tvangssymptomer.T.Male,

    # <-- add per-dimension validity flags
    calculation.Angst.Valid,
    calculation.Depresjon.Valid,
    calculation.Fiendtlighet.Valid,
    calculation.Fobisk.Valid,
    calculation.Interpersonlig.Valid,
    calculation.Paranoid.Valid,
    calculation.Psykotisisme.Valid,
    calculation.Somatisering.Valid,
    calculation.Tvangssymptomer.Valid
  ) %>%
  rename(
    respondent_id            = respondent.id,
    assessment_context_label = assessment.instance.context.label,
    treatment_id             = treatment.id,
    treatment_name           = treatment.name,
    treatment_type_id        = treatment.type.id,
    scl_gender               = respondent.gender
  ) %>%
  # 2) collapse sex-specific T to one T per scale
  mutate(
    scl_gsi_t  = case_when(
      scl_gender == "female" ~ calculation.GSI.T.Female,
      scl_gender == "male"   ~ calculation.GSI.T.Male,
      TRUE ~ NA_real_
    ),
    scl_psdi_t = case_when(
      scl_gender == "female" ~ calculation.PSDI.T.Female,
      scl_gender == "male"   ~ calculation.PSDI.T.Male,
      TRUE ~ NA_real_
    ),
    scl_pst_t  = case_when(
      scl_gender == "female" ~ calculation.PST.T.Female,
      scl_gender == "male"   ~ calculation.PST.T.Male,
      TRUE ~ NA_real_
    ),
    scl_anxiety_t = if_else(
      scl_gender == "female", calculation.Angst.T.Female,
      calculation.Angst.T.Male, missing = NA_real_
    ),
    scl_depression_t = if_else(
      scl_gender == "female", calculation.Depresjon.T.Female,
      calculation.Depresjon.T.Male, missing = NA_real_
    ),
    scl_hostility_t = if_else(
      scl_gender == "female", calculation.Fiendtlighet.T.Female,
      calculation.Fiendtlighet.T.Male, missing = NA_real_
    ),
    scl_phobic_t = if_else(
      scl_gender == "female", calculation.Fobisk.T.Female,
      calculation.Fobisk.T.Male, missing = NA_real_
    ),
    scl_interpersonal_t = if_else(
      scl_gender == "female", calculation.Interpersonlig.T.Female,
      calculation.Interpersonlig.T.Male, missing = NA_real_
    ),
    scl_paranoid_t = if_else(
      scl_gender == "female", calculation.Paranoid.T.Female,
      calculation.Paranoid.T.Male, missing = NA_real_
    ),
    scl_psychoticism_t = if_else(
      scl_gender == "female", calculation.Psykotisisme.T.Female,
      calculation.Psykotisisme.T.Male, missing = NA_real_
    ),
    scl_somatization_t = if_else(
      scl_gender == "female", calculation.Somatisering.T.Female,
      calculation.Somatisering.T.Male, missing = NA_real_
    ),
    scl_ocd_t = if_else(
      scl_gender == "female", calculation.Tvangssymptomer.T.Female,
      calculation.Tvangssymptomer.T.Male, missing = NA_real_
    )
  ) %>%
  # 3) mask T-scores when their dimension’s Valid != 1
  mutate(
    scl_anxiety_t = if_else(
      as.integer(calculation.Angst.Valid) == 1L,
      scl_anxiety_t, NA_real_
    ),
    scl_depression_t = if_else(
      as.integer(calculation.Depresjon.Valid) == 1L,
      scl_depression_t, NA_real_
    ),
    scl_hostility_t = if_else(
      as.integer(calculation.Fiendtlighet.Valid) == 1L,
      scl_hostility_t, NA_real_
    ),
    scl_phobic_t = if_else(
      as.integer(calculation.Fobisk.Valid) == 1L,
      scl_phobic_t, NA_real_
    ),
    scl_interpersonal_t = if_else(
      as.integer(calculation.Interpersonlig.Valid) == 1L,
      scl_interpersonal_t, NA_real_
    ),
    scl_paranoid_t = if_else(
      as.integer(calculation.Paranoid.Valid) == 1L,
      scl_paranoid_t, NA_real_
    ),
    scl_psychoticism_t = if_else(
      as.integer(calculation.Psykotisisme.Valid) == 1L,
      scl_psychoticism_t, NA_real_
    ),
    scl_somatization_t = if_else(
      as.integer(calculation.Somatisering.Valid) == 1L,
      scl_somatization_t, NA_real_
    ),
    scl_ocd_t = if_else(
      as.integer(calculation.Tvangssymptomer.Valid) == 1L,
      scl_ocd_t, NA_real_
    )
  )

# 4) keep only IDs + final T scores; filter on global validity
SCL <- SCL %>%
  filter(as.integer(calculation.GSI.Valid) == 1L) %>%
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,
    scl_psdi_t, scl_pst_t,
    scl_anxiety_t, scl_depression_t, scl_hostility_t,
    scl_phobic_t, scl_interpersonal_t, scl_paranoid_t,
    scl_psychoticism_t, scl_somatization_t, scl_ocd_t
  )

# Quality Control
sapply(SCL, function(x) sum(is.na(x)))
summary(SCL)
# used modum derived t scores because the raw scores were not falling in the 
#plausable range, not sure why

