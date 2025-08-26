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
library(stringr)

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# SCL Assessment data
SCL<- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/SCL_a.csv")

# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var
# ---------------------------------------------------------
# Rename n select SCL column names for consistency
# ---------------------------------------------------------

# Select only from the dataset
SCL <- SCL %>%
  # select existing columns (already renamed in your data)
  select(
    respondent_id,
    assessment_context_label,
    treatment_id,
    treatment_name,
    treatment_type_id,
    matches("^Q\\d+$"),
    starts_with("calc_")
  ) %>%
  # Q1..Q90 -> scl_q01..scl_q90
  rename_with(
    ~ sprintf("scl_q%02d", as.integer(sub("^Q", "", .x))),
    matches("^Q\\d+$")
  ) %>%
  # map current calc_* names -> consistent calc_scl_* names
  rename(
    # Anxiety
    calc_scl_anxiety_raw          = calc_angst_raw,
    calc_scl_anxiety_sum          = calc_angst_sum,
    calc_scl_anxiety_t_female     = calc_angst_female,
    calc_scl_anxiety_t_male       = calc_angst_male,
    calc_scl_anxiety_valid        = calc_angst_valid,

    # Depression
    calc_scl_depression_raw       = calc_dep_raw,
    calc_scl_depression_sum       = calc_dep_sum,
    calc_scl_depression_t_female  = calc_dep_female,
    calc_scl_depression_t_male    = calc_dep_male,
    calc_scl_depression_valid     = calc_dep_valid,

    # Hostility
    calc_scl_hostility_raw        = calc_host_raw,
    calc_scl_hostility_sum        = calc_host_sum,
    calc_scl_hostility_t_female   = calc_host_female,
    calc_scl_hostility_t_male     = calc_host_male,
    calc_scl_hostility_valid      = calc_host_valid,

    # Phobic anxiety
    calc_scl_phobic_raw           = calc_phob_raw,
    calc_scl_phobic_sum           = calc_phob_sum,
    calc_scl_phobic_t_female      = calc_phob_female,
    calc_scl_phobic_t_male        = calc_phob_male,
    calc_scl_phobic_valid         = calc_phob_valid,

    # Interpersonal sensitivity
    calc_scl_interpersonal_raw       = calc_inter_raw,
    calc_scl_interpersonal_sum       = calc_inter_sum,
    calc_scl_interpersonal_t_female  = calc_inter_female,
    calc_scl_interpersonal_t_male    = calc_inter_male,
    calc_scl_interpersonal_valid     = calc_inter_valid,

    # Paranoid ideation
    calc_scl_paranoid_raw         = calc_paranoid_raw,
    calc_scl_paranoid_sum         = calc_paranoid_sum,
    calc_scl_paranoid_t_female    = calc_paranoid_female,
    calc_scl_paranoid_t_male      = calc_paranoid_male,
    calc_scl_paranoid_valid       = calc_paranoid_valid,

    # Psychoticism
    calc_scl_psychoticism_raw     = calc_psycho_raw,
    calc_scl_psychoticism_sum     = calc_psycho_sum,
    calc_scl_psychoticism_t_female= calc_psycho_female,
    calc_scl_psychoticism_t_male  = calc_psycho_male,
    calc_scl_psychoticism_valid   = calc_psycho_valid,

    # Somatization
    calc_scl_somatization_raw     = calc_soma_raw,
    calc_scl_somatization_sum     = calc_soma_sum,
    calc_scl_somatization_t_female= calc_soma_female,
    calc_scl_somatization_t_male  = calc_soma_male,
    calc_scl_somatization_valid   = calc_soma_valid,

    # Additional items
    calc_scl_additional_raw       = calc_add_raw,
    calc_scl_additional_sum       = calc_add_sum,
    calc_scl_additional_valid     = calc_add_valid,

    # Obsessive-compulsive (Tvangssymptomer)
    calc_scl_ocd_raw              = calc_compul_raw,
    calc_scl_ocd_sum              = calc_compul_sum,
    calc_scl_ocd_t_female         = calc_compul_female,
    calc_scl_ocd_t_male           = calc_compul_male,
    calc_scl_ocd_valid            = calc_compul_valid,

    # Global indices (sex-specific T + validity)
    calc_scl_gsi_t_female         = calc_GSI_female,
    calc_scl_gsi_t_male           = calc_GSI_male,
    calc_scl_gsi_valid            = calc_GSI_valid,

    calc_scl_psdi_t_female        = calc_PSDI_female,
    calc_scl_psdi_t_male          = calc_PSDI_male,

    calc_scl_pst_t_female         = calc_PST_female,
    calc_scl_pst_t_male           = calc_PST_male,

    # SCL-90-R summaries
    calc_scl_total_aa             = calc_r_AA,
    calc_scl_total_gsi            = calc_r_GSI,
    calc_scl_total_psdi           = calc_r_PSDI,
    calc_scl_total_pst            = calc_r_PST,
    calc_scl_total_sum            = calc_r_total_sum,

    # PSI
    calc_scl_psi_valid            = calc_r_PSI_valid,
    calc_scl_psi                  = calc_r_PSI
  )

# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

  # Merge only the 'consent' column with the SCL dataset by 'respondent_id'
SCL <- merge(PSWQ, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
SCL  <- SCL  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(SCL))