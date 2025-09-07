# ---------------------------------------------------------
# Load IIP Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw IIP assessment data and consent records.
# ---------------------------------------------------------

# Libraries
library(readxl)
library(readr)
library(dplyr)

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

IIP <- read_excel("/Users/maggieheimvik/Desktop/COPE/data/dataset/IIP64_avid.xls") #13646 obs of 144 var. 

# ---------------------------------------------------------
# Rename n select IIP column names for consistency
# ---------------------------------------------------------

IIP <- IIP %>%
  select(
    `respondent id`, `assessment instance context label`,
    `treatment id`, `treatment name`, `treatment type id`,
    starts_with("calculation:IIP-64-")
  ) %>%
  mutate(across(
    matches("^calculation:IIP-64-.*-(Raw|AA)$"),
    ~ suppressWarnings(as.numeric(na_if(trimws(.), "")))
  )) %>%
  rename(
    respondent_id            = `respondent id`,
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`,

    # raw
    iip_pa = `calculation:IIP-64-Domi-Raw`,
    iip_bc = `calculation:IIP-64-Hevn-Raw`,
    iip_de = `calculation:IIP-64-Kald-Raw`,
    iip_fg = `calculation:IIP-64-Usikk-Raw`,
    iip_hi = `calculation:IIP-64-Lise-Raw`,
    iip_jk = `calculation:IIP-64-Foye-Raw`,
    iip_lm = `calculation:IIP-64-Ofre-Raw`,
    iip_no = `calculation:IIP-64-Kreve-Raw`,

    # answered
    iip_pa_aa = `calculation:IIP-64-Domi-AA`,
    iip_bc_aa = `calculation:IIP-64-Hevn-AA`,
    iip_de_aa = `calculation:IIP-64-Kald-AA`,
    iip_fg_aa = `calculation:IIP-64-Usikk-AA`,
    iip_hi_aa = `calculation:IIP-64-Lise-AA`,
    iip_jk_aa = `calculation:IIP-64-Foye-AA`,
    iip_lm_aa = `calculation:IIP-64-Ofre-AA`,
    iip_no_aa = `calculation:IIP-64-Kreve-AA`
  ) %>%
  mutate(
    iip_pa_prorated = if_else(iip_pa_aa/8 >= 0.70, score_prorate(iip_pa, iip_pa_aa, n_total = 8, min_prop = 0.70), NA_real_),
    iip_bc_prorated = if_else(iip_bc_aa/8 >= 0.70, score_prorate(iip_bc, iip_bc_aa, n_total = 8, min_prop = 0.70), NA_real_),
    iip_de_prorated = if_else(iip_de_aa/8 >= 0.70, score_prorate(iip_de, iip_de_aa, n_total = 8, min_prop = 0.70), NA_real_),
    iip_fg_prorated = if_else(iip_fg_aa/8 >= 0.70, score_prorate(iip_fg, iip_fg_aa, n_total = 8, min_prop = 0.70), NA_real_),
    iip_hi_prorated = if_else(iip_hi_aa/8 >= 0.70, score_prorate(iip_hi, iip_hi_aa, n_total = 8, min_prop = 0.70), NA_real_),
    iip_jk_prorated = if_else(iip_jk_aa/8 >= 0.70, score_prorate(iip_jk, iip_jk_aa, n_total = 8, min_prop = 0.70), NA_real_),
    iip_lm_prorated = if_else(iip_lm_aa/8 >= 0.70, score_prorate(iip_lm, iip_lm_aa, n_total = 8, min_prop = 0.70), NA_real_),
    iip_no_prorated = if_else(iip_no_aa/8 >= 0.70, score_prorate(iip_no, iip_no_aa, n_total = 8, min_prop = 0.70), NA_real_)
  ) %>%
  select(
    respondent_id,
    assessment_context_label,
    treatment_id,
    treatment_name,
    treatment_type_id,

    # raw + prorated
    iip_pa, iip_pa_prorated,
    iip_bc, iip_bc_prorated,
    iip_de, iip_de_prorated,
    iip_fg, iip_fg_prorated,
    iip_hi, iip_hi_prorated,
    iip_jk, iip_jk_prorated,
    iip_lm, iip_lm_prorated,
    iip_no, iip_no_prorated
  )

# Check N
print(summarize_patient_counts(IIP))
