# ---------------------------------------------------------
# Load IIP Data
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw IIP assessment data
# ---------------------------------------------------------

library(readxl)
library(dplyr)

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

IIP <- read_excel(file.path(data_dir, "IIP64_avid.xls")) #13646 obs of 144 var.

# ---------------------------------------------------------
# Rename n select IIP column names for consistency
# ---------------------------------------------------------

IIP <- IIP %>%
  select(
    `respondent id`, `assessment instance context label`,
    `treatment id`, `treatment name`, `treatment type id`,
    # keep AA (answered counts) and T (T-scores); ignore P/PIP/Raw
    `calculation:IIP-64-Domi-AA`, `calculation:IIP-64-Domi-T`,
    `calculation:IIP-64-Hevn-AA`, `calculation:IIP-64-Hevn-T`,
    `calculation:IIP-64-Kald-AA`, `calculation:IIP-64-Kald-T`,
    `calculation:IIP-64-Usikk-AA`, `calculation:IIP-64-Usikk-T`,
    `calculation:IIP-64-Lise-AA`, `calculation:IIP-64-Lise-T`,
    `calculation:IIP-64-Foye-AA`, `calculation:IIP-64-Foye-T`,
    `calculation:IIP-64-Ofre-AA`, `calculation:IIP-64-Ofre-T`,
    `calculation:IIP-64-Kreve-AA`, `calculation:IIP-64-Kreve-T`
  ) %>%
  rename(
    respondent_id            = `respondent id`,
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`,

    iip_pa_aa = `calculation:IIP-64-Domi-AA`,
    iip_pa_t  = `calculation:IIP-64-Domi-T`,

    iip_bc_aa = `calculation:IIP-64-Hevn-AA`,
    iip_bc_t  = `calculation:IIP-64-Hevn-T`,

    iip_de_aa = `calculation:IIP-64-Kald-AA`,
    iip_de_t  = `calculation:IIP-64-Kald-T`,

    iip_fg_aa = `calculation:IIP-64-Usikk-AA`,
    iip_fg_t  = `calculation:IIP-64-Usikk-T`,

    iip_hi_aa = `calculation:IIP-64-Lise-AA`,
    iip_hi_t  = `calculation:IIP-64-Lise-T`,

    iip_jk_aa = `calculation:IIP-64-Foye-AA`,
    iip_jk_t  = `calculation:IIP-64-Foye-T`,

    iip_lm_aa = `calculation:IIP-64-Ofre-AA`,
    iip_lm_t  = `calculation:IIP-64-Ofre-T`,

    iip_no_aa = `calculation:IIP-64-Kreve-AA`,
    iip_no_t  = `calculation:IIP-64-Kreve-T`
  ) %>%
  # coerce AA and T robustly
  mutate(
    across(ends_with("_aa"), ~ suppressWarnings(as.numeric(.))),
    across(ends_with("_t"),  ~ suppressWarnings(as.numeric(.)))
  ) %>%
  # keep T only if AA >= 6 (70% of 8 items); else NA
  mutate(
    across(ends_with("_t"), ~ .x, .names = "{.col}_orig"),
    iip_pa_t = if_else(iip_pa_aa >= 6, iip_pa_t, NA_real_),
    iip_bc_t = if_else(iip_bc_aa >= 6, iip_bc_t, NA_real_),
    iip_de_t = if_else(iip_de_aa >= 6, iip_de_t, NA_real_),
    iip_fg_t = if_else(iip_fg_aa >= 6, iip_fg_t, NA_real_),
    iip_hi_t = if_else(iip_hi_aa >= 6, iip_hi_t, NA_real_),
    iip_jk_t = if_else(iip_jk_aa >= 6, iip_jk_t, NA_real_),
    iip_lm_t = if_else(iip_lm_aa >= 6, iip_lm_t, NA_real_),
    iip_no_t = if_else(iip_no_aa >= 6, iip_no_t, NA_real_)
  ) %>%
# basic sanity cap for T-scores (expected ~20–100)
mutate(
  across(
    ends_with("_t"),
    ~ ifelse(!is.na(.x) & (.x < 20 | .x > 100), NA_real_, .x)
  )
) %>%
select(
  respondent_id,
  assessment_context_label,
  treatment_id,
  treatment_name,
  treatment_type_id,
  iip_pa_t,
  iip_bc_t,
  iip_de_t,
  iip_fg_t,
  iip_hi_t,
  iip_jk_t,
  iip_lm_t,
  iip_no_t
)

  # Quality Control
sapply(IIP, function(x) sum(is.na(x)))
summary(IIP)