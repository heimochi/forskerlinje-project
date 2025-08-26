# ---------------------------------------------------------
# Load IIP Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw IIP assessment data and consent records.
# ---------------------------------------------------------

# Libraries
library(readxl)     # for reading Excel files
library(readr)      # for reading CSV files
library(dplyr)      # for data manipulation

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# IIP Assessment data
IIP <- read_excel("/Users/maggieheimvik/Desktop/COPE/data/dataset/IIP64_avid.xls") #13646 obs of 144 var. 

# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var
# ---------------------------------------------------------
# Rename n select IIP column names for consistency
# ---------------------------------------------------------

# Select only from the dataset
IIP <- IIP %>%
  select(
    `assessment instance context label`,
    `treatment id`,
    `treatment name`,
    `treatment type id`,
    `respondent id`,
    starts_with("Q"),
    starts_with("calc")
  ) %>%
  rename(
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`,
    respondent_id            = `respondent id`,

    # IIP subscales
    calc_iip_domineering_aa   = `calculation:IIP-64-Domi-AA`,
    calc_iip_domineering_p    = `calculation:IIP-64-Domi-P`,
    calc_iip_domineering_pip  = `calculation:IIP-64-Domi-PIP`,
    calc_iip_domineering_raw  = `calculation:IIP-64-Domi-Raw`,
    calc_iip_domineering_t    = `calculation:IIP-64-Domi-T`,
    calc_iip_domineering_tip  = `calculation:IIP-64-Domi-TIP`,

    calc_iip_vindictive_aa    = `calculation:IIP-64-Foye-AA`,
    calc_iip_vindictive_p     = `calculation:IIP-64-Foye-P`,
    calc_iip_vindictive_pip   = `calculation:IIP-64-Foye-PIP`,
    calc_iip_vindictive_raw   = `calculation:IIP-64-Foye-Raw`,
    calc_iip_vindictive_t     = `calculation:IIP-64-Foye-T`,
    calc_iip_vindictive_tip   = `calculation:IIP-64-Foye-TIP`,

    calc_iip_cold_aa          = `calculation:IIP-64-Hevn-AA`,
    calc_iip_cold_p           = `calculation:IIP-64-Hevn-P`,
    calc_iip_cold_pip         = `calculation:IIP-64-Hevn-PIP`,
    calc_iip_cold_raw         = `calculation:IIP-64-Hevn-Raw`,
    calc_iip_cold_t           = `calculation:IIP-64-Hevn-T`,
    calc_iip_cold_tip         = `calculation:IIP-64-Hevn-TIP`,

    calc_iip_inhibited_aa     = `calculation:IIP-64-Kald-AA`,
    calc_iip_inhibited_p      = `calculation:IIP-64-Kald-P`,
    calc_iip_inhibited_pip    = `calculation:IIP-64-Kald-PIP`,
    calc_iip_inhibited_raw    = `calculation:IIP-64-Kald-Raw`,
    calc_iip_inhibited_t      = `calculation:IIP-64-Kald-T`,
    calc_iip_inhibited_tip    = `calculation:IIP-64-Kald-TIP`,

    calc_iip_nonassertive_aa  = `calculation:IIP-64-Kreve-AA`,
    calc_iip_nonassertive_p   = `calculation:IIP-64-Kreve-P`,
    calc_iip_nonassertive_pip = `calculation:IIP-64-Kreve-PIP`,
    calc_iip_nonassertive_raw = `calculation:IIP-64-Kreve-Raw`,
    calc_iip_nonassertive_t   = `calculation:IIP-64-Kreve-T`,
    calc_iip_nonassertive_tip = `calculation:IIP-64-Kreve-TIP`,

    calc_iip_accommodating_aa  = `calculation:IIP-64-Lise-AA`,
    calc_iip_accommodating_p   = `calculation:IIP-64-Lise-P`,
    calc_iip_accommodating_pip = `calculation:IIP-64-Lise-PIP`,
    calc_iip_accommodating_raw = `calculation:IIP-64-Lise-Raw`,
    calc_iip_accommodating_t   = `calculation:IIP-64-Lise-T`,
    calc_iip_accommodating_tip = `calculation:IIP-64-Lise-TIP`,

    calc_iip_selfsac_aa       = `calculation:IIP-64-Ofre-AA`,
    calc_iip_selfsac_p        = `calculation:IIP-64-Ofre-P`,
    calc_iip_selfsac_pip      = `calculation:IIP-64-Ofre-PIP`,
    calc_iip_selfsac_raw      = `calculation:IIP-64-Ofre-Raw`,
    calc_iip_selfsac_t        = `calculation:IIP-64-Ofre-T`,
    calc_iip_selfsac_tip      = `calculation:IIP-64-Ofre-TIP`,

    calc_iip_intrusive_aa     = `calculation:IIP-64-Usikk-AA`,
    calc_iip_intrusive_p      = `calculation:IIP-64-Usikk-P`,
    calc_iip_intrusive_pip    = `calculation:IIP-64-Usikk-PIP`,
    calc_iip_intrusive_raw    = `calculation:IIP-64-Usikk-Raw`,
    calc_iip_intrusive_t      = `calculation:IIP-64-Usikk-T`,
    calc_iip_intrusive_tip    = `calculation:IIP-64-Usikk-TIP`,

    calc_iip_total_aa         = `calculation:IIP-64-Tot-AA`,
    calc_iip_total_p          = `calculation:IIP-64-Tot-P`,
    calc_iip_total_raw        = `calculation:IIP-64-Tot-Raw`,
    calc_iip_total_t          = `calculation:IIP-64-Tot-T`
  )

# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

  # Merge only the 'consent' column with the IIP dataset by 'respondent_id'
IIP <- merge(IIP, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
IIP  <- IIP  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(IIP))