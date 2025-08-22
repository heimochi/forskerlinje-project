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
    starts_with("calculation")
  ) %>%
  rename(
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`,
    respondent_id            = `respondent id`

    # Subscales with English labels
    calc_IIP_domineering_AA   = `calculation:IIP-64-Domi-AA`,
    calc_IIP_domineering_P    = `calculation:IIP-64-Domi-P`,
    calc_IIP_domineering_PIP  = `calculation:IIP-64-Domi-PIP`,
    calc_IIP_domineering_raw  = `calculation:IIP-64-Domi-Raw`,
    calc_IIP_domineering_T    = `calculation:IIP-64-Domi-T`,
    calc_IIP_domineering_TIP  = `calculation:IIP-64-Domi-TIP`,

    calc_IIP_vindictive_AA    = `calculation:IIP-64-Foye-AA`,
    calc_IIP_vindictive_P     = `calculation:IIP-64-Foye-P`,
    calc_IIP_vindictive_PIP   = `calculation:IIP-64-Foye-PIP`,
    calc_IIP_vindictive_raw   = `calculation:IIP-64-Foye-Raw`,
    calc_IIP_vindictive_T     = `calculation:IIP-64-Foye-T`,
    calc_IIP_vindictive_TIP   = `calculation:IIP-64-Foye-TIP`,

    calc_IIP_cold_AA          = `calculation:IIP-64-Hevn-AA`,
    calc_IIP_cold_P           = `calculation:IIP-64-Hevn-P`,
    calc_IIP_cold_PIP         = `calculation:IIP-64-Hevn-PIP`,
    calc_IIP_cold_raw         = `calculation:IIP-64-Hevn-Raw`,
    calc_IIP_cold_T           = `calculation:IIP-64-Hevn-T`,
    calc_IIP_cold_TIP         = `calculation:IIP-64-Hevn-TIP`,

    calc_IIP_inhibited_AA     = `calculation:IIP-64-Kald-AA`,
    calc_IIP_inhibited_P      = `calculation:IIP-64-Kald-P`,
    calc_IIP_inhibited_PIP    = `calculation:IIP-64-Kald-PIP`,
    calc_IIP_inhibited_raw    = `calculation:IIP-64-Kald-Raw`,
    calc_IIP_inhibited_T      = `calculation:IIP-64-Kald-T`,
    calc_IIP_inhibited_TIP    = `calculation:IIP-64-Kald-TIP`,

    calc_IIP_nonassertive_AA  = `calculation:IIP-64-Kreve-AA`,
    calc_IIP_nonassertive_P   = `calculation:IIP-64-Kreve-P`,
    calc_IIP_nonassertive_PIP = `calculation:IIP-64-Kreve-PIP`,
    calc_IIP_nonassertive_raw = `calculation:IIP-64-Kreve-Raw`,
    calc_IIP_nonassertive_T   = `calculation:IIP-64-Kreve-T`,
    calc_IIP_nonassertive_TIP = `calculation:IIP-64-Kreve-TIP`,

    calc_IIP_accommodating_AA = `calculation:IIP-64-Lise-AA`,
    calc_IIP_accommodating_P  = `calculation:IIP-64-Lise-P`,
    calc_IIP_accommodating_PIP= `calculation:IIP-64-Lise-PIP`,
    calc_IIP_accommodating_raw= `calculation:IIP-64-Lise-Raw`,
    calc_IIP_accommodating_T  = `calculation:IIP-64-Lise-T`,
    calc_IIP_accommodating_TIP= `calculation:IIP-64-Lise-TIP`,

    calc_IIP_selfsac_AA       = `calculation:IIP-64-Ofre-AA`,
    calc_IIP_selfsac_P        = `calculation:IIP-64-Ofre-P`,
    calc_IIP_selfsac_PIP      = `calculation:IIP-64-Ofre-PIP`,
    calc_IIP_selfsac_raw      = `calculation:IIP-64-Ofre-Raw`,
    calc_IIP_selfsac_T        = `calculation:IIP-64-Ofre-T`,
    calc_IIP_selfsac_TIP      = `calculation:IIP-64-Ofre-TIP`,

    calc_IIP_intrusive_AA     = `calculation:IIP-64-Usikk-AA`,
    calc_IIP_intrusive_P      = `calculation:IIP-64-Usikk-P`,
    calc_IIP_intrusive_PIP    = `calculation:IIP-64-Usikk-PIP`,
    calc_IIP_intrusive_raw    = `calculation:IIP-64-Usikk-Raw`,
    calc_IIP_intrusive_T      = `calculation:IIP-64-Usikk-T`,
    calc_IIP_intrusive_TIP    = `calculation:IIP-64-Usikk-TIP`,

    calc_IIP_total_AA         = `calculation:IIP-64-Tot-AA`,
    calc_IIP_total_P          = `calculation:IIP-64-Tot-P`,
    calc_IIP_total_raw        = `calculation:IIP-64-Tot-Raw`,
    calc_IIP_total_T          = `calculation:IIP-64-Tot-T`
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