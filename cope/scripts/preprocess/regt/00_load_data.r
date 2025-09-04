# ---------------------------------------------------------
# Load Therapist Registration Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw REGT assessment data and consent records.
# ---------------------------------------------------------

# Libraries
library(readxl)     # for reading Excel files
library(readr)      # for reading CSV files
library(dplyr)      # for data manipulation
library(stringr)
library(lubridate)  #for dates manipulation (admission n discharge)
library(tidyr)
# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# REGP Assessment data
REGT <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/reg_t_a.csv") # 242 obs. of 30 var

# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var

# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

# Merge only the 'consent' column with the REGT dataset by 'respondent_id'
REGT <- REGT %>%                                      # 382 obs. of 84 variables 
  left_join(consent, by = "respondent_id") %>%        # 368 obs. of 84 variables 
  filter(is.na(consent) | consent == 1)

# Replace empty strings with NA only in character columns
REGT  <- REGT  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# ---------------------------------------------------------
# Rename n select REGP column names for consistency
# ---------------------------------------------------------
REGT <- REGT %>%
  select(
    respondent_id,
    assessment_context_label,
    treatment_id,          
    treatment_name,
    treatment_type_id,
    treatment_type_name,   

    # admission/discharge & program
    assessment_start_date,  # admission
    assessment_end_date,  # discharge
    Q3, # alcohol or dugs
    Q6, # childhood trauma sexual
    Q7, # childhood trauma physical
    Q8, # childhood trauma other
    Q9, # adult trauma abuse
    Q10, # adult trauma other

    # diagnostics
    Q30:Q46,

    # GAF scores
    calc_MB_GAF_F_in_sum,
    calc_MB_GAF_F_out_sum,
    calc_MB_GAF_S_in_sum,
    calc_MB_GAF_S_out_sum
  ) %>%
  rename(
    # program
    regt_admission_date   = assessment_start_date,
    regt_discharge_date   = assessment_end_date,

    # Substance use
    regt_alcohol_drug_abuse       = Q3,
    
    # Childhood trauma
    regt_trauma_child_sexual      = Q6,
    regt_trauma_child_physical    = Q7,
    regt_trauma_child_other       = Q8,
    
    # Adult trauma
    regt_trauma_adult_abuse       = Q9,
    regt_trauma_adult_other       = Q10,

    # ICD-10
    regt_icd_primary      = Q30,
    regt_icd_bi_1         = Q31,
    regt_icd_bi_2         = Q32,
    regt_icd_bi_3         = Q33,

    # SCID-II
    regt_scid_done        = Q35,
    regt_scid_avoidant    = Q36,
    regt_scid_dependent   = Q37,
    regt_scid_ocd         = Q38,
    regt_scid_paranoid    = Q39,
    regt_scid_schizotypal = Q40,
    regt_scid_schizoid    = Q41,
    regt_scid_histrionic  = Q42,
    regt_scid_narcissistic= Q43,
    regt_scid_borderline  = Q44,
    regt_scid_antisocial  = Q45,
    regt_scid_other       = Q46,

    # GAF
    calc_regt_gaf_func_in  = calc_MB_GAF_F_in_sum,
    calc_regt_gaf_func_out = calc_MB_GAF_F_out_sum,
    calc_regt_gaf_symp_in  = calc_MB_GAF_S_in_sum,
    calc_regt_gaf_symp_out = calc_MB_GAF_S_out_sum
  ) %>%
  mutate(
    # parse i.e., "23.10.2019 13:14"
    regt_admission_dt  = dmy_hm(regt_admission_date, tz = "UTC"),
    regt_discharge_dt  = dmy_hm(regt_discharge_date, tz = "UTC"),
    
    # keep a Date version if you prefer date-only
    regt_admission_date  = as_date(regt_admission_dt),
    regt_discharge_date  = as_date(regt_discharge_dt),
    
    # binary tags 
    regt_alcohol_drug_abuse_bin   = ifelse(regt_alcohol_drug_abuse == 1, 1, 0),
    
    regt_trauma_child_sexual_bin  = ifelse(regt_trauma_child_sexual != "Nei", 1, 0),
    regt_trauma_child_physical_bin= ifelse(regt_trauma_child_physical != "Nei", 1, 0),
    regt_trauma_child_other_bin   = ifelse(regt_trauma_child_other != "Nei", 1, 0),
    
    regt_trauma_adult_abuse_bin   = ifelse(regt_trauma_adult_abuse != "Nei", 1, 0),
    regt_trauma_adult_other_bin   = ifelse(regt_trauma_adult_other != "Nei", 1, 0),

    # SCID flags to logicals
    across(starts_with("regt_scid_"), ~ .x == 1),

    # GAF change scores
    calc_regt_gaf_func_change = calc_regt_gaf_func_out - calc_regt_gaf_func_in,
    calc_regt_gaf_symp_change = calc_regt_gaf_symp_out - calc_regt_gaf_symp_in
  )
  

# ---------------------------------------------------------
# Define variables for downstream analysis of complexity
# ---------------------------------------------------------

  ## Define variables
#1. Number of Diagnoses: The total count of diagnosed disorders (counts).
#2. Comorbidity: Presence of mood disorder (binary; yes/no).
#3. Additional Complexity: Presence of a personality disorder (binary; yes/no).
#4. length of stay

# ---------------------------------------------------------
# Helpers
# ---------------------------------------------------------
normalize_icd <- function(icd) {
  icd %>%
    str_to_upper() %>%                 # uppercase
    str_remove_all("[^A-Z0-9.]") %>%   # keep letters/digits/dot
    str_trim()
}

# ---------------------------------------------------------
# Define variables for downstream analysis
# ---------------------------------------------------------
REGT <- REGT %>%
  # standardize ICD code fields
  mutate(across(c(regt_icd_primary, regt_icd_bi_1, regt_icd_bi_2, regt_icd_bi_3), normalize_icd)) %>%
  
  # number of diagnoses (count non-missing/non-empty ICDs)
  mutate(
    regt_number_of_diagnoses = {
      icds <- select(., regt_icd_primary, regt_icd_bi_1, regt_icd_bi_2, regt_icd_bi_3)
      rowSums(icds != "" & !is.na(icds))
    }
  ) %>%
  
  # mood disorder comorbidity (F30/31/32/33/34/38/39 anywhere)
  mutate(
    regt_mood_disorder = if_else(
      str_detect(
        paste(regt_icd_primary, regt_icd_bi_1, regt_icd_bi_2, regt_icd_bi_3, sep = " "),
        "\\bF3(0|1|2|3|4|8|9)\\b"
      ),
      1L, 0L   # integers, not characters
    )
  ) %>%
  
  # additional complexity: SCID interview done + any PD flagged
  mutate(
    regt_additional_complexity = case_when(
      is.na(regt_scid_done) ~ NA_integer_,  # NA stays as integer
      regt_scid_done & if_any(
        starts_with("regt_scid_") & !matches("^regt_scid_done$"),
        ~ .x %in% c(TRUE, 1)
      ) ~ 1L,
      regt_scid_done ~ 0L,
      TRUE ~ 0L
    )
  ) %>%
  
  # length of stay (assumes regt_admission_date / regt_discharge_date already parsed)
  mutate(
    regt_length_of_stay_days = as.integer(regt_discharge_date - regt_admission_date),
    regt_length_of_stay_days = if_else(regt_length_of_stay_days < 0, NA_integer_, regt_length_of_stay_days)
  ) %>%
  
  # combined trauma flag (any childhood/adult trauma bin = 1)
  mutate(
    regt_trauma_any_bin = if_else(
      rowSums(across(starts_with("regt_trauma_"), ~ replace_na(as.integer(.x), 0)), na.rm = TRUE) > 0,
      1L, 0L
    )
  ) %>%
  
  # keep only variables to use later
  select(
    respondent_id, assessment_context_label, treatment_id, treatment_name,
    regt_length_of_stay_days, regt_number_of_diagnoses, regt_mood_disorder, 
    regt_additional_complexity, regt_trauma_any_bin, regt_alcohol_drug_abuse_bin,
    calc_regt_gaf_symp_change, calc_regt_gaf_func_change
  )

# ---------------------------------------------------------
# Check
# ---------------------------------------------------------

# Check N
print(summarize_patient_counts(REGT))