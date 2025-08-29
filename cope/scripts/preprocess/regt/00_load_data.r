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

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# REGP Assessment data
REGT <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/reg_t_a.csv") #5901 obs. of 30 var


# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var
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
    Q1_QT3_O16,  # admission
    Q1_QT3_O17,  # discharge
    Q2_QT4_O18,  # group
    Q2_QT4_O19,  # dropout reason
    Q2_QT4_O20,  # dropout time
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
    regt_admission_date   = Q1_QT3_O16,
    regt_discharge_date   = Q1_QT3_O17,
    regt_group            = Q2_QT4_O18,
    regt_dropout_reason   = Q2_QT4_O19,
    regt_dropout_time     = Q2_QT4_O20,

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
    # parse dates (format: dd.mm.yy)
    regt_admission_date = dmy(regt_admission_date),
    regt_discharge_date = dmy(regt_discharge_date),

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
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

  # Merge only the 'consent' column with the REGT dataset by 'respondent_id'
REGT <- merge(REGT, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
REGT  <- REGT  %>%
  mutate(across(where(is.character), ~na_if(., '')))



  ## Define variables
#1. Number of Diagnoses: The total count of diagnosed disorders (counts).
#2. Comorbidity: Presence of mood disorder (binary; yes/no).
#3. Additional Complexity: Presence of a personality disorder (binary; yes/no).
#4. length of stay

# Standardize ICD codes
normalize_icd <- function(icd) {
  icd %>%
    str_to_upper() %>% # Convert to uppercase
    str_remove_all("[^A-Z0-9.]") %>% # Remove non-alphanumeric/non-dot characters
    str_trim() # Trim whitespace
}

# Clean and transform data
REGT <- REGT %>%
  # Normalize ICD codes
  mutate(across(c(icd_primary, icd_bi_1, icd_bi_2, icd_bi_3), normalize_icd)) %>%
  
  # Create "Number of Diagnoses" as the count of non-missing ICD codes
  mutate(number_of_diagnoses = case_when(
    rowSums(is.na(select(., icd_primary, icd_bi_1, icd_bi_2, icd_bi_3))) == length(select(., icd_primary, icd_bi_1, icd_bi_2, icd_bi_3)) ~ NA_integer_,
    TRUE ~ rowSums(!is.na(select(., icd_primary, icd_bi_1, icd_bi_2, icd_bi_3)) & select(., icd_primary, icd_bi_1, icd_bi_2, icd_bi_3) != "")
  )) %>%
  
  # Create comorbidity for mood disorders
  mutate(mood_disorder = if_else(grepl("F30|F31|F32|F33|F34|F38|F39", paste(icd_primary, icd_bi_1, icd_bi_2, icd_bi_3, sep = " "), ignore.case = TRUE), "yes", "no")) %>%
  
  # Create additional_complexity considering SCID interview and potential NA values
  mutate(additional_complexity = case_when(
    is.na(scid) ~ NA_character_,
    scid == 1 & rowSums(select(., starts_with("scid_")) == 1, na.rm = TRUE) > 0 ~ "yes",
    scid == 1 & rowSums(select(., starts_with("scid_")) == 1, na.rm = TRUE) == 0 ~ "no",
    TRUE ~ "no" # Default case if SCID is not 1
  ))

  #length of stay
# keep only the newly defined variables and leave out the others
# View first few rows of the cleaned data
head(REGT)
```

# Check N
print(summarize_patient_counts(REGT))