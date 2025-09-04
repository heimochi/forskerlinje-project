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

# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var
# ---------------------------------------------------------
# Rename n select REGP column names for consistency
# ---------------------------------------------------------

# Select only from the dataset
REGP <- REGP %>%
  select(
    respondent.id,
    assessment.instance.context.label,
    treatment.id,          
    treatment.name,
    treatment.type.id,
    treatment.type.name,   
    respondent.gender,     
    Q1:Q16
  ) %>%
  rename(
    respondent_id             = respondent.id,
    assessment_context_label  = assessment.instance.context.label,
    treatment_id              = treatment.id,
    treatment_name            = treatment.name,
    treatment_type_id         = treatment.type.id,
    treatment_type_name       = treatment.type.name,
    regp_gender               = respondent.gender,
    
    regp_birth_year           = Q1,
    regp_partner              = Q2,
    regp_children_n           = Q3,
    regp_work_status          = Q4,
    regp_work_paid            = Q5,
    regp_work_type            = Q6,
    regp_work_paid_months     = Q7,
    regp_sickleave_months     = Q8,
    regp_sickleave_reason     = Q9,
    regp_prev_outpatient      = Q10,
    regp_prev_outpatient_len  = Q11,
    regp_prev_inpatient       = Q12,
    regp_prev_inpatient_len   = Q13,
    regp_prev_mbad            = Q14,
    regp_prev_mbad_len        = Q15,
    regp_onset_year           = Q16
  )

# ---------------------------------------------------------
# Filter for only participants that consented to having their data used
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)

  # Merge only the 'consent' column with the REGP dataset by 'respondent_id'
REGP <- merge(REGP, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
REGP  <- REGP  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Select only necessary
REGP  <- REGP  %>%
  select(
   respondent_id, 
   assessment_context_label, 
   treatment_id,             
   treatment_name,           
   treatment_type_id,        
   treatment_type_name,      
   regp_gender,              
   regp_birth_year,          
   regp_partner,             
   regp_prev_outpatient,     
   regp_prev_inpatient
  )

# Check N
print(summarize_patient_counts(REGP))