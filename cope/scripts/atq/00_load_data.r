# ---------------------------------------------------------
# Load ATQ Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-04
# Description: Loads raw ATQ assessment data and consent records.
# ---------------------------------------------------------

# Required Libraries
library(readxl)     # for reading Excel files
library(readr)      # for reading CSV files
library(dplyr)      # for data manipulation

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# ATQ Assessment data
ATQ <- read_excel("data/dataset/ATQ_avid.xls")  # 3710 obs. of 57 variables

# Consent data
consent <- read_csv("data/dataset/scripts/anon/consent_a.csv")  # 5901 obs. of 30 variables

# ---------------------------------------------------------
# Rename ATQ column names for consistency
# ---------------------------------------------------------

ATQ <- ATQ %>% 
  rename(
    assessment_id = `assessment instance id`,
    assessment_version = `assessment version`,
    assessment_title = `assessment instance title`,
    assessment_owner_id = `assessment instance owner id`,
    assessment_start_date = `assessment instance start date`,
    assessment_end_date = `assessment instance end date`,
    assessment_created_date = `assessment instance created date`,
    assessment_last_modified = `assessment instance last modified/submitted`,
    assessment_started = `assessment instance has started`,
    assessment_submitted = `assessment instance is submitted`,
    assessment_closed = `assessment instance is closed`,
    assessment_context_label = `assessment instance context label`,
    assessment_first_started_date = `assessment instance first time started date`,
    assessment_first_submitted_date = `assessment instance first time submitted date`,
    assessment_portal = `assessment instance portal of submission`,
    treatment_id = `treatment id`,
    treatment_name = `treatment name`,
    treatment_type_id = `treatment type id`,
    treatment_type_name = `treatment type name`,
    treatment_role = `treatment role`,
    respondent_id = `respondent id`,
    respondent_username = `respondent username`,
    respondent_account_enabled = `respondent account enabled`,
    respondent_test_account = `respondent test account`,
    respondent_last_login = `respondent last login`,
    respondent_communcation_disabled = `respondent communication disabled`, 
    Q1 = `Q1`, Q2 = `Q2`, Q3 = `Q3`, Q4 = `Q4`, Q5 = `Q5`,
    Q6 = `Q6`, Q7 = `Q7`, Q8 = `Q8`, Q9 = `Q9`, Q10 = `Q10`,
    Q11 = `Q11`, Q12 = `Q12`, Q13 = `Q13`, Q14 = `Q14`, Q15 = `Q15`,
    Q16 = `Q16`, Q17 = `Q17`, Q18 = `Q18`, Q19 = `Q19`, Q20 = `Q20`,
    Q21 = `Q21`, Q22 = `Q22`, Q23 = `Q23`,
    calc_aa = `calculation:MODUMBAD-ATQ-AA`,
    calc_pan = `calculation:MODUMBAD-ATQ-PAN-AA`,
    calc_pan_sum = `calculation:MODUMBAD-ATQ-PAN-SUM`,
    calc_ptsd = `calculation:MODUMBAD-ATQ-PTSD-AA`,
    calc_ptsd_sum = `calculation:MODUMBAD-ATQ-PTSD-SUM`,
    calc_sos = `calculation:MODUMBAD-ATQ-SOS-AA`,
    calc_sos_sum = `calculation:MODUMBAD-ATQ-SOS-SUM`,
    calc_sum = `calculation:MODUMBAD-ATQ-SUM`
  )

# ---------------------------------------------------------
# Select only necessary columns
# ---------------------------------------------------------

ATQ <- ATQ %>%
  select(
    respondent_id,
    assessment_context_label,
    assessment_created_date,
    treatment_id,
    treatment_name,
    treatment_type_id,
    starts_with("Q"),
    starts_with("calc")
  )

# Filter for only participants that consented to having their data used
consent <- consent %>%
  select(respondent_id, consent)