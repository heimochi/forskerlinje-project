# ---------------------------------------------------------
# Load BDI Data and Consent
# Author: MochiBear.Hei
# Created: 2025-08-22
# Description: Loads raw BDI assessment data and consent records.
# ---------------------------------------------------------

# Libraries
library(readxl)     # for reading Excel files
library(readr)      # for reading CSV files
library(dplyr)      # for data manipulation

# ---------------------------------------------------------
# Load raw data
# ---------------------------------------------------------

# BAI Assessment data
BDI <- read_excel("/Users/maggieheimvik/Desktop/COPE/data/dataset/BDI_avid.xls") #12035 obs. 53 var
# Consent data
consent <- read_csv("/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") #5901 obs. of 30 var


# ---------------------------------------------------------
# Rename n select BDI variables
# ---------------------------------------------------------

#reminder that helper is in utilities!

# define item sets (cognitive affective vs somatic affective)
CA_items <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q12","Q13","Q14")  # 12 items
SA_items <- c("Q10","Q11","Q15","Q16","Q17","Q18","Q19","Q20","Q21")           # 9 items

BDI <- BDI %>%
  select(
    `respondent id`, `assessment instance context label`,
    `treatment id`, `treatment name`, `treatment type id`,
    all_of(CA_items), all_of(SA_items)
  ) %>%
  # standardize names
  rename(
    respondent_id            = `respondent id`,
    assessment_context_label = `assessment instance context label`,
    treatment_id             = `treatment id`,
    treatment_name           = `treatment name`,
    treatment_type_id        = `treatment type id`
  ) %>%
  # make sure items are numeric (0–3)
  mutate(across(all_of(c(CA_items, SA_items)), ~ suppressWarnings(as.numeric(.)))) %>%
  # counts & sums
  mutate(
    bdi_ca_items = rowSums(across(all_of(CA_items),    ~ !is.na(.x))),
    bdi_sa_items = rowSums(across(all_of(SA_items),    ~ !is.na(.x))),
    bdi_ca_sum   = rowSums(across(all_of(CA_items)),   na.rm = TRUE),
    bdi_sa_sum   = rowSums(across(all_of(SA_items)),   na.rm = TRUE),
    #proration to full length (12 for CA, 9 for SA)
    bdi_ca_sum_prorated = score_prorate(bdi_ca_sum, bdi_ca_items, 12, min_prop = 0.70),
    bdi_sa_sum_prorated = score_prorate(bdi_sa_sum, bdi_sa_items,  9, min_prop = 0.70)
  ) %>%
  select(
    respondent_id, assessment_context_label,
    treatment_id, treatment_name, treatment_type_id,
    bdi_ca_sum, bdi_sa_sum, bdi_ca_sum_prorated, bdi_sa_sum_prorated
  )

# Check N
print(summarize_patient_counts(BDI))