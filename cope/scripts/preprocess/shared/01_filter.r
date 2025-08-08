# ---------------------------------------------------------
# Filter Module
# Author: MochiBear.Hei
# Created: 2025-06-08
# Description: Standardized filtering for instruments (e.g., ATQ, BAI, etc.)
# ---------------------------------------------------------

# Required Libraries
library(dplyr)

# ---------------------------------------------------------
# Function: filter_instrument()
# Filters and cleans input instrument data based on:
# - Consent
# - Context label recoding
# - Department filtering
# - Treatment name standardization
# - Duplicate removal
# ---------------------------------------------------------

filter_instrument <- function(df, consent, instrument_label = "DATA") {
  
  # 1. Merge with consent
  df <- merge(df, consent, by = "respondent_id", all = TRUE, suffixes = c("", "_c"))
  
  # 2. Replace empty strings with NA (in character columns)
  df <- df %>%
    mutate(across(where(is.character), ~na_if(., '')))
  
  # 3. Recode assessment_context_label
  df <- df %>%
    mutate(assessment_context_label = case_when(
      assessment_context_label %in% c("Utredning", "P1 Kartlegging") ~ "Assessment",
      assessment_context_label %in% c("Innkomst", "Behandlingsstart", "P2 Innkomst") ~ "Admission",
      assessment_context_label %in% c("Utskriving", "P2 Utskriving", "Behandlingsslutt") ~ "Post-treatment",
      TRUE ~ assessment_context_label
    )) %>%
    filter(assessment_context_label %in% c("Assessment", "Admission", "Post-treatment"))
  
  # 4. Filter by treatment_type_id and consent
  df <- df %>%
    filter(treatment_type_id == 10, consent %in% c(1, NA))
  
  # 5. Standardize treatment names
  df <- df %>%
    mutate(treatment_name = case_when(
      treatment_name %in% c("Angst1", "Angst 1", "Angst1 UO") ~ "Angst 1",
      treatment_name %in% c("Angst2", "Angst 2") ~ "Angst 2",
      treatment_name %in% c("Angst3", "Angst 3") ~ "Angst 3",
      treatment_name == "Angst4" ~ "Angst 4",
      treatment_name == "Angst 5" ~ "Angst 5",
      treatment_name %in% c("Angst12", "Angst 12") ~ "Angst 12",
      treatment_name == "Angst P2" ~ "Angst P2",
      treatment_name == "Angst1 Booster" ~ "Angst 1 Booster",
      treatment_name == "Angst2 Booster" ~ "Angst 2 Booster",
      TRUE ~ treatment_name
    )) %>%
    filter(treatment_name %in% c("Angst", "Angst 1", "Angst 2", "Angst 3",
                                 "Angst 4", "Angst 5", "Angst 12", "Angst P1", "Angst P2"))
  
  # 6. Remove duplicates by respondent_id and assessment_context_label
  df <- df %>%
    distinct(respondent_id, assessment_context_label, .keep_all = TRUE)

  # Optional: Print summary if utility function is available
  if ("summarize_patient_counts" %in% ls()) {
    cat("\nSummary for", instrument_label, "\n")
    print(summarize_patient_counts(df))
  }
  
  return(df)
}
