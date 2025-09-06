# ---------------------------------------------------------
# Filter Module
# Author: MochiBear.Hei
# Created: 2025-06-08
# Description: Standardized filtering for instruments (e.g., ATQ, BAI, etc.)
# ---------------------------------------------------------

library(dplyr)

# ---------------------------------------------------------
# Function: filter_instrument()
# Filters and filters input instrument data based on:
# - Consent
# - fixing NAs
# - Department filtering
# - Context label recoding (Assessment, Admission, Post-Treatment)
# - Treatment name standardization (Remove Boosters)
# - Duplicate removal 
# ---------------------------------------------------------

filter_clinical_data <- function(data, consent_df, id_col = "respondent_id") {
  data %>%
    peek_counts("start") %>%

    # keep only consent == 1 or NA (no row inflation)
    semi_join(consent_df %>% filter(is.na(consent) | consent == 1L),
              by = setNames("respondent_id", id_col)) %>%
    mutate(across(where(is.character), ~ na_if(.x, ""))) %>%
    peek_counts("after consent + na_if") %>%

    filter(treatment_type_id == 10) %>%
    peek_counts("after treatment_type_id == 10") %>%

    mutate(
      assessment_context_label = case_when(
        assessment_context_label %in% c("Utredning","P1 Kartlegging","P0 Basisutredning","SGKT Utredning") ~ "Assessment",
        assessment_context_label %in% c("Innkomst","P2 Innkomst","Inn sekv2","Behandlingsstart")           ~ "Admission",
        assessment_context_label %in% c("Utskriving","P2 Utskriving","Ut sekv 1","Behandlingsslutt","SGKT Gruppeslutt") ~ "Post-treatment",
        TRUE ~ assessment_context_label
      ),
      treatment_name = case_when(
        treatment_name %in% c("Angst1","Angst 1","Angst1 UO") ~ "Angst 1",
        treatment_name %in% c("Angst2","Angst 2")             ~ "Angst 2",
        treatment_name == "Angst1 Booster"                    ~ "Angst 1 Booster",
        treatment_name == "Angst2 Booster"                    ~ "Angst 2 Booster",
        treatment_name %in% c("Angst3","Angst 3")             ~ "Angst 3",
        treatment_name == "Angst4"                            ~ "Angst 4",
        treatment_name == "Angst 5"                           ~ "Angst 5",
        treatment_name %in% c("Angst12","Angst 12")           ~ "Angst 12",
        treatment_name == "Angst P1"                          ~ "Angst P1",
        treatment_name == "Angst P2"                          ~ "Angst P2",
        TRUE ~ treatment_name
      )
    ) %>%
    filter(
      assessment_context_label %in% c("Assessment","Admission","Post-treatment"),
      treatment_name %in% c("Angst","Angst 1","Angst 2","Angst 3","Angst 4","Angst 5","Angst 12","Angst P1","Angst P2")
    ) %>%
    peek_counts("after context + treatment filters") %>%

    distinct(across(all_of(c(id_col, "assessment_context_label"))), .keep_all = TRUE) %>%
    peek_counts("after dedup")
}