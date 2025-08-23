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
# Filters and filters input instrument data based on:
# - Department filtering
# - Context label recoding (Assessment, Admission, Post-Treatment)
# - Treatment name standardization (Remove Boosters)
# - Duplicate removal 
# ---------------------------------------------------------

filter_clinical_data <- function(data, id_col = "respondent_id") {

  # Standardize assessment_context_label
  data <- data %>% 

      # Filter based on treatment_type_id and consent
    filter(treatment_type_id == 10, consent %in% c(1, NA)) %>%

    mutate(
      assessment_context_label = case_when(
        assessment_context_label %in% c("Utredning", "P1 Kartlegging", "P0 Basisutredning",
          "SGKT Utredning") ~ "Assessment",
        assessment_context_label %in% c("Innkomst", "P2 Innkomst", "Inn sekv2", "Behandlingsstart") ~ "Admission",
        assessment_context_label %in% c("Utskriving", "P2 Utskriving", "Ut sekv 1",
          "Behandlingsslutt", "SGKT Gruppeslutt") ~ "Post-treatment",
        TRUE ~ assessment_context_label
      )
    ) %>%

    # Keep only the three relevant context groups
    filter(assessment_context_label %in% c("Assessment", "Admission", "Post-treatment")) %>%

    # Standardize treatment_name
    mutate(
      treatment_name = case_when(
        treatment_name %in% c("Angst1", "Angst 1", "Angst1 UO") ~ "Angst 1", #UO= Utredning og Oppstart, Assessment and Start
        treatment_name %in% c("Angst2", "Angst 2") ~ "Angst 2",
        treatment_name == "Angst1 Booster" ~ "Angst 1 Booster",
        treatment_name == "Angst2 Booster" ~ "Angst 2 Booster",
        treatment_name %in% c("Angst3", "Angst 3") ~ "Angst 3",
        treatment_name == "Angst4" ~ "Angst 4",
        treatment_name =="Angst 5" ~ "Angst 5",
        treatment_name %in% c("Angst12", "Angst 12") ~ "Angst 12",
        treatment_name == "Angst P1" ~ "Angst P1", #Pilot?
        treatment_name == "Angst P2" ~ "Angst P2", #Pilot
        TRUE ~ treatment_name
      )
    ) %>%

    # Keep only valid treatments
    filter(treatment_name %in% c("Angst", "Angst 1", "Angst 2", "Angst 3", "Angst 4", "Angst 5", "Angst 12", "Angst P1", "Angst P2")) %>%

    # Remove duplicates
    distinct(across(all_of(c(id_col, "assessment_context_label"))), .keep_all = TRUE) %>%


  return(data)
}


