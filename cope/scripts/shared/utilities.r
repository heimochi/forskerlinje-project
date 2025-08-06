# ---------------------------
# Utility Functions for Preprocessing
# Author: mochibear
# ---------------------------

# Function: summarize_patient_counts
# Description:
#   Counts the number of unique respondents per assessment timepoint (e.g., Assessment, Admission, Post-treatment).
#   Useful for checking N per measurement stage.

summarize_patient_counts <- function(dataset) {
  patient_counts <- dataset %>%
    dplyr::group_by(assessment_context_label) %>%
    dplyr::summarize(
      patient_count = dplyr::n_distinct(respondent_id),
      .groups = "drop"
    )
  return(patient_counts)
}

# Example usage:
# counts <- summarize_patient_counts(my_data)
# print(counts)



