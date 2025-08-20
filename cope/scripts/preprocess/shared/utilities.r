# ---------------------------
# Utility Functions for Preprocessing
# Author: mochibear
# ---------------------------
library(dplyr)

# Function: summarize_patient_counts
# Description:
#   Counts the number of unique respondents per assessment timepoint (e.g., Assessment, Admission, Post-treatment).
#   Useful for checking N per measurement stage.
summarize_patient_counts <- function(dataset) {
  
  # Summarize the count of patients for each assessment context label
  patient_counts <- dataset %>%
    group_by(assessment_context_label) %>%
    summarize(
      patient_count = n_distinct(respondent_id),
      .groups = 'drop' # To ungroup after summarization
    )
  
  return(patient_counts)
}

# Example usage:
# counts <- summarize_patient_counts(my_data)
# print(counts)
