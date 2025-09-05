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

# Function: for proration
#Description:

score_prorate <- function(sum, n_answered, n_total, min_prop = 0.7) {
  ok <- n_answered / n_total >= min_prop
  ifelse(ok, (sum / pmax(n_answered, 1)) * n_total, NA_real_)
}
