# ---------------------------
# Utility Functions for Preprocessing
# Author: mochibear
# ---------------------------
library(dplyr)

# ---------------------------------------------------------
# Function: summarize_patient_counts
# Description:
#   Counts the number of unique respondents per assessment timepoint 
# (e.g., Assessment, Admission, Post-treatment).
# ---------------------------------------------------------

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

# ---------------------------------------------------------
# Function: peek_counts
# Description:
#   to use summarize_patient_counts within functions n pipes
# ---------------------------------------------------------
peek_counts <- function(df, label = NULL) {
  out <- summarize_patient_counts(df)
  if (isTRUE(getOption("knitr.in.progress"))) {
    # In Quarto/knitr: keep label attached to the table
    print(knitr::kable(out, caption = paste("Step:", label)))
  } else {
    # In the R console: this is fine
    if (!is.null(label)) cat("\n---", label, "---\n")
    print(out)
  }
  df  # pass-through
}

# ---------------------------------------------------------
# Function: for proration
#Description:
# ---------------------------------------------------------

score_prorate <- function(sum, n_answered, n_total, min_prop = 0.7) {
  ok <- n_answered / n_total >= min_prop
  ifelse(ok, (sum / pmax(n_answered, 1)) * n_total, NA_real_)
}
