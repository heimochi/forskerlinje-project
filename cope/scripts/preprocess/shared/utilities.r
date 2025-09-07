# ---------------------------
# Utility Functions for Preprocessing
# Author: mochibear
# ---------------------------

# ---------------------------------------------------------
# Function: summarize_patient_counts
# Description:
#   Counts the number of unique respondents per assessment timepoint 
# (e.g., Assessment, Admission, Post-treatment).
# ---------------------------------------------------------

summarize_patient_counts <- function(dataset) {
  by_ctx <- dataset %>%
    dplyr::group_by(assessment_context_label) %>%
    dplyr::summarise(
      patient_count = dplyr::n_distinct(respondent_id),
      .groups = "drop"
    )

  total_unique <- dplyr::n_distinct(dataset$respondent_id)

  dplyr::bind_rows(
    by_ctx,
    tibble::tibble(
      assessment_context_label = "TOTAL (unique patients)",
      patient_count = total_unique
    )
  )
}

# ---------------------------------------------------------
# Function: peek_counts
# Description:
#   to use summarize_patient_counts within functions n pipes
# ---------------------------------------------------------

peek_counts <- function(df, label = NULL) {
  out <- summarize_patient_counts(df)

  if (isTRUE(getOption("knitr.in.progress"))) {
    # When knitting (Quarto/knitr): attach the label as a caption and show all rows
    print(knitr::kable(out, caption = paste("Step:", label %||% "")))
  } else {
    # In the console: print label + ALL rows/columns
    if (!is.null(label)) cat("\n--- ", label, " ---\n", sep = "")
    print(out, n = Inf, width = Inf)
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
