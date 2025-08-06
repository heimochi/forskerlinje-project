---
title: "Utilities"
author: "mochibear"
format: html
editor: visual
---

## Shared functions

## Function for patient count at each measurement 

```{r}
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

# Example usage
# counts <- summarize_patient_counts(your_dataset)
# print(counts)

```



```{r}

```

The `echo: false` option disables the printing of code (only output is displayed).
