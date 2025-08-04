# Calculating ATQ sum score

## Prepare dataset
```{r}
# Filter to retain only 'Admission' and 'Post-treatment' rows
atq_sumscore <- filled_data %>%
  filter(assessment_context_label %in% c("Admission", "Post-treatment"))


# Display the first few rows of the merged data
head(atq_sumscore)
print(summarize_patient_counts(atq_sumscore))

```