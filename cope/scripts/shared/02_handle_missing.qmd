# -------------------------------
# 03_handle_missing.R
# Missing data handling
# Expects: `data` to be a dataframe with item columns starting with "Q"
# -------------------------------

library(dplyr)

```{r, warning=FALSE, message=FALSE}
# 1. Remove rows where all item scores (Q*) are NA
data <- data %>%
  rowwise() %>%
  filter(sum(is.na(c_across(starts_with("Q")))) != length(c_across(starts_with("Q")))) %>%
  ungroup()

# 2. Remove observations with >30% missing item data
data <- data %>%
  rowwise() %>%
  mutate(missing_prop = sum(is.na(c_across(starts_with("Q")))) / length(c_across(starts_with("Q")))) %>%
  ungroup() %>%
  filter(missing_prop <= 0.30) %>%
  select(-missing_prop)

# 3. Keep only respondents with at least one Post-treatment measurement
data <- data %>%
  group_by(respondent_id) %>%
  filter(any(assessment_context_label == "Post-treatment")) %>%
  ungroup()

# 4. Remove rows where all Post-treatment items are missing
data <- data %>%
  rowwise() %>%
  filter(!(assessment_context_label == "Post-treatment" &
           sum(is.na(c_across(starts_with("Q")))) == length(c_across(starts_with("Q"))))) %>%
  ungroup()

# 5. Create missing "Admission" rows based on "Assessment"
context_labels <- c("Assessment", "Admission", "Post-treatment")
all_combinations <- expand.grid(
  respondent_id = unique(data$respondent_id),
  assessment_context_label = context_labels
)

full_data <- left_join(all_combinations, data, 
                       by = c("respondent_id", "assessment_context_label")) %>%
  arrange(respondent_id)

# Keep only the item columns
impute_full_data <- full_data %>%
  select(respondent_id, assessment_context_label, starts_with("Q"))

# 6. Impute missing "Admission" item scores using "Assessment" scores
filled_data <- impute_full_data %>%
  group_by(respondent_id) %>%
  mutate(across(starts_with("Q"), ~ ifelse(assessment_context_label == "Admission" & is.na(.),
                                           first(.[assessment_context_label == "Assessment"], default = NA),
                                           .))) %>%
  ungroup()

# 7. Merge imputed Admission scores back in
data <- full_data %>%
  left_join(
    filled_data %>% filter(assessment_context_label == "Admission") %>% 
      select(respondent_id, assessment_context_label, starts_with("Q")),
    by = c("respondent_id", "assessment_context_label"),
    suffix = c("", ".imputed")
  ) %>%
  mutate(across(starts_with("Q"), ~ coalesce(.x, get(paste0(cur_column(), ".imputed"))))) %>%
  select(-ends_with(".imputed"))

# 8. Keep only relevant timepoints
data <- data %>%
  filter(assessment_context_label %in% c("Admission", "Post-treatment")) %>%
  select(respondent_id, assessment_context_label, starts_with("Q"))
  ```