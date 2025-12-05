library(dplyr)
library(caret)
library(readr)
library(stringr)

full_dat <- read_csv(file.path(data_dir, "cleaned_all.csv")) #8743 obs. of 52 var

set.seed(123)

#seperate data 
admission <- full_dat %>%
  filter(assessment_context_label == "Admission")

postt <- full_dat %>%
  filter(assessment_context_label == "Post-treatment") %>%
  select(respondent_id, treatment_id, bai_sum_prorated) %>%
  rename(bai_post = bai_sum_prorated)

mod_dat <- admission %>%
  inner_join(posttx, by = c("respondent_id","treatment_id"))

# 2) remove irrelevant columns from predictors
cols_to_remove <- c("respondent_id",
                    "assessment_context_label", "treatment_id",
                    "respondent_id")

full_dat %>%
  filter(!str_detect(atq_sum_prorated, "^\\s*-?\\d+(\\.\\d+)?\\s*$")) %>%
  count(atq_sum_prorated, sort = TRUE)
