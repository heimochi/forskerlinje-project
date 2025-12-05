# -------------------------------
# 04_handle_missing.R
# Missing data handling
# -------------------------------

library(dplyr)
library(missForest)
library(stringi)

# -------------------------------
# seperate pre and post treatment scores, then also seperate 
# demographics so they will not be touched in imputation
# Split by context

aa <- merged_all %>% 
  filter(assessment_context_label %in% c("Admission", "Assessment"))

pt <- merged_all %>% 
  filter(assessment_context_label == "Post-treatment")

# Separate demographics
demo_cols <- grep("^(regp_|regt_)", names(aa), value = TRUE)
id_cols   <- c("respondent_id", "treatment_id", "assessment_context_label")

aa_demo <- aa %>% select(all_of(c(id_cols, demo_cols)))
aa_scores <- aa %>% select(-any_of(demo_cols))

# ---------------------------------------------------------
# QC: Check for missing and invalid numeric values in aa_scores
# ---------------------------------------------------------

# Identify numeric-like columns (actual numeric or possibly dirty)
num_cols <- names(aa_scores)[vapply(aa_scores, function(x)
  is.numeric(x) || is.character(x) || is.factor(x), logical(1))]

num_cols <- setdiff(num_cols, c("respondent_id", "treatment_id"))
N <- nrow(aa_scores)

qc_summary <- lapply(num_cols, function(col) {
  x <- aa_scores[[col]]

  # Try to coerce to numeric safely
  suppressWarnings(num_x <- as.numeric(as.character(x)))

  # Invalids = became NA after coercion but weren’t originally NA
  n_invalid <- sum(is.na(num_x) & !is.na(x))
  n_na      <- sum(is.na(x))

  tibble(
    variable   = col,
    n_total    = N,
    n_na       = n_na,
    pct_na     = round(100 * n_na / N, 1),
    n_invalid  = n_invalid,
    pct_invalid= round(100 * n_invalid / N, 1)
  )
}) %>%
  bind_rows() %>%
  arrange(desc(pct_na), desc(n_invalid))

print(qc_summary, n = Inf)
#scl_psdi_t has highest missingness with 244/1346=18.12%


# -------------------------------
# replace invalid numbers with NA
# -------------------------------

# -------------------------------
###Imputation###
# -------------------------------

# IDs and scores (aa_scores already has demographics removed)
score_cols <- setdiff(names(aa_scores), id_cols)

# Check missingness
# admission
adm <- aa_scores %>%
  filter(assessment_context_label == "Admission") %>%
  mutate(n_miss = rowSums(is.na(across(all_of(score_cols)))))

to_exclude_adm <- filter(adm, n_miss > 2)  # 101
to_keep_adm    <- filter(adm, n_miss == 0) # 359
to_impute_adm  <- filter(adm, n_miss > 0 & n_miss <= 2) # 50

# assessment
ass <- aa_scores %>%
  filter(assessment_context_label == "Assessment") %>%
  mutate(n_miss = rowSums(is.na(across(all_of(score_cols)))))

to_exclude_ass <- filter(ass, n_miss > 2)  #131
to_keep_ass    <- filter(ass, n_miss == 0) # 649
to_impute_ass  <- filter(ass, n_miss > 0 & n_miss <= 2) #56

# Impute (if needed) with missForest; treat low-cardinality numerics as factors
if (nrow(to_impute) > 0) {
  ord_cols <- names(to_impute[score_cols])[sapply(
    to_impute[score_cols],
    function(x) is.numeric(x) && dplyr::n_distinct(x, na.rm = TRUE) <= 5
  )]

  mf_in <- to_impute %>%
    select(all_of(score_cols)) %>%
    mutate(across(all_of(ord_cols),
                  ~ factor(.x, levels = sort(unique(.x[!is.na(.x)]))))) %>%
    as.data.frame()

  set.seed(123)
  mf_out <- missForest(mf_in, maxiter = 5, verbose = FALSE)
  imp <- as.data.frame(mf_out$ximp)

  if (length(ord_cols))
    imp <- imp %>%
      mutate(across(all_of(ord_cols), ~ as.numeric(as.character(.))))

  adm_imputed <- to_impute %>%
    select(all_of(id_cols)) %>%
    bind_cols(setNames(imp, score_cols))
} else {
  adm_imputed <- to_impute # empty
}

# Rebuild: Assessment untouched + kept Admission + imputed Admission
aa_scores_imp <- bind_rows(
  aa_scores %>% filter(assessment_context_label == "Assessment"),
  to_keep %>% select(all_of(c(id_cols, score_cols))),
  adm_imputed
) %>%
  arrange(respondent_id, treatment_id,
          factor(assessment_context_label,
                 levels = c("Admission","Assessment")))

# Keys excluded (Admission rows with >2 missing scores)
excluded_admission <- to_exclude %>%
  select(all_of(id_cols)) %>%
  distinct()