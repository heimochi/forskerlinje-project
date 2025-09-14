# -------------------------------
# 04_handle_missing.R
# Missing data handling
# Expects: `data` to be a dataframe with item columns starting with "Q"
# -------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(missForest)
  library(tibble)
})
# -------------------------------
# Description: for assessment, admission and post-treatment
# -------------------------------

# -----------------------------------------------------------
# Missing-data handling + coverage helpers (≤80-char lines)
# -----------------------------------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(missForest)
})

# -----------------------------------------------------------
# handle_missing()
#  - Fill Admission NAs from Assessment (same id + treatment)
#  - On Admission only:
#      - if 0–1 questionnaire sums/T missing → missForest impute
#      - if ≥2 missing → exclude that Admission row
#  - REGP/REGT vars (age, gender, chronicity) are untouched
# -----------------------------------------------------------
handle_missing <- function(
  data,
  max_missing_to_impute = 1,  # "fewer than two" = 0 or 1
  seed = 123
) {
  stopifnot(all(
    c("respondent_id", "assessment_context_label", "treatment_id")
    %in% names(data)
  ))

  # 1) Questionnaire score columns considered (REGP/REGT excluded)
  score_cols <- intersect(c(
    "atq_sum_prorated",
    "bai_sum_prorated",
    "bdi_ca_sum_prorated", "bdi_sa_sum_prorated",
    "iip_pa_t", "iip_bc_t", "iip_de_t", "iip_fg_t",
    "iip_hi_t", "iip_jk_t", "iip_lm_t", "iip_no_t",
    "mcq_cc_prorated", "mcq_pos_prorated", "mcq_csc_prorated",
    "mcq_neg_prorated", "mcq_nc_prorated",
    "pswq_core_prorated", "pswq_uncont_prorated", "pswq_engage_prorated",
    "calc_scl_psdi_t", "calc_scl_pst_t", "calc_scl_anxiety_t",
    "calc_scl_depression_t", "calc_scl_hostility_t", "calc_scl_phobic_t",
    "calc_scl_interpersonal_t", "calc_scl_paranoid_t",
    "calc_scl_psychoticism_t", "calc_scl_somatization_t",
    "calc_scl_ocd_t"
  ), names(data))

  if (length(score_cols) == 0) return(data)

  # 2) Fill Admission NAs from Assessment values
  assess_scores <- data %>%
    filter(assessment_context_label == "Assessment") %>%
    select(respondent_id, treatment_id, all_of(score_cols)) %>%
    rename_with(~ paste0(.x, "_assess"), all_of(score_cols))

  data2 <- data %>%
    left_join(assess_scores, by = c("respondent_id", "treatment_id")) %>%
    mutate(across(
      all_of(score_cols),
      ~ if_else(
        assessment_context_label == "Admission" & is.na(.x),
        get(paste0(cur_column(), "_assess")),
        .x
      )
    )) %>%
    select(-ends_with("_assess"))

  # 3) Admission rows → exclude or impute
  adm <- data2 %>%
    filter(assessment_context_label == "Admission") %>%
    mutate(n_missing_scores = rowSums(
      is.na(across(all_of(score_cols)))
    ))

  to_exclude <- adm %>%
    filter(n_missing_scores > max_missing_to_impute)

  to_keep <- adm %>%
    filter(n_missing_scores == 0)

  to_impute <- adm %>%
    filter(n_missing_scores > 0 &
             n_missing_scores <= max_missing_to_impute)

  if (nrow(to_impute) > 0) {
    set.seed(seed)
    mf_in <- to_impute %>%
      select(all_of(score_cols)) %>%
      as.data.frame()

    mf_out <- missForest(
      mf_in, maxiter = 5, verbose = FALSE
    )

    imputed_scores <- as_tibble(mf_out$ximp) %>%
      setNames(score_cols)

    adm_imputed <- to_impute %>%
      select(-all_of(score_cols)) %>%
      bind_cols(imputed_scores)
  } else {
    adm_imputed <- to_impute
  }

  adm_clean <- bind_rows(to_keep, adm_imputed)

  # 4) Recombine with non-Admission rows (Assessment/Post unchanged)
  other <- data2 %>%
    filter(assessment_context_label != "Admission")

  out <- bind_rows(other, adm_clean) %>%
    arrange(
      respondent_id,
      treatment_id,
      factor(
        assessment_context_label,
        levels = c("Assessment", "Admission", "Post-treatment")
      )
    )

  # Audit
  message(
    "# excluded Admission rows (≥ ",
    max_missing_to_impute + 1,
    " missing): ",
    nrow(to_exclude)
  )
  if (nrow(to_exclude) > 0) {
    print(to_exclude %>% select(respondent_id, treatment_id), n = 20)
  }

  out
}

# -----------------------------------------------------------
# instrument_of(): classify variables by instrument
# -----------------------------------------------------------
instrument_of <- function(vars) {
  regp_unpref <- c(
    "age_at_admission", "symptom_duration",
    "regp_work_any_bin", "regp_prev_treat_bin",
    "regp_partner_bin", "regp_work_pastyear_bin",
    "regp_work_current_bin", "regp_prev_outpt_bin",
    "regp_prev_inpt_bin", "symptom_duration_bin"
  )

  out <- ifelse(
    grepl("^calc_scl_", vars), "SCL",
    ifelse(
      grepl("^(bai|atq|bdi|iip|mcq|pswq)_", vars, ignore.case = TRUE),
      toupper(sub("^([a-z]+).*", "\\1", vars)),
      ifelse(
        grepl("^regp_", vars, ignore.case = TRUE) |
          vars %in% regp_unpref,
        "REGP",
        ifelse(
          grepl("^regt_", vars, ignore.case = TRUE),
          "REGT",
          "OTHER"
        )
      )
    )
  )
  out
}

# -----------------------------------------------------------
# coverage_by_instrument(): mean/min % non-missing per instrument
# -----------------------------------------------------------
coverage_by_instrument <- function(df) {
  score_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  score_cols <- setdiff(score_cols, c("respondent_id", "treatment_id"))

  inst <- instrument_of(score_cols)

  tibble(
    instrument = toupper(inst),
    variable = score_cols,
    non_na = colSums(!is.na(df[score_cols])),
    pct = round(100 * non_na / nrow(df), 1)
  ) %>%
    group_by(instrument) %>%
    summarise(
      vars = n(),
      mean_pct = round(mean(pct), 1),
      min_pct = min(pct),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_pct))
}

# -----------------------------------------------------------
# flag_low_coverage(): list variables with % non-missing < thresh
# -----------------------------------------------------------
flag_low_coverage <- function(df, thresh = 85) {
  score_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  score_cols <- setdiff(score_cols, c("respondent_id", "treatment_id"))

  tibble(
    instrument = toupper(instrument_of(score_cols)),
    variable = score_cols,
    pct = round(100 * colMeans(!is.na(df[score_cols])), 1)
  ) %>%
    filter(pct < thresh) %>%
    arrange(pct)
}
