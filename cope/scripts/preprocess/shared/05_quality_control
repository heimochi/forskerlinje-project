# -------------------------------
# 05_quality_control.R
# quality control of the cleaned dataset, checking imputation
# -------------------------------
# instrument_of()
# - Classifies variable names into instruments.
# - "calc_scl_*" → "SCL".
# - Prefixes ^bai|atq|bdi|iip|mcq|pswq → uppercased instrument.
# - REGP: names starting "regp_" or in the regp_unpref list.
# - REGT: names starting "regt_".
# - Anything else → "OTHER".
# - Used by coverage_by_instrument() and flag_low_coverage().
# - Returns: character vector of instrument labels.
# ---------------------------------------------------------

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

# ---------------------------------------------------------
# coverage_by_instrument()
# - Computes non-missing coverage for numeric variables.
# - Excludes IDs: respondent_id, treatment_id.
# - Uses instrument_of() to group variables by instrument.
# - For each instrument, reports:
#     - vars: number of variables,
#     - mean_pct: mean % non-missing,
#     - min_pct: minimum % non-missing.
# - Sorted by mean_pct (descending) for quick scanning.
# - Returns: tibble with instrument, vars, mean_pct, min_pct.
# ---------------------------------------------------------

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

# ---------------------------------------------------------
# flag_low_coverage()
# - Lists numeric variables with % non-missing below a threshold.
# - Excludes IDs: respondent_id, treatment_id.
# - Uses instrument_of() to label each variable.
# - Argument 'thresh' is the % non-missing cutoff (default 85).
# - Sorted by pct ascending to surface the worst offenders.
# - Returns: tibble with instrument, variable, pct.
# ---------------------------------------------------------

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

# ---------------------------------------------------------
# print_impute_qc()
# - Prints compact QC for imputation pulled from attr 'qc_impute'.
# - Shows excluded rows, missForest OOB error, and top vars imputed.
# - Returns a tibble of per-variable imputed counts and pct.
# ---------------------------------------------------------
print_impute_qc <- function(x, top_n = 15) {
  qc <- attr(x, "qc_impute")
  if (is.null(qc)) {
    cat("No 'qc_impute' attribute found.\n")
    return(invisible(NULL))
  }

  cat("\n# ---- Imputation QC ----\n")
  cat("Excluded Admission rows: ", qc$excluded_n, "\n", sep = "")
  cat("missForest OOB error: ")
  print(qc$oob_error)

  vars <- names(qc$imputed_counts)
  tib <- tibble(
    variable = vars,
    n_imputed = as.integer(qc$imputed_counts[vars]),
    pct_imputed = as.numeric(qc$imputed_pct[vars])
  ) %>%
    arrange(desc(n_imputed))

  cat("\nTop variables by # imputed:\n")
  print(head(tib, top_n), n = top_n)

  if (nrow(qc$imputed_map) > 0) {
    cat("\nSample imputed cells (first 10):\n")
    print(head(qc$imputed_map, 10), n = 10)
  }

  invisible(tib)
}

# ---------------------------------------------------------
# qc_dist_check()
# - Compares observed vs imputed distributions on Admission rows.
# - For each variable in 'vars', returns n, mean, sd for both sets.
# - Use to spot shifts that look implausible after imputation.
# ---------------------------------------------------------
qc_dist_check <- function(x, vars) {
  qc <- attr(x, "qc_impute")
  if (is.null(qc) || is.null(qc$imputed_map)) {
    stop("No imputation map available; run handle_missing() first.")
  }

  adm <- x %>% filter(assessment_context_label == "Admission")
  imap <- qc$imputed_map %>% filter(variable %in% vars)

  if (nrow(imap) == 0) {
    message("No imputed cells for the selected variables.")
    return(invisible(NULL))
  }

  res <- lapply(unique(imap$variable), function(v) {
    imp_keys <- imap %>% filter(variable == v) %>%
      select(respondent_id, treatment_id)
    imp_vals <- adm %>% inner_join(imp_keys,
                          by = c("respondent_id","treatment_id")) %>%
      pull(all_of(v))
    obs_vals <- adm %>% filter(!is.na(.data[[v]])) %>%
      anti_join(imp_keys, by = c("respondent_id","treatment_id")) %>%
      pull(all_of(v))
    tibble(
      variable = v,
      n_obs = sum(!is.na(obs_vals)),
      n_imp = sum(!is.na(imp_vals)),
      mean_obs = mean(obs_vals, na.rm = TRUE),
      mean_imp = mean(imp_vals, na.rm = TRUE),
      sd_obs = sd(obs_vals, na.rm = TRUE),
      sd_imp = sd(imp_vals, na.rm = TRUE)
    )
  })

  out <- bind_rows(res) %>% arrange(desc(n_imp))
  print(out, n = nrow(out))
  invisible(out)
}

# ---------------------------------------------------------
# qc_excluded_keys()
# - Returns the list of excluded Admission rows captured in QC.
# ---------------------------------------------------------
qc_excluded_keys <- function(x) {
  qc <- attr(x, "qc_impute")
  if (is.null(qc)) {
    stop("No 'qc_impute' attribute found.")
  }
  qc$excluded_keys
}

