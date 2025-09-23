# -------------------------------
# 04_handle_missing.R
# Missing data handling
# -------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(missForest)
  library(tibble)
})

# ---------------------------------------------------------
# handle_missing()
# - Fills Admission NAs from the same person's Assessment
#   (match on respondent_id + treatment_id), then DROPS the
#   Assessment rows from the output.
# - On Admission rows only:
#     - If 0–2 questionnaire sums/T are missing → impute with missForest.
#     - If >2 questionnaire sums/T are missing → exclude that Admission row.
# - If a respondent_id does NOT have BOTH an Admission row AND a
#   Post-treatment row in the final data, remove that respondent_id
#   entirely from the analysis.
# - Treats low-cardinality numeric vars (≤5 unique values) as
#   factors during imputation, then converts back to numeric.
# - Leaves REGP/REGT (age, gender, chronicity, etc.) untouched.
# - Prints an audit count of excluded Admission rows.
# - Returns: data with Admission imputed/excluded; Assessment and
#   Post-treatment unchanged; contexts ordered.
# ---------------------------------------------------------
handle_missing <- function(
  data,
  max_missing_to_impute = 2,   # new rule: 0–2 → impute, >2 → exclude
  seed = 123
) {
  stopifnot(all(
    c("respondent_id","assessment_context_label","treatment_id") %in% names(data)
  ))

  # Columns we consider for missingness/imputation (use only those present)
  score_cols <- intersect(c(
    "atq_sum_prorated","bai_sum_prorated",
    "bdi_ca_sum_prorated","bdi_sa_sum_prorated",
    "iip_pa_t","iip_bc_t","iip_de_t","iip_fg_t","iip_hi_t","iip_jk_t",
    "iip_lm_t","iip_no_t",
    "mcq_cc_prorated","mcq_pos_prorated","mcq_csc_prorated",
    "mcq_neg_prorated","mcq_nc_prorated",
    "pswq_core_prorated","pswq_uncont_prorated","pswq_engage_prorated",
    "calc_scl_psdi_t","calc_scl_pst_t","calc_scl_anxiety_t",
    "calc_scl_depression_t","calc_scl_hostility_t","calc_scl_phobic_t",
    "calc_scl_interpersonal_t","calc_scl_paranoid_t",
    "calc_scl_psychoticism_t","calc_scl_somatization_t","calc_scl_ocd_t"
  ), names(data))

  # If no score columns are present, just return (but still sort contexts)
  if (length(score_cols) == 0) {
    return(
      data %>%
        arrange(
          respondent_id, treatment_id,
          factor(assessment_context_label, levels = c("Admission","Post-treatment","Assessment"))
        )
    )
  }

  # ---- Pull Assessment scores to use as fill for Admission NAs ----
  assess_scores <- data %>%
    filter(assessment_context_label == "Assessment") %>%
    select(respondent_id, treatment_id, all_of(score_cols)) %>%
    # In case of duplicate Assessment rows per (id, tx), pick the first non-missing per column
    group_by(respondent_id, treatment_id) %>%
    summarise(across(all_of(score_cols), ~ .x[which(!is.na(.x))[1]] %||% NA_real_), .groups = "drop") %>%
    rename_with(~ paste0(.x, "_assess"), all_of(score_cols))

  # ---- Join assessment values; fill Admission NAs from Assessment; drop *_assess helpers ----
  data2 <- data %>%
    left_join(assess_scores, by = c("respondent_id","treatment_id")) %>%
    mutate(across(
      all_of(score_cols),
      ~ if_else(
        assessment_context_label == "Admission",
        coalesce(.x, .data[[paste0(cur_column(), "_assess")]]),
        .x
      )
    )) %>%
    select(-ends_with("_assess"))

  # ---- Focus Admission rows for missingness counts / decisions ----
  adm <- data2 %>%
    filter(assessment_context_label == "Admission") %>%
    mutate(n_missing_scores = rowSums(is.na(across(all_of(score_cols)))))

  to_exclude <- adm %>% filter(n_missing_scores > max_missing_to_impute)
  to_keep    <- adm %>% filter(n_missing_scores == 0)
  to_impute  <- adm %>% filter(n_missing_scores > 0 & n_missing_scores <= max_missing_to_impute)

  # QC containers (filled below)
  qc_oob <- NA
  qc_counts <- setNames(rep(0L, length(score_cols)), score_cols)
  qc_pct <- setNames(rep(0, length(score_cols)), score_cols)
  imputed_map <- NULL

  # ---- Impute where allowed (0–2 missing) ----
  if (nrow(to_impute) > 0) {
    set.seed(seed)

    # Identify low-cardinality numeric columns among the score set
    ord_cols <- names(select(to_impute, all_of(score_cols)))
    ord_cols <- ord_cols[vapply(
      to_impute[ord_cols],
      function(x) is.numeric(x) && dplyr::n_distinct(x, na.rm = TRUE) <= 5,
      logical(1)
    )]

    mf_in <- to_impute %>%
      select(all_of(score_cols)) %>%
      mutate(across(all_of(ord_cols), ~ factor(.x, levels = sort(unique(.x[!is.na(.x)]))))) %>%
      as.data.frame()

    # Guard: if any score column is all-NA across the to_impute rows,
    # missForest cannot learn to impute it → exclude those rows.
    all_na_cols <- names(mf_in)[vapply(mf_in, function(x) all(is.na(x)), logical(1))]
    if (length(all_na_cols)) {
      # Move the affected rows (which is all rows in to_impute) to exclusions
      to_exclude <- bind_rows(to_exclude, to_impute)
      to_impute  <- to_impute[0, , drop = FALSE]
    }

    if (nrow(to_impute) > 0) {
      mf_in <- to_impute %>%
        select(all_of(score_cols)) %>%
        mutate(across(all_of(ord_cols), ~ factor(.x, levels = sort(unique(.x[!is.na(.x)]))))) %>%
        as.data.frame()

      # Track which cells were NA
      miss_mask <- is.na(mf_in)

      # Safety: convert Inf/-Inf to NA (missForest will choke on Infs)
      mf_in[] <- lapply(mf_in, function(x) { x[is.infinite(x)] <- NA; x })

      mf_out <- missForest(mf_in, maxiter = 5, verbose = FALSE)

      imputed_scores <- as.data.frame(mf_out$ximp)

      # Convert ordinal factors back to numeric
      if (length(ord_cols)) {
        imputed_scores <- imputed_scores %>%
          mutate(across(all_of(ord_cols), ~ as.numeric(as.character(.))),
                 .keep = "all")
      }

      # Name columns back to original score names
      imputed_scores <- setNames(imputed_scores, score_cols)

      adm_imputed <- to_impute %>%
        select(-all_of(score_cols)) %>%
        bind_cols(imputed_scores)

      # QC
      qc_oob    <- mf_out$OOBerror
      qc_counts <- colSums(miss_mask)
      qc_pct    <- round(100 * qc_counts / nrow(miss_mask), 1)

      # Map of which cells were imputed (keys + variable)
      row_keys <- to_impute %>% select(respondent_id, treatment_id)
      if (any(miss_mask)) {
        idx <- which(miss_mask, arr.ind = TRUE)
        imputed_map <- tibble::tibble(
          respondent_id = row_keys$respondent_id[idx[, 1]],
          treatment_id  = row_keys$treatment_id[idx[, 1]],
          variable      = colnames(miss_mask)[idx[, 2]]
        )
      } else {
        imputed_map <- to_impute %>% select(respondent_id, treatment_id) %>% slice(0) %>%
          bind_cols(tibble(variable = character()))
      }
    } else {
      # Nothing to impute after the all-NA guard
      adm_imputed <- to_impute
      # qc_* already initialized
      if (is.null(imputed_map)) {
        imputed_map <- adm_imputed %>% select(respondent_id, treatment_id) %>% slice(0) %>%
          bind_cols(tibble(variable = character()))
      }
    }
  } else {
    adm_imputed <- to_impute
    imputed_map <- adm_imputed %>% select(respondent_id, treatment_id) %>% slice(0) %>%
      bind_cols(tibble(variable = character()))
  }

  # ---- Reassemble Admission (kept + imputed); drop Assessment rows entirely ----
  adm_clean <- bind_rows(to_keep, adm_imputed)

  other <- data2 %>%
    filter(assessment_context_label == "Post-treatment")  # keep only Post-treatment

  out <- bind_rows(other, adm_clean) %>%
    arrange(
      respondent_id, treatment_id,
      factor(assessment_context_label, levels = c("Admission","Post-treatment"))
    )

  # ---- Remove respondents that don't have BOTH Admission AND Post-treatment ----
  keep_ids <- out %>%
    distinct(respondent_id, assessment_context_label) %>%
    tidyr::pivot_wider(names_from = assessment_context_label, values_from = assessment_context_label,
                       values_fn = length, values_fill = 0) %>%
    filter(Admission > 0, `Post-treatment` > 0) %>%
    pull(respondent_id)

  out <- out %>% filter(respondent_id %in% keep_ids)

  # Admission rows excluded due to > max_missing or all-NA guard
  excluded_keys <- to_exclude %>% select(respondent_id, treatment_id) %>% distinct()

  message(
    "# excluded Admission rows (>", max_missing_to_impute, " missing OR non-imputable): ",
    nrow(excluded_keys)
  )
  if (nrow(excluded_keys) > 0) {
    print(excluded_keys, n = 20)
  }

  # Attach QC attribute
  attr(out, "qc_impute") <- list(
    oob_error = qc_oob,
    excluded_n = nrow(excluded_keys),
    excluded_keys = excluded_keys,
    imputed_counts = qc_counts,
    imputed_pct = qc_pct,
    imputed_map = imputed_map
  )

  out
}