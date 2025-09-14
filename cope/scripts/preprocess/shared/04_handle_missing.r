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
# - Applies prereg missing-data rules before coverage checks.
# - Fills Admission NAs from same person's Assessment (match on
#   respondent_id + treatment_id).
# - On Admission rows only:
#     - If 0–1 questionnaire sums/T are missing → impute with
#       missForest.
#     - If ≥2 questionnaire sums/T are missing → exclude that
#       Admission row.
# - Treats low-cardinality numeric vars (≤5 unique values) as
#   factors during imputation, then converts back to numeric.
# - Leaves REGP/REGT (age, gender, chronicity, etc.) untouched.
# - Prints an audit count of excluded Admission rows.
# - Returns: data with Admission imputed/excluded; Assessment and
#   Post-treatment unchanged; contexts ordered.
# ---------------------------------------------------------
handle_missing <- function(
  data,
  max_missing_to_impute = 1,  # "fewer than two" = 0 or 1
  seed = 123
) {
  stopifnot(all(
    c("respondent_id","assessment_context_label","treatment_id")
    %in% names(data)
  ))

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

  if (length(score_cols) == 0) return(data)

  assess_scores <- data %>%
    filter(assessment_context_label == "Assessment") %>%
    select(respondent_id, treatment_id, all_of(score_cols)) %>%
    rename_with(~ paste0(.x, "_assess"), all_of(score_cols))

  data2 <- data %>%
    left_join(assess_scores, by = c("respondent_id","treatment_id")) %>%
    mutate(across(
      all_of(score_cols),
      ~ if_else(
        assessment_context_label == "Admission" & is.na(.x),
        get(paste0(cur_column(), "_assess")),
        .x
      )
    )) %>%
    select(-ends_with("_assess"))

  adm <- data2 %>%
    filter(assessment_context_label == "Admission") %>%
    mutate(n_missing_scores = rowSums(is.na(across(all_of(score_cols)))))

  to_exclude <- adm %>% filter(n_missing_scores > max_missing_to_impute)
  to_keep    <- adm %>% filter(n_missing_scores == 0)
  to_impute  <- adm %>%
    filter(n_missing_scores > 0 & n_missing_scores <= max_missing_to_impute)

  if (nrow(to_impute) > 0) {
    set.seed(seed)

    ord_cols <- names(select(to_impute, all_of(score_cols)))
    ord_cols <- ord_cols[vapply(
      to_impute[ord_cols],
      function(x) is.numeric(x) &&
        length(unique(x[!is.na(x)])) <= 5,
      logical(1)
    )]

    mf_in <- to_impute %>%
      select(all_of(score_cols)) %>%
      mutate(across(
        all_of(ord_cols),
        ~ factor(.x, levels = sort(unique(.x[!is.na(.x)])))
      )) %>%
      as.data.frame()

    miss_mask <- is.na(mf_in)

    mf_out <- missForest(mf_in, maxiter = 5, verbose = FALSE)

    imputed_scores <- as.data.frame(mf_out$ximp)

    if (length(ord_cols)) {
      imputed_scores <- imputed_scores %>%
        mutate(across(
          all_of(ord_cols),
          ~ as.numeric(as.character(.x))
        ))
    }

    imputed_scores <- setNames(imputed_scores, score_cols)

    adm_imputed <- to_impute %>%
      select(-all_of(score_cols)) %>%
      bind_cols(imputed_scores)

    qc_oob    <- mf_out$OOBerror
    qc_counts <- colSums(miss_mask)
    qc_pct    <- round(100 * qc_counts / nrow(miss_mask), 1)

    row_keys <- to_impute %>% select(respondent_id, treatment_id)
    if (any(miss_mask)) {
      idx <- which(miss_mask, arr.ind = TRUE)
      imputed_map <- tibble::tibble(
        respondent_id = row_keys$respondent_id[idx[, 1]],
        treatment_id  = row_keys$treatment_id[idx[, 1]],
        variable      = colnames(miss_mask)[idx[, 2]]
      )
    } else {
      imputed_map <- tibble::tibble(
        respondent_id = integer(),
        treatment_id  = integer(),
        variable      = character()
      )
    }
  } else {
    adm_imputed <- to_impute
    qc_oob <- NA
    qc_counts <- setNames(rep(0, length(score_cols)), score_cols)
    qc_pct <- setNames(rep(0, length(score_cols)), score_cols)
    imputed_map <- tibble::tibble(
      respondent_id = integer(),
      treatment_id  = integer(),
      variable      = character()
    )
  }

  adm_clean <- bind_rows(to_keep, adm_imputed)

  other <- data2 %>%
    filter(assessment_context_label != "Admission")

  out <- bind_rows(other, adm_clean) %>%
    arrange(
      respondent_id,
      treatment_id,
      factor(
        assessment_context_label,
        levels = c("Assessment","Admission","Post-treatment")
      )
    )

  message(
    "# excluded Admission rows (≥ ",
    max_missing_to_impute + 1,
    " missing): ",
    nrow(to_exclude)
  )
  if (nrow(to_exclude) > 0) {
    print(to_exclude %>% select(respondent_id, treatment_id), n = 20)
  }

  attr(out, "qc_impute") <- list(
    oob_error = qc_oob,
    excluded_n = nrow(to_exclude),
    excluded_keys = to_exclude %>%
      select(respondent_id, treatment_id),
    imputed_counts = qc_counts,
    imputed_pct = qc_pct,
    imputed_map = imputed_map
  )

  out
}