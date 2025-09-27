# -------------------------------
# 04_handle_missing.R
# Missing data handling
# -------------------------------

suppressPackageStartupMessages({
  library(dplyr)
  library(missForest)
  library(tibble)
  library(rlang)
  library(tidyr)
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
#   factors during imputation, then converts back to numeric 
#   - this is used for PSWQ (classification)
# - Leaves REGP/REGT (age, gender, chronicity, etc.) untouched.
# - Prints an audit count of excluded Admission rows.
# - Returns: data with Admission imputed/excluded; Assessment and
#   Post-treatment unchanged; contexts ordered.
# ---------------------------------------------------------
handle_missing <- function(
  data,
  max_missing_to_impute = 2,
  seed = 123
) {
  stopifnot(all(
    c("respondent_id", "assessment_context_label",
      "treatment_id") %in% names(data)
  ))

# Score columns
scl_cols <- grep("^scl_.*_t$", names(data), value = TRUE)
score_cols <- intersect(c(
  "atq_sum_prorated", "bai_sum_prorated",
  "bdi_ca_sum_prorated", "bdi_sa_sum_prorated",
  "iip_pa_t", "iip_bc_t", "iip_de_t", "iip_fg_t",
  "iip_hi_t", "iip_jk_t", "iip_lm_t", "iip_no_t",
  "mcq_cc_prorated", "mcq_pos_prorated",
  "mcq_csc_prorated", "mcq_neg_prorated",
  "mcq_nc_prorated", "pswq_core_prorated",
  "pswq_uncont_prorated", "pswq_engage_prorated"
), names(data))
score_cols <- union(score_cols, scl_cols)

# <-- sanitize types (no-op if score_cols is empty)
data <- data %>%
  mutate(across(
    any_of(score_cols),
    ~{
      x <- .
      if (is.factor(x))   x <- as.character(x)
      if (is.character(x)) x <- suppressWarnings(as.numeric(x))
      x[!is.finite(x)] <- NA_real_
      as.double(x)
    }
  ))

# keep your existing early return
if (length(score_cols) == 0) {
  return(
    data %>%
      arrange(
        respondent_id, treatment_id,
        factor(
          assessment_context_label,
          levels = c("Admission", "Post-treatment", "Assessment")
        )
      )
  )
}
  # ---- borrow Assessment values ----
assess_scores <- data %>%
  filter(assessment_context_label == "Assessment") %>%
  select(respondent_id, treatment_id, all_of(score_cols)) %>%
  group_by(respondent_id, treatment_id) %>%
  summarise(
    across(all_of(score_cols),
           ~ first(.x[!is.na(.x)], default = NA_real_)),
    .groups = "drop"
  ) %>%
  rename_with(~ paste0(.x, "_assess"), all_of(score_cols))

data2 <- data %>%
  left_join(assess_scores, by = c("respondent_id", "treatment_id")) %>%
  { 
    df <- .
    # Fill Admission NAs in each score col from *_assess
    for (nm in score_cols) {
      assess_nm <- paste0(nm, "_assess")
      df[[nm]] <- ifelse(
        df$assessment_context_label == "Admission" & is.na(df[[nm]]),
        df[[assess_nm]],
        df[[nm]]
      )
    }
    df %>% select(-ends_with("_assess"))
  }

  # ---- Admission rows: missingness decisions ----
  adm <- data2 %>%
  filter(assessment_context_label == "Admission") %>%
  mutate(
    n_missing_scores = rowSums(
      is.na(across(all_of(score_cols)))
    )
  )

  to_exclude <- adm %>% filter(n_missing_scores > max_missing_to_impute)
  to_keep    <- adm %>% filter(n_missing_scores == 0)
  to_impute  <- adm %>%
    filter(n_missing_scores > 0 & 
             n_missing_scores <= max_missing_to_impute)

  qc_oob <- NA
  qc_counts <- setNames(rep(0L, length(score_cols)), score_cols)
  qc_pct <- setNames(rep(0, length(score_cols)), score_cols)
  imputed_map <- NULL

  if (nrow(to_impute) > 0) {
    set.seed(seed)

    ord_cols <- names(select(to_impute, all_of(score_cols)))
    ord_cols <- ord_cols[vapply(
      to_impute[ord_cols],
      function(x) is.numeric(x) &&
        dplyr::n_distinct(x, na.rm = TRUE) <= 5,
      logical(1)
    )]

    mf_in <- to_impute %>%
      select(all_of(score_cols)) %>%
      mutate(across(
        all_of(ord_cols),
        ~ factor(.x, levels = sort(unique(.x[!is.na(.x)])))
      )) %>%
      as.data.frame()

    all_na_cols <- names(mf_in)[
      vapply(mf_in, function(x) all(is.na(x)), logical(1))
    ]
    if (length(all_na_cols)) {
      to_exclude <- bind_rows(to_exclude, to_impute)
      to_impute <- to_impute[0, , drop = FALSE]
    }

    if (nrow(to_impute) > 0) {
      miss_mask <- is.na(mf_in)
      mf_in[] <- lapply(mf_in, function(x) {
        x[is.infinite(x)] <- NA
        x
      })

      mf_out <- missForest(mf_in, maxiter = 5, verbose = FALSE)
      imputed_scores <- as.data.frame(mf_out$ximp)

      if (length(ord_cols)) {
        imputed_scores <- imputed_scores %>%
          mutate(across(
            all_of(ord_cols),
            ~ as.numeric(as.character(.))
          ))
      }
      imputed_scores <- setNames(imputed_scores, score_cols)

      adm_imputed <- to_impute %>%
        select(-all_of(score_cols)) %>%
        bind_cols(imputed_scores)

      qc_oob <- mf_out$OOBerror
      qc_counts <- colSums(miss_mask)
      qc_pct <- round(100 * qc_counts / nrow(miss_mask), 1)

      row_keys <- to_impute %>%
        select(respondent_id, treatment_id)
      if (any(miss_mask)) {
        idx <- which(miss_mask, arr.ind = TRUE)
        imputed_map <- tibble(
          respondent_id = row_keys$respondent_id[idx[, 1]],
          treatment_id  = row_keys$treatment_id[idx[, 1]],
          variable      = colnames(miss_mask)[idx[, 2]]
        )
      } else {
        imputed_map <- row_keys %>%
          slice(0) %>%
          bind_cols(tibble(variable = character()))
      }
    } else {
      adm_imputed <- to_impute
      imputed_map <- tibble(
        respondent_id = integer(),
        treatment_id  = integer(),
        variable      = character()
      )
    }
  } else {
    adm_imputed <- to_impute
    imputed_map <- tibble(
      respondent_id = integer(),
      treatment_id  = integer(),
      variable      = character()
    )
  }

  adm_clean <- bind_rows(to_keep, adm_imputed)

  other <- data2 %>%
    filter(assessment_context_label == "Post-treatment")

  out <- bind_rows(other, adm_clean) %>%
    arrange(
      respondent_id, treatment_id,
      factor(
        assessment_context_label,
        levels = c("Admission", "Post-treatment")
      )
    )

  keep_ids <- out %>%
    distinct(respondent_id, assessment_context_label) %>%
    pivot_wider(
      names_from = assessment_context_label,
      values_from = assessment_context_label,
      values_fn = length, values_fill = 0
    ) %>%
    filter(Admission > 0, `Post-treatment` > 0) %>%
    pull(respondent_id)

  out <- out %>% filter(respondent_id %in% keep_ids)

  excluded_keys <- to_exclude %>%
    select(respondent_id, treatment_id) %>%
    distinct()

  message(
    "# excluded Admission rows (> ",
    max_missing_to_impute,
    " missing or non-imputable): ",
    nrow(excluded_keys)
  )
  if (nrow(excluded_keys) > 0) {
    print(excluded_keys, n = 20)
  }

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
