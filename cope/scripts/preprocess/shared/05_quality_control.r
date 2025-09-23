# -------------------------------
# 05_quality_control.R
# quality control of the cleaned dataset, checking imputation
# -------------------------------
library(ggplot2)  
library(dplyr)
library(rlang)
library(tibble)
# ---------------------------------------------------------
# - Builds tidy QC table from qc$imputed_counts and qc$imputed_pct
# - One row per variable with n_imputed and % imputed
# - Sorted by descending number of imputations
# - Used to inspect scope of imputation across scales
# ---------------------------------------------------------
qc_summary <- tibble::tibble(
  variable = names(qc$imputed_counts),
  n_imputed = qc$imputed_counts,
  pct_imputed = qc$imputed_pct
) %>%
  arrange(desc(n_imputed))

# ---------------------------------------------------------
# qc_plot_var()
# - Input: cleaned dataset, qc object, variable name (string)
# - Flags rows as Imputed vs Observed using qc$imputed_map
# - Plots density distribution + rug marks of observed vs imputed
# - Used to QC plausibility of imputations per variable
# ---------------------------------------------------------
qc_plot_var <- function(data, qc, varname) {
  stopifnot(varname %in% names(data))

  imputed_ids <- qc$imputed_map %>%
    filter(variable == varname) %>%
    select(respondent_id, treatment_id)

  with_flag <- data %>%
    select(respondent_id, treatment_id, !!sym(varname)) %>%
    mutate(
      imputed = ifelse(
        paste(respondent_id, treatment_id) %in%
          paste(imputed_ids$respondent_id,
                imputed_ids$treatment_id),
        "Imputed", "Observed"
      )
    )

  ggplot(with_flag, aes(x = !!sym(varname), fill = imputed)) +
    geom_density(alpha = 0.4, adjust = 1.2) +
    geom_rug(
      data = filter(with_flag, imputed == "Imputed"),
      aes(color = imputed), sides = "b", inherit.aes = FALSE
    ) +
    labs(title = paste("QC:", varname), x = varname, y = "Density") +
    theme_minimal()
}
