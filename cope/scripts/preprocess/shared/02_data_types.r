# ---------------------------------------------------------
# Data types Module
# Author: MochiBear.Hei
# Created: 2025-06-08
# Description: Standardized filtering for instruments (e.g., ATQ, BAI, etc.)
# ---------------------------------------------------------

library(dplyr)

# ---------------------------------------------------------
# Ensures:
# - respondent_id, treatment_id, treatment_type_id, 
#   regt_number_of_diagnoses → integer
# - assessment_context_label, treatment_name, treatment_type_name → character
# - regt_mood_disorder, regt_personality_disorder → integer 0/1 
#   (handles yes/no/true/false)
# - all *_prorated and *_t score variables → numeric
# ---------------------------------------------------------

data_types <- function(df) {
  stopifnot(is.data.frame(df))

  id_ints <- c("respondent_id", "treatment_id", "treatment_type_id",
               "regt_number_of_diagnoses")

  chr_cols <- c("assessment_context_label", "treatment_name", "treatment_type_name")

  regt_binary_ints <- c("regt_mood_disorder", "regt_personality_disorder")

  # Any *_prorated or *_t score columns across scales
  score_regex <- "(?:_prorated|_t)$"

  df %>%
    mutate(
      # IDs / counts
      across(any_of(id_ints), ~ suppressWarnings(as.integer(round(as.numeric(.))))),

      # Labels / names
      across(any_of(chr_cols), as.character),

      # REGT binaries -> 0/1 integers (accept yes/no/true/false/0/1)
      across(any_of(regt_binary_ints), ~ {
        v <- tolower(trimws(as.character(.)))
        case_when(
          v %in% c("1","true","t","yes","y","ja")  ~ 1L,
          v %in% c("0","false","f","no","n","nei") ~ 0L,
          TRUE ~ suppressWarnings(as.integer(as.numeric(.)))
        )
      }),

      # Questionnaire scores to numeric
      across(matches(score_regex), ~ suppressWarnings(as.numeric(.)))
    )
}

# quick checker for those columns (prints their classes)
check_types <- function(df) {
  cols <- c(
    "respondent_id","assessment_context_label","treatment_id","treatment_name",
    "treatment_type_id","treatment_type_name","regt_number_of_diagnoses",
    "regt_mood_disorder","regt_personality_disorder",
    "atq_sum_prorated","bai_sum_prorated",
    "bdi_ca_sum_prorated","bdi_sa_sum_prorated",
    "iip_pa_t","iip_bc_t","iip_de_t","iip_fg_t","iip_hi_t","iip_jk_t","iip_lm_t","iip_no_t",
    "mcq_cc_prorated","mcq_pos_prorated","mcq_csc_prorated","mcq_neg_prorated","mcq_nc_prorated",
    "pswq_core_prorated","pswq_uncont_prorated","pswq_engage_prorated",
    "calc_scl_psdi_t","calc_scl_pst_t","calc_scl_anxiety_t","calc_scl_depression_t",
    "calc_scl_hostility_t","calc_scl_phobic_t","calc_scl_interpersonal_t",
    "calc_scl_paranoid_t","calc_scl_psychoticism_t","calc_scl_somatization_t","calc_scl_ocd_t"
  )
  cols <- intersect(cols, names(df))
  vapply(df[cols], function(x) paste(class(x), collapse="/"), character(1))
}