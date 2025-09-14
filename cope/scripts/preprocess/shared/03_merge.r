# ---------------------------------------------------------
# Merge Module
# Author: MochiBear.Hei
# Created: 2025-09-11
# Description: Merges instrument data frames into one wide table.
# ---------------------------------------------------------

library(dplyr)

# ---------------------------------------------------------
# - Keys: respondent_id + assessment_context_label
# - Deduplicates per key (keeps first row per instrument)
# - Renames treatment_id and treatment_name with instrument prefix
# - Keeps all numeric score columns as-is (already prefixed)
# - Full-joins across instruments; preserves all keys
# - Verbose: prints per-instrument rows kept and numeric cols kept
# - Returns a single wide dataset with questionnaire-specific
#   treatment_id/treatment_name and harmonized score variables
# ---------------------------------------------------------

merge_instruments <- function(dflist,
                              keys = c("respondent_id", "assessment_context_label", "treatment_id"),
                              verbose = TRUE) {
  stopifnot(is.list(dflist), length(dflist) > 0)

  msg <- function(...) if (verbose) cat(..., "\n")

  rename_meta <- function(df, qname) {
    df %>%
      rename_with(~ paste0(qname, "_", .x),
                  .cols = any_of(c("treatment_id", "treatment_name")))
  }

  # Deduplicate per instrument
  dedup <- function(df) df %>%
    group_by(across(all_of(keys))) %>%
    slice(1L) %>%
    ungroup()

  # Wrap: prefix + dedup + log
  cleaned <- lapply(names(dflist), function(q) {
    df <- dflist[[q]] %>% rename_meta(q) %>% dedup()
    msg(sprintf("[%s] rows: %d, numeric cols kept: %d",
                q, nrow(df), sum(sapply(df, is.numeric))))
    df
  })

  # Merge all
  Reduce(function(x, y) full_join(x, y, by = keys), cleaned)
}