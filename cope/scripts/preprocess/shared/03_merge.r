# ---------------------------------------------------------
# Merge Module
# Author: MochiBear.Hei
# Created: 2025-09-11
# Description: Merges instrument data frames into one wide table.
# ---------------------------------------------------------

library(dplyr)

# ---------------------------------------------------------
# - Keys: respondent_id + assessment_context_label
# - Deduplicates per key (keeps first row per key)
# - Keeps only numeric score columns; drops treatment_id/type_id
# - Full-joins across instruments; preserves all keys
# - Verbose: prints per-instrument rows kept and numeric cols kept
# ---------------------------------------------------------

merge_wide <- function(dflist,
                       keys = c("respondent_id",
                                "assessment_context_label"),
                      # meta = c("treatment_id",
                      #          "treatment_name"),
                      # prefer_meta = NULL,
                       verbose = TRUE) {
  stopifnot(is.list(dflist), length(dflist) > 0)

  msg <- function(...) if (verbose) cat(..., "\n")

  # ensure one row per key for each instrument
  dedup <- function(df) {
    df %>%
      group_by(across(all_of(keys))) %>%
      slice(1L) %>%
      ungroup()
  }

  # scores: keys + numeric columns (drop meta to avoid duplicates)
  scores <- lapply(dflist, function(df) {
    df %>%
      dedup() %>%
      select(all_of(keys), where(is.numeric)) %>%
      select(-any_of(meta))
  })

  merged <- Reduce(function(x, y) full_join(x, y, by = keys), scores)

  # build meta table from all instruments with preference
  first_non_na <- function(x) {
    x <- x[!is.na(x)]
    if (length(x)) x[[1]] else NA
  }

  meta_bind <- lapply(names(dflist), function(nm) {
    dflist[[nm]] %>%
      select(any_of(c(keys, meta))) %>%
      mutate(.src = nm)
  }) %>% bind_rows() %>%
    filter(if_all(all_of(keys), ~ !is.na(.x)))

  if (!is.null(prefer_meta) && prefer_meta %in% names(dflist)) {
    meta_bind <- meta_bind %>%
      mutate(.pref = .src == prefer_meta) %>%
      arrange(desc(.pref))
  }

  meta_one <- meta_bind %>%
    group_by(across(all_of(keys))) %>%
    summarise(across(all_of(meta), first_non_na), .groups = "drop")

  merged <- merged %>%
    left_join(meta_one, by = keys) %>%
    relocate(all_of(meta), .after = last_col())

  # diagnostics
  msg(sprintf("Merged: %d rows x %d cols", nrow(merged), ncol(merged)))
  dups <- merged %>%
    count(across(all_of(keys))) %>%
    filter(n > 1) %>%
    nrow()
  if (dups > 0) msg(sprintf("WARNING: %d duplicate key rows", dups))

  merged
}
