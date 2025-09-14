# ---------------------------------------------------------
# Merge Module
# Author: MochiBear.Hei
# Created: 2025-09-11
# Description: Merges instrument data frames into one wide table.
# ---------------------------------------------------------

library(dplyr)

# ---------------------------------------------------------
# - Keys: respondent_id + assessment_context_label + treatment_id
# - Keeps all score columns as-is (already prefixed)
# - Full-joins across instruments; preserves all keys
# - Verbose: prints per-instrument rows kept and numeric cols kept
# - Returns a single wide dataset with harmonized score variables
# ---------------------------------------------------------

merge_instruments <- function(
  dflist,
  keys = c(
    "respondent_id",
    "assessment_context_label",
    "treatment_id"
  ),
  verbose = TRUE
) {
  stopifnot(length(dflist) >= 1)

  nm <- names(dflist)
  if (is.null(nm) || any(nm == "")) nm <- paste0("df", seq_along(dflist))

  is_score_col <- function(df) {
    nms <- names(df)
    keep <- vapply(df, is.numeric, logical(1))
    drop_ids <- c("respondent_id", "treatment_id", "treatment_type_id")
    keep[nms %in% drop_ids] <- FALSE
    pat <- "(_aa$|_items$|items_answered|_valid$)"
    keep[grepl(pat, nms, ignore.case = TRUE)] <- FALSE
    keep
  }

  coalesce_dupes <- function(df) {
    base <- sub("(\\.[xy])+$", "", names(df))
    dups <- unique(base[duplicated(base)])
    for (b in dups) {
      idx <- which(base == b)
      df[[b]] <- Reduce(coalesce, df[idx])
    }
    keep <- !grepl("(\\.[xy])+$", names(df))
    df[, keep, drop = FALSE]
  }

  for (i in seq_along(dflist)) {
    miss <- setdiff(keys, names(dflist[[i]]))
    if (length(miss)) {
      stop(sprintf("'%s' missing key(s): %s",
                   nm[i], paste(miss, collapse = ", ")))
    }
  }

  if (isTRUE(verbose)) {
    cat("\n# ---- Merge plan ----\n")
    cat("Keys: ", paste(keys, collapse = " + "), "\n\n", sep = "")
    for (i in seq_along(dflist)) {
      df <- dflist[[i]]
      n_rows <- nrow(df)
      n_keys <- n_distinct(df[, keys, drop = FALSE])
      sc <- names(df)[is_score_col(df)]
      cat(sprintf("[%s] rows: %s | keys: %s | score cols: %s\n",
                  nm[i], n_rows, n_keys, length(sc)))
      if (length(sc)) {
        cat("   • ", paste(sc, collapse = ", "), "\n", sep = "")
      }
    }
    cat("\n")
  }

  out <- Reduce(function(acc, df) {
    j <- full_join(acc, df, by = keys, suffix = c(".x", ".y"))
    coalesce_dupes(j)
  }, dflist)

  if (isTRUE(verbose)) {
    cat("# ---- Merge complete ----\n")
    cat("Final rows: ", nrow(out),
        " | Final columns: ", ncol(out), "\n\n", sep = "")
  }

  out
}