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
  keys = c("respondent_id","assessment_context_label","treatment_id"),
  verbose = TRUE
) {
  stopifnot(length(dflist) >= 1)

  nm <- names(dflist)
  if (is.null(nm) || any(nm == "")) nm <- paste0("df", seq_along(dflist))

  is_score_col <- function(df) {
    nms <- names(df)
    keep <- vapply(df, is.numeric, logical(1))
    drop_ids <- c("respondent_id","treatment_id","treatment_type_id")
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

  allowed_ctx <- c("Assessment","Admission","Post-treatment")

  dflist <- lapply(dflist, function(df) {
    df %>%
      select(-any_of(c("treatment_name","treatment_type_id"))) %>%
      filter(assessment_context_label %in% allowed_ctx)
  })

  for (i in seq_along(dflist)) {
    miss <- setdiff(keys, names(dflist[[i]]))
    if (length(miss)) {
      stop(sprintf("'%s' missing key(s): %s",
                   nm[i], paste(miss, collapse = ", ")))
    }
  }

  input_sum <- lapply(seq_along(dflist), function(i) {
    df <- dflist[[i]]
    n_rows <- nrow(df)
    n_keys <- n_distinct(df[, keys, drop = FALSE])
    sc <- names(df)[is_score_col(df)]
    list(
      name = nm[i], rows = n_rows, keys = n_keys,
      n_score = length(sc), score_names = sc
    )
  })

  if (isTRUE(verbose)) {
    cat("\n# ---- Merge plan ----\n")
    cat("Keys: ", paste(keys, collapse = " + "), "\n\n", sep = "")
    for (s in input_sum) {
      cat(sprintf("[%s] rows: %s | keys: %s | score cols: %s\n",
                  s$name, s$rows, s$keys, s$n_score))
      if (length(s$score_names)) {
        cat("   • ", paste(s$score_names, collapse = ", "), "\n", sep = "")
      }
    }
    cat("\n")
  }

  raw_out <- Reduce(function(acc, df) {
    j <- full_join(acc, df, by = keys, suffix = c(".x",".y"))
    coalesce_dupes(j)
  }, dflist)

  forbidden <- intersect(names(raw_out),
                         c("treatment_name","treatment_type_id"))

  bad_ctx <- setdiff(unique(raw_out$assessment_context_label), allowed_ctx)

  dup_keys <- raw_out %>%
    count(across(all_of(keys)), name = "n") %>%
    filter(n > 1)

  out <- raw_out %>%
    filter(assessment_context_label %in% allowed_ctx)

  if (isTRUE(verbose)) {
    cat("# ---- Merge complete ----\n")
    cat("Final rows: ", nrow(out),
        " | Final columns: ", ncol(out), "\n", sep = "")
    if (length(forbidden)) {
      cat("\n[QC] Forbidden columns resurrected: ",
          paste(forbidden, collapse = ", "), "\n", sep = "")
    } else {
      cat("\n[QC] Forbidden columns resurrected: none\n")
    }
    if (length(bad_ctx)) {
      cat("[QC] Unexpected context labels (pre-filter): ",
          paste(bad_ctx, collapse = ", "), "\n", sep = "")
    } else {
      cat("[QC] Unexpected context labels (pre-filter): none\n")
    }
    if (nrow(dup_keys)) {
      cat("[QC] Duplicate key rows: ", nrow(dup_keys), "\n", sep = "")
      print(head(dup_keys, 10), n = 10)
    } else {
      cat("[QC] Duplicate key rows: none\n")
    }
    cat("\n")
  }

  attr(out, "merge_qc") <- list(
    inputs = input_sum,
    forbidden_cols = forbidden,
    unexpected_contexts = bad_ctx,
    duplicate_keys = dup_keys
  )

  out
}

# ---------------------------------------------------------
# print_merge_qc()
# - Prints a compact report from attr 'merge_qc'
# ---------------------------------------------------------
print_merge_qc <- function(x) {
  qc <- attr(x, "merge_qc")
  if (is.null(qc)) {
    cat("No 'merge_qc' attribute.\n")
    return(invisible(NULL))
  }
  cat("# ---- Merge QC ----\n")
  cat("Inputs:\n")
  for (s in qc$inputs) {
    cat(sprintf("  - %s: rows=%s keys=%s score_cols=%s\n",
                s$name, s$rows, s$keys, s$n_score))
  }
  if (length(qc$forbidden_cols)) {
    cat("Forbidden resurrected: ",
        paste(qc$forbidden_cols, collapse = ", "), "\n", sep = "")
  } else {
    cat("Forbidden resurrected: none\n")
  }
  if (length(qc$unexpected_contexts)) {
    cat("Unexpected contexts: ",
        paste(qc$unexpected_contexts, collapse = ", "), "\n", sep = "")
  } else {
    cat("Unexpected contexts: none\n")
  }
  if (is.data.frame(qc$duplicate_keys) && nrow(qc$duplicate_keys)) {
    cat("Duplicate key rows: ", nrow(qc$duplicate_keys), "\n", sep = "")
    print(head(qc$duplicate_keys, 10), n = 10)
  } else {
    cat("Duplicate key rows: none\n")
  }
  invisible(qc)
}