# ---------------------------------------------------------
# Consent Module
# Author: MochiBear.Hei
# Created: 2025-12-14
# Description: Load and clean consent data from CSV.
#   - Reads consent file
#   - Standardizes column names
#   - Maps consent values to 1 / 0 / NA
#   - Collapses duplicates per respondent
# ---------------------------------------------------------

library(readr)
library(dplyr)
library(janitor)

# ---------------------------------------------------------

# ---------------------------------------------------------
# Read file
# ---------------------------------------------------------

consent <- read_csv(
  file.path(data_dir, "scripts/anon/consent_a.csv"),
  show_col_types = FALSE
)
# Check
#View(consent)
#head(consent)
#names(consent)  # check the column names

# ---------------------------------------------------------
# Clean column names 
# ---------------------------------------------------------

consent <- consent %>%
  clean_names() #  them lowercase and consistent
#names(consent)  # check

# ---------------------------------------------------------
# Keep only the columns we need
# ---------------------------------------------------------

consent <- consent %>%
  select(respondent_id, consent)
#head(consent)  # check columns
#nrow(consent)  # check how many rows

# ---------------------------------------------------------
# Standardize consent values to 1, 0, or NA
# ---------------------------------------------------------

consent <- consent %>%
  mutate(
    consent = case_when(
      # If consent is "1", "true", "yes", or "y" -> make it 1
      tolower(as.character(consent)) %in% c("1", "true", "yes", "y") ~ 1L,
      
      # If consent is "0", "false", "no", or "n" -> make it 0
      tolower(as.character(consent)) %in% c("0", "false", "no", "n") ~ 0L,
      
      # Everything else (including blanks) -> make it NA
      TRUE ~ NA_integer_
    )
  )
table(consent$consent, useNA = "always") #check
#unique(consent$consent)

# 0=356, 1=5522, NA=23

# ---------------------------------------------------------
# Duplicates
# ---------------------------------------------------------
consent <- consent %>%
  group_by(respondent_id) %>%
  summarise(
    consent = case_when(
      any(consent == 1L, na.rm = TRUE) ~ 1L,      # Any 1 -> 1
      any(consent == 0L, na.rm = TRUE) ~ 0L,      # Any 0 -> 0
      TRUE                             ~ NA_integer_  # All NA -> NA
    ),
    .groups = "drop"
  )

# Check the result
table(consent$consent, useNA = "always")

# 0=260, 1=4122, NA=15

#---------------------------------------------------
# final
# ---------------------------------------------------------

# Summary
cat("Total respondents:", nrow(consent), "\n")
cat("Consented (1):", sum(consent$consent == 1, na.rm = TRUE), "\n")
cat("Declined (0):", sum(consent$consent == 0, na.rm = TRUE), "\n")
cat("Unknown (NA):", sum(is.na(consent$consent)), "\n")

#---------------------------------------------------
# save
# ---------------------------------------------------------

write_csv(consent, file.path(data_dir, "clean/consent.csv"))
