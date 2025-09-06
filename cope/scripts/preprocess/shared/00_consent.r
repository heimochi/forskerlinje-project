library(readr)
library(dplyr)
library(janitor)

load_consent <- function(csv_path = "/Users/maggieheimvik/Desktop/COPE/data/dataset/scripts/anon/consent_a.csv") {

  read_csv(csv_path, show_col_types = FALSE) %>%
    clean_names() %>%                                        # makes respondent_id, consent
    rename(respondent_id = any_of(c("respondent_id","respondent.id"))) %>%
    select(respondent_id, consent) %>%
    mutate(                                                  # map to 1/0/NA robustly
      consent = case_when(
        tolower(as.character(consent)) %in% c("1","true","yes","y") ~ 1L,
        tolower(as.character(consent)) %in% c("0","false","no","n") ~ 0L,
        TRUE ~ NA_integer_
      )
    ) %>%
    group_by(respondent_id) %>%                              # collapse duplicates
    summarise(
      consent = case_when(
        any(consent == 1L, na.rm = TRUE) ~ 1L,              # any 1 -> 1
        all(is.na(consent))              ~ NA_integer_,      # all NA -> NA
        TRUE                             ~ 0L                # otherwise -> 0
      ),
      .groups = "drop"
    )
}