# ---------------------------------------------------------
# Filter Module
# Author: MochiBear.Hei
# Created: 2025-09-01
# Description: Standardized filtering for instruments (e.g., ATQ, BAI, etc.)
# ---------------------------------------------------------
library(dplyr)
library(readxl)
# ---------------------------------------------------------
#Load data
# ---------------------------------------------------------
data_dir <- "/Users/maggieheimvik/Desktop/COPE/data/dataset"
consent <- read_csv(file.path(data_dir, "clean/consent.csv"))
# ---------------------------------------------------------
BAI <- read_csv(file.path(data_dir, "scripts/anon/BAI_a.csv")) #8743 of 52
ATQ <- read_excel(file.path(data_dir, "ATQ_avid.xls"))  #3710 of 57
BDI <- read_excel(file.path(data_dir, "BDI_avid.xls")) #12035 of 53
IIP <- read_excel(file.path(data_dir, "IIP64_avid.xls")) #13646 of 144
MCQ <- read_excel(file.path(data_dir, "MCQ_avid.xls")) #4100 of 68
PSWQ <- read_excel(file.path(data_dir, "PSWQavid.xls"))    # 2729 of 47 
REGP <- read_csv2(file.path(data_dir, "regp.csv")) #487 of 44
REGT <- read_csv(file.path(data_dir, "reg_t_a.csv")) #242 of 83
SCL<- read_csv2(file.path(data_dir, "SCL90.csv"))  #2081 of 181
# ---------------------------------------------------------

data <- SCL #####CHANGE NAME HERE

# ---------------------------------------------------------
# Clean column names 
# ---------------------------------------------------------

data <- data %>%
  clean_names() #  them lowercase and consistent
names(data)  # check

if ("assessment_instance_context_label" %in% colnames(data)) {
  data <- data %>%
    rename(assessment_context_label = assessment_instance_context_label)
} 

## ---------------------------------------------------------
#Check
# ---------------------------------------------------------
View(data)
colnames(data)
cat("Unique people:", n_distinct(data$respondent_id), "\n")

table(data$treatment_type_id, useNA = "always")
table(data$assessment_context_label, useNA = "always")
table(data$treatment_name, useNA = "always")

# Summary
str(data)
summary(data)

# ---------------------------------------------------------
# Consent
# ---------------------------------------------------------
consent <- consent %>%
  filter(is.na(consent) | consent == 1L) %>%   #keeps NA OR 1
  select(respondent_id)

data <- data %>%
  semi_join(consent, by = "respondent_id")

# Check
cat("People after consent filter:", n_distinct(data$respondent_id), "\n")

# ---------------------------------------------------------
# Convert Empty Strings to NA
# ---------------------------------------------------------
data <- data %>%
  mutate(across(where(is.character), ~ na_if(.x, "")))
 
# Check
cat("People after empty string conversion:", n_distinct(data$respondent_id), "\n")

# ---------------------------------------------------------
# Treatment Type
# ---------------------------------------------------------
# Keep only treatment_type_id = 10
data <- data %>%
  filter(treatment_type_id == 10) %>%
  select(-treatment_type_id)

# Check
cat("Rows after treatment type filter:", nrow(data), "\n")
cat("People after treatment type filter:", n_distinct(data$respondent_id), "\n")

# ---------------------------------------------------------
# Assessment Context Labels
# ---------------------------------------------------------
data <- data %>%
  mutate(
    assessment_context_label = case_when(
      assessment_context_label %in% c(
        "Utredning", "P1 Kartlegging", "P0 Basisutredning", "SGKT Utredning"
      ) ~ "Assessment",
      assessment_context_label %in% c(
        "Innkomst", "P2 Innkomst", "Inn sekv2", "Behandlingsstart"
      ) ~ "Admission",
      assessment_context_label %in% c(
        "Utskriving", "P2 Utskriving", "Ut sekv 1", "Behandlingsslutt",
        "SGKT Gruppeslutt"
      ) ~ "Post-treatment",
      TRUE ~ assessment_context_label
    ) 
  ) %>%
  filter(assessment_context_label %in% c("Assessment", "Admission", "Post-treatment"))

# Check
cat("Rows after context filter:", nrow(data), "\n")
cat("People after context filter:", n_distinct(data$respondent_id), "\n")
table(data$assessment_context_label, useNA = "always")

# ---------------------------------------------------------
# Treatment Name
# ---------------------------------------------------------

data <- data %>%
  mutate(
    treatment_name = case_when(
      treatment_name %in% c("Angst1", "Angst 1", "Angst1 UO") ~ "Angst 1",
      treatment_name %in% c("Angst2", "Angst 2") ~ "Angst 2",
      treatment_name == "Angst1 Booster" ~ "Angst 1 Booster",
      treatment_name == "Angst2 Booster" ~ "Angst 2 Booster",
      treatment_name %in% c("Angst3", "Angst 3") ~ "Angst 3",
      treatment_name == "Angst4" ~ "Angst 4",
      treatment_name == "Angst 5" ~ "Angst 5",
      treatment_name %in% c("Angst12", "Angst 12") ~ "Angst 12",
      treatment_name == "Angst P1" ~ "Angst P1",
      treatment_name == "Angst P2" ~ "Angst P2",
      TRUE ~ treatment_name
    )
  ) %>%
  filter(treatment_name %in% c(
    "Angst", "Angst 1", "Angst 2", "Angst 3", "Angst 4",
    "Angst 5", "Angst 12", "Angst P1", "Angst P2"
  ))

# Check
cat("Rows after treatment name filter:", nrow(data), "\n")
cat("People after treatment name filter:", n_distinct(data$respondent_id), "\n")
table(data$treatment_name, useNA = "always")


# ---------------------------------------------------------
# Duplicates
# ---------------------------------------------------------
data <- data %>%
  distinct(
    respondent_id, assessment_context_label, treatment_id,
    .keep_all = TRUE
  )

# Check
cat("Rows after removing duplicates:", nrow(data), "\n")
cat("People after removing duplicates:", n_distinct(data$respondent_id), "\n")

#---------------------------------------------------
# save
# ---------------------------------------------------------

write_csv(data, file.path(data_dir, "clean/filtered/scl.csv")) ##CHANGE NAME
