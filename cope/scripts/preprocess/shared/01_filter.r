---
title: "Filter"
author: "MochiBear.Hei"
date: "09.05.2024"
format:
  html:
    toc: true
    toc-location: left
    embed-resources: true
    page-layout: full
    html-math-method: katex
editor: source
---

```{css, echo = F}
body{
  font-family: Helvetica;
  font-size: 16pt;
  max-width: 1000px;
  margin: auto;
  margin-left:310px;
}
pre{
  font-size: 14px;
}
/* Headers */
h1{
    font-size: 24pt;
  }
h1,h2{
    font-size: 20pt;
  }
h3,h4,h5,h6{
  font-size: 18pt;
}
#TOC {
  position: fixed;
  left: 0;
  top: 0;
  width: 300px;
  height: 100%;
  overflow:auto;
}
```

```{r setup,}
#| include: false
#| message: false
#| warning: false
#| results: hide
knitr::opts_chunk$set(echo = TRUE, dpi = 300)

# Data manipulation
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(tidyverse)
library(reshape2)

# Plotting
library(ggplot2)
library(naniar)

# Factor analysis
library(psych)
library(lavaan)

# Random Forest visualizations
library(randomForest)
library(caret)
```


```{r, warning=FALSE, message=FALSE}
# modules/filter_master.R

filter_instrument <- function(df, consent, instrument_label = "DATA") {
  # 1. Merge with consent
  df <- merge(df, consent, by = "respondent_id", all = TRUE, suffixes = c("", "_c"))

  # 2. Replace "" with NA
  df <- df %>%
    mutate(across(where(is.character), ~na_if(., '')))

  # 3. Recode assessment_context_label
  df <- df %>%
    mutate(assessment_context_label = case_when(
      assessment_context_label %in% c("Utredning", "P1 Kartlegging") ~ "Assessment",
      assessment_context_label %in% c("Innkomst", "Behandlingsstart", "P2 Innkomst") ~ "Admission",
      assessment_context_label %in% c("Utskriving", "P2 Utskriving", "Behandlingsslutt") ~ "Post-treatment",
      TRUE ~ assessment_context_label
    )) %>%
    filter(assessment_context_label %in% c("Assessment", "Admission", "Post-treatment"))

  # 4. Filter by department + consent
  df <- df %>%
    filter(treatment_type_id == 10, consent %in% c(1, NA))

  # 5. Clean treatment_name
  df <- df %>%
    mutate(treatment_name = case_when(
      treatment_name %in% c("Angst1", "Angst 1", "Angst1 UO") ~ "Angst 1",
      treatment_name %in% c("Angst2", "Angst 2") ~ "Angst 2",
      treatment_name %in% c("Angst3", "Angst 3") ~ "Angst 3",
      treatment_name == "Angst4" ~ "Angst 4",
      treatment_name == "Angst 5" ~ "Angst 5",
      treatment_name %in% c("Angst12", "Angst 12") ~ "Angst 12",
      treatment_name == "Angst P2" ~ "Angst P2",
      treatment_name == "Angst1 Booster" ~ "Angst 1 Booster",
      treatment_name == "Angst2 Booster" ~ "Angst 2 Booster",
      TRUE ~ treatment_name
    )) %>%
    filter(treatment_name %in% c("Angst", "Angst 1","Angst 2","Angst 3", "Angst 4", "Angst 5", "Angst 12", "Angst P1", "Angst P2"))

  # 6. Remove duplicates
  df <- df %>%
    distinct(respondent_id, assessment_context_label, .keep_all = TRUE)

  # Optional: Print counts
  if ("summarize_patient_counts" %in% ls()) {
    print(paste("Summary for", instrument_label))
    print(summarize_patient_counts(df))
  }

  return(df)
}
```

############################################################################################################

# Consistent coding and filtering

```{r}
# Merge only the 'consent' column with the BAI dataset by 'respondent_id'
filt_ATQ <- merge(ATQ, consent, 
                    by = "respondent_id", 
                    all = TRUE, 
                    suffixes = c("", "_c"))
  

# Replace empty strings with NA only in character columns
filt_ATQ  <- filt_ATQ  %>%
  mutate(across(where(is.character), ~na_if(., '')))

# Check N
print(summarize_patient_counts(filt_ATQ )) 
```

## Assessment context label

Required measurements:
Assessment = "Utredning", "P1 Kartlegging"
Admission = "Innkomst", "Behandlingsstart", "P2 Innkomst"
Post-treatment = "Utskriving", "P2 Utskriving"
  
```{r}
# Check assessment_context_label
unique(filt_ATQ $assessment_context_label)


# Consistent coding for assessment_context_label
filt_ATQ  <- filt_ATQ  %>%
  mutate(
    assessment_context_label = case_when(
      assessment_context_label %in% c("Utredning", "P1 Kartlegging") ~ "Assessment",
      assessment_context_label %in% c("Innkomst", "Behandlingsstart", "P2 Innkomst") ~ "Admission",
      assessment_context_label %in% c("Utskriving", "P2 Utskriving") ~ "Post-treatment", 
      TRUE ~ assessment_context_label
    )
  )

# Check N
print(summarize_patient_counts(filt_ATQ )) 

# Filter out to keep only rows with specified assessment_context_labels
filt_ATQ  <- filt_ATQ  %>%
  filter(assessment_context_label %in% c("Assessment", "Admission", "Post-treatment"))

# Check N
print(summarize_patient_counts(filt_ATQ )) 
```

## Department and consent
```{r}
# Filter the merged dataset based on 'consent' and 'treatment_type_id'
filt_ATQ  <- filt_ATQ  %>%
  filter(treatment_type_id == 10, consent %in% c(1, NA))

# Check N
print(summarize_patient_counts(filt_ATQ))
head(filt_ATQ)
```

## Treatment Name

```{r}
# Check treatment_name
unique(filt_ATQ$treatment_name)

# Consistent coding for treatment_name
filt_ATQ <- filt_ATQ %>%
  mutate(
    treatment_name = case_when(
      treatment_name %in% c("Angst1", "Angst 1", "Angst1 UO") ~ "Angst 1",
      treatment_name %in% c("Angst2", "Angst 2") ~ "Angst 2",
      treatment_name == "Angst1 Booster" ~ "Angst 1 Booster",
      treatment_name == "Angst2 Booster" ~ "Angst 2 Booster",
      treatment_name %in% c("Angst3", "Angst 3") ~ "Angst 3",
      treatment_name %in% c("Angst4", "Angst 4") ~ "Angst 4",
      treatment_name %in% c("Angst5", "Angst 5") ~ "Angst 5",
      treatment_name %in% c("Angst12", "Angst 12") ~ "Angst 12",
      treatment_name == "Angst P2" ~ "Angst P2",
      TRUE ~ treatment_name
    )
  )

# Remove boosters (patients that have returned to treatment)
filt_ATQ <- filt_ATQ %>%
 filter(treatment_name %in% c("Angst", "Angst 1","Angst 2","Angst 3", "Angst 4", "Angst 5", "Angst 12", "Angst P1", "Angst P2" ))

# Check N
print(summarize_patient_counts(filt_ATQ))
```

## Handle duplicate records

```{r}
# Define the columns to consider for identifying duplicates
id_columns <- c("respondent_id", "assessment_context_label")

# Remove duplicates based on specific columns, keeping the first occurrence
filt_ATQ  <- filt_ATQ  %>%
  distinct(across(all_of(id_columns)), .keep_all = TRUE)

# Check if duplicates are removed
print(nrow(filt_ATQ)) #dataset w/ duplicates removed
print(nrow(filt_ATQ))

# Check N
print(summarize_patient_counts(filt_ATQ))

```


