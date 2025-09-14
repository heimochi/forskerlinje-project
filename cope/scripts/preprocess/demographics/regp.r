REGP_full

# histogram for distribution (optional if running interactively)
hist(REGP_analysis$symptom_duration,
     breaks = 30,
     main = "Symptom Duration Distribution",
     xlab = "Years since onset",
     col = "lightblue")

# check extremes
cat("\nQC — symptom_duration <0 or >80:\n")
print(REGP_analysis %>%
        filter(!is.na(symptom_duration) &
               (symptom_duration < 0 | symptom_duration > 80)) %>%
        select(respondent_id, admission_year, regp_onset_year_num, symptom_duration))

# missingness
cat("\nQC — Missing symptom_duration:\n")
cat(sum(is.na(REGP_analysis$symptom_duration)), " / ",
    nrow(REGP_analysis), " (",
    round(100*mean(is.na(REGP_analysis$symptom_duration)),1), "%)\n", sep = "")
