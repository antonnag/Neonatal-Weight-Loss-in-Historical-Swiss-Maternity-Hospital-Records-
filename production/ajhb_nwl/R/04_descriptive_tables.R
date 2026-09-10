# ============================================================
# 04_descriptive_tables.R
# Main manuscript and appendix descriptive tables
# ============================================================

# MAIN MANUSCRIPT TABLE 1
# Table 1. Characteristics of the analytical study population
# Placement: Results 3.1 Study population characteristics
# ============================================================
n_table1 <- nrow(df_analysis_clean)

fmt_mean_sd <- function(x, digits = 1) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"),
          mean(x, na.rm = TRUE),
          sd(x, na.rm = TRUE))
}

fmt_median_iqr <- function(x, digits = 2) {
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"),
          median(x, na.rm = TRUE),
          IQR(x, na.rm = TRUE))
}

fmt_n_pct <- function(condition, denominator = n_table1, digits = 1) {
  n <- sum(condition, na.rm = TRUE)
  pct <- round(n / denominator * 100, digits)
  sprintf(paste0("%d (%.", digits, "f%%)"), n, pct)
}

table_1_population <- tibble(
  Characteristic = c(
    "Continuous characteristics",
    "Maternal age, years",
    "Gestational age, weeks",
    "Birthweight, g",
    "Categorical characteristics",
    "Parity",
    "    Parity = 1",
    "    Parity = 2",
    "    Parity = 3",
    "    Parity ≥4",
    "Historical exposures",
    "    Pregnancy during World War I",
    "    Pregnancy during influenza pandemic",
    "    Maternal influenza during pregnancy",
    "Neonatal characteristics",
    "    Male sex",
    "    Female sex",
    "    Preterm birth (<37 weeks)",
    "    Low birth weight (<2500 g)",
    "    Neonatal mortality within 28 days, n (%)",
    "Feeding characteristics",
    "    Artificial",
    "    Breastfeeding",
    "    Mixed",
    "    Others",
    "Outcome",
    "    Maximum neonatal weight loss, % (mean, SD)",
    "    Maximum neonatal weight loss, % (median, IQR)",
    "    Excessive neonatal weight loss (>10%)"
  ),
  Value = c(
    "",
    fmt_mean_sd(df_analysis_clean$age_mother, digits = 1),
    fmt_mean_sd(df_analysis_clean$ga_model, digits = 1),
    fmt_mean_sd(df_analysis_clean$birthweight, digits = 0),
    "",
    "",
    fmt_n_pct(df_analysis_clean$parity == 1),
    fmt_n_pct(df_analysis_clean$parity == 2),
    fmt_n_pct(df_analysis_clean$parity == 3),
    fmt_n_pct(df_analysis_clean$parity >= 4),
    "",
    fmt_n_pct(df_analysis_clean$pregn_during_WW1 == "yes"),
    fmt_n_pct(df_analysis_clean$pregn_during_pandemic == "yes"),
    fmt_n_pct(df_analysis_clean$grippe_during_pregnancy == "yes"),
    "",
    fmt_n_pct(df_analysis_clean$sex_cat == "male"),
    fmt_n_pct(df_analysis_clean$sex_cat == "female"),
    fmt_n_pct(df_analysis_clean$ptb_group == "preterm"),
    fmt_n_pct(df_analysis_clean$lbw_group == "low_birthweight"),
    fmt_n_pct(df_analysis_clean$neonatal_death_28 == 1, digits = 2),
    "",
    fmt_n_pct(df_analysis_clean$feeding_cat == "artificial"),
    fmt_n_pct(df_analysis_clean$feeding_cat == "breastfeeding"),
    fmt_n_pct(df_analysis_clean$feeding_cat == "mixed"),
    fmt_n_pct(df_analysis_clean$feeding_cat == "other"),
    "",
    fmt_mean_sd(df_analysis_clean$max_weight_loss_pct, digits = 2),
    fmt_median_iqr(df_analysis_clean$max_weight_loss_pct, digits = 2),
    fmt_n_pct(df_analysis_clean$excessive_weight_loss_10 == 1)
  )
)

table_2_feeding_weightloss <- df_analysis_clean %>%
  filter(!is.na(feeding_cat)) %>%
  group_by(feeding_cat) %>%
  summarise(
    n = n(),
    percentage = round(n / nrow(df_analysis_clean) * 100, 1),
    mean_weight_loss = round(mean(max_weight_loss_pct, na.rm = TRUE), 2),
    sd_weight_loss = round(sd(max_weight_loss_pct, na.rm = TRUE), 2),
    median_weight_loss = round(median(max_weight_loss_pct, na.rm = TRUE), 2),
    iqr_weight_loss = round(IQR(max_weight_loss_pct, na.rm = TRUE), 2),
    excessive_cases = sum(excessive_weight_loss_10 == 1, na.rm = TRUE),
    excessive_percent = round(mean(excessive_weight_loss_10 == 1, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  )

table_3_period_weightloss <- bind_rows(
  df_analysis_clean %>%
    filter(!is.na(pregn_during_WW1)) %>%
    group_by(exposure = "Pregnancy overlapped WWI", level = pregn_during_WW1) %>%
    summarise(
      n = n(),
      mean_weight_loss = round(mean(max_weight_loss_pct, na.rm = TRUE), 2),
      sd_weight_loss = round(sd(max_weight_loss_pct, na.rm = TRUE), 2),
      median_weight_loss = round(median(max_weight_loss_pct, na.rm = TRUE), 2),
      iqr_weight_loss = round(IQR(max_weight_loss_pct, na.rm = TRUE), 2),
      excessive_cases = sum(excessive_weight_loss_10 == 1, na.rm = TRUE),
      excessive_percent = round(mean(excessive_weight_loss_10 == 1, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ),
  df_analysis_clean %>%
    filter(!is.na(pregn_during_pandemic)) %>%
    group_by(exposure = "Pregnancy overlapped influenza pandemic", level = pregn_during_pandemic) %>%
    summarise(
      n = n(),
      mean_weight_loss = round(mean(max_weight_loss_pct, na.rm = TRUE), 2),
      sd_weight_loss = round(sd(max_weight_loss_pct, na.rm = TRUE), 2),
      median_weight_loss = round(median(max_weight_loss_pct, na.rm = TRUE), 2),
      iqr_weight_loss = round(IQR(max_weight_loss_pct, na.rm = TRUE), 2),
      excessive_cases = sum(excessive_weight_loss_10 == 1, na.rm = TRUE),
      excessive_percent = round(mean(excessive_weight_loss_10 == 1, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )
)

fmt_mean_sd_s7 <- function(x, digits = 2) {
  if (all(is.na(x))) return(NA_character_)
  sprintf(paste0("%.", digits, "f (%.", digits, "f)"),
          mean(x, na.rm = TRUE), sd(x, na.rm = TRUE))
}

fmt_n_pct_s7 <- function(condition, denominator, digits = 1) {
  if (is.na(denominator) || denominator == 0) return(NA_character_)
  n <- sum(condition, na.rm = TRUE)
  pct <- round(n / denominator * 100, digits)
  sprintf(paste0("%d (%.", digits, "f%%)"), n, pct)
}

df_exclusion_comparison <- bind_rows(
  df_analysis_clean %>% mutate(exclusion_group = "Included analytical cohort"),
  df_non_evaluable_weight_trajectory %>% mutate(exclusion_group = "Excluded non-evaluable NWL trajectory")
) %>%
  mutate(exclusion_group = factor(
    exclusion_group,
    levels = c("Included analytical cohort", "Excluded non-evaluable NWL trajectory")
  ))

safe_t_test_s7 <- function(var) {
  if (!var %in% names(df_exclusion_comparison)) return(NA_real_)
  test_data <- df_exclusion_comparison %>% filter(!is.na(.data[[var]]), !is.na(exclusion_group))
  if (n_distinct(test_data$exclusion_group) < 2 || nrow(test_data) == 0) return(NA_real_)
  tryCatch(t.test(as.formula(paste(var, "~ exclusion_group")), data = test_data)$p.value,
           error = function(e) NA_real_)
}

safe_cat_test_s7 <- function(var) {
  if (!var %in% names(df_exclusion_comparison)) return(NA_real_)
  test_data <- df_exclusion_comparison %>% filter(!is.na(.data[[var]]), !is.na(exclusion_group))
  if (n_distinct(test_data$exclusion_group) < 2 || n_distinct(test_data[[var]]) < 2) return(NA_real_)
  tab <- table(test_data$exclusion_group, test_data[[var]])
  tryCatch(if (any(tab < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value,
           error = function(e) NA_real_)
}

n_included_s7 <- nrow(df_analysis_clean)
n_excluded_s7 <- nrow(df_non_evaluable_weight_trajectory)

Supplementary_Table_S7_Excluded_vs_Included <- tibble(
  Characteristic = c(
    "N",
    "Maternal age, years, mean (SD)",
    "Birthweight, g, mean (SD)",
    "Gestational age, weeks, mean (SD)",
    "Preterm birth (<37 weeks), n (%)",
    "Low birthweight (<2500 g), n (%)",
    "Female sex, n (%)",
    "Male sex, n (%)",
    "Artificial feeding, n (%)",
    "Breastfeeding, n (%)",
    "Mixed feeding, n (%)",
    "Other feeding, n (%)",
    "Pregnancy during World War I, n (%)",
    "Pregnancy during influenza pandemic, n (%)",
    "Maternal influenza during pregnancy, n (%)"
  ),
  `Included analytical cohort` = c(
    as.character(n_included_s7),
    fmt_mean_sd_s7(df_analysis_clean$age_mother),
    fmt_mean_sd_s7(df_analysis_clean$birthweight),
    fmt_mean_sd_s7(df_analysis_clean$ga_model),
    fmt_n_pct_s7(df_analysis_clean$ptb_group == "preterm", n_included_s7),
    fmt_n_pct_s7(df_analysis_clean$lbw_group == "low_birthweight", n_included_s7),
    fmt_n_pct_s7(df_analysis_clean$sex_cat == "female", n_included_s7),
    fmt_n_pct_s7(df_analysis_clean$sex_cat == "male", n_included_s7),
    fmt_n_pct_s7(df_analysis_clean$feeding_cat == "artificial", n_included_s7),
    fmt_n_pct_s7(df_analysis_clean$feeding_cat == "breastfeeding", n_included_s7),
    fmt_n_pct_s7(df_analysis_clean$feeding_cat == "mixed", n_included_s7),
    fmt_n_pct_s7(df_analysis_clean$feeding_cat == "other", n_included_s7),
    fmt_n_pct_s7(df_analysis_clean$pregn_during_WW1 == "yes", n_included_s7),
    fmt_n_pct_s7(df_analysis_clean$pregn_during_pandemic == "yes", n_included_s7),
    fmt_n_pct_s7(df_analysis_clean$grippe_during_pregnancy == "yes", n_included_s7)
  ),
  `Excluded non-evaluable NWL trajectory` = c(
    as.character(n_excluded_s7),
    fmt_mean_sd_s7(df_non_evaluable_weight_trajectory$age_mother),
    fmt_mean_sd_s7(df_non_evaluable_weight_trajectory$birthweight),
    fmt_mean_sd_s7(df_non_evaluable_weight_trajectory$ga_model),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$ptb_group == "preterm", n_excluded_s7),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$lbw_group == "low_birthweight", n_excluded_s7),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$sex_cat == "female", n_excluded_s7),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$sex_cat == "male", n_excluded_s7),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$feeding_cat == "artificial", n_excluded_s7),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$feeding_cat == "breastfeeding", n_excluded_s7),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$feeding_cat == "mixed", n_excluded_s7),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$feeding_cat == "other", n_excluded_s7),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$pregn_during_WW1 == "yes", n_excluded_s7),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$pregn_during_pandemic == "yes", n_excluded_s7),
    fmt_n_pct_s7(df_non_evaluable_weight_trajectory$grippe_during_pregnancy == "yes", n_excluded_s7)
  ),
  Test = c(
    NA, "t-test", "t-test", "t-test",
    "Chi-square/Fisher", "Chi-square/Fisher", "Chi-square/Fisher", "Chi-square/Fisher",
    "Chi-square/Fisher", "Chi-square/Fisher", "Chi-square/Fisher", "Chi-square/Fisher",
    "Chi-square/Fisher", "Chi-square/Fisher", "Chi-square/Fisher"
  ),
  p.value = c(
    NA,
    format_p_value(safe_t_test_s7("age_mother")),
    format_p_value(safe_t_test_s7("birthweight")),
    format_p_value(safe_t_test_s7("ga_model")),
    format_p_value(safe_cat_test_s7("ptb_group")),
    format_p_value(safe_cat_test_s7("lbw_group")),
    format_p_value(safe_cat_test_s7("sex_cat")),
    format_p_value(safe_cat_test_s7("sex_cat")),
    format_p_value(safe_cat_test_s7("feeding_cat")),
    format_p_value(safe_cat_test_s7("feeding_cat")),
    format_p_value(safe_cat_test_s7("feeding_cat")),
    format_p_value(safe_cat_test_s7("feeding_cat")),
    format_p_value(safe_cat_test_s7("pregn_during_WW1")),
    format_p_value(safe_cat_test_s7("pregn_during_pandemic")),
    format_p_value(safe_cat_test_s7("grippe_during_pregnancy"))
  )
)

Supplementary_Table_S7_Exclusion_Reasons <- df_non_evaluable_weight_trajectory %>%
  count(exclusion_reason, name = "n") %>%
  mutate(
    percentage_of_excluded = round(n / sum(n) * 100, 1),
    percentage_of_eligible = round(n / nrow(df_analysis) * 100, 2)
  )
