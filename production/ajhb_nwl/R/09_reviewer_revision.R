# ============================================================
# 09_reviewer_revision.R
# AJHB reviewer revision: postnatal weight measurement completeness,
# timing, and exact day-5/day-10 sensitivity analyses
# ============================================================
# Scope of this file:
# 1) quantify one vs two postnatal weight measurements and actual timing;
# 2) compare infants with one vs two postnatal measurements;
# 3) define the exact day-5/day-10 "best data" subcohort;
# 4) repeat only the two primary GA-adjusted models in that subcohort.
#
# Existing main and supplementary analyses are intentionally left unchanged.

# ------------------------------------------------------------
# 0) Safety checks
# ------------------------------------------------------------
required_reviewer_columns <- c(
  "weight_a", "weight_b", "day_a", "day_b",
  "max_weight_loss_pct", "excessive_weight_loss_10",
  "pregn_during_WW1", "pregn_during_pandemic", "feeding_cat",
  "ga_model", "sex_cat", "parity", "age_mother",
  "birthweight", "ptb_group", "lbw_group", "neonatal_death_28"
)

missing_reviewer_columns <- setdiff(required_reviewer_columns, names(df_analysis_clean))
if (length(missing_reviewer_columns) > 0) {
  stop(
    "Reviewer revision cannot run because required columns are missing: ",
    paste(missing_reviewer_columns, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 1) Number and timing of postnatal weight measurements
# ------------------------------------------------------------
df_reviewer_measurements <- df_analysis_clean %>%
  mutate(
    n_postnatal_measurements = as.integer(!is.na(weight_a)) + as.integer(!is.na(weight_b)),
    measurement_pattern = case_when(
      !is.na(weight_a) & !is.na(weight_b) ~ "Both measurements",
      !is.na(weight_a) &  is.na(weight_b) ~ "Only first measurement",
       is.na(weight_a) & !is.na(weight_b) ~ "Only second measurement",
      TRUE ~ "No postnatal measurement"
    ),
    measurement_group = case_when(
      n_postnatal_measurements == 1 ~ "One measurement",
      n_postnatal_measurements == 2 ~ "Two measurements",
      TRUE ~ NA_character_
    ),
    measurement_group = factor(
      measurement_group,
      levels = c("One measurement", "Two measurements")
    ),
    exact_day5_day10 =
      !is.na(weight_a) & !is.na(weight_b) &
      !is.na(day_a) & !is.na(day_b) &
      day_a == 5 & day_b == 10,
    timing_group = if_else(
      exact_day5_day10,
      "Exact days 5 and 10",
      "All other measurement patterns"
    ),
    timing_group = factor(
      timing_group,
      levels = c("Exact days 5 and 10", "All other measurement patterns")
    )
  )

if (any(df_reviewer_measurements$n_postnatal_measurements == 0, na.rm = TRUE)) {
  warning("At least one final-cohort infant has zero postnatal weights; check cohort logic.")
}

n_reviewer_total <- nrow(df_reviewer_measurements)

table_reviewer_measurement_completeness <- df_reviewer_measurements %>%
  count(n_postnatal_measurements, measurement_group, name = "n") %>%
  mutate(
    percent_of_final_cohort = round(100 * n / n_reviewer_total, 2)
  ) %>%
  arrange(n_postnatal_measurements)

table_reviewer_measurement_pattern <- df_reviewer_measurements %>%
  count(measurement_pattern, name = "n") %>%
  mutate(
    percent_of_final_cohort = round(100 * n / n_reviewer_total, 2)
  ) %>%
  arrange(desc(n))

# Actual timing distributions are restricted to records where the corresponding
# weight AND measurement day are both available.
table_reviewer_day_a_distribution <- df_reviewer_measurements %>%
  filter(!is.na(weight_a), !is.na(day_a)) %>%
  count(day_a, name = "n") %>%
  mutate(
    percent_among_first_measurements_with_day = round(100 * n / sum(n), 2)
  ) %>%
  arrange(day_a)

table_reviewer_day_b_distribution <- df_reviewer_measurements %>%
  filter(!is.na(weight_b), !is.na(day_b)) %>%
  count(day_b, name = "n") %>%
  mutate(
    percent_among_second_measurements_with_day = round(100 * n / sum(n), 2)
  ) %>%
  arrange(day_b)

table_reviewer_measurement_day_availability <- df_reviewer_measurements %>%
  summarise(
    n_final_cohort = n(),
    n_weight_a = sum(!is.na(weight_a)),
    n_weight_a_with_day = sum(!is.na(weight_a) & !is.na(day_a)),
    n_weight_a_missing_day = sum(!is.na(weight_a) & is.na(day_a)),
    n_weight_b = sum(!is.na(weight_b)),
    n_weight_b_with_day = sum(!is.na(weight_b) & !is.na(day_b)),
    n_weight_b_missing_day = sum(!is.na(weight_b) & is.na(day_b))
  )

# ------------------------------------------------------------
# 2) Comparison of infants with one vs two postnatal measurements
# ------------------------------------------------------------
reviewer_fmt_mean_sd <- function(x, digits = 2) {
  if (sum(!is.na(x)) == 0) return(NA_character_)
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)"),
    mean(x, na.rm = TRUE),
    sd(x, na.rm = TRUE)
  )
}

reviewer_fmt_median_iqr <- function(x, digits = 2) {
  if (sum(!is.na(x)) == 0) return(NA_character_)
  sprintf(
    paste0("%.", digits, "f (%.", digits, "f)"),
    median(x, na.rm = TRUE),
    IQR(x, na.rm = TRUE)
  )
}

reviewer_fmt_n_pct <- function(x, level, digits = 1) {
  denom <- sum(!is.na(x))
  if (denom == 0) return(NA_character_)
  n_level <- sum(as.character(x) == as.character(level), na.rm = TRUE)
  sprintf(
    paste0("%d (%.", digits, "f%%)"),
    n_level,
    100 * n_level / denom
  )
}

# Rare-event formatter: retain two decimals below 10% while avoiding
# unnecessary precision for larger event rates.
reviewer_fmt_event_n_pct <- function(x, level = 1) {
  denom <- sum(!is.na(x))
  if (denom == 0) return(NA_character_)
  n_level <- sum(as.character(x) == as.character(level), na.rm = TRUE)
  pct <- 100 * n_level / denom
  digits <- if (pct < 10) 2 else 1
  sprintf(
    paste0("%d (%.", digits, "f%%)"),
    n_level,
    pct
  )
}

reviewer_safe_t_test <- function(var) {
  dat <- df_reviewer_measurements %>%
    filter(!is.na(measurement_group), !is.na(.data[[var]]))
  if (n_distinct(dat$measurement_group) < 2 || nrow(dat) == 0) return(NA_real_)
  tryCatch(
    t.test(as.formula(paste(var, "~ measurement_group")), data = dat)$p.value,
    error = function(e) NA_real_
  )
}

reviewer_safe_cat_test <- function(var) {
  dat <- df_reviewer_measurements %>%
    filter(!is.na(measurement_group), !is.na(.data[[var]]))
  if (n_distinct(dat$measurement_group) < 2 || n_distinct(dat[[var]]) < 2) return(NA_real_)
  tab <- table(dat$measurement_group, dat[[var]])
  tryCatch(
    if (any(tab < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value,
    error = function(e) NA_real_
  )
}

reviewer_safe_fisher_test <- function(var) {
  dat <- df_reviewer_measurements %>%
    filter(!is.na(measurement_group), !is.na(.data[[var]]))
  if (n_distinct(dat$measurement_group) < 2 || n_distinct(dat[[var]]) < 2) return(NA_real_)
  tab <- table(dat$measurement_group, dat[[var]])
  tryCatch(fisher.test(tab)$p.value, error = function(e) NA_real_)
}

reviewer_safe_t_test_timing <- function(var) {
  dat <- df_reviewer_measurements %>%
    filter(!is.na(timing_group), !is.na(.data[[var]]))
  if (n_distinct(dat$timing_group) < 2 || nrow(dat) == 0) return(NA_real_)
  tryCatch(
    t.test(as.formula(paste(var, "~ timing_group")), data = dat)$p.value,
    error = function(e) NA_real_
  )
}

reviewer_safe_cat_test_timing <- function(var) {
  dat <- df_reviewer_measurements %>%
    filter(!is.na(timing_group), !is.na(.data[[var]]))
  if (n_distinct(dat$timing_group) < 2 || n_distinct(dat[[var]]) < 2) return(NA_real_)
  tab <- table(dat$timing_group, dat[[var]])
  tryCatch(
    if (any(tab < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value,
    error = function(e) NA_real_
  )
}

reviewer_safe_fisher_test_timing <- function(var) {
  dat <- df_reviewer_measurements %>%
    filter(!is.na(timing_group), !is.na(.data[[var]]))
  if (n_distinct(dat$timing_group) < 2 || n_distinct(dat[[var]]) < 2) return(NA_real_)
  tab <- table(dat$timing_group, dat[[var]])
  tryCatch(fisher.test(tab)$p.value, error = function(e) NA_real_)
}

reviewer_continuous_row <- function(var, label, digits = 2) {
  one <- df_reviewer_measurements %>%
    filter(measurement_group == "One measurement") %>%
    pull(.data[[var]])
  two <- df_reviewer_measurements %>%
    filter(measurement_group == "Two measurements") %>%
    pull(.data[[var]])

  tibble(
    Characteristic = label,
    `One measurement` = reviewer_fmt_mean_sd(one, digits),
    `Two measurements` = reviewer_fmt_mean_sd(two, digits),
    Test = "t-test",
    p.value = format_p_value(reviewer_safe_t_test(var))
  )
}

reviewer_binary_event_row <- function(var, label, event = 1) {
  one <- df_reviewer_measurements %>%
    filter(measurement_group == "One measurement") %>%
    pull(.data[[var]])
  two <- df_reviewer_measurements %>%
    filter(measurement_group == "Two measurements") %>%
    pull(.data[[var]])

  tibble(
    Characteristic = label,
    `One measurement` = reviewer_fmt_event_n_pct(one, event),
    `Two measurements` = reviewer_fmt_event_n_pct(two, event),
    Test = "Fisher's exact",
    p.value = format_p_value(reviewer_safe_fisher_test(var))
  )
}

reviewer_categorical_rows <- function(var, label) {
  p_cat <- reviewer_safe_cat_test(var)
  observed_levels <- df_reviewer_measurements %>%
    filter(!is.na(.data[[var]])) %>%
    pull(.data[[var]]) %>%
    as.character() %>%
    unique()

  if (length(observed_levels) == 0) return(tibble())

  bind_rows(lapply(seq_along(observed_levels), function(i) {
    level_i <- observed_levels[i]
    one <- df_reviewer_measurements %>%
      filter(measurement_group == "One measurement") %>%
      pull(.data[[var]])
    two <- df_reviewer_measurements %>%
      filter(measurement_group == "Two measurements") %>%
      pull(.data[[var]])

    tibble(
      Characteristic = paste0(label, ": ", level_i),
      `One measurement` = reviewer_fmt_n_pct(one, level_i),
      `Two measurements` = reviewer_fmt_n_pct(two, level_i),
      Test = if (i == 1) "Chi-square/Fisher" else NA_character_,
      p.value = if (i == 1) format_p_value(p_cat) else NA_character_
    )
  }))
}

maternal_health_reviewer_rows <- if ("etat_general_cat" %in% names(df_reviewer_measurements)) {
  reviewer_categorical_rows("etat_general_cat", "Maternal health status")
} else {
  tibble()
}

table_reviewer_one_vs_two_characteristics <- bind_rows(
  tibble(
    Characteristic = "N",
    `One measurement` = as.character(sum(df_reviewer_measurements$measurement_group == "One measurement", na.rm = TRUE)),
    `Two measurements` = as.character(sum(df_reviewer_measurements$measurement_group == "Two measurements", na.rm = TRUE)),
    Test = NA_character_,
    p.value = NA_character_
  ),
  reviewer_continuous_row("age_mother", "Maternal age, years", 2),
  reviewer_continuous_row("parity", "Parity", 2),
  maternal_health_reviewer_rows,
  reviewer_continuous_row("ga_model", "Gestational age, weeks", 2),
  reviewer_continuous_row("birthweight", "Birthweight, g", 0),
  reviewer_categorical_rows("ptb_group", "Preterm birth status"),
  reviewer_categorical_rows("lbw_group", "Birthweight status"),
  reviewer_binary_event_row(
    "neonatal_death_28",
    "Neonatal mortality within 28 days",
    event = 1
  ),
  reviewer_categorical_rows("sex_cat", "Neonatal sex"),
  reviewer_categorical_rows("feeding_cat", "Feeding mode")
)

# Diagnostic outcome comparison. This is kept separate from baseline
# characteristics because number of measurements can itself influence capture
# of the observed nadir.
table_reviewer_one_vs_two_outcomes <- df_reviewer_measurements %>%
  filter(!is.na(measurement_group)) %>%
  group_by(measurement_group) %>%
  summarise(
    n = n(),
    mean_max_nwl_pct = round(mean(max_weight_loss_pct, na.rm = TRUE), 3),
    sd_max_nwl_pct = round(sd(max_weight_loss_pct, na.rm = TRUE), 3),
    median_max_nwl_pct = round(median(max_weight_loss_pct, na.rm = TRUE), 3),
    iqr_max_nwl_pct = round(IQR(max_weight_loss_pct, na.rm = TRUE), 3),
    high_nwl_over_10_n = sum(excessive_weight_loss_10 == 1, na.rm = TRUE),
    high_nwl_over_10_percent = round(mean(excessive_weight_loss_10 == 1, na.rm = TRUE) * 100, 2),
    .groups = "drop"
  )

# ------------------------------------------------------------
# 3) Exact day-5/day-10 timing comparison and "best data" subcohort
# ------------------------------------------------------------
reviewer_exact_timing <- df_reviewer_measurements %>%
  filter(timing_group == "Exact days 5 and 10")

reviewer_other_timing <- df_reviewer_measurements %>%
  filter(timing_group == "All other measurement patterns")

table_reviewer_exact_vs_other_characteristics <- tibble(
  Characteristic = c(
    "N",
    "Maternal age, years, mean (SD)",
    "Parity, mean (SD)",
    "Gestational age, weeks, mean (SD)",
    "Birthweight, g, mean (SD)",
    "Preterm birth (<37 weeks), n (%)",
    "Low birthweight (<2500 g), n (%)",
    "Neonatal mortality within 28 days, n (%)",
    "Female sex, n (%)",
    "Male sex, n (%)",
    "Artificial feeding, n (%)",
    "Breastfeeding, n (%)",
    "Mixed feeding, n (%)",
    "Other feeding, n (%)"
  ),
  `Exact days 5 and 10` = c(
    as.character(nrow(reviewer_exact_timing)),
    reviewer_fmt_mean_sd(reviewer_exact_timing$age_mother, 2),
    reviewer_fmt_mean_sd(reviewer_exact_timing$parity, 2),
    reviewer_fmt_mean_sd(reviewer_exact_timing$ga_model, 2),
    reviewer_fmt_mean_sd(reviewer_exact_timing$birthweight, 0),
    reviewer_fmt_n_pct(reviewer_exact_timing$ptb_group, "preterm"),
    reviewer_fmt_n_pct(reviewer_exact_timing$lbw_group, "low_birthweight"),
    reviewer_fmt_event_n_pct(reviewer_exact_timing$neonatal_death_28, 1),
    reviewer_fmt_n_pct(reviewer_exact_timing$sex_cat, "female"),
    reviewer_fmt_n_pct(reviewer_exact_timing$sex_cat, "male"),
    reviewer_fmt_n_pct(reviewer_exact_timing$feeding_cat, "artificial"),
    reviewer_fmt_n_pct(reviewer_exact_timing$feeding_cat, "breastfeeding"),
    reviewer_fmt_n_pct(reviewer_exact_timing$feeding_cat, "mixed"),
    reviewer_fmt_n_pct(reviewer_exact_timing$feeding_cat, "other")
  ),
  `All other measurement patterns` = c(
    as.character(nrow(reviewer_other_timing)),
    reviewer_fmt_mean_sd(reviewer_other_timing$age_mother, 2),
    reviewer_fmt_mean_sd(reviewer_other_timing$parity, 2),
    reviewer_fmt_mean_sd(reviewer_other_timing$ga_model, 2),
    reviewer_fmt_mean_sd(reviewer_other_timing$birthweight, 0),
    reviewer_fmt_n_pct(reviewer_other_timing$ptb_group, "preterm"),
    reviewer_fmt_n_pct(reviewer_other_timing$lbw_group, "low_birthweight"),
    reviewer_fmt_event_n_pct(reviewer_other_timing$neonatal_death_28, 1),
    reviewer_fmt_n_pct(reviewer_other_timing$sex_cat, "female"),
    reviewer_fmt_n_pct(reviewer_other_timing$sex_cat, "male"),
    reviewer_fmt_n_pct(reviewer_other_timing$feeding_cat, "artificial"),
    reviewer_fmt_n_pct(reviewer_other_timing$feeding_cat, "breastfeeding"),
    reviewer_fmt_n_pct(reviewer_other_timing$feeding_cat, "mixed"),
    reviewer_fmt_n_pct(reviewer_other_timing$feeding_cat, "other")
  ),
  Test = c(
    NA,
    "t-test", "t-test", "t-test", "t-test",
    "Chi-square/Fisher", "Chi-square/Fisher",
    "Fisher's exact",
    "Chi-square/Fisher", "Chi-square/Fisher",
    "Chi-square/Fisher", "Chi-square/Fisher", "Chi-square/Fisher", "Chi-square/Fisher"
  ),
  p.value = c(
    NA,
    format_p_value(reviewer_safe_t_test_timing("age_mother")),
    format_p_value(reviewer_safe_t_test_timing("parity")),
    format_p_value(reviewer_safe_t_test_timing("ga_model")),
    format_p_value(reviewer_safe_t_test_timing("birthweight")),
    format_p_value(reviewer_safe_cat_test_timing("ptb_group")),
    format_p_value(reviewer_safe_cat_test_timing("lbw_group")),
    format_p_value(reviewer_safe_fisher_test_timing("neonatal_death_28")),
    format_p_value(reviewer_safe_cat_test_timing("sex_cat")),
    format_p_value(reviewer_safe_cat_test_timing("sex_cat")),
    format_p_value(reviewer_safe_cat_test_timing("feeding_cat")),
    format_p_value(reviewer_safe_cat_test_timing("feeding_cat")),
    format_p_value(reviewer_safe_cat_test_timing("feeding_cat")),
    format_p_value(reviewer_safe_cat_test_timing("feeding_cat"))
  )
)

df_reviewer_exact_day5_day10 <- df_reviewer_measurements %>%
  filter(exact_day5_day10) %>%
  droplevels()

n_reviewer_exact_day5_day10 <- nrow(df_reviewer_exact_day5_day10)

if (n_reviewer_exact_day5_day10 == 0) {
  stop("No infants met the exact day-5/day-10 sensitivity definition.")
}

# ------------------------------------------------------------
# 4) Repeat only the two primary GA-adjusted models
# ------------------------------------------------------------
# Reuse the exact formulas from the established main models. This ensures that
# the sensitivity analysis changes only the analytic population, not the model.
model_reviewer_exact_day5_day10_linear_GA <- lm(
  formula(model_MX_linear_period_GA),
  data = df_reviewer_exact_day5_day10
)

model_reviewer_exact_day5_day10_logistic_GA <- glm(
  formula(model_MX_logistic_period_GA),
  data = df_reviewer_exact_day5_day10,
  family = binomial()
)

table_reviewer_exact_day5_day10_linear <- tidy_linear(
  model_reviewer_exact_day5_day10_linear_GA
)

table_reviewer_exact_day5_day10_logistic <- tidy_logistic(
  model_reviewer_exact_day5_day10_logistic_GA
)

table_reviewer_exact_day5_day10_sample <- tibble(
  sample = c(
    "Final analytical cohort",
    "Exact day 5 and day 10 subcohort",
    "Linear model complete cases",
    "Logistic model complete cases"
  ),
  n = c(
    n_reviewer_total,
    n_reviewer_exact_day5_day10,
    stats::nobs(model_reviewer_exact_day5_day10_linear_GA),
    stats::nobs(model_reviewer_exact_day5_day10_logistic_GA)
  )
) %>%
  mutate(
    percent_of_final_cohort = round(100 * n / n_reviewer_total, 2)
  )

# Side-by-side comparison with the established main models.
table_reviewer_linear_main_vs_exact <- table_MX_linear_period_GA %>%
  transmute(
    term,
    main_estimate = estimate,
    main_ci_low = conf.low,
    main_ci_high = conf.high,
    main_p_value = p.value
  ) %>%
  full_join(
    table_reviewer_exact_day5_day10_linear %>%
      transmute(
        term,
        exact_day5_day10_estimate = estimate,
        exact_day5_day10_ci_low = conf.low,
        exact_day5_day10_ci_high = conf.high,
        exact_day5_day10_p_value = p.value
      ),
    by = "term"
  )

table_reviewer_logistic_main_vs_exact <- table_MX_logistic_period_GA %>%
  transmute(
    term,
    main_odds_ratio = odds_ratio,
    main_ci_low = ci_low,
    main_ci_high = ci_high,
    main_p_value = p.value
  ) %>%
  full_join(
    table_reviewer_exact_day5_day10_logistic %>%
      transmute(
        term,
        exact_day5_day10_odds_ratio = odds_ratio,
        exact_day5_day10_ci_low = ci_low,
        exact_day5_day10_ci_high = ci_high,
        exact_day5_day10_p_value = p.value
      ),
    by = "term"
  )

# ------------------------------------------------------------
# Reviewer-specific outputs
# ------------------------------------------------------------
# Keep these separate from the established manuscript outputs so changes to the
# reviewer analyses do not silently overwrite the pre-reviewer baseline.
reviewer_output_dir <- file.path("outputs", "reviewer_revision")
dir.create(reviewer_output_dir, recursive = TRUE, showWarnings = FALSE)

write_xlsx(
  list(
    Measurement_completeness = round_numeric_df(table_reviewer_measurement_completeness),
    Measurement_pattern = round_numeric_df(table_reviewer_measurement_pattern),
    Day_A_distribution = round_numeric_df(table_reviewer_day_a_distribution),
    Day_B_distribution = round_numeric_df(table_reviewer_day_b_distribution),
    Day_availability = round_numeric_df(table_reviewer_measurement_day_availability),
    One_vs_two_characteristics = round_numeric_df(table_reviewer_one_vs_two_characteristics),
    One_vs_two_outcomes = round_numeric_df(table_reviewer_one_vs_two_outcomes),
    Exact_vs_other_characteristics = round_numeric_df(table_reviewer_exact_vs_other_characteristics),
    Exact_day5_day10_sample = round_numeric_df(table_reviewer_exact_day5_day10_sample),
    Exact_day5_day10_linear = round_numeric_df(table_reviewer_exact_day5_day10_linear),
    Exact_day5_day10_logistic = round_numeric_df(table_reviewer_exact_day5_day10_logistic),
    Linear_main_vs_exact = round_numeric_df(table_reviewer_linear_main_vs_exact),
    Logistic_main_vs_exact = round_numeric_df(table_reviewer_logistic_main_vs_exact)
  ),
  path = file.path(reviewer_output_dir, "AJHB_reviewer_revision_analyses.xlsx")
)

capture.output(
  summary(model_reviewer_exact_day5_day10_linear_GA),
  file = file.path(reviewer_output_dir, "Exact_day5_day10_linear_GA_summary.txt")
)

capture.output(
  summary(model_reviewer_exact_day5_day10_logistic_GA),
  file = file.path(reviewer_output_dir, "Exact_day5_day10_logistic_GA_summary.txt")
)

# Console summary for immediate review after source("00_master_run_all.R").
message("AJHB reviewer revision analyses completed.")
message(
  "Exact day-5/day-10 subcohort: n = ", n_reviewer_exact_day5_day10,
  " (", round(100 * n_reviewer_exact_day5_day10 / n_reviewer_total, 2), "% of final cohort)."
)
print(table_reviewer_measurement_completeness)
print(table_reviewer_measurement_pattern)
print(table_reviewer_exact_day5_day10_sample)
