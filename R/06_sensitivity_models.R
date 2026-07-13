# ============================================================
# 06_sensitivity_models.R
# Sensitivity analyses, feeding allocation comparisons,
# and stratified breastfeeding analyses
# ============================================================
# - Healthy-subset sensitivity analysis removed.
# - GA-adjusted period model is the main model.
# - BW-adjusted period model is retained as supplementary analysis.
# - Multivariable feeding-allocation model replaced by simple group comparisons.
# - STR6 GA-median stratification removed and replaced by sex stratification.
# - Appendix Table S4 expanded with maternal characteristics and neonatal outcomes.

# ------------------------------------------------------------
# 1) Helper functions for feeding-mode comparison tables
# ------------------------------------------------------------
# These helpers create descriptive feeding-allocation tables
# instead of a multivariable model, reducing reverse-causality overinterpretation.

first_existing_col <- function(data, candidates) {
  found <- candidates[candidates %in% names(data)]
  if (length(found) == 0) return(NA_character_)
  found[1]
}

format_mean_sd <- function(x) {
  if (all(is.na(x))) return(NA_character_)
  paste0(round(mean(x, na.rm = TRUE), 2), " (", round(sd(x, na.rm = TRUE), 2), ")")
}

format_n_percent <- function(x, level) {
  denom <- sum(!is.na(x))
  if (denom == 0) return(NA_character_)
  n_level <- sum(x == level, na.rm = TRUE)
  paste0(n_level, " (", round(n_level / denom * 100, 1), ")")
}

safe_p_continuous <- function(data, var) {
  test_data <- data %>% filter(!is.na(feeding_cat), !is.na(.data[[var]]))
  if (n_distinct(test_data$feeding_cat) < 2 || nrow(test_data) == 0) return(NA_real_)
  tryCatch(
    kruskal.test(as.formula(paste(var, "~ feeding_cat")), data = test_data)$p.value,
    error = function(e) NA_real_
  )
}

safe_p_categorical <- function(data, var) {
  test_data <- data %>% filter(!is.na(feeding_cat), !is.na(.data[[var]]))
  if (n_distinct(test_data$feeding_cat) < 2 || n_distinct(test_data[[var]]) < 2) return(NA_real_)
  tab <- table(test_data$feeding_cat, test_data[[var]])
  tryCatch(
    if (any(tab < 5)) fisher.test(tab)$p.value else chisq.test(tab)$p.value,
    error = function(e) NA_real_
  )
}

continuous_feeding_rows <- function(data, var, label) {
  if (is.na(var) || !var %in% names(data)) return(tibble())
  data %>%
    filter(!is.na(feeding_cat)) %>%
    group_by(feeding_cat) %>%
    summarise(value = format_mean_sd(.data[[var]]), .groups = "drop") %>%
    mutate(feeding_cat = as.character(feeding_cat)) %>%
    tidyr::pivot_wider(names_from = feeding_cat, values_from = value) %>%
    mutate(
      variable = label,
      level = "mean (SD)",
      test = "Kruskal-Wallis",
      p.value = format_p_value(safe_p_continuous(data, var))
    ) %>%
    select(variable, level, any_of(c("artificial", "breastfeeding", "mixed", "other")), test, p.value)
}

categorical_feeding_rows <- function(data, var, label) {
  if (is.na(var) || !var %in% names(data)) return(tibble())
  levels_var <- data %>%
    filter(!is.na(.data[[var]])) %>%
    pull(.data[[var]]) %>%
    as.character() %>%
    unique()
  
  if (length(levels_var) == 0) return(tibble())
  
  bind_rows(lapply(levels_var, function(current_level) {
    data %>%
      filter(!is.na(feeding_cat)) %>%
      group_by(feeding_cat) %>%
      summarise(value = format_n_percent(as.character(.data[[var]]), current_level), .groups = "drop") %>%
      mutate(feeding_cat = as.character(feeding_cat)) %>%
      tidyr::pivot_wider(names_from = feeding_cat, values_from = value) %>%
      mutate(
        variable = label,
        level = current_level,
        test = "Chi-square/Fisher",
        p.value = format_p_value(safe_p_categorical(data, var))
      ) %>%
      select(variable, level, any_of(c("artificial", "breastfeeding", "mixed", "other")), test, p.value)
  }))
}

# ------------------------------------------------------------
# 2) Table 6 / Appendix S4: feeding allocation by feeding mode
# ------------------------------------------------------------

#Feedback06Mathilde:  
maternal_health_col <- first_existing_col(
  df_analysis_clean,
  c("etat_general_cat")
)





#Feedback06Mathilde: Expanded descriptive table according to feeding mode.
appendix_table_s4_vulnerability_by_feeding <- bind_rows(
  continuous_feeding_rows(df_analysis_clean, "age_mother", "Maternal age"),
  continuous_feeding_rows(df_analysis_clean, "parity", "Parity"),
  categorical_feeding_rows(df_analysis_clean, maternal_health_col, "Maternal health status"),
  continuous_feeding_rows(df_analysis_clean, "ga_model", "Gestational age"),
  continuous_feeding_rows(df_analysis_clean, "birthweight", "Birthweight"),
  categorical_feeding_rows(df_analysis_clean, "lbw_group", "Low birthweight"),
  categorical_feeding_rows(df_analysis_clean, "ptb_group", "Preterm birth"),
  categorical_feeding_rows(df_analysis_clean, "sex_cat", "Neonatal sex"),
  categorical_feeding_rows(df_analysis_clean, "grippe_during_pregnancy", "Maternal influenza during pregnancy"),
  categorical_feeding_rows(df_analysis_clean, "flu_in_pregn_and_pandemic", "Influenza in pregnancy during pandemic")
)

#Table 6 is now a descriptive comparison table, not a regression model.
table_6_feeding_allocation <- appendix_table_s4_vulnerability_by_feeding

# ------------------------------------------------------------
# 3) Supplementary analyses: GA main model vs BW-adjusted model
# ------------------------------------------------------------

#Feedback Mathilde: The birthweight-adjusted model is retained only as supplementary analysis.
appendix_table_sensitivity_BW_linear <- table_MX_linear_period_BW
appendix_table_sensitivity_BW_logistic <- table_MX_logistic_period_BW


#Feedback Mathilde: Figure/table comparison uses the main GA model and supplementary BW model.
comparison_feeding_effects <- bind_rows(
  tidy(model_MX_linear_period_GA, conf.int = TRUE) %>%
    filter(str_detect(term, "feeding_cat")) %>%
    mutate(model = "Main model: GA-adjusted"),
  tidy(model_MX_linear_period_BW, conf.int = TRUE) %>%
    filter(str_detect(term, "feeding_cat")) %>%
    mutate(model = "Supplementary model: birthweight-adjusted")
) %>%
  mutate(
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    p.value = format_p_value(p.value)
  ) %>%
  select(model, term, estimate, conf.low, conf.high, p.value)

appendix_table_s5_sensitivity_comparison <- comparison_feeding_effects

#Feedback06Mathilde: Backward-compatible object names; healthy-subset analysis removed.
appendix_table_s2_healthy_subset <- tibble(note = "Removed after Feedback06Mathilde: healthy-subset sensitivity analysis replaced by birthweight-adjusted supplementary model.")
appendix_table_s3_healthy_regression <- appendix_table_sensitivity_BW_linear
model_weightloss_adjusted_vulnerability <- model_MX_linear_period_GA
appendix_table_weightloss_adjusted_vulnerability <- table_MX_linear_period_GA

# ------------------------------------------------------------
# 4) Additional direct influenza sensitivity analyses
# ------------------------------------------------------------
# Retained as optional appendix analyses, adjusted with GA to match the main model strategy.

model_S3_direct_flu_GA <- lm(
  max_weight_loss_pct ~ feeding_cat + grippe_during_pregnancy + ga_model +
    sex_cat + parity + age_mother,
  data = df_analysis_clean
)

appendix_table_S3_direct_flu_GA <- tidy(model_S3_direct_flu_GA, conf.int = TRUE) %>%
  mutate(
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    p.value = format_p_value(p.value)
  )

model_S4_flu_in_pregn_and_pandemic_GA <- lm(
  max_weight_loss_pct ~ feeding_cat + flu_in_pregn_and_pandemic + ga_model +
    sex_cat + parity + age_mother,
  data = df_analysis_clean
)

appendix_table_S4_flu_in_pregn_and_pandemic_GA <- tidy(model_S4_flu_in_pregn_and_pandemic_GA, conf.int = TRUE) %>%
  mutate(
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    p.value = format_p_value(p.value)
  )

# Optional BW-adjusted versions retained only as supplementary/diagnostic objects.
model_S3_direct_flu_BW <- lm(
  max_weight_loss_pct ~ feeding_cat + grippe_during_pregnancy + birthweight_100g +
    sex_cat + parity + age_mother,
  data = df_analysis_clean
)

appendix_table_S3_direct_flu_BW <- tidy(model_S3_direct_flu_BW, conf.int = TRUE) %>%
  mutate(
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    p.value = format_p_value(p.value)
  )

model_S4_flu_in_pregn_and_pandemic_BW <- lm(
  max_weight_loss_pct ~ feeding_cat + flu_in_pregn_and_pandemic + birthweight_100g +
    sex_cat + parity + age_mother,
  data = df_analysis_clean
)

appendix_table_S4_flu_in_pregn_and_pandemic_BW <- tidy(model_S4_flu_in_pregn_and_pandemic_BW, conf.int = TRUE) %>%
  mutate(
    estimate = round(estimate, 3),
    conf.low = round(conf.low, 3),
    conf.high = round(conf.high, 3),
    p.value = format_p_value(p.value)
  )

# Backward-compatible names used in export scripts.
model_S1_vulnerability_GA <- model_MX_linear_period_GA
appendix_table_S1_vulnerability_GA <- table_MX_linear_period_GA
model_S2_vulnerability_BW <- model_MX_linear_period_BW
appendix_table_S2_vulnerability_BW <- table_MX_linear_period_BW

# ------------------------------------------------------------
# 5) Stratified breastfeeding analyses for Mathilde/Noémie Excel sheet
# ------------------------------------------------------------
#STR6 GA-median removed and replaced by sex stratification.

safe_glm <- function(formula, data) {
  tryCatch(
    glm(formula, data = data, family = binomial()),
    error = function(e) NULL
  )
}

safe_lm <- function(formula, data) {
  tryCatch(
    lm(formula, data = data),
    error = function(e) NULL
  )
}

extract_breastfeeding_or <- function(model, analysis, stratum) {
  # Always return one row. If the breastfeeding contrast
  # cannot be estimated, return NA values rather than dropping the stratum.
  empty_row <- tibble(
    analysis = analysis, stratum = stratum, term = "feeding_catbreastfeeding",
    OR = NA_real_, CI_low = NA_real_, CI_high = NA_real_, p_value = NA_real_
  )
  
  if (is.null(model)) return(empty_row)
  
  out <- tryCatch(
    tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
      filter(term == "feeding_catbreastfeeding") %>%
      transmute(
        analysis = analysis,
        stratum = stratum,
        term = term,
        OR = round(estimate, 3),
        CI_low = round(conf.low, 3),
        CI_high = round(conf.high, 3),
        log_OR = estimate,
        SE_log_OR = std.error,
        p_value = p.value
      ),
    error = function(e) empty_row
  )
  
  if (nrow(out) == 0) empty_row else out
}

extract_breastfeeding_beta <- function(model, analysis, stratum) {
  # Always return one row. If the breastfeeding contrast
  # cannot be estimated, return NA values rather than dropping the stratum.
  empty_row <- tibble(
    analysis = analysis, stratum = stratum, term = "feeding_catbreastfeeding",
    beta = NA_real_, CI_low = NA_real_, CI_high = NA_real_, p_value = NA_real_
  )
  
  if (is.null(model)) return(empty_row)
  
  out <- tryCatch(
    tidy(model, conf.int = TRUE) %>%
      filter(term == "feeding_catbreastfeeding") %>%
      transmute(
        analysis = analysis,
        stratum = stratum,
        term = term,
        beta = round(estimate, 3),
        CI_low = round(conf.low, 3),
        CI_high = round(conf.high, 3),
        p_value = p.value
      ),
    error = function(e) empty_row
  )
  
  if (nrow(out) == 0) empty_row else out
}

# Helper to build robust model formulas for stratified analyses.
# Removes the stratification variable itself and variables with only one observed
# level inside the analysed subset. This prevents empty results for WWI/pandemic
# strata caused by single-level factor covariates within a stratum.
build_stratified_formula <- function(data, outcome, strat_var = NULL, include_strat_var = FALSE) {
  confounders <- c(
    "ga_model",
    "parity",
    "age_mother",
    "sex_cat",
    "pregn_during_WW1",
    "pregn_during_pandemic"
  )
  
  #Do not adjust for the stratification variable inside its own strata.
  if (!is.null(strat_var) && !include_strat_var) {
    confounders <- setdiff(confounders, strat_var)
  }
  
  #Keep only covariates that exist and are usable in this subset.
  confounders <- confounders[confounders %in% names(data)]
  usable_confounders <- confounders[vapply(confounders, function(v) {
    x <- data[[v]]
    if (all(is.na(x))) return(FALSE)
    if (is.factor(x) || is.character(x) || is.logical(x)) {
      return(dplyr::n_distinct(x[!is.na(x)]) >= 2)
    }
    return(sum(!is.na(x)) > 1 && stats::sd(x, na.rm = TRUE) > 0)
  }, logical(1))]
  
  rhs_terms <- c("feeding_cat", usable_confounders)
  if (!is.null(strat_var) && include_strat_var) {
    rhs_terms <- c("feeding_cat", strat_var, usable_confounders)
  }
  rhs_terms <- unique(rhs_terms)
  
  as.formula(paste(outcome, "~", paste(rhs_terms, collapse = " + ")))
}

# Helper to confirm that a model can estimate the breastfeeding
# contrast. It requires outcome variation and both artificial and breastfeeding groups.
has_required_model_variation <- function(data, outcome) {
  if (!outcome %in% names(data) || !"feeding_cat" %in% names(data)) return(FALSE)
  test_data <- data %>%
    filter(!is.na(.data[[outcome]]), !is.na(feeding_cat))
  if (nrow(test_data) == 0) return(FALSE)
  feeding_levels <- unique(as.character(test_data$feeding_cat))
  if (!all(c("artificial", "breastfeeding") %in% feeding_levels)) return(FALSE)
  if (dplyr::n_distinct(test_data[[outcome]]) < 2) return(FALSE)
  TRUE
}

run_stratified_breastfeeding_logistic <- function(data, strat_var, analysis_label) {
  #Main stratified models use GA-adjustment to match the main model strategy.
  #Robust refit. Removes the current stratification variable
  #and any single-level covariates within each stratum to avoid missing estimates.
  
  outcome <- "excessive_weight_loss_10"
  formula_all <- build_stratified_formula(data, outcome, strat_var = strat_var, include_strat_var = FALSE)
  model_all <- if (has_required_model_variation(data, outcome)) safe_glm(formula_all, data) else NULL
  out <- extract_breastfeeding_or(model_all, analysis_label, "all_births") %>%
    mutate(model_formula = paste(deparse(formula_all), collapse = " "))
  
  strata <- data %>%
    filter(!is.na(.data[[strat_var]])) %>%
    pull(.data[[strat_var]]) %>%
    as.character() %>%
    unique()
  
  for (s in strata) {
    data_s <- data %>% filter(as.character(.data[[strat_var]]) == s)
    formula_s <- build_stratified_formula(data_s, outcome, strat_var = strat_var, include_strat_var = FALSE)
    model_s <- if (has_required_model_variation(data_s, outcome)) safe_glm(formula_s, data_s) else NULL
    out <- bind_rows(
      out,
      extract_breastfeeding_or(model_s, analysis_label, s) %>%
        mutate(model_formula = paste(deparse(formula_s), collapse = " "))
    )
  }
  
  out %>%
    mutate(
      excel_note = "For Mathilde/Noemie sheet: enter OR and upper CI for breastfeeding vs artificial. Missing values indicate insufficient variation/cases in that stratum."
    )
}

run_stratified_breastfeeding_linear <- function(data, strat_var, analysis_label) {
  #Main stratified models use GA-adjustment to match the main model strategy.
  #Robust refit. Removes the current stratification variable
  # and any single-level covariates within each stratum to avoid missing estimates.
  
  outcome <- "max_weight_loss_pct"
  formula_all <- build_stratified_formula(data, outcome, strat_var = strat_var, include_strat_var = FALSE)
  model_all <- if (has_required_model_variation(data, outcome)) safe_lm(formula_all, data) else NULL
  out <- extract_breastfeeding_beta(model_all, analysis_label, "all_births") %>%
    mutate(model_formula = paste(deparse(formula_all), collapse = " "))
  
  strata <- data %>%
    filter(!is.na(.data[[strat_var]])) %>%
    pull(.data[[strat_var]]) %>%
    as.character() %>%
    unique()
  
  for (s in strata) {
    data_s <- data %>% filter(as.character(.data[[strat_var]]) == s)
    formula_s <- build_stratified_formula(data_s, outcome, strat_var = strat_var, include_strat_var = FALSE)
    model_s <- if (has_required_model_variation(data_s, outcome)) safe_lm(formula_s, data_s) else NULL
    out <- bind_rows(
      out,
      extract_breastfeeding_beta(model_s, analysis_label, s) %>%
        mutate(model_formula = paste(deparse(formula_s), collapse = " "))
    )
  }
  
  out
}

#STR6_GA_below_vs_above_median removed; STR6 is now sex.
table_STR_logistic_excel_inputs <- bind_rows(
  run_stratified_breastfeeding_logistic(df_analysis_clean, "ptb_group", "STR1_PTB_vs_term"),
  run_stratified_breastfeeding_logistic(df_analysis_clean, "lbw_group", "STR2_LBW_vs_normal_BW"),
  run_stratified_breastfeeding_logistic(df_analysis_clean, "pregn_during_pandemic", "STR3_pandemic_pregnancy_yes_no"),
  run_stratified_breastfeeding_logistic(df_analysis_clean, "pregn_during_WW1", "STR4_WW1_pregnancy_yes_no"),
  run_stratified_breastfeeding_logistic(df_analysis_clean, "flu_in_pregn_and_pandemic", "STR5_direct_flu_in_pregnancy_and_pandemic"),
  run_stratified_breastfeeding_logistic(df_analysis_clean, "sex_cat", "STR6_sex_female_vs_male")
)

table_STR_linear_beta_inputs <- bind_rows(
  run_stratified_breastfeeding_linear(df_analysis_clean, "ptb_group", "STR1_PTB_vs_term"),
  run_stratified_breastfeeding_linear(df_analysis_clean, "lbw_group", "STR2_LBW_vs_normal_BW"),
  run_stratified_breastfeeding_linear(df_analysis_clean, "pregn_during_pandemic", "STR3_pandemic_pregnancy_yes_no"),
  run_stratified_breastfeeding_linear(df_analysis_clean, "pregn_during_WW1", "STR4_WW1_pregnancy_yes_no"),
  run_stratified_breastfeeding_linear(df_analysis_clean, "flu_in_pregn_and_pandemic", "STR5_direct_flu_in_pregnancy_and_pandemic"),
  run_stratified_breastfeeding_linear(df_analysis_clean, "sex_cat", "STR6_sex_female_vs_male")
)

# ------------------------------------------------------------
# 6) Cochran-Q heterogeneity tests for subgroup analyses
# ------------------------------------------------------------
#Heterogeneity is now
# assessed with Cochran-Q tests using the stratum-specific breastfeeding ORs
# from the stratified logistic models.

format_or_ci <- function(or, low, high) {
  ifelse(
    is.na(or) | is.na(low) | is.na(high),
    NA_character_,
    sprintf("%.3f (%.3f to %.3f)", or, low, high)
  )
}

safe_cochran_q <- function(stratified_results, analysis_label) {
  
  dat_all <- stratified_results %>%
    filter(analysis == analysis_label, stratum == "all_births") %>%
    filter(!is.na(OR)) %>%
    slice(1)
  
  dat_q <- stratified_results %>%
    filter(analysis == analysis_label, stratum != "all_births") %>%
    filter(!is.na(OR), !is.na(CI_high), OR > 0, CI_high > 0)
  
  k <- nrow(dat_q)
  
  if (k < 2 || nrow(dat_all) == 0) {
    return(tibble(
      analysis = analysis_label,
      cochran_Q = NA_real_,
      df = NA_integer_,
      cochran_Q_p_value_numeric = NA_real_,
      cochran_Q_p_value = NA_character_
    ))
  }
  
  overall_log_or <- log(dat_all$OR[1])
  
  dat_q <- dat_q %>%
    mutate(
      log_or_excel = log(OR),
      se_log_or_excel = (log(CI_high) - log(OR)) / 1.96,
      q_component = ((log_or_excel - overall_log_or)^2) / (se_log_or_excel^2)
    )
  
  q_stat <- sum(dat_q$q_component, na.rm = TRUE)
  df_q <- k - 1
  p_q <- stats::pchisq(q_stat, df = df_q, lower.tail = FALSE)
  
  tibble(
    analysis = analysis_label,
    cochran_Q = round(q_stat, 3),
    df = df_q,
    cochran_Q_p_value_numeric = p_q,
    cochran_Q_p_value = format_p_value(p_q)
  )
}

subgroup_labels <- tibble::tribble(
  ~analysis, ~Subgroup,
  "STR1_PTB_vs_term", "Prematurity status",
  "STR2_LBW_vs_normal_BW", "Birthweight status",
  "STR6_sex_female_vs_male", "Neonatal sex",
  "STR4_WW1_pregnancy_yes_no", "WWI exposure",
  "STR3_pandemic_pregnancy_yes_no", "Pandemic exposure",
  "STR5_direct_flu_in_pregnancy_and_pandemic", "Maternal influenza during pandemic pregnancy"
)

table_subgroup_heterogeneity_cochran_q <- bind_rows(lapply(
  subgroup_labels$analysis,
  function(a) safe_cochran_q(table_STR_logistic_excel_inputs, a)
)) %>%
  left_join(subgroup_labels, by = "analysis") %>%
  select(Subgroup, analysis, cochran_Q, df, cochran_Q_p_value, cochran_Q_p_value_numeric)

table_STR_logistic_excel_inputs <- table_STR_logistic_excel_inputs %>%
  mutate(p_value = format_p_value(p_value))

table_STR_linear_beta_inputs <- table_STR_linear_beta_inputs %>%
  mutate(p_value = format_p_value(p_value))

table_S6_subgroup_analyses <- table_STR_logistic_excel_inputs %>%
  filter(stratum != "all_births") %>%
  left_join(subgroup_labels, by = "analysis") %>%
  left_join(
    table_subgroup_heterogeneity_cochran_q %>%
      select(analysis, cochran_Q_p_value),
    by = "analysis"
  ) %>%
  group_by(analysis) %>%
  mutate(
    `Cochran–Q p-value` = if_else(row_number() == 1L, cochran_Q_p_value, NA_character_),
    `OR (95% CI)` = format_or_ci(OR, CI_low, CI_high)
  ) %>%
  ungroup() %>%
  arrange(match(analysis, subgroup_labels$analysis), stratum) %>%
  transmute(
    Subgroup,
    Stratum = stratum,
    `OR (95% CI)`,
    `Cochran–Q p-value`
  )

# Backward-compatible alias for export scripts and manuscript naming.
Supplementary_Table_S6_Subgroup_Analyses <- table_S6_subgroup_analyses
