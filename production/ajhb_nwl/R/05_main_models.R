# ============================================================
# 05_main_models.R
# Main linear and logistic regression models
# ============================================================
# - Historical-period exposure and feeding mode are analysed simultaneously.
# - No-period models were removed from the active analysis set.
# - GA-adjusted period models are the main models.
# - Birthweight-adjusted period models are retained as supplementary analyses because BW is part of the NWL formula.
# - Estimates should be reported with 95% confidence intervals.

# ------------------------------------------------------------
# Helper for tidy model output
# ------------------------------------------------------------
tidy_linear <- function(model) {
  tidy(model, conf.int = TRUE) %>%
    mutate(
      estimate = round(estimate, 3),
      conf.low = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value = format_p_value(p.value)
    )
}

tidy_logistic <- function(model) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) %>%
    mutate(
      estimate = round(estimate, 3),
      conf.low = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value = format_p_value(p.value)
    ) %>%
    rename(odds_ratio = estimate, ci_low = conf.low, ci_high = conf.high)
}

# ------------------------------------------------------------
# Period-adjusted linear regression models
# Outcome: maximum neonatal weight loss (%)
# ------------------------------------------------------------

#Feedback Mathilde: Main model, adjusted for gestational age.
model_MX_linear_period_GA <- lm(
  max_weight_loss_pct ~ pregn_during_WW1 + pregn_during_pandemic + feeding_cat +
    ga_model + sex_cat + parity + age_mother,
  data = df_analysis_clean
)

table_MX_linear_period_GA <- tidy_linear(model_MX_linear_period_GA)

#Feedback Mathilde: Supplementary model, adjusted for birthweight.
model_MX_linear_period_BW <- lm(
  max_weight_loss_pct ~ pregn_during_WW1 + pregn_during_pandemic + feeding_cat +
    birthweight_100g + sex_cat + parity + age_mother,
  data = df_analysis_clean
)

table_MX_linear_period_BW <- tidy_linear(model_MX_linear_period_BW)

# ------------------------------------------------------------
# Period-adjusted logistic regression models
# Outcome: excessive neonatal weight loss >10%
# ------------------------------------------------------------

#Feedback Mathilde: Main model, adjusted for gestational age.
model_MX_logistic_period_GA <- glm(
  excessive_weight_loss_10 ~ pregn_during_WW1 + pregn_during_pandemic + feeding_cat +
    ga_model + sex_cat + parity + age_mother,
  data = df_analysis_clean,
  family = binomial()
)

table_MX_logistic_period_GA <- tidy_logistic(model_MX_logistic_period_GA)

#Feedback Mathilde: Supplementary model, adjusted for birthweight.
model_MX_logistic_period_BW <- glm(
  excessive_weight_loss_10 ~ pregn_during_WW1 + pregn_during_pandemic + feeding_cat +
    birthweight_100g + sex_cat + parity + age_mother,
  data = df_analysis_clean,
  family = binomial()
)

table_MX_logistic_period_BW <- tidy_logistic(model_MX_logistic_period_BW)

# ------------------------------------------------------------
# Manuscript tables and appendix aliases
# ------------------------------------------------------------

#Feedback Mathilde: Main manuscript tables now use the GA-adjusted period models.
table_4_linear_regression <- table_MX_linear_period_GA
table_5_logistic_regression <- table_MX_logistic_period_GA

#Feedback Mathilde: Birthweight-adjusted models are retained only as supplementary analyses.
appendix_table_linear_model_BW_supplementary <- table_MX_linear_period_BW
appendix_table_logistic_model_BW_supplementary <- table_MX_logistic_period_BW


#Feedback06Mathilde: Backward-compatible aliases for older scripts/objects.
# These aliases now point to the updated period models only.
model_M3_linear_period_GA <- model_MX_linear_period_GA
table_M3_linear_period_GA <- table_MX_linear_period_GA
model_M4_linear_period_BW <- model_MX_linear_period_BW
table_M4_linear_period_BW <- table_MX_linear_period_BW
model_M7_logistic_period_GA <- model_MX_logistic_period_GA
table_M7_logistic_period_GA <- table_MX_logistic_period_GA
model_M8_logistic_period_BW <- model_MX_logistic_period_BW
table_M8_logistic_period_BW <- table_MX_logistic_period_BW

# Backward-compatible general names.
model_linear_period <- model_MX_linear_period_GA
model_logistic_period <- model_MX_logistic_period_GA

# ------------------------------------------------------------
# Feeding-term comparison: main GA model vs BW supplementary model
# ------------------------------------------------------------
extract_feeding_terms <- function(model, model_id, model_type, exponentiate = FALSE) {
  tidy(model, conf.int = TRUE, exponentiate = exponentiate) %>%
    filter(str_detect(term, "^feeding_cat")) %>%
    mutate(
      model_id = model_id,
      model_type = model_type,
      effect_scale = ifelse(exponentiate, "odds ratio", "beta coefficient"),
      estimate = round(estimate, 3),
      conf.low = round(conf.low, 3),
      conf.high = round(conf.high, 3),
      p.value = format_p_value(p.value)
    ) %>%
    select(model_id, model_type, effect_scale, term, estimate, conf.low, conf.high, p.value)
}

#Feedback Mathilde: No-period models removed; comparison limited to main GA and supplementary BW models.
table_main_sensitivity_feeding_comparison <- bind_rows(
  extract_feeding_terms(model_MX_linear_period_GA, "Main_linear_GA", "Linear, WWI+pandemic exposures, GA-adjusted", FALSE),
  extract_feeding_terms(model_MX_linear_period_BW, "Supplementary_linear_BW", "Linear, WWI+pandemic exposures, birthweight-adjusted", FALSE),
  extract_feeding_terms(model_MX_logistic_period_GA, "Main_logistic_GA", "Logistic, WWI+pandemic exposures, GA-adjusted", TRUE),
  extract_feeding_terms(model_MX_logistic_period_BW, "Supplementary_logistic_BW", "Logistic, WWI+pandemic exposures, birthweight-adjusted", TRUE)
)

#Feedback06Mathilde: Backward-compatible export name; content no longer contains M1-M8 no-period models.
table_M1_to_M8_feeding_comparison <- table_main_sensitivity_feeding_comparison
