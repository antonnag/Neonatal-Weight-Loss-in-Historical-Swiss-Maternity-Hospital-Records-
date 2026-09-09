# ============================================================
# 03_variables_outcomes.R
# Outcomes, exclusions, feeding recoding, covariates,
# and pregnancy-based historical exposure definitions
# ============================================================

# ------------------------------------------------------------
# 1) Neonatal weight-loss outcomes
# ------------------------------------------------------------
# The historical extraction uses two postnatal weight fields. The lower of the
# two available postnatal weights is used to approximate the maximum documented
# neonatal weight loss.
df_analysis <- df_analysis %>%
  mutate(
    lowest_postnatal_weight = pmin(weight_a, weight_b, na.rm = TRUE),
    max_weight_loss_g = birthweight - lowest_postnatal_weight,
    max_weight_loss_pct = (max_weight_loss_g / birthweight) * 100
  )

# Exclude non-evaluable weight trajectories for the primary analysis.
# These are labelled as "non-evaluable", because
# they are mainly records where the available values do not allow a useful nadir estimate
# for the research question.
df_non_evaluable_weight_trajectory <- df_analysis %>%
  filter(max_weight_loss_pct < 0 | max_weight_loss_pct > 20)

n_non_evaluable_weight_trajectory <- nrow(df_non_evaluable_weight_trajectory)
df_analysis_clean <- df_analysis %>%
  filter(max_weight_loss_pct >= 0, max_weight_loss_pct <= 20)

n_final <- nrow(df_analysis_clean)

table_exclusions <- tibble(
  step = c(
    "Original historical birth records",
    "Eligible live births with birthweight and postnatal weight",
    "Excluded due to inadequate longitudinal weight data",
    "Final analytical sample"
  ),
  n = c(n_original, n_eligible, n_non_evaluable_weight_trajectory, n_final),
  percent_of_previous_step = c(
    NA,
    round(n_eligible / n_original * 100, 2),
    round(n_non_evaluable_weight_trajectory / n_eligible * 100, 2),
    round(n_final / n_eligible * 100, 2)
  )
)

summary(df_analysis_clean$max_weight_loss_pct)

# Breakdown of excluded weight trajectories
table_non_evaluable_weight_trajectory <- df_non_evaluable_weight_trajectory %>%
  mutate(
    exclusion_reason = case_when(
      max_weight_loss_pct < 0 ~ "Weight gain (<0%)",
      max_weight_loss_pct > 20 ~ "Weight loss >20%",
      TRUE ~ NA_character_
    )
  ) %>%
  count(exclusion_reason, name = "n") %>%
  mutate(percent = round(n / n_eligible * 100, 2))

table_non_evaluable_weight_trajectory

# ------------------------------------------------------------
# 2) Timing of postnatal weight measurements
# ------------------------------------------------------------
table_weight_measurement_timing <- df_analysis_clean %>%
  summarise(
    day_a_median = median(day_a, na.rm = TRUE),
    day_a_iqr = IQR(day_a, na.rm = TRUE),
    day_a_min = min(day_a, na.rm = TRUE),
    day_a_max = max(day_a, na.rm = TRUE),
    day_b_median = median(day_b, na.rm = TRUE),
    day_b_iqr = IQR(day_b, na.rm = TRUE),
    day_b_min = min(day_b, na.rm = TRUE),
    day_b_max = max(day_b, na.rm = TRUE)
  )

table_day_a <- df_analysis_clean %>%
  count(day_a, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  arrange(day_a)

table_day_b <- df_analysis_clean %>%
  count(day_b, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  arrange(day_b)

# Determine at which measurement the lowest postnatal weight occurred.
df_analysis_clean <- df_analysis_clean %>%
  mutate(
    lowest_weight_day = case_when(
      !is.na(weight_a) & !is.na(weight_b) & weight_a <= weight_b ~ day_a,
      !is.na(weight_a) & !is.na(weight_b) & weight_b < weight_a ~ day_b,
      !is.na(weight_a) & is.na(weight_b) ~ day_a,
      is.na(weight_a) & !is.na(weight_b) ~ day_b,
      TRUE ~ NA_real_
    ),
    lowest_weight_measurement = case_when(
      !is.na(weight_a) & !is.na(weight_b) & weight_a <= weight_b ~ "first_measurement",
      !is.na(weight_a) & !is.na(weight_b) & weight_b < weight_a ~ "second_measurement",
      !is.na(weight_a) & is.na(weight_b) ~ "first_measurement",
      is.na(weight_a) & !is.na(weight_b) ~ "second_measurement",
      TRUE ~ NA_character_
    )
  )

table_lowest_weight_day <- df_analysis_clean %>%
  count(lowest_weight_day, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  arrange(lowest_weight_day)

summary_lowest_weight_day <- df_analysis_clean %>%
  summarise(
    median_day = median(lowest_weight_day, na.rm = TRUE),
    iqr_day = IQR(lowest_weight_day, na.rm = TRUE),
    min_day = min(lowest_weight_day, na.rm = TRUE),
    max_day = max(lowest_weight_day, na.rm = TRUE)
  )

table_lowest_measurement <- df_analysis_clean %>%
  count(lowest_weight_measurement, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

# ------------------------------------------------------------
# 3) Feeding recoding
# ------------------------------------------------------------
feeding_original_distribution <- df_analysis_clean %>%
  count(feeding, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

df_analysis_clean <- df_analysis_clean %>%
  mutate(
    feeding_cat = case_when(
      feeding == "maternal" ~ "breastfeeding",
      feeding %in% c("mixed", "maternal then mixed", "maternal then mixed then artificial") ~ "mixed",
      feeding %in% c("artificial", "maternal then artificial") ~ "artificial",
      feeding %in% c("human milk", "other") ~ "other",
      is.na(feeding) ~ NA_character_,
      TRUE ~ NA_character_
    ),
    feeding_cat = factor(feeding_cat, levels = c("artificial", "breastfeeding", "mixed", "other"))
  )

feeding_recoded_distribution <- df_analysis_clean %>%
  count(feeding_cat, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

feeding_recoding_table <- tibble(
  original_historical_category = c(
    "maternal", "mixed", "maternal then mixed",
    "maternal then mixed then artificial", "artificial",
    "maternal then artificial", "human milk", "other"
  ),
  recoded_category = c(
    "breastfeeding", "mixed", "mixed", "mixed",
    "artificial", "artificial", "other", "other"
  )
)

# ------------------------------------------------------------
# 4) Core covariates
# ------------------------------------------------------------
if ("ga_weeks_corrected" %in% names(df_analysis_clean)) {
  df_analysis_clean <- df_analysis_clean %>%
    mutate(ga_model = ifelse(!is.na(ga_weeks_corrected), ga_weeks_corrected, ga_weeks))
} else {
  df_analysis_clean <- df_analysis_clean %>%
    mutate(ga_model = ga_weeks)
}

df_analysis_clean <- df_analysis_clean %>%
  mutate(
    birthweight_100g = birthweight / 100,
    sex_cat = factor(sex, levels = c(0, 1), labels = c("female", "male")),
    excessive_weight_loss_10 = ifelse(max_weight_loss_pct > 10, 1, 0),
    feeding_nonbf = case_when(
      feeding_cat == "breastfeeding" ~ 0,
      feeding_cat %in% c("artificial", "mixed", "other") ~ 1,
      TRUE ~ NA_real_
    )
  )

# ------------------------------------------------------------
# 5) Pregnancy-based historical exposure definitions
# ------------------------------------------------------------
ww1_start <- as.Date("1914-07-28")
ww1_end <- as.Date("1918-11-11")

pandemic_wave1_start <- as.Date("1918-07-01")
pandemic_wave1_end <- as.Date("1919-04-01")
pandemic_wave2_start <- as.Date("1920-01-01")
pandemic_wave2_end <- as.Date("1920-04-01")

df_analysis_clean <- df_analysis_clean %>%
  mutate(
    birthdate_parsed = parse_historical_date(birthdate),
    month_1_parsed = parse_historical_date(month_1),
    pregn_during_WW1 = case_when(
      is.na(month_1_parsed) | is.na(birthdate_parsed) ~ NA_character_,
      month_1_parsed < ww1_end & birthdate_parsed >= ww1_start ~ "yes",
      TRUE ~ "no"
    ),
    pregn_during_pandemic = case_when(
      is.na(month_1_parsed) | is.na(birthdate_parsed) ~ NA_character_,
      (month_1_parsed < pandemic_wave1_end & birthdate_parsed >= pandemic_wave1_start) |
        (month_1_parsed < pandemic_wave2_end & birthdate_parsed >= pandemic_wave2_start) ~ "yes",
      TRUE ~ "no"
    ),
    pregn_pre_WW1 = case_when(
      is.na(birthdate_parsed) ~ NA_character_,
      birthdate_parsed < ww1_start ~ "yes",
      TRUE ~ "no"
    ),
    pregn_post_WW1 = case_when(
      is.na(month_1_parsed) ~ NA_character_,
      month_1_parsed >= ww1_end ~ "yes",
      TRUE ~ "no"
    ),
    pregn_during_WW1 = factor(pregn_during_WW1, levels = c("no", "yes")),
    pregn_during_pandemic = factor(pregn_during_pandemic, levels = c("no", "yes")),
    pregn_pre_WW1 = factor(pregn_pre_WW1, levels = c("no", "yes")),
    pregn_post_WW1 = factor(pregn_post_WW1, levels = c("no", "yes")),
    historical_period = case_when(
      pregn_pre_WW1 == "yes" ~ "pre-war",
      pregn_during_pandemic == "yes" ~ "influenza_pandemic_pregnancy",
      pregn_during_WW1 == "yes" ~ "world_war_i_pregnancy",
      pregn_post_WW1 == "yes" ~ "post-war_or_post-pandemic",
      TRUE ~ NA_character_
    ),
    historical_period = factor(
      historical_period,
      levels = c("pre-war", "world_war_i_pregnancy", "influenza_pandemic_pregnancy", "post-war_or_post-pandemic")
    )
  )

table_period_exposure_counts <- df_analysis_clean %>%
  summarise(
    n_total = n(),
    n_missing_month_1 = sum(is.na(month_1_parsed)),
    n_missing_birthdate = sum(is.na(birthdate_parsed)),
    pregn_during_WW1_yes = sum(pregn_during_WW1 == "yes", na.rm = TRUE),
    pregn_during_pandemic_yes = sum(pregn_during_pandemic == "yes", na.rm = TRUE),
    pregn_pre_WW1_yes = sum(pregn_pre_WW1 == "yes", na.rm = TRUE),
    pregn_post_WW1_yes = sum(pregn_post_WW1 == "yes", na.rm = TRUE)
  )

table_historical_period_counts <- df_analysis_clean %>%
  count(historical_period, name = "n") %>%
  mutate(percentage = round(n / sum(n) * 100, 2))

table_historical_exposure_variable_counts <- bind_rows(
  df_analysis_clean %>%
    count(pregn_during_WW1, name = "n") %>%
    mutate(exposure = "pregn_during_WW1", level = as.character(pregn_during_WW1)) %>%
    select(exposure, level, n),
  df_analysis_clean %>%
    count(pregn_during_pandemic, name = "n") %>%
    mutate(exposure = "pregn_during_pandemic", level = as.character(pregn_during_pandemic)) %>%
    select(exposure, level, n)
) %>%
  group_by(exposure) %>%
  mutate(percentage = round(n / sum(n) * 100, 2)) %>%
  ungroup()

# ------------------------------------------------------------
# 6) Direct maternal influenza exposure during pregnancy
# ------------------------------------------------------------
df_analysis_clean <- df_analysis_clean %>%
  mutate(
    grippe_during_pregnancy = case_when(
      grippe_cat %in% c("B", "C") ~ "yes",
      grippe_cat %in% c("A", "D") | is.na(grippe_cat) ~ "no",
      TRUE ~ "no"
    ),
    grippe_during_pregnancy_extended = case_when(
      grippe_cat %in% c("B", "C", "D") ~ "yes",
      grippe_cat %in% c("A") | is.na(grippe_cat) ~ "no",
      TRUE ~ "no"
    ),
    flu_in_pregn_and_pandemic = case_when(
      grippe_during_pregnancy == "yes" & pregn_during_pandemic == "yes" ~ "yes",
      is.na(pregn_during_pandemic) ~ NA_character_,
      TRUE ~ "no"
    ),
    grippe_during_pregnancy = factor(grippe_during_pregnancy, levels = c("no", "yes")),
    grippe_during_pregnancy_extended = factor(grippe_during_pregnancy_extended, levels = c("no", "yes")),
    flu_in_pregn_and_pandemic = factor(flu_in_pregn_and_pandemic, levels = c("no", "yes"))
  )

table_grippe_exposure_counts <- df_analysis_clean %>%
  count(grippe_cat, grippe_during_pregnancy, grippe_during_pregnancy_extended, flu_in_pregn_and_pandemic, name = "n") %>%
  arrange(grippe_cat)

# ------------------------------------------------------------
# 7) Clinically relevant subgroup variables
# ------------------------------------------------------------
df_analysis_clean <- df_analysis_clean %>%
  mutate(
    ptb_group = case_when(
      ptb_corrected == 1 ~ "preterm",
      ptb_corrected == 0 ~ "term",
      TRUE ~ NA_character_
    ),
    lbw_group = case_when(
      lbw == 1 ~ "low_birthweight",
      lbw == 0 ~ "normal_birthweight",
      TRUE ~ NA_character_
    ),
    ga_median_group = case_when(
      is.na(ga_model) ~ NA_character_,
      ga_model < median(ga_model, na.rm = TRUE) ~ "below_median_GA",
      ga_model >= median(ga_model, na.rm = TRUE) ~ "at_or_above_median_GA"
    ),
    ptb_group = factor(ptb_group, levels = c("term", "preterm")),
    lbw_group = factor(lbw_group, levels = c("normal_birthweight", "low_birthweight")),
    ga_median_group = factor(ga_median_group, levels = c("at_or_above_median_GA", "below_median_GA"))
  )

# ------------------------------------------------------------
# 8) Apply analysis covariates to NWL-excluded eligible infants
# ------------------------------------------------------------
if (nrow(df_non_evaluable_weight_trajectory) > 0) {
  df_non_evaluable_weight_trajectory <- df_non_evaluable_weight_trajectory %>%
    mutate(
      exclusion_reason = case_when(
        max_weight_loss_pct < 0 ~ "Weight gain (<0%)",
        max_weight_loss_pct > 20 ~ "Weight loss >20%",
        TRUE ~ NA_character_
      ),
      feeding_cat = case_when(
        feeding == "maternal" ~ "breastfeeding",
        feeding %in% c("mixed", "maternal then mixed", "maternal then mixed then artificial") ~ "mixed",
        feeding %in% c("artificial", "maternal then artificial") ~ "artificial",
        feeding %in% c("human milk", "other") ~ "other",
        is.na(feeding) ~ NA_character_,
        TRUE ~ NA_character_
      ),
      feeding_cat = factor(feeding_cat, levels = c("artificial", "breastfeeding", "mixed", "other"))
    )

  if ("ga_weeks_corrected" %in% names(df_non_evaluable_weight_trajectory)) {
    df_non_evaluable_weight_trajectory <- df_non_evaluable_weight_trajectory %>%
      mutate(ga_model = ifelse(!is.na(ga_weeks_corrected), ga_weeks_corrected, ga_weeks))
  } else {
    df_non_evaluable_weight_trajectory <- df_non_evaluable_weight_trajectory %>%
      mutate(ga_model = ga_weeks)
  }

  df_non_evaluable_weight_trajectory <- df_non_evaluable_weight_trajectory %>%
    mutate(
      birthweight_100g = birthweight / 100,
      sex_cat = factor(sex, levels = c(0, 1), labels = c("female", "male")),
      birthdate_parsed = parse_historical_date(birthdate),
      month_1_parsed = parse_historical_date(month_1),
      pregn_during_WW1 = case_when(
        is.na(month_1_parsed) | is.na(birthdate_parsed) ~ NA_character_,
        month_1_parsed < ww1_end & birthdate_parsed >= ww1_start ~ "yes",
        TRUE ~ "no"
      ),
      pregn_during_pandemic = case_when(
        is.na(month_1_parsed) | is.na(birthdate_parsed) ~ NA_character_,
        (month_1_parsed < pandemic_wave1_end & birthdate_parsed >= pandemic_wave1_start) |
          (month_1_parsed < pandemic_wave2_end & birthdate_parsed >= pandemic_wave2_start) ~ "yes",
        TRUE ~ "no"
      ),
      pregn_during_WW1 = factor(pregn_during_WW1, levels = c("no", "yes")),
      pregn_during_pandemic = factor(pregn_during_pandemic, levels = c("no", "yes")),
      grippe_during_pregnancy = case_when(
        grippe_cat %in% c("B", "C") ~ "yes",
        grippe_cat %in% c("A", "D") | is.na(grippe_cat) ~ "no",
        TRUE ~ "no"
      ),
      grippe_during_pregnancy = factor(grippe_during_pregnancy, levels = c("no", "yes")),
      flu_in_pregn_and_pandemic = case_when(
        grippe_during_pregnancy == "yes" & pregn_during_pandemic == "yes" ~ "yes",
        is.na(pregn_during_pandemic) ~ NA_character_,
        TRUE ~ "no"
      ),
      flu_in_pregn_and_pandemic = factor(flu_in_pregn_and_pandemic, levels = c("no", "yes")),
      ptb_group = case_when(
        ptb_corrected == 1 ~ "preterm",
        ptb_corrected == 0 ~ "term",
        TRUE ~ NA_character_
      ),
      lbw_group = case_when(
        lbw == 1 ~ "low_birthweight",
        lbw == 0 ~ "normal_birthweight",
        TRUE ~ NA_character_
      ),
      ptb_group = factor(ptb_group, levels = c("term", "preterm")),
      lbw_group = factor(lbw_group, levels = c("normal_birthweight", "low_birthweight"))
    )
}

# Save processed dataset for downstream scripts.
saveRDS(df_analysis_clean, path_processed_data)
