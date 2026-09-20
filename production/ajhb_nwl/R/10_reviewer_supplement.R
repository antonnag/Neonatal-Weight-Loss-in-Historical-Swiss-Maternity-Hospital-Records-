# ============================================================
# 10_reviewer_supplement.R
# Publication-ready supplementary outputs for AJHB reviewer revision
# ============================================================
# Depends on objects created in R/09_reviewer_revision.R and figure helpers
# created in R/07_figures.R.
# Creates:
#   Supplementary Figure S7: timing of first and second postnatal weights
#   Supplementary Table S8: characteristics by one vs two measurements
#   Supplementary Table S9: exact day-5/day-10 vs other measurement patterns
#   Supplementary Table S10: main vs exact day-5/day-10 sensitivity models
# Existing manuscript outputs are not overwritten.

required_reviewer_objects <- c(
  "table_reviewer_day_a_distribution",
  "table_reviewer_day_b_distribution",
  "df_reviewer_measurements",
  "table_reviewer_exact_vs_other_characteristics",
  "table_reviewer_linear_main_vs_exact",
  "table_reviewer_logistic_main_vs_exact"
)

missing_reviewer_objects <- required_reviewer_objects[
  !vapply(required_reviewer_objects, exists, logical(1), inherits = TRUE)
]

if (length(missing_reviewer_objects) > 0) {
  stop(
    "Reviewer supplement cannot run because required objects are missing: ",
    paste(missing_reviewer_objects, collapse = ", ")
  )
}

# ------------------------------------------------------------
# Helper: neutral journal-style table export
# ------------------------------------------------------------
export_reviewer_gt_png <- function(data,
                                   title,
                                   filename,
                                   folder,
                                   width,
                                   height,
                                   font_size = 9,
                                   groupname_col = NULL) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  if (is.null(groupname_col)) {
    gt_table <- gt::gt(data)
  } else {
    gt_table <- gt::gt(data, groupname_col = groupname_col)
  }

  gt_table <- gt_table %>%
    gt::tab_header(title = title) %>%
    gt::tab_options(
      table.font.size = gt::px(font_size),
      heading.title.font.size = gt::px(font_size + 2),
      heading.align = "center",
      table.width = gt::pct(100),
      data_row.padding = gt::px(3),
      column_labels.font.weight = "bold",
      table.border.top.width = gt::px(1),
      table.border.bottom.width = gt::px(1),
      heading.border.bottom.width = gt::px(1),
      column_labels.border.top.width = gt::px(1),
      column_labels.border.bottom.width = gt::px(1)
    )

  gt::gtsave(
    data = gt_table,
    filename = file.path(folder, filename),
    vwidth = width,
    vheight = height,
    expand = 20
  )

  invisible(gt_table)
}

# ------------------------------------------------------------
# Supplementary Figure S7: actual measurement timing
# ------------------------------------------------------------
reviewer_timing_plot_data <- bind_rows(
  table_reviewer_day_a_distribution %>%
    transmute(
      measurement = "A. First postnatal weight measurement",
      day = day_a,
      n = n,
      percent = percent_among_first_measurements_with_day
    ),
  table_reviewer_day_b_distribution %>%
    transmute(
      measurement = "B. Second postnatal weight measurement",
      day = day_b,
      n = n,
      percent = percent_among_second_measurements_with_day
    )
) %>%
  mutate(
    measurement = factor(
      measurement,
      levels = c(
        "A. First postnatal weight measurement",
        "B. Second postnatal weight measurement"
      )
    )
  )

reviewer_timing_key_labels <- reviewer_timing_plot_data %>%
  filter(
    (measurement == "A. First postnatal weight measurement" & day == 5) |
      (measurement == "B. Second postnatal weight measurement" & day == 10)
  ) %>%
  mutate(label = sprintf("%.1f%%", percent))

supplementary_figure_s7_measurement_timing <- ggplot(
  reviewer_timing_plot_data,
  aes(x = day, y = n)
) +
  geom_col(width = 0.8) +
  geom_text(
    data = reviewer_timing_key_labels,
    aes(label = label),
    vjust = -0.45,
    size = 3.5
  ) +
  facet_wrap(~ measurement, ncol = 1, scales = "free_x") +
  scale_x_continuous(
    breaks = seq(1, 20, by = 1),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.10))
  ) +
  labs(
    title = "Supplementary Figure S7. Timing of postnatal weight measurements",
    x = "Postnatal day",
    y = "Number of newborns"
  ) +
  paper_plot_theme()

export_figure(
  supplementary_figure_s7_measurement_timing,
  filename = "Supplementary_Figure_S7_Weight_Measurement_Timing.png",
  folder = file.path("outputs", "appendix_figures"),
  width = 9,
  height = 7,
  dpi = FIGURE_DPI
)

# ------------------------------------------------------------
# Supplementary Table S8: one vs two postnatal measurements
# ------------------------------------------------------------
# Use the same reader-facing terminology as the existing descriptive tables.
reviewer_one <- df_reviewer_measurements %>%
  filter(measurement_group == "One measurement")
reviewer_two <- df_reviewer_measurements %>%
  filter(measurement_group == "Two measurements")

supplementary_table_s8_measurement_completeness <- tibble(
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
  `One postnatal weight measurement` = c(
    as.character(nrow(reviewer_one)),
    reviewer_fmt_mean_sd(reviewer_one$age_mother, 2),
    reviewer_fmt_mean_sd(reviewer_one$parity, 2),
    reviewer_fmt_mean_sd(reviewer_one$ga_model, 2),
    reviewer_fmt_mean_sd(reviewer_one$birthweight, 0),
    reviewer_fmt_n_pct(reviewer_one$ptb_group, "preterm"),
    reviewer_fmt_n_pct(reviewer_one$lbw_group, "low_birthweight"),
    reviewer_fmt_event_n_pct(reviewer_one$neonatal_death_28, 1),
    reviewer_fmt_n_pct(reviewer_one$sex_cat, "female"),
    reviewer_fmt_n_pct(reviewer_one$sex_cat, "male"),
    reviewer_fmt_n_pct(reviewer_one$feeding_cat, "artificial"),
    reviewer_fmt_n_pct(reviewer_one$feeding_cat, "breastfeeding"),
    reviewer_fmt_n_pct(reviewer_one$feeding_cat, "mixed"),
    reviewer_fmt_n_pct(reviewer_one$feeding_cat, "other")
  ),
  `Two postnatal weight measurements` = c(
    as.character(nrow(reviewer_two)),
    reviewer_fmt_mean_sd(reviewer_two$age_mother, 2),
    reviewer_fmt_mean_sd(reviewer_two$parity, 2),
    reviewer_fmt_mean_sd(reviewer_two$ga_model, 2),
    reviewer_fmt_mean_sd(reviewer_two$birthweight, 0),
    reviewer_fmt_n_pct(reviewer_two$ptb_group, "preterm"),
    reviewer_fmt_n_pct(reviewer_two$lbw_group, "low_birthweight"),
    reviewer_fmt_event_n_pct(reviewer_two$neonatal_death_28, 1),
    reviewer_fmt_n_pct(reviewer_two$sex_cat, "female"),
    reviewer_fmt_n_pct(reviewer_two$sex_cat, "male"),
    reviewer_fmt_n_pct(reviewer_two$feeding_cat, "artificial"),
    reviewer_fmt_n_pct(reviewer_two$feeding_cat, "breastfeeding"),
    reviewer_fmt_n_pct(reviewer_two$feeding_cat, "mixed"),
    reviewer_fmt_n_pct(reviewer_two$feeding_cat, "other")
  ),
  Test = c(
    NA,
    "t-test", "t-test", "t-test", "t-test",
    "Chi-square/Fisher", "Chi-square/Fisher",
    "Fisher's exact",
    "Chi-square/Fisher", "Chi-square/Fisher",
    "Chi-square/Fisher", "Chi-square/Fisher", "Chi-square/Fisher", "Chi-square/Fisher"
  ),
  `P value` = c(
    NA,
    format_p_value(reviewer_safe_t_test("age_mother")),
    format_p_value(reviewer_safe_t_test("parity")),
    format_p_value(reviewer_safe_t_test("ga_model")),
    format_p_value(reviewer_safe_t_test("birthweight")),
    format_p_value(reviewer_safe_cat_test("ptb_group")),
    format_p_value(reviewer_safe_cat_test("lbw_group")),
    format_p_value(reviewer_safe_fisher_test("neonatal_death_28")),
    format_p_value(reviewer_safe_cat_test("sex_cat")),
    format_p_value(reviewer_safe_cat_test("sex_cat")),
    format_p_value(reviewer_safe_cat_test("feeding_cat")),
    format_p_value(reviewer_safe_cat_test("feeding_cat")),
    format_p_value(reviewer_safe_cat_test("feeding_cat")),
    format_p_value(reviewer_safe_cat_test("feeding_cat"))
  )
)

export_reviewer_gt_png(
  supplementary_table_s8_measurement_completeness,
  title = paste0(
    "Supplementary Table S8. Maternal and neonatal characteristics according ",
    "to availability of one versus two postnatal weight measurements"
  ),
  filename = "Supplementary_Table_S8_One_vs_Two_Weight_Measurements.png",
  folder = file.path("outputs", "appendix_tables"),
  width = 1650,
  height = 1200,
  font_size = 9
)

# ------------------------------------------------------------
# Supplementary Table S9: exact days 5 and 10 vs other patterns
# ------------------------------------------------------------
supplementary_table_s9_exact_vs_other <- table_reviewer_exact_vs_other_characteristics %>%
  rename(`P value` = p.value)

stale_s9_sensitivity_file <- file.path(
  "outputs",
  "appendix_tables",
  "Supplementary_Table_S9_Exact_Day5_Day10_Sensitivity.png"
)
if (file.exists(stale_s9_sensitivity_file)) {
  unlink(stale_s9_sensitivity_file)
}

export_reviewer_gt_png(
  supplementary_table_s9_exact_vs_other,
  title = paste0(
    "Supplementary Table S9. Maternal and neonatal characteristics of infants ",
    "with postnatal weights measured exactly on days 5 and 10 versus all other ",
    "measurement patterns"
  ),
  filename = "Supplementary_Table_S9_Exact_Day5_Day10_vs_Other_Patterns.png",
  folder = file.path("outputs", "appendix_tables"),
  width = 1800,
  height = 1250,
  font_size = 9
)

# ------------------------------------------------------------
# Supplementary Table S10: exact day-5/day-10 model sensitivity
# ------------------------------------------------------------
format_reviewer_effect_ci <- function(est, low, high) {
  ifelse(
    is.na(est) | is.na(low) | is.na(high),
    NA_character_,
    sprintf("%.3f (%.3f to %.3f)", est, low, high)
  )
}

supplementary_table_s10_linear <- table_reviewer_linear_main_vs_exact %>%
  filter(term != "(Intercept)") %>%
  transmute(
    Outcome = "Linear model: maximum neonatal weight loss (%)",
    Characteristic = label_model_terms(term),
    `Main cohort effect (95% CI)` = format_reviewer_effect_ci(
      main_estimate, main_ci_low, main_ci_high
    ),
    `Main P value` = main_p_value,
    `Exact day 5/day 10 effect (95% CI)` = format_reviewer_effect_ci(
      exact_day5_day10_estimate,
      exact_day5_day10_ci_low,
      exact_day5_day10_ci_high
    ),
    `Exact day 5/day 10 P value` = exact_day5_day10_p_value
  )

supplementary_table_s10_logistic <- table_reviewer_logistic_main_vs_exact %>%
  filter(term != "(Intercept)") %>%
  transmute(
    Outcome = "Logistic model: excessive neonatal weight loss >10%",
    Characteristic = label_model_terms(term),
    `Main cohort effect (95% CI)` = format_reviewer_effect_ci(
      main_odds_ratio, main_ci_low, main_ci_high
    ),
    `Main P value` = main_p_value,
    `Exact day 5/day 10 effect (95% CI)` = format_reviewer_effect_ci(
      exact_day5_day10_odds_ratio,
      exact_day5_day10_ci_low,
      exact_day5_day10_ci_high
    ),
    `Exact day 5/day 10 P value` = exact_day5_day10_p_value
  )

supplementary_table_s10_exact_day_sensitivity <- bind_rows(
  supplementary_table_s10_linear,
  supplementary_table_s10_logistic
)

export_reviewer_gt_png(
  supplementary_table_s10_exact_day_sensitivity,
  title = paste0(
    "Supplementary Table S10. Sensitivity analysis restricted to infants with ",
    "postnatal weights measured exactly on days 5 and 10"
  ),
  filename = "Supplementary_Table_S10_Exact_Day5_Day10_Sensitivity.png",
  folder = file.path("outputs", "appendix_tables"),
  width = 1900,
  height = 1650,
  font_size = 8,
  groupname_col = "Outcome"
)

# Also save publication-ready source tables as a separate workbook.
write_xlsx(
  list(
    Table_S8_One_vs_Two = round_numeric_df(
      supplementary_table_s8_measurement_completeness
    ),
    Table_S9_Exact_vs_Other = round_numeric_df(
      supplementary_table_s9_exact_vs_other
    ),
    Table_S10_Exact_Day_Sensitivity = round_numeric_df(
      supplementary_table_s10_exact_day_sensitivity
    ),
    Figure_S7_Plot_Data = round_numeric_df(reviewer_timing_plot_data)
  ),
  path = file.path(
    "outputs",
    "reviewer_revision",
    "AJHB_reviewer_supplement_ready.xlsx"
  )
)

message(
  "Reviewer supplement created: Figure S7 and Tables S8, S9, and S10."
)
