# ============================================================
# 10_reviewer_supplement.R
# Publication-ready supplementary outputs for AJHB reviewer revision
# ============================================================
# Depends on objects created in R/09_reviewer_revision.R.
# Creates:
#   Supplementary Figure S7: timing of first and second postnatal weights
#   Supplementary Table S8: characteristics by one vs two measurements
#   Supplementary Table S9: main vs exact day-5/day-10 sensitivity models
# Existing manuscript outputs are not overwritten.

required_reviewer_objects <- c(
  "table_reviewer_day_a_distribution",
  "table_reviewer_day_b_distribution",
  "table_reviewer_one_vs_two_characteristics",
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
  mutate(label = paste0(percent, "%"))

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
    x = "Postnatal day of weight measurement",
    y = "Number of infants"
  ) +
  theme_classic(base_size = 11) +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0),
    axis.text.x = element_text(size = 9),
    plot.margin = margin(10, 16, 10, 16)
  )

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
supplementary_table_s8_measurement_completeness <-
  table_reviewer_one_vs_two_characteristics %>%
  rename(`P value` = p.value)

export_gt_png(
  supplementary_table_s8_measurement_completeness,
  title = paste0(
    "Supplementary Table S8. Maternal and neonatal characteristics according ",
    "to availability of one versus two postnatal weight measurements"
  ),
  filename = "Supplementary_Table_S8_One_vs_Two_Weight_Measurements.png",
  folder = file.path("outputs", "appendix_tables"),
  width = 1500,
  height = 1400,
  font_size = 9
)

# ------------------------------------------------------------
# Supplementary Table S9: exact day-5/day-10 sensitivity
# ------------------------------------------------------------
reviewer_term_labels <- c(
  "pregn_during_WW1yes" = "Pregnancy overlapping World War I: yes vs no",
  "pregn_during_pandemicyes" = "Pregnancy overlapping influenza pandemic: yes vs no",
  "feeding_catbreastfeeding" = "Feeding mode: breastfeeding vs artificial feeding",
  "feeding_catmixed" = "Feeding mode: mixed feeding vs artificial feeding",
  "feeding_catother" = "Feeding mode: other vs artificial feeding",
  "ga_model" = "Gestational age, weeks",
  "sex_catmale" = "Male sex vs female",
  "parity" = "Parity",
  "age_mother" = "Maternal age, years"
)

format_reviewer_effect_ci <- function(est, low, high) {
  ifelse(
    is.na(est) | is.na(low) | is.na(high),
    NA_character_,
    sprintf("%.3f (%.3f to %.3f)", est, low, high)
  )
}

supplementary_table_s9_linear <- table_reviewer_linear_main_vs_exact %>%
  filter(term != "(Intercept)") %>%
  transmute(
    Outcome = "Maximum neonatal weight loss, % (beta)",
    Characteristic = dplyr::recode(term, !!!reviewer_term_labels, .default = term),
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

supplementary_table_s9_logistic <- table_reviewer_logistic_main_vs_exact %>%
  filter(term != "(Intercept)") %>%
  transmute(
    Outcome = "High neonatal weight loss >10% (odds ratio)",
    Characteristic = dplyr::recode(term, !!!reviewer_term_labels, .default = term),
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

supplementary_table_s9_exact_day_sensitivity <- bind_rows(
  supplementary_table_s9_linear,
  supplementary_table_s9_logistic
)

export_gt_png(
  supplementary_table_s9_exact_day_sensitivity,
  title = paste0(
    "Supplementary Table S9. Sensitivity analysis restricted to infants with ",
    "postnatal weights measured exactly on days 5 and 10"
  ),
  filename = "Supplementary_Table_S9_Exact_Day5_Day10_Sensitivity.png",
  folder = file.path("outputs", "appendix_tables"),
  width = 1900,
  height = 1800,
  font_size = 8
)

# Also save publication-ready source tables as a separate workbook.
write_xlsx(
  list(
    Table_S8_One_vs_Two = round_numeric_df(
      supplementary_table_s8_measurement_completeness
    ),
    Table_S9_Exact_Day_Sensitivity = round_numeric_df(
      supplementary_table_s9_exact_day_sensitivity
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
  "Reviewer supplement created: Figure S7, Table S8, and Table S9."
)
