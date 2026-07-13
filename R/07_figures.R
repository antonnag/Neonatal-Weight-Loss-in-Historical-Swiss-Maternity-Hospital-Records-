# ============================================================
# 07_figures.R
# Journal-aligned main manuscript and supplementary figures
# ============================================================
# Main manuscript figures:
#   Figure 1. Study flowchart
#   Figure 2. Distribution of maximum neonatal weight loss
#   Figure 3. Neonatal weight loss by feeding mode
#   Figure 4. Adjusted associations from the main GA-adjusted models
# Supplementary figures:
#   Figure S1. Breastfeeding and excessive NWL across subgroups
#   Figure S2. Mean NWL by pregnancy-based historical exposure
#   Figure S3. Feeding-effect comparison: BW main vs GA sensitivity
#   Figure S4. Excessive NWL by feeding mode
#   Figure S5. Mean NWL by birth year
#   Figure S6. Feeding type distribution by birth year

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
label_model_terms <- function(x) {
  dplyr::recode(
    x,
    "pregn_during_WW1yes" = "Pregnancy overlapped WWI",
    "pregn_during_pandemicyes" = "Pregnancy overlapped influenza pandemic",
    "feeding_catbreastfeeding" = "Breastfeeding vs artificial",
    "feeding_catmixed" = "Mixed feeding vs artificial",
    "feeding_catother" = "Other feeding vs artificial",
    "birthweight_100g" = "Birthweight, per 100 g",
    "ga_model" = "Gestational age, per week",
    "sex_catmale" = "Male vs female",
    "parity" = "Parity",
    "age_mother" = "Maternal age, per year",
    .default = x
  )
}

ensure_figure_dirs <- function() {
  dir.create(file.path("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("outputs", "appendix_figures"), recursive = TRUE, showWarnings = FALSE)
}
ensure_figure_dirs()

# ------------------------------------------------------------
# Paper-style figure theme helpers
# ------------------------------------------------------------
paper_plot_theme <- function() {
  theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12, margin = margin(b = 8)),
      plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 8)),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      strip.text = element_text(size = 10, face = "bold"),
      panel.grid = element_blank(),
      plot.margin = margin(t = 18, r = 28, b = 18, l = 28, unit = "pt")
    )
}

paper_forest_theme <- function() {
  theme_classic(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12, margin = margin(b = 8)),
      plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 8)),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      strip.text = element_text(size = 10, face = "bold"),
      panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 20, r = 32, b = 18, l = 32, unit = "pt")
    )
}


paper_forest_theme_s1 <- function() {
  theme_classic(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12, margin = margin(b = 8)),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 9),
      axis.text.y = element_text(size = 8),
      legend.position = "bottom",
      panel.grid.major.x = element_line(colour = "grey75", linewidth = 0.6),
      panel.grid.minor.x = element_line(colour = "grey88", linewidth = 0.35),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      plot.margin = margin(t = 20, r = 32, b = 18, l = 32, unit = "pt")
    )
}

paper_ci_theme <- function() {
  theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12, margin = margin(b = 8)),
      plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 8)),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      strip.text = element_text(size = 10, face = "bold"),
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 18, r = 28, b = 18, l = 28, unit = "pt")
    )
}

paper_time_theme <- function() {
  theme_classic(base_size = 12) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12, margin = margin(b = 8)),
      plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 8)),
      axis.title = element_text(size = 11),
      axis.text = element_text(size = 10),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      strip.text = element_text(size = 10, face = "bold"),
      panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.3),
      panel.grid.major.x = element_line(colour = "grey95", linewidth = 0.25),
      panel.grid.minor = element_blank(),
      plot.margin = margin(t = 18, r = 28, b = 18, l = 28, unit = "pt")
    )
}

# ------------------------------------------------------------
# MAIN MANUSCRIPT FIGURES
# ------------------------------------------------------------

# Figure 1. Study population flowchart
flow_data <- tibble(
  step = factor(
    c(
      "Historical birth records",
      "Eligible live births with birthweight\nand postnatal weight",
      "Excluded due to inadequate longitudinal\nweight data",
      "Final analytical sample"
    ),
    levels = c(
      "Historical birth records",
      "Eligible live births with birthweight\nand postnatal weight",
      "Excluded due to inadequate longitudinal\nweight data",
      "Final analytical sample"
    )
  ),
  n = c(n_original, n_eligible, n_non_evaluable_weight_trajectory, n_final),
  y = c(4, 3, 2, 1)
)

figure_1_study_flowchart <- ggplot(flow_data, aes(x = 1, y = y)) +
  geom_label(aes(label = paste0(step, "\n", "n = ", n)), size = 4, label.size = 0.4) +
  geom_segment(
    data = tibble(y_start = c(3.75, 2.75, 1.75), y_end = c(3.25, 2.25, 1.25)),
    aes(x = 1, xend = 1, y = y_start, yend = y_end),
    arrow = arrow(length = unit(0.2, "cm")),
    inherit.aes = FALSE
  ) +
  theme_void() +
  labs(title = "Figure 1. Study population flowchart")

ggsave(
  file.path("outputs", "figures", "Figure_1_Study_Flowchart.png"),
  figure_1_study_flowchart,
  width = 7,
  height = 6,
  dpi = 300
)

# Figure 2. Distribution of maximum neonatal weight loss
figure_2_weightloss_distribution <- ggplot(df_analysis_clean, aes(x = max_weight_loss_pct)) +
  geom_histogram(bins = 50, color = "black") +
  geom_vline(xintercept = 10, linetype = "dashed") +
  labs(
    title = "Figure 2. Distribution of maximum neonatal weight loss",
    x = "Maximum neonatal weight loss (%)",
    y = "Number of newborns"
  ) +
  paper_plot_theme()

ggsave(
  file.path("outputs", "figures", "Figure_2_Weightloss_Distribution.png"),
  figure_2_weightloss_distribution,
  width = 7,
  height = 5,
  dpi = 300
)

# Figure 3. Neonatal weight loss according to feeding type

figure_3_weightloss_by_feeding <- ggplot(
  df_analysis_clean %>% filter(!is.na(feeding_cat)),
  aes(x = feeding_cat, y = max_weight_loss_pct)
) +
  geom_violin(
    trim = FALSE,
    alpha = 0.5
  ) +
  geom_boxplot(
    width = 0.12,
    outlier.alpha = 0.15,
    outlier.size = 0.8
  ) +
  labs(
    title = "Figure 3. Neonatal weight loss according to feeding type",
    x = "Feeding type",
    y = "Maximum neonatal weight loss (%)"
  ) +
  paper_plot_theme()

ggsave(
  file.path("outputs", "figures", "Figure_3_Weightloss_By_Feeding.png"),
  figure_3_weightloss_by_feeding,
  width = 7,
  height = 5,
  dpi = 300
)

# Figure 4. Adjusted associations from the two main GA-adjusted models
# Panel A: beta coefficients from linear model. Panel B: ORs from logistic model.
main_linear_forest <- table_MX_linear_period_GA %>%
  filter(term != "(Intercept)") %>%
  transmute(
    panel = "A. Maximum NWL (%)",
    term = label_model_terms(term),
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high),
    reference = 0
  )

main_logistic_forest <- table_MX_logistic_period_GA %>%
  filter(term != "(Intercept)") %>%
  transmute(
    panel = "B. Excessive NWL >10%",
    term = label_model_terms(term),
    estimate = as.numeric(odds_ratio),
    conf.low = as.numeric(ci_low),
    conf.high = as.numeric(ci_high),
    reference = 1
  )

forest_order <- c(
  "Breastfeeding vs artificial",
  "Mixed feeding vs artificial",
  "Other feeding vs artificial",
  "Pregnancy overlapped influenza pandemic",
  "Pregnancy overlapped WWI",
  "Gestational age, per week",
  "Male vs female",
  "Parity",
  "Maternal age, per year"
)

figure_4_adjusted_forest <- bind_rows(main_linear_forest, main_logistic_forest) %>%
  mutate(term = factor(term, levels = rev(forest_order))) %>%
  ggplot(aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)) +
  geom_vline(aes(xintercept = reference), linetype = "dashed") +
  geom_pointrange() +
  facet_wrap(~ panel, scales = "free_x") +
  labs(
    title = "Figure 4. Adjusted associations with neonatal weight loss",
    subtitle = "Gestational-age-adjusted main models",
    x = "Effect estimate (beta coefficient or odds ratio)",
    y = NULL
  ) +
  paper_plot_theme()

ggsave(
  file.path("outputs", "figures", "Figure_4_Adjusted_Associations_Forestplot.png"),
  figure_4_adjusted_forest,
  width = 10,
  height = 6,
  dpi = 300
)

# ------------------------------------------------------------
# SUPPLEMENTARY FIGURES
# ------------------------------------------------------------

# Supplementary Figure S1. Breastfeeding effect across subgroups
appendix_figure_s1_subgroup_forest <- table_STR_logistic_excel_inputs %>%
  filter(stratum != "all_births") %>%
  mutate(
    analysis = dplyr::recode(
      analysis,
      "STR1_PTB_vs_term" = "Preterm birth",
      "STR2_LBW_vs_normal_BW" = "Low birthweight",
      "STR3_pandemic_pregnancy_yes_no" = "Pandemic exposure",
      "STR4_WW1_pregnancy_yes_no" = "WWI exposure",
      "STR5_direct_flu_in_pregnancy_and_pandemic" = "Maternal flu during pandemic pregnancy",
      "STR6_sex_female_vs_male" = "Neonatal sex",
      .default = analysis
    ),
    subgroup = paste0(analysis, ": ", stratum),
    OR = as.numeric(OR),
    CI_low = as.numeric(CI_low),
    CI_high = as.numeric(CI_high)
  ) %>%
  filter(!is.na(OR), !is.na(CI_low), !is.na(CI_high)) %>%
  mutate(subgroup = factor(subgroup, levels = rev(unique(subgroup)))) %>%
  ggplot(aes(x = OR, y = subgroup, xmin = CI_low, xmax = CI_high)) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_pointrange() +
  labs(
    title = "Supplementary Figure S1. Breastfeeding and excessive neonatal weight loss across subgroups",
    x = "Odds ratio for excessive neonatal weight loss (>10%)",
    y = NULL
  ) +
  scale_x_continuous(
    breaks = seq(0, 2.5, by = 0.25),
    minor_breaks = seq(0, 2.5, by = 0.125)
  ) +
  paper_forest_theme_s1()

export_figure(
  appendix_figure_s1_subgroup_forest,
  "Appendix_Figure_S1_Subgroup_Breastfeeding_Forestplot.png",
  file.path("outputs", "appendix_figures"),
  width = 12,
  height = 7
)

# Supplementary Figure S2. Mean neonatal weight loss by pregnancy-based historical exposures
period_summary <- bind_rows(
  df_analysis_clean %>%
    filter(!is.na(pregn_during_WW1)) %>%
    group_by(exposure = "Pregnancy overlapped WWI", level = pregn_during_WW1) %>%
    summarise(
      mean_weight_loss = mean(max_weight_loss_pct, na.rm = TRUE),
      sd_weight_loss = sd(max_weight_loss_pct, na.rm = TRUE),
      n = n(),
      se = sd_weight_loss / sqrt(n),
      ci_low = mean_weight_loss - 1.96 * se,
      ci_high = mean_weight_loss + 1.96 * se,
      .groups = "drop"
    ),
  df_analysis_clean %>%
    filter(!is.na(pregn_during_pandemic)) %>%
    group_by(exposure = "Pregnancy overlapped influenza pandemic", level = pregn_during_pandemic) %>%
    summarise(
      mean_weight_loss = mean(max_weight_loss_pct, na.rm = TRUE),
      sd_weight_loss = sd(max_weight_loss_pct, na.rm = TRUE),
      n = n(),
      se = sd_weight_loss / sqrt(n),
      ci_low = mean_weight_loss - 1.96 * se,
      ci_high = mean_weight_loss + 1.96 * se,
      .groups = "drop"
    )
)

appendix_figure_s2_historical_exposures <- ggplot(period_summary, aes(x = level, y = mean_weight_loss)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.15) +
  facet_wrap(~ exposure) +
  labs(
    title = "Supplementary Figure S2. Mean neonatal weight loss by pregnancy-based historical exposure",
    x = "Exposure during pregnancy",
    y = "Mean maximum neonatal weight loss (%)"
  ) +
  paper_ci_theme()

export_figure(
  appendix_figure_s2_historical_exposures,
  "Appendix_Figure_S2_Historical_Exposures.png",
  file.path("outputs", "appendix_figures"),
  width = 14,
  height = 7.5
)

# Supplementary Figure S3. Sensitivity analysis: feeding-associated differences in neonatal weight loss
forest_feeding <- comparison_feeding_effects %>%
  mutate(
    term = label_model_terms(term),
    term = factor(term, levels = rev(c("Breastfeeding vs artificial", "Mixed feeding vs artificial", "Other feeding vs artificial"))),
    model = factor(model, levels = c("Main model: GA-adjusted", "Supplementary model: birthweight-adjusted")),
    estimate = as.numeric(estimate),
    conf.low = as.numeric(conf.low),
    conf.high = as.numeric(conf.high)
  )

appendix_figure_s3_sensitivity_forestplot <- ggplot(
  forest_feeding,
  aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high, shape = model)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(width = 0.5)) +
  labs(
    title = "Supplementary Figure S3. Feeding-associated differences in neonatal weight loss",
    subtitle = "Comparison of the main GA-adjusted model and supplementary birthweight-adjusted model",
    x = "Regression coefficient for maximum neonatal weight loss (%)",
    y = NULL,
    shape = "Model"
  ) +
  paper_forest_theme_s1()

export_figure(
  appendix_figure_s3_sensitivity_forestplot,
  "Appendix_Figure_S3_Sensitivity_Forestplot.png",
  file.path("outputs", "appendix_figures"),
  width = 14,
  height = 7.5
)

# Supplementary Figure S4. Excessive neonatal weight loss by feeding type
appendix_figure_s4_excessive_weightloss <- df_analysis_clean %>%
  filter(!is.na(feeding_cat)) %>%
  group_by(feeding_cat) %>%
  summarise(excessive_percent = mean(excessive_weight_loss_10, na.rm = TRUE) * 100, .groups = "drop") %>%
  ggplot(aes(x = feeding_cat, y = excessive_percent)) +
  geom_col() +
  labs(
    title = "Supplementary Figure S4. Excessive neonatal weight loss (>10%) by feeding type",
    x = "Feeding type",
    y = "Newborns with excessive weight loss (%)"
  ) +
  paper_plot_theme()

export_figure(
  appendix_figure_s4_excessive_weightloss,
  "Appendix_Figure_S4_Excessive_Weightloss_By_Feeding.png",
  file.path("outputs", "appendix_figures"),
  width = 12,
  height = 7
)

# Supplementary Figure S5. Mean neonatal weight loss by birth year
appendix_figure_s5_weightloss_by_year <- df_analysis_clean %>%
  filter(!is.na(birthyear)) %>%
  group_by(birthyear) %>%
  summarise(
    mean_weight_loss = mean(max_weight_loss_pct, na.rm = TRUE),
    sd_weight_loss = sd(max_weight_loss_pct, na.rm = TRUE),
    n = n(),
    se = sd_weight_loss / sqrt(n),
    ci_low = mean_weight_loss - 1.96 * se,
    ci_high = mean_weight_loss + 1.96 * se,
    .groups = "drop"
  ) %>%
  ggplot(aes(x = birthyear, y = mean_weight_loss)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = 0.2) +
  labs(
    title = "Supplementary Figure S5. Mean neonatal weight loss by birth year",
    x = "Birth year",
    y = "Mean maximum neonatal weight loss (%)"
  ) +
  paper_time_theme()

ggsave(
  file.path("outputs", "appendix_figures", "Appendix_Figure_S5_Weightloss_By_Year.png"),
  appendix_figure_s5_weightloss_by_year,
  width = 8,
  height = 5,
  dpi = 300
)

# Supplementary Figure S6. Feeding type distribution by birth year
appendix_figure_s6_feeding_by_year <- df_analysis_clean %>%
  filter(!is.na(birthyear), !is.na(feeding_cat)) %>%
  count(birthyear, feeding_cat) %>%
  group_by(birthyear) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ungroup() %>%
  ggplot(aes(x = birthyear, y = percentage, colour = feeding_cat)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Supplementary Figure S6. Distribution of feeding types by birth year",
    x = "Birth year",
    y = "Percentage of newborns (%)",
    colour = "Feeding type"
  ) +
  paper_time_theme()

ggsave(
  file.path("outputs", "appendix_figures", "Appendix_Figure_S6_Feeding_By_Birthyear.png"),
  appendix_figure_s6_feeding_by_year,
  width = 8,
  height = 5,
  dpi = 300
)
