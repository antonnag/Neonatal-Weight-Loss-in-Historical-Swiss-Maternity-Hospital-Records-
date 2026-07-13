# ============================================================
# 08_export_outputs.R
# Export journal-aligned tables, figures, and model summaries
# ============================================================
# Main manuscript tables:
#   Table 1. Cohort characteristics
#   Table 2. Main GA-adjusted linear regression
#   Table 3. Main GA-adjusted logistic regression
# Supplementary tables:
#   Table S1. Included vs NWL-excluded eligible infants
#   Table S2. Feeding allocation / confounding by indication
#   Table S3. Direct maternal influenza model, GA-adjusted
#   Table S4. Influenza in pregnancy during pandemic model, GA-adjusted
#   Table S5. BW-adjusted supplementary linear model
#   Table S6. BW-adjusted supplementary logistic model
#   Table S7. Stratified breastfeeding subgroup analyses with Cochran-Q heterogeneity tests

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
ensure_export_dirs <- function() {
  dir.create(file.path("outputs", "tables_excel"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("outputs", "tables_png"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("outputs", "appendix_tables"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("outputs", "figures"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("outputs", "appendix_figures"), recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path("outputs", "model_summaries"), recursive = TRUE, showWarnings = FALSE)
}
ensure_export_dirs()

clean_main_model_terms <- function(df) {
  df %>%
    dplyr::mutate(
      term = dplyr::case_when(
        term == "(Intercept)" ~ "Intercept",
        
        term == "pregn_during_WW1yes" ~
          "Pregnancy overlapping World War I: yes vs no",
        
        term == "pregn_during_pandemicyes" ~
          "Pregnancy overlapping influenza pandemic: yes vs no",
        
        term == "feeding_catbreastfeeding" ~
          "Feeding mode: breastfeeding vs artificial feeding",
        
        term == "feeding_catmixed" ~
          "Feeding mode: mixed feeding vs artificial feeding",
        
        term == "feeding_catother" ~
          "Feeding mode: other vs artificial feeding",
        
        term == "ga_model" ~
          "Gestational age, weeks",
        
        term == "sex_catmale" ~
          "Male sex vs female",
        
        term == "parity" ~
          "Parity",
        
        term == "age_mother" ~
          "Maternal age, years",
        
        TRUE ~ term
      )
    )
}

table_MX_linear_period_GA_clean <- table_MX_linear_period_GA %>%
  clean_main_model_terms()

table_MX_logistic_period_GA_clean <- table_MX_logistic_period_GA %>%
  clean_main_model_terms()

# Remove technical working columns from stratified model inputs before export
table_STR_logistic_excel_inputs_export <- table_STR_logistic_excel_inputs %>%
  dplyr::select(-dplyr::any_of(c("model_formula", "excel_note")))

# ------------------------------------------------------------
# 1) Export clean journal workbook
# ------------------------------------------------------------
write_xlsx(
  list(
    # Main manuscript tables
    Table_1_Cohort_Characteristics = round_numeric_df(table_1_population),
    Table_2_Linear_GA_Main = round_numeric_df(table_MX_linear_period_GA_clean),
    Table_3_Logistic_GA_Main = round_numeric_df(table_MX_logistic_period_GA_clean),
    
    # Supplementary tables
    Table_S1_Excluded_vs_Included = round_numeric_df(Supplementary_Table_S7_Excluded_vs_Included),
    Table_S1_Exclusion_Reasons = round_numeric_df(Supplementary_Table_S7_Exclusion_Reasons),
    Table_S2_Feeding_Allocation = round_numeric_df(table_6_feeding_allocation),
    Table_S3_BW_Linear_Supplementary = round_numeric_df(table_MX_linear_period_BW),
    Table_S4_BW_Logistic_Supplementary = round_numeric_df(table_MX_logistic_period_BW),
    Table_S5_Direct_Maternal_Flu_GA = round_numeric_df(appendix_table_S3_direct_flu_GA),
    Table_S6_Flu_Pregnancy_Pandemic_GA = round_numeric_df(appendix_table_S4_flu_in_pregn_and_pandemic_GA),
    Table_S7_Subgroup_Analyses = round_numeric_df(Supplementary_Table_S6_Subgroup_Analyses),
    
    # Optional working/descriptive sheets, not recommended as main manuscript tables
    Working_Feeding_Weightloss = round_numeric_df(table_2_feeding_weightloss),
    Working_Historical_Exposures = round_numeric_df(table_3_period_weightloss),
    Working_Feeding_Recoding = round_numeric_df(feeding_recoding_table),
    Working_Exclusion_Counts = round_numeric_df(table_exclusions),
    Working_Weight_Measurement_Timing = round_numeric_df(table_weight_measurement_timing),
    Working_Day_A_Distribution = round_numeric_df(table_day_a),
    Working_Day_B_Distribution = round_numeric_df(table_day_b),
    Working_Lowest_Weight_Day = round_numeric_df(table_lowest_weight_day),
    Working_Lowest_Measurement = round_numeric_df(table_lowest_measurement),
    Working_Summary_Lowest_Weight_Day = round_numeric_df(summary_lowest_weight_day)
  ),
  path = file.path("outputs", "tables_excel", "neonatal_weightloss_tables_JOURNAL_ALIGNED.xlsx")
)

# Separate Excel file for subgroup/Cochran-Q checks.
write_xlsx(
  list(
    Supplementary_Table_S7_Subgroup_Analyses = round_numeric_df(Supplementary_Table_S6_Subgroup_Analyses),
    STR_Logistic_Model_Inputs = round_numeric_df(table_STR_logistic_excel_inputs_export),
    Cochran_Q_Heterogeneity_Tests = round_numeric_df(table_subgroup_heterogeneity_cochran_q)
  ),
  path = file.path("outputs", "tables_excel", "subgroup_analyses_cochran_q_inputs.xlsx")
)

# ------------------------------------------------------------
# 2) Export main manuscript tables as PNG
# ------------------------------------------------------------
png_table_1_population <- export_gt_png(
  table_1_population,
  "Table 1. Characteristics of the analytical study population",
  "Table_1_Cohort_Characteristics.png",
  file.path("outputs", "tables_png"),
  width = 700,
  font_size = 10
)

png_table_2_linear_regression <- export_gt_png(
  round_numeric_df(table_MX_linear_period_GA_clean),
  "Table 2. Linear regression: maximum neonatal weight loss, gestational-age-adjusted main model",
  "Table_2_Linear_GA_Main.png",
  file.path("outputs", "tables_png")
)

png_table_3_logistic_regression <- export_gt_png(
  round_numeric_df(table_MX_logistic_period_GA_clean),
  "Table 3. Logistic regression: excessive neonatal weight loss >10%, gestational-age-adjusted main model",
  "Table_3_Logistic_GA_Main.png",
  file.path("outputs", "tables_png")
)

# ------------------------------------------------------------
# 3) Export supplementary tables as PNG
# New supplementary numbering follows first mention in the manuscript:
#   old S7 -> new S1
#   old S1 -> new S2
#   old S2 -> new S3 -> S5
#   old S3 -> new S4 -> S6
#   old S4 -> new S5 -> S3
#   old S5 -> new S6 -> S4
#   old S6 -> new S7
# ------------------------------------------------------------
png_appendix_table_s1 <- export_gt_png(
  round_numeric_df(Supplementary_Table_S7_Excluded_vs_Included),
  "Supplementary Table S1. Characteristics of included infants and infants excluded because of non-evaluable neonatal weight trajectories",
  "Supplementary_Table_S1_Excluded_vs_Included.png",
  file.path("outputs", "appendix_tables"),
  width = 1400,
  font_size = 9
)

png_appendix_table_s2 <- export_gt_png(
  round_numeric_df(table_6_feeding_allocation),
  "Supplementary Table S2. Maternal and neonatal characteristics according to feeding mode",
  "Supplementary_Table_S2_Feeding_Allocation.png",
  file.path("outputs", "appendix_tables")
)

png_appendix_table_s3 <- export_gt_png(
  round_numeric_df(appendix_table_S3_direct_flu_GA),
  "Supplementary Table S3. Direct maternal influenza model, gestational-age-adjusted",
  "Supplementary_Table_S3_Direct_Maternal_Flu_GA.png",
  file.path("outputs", "appendix_tables")
)

png_appendix_table_s4 <- export_gt_png(
  round_numeric_df(appendix_table_S4_flu_in_pregn_and_pandemic_GA),
  "Supplementary Table S4. Influenza in pregnancy during pandemic model, gestational-age-adjusted",
  "Supplementary_Table_S4_Flu_Pregnancy_Pandemic_GA.png",
  file.path("outputs", "appendix_tables")
)



png_appendix_table_s5 <- export_gt_png(
  round_numeric_df(table_MX_linear_period_BW),
  "Supplementary Table S5. Birthweight-adjusted supplementary model: maximum neonatal weight loss",
  "Supplementary_Table_S5_BW_Linear_Supplementary.png",
  file.path("outputs", "appendix_tables")
)

png_appendix_table_s6 <- export_gt_png(
  round_numeric_df(table_MX_logistic_period_BW),
  "Supplementary Table S6. Birthweight-adjusted supplementary model: excessive neonatal weight loss >10%",
  "Supplementary_Table_S6_BW_Logistic_Supplementary.png",
  file.path("outputs", "appendix_tables")
)

png_appendix_table_s7 <- export_gt_png(
  round_numeric_df(Supplementary_Table_S6_Subgroup_Analyses),
  "Supplementary Table S7. Stratified breastfeeding subgroup analyses with Cochran-Q heterogeneity tests",
  "Supplementary_Table_S7_Subgroup_Analyses.png",
  file.path("outputs", "appendix_tables")
)

# ------------------------------------------------------------
# 4) Export model summaries as text files
# ------------------------------------------------------------
capture.output(summary(model_MX_linear_period_GA), file = file.path("outputs", "model_summaries", "Table_2_main_linear_GA_summary.txt"))
capture.output(summary(model_MX_logistic_period_GA), file = file.path("outputs", "model_summaries", "Table_3_main_logistic_GA_summary.txt"))
capture.output(summary(model_MX_linear_period_BW), file = file.path("outputs", "model_summaries", "S5_supplementary_linear_BW_summary.txt"))
capture.output(summary(model_MX_logistic_period_BW), file = file.path("outputs", "model_summaries", "S6_supplementary_logistic_BW_summary.txt"))
capture.output(summary(model_S3_direct_flu_GA), file = file.path("outputs", "model_summaries", "S3_direct_maternal_flu_GA_summary.txt"))
capture.output(summary(model_S4_flu_in_pregn_and_pandemic_GA), file = file.path("outputs", "model_summaries", "S4_flu_pregnancy_pandemic_GA_summary.txt"))



# ------------------------------------------------------------
# 5) Export main and supplementary figures as PNG
# ------------------------------------------------------------
# This block assumes that 07_figures.R has been sourced before this script,
# so that the ggplot objects below already exist in the R environment.
# If a figure object is missing, the script prints a warning and continues.

export_existing_figure <- function(object_name, filename, folder, width, height, dpi = FIGURE_DPI) {
  if (!exists(object_name, inherits = TRUE)) {
    warning(paste0("Figure object not found and not exported: ", object_name))
    return(invisible(NULL))
  }
  export_figure(
    get(object_name, inherits = TRUE),
    filename = filename,
    folder = folder,
    width = width,
    height = height,
    dpi = dpi
  )
}

# Main manuscript figures
export_existing_figure(
  "figure_1_study_flowchart",
  "Figure_1_Study_Flowchart.png",
  file.path("outputs", "figures"),
  width = 7,
  height = 6
)

export_existing_figure(
  "figure_2_weightloss_distribution",
  "Figure_2_Weightloss_Distribution.png",
  file.path("outputs", "figures"),
  width = 7,
  height = 5
)

export_existing_figure(
  "figure_3_weightloss_by_feeding",
  "Figure_3_Weightloss_By_Feeding.png",
  file.path("outputs", "figures"),
  width = 7,
  height = 5
)

export_existing_figure(
  "figure_4_adjusted_forest",
  "Figure_4_Adjusted_Associations_Forestplot.png",
  file.path("outputs", "figures"),
  width = 10,
  height = 6
)

# Supplementary figures
export_existing_figure(
  "appendix_figure_s1_subgroup_forest",
  "Supplementary_Figure_S1_Breastfeeding_Subgroups.png",
  file.path("outputs", "appendix_figures"),
  width = 12,
  height = 7
)

export_existing_figure(
  "appendix_figure_s2_historical_exposures",
  "Supplementary_Figure_S2_Historical_Exposures.png",
  file.path("outputs", "appendix_figures"),
  width = 14,
  height = 7.5
)

export_existing_figure(
  "appendix_figure_s3_sensitivity_forestplot",
  "Supplementary_Figure_S3_Sensitivity_Forestplot.png",
  file.path("outputs", "appendix_figures"),
  width = 14,
  height = 7.5
)

export_existing_figure(
  "appendix_figure_s4_excessive_weightloss",
  "Supplementary_Figure_S4_Excessive_Weightloss_By_Feeding.png",
  file.path("outputs", "appendix_figures"),
  width = 12,
  height = 7
)

export_existing_figure(
  "appendix_figure_s5_weightloss_by_year",
  "Supplementary_Figure_S5_Weightloss_By_Year.png",
  file.path("outputs", "appendix_figures"),
  width = 8,
  height = 5
)

export_existing_figure(
  "appendix_figure_s6_feeding_by_year",
  "Supplementary_Figure_S6_Feeding_By_Birthyear.png",
  file.path("outputs", "appendix_figures"),
  width = 8,
  height = 5
)


# ------------------------------------------------------------
# 6) Placement guide
# ------------------------------------------------------------
placement_guide <- tibble(
  item = c(
    "Figure 1. Study population flowchart",
    "Figure 2. Distribution of maximum neonatal weight loss",
    "Figure 3. Neonatal weight loss according to feeding type",
    "Figure 4. Adjusted associations with neonatal weight loss",
    "Table 1. Characteristics of the analytical study population",
    "Table 2. Linear regression: maximum neonatal weight loss, GA-adjusted main model",
    "Table 3. Logistic regression: excessive neonatal weight loss >10%, GA-adjusted main model",
    "Supplementary Table S1. Included vs NWL-excluded eligible infants",
    "Supplementary Table S2. Feeding allocation / confounding by indication",
    "Supplementary Table S3. Direct maternal influenza model, GA-adjusted",
    "Supplementary Table S4. Influenza in pregnancy during pandemic model, GA-adjusted",
    "Supplementary Table S5. BW-adjusted supplementary linear model",
    "Supplementary Table S6. BW-adjusted supplementary logistic model",
    "Supplementary Table S7. Stratified breastfeeding subgroup analyses with Cochran-Q heterogeneity tests",
    "Supplementary Figure S1. Breastfeeding effect across subgroups",
    "Supplementary Figure S2. Mean neonatal weight loss by historical exposure",
    "Supplementary Figure S3. Feeding-effect comparison: GA main vs BW supplementary",
    "Supplementary Figure S4. Excessive NWL by feeding mode",
    "Supplementary Figure S5. Mean NWL by birth year",
    "Supplementary Figure S6. Feeding type distribution by birth year"
  ),
  placement = c(
    "Main manuscript, Results: study population",
    "Main manuscript, Results: outcome distribution",
    "Main manuscript, Results: feeding mode and NWL",
    "Main manuscript, Results: adjusted main models",
    "Main manuscript, Results: study population",
    "Main manuscript, Results: main multivariable analyses",
    "Main manuscript, Results: main multivariable analyses",
    "Supplement: supports assessment of selection bias from NWL-based exclusions",
    "Supplement: supports confounding-by-indication discussion",
    "Supplement: maternal influenza analysis",
    "Supplement: maternal influenza/pandemic analysis",
    "Supplement: supplementary BW adjustment check",
    "Supplement: supplementary BW adjustment check",
    "Supplement: subgroup analyses and Cochran-Q heterogeneity tests",
    "Supplement: subgroup robustness",
    "Supplement: descriptive historical context",
    "Supplement: robustness of feeding effects",
    "Supplement: descriptive feeding/EWL pattern",
    "Supplement: descriptive historical trend",
    "Supplement: descriptive feeding trend"
  ),
  rationale = c(
    "Replaces exclusion table and shows cohort derivation clearly.",
    "Introduces the primary outcome distribution and the >10% threshold.",
    "Visualizes the dominant exposure: feeding mode.",
    "Summarizes the adjusted main effects and reduces text redundancy.",
    "Standard baseline table.",
    "Primary outcome model; central quantitative evidence.",
    "Secondary outcome model; central quantitative evidence.",
    "Addresses feedback to check whether NWL-based exclusions are selective.",
    "Important, but explanatory rather than a main result.",
    "Secondary model; supports influenza interpretation.",
    "Secondary model; supports influenza interpretation.",
    "Retained only as supplementary analysis because BW is part of the NWL formula.",
    "Retained only as supplementary analysis because BW is part of the NWL formula.",
    "Supports the statement that breastfeeding association is consistent across strata.",
    "Best visual support for subgroup consistency.",
    "Useful historical context but weaker than adjusted models.",
    "Robustness visual; not needed as main figure.",
    "Descriptive and redundant with main feeding figure/table.",
    "Historical context; optional.",
    "Historical context; optional."
  )
)

write_xlsx(
  list(
    Table_1_Cohort_Characteristics = round_numeric_df(table_1_population),
    Table_2_Linear_GA_Main = round_numeric_df(table_MX_linear_period_GA_clean),
    Table_3_Logistic_GA_Main = round_numeric_df(table_MX_logistic_period_GA_clean),
    Table_S1_Excluded_vs_Included = round_numeric_df(Supplementary_Table_S7_Excluded_vs_Included),
    Placement_Guide = placement_guide
  ),
  path = file.path("outputs", "tables_excel", "placement_guide_tables_figures_JOURNAL_ALIGNED.xlsx")
)
