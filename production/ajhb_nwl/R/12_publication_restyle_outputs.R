# ============================================================
# 12_publication_restyle_outputs.R
# Central publication-style pass for all manuscript outputs
# ============================================================
# Responsibility of this file:
#   - apply final figure typography and export publication PNGs;
#   - apply one consistent GT style to main/supplementary tables;
#   - size table PNGs according to their visible content;
#   - act as the single final presentation/export layer.
#
# Statistical objects and estimates are created upstream and are not changed
# here. This script is intentionally sourced last by 00_master_run_all.R.

PUBLICATION_DPI <- 600
PUBLICATION_BASE_SIZE <- 11
PUBLICATION_TITLE_SIZE <- 12
PUBLICATION_SUBTITLE_SIZE <- 10
PUBLICATION_AXIS_TITLE_SIZE <- 11
PUBLICATION_AXIS_TEXT_SIZE <- 10
PUBLICATION_LEGEND_TITLE_SIZE <- 10
PUBLICATION_LEGEND_TEXT_SIZE <- 9
PUBLICATION_TABLE_BODY_SIZE <- 9
PUBLICATION_TABLE_TITLE_SIZE <- 11
PUBLICATION_TABLE_HEADER_BG <- "#E6E6E6"
PUBLICATION_TABLE_COLUMN_BG <- "#F2F2F2"
PUBLICATION_TABLE_SECTION_BG <- "#F7F7F7"

# ------------------------------------------------------------
# 1) Figure helpers and shared typography
# ------------------------------------------------------------
publication_theme_override <- ggplot2::theme(
  plot.title = ggplot2::element_text(
    face = "bold",
    hjust = 0.5,
    size = PUBLICATION_TITLE_SIZE,
    margin = ggplot2::margin(b = 8)
  ),
  plot.subtitle = ggplot2::element_text(
    hjust = 0.5,
    size = PUBLICATION_SUBTITLE_SIZE,
    margin = ggplot2::margin(b = 8)
  ),
  axis.title = ggplot2::element_text(size = PUBLICATION_AXIS_TITLE_SIZE),
  axis.text = ggplot2::element_text(size = PUBLICATION_AXIS_TEXT_SIZE),
  legend.title = ggplot2::element_text(size = PUBLICATION_LEGEND_TITLE_SIZE),
  legend.text = ggplot2::element_text(size = PUBLICATION_LEGEND_TEXT_SIZE),
  strip.text = ggplot2::element_text(size = 10, face = "bold"),
  plot.margin = ggplot2::margin(t = 18, r = 28, b = 18, l = 28, unit = "pt")
)

publicationize_plot <- function(plot) {
  plot + publication_theme_override
}

save_publication_plot <- function(plot,
                                  filename,
                                  folder,
                                  width,
                                  height,
                                  dpi = PUBLICATION_DPI) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  ggplot2::ggsave(
    filename = file.path(folder, filename),
    plot = plot,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
}

# ------------------------------------------------------------
# 2) Main and supplementary figure exports
# ------------------------------------------------------------
# Figure 1 is a flowchart and therefore intentionally has no x/y scales.
if (exists("figure_1_study_flowchart", inherits = TRUE)) {
  figure_1_flowchart_publication <- figure_1_study_flowchart +
    ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::theme_void() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(
        face = "bold",
        hjust = 0.5,
        size = PUBLICATION_TITLE_SIZE,
        margin = ggplot2::margin(b = 8)
      ),
      plot.margin = ggplot2::margin(
        t = 18, r = 28, b = 18, l = 28,
        unit = "pt"
      )
    )

  save_publication_plot(
    figure_1_flowchart_publication,
    filename = "Figure_1_Study_Flowchart.png",
    folder = file.path("outputs", "figures"),
    width = 7,
    height = 6
  )
}

# Named specifications keep object, filename, destination, and dimensions
# readable without relying on positional spec[[n]] indices.
figure_specs <- list(
  list(
    object = "figure_2_weightloss_distribution",
    filename = "Figure_2_Weightloss_Distribution.png",
    folder = "figures",
    width = 7,
    height = 5
  ),
  list(
    object = "figure_3_weightloss_by_feeding",
    filename = "Figure_3_Weightloss_By_Feeding.png",
    folder = "figures",
    width = 7,
    height = 5
  ),
  list(
    object = "appendix_figure_s1_subgroup_forest",
    filename = "Supplementary_Figure_S1_Breastfeeding_Subgroups.png",
    folder = "appendix_figures",
    width = 12,
    height = 7
  ),
  list(
    object = "appendix_figure_s2_historical_exposures",
    filename = "Supplementary_Figure_S2_Historical_Exposures.png",
    folder = "appendix_figures",
    width = 14,
    height = 7.5
  ),
  list(
    object = "appendix_figure_s3_sensitivity_forestplot",
    filename = "Supplementary_Figure_S3_Sensitivity_Forestplot.png",
    folder = "appendix_figures",
    width = 14,
    height = 7.5
  ),
  list(
    object = "appendix_figure_s4_excessive_weightloss",
    filename = "Supplementary_Figure_S4_Excessive_Weightloss_By_Feeding.png",
    folder = "appendix_figures",
    width = 12,
    height = 7
  ),
  list(
    object = "appendix_figure_s5_weightloss_by_year",
    filename = "Supplementary_Figure_S5_Weightloss_By_Year.png",
    folder = "appendix_figures",
    width = 8,
    height = 5
  ),
  list(
    object = "appendix_figure_s6_feeding_by_year",
    filename = "Supplementary_Figure_S6_Feeding_By_Birthyear.png",
    folder = "appendix_figures",
    width = 8,
    height = 5
  ),
  list(
    object = "supplementary_figure_s7_measurement_timing",
    filename = "Supplementary_Figure_S7_Weight_Measurement_Timing.png",
    folder = "appendix_figures",
    width = 9,
    height = 7
  )
)

for (spec in figure_specs) {
  if (!exists(spec$object, inherits = TRUE)) {
    warning("Publication restyle skipped missing figure object: ", spec$object)
    next
  }

  save_publication_plot(
    publicationize_plot(get(spec$object, inherits = TRUE)),
    filename = spec$filename,
    folder = file.path("outputs", spec$folder),
    width = spec$width,
    height = spec$height
  )
}

# Figure 4 is constructed from the reviewer-specific panels prepared in R/11.
# Recomposition here preserves separate beta/OR x-axes while keeping all final
# publication typography and export settings in this one script.
if (exists("figure_4_panel_linear", inherits = TRUE) &&
    exists("figure_4_panel_logistic", inherits = TRUE) &&
    requireNamespace("patchwork", quietly = TRUE)) {

  publication_figure4_panel_theme <- ggplot2::theme(
    plot.title = ggplot2::element_text(
      face = "bold", hjust = 0, size = 11,
      margin = ggplot2::margin(b = 8)
    ),
    axis.title.x = ggplot2::element_text(
      size = PUBLICATION_AXIS_TITLE_SIZE,
      margin = ggplot2::margin(t = 7)
    ),
    axis.text.x = ggplot2::element_text(size = PUBLICATION_AXIS_TEXT_SIZE),
    axis.text.y = ggplot2::element_text(size = PUBLICATION_AXIS_TEXT_SIZE)
  )

  figure_4_panel_linear_publication <-
    figure_4_panel_linear + publication_figure4_panel_theme

  figure_4_panel_logistic_publication <-
    figure_4_panel_logistic + publication_figure4_panel_theme +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank()
    )

  figure_4_adjusted_forest <- patchwork::wrap_plots(
    figure_4_panel_linear_publication,
    figure_4_panel_logistic_publication,
    nrow = 1,
    widths = c(1.35, 1)
  ) +
    patchwork::plot_annotation(
      title = "Figure 4. Adjusted associations with neonatal weight loss",
      subtitle = "Gestational-age-adjusted main models",
      theme = ggplot2::theme(
        plot.title = ggplot2::element_text(
          face = "bold",
          hjust = 0.5,
          size = PUBLICATION_TITLE_SIZE,
          margin = ggplot2::margin(b = 4)
        ),
        plot.subtitle = ggplot2::element_text(
          hjust = 0.5,
          size = PUBLICATION_SUBTITLE_SIZE,
          margin = ggplot2::margin(b = 8)
        ),
        plot.margin = ggplot2::margin(t = 8, r = 10, b = 6, l = 10, unit = "pt")
      )
    )

  save_publication_plot(
    figure_4_adjusted_forest,
    filename = "Figure_4_Adjusted_Associations_Forestplot.png",
    folder = file.path("outputs", "figures"),
    width = 11.2,
    height = 6.5
  )
}

# ------------------------------------------------------------
# 3) Shared GT table styling
# ------------------------------------------------------------
build_publication_gt <- function(data,
                                 title,
                                 groupname_col = NULL,
                                 highlight_sections = FALSE,
                                 font_size = PUBLICATION_TABLE_BODY_SIZE) {
  if (is.null(groupname_col)) {
    tbl <- gt::gt(data)
    visible_columns <- names(data)
  } else {
    tbl <- gt::gt(data, groupname_col = groupname_col)
    visible_columns <- setdiff(names(data), groupname_col)
  }

  tbl <- tbl %>%
    gt::tab_header(title = title) %>%
    gt::tab_options(
      table.font.size = gt::px(font_size),
      heading.title.font.size = gt::px(PUBLICATION_TABLE_TITLE_SIZE),
      heading.title.font.weight = "bold",
      heading.align = "left",
      heading.padding = gt::px(8),
      heading.background.color = PUBLICATION_TABLE_HEADER_BG,
      table.width = gt::pct(100),
      table.background.color = "white",
      data_row.padding = gt::px(4),
      column_labels.padding = gt::px(6),
      column_labels.font.weight = "bold",
      column_labels.background.color = PUBLICATION_TABLE_COLUMN_BG,
      table.border.top.width = gt::px(1.2),
      table.border.bottom.width = gt::px(1.2),
      table.border.top.color = "#222222",
      table.border.bottom.color = "#222222",
      heading.border.bottom.width = gt::px(0),
      column_labels.border.top.width = gt::px(0),
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "#555555",
      row_group.font.weight = "bold",
      row_group.background.color = PUBLICATION_TABLE_SECTION_BG,
      row_group.padding = gt::px(5),
      source_notes.font.size = gt::px(8)
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        color = "#E5E5E5",
        weight = gt::px(0.5)
      ),
      locations = gt::cells_body()
    )

  if (length(visible_columns) >= 1) {
    tbl <- tbl %>%
      gt::cols_align(
        align = "left",
        columns = dplyr::all_of(visible_columns[1])
      )
  }

  if (length(visible_columns) >= 2) {
    tbl <- tbl %>%
      gt::cols_align(
        align = "center",
        columns = dplyr::all_of(visible_columns[-1])
      )
  }

  if (highlight_sections && "Characteristic" %in% visible_columns) {
    main_section_rows <- c(
      "Continuous characteristics",
      "Categorical characteristics",
      "Outcome"
    )
    subsection_rows <- c(
      "Parity",
      "Historical exposures",
      "Neonatal characteristics",
      "Feeding characteristics"
    )

    tbl <- tbl %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_fill(color = PUBLICATION_TABLE_SECTION_BG)
        ),
        locations = gt::cells_body(
          columns = dplyr::any_of(c("Characteristic", "Value")),
          rows = Characteristic %in% main_section_rows
        )
      ) %>%
      gt::tab_style(
        style = gt::cell_text(weight = "bold", style = "italic"),
        locations = gt::cells_body(
          columns = Characteristic,
          rows = Characteristic %in% subsection_rows
        )
      )
  }

  tbl
}

# ------------------------------------------------------------
# 4) Content-aware table width
# ------------------------------------------------------------
estimate_publication_table_width <- function(data,
                                             groupname_col = NULL,
                                             max_width = 1600) {
  visible_columns <- if (is.null(groupname_col)) {
    names(data)
  } else {
    setdiff(names(data), groupname_col)
  }

  if (length(visible_columns) == 0) {
    return(min(max_width, 700))
  }

  estimate_column_px <- function(column_name) {
    values <- as.character(data[[column_name]])
    values <- values[!is.na(values)]
    longest <- max(
      nchar(c(column_name, values), type = "width"),
      na.rm = TRUE
    )

    if (identical(column_name, "Characteristic")) {
      # Descriptive labels may wrap rather than forcing a very wide table.
      longest <- min(longest, 52)
      return(max(190, min(390, 24 + longest * 6.6)))
    }

    # Result/value columns stay compact but remain large enough for their data.
    longest <- min(longest, 32)
    max(95, min(230, 24 + longest * 6.4))
  }

  estimated_width <- sum(vapply(
    visible_columns,
    estimate_column_px,
    numeric(1)
  )) + 50

  max_width <- if (is.null(max_width) || !is.finite(max_width)) {
    1600
  } else {
    max_width
  }

  max(520, min(ceiling(estimated_width), max_width))
}

save_publication_table <- function(data,
                                   title,
                                   filename,
                                   folder,
                                   width,
                                   height,
                                   groupname_col = NULL,
                                   highlight_sections = FALSE,
                                   font_size = PUBLICATION_TABLE_BODY_SIZE) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  tbl <- build_publication_gt(
    data = data,
    title = title,
    groupname_col = groupname_col,
    highlight_sections = highlight_sections,
    font_size = font_size
  )

  render_width <- estimate_publication_table_width(
    data = data,
    groupname_col = groupname_col,
    max_width = width
  )

  gt::gtsave(
    data = tbl,
    filename = file.path(folder, filename),
    vwidth = render_width,
    vheight = height,
    expand = 20
  )

  invisible(tbl)
}

# ------------------------------------------------------------
# 5) Main table exports
# ------------------------------------------------------------
if (exists("table_1_population", inherits = TRUE)) {
  save_publication_table(
    table_1_population,
    "Table 1. Characteristics of the analytical study population",
    "Table_1_Cohort_Characteristics.png",
    file.path("outputs", "tables_png"),
    width = 1200,
    height = 1200,
    highlight_sections = TRUE
  )
}

if (exists("table_MX_linear_period_GA_clean", inherits = TRUE)) {
  save_publication_table(
    round_numeric_df(table_MX_linear_period_GA_clean),
    "Table 2. Linear regression: maximum neonatal weight loss, gestational-age-adjusted main model",
    "Table_2_Linear_GA_Main.png",
    file.path("outputs", "tables_png"),
    width = 1400,
    height = 1100
  )
}

if (exists("table_MX_logistic_period_GA_clean", inherits = TRUE)) {
  save_publication_table(
    round_numeric_df(table_MX_logistic_period_GA_clean),
    "Table 3. Logistic regression: high neonatal weight loss >10%, gestational-age-adjusted main model",
    "Table_3_Logistic_GA_Main.png",
    file.path("outputs", "tables_png"),
    width = 1400,
    height = 1100
  )
}

# ------------------------------------------------------------
# 6) Supplementary table exports S1-S10
# ------------------------------------------------------------
# Remove the obsolete pre-renumbering S9 file if it is still present locally.
stale_s9_sensitivity_file <- file.path(
  "outputs",
  "appendix_tables",
  "Supplementary_Table_S9_Exact_Day5_Day10_Sensitivity.png"
)
if (file.exists(stale_s9_sensitivity_file)) {
  unlink(stale_s9_sensitivity_file)
}

supplementary_table_specs <- list(
  list(
    object = "Supplementary_Table_S7_Excluded_vs_Included",
    title = paste0(
      "Supplementary Table S1. Characteristics of included infants and infants ",
      "excluded because of non-evaluable neonatal weight trajectories"
    ),
    filename = "Supplementary_Table_S1_Excluded_vs_Included.png",
    width = 1500,
    height = 1200,
    groupname_col = NULL,
    highlight_sections = TRUE,
    font_size = 9
  ),
  list(
    object = "table_6_feeding_allocation",
    title = "Supplementary Table S2. Maternal and neonatal characteristics according to feeding mode",
    filename = "Supplementary_Table_S2_Feeding_Allocation.png",
    width = 1500,
    height = 1250,
    groupname_col = NULL,
    highlight_sections = FALSE,
    font_size = 9
  ),
  list(
    object = "appendix_table_S3_direct_flu_GA",
    title = "Supplementary Table S3. Direct maternal influenza model, gestational-age-adjusted",
    filename = "Supplementary_Table_S3_Direct_Maternal_Flu_GA.png",
    width = 1450,
    height = 1100,
    groupname_col = NULL,
    highlight_sections = FALSE,
    font_size = 9
  ),
  list(
    object = "appendix_table_S4_flu_in_pregn_and_pandemic_GA",
    title = "Supplementary Table S4. Influenza in pregnancy during pandemic model, gestational-age-adjusted",
    filename = "Supplementary_Table_S4_Flu_Pregnancy_Pandemic_GA.png",
    width = 1450,
    height = 1100,
    groupname_col = NULL,
    highlight_sections = FALSE,
    font_size = 9
  ),
  list(
    object = "table_MX_linear_period_BW",
    title = "Supplementary Table S5. Birthweight-adjusted sensitivity analysis: maximum neonatal weight loss",
    filename = "Supplementary_Table_S5_BW_Linear_Supplementary.png",
    width = 1450,
    height = 1100,
    groupname_col = NULL,
    highlight_sections = FALSE,
    font_size = 9
  ),
  list(
    object = "table_MX_logistic_period_BW",
    title = "Supplementary Table S6. Birthweight-adjusted sensitivity analysis: high neonatal weight loss >10%",
    filename = "Supplementary_Table_S6_BW_Logistic_Supplementary.png",
    width = 1450,
    height = 1100,
    groupname_col = NULL,
    highlight_sections = FALSE,
    font_size = 9
  ),
  list(
    object = "Supplementary_Table_S6_Subgroup_Analyses",
    title = "Supplementary Table S7. Stratified breastfeeding subgroup analyses with Cochran-Q heterogeneity tests",
    filename = "Supplementary_Table_S7_Subgroup_Analyses.png",
    width = 1600,
    height = 1500,
    groupname_col = NULL,
    highlight_sections = FALSE,
    font_size = 9
  ),
  list(
    object = "supplementary_table_s8_measurement_completeness",
    title = paste0(
      "Supplementary Table S8. Maternal and neonatal characteristics according ",
      "to availability of one versus two postnatal weight measurements"
    ),
    filename = "Supplementary_Table_S8_One_vs_Two_Weight_Measurements.png",
    width = 1650,
    height = 1200,
    groupname_col = NULL,
    highlight_sections = FALSE,
    font_size = 9
  ),
  list(
    object = "supplementary_table_s9_exact_vs_other",
    title = paste0(
      "Supplementary Table S9. Maternal and neonatal characteristics of infants ",
      "with postnatal weights measured exactly on days 5 and 10 versus all other ",
      "measurement patterns"
    ),
    filename = "Supplementary_Table_S9_Exact_Day5_Day10_vs_Other_Patterns.png",
    width = 1800,
    height = 1250,
    groupname_col = NULL,
    highlight_sections = FALSE,
    font_size = 9
  ),
  list(
    object = "supplementary_table_s10_exact_day_sensitivity",
    title = paste0(
      "Supplementary Table S10. Sensitivity analysis restricted to infants with ",
      "postnatal weights measured exactly on days 5 and 10"
    ),
    filename = "Supplementary_Table_S10_Exact_Day5_Day10_Sensitivity.png",
    width = 1900,
    height = 1650,
    groupname_col = "Outcome",
    highlight_sections = FALSE,
    font_size = 8
  )
)

for (spec in supplementary_table_specs) {
  if (!exists(spec$object, inherits = TRUE)) {
    warning("Publication restyle skipped missing table object: ", spec$object)
    next
  }

  save_publication_table(
    data = round_numeric_df(get(spec$object, inherits = TRUE)),
    title = spec$title,
    filename = spec$filename,
    folder = file.path("outputs", "appendix_tables"),
    width = spec$width,
    height = spec$height,
    groupname_col = spec$groupname_col,
    highlight_sections = spec$highlight_sections,
    font_size = spec$font_size
  )
}

message(
  "Publication styling pass complete: final figures and Tables S1-S10 exported from R/12."
)
