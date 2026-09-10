# ============================================================
# 01_setup_packages_paths.R
# Packages, paths, output folders, and helper functions
# ============================================================

required_packages <- c(
  "readr", "dplyr", "tidyr", "ggplot2", "janitor", "broom",
  "forcats", "stringr", "writexl", "gt", "webshot2", "lubridate"
)

missing_packages <- required_packages[!required_packages %in% rownames(installed.packages())]
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

invisible(lapply(required_packages, library, character.only = TRUE))

# File paths
path_raw_data <- file.path("data_raw", "laus_cleaned_2024-09-06.csv")
path_processed_data <- file.path("data_processed", "df_analysis_clean.rds")

# Output folders
output_dirs <- c(
  "data_raw",
  "data_processed",
  file.path("outputs", "figures"),
  file.path("outputs", "tables_png"),
  file.path("outputs", "tables_excel"),
  file.path("outputs", "appendix_figures"),
  file.path("outputs", "appendix_tables"),
  file.path("outputs", "model_summaries")
)

for (dir in output_dirs) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
}

# Helper: format p values consistently
format_p_value <- function(p) {
  ifelse(p < 0.001, "<0.001", as.character(round(p, 3)))
}


# Helper: robust date parser for historical date fields.
# The raw CSV may import dates either as character strings or already as Date objects.
parse_historical_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  x_chr <- as.character(x)
  parsed <- suppressWarnings(as.Date(x_chr))
  if (all(is.na(parsed)) && requireNamespace("lubridate", quietly = TRUE)) {
    parsed <- suppressWarnings(lubridate::ymd(x_chr))
  }
  return(parsed)
}

# Helper: publication-style gt tables.
# A single neutral layout is used for main and supplementary tables so that
# all exported tables have consistent typography, spacing and alignment.
build_journal_gt <- function(data,
                             title,
                             font_size = 10,
                             groupname_col = NULL,
                             source_note = NULL) {
  if (is.null(groupname_col)) {
    gt_table <- gt::gt(data)
  } else {
    gt_table <- gt::gt(data, groupname_col = groupname_col)
  }

  gt_table <- gt_table %>%
    gt::tab_header(title = title) %>%
    gt::tab_options(
      table.font.size = gt::px(font_size),
      heading.title.font.size = gt::px(font_size + 1),
      heading.title.font.weight = "bold",
      heading.align = "left",
      heading.padding = gt::px(8),
      table.width = gt::pct(100),
      table.background.color = "white",
      data_row.padding = gt::px(5),
      column_labels.padding = gt::px(6),
      column_labels.font.weight = "bold",
      column_labels.background.color = "#F3F3F3",
      table.border.top.width = gt::px(1.2),
      table.border.bottom.width = gt::px(1.2),
      table.border.top.color = "#222222",
      table.border.bottom.color = "#222222",
      heading.border.bottom.width = gt::px(0),
      column_labels.border.top.width = gt::px(0),
      column_labels.border.bottom.width = gt::px(1),
      column_labels.border.bottom.color = "#555555",
      row_group.font.weight = "bold",
      row_group.padding = gt::px(5),
      source_notes.font.size = gt::px(max(font_size - 1, 8))
    ) %>%
    gt::tab_style(
      style = gt::cell_borders(
        sides = "bottom",
        color = "#E5E5E5",
        weight = gt::px(0.5)
      ),
      locations = gt::cells_body()
    )

  if (ncol(data) >= 1) {
    gt_table <- gt_table %>%
      gt::cols_align(align = "left", columns = 1)
  }
  if (ncol(data) >= 2) {
    gt_table <- gt_table %>%
      gt::cols_align(align = "center", columns = 2:ncol(data))
  }

  if ("Characteristic" %in% names(data)) {
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

    gt_table <- gt_table %>%
      gt::tab_style(
        style = list(
          gt::cell_text(weight = "bold"),
          gt::cell_fill(color = "#F7F7F7")
        ),
        locations = gt::cells_body(
          columns = Characteristic,
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

  if (!is.null(source_note) && nzchar(source_note)) {
    gt_table <- gt_table %>%
      gt::tab_source_note(source_note = source_note)
  }

  gt_table
}

export_gt_png <- function(data,
                          title,
                          filename,
                          folder = file.path("outputs", "tables_png"),
                          width = 1200,
                          height = 1000,
                          font_size = 10,
                          groupname_col = NULL,
                          source_note = NULL) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  gt_table <- build_journal_gt(
    data = data,
    title = title,
    font_size = font_size,
    groupname_col = groupname_col,
    source_note = source_note
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

FIGURE_DPI <- 600

FIGURE_WIDTH_STANDARD <- 8

FIGURE_HEIGHT_STANDARD <- 5

FIGURE_WIDTH_WIDE <- 14

FIGURE_HEIGHT_WIDE <- 8

FIGURE_WIDTH_TALL <- 14

FIGURE_HEIGHT_TALL <- 9

TABLE_SMALL <- 1200

TABLE_MEDIUM <- 1400

TABLE_WIDE <- 1600

TABLE_TALL <- 1800


# Helper: export ggplot figures with paper-friendly margins and no clipping
export_figure <- function(plot,
                          filename,
                          folder,
                          width = FIGURE_WIDTH_STANDARD,
                          height = FIGURE_HEIGHT_STANDARD,
                          dpi = FIGURE_DPI) {
  dir.create(folder, recursive = TRUE, showWarnings = FALSE)

  plot_to_save <- plot +
    theme(
      plot.margin = ggplot2::margin(t = 18, r = 28, b = 18, l = 28, unit = "pt")
    )

  ggsave(
    filename = file.path(folder, filename),
    plot = plot_to_save,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white",
    limitsize = FALSE
  )
}


# Helper: round all numeric columns to 3 decimals
round_numeric_df <- function(df, digits = 3) {
  dplyr::mutate(df, dplyr::across(where(is.numeric), ~ round(.x, digits)))
}
