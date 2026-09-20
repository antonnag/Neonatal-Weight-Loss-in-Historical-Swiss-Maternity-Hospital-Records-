# ============================================================
# 13_fix_figure1_flowchart.R
# Final Figure 1-only presentation fix
# ============================================================
# Figure 1 is a flowchart, not an x/y data display. The global publication
# override intentionally remains unchanged for all other figures. This script
# is sourced last and re-exports only Figure 1 with axes fully suppressed.

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
      ),
      axis.title = ggplot2::element_blank(),
      axis.text = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    filename = file.path("outputs", "figures", "Figure_1_Study_Flowchart.png"),
    plot = figure_1_flowchart_publication,
    width = 7,
    height = 6,
    dpi = PUBLICATION_DPI,
    bg = "white",
    limitsize = FALSE
  )

  message("Figure 1 re-exported as an axis-free flowchart; all other figure styling unchanged.")
}
