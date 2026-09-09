# ============================================================
# 11_figure4_reviewer_update.R
# AJHB reviewer revision: separate x-axis labels for Figure 4 panels
# ============================================================
# Reviewer request: distinguish the linear-model beta coefficient from the
# logistic-model odds ratio with panel-specific x-axis labels.
# This file changes presentation only; model estimates are unchanged.

required_figure4_objects <- c(
  "main_linear_forest",
  "main_logistic_forest",
  "forest_order",
  "paper_forest_theme"
)

missing_figure4_objects <- required_figure4_objects[
  !vapply(required_figure4_objects, exists, logical(1), inherits = TRUE)
]

if (length(missing_figure4_objects) > 0) {
  stop(
    "Figure 4 reviewer update cannot run because required objects are missing: ",
    paste(missing_figure4_objects, collapse = ", ")
  )
}

figure_4_linear_data <- main_linear_forest %>%
  mutate(term = factor(term, levels = rev(forest_order)))

figure_4_logistic_data <- main_logistic_forest %>%
  mutate(term = factor(term, levels = rev(forest_order)))

# Left panel carries the shared variable labels.
figure_4_panel_linear <- ggplot(
  figure_4_linear_data,
  aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)
) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_pointrange() +
  labs(
    title = "A. Maximum NWL (%)",
    x = "Estimate (β)",
    y = NULL
  ) +
  paper_forest_theme() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10.5, margin = margin(b = 6)),
    axis.text.y = element_text(size = 8.5),
    plot.margin = margin(t = 8, r = 14, b = 14, l = 18, unit = "pt")
  )

# Right panel shares the same variables and therefore suppresses duplicated
# y-axis labels. This leaves substantially more horizontal room for the ORs.
figure_4_panel_logistic <- ggplot(
  figure_4_logistic_data,
  aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)
) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  geom_pointrange() +
  labs(
    title = "B. Excessive NWL >10%",
    x = "Odds ratio (OR)",
    y = NULL
  ) +
  paper_forest_theme() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 10.5, margin = margin(b = 6)),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = margin(t = 8, r = 18, b = 14, l = 6, unit = "pt")
  )

figure_4_output <- file.path(
  "outputs",
  "figures",
  "Figure_4_Adjusted_Associations_Forestplot.png"
)

dir.create(dirname(figure_4_output), recursive = TRUE, showWarnings = FALSE)

png(
  filename = figure_4_output,
  width = 11.5,
  height = 6.2,
  units = "in",
  res = 300,
  bg = "white"
)

grid::grid.newpage()
figure_4_layout <- grid::grid.layout(
  nrow = 3,
  ncol = 2,
  heights = grid::unit(c(0.075, 0.055, 0.87), "npc"),
  widths = grid::unit(c(0.58, 0.42), "npc")
)
grid::pushViewport(grid::viewport(layout = figure_4_layout))

grid::grid.text(
  "Figure 4. Adjusted associations with neonatal weight loss",
  vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1:2),
  gp = grid::gpar(fontface = "bold", fontsize = 12)
)

grid::grid.text(
  "Gestational-age-adjusted main models",
  vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1:2),
  gp = grid::gpar(fontsize = 10)
)

print(
  figure_4_panel_linear,
  vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 1)
)
print(
  figure_4_panel_logistic,
  vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 2)
)

grid::popViewport()
dev.off()

message(
  "Figure 4 updated with panel-specific x-axis labels and a shared variable axis."
)
