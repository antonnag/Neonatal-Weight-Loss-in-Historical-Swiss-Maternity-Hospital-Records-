# ============================================================
# 11_figure4_reviewer_update.R
# AJHB reviewer revision: Figure 4 panel construction
# ============================================================
# Reviewer request: distinguish the linear-model beta coefficient from the
# logistic-model odds ratio with panel-specific x-axis labels.
#
# Responsibility of this file:
#   - prepare the two aligned Figure 4 panels with the correct axis semantics.
#
# Final composition, publication styling, and PNG export are centralized in
# R/12_publication_restyle_outputs.R. Model estimates are unchanged.

# ------------------------------------------------------------
# 0) Required objects
# ------------------------------------------------------------
required_figure4_objects <- c(
  "main_linear_forest",
  "main_logistic_forest",
  "forest_order"
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

# ------------------------------------------------------------
# 1) Panel data
# ------------------------------------------------------------
figure_4_linear_data <- main_linear_forest %>%
  mutate(term = factor(term, levels = rev(forest_order)))

figure_4_logistic_data <- main_logistic_forest %>%
  mutate(term = factor(term, levels = rev(forest_order)))

# ------------------------------------------------------------
# 2) Shared panel theme
# ------------------------------------------------------------
figure_4_panel_theme <- theme_classic(base_size = 11) +
  theme(
    plot.title = element_text(
      face = "bold", hjust = 0, size = 11,
      margin = margin(b = 8)
    ),
    axis.title.x = element_text(size = 10.5, margin = margin(t = 7)),
    axis.text.x = element_text(size = 9.5),
    axis.text.y = element_text(size = 9.5),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_line(colour = "grey92", linewidth = 0.35),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    plot.margin = margin(t = 6, r = 12, b = 8, l = 6, unit = "pt")
  )

# ------------------------------------------------------------
# 3) Panel A: linear model
# ------------------------------------------------------------
# Variable labels appear only here and act as shared row labels for both panels.
figure_4_panel_linear <- ggplot(
  figure_4_linear_data,
  aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)
) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.55) +
  geom_errorbarh(height = 0, linewidth = 0.55) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = pretty(
      range(
        c(figure_4_linear_data$conf.low, figure_4_linear_data$conf.high),
        na.rm = TRUE
      ),
      n = 5
    ),
    expand = expansion(mult = c(0.07, 0.07))
  ) +
  labs(
    title = "A. Maximum neonatal weight loss (%)",
    x = "Estimate (β)"
  ) +
  figure_4_panel_theme +
  theme(
    plot.margin = margin(t = 6, r = 16, b = 8, l = 8, unit = "pt")
  )

# ------------------------------------------------------------
# 4) Panel B: logistic model
# ------------------------------------------------------------
# Duplicated row labels are suppressed while y positions remain aligned.
figure_4_panel_logistic <- ggplot(
  figure_4_logistic_data,
  aes(x = estimate, y = term, xmin = conf.low, xmax = conf.high)
) +
  geom_vline(xintercept = 1, linetype = "dashed", linewidth = 0.55) +
  geom_errorbarh(height = 0, linewidth = 0.55) +
  geom_point(size = 2.5) +
  scale_x_continuous(
    breaks = pretty(
      range(
        c(figure_4_logistic_data$conf.low, figure_4_logistic_data$conf.high),
        na.rm = TRUE
      ),
      n = 5
    ),
    expand = expansion(mult = c(0.07, 0.07))
  ) +
  labs(
    title = "B. Neonatal weight loss >10%",
    x = "Odds ratio (OR)"
  ) +
  figure_4_panel_theme +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank(),
    plot.margin = margin(t = 6, r = 8, b = 8, l = 2, unit = "pt")
  )

message(
  "Figure 4 reviewer panels prepared; final composition and export are handled in R/12_publication_restyle_outputs.R."
)
