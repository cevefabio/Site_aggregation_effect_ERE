# Plot Monte Carlo results from the aggregated-OOR simulation
# =============================================================================
#
# This script does not run the simulation. It reads the RDS outputs produced by:
#
#   02_run_monte_carlo_aggregation.R
#
# and creates plots in the same output folder.
#
# This separation makes it easier to customize figures without rerunning the
# Monte Carlo simulation.

rm(list = ls())

output_dir <- file.path("output", "models", "monte_carlo_outputs_aggregated_only")
summary_file <- file.path(output_dir, "mc_summary_aggregated_only.rds")

if (!file.exists(summary_file)) {
  stop("Summary file not found: ", summary_file)
}

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  stop("Package 'ggplot2' is required to produce plots.")
}

mc_summary <- readRDS(summary_file)

plot_dir <- file.path(output_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

plot_data <- mc_summary

### Plot the average bias

library(ggplot2)

plot_data$share_label <- factor(
  plot_data$share_aggregated_choices,
  levels = c(0.05, 0.15, 0.25),
  labels = c("5%", "15%", "25%")
)

plot_data$lambda_label <- factor(
  plot_data$lambda_substitution,
  levels = c(0, 1, 2),
  labels = c("0", "1", "2")
)

plot_data$heterogeneity_label <- factor(
  plot_data$within_A_resp_sd,
  levels = c(0, 1, 2),
  labels = c("σ = 0", "σ = 1", "σ = 2")
)

p <- ggplot(
  plot_data,
  aes(
    x = share_label,
    y = lambda_label,
    fill = 100 * average_bias
  )
) +
  geom_tile(color = "white", linewidth = 0.4) +
  geom_text(
    aes(label = round(100 * average_bias, 1)),
    size = 3
  ) +
  facet_wrap(~ heterogeneity_label, nrow = 1) +
  coord_fixed(ratio = 1) +
  scale_y_discrete(limits = rev) +
  scale_fill_gradient(
    low = "#d7191c",
    high = "#ffffbf"
  ) +
  labs(
    title = "Average bias (%)",
    x = "Choice share of peripheral sites",
    y = expression(paste("Non-separability (", lambda, ")")),
    fill = "Average bias (%)"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid = element_blank(),
    
    axis.text.x = element_text(
      angle = 0,
      hjust = 0.5
    ),
    
    # Titolo principale più grande
    plot.title = element_text(
      size = 15,
      face = "bold",
      hjust = 0.5
    ),
    
    # Titoli degli assi più piccoli
    axis.title.x = element_text(
      size = 10,
      margin = margin(t = 10)
    ),
    axis.title.y = element_text(
      size = 10
    ),
    
    # Stessa dimensione dei titoli degli assi
    strip.text = element_text(
      size = 10
    ),
    
    legend.position = "none"
  )

p
ggsave(
  filename = file.path(plot_dir, "figure1_average_bias_heatmap.png"),
  plot = p,
  width = 6,
  height = 2.4,
  units = "in",
  dpi = 300,
  bg = "white"
)
