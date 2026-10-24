############################################################
#### FIGURE 5: ANNUAL WELFARE LOSSES FROM BW EVENTS ########
############################################################

rm(list = ls())

library(tidyverse)
library(openxlsx)

results_path <- file.path("output", "welfare", "post_estimation", "welfare_results.rds")
if (!file.exists(results_path)) {
  stop("Welfare results not found: ", results_path)
}

dir.create(file.path("output", "figures"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path("output", "tables"), recursive = TRUE, showWarnings = FALSE)

results <- readRDS(results_path)

parse_result_name <- function(name) {
  parts <- strsplit(name, "_")[[1]]
  tibble(
    model_code = parts[1],
    coverage_code = parts[2],
    scenario_code = paste(parts[-c(1, 2)], collapse = "_")
  )
}

format_million_label <- function(x) {
  value <- trimws(format(abs(round(x, 1)), nsmall = 1))
  ifelse(x < 0, paste0("-$", value, "M/yr"), paste0("$", value, "M/yr"))
}

plot_data <- purrr::map2_dfr(results, names(results), function(res, name) {
  meta <- parse_result_name(name)
  sign_multiplier <- ifelse(meta$scenario_code == "bwquality", -1, 1)
  annual_loss_draws <- sign_multiplier * as.numeric(res$draws) / 5

  meta |>
    mutate(
      mc_draws = length(annual_loss_draws),
      point = sign_multiplier * as.numeric(res$point) / 5,
      mean = mean(annual_loss_draws, na.rm = TRUE),
      ci_lower = unname(quantile(annual_loss_draws, 0.025, na.rm = TRUE)),
      ci_upper = unname(quantile(annual_loss_draws, 0.975, na.rm = TRUE))
    )
})

draw_data <- purrr::map2_dfr(results, names(results), function(res, name) {
  meta <- parse_result_name(name)
  sign_multiplier <- ifelse(meta$scenario_code == "bwquality", -1, 1)
  annual_loss_draws <- sign_multiplier * as.numeric(res$draws) / 5

  meta |>
    slice(rep(1, length(annual_loss_draws))) |>
    mutate(draw = annual_loss_draws)
})

plot_data <- plot_data |>
  mutate(
    model = recode(
      model_code,
      CL = "Multinomial logit",
      FE2 = "Two-stage"
    ),
    coverage = recode(
      coverage_code,
      full = "All sites",
      aggregated = "Aggregated sites"
    ),
    scenario = recode(
      scenario_code,
      siteclosure = "Site closure",
      bwquality = "Water restriction"
    ),
    model = factor(model, levels = c("Multinomial logit", "Two-stage")),
    coverage = factor(coverage, levels = c("All sites", "Aggregated sites")),
    scenario = factor(scenario, levels = c("Site closure", "Water restriction")),
    label = format_million_label(point)
  )

draw_data <- draw_data |>
  mutate(
    model = recode(
      model_code,
      CL = "Multinomial logit",
      FE2 = "Two-stage"
    ),
    coverage = recode(
      coverage_code,
      full = "All sites",
      aggregated = "Aggregated sites"
    ),
    scenario = recode(
      scenario_code,
      siteclosure = "Site closure",
      bwquality = "Water restriction"
    ),
    model = factor(model, levels = c("Multinomial logit", "Two-stage")),
    coverage = factor(coverage, levels = c("All sites", "Aggregated sites")),
    scenario = factor(scenario, levels = c("Site closure", "Water restriction"))
  )

figure5_table_path <- file.path("output", "tables", "figure5_welfare_simulation_data.xlsx")
figure5_table_fallback <- file.path(
  "output", "tables", "figure5_welfare_simulation_data_loss.xlsx"
)

table_written <- tryCatch({
  write.xlsx(plot_data, file = figure5_table_path, overwrite = TRUE)
  TRUE
}, warning = function(w) {
  message("Could not overwrite ", figure5_table_path, ": ", conditionMessage(w))
  FALSE
}, error = function(e) {
  message("Could not overwrite ", figure5_table_path, ": ", conditionMessage(e))
  FALSE
})

if (!table_written) {
  write.xlsx(plot_data, file = figure5_table_fallback, overwrite = TRUE)
}

boxplot_data <- draw_data |>
  group_by(model, coverage, scenario) |>
  summarise(
    ymin = unname(quantile(draw, 0.025, na.rm = TRUE)),
    lower = unname(quantile(draw, 0.25, na.rm = TRUE)),
    middle = median(draw, na.rm = TRUE),
    upper = unname(quantile(draw, 0.75, na.rm = TRUE)),
    ymax = unname(quantile(draw, 0.975, na.rm = TRUE)),
    .groups = "drop"
  )

scenario_colors <- c(
  "Site closure" = "steelblue",
  "Water restriction" = "darkorange"
)

figure5 <- ggplot(plot_data, aes(x = coverage, y = point, color = scenario)) +
  geom_errorbar(
    aes(ymin = ci_lower, ymax = ci_upper),
    width = 0.12,
    linewidth = 0.8,
    position = position_dodge(width = 0.45)
  ) +
  geom_point(
    size = 3,
    position = position_dodge(width = 0.45)
  ) +
  geom_text(
    aes(label = label),
    position = position_dodge(width = 0.45),
    vjust = -1.05,
    size = 3.4,
    show.legend = FALSE
  ) +
  facet_grid(cols = vars(model), scales = "free_y") +
  scale_color_manual(values = scenario_colors) +
  scale_y_continuous(
    labels = function(x) paste0("$", x, "M"),
    expand = expansion(mult = c(0.08, 0.22))
  ) +
  labs(
    x = NULL,
    y = "Annual welfare loss (million USD per year)",
    color = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    panel.spacing.x = unit(1.5, "lines"),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey30")
  )

ggsave(
  file.path("output", "figures", "figure5_welfare_simulations.jpeg"),
  figure5,
  width = 10,
  height = 5.5,
  dpi = 320
)

ggsave(
  file.path("output", "figures", "figure5_welfare_simulations.pdf"),
  figure5,
  width = 10,
  height = 5.5
)

figure5_boxplot <- ggplot(
  boxplot_data,
  aes(x = coverage, fill = scenario, color = scenario)
) +
  geom_boxplot(
    aes(
      ymin = ymin,
      lower = lower,
      middle = middle,
      upper = upper,
      ymax = ymax
    ),
    stat = "identity",
    width = 0.42,
    position = position_dodge(width = 0.65),
    alpha = 0.28,
    linewidth = 0.75,
    outlier.shape = NA
  ) +
  geom_point(
    data = plot_data,
    aes(x = coverage, y = point, color = scenario),
    position = position_dodge(width = 0.65),
    size = 2.2,
    inherit.aes = FALSE
  ) +
  facet_grid(cols = vars(model), scales = "free_y") +
  scale_color_manual(values = scenario_colors) +
  scale_fill_manual(values = scenario_colors) +
  scale_y_continuous(
    labels = function(x) paste0("$", x, "M"),
    expand = expansion(mult = c(0.08, 0.15))
  ) +
  labs(
    x = NULL,
    y = "Annual welfare loss (million USD per year)",
    color = NULL,
    fill = NULL,
    caption = "Boxes show the interquartile range; whiskers show 95% Monte Carlo intervals. Dots show point estimates."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    strip.text = element_text(face = "bold", size = 13),
    axis.text.x = element_text(angle = 35, hjust = 1),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "top",
    panel.spacing.x = unit(1.5, "lines"),
    panel.border = element_rect(color = "grey80", fill = NA, linewidth = 0.5),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey30")
  )

ggsave(
  file.path("output", "figures", "figure5_welfare_simulations_boxplot.jpeg"),
  figure5_boxplot,
  width = 10,
  height = 5.5,
  dpi = 320
)

ggsave(
  file.path("output", "figures", "figure5_welfare_simulations_boxplot.pdf"),
  figure5_boxplot,
  width = 10,
  height = 5.5
)

print(figure5)
print(figure5_boxplot)
