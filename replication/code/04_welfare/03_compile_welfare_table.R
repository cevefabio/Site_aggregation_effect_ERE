############################################################
## Compile Table 2 welfare estimates
############################################################

rm(list = ls())

library(openxlsx)

results_file <- file.path(
  "output", "welfare", "post_estimation", "welfare_results.rds"
)
if (!file.exists(results_file)) {
  stop("Run code/04_welfare/02_estimate_welfare.R first.")
}

results <- readRDS(results_file)
summary_rows <- vector("list", length(results))

for (result_number in seq_along(results)) {
  result_name <- names(results)[result_number]
  result <- results[[result_number]]
  name_parts <- strsplit(result_name, "_")[[1]]

  model_type <- name_parts[1]
  coverage <- name_parts[2]
  scenario <- paste(name_parts[-c(1, 2)], collapse = "_")

  # Water-restriction calculations are utility changes and are negative when
  # quality falls. Table 2 reports their absolute value as a welfare loss.
  sign_multiplier <- ifelse(scenario == "bwquality", -1, 1)
  loss_draws <- sign_multiplier * as.numeric(result$draws)
  loss_point <- sign_multiplier * result$point

  summary_rows[[result_number]] <- data.frame(
    model_type = model_type,
    coverage = coverage,
    scenario = scenario,
    mc_draws = length(loss_draws),
    point_million_total = loss_point,
    mean_million_total = mean(loss_draws, na.rm = TRUE),
    ci_lower_million_total = unname(
      quantile(loss_draws, 0.025, na.rm = TRUE)
    ),
    ci_upper_million_total = unname(
      quantile(loss_draws, 0.975, na.rm = TRUE)
    ),
    point_million_annual = loss_point / 5,
    mean_million_annual = mean(loss_draws, na.rm = TRUE) / 5,
    ci_lower_million_annual = unname(
      quantile(loss_draws, 0.025, na.rm = TRUE)
    ) / 5,
    ci_upper_million_annual = unname(
      quantile(loss_draws, 0.975, na.rm = TRUE)
    ) / 5
  )
}

welfare_summary <- do.call(rbind, summary_rows)

tables_dir <- file.path("output", "tables")
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

write.xlsx(
  list(summary = welfare_summary),
  file.path(tables_dir, "paper_welfare_estimates.xlsx"),
  overwrite = TRUE
)

print(welfare_summary)
