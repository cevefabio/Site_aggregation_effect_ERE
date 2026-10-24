# Monte Carlo simulation: aggregated-OOR estimator only
# =============================================================================
#
# This script runs the Monte Carlo simulation and saves the numerical outputs.
# Plotting is handled separately in:
#
#   03_plot_monte_carlo_results.R
#
# The simulation now estimates only the aggregated-OOR model. We do not estimate
# the true all-sites model because, in this simplified experiment, the object of
# interest is the performance of the aggregated specification relative to the
# known true coefficient ratio.
#
# The single estimand is:
#
#   beta_x1 / -beta_cost
#
# This ratio is scale invariant and is therefore comparable across logit models.

rm(list = ls())

script_dir <- file.path("code", "06_monte_carlo")

# Load the synthetic data generator.
source(file.path(script_dir, "01_generate_synthetic_choice_data.R"))

estimate_aggregated_mnl_ratio <- function(data, maxit = 1000) {
  # Estimate the simplified aggregated-OOR multinomial logit model.
  #
  # Utility specification:
  #
  #   V_ij = beta_cost * cost_ij
  #        + beta_x1 * x1_j
  #        + gamma_A * is_A_j
  #
  # OOR has cost = 0 and x1 = 0, so its utility is represented only by gamma_A.
  # The cost and x1 coefficients are identified from the focal alternatives.
  #
  # The only reported estimand is beta_x1 / -beta_cost. The A/OOR ASC is kept
  # because the aggregated alternative needs its own mean utility term, but it
  # is not a target parameter.

  data <- data[order(data$obs_id), ]
  obs_index <- as.integer(factor(data$obs_id))

  x_cols <- c("cost", "x1", "is_A")
  X <- as.matrix(data[, x_cols])
  y <- data$choice == 1

  nll <- function(par) {
    # Negative log-likelihood for multinomial logit.
    utility <- as.vector(X %*% par)

    # Stable log-sum-exp denominator by choice occasion.
    max_u <- ave(utility, obs_index, FUN = max)
    exp_u <- exp(utility - max_u)
    denom <- rowsum(exp_u, obs_index, reorder = FALSE)[, 1]

    chosen_rows <- which(y)
    log_denom <- log(denom[obs_index[chosen_rows]]) + max_u[chosen_rows]

    -sum(utility[chosen_rows] - log_denom)
  }

  # Starting values. The cost coefficient starts negative.
  start <- c(cost = -0.80, x1 = 0.80, is_A = 0.40)

  fit <- try(
    optim(
      par = start,
      fn = nll,
      method = "BFGS",
      hessian = FALSE,
      control = list(maxit = maxit)
    ),
    silent = TRUE
  )

  if (inherits(fit, "try-error")) {
    return(data.frame(
      converged = FALSE,
      beta_cost = NA_real_,
      beta_x1 = NA_real_,
      asc_A = NA_real_,
      ratio_x1_cost = NA_real_,
      logLik = NA_real_
    ))
  }

  par <- fit$par

  data.frame(
    converged = isTRUE(fit$convergence == 0),
    beta_cost = unname(par["cost"]),
    beta_x1 = unname(par["x1"]),
    asc_A = unname(par["is_A"]),
    ratio_x1_cost = unname(par["x1"] / (-par["cost"])),
    logLik = -fit$value
  )
}

run_one_replication <- function(rep_id, scenario, base_seed = 86) {
  # Run one replication:
  # 1. generate one synthetic full dataset under the scenario;
  # 2. collapse A sites into OOR using the generator output;
  # 3. estimate only the aggregated-OOR model;
  # 4. store the estimated ratio and its distance from the true ratio.

  generated <- do.call(
    generate_mc_recreation_data,
    c(as.list(scenario), list(seed = base_seed + rep_id))
  )

  estimate <- estimate_aggregated_mnl_ratio(generated$aggregated_data)

  true_ratio <- generated$true_parameters["ratio_x1_cost"]
  estimate$replication <- rep_id
  estimate$true_ratio_x1_cost <- true_ratio
  estimate$estimation_error <- estimate$ratio_x1_cost - true_ratio
  estimate$actual_A_choice_share <-
    sum(generated$full_data$choice[generated$full_data$is_A == 1]) /
    length(unique(generated$full_data$obs_id))

  # Store the utility-scale percentages for this replication. These columns are
  # retained in the replication-level output and averaged by scenario below.
  scale_decomposition <- generated$utility_scale_decomposition
  for (i in seq_len(nrow(scale_decomposition))) {
    component_name <- paste0("utility_pct_", scale_decomposition$component[i])
    estimate[[component_name]] <- scale_decomposition$percentage[i]
  }

  estimate
}

run_replications_with_progress <- function(scenario, scenario_id,
                                           n_replications,
                                           print_every = 50) {
  # Run all replications for one scenario while printing progress.
  #
  # The default prints replication 1, every 50th replication, and the final
  # replication. Lower print_every if you want denser console output.
  replication_results <- vector("list", n_replications)

  for (rep_id in seq_len(n_replications)) {
    if (rep_id == 1 ||
        rep_id %% print_every == 0 ||
        rep_id == n_replications) {
      message(
        "  Scenario ", scenario_id, ": replication ",
        rep_id, " / ", n_replications,
        " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      )
    }

    replication_results[[rep_id]] <- run_one_replication(
      rep_id = rep_id,
      scenario = scenario
    )
  }

  do.call(rbind, replication_results)
}

summarise_monte_carlo <- function(mc_results) {
  # Summarise the Monte Carlo distribution for the aggregated estimator.
  #
  # Requested performance measures:
  # - average_bias: mean(estimate - true value);
  # - rmse: sqrt(mean((estimate - true value)^2)).
  keep <- is.finite(mc_results$ratio_x1_cost) &
    is.finite(mc_results$true_ratio_x1_cost)

  estimates <- mc_results$ratio_x1_cost[keep]
  truth <- mc_results$true_ratio_x1_cost[keep]
  errors <- estimates - truth

  data.frame(
    estimand = "ratio_x1_cost",
    n_replications = length(estimates),
    true_value = mean(truth, na.rm = TRUE),
    mean_estimate = mean(estimates, na.rm = TRUE),
    average_bias = mean(errors, na.rm = TRUE),
    rmse = sqrt(mean(errors^2, na.rm = TRUE)),
    sd_estimate = sd(estimates, na.rm = TRUE),
    convergence_rate = mean(mc_results$converged, na.rm = TRUE),
    mean_actual_A_choice_share = mean(mc_results$actual_A_choice_share,
                                      na.rm = TRUE)
  )
}

summarise_utility_scale <- function(mc_results) {
  # Average the replication-level utility-scale percentages within a scenario.
  scale_columns <- grep("^utility_pct_", names(mc_results), value = TRUE)
  scale_means <- colMeans(mc_results[, scale_columns, drop = FALSE],
                          na.rm = TRUE)
  as.data.frame(as.list(scale_means), check.names = FALSE)
}

# Scenario grid
# -------------
# The design varies three dimensions:
# - A/OOR choice share;
# - respondent-specific attractiveness heterogeneity inside A;
# - the strength of the link between A taste and focal-site substitution.
#
# Sample size and the number of aggregated sites are fixed.
scenario_grid <- expand.grid(
  n_sites_total = c(20),
  n_sites_aggregated = c(5),
  share_aggregated_choices = c(0.05, 0.15, 0.25),
  within_A_resp_sd = c(0, 1, 2),
  lambda_substitution = c(0, 1, 2),
  n_resp = c(300),
  n_choices_per_resp = c(7),
  KEEP.OUT.ATTRS = FALSE
)

scenario_grid$scenario_id <- seq_len(nrow(scenario_grid))

# Number of Monte Carlo replications per scenario.
n_replications <- 1000

############################################################
## Run the 27 scenarios
############################################################

output_dir <- file.path(
  "output", "models", "monte_carlo_outputs_aggregated_only"
)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

all_results <- list()
all_summaries <- list()
all_scale_summaries <- list()

for (scenario_number in seq_len(nrow(scenario_grid))) {
  message(
    "Running scenario ", scenario_number, " of ", nrow(scenario_grid),
    " at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )

  scenario <- scenario_grid[scenario_number, ]
  scenario$scenario_id <- NULL

  replication_results <- run_replications_with_progress(
    scenario = scenario,
    scenario_id = scenario_number,
    n_replications = n_replications,
    print_every = 50
  )
  replication_results$scenario_id <- scenario_number

  scenario_summary <- summarise_monte_carlo(replication_results)
  scenario_summary$scenario_id <- scenario_number

  scale_summary <- summarise_utility_scale(replication_results)
  scale_summary$scenario_id <- scenario_number

  all_results[[scenario_number]] <- replication_results
  all_summaries[[scenario_number]] <- scenario_summary
  all_scale_summaries[[scenario_number]] <- scale_summary

  # Partial files allow the completed scenarios to be inspected if the long
  # simulation is interrupted.
  saveRDS(
    do.call(rbind, all_results),
    file.path(output_dir, "mc_results_partial.rds")
  )
  saveRDS(
    do.call(rbind, all_summaries),
    file.path(output_dir, "mc_summary_partial.rds")
  )
  saveRDS(
    do.call(rbind, all_scale_summaries),
    file.path(output_dir, "mc_utility_scale_partial.rds")
  )
}


############################################################
## Combine and save the completed simulation
############################################################

mc_results <- merge(
  do.call(rbind, all_results),
  scenario_grid,
  by = "scenario_id"
)

mc_summary <- merge(
  do.call(rbind, all_summaries),
  scenario_grid,
  by = "scenario_id"
)

utility_scale_summary <- merge(
  do.call(rbind, all_scale_summaries),
  scenario_grid,
  by = "scenario_id"
)

saveRDS(
  mc_results,
  file.path(output_dir, "mc_results_aggregated_only.rds")
)
saveRDS(
  mc_summary,
  file.path(output_dir, "mc_summary_aggregated_only.rds")
)
saveRDS(
  utility_scale_summary,
  file.path(output_dir, "mc_utility_scale_aggregated_only.rds")
)

if (!requireNamespace("openxlsx", quietly = TRUE)) {
  stop("Package 'openxlsx' is required to write the Monte Carlo workbook.")
}

workbook <- openxlsx::createWorkbook()
openxlsx::addWorksheet(workbook, "scenario_grid")
openxlsx::writeData(workbook, "scenario_grid", scenario_grid)
openxlsx::addWorksheet(workbook, "summary")
openxlsx::writeData(workbook, "summary", mc_summary)
openxlsx::addWorksheet(workbook, "replication_results")
openxlsx::writeData(workbook, "replication_results", mc_results)
openxlsx::addWorksheet(workbook, "utility_scale")
openxlsx::writeData(workbook, "utility_scale", utility_scale_summary)

excel_file <- file.path(output_dir, "mc_outputs_aggregated_only.xlsx")
openxlsx::saveWorkbook(workbook, excel_file, overwrite = TRUE)

message("Monte Carlo complete.")
message("Outputs saved in: ", output_dir)
